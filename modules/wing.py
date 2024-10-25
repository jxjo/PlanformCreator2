#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""  

    Wing model with planform, wing sections, airfoils 

    Wing                                - main class of data model 
        |-- WingSection                 - the various stations defined by user 
                |-- Airfoil             - the airfoil at a section
        |-- Planform                    - describes geometry, outline of the wing  
        |     (ellipsoid, trapezoid, straightTE, DXF)
        |-- Flap                        - the outline of a flap - dynamically created based on flap group
        |
        |-- refPlanform                 - a ellipsoid reference planform 
        |-- refPlanform_dxf             - a DXF based reference planform 
        |-- xflr5Exporter               - handles export to Xflr5
                |--Planform_Paneled     - Planform which is paneled in x,y direction 
        |-- flzExporter                 - handles export to FLZ_vortex
                |--Planform_Paneled     - Planform which is paneled in x,y direction 
"""

import os
import numpy as np
import bisect
import sys
import copy
from typing                 import override
from pathlib                import Path


# let python find the other modules in the dir of self  
sys.path.append(Path(__file__).parent)
from base.common_utils      import * 
from base.math_util         import * 
from base.spline            import Bezier
from model.airfoil          import Airfoil, GEO_BASIC, GEO_SPLINE
from model.airfoil_examples import Root_Example, Tip_Example
from dxf_utils              import import_fromDXF

import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


class Wing:
    """ 

    Main object - holds the model 

    """
    unit = 'mm'

    def __init__(self, parm_filePath):
        """
        Init wing from parameters in parm_filePath
        """

        dataDict = Parameters (parm_filePath).get_dataDict()
        if not dataDict:
            logger.info ('No input data - a default wing will be created')
            self.parm_filePath = None
        else: 
            parm_version = fromDict (dataDict, "parameter_version", 1.0)
            logger.info (f"Reading wing parameters from '{parm_filePath}' - parameter version {parm_version}")
            self.parm_filePath = parm_filePath

            if parm_version == 1.0:
                dataDict = self._convert_parm_file_v2 (dataDict)

        # handler for the realtive path to the paramter file (working directory)
        self.pathHandler = PathHandler (onFile=parm_filePath)

        self.dataDict = dataDict

        self._name            = fromDict (dataDict, "wing_name", "My new Wing")
        self._wingspan        = fromDict (dataDict, "wingspan", 2000.0) 
        self._chord_root      = fromDict (dataDict, "chord_root", 200.0)
        self._chord_tip       = fromDict (dataDict, "chord_tip", self._chord_root/4)

        # version 1
        self._hingeAngle      = fromDict (dataDict, "hingeLineAngle", 0.0)
        self._flapDepthRoot   = fromDict (dataDict, "flapDepthRoot", 25.0)
        self._flapDepthTip    = fromDict (dataDict, "flapDepthTip", 25.0)

        # create reference line 

        self._reference_line = Reference_Line (self, dataDict=fromDict (dataDict, "reference_line", {})) 

        # attach the Planform 
        self._planform        = None
        planform_type         = fromDict (dataDict, "planform_type", "Bezier")
        self.set_planform      (Planform.having (planform_type, self, dataDict))

        self._wingSections    = WingSections (self, dataDict=fromDict(dataDict, "wingSections", None))
    
        # create reference planforms   
        self.refPlanform_dxf  = Planform_DXF (self, dataDict, ref=True)  # could be not 'isValid'
        self.refPlanform_elli = Planform_Pure_Elliptical (self)     

        # will hold the class which manages Xflr5, FLZ export including its parameters
        self._exporterXflr5     = None 
        self._exporterFlz       = None 
        self._exporterDxf       = None 
        self._exporterAirfoils  = None 

        # miscellaneous parms
        self._rootRe            = fromDict (dataDict, "rootRe", 400000)
        self._airfoilNickPrefix = fromDict (dataDict, "airfoilNickPrefix", "JX")
        
        logger.info (str(self)  + ' created')


    def __repr__(self) -> str:
        # overwrite to get a nice print string 
        return f"{type(self).__name__} \'{self.name}\'"


    # --- save --------------------- 

    def _save (self):
        """ stores the variables into the dataDict"""

        dataDict = {}

        toDict (dataDict, "wingName",           self._name) 
        toDict (dataDict, "wingspan",           self._wingspan) 
        toDict (dataDict, "rootchord",          self._chord_root) 
        toDict (dataDict, "tipchord",           self._chord_tip) 
        toDict (dataDict, "hingeLineAngle",     self._hingeAngle) 
        toDict (dataDict, "flapDepthRoot",      self._flapDepthRoot) 
        toDict (dataDict, "flapDepthTip",       self._flapDepthTip) 

        toDict (dataDict, "rootRe",             self._rootRe) 
        toDict (dataDict, "airfoilNickPrefix",  self._airfoilNickPrefix) 

        toDict (dataDict, "planformType",       self.planform.planformType) 
        self.planform._save (dataDict)

        sectionsList = []
        for section in self.wingSections:
            sectionsList.append (section._save ({}))
        toDict (dataDict, "wingSections", sectionsList) 

        self.refPlanform_dxf._save (dataDict)

        if self._exporterXflr5: 
            toDict (dataDict, "xflr5",              self.exporterXflr5._save()) 
        if self._exporterFlz: 
            toDict (dataDict, "flz",                self.exporterFlz._save()) 
        if self._exporterDxf: 
            toDict (dataDict, "dxf",                self.exporterDxf._save()) 
        if self._exporterAirfoils: 
            toDict (dataDict, "airfoils",           self.exporterAirfoils._save()) 

        return dataDict


    def _convert_parm_file_v2 (self, dataDict :dict) -> dict:
        """ convert paramter file from version 1 to version 2"""

        logger.info (f"Converting parameters to version 2.0")

        dict_v2 = copy.deepcopy (dataDict) # {}

        toDict (dict_v2, "wing_name",       fromDict (dataDict, "wingName", None))
        toDict (dict_v2, "wingspan",        fromDict (dataDict, "wingspan", None))
        toDict (dict_v2, "chord_root",      fromDict (dataDict, "rootchord", None))
        toDict (dict_v2, "chord_tip",       fromDict (dataDict, "tipchord", None))

        # planform

        planform_type = fromDict (dataDict, "planformType", None)
        toDict (dict_v2, "planform_type",   planform_type)

        # reference line 

        refDict = {}

        if planform_type == 'Bezier TE straight':
            # legacy planform 
            toDict (refDict, "xn_root", 1.0)
            toDict (refDict, "xn_tip", 1.0)
            toDict (refDict, "angle",           fromDict (dataDict, "hingeLineAngle", None))

        else:
            # flap hinge line -> reference line 
            f = fromDict (dataDict, "flapDepthRoot", None)
            if f:   toDict (refDict, "xn_root", (100 - f)/100)
            f = fromDict (dataDict, "flapDepthTip", None)
            if f:   toDict (refDict, "xn_tip", (100 - f)/100)
            toDict (refDict, "angle",           fromDict (dataDict, "hingeLineAngle", None))

        if refDict:
            toDict (dict_v2, "reference_line", refDict) 

        # wing sections 

        sectionsList = fromDict (dataDict, "wingSections", None)
        new_sectionsList = []
        if sectionsList: 
            for sectionDict in sectionsList:
                new_sectionDict = {}
                toDict (new_sectionDict, "y",                   fromDict (sectionDict, "position", None))
                toDict (new_sectionDict, "norm_chord",          fromDict (sectionDict, "norm_chord", None))
                toDict (new_sectionDict, "flap_group",          fromDict (sectionDict, "flapGroup", None))
                toDict (new_sectionDict, "eitherPosOrChord",    fromDict (sectionDict, "eitherPosOrChord", None))
                toDict (new_sectionDict, "airfoil",             fromDict (sectionDict, "airfoil", None))

                new_sectionsList.append (new_sectionDict)

        if new_sectionsList:
            toDict (dict_v2, "wingSections", new_sectionsList) 


        return dict_v2


    # ---Properties --------------------- 

    @property
    def name(self):  return self._name
    def set_name(self, newName):  self._name = newName


    @property
    def reference_line (self) -> 'Reference_Line_Abstract':
        """ reference line object of the planform(s)"""
        return self._reference_line


    @property 
    def planform (self): return self._planform
    def set_planform (self, newPlanform: 'Planform'): 
        """ assign new planform to wing"""

        # special treatment for dxf because it could be invalid 
        if newPlanform.is_dxf and not newPlanform.isValid:
            raise Exception            
        
        # adapt existing wing sections to new planform 
        if self.planform:                           # is there already a planform
            section : WingSection
            # switch to trapez from other type? --> fix pos and chord of sections
            if newPlanform.planform_depend_on_sections and self.planform.sections_depend_on_planform:
                for section in self.wingSections:
                    section.fixChordAndPosition()
            # switch to from other type to trapez? -> make sections flexible again 
            elif newPlanform.sections_depend_on_planform and self.planform.planform_depend_on_sections:
                for section in self.wingSections:
                    section.releaseFixedChordWithPosition()

        # store dxf path in wing data - 
        if newPlanform.is_dxf:
            newPlanform.assignToWing()              # get hinge and flap from dxf 
        # we got it 
        self._planform = newPlanform


    @property
    def planformType(self): return self.planform.planformType
    def set_planformType(self, newPlanformType):
        """ set planformType - will create a new planform object for this wing  """
        if (self.planformType != newPlanformType):
            self.set_planform (Planform.having (newPlanformType, self, self.dataDict))
            self.wingSections.sort_by_y()

    @property
    def wingspan(self): return self._wingspan
    def set_wingspan(self, newSpan):
        """ set wingspan - update sections having fixed positions """
        if (newSpan > 10.0):
            oldSpan = self._wingspan
            self._wingspan = newSpan
            section : WingSection
            for section  in self.wingSections:    # all sections within new half wing span
                section.adjustToWing (oldSpan)
            self.wingSections.sort_by_y()

    @property
    def chord_root(self): return self._chord_root
    def set_chord_root(self, newChord):
        """ set chord_root - update first section with new chord  """
        if (newChord > 10.0):
            self._chord_root = newChord
            self.rootSection.adjustToWing()
            self.wingSections.sort_by_y()

    @property
    def chord_tip(self): return self._chord_tip
    def set_chord_tip(self, newChord):
        """ set chord_tip - update tip (last) section with new chord  """
        if (newChord > 1.0):
            self._chord_tip = newChord
            self.tipSection.adjustToWing()
            #self.planform.refresh()             # e.g. update Bezier curve        
            #self.wingSections.sort_by_y()

    @property
    def hingeAngle(self) -> float: 
        """ hingle angle in degrees"""
        return self._hingeAngle

    @property
    def flapDepthRoot(self): return self._flapDepthRoot
    def set_flapDepthRoot(self, newDepth): self._flapDepthRoot = newDepth

    @property
    def flapDepthTip(self): return self._flapDepthTip
    def set_flapDepthTip(self, newDepth): self._flapDepthTip = newDepth

    @property
    def wingSections (self) -> 'WingSections':
        return self._wingSections
    
    @property
    def rootSection (self)   -> 'WingSection':  return self.wingSections[0]

    @property
    def tipSection (self)    -> 'WingSection':  return self.wingSections[-1]

    @property
    def halfwingspan (self):    return (self.wingspan / 2)

    @property
    def rootRe(self): return self._rootRe
    def set_rootRe(self, newRe): self._rootRe = newRe

    @property
    def airfoilNickPrefix(self): return self._airfoilNickPrefix
    def set_airfoilNickPrefix(self, newStr): self._airfoilNickPrefix = newStr


    @property
    def exporterXflr5 (self) : 
        """ returns class managing Xflr5 export """
        from export_Xflr5       import Export_Xflr5         # here - otherwise circular errors

        if self._exporterXflr5 is None:                     # init exporter with parameters in sub dictionary
            xflr5Dict           = fromDict (self.dataDict, "xlfr5", "")
            self._exporterXflr5 = Export_Xflr5(self, xflr5Dict) 
        return self._exporterXflr5     


    @property
    def exporterDxf (self): 
        """ returns class managing Dxf export """
        from export_Dxf       import Export_Dxf         # here - otherwise circular errors

        if self._exporterDxf is None:                   # init exporter with parameters in sub dictionary       
            dxfDict           = fromDict (self.dataDict, "dxf", "")
            self._exporterDxf = Export_Dxf(self, dxfDict) 
        return self._exporterDxf     


    @property
    def exporterFlz (self): 
        """ returns class managing FLZ export """
        from export_FLZ  import Export_FLZ              # here - otherwise circular errors

        if self._exporterFlz is None:                   # init exporter with parameters in sub dictionary
            flzDict           = fromDict (self.dataDict, "flz", "")
            self._exporterFlz = Export_FLZ(self, flzDict) 
        return self._exporterFlz     


    @property
    def exporterAirfoils (self): 
        """ returns class managing export of airfoils"""
        if self._exporterAirfoils is None:              # init exporter with parameters in sub dictionary
            airfoilsDict      = fromDict (self.dataDict, "airfoils", "")
            self._exporterAirfoils = Export_Airfoils(self, airfoilsDict) 
        return self._exporterAirfoils     


    @property
    def workingDir(self): 
        """returns the current working directory which is the dir of the paramter file"""
        return self.pathHandler.workingDir


    # ---Methods --------------------- 


    def save (self, pathFileName):
        """ store data dict to file pathFileName        
        :Returns: 
            True : if succeded, False if failed"""

        currentDict = self._save()

        saveOk = Parameters (pathFileName).write_dataDict(currentDict)

        if saveOk:
            # keep dataDict for later change detection 
            self.dataDict = currentDict  
            # set the current working Dir to the dir of the new saved parameter file            
            self.pathHandler.set_workingDirFromFile (pathFileName)
        return saveOk


    def hasChanged (self):
        """returns true if the parameters has been changed since last save() of parameters"""

        return self._save() != self.dataDict
  
        

    def do_strak (self, geometry  = None): 
        """
        straks the airfoil of all wing sections having a Strak-Airfoil which is 
        created by blending with its real neighbours

        Args: 
            geometry: optional - the desired geometry of the new straked airfoils 
                                 either GEO_BASIC or GEO_SPLINE
        """
        sec: WingSection

        for sec in self.wingSections:
            if sec.airfoil.isStrakAirfoil: 

                # get the neighbour wing sections  with real airfoils 
                leftSec, rightSec = self.wingSections.neighbours_having_airfoil(sec) 

                blendBy  = (sec.norm_chord - leftSec. norm_chord) / \
                           (rightSec.norm_chord - leftSec. norm_chord)

                # strak - set new geometry to achieve higher quality with splined airfoils 

                sec.airfoil.do_strak(leftSec.airfoil,  rightSec.airfoil, blendBy, geometry)


    def do_export_airfoils (self,toDir, useNick=True, teGap_mm = None): 
        """
        exports all also straked airfoils into directory 'toDir'. 
        Optionally use airfoils Nickname as new airfoil name.
        Optionally a te gap in mm can be set for all exported airfoils"""
        fileList  = []
        
        self.do_strak (geometry=GEO_SPLINE)          # ensure strak airfoils are uptodate and splined (quality) 

        sec: WingSection
        for sec in self.wingSections:
            fileList.append (sec.do_export_airfoil (toDir, useNick=useNick, teGap_mm = teGap_mm))
        return fileList


    def getFlaps (self): 
        """
        returns the flap objects based on wing sections flap group
        """
        flapList = []
        if not self.wingSections: return flapList 

        sectionStart : WingSection = self.wingSections[0]
        section : WingSection

        for section in self.wingSections:
            if (section.flap_group != sectionStart.flap_group or \
                section == self.wingSections[-1]) :
                # new flap group or last section -> create flap 
                flapList.append(Flap(self, sectionStart, section))
                sectionStart = section 

        return flapList
    

#-------------------------------------------------------------------------------
# Reference Line   
#-------------------------------------------------------------------------------

class Reference_Line_Abstract:
    """ 
    Abstract super class - defines the reference line where chord distribution is applied  

    """

    name = "Reference abstract"

    def __init__(self, wing: 'Wing', dataDict: dict = None):

        self._wing = wing

        self._xn_root = fromDict (dataDict, "xn_root", 0.75)        # normed x at root
        self._xn_tip  = fromDict (dataDict, "xn_tip", 0.75)         # normed x at tip
        self._angle   = fromDict (dataDict, "angle", 0.0)           # angle in degrees 


    @property
    def wing (self) -> 'Wing':
        """ planform of self"""
        return self._wing

    @property
    def halfwingspan (self) -> float:
        """ halfwingspan - for convenience """
        return self.wing.halfwingspan


    @property 
    def angle (self) -> float:
        """ angle of reference line in degrees"""
        return self._angle 

    def set_angle (self, aVal : float):
        """ set angle of reference line in degrees"""
        aVal = max (-60.0, aVal)
        aVal = min ( 60.0, aVal)
        self._angle= aVal 


    @property
    def xn_root (self) -> float:
        """ normed x at root """
        return self._xn_root
    
    def set_xn_root (self, aVal : float):
        """ set xn value at root"""
        aVal = max (0.0, aVal)
        aVal = min (aVal, 1.0)
        self._xn_root = aVal

    @property
    def x_root (self) -> float:
        """ x at root """
        return self.xn_root * self.wing.chord_root

    def set_x_root (self, aVal : float):
        """ set xn value at root"""
        self.set_xn_root (aVal / self.wing.chord_root)

    @property
    def xn_tip (self) -> float:
        """ normed x at tip """
        return self._xn_tip

    def set_xn_tip (self, aVal : float):
        """ set xn value at tip"""
        aVal = max (0.0, aVal)
        aVal = min (aVal, 1.0)
        self._xn_tip = aVal
    
    
    def x_at (self, y) -> float:
        """ Returns the x-coordinate of the reference line at y""" 
        raise NotImplementedError

    @property
    def x_at_tip (self) -> float:
        """ Returns the x-coordinate of the reference line at tip""" 
        return self.x_at (self.halfwingspan)

    def set_x_tip (self, aVal : float):
        """ set x value at tip - will change angle"""
        # must be overriden
        raise NotImplementedError


    def xn_at (self, y) -> float:
        """ Returns the normed x value of the reference line at y""" 
        # must be overriden
        raise NotImplementedError


    def polyline (self):
        """ Returns self as polyline defined by [x],[y]"""
        raise NotImplementedError


    def polyline_norm (self):
        """ Returns self as polyline defined by [xn],[y]"""
        raise NotImplementedError



class Reference_Line (Reference_Line_Abstract):
    """ Reference line as straight line having angle and x chord position at root and tip"""

    name = "Reference Line"

    
    def x_at (self, y) -> float:
        """ Returns the x-coordinate of the reference line at y""" 

        return self.x_root + np.tan((self.angle/180) * np.pi) * y


    def set_x_tip (self, x_tip : float):
        """ set x value at tip - will change angle and not xn_tip!"""

        dx = x_tip - self.x_root
        dy = self.halfwingspan
        angle = np.arctan (dx/dy) * 180 / np.pi                     # new angle in degrees 
        self.set_angle (angle) 


    def polyline (self):
        """ Returns self as polyline defined by [x],[y]"""

        y = np.asarray([0.0, self.halfwingspan])
        x = np.asarray([self.x_at(0.0), self.x_at(self.halfwingspan)])
        return x, y


    def polyline_norm (self):
        """ Returns self as polyline defined by [xn],[y]"""

        y = np.asarray([0.0, self.halfwingspan])
        x = np.asarray([self.xn_at(0.0), self.xn_at(self.halfwingspan)])
        return x, y


    
    def xn_at (self, y) -> float:
        """ Returns the normed x value of the reference line at y""" 

        yn = y / self.halfwingspan                              # normalized y pos 
        xn = (self.xn_tip - self.xn_root) * yn + self.xn_root 
        return xn




        

#-------------------------------------------------------------------------------
# Planform  
#-------------------------------------------------------------------------------

class Planform:
    """ 
    Abstract super class - defines the outline of a wing based on parameters  

    Wing
       |-- Planform
                Planform_Bezier
                Planform_Trapezoidal
                Planform_Pure_Elliptical        (for reference purposes)
                Planform_DXF                    (for reference purposes)
    """
    planformType  = "abstract"
    isTemplate    = False

    shortDescription = " Abstract - this is a short desciption of planform specialities"

    # is the planform defined by wing section or vice versa - overwrite 
    sections_depend_on_planform = True          # e.g.Bezier    
    planform_depend_on_sections = False         # e.g trapezoid

    is_dxf      = False                         # needs special treatment ...
    _isValid    = True                          # ... because there could be no dxf data

    #    Wing sections may only be defined either by position or by chord
    #    Is defined by planform typ. Within a trapezoidal a section may have both.
    #    An elliptic planform only one of both is valid   

    wingSection_eitherPosOrChord = True

    # instances = []                  # testing 

    def __init__(self, myWing: Wing, dataDict: dict = None):
        """
        Main constructor for new planform belonging to a wing 
        Args:
            :myWing: the wing object self belongs to  
            :dataDict: optional - dictonary with all the data for a valid section
        """

        self.wing   = myWing

        # self.__class__.instances.append(weakref.proxy(self))
        if dataDict:                                    # no info for internal planforms
            logger.info (str(self)  + ' created')

    def __repr__(self) -> str:
        # overwrite class method to get a nice print string of self 
        return f"<{type(self).__name__}>"


    @classmethod
    def get_all_subclasses(cls, acls):
        all_subclasses = []
        for subclass in acls.__subclasses__():
            all_subclasses.append(subclass)
            all_subclasses.extend(cls.get_all_subclasses(subclass))
        return all_subclasses


    @classmethod
    def having(cls, planformType, myWing: Wing, dataDict: dict = None):
        """
        Alternate constructor for new planform based on type of planform
        Only subclasse being 'isTemplate' are taken into account

        Args:
            :planformType: string like 'Bezier' 
            :myWing: the wing object self belongs to  
            :dataDict: optional - dictonary with all the data for a valid section
        Returns:
            :newClass: the class for the planformType
        """

        planformClass = None
        subclass : Planform

        for subclass in cls.get_all_subclasses(cls):
            if (subclass.planformType ==  planformType and subclass.isTemplate):
                planformClass = subclass
                break

        if (not planformClass):
            logger.error("Planform_Class for planformType '%s' not found - using 'Bezier'" %planformType)
            planformClass = Planform_Bezier

        return planformClass(myWing, dataDict)


    @classmethod
    def allTemplatePlanformTypes (cls):
        """
        List of all Planform types which can be used as Templates for a new wing
        Only subclasse being 'isTemplate' are taken into account
        :returns: a list of strings
        """

        allTypes = []
        subclass: Planform

        for subclass in cls.get_all_subclasses(cls):
            if subclass.isTemplate:
                allTypes.append(subclass.planformType)
        return allTypes


    @property
    def chord_root (self):
        """ chord at root - convenience"""       
        return self.wing.chord_root

    def set_chord_root (self, aVal : float):
        """ set root chord"""
        self.wing.set_chord_root (aVal) 


    @property
    def chord_tip (self) :       return self.wing.chord_tip

    def set_chord_tip (self, aVal : float):
        """ set tip chord"""
        self.wing.set_chord_tip (aVal) 


    @property
    def halfwingspan (self):    return self.wing.halfwingspan

    def set_halfwingspan (self, aVal : float):
        """ set halfwingspan"""
        self.wing.set_wingspan (aVal * 2)


    @property
    def hingeAngle(self) -> float:
        """ hinge angle in degrees """       
        return self.wing.hingeAngle
    @property
    def flapDepthRoot(self):    return self.wing.flapDepthRoot
    @property
    def flapDepthTip(self):     return self.wing.flapDepthTip
    @property
    def isValid (self):         return self._isValid         # to overwrite


    @property
    def reference_line (self) -> Reference_Line_Abstract:
        """ reference line object of self"""
        return self.wing.reference_line
    

    def _save (self, dataDict):
        """ stores the variables into the dataDict"""
        # to be overloaded
        pass

    def _yn_points (self):
        """ array of y points along the spann for the different lines  """
        return np.sin(np.linspace(0.0, 1.0, num=50) * np.pi/2)


    def hingeLine (self):
        """ Returns the hinge line based on defined hinge line angle 
        y:     array of the y-stations of the line (root & tip) 
        hinge: array of hinge x values 
        """
        y = np.asarray([0.0, self.halfwingspan])
        x = np.asarray([self.hingePointAt(0), self.hingePointAt(self.halfwingspan)])
        return y, x


    def hingePointAt (self, y):
        """  Returns the x-coordinate of the hinge line at y

        Arguments:
            y --  y-coordinate of interest 
         """        
        xHingeAtRoot = (1-(self.flapDepthRoot/100)) * self.chord_root
        x = xHingeAtRoot + np.tan((self.hingeAngle/180) * np.pi) * y
        return x


    def _norm_chord_at (self, yn, fast=False):
        """
        abstract: returns the normalized chord at position yn 0..1 
        """
        raise NotImplementedError 

    def norm_chord_at (self, y, fast=False):
        """
        returns the chord at position y 
        """
        return self._norm_chord_at (y/self.halfwingspan, fast=fast) 


    def chord_at (self, y, fast=False):
        """
        returns the chord at position y 
        """
        return self._norm_chord_at (y/self.halfwingspan, fast=fast) * self.chord_root 
    

    def norm_chord_line (self):
        """
        normalized chord distribution along the span yn 0..1
        """
        yn         = self._yn_points() 
        norm_chord = np.empty (yn.size)

        for i, yni in enumerate(yn): 
            norm_chord[i] = self._norm_chord_at (yni)
        return yn, norm_chord


    def norm_ref_line (self):
        """ Returns referecne line as polyline in chord distribution defined by [xn],[y]"""

        yn, cn = self.norm_chord_line ()
        xn = np.empty (yn.size)

        xn_root = self.reference_line.xn_root
        xn_tip  = self.reference_line.xn_tip

        for i, yni in enumerate(yn):
            xn[i] = ((xn_tip - xn_root) * yni + xn_root) * cn[i] 

        y = yn * self.halfwingspan

        return xn, y


    def _planform_le_te (self):
        """
        leading and trailing edge x coordinates as arrays

        Returns:
            y: array of the spanwise y-stations 
            le_x: array of x coordinates of leading edge
            te_x: array of x coordinates of trailing edge
        """
        y = self._yn_points() * self.halfwingspan

        le_x = np.zeros (y.size)
        te_x = np.zeros (y.size)
        for i in range(y.size): 
            le_x[i], te_x[i] = self._planform_at (y[i])

        return y, le_x, te_x



    def _planform_at (self, y, chord= None):
        """
        Returns leading and trailing edge x coordinate of planform at y 

        Args:
            y: the y-Position in the planform 0.. halfSpanWidth
            chord: chord length at y - optional - for optimization 
        """

        if chord is None: 
            chord =  self.chord_at (y)

        # add chord to ref line 
        ref_line_x = self.reference_line.x_at (y)               # x coordinate of ref_line 
        ref_line_xn = self.reference_line.xn_at (y)             # rel. pos of ref line within chord

        le_x  = ref_line_x - (chord * ref_line_xn)
        te_x = le_x + chord

        return le_x, te_x


    @property
    def planform_at_tip (self):
        """
        Returns leading and trailing edge x coordinate of planform at tip 
        """
        return self._planform_at (self.halfwingspan)


    def polyline (self):
        """
        The planform as polyline 

        Returns:
            :y: array of the spanwise y-stations of lines 
            :x: array of x coordinates (chord direction)
        """

        y, le_x, te_x = self._planform_le_te ()

        # append lines and nose point to close the polygon 
        polyline_y = np.append(y, np.flip(y))
        polyline_y = np.append(polyline_y, [y[0]])
        
        polyline_x = np.append(le_x, np.flip(te_x))
        polyline_x = np.append(polyline_x, [le_x[0]])

        return polyline_y, polyline_x

    

    def flapPolygon  (self, fromY, toY, nPoints = 50):
        """
        returns an y,x polygon describing a flap between fromY and toY   

        Arguments:
            nPoints --  number of points for idealization of trailing edge - default 20
        """
        yFlapLine = []
        xFlapLine = [] 
        # we start a TE at fromY and going clockwise around the flap upto toY
        yFlapLine.append(fromY)
        xFlapLine.append(self._planform_at (fromY)[1])
        # go to hinge point
        yFlapLine.append(fromY)
        xFlapLine.append(self.hingePointAt(fromY))
        # go along the hinge 
        yFlapLine.append(toY)
        xFlapLine.append(self.hingePointAt(toY))
        # back to TE 
        yFlapLine.append(toY)
        xFlapLine.append(self._planform_at (toY)[1])
        # ... finally along TE to starting point back to TE
        y, le, te = self._planform_le_te()
        i1 = min(bisect.bisect(y, fromY)-1, len(y) -2)    # get te coordinates between fromY and yTo
        i2 = min(bisect.bisect(y, toY)  -1, len(y) -2)
        yFlapLine.extend (np.flip(y [i1:i2+1]))  
        xFlapLine.extend (np.flip(te[i1:i2+1]))

        return np.asarray(yFlapLine), np.asarray(xFlapLine)
    
    def flapDepthAt (self, yPos):
        """ returns the normed flap depth at position yPos
        """
        xHinge   = self.hingePointAt(yPos)
        xLE, xTE = self._planform_at (yPos)
        flapDepth = (xTE - xHinge) / (xTE-xLE)

        if flapDepth > 0.9:                         # sanity - this should not happen 
            flapDepth = 0.9                         # hinge end point of Amokka-JX is outside planform
        return flapDepth

    
    def flapLineAt (self, yPos):
        """ returns a section line (from TE to hinge point) at position yPos
        """
        yFlapLine = []
        xFlapLine = [] 
        # we start a TE at fromY and going clockwise around the flap upto toY
        yFlapLine.append(yPos)
        xFlapLine.append(self._planform_at (yPos)[1])
        # go to hinge point
        yFlapLine.append(yPos)
        xFlapLine.append(self.hingePointAt(yPos))
        return yFlapLine, xFlapLine


    def find_yFromChord (self, chord):
        """
        calculates the y-Position from a chord length 
        Returns:
            :y:   
        """

        # some kind of bubble search is performed until accuracy is better than epsilon
        # Should be overloaded for more precision 

        epsilon = 0.0001                           # normalized chord accuracy 
        myChord = chord / self.chord_root            # normalize 

        if(myChord > 1.0 or myChord < 0): 
            logger.error ("Chord %f must be between 0.0 and root chord" % chord)
            return

        y       = None
        yLeft   = 0 
        yRight  = 1
        chordLeft   = self._norm_chord_at (yLeft,  fast=False)
        chordRight  = self._norm_chord_at (yRight, fast=False)
        
        while y == None:
            if   (abs (myChord - chordLeft)  < epsilon):
                y = yLeft 
                break
            elif (abs (myChord - chordRight) < epsilon):
                y = yRight
                break
            yMiddle = (yLeft + yRight) / 2 
            chordMiddle = self._norm_chord_at (yMiddle, fast=False)
            if (myChord > chordMiddle): 
                yRight     = yMiddle 
                chordRight = chordMiddle
            else:
                yLeft      = yMiddle
                chordLeft  = chordMiddle

        return y * self.halfwingspan



    def calc_area_AR (self, x, y):
        """calculates (approximates) halfwing area and aspect ration AR based 
        on the closed polygon defined by x,y points which are already calculated

        Parameters
        ----------
        x,y : array_like    coordinates of the closed polygon 

        Returns
        -------
        half_area 
        aspectRatio
        """

        # see https://stackoverflow.com/questions/24467972/calculate-area-of-polygon-given-x-y-coordinates
        correction = x[-1] * y[0] - y[-1]* x[0]
        main_area = np.dot(x[:-1], y[1:]) - np.dot(y[:-1], x[1:])
        
        half_area =  0.5*np.abs(main_area + correction) 

        aspectRatio = 2 * self.halfwingspan **2 / half_area
        return half_area, aspectRatio
    

    def refresh (self): 
        """ refresh planform if wing parameters e.g. chord_tip have changed"""
        # to be overloaded when needed
        pass

#-------------------------------------------------------------------------------

class Planform_Bezier(Planform):
    """ 
    Chord distribution is defined by a Bezier Curve 
    """
    planformType  = "Bezier"
    isTemplate    = True

    shortDescription = "Planform based on a Bezier curve function, which is defined by its\n" + \
                        "root and tip tangent. The banana function adapts the flap depth."

    # is the planform defined by wing section or vice versa - overwrite 
    sections_depend_on_planform = True           # e.g.elliptical    
    planform_depend_on_sections = False          # e.g trapezoid

    wingSection_eitherPosOrChord = True


    def __init__(self, myWing: Wing, dataDict: dict = None):
        super().__init__(myWing, dataDict)
        """
        Args:
            :myWing: the wing object self belongs to  
            :dataDict: optional - dictonary with all the data for a valid section
        """

        # init Cubic Bezier for chord distribution 
       
        self._px = [1.0, 1.0, 0.55, 0.0]                            # wing coordinate system 
        self._py = [0.0, 0.55, 1.0, 1.0]
        # from dict only the variable point coordinates of Bezier
        self._px[1]   = fromDict (dataDict, "p1x", 1.00)
        self._px[2]   = fromDict (dataDict, "p2x", 0.55)     # nearly elliptic
        self._py[1]   = fromDict (dataDict, "p1y", 0.55)
        self._px[3]   = self.chord_tip / self.chord_root              # p3 sits on tip chord

        self._bezier = Bezier (self._py, self._px)


        # init Quadratic Bezier for banana of leading edge  
       
        self._banana_px = [0.0, 0.0, 0.0]                           # wing coordinate system 
        self._banana_py = [0.0, 0.5, 1.0]
        # from dict only the variable point coordinates of Bezier
        self._banana_px[1]   = fromDict (dataDict, "banana_p1x", 0.0)
        self._banana_py[1]   = fromDict (dataDict, "banana_p1y", 0.5)

        self._banana = None                                         # init see _norm_banana_function 



    def _save (self, dataDict):
        """ stores the variables into the dataDict"""

        # the flex points of chord Bezier curve 
        toDict (dataDict, "p1x", self._px[1]) 
        toDict (dataDict, "p2x", self._px[2]) 
        toDict (dataDict, "p1y", self._py[1]) 


    # ---Properties --------------------- 

    # Bezier free definition points for chord distribution

    @property                                       # root tangent 
    def p1x(self): return self._px[1]
    def set_p1x(self, aVal): 
        self._px[1] = min (aVal, self._px[0])       # The angle may not become negative as chord value won't be unique!
        self._bezier.set_points (self._py, self._px)
        self.wing.wingSections.sort_by_y()             # oder of sections could have changed 

    @property
    def p1y(self): return self._py[1]
    def set_p1y(self, aVal): 
        self._py[1] = aVal 
        self._bezier.set_points (self._py, self._px)
        self.wing.wingSections.sort_by_y()             # oder of sections could have changed 

    @property
    def tangentAngle_root (self):
        """ angle in degrees of the bezier tangent at root """
        # watch the different coordinate system 
        dx = self._py[1] - self._py[0]
        dy = self._px[1] - self._px[0]
        return np.arctan (dy/dx) * 180 / np.pi
    def set_tangentAngle_root (self, anAngle : float):
        """ set angle in degrees of the bezier tangent at root.
            ! The angle may not become negative as chord value won't be unique! """
        # watch the different coordinate system 
        anAngle = min (anAngle, 0.0)
        hypo = self.tangentLength_root
        dy = hypo * np.sin (anAngle * np.pi / 180.0)
        dx = hypo * np.cos (anAngle * np.pi / 180.0)
        self.set_p1x (self._px[0] + dy)
        self.set_p1y (self._py[0] + dx)
        
    @property
    def tangentLength_root (self):
        dx = self._py[1] - self._py[0]
        dy = self._px[1] - self._px[0]
        return (dx**2 + dy**2)**0.5
    def set_tangentLength_root (self, aLength):
        angle = self.tangentAngle_root
        dy = aLength * np.sin (angle * np.pi / 180.0)
        dx = aLength * np.cos (angle * np.pi / 180.0)
        self.set_p1x (self._px[0] + dy)
        self.set_p1y (self._py[0] + dx)

    @property                                   # tip tangent
    def p2x(self): return self._px[2]
    def set_p2x(self, aVal): 
        self._px[2] = aVal 
        self._bezier.set_points (self._py, self._px)
        self.wing.wingSections.sort_by_y()             # oder of sections could have changed 

    @property                                   
    def p3x(self): return self._px[3]
    def set_p3x(self, aVal): 
        self._px[3] = aVal 
        self._bezier.set_points (self._py, self._px)
        self.wing.wingSections.sort_by_y()             # oder of sections could have changed 

    @property                                    
    def p2y(self): return self._py[2]

    @property
    def tangentAngle_tip (self):
        """ angle in degrees of the bezier tangent at tip - fixed to 90° """
        return 90.0
    
    @property
    def tangentLength_tip (self):
        dx = self._py[3] - self._py[2]
        dy = self._px[3] - self._px[2]
        return (dx**2 + dy**2)**0.5
    def set_tangentLength_tip (self, aLength):
        angle = self.tangentAngle_tip
        dy = aLength * np.sin (angle * np.pi / 180.0)
        self.set_p2x (self._px[3] + dy)


    def controlPoints_as_jpoints (self) -> list[JPoint]: 
        """ 
        Bezier control points as JPoints with limits and fixed property
            x values scalad to halfwingspan 
            y values normed 0..1
        """
        jpoints = []

        for i, point in enumerate(self._bezier.points):

            jpoint = JPoint (point)           
            jpoint.set_x (jpoint.x * self.halfwingspan)     # scale to halfwingspan

            if i == 0:                                      # root 
                jpoint.set_fixed (True) 
            elif i == 1:                                    # root tangent
                jpoint.set_x_limits ((0,self.halfwingspan))
                jpoint.set_y_limits ((0,1))
            elif i == 2:                                    # tip tangent
                jpoint.set_x_limits ((self.halfwingspan,self.halfwingspan))
                jpoint.set_y_limits ((0.2,1))
            else:
                # jpoint.set_fixed (True)                     # tip
                jpoint.set_x_limits ((self.halfwingspan,self.halfwingspan))
                jpoint.set_y_limits ((0.01,0.5))

            jpoints.append(jpoint)

        return jpoints

    def controlPoints_from_jpoints (self, jpoints : list[JPoint]): 
        """ set Bezier control points from JPoints which are scaled to halfwingspan  """

        px, py = [], []
        for jpoint in jpoints:
            px.append(jpoint.x / self.halfwingspan)
            py.append(jpoint.y)

        self._bezier.set_points (px, py)
        self.wing.wingSections.sort_by_y()                     # order of sections could have changed 


    def set_elliptical (self):
        """ sets the tangents so that planform becoms elliptical """

        # see https://stackoverflow.com/questions/14169234/the-relation-of-the-bezier-curve-and-ellipse

        tangent_length = 0.55228474983
        
        self.set_tangentLength_root(tangent_length)
        self.set_tangentLength_tip(tangent_length)
        self.set_tangentAngle_root(0)


    # Banana quadratic Bezier free definition points 

    @property                                   
    def banana_p1x(self): return self._banana_px[1]
    @property                                 
    def banana_p1y(self): return self._banana_py[1]
    def set_banana_p1x(self, aVal): 
        self._banana_px[1] = aVal 
        self._banana = None                             # Bezier with cache will be rebuild
    def set_banana_p1y(self, aVal): 
        self._banana_py[1] = aVal 
        self._banana = None                             # Bezier with cache will be rebuild

    def banana_line (self):
        """
        returns the outline of the banana in wing coordinates 
        Returns:
            :y: array of the spanwise y-stations of lines 
            :banana_x:  array of x coordinates (chord direction)
        """

        if self._banana is None: 
            self._banana = Bezier (self._banana_py, self._banana_px)

        norm_y, norm_x = self._banana.eval (self._norm_u_points ())

        y        = norm_y * self.halfwingspan
        banana_x = norm_x * self.chord_root

        return y, banana_x


    def _norm_u_points (self):
        """ array of u (arc) points along the chord line  """
        return np.linspace(0.0, 1.0, num=100) 


    @override
    def norm_chord_line (self):
        """
        the normalized chord distribution along the span
        """
        # overloaded  - Bezier needs arc points u not y coordinates 

        yn, norm_chord = self._bezier.eval (self._norm_u_points() )
        return yn, norm_chord


    @override
    def _norm_chord_at (self, yn, fast=True):
        """
        Returns the normalized chord of the planform at yn 0..1 (y_normed=True) 
        Normally a linear interpolation is done for fast evaulation (fast=True). 
        Higher Precision is achieved with interpolation of the curve (fast=False) 
        """

        return self._bezier.eval_y_on_x (yn, fast=fast)        # wing coordinate system 


    def _norm_banana_function (self, y_norm):
        """
        Returns the value of the "banana curve" bending the planform 
        Args:
            :y_norm: the normalized y-Position in the planform 0..1
        Returns:
            :banana_x: the value 0..1 at y
        """

        if self._banana_px[1] == 0.0:                               # optimize for no banana
            banana_x = 0.0 
        else: 
            if self._banana is None: 

                # init cache of Bezier curve so following calls will be optimized
                self._banana = Bezier (self._banana_py, self._banana_px)
                x,y = self._banana.eval (self._norm_u_points ())

            banana_x = self._banana.eval_y_on_x (y_norm, fast=True)            # wing coordinate system 
 
        return banana_x       


    @override
    def _planform_le_te (self):
        """
        leading and trailing edge x coordinates as arrays

        Returns:
            y: array of the spanwise y-stations 
            le_x: array of x coordinates of leading edge
            te_x: array of x coordinates of trailing edge
        """
        # overloaded to optimize for Bezier 

        yn, chord_n = self.norm_chord_line ()
        y     = yn * self.halfwingspan
        chord = chord_n * self.chord_root

        le_x = np.zeros (y.size)
        te_x = np.zeros (y.size)
        for i in range(y.size): 
            le_x[i], te_x[i] = self._planform_at (y[i], chord=chord[i])

        return y, le_x, te_x
    
            
    
    def find_yFromChord (self, chord):
        """
        calculates the y-Position from a chord length 
        Returns:
            :y:   
        """
        
        # overloaded for Bezier 

        normChord = chord / self.wing.chord_root       # normalize 

        yPos = self._bezier.eval_x_on_y (normChord, fast=False) * self.wing.halfwingspan

        return yPos 


    
    def refresh (self): 
        """ refresh planform if wing parameters e.g. chord_tip have changed"""

        # when a new tip chord is set, move the bezier points p2 and p3 accordingly 
        #    so length of the tangent at tip is nit changed 

        tangentLength = self.tangentLength_tip          # store current length

        self.set_p3x (self.chord_tip / self.chord_root)
        self.set_p2x (self.p3x + tangentLength)



#-------------------------------------------------------------------------------

class Planform_Pure_Elliptical (Planform):
    """ 
    Defines the outline of an unmodified (more or less) elliptical planform as reference
    """
    planformType  = "Elliptical"
    isTemplate    = False


    def _norm_chord_at (self, yn, fast=False) -> float:
        """
        Returns the normalized chord of an elliptical planform at yn 0..1
        """

        return np.sqrt(1.0-(yn  ** 2))




#-------------------------------------------------------------------------------

class Planform_Trapezoidal(Planform):
    """ 
    Defines the outline of a trapezoidal planform  
    """
    planformType  = "trapezoidal"
    isTemplate    = True

    wingSection_eitherPosOrChord = False   # both pos and chord my be defined by user

    sections_depend_on_planform = False          
    planform_depend_on_sections = True           # sections define the shape of the planform

    shortDescription = "Planform defined by its wing sections,\n" + \
                       "which have the attribut 'Defines planform'" 

    def _yn_points (self):
        """
        array of y points along the spann for the different lines   
        """
        # just the sections yPos is needed (sections with yPos and chord defined)
        norm_y_points, chords = self.wing.wingSections.yn_cn_having_fixed () 
        return np.asarray(norm_y_points)


    def _norm_chord_at (self, yn, fast=False):
        """
        Returns the normalized chord of the planform at yn 0..1.
        The planform is defined by the wing sections having a position and a chord value.
        With these a multi trapezoid is defined
        """

        y_chords, chords = self.wing.wingSections.yn_cn_having_fixed ()   # tuple y_norm, c_norm

        for iSec in range(len(y_chords) - 1):
            y_before = y_chords [iSec]
            y_after  = y_chords [iSec+1]
            if yn  >= y_before and  yn  <= y_after:
                chord_before = chords[iSec]
                chord_after  = chords[iSec+1]
                return interpolate(y_before, y_after, chord_before, chord_after, yn) 

        # not found ...? 
        raise ValueError ("Could not interpolate for trapezoid the value ", yn)



    def adjust_planform_to_reference (self):
        """
        Adjust the relevant wing sections of trapezoid to become close to reference(ellipse)
        """
         
        y_list, _ = self.wing.wingSections.yn_cn_having_fixed()

        for index, yPos in enumerate(y_list):        # skip root and tip 
            if not (index == 0 or index == (len(y_list)-1)): 

                section = self.wing.wingSections.at_y (yPos)

                if section: 
                    # go a little left to get chord so ellipsoid is better approx.
                    leftPos = y_list [index-1]
                    refPos = leftPos + 0.95 *  (yPos - leftPos)
                    newChord = self.wing.refPlanform_elli._norm_chord_at(refPos)
                    section.set_norm_chord (newChord)
                else: 
                    raise ValueError ("Section at norm position %f not found" % yPos)


#-------------------------------------------------------------------------------

class Planform_Paneled (Planform_Trapezoidal):
    """ 
    Helper (slave) Planform which presents the actual planform as a paneled (trapezoid) version
    Used for Xflr5 and FLZ export   
    """
    planformType  = "paneled"
    isTemplate    = False

    wingSection_eitherPosOrChord = False   # both pos and chord my be defined by user

    sections_depend_on_planform = False          
    planform_depend_on_sections = True           # sections define the shape of the planform

    shortDescription = "Planform defined by its wing sections,\n" + \
                       "which have a defined position and chord." 

    def __init__(self, myWing: Wing, dataDict: dict = None):
        super().__init__(myWing, dataDict)
        """
        Args:
            :myWing: the wing object self belongs to  
            :dataDict: optional - dictonary with all the data for a valid section
        """
        # read additional parameters of this shapeform 
        self._x_panels    = fromDict (dataDict, "x-panels", 10)
        self._x_dist      = fromDict (dataDict, "x-distribution", "cosine")
        self._y_panels    = fromDict (dataDict, "y-panels", 10)
        self._y_dist      = fromDict (dataDict, "y-distribution", "uniform")
        self._y_minWidth  = fromDict (dataDict, "y-minWidth", 20)
        self._minchord_tip = fromDict (dataDict, "minTipChord", 30)

        self.distribution_fns = {}
        self.distribution_fns["uniform"]= lambda y : y
        self.distribution_fns["-sine"]  = lambda y : np.sin (y     * np.pi/2)
        self.distribution_fns["sine"]   = lambda y : np.sin ((y+2) * np.pi/2) + 1
        self.distribution_fns["cosine"] = lambda y : ((np.cos ((y+1) * np.pi)) + 1) / 2


    def _save (self, dataDict):
        """ stores the variables into the dataDict"""
        toDict (dataDict, "x-panels",       self._x_panels) 
        toDict (dataDict, "x-distribution", self._x_dist) 
        toDict (dataDict, "y-panels",       self._y_panels) 
        toDict (dataDict, "y-panels",       self._y_panels) 
        toDict (dataDict, "y-minWidth",     self._y_minWidth) 
        toDict (dataDict, "minTipChord",    self.min_chord_tip) 


    # ---Properties --------------------- 

    @property
    def x_panels (self):                return self._x_panels
    def set_x_panels (self, val: int):  self._x_panels = int(val)

    @property
    def x_dist (self):                  return self._x_dist
    def set_x_dist (self, val):  
        if val in self.distribution_fns:
            self._x_dist = val

    @property
    def y_panels (self):                return self._y_panels
    def set_y_panels (self, val: int):  self._y_panels = int(val)

    @property
    def y_dist (self):                  return self._y_dist
    def set_y_dist (self, val):  
        if val in self.distribution_fns:
            self._y_dist = val

    @property
    def y_minWidth (self):              return self._y_minWidth
    def set_y_minWidth (self, val):     self._y_minWidth = val

    @property
    def min_chord_tip (self):             
        """ minimum chord at tip when generating panels"""
        return self._min_chord_tip
    
    def set_min_chord_tip (self, val): 

        if not val is None: 
            _, chords = self.wing.wingSections.y_c() 
            # minchord_tip must be less than 90% wing section chord before tip  
            val = min (val, chords[-2] * 0.9)  
            # ... more than tip chord  
            val = max (val, self.wing.chord_tip)  
        self._min_chord_tip = val

    @property
    def isTipCutted (self):                     
        """ is tip cutted due to minchord_tip?""" 

        # sanity check: is minchord_tip between chord at tip and chord tip-1

        _, chords = self.wing.wingSections.y_c()
        return chords[-1] < self._min_chord_tip and chords[-2] > self._min_chord_tip


    # --- Methods --------------------- 

    def _calc_reducedTipPos (self):
        """ 
        returns a reduced tip y position if chord_tip is smaller than minTipCord
         - other tip y position remains halfwingspan 
        """

        min_chord_tip = self._min_chord_tip
        newTipPos   = self.halfwingspan

        # sanity check: is minchord_tip between chord at tip and chord tip-1

        yPosList, chordList = self.wing.wingSections.y_c()
        if chordList[-1] > min_chord_tip or chordList[-2] < min_chord_tip: return self.halfwingspan

        # find the y position having minchord_tip 
        yTip     = yPosList[-1]
        yLeftTip = yPosList[-2]

        firstGuess = yLeftTip + (yTip - yLeftTip) * 0.25 
        bounds     = (yLeftTip, yTip)      
        fn         = lambda y : self.wing.planform.chord_at(y) - min_chord_tip

        try: 
            newTipPos = findRoot (fn, firstGuess , no_improve_thr=10e-5, bounds=bounds) 
        except:
            newTipPos = self.halfwingspan

        return newTipPos


    def _sections_y_chord (self):
        """ returns yPos and chord of all sections as two lists"""

        y, c = self.wing.wingSections.y_c()

        # is there a new tip yPos because of minimum tip chord? 
        if self.isTipCutted:

            reduced_y_tip = self._calc_reducedTipPos()
            y[-1]  = reduced_y_tip                        # set "new" tip 
            c[-1] = self.wing.planform.chord_at(reduced_y_tip)
            
        return y, c
    

    def y_panels_forSection(self, sections_y, iSec):
        """y-panels for section i depending on min panel width"""

        if iSec < len(sections_y)-1:
            y_left = sections_y [iSec]
            y_right  = sections_y [iSec+1]
            dy_sec = y_right - y_left
            # assure a min panel width in y-direction 
            npanels = min ( self.y_panels, round(dy_sec/self.y_minWidth))
            npanels = max (npanels, 1)                                      # ensure at least 1 panel
        else:
            npanels = 0 
        return npanels


    def distribution_fns_names (self):
        """ a list of available distribution functions"""
        return list(self.distribution_fns.keys())


    def _planform_at (self, y):
        """
        the planform represented as trapezoid
        """

        sections_y = self._sections_y_chord() [0]

        for iSec in range(len(sections_y) - 1):
            y_before = sections_y [iSec]
            y_after  = sections_y [iSec+1]
            if y  >= y_before and  y <= y_after:
                le_before, te_before = self.wing.planform._planform_at (y_before)
                le_after, te_after   = self.wing.planform._planform_at (y_after)
                le = interpolate(y_before, y_after, le_before, le_after, y) 
                te = interpolate(y_before, y_after, te_before, te_after, y) 
                return le, te

        # not found ...? 
        raise ValueError ("Could not interpolate '%s'for paneled planform the value " % y)

    def y_panel_lines (self):
        """
        the lines from LE to TE representing the y panels of the planform 
        Returns:
            :y: list of array of the y-stations of the line  
            :x: list of array of x values of LE and TE 
            :deviation: in percent to the actual planform chord 
        """
        lines_y = []
        lines_le_to_te = []  

        deviations = []        

        y_distribution_fn = self.distribution_fns [self.y_dist]
        sections_y = self._sections_y_chord() [0]

        # get le te for all sections 
        sections_le_te = []
        for y_sec in sections_y: 
            # take actual planform .. function (maybe faster) 
            sections_le_te.append(self.wing.planform._planform_at (y_sec))

        # now calc a y line according to y distribution function between sections 
        for iSec in range(len(sections_y) - 1):
            y_left   = sections_y [iSec]
            y_right  = sections_y [iSec+1]
            dy_sec   = y_right - y_left
            le_left  = sections_le_te[iSec][0]
            te_left  = sections_le_te[iSec][1]
            le_right = sections_le_te[iSec+1][0]
            te_right = sections_le_te[iSec+1][1]

            # assure a min panel width in y-direction 
            y_panels = self.y_panels_forSection(sections_y, iSec)

            # line on section will be double
            for d_yn_pan in np.linspace (0, 1, y_panels +1): 
                # ! perform the distribution function
                yn_distrib_pan = y_distribution_fn (d_yn_pan)

                le = interpolate(0.0, 1, le_left, le_right, yn_distrib_pan)
                te = interpolate(0.0, 1, te_left, te_right, yn_distrib_pan)
                yPos = y_left + yn_distrib_pan * dy_sec

                lines_y.append([yPos, yPos])
                lines_le_to_te.append([le, te])

                # calculate the deviation to actual planform upto close to tip 
                if yPos < self.halfwingspan:
                    chord_paneled = te-le
                    le,te  =  self.wing.planform._planform_at (yPos) 
                    chord_actual  = te-le
                    deviation = abs((chord_actual - chord_paneled) / chord_actual) * 100
                else:
                    deviation = 0 
                deviations.append(deviation)

        return lines_y, lines_le_to_te, deviations
    
    def x_panel_lines (self):
        """
        the lines from one section to the next representing the x panels of the planform 
        Returns:
            :y: list of array of the y-stations of the line  
            :x: list of array of x values of LE and TE 
        """
        lines_y = []
        lines_panels_x = []           
        x_distribution_fn = self.distribution_fns [self.x_dist]
        sections_y = self._sections_y_chord() [0]

        # get le te for all sections 
        sections_le_te = []
        for y_sec in sections_y: 
            sections_le_te.append(self.wing.planform._planform_at (y_sec))


        # now calc a x horizontal line according to x distribution function between sections 
        for iSec in range(len(sections_y) - 1):
            y_left = sections_y [iSec]
            y_right  = sections_y [iSec+1]

            le_left  = sections_le_te[iSec][0]
            te_left  = sections_le_te[iSec][1]
            le_right = sections_le_te[iSec+1][0]
            te_right = sections_le_te[iSec+1][1]

            chord_left  = te_left  - le_left
            chord_right = te_right - le_right

            # line on section will be double
            for dxn_pan in np.linspace (0, 1, self.x_panels +1): 
                # ! perform the distribution function
                xn_distrib_pan = x_distribution_fn (dxn_pan)

                x_left  = le_left  + xn_distrib_pan * chord_left
                x_right = le_right + xn_distrib_pan * chord_right
                lines_y.append          ([y_left, y_right])
                lines_panels_x.append   ([x_left, x_right])

        return lines_y, lines_panels_x


#-------------------------------------------------------------------------------
# Planform_DXF class
#-------------------------------------------------------------------------------

class Planform_DXF(Planform):
    """ 
    Defines the outline of an DXF imported planform  

    Wing
       |-- Planform
                Planform_DXF
    """

    planformType  = "DXF file"
    isTemplate    = True

    is_dxf        = True

    shortDescription = "Planform loaded from a DXF file. \n" +\
                       "Therefore either position or chord of a section can be defined."
    
    wingSection_eitherPosOrChord = True         # either pos and chord my be defined by user

    sections_depend_on_planform = True          
    planform_depend_on_sections = False           # sections define the shape of the planform



    def __init__(self, myWing, dataDict: dict = None, dxf_Path= None, ref:bool = False):
        super().__init__(myWing, dataDict)
        """
        Args:
            :myWing: the wing object self belongs to  
            :dataDict: optional - dictonary with all the data for a valid section
        """

        self._dxf_isReference   = ref   # is it a reference planform 

        self._dxfMirrorX       = fromDict (dataDict, "dxfMirrorX", True)   

        self.le_norm_dxf        = None   # the normalized leading edge in points from DXF
        self.te_norm_dxf        = None   # the normalized trailing edge in points from DXF
        self.hingeLine_norm_dxf = None   # the normalized hinge line in points from DXF
        self.hingeAngle_dxf     = None
        self.adaptHingeAngle    = False   # the hinge angle will be adapted to wing (for reference DXF)
        self.flapDepthRoot_dxf  = None
        self.flapDepthTip_dxf   = None 
        self.infoText           = ''     # info about dxf parsing 

        if dxf_Path: 
            self._dxfPathFilename  = dxf_Path
        else: 
            if ref:
                self._dxfPathFilename  = fromDict (dataDict, "refPlanform_dxfPath", None)
            else:
                self._dxfPathFilename  = fromDict (dataDict, "planform_dxfPath", None)


        if self._dxfPathFilename is not None:
            dxfFullPathName = self.wing.pathHandler.fullFilePath (self._dxfPathFilename)
            if os.path.isfile (dxfFullPathName):
                self.load_dxf (dxfFullPathName)
            else:
                logger.error ("The dxf file '%s' doesn't exist" % self._dxfPathFilename)
                self._dxfPathFilename = None


    def _save (self, dataDict):
        """ stores the variables into the dataDict"""
        if self.isValid:
            if self._dxf_isReference:
                toDict (dataDict, "refPlanform_dxfPath", self._dxfPathFilename) 
            else:
                toDict (dataDict, "planform_dxfPath",    self._dxfPathFilename) 
        

    # ---Properties --------------------- 
    @property
    def dxf_isReference (self):
        return self._dxf_isReference
    def set_dxf_isReference (self, isOverlay: bool):
        self._dxf_isReference = isOverlay

    def dxf_filename (self):
        """ just the filename without the path of the dxf file"""
        if self._dxfPathFilename != None: 
            fileName = os.path.basename(self._dxfPathFilename)
        else: 
            fileName = ''
        return fileName

    @property
    def dxf_pathFilename (self):
        """ the complete filename with the path of the dxf file"""
        return self._dxfPathFilename
    
    def set_dxfPathFilename (self, aNewPathFile):
        """ set path and import dxf"""
        
        if aNewPathFile:
            # the path could be either absolute or relative to parameter dict
            loadPathFile = self.wing.pathHandler.fullFilePath (aNewPathFile)
            if not os.path.isfile(loadPathFile):
                logger.error ("DXF file \'%s\' does not exist" % loadPathFile)
            else:
                # load and parse data from file
                self.load_dxf (loadPathFile)

            if self.isValid:
                self._dxfPathFilename = self.wing.pathHandler.relFilePath(loadPathFile)
        else: 
            # clear self
            self._dxfPathFilename   = None
            self.le_norm_dxf        = None   
            self.te_norm_dxf        = None   
            self.hingeLine_norm_dxf = None   
            self.hingeAngle_dxf     = None
            self.flapDepthRoot_dxf  = None
            self.flapDepthTip_dxf   = None 
            self.infoText           = ''     

    #---------- overwrites of class Planform ------------------------------

    @property
    def hingeAngle(self):      
        # overwrite - in case of reference wing the hinge angle is the
        # delta between the hinge angle of dxf and the angle of the wing 
        if self.dxf_isReference: 
            return self.wing.hingeAngle - self.hingeAngle_dxf
        else: 
            return self.wing.hingeAngle 

    @property
    def isValid (self): 
        return (self.le_norm_dxf != None and self.te_norm_dxf != None)    
    
    def _planform_le_te (self):
        # self can be empty
        if not self.isValid: return [],[],[]
        return super()._planform_le_te()


    def _norm_chord_at (self, yn, fast=False):
        """
        Returns the normalized chord of the planform at yn 0..1
        Args:
            :y_norm: the normalized y-Position in the planform 0..1
        Returns:
            :chord: the chord 0..1 at y
        """
        le = self.__get_xFromY(self.le_norm_dxf, yn)
        te = self.__get_xFromY(self.te_norm_dxf, yn)

        if (le == None) or (te == None):
            logger.error("DXF import: y-coordinate not found, planform could not be created")
            return None
        else:
            return abs(te-le)


    def _planform_at (self, y):
        """
        calculates all relevant geo data of the planform at y 

        Args:
            :y: the y-Position in the planform 0.. halfSpanWidth
        Returns:
            :leadingEdge:  ...-point at y
            :trailingEdge: ...-point at y
        """
        if not self.isValid: return [],[]

        # _planform_function_dxf takes the original contour from le an te 
        #    This especially for straight te the better choice 
        if self.dxf_isReference: 
            return self._planform_function_dxf (y)

        hingeRootx = (1-(self.flapDepthRoot/100)) * self.chord_root
        hingeTipx = hingeRootx +  \
                            np.tan((self.hingeAngle/180) * np.pi) * self.halfwingspan

        # chord-length at y
        chord =  self.chord_at (y)  

        # calculate hingeDepth in percent at this particular point along the wing
        hingeDepth_y = interpolate(0.0, self.halfwingspan,
                                    self.flapDepthRoot, self.flapDepthTip,y)

        flapDepth = (hingeDepth_y/100) * chord 

        # finally the main "lines" at position y
        hinge        = (hingeTipx-hingeRootx)/(self.halfwingspan) * (y) + hingeRootx
        leadingEdge  = hinge - (chord - flapDepth)
        trailingEdge = leadingEdge + chord

        return leadingEdge, trailingEdge


    def _planform_function_dxf (self, y):
        """
        retrieve le and te out DXF le and te 
        This is best if self is a reference planform - it should be close to original 
        """
        if not self.isValid: return [],[]

        y_norm = y / self.halfwingspan
        le_norm = self.__get_xFromY(self.le_norm_dxf, y_norm)
        te_norm = self.__get_xFromY(self.te_norm_dxf, y_norm)
        
        if (le_norm == None) or (te_norm == None):
            logger.error("DXF import: y-coordinate not found, planform could not be created")
            return 0.0, 0.0

        le = le_norm * self.chord_root
        te = te_norm * self.chord_root

        # add hinge line angle 
        if self.adaptHingeAngle:
            deltaHinge = np.tan((self.hingeAngle/180) * np.pi) * y_norm * self.halfwingspan
            le += deltaHinge 
            te += deltaHinge

        return le, te


    def hingeLine_dxf (self):
        """
        Returns the hinge line based on DXF data 
        Returns:
            :y:  array of the y-stations of the line (root & tip) 
            :hinge: array of hinge x values
        """
        if not self.isValid or not self.hingeLine_norm_dxf: return [],[]

        npoints = len(self.hingeLine_norm_dxf)
        y       = np.empty (npoints)
        hinge   = np.empty (npoints)
        for i in range(npoints): 
            y[i]     = self.hingeLine_norm_dxf[i][0] * self.halfwingspan
            hinge[i] = self.hingeLine_norm_dxf[i][1] * self.chord_root

            # add hinge line angle to show original
            if self.adaptHingeAngle:
                deltaHinge = np.tan((self.hingeAngle/180) * np.pi) * y[i]
                hinge[i] += deltaHinge 
        
        return y , hinge 


    def __get_xFromY(self, line, x):
        
        eps = 0.00001                           # deviation for 2 points to be equal
        y = None

        iIdendical = None
        iGreater = None 

        for idx in range(len(line)):
            xp, yp = line[idx]
            if (abs(x-xp) < eps):               # found identical point ?
                iIdendical = idx
            elif (xp >= (x+ eps)) and (idx>=1):        # first point with x value >= x
                iGreater = idx
                break

        if iIdendical:
             x, y = line[iIdendical] 
        elif iGreater:
            x1, y1 = line[iGreater-1]
            x2, y2 = line[iGreater]
            y = interpolate(x1, x2, y1, y2, x)
        else:
            logger.error("__get_yFromX, xcoordinate %f not found" % x)
            y = 0.0
                
        return y

    def mirror_dxf (self):

        # assuming first point is at root
        x, LE_y = self.le_norm_dxf[0]
        x, TE_y = self.te_norm_dxf[0]
        y_mirror = (LE_y + TE_y) / 2
        for i, p in enumerate(self.le_norm_dxf):
            x,y = p
            self.le_norm_dxf [i] = x, y_mirror - (y - y_mirror)
        for i, p in enumerate(self.te_norm_dxf):
            x,y = p
            self.te_norm_dxf [i] = x, y_mirror - (y - y_mirror)
        for i, p in enumerate(self.hingeLine_norm_dxf):
            x,y = p
            self.hingeLine_norm_dxf [i] = x, y_mirror - (y - y_mirror)

    def flapDepth_dxf (self, tipAt = 0.95):
        """returns flapDepth root and tip of dxf in percent"""
        
        flapDepthRoot_DXF = 0.0  
        flapDepthTip_DXF = 0.0
        
        if  self.hingeLine_norm_dxf == None: return

        le = self.__get_xFromY(self.le_norm_dxf, 0)
        te = self.__get_xFromY(self.te_norm_dxf, 0)
        hl = self.__get_xFromY(self.hingeLine_norm_dxf, 0)
        flapDepthRoot_DXF = (hl - te) / (le - te) 

        le = self.__get_xFromY(self.le_norm_dxf, tipAt)
        te = self.__get_xFromY(self.te_norm_dxf, tipAt)
        hl = self.__get_xFromY(self.hingeLine_norm_dxf, tipAt)
        flapDepthTip_DXF = (hl - te) / (le - te) 

        return flapDepthRoot_DXF * 100, flapDepthTip_DXF * 100 

    def assignToWing (self):
        """ assign and adjusts the dxf to the wing data - or vice versa"""

        if self.isValid and not self.dxf_isReference:
            # dxf data is master for the wing 
            if self.hingeAngle_dxf != None: 
                self.wing.reference_line.set_angle (self.hingeAngle_dxf)
                self.wing.set_flapDepthRoot (self.flapDepthRoot_dxf)
                self.wing.set_flapDepthTip  (self.flapDepthTip_dxf)


    def load_dxf (self, dxf_file):
        """ parse and load a dxf planform from file"""

        infoText = []
        self.le_norm_dxf, self.te_norm_dxf, self.hingeLine_norm_dxf, self.hingeAngle_dxf = \
            import_fromDXF(dxf_file)

        # check result
        if self.le_norm_dxf != None:
            logger.info("DXF planform imported from file %s" % dxf_file)

            infoText.append(" - Leading edge %d points"  % (len(self.le_norm_dxf)))
            infoText.append(" - Trailing edge %d points" % (len(self.te_norm_dxf)))

            if (self._dxfMirrorX):
                infoText.append(" - mirrored along y-axis")
                logger.info("Mirroring DXF planform for LE showing upwards")
                self.mirror_dxf()
            if self.hingeLine_norm_dxf != None: 
                if self._dxfMirrorX:               # also mirror hinge line 
                    self.hingeAngle_dxf = - self.hingeAngle_dxf
                infoText.append(" - Hinge line angle %.2f degrees" %self.hingeAngle_dxf)

                yTip = 0.98
                self.flapDepthRoot_dxf, self.flapDepthTip_dxf = self.flapDepth_dxf (tipAt = yTip)
                infoText.append(" - Flap at root %.1f %%"       % (self.flapDepthRoot_dxf))
                infoText.append(" - Flap at tip (%.2f) %.1f %%" % (yTip, self.flapDepthTip_dxf))
            else:
                infoText.append(" - No hinge line detected")


            self.infoText = '\n'.join(infoText)
        else:
            logger.error("DXF import from file %s failed" % dxf_file)
            self.infoText = "DXF file couldn't be loaded"



#-------------------------------------------------------------------------------
# WingSections  
#-------------------------------------------------------------------------------

class WingSection:
    """ 
    A certain station of wing. Is defined by its Re or fixed by y pos.
    There is always a root section (index 0) and a tip section (last index)   

    Wing
       |-- WingSection
             |-- airfoil
    """
    def __init__(self, myWing: Wing, dataDict: dict = None):
        """
        Main constructor for new section belonging to a wing 

        Args:
            myWing: the wing object self belongs to  
            dataDict: optional - dictonary with all the data for a valid section
        """

        # get initial data from dict 
        self.wing : Wing = myWing

        self._y                 = fromDict (dataDict, "y", None)
        self._norm_chord        = fromDict (dataDict, "norm_chord", None)
        self._flap_group        = fromDict (dataDict, "flap_group", 1)
        self._eitherPosOrChord  = fromDict (dataDict, "eitherPosOrChord", None)

        self.isRoot = (self._y == 0.0) 
        self.isTip  = (self._y == self.wing.halfwingspan)

        if self.isRoot:
            self._norm_chord = 1.0
 
        if (self._y is None and self._norm_chord is None):
            logger.error ("Wing section: Either position or chord / reynolds must be set")
            logger.info  ("Wing section: Setting chord to 0.8 of root")
            self._norm_chord    = self.wing.chord_root * 0.8

        # section may have either a position or chord for
        # ... trapezoid planform: section may have both which will defne planform  
        if self._eitherPosOrChord is None:
            self._eitherPosOrChord = self.wing.planform.wingSection_eitherPosOrChord

        if not self.isRootOrTip:
            if self.eitherPosOrChord:
                # either position or chord should be flexibel if planform defines sections
                if (not self._y is None) and (not self._norm_chord is None):
                    self._norm_chord = None
            else:
                # for trapezoid both position and chord must have a value 
                if self._norm_chord is None:    self._norm_chord = self.norm_chord
                if self._y is None:             self._y = self.y


        # create airfoil and load coordinates if exist 
        self._init_airfoil (dataDict = fromDict (dataDict, "airfoil", None))


    def _save (self, sectionDict):
        """ stores the variables into the dataDict - returns the filled dict"""

        toDict (sectionDict, "y",                   self._y) 
        toDict (sectionDict, "norm_chord",          self._norm_chord) 
        toDict (sectionDict, "flap_group",          self._flap_group) 
        toDict (sectionDict, "eitherPosOrChord",    self._eitherPosOrChord) 

        airfoilDict = self.airfoil._save ({})
        toDict (sectionDict, "airfoil",  airfoilDict) 

        return sectionDict


    def _init_airfoil (self, dataDict = None, pathFileName = None, workingDir=None):
        """create airfoil for section """
 
        # read data for airfoil either from dict of parameter file 
        #  or get filepath from user selection 
        # then create new 'Airfoil' 

        if dataDict: 
            airfoil = Airfoil.onDict (dataDict, geometry=GEO_BASIC,
                                      workingDir= self.wing.pathHandler.workingDir)
        else: 
            airfoil = Airfoil (pathFileName= pathFileName, geometry=GEO_BASIC,
                               workingDir=workingDir)

        if airfoil.isExisting:
            airfoil.load()
        else:
            if self.isRoot: 
                airfoil = Root_Example()
            elif self.isTip:
                airfoil = Tip_Example()
            else:
                airfoil = Airfoil(name="<strak>", geometry=GEO_BASIC)
                #airfoil.set_isStrakAirfoil (True)

        self.airfoil : Airfoil = airfoil 


    def __repr__(self) -> str:
        # overwrite class method to get a nice print string of self 
        if (self._y != None):
            info = f"y={self.y}mm"
        else:
            info = f"chord={self.norm_chord:.2f}" 
        return f"<{type(self).__name__} {info}>"

    #-----------------------------------------------------------

    @property
    def y (self):
        """
        y-position of wing section - either from fix position or calculated from chord position
        """
        if (self._y != None):
            pos =  self._y
        elif (self.isRoot):                     # enforce position 
            pos = 0.0
        elif (self.isTip):                      # enforce position
            pos = self.wing.halfwingspan
        elif (self._norm_chord != None): 
            pos = self.wing.planform.find_yFromChord (self._norm_chord * self.wing.chord_root) 
        else:
            pos = None
        return pos 

    def set_y (self, value : float):
        """
        set y-position of wing section to a fixed value - removing rel. chord setting
        """
        if (value == None):
            self._y = None
        elif (value < 0.0 or value > self.wing.halfwingspan):
            logger.error ("wingSection: Position must be inside half wingspan %dmm" %self.wing.halfwingspan)
        elif (self.isRoot or self.isTip):
            logger.error ("wingSection: Do not set root or tip chord via wing section")
        else:
            self._y = value
            if self.eitherPosOrChord:
                self._norm_chord  = None             # can't have fix position *and* relative chord setting
            else: 
                self._norm_chord  = self.norm_chord  # also fix chord (trapezoid)

    @property
    def yn (self):
        """ normed y-position of wing section 0..1
        """
        return self.y / self.wing.halfwingspan

    def set_yn (self, value):
        """
        set normed y-position of wing section 0..1 - removing rel. chord setting
        """
        self.set_y (value * self.wing.halfwingspan)


    @property
    def norm_chord (self):
        """ normalized chord 0..1 - either from property or calculated from position  """
        if   (not self._norm_chord is None): return self._norm_chord
        elif (not self._y is None): 
            return self.wing.planform._norm_chord_at (self.yn) 
        else:
            raise ValueError ("Wingsection: Both position and norm chord are not defined")

    def set_norm_chord (self, value):
        """ set the normaized chord 0..1 - the section will move to a new position   """
        if (value is None and self._y is None):
            logger.error ("wingSection: Can't remove normalized chord. Either position or chord must be defined")
        elif(value < 0.0):
            logger.error ("wingSection: Normalized chord must => 0")
        elif(value > 1.0):
            logger.error ("wingSection: Normalized chord must <= 1")
        elif (self.isRoot or self.isTip):
            logger.error ("wingSection: Do not set root or tip chord via wing section")
        else:
            self._norm_chord = value
            if self.eitherPosOrChord:
                self._y       = None         # remove other values, section is flex
            else: 
                self._y       = self.y    # also fix position (trapezoid)

    @property
    def chord (self):
        """ chord 0..chord_root - calculated from normalized chord """
        return self.norm_chord * self.wing.chord_root

    def set_chord (self, value):
        """ set the chord 0..chord_root - set to normalized chord  """
        self.set_norm_chord (value / self.wing.chord_root)

    @property
    def flap_group (self):
        """ flap group beginning with this section """
        return self._flap_group
    def set_flap_group (self, value):
        self._flap_group = value

    @property
    def isRootOrTip (self): 
        return self.isRoot or self.isTip

    @property
    def Re (self):
        """ Reynolds number at this section calculated from chord """
        return self.norm_chord * self.wing.rootRe
    def set_Re (self, value):
        """ set Re number of this section - the section will move to a new position  """
        if (not value is None and value > 0) and (not self.isRootOrTip ):
            self.set_norm_chord  (value / self.wing.rootRe)

    @property
    def eitherPosOrChord (self) -> bool: 
        """ self may only be defined by user either by position or by chord.
        Is defined by planform typ. Within a trapezoidal a section may have both   """
        return self._eitherPosOrChord

    def set_eitherPosOrChord (self, aBool : bool):

        if self.isRootOrTip:
            self._eitherPosOrChord = False
        else:
            self._eitherPosOrChord = aBool

            if self._eitherPosOrChord:
                # either position or chord should be flexibel if planform defines sections
                if (not self._y is None) and (not self._norm_chord is None) :
                    self._norm_chord = None
            else:
                # for trapezoid both position and chord must have a value 
                if self._norm_chord is None:    self._norm_chord = self.norm_chord
                if self._y is None:             self._y = self.y


    def isSet_eitherPosOrChord_disabled (self): 
        """ change of fixPosChord of section allowed? only for trapezoid planform  """
        return self.wing.planform.wingSection_eitherPosOrChord or self.isRootOrTip



    # ---Methods --------------------- 

    def isReDisabled (self):
        """ true if Re calculation is not possible  - e.g. missing Re at root"""
        return (self.wing.rootRe is None or self.wing.rootRe <= 0) or (self.isRootOrTip)


    def name (self) -> str:
        """ short unique name for wing section like "Section 7"""
        if self.isRoot:
            info = "Root"
        elif self.isTip:
            info = "Tip"
        else:
            info = "Section %s" % self.index ()
        return info


    def name_short (self) -> str:
        """ very short unique name for wing section like "7"""
        if self.isRoot:
            info = "Root"
        elif self.isTip:
            info = "Tip"
        else:
            info = f"{self.index()}"
        return info


    def label (self) -> str:
        """ short unique label for wing section like "7: at 1240mm """
        index = self.index()
        if self.isRoot:
            info = "Root"
        elif self.isTip:
            info = "Tip"
        elif (self._y != None):
            info = "at %.0fmm" %self._y
        elif (self._norm_chord != None):
            info = "with %.0fmm" % (self._norm_chord * self.wing.chord_root)
        elif (self.Re != None):
            info = "Re %.0f" %self.Re
        else:
            info = "flex" 
        return f"{index}: {info}"


    def adjustToWing (self, oldSpan = None):
        """ takes care of a modified wing main data - correct position, chord etc 
        :Args: oldSpan the wing span before modification
        """
        if self.isTip:
            self._y  = self.wing.halfwingspan  
            self._norm_chord = self.wing.chord_tip / self.wing.chord_root
        elif self.isRoot:
            self._y = 0.0
            self._norm_chord = 1.0 
        elif self.has_fixed_y: 
            if oldSpan:
                self._y = self._y * self.wing.wingspan / oldSpan  

    @property
    def has_fixed_y (self):
        """ has wing section a fixed y position within wing? """
        return (not self._y is None)


    @property
    def has_fixed_chord (self):
        """ has wing section a fixed chord within wing? """
        return (not self._norm_chord is None)


    def fixChordAndPosition (self):
        """ sets the chord and yPos (for trapezoid planform at creation)
        """
        if self._y is None:   
            self._y = self.y                  # write calculated value fix into variable    
        if self._norm_chord is None: 
            self._norm_chord = self.norm_chord      # write calculated value fix into variable                
        return 
    

    def releaseFixedChordWithPosition (self):
        """ removes a fixed chord when position is defined
        (for not trapezoid planform at creation)
        """
        if ( self._y is None and self._norm_chord is None):
            raise ValueError ("Wingsection: Both position and norm chord are not defined")
        self._norm_chord = None      # remove fixed 
        return 


    def hasFixPosChord (self) -> bool:
        """ has wing section a fixed position and chord within wing? (for trapezoid planform)
        """
        return self._y is not None and self._norm_chord is not None
    
    def set_hasFixPosChord (self, aBool : bool):
        self.set_eitherPosOrChord (not aBool ) 
    

    def y_limits (self) -> tuple: 
        """ y position limits as tuple of self before touching the neighbour section
        """
        leftSec  : WingSection = None
        rightSec : WingSection = None

        leftSec, rightSec = self.wing.wingSections.neighbours (self) 

        if self.isRoot or self.isTip:
            safety = 0.0                # = fixed
            leftLimit  = self.y
            rightLimit = self.y
        else:
            safety = int (self.wing.halfwingspan / 500.0) 
            if leftSec: 
                leftLimit = leftSec.y
            else:
                leftLimit = self.y
            if rightSec: 
                rightLimit = rightSec.y
            else:
                rightLimit = self.y
        return (leftLimit + safety, rightLimit - safety)


    def yn_limits (self) -> tuple: 
        """ yn position limits as tuple of self before touching the neighbour section
        """
        left, right = self.y_limits()
        return ( left / self.wing.halfwingspan, right / self.wing.halfwingspan)


    def norm_chord_limits (self) -> tuple: 
        """ norm chord limits of self as tuple - must be between left and right neighbour
        """
        leftSec  : WingSection = None
        rightSec : WingSection = None

        leftSec, rightSec = self.wing.wingSections.neighbours (self) 

        if self.isRoot or self.isTip:
            safety = 1.0                # fixed
            upperLimit  = self.norm_chord
            lowerLimit  = self.norm_chord
        else:
            safety = 1.005              # = 0,5% 
            if leftSec: 
                upperLimit = leftSec.norm_chord
            else:
                upperLimit = self.norm_chord
            if rightSec: 
                lowerLimit = rightSec.norm_chord
            else:
                lowerLimit = self.norm_chord
        return lowerLimit * safety, upperLimit / safety 


    def chord_limits (self): 
        """  chord limits of self as tuple - must be between left and right neighbour
        """
        left, right = self.norm_chord_limits ()
        left  = left  * self.wing.chord_root
        right = right * self.wing.chord_root
        return (left, right)


    def Re_limits (self): 
        """  Re limits of self as tuple - must be between left and right neighbour
        """
        left, right = self.norm_chord_limits ()
        left  = left  * self.wing.rootRe
        right = right * self.wing.rootRe
        return (left, right)

    
    def airfoilNickPostfix(self): 
        """ the postfix like '-16' of airfoils nickname
        if there is rootRe take this else use normchord"""

        if self.wing.rootRe >= 1000000:
            return "-%03d" % int(self.Re/10000)
        elif self.wing.rootRe >= 100000:
            return "-%02d" % int(self.Re/10000)
        elif self.wing.rootRe >= 10000:
            return "-%02d" % int(self.Re/1000)
        elif self.wing.rootRe > 0:
            return "-%d" % int(self.Re)
        else:
            return "-%03d" % int(self.norm_chord * 100)
        
    def airfoilNick(self): 
        """ the airfoil nickname like GP-16 from nickname prefix and reynolds (or normchord)"""

        if self.wing.airfoilNickPrefix:
            return self.wing.airfoilNickPrefix + self.airfoilNickPostfix()
        else:
            return None

    def airfoil_canBeRemoved (self):
        return (not self.isRootOrTip) and (not self.airfoil.isStrakAirfoil)
    
    def airfoil_canBeEdited (self):
        return not self.airfoil.isStrakAirfoil
    
    def set_airfoilWithPathFileName (self, pathFileName):
        """ sets a new real Airfoil based on its path and loads it """

        relPathFile = self.wing.pathHandler.relFilePath (pathFileName)
        self._init_airfoil (pathFileName=relPathFile, workingDir= self.wing.workingDir)
        self.airfoil.load()


    def do_export_airfoil (self,toDir, useNick=True, teGap_mm = None): 
        """ exports airfoil into directory 'toDir'. 
        Optionally use airfoils Nickname as new airfoil name.
        Optionally define a teGap in mm for the exported airfoil 
        Returns the filename of the exported airfoil
        """
        if useNick and self.airfoilNick(): 
            newName = self.airfoilNick()
        else: 
            newName = None

        # te gap in mm? if yes scale it to normed ... do it
        if not teGap_mm is None and teGap_mm >= 0.0: 
            teGap = teGap_mm / self.chord
        else: 
            teGap = None 

        filePathName = self.airfoil.save_copyAs(dir=toDir, destName = newName, teGap = teGap)

        return os.path.basename(filePathName) 
        
    
    def line (self):
        """
        the wing section as a line from LE to TE
        Returns:
            :y: array of the y-stations of the line  
            :x: array of x values of LE and TE 
        """
        le, te = self.wing.planform._planform_at (self.y)
        y = [self.y, self.y]
        x = [le, te]
        return y, x

    def norm_line (self):
        """
        the wing section as a normed line from LE to TE
        Returns:
            y:  array of the y-stations of the line  
            xn: array of normed xn values of LE and TE 
        """
        y  = [self.y, self.y]
        xn_le = self.wing.planform._norm_chord_at (self.yn)
        xn = [xn_le, 0]
        return y, xn


    def index (self) -> int:
        """ index 1..n of self within wingSections"""
        return self.wing.wingSections.number_of (self)


class WingSections (list):
    """ container class (list) for wing sections of a wing"""

    def __init__ (self, myWing: Wing, dataDict: dict = None):
        super().__init__ ([])

        self.wing = myWing

        # create all sections based on sections list in dataDict 

        curSections =[]
        if dataDict:
            for sectionDict in dataDict:
                curSections.append(WingSection (myWing, sectionDict))

        newSections = curSections
        if (curSections ==[]):
            newWing = True
            logger.info (' - creating example wing sections...')
        else:
            newWing = False

        # create at least a root section
        if (newWing or not curSections [0].isRoot ):
            if not newWing: logger.info ('Root wing section missing. Creating a new one ...')
            newSections.insert(0, WingSection (myWing, {"y": 0.0}))

        # create a third example section for a new wing
        if (newWing):
            newSections.append(WingSection (myWing, {"norm_chord": 0.8}))

        # create at least a tip section
        if (newWing or not newSections [-1].isTip ):
            if not newWing: logger.info ('Tip wing section missing. Creating a new one ...')
            newSections.append(WingSection (myWing, {"y": myWing.halfwingspan}))

        logger.info (" %d Wing sections added" % len(newSections))

        self.extend (newSections)



    def create_after (self, aSection: 'WingSection') -> 'WingSection' : 
        """
        creates and inserts a new wing section after aSection with a chord in the middle to the next neighbour 
        Args:
            :aSection: a wingSection
        Return: 
            :newSection: the new wingSection
        """
        rightSection : WingSection = None
        newSection   : WingSection = None

        if isinstance(aSection, WingSection) and not aSection.isTip :

            _, rightSection = self.neighbours (aSection)
            left_norm_chord  = aSection.norm_chord
            right_norm_chord = rightSection.norm_chord
            new_norm_chord = (left_norm_chord + right_norm_chord) / 2
            aSection_index = self.number_of (aSection)

            newSection = WingSection (self.wing, {"norm_chord": new_norm_chord})
            newSection.set_flap_group (aSection.flap_group)
            self.insert(aSection_index+1,newSection)
        return newSection


    def create_at (self, y : float): 
        """
        create and insert a new wing section at y pos 
        """
        
        # check y
        if y < (self.wing.halfwingspan / 500) : return 
        if y > (self.wing.halfwingspan * 499 / 500) : return 

        newSection = WingSection (self.wing, {"y": y})
        #     newSection.set_flap_group (aSection.flap_group)
        self.insert(1,newSection)
        self.sort_by_y ()

        return newSection


    def delete (self, aSection: 'WingSection') -> bool: 
        """  delete wing section - return new current if succeeded"""

        if aSection and not aSection.isRoot and not aSection.isTip:

            try:
                index = self.index (aSection)
                self.remove (aSection)

                return self[index-1] 
            except: 
                pass

        return None  


    def sort_by_y (self):
        """ 
        Re-sort wing sections to an ascending y pos. 
        When changing major wing parms sections could become out of sort order when
            they have fixed yPos and chord mixed    
        """
        self.sort (key=lambda sec: sec.y) 


    def neighbours (self, aSection):
        """
        returns the neighbour before and after a wingSection - if no neighbour return None 
        """
        try:
            index = self.index (aSection) 
        except: 
            return None, None
        
        if index == 0 : 
            leftSec = None
        else:    
            leftSec = self[index - 1] 
        try:
            rightSec = self[index + 1] 
        except: 
            rightSec = None
        return leftSec, rightSec


    def neighbours_having_airfoil (self, aSection: 'WingSection'):
        """
        returns the neighbour before and after a wingSection, which a existing, real
         airfoil. Needed for 'strak'  - if no neighbour return None 
        """
        leftSec :  WingSection = None
        rightSec : WingSection = None

        try:
            index = self.index (aSection) 
        except: 
            return None, None

        sec: WingSection
        #left neighbour 
        if aSection.isRoot: leftSec = None
        else: 
            for sec in reversed(self [0:index]):
                if sec.airfoil.isExisting:
                    leftSec = sec
                    break 
        #right neighbour 
        if aSection.isTip: rightSec = None
        else: 
            for sec in self [index+1: ]:
                if sec.airfoil.isExisting:
                    rightSec = sec
                    break 
        return leftSec, rightSec


    def y_c (self) -> tuple [list, list]:
        """
        returns y position and chord of all wing sections as two lists.
        """
        aSection : WingSection = None
        y = []
        c = []
        for aSection in self:
            c.append(aSection.chord)
            y.append(aSection.y)
        return y, c


    def yn_cn_having_fixed (self) -> tuple [list, list]:
        """
        returns the norm pos and chord of all wing sections having these defined.
        as two list  yn and  norm_chord - including root and tip 

        returns: 
            yn list: --  list of normed y positions 
            cn list: -- list of normed chords 
        """
        aSection : WingSection = None
        yn = []
        cn = []
        for aSection in self:
            if aSection.hasFixPosChord():
                cn.append(aSection._norm_chord)
                yn.append  (aSection._y / self.wing.halfwingspan)
        return yn, cn



    def at_y (self, y : float) -> 'WingSection':
        """
        returns the section at position y.
        """
        aSection : WingSection = None
        for aSection in self:
            if aSection.y == y :
                return aSection
        return None



    def number_of (self, aSection : WingSection) -> int | None:
        """
        returns the corrected index 1..n of a wing section in all wing sections
        """
        try:
            index = self.index (aSection) + 1
        except: 
            index = None
        return index

 

#-------------------------------------------------------------------------------
# Flap   
#-------------------------------------------------------------------------------

class Flap:
    """ 
    defining the outline of a single flap based on flap group in the wing sections  

    Wing
       |-- Flap 
    """
    def __init__(self, myWing: Wing, sectionLeft: WingSection, sectionRight: WingSection):
        """
        Main constructor for new flap belonging to a wing 
        """
        self.wing : Wing = myWing

        self.flap_group  = sectionLeft.flap_group
        self.sectionName = sectionLeft.name()
        self.y , self.x = myWing.planform.flapPolygon (sectionLeft.y, sectionRight.y)
        self.depthLeft  = myWing.planform.flapDepthAt (sectionLeft.y)
        self.depthRight = myWing.planform.flapDepthAt (sectionRight.y)
        self.lineLeft   = myWing.planform.flapLineAt  (sectionLeft.y)
        self.lineRight  = myWing.planform.flapLineAt  (sectionRight.y)
         

#-------------------------------------------------------------------------------
# Export airfoils of all wing sections    
#-------------------------------------------------------------------------------

class Export_Airfoils:
    """ 
    Handle export of the current airfoils of wing to a subdirectory
    """
    def __init__(self, wing : Wing, myDict: dict = None):
 
        self.wing       = wing
        self.workingDir = wing.workingDir       
        self._exportDir         = fromDict (myDict, "exportDir", "airfoils")
        self._useNick           = fromDict (myDict, "useNick", True)
        self._setTeGap          = fromDict (myDict, "setTeGap", False)
        self._teGap_mm          = fromDict (myDict, "teGap_mm", 0.5)


    def _save (self):
        """ returns the parameters of self in dataDict"""
        myDict = {}
        toDict (myDict, "exportDir",        self._exportDir) 
        toDict (myDict, "useNick",          self._useNick) 
        toDict (myDict, "setTeGap",         self._setTeGap) 
        toDict (myDict, "teGap_mm",         self._teGap_mm) 
        return myDict


    @property
    def exportDir(self):
        """the directory for flz export - path is relativ to current or absolute """
        return self._exportDir
    
    def set_exportDir(self, newStr): 
        # insure a valid, relativ path 
        self._exportDir = PathHandler (workingDir=self.workingDir).relFilePath (newStr)

    @property
    def baseAndExportDir(self):
        """the directory for airfoil export including current dir """
        return PathHandler (workingDir=self.workingDir).fullFilePath (self.exportDir)

    @property
    def useNick(self) -> bool: return self._useNick
    def set_useNick(self, aBool): self._useNick = aBool

    @property
    def setTeGap(self) -> bool: return self._setTeGap
    def set_setTeGap(self, aBool): self._setTeGap = aBool

    @property
    def teGap_mm(self) -> float: return self._teGap_mm
    def set_teGap_mm(self, aVal): self._teGap_mm = aVal


    def doIt (self): 
        """ main entry: start the export to the file defined in self parameters.
        Returns a message string what was done """

        if self.setTeGap: 
            teGap = self.teGap_mm
        else: 
            teGap = None

        targetDir = self.baseAndExportDir

        airfoilList = self.wing.do_export_airfoils (targetDir, useNick=self.useNick, teGap_mm=teGap)      

        logger.info ("Airfoils written to " + targetDir) 
        message = "Airfoils: \n\n" + \
                  ',  '.join(airfoilList)  + \
                  "\n\n exported to \n\n" + \
                  "'" +  targetDir + "'"      
        return message




#-------------------------------------------------------------------------------

# Main program for testing 
if __name__ == "__main__":

    print ("Current directory: ",os.getcwd())
    filename = "..\\examples\\Amokka-JX\\Amokka-JX.json"
    # filename = ""
    myWing = Wing (filename)

