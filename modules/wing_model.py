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
        |-- refPlanform_DXF             - a DXF based reference planform 
        |-- xflr5Exporter               - handles export to Xflr5
                |--Planform_Paneled     - Planform which is paneled in x,y direction 
        |-- flzExporter                 - handles export to FLZ_vortex
                |--Planform_Paneled     - Planform which is paneled in x,y direction 
"""

import os
import numpy as np
from math import  sin
import bisect
import json
import sys
from pathlib import Path
from math_util import findRoot


# let python find the other modules in the dir of self  
sys.path.append(Path(__file__).parent)
from common_utils       import *
from spline             import Bezier
from airfoil            import Airfoil
from airfoil_examples   import Root_Example, Tip_Example


# disables all print output to console
print_disabled = False



class Wing:
    """ 

    Main object - holds the model 

    """
    unit = 'mm'

    def __init__(self, paramFilePath):
        """
        Init wing from parameters in paramFilePath
        """

        dataDict = Parameters (paramFilePath).get_dataDict()
        if dataDict is None:
            NoteMsg ('No input data - a default wing will be created')
            wingExists = False
            self.paramFilePath = None
        else: 
            InfoMsg ('Reading wing parameters from %s' % paramFilePath)
            wingExists = True
            self.paramFilePath = paramFilePath

        # handler for the realtive path to the paramter file (working directory)
        self.pathHandler = PathHandler (onFile=paramFilePath)

        self.dataDict = dataDict

        self._name            = fromDict (dataDict, "wingName", "My new Wing", wingExists)
        self._wingspan        = fromDict (dataDict, "wingspan", 2000.0, wingExists) 
        self._rootchord       = fromDict (dataDict, "rootchord", 200.0, wingExists)
        self._tipchord        = fromDict (dataDict, "tipchord", self._rootchord/4, wingExists)

        self._hingeAngle      = fromDict (dataDict, "hingeLineAngle", 0.0, wingExists)
        self._flapDepthRoot   = fromDict (dataDict, "flapDepthRoot", 25.0, wingExists)
        self._flapDepthTip    = fromDict (dataDict, "flapDepthTip", 25.0, wingExists)


        # attach the Planform 
        tmp_planformType      = fromDict (dataDict, "planformType", "Bezier", wingExists)
        self._planform        = None
        self.planform         = Planform.having (tmp_planformType, self, dataDict) # via setter

        self.wingSections     = self.createSectionsOn (fromDict(dataDict, "wingSections", None))
    
        # create an extra Planform as dxf reference  
        self.refPlanform_DXF  = Planform_DXF (self, dataDict, ref=True)
        
        # create an extra Planform as major reference ()
        self.refPlanform      = Planform_Pure_Elliptical (self)     # could be not 'isValid'

        # will hold the class which manages Xflr5, FLZ export including its parameters
        self._exporterXflr5     = None 
        self._exporterFlz       = None 
        self._exporterDxf       = None 
        self._exporterAirfoils  = None 

        # miscellaneous parms
        self._rootRe            = fromDict (dataDict, "rootRe", 400000, wingExists)
        self._airfoilNickPrefix = fromDict (dataDict, "airfoilNickPrefix", "JX", msg=False)
        
        InfoMsg (str(self)  + ' created')


    def __repr__(self) -> str:
        # overwrite to get a nice print string 
        return f"{type(self).__name__} \'{self.name}\'"


    # --- save --------------------- 

    def _save (self):
        """ stores the variables into the dataDict"""

        dataDict = {}

        toDict (dataDict, "wingName",           self._name) 
        toDict (dataDict, "wingspan",           self._wingspan) 
        toDict (dataDict, "rootchord",          self._rootchord) 
        toDict (dataDict, "tipchord",           self._tipchord) 
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

        self.refPlanform_DXF._save (dataDict)

        if self._exporterXflr5: 
            toDict (dataDict, "xflr5",              self.exporterXflr5._save()) 
        if self._exporterFlz: 
            toDict (dataDict, "flz",                self.exporterFlz._save()) 
        if self._exporterDxf: 
            toDict (dataDict, "dxf",                self.exporterDxf._save()) 
        if self._exporterAirfoils: 
            toDict (dataDict, "airfoils",           self.exporterAirfoils._save()) 

        return dataDict

    # ---Properties --------------------- 

    @property
    def name(self):  return self._name
    def set_name(self, newName):  self._name = newName

    @property 
    def planform (self): return self._planform
    
    @planform.setter 
    def planform (self, newPlanform: 'Planform'): 
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
            self.planform =  Planform.having (newPlanformType, self, self.dataDict)

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

    @property
    def rootchord(self): return self._rootchord
    def set_rootchord(self, newChord):
        """ set rootchord - update first section with new chord  """
        if (newChord > 10.0):
            self._rootchord = newChord
            self.rootSection.adjustToWing()

    @property
    def tipchord(self): return self._tipchord
    def set_tipchord(self, newChord):
        """ set tipchord - update tip (last) section with new chord  """
        if (newChord > 1.0):
            self._tipchord = newChord
            self.tipSection.adjustToWing()
            self.planform.refresh()             # e.g. update Bezier curve        

    @property
    def hingeAngle(self): return self._hingeAngle
    def set_hingeAngle(self, newAngle): 
        newAngle = max (newAngle, -5)
        newAngle = min (newAngle, 45)
        self._hingeAngle = newAngle

    @property
    def flapDepthRoot(self): return self._flapDepthRoot
    def set_flapDepthRoot(self, newDepth): self._flapDepthRoot = newDepth

    @property
    def flapDepthTip(self): return self._flapDepthTip
    def set_flapDepthTip(self, newDepth): self._flapDepthTip = newDepth
    
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
    def exporterXflr5 (self): 
        """ returns class managing Xflr5 export """
        from export_Xflr5       import Export_Xflr5         # here - otherwise circular errors

        if self._exporterXflr5 is None:                     # init exporter with parameters in sub dictionary
            xflr5Dict           = fromDict (self.dataDict, "xlfr5", "", msg=False)
            self._exporterXflr5 = Export_Xflr5(self, xflr5Dict) 
        return self._exporterXflr5     


    @property
    def exporterDxf (self): 
        """ returns class managing Dxf export """
        from export_Dxf       import Export_Dxf         # here - otherwise circular errors

        if self._exporterDxf is None:                   # init exporter with parameters in sub dictionary       
            dxfDict           = fromDict (self.dataDict, "dxf", "", msg=False)
            self._exporterDxf = Export_Dxf(self, dxfDict) 
        return self._exporterDxf     


    @property
    def exporterFlz (self): 
        """ returns class managing FLZ export """
        from export_FLZ  import Export_FLZ              # here - otherwise circular errors

        if self._exporterFlz is None:                   # init exporter with parameters in sub dictionary
            flzDict           = fromDict (self.dataDict, "flz", "", msg=False)
            self._exporterFlz = Export_FLZ(self, flzDict) 
        return self._exporterFlz     


    @property
    def exporterAirfoils (self): 
        """ returns class managing export of airfoils"""
        if self._exporterAirfoils is None:              # init exporter with parameters in sub dictionary
            airfoilsDict      = fromDict (self.dataDict, "airfoils", "", msg=False)
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


    def createSectionsOn (self, sectionsDict): 
        """
        create new wingSections based on data in the dict
        Args:
            :sectionsDict: the wingSections entry in the data dict 
        Returns: 
            list of wingSections
        """
        curSections =[]
        # create all sections based on data dictionary
        if (sectionsDict !=[] and sectionsDict != None):
            for sectionDict in sectionsDict:
                curSections.append(WingSection (self, sectionDict))

        newSections = curSections
        if (curSections ==[]):
            newWing = True
            InfoMsg (' - creating example wing sections...')
        else:
            newWing = False

        # create at least a root section
        if (newWing or not curSections [0].isRoot ):
            if not newWing: NoteMsg ('Root wing section missing. Creating a new one ...')
            newSections.insert(0, WingSection (self, {"position": 0.0}))

        # create a third example section for a new wing
        if (newWing):
            newSections.append(WingSection (self, {"norm_chord": 0.8}))

        # create at least a tip section
        if (newWing or not newSections [-1].isTip ):
            if not newWing: NoteMsg ('Tip wing section missing. Creating a new one ...')
            newSections.append(WingSection (self, {"position": self.halfwingspan}))

        InfoMsg (" %d Wing sections added" % len(newSections))

        return newSections


    def createSectionAfter (self, aSection: 'WingSection'): 
        """
        creates a new wing section after aSection with a chord in the middle to the next neighbour 
        Args:
            :aSection: a wingSection
        Return: 
            :newSection: the new wingSection
        """
        rightSection : WingSection = None
        newSection   : WingSection = None

        if not aSection is None and not aSection.isTip :
            dummySec, rightSection = self.getNeighbourSectionsOf (aSection)
            left_norm_chord  = aSection.norm_chord
            right_norm_chord = rightSection.norm_chord
            new_norm_chord = (left_norm_chord + right_norm_chord) / 2
            aSection_index = self.wingSections.index (aSection)

            newSection = WingSection (self, {"norm_chord": new_norm_chord})
            newSection.set_flapGroup (aSection.flapGroup)
            self.wingSections.insert(aSection_index+1,newSection)
        return newSection


    def deleteSection (self, aSection: 'WingSection'): 
        """
        deletes wing section
        Args:
            :aSection: a wingSection
        """
        if aSection and not aSection.isRoot and not aSection.isTip:
            self.wingSections.remove (aSection) 
        return 


    def get_wingSections_having_normChord (self):
        """
        returns the norm pos and chord of all wing sections having these defined.
        as two list  y_norm and  norm_chord - including root and tip 

        returns: 
            posList --  list of normed y positions 
            chordList -- list of normed chords :)
        """
        aSection : WingSection = None
        yPosList = []
        chordList = []
        for aSection in self.wingSections:
            if aSection.hasFixedPositionAndChord():
                chordList.append(aSection.norm_chord)
                yPosList.append  (aSection.norm_yPos)
        return yPosList, chordList


    def get_wingSections_yPos_chord (self):
        """
        returns yPos and chord of all wing sections as two lists.
        """
        aSection : WingSection = None
        yPosList = []
        chordList = []
        for aSection in self.wingSections:
            chordList.append(aSection.chord)
            yPosList.append(aSection.yPos)
        return yPosList, chordList


    def get_wingSections_norm_Pos (self):
        """
        returns the normed y pos all wing sections 

        returns: 
            posList --  list of normed y positions 
        """
        aSection : WingSection = None
        yPosList = []
        for aSection in self.wingSections:
            yPosList.append  (aSection.norm_yPos)
        return yPosList

    def get_wingSectionBy_norm_yPos (self, norm_yPos) -> 'WingSection':
        """
        returns the section at norm position norm_yPos.
        """
        aSection : WingSection = None
        for aSection in self.wingSections:
            if aSection.norm_yPos == norm_yPos :
                return aSection
        return None

    def set_wingSection_norm_chord (self, norm_yPos, newChord):
        """
        sets the norm pos and chord of all wing sections having these defined.
         
        arguments: 
            norm_yPos -- the normed position of the affected wing section 
            chordList -- the new chord of the section  :)
        """
        aSection : WingSection = None
        aSection = self.get_wingSectionBy_norm_yPos (norm_yPos) 
        if aSection: 
            aSection.set_norm_chord (newChord)
        else: 
            raise ValueError ("Section at norm position %f not found" % norm_yPos)
       

    def wingSections_eitherPosOrChord (self):
        """ 
        Wing sections may only be defined by user either by position or by chord
        Is defined by planform typ. Within a trapezoidal a section may have both.
        An elliptic planform only one of both is valid   """
        return self.planform.wingSection_eitherPosOrChord
    

    def wingSectionIndexOf (self, aSection):
        """
        returns the index 1..n of a wing section in all wing sections
        """
        try:
            index = self.wingSections.index (aSection) + 1
        except: 
            index = None
        return index

    def getNeighbourSectionsOf (self, aSection):
        """
        returns the neighbour before and after a wingSection - if no neighbour return None 
        """
        try:
            index = self.wingSections.index (aSection) 
        except: 
            return None, None
        
        if index == 0 : 
            leftSec = None
        else:    
            leftSec = self.wingSections[index - 1] 
        try:
            rightSec = self.wingSections[index + 1] 
        except: 
            rightSec = None
        return leftSec, rightSec
    

    def getNeighbourSectionsHavingAirfoil (self, aSection: 'WingSection'):
        """
        returns the neighbour before and after a wingSection, which a existing, real
         airfoil. Needed for 'strak'  - if no neighbour return None 
        """
        leftSec :  WingSection = None
        RightSec : WingSection = None

        try:
            index = self.wingSections.index (aSection) 
        except: 
            return None, None

        sec: WingSection
        #left neighbour 
        if aSection.isRoot: leftSec = None
        else: 
            for sec in reversed(self.wingSections[0:index]):
                if sec.airfoil.isExisting:
                    leftSec = sec
                    break 
        #right neighbour 
        if aSection.isTip: rightSec = None
        else: 
            for sec in self.wingSections[index+1: ]:
                if sec.airfoil.isExisting:
                    rightSec = sec
                    break 
        return leftSec, rightSec
    


    def do_strak (self): 
        """
        straks the airfoil of all wing sections having a Strak-Airfoil which is 
        created by blending with its real neighbours
        """
        sec: WingSection

        for sec in self.wingSections:
            if sec.airfoil.isStrakAirfoil: 
                leftSec, rightSec = self.getNeighbourSectionsHavingAirfoil(sec) 

                blendBy  = (sec.norm_chord - leftSec. norm_chord) / \
                           (rightSec.norm_chord - leftSec. norm_chord)

                sec.airfoil.do_strak(leftSec.airfoil,  rightSec.airfoil, blendBy)


    def do_export_airfoils (self,toDir, useNick=True, teGap_mm = None): 
        """
        exports all also straked airfoils into directory 'toDir'. 
        Optionally use airfoils Nickname as new airfoil name.
        Optionally a te gap in mm can be set for all exported airfoils"""
        fileList  = []
        
        self.do_strak() 

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
            if (section.flapGroup != sectionStart.flapGroup or \
                section == self.wingSections[-1]) :
                # new flap group or last section -> create flap 
                flapList.append(Flap(self, sectionStart, section))
                sectionStart = section 

        return flapList
    



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

    #    Wing sections may only be defined by user either by position or by chord
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
            InfoMsg (' ' + str(self)  + ' created')

    def __repr__(self) -> str:
        # overwrite class method to get a nice print string of self 
        return f"{type(self).__name__}"


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
            ErrorMsg("Planform_Class for planformType '%s' not found - using 'Bezier'" %planformType)
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
    def rootchord (self):       return self.wing.rootchord
    @property
    def tipchord (self) :       return self.wing.tipchord
    @property
    def halfwingspan (self):    return self.wing.halfwingspan
    @property
    def hingeAngle(self):       return self.wing.hingeAngle
    @property
    def flapDepthRoot(self):    return self.wing.flapDepthRoot
    @property
    def flapDepthTip(self):     return self.wing.flapDepthTip
    @property
    def isValid (self):         return self._isValid         # to overwrite


    def _save (self, dataDict):
        """ stores the variables into the dataDict"""
        # to be overloaded
        pass

    def _norm_y_points (self):
        """ array of y points along the spann for the different lines  """
        return np.sin(np.linspace(0.0, 1.0, num=50) * np.pi/2)

    def hingeLine (self):
        """ Returns the hinge line based on defined hinge line angle 
        :y:  array of the y-stations of the line (root & tip) :)
        :hinge: array of hinge x values :)
        """
        y = [0.0, self.halfwingspan]
        x = [self.hingePointAt(0), self.hingePointAt(self.halfwingspan)]
        return y, x


    def hingePointAt (self, y):
        """  Returns the x-coordinate of the hinge line at y

        Arguments:
            y --  y-coordinate of interest 
         """        
        xHingeAtRoot = (1-(self.flapDepthRoot/100)) * self.rootchord
        x = xHingeAtRoot + np.tan((self.hingeAngle/180) * np.pi) * y
        return x


    def norm_chord_function (self, y_norm, fast=False):
        """
        abstract: returns the normalized chord at position yPos 
        """
        return 0 
    

    def chord_function (self, yPos, fast=False):
        """
        returns the chord at position yPos 
        """
        return self.norm_chord_function (yPos/self.halfwingspan, fast=fast) * self.rootchord 
    

    def norm_chord_line (self):
        """
        the normalized chord distribution along the span
        Returns:
            :y: array of the y-stations of the lines 
            :chord: array of chord values 
        """
        y       = self._norm_y_points() 
        chord   = np.empty (y.size)
        for i in range(y.size): chord[i] = self.norm_chord_function (y[i])
        return y, chord


    def _planform_function (self, y):
        """
        implemented in subclass
        """
        leadingEdge = 0.0
        trailingEdge = 0.0

        return leadingEdge, trailingEdge


    def lines (self):
        """
        returns the major lines leading and trailing edge as arrays  
        Returns:
            :y: array of the spanwise y-stations of lines 
            :leadingEdge:  array of x coordinates (chord direction)
            :trailingEdge: array of x coordinates (chord direction)
        """
        y = self._norm_y_points() * self.halfwingspan
        leadingEdge  = np.empty (y.size)
        trailingEdge = np.empty (y.size)
        for i in range(y.size): 
            leadingEdge[i], trailingEdge[i] = self._planform_function (y[i])

        return y, leadingEdge, trailingEdge

    def linesPolygon (self):
        """
        returns the major lines leading and trailing edge as a closed polygon   
        Returns:
            :outline_y: array of the spanwise y-stations of lines 
            :outline_x: array of x coordinates (chord direction)
        """

        y, leadingEdge, trailingEdge = self.lines()

        # append lines and nose point to close the polygon 
        outline_y = np.append(y, np.flip(y))
        outline_y = np.append(outline_y, [y[0]])
        
        outline_x = np.append(leadingEdge, np.flip(trailingEdge))
        outline_x = np.append(outline_x, [leadingEdge[0]])

        return outline_y, outline_x
    

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
        xFlapLine.append(self._planform_function (fromY)[1])
        # go to hinge point
        yFlapLine.append(fromY)
        xFlapLine.append(self.hingePointAt(fromY))
        # go along the hinge 
        yFlapLine.append(toY)
        xFlapLine.append(self.hingePointAt(toY))
        # back to TE 
        yFlapLine.append(toY)
        xFlapLine.append(self._planform_function (toY)[1])
        # ... finally along TE to starting point back to TE
        y, le, te = self.lines()
        i1 = min(bisect.bisect(y, fromY)-1, len(y) -2)
        i2 = min(bisect.bisect(y, toY)  -1, len(y) -2)
        yFlapLine.extend (np.flip(y [i1:i2]))  
        xFlapLine.extend (np.flip(te[i1:i2]))

        return yFlapLine, xFlapLine
    
    def flapDepthAt (self, yPos):
        """ returns the normed flap depth at position yPos
        """
        xHinge   = self.hingePointAt(yPos)
        xLE, xTE = self._planform_function (yPos)
        return (xTE - xHinge) / (xTE-xLE)

    
    def flapLineAt (self, yPos):
        """ returns a section line (from TE to hinge point) at position yPos
        """
        yFlapLine = []
        xFlapLine = [] 
        # we start a TE at fromY and going clockwise around the flap upto toY
        yFlapLine.append(yPos)
        xFlapLine.append(self._planform_function (yPos)[1])
        # go to hinge point
        yFlapLine.append(yPos)
        xFlapLine.append(self.hingePointAt(yPos))
        return yFlapLine, xFlapLine


    def find_yPosFromChord (self, chord):
        """
        calculates the y-Position from a chord length 
        Returns:
            :y:   
        """

        # some kind of bubble search is performed until accuracy is better than epsilon
        # Should be overloaded for more precision 

        epsilon = 0.0001                           # normalized chord accuracy 
        myChord = chord / self.rootchord            # normalize 

        if(myChord > 1.0 or myChord < 0): 
            ErrorMsg ("Chord %f must be between 0.0 and root chord" % chord)
            return

        y       = None
        yLeft   = 0 
        yRight  = 1
        chordLeft   = self.norm_chord_function (yLeft,  fast=False)
        chordRight  = self.norm_chord_function (yRight, fast=False)
        
        while y == None:
            if   (abs (myChord - chordLeft)  < epsilon):
                y = yLeft 
                break
            elif (abs (myChord - chordRight) < epsilon):
                y = yRight
                break
            yMiddle = (yLeft + yRight) / 2 
            chordMiddle = self.norm_chord_function (yMiddle, fast=False)
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
        """ refresh planform if wing parameters e.g. tipchord have changed"""
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
        self._px[1]   = fromDict (dataDict, "p1x", 1.00, False)
        self._px[2]   = fromDict (dataDict, "p2x", 0.55, False)     # nearly elliptic
        self._py[1]   = fromDict (dataDict, "p1y", 0.55, False)
        self._px[3]   = self.tipchord / self.rootchord              # p3 sits on tip chord

        self._bezier = Bezier (self._py, self._px)


        # init Quadratic Bezier for banana of leading edge  
       
        self._banana_px = [0.0, 0.0, 0.0]                           # wing coordinate system 
        self._banana_py = [0.0, 0.5, 1.0]
        # from dict only the variable point coordinates of Bezier
        self._banana_px[1]   = fromDict (dataDict, "banana_p1x", 0.0, False)
        self._banana_py[1]   = fromDict (dataDict, "banana_p1y", 0.5, False)

        self._banana = None                                         # init see _norm_banana_function 



    def _save (self, dataDict):
        """ stores the variables into the dataDict"""

        # the flex points of chord Bezier curve 
        toDict (dataDict, "p1x", self._px[1]) 
        toDict (dataDict, "p2x", self._px[2]) 
        toDict (dataDict, "p1y", self._py[1]) 


    # ---Properties --------------------- 

    # Bezier free definition points for chord distribution

    @property                                   # root tangent 
    def p1x(self): return self._px[1]
    def set_p1x(self, aVal): 
        self._px[1] = min (aVal, self._px[0])      # The angle may not become negative as chord value won't be unique!
        self._bezier.set_points (self._py, self._px)
    @property
    def p1y(self): return self._py[1]
    def set_p1y(self, aVal): 
        self._py[1] = aVal 
        self._bezier.set_points (self._py, self._px)

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
    @property                                   
    def p3x(self): return self._px[3]
    def set_p3x(self, aVal): 
        self._px[3] = aVal 
        self._bezier.set_points (self._py, self._px)
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
        banana_x = norm_x * self.rootchord

        return y, banana_x


    def _norm_u_points (self):
        """ array of u (arc) points along the chord line  """
        return np.linspace(0.0, 1.0, num=100) 


    def norm_chord_line (self):
        """
        the normalized chord distribution along the span
        Returns:
            :y: array of the y-stations of the lines 
            :chord: array of chord values 
        """
        #
        # overloaded  - Bezier needs arc points u not y coordinates 
        #

        y, chord = self._bezier.eval (self._norm_u_points() )
        return y, chord

    def norm_chord_function (self, y_norm, fast=True):
        """
        Returns the normalized chord of the planform at y 0..1
        Normally a linear interpolation is done for fast evaulation (fast=True). 
        Higher Precision is achieved with interpolation of the curve (fast=False) 
        Args:
            :y_norm: the normalized y-Position in the planform 0..1
        Returns:
            :chord: the chord 0..1 at y
        """

        return self._bezier.eval_y_on_x (y_norm, fast=fast)        # wing coordinate system 
    
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


    def lines (self):
        """
        returns the major lines leading and trailing edge as arrays  
        Returns:
            :y: array of the spanwise y-stations of lines 
            :leadingEdge:  array of x coordinates (chord direction)
            :trailingEdge: array of x coordinates (chord direction)
        """

        # overloaded to optimize for Bezier 

        norm_y, norm_chord = self.norm_chord_line ()
        y = norm_y * self.halfwingspan
        chord = norm_chord * self.rootchord

        leadingEdge  = np.zeros (y.size)
        trailingEdge = np.zeros (y.size)
        for i in range(y.size): 
            leadingEdge[i], trailingEdge[i] = self._planform_function (y[i], chord=chord[i])

        return y, leadingEdge, trailingEdge
    

    def _planform_function (self, y, chord= None):
        """
        calculates all relevant geo data of the planform at y 

        Args:
            :y: the y-Position in the planform 0.. halfSpanWidth
            :chord: chord length at y - optional - for optimization 
        Returns:
            :leadingEdge:  ...-point at y
            :trailingEdge: ...-point at y
        """

        # chord-length at y
        y_norm = y / self.halfwingspan
        if chord is None: 
            chord =  self.norm_chord_function (y_norm) * self.rootchord 

        # calculate hingeDepth in percent at this particular point along the wing
        hingeRootx   = (1-(self.flapDepthRoot/100)) * self.rootchord
        hingeTipx    = hingeRootx + np.tan((self.hingeAngle/180) * np.pi) * self.halfwingspan
        hingeDepth_y = interpolate(0.0, self.halfwingspan, self.flapDepthRoot, self.flapDepthTip,y)

        # apply banana (bending of planform) at flapDepth
        flapDepth = (hingeDepth_y/100) * chord + \
                    self._norm_banana_function (y_norm) * self.rootchord 

        # finally the main "lines" at position y
        hinge        = (hingeTipx-hingeRootx)/(self.halfwingspan) * (y) + hingeRootx
        leadingEdge  = hinge - (chord - flapDepth)
        trailingEdge = leadingEdge + chord

        return leadingEdge, trailingEdge
    

    
    def find_yPosFromChord (self, chord):
        """
        calculates the y-Position from a chord length 
        Returns:
            :y:   
        """
        
        # overloaded for Bezier 

        normChord = chord / self.wing.rootchord       # normalize 

        yPos = self._bezier.eval_x_on_y (normChord, fast=False) * self.wing.halfwingspan

        return yPos 


    
    def refresh (self): 
        """ refresh planform if wing parameters e.g. tipchord have changed"""

        # when a new tip chord is set, move the bezier points p2 and p3 accordingly 
        #    so length of the tangent at tip is nit changed 

        tangentLength = self.tangentLength_tip          # store current length

        self.set_p3x (self.tipchord / self.rootchord)
        self.set_p2x (self.p3x + tangentLength)



#-------------------------------------------------------------------------------

class Planform_Pure_Elliptical(Planform):
    """ 
    Defines the outline of a unmodified (more or less) elliptical planform for
    reference purposes
    """
    planformType  = "Pure elliptical"
    isTemplate    = False

    def __init__(self, myWing: Wing):
        super().__init__(myWing)
        """
        Args:
            :myWing: the wing object self belongs to  
        """
        

    def norm_chord_function (self, y_norm, fast=False):
        """
        Returns the normalized chord of an elliptical planform at y 0..1
        Args:
            :y_norm: the normalized y-Position in the planform 0..1
        Returns:
            :chord: the chord 0..1 at y
        """

        return np.sqrt(1.0-(y_norm  ** 2))


    def _planform_function (self, y):
        """
        calculates all relevant geo data of the planform at y 

        Args:
            :y: the y-Position in the planform 0.. halfSpanWidth
        Returns:
            :leadingEdge:  ...-point at y
            :trailingEdge: ...-point at y
        """
        hingeRootx = (1-(self.flapDepthRoot/100)) * self.rootchord
        hingeTipx  = hingeRootx +  \
                            np.tan((self.hingeAngle/180) * np.pi) * self.halfwingspan

        # chord-length at y
        y_norm = y / self.halfwingspan
        chord =  self.norm_chord_function (y_norm) * self.rootchord 

        # calculate hingeDepth in percent at this particular point along the wing
        hingeDepth_y = interpolate(0.0, self.halfwingspan,
                                    self.flapDepthRoot, self.flapDepthTip,y)

        flapDepth = (hingeDepth_y/100) * chord 

        # finally the main "lines" at position y
        hinge        = (hingeTipx-hingeRootx)/(self.halfwingspan) * (y) + hingeRootx
        leadingEdge  = hinge - (chord - flapDepth)
        trailingEdge = leadingEdge + chord

        return leadingEdge, trailingEdge




#-------------------------------------------------------------------------------

class Planform_Bezier_StraightTE (Planform_Bezier):
    """ 
    Defines the outline of a Bezier based planform with straight trailing edge like Amokka 
    """

    shortDescription = "Planform with a straight TE based on a Bezier curve function.\n" + \
                       "Therefore either position or chord of a section can be defined."
        
    planformType  = "Bezier TE straight"
    isTemplate    = True                        # user may make mods 

    def _norm_banana_function (self, y_norm):
        """
        Returns the value of the "banana curve" bending the planform 
        Args:
            :y_norm: the normalized y-Position in the planform 0..1
        Returns:
            :banana_x: the value 0..1 at y
        """

        # overloaded - straight TE doesn't allow banana
        return 0.0
    

    def _planform_function (self, y, chord= None):
        """
        calculates LE and TE of the planform at y 

        Args:
            :y: the y-Position in the planform 0.. halfSpanWidth
        Returns:
            :leadingEdge:  ...-point at y
            :trailingEdge: ...-point at y
        """
        hingeRootx = (1-(self.flapDepthRoot/100)) * self.rootchord
        teRootx    = self.rootchord

        # with a straight te there is the problem that the flap depth a tip 
        # has to be very high that the flap depth at y= 0.8 or so won't be too small
        # there go "close to tip" as reference 

        closeToTip        = 0.90
        teCloseToTipy     = self.halfwingspan * closeToTip
        hingeCloseToTipx  = hingeRootx +  \
                            np.tan((self.hingeAngle/180) * np.pi) * teCloseToTipy
        chordCloseToTip   = self.norm_chord_function (closeToTip) * self.rootchord
        teCloseToTipx     = hingeCloseToTipx + (self.flapDepthTip/100) *  chordCloseToTip

        # chord-length at y
        if chord is None: 
            y_norm = y / self.halfwingspan
            chord =  self.norm_chord_function (y_norm) * self.rootchord 

        # trailingEdge = interpolate(0.0, self.halfwingspan, teRootx, teTipx, y)
        trailingEdge = interpolate(0.0, teCloseToTipy, teRootx, teCloseToTipx, y)
        leadingEdge  = trailingEdge - chord

        return leadingEdge, trailingEdge



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
                       "which have a defined position and chord." 

    def __init__(self, myWing: Wing, dataDict: dict = None):
        super().__init__(myWing, dataDict)
        """
        Args:
            :myWing: the wing object self belongs to  
            :dataDict: optional - dictonary with all the data for a valid section
        """
        # read additional parameters of this shapeform 
        self._leCorrection  = fromDict (dataDict, "leCorrection", 0, False)


    def _save (self, dataDict):
        """ stores the variables into the dataDict"""

        toDict (dataDict, "leCorrection", self._leCorrection) 


    def _norm_y_points (self):
        """
        array of y points along the spann for the different lines   
        """
        # just the sections yPos is needed (sections with yPos and chord defined)
        norm_y_points, chords = self.wing.get_wingSections_having_normChord () 
        return np.asarray(norm_y_points)


    def norm_chord_function (self, y_norm, fast=False):
        """
        Returns the normalized chord of the planform at y 0..1.
        The planform is defined by the wing sections having a position and a chord value.
        With these a multi trapezoid is defined
        Args:
            :y_norm: the y-Position in the planform 0..1
        Returns:
            :chord: the chord 0..1 at y
        """

        y_chords, chords = self.wing.get_wingSections_having_normChord ()   # tuple y_norm, c_norm

        for iSec in range(len(y_chords) - 1):
            y_before = y_chords [iSec]
            y_after  = y_chords [iSec+1]
            if y_norm  >= y_before and  y_norm  <= y_after:
                chord_before = chords[iSec]
                chord_after  = chords[iSec+1]
                return interpolate(y_before, y_after, chord_before, chord_after, y_norm) 

        # not found ...? 
        raise ValueError ("Could not interpolate for trapezoid the value ", y_norm)


    def _planform_function (self, y):
        """
        calculates LE and TE of the planform at y 

        Args:
            :y: the y-Position in the planform 0.. halfSpanWidth
        Returns:
            :leadingEdge:  ...-point at y
            :trailingEdge: ...-point at y
        """
        hingeRootx = (1-(self.flapDepthRoot/100)) * self.rootchord
        # calculate hingeTipx from hinge line angle
        hingeTipx = hingeRootx +  \
                            np.tan((self.hingeAngle/180) * np.pi) * self.halfwingspan

        # chord-length at y
        y_norm = y / self.halfwingspan
        chord =  self.norm_chord_function (y_norm) * self.rootchord 

        # calculate hingeDepth in percent at this particular point along the wing
        hingeDepth_y = interpolate(0.0, self.halfwingspan,
                                    self.flapDepthRoot, self.flapDepthTip,y)

        flapDepth = (hingeDepth_y/100) * chord 

        # finally the main "lines" at position y
        hinge        = (hingeTipx-hingeRootx)/(self.halfwingspan) * (y) + hingeRootx
        leadingEdge  = hinge - (chord - flapDepth)
        trailingEdge = leadingEdge + chord

        return leadingEdge, trailingEdge
    
    def adjust_planform_to_reference (self):
        """
        Adjust the relevant wing sections of trapezoid to become close to reference(ellipse)
        """
         
        yPosList, chordList = self.wing.get_wingSections_having_normChord()

        for index, yPos in enumerate(yPosList):        # skip root and tip 
            if not (index == 0 or index == (len(yPosList)-1)): 
                # go a little left to get chord so ellipsoid is better approx.
                leftPos = yPosList [index-1]
                refPos = leftPos + 0.95 *  (yPos - leftPos)
                newChord = self.wing.refPlanform.norm_chord_function(refPos)
                self.wing.set_wingSection_norm_chord(yPos, newChord)


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
        self._x_panels    = fromDict (dataDict, "x-panels", 10, False)
        self._x_dist      = fromDict (dataDict, "x-distribution", "cosine", False)
        self._y_panels    = fromDict (dataDict, "y-panels", 10, False)
        self._y_dist      = fromDict (dataDict, "y-distribution", "uniform", False)
        self._y_minWidth  = fromDict (dataDict, "y-minWidth", 20 , False)
        self._minTipChord = fromDict (dataDict, "minTipChord", 30 , False)

        self._yPosList    = None                # cache wing section data 
        self._chordList   = None 
    

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
        toDict (dataDict, "minTipChord",    self.minTipChord) 


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
    def minTipChord (self):             
        """ minimum chord at tip when generating panels"""
        return self._minTipChord
    
    def set_minTipChord (self, val): 

        if not val is None: 
            _, chordList = self.wing.get_wingSections_yPos_chord() 
            # minTipChord must be less than 90% wing section chord before tip  
            val = min (val, chordList[-2] * 0.9)  
            # ... more than tip chord  
            val = max (val, self.wing.tipchord)  
        self._minTipChord = val
        self._yPosList    = None                # reset cache wing section data 
        self._chordList   = None 

    @property
    def isTipCutted (self):                     
        """ is tip cutted due to minTipChord?""" 
        return self._calc_reducedTipPos() != self.halfwingspan

    # --- Methods --------------------- 

    def _calc_reducedTipPos (self):
        """ 
        returns a reduced tip y position if tipChord is smaller than minTipCord
         - other tip y position remains halfwingspan 
        """

        minTipChord = self._minTipChord
        newTipPos   = self.halfwingspan

        # sanity check: is minTipChord between chord at tip and chord tip-1

        yPosList, chordList = self.wing.get_wingSections_yPos_chord()
        if chordList[-1] > minTipChord or chordList[-2] < minTipChord: return self.halfwingspan

        # find the y position having minTipChord 
        firstGuess = (yPosList[-1] + yPosList[-2]) / 2        # in the middle 
        bounds     = (yPosList[-2], yPosList[-1])             # between the last sections 
        fn         = lambda y : self.wing.planform.chord_function(y) - minTipChord
        newTipPos = findRoot (fn, firstGuess , no_improve_thr=10e-5, bounds=bounds) 

        if newTipPos is None: 
            return self.halfwingspan
        else: 
            return newTipPos


    def _sections_yPos_chord (self):
        """ returns yPos and chord of all sections as two lists"""

        if self._yPosList is None:                      # cache values 

            yPosList, chordList = self.wing.get_wingSections_yPos_chord()

            # is there a new tip yPos because of minimum tip chord? 
            if self.isTipCutted:
                reducedTipPos = self._calc_reducedTipPos()
                yPosList[-1]  = reducedTipPos                        # set "new" tip 
                chordList[-1] = self.wing.planform.chord_function(reducedTipPos)
            
            self._yPosList = yPosList
            self._chordList = chordList 

        return self._yPosList, self._chordList
    

    def y_panels_forSection(self, iSec):
        """y-panels for section i depending on min panel width"""

        sections_yPos = self._sections_yPos_chord() [0]

        if iSec < len(sections_yPos)-1:
            y_left = sections_yPos [iSec]
            y_right  = sections_yPos [iSec+1]
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


    def _planform_function (self, y):
        """
        the planform represented as trapezoid
        """

        sections_yPos = self._sections_yPos_chord() [0]

        for iSec in range(len(sections_yPos) - 1):
            y_before = sections_yPos [iSec]
            y_after  = sections_yPos [iSec+1]
            if y  >= y_before and  y <= y_after:
                le_before, te_before = self.wing.planform._planform_function (y_before)
                le_after, te_after   = self.wing.planform._planform_function (y_after)
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
        sections_yPos = self._sections_yPos_chord() [0]

        # get le te for all sections 
        sections_le_te = []
        for y_sec in sections_yPos: 
            # take actual planform .. function (maybe faster) 
            sections_le_te.append(self.wing.planform._planform_function (y_sec))

        # now calc a y line according to y distribution function between sections 
        for iSec in range(len(sections_yPos) - 1):
            y_left   = sections_yPos [iSec]
            y_right  = sections_yPos [iSec+1]
            dy_sec   = y_right - y_left
            le_left  = sections_le_te[iSec][0]
            te_left  = sections_le_te[iSec][1]
            le_right = sections_le_te[iSec+1][0]
            te_right = sections_le_te[iSec+1][1]

            # assure a min panel width in y-direction 
            y_panels = self.y_panels_forSection(iSec)

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
                    le,te  =  self.wing.planform._planform_function (yPos) 
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
        sections_yPos = self._sections_yPos_chord() [0]

        # get le te for all sections 
        sections_le_te = []
        for y_sec in sections_yPos: 
            sections_le_te.append(self.wing.planform._planform_function (y_sec))


        # now calc a x horizontal line according to x distribution function between sections 
        for iSec in range(len(sections_yPos) - 1):
            y_left = sections_yPos [iSec]
            y_right  = sections_yPos [iSec+1]

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

        self._dxfMirrorX       = fromDict (dataDict, "dxfMirrorX", True, msg=False)   

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
                ErrorMsg ("The dxf file '%s' doesn't exist" % self._dxfPathFilename)
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
                ErrorMsg ("DXF file \'%s\' does not exist" % loadPathFile)
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
    
    def lines (self):
        # self can be empty
        if not self.isValid: return [],[],[]
        return super().lines()


    def norm_chord_function (self, y_norm, fast=False):
        """
        Returns the normalized chord of the planform at y 0..1
        Args:
            :y_norm: the normalized y-Position in the planform 0..1
        Returns:
            :chord: the chord 0..1 at y
        """
        le = self.__get_xFromY(self.le_norm_dxf, y_norm)
        te = self.__get_xFromY(self.te_norm_dxf, y_norm)

        if (le == None) or (te == None):
            ErrorMsg("DXF import: y-coordinate not found, planform could not be created")
            return None
        else:
            return abs(te-le)


    def _planform_function (self, y):
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

        hingeRootx = (1-(self.flapDepthRoot/100)) * self.rootchord
        hingeTipx = hingeRootx +  \
                            np.tan((self.hingeAngle/180) * np.pi) * self.halfwingspan

        # chord-length at y
        y_norm = y / self.halfwingspan
        chord =  self.norm_chord_function (y_norm) * self.rootchord 

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
            ErrorMsg("DXF import: y-coordinate not found, planform could not be created")
            return 0.0, 0.0

        le = le_norm * self.rootchord
        te = te_norm * self.rootchord

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
            hinge[i] = self.hingeLine_norm_dxf[i][1] * self.rootchord

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
            ErrorMsg("__get_yFromX, xcoordinate %f not found" % x)
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
                self.wing.set_hingeAngle    (self.hingeAngle_dxf)
                self.wing.set_flapDepthRoot (self.flapDepthRoot_dxf)
                self.wing.set_flapDepthTip  (self.flapDepthTip_dxf)


    def load_dxf (self, dxf_file):
        """ parse and load a dxf planform from file"""

        from dxf_utils import import_fromDXF

        infoText = []
        self.le_norm_dxf, self.te_norm_dxf, self.hingeLine_norm_dxf, self.hingeAngle_dxf = \
            import_fromDXF(dxf_file)

        # check result
        if self.le_norm_dxf != None:
            InfoMsg("DXF planform imported from file %s" % dxf_file)

            infoText.append(" - Leading edge %d points"  % (len(self.le_norm_dxf)))
            infoText.append(" - Trailing edge %d points" % (len(self.te_norm_dxf)))

            if (self._dxfMirrorX):
                infoText.append(" - mirrored along y-axis")
                InfoMsg("Mirroring DXF planform for LE showing upwards")
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
            ErrorMsg("DXF import from file %s failed" % dxf_file)
            self.infoText = "DXF file couldn't be loaded"



#-------------------------------------------------------------------------------
# WingSection  
#-------------------------------------------------------------------------------
class WingSection:
    """ 
    A certain station of wing. Is defined by its Re or fixed by y pos
    There is always a root section (index 0) and a tip section (last index)   

    Wing
       |-- WingSection
             |-- airfoil
    """
    def __init__(self, myWing: Wing, dataDict: dict = None):
        """
        Main constructor for new section belonging to a wing 

        Args:
            :myWing: the wing object self belongs to  
            :dataDict: optional - dictonary with all the data for a valid section
        """

        # get initial data from dict 
        self.wing : Wing = myWing

        self._yPos       = fromDict (dataDict, "position", None)
        self._norm_chord = fromDict (dataDict, "norm_chord", None)
        self._flapGroup  = fromDict (dataDict, "flapGroup", 1, False)

        self.isRoot = (self._yPos == 0.0) 
        self.isTip  = (self._yPos == self.wing.halfwingspan)

        if self.isRoot:
            self._norm_chord = 1.0
 
        if (self._yPos is None and self._norm_chord is None):
            ErrorMsg ("Wing section: Either position or chord / reynolds must be set")
            NoteMsg  ("Wing section: Setting chord to 0.8 of root")
            self._norm_chord    = self.wing.rootchord * 0.8

        if self.eitherPosOrChord():
            # either position or chord should be flexibel if planform defines sections
            if (not self._yPos is None) and (not self._norm_chord is None) and (not self.isRootOrTip):
                self._norm_chord = None
        else:
            # for trapezoid both position and chord must have a value 
            if (self._norm_chord is None) and (not self.isRootOrTip):
                self._norm_chord = self.norm_chord
            if (self._yPos is None) and (not self.isRootOrTip):
                self._yPos = self.yPos


        # create airfoil and load coordinates if exist 
        self._init_airfoil (dataDict = fromDict (dataDict, "airfoil", None))


    def _save (self, sectionDict):
        """ stores the variables into the dataDict - returns the filled dict"""

        toDict (sectionDict, "position",    self._yPos) 
        toDict (sectionDict, "norm_chord",  self._norm_chord) 
        toDict (sectionDict, "flapGroup",   self._flapGroup) 

        airfoilDict = self.airfoil._save ({})
        toDict (sectionDict, "airfoil",  airfoilDict) 

        return sectionDict


    def _init_airfoil (self, dataDict = None, pathFileName = None, workingDir=None):
        """create airfoil for section """
 
        # read data for airfoil either from dict of parameter file 
        #  or get filepath from user selection 
        # then create new 'Airfoil' 

        if dataDict: 
            airfoil = Airfoil.onDict (dataDict, workingDir= self.wing.pathHandler.workingDir)
        else: 
            airfoil = Airfoil (pathFileName= pathFileName, workingDir=workingDir)

        if airfoil.isExisting:
            airfoil.load()
        else:
            if self.isRoot: 
                airfoil = Root_Example()
            elif self.isTip:
                airfoil = Tip_Example()
            else:
                airfoil = Airfoil(name="<strak>")
                airfoil.set_isStrakAirfoil (True)

        self.airfoil = airfoil 


    def __repr__(self) -> str:
        # overwrite class method to get a nice print string of self 
        if (self._yPos != None):
            info = f"at Pos={self.yPos}mm"
        else:
            info = "flex with normalized chord=%.2f" % self.norm_chord
        return f"{type(self).__name__} {info}"

    #-----------------------------------------------------------

    @property
    def yPos (self):
        """
        y-position of wing section - either from fix position or calculated from chord position
        """
        if (self._yPos != None):
            pos =  self._yPos
        elif (self.isRoot):                     # enforce position 
            pos = 0.0
        elif (self.isTip):                      # enforce position
            pos = self.wing.halfwingspan
        elif (self._norm_chord != None): 
            pos = self.wing.planform.find_yPosFromChord (self._norm_chord * self.wing.rootchord) 
        else:
            pos = None
        return pos 

    def set_yPos (self, value):
        """
        set y-position of wing section to a fixed value - removing rel. chord setting
        """
        if (value == None):
            self._yPos = None
        elif (value < 0.0 or value > self.wing.halfwingspan):
            ErrorMsg ("wingSection: Position must be inside half wingspan %dmm" %self.wing.halfwingspan)
        elif (self.isRoot or self.isTip):
            ErrorMsg ("wingSection: Do not set root or tip chord via wing section")
        else:
            self._yPos = value
            if self.eitherPosOrChord():
                self._norm_chord  = None             # can't have fix position *and* relative chord setting
            else: 
                self._norm_chord  = self.norm_chord  # also fix chord (trapezoid)

    @property
    def norm_yPos (self):
        """ normed y-position of wing section 0..1
        """
        return self.yPos / self.wing.halfwingspan

    def set_norm_yPos (self, value):
        """
        set normed y-position of wing section 0..1 - removing rel. chord setting
        """
        self.set_yPos (value * self.wing.halfwingspan)


    @property
    def norm_chord (self):
        """ normalized chord 0..1 - either from property or calculated from position  """
        if   (not self._norm_chord is None): return self._norm_chord
        elif (not self._yPos is None): 
            return self.wing.planform.norm_chord_function (self.norm_yPos) 
        else:
            raise ValueError ("Wingsection: Both position and norm chord are not defined")

    def set_norm_chord (self, value):
        """ set the normaized chord 0..1 - the section will move to a new position   """
        if (value is None and self._yPos is None):
            ErrorMsg ("wingSection: Can't remove normalized chord. Either position or chord must be defined")
        elif(value < 0.0):
            ErrorMsg ("wingSection: Normalized chord must => 0")
        elif(value > 1.0):
            ErrorMsg ("wingSection: Normalized chord must <= 1")
        elif (self.isRoot or self.isTip):
            ErrorMsg ("wingSection: Do not set root or tip chord via wing section")
        else:
            self._norm_chord = value
            if self.eitherPosOrChord():
                self._yPos       = None         # remove other values, section is flex
            else: 
                self._yPos       = self.yPos    # also fix position (trapezoid)

    @property
    def chord (self):
        """ chord 0..rootChord - calculated from normalized chord """
        return self.norm_chord * self.wing.rootchord

    def set_chord (self, value):
        """ set the chord 0..rootchord - set to normalized chord  """
        self.set_norm_chord (value / self.wing.rootchord)

    @property
    def flapGroup (self):
        """ flap group beginning with this section """
        return self._flapGroup
    def set_flapGroup (self, value):
        self._flapGroup = value

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

    # ---Methods --------------------- 

    def eitherPosOrChord (self): 
        """ self may only be defined by user either by position or by chord.
        Is defined by planform typ. Within a trapezoidal a section may have both   """
        return self.wing.wingSections_eitherPosOrChord()

    def isReDisabled (self):
        """ true if Re calculation is not possible  - e.g. missing Re at root"""
        return (self.wing.rootRe is None or self.wing.rootRe <= 0) or (self.isRootOrTip)

    def name (self) -> str:
        """ very short unique name for wing section like "Section 7"""
        if self.isRoot:
            info = "Root"
        elif self.isTip:
            info = "Tip"
        else:
            info = "Section %s" % self.wing.wingSectionIndexOf (self)
        return info

    
    def label (self) -> str:
        """ short unique label for wing section like "7: at 1240mm """
        index = self.wing.wingSectionIndexOf (self)
        if self.isRoot:
            info = "Root"
        elif self.isTip:
            info = "Tip"
        elif (self._yPos != None):
            info = "at %.0fmm" %self._yPos
        elif (self._norm_chord != None):
            info = "with %.0fmm" % (self._norm_chord * self.wing.rootchord)
        elif (self.Re != None):
            info = "Re %.0f" %self.Re
        else:
            info = "flex" 
        return f"{index}: {info}"


    def adjustToWing (self, oldSpan = None):
        """ takes care of a modified wing main data - correct position, chord etc 
        :Args: oldSpam the wing span before modification
        """
        if self.isTip:
            self._yPos  = self.wing.halfwingspan  
            self._norm_chord = self.wing.tipchord / self.wing.rootchord
        elif self.isRoot:
            self._yPos = 0.0
            self._norm_chord = 1.0 
        elif self._yPos: 
            if oldSpan:
                self._yPos = self._yPos * self.wing.wingspan / oldSpan  

 
    def hasFixedPosition (self):
        """ has wing section a fixed position within wing?
        """
        return (not self._yPos is None)


    def fixChordAndPosition (self):
        """ sets the chord and yPos (for trapezoid planform at creation)
        """
        if self._yPos is None:   
            self._yPos = self.yPos                  # write calculated value fix into variable    
        if self._norm_chord is None: 
            self._norm_chord = self.norm_chord      # write calculated value fix into variable                
        return 
    

    def releaseFixedChordWithPosition (self):
        """ removes a fixed chord when position is defined
        (for not trapezoid planform at creation)
        """
        if ( self._yPos is None and self._norm_chord is None):
            raise ValueError ("Wingsection: Both position and norm chord are not defined")
        self._norm_chord = None      # remove fixed 
        return 


    def hasFixedPositionAndChord (self):
        """ has wing section a fixed position and chord within wing? (for trapezoid planform)
        """
        return (not self._yPos is None) and (not self._norm_chord is None)
    

    def limits_yPos (self): 
        """ position limits as tuple of self before touching the neighbour section
        """
        leftSec  : WingSection = None
        rightSec : WingSection = None


        leftSec, rightSec = self.wing.getNeighbourSectionsOf (self) 

        if self.isRoot or self.isTip:
            safety = 0.0                # = fixed
            leftLimit  = self.yPos
            rightLimit = self.yPos
        else:
            safety = int (self.wing.halfwingspan / 500.0) 
            if leftSec: 
                leftLimit = leftSec.yPos
            else:
                leftLimit = self.yPos
            if rightSec: 
                rightLimit = rightSec.yPos
            else:
                rightLimit = self.yPos
        return (leftLimit + safety, rightLimit - safety)
    
    def limits_norm_yPos (self): 
        """ position limits as tuple of self before touching the neighbour section
        """
        left, right = self.limits_yPos()
        return ( left / self.wing.halfwingspan, right / self.wing.halfwingspan)
    
    def limits_normChord (self): 
        """ norm chord limits of self as tuple - must be between left and right neighbour
        """
        leftSec  : WingSection = None
        rightSec : WingSection = None

        
        leftSec, rightSec = self.wing.getNeighbourSectionsOf (self) 

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

    def limits_Chord (self): 
        """  chord limits of self as tuple - must be between left and right neighbour
        """
        left, right = self.limits_normChord ()
        left  = left  * self.wing.rootchord
        right = right * self.wing.rootchord
        return (left, right)


    def limits_Re (self): 
        """  Re limits of self as tuple - must be between left and right neighbour
        """
        left, right = self.limits_normChord ()
        left  = left  * self.wing.rootRe
        right = right * self.wing.rootRe
        return (left, right)

    def airfoilName (self):
        return self.airfoil.name
    
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

        filePathName = self.airfoil.copyAs(dir=toDir, destName = newName, teGap = teGap)

        return os.path.basename(filePathName) 
        
    
    def line (self):
        """
        the wing section as a line from LE to TE
        Returns:
            :y: array of the y-stations of the line  
            :x: array of x values of LE and TE 
        """
        le, te = self.wing.planform._planform_function (self.yPos)
        y = [self.yPos, self.yPos]
        x = [le, te]
        return y, x

    def norm_line (self):
        """
        the wing section as a normed line from LE to TE
        Returns:
            :y: array of the y-stations of the line  
            :x: array of x values of LE and TE 
        """
        le = self.wing.planform.norm_chord_function (self.norm_yPos)
        y = [self.norm_yPos, self.norm_yPos]
        x = [le, 0]
        return y, x

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

        self.flapGroup   = sectionLeft.flapGroup
        self.sectionName = sectionLeft.name()
        self.y , self.x = myWing.planform.flapPolygon (sectionLeft.yPos, sectionRight.yPos)
        self.depthLeft  = myWing.planform.flapDepthAt (sectionLeft.yPos)
        self.depthRight = myWing.planform.flapDepthAt (sectionRight.yPos)
        self.lineLeft   = myWing.planform.flapLineAt  (sectionLeft.yPos)
        self.lineRight  = myWing.planform.flapLineAt  (sectionRight.yPos)
         

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
        self._exportDir         = fromDict (myDict, "exportDir", "airfoils", msg=False)
        self._useNick           = fromDict (myDict, "useNick", True, msg=False)
        self._setTeGap          = fromDict (myDict, "setTeGap", False, msg=False)
        self._teGap_mm          = fromDict (myDict, "teGap_mm", 0.5, msg=False)


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

        InfoMsg ("Airfoils written to " + targetDir) 
        message = "Airfoils: \n\n" + \
                  ',  '.join(airfoilList)  + \
                  "\n\n exported to \n\n" + \
                  "'" +  targetDir + "'"      
        return message




#-------------------------------------------------------------------------------

# Main program for testing 
if __name__ == "__main__":

    print ("Current directory: ",os.getcwd())
    filename = "..\examples\Amokka-JX\Amokka-JX.json"
    # filename = ""
    myWing = Wing (filename)

