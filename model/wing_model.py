#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""  

Wing model with planform, wing sections, airfoils 

"""

import os
import numpy as np
from math import  sin
import json

from common_utils       import *
from airfoil_polar      import Airfoil, Strak_Airfoil
from airfoil_examples   import Root_Example, Tip_Example


# disables all print output to console
print_disabled = False

# user exceptions 
class Except_Planform_DXF_notValid (Exception):
    "Raised when an invalid Planform_DXF is assigned to the wing"
    pass



#-------------------------------------------------------------------------------
# wing  - the root class
#-------------------------------------------------------------------------------
class Wing:
    """ 
    Wing                            - main class of data model 
        |-- WingSection             - the various stations defined by user 
                |-- Airfoil         - the airfoil at an section
        |-- Planform                - describes geometry, outline of the wing  
        |-- Flap                    - the outline of a flap - dynamically created based on flap group
    """

    unit = 'mm'

    def __init__(self, dataDict):
        """
        Args:
            :dataDict: dictonary having all data nedded for a complete wing definition  
        """


        InfoMsg("Starting wing model ...")

        if dataDict is None:
            NoteMsg ('No input data - a default wing will be created')
            wingExists = False
        else: 
            wingExists = True


        self.dataDict = dataDict

        self._name            = fromDict (dataDict, "wingName", "My new Wing", wingExists)
        self._wingspan        = fromDict (dataDict, "wingspan", 2000.0, wingExists) 
        self._rootchord       = fromDict (dataDict, "rootchord", 200.0, wingExists)
        self._tipchord        = fromDict (dataDict, "tipchord", self._rootchord/4, wingExists)

        self._hingeAngle      = fromDict (dataDict, "hingeLineAngle", 0.0, wingExists)
        self._flapDepthRoot   = fromDict (dataDict, "flapDepthRoot", 25.0, wingExists)
        self._flapDepthTip    = fromDict (dataDict, "flapDepthTip", 25.0, wingExists)


        # attach the Planform 
        tmp_planformType      = fromDict (dataDict, "planformType", "elliptical", wingExists)
        self._planform        = None
        self.planform         = Planform.having (tmp_planformType, self, dataDict) # via setter

        self.wingSections     = self.createSectionsOn (fromDict(dataDict, "wingSections", None))
    
        # create an extra Planform as dxf reference  
        self.refPlanform_DXF  = Planform_DXF (self, dataDict, ref=True)
        
        # create an extra Planform as major reference ()
        self.refPlanform      = Planform_Pure_Elliptical (self)

        # will hold the class which manages Xflr5, FLZ export including its parameters
        self._xflr5Exporter   = None 
        self._flzExporter     = None 

        # miscellaneous parms
        self._rootRe            = fromDict (dataDict, "rootRe", 400000, wingExists)
        self._airfoilNickPrefix = fromDict (dataDict, "airfoilNickPrefix", "JX", msg=False)
        
        self._dxfPathFileName   = fromDict (dataDict, "dxfFile", "", msg=False)
        self._dxfAirfoilsTeGap  = fromDict (dataDict, "dxfAirfoilsTeGap", None, msg=False)
        self._dxfAirfoilsToo    = fromDict (dataDict, "dxfAirfoilsToo", True, msg=False)

        InfoMsg (str(self)  + ' created...')


    def __repr__(self) -> str:
        # overwrite to get a nice print string 
        return f"{type(self).__name__} \'{self.name}\'"


    @classmethod
    def onFile(cls, paramFilePath):
        """
        Alternate constructor for new wing based on a file  
        Args:
            :aFilePath: string of parameter file path
        """
        dataDict = None
        if (len(paramFilePath)):
            try:
                paramFile = open(paramFilePath)
                try:
                    dataDict = json.load(paramFile)
                    paramFile.close()
                except ValueError as e:
                    ErrorMsg('Invalid json: %s' % e)
                    paramFile.close()
                    dataDict = None
            except:
                ErrorMsg ("Input file %s not found" % paramFilePath)

        if (dataDict):
            InfoMsg ('Reading data from %s' % paramFilePath)

        return cls(dataDict)

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
        toDict (dataDict, "dxfFile",            self._dxfPathFileName) 

        toDict (dataDict, "planformType",       self.planform.planformType) 
        self.planform._save (dataDict)

        sectionsList = []
        for section in self.wingSections:
            sectionsList.append (section._save ({}))
        toDict (dataDict, "wingSections", sectionsList) 

        self.refPlanform_DXF._save (dataDict)

        toDict (dataDict, "xflr5",              self.xflr5Exporter._save()) 
        toDict (dataDict, "flz",                self.flzExporter._save()) 

        return dataDict

    # ---Properties --------------------- 

    @property
    def name(self):  return self._name
    def set_name(self, newName):  self._name = newName

    @property 
    def planform (self):
        return self._planform
    
    @planform.setter 
    def planform (self, newPlanform: 'Planform'): 

        # special treatment for dxf because it could be invalid 
        if newPlanform.is_dxf and not newPlanform.isValid:
            raise Except_Planform_DXF_notValid             # assure no corrupt data
        
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
        """
        set planformType - create a new planform object for this wing
        """
        if (self.planformType != newPlanformType):

            self.planform =  Planform.having (newPlanformType, self, self.dataDict)


    @property
    def wingspan(self): return self._wingspan
    def set_wingspan(self, newSpan):
        """
        set wingspan - update sections having fixed positions
        """
        if (newSpan > 10.0):
            oldSpan = self._wingspan
            self._wingspan = newSpan
            section : WingSection
            for section  in self.wingSections:    # all sections within new half wing span
                section.adjustToWing (oldSpan)

    @property
    def rootchord(self): 
        return self._rootchord
    def set_rootchord(self, newChord):
        """
        set rootchord - update first section with new chord  
        """
        if (newChord > 10.0):
            self._rootchord = newChord
            self.rootSection.adjustToWing()

    @property
    def tipchord(self): return self._tipchord
    def set_tipchord(self, newChord):
        """ set tipchord - update tip (last) section with new chord  
        """
        if (newChord > 1.0):
            self._tipchord = newChord
            self.tipSection.adjustToWing()

    @property
    def hingeAngle(self): return self._hingeAngle
    def set_hingeAngle(self, newAngle): self._hingeAngle = newAngle


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
    def dxfPathFileName(self):
        if self._dxfPathFileName:
            return self._dxfPathFileName
        else: 
            return self.name + ".dxf"
    def set_dxfPathFileName(self, newPathFilename):
        self._dxfPathFileName = os.path.normpath(newPathFilename)

    @property
    def dxfAirfoilsTeGap (self): return self._dxfAirfoilsTeGap
    def set_dxfAirfoilsTeGap (self, aVal): self._dxfAirfoilsTeGap = aVal 

    @property
    def dxfAirfoilsToo (self): return self._dxfAirfoilsToo
    def set_dxfAirfoilsToo (self, aVal): self._dxfAirfoilsToo = bool(aVal) 

    @property
    def xflr5Exporter (self): 
        """ returns class managing Xflr5 export """
        from export_Xflr5       import Export_Xflr5         # here - otherwise circular errors

        if self._xflr5Exporter is None: 
            # init class for Xlfr5 export with parameters in sub dict xlfr5 
            xflr5Dict           = fromDict (self.dataDict, "xlfr5", "", msg=False)
            self._xflr5Exporter = Export_Xflr5(self, xflr5Dict) 
        return self._xflr5Exporter     

    @property
    def flzExporter (self): 
        """ returns class managing FLZ export """
        from export_FLZ  import Export_FLZ         # here - otherwise circular errors

        if self._flzExporter is None: 
            # init class for Xlfr5 export with parameters in sub dict xlfr5 
            flzDict           = fromDict (self.dataDict, "xlfr5", "", msg=False)
            self._flzExporter = Export_FLZ(self, flzDict) 
        return self._flzExporter     



    # ---Methods --------------------- 


    def save (self, pathFileName):
        """ store data dict to file pathFileName
        
        :Returns: 
            0 : if succeded, -1 if failed"""

        try:
            paramFile = open(pathFileName, 'w')
        except:
            ErrorMsg("Failed to open file %s" % pathFileName)
            return -1

        currentDict = self._save()

        # save parameter dictionary to .json-file
        try:
            json.dump(currentDict, paramFile, indent=2, separators=(',', ':'))
            paramFile.close()
            InfoMsg ("Parameters saved to '%s'" % pathFileName)
            # store the actual dict to allow changed detection 
            self.dataDict = currentDict

        except ValueError as e:
            ErrorMsg('invalid json: %s' % e)
            ErrorMsg('Error, failed to save data to file %s' % pathFileName)
            paramFile.close()
            return -1
        return 0

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


    def get_wingSections_norm_chord (self):
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
                sec.airfoil.do_strak(sec.norm_chord, leftSec.airfoil,  leftSec. norm_chord,
                                                     rightSec.airfoil, rightSec.norm_chord)


    def do_export_airfoils (self,toDir, useNick=True): 
        """
        exports all also straked airfoils into directory 'toDir'. Optionally use airfoils Nickname
        as new airfoil name."""
        fileList  = []
        
        self.do_strak() 

        sec: WingSection
        for sec in self.wingSections:
            fileList.append (sec.do_export_airfoil (toDir, useNick=useNick))
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
    

    def export_toDxf (self):
        """exports self to a dxf file specified in the dxf parameters
          - returns a text string of exported artefacts """

        from .export_Dxf import Dxf_Artist

        dxf = Dxf_Artist(self)

        dxf.plot_planform()
        dxf.plot_hingeLine ()
        dxf.plot_wingSections ()
        dxf.plot_title ()

        if self.dxfAirfoilsToo:
            self.do_strak ()                    # ensure strak airfoils are uptodate 
            dxf.plot_airfoils (teGap_mm=self.dxfAirfoilsTeGap)

        pathFileName = "myDxfle.dxf"
        dxf.save (pathFileName)

        message = "Wing exported to '" + self.dxfPathFileName + "'"
        return message




#-------------------------------------------------------------------------------
# Planform  
#-------------------------------------------------------------------------------

class Planform:
    """ 
    Abstract super class - defines the outline of a wing based on parameters  

    Wing
       |-- Planform
                Planform_Elliptical
                Planform_Trapezoidal
                Planform_Pure_Elliptical        (for reference purposes)
                Planform_DXF                    (for reference purposes)
    """
    planformType  = "abstract"
    isTemplate    = False

    shortDescription = " Abstract - this is a short desciption of planform specialities"

    # is the planform defined by wing section or vice versa - overwrite 
    sections_depend_on_planform = True          # e.g.elliptical    
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
        InfoMsg (' ' + str(self)  + ' created...')

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
            :planformType: string like 'elliptical' 
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
            ErrorMsg("Planform_Class for planformType '%s' not found - using 'elliptical'" %planformType)
            planformClass = Planform_Elliptical

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
        return np.sin(np.linspace(0.0, 1.0, num=30) * np.pi/2)

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


    def norm_chord_function (self, y_norm):
        """
        abstract: returns the normalized chord at position yPos 
        """
        return 0 
    

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


    def flapPolygon  (self, fromY, toY, nPoints = 20):
        """
        returns an y,x polygon describing a flap between fromY and toY   

        Keyword Arguments:
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
        for y in np.linspace(toY, fromY, num=nPoints):
           yFlapLine.append(y)  
           xFlapLine.append(self._planform_function (y)[1])

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


    def calc_area_AR (self, y, le, te):
        """calculates (approximates) wing area and aspect ration AR based 
        on le and te points which are already calculated"""

        area = 0 
        # trapez verfahren 
        for i in range(len(y) -1): 
            area += (y[i+1] - y[i]) * ((te[i] - le[i]) + (te[i+1] - le[i+1])) / 2

        aspectRatio = 2 * self.halfwingspan **2 / area
        return area, aspectRatio


#-------------------------------------------------------------------------------

class Planform_Pure_Elliptical(Planform):
    """ 
    Defines the outline of a unmodified (more or less) elliptical planform for
    reference purposes
    """
    planformType  = "pure_elliptical"
    isTemplate    = False

    def __init__(self, myWing: Wing, dataDict: dict = None):
        super().__init__(myWing, dataDict)
        """
        Args:
            :myWing: the wing object self belongs to  
            :dataDict: optional - dictonary with all the data for a valid section
        """
        

    def norm_chord_function (self, y_norm):
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


class Planform_Elliptical(Planform):
    """ 
    Defines the outline of an elliptical planform  
    """
    planformType  = "elliptical"
    isTemplate    = True

    shortDescription = "Elliptical based planform generated by various functions.\n" + \
                       "Therefore either position or chord of a section can be defined."

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
        # read additional parameters of this shapeform 
        self._ellipseTipBelly        = fromDict (dataDict, "ellipseTipBelly", 0.25, False)
        self._ellipseBellyWidth      = fromDict (dataDict, "ellipseBellyWidth", 0.5, False)
        self._leCorrection           = fromDict (dataDict, "leCorrection", 0.0, False)
        self._ellipseCorrection      = fromDict (dataDict, "ellipseCorrection", 0.3, False)
        self._ellipseShift           = fromDict (dataDict, "ellipseShift", 0.0, False)


    def _save (self, dataDict):
        """ stores the variables into the dataDict"""

        toDict (dataDict, "ellipseTipBelly", self._ellipseTipBelly) 
        toDict (dataDict, "ellipseBellyWidth", self._ellipseBellyWidth) 
        toDict (dataDict, "leCorrection", self._leCorrection) 
        toDict (dataDict, "ellipseCorrection", self._ellipseCorrection) 
        toDict (dataDict, "ellipseShift", self._ellipseShift) 


    # ---Properties --------------------- 
    @property
    def ellipseTipBelly(self): return self._ellipseTipBelly
    def set_ellipseTipBelly(self, newVal): self._ellipseTipBelly = newVal

    @property
    def ellipseBellyWidth(self): return self._ellipseBellyWidth
    def set_ellipseBellyWidth(self, newVal): self._ellipseBellyWidth = newVal

    @property
    def leCorrection(self): return self._leCorrection
    def set_leCorrection(self, newVal): self._leCorrection = newVal

    @property
    def ellipseCorrection(self): return self._ellipseCorrection
    def set_ellipseCorrection(self, newVal): self._ellipseCorrection = newVal

    @property
    def ellipseShift(self): return self._ellipseShift
    def set_ellipseShift(self, newVal): self._ellipseShift = newVal


    # ---functions --------------------- 
    def norm_chord_function (self, y_norm):
        """
        Returns the normalized chord of the planform at y 0..1
        Args:
            :y_norm: the y-Position in the planform 0..1
        Returns:
            :chord: the chord 0..1 at y
        """
        # ----------------------------------------
        # core function for ellipse based planform 
        # ----------------------------------------
        # 
        # ellipse       x^2/a^2 + y^2/b^2 = 1
        #               x = a * sqrt (1 - (y/b)^2)  ! y is span direction, x is chord !
        # normed        x = sqrt(1- y^2)
        y = y_norm

        # first ellipse as an egg - non-symmetric to x - to achieve le angle != 0
        # oval          x^2/(a^2 - o*x) + y^2/b^2 = 1 
        # normed        a = sqrt (1 - o*y)
        #               x = sqrt (1 - o*y) * sqrt (1 - y^2)
        o = self.ellipseShift
        # x = np.sqrt (1- o *y) * np.sqrt(1- y**2)

        # then stretch - x to become y = tipChord at x= 1 
        #               b = sqrt (1 / (1 - x^2))
        #               x = tip
        # strech fac.   a = sqrt  (1 / (1 - tip ^2))
        tip = self.tipchord / self.rootchord
        b = np.sqrt (1 / (1 - tip**2 / (1-o)))
        x = np.sqrt(1 - o *y) * np.sqrt (1 - (y/b)**2)

        # then belly - the special secret function 
        #
        belly  = self._norm_tipBelly_function (y)
        x = ((1.0-belly) * x + belly) 

        # finally add a sin function along span 
        #               # x = x + c * sin (y * pi)
        c = self.ellipseCorrection * 0.1
        x = x + c * sin(y *  np.pi) 

        # chord shouldn't be more than chord
        x = min(1.0, x)                  

        return x


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
        hingeTipx = hingeRootx +  \
                            np.tan((self.hingeAngle/180) * np.pi) * self.halfwingspan

        # chord-length at y
        y_norm = y / self.halfwingspan
        chord =  self.norm_chord_function (y_norm) * self.rootchord 

        # calculate hingeDepth in percent at this particular point along the wing
        hingeDepth_y = interpolate(0.0, self.halfwingspan,
                                    self.flapDepthRoot, self.flapDepthTip,y)

        # correction of leading edge for elliptical planform, avoid swept forward part of the wing
        delta = (self.leCorrection) \
                 * sin(interpolate(0.0, self.halfwingspan, 0.0, np.pi, y)) \
                 * 0.1 * self.rootchord        # scale to proper result 0..1 

        flapDepth = (hingeDepth_y/100) * chord + delta 

        # finally the main "lines" at position y
        hinge        = (hingeTipx-hingeRootx)/(self.halfwingspan) * (y) + hingeRootx
        leadingEdge  = hinge - (chord - flapDepth)
        trailingEdge = leadingEdge + chord

        return leadingEdge, trailingEdge


    def _norm_tipBelly_function (self,y):
        """
        the normalized chord distribution along the span
        :Args:
            :y: y-position 
        Returns:
            :chord: array of chord values 
        """
        # calculate actual distance to tip
        distanceToTip = 1.0 - y

        # calculate delta that will be added to pure ellipse
        if (distanceToTip > self.ellipseBellyWidth):
            # add constant value, as we are far away from the tip
            belly = self.ellipseTipBelly
        else:
            # add decreasing value according to quarter ellipse
            a  = self.ellipseBellyWidth
            y1 = self.ellipseBellyWidth - distanceToTip
            b  = self.ellipseTipBelly
            radicand = (a*a)-(y1*y1)
            if radicand > 0: belly = (b/a) * np.sqrt(radicand) # quarter ellipse formula
            else:            belly = 0

        return belly



#-------------------------------------------------------------------------------

class Planform_Elliptical_StraightTE (Planform_Elliptical):
    """ 
    Defines the outline of an elliptical planform with straight trailing edge -
    (like Amokka) adapted with ellipse functions 
    """
    shortDescription = "Elliptical chord distribution with a straight TE generated by functions.\n" + \
                       "Therefore either position or chord of a section can be defined.\n" + \
                       "'Flap depth tip' is calculated at 90% span width..." 
    
    planformType  = "elliptical TE straight"
    isTemplate    = True                        # user may make mods 


    def __init__(self, myWing: Wing, dataDict: dict = None):
        super().__init__(myWing, dataDict)
        """
        Args:
            :myWing: the wing object self belongs to  
            :dataDict: optional - dictonary with all the data for a valid section
        """
        self._ellipseShift           = 0     # can't be changed


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

        y_norm = y / self.halfwingspan
        chord =  self.norm_chord_function (y_norm) * self.rootchord 

        # trailingEdge = interpolate(0.0, self.halfwingspan, teRootx, teTipx, y)
        trailingEdge = interpolate(0.0, teCloseToTipy, teRootx, teCloseToTipx, y)
        leadingEdge  = trailingEdge - chord

        return leadingEdge, trailingEdge



#-------------------------------------------------------------------------------

class Planform_Trapezoidal(Planform):
    """ 
    Defines the outline of an elliptical planform  
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
        norm_y_points, chords = self.wing.get_wingSections_norm_chord () 
        return np.asarray(norm_y_points)


    def norm_chord_function (self, y_norm):
        """
        Returns the normalized chord of the planform at y 0..1.
        The planform is defined by the wing sections having a position and a chord value.
        With these a multi trapezoid is defined
        Args:
            :y_norm: the y-Position in the planform 0..1
        Returns:
            :chord: the chord 0..1 at y
        """

        y_chords, chords = self.wing.get_wingSections_norm_chord ()   # tuple y_norm, c_norm

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
         
        yPosList, chordList = self.wing.get_wingSections_norm_chord()

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
        self._x_panels   = fromDict (dataDict, "x-panels", 10, False)
        self._x_dist     = fromDict (dataDict, "x-distribution", "cosine", False)
        self._y_panels   = fromDict (dataDict, "y-panels", 10, False)
        self._y_dist     = fromDict (dataDict, "y-distribution", "uniform", False)
        self._y_minWidth = fromDict (dataDict, "y-minWidth", 5 , False)
        

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

    def y_panels_forSection(self, iSec):
        """y-panels for section i depending on min panel width"""
        y_sections = self._norm_y_points() * self.halfwingspan
        if iSec < len(y_sections)-1:
            y_left = y_sections [iSec]
            y_right  = y_sections [iSec+1]
            dy_sec = y_right - y_left
            # assure a min panel width in y-direction 
            return min ( self.y_panels, round(dy_sec/self.y_minWidth))
        else:
            return 0 


    def distribution_fns_names (self):
        """ a list of available distribution functions"""
        return list(self.distribution_fns.keys())

    def _norm_y_points (self):
        """
        array of y points along the span - here the wing sections pos 
        """
        # overwritten to get y of sections 
        norm_y_points = self.wing.get_wingSections_norm_Pos () 
        return np.asarray(norm_y_points)


    def _planform_function (self, y):
        """
        the planform represented as trapezoid
        """
        y_sections = self._norm_y_points() * self.halfwingspan

        for iSec in range(len(y_sections) - 1):
            y_before = y_sections [iSec]
            y_after  = y_sections [iSec+1]
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
        tipArea   = 25                          # do not tke this tip mm into account for deviations 

        y_distribution_fn = self.distribution_fns [self.y_dist]
        y_sections = self._norm_y_points() * self.halfwingspan

        # get le te for all sections 
        sections_le_te = []
        for y_sec in y_sections: 
            # take actual planform .. function (maybe faster) 
            sections_le_te.append(self.wing.planform._planform_function (y_sec))

        # now calc a y line according to y distribution function between sections 
        for iSec in range(len(y_sections) - 1):
            y_left   = y_sections [iSec]
            y_right  = y_sections [iSec+1]
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
                if yPos < (self.halfwingspan - tipArea):
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
        y_sections = self._norm_y_points() * self.halfwingspan

        # get le te for all sections 
        sections_le_te = []
        for y_sec in y_sections: 
            sections_le_te.append(self.wing.planform._planform_function (y_sec))


        # now calc a x horizontal line according to x distribution function between sections 
        for iSec in range(len(y_sections) - 1):
            y_left = y_sections [iSec]
            y_right  = y_sections [iSec+1]

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
                self._dxfPathFilename  = fromDict (dataDict, "refPlanform_dxfPath", "")
            else:
                self._dxfPathFilename  = fromDict (dataDict, "planform_dxfPath", "")


        if self._dxfPathFilename != None and os.path.isfile(self._dxfPathFilename):
            self.load_dxf(self._dxfPathFilename)
        else:
            if self._dxfPathFilename:
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

    def dxf_pathFilename (self):
        """ the complete filename with the path of the dxf file"""
        return self._dxfPathFilename
    
    def set_dxfPathFilename (self, aNewPathFile):
        
        if aNewPathFile:
            # load and parse data from file
            self.load_dxf (aNewPathFile)
            if self.isValid:
                self._dxfPathFilename = aNewPathFile
        else: 
            # clear self
            self._dxfPathFilename  = None
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


    def norm_chord_function (self, y_norm):
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

        for idx in range(len(line)):
            xp, yp = line[idx]
            if (abs(x-xp) < eps):               # found identical point ?
                x, y = line[idx]
                break
            elif (xp >= x) and (idx>=1):        # first point with x value >= x
                x1, y1 = line[idx-1]
                x2, y2 = line[idx]
                y = interpolate(x1, x2, y1, y2, x)
                break

        if (y == None): 
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

        from .dxf_utils import import_fromDXF

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

        if (self._yPos == 0.0 or self._norm_chord == 1.0): 
            self._yPos = 0.0
            self._norm_chord = 1.0
            self.isRoot = True
        else:
            self.isRoot = False

        if (self._yPos == self.wing.halfwingspan): 
            self.isTip  = True
        else:
            self.isTip  = False

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

        InfoMsg ('  '   + str(self)  + ' created...')


    def _save (self, sectionDict):
        """ stores the variables into the dataDict - returns the filled dict"""

        toDict (sectionDict, "position",    self._yPos) 
        toDict (sectionDict, "norm_chord",  self._norm_chord) 
        toDict (sectionDict, "flapGroup",   self._flapGroup) 

        airfoilDict = self.airfoil._save ({})
        toDict (sectionDict, "airfoil",  airfoilDict) 

        return sectionDict


    def _init_airfoil (self, dataDict = None, pathFileName = None):
        # read data for airfoil from dict and create new 'Airfoil' 

        if dataDict: 
            airfoil = Airfoil.onDict (dataDict)
        else: 
            airfoil = Airfoil (pathFileName= pathFileName)

        if not airfoil.isExisting:
            if self.isRoot: 
                airfoil = Root_Example()
            elif self.isTip:
                airfoil = Tip_Example()
            else:
                airfoil = Strak_Airfoil()
        if airfoil.isExisting:
            airfoil.load()

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
            pos = self.find_yPosFromChord (self._norm_chord * self.wing.rootchord) 
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
            safety = 1.0                # = fixed
            leftLimit  = self.yPos
            rightLimit = self.yPos
        else:
            safety = 1.01               # = 1%
            if leftSec: 
                leftLimit = leftSec.yPos
            else:
                leftLimit = self.yPos
            if rightSec: 
                rightLimit = rightSec.yPos
            else:
                rightLimit = self.yPos
        return (leftLimit * safety, rightLimit / safety)
    
    
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
        if there is rootRe takke this else use normchord"""

        if self.wing.rootRe >= 100000:
            return "-%d" % int(self.Re/10000)
        elif self.wing.rootRe >= 10000:
            return "-%d" % int(self.Re/1000)
        elif self.wing.rootRe > 0:
            return "-%d" % int(self.Re)
        else:
            return "-%d" % int(self.norm_chord * 100)
        
    def airfoilNick(self): 
        """ the airfoil nickname like GP-16 from nickname prefix and reynolds (or normchord)"""

        if self.wing.airfoilNickPrefix:
            prefix = self.wing.airfoilNickPrefix
        else:
            prefix = "JX"
        return prefix + self.airfoilNickPostfix()


    def airfoil_canBeRemoved (self):
        return (not self.isRootOrTip) and (not self.airfoil.isStrakAirfoil)
    
    def set_airfoilWithPathFileName (self, pathFileName):
        """ sets a new real Airfoil based on its path and loads it """

        self._init_airfoil (pathFileName=pathFileName)
        self.airfoil.load()


    def do_export_airfoil (self,toDir, useNick=True): 
        """ exports airfoil into directory 'toDir'. Optionally use airfoils Nickname
        as new airfoil name.
        Returns the filename of the exported airfoil
        """
        if useNick: 
            newName = self.airfoilNick()
        else: 
            newName = None

        filePathName = self.airfoil.copyAs(dir=toDir, destName = newName)

        return os.path.basename(filePathName) 
        

    def find_yPosFromChord (self, chord):
        """
        calculates the y-Position from a chord length 
        Returns:
            :y:   
        """
        # some kind of bubble search is performed until accuracy is better than epsilon
        epsilon = 0.001                             # normalized y accuracy 
        myChord = chord / self.wing.rootchord       # normalize 
        planform = self.wing.planform

        if(myChord > 1.0 or myChord < 0): 
            ErrorMsg ("Chord %f must be between 0.0 and root chord" % chord)
            return

        yLeft   = 0 
        y       = None
        yRight  = 1
        
        while y == None:
            chordLeft   = planform.norm_chord_function (yLeft)
            chordRight  = planform.norm_chord_function (yRight)
            if   (abs (myChord - chordLeft)  < epsilon):
                y = yLeft 
                break
            elif (abs (myChord - chordRight) < epsilon):
                y = yRight
                break
            yMiddle = (yLeft + yRight) / 2 
            chordMiddle = planform.norm_chord_function (yMiddle)
            if (myChord > chordMiddle): 
                yRight = yMiddle 
            else:
                yLeft  = yMiddle

        return y * self.wing.halfwingspan

    
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
#-------------------------------------------------------------------------------



# Main program for testing 
if __name__ == "__main__":


    print ("Current directory: ",os.getcwd())
    filename = ".\\ressources\\planformdata.json"
    # filename = ""
    myWing = Wing.onFile (filename)

    y, x = myWing.planform.flapPolygon (0,500, nPoints=50)

    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(figsize=(10, 6))
    for flap in myWing.getFlaps():
       ax.plot (flap.y,flap.x, label = flap.flapGroup) 
    ax.legend()
    plt.show()

    print  (myWing.planform.flapPolygon (0,500, nPoints=4) )

"""     for i in range(5):
        if myWing.planformType == "elliptical": 
            myWing.set_planformType ("trapezoidal")
        else:
            myWing.set_planformType ("elliptical")
 """