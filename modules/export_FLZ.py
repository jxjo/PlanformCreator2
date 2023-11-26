#!/usr/bin/env python

import os
from math import atan, pi
from datetime import datetime 
from typing import TextIO
import getpass
from common_utils import *
from wing_model import Wing, WingSection, Airfoil, Planform_Paneled


# FLZ panel distribution names to self names
distrib_name_map ={}
distrib_name_map ["uniform"] = "LINEAR"
distrib_name_map ["-sine"]   = "SIN_R"      # ! left and right halfwing flip
distrib_name_map ["sine"]    = "SIN_L"
distrib_name_map ["cosine"]  = "COS"


class Export_FLZ:
    """ 

    Handle export of a paneled planform to an FLZ file  

    """
    def __init__(self, wing : Wing, myDict: dict = None):
 
        self.wing       = wing
        self.workingDir = wing.workingDir       
        self._exportDir = fromDict (myDict, "exportDir", "Flz_vortex", msg=False)
        self._useNick   = fromDict (myDict, "useNick", True, msg=False)

        # create an extra Planform for the paneled planform to export later to Xflr5, FLZ, ...
        self.paneledPlanform  = Planform_Paneled (self.wing, myDict)

    def _save (self):
        """ returns the parameters of self in dataDict"""

        myDict = {}
        toDict (myDict, "exportDir",         self._exportDir) 
        toDict (myDict, "useNick",           self._useNick) 
        self.paneledPlanform._save (myDict)

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
        """the directory for flz export including current dir """
        return PathHandler (workingDir=self.workingDir).fullFilePath (self.exportDir)

    @property
    def useNick(self) -> bool: return self._useNick
    def set_useNick(self, aBool): self._useNick = aBool

    @property
    def fileName(self): 
        return self.wing.name.strip() +  '_wing.flz'


    def doIt (self): 
        """ main entry: start the export to the file defined in self parameters.
        Returns a message string what was done """

        # ensure straked airfoils are loaded 
        self.wing.do_strak()

        targetDir = self.baseAndExportDir

        if not os.path.exists(targetDir): os.makedirs(targetDir)
        pathFileName = os.path.join (targetDir, self.fileName)

        fileStream = open(pathFileName, 'w')
        # build Flz data structure and write it on stream 
        FLUGZEUG (self.wing, self.paneledPlanform).write(fileStream)
        fileStream.close()

        InfoMsg("FLZ_vortex file written to %s." % pathFileName)
        message = "'"+ self.wing.name + "'\n\n exported to \n\n\'" + pathFileName + "'"      

        return message


# structure of a FLZ-file:
#
#    [FLUGZEUG]                     equals 'Dummy' 
#      data
#      [FLAECHE0]                   equals Wing
#        data
#        [PROFIL]                   equals root Airfoil 
#          data
#        [PROFIL ENDE] 
#        [SEGMENT0]                 equals WingSection from wing left to right 
#          data
#          [PROFIL]                 equals Airfoil at section towards tip 
#            data
#          [PROFIL ENDE] 
#        [SEGMENT ENDE]
#        [SEGMENT1]
#          data
#        [SEGMENT ENDE]
#        further segments
#      [FLAECHE ENDE]
#      [FLAECHE1]
#        data, segments etc. like above
#      [FLAECHE ENDE]
#  [FLUGZEUG ENDE]
#  [SCHALTER]
#  [SCHALTER ENDE]
#  [EINSTELLUNGEN]
#  [EINSTELLUNGEN ENDE]
#
#  For each section of the file, there is a class to handle data and export 

class FLZ_Element: 

    def __init__(self,wing : Wing, paneledPlanform : Planform_Paneled, index=None):
    
        self.wing = wing
        self.paneledPlanform = paneledPlanform
        self.index = index


    @property
    def startTag(self): 
        index = str(self.index) if not self.index is None else ""
        return '['+self.__class__.__name__+ index +']'
    @property
    def endTag(self): 
        return '['+self.__class__.__name__ + ' ENDE]'

    def _write (self, aStream : TextIO, aString ):
        aStream.write (aString + '\n')

    def write (self, aStream):
        pass


class FLUGZEUG (FLZ_Element): 
    def __init__(self, wing : Wing, paneledPlanform):
        super().__init__(wing, paneledPlanform)

        # build the flz datstructure tree
        self.flaeche        = FLAECHE(wing, paneledPlanform, index=0)
        self.schalter       = SCHALTER()
        self.einstellungen  = EINSTELLUNGEN()

    def write (self, aStream):

        # FLZ_VORTEX
        # 11.03.2023 03:59:52
        #

        self._write (aStream, "FLZ_VORTEX")
        now = datetime.now()
        self._write (aStream, now.strftime("%d.%m.%Y %H:%M:%S"))
        self._write (aStream, "")

        # KONSTRUKTEUR=jojo
        # BEZEICHNUNG=VJX
        # POSITION X,Y,Z=-0.38041 0.15083 2.78949
        # BETRACHTUNGSWINKEL X,Y,Z=22.00000 31.00000 0.00000
        # ANSTELLWINKEL=0.00000
        # CA=0.00000
        # # STABILITAETSMASS=0.00000
        # # SCHWERPUNKT X=0.00000
        # FLUG_GESCHWINDIGKEIT=0.00000

        me = getpass.getuser()
        aoa = 8.0                               # degrees
        cl  = 0.8 
        v   = 20                                # m/s          

        self._write (aStream, self.startTag)

        self._write (aStream, "KONSTRUKTEUR=%s"             % me)
        self._write (aStream, "BEZEICHNUNG=%s"              % self.wing.name)
        self._write (aStream, "POSITION X,Y,Z=%s"           % "-0.38041 0.15083 2.78949")
        self._write (aStream, "BETRACHTUNGSWINKEL X,Y,Z=%s" % "22.00000 31.00000 0.00000")
        self._write (aStream, "ANSTELLWINKEL=%.5f"          % aoa)
        self._write (aStream, "CA=%.5f"                     % cl)
        self._write (aStream, "FLUG_GESCHWINDIGKEIT=%.5f"   % v)

        self.flaeche.write(aStream)

        self._write (aStream, self.endTag)

        self.schalter.write(aStream)
        self.einstellungen.write(aStream)


class FLAECHE (FLZ_Element):
    def __init__(self, wing : Wing, paneledPlanform, index=None):
        super().__init__(wing, paneledPlanform, index)

        self.profil = PROFIL (wing.rootSection.airfoil)


        wingSections = wing.wingSections

        # segments: first right FLZ half wing 
        rightSegments =[]
        for i,sec in enumerate(wingSections): 
            if i < len(wingSections) - 1:
                newSegment = SEGMENT(wing, paneledPlanform, sec, wingSections[i+1])
                rightSegments.append(newSegment)  
        # segments: then left FLZ half wing 
        leftSegments =[]
        for i,sec in reversed (list(enumerate(wingSections))): 
            if i < len(wingSections) - 1:
                # flip section order 
                newSegment = SEGMENT(wing, paneledPlanform, wingSections[i+1], sec)
                leftSegments.append(newSegment)  

        self.segments = leftSegments + rightSegments

        for i, seg in enumerate (self.segments): 
            seg.index = i                       # put FLZ segment index into segments

    def write (self, aStream):

        # ART=FLUEGEL
        # BEZEICHNUNG=mySuperWing                
        # PROFILTIEFE=0.20000                   
        # BEZUGSPUNKT_PROFILTIEFE=0.00000
        # ANZAHL PANELS X=4
        # VERTEILUNG=LINEAR
        # ANZAHL PANELS VOLUMENDARSTELLUNG=30
        # MASSE=0.20000
        # [PROFIL]....

        distrib = distrib_name_map[self.paneledPlanform.x_dist]
        mass    = 3                 # kg

        self._write (aStream, self.startTag)
      
        self._write (aStream, "ART=FLUEGEL")
        self._write (aStream, "BEZEICHNUNG=%s"      % self.wing.name)
        self._write (aStream, "PROFILTIEFE=%.5f"    % (self.wing.rootchord/1000))
        self._write (aStream, "BEZUGSPUNKT_PROFILTIEFE=%.5f"    % (0.0))
        self._write (aStream, "ANZAHL PANELS X=%d"  % self.paneledPlanform.x_panels)
        self._write (aStream, "VERTEILUNG=%s"       % distrib)
        self._write (aStream, "ANZAHL PANELS VOLUMENDARSTELLUNG=%s" % 30)
        self._write (aStream, "MASSE=%.5f"          % mass)

        self.profil.write (aStream)

        seg: SEGMENT
        for seg in self.segments:
            seg.write(aStream) 

        self._write (aStream, self.endTag)


class SEGMENT (FLZ_Element):
    def __init__(self, wing, paneledPlanform, 
                 leftSection: WingSection, rightSection: WingSection, index= None):
        super().__init__(wing, paneledPlanform, index)

        self.leftSection  = leftSection
        self.rightSection = rightSection
        # geht the right airfoil according to FLZ sequenze 
        if leftSection.yPos < rightSection.yPos:
            self.profil = PROFIL (rightSection.airfoil)
        else: 
            self.profil = PROFIL (leftSection.airfoil)

    def write (self, aStream):

        # SEGMENTBREITE=-0.50000
        # PROFILTIEFE=0.20000
        # BEZUGSPUNKT_PROFILTIEFE=0.00000
        # VERWINDUNGSWINKEL=0.00000
        # V-FORM_WINKEL=0.00000
        # PFEILWINKEL=0.00000
        # BEZUGSPUNKT_PFEILWINKEL=0.00000
        # ANZAHL PANELS Y=21
        # VERTEILUNG=LINEAR
        # KLAPPENTIEFE LINKS,RECHTS=25.00000 25.00000
        # KLAPPENGRUPPE=0

        distrib  = distrib_name_map[self.paneledPlanform.y_dist]

        leftSectionIndex  = self.wing.wingSectionIndexOf (self.leftSection)  - 1  # this index is +1 
        rightSectionIndex = self.wing.wingSectionIndexOf (self.rightSection) - 1  # this index is +1 

        # use sections of the paneled planform as original tip cut be cutted due to minTipChord ... 
        sections_yPos, sections_chord = self.paneledPlanform._sections_yPos_chord()
        leftSection_yPos   = sections_yPos  [leftSectionIndex]
        leftSection_chord  = sections_chord [leftSectionIndex]
        rightSection_yPos  = sections_yPos  [rightSectionIndex]
        rightSection_chord = sections_chord [rightSectionIndex]

        if leftSection_yPos <rightSection_yPos:          # right halfwing
            chord     = rightSection_chord / 1000
            flapGroup = self.leftSection.flapGroup
            y_panels  = self.paneledPlanform.y_panels_forSection(sections_yPos,leftSectionIndex)
        else:                                                       # left halfwing 
            chord     = leftSection_chord / 1000
            flapGroup = self.rightSection.flapGroup
            y_panels  = self.paneledPlanform.y_panels_forSection(sections_yPos, rightSectionIndex)
            # we have to flip FLZ SIN_R - SIN_L  on left side 
            if   distrib == "SIN_R": distrib = "SIN_L"
            elif distrib == "SIN_L": distrib = "SIN_R"
        width = (rightSection_yPos - leftSection_yPos) / 1000
        refChord = 0.0 
        twist = 0.0 
        dihedral = 0.0

        le_left,_  = self.wing.planform._planform_function (leftSection_yPos)  
        le_right,_ = self.wing.planform._planform_function (rightSection_yPos) 
        sweep = atan (abs((le_left - le_right)/(1000 *width))) * 180 / pi
        refSweep = 0

        flapDepthLeft  = self.wing.planform.flapDepthAt (leftSection_yPos)  * 100
        flapDepthRight = self.wing.planform.flapDepthAt (rightSection_yPos) * 100

        self._write (aStream, self.startTag)

        self._write (aStream, "SEGMENTBREITE=%.5f"      %width)
        self._write (aStream, "PROFILTIEFE=%.5f"        % chord)
        self._write (aStream, "BEZUGSPUNKT_PROFILTIEFE=%.5f" % refChord)
        self._write (aStream, "VERWINDUNGSWINKEL=%.5f" % twist)
        self._write (aStream, "V-FORM_WINKEL=%.5f" % dihedral)
        self._write (aStream, "PFEILWINKEL=%.5f" % sweep)
        self._write (aStream, "BEZUGSPUNKT_PFEILWINKEL=%.5f" % refSweep)
        self._write (aStream, "ANZAHL PANELS Y=%d" % y_panels)
        self._write (aStream, "VERTEILUNG=%s" % distrib)
        self._write (aStream, "KLAPPENTIEFE LINKS,RECHTS=%.5f %.5f" % (flapDepthLeft, flapDepthRight))
        self._write (aStream, "KLAPPENGRUPPE=%d" % flapGroup)

        self.profil.write(aStream) 

        self._write (aStream, self.endTag)


class PROFIL (FLZ_Element):
    def __init__(self, airfoil: Airfoil):

        self.index = None
        self.airfoil = airfoil

    def write (self, aStream):

        self._write (aStream, self.startTag)
        self._write (aStream, "PROFILDATEINAME=%s.dat" % self.airfoil.name)

        for i, xc in enumerate (self.airfoil.x):
            yc = self.airfoil.y[i]
            # if i < 3 : 
            self._write (aStream, "PK%d=%.5f %.5f" % (i, xc, yc))

        self._write (aStream, self.endTag)


class SCHALTER (FLZ_Element):
    def __init__(self):

        self.index = None

    def write (self, aStream):

        # BUTTON GESAMT FLUGZEUG DARSTELLUNG=FALSE
        # BUTTON KLAPPEN WINKEL AUSGABE=FALSE
        # BUTTON CWI LOKAL=TRUE
        # BUTTON CWV LOKAL=FALSE
        # BUTTON CWG LOKAL=FALSE
        # BUTTON GAMMA LOKAL=TRUE
        # BUTTON GAMMA VORGABE=FALSE
        # CHECKBOX GAMMA VORGABE INTEGRAL=FALSE
        # BUTTON CA LOKAL=TRUE
        # CHECKBOX CA LOKAL MIN MAX=TRUE
        # BUTTON AI LOKAL=FALSE
        # BUTTON BLASENWARNUNG=FALSE
        # BUTTON RE LOKAL=FALSE
        # BUTTON XD=FALSE
        # BUTTON XS=FALSE
        # BUTTON XN=FALSE

        self._write (aStream, self.startTag)

        self._write (aStream, "BUTTON CWI LOKAL=%s"             % "TRUE")
        self._write (aStream, "BUTTON GAMMA LOKAL=%s"           % "TRUE")
        self._write (aStream, "BUTTON CA LOKAL=%s"              % "TRUE")
        self._write (aStream, "CHECKBOX CA LOKAL MIN MAX=%s"    % "TRUE")

        self._write (aStream, self.endTag)


class EINSTELLUNGEN (FLZ_Element):
    def __init__(self):

        self.index = None

    def write (self, aStream):

        # SPLITTERPOS=1300
        self._write (aStream, self.startTag)
        self._write (aStream, "SPLITTERPOS=%s"             % "1400")
        self._write (aStream, self.endTag)

#-------------------------------------------------------------------------------

# Main program for testing 
if __name__ == "__main__":

    myWing = Wing ("")

    Export_FLZ(myWing).doIt ()
