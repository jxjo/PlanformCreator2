#!/usr/bin/env pythonbutton_color
# -*- coding: utf-8 -*-

"""  

Handle export of airfoils to file 

"""

import logging
import os, re
import io
import numpy as np

from copy                   import deepcopy
from typing                 import TextIO
from datetime               import datetime 
from math                   import atan, pi

from base.common_utils      import * 
from wing                   import Wing, Planform, Planform_Paneled
from wing                   import WingSection, WingSections, Flap
from model.airfoil          import Airfoil, GEO_SPLINE

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


class Export_Airfoils:
    """ 
    Handle export of the current airfoils of wing to a subdirectory
    """
    def __init__(self, wing : Wing, myDict: dict = None):
 
        self._wing       = wing
        self._working_dir = wing.workingDir       
        self._export_dir         = fromDict (myDict, "export_dir", "airfoils")
        self._use_nick_name      = fromDict (myDict, "use_nick_name", True)
        self._adapt_te_gap       = fromDict (myDict, "adapt_te_gap", False)
        self._te_gap_mm          = fromDict (myDict, "te_gap_mm", 0.5)

    def _as_dict (self) -> dict:
        """ returns a data dict with the paramters of self"""

        d = {}
        toDict (d, "export_dir",        self._export_dir) 
        toDict (d, "use_nick_name",     self._use_nick_name) 
        toDict (d, "adapt_te_gap",      self._adapt_te_gap) 
        toDict (d, "te_gap_mm",         self._te_gap_mm) 
        return d

    @property
    def _chord_root (self) -> float: 
        return self._wing.planform.chord_root

    @property
    def _wingSections (self) -> WingSections: 
        return self._wing.planform.wingSections

    @property
    def export_dir(self):
        """the directory for airfoils export - path is relativ to current or absolute """
        return self._export_dir
    
    def set_export_dir(self, newStr): 
        self._export_dir = PathHandler (workingDir=self._working_dir).relFilePath (newStr) # ensure a valid, relativ path 

    @property
    def base_and_export_dir(self):
        """the directory for airfoil export including current dir """
        return PathHandler (workingDir=self._working_dir).fullFilePath (self.export_dir)

    @property
    def use_nick_name(self) -> bool: return self._use_nick_name
    def set_use_nick_name(self, aBool): self._use_nick_name = aBool

    @property
    def adapt_te_gap(self) -> bool: return self._adapt_te_gap
    def set_adapt_te_gap(self, aBool): self._adapt_te_gap = aBool

    @property
    def te_gap_mm(self) -> float: return self._te_gap_mm
    def set_te_gap_mm(self, aVal): 
        self._te_gap_mm = aVal

    @property
    def n_airfoils (self) -> int:
        """ nuber of airfoils being exported"""
        return len (self._wingSections)
    


    def do_it (self, toDir : str = None, use_nick_name = None): 
        """ 
        main entry: start the export to the file defined in self parameters.
        
        Args:
            toDir: alternate target directory
        Returns:
            list of airfoil fieNames 
        """

        if toDir is None:
            targetDir = self.base_and_export_dir
        else: 
            targetDir = toDir

        if use_nick_name is None: 
            use_nick_name = self.use_nick_name

        # ensure all airfoils are up to date and splined (quality) 

        self._wingSections.do_strak (geometry_class=GEO_SPLINE)          

        # save airfoils of all sections  

        fileNames  = []
        section : WingSection

        for section in self._wingSections:

            if use_nick_name and section.airfoil_nick_name: 
                newName = section.airfoil_nick_name
            else: 
                newName = None
            
            if self.adapt_te_gap:                                       # te gap in mm? if yes scale it to normed
                te_gap = self.te_gap_mm / (section.cn * self._chord_root)
            else: 
                te_gap = None 
        
            pathFileName = section.airfoil.save_copyAs (dir=targetDir, destName = newName, te_gap = te_gap)

            fileNames.append (os.path.basename(pathFileName) )


        logger.info (f"{self.n_airfoils} Airfoils written to '{targetDir}'") 

        return 



import xml.etree.ElementTree as ET


class Export_Xflr5:
    """ 
    Handle export of a paneled planform to an Xflr5 xml file   
    """

    distrib_name_map ={}
    distrib_name_map ["uniform"] = "UNIFORM"
    distrib_name_map ["-sine"]   = "INVERSE SINE"
    distrib_name_map ["sine"]    = "SINE"
    distrib_name_map ["cosine"]  = "COSINE"

    def __init__(self, wing: Wing, planform_paneled : Planform_Paneled, myDict: dict = None): 

        self._wing        = wing
        self._working_dir  = wing.workingDir  
        self._planform_paneled  = planform_paneled

        self._export_dir = fromDict (myDict, "export_dir", "xflr5")


    def _as_dict (self) -> dict:
        """ returns a data dict with the paramters of self"""

        d = {}
        toDict (d, "export_dir", self._export_dir) 
        return d
    
    @property
    def _planform (self) -> Planform:
        return self._wing.planform

    @property
    def _wingSections (self) -> list[WingSection]:
        return self._planform_paneled.wingSections_reduced()
    
    @property
    def export_dir(self):
        """the directory for xflr5 export - path is relativ to current or absolute """
        return self._export_dir
    
    def set_export_dir(self, newStr): 
        self._export_dir = PathHandler (workingDir=self._working_dir).relFilePath (newStr)

    @property
    def base_and_export_dir(self):
        """the directory for flz export including current dir """
        return PathHandler (workingDir=self._working_dir).fullFilePath (self.export_dir)

    @property
    def filename(self): 
        return self._wing.name.strip() +  '_wing.xml'

    @property
    def use_nick_name (self) -> bool: return self._planform_paneled.use_nick_name

    @property
    def n_airfoils (self) -> int:
        """ nuber of airfoils being exported"""
        return self._wing.export_airfoils.n_airfoils
    

    def do_it (self): 
        """ 
        Main entry: start the export to the file defined in paramters.
        Airfoils will also be copied into the xflr5 directory
        """

        # if necessary create directory 

        targetDir = self.base_and_export_dir
        if not os.path.exists(targetDir): os.makedirs(targetDir)
        pathFileName = os.path.join (targetDir, self.filename)

        # first export airfoils to ensure actual airfoil names 

        self._wing.export_airfoils.do_it (toDir = targetDir, use_nick_name=self.use_nick_name)

        # export wing 

        self.export_wing (pathFileName)

        logger.info ("Xflr5 wing data written to %s." % pathFileName)

        return 


    def export_wing (self, pathFileName):

        # get file object with xflr xml templae 
        templateFile = Export_Xflr5.Xflr5_template().get_template_wing()

        # basically parse the XML-file
        tree = ET.parse(templateFile)
        templateFile.close()

        # get root of XML-tree
        root = tree.getroot()

        # find wing-data
        for wingXml in root.iter('wing'): pass

        if (wingXml == None):
            raise ValueError ("wing not found in xml-template")

        # wing name
        for nameXml in wingXml.iter('Name'):
            nameXml.text = self._wing.name
        for descriptionXml in wingXml.iter('Description'):
            descriptionXml.text = "by Planform Creator 2"

        # find sections-data-template
        for sectionsTemplateXml in wingXml.iter('Sections'):
            # copy the template
            sectionsXml = deepcopy(sectionsTemplateXml)

            # remove the template from wing 
            wingXml.remove(sectionsTemplateXml)

            # find section-data-template
            for sectionTemplateXml in sectionsXml.iter('Section'):
                newSectionXml = deepcopy(sectionTemplateXml)

                # remove the template from sections
                sectionsXml.remove(sectionTemplateXml)

        # write the new section-data to the wing
        section : WingSection

        # get section yPos and chord from paneled planform as tip could be cutted 
        #
        # ! x and y are swapped !
        #

        for iSec, section in enumerate(self._wingSections):
            # copy the template
            newSectionXml = deepcopy(sectionTemplateXml)

            # transform to planform coordinates
            section_line_x, section_line_y = section.line() 
            section_x  = section_line_x[0]    
            section_c  = section_line_y[1] - section_line_y[0]
            section_le = section_line_y[0]

            # x
            for x_number_of_panels in newSectionXml.iter('x_number_of_panels'):
                x_number_of_panels.text = str(self._planform_paneled.wx_panels)
            for x_panel_distribution in newSectionXml.iter('x_panel_distribution'):
                # map to xflr5 distribution names 
                xflr5_dist = self.distrib_name_map[self._planform_paneled.wx_dist]
                x_panel_distribution.text = str(xflr5_dist)

            # y
            for y_number_of_panels in newSectionXml.iter('y_number_of_panels'):
                y_number_of_panels.text = str(self._planform_paneled.nx_panels_of_section (iSec))
            for y_panel_distribution in newSectionXml.iter('y_panel_distribution'):
                # map to xflr5 distribution names 
                xflr5_dist = self.distrib_name_map[self._planform_paneled.wy_dist]
                y_panel_distribution.text = str(xflr5_dist)

            for yPosition in newSectionXml.iter('y_position'):
                yPosition.text = str(section_x)

            for chord in newSectionXml.iter('Chord'):
                chord.text = str(section_c)

            for xOffset in newSectionXml.iter('xOffset'):
                xOffset.text = str(section_le)

            for dihedral in newSectionXml.iter('Dihedral'):
                # tbd
                dihedral.text = str(0)

            # airfoil - use nick name? 
            if self.use_nick_name:
                airfoilName = section.airfoil_nick_name
            else: 
                airfoilName = section.airfoil.name
                
            for foilName in newSectionXml.iter('Left_Side_FoilName'):
                foilName.text = re.sub('.dat', '', airfoilName)

            for foilName in newSectionXml.iter('Right_Side_FoilName'):
                foilName.text = re.sub('.dat', '', airfoilName)

            # add the new section to the tree
            sectionsXml.append(newSectionXml)
        
        wingXml.append(sectionsXml)

        tree.write(pathFileName)

        return 


    class Xflr5_template ():
        """ A xflr5 template to use for export"""

        def get_template_wing (self) -> io.StringIO:
            """ returns the template file object as a string file"""

            templateFile = io.StringIO()
            templateFile.write (
    """<?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE explane>
    <explane version="1.0">
        <Units>
            <length_unit_to_meter>0.001</length_unit_to_meter>
            <mass_unit_to_kg>0.001</mass_unit_to_kg>
        </Units>
        <wing>
            <Name>Main Wing</Name>
            <Type>MAINWING</Type>
            <Color>
                <red>255</red>
                <green>255</green>
                <blue>127</blue>
                <alpha>255</alpha>
            </Color>
            <Description></Description>
            <Position>          0,           0,           0</Position>
            <Tilt_angle>  0.000</Tilt_angle>
            <Symetric>true</Symetric>
            <isFin>false</isFin>
            <isDoubleFin>false</isDoubleFin>
            <isSymFin>false</isSymFin>
            <Inertia>
                <Volume_Mass>  2.500</Volume_Mass>
            </Inertia>
            <Sections>
                <Section>
                    <y_position>  0.000000</y_position>
                    <Chord>  0.220000</Chord>
                    <xOffset>  0.000000</xOffset>
                    <Dihedral>  3.000000</Dihedral>
                    <Twist>  0.000</Twist>
                    <x_number_of_panels>13</x_number_of_panels>
                    <x_panel_distribution>COSINE</x_panel_distribution>
                    <y_number_of_panels>2</y_number_of_panels>
                    <y_panel_distribution>UNIFORM</y_panel_distribution>
                    <Left_Side_FoilName>MainWing_1</Left_Side_FoilName>
                    <Right_Side_FoilName>MainWing_1</Right_Side_FoilName>
                </Section>
            </Sections>
        </wing>
    </explane>""")
            
            templateFile.seek(0)
            return templateFile




class Export_FLZ:
    """ 
    Handle export of a paneled planform to an FLZ file  
    """

    # FLZ panel distribution names to self names
    distrib_name_map ={}
    distrib_name_map ["uniform"] = "LINEAR"
    distrib_name_map ["-sine"]   = "SIN_R"      # ! left and right halfwing flip
    distrib_name_map ["sine"]    = "SIN_L"
    distrib_name_map ["cosine"]  = "COS"


    def __init__(self, wing : Wing, planform_paneled : Planform_Paneled, myDict: dict = None):
 
        self._wing        = wing
        self._working_dir  = wing.workingDir  
        self._planform_paneled  = planform_paneled

        self._export_dir = fromDict (myDict, "export_dir", "flz")


    def _as_dict (self) -> dict:
        """ returns a data dict with the paramters of self"""

        d = {}
        toDict (d, "export_dir", self._export_dir) 
        return d

    @property
    def _planform (self) -> Planform:
        return self._wing.planform

    @property
    def _wingSections (self) -> list[WingSection]:
        return self._planform_paneled.wingSections_reduced()

    @property
    def export_dir(self):
        """the directory for xflr5 export - path is relativ to current or absolute """
        return self._export_dir
    
    def set_export_dir(self, newStr): 
        self._export_dir = PathHandler (workingDir=self._working_dir).relFilePath (newStr)

    @property
    def base_and_export_dir(self):
        """the directory for flz export including current dir """
        return PathHandler (workingDir=self._working_dir).fullFilePath (self.export_dir)

    @property
    def use_nick(self) -> bool: return self._use_nick
    def set_use_nick(self, aBool): self._use_nick = aBool

    @property
    def filename(self): 
        return self._wing.name.strip() +  '_wing.flz'

    @property
    def use_nick_name (self) -> bool: return self._planform_paneled.use_nick_name

    @property
    def n_airfoils (self) -> int:
        """ nuber of airfoils being exported"""
        return self._wing.export_airfoils.n_airfoils



    def do_it (self): 
        """ 
        Main entry: start the export to the file defined in paramters.
        Airfoils will also be copied into the xflr5 directory
        """

        # ensure all airfoils are up to date and splined (quality) 

        self._planform.wingSections.do_strak (geometry_class=GEO_SPLINE)          

        # if necessary create directory 

        targetDir = self.base_and_export_dir
        if not os.path.exists(targetDir): os.makedirs(targetDir)
        pathFileName = os.path.join (targetDir, self.filename)

        # let FLUGZEUG write to stream all the data 
        
        fileStream = open(pathFileName, 'w')
        Export_FLZ.FLUGZEUG (self._wing, self._planform_paneled).write(fileStream)
        fileStream.close()

        logger.info ("FLZ_vortex file written to %s." % pathFileName)

        return 


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

        def __init__(self, wing : Wing, planform_paneled : Planform_Paneled, index = None):
    
            self._wing        = wing
            self._working_dir  = wing.workingDir  
            self._planform_paneled  = planform_paneled
            self._index = index

        @property
        def _wingSections (self) -> list[WingSection]:
            return self._planform_paneled.wingSections_reduced()

        @property
        def start_tag(self): 
            index = str(self._index) if not self._index is None else ""
            return '['+self.__class__.__name__+ index +']'
        
        @property
        def end_tag(self): 
            return '['+self.__class__.__name__ + ' ENDE]'

        def _write (self, aStream : TextIO, aString ):
            aStream.write (aString + '\n')

        def write (self, aStream):
            pass


    class FLUGZEUG (FLZ_Element): 
        def __init__(self, *args):
            super().__init__(*args)

            # build the flz datstructure tree
            self.flaeche        = Export_FLZ.FLAECHE (*args, index=0)
            self.schalter       = Export_FLZ.SCHALTER()
            self.einstellungen  = Export_FLZ.EINSTELLUNGEN()

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

            me = "myself"
            aoa = 8.0                               # degrees
            cl  = 0.8 
            v   = 20                                # m/s          

            self._write (aStream, self.start_tag)

            self._write (aStream, "KONSTRUKTEUR=%s"             % me)
            self._write (aStream, "BEZEICHNUNG=%s"              % self._wing.name)
            self._write (aStream, "POSITION X,Y,Z=%s"           % "-0.38041 0.15083 2.78949")
            self._write (aStream, "BETRACHTUNGSWINKEL X,Y,Z=%s" % "22.00000 31.00000 0.00000")
            self._write (aStream, "ANSTELLWINKEL=%.5f"          % aoa)
            self._write (aStream, "CA=%.5f"                     % cl)
            self._write (aStream, "FLUG_GESCHWINDIGKEIT=%.5f"   % v)

            self.flaeche.write(aStream)

            self._write (aStream, self.end_tag)

            self.schalter.write(aStream)
            self.einstellungen.write(aStream)


    class FLAECHE (FLZ_Element):

        def __init__(self, *args, index=None):
            super().__init__(*args, index)

            self.profil = Export_FLZ.PROFIL (*args, self._wingSections[0])

            # segments: first right FLZ half wing 

            right_segments =[]
            for i,sec in enumerate(self._wingSections): 
                if i < len(self._wingSections) - 1:
                    new_segment = Export_FLZ.SEGMENT (*args, sec, self._wingSections[i+1])
                    right_segments.append(new_segment)  

            # segments: then left FLZ half wing 

            left_segments =[]
            for i,sec in reversed (list(enumerate(self._wingSections))): 
                if i < len(self._wingSections) - 1:
                    # flip section order 
                    new_segment = Export_FLZ.SEGMENT (*args, self._wingSections[i+1], sec)
                    left_segments.append(new_segment)  

            self.segments = left_segments + right_segments

            seg : Export_FLZ.SEGMENT
            for i, seg in enumerate (self.segments): 
                seg._index = i                       # put FLZ segment index into segments

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

            distrib = Export_FLZ.distrib_name_map [self._planform_paneled.wx_dist]
            mass    = 3                                                             # kg

            self._write (aStream, self.start_tag)
        
            self._write (aStream, "ART=FLUEGEL")
            self._write (aStream, "BEZEICHNUNG=%s"      % self._wing.name)
            self._write (aStream, "PROFILTIEFE=%.5f"    % (self._wing.planform.chord_root/1000))
            self._write (aStream, "BEZUGSPUNKT_PROFILTIEFE=%.5f"    % (0.0))
            self._write (aStream, "ANZAHL PANELS X=%d"  % self._planform_paneled.wx_panels)
            self._write (aStream, "VERTEILUNG=%s"       % distrib)
            self._write (aStream, "ANZAHL PANELS VOLUMENDARSTELLUNG=%s" % 30)
            self._write (aStream, "MASSE=%.5f"          % mass)

            self.profil.write (aStream)

            seg:  Export_FLZ.SEGMENT
            for seg in self.segments:
                seg.write(aStream) 

            self._write (aStream, self.end_tag)



    class SEGMENT (FLZ_Element):

        def __init__(self, wing : Wing, planform_paneled : Planform_Paneled, 
                     left_section: WingSection, right_section: WingSection, index= None):
            super().__init__(wing, planform_paneled, index)

            self.left_section  = left_section
            self.right_section = right_section

            # geht the right airfoil according to FLZ sequenze 

            if left_section.xn < right_section.xn:
                self.profil = Export_FLZ.PROFIL (wing, planform_paneled, right_section)
            else: 
                self.profil = Export_FLZ.PROFIL (wing, planform_paneled, left_section)

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

            distrib  = Export_FLZ.distrib_name_map[self._planform_paneled.wy_dist]

            if self.left_section.x < self.right_section.x:                     # right halfwing
                chord     = self.right_section.c / 1000
                flapGroup = self.left_section.flap_group
                wy_panels = self._planform_paneled.nx_panels_of_section (self.left_section.index())
            else:                                                               # left halfwing 
                chord     = self.left_section.c / 1000
                flapGroup = self.right_section.flap_group
                wy_panels = self._planform_paneled.nx_panels_of_section (self.right_section.index())
                # we have to flip FLZ SIN_R - SIN_L  on left side 
                if   distrib == "SIN_R": distrib = "SIN_L"
                elif distrib == "SIN_L": distrib = "SIN_R"

            width = (self.right_section.x - self.left_section.x) / 1000
            refChord = 0.0 
            twist = 0.0 
            dihedral = 0.0

            sweep = atan (abs((self.left_section.le_te()[0] - self.right_section.le_te()[0])/(1000 *width))) * 180 / pi
            refSweep = 0

            left_flap_depth  = self.left_section.flap_cn  * 100.0
            right_flap_depth = self.right_section.flap_cn * 100.0

            self._write (aStream, self.start_tag)

            self._write (aStream, "SEGMENTBREITE=%.5f"      %width)
            self._write (aStream, "PROFILTIEFE=%.5f"        % chord)
            self._write (aStream, "BEZUGSPUNKT_PROFILTIEFE=%.5f" % refChord)
            self._write (aStream, "VERWINDUNGSWINKEL=%.5f" % twist)
            self._write (aStream, "V-FORM_WINKEL=%.5f" % dihedral)
            self._write (aStream, "PFEILWINKEL=%.5f" % sweep)
            self._write (aStream, "BEZUGSPUNKT_PFEILWINKEL=%.5f" % refSweep)
            self._write (aStream, "ANZAHL PANELS Y=%d" % wy_panels)
            self._write (aStream, "VERTEILUNG=%s" % distrib)
            self._write (aStream, "KLAPPENTIEFE LINKS,RECHTS=%.5f %.5f" % (left_flap_depth, right_flap_depth))
            self._write (aStream, "KLAPPENGRUPPE=%d" % flapGroup)

            self.profil.write(aStream) 

            self._write (aStream, self.end_tag)


    class PROFIL (FLZ_Element):
        def __init__(self, wing, planform_paneled, section: WingSection):
            super().__init__ (wing, planform_paneled)

            self._index = None
            self._airfoil = section.airfoil
            if self._planform_paneled.use_nick_name:
                self._airfoil_name = section.airfoil_nick_name
            else:
                self._airfoil_name = section.airfoil.name


        def write (self, aStream):

            self._write (aStream, self.start_tag)
            self._write (aStream, f"PROFILDATEINAME={self._airfoil_name}.dat" )

            for i, xc in enumerate (self._airfoil.x):
                yc = self._airfoil.y[i]
                self._write (aStream, "PK%d=%.5f %.5f" % (i, xc, yc))

            self._write (aStream, self.end_tag)



    class SCHALTER (FLZ_Element):
        def __init__(self):

            self._index = None

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

            self._write (aStream, self.start_tag)

            self._write (aStream, "BUTTON CWI LOKAL=%s"             % "TRUE")
            self._write (aStream, "BUTTON GAMMA LOKAL=%s"           % "TRUE")
            self._write (aStream, "BUTTON CA LOKAL=%s"              % "TRUE")
            self._write (aStream, "CHECKBOX CA LOKAL MIN MAX=%s"    % "TRUE")

            self._write (aStream, self.end_tag)


    class EINSTELLUNGEN (FLZ_Element):
        def __init__(self):

            self._index = None

        def write (self, aStream):

            # SPLITTERPOS=1300
            self._write (aStream, self.start_tag)
            self._write (aStream, "SPLITTERPOS=%s"             % "1400")
            self._write (aStream, self.end_tag)



import ezdxf
from ezdxf import enums

class Export_Dxf:
    """ 

    Handle export of the planform and airfoils to dxf 
    Additionally export airfoils to file. 

    """
    def __init__(self, wing : Wing, myDict: dict = None):
 
        self._wing          = wing
        self._working_dir   = wing.workingDir  

        self._export_dir         = fromDict (myDict, "export_dir", "dxf")
        self._export_airfoils    = fromDict (myDict, "export_airfoils", True)


    def _as_dict (self) -> dict:
        """ returns a data dict with the paramters of self"""

        d = {}
        toDict (d, "export_dir",        self._export_dir) 
        toDict (d, "export_airfoils",   self._export_airfoils) 

        return d

    @property
    def _wingSections (self) -> WingSections: 
        return self._wing.planform.wingSections

    @property
    def export_dir(self):
        """the directory for flz export - path is relativ to current or absolute """
        return self._export_dir
    
    def set_export_dir(self, newStr): 
        # insure a valid, relativ path 
        self._export_dir = PathHandler (workingDir=self._working_dir).relFilePath (newStr)

    @property
    def base_and_export_dir(self):
        """the directory for dxf export including current dir """
        return PathHandler (workingDir=self._working_dir).fullFilePath (self.export_dir)

    @property
    def adapt_te_gap(self) -> bool: 
        return self._wing.export_airfoils.adapt_te_gap

    @property
    def te_gap_mm(self) -> float: 
        return self._wing.export_airfoils.te_gap_mm

    @property
    def export_airfoils(self) -> bool: return self._export_airfoils
    def set_export_airfoils(self, aBool): self._export_airfoils = aBool

    @property
    def n_airfoils (self) -> int:
        return self._wing.export_airfoils.n_airfoils
    
    @property
    def filename(self): 
        return self._wing.name.strip() +  '_wing.dxf'


    def do_it (self): 
        """ 
        main entry: start the export to the file defined in self parameters.
        Returns a message string what was done 
        """

        # plot the different parts in a dxf document 

        dxf = self.Dxf_Artist(self._wing)

        dxf.plot_planform()
        dxf.plot_hingeLine ()
        dxf.plot_wingSections ()
        dxf.plot_flapLines()
        dxf.plot_title ()
        dxf.plot_warning_polyline () 

        self._wingSections.do_strak (geometry_class=GEO_SPLINE)               # ensure strak airfoils are uptodate and splined (quality) 

        te_gap_mm = self.te_gap_mm if self.adapt_te_gap else None
        dxf.plot_airfoils (te_gap_mm=te_gap_mm)

        # save dxf document  

        targetDir = self.base_and_export_dir
        if not os.path.exists(targetDir): os.makedirs(targetDir)
        pathFileName = os.path.join (targetDir, self.filename)

        dxf.doc.saveas(pathFileName)  

        # export airfoils 

        if self.export_airfoils:
            self._wing.export_airfoils.do_it (toDir = targetDir)

        logger.info ("DXF file " + self.filename + " written to " + targetDir) 

        return 



    class Dxf_Artist:
        """ 
        - open an dxf document 
        - 'plots' different wing artefacts into dxf document 
        - save dxf document to file   
        """
        def __init__(self, wing : Wing): 

            self._wing = wing
            self._planform = wing.planform

            self.doc = ezdxf.new('R2010')
            self.msp = self.doc.modelspace()


        def _arr_to_poly (self, x,y):
            """ converts the two x,y arrays to an array of points (x,y) """
            poly = []
            for i, x in enumerate(x): 
                poly.append ((x, y[i]))
            return poly


        def _y_mirror (self, y_arr : list) -> list: 
            """ mirrors y values so that 
            - te point of root will be at 0,0 
            - le point of root will be at 0, rootchord"""
            mirrored_y = np.empty (len(y_arr))
            y_mirror = self._planform.chord_root / 2               # flip x  around half rootchord
            for i, y in enumerate(y_arr):
                mirrored_y [i] = y_mirror - (y - y_mirror)
            return mirrored_y


        def _plot_line_fromPoints (self, pointList ):
            """plots a (poly) line defined by an array of points """

            self.msp.add_lwpolyline (pointList)

        def _plot_line_fromArray (self, x: list , y:list ):
            """plots a (poly) line defined by two arrays x and y """

            self.msp.add_lwpolyline (self._arr_to_poly (x,y))

        # --------  public ----------------

        def plot_planform (self):
            
            x, le, te, = self._planform.le_te_polyline()

            # mirror the lines along span so that root-te will be at 0,0 
            le = self._y_mirror (le)
            te = self._y_mirror (te)

            # make a polygon for the planform contour from 0,0 - root - le - tip - te - root 
            x_c = [0.0]
            y_c = [0.0]
            x_c.extend(x)
            y_c.extend(le)
            x  = np.flip(x)
            te = np.flip(te)
            x_c.extend(x)
            y_c.extend(te)

            # insert into dxf doc
            self._plot_line_fromArray (x_c, y_c)


        def plot_hingeLine (self):

            x, y = self._planform.flaps.hinge_polyline()

            # mirror the lines along span so that root-te will be at 0,0 
            y = self._y_mirror (y)
            # insert into dxf doc
            self._plot_line_fromArray (x, y)


        def plot_flapLines (self):

            flaps = self._planform.flaps.get()

            flap : Flap
            for i, flap in enumerate (flaps):
                if i < (len (flaps) - 1):                           # no flap line at tip 

                    x, y = flap.line_right ()                       # only one line of flap box needed
                    y = self._y_mirror (y)
                    # insert into dxf doc
                    self._plot_line_fromArray (x, y)


        def plot_wingSections (self):
            # plot a little vertical marker line at wing section station above le 
            #  + airfoil nick name

            fontsize = (self._planform.chord_root / 230.0) * 7.0

            sec : WingSection
            for sec in self._planform.wingSections:

                _, le_te = sec.line()               # le from sec line - we have to mirror 
                y_m = self._y_mirror (le_te)[0] + 20  

                x_m = sec.x
                p1 = (x_m, y_m)
                p2 = (x_m, y_m + 15) 
                line = [p1,p2]
                self._plot_line_fromPoints (line)

                if sec.airfoil_nick_name:
                    self.msp.add_text(f"'{sec.airfoil_nick_name}'", height = fontsize).set_placement(
                                        (x_m, y_m+35), align=enums.TextEntityAlignment.CENTER)

                self.msp.add_text(f"{sec.airfoil.name}", height = fontsize).set_placement(
                                    (x_m, y_m+20), align=enums.TextEntityAlignment.CENTER)


        def plot_airfoils (self, te_gap_mm = None):
            # plot the airfoils in real size to the left of the planform 
            # an absolute Te gap is set    

            # ! here in dxf and airfoil coordinate system (not wing) !

            airfoil: Airfoil
            sec : WingSection
            for sec in self._planform.wingSections:

                te_gap = None 

                # te gap in mm? if yes scale it to normed ... do it
                if not te_gap_mm is None and te_gap_mm >= 0.0: 
                    te_gap  = te_gap_mm / sec.c
                    airfoil = sec.airfoil.asCopy ()
                    airfoil.geo.set_te_gap(te_gap)
                else: 
                    airfoil = sec.airfoil

                x = airfoil.x
                y = airfoil.y

                # scale to real size 
                x = x * sec.c
                y = y * sec.c

                x = x + sec.x - sec.c /4                # center t/4 above ypos of section 

                _, le_te = sec.line()                   # le from sec line - we have to mirror 
                y_m = self._y_mirror (le_te)[0] + 20 + 80 
                y = y + y_m                             # shift upward 

                self._plot_line_fromArray (x,y)

                # plot te gap info if it was set 
                if te_gap: 
                    # just above te a small text marker 
                    y_m = y[0] + 20 
                    x_m = x[0]
                    fontsize = 4 
                    self.msp.add_text(f"Te gap = {te_gap_mm:.1f} mm", height = fontsize).set_placement(
                                    (x_m, y_m), align=enums.TextEntityAlignment.CENTER)


        def plot_title (self):
            # plot wing name at the bottom
            y_m = - self._planform.chord_root * 0.2
            x_m = 0.0
            fontsize = (self._planform.chord_root / 230.0) * 10.0
            self.msp.add_text(self._wing.name, height = fontsize).set_placement(
                                (x_m, y_m), align=enums.TextEntityAlignment.TOP_LEFT)

            y_m = y_m - 20 
            fontsize = (self._planform.chord_root / 230.0) * 6.0
            self.msp.add_text("Generated by Planform Creator 2", height = fontsize).set_placement(
                                (x_m, y_m), align=enums.TextEntityAlignment.TOP_LEFT)


        def plot_warning_polyline (self):
            # plot warning that the planform is idealized as polyline
            y_m = self._planform.chord_root * 0.5
            x_m = self._planform.span * 0.4

            fontsize = (self._planform.chord_root / 230.0) * 5.0
            msg  = "The planform is idealized as a polyline. Convert to a spline for further processing."
            self.msp.add_text(msg, height = fontsize).set_placement(
                                (x_m, y_m), align=enums.TextEntityAlignment.MIDDLE_CENTER)


        def save (self, pathFileName):
            """ save the current dxf document to pathFileName"""  
            
            self.doc.saveas(pathFileName)  


