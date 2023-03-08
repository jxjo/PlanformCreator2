#!/usr/bin/env python

import xml.etree.ElementTree as ET
import os, re
import io
from copy import deepcopy
from common_utils import *
from wing_model import Wing, WingSection, Planform_Paneled


class Export_Xflr5:
    """ 

    Handle export of a paneled planform to an Xflr5 xml fil   

    """

    distrib_name_map ={}
    distrib_name_map ["uniform"] = "UNIFORM"
    distrib_name_map ["-sine"]   = "INVERSE SINE"
    distrib_name_map ["sine"]    = "SINE"
    distrib_name_map ["cosine"]  = "COSINE"


    def export_wing (self, wing : Wing, planform : Planform_Paneled):

        # get file object with xflr xml templae 
        templateFile = Xflr5_template().get_template_wing()

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
            nameXml.text = wing.name
        for descriptionXml in wingXml.iter('Description'):
            descriptionXml.text = "by Planform Creator 2"

        # find sections-data-template
        for sectionTemplateXml in wingXml.iter('Sections'):
            # copy the template
            newSectionXml = deepcopy(sectionTemplateXml)

            # remove the template
            wingXml.remove(sectionTemplateXml)

        # write the new section-data to the wing
        section : WingSection

        for iSec, section in enumerate(wing.wingSections):
            # copy the template
            newSectionXml = deepcopy(sectionTemplateXml)

            # x
            for x_number_of_panels in newSectionXml.iter('x_number_of_panels'):
                x_number_of_panels.text = str(planform.x_panels)
            for x_panel_distribution in newSectionXml.iter('x_panel_distribution'):
                # map to xflr5 distribution names 
                xflr5_dist = self.distrib_name_map[planform.x_dist]
                x_panel_distribution.text = str(xflr5_dist)

            # y
            for y_number_of_panels in newSectionXml.iter('y_number_of_panels'):
                y_number_of_panels.text = str(planform.y_panels_forSection(iSec))
            for y_panel_distribution in newSectionXml.iter('y_panel_distribution'):
                # map to xflr5 distribution names 
                xflr5_dist = self.distrib_name_map[planform.y_dist]
                y_panel_distribution.text = str(xflr5_dist)

            for yPosition in newSectionXml.iter('y_position'):
                yPosition.text = str(section.yPos)

            for chord in newSectionXml.iter('Chord'):
                chord.text = str(section.chord)

            for xOffset in newSectionXml.iter('xOffset'):
                le, te = planform._planform_function (section.yPos)
                xOffset.text = str(le)

            for dihedral in newSectionXml.iter('Dihedral'):
                # tbd
                dihedral.text = str(0)

            # airfoil - use nick name? 
            if wing.xflr5UseNick:
                airfoilName = section.airfoilNick()
            else: 
                airfoilName = section.airfoilName()
            for foilName in newSectionXml.iter('Left_Side_FoilName'):
                foilName.text = re.sub('.dat', '', airfoilName)

            for foilName in newSectionXml.iter('Right_Side_FoilName'):
                foilName.text = re.sub('.dat', '', airfoilName)

            # add the new section to the tree
            wingXml.append(newSectionXml)

        # delete existing file, write all data to the new file

        xflr5Dir  = 'to_Xflr5'
        xflr5File = wing.name.strip() +  '_wing.xml'
        if xflr5Dir and not os.path.isdir (xflr5Dir):
            os.mkdir(xflr5Dir)

        xflr5FilePath = os.path.join (xflr5Dir, xflr5File)
        tree.write(xflr5FilePath)

        InfoMsg("XFLR5 data was successfully written to %s." % xflr5FilePath)

        return xflr5FilePath


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


class Xflr5_fin_template ():
    """ A xflr5 template to use for export"""

    def _template (self):
        """ returns the template file as a string file"""

        output = io.StringIO()
        output.write (self.name)
        output.write ("""
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE explane>
<explane version="1.0">
    <Units>
        <length_unit_to_meter>1</length_unit_to_meter>
        <mass_unit_to_kg>1</mass_unit_to_kg>
    </Units>
    <Plane>
        <Name>First_F3F</Name>
        <Description></Description>
        <Inertia>
            <Point_Mass>
                <Tag></Tag>
                <Mass>  0.410</Mass>
                <coordinates>      -0.100,           0,           0</coordinates>
            </Point_Mass>
        </Inertia>
        <has_body>false</has_body>
        <wing>
            <Name>Fin</Name>
            <Type>FIN</Type>
            <Color>
                <red>172</red>
                <green>133</green>
                <blue>225</blue>
                <alpha>255</alpha>
            </Color>
            <Description></Description>
            <Position>      1.095,           0,           0</Position>
            <Tilt_angle>  0.000</Tilt_angle>
            <Symetric>true</Symetric>
            <isFin>true</isFin>
            <isDoubleFin>true</isDoubleFin>
            <isSymFin>false</isSymFin>
            <Inertia>
                <Volume_Mass>  0.085</Volume_Mass>
            </Inertia>
            <Sections>
                <Section>
                    <y_position>  0.000000</y_position>
                    <Chord>  0.110000</Chord>
                    <xOffset>  0.000000</xOffset>
                    <Dihedral>  40.000000</Dihedral>
                    <Twist>  0.000</Twist>
                    <x_number_of_panels>13</x_number_of_panels>
                    <x_panel_distribution>COSINE</x_panel_distribution>
                    <y_number_of_panels>2</y_number_of_panels>
                    <y_panel_distribution>UNIFORM</y_panel_distribution>
                    <Left_Side_FoilName>Fin_1</Left_Side_FoilName>
                    <Right_Side_FoilName>Fin_1</Right_Side_FoilName>
                </Section>
            </Sections>
        </wing>
    </Plane>
</explane>""")
