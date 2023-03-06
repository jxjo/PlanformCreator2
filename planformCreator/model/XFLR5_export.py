#!/usr/bin/env python

#  This file is part of "The Strak Machine".

#  "The Strak Machine" is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.

#  "The Strak Machine" is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.

#  You should have received a copy of the GNU General Public License
#  along with "The Strak Machine".  If not, see <http://www.gnu.org/licenses/>.

#  Copyright (C) 2020-2022 Matthias Boese

import xml.etree.ElementTree as ET
import os, re
import io
from copy import deepcopy
from common_utils import *

# Scale from mm --> m
scaleFactor = (1.0/1000.0)

# Minimum chord (in mm) in case chord is exactly 0.0
min_chord = 2.0

def get_wing(root, wingFinSwitch):
    for wing in root.iter('wing'):
        for XMLwingFinSwitch in wing.iter('isFin'):
            # convert string to boolean value
            if (XMLwingFinSwitch.text == 'true') or (XMLwingFinSwitch.text == 'True'):
                value = True
            else:
                value = False

            # check against value of wingFinswitch
            if (value == wingFinSwitch):
                return wing

def export_toXFLR5(data, FileName, xPanels, yPanels):

    # read in all the data
    InfoMsg("Reading data from XFLR5 template file %s" % FileName)

    # basically parse the XML-file
    tree = ET.parse(FileName)

    # get root of XML-tree
    root = tree.getroot()

    # find wing-data
    wing = get_wing(root, data.params.isFin)

    if (wing == None):
        ErrorMsg("wing not found\n")
        return -1

    # find sections-data-template
    for sectionTemplate in wing.iter('Sections'):
        # copy the template
        newSection = deepcopy(sectionTemplate)

        # remove the template
        wing.remove(sectionTemplate)

    # write the new section-data to the wing
    for section in data.sections:
        # copy the template
        newSection = deepcopy(sectionTemplate)

        # enter the new data
        for x_number_of_panels in newSection.iter('x_number_of_panels'):
            x_number_of_panels.text = str(xPanels)

        for y_number_of_panels in newSection.iter('y_number_of_panels'):
            y_number_of_panels.text = str(yPanels)

        for yPosition in newSection.iter('y_position'):
            # convert float to text
            yPosition.text = str(section.y * scaleFactor)

        for chord in newSection.iter('Chord'):
            # limit chord to values >= 1 mm
            chord_float = max(min_chord, section.chord)
            # scale chord to m
            chord_float *= scaleFactor
            # convert float to text
            chord.text = str(chord_float)

        for xOffset in newSection.iter('xOffset'):
            # convert float to text
            xOffset.text = str(section.leadingEdge * scaleFactor)

        for dihedral in newSection.iter('Dihedral'):
            # convert float to text
            dihedral.text = str(section.dihedral)

        for foilName in newSection.iter('Left_Side_FoilName'):
            foilName.text = re.sub('.dat', '', section.airfoilName)

        for foilName in newSection.iter('Right_Side_FoilName'):
            foilName.text = re.sub('.dat', '', section.airfoilName)

        # add the new section to the tree
        wing.append(newSection)
        if (section.chord > 0.0):
            hingeDepthPercent = (section.flapDepth /section.chord )*100
        else:
            hingeDepthPercent = 0.0
        TraceMsg("Section %d: position: %.0f mm, chordlength %.0f mm, hingeDepth %.1f  %%, airfoilName %s was inserted" %
          (section.number, section.y, section.chord, hingeDepthPercent, section.airfoilName))

    # delete existing file, write all data to the new file
    os.remove(FileName)
    tree.write(FileName)
    InfoMsg("XFLR5 data was successfully written to %s." % FileName)
    return 0


class Xflr5_template ():
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
            <Tilt_angle>  0.100</Tilt_angle>
            <Symetric>true</Symetric>
            <isFin>false</isFin>
            <isDoubleFin>false</isDoubleFin>
            <isSymFin>false</isSymFin>
            <Inertia>
                <Volume_Mass>  1.700</Volume_Mass>
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
