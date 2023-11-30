#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""

    Airfoil pytest classes

"""

import pytest
from airfoil2_examples import Root_Example
from airfoil2_geometry import Geometry, Geometry_Spline

class TestAirfoil:

    def test_geo_basic (self): 

        airfoil = Root_Example(geometryClass = Geometry)
        geo = airfoil.geo
        print (f"\n{airfoil} having {geo}")

        assert airfoil.nPoints == 200
        assert airfoil.nPanels == 199
        assert airfoil.isNormalized
        
        assert airfoil.teGap == 0.03

        print ("Thickness: ", geo.thickness.maximum )
        print ("Camber:    ", geo.camber.maximum )
        assert geo.thickness.maximum == (0.2853921, 0.0764922 )
        assert geo.camber.maximum    == (0.4097331, 0.0170114 )


    def test_geo_spline (self): 

        airfoil = Root_Example(geometryClass = Geometry_Spline)
        geo = airfoil.geo
        print (f"\n{airfoil} having {geo}")
        assert airfoil.nPoints == 200
        assert airfoil.nPanels == 199
        assert airfoil.isNormalized
        
        assert airfoil.teGap == 0.03

        assert geo.le_real           == (0.0, 1.29e-05)
        assert geo.thickness.maximum == (0.2903519, 0.076502 )
        assert geo.camber.maximum    == (0.4152486, 0.0170127 )
        print ("LE:        ", geo.le )
        print ("LE_real:   ", geo.le_real )
        print (f"\nSet camber & thickness")
        print ("Thickness: ", geo.thickness.maximum )
        print ("Camber:    ", geo.camber.maximum )

        assert not geo._normalize(), "should be False because no normalizaton needed"
        assert geo.isNormalized

        # set thickness and camber 

        geo.set_maxThick  (0.08)
        assert geo.maxThick == 0.08

        geo.set_maxThickX (0.40)
        assert round(geo.maxThickX,4) == 0.40

        geo.set_maxCamb   (0.02)
        assert geo.maxCamb == 0.02

        geo.set_maxCambX  (0.30)
        assert round(geo.maxCambX,4) == 0.30

        assert geo.nPoints == 203, "nPoints changed with rebuild from upper and lower "

        print ("Thickness: ", geo.thickness.maximum )
        print ("Camber:    ", geo.camber.maximum )
        



# Main program for testing 
if __name__ == "__main__":

    test = TestAirfoil()
    test.test_geo_basic()
    test.test_geo_spline()

