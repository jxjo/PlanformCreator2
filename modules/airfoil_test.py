#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""

    Airfoil pytest classes

"""

import pytest
from airfoil2_examples import Root_Example
from airfoil2_geometry import Geometry_Basic, Geometry_Spline

class TestAirfoil:

    def test_geo_basic (self, capsys): 

        airfoil = Root_Example(geometryClass = Geometry_Basic)
        assert airfoil.nPoints == 200
        assert airfoil.nPanels == 199
        assert airfoil.isNormalized
        
        assert airfoil.teGap == 0.03

        geo = airfoil.geo
        with capsys.disabled():
            print ()
            print ("Thickness: ", geo.thickness.maximum )
            print ("Camber:    ", geo.camber.maximum )
        assert geo.thickness.maximum == (0.2853921, 0.0764922 )
        assert geo.camber.maximum    == (0.4097331, 0.0170114 )


    def test_geo_spline (self, capsys): 

        airfoil = Root_Example(geometryClass = Geometry_Spline)
        geo = airfoil.geo
        assert airfoil.nPoints == 200
        assert airfoil.nPanels == 199
        with capsys.disabled():
            print ()
            print ("LE: ", geo.le )
        assert not airfoil.isNormalized
        
        assert airfoil.teGap == 0.03

        with capsys.disabled():
            print ()
            print ("Thickness: ", geo.thickness.maximum )
            print ("Camber:    ", geo.camber.maximum )
        assert geo.thickness.maximum == (0.2853921, 0.0764922 )
        assert geo.camber.maximum    == (0.4097331, 0.0170114 )

