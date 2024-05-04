#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""

    Airfoil pytest classes

"""

import pytest

import numpy as np 

from airfoil import Airfoil, Airfoil_Bezier, GEO_BASIC, GEO_SPLINE
from airfoil_examples import Root_Example, Tip_Example
from airfoil_geometry import Geometry, Geometry_Splined, Geometry_Bezier
from airfoil_geometry import Curvature_of_xy, Curvature_of_Spline, Curvature_of_Bezier


class Test_Airfoil:

    def test_geo_basic (self): 

        airfoil = Root_Example(geometry = GEO_BASIC)
        geo = airfoil.geo
        print (f"\n{airfoil} having {geo}")

        # base data 

        assert airfoil.nPoints == 200
        assert airfoil.nPanels == 199
        assert airfoil.isNormalized
        assert airfoil.teGap_perc == 0.03

        # thickness, camber 

        assert geo.thickness.maximum == (0.2903642, 0.0764996)
        assert geo.camber.maximum    == (0.4152061, 0.0170131)

        geo.set_maxThick  (0.08)
        assert round(geo.maxThick,4) == 0.08

        with pytest.raises(NotImplementedError):
            geo.set_maxThickX (0.40)

        geo.set_maxCamb   (0.02)
        assert round(geo.maxCamb,3) == 0.02

        with pytest.raises(NotImplementedError):
            geo.set_maxCambX  (0.30)

        # normalize, repanel 

        assert not geo.normalize(), "should be False because no normalizaton needed"
        assert geo.isNormalized
        with pytest.raises(NotImplementedError):
            geo.repanel()
        
        # curvature

        airfoil = Root_Example(geometry = GEO_BASIC)
        curv : Curvature_of_xy = airfoil.geo.curvature

        assert round(curv.upper.maximum[1],0) == 317
        assert round(np.min (np.abs(curv.lower.y[-10:])),3) == 0.032

    
    def test_geo_splined (self): 


        airfoil = Root_Example(geometry = GEO_SPLINE)
        geo : Geometry_Splined = airfoil.geo
        print (f"\n{airfoil} having {geo}")

        # base data 

        assert airfoil.nPoints == 200
        assert airfoil.nPanels == 199
        assert not airfoil.isNormalized       
        assert airfoil.teGap_perc == 0.03
        assert geo.le_real           == (0.0, 1.29e-05)

        # thickness, camber 

        assert geo.thickness.maximum == (0.2903414, 0.076502)
        assert geo.camber.maximum    == (0.4153504, 0.0170052)

        geo.set_maxThick  (0.08)
        assert round(geo.maxThick,4) == 0.08
        assert geo.x[geo.iLe] == 0.0 
        assert geo.y[geo.iLe] == 0.0 

        geo.set_maxThickX (0.40)
        assert round(geo.maxThickX,3) == 0.40

        geo.set_maxCamb   (0.02)
        assert round(geo.maxCamb,3) == 0.02

        geo.set_maxCambX  (0.30)
        assert round(geo.maxCambX,3) == 0.30

        assert geo.nPoints == 200, "nPoints changed with rebuild from upper and lower "


        # normalize, repanel 

        airfoil = Root_Example(geometry = GEO_SPLINE)
        geo : Geometry_Splined = airfoil.geo

        assert geo.le == (0.0, 0.0) 
        assert geo.le_real == (0.0, 1.29e-05)          

        assert geo.normalize(), "should be True because normalizaton needed"
        assert geo.isNormalized
        assert geo.le == (0.0, 0.0) 

        geo.repanel(nPanels=250)
        assert geo.nPanels == 250

        
        # curvature

        airfoil = Root_Example(geometry = GEO_SPLINE)
        curv : Curvature_of_Spline = airfoil.geo.curvature

        assert round(curv.upper.maximum[1],0) == 317
        assert round(curv.lower.maximum[1],0) == 372
        assert round(np.min (np.abs(curv.lower.y[-10:])),3) == 0.032



    def test_airfoil_geo_functions (self):

        airfoil = Root_Example(geometry = GEO_SPLINE)

        # teGap 

        airfoil.set_teGap_perc (1.0)
        assert airfoil.teGap_perc == 1.0
        
        # normalize, repanel 

        assert not airfoil.normalize(), "should be False because no normalizaton needed"
        assert airfoil.isNormalized

        airfoil.set_nPanelsNew (250) 
        assert airfoil.nPanels == 250

        # thickness, camber 

        airfoil.set_maxThickness (10.0)
        assert round(airfoil.maxThickness,3) == 10.0
        airfoil.set_maxThicknessX (30.0)
        assert round(airfoil.maxThicknessX,2) == 30.00
        airfoil.set_maxCamber (3.0)
        assert round(airfoil.maxCamber,3) == 3.0
        airfoil.set_maxCamberX (40.0)
        assert round(airfoil.maxCamberX,2) == 40.00

        # strak  - splined

        airfoil  = Airfoil (name="<strak>", geometry = GEO_SPLINE)
        airfoil1 = Root_Example(geometry = GEO_SPLINE)
        airfoil2 = Tip_Example (geometry = GEO_SPLINE)

        # as airfoil1 and 2 are not normalized thickness would differ a little 
        airfoil1.normalize()
        airfoil2.normalize()

        airfoil.do_strak (airfoil1, airfoil2, blendBy=0.0)
        assert airfoil1.maxThickness == airfoil.maxThickness

        airfoil.do_strak (airfoil1, airfoil2, blendBy=0.5)
        assert airfoil.maxThickness == 7.30151
        y30_splined = airfoil.y[30]

        airfoil.do_strak (airfoil1, airfoil2, blendBy=1.0)
        assert airfoil.maxCamber == airfoil2.maxCamber

        # strak  - basic 

        airfoil  = Airfoil (name="<strak>", geometry = GEO_BASIC)
        airfoil1 = Root_Example(geometry = GEO_BASIC)
        airfoil2 = Tip_Example (geometry = GEO_BASIC)

        airfoil.do_strak (airfoil1, airfoil2, blendBy=0.0)
        assert airfoil.maxThickness == airfoil1.maxThickness

        airfoil.do_strak (airfoil1, airfoil2, blendBy=1.0)
        assert airfoil.maxCamber == airfoil2.maxCamber

        airfoil.do_strak (airfoil1, airfoil2, blendBy=0.5)
        assert airfoil.maxThickness == 7.30088

        airfoil.do_strak (airfoil1, airfoil2, blendBy=0.5, geometry=GEO_SPLINE)
        assert airfoil.y[30] == y30_splined 



    def test_airfoil_file_functions (self):

        from pathlib import Path
        import shutil
        import numpy as np 


        airfoil = Root_Example(geometry = GEO_SPLINE)
        p_tmp = Path.cwd() / 'tmp'
        airfoil.normalize()

        # copyAs with te gap 

        teGap_perc = 1
        destName = 'huhu'
        newPathFileName = airfoil.save_copyAs (dir=str(p_tmp), destName=destName, teGap=teGap_perc/100)

        new_airfoil = Airfoil (pathFileName=newPathFileName)
        new_airfoil.load()
        new_airfoil.normalize()

        assert new_airfoil.teGap_perc == teGap_perc
        assert new_airfoil.name  == destName + "_te=1.00"

        # after set_teGap x-values should be equal - y-values different
        assert np.sum(airfoil.x) == np.sum(new_airfoil.x)
        assert np.sum(airfoil.y) != np.sum(new_airfoil.y)

        # saveAs 

        airfoil = Root_Example(geometry = GEO_SPLINE)

        destName = 'haha'
        newPathFileName = airfoil.saveAs (dir=str(p_tmp), destName=destName)
        new_airfoil = Airfoil (pathFileName=newPathFileName)
        new_airfoil.load()


        shutil.rmtree(str(p_tmp))



class Test_Airfoil_Bezier:


    def test_geo_bezier (self): 

        airfoil = Airfoil_Bezier()
        geo : Geometry_Bezier = airfoil.geo
        print (f"\n{airfoil} having {geo}")

        # base data 

        assert airfoil.nPoints == 201
        assert airfoil.nPanels == 200
        assert airfoil.isNormalized       
        assert airfoil.teGap_perc == 0.0
        assert geo.le_real == (0.0, 0.0)

        # u default distribution  

        assert len(geo.upper._u) == 101
        assert round(sum(geo.upper._u),6) == 50.732829

        # eval y on u

        checksum = 0.0 
        u = np.linspace (0.0, 1.0, 10)
        y = [0] * 10 
        for i in range(len(u)):
            y[i] = geo.upper.bezier.eval_y (u[i])
            checksum += y[i]
        print (y)

        # eval y on x

        checksum = 0.0 
        x = np.linspace (0.0, 1.0, 10)
        y = [0] * 10 
        for i in range(len(x)):
            y[i] = geo.upper.yFn (x[i])
            checksum += y[i]
        print (y)

        
        # thickness, camber 

        assert geo.thickness.maximum == (0.3140345, 0.111065)
        assert geo.camber.maximum    == (0.3973174, 0.014023)

        with pytest.raises(NotImplementedError):
            geo.set_maxThick  (0.08)


        # normalize, repanel 

        assert not geo.normalize(), "should be False because no normalizaton needed"
        assert geo.isNormalized

        geo.repanel(nPanels=250)
        assert geo.nPanels == 250

        
        # curvature

        airfoil = Airfoil_Bezier()
        curv : Curvature_of_Bezier = airfoil.geo.curvature

        assert round(curv.upper.maximum[1],0) == 61.0
        assert round(curv.lower.maximum[1],0) == 105.0

        te_curv = curv.lower.y[-10:]
        assert round(np.min (np.abs(te_curv)),3) == 0.062
    


# Main program for testing 
if __name__ == "__main__":

    test = Test_Airfoil()
    test.test_geo_basic()
    test.test_geo_splined()

