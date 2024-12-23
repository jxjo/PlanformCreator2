#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""

    Airfoil pytest classes

"""

import pytest

import numpy as np 
import os
import sys

# let python find the other modules in modules relativ to path of self  
sys.path.append('./modules')

from model.airfoil          import Airfoil, Airfoil_Bezier, GEO_BASIC, GEO_SPLINE
from model.airfoil_examples import Root_Example, Tip_Example
from model.airfoil_geometry import Geometry, Geometry_Splined, Geometry_Bezier
from model.airfoil_geometry import Curvature_of_xy, Curvature_of_Spline, Curvature_of_Bezier


class Test_Airfoil:

    def test_geo_basic (self): 

        airfoil = Root_Example(geometry = GEO_BASIC)
        geo : Geometry = airfoil.geo
        print (f"\n{airfoil} having {geo}")

        # base data 

        assert airfoil.nPoints == 200
        assert airfoil.nPanels == 199
        assert airfoil.isNormalized
        assert geo.te_gap == 0.03 / 100

        # thickness, camber 

        assert geo.thickness._get_maximum() == (0.2903642, 0.0764996)
        assert geo.camber._get_maximum()    == (0.4152061, 0.0170131)

        geo.set_max_thick  (0.08)
        assert round(geo.max_thick,4) == 0.08

        # with pytest.raises(NotImplementedError):
        geo.set_max_thick_x (0.40)

        geo.set_max_camb   (0.02)
        assert round(geo.max_camb,3) == 0.02

        # with pytest.raises(NotImplementedError):
        geo.set_max_camb_x  (0.30)

        # normalize, repanel 

        assert not geo.normalize(), "should be False because no normalizaton needed"
        assert geo.isNormalized
        with pytest.raises(NotImplementedError):
            geo.repanel()
        
        # curvature

        airfoil = Root_Example(geometry = GEO_BASIC)
        curv : Curvature_of_xy = airfoil.geo.curvature

        assert round(curv.upper._get_maximum()[1],0) == 317
        assert round(np.min (np.abs(curv.lower.y[-10:])),3) == 0.032

    
    def test_geo_splined (self): 


        airfoil = Root_Example(geometry = GEO_SPLINE)
        geo : Geometry_Splined = airfoil.geo
        print (f"\n{airfoil} having {geo}")

        # base data 

        assert airfoil.nPoints == 200
        assert airfoil.nPanels == 199
        assert not airfoil.isNormalized       
        assert geo.te_gap == 0.03 / 100 
        assert geo.le_real           == (0.0, 1.29e-05)

        # thickness, camber 

        assert geo.thickness._get_maximum() == (0.2903512, 0.076502)
        assert geo.camber._get_maximum()    == (0.4152475, 0.0170127)

        geo.set_max_thick  (0.08)
        assert round(geo.max_thick,4) == 0.08
        assert geo.x[geo.iLe] == 0.0 
        assert geo.y[geo.iLe] == 0.0 

        geo.set_max_thick_x (0.40)
        assert round(geo.max_thick_x,3) == 0.40

        geo.set_max_camb   (0.02)
        assert round(geo.max_camb,3) == 0.02

        geo.set_max_camb_x  (0.30)
        assert round(geo.max_camb_x,3) == 0.30

        assert geo.nPoints == 203, "nPoints changed with rebuild from upper and lower "


        # normalize, repanel 

        airfoil = Root_Example(geometry = GEO_SPLINE)
        geo : Geometry_Splined = airfoil.geo

        assert geo.le == (0.0, 0.0) 
        assert geo.le_real == (0.0, 1.29e-05)          

        assert geo.normalize(), "should be True because normalization needed"
        assert geo.isNormalized
        assert geo.le == (0.0, 0.0) 

        geo.repanel(nPanels=250)
        assert geo.nPanels == 250

        
        # curvature

        airfoil = Root_Example(geometry = GEO_SPLINE)
        curv : Curvature_of_Spline = airfoil.geo.curvature

        assert round(curv.upper._get_maximum()[1],0) == 317
        assert round(curv.lower._get_maximum()[1],0) == 372
        assert round(np.min (np.abs(curv.lower.y[-10:])),3) == 0.032



    def test_airfoil_geo_functions (self):

        airfoil = Root_Example(geometry = GEO_SPLINE)
        geo : Geometry_Splined = airfoil.geo

        # teGap 

        geo.set_te_gap (1.0 / 100)
        assert geo.te_gap == 1.0 / 100 
        
        # normalize, repanel 

        assert airfoil.normalize(), "should be True because normalizaton needed"
        assert airfoil.isNormalized

        #todo implement repanel 
        # geo.set_nPanelsNew (250) 
        # assert geo.nPanels == 250

        # thickness, camber 

        geo.set_max_thick (10.0 / 100)
        assert round(geo.max_thick,3) == 10.0 / 100 
        geo.set_max_thick_x (30.0 / 100)
        assert round(geo.max_thick_x,2) == 30.00 / 100
        geo.set_max_camb (3.0 / 100)
        assert round(geo.max_camb,3) == 3.0 / 100
        geo.set_max_camb_x (40.0 / 100)
        assert round(geo.max_camb_x,2) == 40.00 / 100

        # blend  - splined

        airfoil  = Airfoil (name="<blend>", geometry = GEO_SPLINE)
        airfoil.set_fileName (airfoil.name + '.dat')
        airfoil1 = Root_Example(geometry = GEO_SPLINE)
        airfoil2 = Tip_Example (geometry = GEO_SPLINE)

        # as airfoil1 and 2 are not normalized thickness would differ a little 
        airfoil1.normalize()
        airfoil2.normalize()

        airfoil.do_blend (airfoil1, airfoil2, blendBy=0.0)
        assert airfoil1.geo.max_thick == airfoil.geo.max_thick

        airfoil.do_blend (airfoil1, airfoil2, blendBy=0.5)
        assert airfoil.geo.max_thick == 7.3015 / 100
        y30_splined = airfoil.y[30]

        # airfoil.do_blend (airfoil1, airfoil2, blendBy=1.0)
        # assert airfoil.geo.max_camb == airfoil2.geo.max_camb

        # # blend  - basic 

        # airfoil  = Airfoil (name="<blend>", geometry = GEO_BASIC)
        # airfoil1 = Root_Example(geometry = GEO_BASIC)
        # airfoil2 = Tip_Example (geometry = GEO_BASIC)

        # airfoil.do_blend (airfoil1, airfoil2, blendBy=0.0)
        # assert airfoil.geo.max_thick == airfoil1.geo.max_thick

        # airfoil.do_blend (airfoil1, airfoil2, blendBy=1.0)
        # assert airfoil.geo.max_camb == airfoil2.geo.max_camb

        # airfoil.do_blend (airfoil1, airfoil2, blendBy=0.5)
        # assert airfoil.geo.max_thick == 7.30088 / 100

        # airfoil.do_blend (airfoil1, airfoil2, blendBy=0.5, geometry=GEO_SPLINE)
        # assert airfoil.y[30] == y30_splined 



    def test_airfoil_file_functions (self):

        from pathlib import Path
        import shutil
        import numpy as np 


        airfoil = Root_Example(geometry = GEO_SPLINE)
        p_tmp = Path.cwd() / 'tmp'
        airfoil.normalize()

        # copyAs with te gap 

        te_gap =  0.01
        destName = 'huhu'
        newPathFileName = airfoil.save_copyAs (dir=str(p_tmp), destName=destName, te_gap=te_gap)

        new_airfoil = Airfoil (pathFileName=newPathFileName)
        new_airfoil.load()
        new_airfoil.normalize()

        assert new_airfoil.geo.te_gap == te_gap
        assert new_airfoil.name  == destName + "_te1.0"

        # after set_teGap x-values should be equal - y-values different
        assert np.sum(np.round(airfoil.x,7)) == np.sum(new_airfoil.x)
        assert np.sum(np.round(airfoil.y)) != np.sum(new_airfoil.y)

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

        assert airfoil.nPoints == 161
        assert airfoil.nPanels == 160
        assert airfoil.isNormalized       
        assert airfoil.geo.te_gap == 0.0
        assert geo.le_real == (0.0, 0.0)

        # u default distribution  

        assert len(geo.upper._u) == 81
        assert round(sum(geo.upper._u),6) == 40.688019

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

        assert geo.thickness._get_maximum() == (0.3140431, 0.1110653)
        assert geo.camber._get_maximum()    == (0.3974017, 0.0140231)

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

        assert round(curv.upper._get_maximum()[1],0) == 61.0
        assert round(curv.lower._get_maximum()[1],0) == 105.0

        te_curv = curv.lower.y[-10:]
        assert round(np.min (np.abs(te_curv)),3) == 0.062
    


# Main program for testing 
if __name__ == "__main__":

    test = Test_Airfoil()
    test.test_geo_basic()
    test.test_geo_splined()
    test.test_airfoil_geo_functions ()
    test.test_airfoil_file_functions ()

    test = Test_Airfoil_Bezier()
    test.test_geo_bezier ()

