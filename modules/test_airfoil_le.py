#!/usr/bin/env python
# -*- coding: utf-8 -*-

from airfoil import Airfoil 
import numpy as np
from airfoil_splines import SplineOfAirfoil


def check_airfoils():
    
    from os import listdir
    from os.path import isfile, join
    from scipy.optimize import fmin, brentq


    airfoil_dir ="modules\\test_airfoils"
    airfoil_files = [join(airfoil_dir, f) for f in listdir(airfoil_dir) if isfile(join(airfoil_dir, f))]

    for airPath in airfoil_files:
        airfoil = Airfoil (pathFileName=airPath)
        airfoil.load()
    
        spl = SplineOfAirfoil (airfoil.x, airfoil.y)
        maxCurv = np.max (spl.curvature)
        imaxCurv = np.argmax (spl.curvature)
        umaxCurve = spl._u [imaxCurv]

        printName = airfoil.name.ljust(30)
        u = 0.5
        # print (printName, "\t cmax: %.2f" % maxCurv, "\t", airfoil.le_fromPoints, "\t", airfoil.te_fromPoints)
        print (printName,  airfoil.le_fromPoints)
        print ("\t", "umax     : %.4f   curv: %.2f" % (umaxCurve, maxCurv)) 
        print ("\t", "uLe      : %.4f   curv: %.2f" % (spl.uLe, spl.curvatureFn (spl.uLe)))

        result  = fmin(lambda u : - abs(spl.curvatureFn (u)), spl.uLe, ftol=0.1, disp=True, full_output=True)
        umax = result[0][0]
        maxCurv = result[1]
        print ("\t", "umax-real: %.4f   curv: %.2f" % (umax, abs(maxCurv)))

if __name__ == "__main__":

    # ---- Test -----
    check_airfoils()