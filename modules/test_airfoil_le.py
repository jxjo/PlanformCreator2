#!/usr/bin/env python
# -*- coding: utf-8 -*-

from airfoil import Airfoil 
import numpy as np
from airfoil_splines import SplineOfAirfoil, _cosinus_distribution


def le_max_curvature():
    
    from os import listdir
    from os.path import isfile, join
    from scipy.optimize import fmin
    import matplotlib.pyplot as plt



    airfoil_dir =".\\test_airfoils"
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
        print (printName)
        print ("\t", "umax     : %.4f   curv: %.2f" % (umaxCurve, maxCurv)) 
        print ("\t", "uLe      : %.4f   curv: %.2f" % (spl.uLe, spl.curvatureFn (spl.uLe)), "   %.6f  %.6f" %airfoil.le_fromPoints)

        result  = fmin(lambda u : - abs(spl.curvatureFn (u)), spl.uLe, ftol=0.1, disp=False, full_output=True)
        umax = result[0][0]
        maxCurv = result[1]

        result = spl.xyFn(umax)
        xle = result[0][()]
        yle = result[1][()]
        print ("\t", "umax-real: %.4f   curv: %.2f" % (umax, abs(maxCurv)), "   %.6f  %.6f" %(xle,yle))

        u_new_upper = np.abs (np.flip(_cosinus_distribution (0.01, 1.2, 81)) -1) * umax
        u_new_lower = _cosinus_distribution (0.01, 1.2, 81) * (1- umax) + umax

        u_new = np.concatenate ((u_new_upper, u_new_lower[1:]))
        print (len(u_new))
        result = spl.xyFn(u_new) 
        xnew = result[0]  
        ynew = result[1]   
        # break 
       
    
        fig, axa = plt.subplots(1, 1, figsize=(7,7))
        fig.subplots_adjust(left=0.08, bottom=0.05, right=0.95, top=0.95, wspace=None, hspace=0.15)

        ax1 = axa # [0]
        # ax2 = axa[1]

        # ax1.axis('equal')
        ax1.set_xlim([ -0.002, 0.01])
        ax1.set_ylim([ -0.007, 0.007])

        ax1.grid(True)
        # ax2.grid(True)

        ax1.plot(airfoil.x, airfoil.y, '-', marker='o', lw=1, fillstyle='none', markersize=4, label=airfoil.name)
        ax1.plot(xnew     , ynew     , '-', marker='o', lw=1, fillstyle='none', markersize=4, label=airfoil.name + ' repan')
        ax1.plot(xle      , yle      , '-', marker='o', lw=1, color = 'red' ,   markersize=4, label=airfoil.name + ' repan')

        ax1.legend()
        # ax2.legend()
        plt.show()

def le_min_x():
    
    from os import listdir
    from os.path import isfile, join
    from scipy.optimize import fmin
    import matplotlib.pyplot as plt

    airfoil_dir =".\\test_airfoils"
    airfoil_files = [join(airfoil_dir, f) for f in listdir(airfoil_dir) if isfile(join(airfoil_dir, f))]

    for airPath in airfoil_files:
        airfoil = Airfoil (pathFileName=airPath)
        airfoil.load()
        printName = airfoil.name.ljust(30)
    
        spl = SplineOfAirfoil (airfoil.x, airfoil.y)
        xLe, yLe = airfoil.le_fromPoints
        print (printName)
        print ("\t", "Le     x y   %.6f  %.6f" %(xLe, yLe))

        # 1 respline with current le and lower res to norm LE area

        u_1_upper = np.abs (np.flip(_cosinus_distribution (0.05, 1.2, 51)) -1) * spl.uLe
        u_1_lower = _cosinus_distribution (0.05, 1.2, 51) * (1- spl.uLe) + spl.uLe

        u_1 = np.concatenate ((u_1_upper, u_1_lower[1:]))
        result = spl.xyFn(u_1) 
        x1 = result[0]  
        y1 = result[1]   
        spl1 = SplineOfAirfoil (x1,y1)

        # find now minimum x  --> new LE

        result  = fmin(lambda u :  abs(spl1.xFn (u)), spl1.uLe, ftol=0.00001, disp=False, full_output=True)
        ule = result[0][0]
        xLe2 = result[1]
        result = spl.xyFn(ule)
        yLe2 = result[1][()]

        print ("\t", "Le min x y   %.6f  %.6f" %(xLe2, yLe2))

        # 2 respline with new LE with higher resultion 

        u_2_upper = np.abs (np.flip(_cosinus_distribution (0.08, 0.8, 101)) -1) * ule
        u_2_lower = _cosinus_distribution (0.08, 0.8, 101) * (1- ule) + ule

        u_2 = np.concatenate ((u_2_upper, u_2_lower[1:]))
        result = spl.xyFn(u_2) 
        x2 = result[0]  
        y2 = result[1] 
        spl2 = SplineOfAirfoil (x2,y2)
       
    
        fig, axa = plt.subplots(1, 2, figsize=(16,7))
        fig.subplots_adjust(left=0.08, bottom=0.05, right=0.95, top=0.95, wspace=None, hspace=0.15)

        ax1 = axa[0]
        ax2 = axa[1]

        # ax1.axis('equal')
        ax1.set_xlim([ -0.002, 0.01])
        ax1.set_ylim([ -0.007, 0.007])

        ax1.grid(True)
        # ax2.grid(True)

        ax1.plot(airfoil.x, airfoil.y, '-', marker='D', lw=1, fillstyle='none', markersize=4, label=airfoil.name)
        ax1.plot(xLe      , yLe      , '-', marker='D', lw=1, color = 'blue' ,   markersize=6, label=airfoil.name + ' LE')
        # ax1.plot(x1       , y1       , '-', marker='o', lw=1, fillstyle='none', markersize=4, label=airfoil.name + ' repan 1')
        ax1.plot(x2       , y2       , '-', marker='o', lw=1, fillstyle='none', markersize=4, label=airfoil.name + ' repan 2')
        ax1.plot(xLe2     , yLe2     , '-', marker='o', lw=1, color = 'red' ,   markersize=4, label=airfoil.name + ' LE repan ')
 
        ax2.axis('equal')
        # ax2.plot(airfoil.x, airfoil.y, '-', marker='D', lw=1, fillstyle='none', markersize=4, label=airfoil.name)
        ax2.plot(x2       , y2       , '-', marker='o', lw=1, fillstyle='none', markersize=4, label=airfoil.name + ' repan 2')
  
        ax1.legend()
        ax2.legend()
        plt.show()

if __name__ == "__main__":

    # ---- Test -----
    le_min_x()