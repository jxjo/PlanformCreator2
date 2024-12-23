#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""

    Spline pytest classes

"""

import pytest

import numpy as np 
from spline import *

class Test_Spline:

    def test_spline_1D (self): 

        x = [  0, 0.5,  2,  3,  4,  5,  7]  
        y = [  0,  3,  0,  2,  0,  2,  0]  

        # scalar 
        spl = Spline1D (x,y, boundary="natural")
        xnew = 1.0
        ynew = spl.eval(xnew, der=0) 
        assert ynew == 2.9546299523643555
        print ("1D natural ", xnew, ynew)


        # scalar 
        spl = Spline1D (x,y, boundary="notaknot")
        xnew = 1.0
        ynew = spl.eval(xnew, der=0) 
        assert ynew == 2.642301710730949
        print ("1D notaknot", xnew, ynew)

        # array
        xnew = np.linspace( x[0], x[-1] , 10)
        ynew = spl.eval(xnew, der=0) 
        print ("1D notaknot der=0", np.sum (ynew), "   x sum", np.sum(xnew))
        assert np.sum (ynew) == 15.463452566096425

        ynew = spl.eval(xnew, der=1) 
        print ("1D notaknot der=1", np.sum (ynew))
        assert np.sum (ynew) == 0.07395887333679596

        ynew = spl.eval(xnew, der=2) 
        print ("1D notaknot der=2", np.sum (ynew))
        assert np.sum (ynew) == -49.18195956454122

        c  = spl.curvature(xnew) 
        print ("1D notaknot curv", np.sum (c))
        assert np.sum (c) == -6.761005232410417

        pass



    def test_spline_2D (self): 

        x = [  0, 0.5,  2,  3,  4,  5,  7]  
        y = [  0,  3,  0,  2,  0,  2,  0]  

        # array 
        spl = Spline2D (x,y, boundary="notaknot")

        u = np.linspace( 0, 1 , 10)

        xnew, ynew = spl.eval (u)
        assert np.sum (xnew) == 28.86562563555188
        assert np.sum (ynew) == 12.849386740364192
        # print ("2D notaknot", np.sum(xnew), np.sum(ynew))

        curv = spl.curvature (u)
        assert np.sum(curv) == -10.268879474628818
        # print ("2D notaknot curvature", np.sum(curv), curv)

        # xnew = np.linspace( x[0], x[-1] , 10)
        # ynew = spl.eval(xnew, der=0) 
        # print ("notaknot der=0", np.sum (ynew), "   x sum", np.sum(xnew))
        # assert np.sum (ynew) == 15.463452566096425

        # ynew = spl.eval(xnew, der=1) 
        # print ("notaknot der=1", np.sum (ynew))
        # assert np.sum (ynew) == 0.07395887333679596

        # ynew = spl.eval(xnew, der=2) 
        # print ("notaknot der=2", np.sum (ynew))
        # assert np.sum (ynew) == -49.18195956454122

        # c  = spl.curvature(xnew) 
        # print ("notaknot curv", np.sum (c))
        # assert np.sum (c) == -6.761005232410417

        pass


# Main program for testing 
if __name__ == "__main__":

    test = Test_Spline()
    test.test_spline_1D()
    test.test_spline_2D()
