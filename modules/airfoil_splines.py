#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""

    Airfoil and operations on it 

"""

import numpy as np
# from common_utils import * 
from scipy.interpolate import splprep, splrep, splev
from scipy.optimize import fmin, brentq
from pycubicspline import Spline


def _cosinus_distribution (xfacStart=0, xfacEnd=1, nPoints=100):
    """ 
    returns an array with cosinues distributed values 0..1
    
    xfacStart: shift of cosinus start point - default 0 towards 1
    xfacEnd:   shift of cosinus endpoint - default 1 towards 0 
    npoints:    number of points to generate - default 100
    """

    xfacStart = max(0.0, xfacStart)
    xfacStart = min(1.0, xfacStart)
    xfacEnd   = max(0.0, xfacEnd)
    xfacEnd   = min(1.0, xfacEnd)

    if xfacStart >= xfacEnd: raise ValueError ("Airfoil x-distribution: start > end")

    beta = np.linspace(xfacStart, xfacEnd * np.pi, nPoints)
    xnew = (1.0 - np.cos(beta)) * 0.5

    # normalize to 0..1
    xmin = np.amin(xnew)
    xmax = np.amax(xnew) 
    xnew = (xnew - xmin) / (xmax-xmin)

    return xnew.round(10)
 

#------------ Spline Classes -----------------------------------



class SplineOfAirfoil: 
    """ 
    2D B-Spline representation of airfoil all around the contour
    
    Parameter is the arc length 0..1 of the splined contour. 
    The 2D spline is used to get the best approximation of the airfoil e.g. for re-paneling
    """

    def __init__ (self, x,y):


        # thickness and camber 
        self._thickness = None                  # array of thickness distribution
        self._camber    = None                  # array of camber distribution
        self._xthick    = None                  # x values for thickness and camber

        # B-spline representation of an N-D curve.
        s = 0.0                                 # no smoothing 
        k = 5                                   # oder of spline - cubic
        self._u    = None                       # the arc positionen around airfoil 0..1
        self._tck  = 0                          # spline parameters - see scipy splprep 
        self._x = x  #test
        self._tck, self._u = splprep([x, y], s=s, k=k)

        # leading edge 
        self.iLe = np.argmin (x)            # index of LE in x,y and u 
        self.uLe = self._u [self.iLe]             # u value at LE 

        # for i in range(len(x)):
        #     print (i, x[i], y[i], self._u[i])


    @property
    def deriv1 (self): 
        """ return derivate 1 of spline at knots"""

        #  splev returns dx/du and dy/du at knot. 
        dx, dy = splev(self._u, self._tck, der=1)
        #  derivative 1 (gradient) is dy/dx = dy/du / dx/du


        return dy/dx

    @property
    def angle (self): 
        """ return the angle in degrees at knots"""
        return np.arctan (self.deriv1) * 180 / np.pi

    @property
    def curvature (self): 
        " return the curvature at knots 0..npoints"

        dx, dy   = splev(self._u, self._tck, der=1)
        ddx, ddy = splev(self._u, self._tck, der=2)

        deriv2 = dx * ddy - dy * ddx

        # get curvature from derivative 2
        n = dx**2 + dy**2
        curv = deriv2 / n**(3./2.)
        return curv 


    def thickness_camber (self): 
        """returns thickness and camber distribution.
        
        Note: 
        
        It's an approximation as thickness is just the sum of y_upper(x) and y_lower(x)
        and camber is just the mean value y_upper(x) and y_lower(x)
        
        """

        # get a new high res distribution for upper and lower 
        # u_new_upper = np.linspace (self.uLe, 0.0, 100)
        # u_new_lower = np.linspace (self.uLe, 1.0, 100)
        u_new_upper = np.abs (np.flip(_cosinus_distribution (0.1, 1.2, 100)) -1) * self.uLe
        u_new_lower = _cosinus_distribution (0.1, 1.2, 100) * (1- self.uLe) + self.uLe

        x_upper, y_upper = splev (u_new_upper, self._tck, der= 0)
        x_lower, y_lower = splev (u_new_lower, self._tck, der= 0)

        x_upper = np.asarray(x_upper).round(10)
        y_upper = np.asarray(y_upper).round(10)
        x_lower = np.asarray(x_lower).round(10)
        y_lower = np.asarray(y_lower).round(10)

        # take x-distrib of upper and interpolate y_lower with these values
        tck = splrep(x_lower, y_lower, k=3)
        y_lower = splev(x_upper, tck, der=0) 
        y_lower = np.asarray(y_lower).round(10)
        x_lower = x_upper

        # thickness and camber can now easily calculated 
        self._thickness = ((y_upper - y_lower)      ).round(10) 
        self._camber    = ((y_upper + y_lower) / 2.0).round(10)
        self._xthick    = x_upper 

        # for i in range(len(x_upper)):
        #     print (i, self._thickness[i], self._camber[i])

        return self._xthick, self._thickness, self._camber 

 
    def get_maxThickness (self): 
        """returns max. thickness (normed) and its position along x"""
        if self._thickness is None: self.thickness_camber()

        # todo interpolate t_max on spline 
        tmax   = np.max(self._thickness)
        tmax_x = self._xthick[np.argmax (self._thickness)]
        return tmax, tmax_x

    def get_maxCamber (self): 
        """returns max. camber (normed) and its position along x"""

        if self._camber is None: self.thickness_camber()

        # todo interpolate c_max on spline 
        cmax   = np.max(self._camber)
        cmax_x = self._xthick[np.argmax (self._camber)]
        return cmax, cmax_x



class LineOfAirfoil: 
    """ 
    1D line of an airfoil like upper, lower side, camber line, curvature etc...

    Uses 1D interpolation to get intermediate points
    """

    default_cosinus = _cosinus_distribution (0.1, 0.8, 80)

    def __init__ (self, x,y, cosinus=False, name=None):

        if cosinus:                             # spline new with cosinus distrivution 
            tck = splrep(x, y, k=3)  
            self._x = self.default_cosinus # "soft" cosinus 
            self._y = splev(self._x, tck, der=0)
        else: 
            self._x = np.asarray(x).round(10)
            self._y = np.asarray(y).round(10)

        self._name = name 
        self._cosinus = cosinus
        self._tck = splrep(x, y, k=3)           # scipy splrep spline definition  

    @property
    def x (self):
        return self._x
    
    @property
    def y (self): 
        return self._y
    
    @property
    def name (self): 
        return self._name

    @property
    def deriv1 (self): 
        """ return derivate 1 of spline at x"""

        # ensure a smooth leading edge - otherwise derivative tends to oscillate at LE 
        if self._cosinus: 
            x = self._x
        else:                                   
            x = self.default_cosinus            
        y_deriv1 = splev(x, self._tck, der=1).round(10)

        return LineOfAirfoil (x, y_deriv1, name='1st derivate')
    
    @property
    def angle (self): 
        """ return the angle in degrees at knots"""

        angle  = (np.arctan (self.deriv1.y) * 180 / np.pi).round(10)
        return LineOfAirfoil (self.deriv1.x, angle, name='angle')

    @property
    def deriv2 (self): 
        """ return derivate 2 of spline at x"""
        # ensure a smooth leading edge - otherwise derivative tends to oscillate at LE 
        if self._cosinus: 
            x = self._x
        else:                                   
            x = self.default_cosinus            
        y_deriv2 = splev(x, self._tck, der=2).round(10)

        return LineOfAirfoil (x, y_deriv2, name='2nd derivate')

    @property
    def deriv3 (self): 
        """ return derivate 3 of spline at x - starting at x=0.05 because of oscillations """
        # ensure a smooth leading edge - otherwise derivative tends to oscillate at LE 
        if self._cosinus: 
            x = self._x
        else:                                   
            x = self.default_cosinus  

        # re-spline deriv1 to get a smoother deriv 3
        tck = splrep(x, self.deriv1.y, k=3)  

        # now get deriv 2 of the splined deriv 1          
        y_deriv3 = splev(x, tck, der=2).round(10)

        # the deriv3 at nose can be extremely oscillated - cut nose 
        iStart = (np.abs(x - 0.05)).argmin()

        return LineOfAirfoil (x[iStart:], y_deriv3[iStart:], name='3rd derivate')
    
    
    @property
    def curvature (self): 
        " return the curvature at knots at x"

        yd1 = self.deriv1.y
        x   = self.deriv1.x
        yd2 = self.deriv2.y

        curv = yd2 / (1 + yd1**2) ** 1.5
        
        return LineOfAirfoil (x, curv, name='Curvature') 


    def reversals (self): 
        """ 
        returns a list of reversals (change of curvature sign equals curvature = 0 )
        A reversal is a tuple (x,y) indicating the reversal on self. 
        """
        curv = self.curvature 

        roots = []
        for i in range(len(curv.x)-1):
            if (curv.y[i] * curv.y[i+1]) < 0.0:
                # interpolate the exact x position of the root (curvature = 0)
                x0 = brentq(lambda y: curv.y_interpol(y), curv.x[i] , curv.x[i+1])
                # get the y-value 
                y  = self.y_interpol (x0)
                roots.append((round(x0,10),round(y,10)))
        return roots


    def spikes (self): 
        """ 
        returns a list of spikes (change of deriv3 sign equals curvature = 0 )
        A spike is a tuple (x,y) indicating the spike on self. 
        """
        curv = self.deriv3 

        roots = []
        for i in range(len(curv.x)-1):
            if (curv.y[i] * curv.y[i+1]) < 0.0:
                # interpolate the exact x position of the root (curvature = 0)
                x0 = brentq(lambda y: curv.y_interpol(y), curv.x[i] , curv.x[i+1])
                # get the y-value 
                y  = self.y_interpol (x0)
                roots.append((round(x0,10),round(y,10)))
        return roots


    def maximum (self): 
        """ 
        returns the x and y position of the maximum y value of self
        """
        # scipy only allows finding minimum - so take negative abs-function  
        # use scipy to find the minimum
        xmax = fmin(lambda x : - abs(self.y_interpol (x)), 0.5, disp=False)[0]

        ymax = self.y_interpol (xmax)
        return round(xmax,10), round(ymax,10)
    
    
    def y_interpol (self,x):
        """ returns interpolated y values based on new x-distribution"""
                
        a = splev(x, self._tck, der=0, ext=3)
        return a.round(10)



# Main program for testing -----------------------------------


def lineTest ():

    import matplotlib.pyplot as plt
    from airfoil_examples import Root_Example, Tip_Example
    
    air =Tip_Example()

    y = air.y
    x = air.x

    # spl = SplineOfAirfoil (x,y) 
    # x_upper, t, c = spl.thickness_camber()

    # print ("Thickness: ", spl.get_maxThickness())
    # print ("Camber:    ", spl.get_maxCamber())

    fig, axa = plt.subplots(3, 1, figsize=(16,8))
    fig.subplots_adjust(left=0.05, bottom=0.05, right=0.98, top=0.95, wspace=None, hspace=0.15)

    fig.suptitle(air.name)
    ax1 = axa[0]
    ax2 = axa[1]
    ax3 = axa[2]
 
    ax1.grid(True)
    ax2.grid(True)
    ax3.grid(True)

    ax1.axis('equal')
    ax1.set_ylim([ -0.2,  0.2])
    # ax1.plot(x, y, '-', label='x y')
    # ax1.plot(x, y,      marker='o', lw=1, fillstyle='none', markersize=4, label='x y points')
    #ax1.plot(x_upper, t, '-.', label='thickness')
    #ax1.plot(x_upper, c, '-', marker='o', lw=1, fillstyle='none', markersize=4, label='camber 2D')
    ax1.plot(air.upper.x, air.upper.y, '-',  marker='o', lw=1, fillstyle='none', markersize=4, label='upper')
    ax1.plot(air.lower.x, air.lower.y, '-', label='lower')
    ax1.plot(air.camber.x, air.camber.y, '-', marker='o', lw=1, fillstyle='none', markersize=4, label='camber 1D')
    ax1.plot(air.thickness.x, air.thickness.y, '-', marker='o', lw=1, fillstyle='none', markersize=4, label='thickness 1D')

    # ax2.set_ylim([ -1.5,  1.5])
    # ax2.plot (air.upper.angle.x,air.upper.angle.y, marker='o', lw=1, fillstyle='none', markersize=4, label='upper angle')
    # ax2.plot (air.lower.angle.x,air.lower.angle.y, marker='o', lw=1, fillstyle='none', markersize=4, label='lower angle')

    # ax2.set_ylim([ -1.5,  1.5])
    # ax2.plot (air.upper.deriv2.x,air.upper.deriv2.y, marker='o', lw=1, fillstyle='none', markersize=4, label='upper deriv2')
    # ax2.plot (air.lower.deriv2.x,air.lower.deriv2.y, marker='o', lw=1, fillstyle='none', markersize=4, label='lower deriv2')

    ax2.set_ylim([ -1.0,  1.0])
    ax2.plot (air.upper.curvature.x, -air.upper.curvature.y, label='upper curvature')
    ax2.plot (air.lower.curvature.x, -air.lower.curvature.y, label='lower curvature')

    ax3.set_ylim([ -10.0,  10.0])
    ax3.set_xlim([ -0.05, 1.05])
    ax3.plot (air.upper.deriv3.x, air.upper.deriv3.y, label='upper deriv3')
    ax3.plot (air.lower.deriv3.x, air.lower.deriv3.y, label='lower deriv3')

    ax1.legend()
    ax2.legend()
    ax3.legend()

    print ("Max   upper:  ", air.upper.maximum())
    print ("Roots upper:  ", air.upper.reversals())
    print ("Max   lower:  ", air.lower.maximum())
    print ("Roots lower:  ", air.lower.reversals())

    print ("Max Thickness: %.2f%%  at %.2f%%" % (air.maxThickness, air.maxThicknessX) )
    print ("Max Camber:    %.2f%%  at %.2f%%" % (air.maxCamber, air.maxCamberX) )
    plt.show()


def cubicSplineTest ():

    import matplotlib.pyplot as plt
    from airfoil_examples import Root_Example, Tip_Example
    
    air = Tip_Example()

    y = air.y
    x = air.x

    spl = SplineOfAirfoil (x,y) 
    x_upper, t, c = spl.thickness_camber()

    print ("Thickness: ", spl.get_maxThickness())
    print ("Camber:    ", spl.get_maxCamber())

    fig, axa = plt.subplots(3, 1, figsize=(16,8))
    ax1 = axa[0]
    ax2 = axa[1]
    ax3 = axa[2]
    ax1.axis('equal')
    ax1.set_ylim([ -0.2,  0.2])
    # ax1.set_xlim([ 0.0, 1.1])

    ax3.set_ylim([ -1,  1])

    ax1.grid(True)
    ax2.grid(True)
    ax3.grid(True)

    ax1.plot(x, y, '-', label='x y')
    ax1.plot(x_upper, t, '-.', label='thickness')
    ax1.plot(x_upper, c, '-', marker='o', lw=1, fillstyle='none', markersize=4, label='camber 2D')
    ax1.plot(air.camber.x, air.camber.y, '-', label='camber 1D')

    # ax2.set_ylim([ -20,  20])
    ax2.plot (x,spl.angle, marker='o', lw=1, fillstyle='none', markersize=4, label='Angle')
    # ax2.plot (x,spl.deriv1, label='Angle')
    ax3.plot (x,spl.curvature, label='Curvature')

    ax1.legend()
    ax2.legend()
    ax3.legend()
    plt.show()



if __name__ == "__main__":

    # ---- Test -----
    # loadFromFile = False

    # blendTest()
    # cubicSplineTest()
    lineTest()

