#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""

    Class Airfoil with Spline functions and operations on it 

    This is in a sperate module which should be imported only if advanced 
    airfoil functions are needed as this module imports heavy SciPy! 

"""
import math
import numpy as np
from scipy.interpolate import splprep, splrep, splev
from scipy.optimize import fmin, brentq
from pycubicspline import Spline 
# from airfoil import Airfoil, panel_angles 


#------------ Util functions -----------------------------------


def _cosinus_distribution (nPoints, le_bunch, te_bunch):
    """ 
    returns an array with cosinues distributed values 0..1
    
    Args: 
    nPoints : new number of coordinate points
    le_bunch : 0..1  where 1 is the full cosinus bunch at leading edge - 0 no bunch 
    te_bunch : 0..1  where 1 is the full cosinus bunch at trailing edge - 0 no bunch 
    """

    xfacStart = 0.1 - le_bunch * 0.1
    xfacEnd   = 0.7 + te_bunch * 0.3 

    xfacStart = max(0.0, xfacStart)
    xfacStart = min(0.5, xfacStart)
    xfacEnd   = max(0.5, xfacEnd)
    xfacEnd   = min(1.0, xfacEnd)

    if xfacStart >= xfacEnd: raise ValueError ("Airfoil cosinus-distribution: start > end")

    beta = np.linspace(xfacStart, xfacEnd , nPoints) * np.pi
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


        self._x = x   
        self._y = y

        self._thickness : SideOfAirfoil = None  # thickness distribution
        self._camber    : SideOfAirfoil = None  # camber line

        self._curvature   = None
        self._curv_upper : SideOfAirfoil = None
        self._curv_lower : SideOfAirfoil = None

        self._deriv3       = None               # ! todo 'deriv3' is derivative of curvature ...
        self._deriv3_upper : SideOfAirfoil = None
        self._deriv3_lower : SideOfAirfoil = None

        # B-spline representation of an N-D curve.
        s = 0.0                                 # no smoothing 
        k = 3                                   # oder of spline - cubic
        self._u    = None                       # the arc positionen around airfoil 0..1
        self._tck  = None                          # spline parameters - see scipy splprep 
        self._tck, self._u = splprep([x, y], s=s, k=k)

        # leading edge 
        self.iLe  = np.argmin (x)                # index of LE in x,y and u 
        self._uLe = None                         # u value at LE 

    @property
    def x (self): return self._x
        
    @property
    def y (self): return self._y

    @property
    def uLe (self): 
        """ u (arc) value of the leading edge """
        if self._uLe is None: 
            self._uLe = self._le_find()
        return self._uLe

    @property
    def le (self): 
        """ returns x,y cordinates of the real (splined)  leading edge """
        xLe, yLe = self.xyFn (self.uLe) 
        return float(xLe), float(yLe) 

    @property
    def camber (self) -> 'SideOfAirfoil': 
        """ return the camber line """
        if self._camber is None: 
            self._eval_thickness_camber()
        return self._camber

    @property
    def thickness (self) -> 'SideOfAirfoil': 
        """ return the thickness distribution as a line """
        if self._thickness is None: 
            self._eval_thickness_camber()
        return self._thickness




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
        if self._curvature is None: 
            self._curvature = self.curvatureFn (self._u) 
        return self._curvature 

    @property
    def curv_upper (self): 
        " return SideOfAirfoil with curvature at knots 0..npoints"
        if self._curv_upper is None: 
            self._curv_upper = SideOfAirfoil (np.flip(self._x[: self.iLe]),
                                              np.flip(self.curvature [: self.iLe]), 
                                              name='Curvature upper' )
            # set default value für reversal detection 
            self._curv_upper.set_threshold (0.1)
        return self._curv_upper 

    @property
    def curv_lower (self): 
        " return SideOfAirfoil with curvature at knots 0..npoints"
        if self._curv_lower is None: 
            self._curv_lower = SideOfAirfoil (self._x[self.iLe: ],
                                              self.curvature [self.iLe: ], 
                                              name='Curvature lower' )
            # set default value für reversal detection 
            self._curv_lower.set_threshold (0.1)
        return self._curv_lower 


    @property
    def deriv3 (self): 
        " return the 3rd derivative at knots 0..npoints"

        # ! in dev !
        # this is the derivative of the *curvature* not the 3rd derivative 
        if self._deriv3 is None: 
            # # re-spline curvature over x to get a smoother derivative of it
            tck, u = splprep([self._x, self.curvature], s=0,k=3)    # 2D spline
            # now get deriv 1 of the splined curvature          
            dx, dy = splev(u, tck, der=1)
            self._deriv3 = dy / dx

        return self._deriv3


    @property
    def deriv3_upper (self): 
        " return SideOfAirfoil with deriv3 at knots 0..npoints"
        if self._deriv3_upper is None: 
            self._deriv3_upper = SideOfAirfoil (np.flip(self._x[: self.iLe]),
                                                np.flip(self.deriv3 [: self.iLe]), 
                                                name='Derivative 3 upper')
            # set default value für reversal detection 
            self._deriv3_upper.set_threshold (0.4)

        return self._deriv3_upper 


    @property
    def deriv3_lower (self): 
        " return SideOfAirfoil with deriv3 at knots 0..npoints"
        if self._deriv3_lower is None: 
            self._deriv3_lower = SideOfAirfoil (self._x[self.iLe: ],
                                                self.deriv3 [self.iLe: ], 
                                                name='Derivative 3 lower' )
            # set default value für reversal detection 
            self._deriv3_lower.set_threshold (0.4)

        return self._deriv3_lower 
    

    @property
    def scalarProduct (self): 
        """ return the scalar product of a vector from TE to u and the tangent at knots 0..npoints
        Used for finding LE where this value is 0.0at u"""

        dot = np.zeros (len(self._u))

        for i, u in enumerate(self._u):
            dot[i] = self.scalarProductFn (u)

        return dot

    #-----------


    def curvatureFn (self,u): 
        " return the curvature at u"

        dx, dy   = splev(u, self._tck, der=1)
        ddx, ddy = splev(u, self._tck, der=2)

        deriv2 = dx * ddy - dy * ddx

        # get curvature from derivative 2
        n = dx**2 + dy**2
        curv = deriv2 / n**(3./2.)
        return curv 

    def deriv1Fn (self,u): 
        " return dx,dy at spline arc u"
        return  splev(u, self._tck, der=1)

    def deriv3Fn (self,u): 
        " return 3rd derivative at spline arc u"
        dx, dy =  splev(u, self._tck, der=3)
        return dy #/dx

    def xyFn (self,u): 
        " return x,y at spline arc u"
        return  splev(u, self._tck, der=0)

    def xFn (self,u): 
        " return x at spline arc u"
        return  splev(u, self._tck, der=0)[0]

    def scalarProductFn (self,u): 
        """ return the scalar product of a vector from TE to u and the tangent at u
        Used for finding LE where this value is 0.0at u"""

        # exact trailing edge point 
        xTe = (self._x[0] + self._x[-1]) / 2
        yTe = (self._y[0] + self._y[-1]) / 2

        x,y = self.xyFn(u) 

        # vector 1 from te to point 
        dxTe = x - xTe
        dyTe = y - yTe
        # verctor2 tangent at point 
        dx, dy = self.deriv1Fn (u)

        dot = dx * dxTe + dy * dyTe

        return dot 

    def _le_find (self):
        """ returns u (arc) value of leading edge based on scalar product tangent and te vector = 0"""

        # first guess for Le point 
        iLeGuess = np.argmin (self._x) 
        # exact determination of root  = scalar product = 0.0 
        uLe = brentq(lambda u: self.scalarProductFn(u) , self._u[iLeGuess-1] , self._u[iLeGuess+1])
        return uLe


    def _eval_thickness_camber (self): 
        """
        evalutes thickness and camber distribution as SideOfAirfoil objects
        with it's on x-distribution.
        
        Note: 
        
        It's an approximation as thickness is just the sum of y_upper(x) and y_lower(x)
        and camber is just the mean value y_upper(x) and y_lower(x)
        """

        # get a new high res distribution for upper and lower 
        u_cosinus = _cosinus_distribution (100, 0.97, 0.8)

        u_new_upper = np.abs (np.flip(u_cosinus) -1) * self.uLe
        u_new_lower = u_cosinus * (1- self.uLe) + self.uLe

        x_upper, y_upper = splev (u_new_upper, self._tck, der= 0)   # x_upper 1..0
        x_lower, y_lower = splev (u_new_lower, self._tck, der= 0)   

        x_upper = np.flip(np.asarray(x_upper).round(10))            # x_upper 0..1
        y_upper = np.flip(np.asarray(y_upper).round(10))
        x_lower = np.asarray(x_lower).round(10)
        y_lower = np.asarray(y_lower).round(10)

        # take x-distrib of upper and interpolate y_lower with these values
        tck = splrep(x_lower, y_lower, k=3)
        y_lower = splev(x_upper, tck, der=0) 
        y_lower = np.asarray(y_lower).round(10)

        # thickness and camber can now easily calculated 
        self._thickness = SideOfAirfoil (x_upper,
                                         ((y_upper - y_lower)      ).round(10), 
                                         name='Thickness distribution')
        self._camber    = SideOfAirfoil (x_upper,
                                         ((y_upper + y_lower) / 2.0).round(10), 
                                         name='Camber line')
        return 

 
    def maxThickness (self): 
        """returns max. thickness (normed) and its position along x"""
        if self._thickness is None: 
            self._eval_thickness_camber()
        return self._thickness.maximum()


    def maxCamber (self): 
        """returns max. camber (normed) and its position along x"""
        if self._camber is None: 
            self._eval_thickness_camber()
        return self._camber.maximum()


    def get_repaneled (self, nPanels, le_bunch, te_bunch): 
        """Returns x,y of self with a repaneled  cosinus distribution 
        
        Args: 
        nPanels : new number of panels (which is no of points -1) 
        le_bunch : 0..1  where 1 is the full cosinus bunch at leading edge - 0 no bunch 
        te_bunch : 0..1  where 1 is the full cosinus bunch at trailing edge - 0 no bunch 
        """

        # new, equal distribution for upper and lower 
        u_cosinus = _cosinus_distribution (int (nPanels/2) + 1, le_bunch, te_bunch)

        u_new_upper = np.abs (np.flip(u_cosinus) -1) * self.uLe
        u_new_lower = u_cosinus * (1- self.uLe) + self.uLe
        u_new = np.concatenate ((u_new_upper, u_new_lower[1:]))

        # return new calculated x,y coordinates  
        return  self.xyFn(u_new) 


# ---------------------------

class SideOfAirfoil: 
    """ 
    1D line of an airfoil like upper, lower side, camber line, curvature etc...
    with x 0..1

    """

    def __init__ (self, x,y, name=None):

        self._x         = x
        self._y         = y
        self._name = name 
        self._threshold = 0.001                 # threshold for reversal dectection 
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
    def threshold (self):   return self._threshold 
    def set_threshold (self, aVal): 
        self._threshold =aVal 


    def reversals (self, xStart= 0.1):
        """ 
        returns a list of reversals (change of curvature sign equals curvature = 0 )
        A reversal is a tuple (x,y) indicating the reversal on self. 
        Reversal detect starts at xStart - to exclude turbulent leading area... 
        """
        # algorithm from Xoptfoil where a change of sign of y[i] is detected 
        x = self.x
        y = self.y

        iToDetect = np.nonzero (x >= xStart)[0]

        reversals = []
        yold    = y[iToDetect[0]]
        for i in iToDetect:
            if abs(y[i]) >= self.threshold:                # outside threshold range
                if (y[i] * yold < 0.0):                     # yes - changed + - 
                    reversals.append((round(x[i],10),round(y[i],10))) 
                yold = y[i]
        return reversals 
    

    def maximum (self): 
        """ 
        returns the maximum y value of self and its  x position  
        """
        # scipy only allows finding minimum - so take negative abs-function  
        # use scipy to find the minimum
        xmax = fmin(lambda x : - abs(self.y_interpol (x)), 0.5, disp=False)[0]

        ymax = self.y_interpol (xmax)
        return round(ymax,10), round(xmax,10) 
    
    
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
    from airfoil import Airfoil
    
   
    # air = Airfoil(pathFileName="test_airfoils\\JX-GP-033.dat")
    # air.load()
    air = Root_Example()

    y = air.y
    x = air.x

    print ("LE angle         : ", air.panelAngle_le)
    print ("Min panel angle  : ", air.panelAngle_min[0],"  at: ",air.panelAngle_min[1] )

    spl = air.spline

    print ("Thickness: ", spl.maxThickness())
    print ("Camber:    ", spl.maxCamber())

    x_new, y_new = spl._repanel (200, 1, 0.8)

    fig, axa = plt.subplots(3, 1, figsize=(16,8))
    ax1 = axa[0]
    ax2 = axa[1]
    ax3 = axa[2]
    ax1.axis('equal')
    ax1.set_ylim([ -0.2,  0.2])
    # ax1.set_xlim([ 0.0, 1.1])

    # ax3.set_ylim([ -1,  1])

    ax1.grid(True)
    ax2.grid(True)
    ax3.grid(True)

    ax1.plot(x, y, '-', marker='o', lw=1, fillstyle='none', markersize=4, label=air.name)
    ax1.plot(x_new, y_new, '-', color = 'red', marker='o', lw=1, fillstyle='none', markersize=4, label=air.name + " repan")
    
    # ax1.plot(x_upper, t, '-.', label='thickness')
    # ax1.plot(x_upper, c, '-', marker='o', lw=1, fillstyle='none', markersize=4, label='camber 2D')
    # ax1.plot(air.camber.x, air.camber.y, '-', label='camber 1D')

    # ax2.plot (x,spl.angle, marker='o', lw=1, fillstyle='none', markersize=4, label='Angle')
    # ax2.plot (x,panel_angles(x,y), marker='o', lw=1, fillstyle='none', markersize=4, label='Panel angle')
    # ax2.plot (x,spl.deriv1, label='Angle')
    ax2.plot (x,spl.scalarProduct, marker='o', fillstyle='none', markersize=4, label='Scalar product')


    ax3.set_ylim([ -10,  10])
    ax3.plot (x,spl.curvature, label='Curvature')
    ax3.plot (spl.curv_upper.x, spl.curv_upper.y, label='Curvature upper')
    ax3.plot (spl.curv_lower.x, - spl.curv_lower.y, label='Curvature lower')

    # leading edge 
    xLe, yLe = spl.le
    print ("Leading edge: ", xLe, yLe)

    ax1.plot (xLe, yLe, marker='o', color= 'red', markersize=5)

    ax1.legend()
    ax2.legend()
    ax3.legend()
    plt.show()



if __name__ == "__main__":

    # ---- Test -----
    # loadFromFile = False

    # blendTest()
    cubicSplineTest()
    # lineTest()

