#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""

    Spline of airfoil functions and operations on it 

"""

import numpy as np
from math_util import * 
from spline import Spline1D, Spline2D




class SplineOfAirfoil: 
    """ 
    2D Spline representation of airfoil all around the contour
    
    Parameter is the arc length 0..1 of the splined contour. 
    The 2D spline is used to get the best approximation of the airfoil e.g. for re-paneling
    """

    def __init__ (self, x,y):

        self._x = np.asarray(x)   
        self._y = np.asarray(y)

        self._thickness  : SideOfAirfoil = None  # thickness distribution
        self._camber     : SideOfAirfoil = None  # camber line
        self._curv_upper : SideOfAirfoil = None
        self._curv_lower : SideOfAirfoil = None

        self._spline : Spline2D           = None  # 2 D cubic spline representation of self
        self._uLe = None                          # leading edge  - u value 

        self._curvature   = None                  # curvature along u  


    @property
    def x (self): return self._x
        
    @property
    def y (self): return self._y

    @property 
    def spline (self) -> Spline2D:
        """ spline representation of self """

        if self._spline is None: 
            self._spline = Spline2D (self.x, self.y)
        return self._spline

    @property
    def isNormalized (self):
        """ true if the *real* LE of the spline is at 0,0 and TE is symmetrical at x=1"""
        
        if self._x is None: return False

        # TE at 1?
        xteUp, yteUp, xteLow, yteLow = self._x[0], self._y[0], self._x[-1], self._y[-1]
        if xteUp != 1.0 or xteLow != 1.0: return False
        if yteUp != - yteLow: return False

        # LE at 0,0? 
        xle, yle = self.le
        if xle != 0.0 or yle != 0.0: return False

        # LE of spline close enough to 0,0? 
        return self.isLe_closeTo_leSpline


    @property
    def le (self): 
        """ returns leading edge x,y of point coordinate data """
        ile = np.argmin (self._x)
        return self._x[ile], self._y[ile]


    @property
    def uLe (self): 
        """ u (arc) value of the leading edge """
        if self._uLe is None: 
            self._uLe = self._le_find()
        return self._uLe

    @property
    def leSpline (self): 
        """ x,y cordinates of the real (splined) leading edge rounded to 7 deciamls """
        xLe, yLe = self.xyFn (self.uLe) 
        
        # + 0.0 ensures not to have -0.0 
        return round(xLe,7) + 0.0, round(yLe,7) + 0.0 

    @property
    def isLe_closeTo_leSpline (self): 
        """ true if LE of x,y cordinates nearly equal to the real (splined) leading edge.
            If not the airfoil should be repaneld...! """

        xle, yle   = self.le
        xleS, yleS = self.leSpline 
        if abs(xle-xleS) > 0.00005 or abs(yle-yleS) > 0.00005: 
            return False
        else: 
            return True

    @property
    def camber (self) -> 'SideOfAirfoil': 
        """ return the camber line """
        if self._camber is None: 
            self._eval_thickness_camber()
        return self._camber

    @property
    def thickness (self) -> 'SideOfAirfoil': 
        """ the thickness distribution as a line object """
        if self._thickness is None: 
            self._eval_thickness_camber()
        return self._thickness

    @property
    def maxThick (self): 
        """ norm max thickness """
        return self.thickness.maximum[1]
    def set_maxThick (self, newY): 
        self.thickness.set_maximum(newY=newY)
        self._rebuildFromThicknessCamber()


    @property
    def maxThickX (self): 
        """  max thickness norm x-Position"""
        return self.thickness.maximum [0]
    def set_maxThickX (self,newX): 
        self.thickness.set_maximum(newX=newX)
        self._rebuildFromThicknessCamber()


    @property
    def maxCamb (self): 
        """ norm max camber """
        return self.camber.maximum [1]
    def set_maxCamb (self, newY): 
        self.camber.set_maximum(newY=newY)
        self._rebuildFromThicknessCamber()


    @property
    def maxCambX (self): 
        """ max camber norm x-Position """
        return self.camber.maximum [0]
    def set_maxCambX (self,newX): 
        self.camber.set_maximum(newX=newX)
        self._rebuildFromThicknessCamber()


    @property
    def deriv1 (self): 
        """ derivate 1 of spline at knots"""

        dx, dy = self.spline.eval (None, der=1)
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
            self._curvature = self.curvatureFn (self.spline.u) 
        return self._curvature 

    @property
    def curv_upper (self): 
        " return SideOfAirfoil with curvature on the upper side"
        if self._curv_upper is None: 
            iLe  = np.argmin (self._x)                # index of LE in x,y and u 
            self._curv_upper = SideOfAirfoil (np.flip(self._x[: iLe]),
                                              np.flip(self.curvature [: iLe]), 
                                              name='Curvature upper' )
            # set default value für reversal detection 
            self._curv_upper.set_threshold (0.1)
        return self._curv_upper 

    @property
    def curv_lower (self): 
        " return SideOfAirfoil with curvature on the lower side"
        if self._curv_lower is None: 
            iLe  = np.argmin (self._x)                # index of LE in x,y and u 
            self._curv_lower = SideOfAirfoil (self._x[iLe: ],
                                              self.curvature [iLe: ], 
                                              name='Curvature lower' )
            # set default value für reversal detection 
            self._curv_lower.set_threshold (0.1)
        return self._curv_lower 
    

    @property
    def scalarProduct (self): 
        """ return the scalar product of a vector from TE to u and the tangent at knots 0..npoints
        Used for finding LE where this value is 0.0at u"""

        dot = np.zeros (len(self.spline.u))

        for i, u in enumerate(self.spline.u):
            dot[i] = self.scalarProductFn (u)

        return dot

    #-----------

    def _reset (self):
        """ reinit self if x,y has changed""" 
        self._thickness  = None                 # thickness distribution
        self._camber     = None                 # camber line
        self._reset_spline()


    def _reset_spline (self):
        """ reinit self spline data if x,y has changed""" 
        self._curvature  = None
        self._curv_upper = None
        self._curv_lower = None
        self._spline     = None
        self._uLe        = None                  # u value at LE 

    
    def _normalize (self):
        """Shift, rotate, scale airfoil so LE is at 0,0 and TE is symmetric at 1,y"""

        if self.isNormalized: return 

        # first do repanel airfoil if LE of coordinates and LE of spline differ too much 

        if not self.isLe_closeTo_leSpline:
            self._repanelDefault() 
            if not self.isLe_closeTo_leSpline:
                # print ("LE after 1st repan: ", self.le, self.leSpline, self.isLe_closeTo_leSpline, self.uLe)
                self._repanelDefault (tryHarder=True) 
                if not self.isLe_closeTo_leSpline:
                    print ("LE after 2nd repan: ", self.le, self.leSpline, self.isLe_closeTo_leSpline, self.uLe)
                    raise ValueError ("Leading edge couldn't be iterated") 

        # Translate so that the leading edge is at 0,0 
        xLe, yLe = self.le
        xn = self._x - xLe
        yn = self._y - yLe

        # Rotate the airfoil so chord is on x-axis 
        angle = np.arctan2 ((yn[0] + yn[-1])/ 2.0, (xn[0] + xn[-1])/ 2.0) 
        cosa  = np.cos (-angle) 
        sina  = np.sin (-angle) 

        for i in range (len(xn)):
            xn[i] = xn[i] * cosa - yn[i] * sina
            yn[i] = xn[i] * sina + yn[i] * cosa
         
        # Scale airfoil so that it has a length of 1 
        #  - there are mal formed airfoils with different TE on upper and lower
        #    scale both to 1.0  
        scale_upper = 1.0 / xn[0]
        scale_lower = 1.0 / xn[-1]

        ile = np.argmin (xn)
        for i in range (len(xn)):
            if i <= ile:
               xn[i] = xn[i] * scale_upper
            else: 
               xn[i] = xn[i] * scale_lower

        # due to numerical issues ensure 0 is 0.0 ..
        xn[ile] = 0.0 
        yn[ile] = 0.0 
        xn[0]   = 1.0 
        xn[-1]  = 1.0
        yn[0]   = round(yn[0],10)
        yn[-1]  = round(yn[-1],10)

        # re-init the spline(s) of self because coordinates changed  
        self._x = xn
        self._y = yn
        self._reset()


    def curvatureFn (self,u): 
        " return the curvature at u"

        dx, dy   = self.spline.eval (u, der=1)
        ddx, ddy = self.spline.eval (u, der=2)

        deriv2 = dx * ddy - dy * ddx
        # get curvature from derivative 2
        n = dx**2 + dy**2
        curv = deriv2 / n**(3./2.)

        return curv 


    def deriv1Fn (self,u): 
        " return dx,dy at spline arc u"
        return  self.spline.eval(u, der=1)

    def xyFn (self,u): 
        " return x,y at spline arc u"
        return  self.spline.eval (u)

    def xFn (self,u): 
        " return x at spline arc u"
        return  self.spline.evalx (u)

    def yFn (self,u): 
        " return y at spline arc u"
        return  self.spline.evaly (u)

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

        iLeGuess = np.argmin (self._x)          # first guess for Le point 
        # exact determination of root  = scalar product = 0.0 
        uLe = findRoot (self.scalarProductFn, self.spline.u[iLeGuess-1] , bounds=(0.3, 0.7)) 

        return uLe


    def _eval_thickness_camber (self): 
        """
        evalutes self thickness and camber distribution as SideOfAirfoil objects
        with a x-distribution of the upper side.
        
        Note: It's an approximation as thickness is just the sum of y_upper(x) and y_lower(x)
              and camber is just the mean value y_upper(x) and y_lower(x)
        """

        # evaluate the corresponding y-values on lower side 
        x_upper, y_upper, y_oppo = self._y_oppoTo ("upper")    

        # get ascending x-coordinate 0..1
        x_upper = np.flip(x_upper)
        y_upper = np.flip(y_upper)
        y_oppo  = np.flip(y_oppo)

        # thickness and camber can now easily calculated 
        self._thickness = SideOfAirfoil (x_upper,  y_upper - y_oppo,        name='Thickness distribution')
        self._camber    = SideOfAirfoil (x_upper, (y_oppo + y_upper) / 2.0, name='Camber line')

        # for symmetric airfoil with unclean data set camber line to 0 
        if np.max(self._camber._y) < 0.00001: 
            self._camber._y = np.zeros (len(self._camber._y))

        return 


    def _y_oppoTo (self, side): 
        """
        Evalutes y values right opposite (having same x-value) to side.
        Note: if self isn't normalized, it will be normalized prior to evaluation

        Parameters
        ----------
        side : either 'upper' or 'lower'
             
        Returns
        -------
        x_from : np array - x_values of source side - for convinience 
        y_from : np array - yvalues of source side - for convinience 
        y_oppo : np array - y_values on the opposite side in the same order as side
        """

        if not self.isNormalized:
            self._normalize()

        iLe = np.argmin (self._x)

        if side == "lower": 
            x_from = self.x [iLe : ]
            y_from = self.y [iLe : ]
            u_from = self.spline.u [iLe : ]                
        elif side == "upper":
            x_from = self.x [0: (iLe +1)] 
            y_from = self.y [0: (iLe +1)] 
            u_from = self.spline.u [0: (iLe +1)]              
        else:
            raise ValueError ("'%s' not supported" % side)
        
        u_oppo = np.zeros (len(x_from))
        for i, xi in enumerate (x_from): 

            u_oppo[i]    = self._eval_u_oppo (u_from[i], xi)

        y_oppo = self.yFn(u_oppo)

        return x_from, y_from, y_oppo



    def _eval_u_oppo (self, uIn, xIn):
        """ returns u at the opposite site having the same x-value"""

        du = abs(self.uLe - uIn)                    # distance to LE

        if   du == 0.0:     return uIn
        elif xIn <= 0.0:    return uIn 
        elif uIn <= 0.0:    return 1.0 
        elif uIn >= 1.0:    return 0.0 

        # define search range for minimum and a good start value for the search 

        elif uIn < self.uLe:                        # will search on lower side 
            uStart = self.uLe                       # search boundaries 
            uEnd   = 1.0 
            if du < 0.1:  uGuess = uStart + 0.1     # estimate a first guess for the search algo 
            else:         uGuess = uStart + du
        else:                                       # will search on upper side 
            uStart = 0.0
            uEnd   = self.uLe
            if du < 0.1:  uGuess = uEnd - 0.1
            else:         uGuess = uEnd - du

        uOpp = findMin (lambda x: abs(self.spline.evalx(x) - xIn), uGuess, bounds=(uStart, uEnd)) 
        return uOpp


    def _repanelDefault (self, tryHarder=False): 
        """Repanels self with a new, default cosinus distribution. Reinit spline.
           'tryHarder=True' will repanel with more panels and more LE bunch of panels"""

        if tryHarder:                           # generate more panels with higer LE bunch
            nPanels = 200                       # ... no experiments 
            le_bunch = 0.90 # 0.93
            te_bunch = 0.7
        else: 
            if len(self._x) < 120:              # ensure a min number of panels for strange (old) airfoils
                nPanels = 120
            else: 
                nPanels = len(self._x)          # do not change no of panels
            le_bunch = 0.84
            te_bunch = 0.7

        self._x, self._y = self.get_repaneled (nPanels, le_bunch, te_bunch)
        self._reset()
        

    def get_y_on (self, side, xIn): 
        """
        Evalutes y values right on 'side' having x-values xIn.
        Note: if self isn't normalized, it will be normalized prior to evaluation

        Parameters
        ----------
        side : either 'upper' or 'lower'
        xIn : x-coordinates on 'side' to evaluate y
             
        Returns
        -------
        yOut : np array - evaluated y values 
        """

        iLe = np.argmin (self._x)

        if side == "lower": 
            uStart = self.spline.u[iLe] 
            uEnd   = self.spline.u[-1]  
            uGuess = 0.75          
        elif side == "upper":
            uStart = self.spline.u[0] 
            # uEnd   = self.spline.u[iLe-1]   
            uEnd   = self.spline.u[iLe]   
            uGuess = 0.25         
        else:
            raise ValueError ("'%s' not supported" % side)

        ux = np.zeros (len(xIn))
        for i, xi in enumerate (xIn): 

            # find matching u to x-value 
            #   no_improve_thr= 10e-6 is sufficient to get average 10e-10  tolerance
            ux[i] = findMin (lambda u: abs(self.spline.evalx(u) - xi), uGuess, bounds=(uStart, uEnd), 
                             no_improve_thr= 10e-6) 
            uGuess = ux[i]

        # get y coordinate from u          
        yOut = self.yFn(ux)

        # ensure Le is 0,0 and Te is at 1
        if  xIn[0] == self._x[iLe]:                         yOut[0]  = self._y[iLe]
        elif xIn[-1] == self._x[0] and side == "upper":     yOut[-1] = self._y[0]
        elif xIn[-1] == self._x[-1] and side == "lower":    yOut[-1] = self._y[-1]

        return yOut 


    def get_repaneled (self, nPanels, le_bunch, te_bunch): 
        """Returns x,y of self with a repaneled  cosinus distribution 
        
        Args: 
        nPanels : new number of panels (which is no of points -1) 
        le_bunch : 0..1  where 1 is the full cosinus bunch at leading edge - 0 no bunch 
        te_bunch : 0..1  where 1 is the full cosinus bunch at trailing edge - 0 no bunch 
        """

        # new, equal distribution for upper and lower 
        u_cosinus = cosinus_distribution (int (nPanels/2) + 1, le_bunch, te_bunch)

        u_new_upper = np.abs (np.flip(u_cosinus) -1) * self.uLe
        u_new_lower = u_cosinus * (1- self.uLe) + self.uLe
        u_new = np.concatenate ((u_new_upper, u_new_lower[1:]))

        # return new calculated x,y coordinates  
        return  self.xyFn(u_new) 
    

    def _rebuildFromThicknessCamber(self):
        """ rebuilds self out of thickness and camber distribution """

        # x values of camber and thickness must be equal
        if not np.array_equal (self.thickness.x, self.camber.x):
            raise ValueError ("Spline rebuild: x-values of thickness and camber are not equal")
        
        # easy sum
        x_upper = self.thickness.x
        y_upper = self.camber.y + self.thickness.y / 2.0 
        x_lower = self.thickness.x
        y_lower = self.camber.y - self.thickness.y / 2.0

        self._x = np.concatenate ((np.flip(x_upper), x_lower[1:]))
        self._y = np.concatenate ((np.flip(y_upper), y_lower[1:]))

        self._reset_spline ()





# ---------------------------

class SideOfAirfoil: 
    """ 
    1D line of an airfoil like upper, lower side, camber line, curvature etc...
    with x 0..1

    """

    def __init__ (self, x,y, name=None):

        self._x         = x
        self._y         = y
        self._name      = name 
        self._threshold = 0.001                 # threshold for reversal dectection 
        self._spline    = None                  # 1D Spline to get max values of line
        self._maximum   = None                  # the highpoint of the spline line


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
    def spline (self) -> Spline1D:
        """ spline representation of self """

        if self._spline is None: 
            self._spline = Spline1D (self.x, self.y)
        return self._spline


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
    

    @property
    def maximum (self): 
        """ 
        returns the x,y position of the maximum y value of self
        """
        if self._maximum is None: 
            if np.max(self.y) == 0.0: 
                xmax = 0.0 
                ymax = 0.0 
            else:
                xmax = findMax (self.yFn, 0.3)
                ymax = self.yFn (xmax)
            self._maximum = (xmax, ymax)
        return self._maximum 


    def set_maximum (self, newX=None, newY=None): 
        """ 
        set x,y of the mx point of self 
        """
        # if e.g. camber is already = 0.0, a new camber line cannot be build
        if np.max(self.y) == 0.0: return

        if not newY is None: 

            if newY <= 0.0: 
                self._y = np.zeros(len(self._y))
            else: 
                curY = self.maximum[1]
                self._y = self._y * (newY / curY)
            self._reset_spline()                

        if not newX is None: 
         
            self._moveMaxX (newX)                   # a little bit more complicated ...
            self._reset_spline ()
    

    def _reset_spline (self):
        """ reinit self spline data if x,y has changed""" 
        self._spline     = None
        self._maximum    = None                     # the highpoint of the line 


    def _moveMaxX (self, newMax):
        """ 
        moves the point of maximum to newMaxX  
        """

        # sanity check - only a certain range of move is possible 
        curMax = self.maximum[0] 
        newMax = max (0.1, newMax)
        newMax = min (0.9, newMax)

        # from xfoil: 
        #    the assumption is that a smooth function (cubic, given by the old and 
        #    new highpoint locations) maps the range 0-1 for x/c
        #    into the range 0-1 for altered x/c distribution for the same y/c
        #    thickness or camber (ie. slide the points smoothly along the x axis)
         
        x = [self.x[0], curMax, self.x[-1]]    
        y = [self.x[0], newMax, self.x[-1]]    
        mapSpl = Spline2D (x,y, boundary='natural')

        unew = np.linspace (0.0, 1.0, 50)
        xmap, ymap = mapSpl.eval(unew)

        mapSpl = Spline1D (xmap,ymap, boundary='natural')

        # finally re-map x-values to move high point 

        newX = np.zeros(len(self.x))
        for i, xi in enumerate (self.x):
            newX[i] = mapSpl.eval(xi)    
            # print (i, "%.8f" %(self.x[i] - newX[i]), newX[i])
        newX[0]  = self.x[0]                # ensure LE and TE not to change due to numeric issues
        newX[-1] = self.x[-1]

        # build a temp spline with the new x and the current y values 
        # 2D spline is needed to avoid oscillations at LE for thickness distribution with high curvature

        tmpSpl = Spline2D (newX, self._y) 

        # ... to remap y values to the current x values 

        newY = np.zeros(len(self.y))
        newY[0]  = self.y[0]
        newY[-1] = self.y[-1]

        for i in range (1, len(self.x)- 1):

            # find the arc position u for the desired x-value 
            ui = findMin (lambda u: abs(tmpSpl.evalx(u) - self.x[i]), 0.41, bounds=(0.0, 1), no_improve_thr=10e-9)

            if (tmpSpl.evalx(ui) - self.x[i]) > 0.0000001: 
                raise ValueError ("Spline - moveMax: Couldn't find corresponding x at %d" %i + 
                                  " delta x: %.7f" % ((tmpSpl.evalx(ui) - self.x[i])))
            # get new y value at this position 
            newY[i] = tmpSpl.evaly(ui)
            
        self._y = newY


    def yFn (self,x):
        """ returns interpolated y values based on new x-distribution"""
                
        return self.spline.eval (x)



# ------------ test functions - to activate  -----------------------------------

# def curvatureTest():

#     import matplotlib.pyplot as plt
#     from airfoil_examples import Root_Example, Tip_Example
    
#     air =Tip_Example()

#     y = air.y
#     x = air.x
#     spl = Spline2D (x,y) 
#     curv = spl.curvature (None)

#     plt.subplots(1, 1, figsize=(16,8))
#     plt.grid(True)
#     # plt.axis("equal")

#     plt.xlabel("u")
#     plt.ylabel("curvature")
#     plt.plot(spl.u, curv, "-r", label="Curvature")
#     plt.legend()

#     plt.show()


# def lineTest ():

#     import matplotlib.pyplot as plt
#     from airfoil_examples import Root_Example, Tip_Example
    
#     spl =Root_Example().spline

#     fig, axa = plt.subplots(2, 1, figsize=(16,8))
#     fig.subplots_adjust(left=0.05, bottom=0.05, right=0.98, top=0.95, wspace=None, hspace=0.15)

#     # fig.suptitle(air.name)
#     ax1 = axa[0]
#     ax2 = axa[1]
 
#     ax1.grid(True)
#     ax2.grid(True)

#     # ax2.set_ylim([ -1.0,  1.0])
#     ax2.plot (spl.curv_upper.x, -spl.curv_upper.y, label='upper curvature org')
#     ax2.plot (spl.curv_lower.x, -spl.curv_lower.y, label='lower curvature org')

#     ax1.axis('equal')
#     ax1.set_ylim([ -0.2,  0.2])

#     ax1.plot(spl.x, spl.y, '-', label='x y')
#     # ax1.plot(x, y,      marker='o', lw=1, fillstyle='none', markersize=4, label='x y points')
#     ax1.plot(spl.camber.x, spl.camber.y, '-', marker='o', lw=1, fillstyle='none', markersize=4, label='camber 1D')
#     ax1.plot(spl.thickness.x, spl.thickness.y, '-', marker='o', lw=1, fillstyle='none', markersize=4, label='thickness 1D')

#     # move camber maxX
#     spl.thickness._moveMaxX (0.2)
#     ax1.plot(spl.thickness.x, spl.thickness.y, 'r-', marker='o', lw=1, fillstyle='none', markersize=4, label='thickness moved 1D')


#     # ax2.set_ylim([ -1.0,  1.0])
#     ax2.plot (spl.curv_upper.x, -spl.curv_upper.y, label='upper curvature')
#     ax2.plot (spl.curv_lower.x, -spl.curv_lower.y, label='lower curvature')

#     ax1.legend()
#     ax2.legend()

#     print ("Max Thickness: %.2f%%  at %.2f%%" % (spl.maxThick*100, spl.maxThickX*100) )
#     print ("Max Camber:    %.2f%%  at %.2f%%" % (spl.maxCamb*100,  spl.maxCambX*100) )
#     plt.show()


# def u_fxy_Test ():

#     import matplotlib.pyplot as plt
#     from airfoil_examples import Root_Example, Tip_Example
    
#     air = Tip_Example()

#     spl : SplineOfAirfoil = air.spline

#     y0 = air.y
#     x0 = air.x
#     le0 = spl.leSpline

#     spl._normalize ()

#     # if True: quit()
#     y = spl.y
#     x = spl.x
#     leNorm = spl.leSpline

#     # x_new, y_new = spl.get_repaneled (200, 0.97, 0.7)

#     fig, axa = plt.subplots(3, 1, figsize=(16,8))
#     ax1 = axa[0]
#     ax2 = axa[1]
#     ax3 = axa[2]
#     ax1.axis('equal')
#     ax1.set_ylim([ -0.2,  0.2])
#     # ax1.set_xlim([ 0.0, 1.1])

#     # ax3.set_ylim([ -1,  1])

#     ax1.grid(True)
#     ax2.grid(True)
#     ax3.grid(True)

#     ax1.plot(x0, y0, '-', marker='o', lw=0.5, fillstyle='none', markersize=4, label=air.name + " org")
#     ax1.plot(x, y, '-', marker='o', lw=0.5, fillstyle='none', markersize=4, label=air.name + " norm")
#     ax1.plot(le0[0], le0[1], "g",  marker='o', lw=0.5, markersize=4,    label= "LE org")
#     ax1.plot(leNorm[0], leNorm[1], "r",  marker='o', lw=0.5, markersize=4, label= "LE norm")
    
#     ax2.plot (x, spl._spline.u, marker='o', fillstyle='none', markersize=3, label='x')
#     # ax2.plot (spl._spline.u, y, marker='o', fillstyle='none', markersize=3, label='y')


#     # find opposite points 

#     ax3.axis('equal')
#     ax3.set_ylim([ -0.2,  0.2])

#     iLe  = np.argmin (x) 
#     x1 = x[:iLe]
#     y1 = y[:iLe]
#     xO = []
#     yO = []  
#     for i in range (iLe+1):
#         ui = spl._spline.u[i]  
#         uOpp = spl._eval_u_oppo (ui, x[i])
#         xOpp, yOpp = spl._spline.eval(uOpp)
#         xO.append(xOpp)
#         yO.append(yOpp)
#         # print (i, ui, x[i], uOpp, xOpp, "  delta: ", abs(x[i]-xOpp))
#         if y[i] != yOpp:
#             ax3.axline ((x[i],y[i]), (xOpp,yOpp))

#     ax3.plot (x,y, '-', marker='o', lw=1, fillstyle='none', markersize=4, label='xy upper')
#     # ax3.plot (xO,yO, '-', marker='o', lw=1, fillstyle='none', markersize=4, label='xy opposite')
#     # ax3.plot (x1,y1, '-', marker='o', lw=1, fillstyle='none', markersize=4, label='xy upper')
#     # ax3.plot (xO,yO, '-', marker='o', lw=1, fillstyle='none', markersize=4, label='xy opposite')

#     # ax3.set_ylim([ -10,  10])
#     # ax3.plot (x,spl.curvature, label='Curvature')
#     # ax3.plot (spl.curv_upper.x, spl.curv_upper.y, label='Curvature upper')
#     # ax3.plot (spl.curv_lower.x, - spl.curv_lower.y, label='Curvature lower')

#     ax1.legend()
#     ax2.legend()
#     ax3.legend()
#     plt.show()


# def cubicSplineTest ():

#     import matplotlib.pyplot as plt
#     from airfoil_examples import Root_Example, Tip_Example
#     from airfoil import Airfoil
    
   
#     # air = Airfoil(pathFileName="test_airfoils\\JX-GP-033.dat")
#     # air.load()
#     air = Root_Example()

#     y = air.y
#     x = air.x

#     print ("LE angle         : ", air.panelAngle_le)
#     print ("Min panel angle  : ", air.panelAngle_min[0],"  at: ",air.panelAngle_min[1] )

#     spl = air.spline

#     print ("Thickness: ", spl.thickness.maximum)
#     print ("Camber:    ", spl.camber.maximum)

#     x_new, y_new = spl.get_repaneled (200, 0.97, 0.7)

#     fig, axa = plt.subplots(3, 1, figsize=(16,8))
#     ax1 = axa[0]
#     ax2 = axa[1]
#     ax3 = axa[2]
#     ax1.axis('equal')
#     ax1.set_ylim([ -0.2,  0.2])
#     # ax1.set_xlim([ 0.0, 1.1])

#     # ax3.set_ylim([ -1,  1])

#     ax1.grid(True)
#     ax2.grid(True)
#     ax3.grid(True)

#     ax1.plot(x, y, '-', marker='o', lw=1, fillstyle='none', markersize=4, label=air.name)
#     ax1.plot(x_new, y_new, '-', color = 'red', marker='o', lw=1, fillstyle='none', markersize=4, label=air.name + " repan")
    
#     # ax1.plot(x_upper, t, '-.', label='thickness')
#     # ax1.plot(x_upper, c, '-', marker='o', lw=1, fillstyle='none', markersize=4, label='camber 2D')
#     # ax1.plot(air.camber.x, air.camber.y, '-', label='camber 1D')

#     # ax2.plot (x,spl.angle, marker='o', lw=1, fillstyle='none', markersize=4, label='Angle')
#     # ax2.plot (x,panel_angles(x,y), marker='o', lw=1, fillstyle='none', markersize=4, label='Panel angle')
#     # ax2.plot (x,spl.deriv1, label='Angle')
#     ax2.plot (x,spl.scalarProduct, marker='o', fillstyle='none', markersize=4, label='Scalar product')


#     # ax3.set_ylim([ -10,  10])
#     ax3.plot (x,spl.curvature, label='Curvature')
#     ax3.plot (spl.curv_upper.x, spl.curv_upper.y, label='Curvature upper')
#     ax3.plot (spl.curv_lower.x, - spl.curv_lower.y, label='Curvature lower')

#     # leading edge 
#     xLe, yLe = spl.leSpline
#     print ("Leading edge: ", xLe, yLe)

#     ax1.plot (xLe, yLe, marker='o', color= 'red', markersize=5)

#     ax1.legend()
#     ax2.legend()
#     ax3.legend()
#     plt.show()


if __name__ == "__main__":

    # ---- Test -----

    # blendTest()
    # cubicSplineTest()
    # lineTest()
    # curvatureTest()
    # u_fxy_Test()

    pass