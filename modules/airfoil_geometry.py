#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
    Geometry of an Airfoil  

    Implements a kind of 'strategy pattern' for the different approaches how 
    the geometry of an airfoil is determined and modified:

    - Basic     linear interpolation of surface
    - Spline    cubic spline interpolation 
    - Bezier    Bezier based represantion of outline 

    A single side of the airfoil or other lines like 'camber' or 'thickness' distribution 
    is representad in a similar way with subclasses

    - Basic     linear interpolation of line
    - Spline    splined represantion 
    - Bezier    Bezier based representation 

    The Curvature holds the curvature of the geometry spline 



    Class hierarchy overview  

        Geometry                                    - basic with linear interpolation 
            |-- Geometry_Splined                     - splined 
            |-- Geometry_Bezier                     - Bezier based 

        Curvature_Abstract    
            |-- Curvature_of_xy                     - based on x,y coordinates
            |-- Curvature_of_Spline                 - based on existing spline
            |-- Curvature_of_Bezier                 - based on Bezier geo upper and lower side 

        Side_Airfoil                               - basic with linear interpolation
            |-- Side_Airfoil_Spline                - splined 
            |-- Side_Airfoil_Bezier                - Bezier based            
                                
                                                    
    Object model - example                          

        airfoil                                     - an airfoils 
            |-- geo : Geometry                      - geometry strategy (basic) 
                    |-- upper  : Side_Airfoil      - upper surface
                    |-- camber : Side_Airfoil      - camber line       
                    |-- Curvature                   - curvature of the geometry spline
                        |-- upper : Side_Airfoil   - curvature of upper surface
                        |-- lower : Side_Airfoil   - curvature of lower surface
                    
                    """


import numpy as np
from math_util import * 
from copy import copy, deepcopy
from spline import Spline1D, Spline2D, Bezier
from spline import print_array_compact

from common_utils import ErrorMsg


UPPER  = 'upper'
LOWER  = 'lower'



# -----------------------------------------------------------------------------
# Match geometry 
# -----------------------------------------------------------------------------


class Match_Bezier:
    """ 
    Abstract superclass 

    Controller for matching Bezier curves to a target which is either a 'Geometry' or a 'Side'

    For 'nelder mead optimization' is used ... 

    """
    def __init__ (self):

        # nelder mead results 
        self.ncp         = None                     # number of bezier control points 
        self.nvar        = None                     # number of optimization variables
        self.norm2       = None                     # norm2 diviation of y points
        self.niter       = None                     # number of iterations needed
        self.max_reached = None                     # max number of iterations reached


    def _map_bezier_to_variables (self): 
        """ maps bezier control points to design variables of objective function
        Returns: 
            list of design variables """
        # must be overloaded

    def _map_variables_to_bezier (self, vars: list): 
        """ maps design variables to bezier (control points)"""
        # must be overloaded

    def _deviation_to_target_applying (self, variables) -> list:
        """returns array of deviations at target_x when variables are applied to bezier"""
        # must be overloaded

    def _objectiveFn (self, variables ):  
        """ returns norm2 value of y deviations of self to target y at x """
        # must be overloaded


    def run (self) :
        """ 
        Optimizes self to best fit to target (either single Side or both sides)
        uses nelder meat root finding
        """
      
        #-- map control point x,y to optimization variable 

        variables_start = self._map_bezier_to_variables ()

        # ----- objective function

        f = lambda variables : self._objectiveFn (variables) 

        # ----- nelder mead find minimum --------

        max_iter  = self.nvar * 80 

        res, niter = nelder_mead (f, variables_start,
                    # step=0.05, no_improve_thr=1e-5,             
                    step=0.02, no_improve_thr=1e-5,             
                    no_improv_break=25, max_iter=max_iter,
                    bounds = None)

        variables = res[0]
        score = res[1]

        #-- evaluate the new y values on Bezier for the target x-coordinate

        devi = self._deviation_to_target_applying (variables)            

        self.norm2       = np.linalg.norm (devi)           # calc the 'real' norm2 (objFun uses another) 
        self.niter       = niter
        self.max_reached = (niter >= max_iter)

        return 



class Match_Side_Bezier (Match_Bezier):
    """ 
    Controller for matching a single Side_Airfoil with Bezier
    """
    def __init__ (self, side : 'Side_Airfoil_Bezier', target_side: 'Side_Airfoil'):
        """match a single Side_Bezier to a target side 

        Args:
            side (Side_Airfoil_Bezier): the side with bezier to match 
            target_side (Side_Airfoil): the target side 
        """
        super().__init__ ()

        self._bezier = deepcopy (side.bezier)

        # number of design variables 
        self.ncp     = self.bezier.npoints
        self.nvar    = (self.ncp - 2) * 2 - 1  # excl. le, te, x of le tangent

        #-- selected target points for objective function
        self._targets_x, self._targets_y   = self._define_targets(target_side)    

        #-- (start) position of control points 
        self._set_initial_bezier (target_side)       


    @property
    def bezier (self) -> Bezier:
        """ bezier curve of self"""
        return self._bezier
    
    @property
    def targets_x (self): return self._targets_x

    @property
    def targets_y (self): return self._targets_y

    def _define_targets(self, target_side: 'Side_Airfoil'):
        """ returns target points where deviation is tested during optimization """

        # we do not take every coordinate point - nelder mead would take much to 
        # long to evaluate x,y on Bezier
        i = 1
        if   len(target_side.x) > 120:
            step = 6
        elif len(target_side.x) > 80:
            step = 5
        else: 
            step = 4
        targ_x = []
        targ_y = []
        while i < (len(target_side.x) -1):
            targ_x.append(target_side.x[i])
            targ_y.append(target_side.y[i])
            i += step
        return np.array(targ_x), np.array(targ_y)


    def _set_initial_bezier (self, targetSide: 'Side_Airfoil'):
        """ returns inital coordinates of control points """

        ncp = self.bezier.npoints
        cp_x, cp_y = np.zeros(ncp), np.zeros(ncp)

        # initial x values 
        cp_x[0]   = 0.0                                 # LE and TE fix pos 
        cp_x[1]   = 0.0 
        cp_x[-1]  = 1.0

        np_between =  ncp - 3                           # equal distribution between                         
        dx = 1.0 / (np_between + 1)
        x = 0.0 
        for ib in range(np_between): 
            icp = 2 + ib
            x += dx
            cp_x[icp] = x

        # initial y values 
        for icp, xi in enumerate (cp_x): 
            if icp == 1:                                # special case y-start value of point 1
                x = 0.1                                 #       take y-coord near LE
            else: 
                x = xi          
            cp_y[icp]  = round(targetSide.yFn (x), 6)                 

        self.bezier.set_points (cp_x, cp_y)


    def _map_bezier_to_variables (self): 
        """ maps bezier control points to design variables of objective function
        
        Returns: 
            list of design variables """

        vars = []
        cp_x, cp_y = self.bezier.points_x, self.bezier.points_y
        ncp = self.bezier.npoints

        for icp in range (ncp): 
            if icp == 0: 
                pass                                    # skip leading edge
            elif icp == ncp-1:                      
                pass                                    # skip trailing edge
            elif icp == 1: 
                vars.append(cp_y[icp])              # le tangent only y
            else:                                       
                vars.append(cp_x[icp])                  # all control points in between 
                vars.append(cp_y[icp])
        return vars


    def _map_variables_to_bezier (self, vars: list): 
        """ maps design variables to bezier (control points)"""

        cp_x, cp_y = self.bezier.points_x, self.bezier.points_y
        ncp = self.bezier.npoints
        if len(vars) == 2: 
            pass
        ivar = 0
        for icp in range (ncp): 
            if icp == 0: 
                pass                                    # skip leading edge
            elif icp == ncp-1:                      
                pass                                    # skip trailing edge
            elif icp == 1:    
                cp_y[icp] = vars[ivar]              # le tangent 
                ivar += 1                  
            else:                                       
                cp_x[icp] = vars[ivar]
                ivar += 1                  
                cp_y[icp] = vars[ivar]
                ivar += 1                  
        self.bezier.set_points (cp_x, cp_y)


    def _deviation_to_target_applying (self, variables):
        """returns array of deviations at target_x when variables are applied to bezier"""

        self._map_variables_to_bezier (variables)

        # evaluate the new y values on Bezier for the target x-coordinate
        y_new = np.zeros (len(self.targets_y))
        for i, x in enumerate(self.targets_x) :
            y_new[i] = self.bezier.eval_y_on_x(x, fast=False, epsilon=1e-7)

        # calculate abs difference between bezier y and target y 
        return np.abs((y_new - self.targets_y))


    def _objectiveFn (self, variables ):  
        """ returns norm2 value of y deviations of self to target y at x """

        # evaluate difference with new bezier out of variables
        devi = self._deviation_to_target_applying (variables)            

        # calculate norm2 of the *relative* deviations 
        base = np.abs(self.targets_y)

        # move base so targets with a small base (at TE) don't become overweighted 
        shift = np.max (base) * 0.4    
        norm2 = np.linalg.norm (devi / (base+shift))

        return norm2 



class Match_Geo_Bezier (Match_Bezier):
    """ 
    Controller for matching a airfoil 'Geometry' (upper and lower side) with 
    joined Bezier curves of upper and lower side

    self uses Match_Side_Bezier for evaluating the deviation for a single side
    """
    def __init__ (self, geo : 'Geometry_Bezier', target_geo: 'Geometry'):
        super().__init__ ()

        self._has_joined_sides = geo.has_joined_sides

        self._match_upper = Match_Side_Bezier (geo.upper, target_geo.upper)
        self._match_lower = Match_Side_Bezier (geo.lower, target_geo.lower)

        self.nvar = self._match_upper.nvar + self._match_lower.nvar
        self.ncp  = self._match_upper.ncp  + self._match_lower.ncp

    @property
    def geo_result (self) -> 'Geometry_Bezier':
        """ returns a new Geometry_Bezier as result of match run 
        """
        # get beziers of upper and lower - build new Geometry_Bezier out of them 
        bezier_upper = self._match_upper.bezier
        bezier_lower = self._match_lower.bezier

        geo = Geometry_Bezier (has_joined_sides = self._has_joined_sides)
        geo.set_newSide_for (UPPER, bezier_upper.points_x, bezier_upper.points_y)
        geo.set_newSide_for (LOWER, bezier_lower.points_x, bezier_lower.points_y)

        return geo 


    def _map_bezier_to_variables (self): 
        """ maps bezier control points to design variables of objective function
        Returns: 
            list of design variables """

        vars_upper = self._match_upper._map_bezier_to_variables()
        vars_lower = self._match_lower._map_bezier_to_variables()

        # join the vars  - excluding le tangent of lower (joined beziers)
        if self._has_joined_sides:
            vars_lower = vars_lower [1:]

        return vars_upper + vars_lower 


    def _map_variables_to_bezier (self, variables: list): 
        """ maps design variables to bezier (control points)"""

        nvar_upper = self._match_upper.nvar

        vars_upper = variables [0:nvar_upper]
        vars_lower = variables [nvar_upper:]

        # if joined - take the negative le tangent for lower side 
        if self._has_joined_sides:
            vars_lower = np.insert (vars_lower, 0, -vars_upper[0])  

        self._match_upper._map_variables_to_bezier (vars_upper)
        self._match_lower._map_variables_to_bezier (vars_lower)

    def _deviation_to_target_applying (self, variables) -> list:
        """deviation array as the sum of upper and lower side"""

        nvar_upper = self._match_upper.nvar

        vars_upper = variables [0:nvar_upper]
        vars_lower = variables [nvar_upper:]

        # if joined - take the negative le tangent for lower side 
        if self._has_joined_sides:
            vars_lower = np.insert (vars_lower, 0, -vars_upper[0])  

        devi_upper = self._match_upper._deviation_to_target_applying (vars_upper)
        devi_lower = self._match_lower._deviation_to_target_applying (vars_lower)

        return devi_upper + devi_lower
    

    def _objectiveFn (self, variables : list):  
        """ returns norm2 value of y deviations of self to target y at x """

        nvar_upper = self._match_upper.nvar

        vars_upper = variables [0:nvar_upper]
        vars_lower = variables [nvar_upper:]

        # if joined - take the negative le tangent for lower side 
        if self._has_joined_sides:
            vars_lower = np.insert (vars_lower, 0, -vars_upper[0])  

        norm2_upper = self._match_upper._objectiveFn (vars_upper)
        norm2_lower = self._match_lower._objectiveFn (vars_lower)

        norm2 = (norm2_upper ** 2 + norm2_lower ** 2) ** 0.5
        # print (f"{norm2_upper:.6f}  {norm2_lower:.6f}  {norm2:.6f}")
        return norm2




# -----------------------------------------------------------------------------
#  Curvature Classes 
# -----------------------------------------------------------------------------


class Curvature_Abstract:
    """
    Curvature of geometry spline at (x,y) 
    """
    def __init__ (self):
        self._upper    = None                   # upper side curvature as Side_Airfoil
        self._lower    = None                   # lower side curvature as Side_Airfoil

    @property
    def upper (self) -> 'Side_Airfoil': return self._upper

    @property
    def lower (self) -> 'Side_Airfoil': return self._lower  



class Curvature_of_xy (Curvature_Abstract):
    """
    Curvature of (x,y) - using a new cubic spline for evaluation
    """

    def __init__ (self,  x : np.ndarray, y: np.ndarray):
        super().__init__()

        self._spline = Spline2D (x, y)
        print ("+++ new spline for curv")
        self._x      = x
        self._y      = y
        self._iLe = int(np.argmin (self._x))

    @property
    def upper (self): 
        " return Side_Airfoil with curvature on the upper side"
        if self._upper is None: 
            self._upper = Side_Airfoil (np.flip(self._x[: self._iLe+1]),
                                         np.flip(self.curvature [: self._iLe+1]), name=UPPER )
        return self._upper 

    @property
    def lower (self): 
        " return Side_Airfoil with curvature on the lower side"
        if self._lower is None: 
            self._lower = Side_Airfoil (self._x[self._iLe: ],
                                        self.curvature [self._iLe: ], name=LOWER )
        return self._lower 

    @property
    def curvature (self): 
        " return the curvature at knots 0..npoints"     
        return self._spline.curvature (self._spline.u)  



class Curvature_of_Spline (Curvature_Abstract):
    """
    Curvature of geometry spline - - using existing spline for evaluation
    """

    def __init__ (self, spline: Spline2D):
        super().__init__()

        self._spline = spline 
        self._x      = spline.x
        self._iLe    = int(np.argmin (self._x))

    @property
    def upper (self): 
        " return Side_Airfoil with curvature on the upper side"
        if self._upper is None: 
            self._upper = Side_Airfoil (np.flip(self._x[: self._iLe+1]),
                                         np.flip(self.curvature [: self._iLe+1]), name=UPPER )
        return self._upper 

    @property
    def lower (self): 
        " return Side_Airfoil with curvature on the lower side"
        if self._lower is None: 
            self._lower = Side_Airfoil (self._x[self._iLe: ],
                                         self.curvature [self._iLe: ], name=LOWER )
        return self._lower 

    @property
    def curvature (self): 
        " return the curvature at knots 0..npoints"     
        return self._spline.curvature (self._spline.u)  



class Curvature_of_Bezier (Curvature_Abstract):
    """
    Curvature of Bezier based geometry - is build from curvature of upper and lower side 
    """

    def __init__ (self,  upper : 'Side_Airfoil_Bezier' , lower : 'Side_Airfoil_Bezier'):
        super().__init__()

        self._upper_side = upper
        self._lower_side = lower

    @property
    def upper (self): 
        " return Side_Airfoil with curvature on the upper side"
        if self._upper is None: 
            self._upper = Side_Airfoil (self._upper_side.x, 
                                         - self._upper_side.curvature.y, name=UPPER)
        return self._upper 

    @property
    def lower (self): 
        " return Side_Airfoil with curvature on the lower side"
        if self._lower is None: 
            self._lower = Side_Airfoil (self._lower_side.x, 
                                         self._lower_side.curvature.y, name=LOWER)
        return self._lower 

    @property
    def curvature (self): 
        " return the curvature at knots 0..npoints"     
        return np.concatenate ((np.flip(self.upper.y), self.lower.y[1:]))




# -----------------------------------------------------------------------------
#   Side_Airfoil Classes 
# -----------------------------------------------------------------------------


class Side_Airfoil: 
    """ 
    1D line of an airfoil like upper, lower side, camber line, curvature etc...
    with x 0..1

    Implements basic linear interpolation. 
    For higher precision use Side_Airfoil_Spline

    """

    def __init__ (self, x,y, name=None):

        self._x         = x
        self._y         = y
        self._name      = name 
        self._threshold = 0.1                   # threshold for reversal dectection 
        self._maximum   = None                  # the highpoint of the spline line

    @property
    def x (self): return self._x
    
    @property
    def y (self): return self._y
    def set_y (self, anArr): 
        self._y = anArr
    
    @property
    def name (self): return self._name
    def set_name (self,aName): 
        self._name = aName
    

    @property
    def threshold (self):   return self._threshold 
    def set_threshold (self, aVal): 
        self._threshold =aVal 

    @property
    def isNormalized (self) -> bool:
        """ true if x[0] == y[0] ==0.0 and x[-1] = 1.0 """
        return self.x[0] == 0.0 and self.y[0] == 0.0 and self.x[-1] == 1.0

    @property
    def maximum (self): 
        """ 
        x,y position of the maximum y value of self

        ! The accuracy of linear interpolation is about 1% compared to a spline or bezier
          based interpolation 
        """
        if self._maximum is None: 
            self._maximum = self._get_maximum()
        return self._maximum 

    @property
    def nreversals (self) -> int: 
        """ number of reversals """
        return len(self.reversals())

    def reversals (self, xStart= 0.1):
        """ 
        returns a list of reversals (change of y sign)
        A reversal is a tuple (x,y) indicating the reversal on self. 
        Reversal detect starts at xStart - to exclude turbulent leading area... 
        """
        # algorithm from Xoptfoil where a change of sign of y[i] is detected 
        reversals = []
        x = self.x
        y = self.y

        if not np.any (y < 0.0): return reversals       # early fail if all are positive       

        iToDetect = np.nonzero (x >= xStart)[0]

        yold    = y[iToDetect[0]]
        for i in iToDetect:
            if abs(y[i]) >= self.threshold:             # outside threshold range
                if (y[i] * yold < 0.0):                 # yes - changed + - 
                    reversals.append((round(x[i],7),round(y[i],7))) 
                yold = y[i]
        return reversals 
    

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
            self._reset()                

        if not newX is None: 
         
            self._moveMaxX (newX)                   # a little bit more complicated ...
            self._reset()
    

    def yFn (self,x):
        """ returns interpolated y values based on a x-value  """

        # find the index in self.x which is right before x
        jl = bisection (self.x, x)
        
        # now interpolate the y-value on lower side 
        if jl < (len(self.x) - 1):
            x1 = self.x[jl]
            x2 = self.x[jl+1]
            y1 = self.y[jl]
            y2 = self.y[jl+1]
            y = interpolate (x1, x2, y1, y2, x)
        else: 
            y = self.y[-1]
        return y



    # ------------------ private ---------------------------


    def _get_maximum (self): 
        """ 
        calculates and returns the x,y position of the maximum y value of self
        """
        max_y = abs(np.max(self.y))
        min_y = abs(np.min(self.y))
        
        if max_y == 0.0 and min_y == 0.0:              # optimize 
            xmax = 0.0 
            ymax = 0.0 
        else: 
            if max_y > min_y:                   # upper side 
                imax = np.argmax(self.y)
            else:                               # lower side
                imax = np.argmin(self.y)

            # build a little helper spline to find exact maximum
            if imax > 3 and imax < (len(self.x) -3 ): 

                istart = imax - 3
                iend   = imax + 3
                self._max_spline = Spline1D (self.x[istart:iend+1], self.y[istart:iend+1])

                # nelder mead search
                xstart = self.x[istart]
                xend   = self.x[iend]
                if max_y > min_y:                   # upper side 
                    xmax = findMax (self._yFn_max, self.x[imax], bounds=(xstart, xend))
                else:                               # lower side
                    xmax = findMin (self._yFn_max, self.x[imax], bounds=(xstart, xend))
                ymax = self._yFn_max (xmax)

                # print (f"delta x  {xmax - self.x[imax]:.5f}" )
                # print (f"delta y  {ymax - max_y:.5f}" )
            else:
                xmax = self.x[imax]
                ymax = self.y[imax]

            # limit decimals to a reasonable value
            xmax = round(xmax, 7)
            ymax = round(ymax, 7)
        return xmax, ymax

    def _yFn_max (self,x):
        """ spline interpolated y values based on a x-value based on little maximum helper spline
        """

        if self._max_spline is None: 
            raise ValueError ("Helper spline for maximum evaluation missing")
        return self._max_spline.eval (x)


    def _moveMaxX (self, newMax):
        """ 
        moves the point of maximum to newMaxX  
        """
        raise NotImplementedError


    def _reset (self):
        """ reinit self if x,y has changed""" 
        self._maximum  = None                 # thickness distribution



class Side_Airfoil_Spline (Side_Airfoil): 
    """ 
    1D line of an airfoil like upper, lower side, camber line, curvature etc...
    with x 0..1

    Represented by a 1D spline

    """

    def __init__ (self, x,y, name=None):

        self._spline    = None                  # 1D Spline to get max values of line
        super().__init__ (x, y, name) 

  
    @property 
    def spline (self) -> Spline1D:
        """ spline representation of self """
        if self._spline is None: 
            self._spline = Spline1D (self.x, self.y)
        return self._spline




    def yFn (self,x):
        """ returns interpolated y values based on a x-value
        """
        return self.spline.eval (x)


    # ------------------ private ---------------------------


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
        # 1D spline with arccos is needed to avoid oscillations at LE for thickness distribution with high curvature

        tmpSpl = Spline1D (newX, self._y, arccos=True) 
        newY = tmpSpl.eval(self._x)

        # ensure start and end is really, really the same (numerical issues) 
        newY[0] = self._y[0]
        newY[-1] = self._y[-1]
        self._y = newY         


    def _reset (self):
        """ reinit self if x,y has changed""" 

        super()._reset()
        self._spline     = None



class Side_Airfoil_Bezier (Side_Airfoil): 
    """ 
    1D line of an airfoil like upper, lower side based on a Bezier curce
    with x 0..1

    """

    isJoined = False                    # self is not a joined side 

    def __init__ (self, px, py, name=None, nPoints = 101):
        """
        1D line of an airfoil like upper, lower side based on a Bezier curve with x 0..1

        Parameters
        ----------
        px, py : array of control point coordinates 
        nPoints : number of points 
        name : either UPPER or LOWER
             
        """
        super().__init__(None, None, name=name)

        if px is None or py is None:
            raise ValueError ("Bezier points missing")
        else:
            self._bezier    = Bezier(px,py)             # the bezier curve 

        # Bezier needs a special u cosinus distribution as the points are bunched
        # by bezier if there is high curvature ... 

        self._u = self._u_distribution_bezier(nPoints)

        # eval Bezier for u - x,y - values will be cached in 'Bezier'
        self.bezier.eval(self._u)

        # the opposite side may be joined to self so LE tangent will have the same length 
        #  -> curvature equal on  upper and lower side 
        self._joined_side     = None    


    def _u_distribution_bezier (self, nPoints):
        # a special distribution for Bezier curve to achieve a similar bunching to splined airfoils

        # for a constant du the resulting arc length of a curve section (panel) is proportional the 
        # reverse of the curvature, so it fits naturally the need of airfoil paneling especially
        # at LE. For LE and TE a little extra bunching is done ...

        te_du_end = 0.5                             # size of last du compared to linear du
        te_du_growth = 1.4                          # how fast panel size will grow 

        le_du_start = 0.7                           # size of first du compared to linear du
        le_du_growth = 1.1                          # how fast panel size will grow 

        nPanels = nPoints - 1
        u  = np.zeros(nPoints)
        du = np.ones(nPanels)

        # start from LE backward - increasing du 
        du_ip = le_du_start 
        ip = 0
        while du_ip < 1.0:
            du[ip] = du_ip
            ip += 1
            du_ip *= le_du_growth

        # run from TE forward - increasing du 
        du_ip = te_du_end
        ip = len(du) - 1
        while du_ip < 1.0:
            du[ip] = du_ip
            ip -= 1
            du_ip *= te_du_growth

        # build u array and normalized to 0..1
        for ip, du_ip in enumerate(du):
            u[ip+1] = u[ip] + du_ip 

        u = u / u[-1]
        return u 


    @property
    def bezier(self) -> Bezier:
        """ returns the bezier object of self"""
        return self._bezier 

    @property
    def controlPoints (self): 
        """ bezier control points """
        return self.bezier.points
    def set_controlPoints(self, px_or_p, py=None):
        """ set the bezier control points"""
        self._bezier.set_points (px_or_p, py)

    @property
    def nPoints (self): 
        """ number of bezier control points """
        return len(self.bezier.points)
    def set_nPoints(self, nPoints):
        """ set the number of x,y points"""
        self._u = self._u_distribution_bezier(nPoints)


    @property
    def x (self):
        # overloaded bezier caches values
        return self.bezier.eval(self._u)[0]
    
    @property
    def y (self): 
        # overloaded bezier caches values
        return self.bezier.eval(self._u)[1]

    @property
    def curvature (self): 
        """returns a Side_Airfoil with curvature in .y """
        return Side_Airfoil (self.x, self.bezier.curvature(self._u), name='curvature')
   
    def set_maximum (self, newX=None, newY=None): 
        """ 
        set x,y of the mx point of self 
        """

        raise NotImplementedError
    
    @property
    def joined_side (self) -> 'Side_Airfoil_Bezier_Joined': 
        """ a side may be joined to self to allow equal LE tangent """
        return self._joined_side

    def set_joined_side (self, aSide: 'Side_Airfoil_Bezier_Joined'):

        if isinstance (aSide, Side_Airfoil_Bezier_Joined):
            self._joined_side = aSide
            aSide._set_p1y (- self.bezier.points_y[1])           # negative = opposite
        else: 
            raise ValueError ("Joined side is not of type 'Side_Airfoil_Bezier_Joined'")


    @property
    def has_joined_side (self) -> bool:
        """ does self has a joined side """
        return not self._joined_side is None 

    # ------------------

    def insert_controlPoint_at (self, x, y): 
        """ insert a new Bezier control point at x,y - taking care of order of points 
        Returns index, where inserted or None if not successful
                and x,y coordinates of new point after checks """

        px = self.bezier.points_x

        # never go beyond p0 and p-1
        if x <= px[0] or x >= px[-1]: return None, None, None

        # find index to insert - never before point 1
        for i_insert, pxi in enumerate (px):
            if i_insert > 1:                        # do not insert before p0 and p1 
                if pxi > x: 
                    break 

        cpoints =  deepcopy(self.bezier.points) 
        cpoints.insert (i_insert, (x, y)) 

        # check if distance to neighbour is ok 
        px_new, py_new = zip(*cpoints)
        dx_right = abs (x - px_new[i_insert+1])
        dx_left  = abs (x - px_new[i_insert-1])
        if (dx_right < 0.01) or (dx_left < 0.01): 
           return None, None, None

        # update Bezier now, so redraw wil use the new curve 
        self.bezier.set_points (cpoints )

        return i_insert, x, y 
    

    def delete_controlPoint_at (self, index): 
        """ delete a  Bezier control point at index - point 0,1 and n-1 are not deleted 
        Returns index if successful - or None"""

        cpoints =  deepcopy(self.bezier.points) 

        # never delete point 0, 1 and -1
        noDelete = [0,1, len(cpoints)-1]

        if not index in noDelete:
            del cpoints [index]
            # update Bezier now, so redraw wil use the new curve 
            self.bezier.set_points (cpoints )
            return index
        else: 
            return None    


    def move_controlPoint_to (self, index, x, y, allow_overtook=False): 
        """ move Bezier control point to x,y - taking care of order of points. If 'allow_overtook' the new point
        may overtook its neighbour. 

        If x OR y is None, the coordinate is not changed

        Returns x, y of new (corrected) position """

        px = self.bezier.points_x
        py = self.bezier.points_y

        if x is None: x = px[index]
        if y is None: y = py[index]

        if index == 0:                          # fixed
            x, y = 0.0, 0.0 
        elif index == 1 and self.isJoined:      #  do not change LE tangent if self is joined 
            return px[1], py[1]
        elif index == len(px) - 2:              # not too close to TE   
            x = min (x, 0.95)       
        elif index == 1:                        # only vertical move
            x = 0.0 
            if py[index] > 0: 
                y = max (y,  0.006)             # LE not too sharp
            else: 
                y = min (y, -0.006)
        elif index == len(px) - 1:              # do not move TE gap   
            x = 1.0 
            y = py[index]                       
        elif not allow_overtook:                      # not too close to neighbour, do not allow to swap position  
            x = min (x, px[index+1] - 0.03)
            x = max (x, px[index-1] + 0.03)

        self.bezier.set_point (index, x,y) 

        # if self has a joined side, set LE control point of it
        if self.has_joined_side and index == 1:
            self.joined_side._set_p1y (-y)                 # negative = opposite

        return x, y 

    @property
    def te_gap (self):
        """ returns y value of the last bezier control point which is half the te gap"""
        return self.bezier.points_y[-1]
    
    def set_te_gap (self, y): 
        """ set te Bezier control point to y to change te gap """
        px = self.bezier.points_x
        self.bezier.set_point (-1, px[-1], y) 


    def match_bezier_to (self, targetSide: Side_Airfoil) :
        """ 
        Optimizes self bezier to best fit to another airfoil side
        uses nelder meat root finding

        Returns: 
        :nvar:   number of optimization variables 
        score:  y-deviation at target points 
        niter:  number of iterations needed
        max_reached: max number of iterations reached 
        """

        #-- define targets x,y which should fit best 

        i = 1
        if   len(targetSide.x) > 120:
            step = 6
        elif len(targetSide.x) > 80:
            step = 5
        else: 
            step = 4
        targ_x = []
        targ_y = []
        while i < (len(targetSide.x) -1):
            targ_x.append(targetSide.x[i])
            targ_y.append(targetSide.y[i])
            i += step
        # print_array_compact (targ_y, header="targ_y")

        #-- (start) position of control points 

        ncp = self.nPoints
        cp_x, cp_y = self.get_initial_bezier(targetSide, ncp)

        self.set_controlPoints (cp_x, cp_y)
      
        #-- map control point x,y to optimization variable 

        vars = []
        for icp in range (ncp): 


            if icp == 0: 
                pass                                    # skip leading edge
            elif icp == ncp-1:                      
                pass                                    # skip trailing edge
            elif icp == 1:                      
                var_dict = {}
                var_dict['icp']    = icp                # le tangent only y
                var_dict['coord']  = 'y'  
                var_dict['start']  = round(cp_y[icp],6)                
                var_dict['bounds'] = None                
                vars.append(var_dict)
            else:                                       # all control points in between 
                var_dict = {}
                var_dict['icp']    = icp
                var_dict['coord']  = 'x'                
                var_dict['start']  = round(cp_x[icp],6) 
                if ncp > 3 and icp == ncp-2:            # special case: last point not too close to TE 
                    var_dict['bounds'] = (0.5, 0.95)
                else: 
                    var_dict['bounds'] =  None                
                vars.append(var_dict)

                var_dict = {}
                var_dict['icp']    = icp
                var_dict['coord']  = 'y'  
                var_dict['start']  = round(cp_y[icp],6)                
                var_dict['bounds'] = None                
                vars.append(copy(var_dict))

        nvar = len(vars)
        if nvar == 0: return 0.0, 0, False

        #-- direct initial population of bezier (old values could hurt move_point during melder nead)

        # bezier_tmp = deepcopy (self)                # optimizer will work with this copy 
        bezier_tmp = self  

        for i, var in enumerate (vars): 
            (px, py) = bezier_tmp.bezier.points[var['icp']]
            if var['coord'] == 'x':  px = var['start']
            else:                    py = var['start']
            bezier_tmp.bezier.set_point (var['icp'], px, py)

        #-- map vars to nelder mead vars 

        var_start  = [0] * nvar 
        var_bounds = [None] * nvar 
        for i, var in enumerate (vars): 
            var_start[i]  = var['start']
            var_bounds[i] = var ['bounds']

        # print_array_compact (var_start, header="var_start")

        # ----- objective function

        f = lambda vars_value : _match_y_objectiveFn (bezier_tmp, targ_x, targ_y, vars, vars_value) 

        # ----- nelder mead find minimum --------

        max_iter = nvar * 70 

        res, niter = nelder_mead (f, var_start,
                    step=0.05, no_improve_thr=1e-5,             
                    no_improv_break=25, max_iter=max_iter,
                    bounds = var_bounds)

        var_result = res[0]
        score = res[1]

        #-- write back result in var_dict for convinience 

        for i, var in enumerate (vars): 
            var['result'] = var_result[i]

        #-- finally move control points to their new position 
        #   print_array_compact (var_result, header="vars")

        for i, var in enumerate (vars): 
            if var['coord'] == 'x':             # allow that a point overtooks its neighbour
                self.move_controlPoint_to (var['icp'] , var['result'], None, allow_overtook=True)    
            else: 
                self.move_controlPoint_to (var['icp'] , None, var['result'])    # x remains unchanged


        #-- evaluate the new y values on Bezier for the target x-coordinate
        y_new = np.zeros (len(targ_x))
        for i, target_x in enumerate(targ_x) :
            y_new[i] = self.bezier.eval_y_on_x(target_x, fast=False, epsilon=1e-7)

        devi = np.abs((y_new - targ_y))        
        norm2 = np.linalg.norm (devi)           # calc the 'real' norm2 (objFun uses another) 

        return norm2, niter, niter >= max_iter


    def set_initial_bezier (self, targetSide: Side_Airfoil, ncp):
        """ sets inital coordinates of control points  close to target side"""
        cp_x, cp_y = self. get_initial_bezier (targetSide, ncp)
        self.set_controlPoints (cp_x, cp_y)


    def get_initial_bezier (self, targetSide: Side_Airfoil, ncp):
        """ returns inital coordinates of control points """

        cp_x, cp_y = np.zeros(ncp), np.zeros(ncp)

        # initial x values 
        cp_x[0]   = 0.0                                 # LE and TE fix pos 
        cp_x[1]   = 0.0 
        cp_x[-1]  = 1.0

        np_between =  ncp - 3                           # equal distribution between                         
        dx = 1.0 / (np_between + 1)
        x = 0.0 
        for ib in range(np_between): 
            icp = 2 + ib
            x += dx
            cp_x[icp] = x

        # initial y values 
        for icp, xi in enumerate (cp_x): 
            if icp == 1:                                # special case y-start value of point 1
                x = 0.1                                 #       take y-coord near LE
            else: 
                x = xi          
            cp_y[icp]  = round(targetSide.yFn (x), 6)                 

        return cp_x, cp_y




class Side_Airfoil_Bezier_Joined (Side_Airfoil_Bezier): 
    """ 
    1D line of an airfoil like upper, lower side based on a Bezier curce
    with x 0..1

    A 'joined' side get's the LE tangent point from the master side.
    So the two sides, upper and lower will be connected at LE having the same 
    curvature.

    """
 
    isJoined = True                    # self is a joined side 

    def _set_p1y (self, y): 
        """ set the y-coordinate of P1 which is the LE tangent """

        self.bezier.set_point ( 1, 0.0, y) 




# -----------------------------------------------------------------------------
#  Geometry Classes 
# -----------------------------------------------------------------------------



class Geometry (): 
    """ 
    Basic geometry strategy class - uses linear interpolation of points 

    no repanel 
    no curvature
    no move of high points of thickness and camber 

    """

    isBasic = True 
    description = "based on linear interpolation"

    sideDefaultClass = Side_Airfoil

    def __init__ (self, x : np.ndarray, y: np.ndarray):

        self._x_org = x                         # copy as numpy is used in geometry 
        self._y_org = y

        self._x = None   
        self._y = None

        self._thickness : Side_Airfoil = None  # thickness distribution
        self._camber    : Side_Airfoil = None  # camber line

        self._curvature : Curvature_of_Spline     = None  # curvature object


    def __repr__(self) -> str:
        # overwritten to get a nice print string 
        return f"{type(self).__name__} nPoints: {self.nPoints}"


    @property
    def x (self): 
        return self._x_org if self._x is None else self._x
        
    @property
    def y (self):
        return self._y_org if self._y is None else self._y

    @property
    def xy (self):
        return self.x, self.y

    @property
    def iLe (self) -> int: 
        """ the index of leading edge in x coordinate array"""
        return int(np.argmin (self.x))

    @property
    def isNormalized (self):
        """ true if LE is at 0,0 and TE is symmetrical at x=1"""

        # LE at 0,0? 
        xle, yle = self.x[self.iLe], self.y[self.iLe]
        normalized =  xle == 0.0 and yle == 0.0

        # TE at 1? - numerical issues happen at the last deicmal (numpy -> python?)  
        xteUp,  yteUp  = self.x[ 0], round(self.y[ 0],10),
        xteLow, yteLow = self.x[-1], round(self.y[-1],10)
        if xteUp != 1.0 or xteLow != 1.0: 
            normalized = False 
        elif yteUp != - yteLow: 
            normalized = False        

        return normalized
    
    @property
    def le (self) -> tuple: 
        """ coordinates of le defined by the smallest x-value (iLe)"""
        return round(self.x[self.iLe],7), round(self.y[self.iLe],7)      
    
    @property
    def le_real (self) -> tuple: 
        """ coordinates of le defined by a virtual curve (eg spline)"""
        # can be overloaded
        # for basic geometry equals to self.le
        return self.le      
    
    @property
    def te (self): 
        """ returns trailing edge upper and lower x,y of point coordinate data """
        return self.x[0], self.y[0], self.x[-1], self.y[-1]
  
    @property
    def teGap (self): 
        """ trailing edge gap"""
        return  self.y[0] - self.y[-1]

    @property
    def nPanels (self): 
        """ number of panels """
        return self.nPoints - 1
      
    @property
    def nPoints (self): 
        """ number of coordinate points"""
        return len (self.x)



    @property 
    def panelAngle_le (self): 
        """returns the panel angle of the 2 panels at leading edge - should be less 170"""

        # panang1 = atan((zt(2)-zt(1))/(xt(2)-xt(1))) *                &
        #           180.d0/acos(-1.d0)
        # panang2 = atan((zb(1)-zb(2))/(xb(2)-xb(1))) *                &
        #           180.d0/acos(-1.d0)
        # maxpanang = max(panang2,panang1)
        ile = self.iLe
        dx = self.x[ile-1] - self.x[ile]
        dy = self.y[ile-1] - self.y[ile]
        if dx > 0.0:
            angleUp = math.atan (dy/dx) * 180.0 / math.acos(-1)
        else: 
            angleUp = 90 

        dx = self.x[ile+1] - self.x[ile]
        dy = self.y[ile] - self.y[ile+1]
        if dx > 0.0:
            angleLo = math.atan (dy/dx) * 180.0 / math.acos(-1)
        else: 
            angleLo = 90 

        if angleUp < 90.0 and angleLo < 90.0: 
            angle = angleUp + angleLo           # total angle 
        else: 
            angle = 180.0                       # pathologic case with vertical le panel
        return angle 

    @property
    def panelAngle_min (self): 
        """ returns the min angle between two panels - something between 160-180° - 
        and the point index of the min point"""
        return np.min(panel_angles(self.x,self.y)),  np.argmin(panel_angles(self.x,self.y))       


    @property
    def upper(self) -> 'Side_Airfoil': 
        """the upper surface as a line object - where x 0..1"""
        return self.sideDefaultClass (np.flip (self.x [0: self.iLe + 1]),
                              np.flip (self.y [0: self.iLe + 1]), name=UPPER)
            
    @property
    def lower(self) -> 'Side_Airfoil': 
        """the lower surface as a line object - where x 0..1"""
        return self.sideDefaultClass (self.x[self.iLe:], self.y[self.iLe:], name=LOWER)


    @property
    def camber (self) -> 'Side_Airfoil': 
        """ return the camber line """
        if self._camber is None: 
            self._eval_thickness_camber()
        return self._camber

    @property
    def thickness (self) -> 'Side_Airfoil': 
        """ the thickness distribution as a line object """
        if self._thickness is None: 
            self._eval_thickness_camber()
        return self._thickness

    @property
    def maxThick (self): 
        """ norm max thickness """
        return self.thickness.maximum[1]

    @property
    def maxThickX (self): 
        """  max thickness norm x-Position"""
        return self.thickness.maximum [0]

    @property
    def maxCamb (self): 
        """ norm max camber """
        return self.camber.maximum [1]

    @property
    def maxCambX (self): 
        """ max camber norm x-Position """
        return self.camber.maximum [0]
    
    @property
    def curvature (self) -> Curvature_of_xy: 
        " return the curvature object"
        if self._curvature is None: 
            self._curvature = Curvature_of_xy (self.x, self.y)  
        return self._curvature 




    def set_teGap (self, newGap, xBlend = 0.8):
        """ set self te gap.
         The procedere is based on xfoil allowing to define a blending distance from le.

        Arguments: 
            newGap:   in y-coordinates - typically 0.01 or so 
            xblend:   the blending distance from trailing edge 0..1 - Default 0.8
        """

        # currently le must be at 0,0 - te must be at 1,gap/2 (normalized airfoil) 
        if not self.isNormalized: 
            self.normalize ()
            if not self.isNormalized: 
                ErrorMsg ("Airfoil can't be normalized. Te gap can't be set.")
                return  
        
        x = np.copy (self.x) 
        y = np.copy (self.y) 
        xBlend = min( max( xBlend , 0.0 ) , 1.0 )

        gap = y[0] - y[-1]
        dgap = newGap - gap 
        ile = self.iLe

        # go over each point, changing the y-thickness appropriately
        for i in range(len(x)):
            # thickness factor tails off exponentially away from trailing edge
            if (xBlend == 0.0): 
                tfac = 0.0
                if (i == 0 or i == (len(x)-1)):
                    tfac = 1.0
            else:
                arg = min ((1.0 - x[i]) * (1.0/xBlend -1.0), 15.0)
                tfac = np.exp(-arg)

            if i <= ile: 
                y[i] = y[i] + 0.5 * dgap * x[i] * tfac # * gap 
            else:
                y[i] = y[i] - 0.5 * dgap * x[i] * tfac # * gap   

        self._x, self._y = x, y 



    def set_maxThick (self, newY): 
        """ change max thickness"""
        self.thickness.set_maximum(newY=newY)
        self._rebuildFromThicknessCamber()

    def set_maxThickX (self,newX): 
        """ change max thickness x position"""
        self.thickness.set_maximum(newX=newX)
        self._rebuildFromThicknessCamber()

    def set_maxCamb (self, newY): 
        """ change max camber"""
        self.camber.set_maximum(newY=newY)
        self._rebuildFromThicknessCamber()

    def set_maxCambX (self,newX): 
        """ change max camber x position"""
        self.camber.set_maximum(newX=newX)
        self._rebuildFromThicknessCamber()


    def upper_new_x (self, new_x): 
        """
        returns side_Airfoil having new_x and new, calculated y coordinates
        
        Using linear interpolation - shall be overloaded 
        """
        # evaluate the corresponding y-values on lower side 
        upper_y = np.zeros (len(new_x))
        for i, x in enumerate (new_x):
            upper_y[i] = self.upper.yFn(x)

        upper_y = np.round(upper_y, 10)

        return self.sideDefaultClass (new_x, upper_y, name=UPPER)


    def lower_new_x (self, new_x): 
        """
        returns side_Airfoil having new_x and new, calculated y coordinates
        
        Using linear interpolation - shall be overloaded 
        """
        # evaluate the corresponding y-values on lower side 
        lower_y = np.zeros (len(new_x))
        for i, x in enumerate (new_x):
            lower_y[i] = self.lower.yFn(x)

        lower_y = np.round(lower_y, 10)

        return self.sideDefaultClass (new_x, lower_y, name=LOWER)


    def normalize (self) -> bool:
        """
        Shift, rotate, scale airfoil so LE is at 0,0 and TE is symmetric at 1,y
        
        Returns True if it was normaized in self._x and _y
        """

        if self.isNormalized: return False

        # ensure a copy of x,y 
        if self._x is None:
            self._x = np.asarray (self.x)
            self._y = np.asarray (self.y) 

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
        print ("++++ normalize ", yn[0])
        xn[ile] = 0.0 
        yn[ile] = 0.0 
        xn[0]   = 1.0 
        xn[-1]  = 1.0

        self._x = np.round (xn, 10)
        self._y = np.round (yn, 10) 

        # re-init 
        self._reset_lines()                     # the child lines like thickness
        self._reset_spline()                    # the spline (if exists)

        return True


    def repanel (self, **kwargs):
        """repanel self with a new cosinus distribution 

            to be overloaded
        """
        raise NotImplementedError
    

    def strak (self, geo1_in : 'Geometry', geo2_in : 'Geometry', blendBy):
        """ straks (blends) self out of two geometries depending on the blendBy factor"""

        # create copies which have the same geomtery class as self
        #   so result has the same accuracy like self (--> upper_new_x!)

        geo1 = self.__class__ (np.copy(geo1_in.x), np.copy(geo1_in.y))
        geo2 = self.__class__ (np.copy(geo2_in.x), np.copy(geo2_in.y))

        if not geo1.isNormalized: geo1.normalize()
        if not geo2.isNormalized: geo2.normalize()

        blendBy = max (0.0, blendBy)
        blendBy = min (1.0, blendBy)
      
        # the leading airfoil is the one with higher share

        if blendBy <= 0.5:                      # the closer airfoil provides x-coordinates
 
            upper1 = geo1.upper
            lower1 = geo1.lower
            upper2 = geo2.upper_new_x (geo1.upper.x)
            lower2 = geo2.lower_new_x (geo1.lower.x)

            upper_x  = geo1.upper.x
            lower_x  = geo1.lower.x

        else:

            upper1 = geo1.upper_new_x (geo2.upper.x)
            lower1 = geo1.lower_new_x (geo2.lower.x)
            upper2 = geo2.upper
            lower2 = geo2.lower

            upper_x  = geo2.upper.x
            lower_x  = geo2.lower.x

        # now blend upper and lower of both airfoils 
        upper_y = (1 - blendBy) * upper1.y + blendBy * upper2.y
        lower_y = (1 - blendBy) * lower1.y + blendBy * lower2.y
        
        # rebuild x,y coordinates 
        self._x = np.concatenate ((np.flip(upper_x), lower_x[1:]))
        self._y = np.concatenate ((np.flip(upper_y), lower_y[1:]))


    # ------------------ private ---------------------------


    def _eval_thickness_camber (self): 
        """
        evalutes self thickness and camber distribution as Side_Airfoil objects
        with a x-distribution of the upper side.
        
        Using linear interpolation - shall be overloaded 

        Note: It's an approximation as thickness is just the sum of y_upper(x) and y_lower(x)
              and camber is just the mean value y_upper(x) and y_lower(x)
        """

        # evaluate the corresponding y-values on lower side 

        # handle not normalized airfoil - without changing self
        #   --> tmp new geo which will be normalized 

        if not self.isNormalized:
            geo_norm = self.__class__(np.copy(self.x), np.copy(self.y))
            geo_norm.normalize()
            upper = geo_norm.upper
            lower = geo_norm.lower_new_x (upper.x) 
            print ("#### normalizing for thickness")
        else: 
            upper = self.upper
            lower = self.lower_new_x (upper.x)

        # thickness and camber can now easily calculated 

        self._thickness = self.sideDefaultClass (upper.x, (upper.y - lower.y), 
                                            name='Thickness distribution')
        self._camber    = self.sideDefaultClass (upper.x, (upper.y + lower.y) / 2.0, 
                                            name='Camber line')

        # for symmetric airfoil with unclean data set camber line to 0 
        if np.max(self._camber._y) < 0.00001: 
            self._camber._y = np.zeros (len(self._camber._y))

        if not self.thickness.isNormalized or not self.camber.isNormalized:
            raise ValueError ("eval thickness: Thickness or Camber are not normalized")

        return 


    def _rebuildFromThicknessCamber(self):
        """ rebuilds self out of thickness and camber distribution """

        # x values of camber and thickness must be equal
        if not np.array_equal (self.thickness.x, self.camber.x):
            raise ValueError ("Geo rebuild: x-values of thickness and camber are not equal")
        if not self.thickness.isNormalized or not self.camber.isNormalized:
            self.thickness.isNormalized or not self.camber.isNormalized
            raise ValueError ("Geo rebuild: Thickness or Camber are not normalized")

        # easy sum of thickness and camber to get new airfoil 

        x_upper = self.thickness.x
        y_upper = self.camber.y + self.thickness.y / 2.0 
        x_lower = self.thickness.x
        y_lower = self.camber.y - self.thickness.y / 2.0

        self._x = np.concatenate ((np.flip(x_upper), x_lower[1:]))
        self._y = np.concatenate ((np.flip(y_upper), y_lower[1:]))

        # rseet only curvature 
        self._curvature = None



    def _reset_lines (self):
        """ reinit the dependand lines of self""" 
        self._thickness  = None                 # thickness distribution
        self._camber     = None                 # camber line
        self._curvature  = None                 # curvature 

    def _reset_spline (self):
        """ reinit self spline data if x,y has changed""" 
        # to be overloaded
        pass



class Geometry_Splined (Geometry): 
    """ 
    Geometry with a 2D cubic spline representation of airfoil all around the contour
    
    The 2D spline is used to get the best approximation of the airfoil e.g. for re-paneling
    """

    isBasic = False
    description = "based on spline interpolation"

    sideDefaultClass = Side_Airfoil_Spline

    def __init__ (self, x,y):
        super().__init__(x,y)        


        self._spline : Spline2D          = None   # 2 D cubic spline representation of self
        self._uLe = None                          # leading edge  - u value 


    @property 
    def spline (self) -> Spline2D:
        """ spline representation of self """

        if self._spline is None: 
            self._spline = Spline2D (self.x, self.y)
            print ("*** new Spline")
        return self._spline

    @property
    def le_real (self): 
        """ le calculated based on spline """
        #overloading
        xLe, yLe = self.xyFn (self.uLe)   
        # + 0.0 ensures not to have -0.0 
        return round(xLe,7) + 0.0, round(yLe,7) + 0.0 
    
    @property
    def uLe (self): 
        """ u (arc) value of the leading edge """
        if self._uLe is None: 
            self._uLe = self._le_find()
        return self._uLe

    @property
    def _isLe_closeTo_le_real (self): 
        """ true if LE of x,y cordinates nearly equal to the real (splined) leading edge.
            If not the airfoil should be repaneled...! """

        xle, yle   = self.le
        xleS, yleS = self.le_real
        if abs(xle-xleS) > 0.00001 or abs(yle-yleS) > 0.00001: 
            return False
        else: 
            return True

    @property
    def isNormalized (self):
        """ 
        true if LE is at 0,0 and TE is symmetrical at x=1
           and real le (spline) is close to le
        """
        return super().isNormalized and self._isLe_closeTo_le_real


    @property
    def curvature (self) -> Curvature_of_Spline: 
        " return the curvature object"
        if self._curvature is None: 
            self._curvature = Curvature_of_Spline (self.spline)  
        return self._curvature 

    @property
    def angle (self): 
        """ return the angle in degrees at knots"""
        return np.arctan (self.spline.deriv1(self.spline.u)) * 180 / np.pi
    

    #-----------


    def upper_new_x (self, new_x) -> 'Side_Airfoil_Spline': 
        """
        returns side_Airfoil having new_x and new, calculated y coordinates
        
        Using spline interpolation  
        """
        # evaluate the corresponding y-values on lower side 
        upper_y = np.zeros (len(new_x))
 
        for i, x in enumerate (new_x):
 
            # nelder mead find min boundaries 
            uStart = 0.0
            uEnd   = self.uLe 
            uGuess = interpolate (0.0, 1.0, uStart, uEnd, x)   # best guess as start value

            u = findMin (lambda xlow: abs(self.spline.evalx(xlow) - x), uGuess, 
                        bounds=(uStart, uEnd), no_improve_thr=1e-8) 

            # with the new u-value we get the y value on lower side 
            upper_y[i] = self.spline.evaly (u)

        upper_y = np.round(upper_y, 10)

        return self.sideDefaultClass (new_x, upper_y, name=UPPER)



    def lower_new_x (self, new_x) -> 'Side_Airfoil_Spline': 
        """
        returns side_Airfoil having new_x and new, calculated y coordinates
        
        Using spline interpolation  
        """
        # evaluate the corresponding y-values on lower side 
        lower_y = np.zeros (len(new_x))
 
        for i, x in enumerate (new_x):

            # first and last point from current lower to avoid numerical issues 
            if i == 0: 
                lower_y[i] = self.lower.y[0]
            elif i == (len(new_x) -1):
                lower_y[i] = self.lower.y[-1]
            else:
 
                # nelder mead find min boundaries 
                uStart = self.uLe
                uEnd   = 1.0 
                uGuess = interpolate (new_x[0], new_x[-1], uStart, 1.0, x)   # best guess as start value

                u = findMin (lambda xlow: abs(self.spline.evalx(xlow) - x), uGuess, 
                            bounds=(uStart, uEnd), no_improve_thr=1e-8) 

                # with the new u-value we get the y value on lower side 
                lower_y[i] = self.spline.evaly (u)

        lower_y = np.round(lower_y, 10)

        return self.sideDefaultClass (new_x, lower_y, name=LOWER)


    def normalize (self):
        """Shift, rotate, scale airfoil so LE is at 0,0 and TE is symmetric at 1,y"""

        if self.isNormalized: return False

        # the exact determination of the splined LE is quite "sensibel"
        # on numeric issues (decimals) 
        # there try to iterate to a goord result 

        n = 0
        while not self.isNormalized and n < 10:
            n += 1
            self.repanel () 
            super().normalize()
        return True


    def xyFn (self,u): 
        " return x,y at spline arc u"
        return  self.spline.eval (u)


    def scalarProductFn (self,u): 
        """ return the scalar product of a vector from TE to u and the tangent at u
        Used for finding LE where this value is 0.0at u"""

        # exact trailing edge point 
        xTe = (self.x[0] + self.x[-1]) / 2
        yTe = (self.y[0] + self.y[-1]) / 2

        x,y = self.xyFn(u) 

        # vector 1 from te to point 
        dxTe = x - xTe
        dyTe = y - yTe
        # verctor2 tangent at point 
        dx, dy = self.spline.eval(u, der=1) 

        dot = dx * dxTe + dy * dyTe

        return dot 
        

    def repanel (self,  nPanels : int = None, 
                        nPan_upper : int = None, nPan_lower : int = None,
                        le_bunch : float = 0.84, te_bunch : float = 0.7):
        """repanel self with a new cosinus distribution.

        If no new panel numbers are defined, the current numbers for upper and lower side 
        remain intact. 

        Args:
            nPanels  (int, optional): new number of panels. Defaults to 200.
            le_bunch (float, optional): leading edge bunch. Defaults to 0.84.
            te_bunch (float, optional): trailing edge bunch. Defaults to 0.7.
        """

        # explicit panels for upper and lower have precedence 
        if not (nPan_upper and nPan_lower):

            # new total number of panels  
            if nPanels: 

                # in case of odd number of panels, upper side will have +1 panels 
                if nPanels % 2 == 0:
                    nPan_upper = int (nPanels / 2)
                    nPan_lower = nPan_upper
                else: 
                    nPan_lower = int(nPanels / 2)
                    nPan_upper = nPan_lower + 1 

            # keep the current number of panels 
            else:
                nPan_upper = self.iLe
                nPan_lower = self.nPanels - nPan_upper
        
        # new distribution for upper and lower - points = +1 
        u_cos_upper = cosinus_distribution (nPan_upper+1, le_bunch, te_bunch)
        u_new_upper = np.abs (np.flip(u_cos_upper) -1) * self.uLe

        u_cos_lower = cosinus_distribution (nPan_lower+1, le_bunch, te_bunch)
        u_new_lower = u_cos_lower * (1- self.uLe) + self.uLe

        # add new upper and lower 
        u_new = np.concatenate ((u_new_upper, u_new_lower[1:]))

        # new calculated x,y coordinates  
        x, y = self.xyFn(u_new)

        self._x = np.round (x, 10)
        self._y = np.round (y, 10)

        # reset the child lines, keep current spline as it was the master
        self._reset_lines()




    # ------------------ private ---------------------------


    def _reset_spline (self):
        """ reinit self spline data if x,y has changed""" 
        self._spline     = None
        self._uLe        = None                  # u value at LE 


    def _rebuildFromThicknessCamber(self):
        """ rebuilds self out of thickness and camber distribution """
        # overloaded to reset spline

        # keep current panel numbers of self 

        nPan_upper = self.iLe
        nPan_lower = self.nPanels - nPan_upper

        super()._rebuildFromThicknessCamber()

        # reset spline so it will be rebuild out of new coordinates

        self._reset_spline ()

        # when panel number changed with rebuild do repanel to get original number again 

        nPan_upper_new = self.iLe
        nPan_lower_new = self.nPanels - nPan_upper_new

        if (nPan_upper != nPan_upper_new) or (nPan_lower != nPan_lower_new):

            self.repanel (nPan_upper=nPan_upper,nPan_lower=nPan_lower)

            # repanel could lead to a slightly different le 
            self.normalize()


    def _le_find (self):
        """ returns u (arc) value of leading edge based on scalar product tangent and te vector = 0"""

        iLeGuess = np.argmin (self.x)          # first guess for Le point 
        # exact determination of root  = scalar product = 0.0 
        try: 
            uLe = findRoot (self.scalarProductFn, self.spline.u[iLeGuess-1] , bounds=(0.4, 0.6)) 
        except: 
            uLe = self.spline.u [iLeGuess]
            print ("Warn: Le not found - taking geometric Le")

        return uLe


    def get_y_on (self, side, xIn): 
        """
        Evalutes y values right on 'side' having x-values xIn.
        Note: if self isn't normalized, it will be normalized prior to evaluation

        Parameters
        ----------
        side : either UPPER or LOWER
        xIn : x-coordinates on 'side' to evaluate y
             
        Returns
        -------
        yOut : np array - evaluated y values 
        """

        iLe = np.argmin (self.x)

        if side == LOWER: 
            uStart = self.spline.u[iLe] 
            uEnd   = self.spline.u[-1]  
            uGuess = 0.75          
        elif side == UPPER:
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
        yOut = self.spline.evaly (ux)

        # ensure Le is 0,0 and Te is at 1
        if   xIn[0]  == self.x[iLe]:                    yOut[0]  = self.y[iLe]
        elif xIn[-1] == self.x[0]  and side == UPPER:   yOut[-1] = self.y[0]
        elif xIn[-1] == self.x[-1] and side == LOWER:   yOut[-1] = self.y[-1]

        return yOut 



class Geometry_Bezier (Geometry): 
    """ 
    Geometry based on two Bezier curves for upper and lower side
    """
    
    isBasic = False
    description = "based on 2 Bezier curves"

    sideDefaultClass = Side_Airfoil

    def __init__ (self, has_joined_sides = False):
        """new Geometry based on two Bezier curves for upper and lower side

        Args:
            has_joined_sides: upper and lower Bezier are joined at LE
        """
        super().__init__(None, None)        

        self._upper      = None                 # upper side as Side_Airfoil_Bezier object
        self._lower      = None                 # lower side 

        self._has_joined_sides = has_joined_sides  # upper and lower bezier are joined at LE 

    
    @property
    def isNormalized (self):
        """ true - Bezier is always normalized"""
        return True

    @property
    def has_joined_sides (self) -> bool:
        """ are upper and lower side joined at LE? """
        return self._has_joined_sides

    @property
    def upper(self) -> 'Side_Airfoil_Bezier' : 
        """upper side as Side_Airfoil_Bezier object"""
        # overloaded
        if self._upper is None: 
            # default side
            px = [   0,  0.0, 0.33,  1]
            py = [   0, 0.06, 0.12,  0]    
            self._upper = Side_Airfoil_Bezier (px, py, name=UPPER)
        return self._upper 

    @property
    def lower(self) -> 'Side_Airfoil_Bezier' : 
        """upper side as Side_Airfoil_Bezier object"""
        # overloaded
        if self._lower is None: 
            # default side 
            px = [   0,   0.0,  0.25,   1]
            py = [   0, -0.04, -0.07,   0]  
            if self.has_joined_sides:   
                self._lower = Side_Airfoil_Bezier_Joined (px, py, name=LOWER)
                self._upper.set_joined_side (self._lower)
            else: 
                self._lower = Side_Airfoil_Bezier (px, py, name=LOWER)

        return self._lower 
    
    def set_newSide_for (self, curveType, px,py): 
        """creates either a new upper or lower side in self
        curveType is either UPPER or LOWER """

        if not px is None and not py is None:
            if curveType == UPPER: 
                self._upper = Side_Airfoil_Bezier (px, py, name=UPPER)
            elif curveType == LOWER:
                if self.has_joined_sides:   
                    self._lower = Side_Airfoil_Bezier_Joined (px, py, name=LOWER)
                    self._upper.set_joined_side (self._lower)
                else: 
                    self._lower = Side_Airfoil_Bezier (px, py, name=LOWER)
            self._reset_lines()

    @property
    def x (self):
        # overloaded  - take from bezier 
        return np.concatenate ((np.flip(self.upper.x), self.lower.x[1:]))

    @property
    def y (self):
        # overloaded  - take from bezier 
        return np.concatenate ((np.flip(self.upper.y), self.lower.y[1:]))
    
    @property
    def nPoints (self): 
        """ number of coordinate points"""
        return len (self.upper.x) + len (self.lower.x) - 1

    @property
    def le (self) -> tuple: 
        """ coordinates of le - Bezier always 0,0 """
        return 0.0, 0.0     
    
    @property
    def le_real (self) -> tuple: 
        """ coordinates of le defined by a virtual curve- - Bezier always 0,0 """
        return self.le
        
    @property
    def teGap (self): 
        """ trailing edge gap in y"""
        #overloaded to get data from Bezier curves
        return  self.upper.te_gap - self.lower.te_gap



    def set_maxThick (self, newY): 
        raise NotImplementedError

    def set_maxThickX (self,newX): 
        raise NotImplementedError

    def set_maxCamb (self, newY): 
        raise NotImplementedError

    def set_maxCambX (self,newX): 
        raise NotImplementedError

    def set_teGap (self, newGap): 
        """ set trailing edge gap to new value which is in y"""
        #overloaded to directly manipulate Bezier
        newGap = max(0.0, newGap)
        newGap = min(5.0, newGap)
        self.upper.set_te_gap (  newGap / 2)
        self.lower.set_te_gap (- newGap / 2)

    @property
    def curvature (self) -> Curvature_of_Bezier: 
        " return the curvature object"
        if self._curvature is None: 
            self._curvature = Curvature_of_Bezier (self.upper, self.lower)  
        return self._curvature 



    def repanel (self,  nPanels : int = 200, 
                        nPan_upper : int = None, nPan_lower : int = None):
        """repanel self with a new cosinus distribution 

        Args:
            nPanels  (int, optional): new number of panels. Defaults to 200.
            le_bunch (float, optional): leading edge bunch. Defaults to 0.84.
            te_bunch (float, optional): trailing edge bunch. Defaults to 0.7.
        """

        # explicit panels for upper and lower have precedence 
        if not (nPan_upper and nPan_lower):

            # in case of odd number of panels, upper side will have +1 panels 
            if nPanels % 2 == 0:
                nPan_upper = int (nPanels / 2)
                nPan_lower = nPan_upper
            else: 
                nPan_lower = int(nPanels / 2)
                nPan_upper = nPan_lower + 1 

        # that's it with bezier    
        self.upper.set_nPoints (nPan_upper + 1)
        self.lower.set_nPoints (nPan_lower + 1)

        # reset chached values
        self._reset_lines()

    # ------------------ private ---------------------------


    def upper_new_x (self, new_x) -> 'Side_Airfoil': 
        """
        returns side_Airfoil having new_x and new, calculated y coordinates
        Using bezier interpolation  
        """
        # evaluate the corresponding y-values on upper side 
        upper_y = np.zeros (len(new_x))
 
        for i, x in enumerate (new_x):
            upper_y[i] = self.upper.bezier.eval_y_on_x (x, fast=True)  

        upper_y = np.round(upper_y, 10)

        return Side_Airfoil (new_x, upper_y, name=LOWER)
        

    def lower_new_x (self, new_x)  -> 'Side_Airfoil': 
        """
        returns side_Airfoil having new_x and new, calculated y coordinates
        Using bezier interpolation  
        """
        # evaluate the corresponding y-values on lower side 
        lower_y = np.zeros (len(new_x))
 
        # !! bezier must be evaluated with u to have x,y !! 
        for i, x in enumerate (new_x):

            # first and last point from current lower to avoid numerical issues 
            if i == 0: 
                lower_y[i] = self.lower.y[0]
            elif i == (len(new_x) -1):
                lower_y[i] = self.lower.y[-1]
            else:
                lower_y[i] = self.lower.bezier.eval_y_on_x (x, fast=True)  

        lower_y = np.round(lower_y, 10)

        return Side_Airfoil (new_x, lower_y, name=LOWER)


# ------------ helper funtions  -----------------------------------


def _match_y_objectiveFn (bezier_tmp : Side_Airfoil_Bezier, 
                          targets_x, targets_y, vars_def, vars_value ):  
    """ returns norm2 value of y deviations of self to target y at x """
        
    # the objective function is *not* in class Side_Bezier to ensure no side effects

    penalty = 0.0

    # set the new control point coordinates in Bezier (=None - do not change) 
    for i, var in enumerate (vars_def): 
        if var['coord'] == 'x':
            # jxjo - test with overtook allowed 
            new_value, _ = bezier_tmp.move_controlPoint_to (var['icp'] , vars_value [i], None, allow_overtook=True)    # y remains unchanged
        else: 
            _, new_value = bezier_tmp.move_controlPoint_to (var['icp'] , None, vars_value [i])    # x remains unchanged

        if vars_value [i] != new_value:                 # value hurted move constraint 
            # print ("   Var %d bounds rejection: %.5F - corrected to: %.5f" %(i, vars_value [i], new_value))
            penalty += 0.1   


    # evaluate the new y values on Bezier for the target x-coordinate
    y_new = np.zeros (len(targets_x))
    for i, target_x in enumerate(targets_x) :
        y_new[i] = bezier_tmp.bezier.eval_y_on_x(target_x, fast=False, epsilon=1e-7)


    # calculate norm2 of the *relative* deviations 
    devi = np.abs((y_new - targets_y))
    base = np.abs(targets_y)

    # move base so targets with a small base (at TE) don't become overweighted 
    shift = np.max (base) * 0.4
  
    norm2 = np.linalg.norm (devi / (base+shift))

    return norm2 + penalty * norm2



# ------------ test functions - to activate  -----------------------------------



if __name__ == "__main__":

    # ---- Test -----

    pass