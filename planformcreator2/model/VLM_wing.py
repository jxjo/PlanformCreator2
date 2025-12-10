#!/usr/bin/env python
# -*- coding: utf-8 -*-
""" 

Wing build with VLM_Panels based on Planform_Paneled

    Switch to metric [m] coordinates and wing coordinates with y in span

                  | u
                  V
                  .---- y       -> right wing tip 
                  |
                  x

                  
    VLM_Wing                            - panel mesh, Qjj matrix for certain Mach
        |-- VLM_Panel                   - single panel  
        |-- VLM_Polar                   - velocity 
                |-- VLM_OpPoint         - angle of attack 
        !-- aerogrid                    - input data structure of PanelAero 
"""
        
import numpy as np
from enum                           import StrEnum
from typing                         import NamedTuple
from math                           import isclose, degrees, radians

from .VLM                           import calc_Qjj
from airfoileditor.model.polar_set  import Polar_Set, RE_SCALE_ROUND_TO, re_from_v, Polar

import logging
logger = logging.getLogger(__name__)
# logger.setLevel(logging.DEBUG)


#-------------------------------------------------------------------------------
# helper classes  and functions 
#-------------------------------------------------------------------------------

class StrEnum_Extended (StrEnum):
    """ enum extension to get a list of all enum values"""
    @classmethod
    def values (cls):
        return [c.value for c in cls]


class OpPoint_Var (StrEnum_Extended):
    """ polar variables """
    Y               = "y position"               
    CL              = "Cl"                                  # viscous loop - Cl > cl_max = 0.0      
    CL_VLM          = "Cl VLM"                              # viscous loop  
    CL_VLM_LINEAR   = "Cl linear"                           # linear 
    CL_MAX          = "Airfoil cl max "               
    MAX_MASK        = "Cl VLM close MAX"                    # numpy mask where Cl_VLM reaches, exceeds CL_MAX              
    ERROR_MASK      = "VLM error"                           # numpy mask where VLM couldn't calculate Cp              
    ALPHA_MAX       = "Airfoil alpha max "               
    ALPHA           = "Alpha"               
    ALPHA_EFF       = "Alpha effective"               
    ALPHA_EFF_VLM   = "Alpha effective VLM"               
    ALPHA_IND       = "Alpha induced"  
    ALPHA0          = "Alpha Cl=0"             
    LIFT            = "Lift"
    LIFT_STRIPE     = "Lift of stripe"


class Point_3D (NamedTuple):
    """ point having x,y,z coordinates"""

    x : float 
    y : float
    z : float 

# AIR_RHO     = 1.225             # density air 
# AIR_NY      = 0.0000182         # kinematic viscosity


# def re_from_v (v : float, chord : float, round_to = 1000) -> float:
#     """ calc Re number from v (velocity)"""

#     re = round (v * chord * AIR_RHO / AIR_NY,0)

#     if isinstance (round_to, int) and round_to:
#         re = round (re / round_to, 0)
#         re = re * round_to

#     return re


# def v_from_re (re : float, chord : float, round_dec = 2) -> float:
#     """ calc v (velocity) from Renumber"""

#     v = re * AIR_NY / (chord * AIR_RHO)

#     if isinstance (round_dec, int):
#         v = round (v, round_dec)

#     return v


#-------------------------------------------------------------------------------
# Model   
#-------------------------------------------------------------------------------


class VLM_Panel:
    """
    Single panel as basis of VLM calculation 

    - defined by corner points
    - provides definition of vortex horseshoe 

    Just used as an intermediate object to build the 'aerogrid' datastructure
    from a wing planforms
    
    Initial geometric definition: 

                        
                 p3 o---------o p2
                    |         |
       u -->        |         |  
                    |         |
                 p0 o---------o p1
            y            
            |
            z.--- x    

    Horseshoe data
                      P3
                    +-o----------  
                    |         |
       u -->        | l  k  j |                 # 25%, 50%, 75% 
                    |         |
                    +-o----------  
            y         P1
            |
            z.--- x    
                     
    """ 

    def __init__ (self, p0 : Point_3D, p1 : Point_3D, p2 : Point_3D, p3 : Point_3D):
      
        self.p0 = np.array(p0)
        self.p1 = np.array(p1)
        self.p2 = np.array(p2)
        self.p3 = np.array(p3)

        # Intermediate values 

        #                   l_2
        #            p3 o---------o p2
        #               |         |
        #  u -->    b_1 | l  k  j | b_2
        #               |         |
        #            p0 o---------o p1
        #                   l_1

        l_1 = self.p1 - self.p0
        l_2 = self.p2 - self.p3
        b_1 = self.p3 - self.p0
        b_2 = self.p2 - self.p1
        l_m = (l_1 + l_2) / 2.0
        b_m = (b_1 + b_2) / 2.0

        # sanity - is panel distorted? 
        #          check if p3 is far behind x of p1 

        sheer = (self.p3[0] - self.p1[0]) / l_1[0]
        if sheer > 1.2:
            self.is_distorted = True 
            logger.warning (f"{self} sheer: {sheer:.1f} ")
        else: 
            self.is_distorted = False 

        # horseshoe data 

        self.l  = l_m [0]                                                   # length in x direction 
        self.b  = b_m [1]                                                   # width  in y direction 
        self.A  = np.linalg.norm(np.cross(l_m, b_m))                        # area
        self.N  = np.cross(l_1, b_1) / np.linalg.norm(np.cross(l_1, b_1))   # norm vector

        self.offset_l   = self.p0 + 0.25 * l_m + 0.5 * b_1
        self.offset_k   = self.p0 + 0.50 * l_m + 0.5 * b_1
        self.offset_j   = self.p0 + 0.75 * l_m + 0.5 * b_1

        self.offset_P1  = self.p0 + 0.25 * l_1 
        self.offset_P3  = self.p3 + 0.25 * l_2 


    def polygon_2D (self) -> tuple [list, list]:
        """ x,y polygon of self (for plotting)""" 

        x = np.array([self.p0[0], self.p1[0], self.p2[0], self.p3[0], self.p0[0]])        
        y = np.array([self.p0[1], self.p1[1], self.p2[1], self.p3[1], self.p0[1]])

        return x, y

    def __repr__(self) -> str:
        # overwrite to get a nice print string
        return f"<{type(self).__name__} at {self.p0[0]:.3f}, {self.p0[1]:.3f}>"


class VLM_Wing:

    """ 
    wing build with VLM_Panels based on Planform_Paneled

    """

    def __init__ (self, planform ):

        self._planform = planform                   # Planform_Paneled as base 

        self._panels_right  = None                  # VLM_Panels of the right wing side 
        self._has_distorted_panels = False          # indicate distored (bad) panels 

        self._aerogrid      = None                  # input datastructure of VLM calculation 
        self._Qjj           = None                  # matrix of aerodynamic influence coefficients
        self._BJJ           = None
        self._Gamma         = None                  # circulation
        self._Q_ind         = None 
        self._A_ges         = None                  # total panel area in m²
        self._y_stripes     = None                  # y (middle) of stripes 
        self._b_stripes     = None                  # width of stripes 

        self._polars        = {}                    # dict of polars


    def __repr__(self) -> str:
        # overwrite to get a nice print string
        return f"<{type(self).__name__}>"


    @property
    def panels_right (self) -> list [VLM_Panel]:
        """ panels of right half wing"""

        if self._panels_right is None:
            self._panels_right, self._has_distorted_panels = self._generate_panels_right ()
        return self._panels_right


    @property 
    def has_distorted_panels (self) -> bool: 
        """ True if some panels are too much distorted for VLM"""
        return self._has_distorted_panels
    
    @property
    def n_panels (self) -> int:
        """ number of panels"""
        return len(self.panels_right)

    @property
    def nx_panels (self) -> int:
        """ number of panels in x direction"""
        return self._planform.wx_panels

    @property
    def ny_panels (self) -> int:
        """ number of panels in y span direction"""
        return int (len(self.panels_right) / self.nx_panels) 

    @property
    def aerogrid (self) -> dict:
        """ aerogrid datastructure as input for VLM calculation"""
        if self._aerogrid is None: 
            self._aerogrid = self._build_aeorogrid (self.panels_right)
        return self._aerogrid 

    @property
    def A_ges (self) -> float:
        """ total panel area of half wing in m²""" 
        if self._A_ges is None: 
            A : np.ndarray  = self.aerogrid['A']
            self._A_ges = A.sum() 
        return self._A_ges

    @property
    def A_panels (self) -> np.ndarray:
        """ panel area  in m²""" 
        return self.aerogrid['A']

    @property
    def N (self) ->np.ndarray:
        """ array of panels normal verctor"""
        return self.aerogrid['N']


    @property
    def Qjj (self) -> np.ndarray:
        """ 
        Matrix of aerodynamic influence coefficients which is indepenend of velocity (only Mach)
            - as self.panels are only right hand side, symmetric left side will be added by VLM
        """

        if self._Qjj is None: 
            # Bjj matrix for induced drag is not needed 
            self._Qjj, _ = calc_Qjj (self.aerogrid, Ma=0.0, xz_symmetry = True)
            # logger.debug ( (f"{self} build Qjj"))

        return self._Qjj 


    @property
    def y_stripes (self) -> np.ndarray:
        """ y position of all panel stripes (middle of panel)"""

        if self._y_stripes is None: 
            y_pos = []
            for i in range (0, self.n_panels, self.nx_panels):
                y_pos.append (self.aerogrid['offset_l'][i][1])
            self._y_stripes = np.array(y_pos)
        return self._y_stripes


    @property
    def b_stripes (self) -> np.ndarray:
        """ width of all panel stripes"""

        if self._b_stripes is None: 
            b = []
            for i in range (0, self.n_panels, self.nx_panels):
                b.append (self.aerogrid['b'][i])
            self._b_stripes = np.array(b)
        return self._b_stripes



    def polar_at (self, vtas: float) -> 'VLM_Polar':
        """ returns polar for air speed vtas"""

        if not isinstance (vtas, (int, float)) or vtas <= 0.0:
            return None

        v = round (vtas, 1)                         #  ensure clean key for dict

        polar = self._polars.get (v, None)          # already exisiting 
        
        if not polar:
            polar = VLM_Polar (self, v)             # calculate new Polar and opPoints 
            self._polars[v] = polar 
            
        return polar 

    def has_polars (self) -> bool:
        """ True if at least one polar is existing"""
        return bool (self._polars)


    def remove_polar_at (self, vtas: float):
        """ removes polar for air speed vtas - will be calculated new on next request"""
        if not isinstance (vtas, (int, float)) or vtas <= 0.0:
            return 

        v = round (vtas, 1)                         # ensure clean key for dict
        self._polars.pop(v, None)                   # remove existing, None if not found



    # ----- private --------------------------------------------------
      

    def _generate_panels_right (self) -> tuple[list [VLM_Panel], bool]:
        """ 
        Generate and return all panels of right half wing 
        
        Returns:
            panels 
            has_distorted_panels    - indicate bad panels 
        """

        # Change to wing coordinates in [m]
        #
        #           | u
        #           V
        #       
        #           .---- y       -> right wing tip 
        #           |
        #           x

        from .wing   import Planform_Paneled
        planform : Planform_Paneled = self._planform 

        has_distorted_panels = False                                    # are there distorted panels  

        panels = []
        y_stations  = planform.x_stations()  
        cn_stations = planform.cn_rel_stations 

        for iy in range (len(y_stations) -1): 

            # get y values of one panel stripe 

            y1 = y_stations [iy]
            y2 = y_stations [iy+1]
            y1_arr = np.full (len(cn_stations), y1)            
            y2_arr = np.full (len(cn_stations), y2)

            # get x values of stripe                               

            le_x1, te_x1 = planform.le_te_at (y1)
            le_x2, te_x2 = planform.le_te_at (y2)

            x1_arr = le_x1 + cn_stations * (te_x1 - le_x1)
            x2_arr = le_x2 + cn_stations * (te_x2 - le_x2)

            # create n-1 panels of the stripe starting at le towards 

            x1_arr = x1_arr / 1000                      # in [m]
            x2_arr = x2_arr / 1000

            y1_arr = y1_arr / 1000
            y2_arr = y2_arr / 1000

            for ip in range (len(cn_stations) - 1):

                p0 = Point_3D (x1_arr[ip],   y1_arr[ip],   0.0)
                p1 = Point_3D (x1_arr[ip+1], y1_arr[ip+1], 0.0)
                p2 = Point_3D (x2_arr[ip+1], y2_arr[ip+1], 0.0)
                p3 = Point_3D (x2_arr[ip],   y2_arr[ip],   0.0)
                panel = VLM_Panel (p0, p1, p2, p3) 

                panels.append(panel)

                if panel.is_distorted:
                    has_distorted_panels = True 

        logger.debug (f"{self} created {len(panels)} panels")

        return panels, has_distorted_panels 
        

    def _build_aeorogrid (self, panels : list [VLM_Panel]) -> dict: 
        """ build the aeorgrid datastructure out of panel data """

        n = len(panels)

        l, b, A   = np.zeros (n), np.zeros (n), np.zeros (n)
        N         = np.zeros ((n,3)) 
        offset_l  = np.zeros ((n,3)) 
        offset_k  = np.zeros ((n,3)) 
        offset_j  = np.zeros ((n,3)) 
        offset_P1 = np.zeros ((n,3))
        offset_P3 = np.zeros ((n,3)) 

        for i, panel in enumerate (panels): 
                
            l [i] = panel.l
            b [i] = panel.b
            A [i] = panel.A
            N [i] = panel.N
            offset_l [i]  = panel.offset_l                
            offset_k [i]  = panel.offset_k                
            offset_j [i]  = panel.offset_j                
            offset_P1 [i] = panel.offset_P1                
            offset_P3 [i] = panel.offset_P3                

        aerogrid = { 
            'l': np.array(l),
            'b': np.array(b),
            'A': np.array(A),
            'N': np.array(N),
            'offset_l': np.array(offset_l),
            'offset_k': np.array(offset_k),
            'offset_j': np.array(offset_j),
            'offset_P1': np.array(offset_P1),
            'offset_P3': np.array(offset_P3),
            'n': n,
            }
        
        # logger.debug (f"{self} build aerogrid")

        return aerogrid 
    

    def test_calculation (self):

        n : int = self.aerogrid['n']
        N : np.ndarray  = self.aerogrid['N']
        A : np.ndarray  = self.aerogrid['A']
        A_m2 = A / 1000**2
        A_ges = A_m2.sum()
        N_T  = N.T

        Vtas = 25.0
        q_dyn = 1.225 / 2.0 * Vtas ** 2

        # step through panels stripe by stripe

        wj = np.ones(n) * 2.18 / Vtas
        cp = self.Qjj.dot(wj)

        Fxyz : np.ndarray = q_dyn * N_T * A_m2 * cp

        n  = len(self.panels_right) 
        nx = self.nx_panels

        lift = []
        y_pos = []
        Fz : np.ndarray = Fxyz[2]

        for i in range (0, n, nx):

            lift_stripe = Fz[i:(i+nx)].sum() 
            y_stripe    = self.aerogrid['offset_l'][i][1]
            A_stripe    = A_m2[i:(i+nx)].sum()
            b_stripe    = self.aerogrid['b'][i]
            lift_local  = lift_stripe / b_stripe
            cl_local    = lift_stripe / A_stripe
            logger.debug (f"{i:3} y={y_stripe:.1f}  lift={lift_local:.4f}  cl?={cl_local:.4f}")
            lift.append  (cl_local) 
            y_pos.append (y_stripe)

        return y_pos, lift 


class VLM_Polar:

    """ 
    VLM solution for certain velocity 
    """

    def __init__ (self, wing: VLM_Wing, vtas: float ):

        self.vlm_wing       = wing 
        self.vtas           = vtas                      # true air speed

        self._opPoints      = {}                        # dict of operating points
        self._error_reason  = []                        # list of error messages eg polar couldn't be loaded 
        self._airfoil_polars= {}                        # dict of wingSections airfoil polar 
        self._generating_airfoil_polars = False         # airfoil polars are currently generated 
        self._use_viscous_loop          = True          # in opPoint calculation

        self._alpha0_stripes= None                      # alpha0 per stripe interpolated from airfoil polar

        logger.debug (f"{self} created")


    def __repr__(self) -> str:
        # overwrite to get a nice print string 
        return f"<{type(self).__name__} {self.vtas:.1f}m/s >"

    @property
    def name (self) -> str:
        """ name of polar like T1-22.3-VLM"""
        if self.use_viscous_loop:
            v = "non_linear"
        else: 
            v = "linear"
        return f"T1-{self.vtas:.1f}-VLM_{v}"


    @property
    def opPoints (self) -> list ['VLM_OpPoint']:
        """ sorted list of existing opPoints of self"""
        ops_sorted = dict(sorted(self._opPoints.items()))
        return ops_sorted.values()


    @property
    def airfoil_polars (self) -> dict:
        """ dict of airfoil polars, key is the wingSection y position"""
        return self._airfoil_polars

    @property
    def error_reason (self) -> list[str]:
        """ list of reasons why self is not ready / airfoil polars couldn't be loaded"""
        return self._error_reason

    @property
    def is_generating_airfoil_polars (self) -> bool:
        """ airfoil polars are currently generated """  
        return self._generating_airfoil_polars 

    @property
    def is_ready_for_op_point (self) -> bool: 
        """ True if airfoil polas are completly loaded for opPoint calculation """
        return bool(self.airfoil_polars) 

    def get_ready_for_op_point (self) -> bool: 
        """ load all airfoil polars for opPoint calculation - return True if ready """
        if self.is_ready_for_op_point:
            return True
        else: 
            # try to load polars 
            self._load_airfoil_polars ()
            return self.is_ready_for_op_point


    def is_ready (self) -> bool: 
        """ True if airfoil polas are complete for opPoint calculation """
        if self.airfoil_polars:
            return True
        else: 
            # try again to laod polars 
            self._load_airfoil_polars ()
            # logger.warning (f"{self} not ready")
            return bool (self.airfoil_polars)

    @property 
    def use_viscous_loop (self) -> bool:
        """ use viscous loop - non-linear aero calculation"""
        return self._use_viscous_loop
    
    def set_use_viscous_loop (self, aBool : bool): 
        self._use_viscous_loop = aBool
        self._opPoints = {}                             # remove current results 


    def opPoint_at (self, alpha: float) -> 'VLM_OpPoint':
        """ returns opPoint at alpha - or None if airfoil polars are not ready"""

        if self.get_ready_for_op_point ():
            a = round (alpha, 1)                        # ensure clean key for dict
            try:
                opPoint = self._opPoints[a]             # already exisiting
            except:
                opPoint = VLM_OpPoint (self, a)         # calculate new opPOint 
                self._opPoints[a] = opPoint 
            return opPoint 
        else: 
            return None 

    def opPoint_at_alpha_max (self) -> 'VLM_OpPoint':
        """ returns opPoint at alpha max - or None if airfoil polars are not ready"""

        if self.get_ready_for_op_point ():
            return self._find_alpha_max ()
        else: 
            # e.g. airfoil polars not loaded up to now
            return None
        

    @property
    def q_dyn (self) -> float:
        """ dynamic pressure """
        return 1.225 / 2.0 * self.vtas ** 2

    @property
    def alpha0_stripes (self) -> np.ndarray:
        """ alpha_cl=0 from the airfoils at y position of panel stripes """
        if self._alpha0_stripes is None: 
            self._alpha0_stripes = self._get_alpha0_stripes ()
        return self._alpha0_stripes

    # ---- private ----


    def _load_airfoil_polars (self):
        """ loads for all wingSections polar of airfoil"""

        from .wing    import WingSection, Planform                          # avoid circular import

        self._airfoil_polars = {}                                           # reset
        self._error_reason  = []
        self._generating_airfoil_polars = False


        # get airfoil polars for all wingSections
        airfoil_polars = {}
        planform : Planform = self.vlm_wing._planform
        planform_paneled = planform.wing.planform_paneled 
        section  : WingSection

        for section in planform_paneled.wingSections_reduced():

            if not section.airfoil.isLoaded: 
                msg = f"{self} section {section} airfoil {section.airfoil} not loaded"
                logger.debug (msg)
                self._error_reason.append (msg)
                break

            section_y  = section.x / 1000
            section_re = re_from_v (self.vtas, section.c / 1000, round_to=RE_SCALE_ROUND_TO)
            airfoil_polarSet : Polar_Set = section.airfoil.polarSet

            if airfoil_polarSet is None: 
                msg = f"{self} section {section} airfoil {section.airfoil} has no polarSet"
                logger.error (msg)
                self._error_reason.append (msg)
                break

            airfoil_polarSet.load_or_generate_polars()

            for polar in airfoil_polarSet.polars:

                if isclose (polar.re, section_re, abs_tol=RE_SCALE_ROUND_TO):
                    if polar.isLoaded:

                        # there is a polar that fits to Re of wingSection
                        airfoil_polars[section_y] = polar

                    else: 
                        if polar.error_occurred: 
                            self._error_reason.append (polar.error_reason)
                        else: 
                            self._generating_airfoil_polars = True 
                    break

            if not isclose (polar.re, section_re, abs_tol=RE_SCALE_ROUND_TO): 
                polars_re_list = [f"{p.re:.0f}" for p in airfoil_polarSet.polars]
                msg = f"No polar with Re = {section_re:.0f} in Polarset of {section} with polars: {polars_re_list}" 
                logger.error (msg)
                self._error_reason.append (msg)
                 

        # if polar couldn't be loaded or error occured, reset airfoil polars 

        if self._error_reason : 
            logger.warning (f"{self} couldn't load polars - resetting airfoil polars")
        elif not self._generating_airfoil_polars:
            self._airfoil_polars = airfoil_polars 
            logger.debug (f"{self} loaded {len(self._airfoil_polars)} airfoil polars")       
        return  


    def _get_alpha0_stripes (self) -> np.ndarray:
        """ 
        returns alpha0 per stripe 
            - interpolated from alpha0 of airfoils at wingSections 
        """

        sections_alpha0 = []
        sections_y      = []

        # collect alpha0 of airfoils of wingSections from airfoil polar

        airfoil_polar : Polar
        for section_y, airfoil_polar in self.airfoil_polars.items():

            alpha0_polar = np.interp(0.0, airfoil_polar.cl, airfoil_polar.alpha)  # 'normal' alpha0 of polar'
                 
            sections_alpha0.append(alpha0_polar)
            sections_y.append(section_y)

        # alpha0 per stripe by interpolation of section alpha0 

        alpha0_stripes = np.interp (self.vlm_wing.y_stripes, sections_y, sections_alpha0)

        return alpha0_stripes
    

    def _find_alpha_max (self) -> 'VLM_OpPoint':
        """ find opPoint which is close before cl_max reached at a span position"""

        found_above = False 
        alpha_start = 0.0
        step        = 5.0
        alpha_max   = 25.0 + step
        for alpha in np.arange (alpha_start, alpha_max, step):
            if self.opPoint_at (alpha).cl_max_almost_reached:
                found_above = True 
                break

        if found_above: 
        
            found_above = False 
            alpha_start = alpha - step 
            step        = 1
            alpha_max   = alpha + step
            for alpha in np.arange (alpha_start, alpha_max, step):
                if self.opPoint_at (alpha).cl_max_almost_reached:
                    found_above = True 
                    break

            if found_above:  

                found_above = False 
                alpha_start = alpha - step 
                step        = 0.2
                alpha_max   = alpha + step
                for alpha in np.arange (alpha_start, alpha_max, step):
                    if self.opPoint_at (alpha).cl_max_almost_reached:
                        found_above = True 
                        break

        if not found_above: 
            logger.warning (f"{self} alpha_max not found ({len(self._opPoints)} opPoints")
            return None 
        else: 
            logger.debug (f"{self} found alpha_max {self.opPoint_at (alpha)}")
            return self.opPoint_at (alpha)


    def export_to_csv (self, pathFileName, alpha_start = -3.0, alpha_end :float = None, step = 0.5):
        """ 
        Write polar data alpha, Cl, Lift to csv 
            - if alpha_end is omitted, the polar ends when Cl is Cl_max / 2        
        """

        import csv

        alpha_max = 25.0                                # maximum for polar 

        with open(pathFileName, 'w', newline='') as csvfile:
            fieldnames = ['Alpha', 'CL', 'Lift']
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames, dialect='excel')

            writer.writer.writerow(["PlanformCreator2"])
            writer.writer.writerow([f"Wing Name :", f"{self.vlm_wing._planform.wing.name}"])
            writer.writer.writerow([f"Speed :", f"{self.vtas:.1f}"])
            writer.writer.writerow([ ])

            writer.writeheader()

            CL_max = -5.0                               # dummy initial value 

            for alpha in np.arange (alpha_start, alpha_max, step):
 
                opPoint = self.opPoint_at (alpha)
                writer.writerow({'Alpha': f"{opPoint.alpha:8.1f}", 'CL': f"{opPoint.CL:8.3f}", 'Lift': f"{opPoint.L:8.2f}"})

                if opPoint.CL < CL_max and opPoint.CL < CL_max / 2:     # handle also negative values
                    break 
                if opPoint.CL > CL_max:
                    CL_max = opPoint.CL



class VLM_OpPoint:
    """ 
    operation point having alpha of a VLM_Polar 
    """

    def __init__ (self, polar: VLM_Polar,  alpha : float ):

        self.alpha = alpha
        self.polar = polar 

        self._cp = None                             # pressure coefficient 
        self._aero_results        = {}              # viscous loop: dict with all results along span
        
        self._cl_max_reached = False                # a stripe has reached cl_max of airfoil
        self._VLM_error      = False                # error occured e.g because of bad paneling 

        # if there are no airfoil polars - break 

        if not self.airfoil_polars:
            raise ValueError ("Airfoil polars missing")


    def __repr__(self) -> str:
        # overwrite to get a nice print string 
        return f"<{type(self).__name__} {self.name}>"

    @property
    def airfoil_polars (self) -> dict:
        """ dict of airfoil polars, key is the wingSection y position"""
        return self.polar.airfoil_polars


    @property
    def wing (self) -> VLM_Wing:
        return self.polar.vlm_wing


    @property
    def name (self) -> str:
        return f"{self.polar.vtas:.1f}m/s, {self.alpha:.1f}°"

    
    @property
    def Cp_panels (self) -> np.ndarray:
        """ 
        cp of panels of VLM calculation - not limited by cl max of airfoil 

        Core calculation of VLM 

        """
        if self._cp is None: 

            self.aero_results                   # will run viscous loop

        return self._cp


    @property
    def Cp_viscous_panels (self) -> np.ndarray:
        """ cp of panels of VLM calculation - being 0.0 if cl_max of airfoil exceeded"""

        if self._cl_max_reached:
            Cp_panels = np.copy (self.Cp_panels)
            Cl_stripes = self.aero_results [OpPoint_Var.CL] 
            n        = self.wing.n_panels
            nx  	 = self.wing.nx_panels
            ns       = int (n / nx)  

            # set cp of a panel where its stripe reached cl_max to 0.0 
            for i_s in range(ns):
                if Cl_stripes [i_s] == 0.0: 
                    istart = i_s * nx
                    Cp_panels [istart : istart+nx] = 0.0
            return Cp_panels
        else: 
            return self.Cp_panels


    @property
    def aero_results (self) -> dict:
        """ viscous loop: dict with all results along span"""

        if not self._aero_results:

            aero_results_list = self._viscous_loop ()

            self._aero_results  = aero_results_list [-1]

            # VLM_linear is result of first iteration 
            aero_results_linear = aero_results_list [0]
            self._aero_results[OpPoint_Var.CL_VLM_LINEAR] = aero_results_linear[OpPoint_Var.CL_VLM]

        return self._aero_results  


    @property
    def L (self) -> float:
        """ Lift of complete wing"""
        lift_stripes : np.ndarray = self.aero_results[OpPoint_Var.LIFT_STRIPE] 
        return lift_stripes.sum() * 2.0 

    @property
    def CL (self) -> float:
        """ CL of wing"""
        return self.L / (self.polar.q_dyn * self.wing.A_ges * 2.0)

    @property
    def cl_max_reached (self) -> bool:
        """ is cl_max of airfoil reached at a span position"""

        self.aero_results                       # force calculation 
        return self._cl_max_reached

    @property
    def cl_max_almost_reached (self) -> bool:
        """ is cl_max of airfoil almost reached (-5% or so) at a span position"""

        mask = self.aero_results[OpPoint_Var.MAX_MASK] 
        return  np.any(mask)

    @property
    def VLM_error (self) -> bool:
        """ error occured in VLM calculation e.g. bad paneling """

        return self._VLM_error


    def export_to_csv (self, pathFileName, alpha_start = -3.0, alpha_end :float = None, step = 0.5):
        """ 
        Write opPoint data y, chord, alpha ind, alpha eff, Cl        
        """

        import csv

        n        = self.wing.n_panels
        nx  	 = self.wing.nx_panels
        ns       = int (n / nx)                                                 # n stripes 

        with open(pathFileName, 'w', newline='') as csvfile:
            fieldnames = ['y pos', 'Alpha ind', 'Alpha eff', 'Cl']
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames, dialect='excel')

            writer.writer.writerow(["PlanformCreator2"])
            writer.writer.writerow([f"Wing Name :", f"{self.wing._planform.wing.name}"])
            writer.writer.writerow([f"Speed :", f"{self.polar.vtas:.1f}"])
            writer.writer.writerow([f"Alpha :", f"{self.alpha:.1f}"])
            writer.writer.writerow([ ])

            writer.writeheader()

            for i in range (ns):

                y         = self.aero_results[OpPoint_Var.Y] [i]
                Cl        = self.aero_results[OpPoint_Var.CL] [i] 
                alpha_ind = self.aero_results[OpPoint_Var.ALPHA_IND] [i]  
                alpha_eff = self.aero_results[OpPoint_Var.ALPHA_EFF] [i]  

                writer.writerow({'y pos': f"{y:8.3f}", 'Alpha ind': f"{alpha_ind:8.3f}", 
                                 'Alpha eff': f"{alpha_eff:8.3f}", 'Cl': f"{Cl:8.3f}"})





    # ----- private --------------------------------------------------

    def _calc_aero_results (self, Cp: np.ndarray, alpha0: np.ndarray) -> tuple[np.ndarray, dict]:
        """ 
        op point results at y stations of panel stripes derivated from main calculation 
            in viscous loop 

        Args: 
            Cp: Cp of each panel
            alpha0: alpha0 per panel stripe 
        
        Returns: 
            aero_results: dict with values per y_pos - see enum OpPoint_Var
        """

        # ---- known values ------

        n        = self.wing.n_panels
        nx  	 = self.wing.nx_panels
        ns       = int (n / nx)                                                 # n stripes 
        aerogrid = self.wing.aerogrid
        q_dyn    = self.polar.q_dyn

        b_panels    : np.ndarray = aerogrid['b']                                # width 
        l_panels    : np.ndarray = aerogrid['l']                                # depth 

        # ---- lift per panel 

        N = self.wing.N                           # normal vector per panel 
        A = self.wing.A_panels                    # area per panel 

        lift_panels =  (self.polar.q_dyn * N.T * A * Cp)[2]

        Cl_max      = self._get_cl_max ()
        y_stripes   = self.wing.y_stripes

        # ---- unknown values ------

        Cl              = np.zeros (ns) 
        Cl_VLM          = np.zeros (ns) 
        lift            = np.zeros (ns) 
        lift_stripe     = np.zeros (ns) 
        alpha_eff_VLM   = np.zeros (ns) 
        alpha_eff       = np.zeros (ns) 
        alpha_ind       = np.zeros (ns) 
        VLM_error       = np.full  (ns, False) 

        self._cl_max_reached = False

        for i in range (0, self.wing.n_panels, nx):                             # step through panels stripewise 

            Cp_min = np.min (Cp [i:(i+nx)])
            Cp_max = np.max (Cp [i:(i+nx)])
            VLM_error_s = False
            
            i_s    = int (i/nx)
            lift_s = lift_panels [i:(i+nx)].sum() 
            b_s    = b_panels [i]                                           	# width of stripe 
            c_s    = l_panels [i:(i+nx)].sum()                                  # chord of stripe 

            # --- core  --  Anderson p. 409 -------------------

            lift_y   = lift_s / b_s 
            Cl_VLM_y = lift_y / (q_dyn * c_s)

            if Cl_VLM_y > Cl_max[i_s]:                                          # Cl break down if > cl_max of airfoil 
                Cl_y     = 0.0
                lift_y   = 0.0                                                    
                lift_s   = 0.0                                                    
                self._cl_max_reached = True 
            elif (Cp_min * Cp_max < 0) and abs(Cp_max - Cp_min) > 10:           # VLM error e.g. bad paneling 
                Cl_y     = 0.0
                Cl_VLM_y = 0.0 
                lift_y   = 0.0                                                    
                lift_s   = 0.0                                                    
                VLM_error_s = True 
            else: 
                Cl_y     = Cl_VLM_y

            a_eff_y     = degrees (Cl_y     / (2 * np.pi)) + alpha0[i_s]  
            a_eff_VLM_y = degrees (Cl_VLM_y / (2 * np.pi)) + alpha0[i_s]    
            a_ind_y     = self.alpha - a_eff_y

            # store results 

            lift [i_s]          = lift_y     
            lift_stripe [i_s]   = lift [i_s] * b_s 
            Cl [i_s]            = Cl_y
            Cl_VLM [i_s]        = Cl_VLM_y
            alpha_eff [i_s]     = a_eff_y
            alpha_ind [i_s]     = a_ind_y
            alpha_eff_VLM [i_s] = a_eff_VLM_y
            VLM_error [i_s]     = VLM_error_s

            if VLM_error_s:
                self._VLM_error = True


        results = {}
        results[OpPoint_Var.Y]              = y_stripes
        results[OpPoint_Var.CL]             = Cl
        results[OpPoint_Var.CL_VLM]         = Cl_VLM
        results[OpPoint_Var.CL_MAX]         = Cl_max 
        results[OpPoint_Var.MAX_MASK]       = (Cl_max * 0.99 - Cl_VLM) < 0          # numpy mask where Cl_VLM reaches, exceeds CL_MAX              
        results[OpPoint_Var.ERROR_MASK]     = VLM_error                             # numpy mask where VLM error occured             

        results[OpPoint_Var.LIFT]           = lift
        results[OpPoint_Var.LIFT_STRIPE]    = lift_stripe
        results[OpPoint_Var.ALPHA_MAX]      = self._get_alpha_max () 
        results[OpPoint_Var.ALPHA_EFF]      = alpha_eff            
        results[OpPoint_Var.ALPHA_EFF_VLM]  = alpha_eff_VLM            
        results[OpPoint_Var.ALPHA_IND]      = -alpha_ind                            # take now negative   
        results[OpPoint_Var.ALPHA]          = np.full (ns, self.alpha)     
        results[OpPoint_Var.ALPHA0]         = alpha0     

        # self._dump_aero_results (results)

        return results 
    

    def _viscous_loop (self):
        """
        Main VLM calculation as a loop to consider non-linear airfoil dcl/dalpha

        Returns:
            results_lists: aero_results of each iteration - the last one is the actual 
        """

        VISCOUS_EPSILON = 0.01                                      # max. delta of Cl-stripe to achieve per loop

        # first guess with alpha0 from airfoil polar 

        alpha0  = self.polar.alpha0_stripes
        wj      = self._calc_downwash (alpha0)                      # add downwash of flow and alpha0 of airfoil
        cp      = self.wing.Qjj.dot(wj)                             # calc cp per panel 

        results = self._calc_aero_results (cp, alpha0)              # derive all values from cp per panels    

        Cl_VLM_prev  = results[OpPoint_Var.CL_VLM]                  # to compare in Loop 

        results_list = [results]

        if self.polar.use_viscous_loop:

            # iterate until only minor change in Cl per stripe 

            for i in range (5):

                # get current Lift to compare with result 

                alpha_eff : np.ndarray = results[OpPoint_Var.ALPHA_EFF_VLM] 

                # calculate new alpha0 per stripe based on alpha_eff of former calculation 

                alpha0  = self._get_alpha0_from_alpha_eff (alpha_eff_stripes=alpha_eff)
                wj      = self._calc_downwash (alpha0)
                cp      = self.wing.Qjj.dot(wj)

                results = self._calc_aero_results (cp, alpha0)

                results_list.append (results)

                # delta Cl of stripe in viscous loop smaller epsilon?

                Cl_VLM_cur   = results[OpPoint_Var.CL_VLM]

                if np.all (Cl_VLM_prev):
                    Cl_VLM_delta = np.abs((Cl_VLM_prev - Cl_VLM_cur) / Cl_VLM_prev)

                    if np.max(Cl_VLM_delta) < VISCOUS_EPSILON:                     
                        break
                else: 
                    break

                Cl_VLM_prev = np.array (Cl_VLM_cur)

                # error in VLM (quite seldom) 

                if self.VLM_error:
                    break

        self._cp = cp

        return results_list


    def _dump_aero_results (self, results : dict): 
        
        print(f"------- v: {self.polar.vtas:.2f}m/s  alpha: {self.alpha:.1f}° \n")

        lift_stripes : np.ndarray = results[OpPoint_Var.LIFT_STRIPE]
        L =  lift_stripes.sum() * 2.0 

        print (f"   Total Lift: {L:.1f}  Mass: {L / 9.81: .2f} ")

        print (f"{"i":>4s} {"y":>8s} {"cl":>8s} {"a0":>8s} {"a ind":>8s} {"a eff":>8s}")  

        for i in range(self.wing.ny_panels):

            y         = results[OpPoint_Var.Y] [i]
            Cl        = results[OpPoint_Var.CL] [i] 
            alpha0    = results[OpPoint_Var.ALPHA0] [i]  
            alpha_ind = results[OpPoint_Var.ALPHA_IND] [i]  
            alpha_eff = results[OpPoint_Var.ALPHA_EFF] [i]  

            print (f"{i:4d} {y:8.2f} {Cl:8.2f} {alpha0:8.2f} {alpha_ind:8.2f} {alpha_eff:8.2f}")  


    def _get_alpha0_from_alpha_eff (self, alpha_eff_stripes : np.ndarray) -> np.ndarray:
        """ 
        returns alpha0 per stripe based on current alpha_eff at stripe
            - alpha_eff is taken to get cl of airfoil 
            - based on alpha0 = alpha - cl/0.1097 alpha0 is calculated per stripe 

        This is a major part of the vsicous calculation loop  
        """

        y_stripes = self.wing.y_stripes

        sections_alpha0 = []
        sections_y      = []

        # collect alpha0 of airfoils of wingSections from airfoil polar

        airfoil_polar : Polar
        for section_y, airfoil_polar in self.airfoil_polars.items():

            # extrapolate alpha_eff of stripe to get value for root and tip 
            if section_y == [*self.airfoil_polars][0]:                      # extrapolate first section
                z = np.polyfit(y_stripes[:2], alpha_eff_stripes[:2], 1)     # calculate polynomial of line
                f = np.poly1d(z)
                alpha_eff = f(section_y)
            elif section_y == [*self.airfoil_polars][-1]:                   # extrapolate last section
                z = np.polyfit(y_stripes[-2:], alpha_eff_stripes[-2:], 1)
                f = np.poly1d(z)
                alpha_eff = f(section_y)
            else:
                alpha_eff =  np.interp(section_y, y_stripes, alpha_eff_stripes)

            # interpolate cl value in airfoil polar base on alpha_eff
            Cl = np.interp(alpha_eff, airfoil_polar.alpha, airfoil_polar.cl)  # 'normal' alpha0 of polar'

            # calaculate the inviscid equivalence alpha0 based on '2*pi' 
            alpha0 = alpha_eff - Cl / 0.1097

            sections_alpha0.append(alpha0)
            sections_y.append(section_y)

        # alpha0 of stripe by interpolation of section alpha0 

        alpha0_stripes = np.interp (self.wing.y_stripes, sections_y, sections_alpha0)

        return alpha0_stripes


    def _get_cl_max (self) -> np.ndarray:
        """ 
        retrieve cl_max per stripe from airfoil polars 
        """

        # collect cl_max of airfoils of wingSections from airfoil polar

        sections_cl_max = []
        sections_y      = []
        airfoil_polar : Polar

        for section_y, airfoil_polar in self.airfoil_polars.items():
            cl_max = airfoil_polar.max_cl.cl if airfoil_polar.max_cl else 0.0
            sections_cl_max.append(cl_max)
            sections_y.append(section_y)

        # alpha0 of stripe by interpolation of section alpha0 

        cl_max_stripes = np.interp (self.wing.y_stripes, sections_y, sections_cl_max)
        return cl_max_stripes



    def _get_alpha_max (self) -> np.ndarray:
        """ 
        retrieve alpha_max (which is alpha at cl_max) per stripe from airfoil polars 
        """

        # collect cl_max of airfoils of wingSections from airfoil polar

        sections_alpha_max = []
        sections_y      = []
        airfoil_polar : Polar
        
        for section_y, airfoil_polar in self.airfoil_polars.items():
            alpha_max = airfoil_polar.max_cl.alpha if airfoil_polar.max_cl else 0.0
            sections_alpha_max.append(alpha_max)
            sections_y.append(section_y)

        # alpha_max of stripe by interpolation of section alpha_max 

        alpha_max_stripes = np.interp (self.wing.y_stripes, sections_y, sections_alpha_max)
        return alpha_max_stripes


    def _calc_downwash (self, alpha0_stripes : np.ndarray) -> np.ndarray:
        """ 
        calculate initial downwash per panel as the sum of 
            - geometric alpha 
            - alpha0 of the airfoils to represent camber of an airfoil 
        """

        downwash_geo   = self.polar.vtas * radians (self.alpha)   

        wj = np.zeros(self.wing.n_panels)

        for istripe in range (self.wing.ny_panels):

            alpha0_stripe   = - alpha0_stripes[istripe]         # alpha0 is negative 
            downwash_alpha0 = self.polar.vtas * radians (alpha0_stripe)  

            istart = istripe * self.wing.nx_panels
            iend   = istart + self.wing.nx_panels 
            wj [istart:iend] = (downwash_geo + downwash_alpha0) / self.polar.vtas

        return wj


