#!/usr/bin/env python
# -*- coding: utf-8 -*-
""" 

Wing build with VLM_Panels based on Planform_Mesh

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
from __future__                     import annotations  # for forward type references in annotations (WingSection, Planform_Mesh)

import numpy as np
from copy                           import copy
from enum                           import StrEnum
from typing                         import Tuple, NamedTuple, TYPE_CHECKING
from time                           import perf_counter

from .VLM                           import calc_Qjj
from airfoileditor.model.polar_set  import Polar_Set, RE_SCALE_ROUND_TO, polarType, Polar, Polar_Definition

if TYPE_CHECKING:
    from .wing                      import Wing, WingSection                              # avoid circular import
    from .planform_mesh             import Planform_Mesh


VLM_Polar_CacheEntry = tuple[Polar_Definition, 'VLM_Polar']

import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


#-------------------------------------------------------------------------------
# helper classes  and functions 
#-------------------------------------------------------------------------------

class StrEnum_Extended (StrEnum):
    """ enum extension to get a list of all enum values"""
    @classmethod
    def values (cls):
        return [c.value for c in cls]


class VLM_Var (StrEnum_Extended):
    """ polar variables """
    Y               = "y position"               
    CL              = "cl"                                  # viscous loop - Cl > cl_max = 0.0      
    CL_VLM          = "cl VLM"                              # viscous loop  
    CL_VLM_LINEAR   = "cl linear"                           # linear 
    CL_MAX_AIRFOIL  = "cl max airfoil"                      # cl max from airfoil polar
    CL_MIN_AIRFOIL  = "cl min airfoil"                      # cl min from airfoil polar
    LIFT_SPAN       = "Lift per span [N/m]"                 # lift per unit span at stripe

    MAX_MASK        = "cl VLM close MAX"                    # numpy mask where Cl_VLM reaches, exceeds CL_MAX              
    ERROR_MASK      = "VLM error"                           # numpy mask where VLM couldn't calculate Cp    

    ALPHA           = "alpha"                               # angle of attack of panel
    ALPHA_EFF       = "alpha effective (VLM)"               # effective angle of attack (based on VLM))
    ALPHA_IND       = "alpha induced (VLM)"                 # induced angle of attack (based on VLM)
    ALPHA0_AIRFOIL  = "alpha0 airfoil"                      # alpha0 from airfoil polar  

    CD              = "cd"                                  # local drag coefficient 
    CD_IND          = "cd induced (VLM)"                    # local induced drag coefficient (based on VLM)
    CD_AIRFOIL      = "cd airfoil"                          # local airfoil drag coefficient
    CD_SPAN         = "cd per span [1/m]"                   # drag coefficient contribution along span 
    CD_IND_SPAN     = "cd induced per span [1/m]"           # induced drag coefficient contribution along span 
    CD_AIRFOIL_SPAN = "cd airfoil per span [1/m]"           # airfoil drag coefficient contribution along span
    DRAG_IND_SPAN   = "Drag induced per span [N/m]"         # induced drag contribution along span
    DRAG_AIRFOIL_SPAN = "Drag airfoil per span [N/m]"       # airfoil drag contribution along span
    DRAG_SPAN       = "Drag per span [N/m]"                 # total drag contribution along span

    CM_AIRFOIL      = "cm airfoil"                          # local airfoil moment coefficient

    WING_ALPHA      = "Alpha"                               # angle of attack of wing
    WING_LIFT       = "Lift [N]"                            # total lift of wing
    WING_CL         = "CL"                                  # total lift coefficient of wing
    WING_DRAG       = "Drag [N]"                            # total drag of wing
    WING_DRAG_IND   = "Drag induced [N]"                    # total induced drag of wing
    WING_DRAG_AIRFOIL= "Drag airfoil [N]"                   # total airfoil drag of wing
    WING_CD         = "CD"                                  # total drag coefficient of wing
    WING_CD_IND     = "CD induced"                          # total induced drag coefficient of wing
    WING_CD_AIRFOIL = "CD airfoil"                          # total airfoil drag coefficient of wing
    WING_GLIDE      = "CL/CD"                               # total glide ratio of wing
    WING_GLIDE_AIRFOIL= "CL/CD airfoil"                     # total airfoil glide ratio of wing 
    WING_CM_AIRFOIL = "CM airfoil"                          # total airfoil moment coefficient of wing


class Point_3D (NamedTuple):
    """ point having x,y,z coordinates"""

    x : float 
    y : float
    z : float 


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
    wing build with VLM_Panels based on Planform_Mesh

    """

    def __init__ (self, wing: Wing):

        self._wing          = wing
        self._planform_mesh: Planform_Mesh = wing.planform_mesh

        self._panels_right  = None                  # VLM_Panels of the right wing side 
        self._has_distorted_panels = False          # indicate distored (bad) panels 

        self._aerogrid      = None                  # input datastructure of VLM calculation 
        self._Qjj           = None                  # matrix of aerodynamic influence coefficients
        self._BJJ           = None
        self._Gamma         = None                  # circulation
        self._Q_ind         = None 
        self._A_ges         = None                  # total panel area in m²
        self._stripes_y     = None                  # y (middle) of stripes 
        self._stripes_width = None                  # width of stripes 
        self._stripes_chord = None                  # chord of stripes
        self._stripes_area  = None                  # area of stripes

        self._invalidate_cached_results ()

        self._sections      = None                  # cached wing sections of the actual mesh
        self._sections_y    = None                  # y position of wing sections in m
        self._polars: list[VLM_Polar_CacheEntry] = []    # list of (root polar def, VLM polar)

        # geometry data from real Wing - used for VLM calculation (not for plotting)
        wing_area, wing_ar, mac, np = wing.wing_data()
        self._wing_area = wing_area / 1_000_000.0
        self._wing_ar   = wing_ar
        self._mac       = mac
        self._np        = np

        # get mesh, create panels for the right wing side
        self._panels_right, self._has_distorted_panels = self._generate_panels_right ()

        # get actual wing sections of the mesh 
        self._sections = self._planform_mesh.wingSections_reduced()


    def __repr__(self) -> str:
        # overwrite to get a nice print string
        return f"<{type(self).__name__}>"

    def _invalidate_cached_results (self):
        """Clear downstream VLM results when the mesh is rebuilt."""
        self._panels_right  = None
        self._aerogrid      = None
        self._Qjj           = None
        self._BJJ           = None
        self._Gamma         = None
        self._Q_ind         = None
        self._A_ges         = None
        self._stripes_y     = None
        self._stripes_width = None
        self._stripes_chord = None
        self._stripes_area  = None
        self._sections_y    = None
        self._polars        = []

    @property
    def wing_area (self) -> float:
        """Total wing area including fuselage in m²."""
        return self._wing_area

    @property
    def wing_loading (self) -> float:
        """Wing loading in N/m²."""
        if self.wing_area is None or self._wing.mass is None:
            return None
        g = 9.80665  # m/s²
        return self._wing.mass * g / self.wing_area

    @property
    def mac (self) -> float:
        """Mean aerodynamic chord of the wing in m."""
        return self._mac
    

    @property
    def sections (self) -> list [WingSection]:
        """ wing sections of actual mesh"""
        return self._sections


    @property
    def sections_y (self) -> np.ndarray:
        """ y position of wing sections in m"""

        if self._sections_y is None: 
            y = []
            section : WingSection
            for section in self.sections:
                y.append (section.x / 1000)
            self._sections_y = np.array(y)
        return self._sections_y


    @property
    def panels_right (self) -> list [VLM_Panel]:
        """ panels of right half wing"""

        if self._panels_right is None:
            t0 = perf_counter()
            self._panels_right, self._has_distorted_panels = self._generate_panels_right ()
            dt = perf_counter() - t0
            logger.debug (f"{self} panel rebuild: {dt:.4f}s ({len(self._panels_right)} panels)")
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
        return self._planform_mesh.strategy.wx_panels

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
    def panels_area (self) -> np.ndarray:
        """ panel area in m²""" 
        return self.aerogrid['A']

    @property
    def panels_normal (self) ->np.ndarray:
        """ panels normal verctor"""
        return self.aerogrid['N']


    @property
    def Qjj (self) -> np.ndarray:
        """ 
        Matrix of aerodynamic influence coefficients which is indepenend of velocity (only Mach)
            - as self.panels are only right hand side, symmetric left side will be added by VLM
        """

        if self._Qjj is None: 
            # Bjj matrix for induced drag is not needed 
            t0 = perf_counter()
            self._Qjj, _ = calc_Qjj (self.aerogrid, Ma=0.0, xz_symmetry = True)
            dt = perf_counter() - t0
            logger.debug (f"{self} Qjj calculation: {dt:.4f}s ({self._Qjj.shape[0]} x {self._Qjj.shape[1]})")

        return self._Qjj 


    @property
    def stripes_y (self) -> np.ndarray:
        """ y position of panel stripes (middle of panel)"""

        if self._stripes_y is None: 
            y_pos = []
            for i in range (0, self.n_panels, self.nx_panels):
                y_pos.append (self.panels_right[i].offset_k[1])          # middle of panel in y direction
            self._stripes_y = np.array(y_pos)
        return self._stripes_y


    @property
    def stripes_width (self) -> np.ndarray:
        """ width of panel stripes"""

        if self._stripes_width is None: 
            b = []
            for i in range (0, self.n_panels, self.nx_panels):
                b.append (self.panels_right[i].b)
            self._stripes_width = np.array(b)
        return self._stripes_width

    @property
    def stripes_chord (self) -> np.ndarray:
        """ chord of the spanwise stripes, summed over all panels in each stripe"""

        if self._stripes_chord is None:
            chord = []
            for i in range (0, self.n_panels, self.nx_panels):
                strip_panels = self.panels_right[i:i + self.nx_panels]
                chord.append (sum(panel.l for panel in strip_panels))
            self._stripes_chord = np.array(chord)
        return self._stripes_chord

    @property
    def stripes_area (self) -> np.ndarray:
        """ area of the spanwise stripes, summed over all panels in each stripe"""

        if self._stripes_area is None:
            area = []
            for i in range (0, self.n_panels, self.nx_panels):
                strip_panels = self.panels_right[i:i + self.nx_panels]
                area.append (sum(panel.A for panel in strip_panels))
            self._stripes_area = np.array(area)
        return self._stripes_area


    def _get_cached_polar (self, root_polar_def: Polar_Definition) -> 'VLM_Polar | None':
        """ return cached VLM polar for a matching root polar definition """

        for cached_root_polar_def, polar in self._polars:
            if cached_root_polar_def.is_equal_to (root_polar_def):
                return polar
        return None


    def polar_at (self, root_polar_def: Polar_Definition) -> 'VLM_Polar':
        """ returns polar for a root airfoil polar definition """

        if not isinstance (root_polar_def, Polar_Definition):
            return None

        polar = self._get_cached_polar (root_polar_def)          # already exisiting 
        
        if not polar:
            polar_def_copy = copy (root_polar_def)               # polar_def in cache must be inmutable 
            polar = VLM_Polar (self, polar_def_copy)             # calculate new Polar and opPoints 
            self._polars.append ((polar_def_copy, polar)) 
            
        return polar 


    def remove_polar_at (self, root_polar_def: Polar_Definition):
        """ removes polar for a root airfoil polar definition - will be calculated new on next request"""

        if not isinstance (root_polar_def, Polar_Definition):
            return 

        for i, (cached_root_polar_def, _) in enumerate (self._polars):
            if cached_root_polar_def.is_equal_to (root_polar_def):
                self._polars.pop (i)
                break


    def handle_airfoil_change (self):
        """ handle changes in the airfoil - reset all cached polars """

        # collect existung root polar definition in vlm_polars
        root_polar_defs = [root_polar_def for root_polar_def, _ in self._polars]

        self._polars.clear()

        # rebuild new vlm_polars for all existing root polar definitions
        for root_polar_def in root_polar_defs:
            vlm_polar = VLM_Polar (self, root_polar_def)            
            self._polars.append ((root_polar_def, vlm_polar)) 


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

        has_distorted_panels = False                                    # are there distorted panels  

        panels = []
        y_stations, x_stations  = self._planform_mesh.create_mesh()
        y_stations  /= 1000
        x_stations  /= 1000
        nx_stations = x_stations.shape[1]

        for iy in range (len(y_stations) -1): 

            # get y values of one panel stripe 

            y1 = y_stations [iy]
            y2 = y_stations [iy+1]

            x1_arr = x_stations [iy]
            x2_arr = x_stations [iy+1]

            # create n-1 panels of the stripe starting at le towards 

            for ip in range (nx_stations - 1):

                p0 = Point_3D (x1_arr[ip],   y1, 0.0)
                p1 = Point_3D (x1_arr[ip+1], y1, 0.0)
                p2 = Point_3D (x2_arr[ip+1], y2, 0.0)
                p3 = Point_3D (x2_arr[ip],   y2, 0.0)
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
            'l': l,
            'b': b,
            'A': A,
            'N': N,
            'offset_l':  offset_l,
            'offset_k':  offset_k,
            'offset_j':  offset_j,
            'offset_P1': offset_P1,
            'offset_P3': offset_P3,
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


class Airfoil_Polar_Interpolated:
    """Airfoil polar interpolated between two spanwise sections."""

    def __init__ (self, polar_left: Polar, polar_right: Polar,
                  chord_left: float, chord_right: float, chord: float):

        self._polar_left  = polar_left
        self._polar_right = polar_right

        dc = chord_right - chord_left
        is_valid_log = chord_left > 0.0 and chord_right > 0.0 and chord > 0.0 and dc != 0.0

        # linear fraction 
        self._chord_fraction     = (0.0 if dc == 0.0     else (chord - chord_left) / dc)

        # logarithmic fraction - handles the non-linear dependency of Reynolds number and polar
        self._chord_fraction_log = (self._chord_fraction if not is_valid_log else
                                     (np.log (chord) - np.log (chord_left)) / (np.log (chord_right) - np.log (chord_left)))

        # logger.debug (f"at chord={chord:.3f} (left={chord_left:.3f}, right={chord_right:.3f})  " +
        #               f"fraction linear={self._chord_fraction:.4f} log={self._chord_fraction_log:.4f} " +
        #               f"diff={(self._chord_fraction_log - self._chord_fraction) * 100:+.2f}%")

        self._alpha = None
        self._cl    = None
        self._cd    = None
        self._cdp   = None
        self._cm    = None
        self._cp_min = None
        self._xtrt  = None
        self._xtrb  = None

        # sanity - polars must be loaded and have same alpha range
        if not polar_left.isLoaded or not polar_right.isLoaded:
            raise ValueError ("polars must be loaded")

        # interpolate main aerodynamic coefficients
        self._alpha, self._cl, self._cd = self._interpolate_curve ()


    def _interpolate_curve (self) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
        alpha_left  = self._polar_left.alpha
        alpha_right = self._polar_right.alpha

        alpha_min = max (alpha_left[0], alpha_right[0])
        alpha_max = min (alpha_left[-1], alpha_right[-1])
        alpha = np.union1d (alpha_left, alpha_right)
        alpha = alpha[(alpha >= alpha_min) & (alpha <= alpha_max)]

        fraction = self._chord_fraction_log
        cl_left = np.interp (alpha, alpha_left, self._polar_left.cl)
        cl_right = np.interp (alpha, alpha_right, self._polar_right.cl)
        cd_left = np.interp (alpha, alpha_left, self._polar_left.cd)
        cd_right = np.interp (alpha, alpha_right, self._polar_right.cd)

        cl = cl_left + fraction * (cl_right - cl_left)
        cd = cd_left + fraction * (cd_right - cd_left)
        return alpha, cl, cd


    def _interpolate_values (self, name: str) -> np.ndarray:
        fraction = self._chord_fraction_log
        values_left = np.interp (self.alpha, self._polar_left.alpha,
                                 getattr (self._polar_left, name))
        values_right = np.interp (self.alpha, self._polar_right.alpha,
                                  getattr (self._polar_right, name))
        return values_left + fraction * (values_right - values_left)


    @property
    def alpha (self) -> np.ndarray:
        return self._alpha

    @property
    def cl (self) -> np.ndarray:
        return self._cl

    @property
    def cd (self) -> np.ndarray:
        return self._cd

    @property
    def cdp (self) -> np.ndarray:
        if self._cdp is None:
            self._cdp = self._interpolate_values ('cdp')
        return self._cdp

    @property
    def cm (self) -> np.ndarray:
        if self._cm is None:
            self._cm = self._interpolate_values ('cm')
        return self._cm

    @property
    def cp_min (self) -> np.ndarray:
        if self._cp_min is None:
            self._cp_min = self._interpolate_values ('cp_min')
        return self._cp_min

    @property
    def xtrt (self) -> np.ndarray:
        if self._xtrt is None:
            self._xtrt = self._interpolate_values ('xtrt')
        return self._xtrt

    @property
    def xtrb (self) -> np.ndarray:
        if self._xtrb is None:
            self._xtrb = self._interpolate_values ('xtrb')
        return self._xtrb

    @property
    def alpha0 (self) -> float | None:
        if len (self.cl) == 0 or np.min (self.cl) > 0.0 or np.max (self.cl) < 0.0:
            return None
        return float (np.interp (0.0, self.cl, self.alpha))

    @property
    def cl_max (self) -> float | None:
        return float (np.max (self.cl)) if len (self.cl) else None

    @property
    def cl_min (self) -> float | None:
        return float (np.min (self.cl)) if len (self.cl) else None

    @property
    def alpha_max (self) -> float | None:
        if not len (self.cl):
            return None
        return float (self.alpha [np.argmax (self.cl)])

    def cd_at_cl (self, cl: float) -> float | None:
        if not len (self.cl) or cl < np.min (self.cl) or cl > np.max (self.cl):
            return None
        return float (np.interp (cl, self.cl, self.cd))

    def cm_at_cl (self, cl: float) -> float | None:
        if not len (self.cl) or cl < np.min (self.cl) or cl > np.max (self.cl):
            return None
        return float (np.interp (cl, self.cl, self.cm))


# ----------------------------------------------------------


class VLM_Polar:

    """ 
    VLM solution for certain velocity 
    """

    def __init__ (self, wing: VLM_Wing, root_polar_def: Polar_Definition):

        self.vlm_wing        = wing                     # my parent wing

        # sanity - only T1 polars allowed for VLM calculation
        if not isinstance (root_polar_def, Polar_Definition) or not root_polar_def.type == polarType.T1:
            raise ValueError ("root polar definition must be a T1 polar definition")

        self._root_polar_def = root_polar_def           # polar definition of root airfoil (wingSection 0) 

        self._vtas                      = None          # true air speed (derived from root polar def)
        self._opPoints                  = {}            # dict of operating points
        self._error_reason              = []            # list of error messages eg polar couldn't be loaded 
        self._use_viscous_loop          = True          # in opPoint calculation
        self._is_polar_generated        = False         # polar is completly generated and ready for use

        self._airfoil_polar_sections            = None          # polar of airfoil per section
        self._airfoil_polar_sections_normal     = None          # normal polar of airfoil per section
        self._airfoil_polar_stripes             = None
        self._airfoil_polar_stripes_normal      = None

        self._alpha0_stripes            = None          # alpha0 per stripe from interpolated stripe polar
        self._cl_min_stripes            = None          # cl min per stripe from interpolated stripe polar
        self._cl_max_stripes            = None          # cl max per stripe from interpolated stripe polar

        logger.debug (f"{self} created")


    def __repr__(self) -> str:
        # overwrite to get a nice print string 
        return f"<{type(self).__name__} {self.vtas:.1f}m/s >"


    @property
    def vtas (self) -> float:
        """ true air speed of self"""

        if isinstance (self._root_polar_def, Polar_Definition) and self._vtas is None:

            # calc v from airfoil polar and chord of root section
            root_chord = self.vlm_wing.sections[0].c
            self._vtas = self._root_polar_def.calc_v_for_chord(root_chord)

        return self._vtas


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
    def airfoil_polar_sections (self) -> list [Polar]:
        """Forced-transition VLM polar of airfoil per section."""
        if self._airfoil_polar_sections is None:
            self._airfoil_polar_sections = self._get_airfoil_polar_sections (is_vlm=True)
        return self._airfoil_polar_sections

    @property
    def airfoil_polar_sections_normal (self) -> list [Polar]:
        """Normal, non-forced-transition polar of airfoil per section."""
        if self._airfoil_polar_sections_normal is None:
            self._airfoil_polar_sections_normal = self._get_airfoil_polar_sections (is_vlm=False)
        return self._airfoil_polar_sections_normal

    @property
    def airfoil_polar_stripes (self) -> list [Airfoil_Polar_Interpolated]:
        """VLM airfoil polars interpolated to the panel stripes."""
        if not self._airfoil_polar_stripes:
            self._airfoil_polar_stripes = self._build_airfoil_polar_stripes (is_vlm=True)
        return self._airfoil_polar_stripes

    @property
    def airfoil_polar_stripes_normal (self) -> list [Airfoil_Polar_Interpolated]:
        """Normal airfoil polars interpolated to the panel stripes."""
        if not self._airfoil_polar_stripes_normal:
            self._airfoil_polar_stripes_normal = self._build_airfoil_polar_stripes (is_vlm=False)
        return self._airfoil_polar_stripes_normal

    @property
    def error_reason (self) -> list[str]:
        """List of reasons why self is not ready / section polars couldn't be loaded."""
        return self._error_reason


    @property
    def is_ready (self) -> bool:
        """True if airfoil section polars are completely loaded for opPoint calculation."""

        if not self.airfoil_polar_sections or not self.airfoil_polar_sections_normal:
            return False

        nsections = len(self.vlm_wing.sections)
        if  len(self.airfoil_polar_sections) != nsections:
            return False
        if  len(self.airfoil_polar_sections_normal) != nsections:
            return False

        return True


    @property 
    def use_viscous_loop (self) -> bool:
        """ use viscous loop - non-linear aero calculation"""
        return self._use_viscous_loop


    def _build_airfoil_polar_stripes (self, is_vlm: bool) -> list[Airfoil_Polar_Interpolated]:
        """Build one interpolated airfoil polar for each panel stripe."""
        section_polars = self.airfoil_polar_sections if is_vlm else self.airfoil_polar_sections_normal
        if not section_polars or len (section_polars) != len (self.vlm_wing.sections):
            return []

        sections = self.vlm_wing.sections
        sections_y = self.vlm_wing.sections_y
        stripes_y = self.vlm_wing.stripes_y
        stripes_chord = self.vlm_wing.stripes_chord
        section_chords = np.array ([section.c / 1000 for section in sections])

        interpolated = []
        i_left, i_right = 0, 1
        for y, chord in zip (stripes_y, stripes_chord):
            while i_right < len (sections_y) - 1 and y > sections_y[i_right]:
                i_left += 1
                i_right += 1

            interpolated.append (Airfoil_Polar_Interpolated (
                section_polars[i_left], section_polars[i_right],
                section_chords[i_left], section_chords[i_right], chord))

        return interpolated


    def set_use_viscous_loop (self, aBool : bool): 
        self._use_viscous_loop = aBool
        self._opPoints = {}                             # remove current results 


    def opPoint_at (self, alpha: float) -> 'VLM_OpPoint':
        """ returns opPoint at alpha - or None if airfoil polars are not ready"""

        if self.is_ready:
            a = round (alpha, 1)                        # ensure clean key for dict
            try:
                opPoint = self._opPoints[a]             # already exisiting
            except:
                opPoint = VLM_OpPoint (self, a)         # calculate new opPOint 

                if not opPoint.cl_max_reached:          # add to polar only if valid
                    self._opPoints[a] = opPoint 
            return opPoint 
        else: 
            return None 

    def opPoint_at_alpha_max (self) -> 'VLM_OpPoint':
        """ returns opPoint at alpha max - or None if airfoil polars are not ready"""

        if self.is_ready:
            return self._find_alpha_max ()
        else: 
            # e.g. airfoil polars not loaded up to now
            return None
        

    @property
    def q_dyn (self) -> float:
        """Dynamic pressure in N/m²."""
        return 1.225 / 2.0 * self.vtas ** 2

    @property
    def cl_level_flight (self) -> float | None:
        """Lift coefficient required for steady level flight L=W condition"""
        if self.vlm_wing.wing_loading is None or self.q_dyn == 0.0:
            return None
        return self.vlm_wing.wing_loading / self.q_dyn

    def cl_level_flight_vars (self, xyVars: Tuple[VLM_Var, VLM_Var]) -> Tuple[float | None, float | None]:
        """ 
        Return the interpolated polar var values for the current level flight cl for the given xyVars.
        e.g. for CL,CD return 0.1, 0.05.

        """
        cl_level = self.cl_level_flight
        if cl_level is None:
            return None, None

        cl = self._ofVar (VLM_Var.WING_CL)
        x_var, y_var = xyVars
        x_values, y_values = self.ofVars ((x_var, y_var))
        x_level = float (np.interp (cl_level, cl, x_values))
        y_level = float (np.interp (cl_level, cl, y_values))

        return x_level, y_level


    @property
    def alpha0_stripes (self) -> np.ndarray:
        """Alpha-zero values from the interpolated stripe polars."""

        if self._alpha0_stripes is None:
            self._alpha0_stripes = np.array ([polar.alpha0 for polar in self.airfoil_polar_stripes])

        return self._alpha0_stripes


    @property
    def cl_max_stripes (self) -> np.ndarray:
        """Maximum lift coefficients from the interpolated stripe polars."""

        if self._cl_max_stripes is None:
            self._cl_max_stripes = np.array ([polar.cl_max for polar in self.airfoil_polar_stripes])

        return self._cl_max_stripes

    @property
    def cl_min_stripes (self) -> np.ndarray:
        """Minimum lift coefficients from the interpolated stripe polars."""

        if self._cl_min_stripes is None:
            self._cl_min_stripes = np.array ([polar.cl_min for polar in self.airfoil_polar_stripes])

        return self._cl_min_stripes


    def cd_at_stripes (self, stripes_cl: np.ndarray) -> np.ndarray:
        """Return drag from the interpolated normal stripe polars."""
        cd_values = [polar.cd_at_cl (cl)
                     for polar, cl in zip (self.airfoil_polar_stripes_normal, stripes_cl)]
        return np.array ([cd if cd is not None else np.nan for cd in cd_values])


    def cm_at_stripes (self, stripes_cl: np.ndarray) -> np.ndarray:
        """Return moment coefficients from the interpolated normal stripe polars."""
        cm_values = [polar.cm_at_cl (cl)
                     for polar, cl in zip (self.airfoil_polar_stripes_normal, stripes_cl)]
        return np.array ([cm if cm is not None else np.nan for cm in cm_values])

    # ---- private ----


    def _get_matching_polar (self, airfoil_polarSet: Polar_Set, section_re: float, is_vlm: bool) -> Polar | None:
        """ return the matching normal or forced-transition polar for a section Re """

        polar_def_target = copy (self._root_polar_def)
        polar_def_target.set_re (section_re)

        matching_polar = None
        best_re_delta  = float("inf")

        polar_list = airfoil_polarSet.polars_VLM if is_vlm else airfoil_polarSet.polars_normal
        for polar in polar_list:
            if polar.is_equal_to (polar_def_target, ignore_active=True, ignore_xtrip=True, re_abs_tolerance=RE_SCALE_ROUND_TO):
                re_delta = abs (polar.re - section_re)
                if re_delta < best_re_delta:
                    matching_polar = polar
                    best_re_delta = re_delta

        return matching_polar


    def _get_airfoil_polar_sections (self, is_vlm: bool) -> list [Polar] | None:
        """
        Load the selected polar type for all wing sections.
        Returns None if any polar is still generating or if an error occurred.
        """

        self._error_reason  = []
        ngenerating = 0
        t0 = perf_counter()

        # get airfoil polars for all wing sections
        airfoil_polar_sections = []

        for section in self.vlm_wing.sections:

            if not section.airfoil.isLoaded: 
                msg = f"{self} section {section} airfoil {section.airfoil} not loaded"
                logger.debug (msg)
                self._error_reason.append (msg)
                break

            airfoil_polarSet : Polar_Set = section.airfoil.polarSet

            if airfoil_polarSet is None: 
                msg = f"{self} section {section} airfoil {section.airfoil} has no polarSet"
                logger.error (msg)
                self._error_reason.append (msg)
                break

            if is_vlm:
                airfoil_polarSet.ensure_polars_VLM()
                airfoil_polarSet.load_or_generate_polars(normal=False, VLM=True)
            else:
                airfoil_polarSet.load_or_generate_polars(normal=True, VLM=False)

            # find polar with matching Re of this wing section

            section_re = self._root_polar_def.re * section.cn
            matching_polar = self._get_matching_polar (airfoil_polarSet, section_re, is_vlm)

            if matching_polar is not None:
                if matching_polar.isLoaded:
                    # there is a polar that fits to Re of wingSection
                    airfoil_polar_sections.append(matching_polar)
                else:
                    if matching_polar.error_occurred:
                        self._error_reason.append (matching_polar.error_reason)
                    else:
                        ngenerating += 1
            else:
                polar_type = "VLM" if is_vlm else "normal"
                msg = (f"No {polar_type} polar matching root definition within ±{RE_SCALE_ROUND_TO:.0f} Re "
                       f"for section {section.id} (target Re={section_re:.0f}) " )
                logger.error (msg)
                self._error_reason.append (msg)

        # if polar couldn't be loaded or error occured, reset section polar data

        if self._error_reason:
            logger.warning (f"{self} couldn't load section polars - resetting section polar data")
        elif ngenerating == 0:
            dt = perf_counter() - t0
            logger.debug (f"{self} loaded {len(airfoil_polar_sections)} section polars (is_vlm={is_vlm}) in {dt:.4f}s")
            return airfoil_polar_sections
        else:
            logger.debug (f"{self} {ngenerating} section polars are still generating (is_vlm={is_vlm})")
        return None


    def _find_alpha_max (self) -> 'VLM_OpPoint':
        """ find opPoint which is close before cl_max reached at a span position"""

        alpha = None
        alpha_start = 0.0
        alpha_end   = 30.0

        for step in (5.0, 1.0, 0.2):
            alpha = None
            for candidate in np.arange (alpha_start, alpha_end + step / 2, step):
                if self.opPoint_at (candidate).cl_max_almost_reached:
                    alpha = candidate
                    break

            if alpha is None:
                break

            alpha_start = alpha - step
            alpha_end = alpha

        if alpha is None:
            logger.warning (f"{self} alpha_max not found ({len(self._opPoints)} opPoints")
            return None 
        else: 
            logger.debug (f"{self} found alpha_max {self.opPoint_at (alpha)}")
            return self.opPoint_at (alpha)


    def ofVars (self, xyVars: Tuple[VLM_Var, VLM_Var]) -> Tuple[np.ndarray, np.ndarray]:
        """ returns x,y polar of the tuple xyVars"""
    
        if isinstance(xyVars, tuple):
            x, y = self._ofVar (xyVars[0]), self._ofVar (xyVars[1])
        else:
            x, y = np.array([]), np.array([])
        return x,y 


    def _ofVar (self, polar_var: VLM_Var) -> np.ndarray:
        """ return cached values for a polar variable """

        # collect values of existing opPoints for the requested polar_var
        vals = []
        opPoint : VLM_OpPoint
        for opPoint in self._opPoints.values():
            vals.append(opPoint.aero_results.get(polar_var, np.nan))

        return np.array(vals)


    def generate_polar (self, alpha_start = -3.0, alpha_max :float = 25.0, step = 0.5):
        """ 
        Generate polar data alpha, Cl, Lift 
            - if alpha_end is omitted, the polar ends when CL_Max is almost reached       
        """

        # already done
        if self._is_polar_generated:
            return

        # wait until airfoil polars are completly loaded
        if not self.is_ready:
            return

        t0 = perf_counter()
        for alpha in np.arange (alpha_start, alpha_max, step):
 
            opPoint = self.opPoint_at (alpha)

            if opPoint.cl_max_reached:
                self._opPoints.pop (opPoint.alpha, None)
                break
            elif opPoint.cl_max_almost_reached:     
                break 

        # re-sort as there could have been already opPoints
        self._opPoints = dict(sorted(self._opPoints.items()))

        self._is_polar_generated = True

        dt = perf_counter() - t0
        logger.debug (f"{self} polar generation: {dt:.4f}s ({len(self._opPoints)} opPoints, alpha={alpha_start:.1f}..{alpha_max:.1f}, step={step})")

        self._test_print()


    def _test_print (self):
        """ print polar data alpha, Cl, Lift """

        print (f"{self} polar data")
        print (f"  {len(self._opPoints)} opPoints")
        print (f"  {'Alpha':>8s} {'CL':>8s} {'Lift':>8s} {'CD':>8s} {'CD_i':>8s} {'Drag':>8s} {'Glide':>8s}")

        alpha = self._ofVar (VLM_Var.WING_ALPHA)
        cl    = self._ofVar (VLM_Var.WING_CL)
        lift  = self._ofVar (VLM_Var.WING_LIFT)
        cd    = self._ofVar (VLM_Var.WING_CD)
        cd_i  = self._ofVar (VLM_Var.WING_CD_IND)
        drag  = self._ofVar (VLM_Var.WING_DRAG)
        glide = self._ofVar (VLM_Var.WING_GLIDE)
        for i in range(len(alpha)):
            print (f"  {alpha[i]:8.1f} {cl[i]:8.3f} {lift[i]:8.1f} {cd[i]:8.3f} {cd_i[i]:8.3f} {drag[i]:8.2f} {glide[i]:8.2f}")

        print ("Level flight conditions:")
        cl = self.cl_level_flight
        if cl is None:
            print ("  Level flight not possible (wing loading or dynamic pressure is zero)")
            return
        alpha, cd = self.cl_level_flight_vars ((VLM_Var.WING_ALPHA, VLM_Var.WING_CD))
        glide, _ = self.cl_level_flight_vars ((VLM_Var.WING_GLIDE, VLM_Var.WING_GLIDE))
        lift, drag = self.cl_level_flight_vars ((VLM_Var.WING_LIFT, VLM_Var.WING_DRAG))
        print (f"  CL: {cl:.3f}  Alpha: {alpha:.1f}°  CD: {cd:.3f}  Glide: {glide:.2f}  Lift: {lift:.1f}N  Drag: {drag:.1f}N")
 

    # def export_to_csv (self, pathFileName, alpha_start = -3.0, alpha_end :float = None, step = 0.5):
    #     """ 
    #     Write polar data alpha, Cl, Lift to csv 
    #         - if alpha_end is omitted, the polar ends when Cl is Cl_max / 2        
    #     """

    #     import csv

    #     alpha_max = 25.0                                # maximum for polar 

    #     with open(pathFileName, 'w', newline='') as csvfile:
    #         fieldnames = ['Alpha', 'CL', 'Lift']
    #         writer = csv.DictWriter(csvfile, fieldnames=fieldnames, dialect='excel')

    #         writer.writerow(["PlanformCreator2"])
    #         writer.writerow([f"Wing Name :", f"{self.vlm_wing._planform_mesh.wing.name}"])
    #         writer.writerow([f"Speed :", f"{self.vtas:.1f}"])
    #         writer.writerow([ ])

    #         writer.writeheader()

    #         CL_max = -5.0                               # dummy initial value 

    #         for alpha in np.arange (alpha_start, alpha_max, step):
 
    #             opPoint = self.opPoint_at (alpha)
    #             writer.writerow({'Alpha': f"{opPoint.alpha:8.1f}", 'CL': f"{opPoint.CL:8.3f}", 'Lift': f"{opPoint.Lift:8.2f}"})

    #             if opPoint.CL < CL_max and opPoint.CL < CL_max / 2:     # handle also negative values
    #                 break 
    #             if opPoint.CL > CL_max:
    #                 CL_max = opPoint.CL



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
        self._has_vlm_error  = False                # error occured e.g because of bad paneling 

        # if there are no airfoil polars - break 

        if not self.polar.airfoil_polar_sections:
            raise ValueError ("Airfoil polars missing")


    def __repr__(self) -> str:
        # overwrite to get a nice print string 
        return f"<{type(self).__name__} {self.name}>"


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
            Cl_stripes = self.aero_results [VLM_Var.CL] 
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
            self._aero_results[VLM_Var.CL_VLM_LINEAR] = aero_results_linear[VLM_Var.CL_VLM]

            # add drag results to aero_results after viscous loop is finished
            drag_results = self._calc_aero_drag ()
            self._aero_results.update (drag_results)

            # add moment results to aero_results after viscous loop is finished
            moment_results = self._calc_aero_moment ()
            self._aero_results.update (moment_results)

        return self._aero_results  


    @property
    def cl_max_reached (self) -> bool:
        """ is cl_max of airfoil reached at a span position"""

        self.aero_results                       # force calculation 
        return self._cl_max_reached

    @property
    def cl_max_almost_reached (self) -> bool:
        """ is cl_max of airfoil almost reached (-5% or so) at a span position"""

        mask = self.aero_results[VLM_Var.MAX_MASK] 
        return  np.any(mask)

    @property
    def has_vlm_error (self) -> bool:
        """ error occured in VLM calculation e.g. bad paneling """

        return self._has_vlm_error


    # def export_to_csv (self, pathFileName, alpha_start = -3.0, alpha_end :float = None, step = 0.5):
    #     """ 
    #     Write opPoint data y, chord, alpha ind, alpha eff, Cl        
    #     """

    #     import csv

    #     n        = self.wing.n_panels
    #     nx  	 = self.wing.nx_panels
    #     ns       = int (n / nx)                                                 # n stripes 

    #     with open(pathFileName, 'w', newline='') as csvfile:
    #         fieldnames = ['y pos', 'Alpha ind', 'Alpha eff', 'Cl']
    #         meta_writer = csv.writer(csvfile, dialect='excel')
    #         writer = csv.DictWriter(csvfile, fieldnames=fieldnames, dialect='excel')

    #         meta_writer.writerow(["PlanformCreator2"])
    #         meta_writer.writerow([f"Wing Name :", f"{self.wing._planform_mesh.wing.name}"])
    #         meta_writer.writerow([f"Speed :", f"{self.polar.vtas:.1f}"])
    #         meta_writer.writerow([f"Alpha :", f"{self.alpha:.1f}"])
    #         meta_writer.writerow([ ])

    #         writer.writeheader()

    #         for i in range (ns):

    #             y         = self.aero_results[OpPoint_Var.Y] [i]
    #             cl        = self.aero_results[OpPoint_Var.CL] [i] 
    #             alpha_ind = self.aero_results[OpPoint_Var.ALPHA_IND] [i]  
    #             alpha_eff = self.aero_results[OpPoint_Var.ALPHA_EFF] [i]  

    #             writer.writerow({'y pos': f"{y:8.3f}", 'Alpha ind': f"{alpha_ind:8.3f}", 
    #                              'Alpha eff': f"{alpha_eff:8.3f}", 'Cl': f"{cl:8.3f}"})





    # ----- private --------------------------------------------------

    def _calc_aero_lift (self, wj: np.ndarray, Cp: np.ndarray, alpha0: np.ndarray) -> dict:
        """ 
        op point results at y stations of panel stripes derivated from main calculation 
            in viscous loop 

        Args: 
            wj: downwash at each panel
            Cp: Cp of each panel
            alpha0: alpha0 per panel stripe - current value of viscous loop 
        
        Returns: 
            aero_results: dict with values per stripe - see enum VLM_Var
        """

        n        = self.wing.n_panels
        nx  	 = self.wing.nx_panels
        ns       = int (n / nx)                                         # n stripes 

        # ---- lift per panel 

        N = self.wing.panels_normal                                     # normal vector per panel 
        A = self.wing.panels_area                                       # area per panel 

        q_dyn       = self.polar.q_dyn
        lift_panels =  q_dyn * N[:, 2] * A * Cp

        cl_min      = self.polar.cl_min_stripes                         # airfoil cl_min at each stripe
        cl_max      = self.polar.cl_max_stripes                         # airfoil cl_max at each stripe

        # stripe geometry and properties

        y           = self.wing.stripes_y                               # y position of each stripe
        dy          = self.wing.stripes_width                           # width of each stripe
        chord       = self.wing.stripes_chord                           # chord of each stripe

        # stripe lift and cl

        lift_panels_block = lift_panels.reshape (ns, nx)
        lift              = lift_panels_block.sum (axis=1)              # lift of stripe
        lift_per_span     = lift / dy                                   # lift per unit span

        cl_vlm            = lift_per_span / (q_dyn * chord)             # cl at span

        # build mask for > cl_max and vlm error

        Cp_block          = Cp.reshape (ns, nx)
        Cp_min            = Cp_block.min (axis=1)
        Cp_max            = Cp_block.max (axis=1)

        cl_min_mask       = cl_vlm < cl_min
        cl_max_mask       = cl_vlm > cl_max
        cl_limit_mask     = cl_min_mask | cl_max_mask
        VLM_error         = ((Cp_min * Cp_max < 0) & (np.abs(Cp_max - Cp_min) > 10))

        # mask cl to 0.0 if cl_max reached or VLM error occured

        lift_per_span     = np.where (cl_limit_mask | VLM_error, np.nan, lift_per_span)
        cl                = np.where (cl_limit_mask | VLM_error, np.nan, cl_vlm)
        cl_vlm_masked     = np.where (VLM_error, 0.0, cl_vlm)

        self._cl_max_reached = bool (cl_limit_mask.any())                # flag this opPoint
        self._has_vlm_error   = bool (VLM_error.any())

        # alpha_eff and alpha_ind based on cl_vlm and alpha0 of airfoil polar

        alpha0            = self.polar.alpha0_stripes
        alpha_eff_VLM     = np.degrees (cl_vlm_masked / (2 * np.pi)) + alpha0
        alpha_ind_VLM     = self.alpha - alpha_eff_VLM

        # total lift and CL of wing

        if self._cl_max_reached or self._has_vlm_error:
            Lift = np.nan
            CL   = np.nan
        else:
            Lift = np.sum (lift_per_span * dy) * 2.0                        # total lift of wing - multiply by 2 for both sides
            CL   = Lift / (q_dyn * self.wing.wing_area)                     # take real wing area for CL calculation

        # -- 

        results = {}
        results[VLM_Var.Y]              = y
        results[VLM_Var.CL]             = cl
        results[VLM_Var.CL_VLM]         = cl_vlm
        results[VLM_Var.CL_MAX_AIRFOIL] = cl_max 
        results[VLM_Var.CL_MIN_AIRFOIL] = cl_min
        results[VLM_Var.MAX_MASK]       = ((cl_max * 0.99 - cl_vlm) < 0) |  (cl_vlm < cl_min * 0.99)      
        results[VLM_Var.ERROR_MASK]     = VLM_error                 # mask where VLM error occured             

        results[VLM_Var.LIFT_SPAN]      = lift_per_span
        results[VLM_Var.ALPHA0_AIRFOIL] = alpha0                            # airfoil alpha0 from polar
        results[VLM_Var.ALPHA_EFF]      = alpha_eff_VLM            
        results[VLM_Var.ALPHA_IND]      = alpha_ind_VLM                      
        results[VLM_Var.ALPHA]          = np.full (ns, self.alpha)     

        results[VLM_Var.WING_ALPHA]     = self.alpha
        results[VLM_Var.WING_LIFT]      = Lift
        results[VLM_Var.WING_CL]        = CL

        logger.debug (f"{self} aero results: Lift {Lift:.2f} N, CL {CL:.3f}, cl_max_reached {self._cl_max_reached}, VLM_error {self._has_vlm_error}")
        return results 


    def _calc_aero_drag (self) -> dict:
        """ calculate drag results from the final core VLM results """

        wing_area     = self.wing.wing_area
        half_wing_area= wing_area / 2.0
        chord         = self.wing.stripes_chord
        y             = self.wing.stripes_y
        dy            = self.wing.stripes_width

        # from aero results of viscous loop per stripe

        cl            = self.aero_results [VLM_Var.CL]
        cl_vlm        = self.aero_results [VLM_Var.CL_VLM]
        alpha_ind     = self.aero_results [VLM_Var.ALPHA_IND]
        alpha_ind_rad = np.deg2rad (alpha_ind)
        CL            = self.aero_results [VLM_Var.WING_CL]

        # drag coefficients per stripe

        cd_ind        = alpha_ind_rad * cl_vlm                      # use 3D VLM
        cd_airfoil    = self.polar.cd_at_stripes (cl)               # use 2D airfoil
        cd            = cd_airfoil + cd_ind

        # normalized drag coefficients contribution per span unit [1/m]

        cd_ind_span        = cd_ind * chord  / half_wing_area
        cd_airfoil_span    = cd_airfoil * chord  / half_wing_area
        cd_span            = cd * chord  / half_wing_area

        # total drag coefficient of wing

        CD_ind         = np.sum(cd_ind_span * dy)
        CD_airfoil     = np.sum(cd_airfoil_span * dy)
        CD             = np.sum(cd_span * dy)

        logger.debug (f"{self} drag coefficients: CD_ind {CD_ind:.4f}, CD_airfoil {CD_airfoil:.4f}, CD {CD:.4f}")

        # drag contribution per span unit [N/m]

        drag_ind_span      = cd_ind     * self.polar.q_dyn * chord
        drag_airfoil_span  = cd_airfoil * self.polar.q_dyn * chord
        drag_span          = drag_airfoil_span + drag_ind_span

        # total drag of wing

        if not np.isnan (cd_airfoil).any ():
            Drag_ind         = CD_ind     * self.polar.q_dyn * wing_area
            Drag_airfoil     = CD_airfoil * self.polar.q_dyn * wing_area
            Drag             = CD         * self.polar.q_dyn * wing_area
        else:
            Drag_ind         = np.nan
            Drag_airfoil     = np.nan
            Drag             = np.nan

        return {
            VLM_Var.Y:                 y,
            VLM_Var.CD:                cd,
            VLM_Var.CD_IND:            cd_ind,
            VLM_Var.CD_AIRFOIL:        cd_airfoil,
            VLM_Var.CD_IND_SPAN:       cd_ind_span,
            VLM_Var.CD_AIRFOIL_SPAN:   cd_airfoil_span,
            VLM_Var.CD_SPAN:           cd_span,
            VLM_Var.DRAG_IND_SPAN:     drag_ind_span,
            VLM_Var.DRAG_AIRFOIL_SPAN: drag_airfoil_span,
            VLM_Var.DRAG_SPAN:         drag_span,

            VLM_Var.WING_CD_IND:       CD_ind,
            VLM_Var.WING_CD_AIRFOIL:   CD_airfoil,
            VLM_Var.WING_CD:           CD,
            VLM_Var.WING_DRAG_IND:     Drag_ind,
            VLM_Var.WING_DRAG_AIRFOIL: Drag_airfoil,
            VLM_Var.WING_DRAG:         Drag,
            VLM_Var.WING_GLIDE:        CL/CD if CD != 0 else np.nan,
            VLM_Var.WING_GLIDE_AIRFOIL:CL/CD_airfoil if CD_airfoil != 0 else np.nan
        }



    def _calc_aero_moment (self) -> dict:
        """ calculate moment results from the final core VLM results """

        wing_area     = self.wing.wing_area
        half_wing_area= wing_area / 2.0
        chord         = self.wing.stripes_chord
        mac           = self.wing.mac
        y             = self.wing.stripes_y
        dy            = self.wing.stripes_width

        # from aero results of viscous loop per stripe

        cl            = self.aero_results [VLM_Var.CL]

        # moment coefficients per stripe

        cm_airfoil    = self.polar.cm_at_stripes (cl)               # use 2D airfoil
        dcm_airfoil   = (cm_airfoil * chord**2 * dy) / (mac * half_wing_area)

        # total moment coefficient of wing

        CM_airfoil     = np.sum(dcm_airfoil)

        logger.debug (f"{self} CM_airfoil {CM_airfoil:.4f}")

        # total moment of wing

        # if not np.isnan (cd_airfoil).any ():
        #     Drag_ind         = CD_ind     * self.polar.q_dyn * wing_area
        #     Drag_airfoil     = CD_airfoil * self.polar.q_dyn * wing_area
        #     Drag             = CD         * self.polar.q_dyn * wing_area
        # else:
        #     Drag_ind         = np.nan
        #     Drag_airfoil     = np.nan
        #     Drag             = np.nan

        return {
            VLM_Var.Y:                 y,
            VLM_Var.CM_AIRFOIL:        cm_airfoil,
            VLM_Var.WING_CM_AIRFOIL:   CM_airfoil,
        }





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

        results = self._calc_aero_lift (wj, cp, alpha0)             # derive all values from cp per panels    

        cl_vlm_prev  = results[VLM_Var.CL_VLM]                      # stripe-wise VLM cl to compare in loop

        results_list = [results]

        if self.polar.use_viscous_loop:

            # iterate until only minor change in Cl per stripe 

            for i in range (5):

                # get current Lift to compare with result 

                alpha_eff : np.ndarray = results[VLM_Var.ALPHA_EFF] 

                # calculate new alpha0 per stripe based on alpha_eff of former calculation 

                alpha0  = self._get_alpha0_from_alpha_eff (alpha_eff_stripes=alpha_eff)
                wj      = self._calc_downwash (alpha0)
                cp      = self.wing.Qjj.dot(wj)

                results = self._calc_aero_lift (wj, cp, alpha0)

                results_list.append (results)

                # delta Cl of stripe in viscous loop smaller epsilon?

                cl_vlm_cur   = results[VLM_Var.CL_VLM]

                if np.all (cl_vlm_prev):
                    cl_vlm_delta = np.abs((cl_vlm_prev - cl_vlm_cur) / cl_vlm_prev)

                    if np.max(cl_vlm_delta) < VISCOUS_EPSILON:                     
                        break
                else: 
                    break

                cl_vlm_prev = np.array (cl_vlm_cur)

                # error in VLM (quite seldom) 

                if self.has_vlm_error:
                    break

        self._cp = cp

        return results_list


    def _get_alpha0_from_alpha_eff (self, alpha_eff_stripes : np.ndarray) -> np.ndarray:
        """ 
        returns alpha0 per stripe based on current alpha_eff at stripe
            - alpha_eff is taken to get cl of airfoil 
            - based on alpha0 = alpha - cl/0.1097 alpha0 is calculated per stripe 

        This is a major part of the vsicous calculation loop  
        """

        y_stripes = self.wing.stripes_y

        sections_alpha0 = []

        # collect alpha0 of airfoils of wing sections from airfoil polar

        for i, section_y in enumerate (self.wing.sections_y):
            airfoil_polar = self.polar.airfoil_polar_sections [i]

            # extrapolate alpha_eff of stripe to get value for root and tip 
            if i == 0:                                                  # extrapolate first section
                z = np.polyfit(y_stripes[:2], alpha_eff_stripes[:2], 1) # calculate polynomial of line
                f = np.poly1d(z)
                alpha_eff = f(section_y)
            elif i == len(self.wing.sections_y) - 1:                    # extrapolate last section
                z = np.polyfit(y_stripes[-2:], alpha_eff_stripes[-2:], 1)
                f = np.poly1d(z)
                alpha_eff = f(section_y)
            else:
                alpha_eff =  np.interp(section_y, y_stripes, alpha_eff_stripes)

            # interpolate cl value in airfoil polar base on alpha_eff
            cl = np.interp(alpha_eff, airfoil_polar.alpha, airfoil_polar.cl)  # local airfoil cl at section alpha

            # calaculate the inviscid equivalence alpha0 based on '2*pi'
            INVISCID_LIFT_SLOPE = 0.1097   # Cl per degree (2*pi per radian = 0.1097 per degree) 
            alpha0 = alpha_eff - cl / INVISCID_LIFT_SLOPE 

            sections_alpha0.append(alpha0)

        # alpha0 of stripe by interpolation of section alpha0 

        alpha0_stripes = np.interp (self.wing.stripes_y, self.wing.sections_y, sections_alpha0)

        return alpha0_stripes


    def _calc_downwash (self, alpha0_stripes : np.ndarray) -> np.ndarray:
        """ 
        calculate initial downwash per panel as the sum of 
            - geometric alpha 
            - alpha0 of the airfoils to represent camber of an airfoil 
        """

        downwash_geo   = self.polar.vtas * np.radians (self.alpha)   

        wj = np.zeros(self.wing.n_panels)

        for istripe in range (self.wing.ny_panels):

            alpha0_stripe   = - alpha0_stripes[istripe]         # alpha0 is negative 
            downwash_alpha0 = self.polar.vtas * np.radians (alpha0_stripe)  

            istart = istripe * self.wing.nx_panels
            iend   = istart + self.wing.nx_panels 
            wj [istart:iend] = (downwash_geo + downwash_alpha0) / self.polar.vtas

        return wj


