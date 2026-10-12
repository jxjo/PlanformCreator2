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

import csv
import numpy as np
from copy                           import copy
from enum                           import StrEnum
from typing                         import Tuple, NamedTuple, TYPE_CHECKING
from time                           import perf_counter

from .VLM                           import calc_Qjj
from airfoileditor.base.math_util   import extremum
from airfoileditor.model.polar_set  import Polar_Set, polarType, Polar, Polar_Definition, RE_SCALE_ROUND_TO
from airfoileditor.model.airfoil    import GEO_BASIC

if TYPE_CHECKING:
    from .wing                      import Wing                                        # avoid circular import
    from .planform                  import WingSection
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
    LIFT_SPAN       = "Lift per span [N]"                   # lift per unit span at stripe

    MAX_MASK        = "cl VLM close MAX"                    # numpy mask where Cl_VLM reaches, exceeds CL_MAX              
    ERROR_MASK      = "VLM error"                           # numpy mask where VLM couldn't calculate Cp    

    ALPHA           = "alpha"                               # angle of attack of panel
    ALPHA_EFF       = "alpha effective (VLM)"               # effective angle of attack (based on VLM))
    ALPHA_IND       = "alpha induced"                      # induced angle from the selected method
    ALPHA0_AIRFOIL  = "alpha0 airfoil"                      # alpha0 from airfoil polar  

    CD              = "cd"                                  # local drag coefficient 
    CD_IND          = "cd induced"                          # local induced drag coefficient
    CD_AIRFOIL      = "cd airfoil"                          # local airfoil drag coefficient
    CD_SPAN         = "cd per span [1/m]"                   # drag coefficient contribution normalized per unit span
    CD_IND_SPAN     = "cd induced per span [1/m]"           # induced drag coefficient contribution normalized per unit span
    CD_AIRFOIL_SPAN = "cd airfoil per span [1/m]"           # airfoil drag coefficient contribution normalized per unit span    

    DRAG_IND_SPAN   = "Drag induced per span [N/m]"         # induced drag force distribution density along span
    DRAG_AIRFOIL_SPAN = "Drag airfoil per span [N/m]"       # airfoil drag force distribution density along span
    DRAG_SPAN       = "Drag per span [N/m]"                 # total drag force distribution density along span

    CM_AIRFOIL      = "cm airfoil 2D"                       # 2D profile moment coefficient (isolated around local stripe quarter-chord)
    CM_VLM          = "cm vlm"                              # VLM moment coefficient contribution around cm_ref
    CM              = "cm"                                  # total moment coefficient contribution (VLM + airfoil) around cm_ref
    MOMENT_SPAN     = "Moment per span [N]"                 # global moment contribution along span around cm_ref

    WING_ALPHA      = "Alpha"                               # angle of attack of wing
    WING_LIFT       = "Lift [N]"                            # total lift of wing
    WING_CL         = "CL"                                  # total lift coefficient of wing
    WING_DRAG       = "Drag [N]"                            # total drag of wing
    WING_DRAG_IND   = "Drag induced [N]"                    # total induced drag of wing
    WING_DRAG_AIRFOIL= "Drag airfoil [N]"                   # total airfoil drag of wing
    WING_CD         = "CD"                                  # total drag coefficient of wing
    WING_CD_IND     = "CD induced"                          # total induced drag coefficient of wing
    WING_CD_AIRFOIL = "CD airfoil"                          # total airfoil drag coefficient of wing
    WING_EFFICIENCY = "e aero"                              # aerodynamic efficiency factor
    WING_GLIDE      = "CL/CD"                               # total glide ratio of wing
    WING_GLIDE_AIRFOIL= "CL/CD airfoil"                     # total airfoil glide ratio of wing 
    WING_CM         = "CM"                                  # total moment coefficient of wing around cm_ref
    WING_MOMENT     = "Moment [N*m]"                        # total moment of wing around cm_ref


class VLM_KPI (StrEnum_Extended):
    """ Key Performance Indicators for VLM wing polar analysis """
    MAX_CL          = "Max CL"
    MIN_CD          = "Min CD"
    MAX_LIFT        = "Max Lift"
    MAX_GLIDE       = "Max Glide"
    MIN_DRAG        = "Min Drag"


class Point_3D (NamedTuple):
    """ point having x,y,z coordinates"""

    x : float 
    y : float
    z : float 



RE_TOLERANCE  = 0.02                                    # Re tolerance for polars to be considered matching


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

        # geometry data from real Wing used for VLM calculation in [m]

        plan_area           = wing.planform.calc_area()
        wing_span           = wing.planform.span * 2.0 / 1000.0
        mac, mac_le_y, np   = wing.planform.calc_mac_np()

        self._wing_area     = plan_area * 2.0 / 1_000_000.0
        self._wing_ar       = wing_span**2 / self._wing_area
        self._mac           = mac / 1000.0
        self._mac_le_x      = mac_le_y / 1000.0
        self._np            = (np[0] / 1000.0, np[1] / 1000.0)

        # get mesh, create panels for the right wing side

        y_stations, x_stations, sections  = wing.planform_mesh.create_mesh()

        self._stations_y    = y_stations / 1000.0   # spanwise mesh stations in meters
        self._stations_x    = x_stations / 1000.0   # chordwise mesh stations in meters
        self._sections      = sections              # wing sections of the actual mesh

        panels_by_stripe, distorted = self._generate_panels ()

        self._panels_by_stripe = panels_by_stripe   # panels indexed by span stripe, then chordwise
        self._has_distorted = distorted             # indicate distorted (bad) panels
        self._sections_y    = None                  # y position of wing sections in m

        # ensure all wing sections have straked airfoils

        self._polars: list[VLM_Polar_CacheEntry] = []    # list of (root polar def, VLM polar)

        wing_sections = wing.planform.wingSections
        if not wing_sections.strak_done:
            wing_sections.do_strak (geometry_class=GEO_BASIC)
    
        # ensure all wingSections have a polar with the current re

        wing_sections.refresh_polar_sets (reset=False)


    def __repr__(self) -> str:
        # overwrite to get a nice print string
        return f"<{type(self).__name__}>"


    @property
    def wing_area (self) -> float:
        """Total wing area including fuselage  [m²]"""
        return self._wing_area

    @property
    def wing_ar (self) -> float:
        """Aspect ratio of the wing"""
        return self._wing_ar

    @property
    def wing_loading (self) -> float:
        """Wing loading in [N/m²]"""
        if self.wing_area is None or self._wing.mass is None:
            return None
        g = 9.80665  # m/s²
        return self._wing.mass * g / self.wing_area

    @property
    def cm_ref_x (self) -> float:
        """x-coordinate of the reference center of mass of the wing [m]"""

        # default to quarter point of the mean aerodynamic chord
        return self._mac_le_x + 0.25 * self._mac
    
    @property
    def mac (self) -> float:
        """Mean aerodynamic chord of the wing [m]"""
        return self._mac

    @property
    def mac_le_x (self) -> float:
        """x-coordinate of the leading edge of the mean aerodynamic chord [m]"""
        return self._mac_le_x

    @property
    def sections (self) -> list [WingSection]:
        """Wing sections of actual mesh"""
        return self._sections


    @property
    def sections_y (self) -> np.ndarray:
        """ y position of wing sections in m"""

        if self._sections_y is None:
            y = [section.x / 1000 for section in self.sections]
            self._sections_y = np.array(y)
        return self._sections_y


    @property
    def stations_y (self) -> np.ndarray:
        """ y position of spanwise mesh stations in m"""
        return self._stations_y


    @property
    def stations_x (self) -> np.ndarray:
        """ x position of chordwise mesh stations in m"""
        return self._stations_x


    @property
    def stations_cn (self) -> np.ndarray:
        """ normed chord of spanwise mesh stations"""
        stations_le = self.stations_x[ :,0]
        stations_te = self.stations_x[ :,-1]
        root_chord = stations_te[0] - stations_le[0]
        return (stations_te - stations_le) / root_chord


    @property
    def panels (self) -> list [VLM_Panel]:
        """Panels of the right half wing in stripe-major order."""
        return [panel for stripe in self.panels_by_stripe for panel in stripe]


    @property
    def panels_by_stripe (self) -> list[list[VLM_Panel]]:
        """Panels grouped by spanwise stripe, then chordwise position."""
        return self._panels_by_stripe


    @property 
    def has_distorted_panels (self) -> bool: 
        """ True if some panels are too much distorted for VLM"""
        return self._has_distorted
    
    @property
    def n_panels (self) -> int:
        """ number of panels"""
        return self.ny_panels * self.nx_panels

    @property
    def nx_panels (self) -> int:
        """ number of panels in x direction"""
        if not self.panels_by_stripe:
            return 0
        return len (self.panels_by_stripe[0])

    @property
    def ny_panels (self) -> int:
        """ number of panels in y span direction"""
        return len (self.panels_by_stripe)

    @property
    def aerogrid (self) -> dict:
        """ aerogrid datastructure as input for VLM calculation"""
        if self._aerogrid is None: 
            self._aerogrid = self._build_aeorogrid (self.panels_by_stripe)
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
            logger.debug (f"{self} Qjj calculation: {self._Qjj.shape[0]} x {self._Qjj.shape[1]} ({dt:.4f}s)")

        return self._Qjj 


    @property
    def stripes_y (self) -> np.ndarray:
        """ y position of panel stripes (middle of panel)"""

        if self._stripes_y is None: 
            self._stripes_y = np.array ([stripe[0].offset_k[1] for stripe in self.panels_by_stripe])
        return self._stripes_y


    @property
    def stripes_width (self) -> np.ndarray:
        """ width of panel stripes"""

        if self._stripes_width is None: 
            self._stripes_width = np.array ([stripe[0].b for stripe in self.panels_by_stripe])
        return self._stripes_width

    @property
    def stripes_chord (self) -> np.ndarray:
        """ chord of the spanwise stripes, summed over all panels in each stripe"""

        if self._stripes_chord is None:
            self._stripes_chord = np.array ([sum (panel.l for panel in stripe)
                                             for stripe in self.panels_by_stripe])
        return self._stripes_chord

    @property
    def stripes_area (self) -> np.ndarray:
        """ area of the spanwise stripes, summed over all panels in each stripe"""

        if self._stripes_area is None:
            self._stripes_area = np.array ([sum (panel.A for panel in stripe)
                                            for stripe in self.panels_by_stripe])
        return self._stripes_area


    def polar_at (self, root_polar_def: Polar_Definition) -> 'VLM_Polar':
        """ returns polar for a root airfoil polar definition """

        if not isinstance (root_polar_def, Polar_Definition):
            return None

        # polar already exisiting in locasl cache ?
        for cached_polar_def, polar in self._polars:
            if cached_polar_def.is_equal_to (root_polar_def):
                return polar

        # create new polar for this root polar definition
        polar_def_copy = copy (root_polar_def)               # polar_def in cache must be inmutable 
        polar = VLM_Polar (self, polar_def_copy)             # calculate new Polar and opPoints 
        self._polars.append ((polar_def_copy, polar)) 
            
        return polar 


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
      

    def _generate_panels (self) -> tuple[list[list[VLM_Panel]], bool]:
        """ 
        Generate and return all panels of right half wing based on y and x station arrays of the mesh.

        The mesh is in [mm] - VLM uses [m]
        
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

        t0 = perf_counter()

        panels_by_stripe = []
        y_stations  = self._stations_y
        x_stations  = self._stations_x
        nx_stations = x_stations.shape[1]

        for iy in range (len(y_stations) -1): 

            # get y values of one panel stripe 

            y1 = y_stations [iy]
            y2 = y_stations [iy+1]

            x1_arr = x_stations [iy]
            x2_arr = x_stations [iy+1]
            stripe_panels = []

            # create n-1 panels of the stripe starting at le towards 

            for ip in range (nx_stations - 1):

                p0 = Point_3D (x1_arr[ip],   y1, 0.0)
                p1 = Point_3D (x1_arr[ip+1], y1, 0.0)
                p2 = Point_3D (x2_arr[ip+1], y2, 0.0)
                p3 = Point_3D (x2_arr[ip],   y2, 0.0)
                panel = VLM_Panel (p0, p1, p2, p3) 

                stripe_panels.append (panel)

                if panel.is_distorted:
                    has_distorted_panels = True 

            panels_by_stripe.append (stripe_panels)

        dt = perf_counter() - t0
        n_panels = sum (len (stripe) for stripe in panels_by_stripe)
        logger.debug (f"{self} {n_panels} panels created in {dt:.4f}s")

        return panels_by_stripe, has_distorted_panels 
        

    def _build_aeorogrid (self, panels_by_stripe : list[list[VLM_Panel]]) -> dict: 
        """ build the aeorgrid datastructure out of panel data """

        n = sum (len (stripe) for stripe in panels_by_stripe)

        l, b, A   = np.zeros (n), np.zeros (n), np.zeros (n)
        N         = np.zeros ((n,3)) 
        offset_l  = np.zeros ((n,3)) 
        offset_k  = np.zeros ((n,3)) 
        offset_j  = np.zeros ((n,3)) 
        offset_P1 = np.zeros ((n,3))
        offset_P3 = np.zeros ((n,3)) 

        i = 0
        for stripe in panels_by_stripe:
            for panel in stripe:
                
                l [i] = panel.l
                b [i] = panel.b
                A [i] = panel.A
                N [i] = panel.N
                offset_l [i]  = panel.offset_l                
                offset_k [i]  = panel.offset_k                
                offset_j [i]  = panel.offset_j                
                offset_P1 [i] = panel.offset_P1                
                offset_P3 [i] = panel.offset_P3
                i += 1

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
        self._branch = None                         # cached indices of increasing-lift branch

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
        return self._interpolate_at_cl (0.0, self.alpha)

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
        return self._interpolate_at_cl (cl, self.cd)

    def cm_at_cl (self, cl: float) -> float | None:
        return self._interpolate_at_cl (cl, self.cm)


    @property
    def _branch_idx (self) -> np.ndarray:
        """indices of the increasing-lift branch, strictly increasing in cl"""

        if self._branch is None:
            i_min, i_max = int (np.argmin (self.cl)), int (np.argmax (self.cl))
            idx      = np.arange (i_min, i_max + 1)
            cl       = self.cl[idx]
            prev_max = np.maximum.accumulate (np.r_[-np.inf, cl[:-1]])
            self._branch = idx[cl > prev_max]               # drop wiggles below a previous max
        return self._branch


    def _interpolate_at_cl (self, cl: float, values: np.ndarray) -> float | None:
        """Interpolate on the increasing-lift branch, excluding post-stall points."""

        if not len (self.cl) or not np.isfinite (cl):
            return None

        idx = self._branch_idx
        val = np.interp (cl, self.cl[idx], values[idx], left=np.nan, right=np.nan)
        return float (val) if np.isfinite (val) else None


# ----------------------------------------------------------


class VLM_Polar_Comparison:
    """Aligned samples from a current polar and its interpolated reference."""

    def __init__ (self, current_polar: VLM_Polar, ref_polar: VLM_Polar | None):

        self._current_polar = current_polar
        self._ref_polar     = ref_polar


    def deviation (self, base_var: VLM_Var, value_var: VLM_Var) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
        """Return base, current, and interpolated reference samples in alpha order."""

        current_alpha   = self._current_polar._of_var (VLM_Var.WING_ALPHA)
        current_base    = self._current_polar._of_var (base_var)
        current_values  = self._current_polar._of_var (value_var)

        n_current = min (len (current_alpha), len (current_base), len (current_values))
        current_alpha   = current_alpha[:n_current]
        current_base    = current_base[:n_current]
        current_values  = current_values[:n_current]

        order = np.argsort (current_alpha, kind="stable")
        current_alpha   = current_alpha[order]
        current_base    = current_base[order]
        current_values  = current_values[order]

        if n_current == 0:
            return current_base, current_values, np.full (n_current, np.nan)

        if self._ref_polar is None:
            reference_values = np.full (n_current, np.nan)
        else:
            ref_alpha   = self._ref_polar._of_var (VLM_Var.WING_ALPHA)
            ref_base    = self._ref_polar._of_var (base_var)
            ref_values  = self._ref_polar._of_var (value_var)
            reference_values = self._interpolate_reference (ref_alpha, ref_base, ref_values, current_base)

        reference_values[~np.isfinite (current_values)] = np.nan

        return current_base, current_values, reference_values


    @staticmethod
    def _interpolate_reference (alpha: np.ndarray, base: np.ndarray, values: np.ndarray,
                                base_values: np.ndarray) -> np.ndarray:
        """Interpolate finite reference samples, rejecting out-of-range or ambiguous values."""

        n_samples = min (len (alpha), len (base), len (values))

        # trim arrays to the same length
        alpha  = np.asarray (alpha[:n_samples], dtype=float)
        base   = np.asarray (base[:n_samples], dtype=float)
        values = np.asarray (values[:n_samples], dtype=float)

        # keep only finite samples for interpolation
        valid  = np.isfinite (alpha) & np.isfinite (base) & np.isfinite (values)
        order  = np.argsort (alpha[valid], kind="stable")

        base   = base[valid][order]
        values = values[valid][order]

        base_values  = np.asarray (base_values, dtype=float)
        interpolated = np.full (base_values.shape, np.nan)

        # interpolate each base value using the reference samples
        for index, base_value in np.ndenumerate (base_values):
            if not np.isfinite (base_value):
                continue

            candidates = list (values[base == base_value])
            for i in range (len (base) - 1):
                base_start, base_end = base[i], base[i + 1]
                if base_start == base_end == base_value:
                    candidates.extend ((values[i], values[i + 1]))
                elif min (base_start, base_end) <= base_value <= max (base_start, base_end):
                    fraction = (base_value - base_start) / (base_end - base_start)
                    candidates.append (values[i] + fraction * (values[i + 1] - values[i]))

            if candidates and np.allclose (candidates, candidates[0], rtol=1e-9, atol=1e-12):
                interpolated[index] = candidates[0]

        return interpolated


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
        self._is_polar_generated        = False         # polar is completly generated and ready for use

        self._use_viscous_loop          = True          # viscous loop for opPoint alpha and cl calculation
        self._use_treffz                = True          # use Trefftz plane for induced drag calculation

        self._airfoil_polar_sections            = None          # polar of airfoil per section
        self._airfoil_polar_sections_normal     = None          # normal polar of airfoil per section
        self._airfoil_polar_stripes             = None
        self._airfoil_polar_stripes_normal      = None

        self._alpha0_stripes            = None          # alpha0 per stripe from interpolated stripe polar
        self._cl_min_stripes            = None          # cl min per stripe from interpolated stripe polar
        self._cl_max_stripes            = None          # cl max per stripe from interpolated stripe polar

        logger.debug (f"{self} created (viscous_loop:{self._use_viscous_loop}, trefftz:{self._use_treffz})")


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
    def is_neuralfoil (self) -> bool:  
        """ True if airfoil polars are from NeuralFoil (not Xfoil)""" 
        return self._root_polar_def.is_neuralfoil if self._root_polar_def else False


    @property 
    def use_viscous_loop (self) -> bool:
        """ use viscous loop - non-linear aero calculation"""
        return self._use_viscous_loop

    @property
    def use_trefftz (self) -> bool:
        """ use Trefftz plane for CD_int, alpha_int calculation"""
        return self._use_treffz


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
            a = round (alpha, 2)                        # ensure clean key for dict
            try:
                opPoint = self._opPoints[a]             # already exisiting
            except KeyError:

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

        cl = self._of_var (VLM_Var.WING_CL)
        x_var, y_var = xyVars
        x_values, y_values = self.of_vars ((x_var, y_var))
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
        return np.array ([polar.cd_at_cl (cl) for polar, cl in zip (self.airfoil_polar_stripes_normal, stripes_cl)],
                         dtype=float)                       # None -> nan


    def cm_at_stripes (self, stripes_cl: np.ndarray) -> np.ndarray:
        """Return moment coefficients from the interpolated normal stripe polars."""
        return np.array ([polar.cm_at_cl (cl) for polar, cl in zip (self.airfoil_polar_stripes_normal, stripes_cl)],
                         dtype=float)                       # None -> nan


    def kpi_in (self, kpi : VLM_KPI, xyvars : tuple [VLM_Var, VLM_Var]) -> tuple[VLM_Var | None, float | None, float | None, float | None]:
        """Return the value of the specified KPI for the given xyvars tuple."""

        if not kpi in VLM_KPI or not self._is_polar_generated:
            return None, None, None, None

        alpha = self._of_var (VLM_Var.WING_ALPHA)
        if kpi == VLM_KPI.MAX_LIFT:
            kpi_var = VLM_Var.WING_LIFT
            lift    = self._of_var (kpi_var)
            alpha_val, kpi_val = extremum (alpha, lift, mode="max")
        elif kpi == VLM_KPI.MAX_GLIDE:
            kpi_var = VLM_Var.WING_GLIDE
            glide   = self._of_var (kpi_var)
            alpha_val, kpi_val = extremum (alpha, glide, mode="max")
        elif kpi == VLM_KPI.MIN_DRAG:
            kpi_var = VLM_Var.WING_DRAG
            drag    = self._of_var (kpi_var)
            alpha_val, kpi_val = extremum (alpha, drag, mode="min")
        elif kpi == VLM_KPI.MAX_CL:
            kpi_var = VLM_Var.WING_CL
            cl      = self._of_var (kpi_var)
            alpha_val, kpi_val = extremum (alpha, cl, mode="max")
        elif kpi == VLM_KPI.MIN_CD:
            kpi_var = VLM_Var.WING_CD
            cd      = self._of_var (kpi_var)
            alpha_val, kpi_val = extremum (alpha, cd, mode="min")
        else:
            kpi_var = None
            kpi_val = None

        if kpi_val is None:
            logger.warning (f"KPI {kpi} could not be determined for {self}")
            return None, None, None, None

        # get x,y values of extremum in the context of the requested xyvars
        xvar, yvar = xyvars

        x,y = self.of_vars ((xvar, yvar))

        x_valid = np.isfinite (alpha) & np.isfinite (x)
        y_valid = np.isfinite (alpha) & np.isfinite (y)
        kpi_base_x = np.interp (alpha_val, alpha[x_valid], x[x_valid]) if np.any (x_valid) else None
        kpi_base_y = np.interp (alpha_val, alpha[y_valid], y[y_valid]) if np.any (y_valid) else None

        return kpi_var, kpi_val, kpi_base_x, kpi_base_y
   
    # ---- private ----


    def _get_matching_polar (self, airfoil_polarSet: Polar_Set, section_re: float,
                             is_vlm: bool, re_scale: float) -> Polar | None:
        """ return the matching normal or forced-transition polar for a section Re """

        polar_def_target = copy (self._root_polar_def)
        polar_def_target.set_re (section_re)
        re_rel_tolerance = RE_TOLERANCE
        if re_scale != 1.0:
            re_rel_tolerance = max (re_rel_tolerance, (RE_SCALE_ROUND_TO / 2) / section_re)

        matching_polar = None
        best_re_delta  = float("inf")

        polar_list = airfoil_polarSet.polars_VLM if is_vlm else airfoil_polarSet.polars_normal
        for polar in polar_list:
            if polar.is_equal_to (polar_def_target, ignore_active=True, ignore_xtrip=True,
                                  re_rel_tolerance=re_rel_tolerance):
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
                msg = f"{self} {section} airfoil {section.airfoil} not loaded"
                logger.debug (msg)
                self._error_reason.append (msg)
                break

            section_re = self._root_polar_def.re * section.cn

            if section_re <= 1000:
                msg = f"{self} {section} has too low Re ({section_re:.0f})"
                logger.error (msg)
                self._error_reason.append(msg)
                break

            airfoil_polarSet : Polar_Set = section.airfoil.polarSet

            if airfoil_polarSet is None: 
                msg = f"{self} {section} airfoil {section.airfoil} has no polarSet"
                logger.error (msg)
                self._error_reason.append (msg)
                break

            if is_vlm:
                airfoil_polarSet.ensure_polars_VLM()

            # Find and load only the polar matching this wing section.

            matching_polar = self._get_matching_polar (airfoil_polarSet, section_re, is_vlm, section.cn)

            if matching_polar is not None:
                airfoil_polarSet.load_or_generate_polars (
                    normal=not is_vlm, VLM=is_vlm, only_polars=[matching_polar])

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
                msg = (f"No {polar_type} polar matching root definition within ±{RE_TOLERANCE:.0%} Re "
                       f"for section {section.id} (target Re={section_re:.0f}) " )
                logger.error (msg)
                logger.debug (f"airfoil polarSet: {airfoil_polarSet.polars}")
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


    def of_vars (self, xyVars: Tuple[VLM_Var, VLM_Var]) -> Tuple[np.ndarray, np.ndarray]:
        """ returns x,y polar of the tuple xyVars"""
    
        if isinstance(xyVars, tuple):
            x, y = self._of_var (xyVars[0]), self._of_var (xyVars[1])
        else:
            x, y = np.array([]), np.array([])
        return x,y 


    def deviation_from (self, ref_polar: 'VLM_Polar',
                        delta_vars: Tuple[VLM_Var, VLM_Var]) -> tuple[np.ndarray, np.ndarray, bool]:
        """
        Return deviation of self to a reference polar.

        Args:
            ref_polar: The reference VLM polar to compare against.
            delta_vars: Tuple containing the base variable and the comparison variable.
        
        Returns:
            (base, deviation, is_relative), with a boolean identifying relative values.
            CD, drag, and glide use (current - reference) / reference;
            other quantities use current - reference in their original units.
            Relative samples with near-zero references are returned as NaN.
            Missing delta_vars returns empty arrays and is_relative=False.
        """

        if delta_vars is None:
            return np.array([]), np.array([]), False

        base_var, value_var = delta_vars
        base, current, reference = VLM_Polar_Comparison (self, ref_polar).deviation (base_var, value_var)

        reference_min = {
            VLM_Var.WING_CD: 1e-8,
            VLM_Var.WING_CD_IND: 1e-8,
            VLM_Var.WING_CD_AIRFOIL: 1e-8,
            VLM_Var.WING_DRAG: 1e-6,
            VLM_Var.WING_DRAG_IND: 1e-6,
            VLM_Var.WING_DRAG_AIRFOIL: 1e-6,
            VLM_Var.WING_GLIDE: 0.05,
            VLM_Var.WING_GLIDE_AIRFOIL: 0.05,
        }.get (value_var)

        if reference_min is None:
            return base, current - reference, False

        valid = np.isfinite (current) & np.isfinite (reference)
        valid &= reference > reference_min
        if value_var in (VLM_Var.WING_GLIDE, VLM_Var.WING_GLIDE_AIRFOIL):
            valid &= base > 0.05

        relative = np.full (current.shape, np.nan)
        np.divide (current - reference, reference, out=relative, where=valid)
        return base, relative, True


    def _of_var (self, polar_var: VLM_Var) -> np.ndarray:
        """ return cached values for a polar variable """

        # collect values of existing opPoints for the requested polar_var
        vals = []
        opPoint : VLM_OpPoint
        for opPoint in self._opPoints.values():
            vals.append(opPoint.aero_results.get(polar_var, np.nan))

        return np.array(vals)


    def generate_polar (self, alpha_start = -3.0, alpha_max :float = 25.0, step = 0.25):
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
        logger.debug (f"{self} polar generation: {len(self._opPoints)} opPoints, alpha={alpha_start:.1f}..{alpha_max:.1f}, step={step} ({dt:.4f}s)")

        # self._test_print()


    def _test_print (self):
        """ print polar data alpha, Cl, Lift """

        print (f"{self} polar data")
        print (f"  {len(self._opPoints)} opPoints")
        print (f"  {'Alpha':>8s} {'CL':>8s} {'Lift':>8s} {'CD':>8s} {'CD_i':>8s} {'Drag':>8s} {'Glide':>8s} {'CM':>8s} {'Moment':>8s}")

        alpha = self._of_var (VLM_Var.WING_ALPHA)
        cl    = self._of_var (VLM_Var.WING_CL)
        lift  = self._of_var (VLM_Var.WING_LIFT)
        cd    = self._of_var (VLM_Var.WING_CD)
        cd_i  = self._of_var (VLM_Var.WING_CD_IND)
        drag  = self._of_var (VLM_Var.WING_DRAG)
        glide = self._of_var (VLM_Var.WING_GLIDE)
        cm    = self._of_var (VLM_Var.WING_CM)
        moment = self._of_var (VLM_Var.WING_MOMENT)
        for i in range(len(alpha)):
            print (f"  {alpha[i]:8.2f} {cl[i]:8.3f} {lift[i]:8.1f} {cd[i]:8.4f} {cd_i[i]:8.4f} {drag[i]:8.2f} {glide[i]:8.2f} {cm[i]:8.3f} {moment[i]:8.3f}")

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
        return f"{self.polar.vtas:.1f}m/s, {self.alpha:5.2f}°"

    
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

            t0 = perf_counter()
            aero_results_list = self._viscous_loop ()

            self._aero_results  = aero_results_list [-1]

            # VLM_linear is result of first iteration 
            aero_results_linear = aero_results_list [0]
            self._aero_results[VLM_Var.CL_VLM_LINEAR] = aero_results_linear[VLM_Var.CL_VLM]

            # add drag results to aero_results after viscous loop is finished
            drag_results = self._calc_drag ()
            self._aero_results.update (drag_results)

            # add moment results to aero_results after viscous loop is finished
            moment_results = self._calc_moment ()
            self._aero_results.update (moment_results)

            dt = perf_counter() - t0
            cl = self.aero_results[VLM_Var.WING_CL]
            logger.debug (f"{self} aero results: CL{cl:6.3f}, cl_max_reached {self._cl_max_reached}  ({dt:.4f} s)")

            print (f"e: {self.aero_results[VLM_Var.WING_EFFICIENCY]:.3f}  CD_ind {self.aero_results[VLM_Var.WING_CD_IND]:.5f} CD_ind_elliptical {self.Cd_ind_elliptical:.5f}")

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


    @property
    def Cd_ind_elliptical (self) -> float:
        """ elliptical induced drag coefficient """

        if not self._aero_results:
            return 0.0

        Cl   = self.aero_results[VLM_Var.WING_CL]
        ar   = self.wing.wing_ar
        Cd_ind_elliptical = Cl**2 / (np.pi * ar) if ar != 0.0 else 0.0

        return Cd_ind_elliptical

    # ----- private --------------------------------------------------

    def _calc_lift (self, wj: np.ndarray, Cp: np.ndarray, alpha0: np.ndarray) -> dict:
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
        self._has_vlm_error  = bool (VLM_error.any())

        # alpha_eff based on cl_vlm and alpha0 of airfoil polar (iteration dependent)

        alpha_eff_VLM     = np.degrees (cl_vlm_masked / (2 * np.pi)) + alpha0

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
        results[VLM_Var.ALPHA0_AIRFOIL] = self.polar.alpha0_stripes         # airfoil alpha0 from polar
        results[VLM_Var.ALPHA_EFF]      = alpha_eff_VLM            
        results[VLM_Var.ALPHA]          = np.full (ns, self.alpha)     

        results[VLM_Var.WING_ALPHA]     = self.alpha
        results[VLM_Var.WING_LIFT]      = Lift
        results[VLM_Var.WING_CL]        = CL

        # logger.debug (f"{self} aero results: Lift {Lift:.2f} N, CL {CL:.3f}, cl_max_reached {self._cl_max_reached}, VLM_error {self._has_vlm_error}")
        return results 


    def _calc_drag (self) -> dict:
        """Calculate drag results using the selected induced-drag method."""

        wing_area     = self.wing.wing_area
        half_wing_area= wing_area / 2.0
        chord         = self.wing.stripes_chord
        y             = self.wing.stripes_y
        dy            = self.wing.stripes_width

        # from aero results of viscous loop per stripe

        cl            = self.aero_results [VLM_Var.CL]
        CL            = self.aero_results [VLM_Var.WING_CL]

        if self.polar._use_treffz:
            induced_results = self._calc_drag_trefftz ()
        else:
            induced_results = self._calc_drag_nearfield ()

        cd_ind          = induced_results[VLM_Var.CD_IND]
        cd_ind_span     = induced_results[VLM_Var.CD_IND_SPAN]
        drag_ind_span   = induced_results[VLM_Var.DRAG_IND_SPAN]
        CD_ind          = induced_results[VLM_Var.WING_CD_IND]
        Drag_ind        = induced_results[VLM_Var.WING_DRAG_IND]
        wing_ar         = self.wing.wing_ar
        efficiency      = CL**2 / (np.pi * wing_ar * CD_ind) if wing_ar != 0.0 and CD_ind != 0.0 else 0.0

        # drag coefficients per stripe

        cd_airfoil    = self.polar.cd_at_stripes (cl)               # use 2D airfoil
        cd            = cd_airfoil + cd_ind

        # normalized drag coefficients contribution per span unit [1/m]

        cd_airfoil_span    = cd_airfoil * chord  / half_wing_area
        cd_span            = cd * chord  / half_wing_area

        # total drag coefficient of wing

        CD_airfoil     = np.sum(cd_airfoil_span * dy)
        CD             = CD_airfoil + CD_ind

        # drag contribution per span unit [N/m]

        drag_airfoil_span  = cd_airfoil * self.polar.q_dyn * chord
        drag_span          = drag_airfoil_span + drag_ind_span

        # total drag of wing

        if not np.isnan (cd_airfoil).any ():
            Drag_airfoil     = CD_airfoil * self.polar.q_dyn * wing_area
            Drag             = CD         * self.polar.q_dyn * wing_area
        else:
            Drag_ind         = np.nan
            Drag_airfoil     = np.nan
            Drag             = np.nan

        drag_results = {
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
            VLM_Var.WING_EFFICIENCY:   efficiency,
            VLM_Var.WING_GLIDE:        CL/CD if CD != 0 else np.nan,
            VLM_Var.WING_GLIDE_AIRFOIL:CL/CD_airfoil if CD_airfoil != 0 else np.nan,
        }

        drag_results[VLM_Var.ALPHA_IND] = induced_results[VLM_Var.ALPHA_IND]

        return drag_results


    def _calc_drag_nearfield (self) -> dict:
        """Calculate induced drag from local lift and near-field downwash."""

        wing      = self.wing
        chord     = wing.stripes_chord
        cl_vlm    = self.aero_results[VLM_Var.CL_VLM]
        alpha_eff = self.aero_results[VLM_Var.ALPHA_EFF]
        alpha_ind = self.alpha - alpha_eff
        cd_ind    = np.deg2rad (alpha_ind) * cl_vlm

        cd_ind_span   = cd_ind * chord / (wing.wing_area / 2.0)
        drag_ind_span = self.polar.q_dyn * chord * cd_ind
        cd_ind_total  = np.sum (cd_ind_span * wing.stripes_width)
        drag_ind      = cd_ind_total * self.polar.q_dyn * wing.wing_area

        return {
            VLM_Var.ALPHA_IND:     alpha_ind,
            VLM_Var.CD_IND:        cd_ind,
            VLM_Var.CD_IND_SPAN:   cd_ind_span,
            VLM_Var.DRAG_IND_SPAN: drag_ind_span,
            VLM_Var.WING_CD_IND:   cd_ind_total,
            VLM_Var.WING_DRAG_IND: drag_ind,
        }


    def _calc_drag_trefftz (self) -> dict:
        """Calculate induced drag from the far-wake Trefftz-plane downwash."""

        wing      = self.wing
        n_stripes = wing.ny_panels

        if n_stripes == 0:
            return {
                VLM_Var.ALPHA_IND: np.array([]),
                VLM_Var.CD_IND: np.array([]),
                VLM_Var.CD_IND_SPAN: np.array([]),
                VLM_Var.DRAG_IND_SPAN: np.array([]),
                VLM_Var.WING_CD_IND: 0.0,
                VLM_Var.WING_DRAG_IND: 0.0,
            }

        vtas    = self.polar.vtas
        chord   = wing.stripes_chord
        y       = wing.stripes_y
        panels_by_stripe = wing.panels_by_stripe

        panel_chord = np.asarray (wing.aerogrid["l"])
        panel_gamma = 0.5 * vtas * panel_chord * self.Cp_panels
        gamma_half  = panel_gamma.reshape (n_stripes, wing.nx_panels).sum (axis=1)

        y_edges_half = np.array ([stripe[0].p0[1] for stripe in panels_by_stripe] +
                                 [panels_by_stripe[-1][0].p3[1]])

        # full wing span distribution
        y_full       = np.concatenate ((-y[::-1], y))
        gamma_full   = np.concatenate ((gamma_half[::-1], gamma_half))
        y_edges_full = np.concatenate ((-y_edges_half[::-1], y_edges_half[1:]))
        width_full   = np.diff (y_edges_full)

        # In the Trefftz plane, each spanwise jump in bound circulation forms
        # a streamwise wake vortex. Mirroring imposes the wing's x-z symmetry.
        gamma_with_tips = np.concatenate (([0.0], gamma_full, [0.0]))
        wake_gamma      = gamma_with_tips[:-1] - gamma_with_tips[1:]
        y_delta         = y_full[:, np.newaxis] - y_edges_full[np.newaxis, :]
        wake_velocity   = -np.sum (wake_gamma / (4.0 * np.pi * y_delta), axis=1)

        # final aerodynamic quantities in the Trefftz plane
        wake_velocity_half = wake_velocity[n_stripes:]
        area_half     = wing.wing_area / 2.0

        alpha_ind     = np.degrees (np.arctan (wake_velocity_half / vtas))
        cd_ind        = 2.0 * gamma_half * wake_velocity_half / (vtas**2 * chord)
        cd_ind_span   = cd_ind * chord / area_half
        drag_ind_span = self.polar.q_dyn * chord * cd_ind

        drag_ind      = (2.0 * self.polar.q_dyn / vtas**2) * np.sum (
                            gamma_full * wake_velocity * width_full)
        cd_ind_total  = drag_ind / (self.polar.q_dyn * wing.wing_area)

        return {
            VLM_Var.ALPHA_IND:      alpha_ind,
            VLM_Var.CD_IND:         cd_ind,
            VLM_Var.CD_IND_SPAN:    cd_ind_span,
            VLM_Var.DRAG_IND_SPAN:  drag_ind_span,
            VLM_Var.WING_CD_IND:    cd_ind_total,
            VLM_Var.WING_DRAG_IND:  drag_ind,
        }


    def _calc_moment (self) -> dict:
        """Calculate aerodynamic moments from VLM and airfoil data matching XFLR5 tracking."""

        n        = self.wing.n_panels
        nx       = self.wing.nx_panels
        ns       = int(n / nx)

        wing_area = self.wing.wing_area
        chord     = self.wing.stripes_chord    # Shape: (ns,)
        dy        = self.wing.stripes_width    # Shape: (ns,)

        mac       = self.wing.mac
        cm_ref_x  = self.wing.cm_ref_x                          # moment reference x-coordinate

        q_dyn     = self.polar.q_dyn

        # panel geometry and pre-computed quantities
        offset_l  = self.wing.aerogrid["offset_l"]
        N         = self.wing.panels_normal
        A         = self.wing.panels_area
        Cp        = self._cp

        # 1. compute 3d panel forces
        lift_panels = q_dyn * N[:, 2] * A * Cp
        F = np.zeros((n, 3))
        F[:, 2] = lift_panels

        # 2. global referenced moments per stripe
        r_global = offset_l - np.array([cm_ref_x, 0.0, 0.0])
        M_global_panels = np.cross(r_global, F)
        
        # The natural right-hand rule cross-product output along the Y-axis 
        # correctly represents a negative nose-down moment.
        M_pitch_global = M_global_panels[:, 1]  

        M_pitch_block = M_pitch_global.reshape(ns, nx)
        M_stripe_vlm  = M_pitch_block.sum(axis=1)  # Shape: (ns,) [N*m]

        # 3. airfoil moment 
        cl = self.aero_results[VLM_Var.CL]
        cm_airfoil = self.polar.cm_at_stripes(cl)
        M_profile_stripe = cm_airfoil * q_dyn * chord**2 * dy

        # Combined dimensional raw moment per stripe [N*m]
        M_stripe_global = M_stripe_vlm + M_profile_stripe

        # 4. dimensional span distribution [n/m]
        moment_per_span = M_stripe_global / dy  

        # 5. fixed spanwise sections normalization
        cm_total_span = moment_per_span / (q_dyn * chord**2)
        cm_vlm_span   = (M_stripe_vlm / dy) / (q_dyn * chord**2)

        # 6. global aircraft total cm
        M_total  = np.sum(M_stripe_global) * 2
        CM_total = M_total / (q_dyn * wing_area * mac)

        return {
            VLM_Var.CM_AIRFOIL:   cm_airfoil,
            VLM_Var.CM_VLM:       cm_vlm_span,     
            VLM_Var.CM:           cm_total_span,   
            VLM_Var.MOMENT_SPAN:  moment_per_span, 
            VLM_Var.WING_MOMENT:  M_total,
            VLM_Var.WING_CM:      CM_total,
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

        results = self._calc_lift (wj, cp, alpha0)             # derive all values from cp per panels    

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

                results = self._calc_lift (wj, cp, alpha0)

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
        sections_y = self.wing.sections_y
        airfoil_polar_sections = self.polar.airfoil_polar_sections
        n_sections = len (sections_y)

        sections_alpha0 = []

        # collect alpha0 of airfoils of wing sections from airfoil polar

        for i, section_y in enumerate (sections_y):
            airfoil_polar = airfoil_polar_sections [i]

            # extrapolate alpha_eff of stripe linearly to get value for root and tip 
            if i == 0:
                y0, y1 = y_stripes[0], y_stripes[1]
                a0, a1 = alpha_eff_stripes[0], alpha_eff_stripes[1]
                alpha_eff = a0 + (a1 - a0) / (y1 - y0) * (section_y - y0)
            elif i == n_sections - 1:
                y0, y1 = y_stripes[-2], y_stripes[-1]
                a0, a1 = alpha_eff_stripes[-2], alpha_eff_stripes[-1]
                alpha_eff = a0 + (a1 - a0) / (y1 - y0) * (section_y - y0)
            else:
                alpha_eff =  np.interp(section_y, y_stripes, alpha_eff_stripes)

            # Use only the pre-stall branch between cl_min and cl_max. On the
            # post-stall branch, cl falls again although alpha_eff is beyond
            # the lift peak. Following that branch could clear MAX_MASK in a
            # later iteration and make stalled stripes appear uncritical.
            # np.interp holds the endpoint cl outside this alpha range:
            # cl_min below the range and cl_max above it. This is a limited
            # continuation for stall detection, not a post-stall flow model.
            index_min = np.argmin (airfoil_polar.cl)
            index_max = np.argmax (airfoil_polar.cl)
            pre_stall = slice (index_min, index_max + 1)
            cl = np.interp (alpha_eff, airfoil_polar.alpha[pre_stall], airfoil_polar.cl[pre_stall])

            # Keep alpha_eff unclipped: alpha0 is the equivalent inviscid
            # offset for this iteration, not the physical zero-lift angle.
            # With cl capped, alpha0 absorbs further changes in alpha_eff.
            
            # The inviscid slope is 2*pi per radian, or 0.1097 per degree.
            INVISCID_LIFT_SLOPE = 0.1097
            alpha0 = alpha_eff - cl / INVISCID_LIFT_SLOPE 

            sections_alpha0.append(alpha0)

        # alpha0 of stripe by interpolation of section alpha0 

        alpha0_stripes = np.interp (y_stripes, sections_y, sections_alpha0)

        return alpha0_stripes


    def _calc_downwash (self, alpha0_stripes : np.ndarray) -> np.ndarray:
        """ 
        calculate initial downwash per panel as the sum of 
            - geometric alpha 
            - alpha0 of the airfoils to represent camber of an airfoil 
        """

        # normalized downwash (w/V) - same value for all panels of a stripe
        wj_stripes = np.radians (self.alpha - np.asarray (alpha0_stripes))

        return np.repeat (wj_stripes, self.wing.nx_panels)


    def export_to_csv (self, pathFileName: str):
        """Write this operating point in an XFLR5-like report with PC2 diagnostics."""

        results = self.aero_results
        polar = self.polar
        wing = self.wing

        columns = [
            ("y-span [m]",      results[VLM_Var.Y], ".3f"),
            ("Chord [m]",       wing.stripes_chord, ".3f"),
            ("Ai [deg]",        -results[VLM_Var.ALPHA_IND], ".3f"),
            ("Cl ",             results[VLM_Var.CL], ".3f"),
            ("Cd airfoil",      results[VLM_Var.CD_AIRFOIL], ".5f"),
            ("Cd induced",      results[VLM_Var.CD_IND], ".5f"),
            ("Cm geo",          results[VLM_Var.CM_VLM], ".3f"),
            ("Cm airf (c/4)",   results[VLM_Var.CM_AIRFOIL], ".3f"),
        ]

        n_stripes = len (results[VLM_Var.Y])
        stripes = polar.airfoil_polar_stripes
        if len (stripes) == n_stripes:
            strip_cl = results[VLM_Var.CL_VLM]
            xtr_top = np.array ([stripe._interpolate_at_cl (cl, stripe.xtrt)
                                 for stripe, cl in zip (stripes, strip_cl)], dtype=object)
            xtr_bottom = np.array ([stripe._interpolate_at_cl (cl, stripe.xtrb)
                                    for stripe, cl in zip (stripes, strip_cl)], dtype=object)
            columns.extend ([
                ("XTrtop", xtr_top, ".3f"),
                ("XTrBot", xtr_bottom, ".3f"),
            ])

        columns.extend ([
            ("Cl VLM",              results[VLM_Var.CL_VLM], ".3f"),
            ("Cl linear",           results[VLM_Var.CL_VLM_LINEAR], ".3f"),
            ("Alpha [°]",           results[VLM_Var.ALPHA], ".2f"),
            ("Alpha eff [°]",       results[VLM_Var.ALPHA_EFF], ".2f"),
            ("Alpha0 airfoil [°]",  results[VLM_Var.ALPHA0_AIRFOIL], ".2f"),
            ("Cd",                  results[VLM_Var.CD], ".5f"),
            ("Cd airfoil",          results[VLM_Var.CD_AIRFOIL], ".5f"),
            ("Lift [N/m]",          results[VLM_Var.LIFT_SPAN], ".2f"),
            ("Drag induced [N/m]",  results[VLM_Var.DRAG_IND_SPAN], ".2f"),
            ("Drag airfoil [N/m]",  results[VLM_Var.DRAG_AIRFOIL_SPAN], ".2f"),
            ("Moment [N*m]",        results[VLM_Var.MOMENT_SPAN], ".2f"),
            ("Cm PC2",              results[VLM_Var.CM], ".3f"),
            ("VLM error",           results[VLM_Var.ERROR_MASK].astype(int), ".0f"),
            ("CL limit",            results[VLM_Var.MAX_MASK].astype(int), ".0f"),
        ])

        with open (pathFileName, mode="w", newline="", encoding="utf-8") as csvfile:
            writer = csv.writer (csvfile, dialect="excel")

            writer.writerow (["PlanformCreator2 VLM"])
            writer.writerow (["Wing",       wing._wing.name])
            writer.writerow (["Polar",      polar.name])
            writer.writerow (["QInf",       f"{polar.vtas:.2f}", "m/s"])
            writer.writerow (["Alpha",      f"{self.alpha:.2f}", "deg"])
            writer.writerow (["Beta",       "0.000000", "deg"])
            writer.writerow (["CL",         f"{results[VLM_Var.WING_CL]:.3f}"])
            writer.writerow (["Cy",        "0.0"])
            writer.writerow (["CD",         f"{results[VLM_Var.WING_CD]:.5f}",
                              "CD induced", f"{results[VLM_Var.WING_CD_IND]:.5f}",
                              "CD airfoil", f"{results[VLM_Var.WING_CD_AIRFOIL]:.5f}"])
            writer.writerow (["Induced drag method", "Trefftz" if polar._use_treffz else "Near field"])
            writer.writerow (["e aero", f"{results[VLM_Var.WING_EFFICIENCY]:.3f}"])
            writer.writerow (["Cm",         f"{results[VLM_Var.WING_CM]:.3f}"])
            writer.writerow (["Moment",     f"{results[VLM_Var.WING_MOMENT]:.2f}", "N*m"])
            writer.writerow ([])
            writer.writerow (["Main Wing"])
            writer.writerow ([column[0] for column in columns])
            for stripe_index in range (n_stripes):
                row = []
                for _, values, precision in columns:
                    value = values[stripe_index]
                    row.append ("" if value is None else format (float (value), precision))
                writer.writerow (row)


