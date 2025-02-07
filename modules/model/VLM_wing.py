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
from enum                       import StrEnum
from typing                     import NamedTuple

from modules.model.VLM         import calc_Qjj, calc_Gamma

import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


#-------------------------------------------------------------------------------
# enums   
#-------------------------------------------------------------------------------

class StrEnum_Extended (StrEnum):
    """ enum extension to get a list of all enum values"""
    @classmethod
    def values (cls):
        return [c.value for c in cls]


class OpPoint_Var (StrEnum_Extended):
    """ polar variables """
    CL          = "Cl"               
    ALPHA       = "Alpha"               
    ALPHA_EFF   = "Alpha effective"               
    ALPHA_IND   = "Alpha induced"               
    LIFT        = "Lift"

#------------------------------------------------------------------------------


class Point_3D (NamedTuple):
    """ point having x,y,z coordinates"""

    x : float 
    y : float
    z : float 


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

        x = [self.p0[0] + 2, self.p1[0] - 2, self.p2[0] - 2, self.p3[0] + 2, self.p0[0] + 2]        
        y = [self.p0[1] + 2, self.p1[1] + 2, self.p2[1] - 2, self.p3[1] - 2, self.p0[1] + 2]

        return x, y



class VLM_Wing:

    """ 
    wing build with VLM_Panels based on Planform_Paneled

    """

    def __init__ (self, planform ):

        self._planform = planform                   # Planform_Paneled as base 

        self._panels_right  = None                  # VLM_Panels of the right wing side 
        self._aerogrid      = None                  # input datastructure of VLM calculation 
        self._Qjj           = None                  # matrix of aerodynamic influence coefficients
        self._BJJ           = None
        self._Gamma         = None                  # circulation
        self._Q_ind         = None 
        self._A_ges         = None                  # total panel area in m²

        self._polars        = {}                    # dict of polars 


    def __repr__(self) -> str:
        # overwrite to get a nice print string 
        return f"<{type(self).__name__}>"


    @property
    def panels_right (self) -> list [VLM_Panel]:
        """ panels of right half wing"""

        if self._panels_right is None: 
            self._panels_right = self._generate_panels_right ()
        return self._panels_right

    
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
            self._A_ges = A.sum() / 1000 ** 2
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
            logger.debug ( (f"{self} build Qjj"))

        return self._Qjj 

    @property
    def Gamma (self) -> np.ndarray:
        """ 
        2D array of gamma (circulation) values per panel
            - as self.panels are only right hand side, symmetric left side will be added by VLM
        """

        if self._Gamma is None: 
            self._Gamma, self._Q_ind = calc_Gamma (self.aerogrid, Ma=0.0, xz_symmetry = True)
            logger.debug ( (f"{self} calc Gamma Qjj"))

        return self._Gamma 

    @property
    def Gamma_panel (self) -> np.ndarray:
        """ 1D array of gamma (circulation) summed per panel"""

        G_panels = []
        for i in range (0, self.n_panels):
            G_panels.append (self.Gamma [i,:].mean())
        return np.array(G_panels) 

    @property
    def Gamma_y (self) -> np.ndarray:
        """ 1D array of gamma (circulation) summed per y stripe"""

        G_y = []
        for i in range (0, self.n_panels, self.nx_panels):
            G_y.append (self.Gamma_panel [i:(i+self.nx_panels)].mean())
        return np.array (G_y) 


    @property
    def y_stripes (self) -> np.ndarray:
        """ y position of all panel stripes (middle of panel)"""

        y_pos = []
        for i in range (0, self.n_panels, self.nx_panels):
            y_pos.append (self.aerogrid['offset_l'][i][1])
        return y_pos


    def polar_at (self, vtas: float) -> 'VLM_Polar':
        """ returns polar for air speed vtas"""

        v = round (vtas, 1)                         #  ensure clean key for dict
        try:
            polar = self._polars[v]                 # already exisiting 
        except:
            polar = VLM_Polar (self, v)         # calculate new opPOint 
            self._polars[v] = polar 
        return polar 


    # ----- private --------------------------------------------------
      

    def _generate_panels_right (self) -> list [VLM_Panel]:
        """ generate all panels of right half wing """

        # Change to wing coordinates in [m]
        #
        #           | u
        #           V
        #       
        #           .---- y       -> right wing tip 
        #           |
        #           x

        from wing                   import Planform_Paneled
        planform : Planform_Paneled = self._planform 

        panels = []
        y_stations  = planform._get_x_stations ()  
        cn_stations = planform._cn_rel_stations ()  

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

        logger.debug (f"{self} created {len(panels)} panels")

        return panels 
        

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
        
        logger.debug (f"{self} build aerogrid")

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

        self.wing           = wing 
        self.vtas           = vtas                  # true air speed 

        self._opPoints      = {}                    # dict of operating points 

        logger.debug (f"{self} created")


    def __repr__(self) -> str:
        # overwrite to get a nice print string 
        return f"<{type(self).__name__} {self.vtas:.1f}m/s >"


    def opPoint_at (self, alpha: float) -> 'VLM_OpPoint':
        """ returns opPOint at alpha"""

        a = round (alpha, 1)                        #  ensure clean key for dict
        try:
            opPoint = self._opPoints[a]             # already exisiting 
        except:
            opPoint = VLM_OpPoint (self, a)         # calculate new opPOint 
            self._opPoints[a] = opPoint 
        return opPoint 




class VLM_OpPoint:

    """ 
    single operation point alpha of a VLM_Polar 
    """

    def __init__ (self, polar: VLM_Polar,  alpha : float ):

        self.alpha = alpha
        self.polar = polar 

        # alpha is applied via downwash of flow 
        downwash =  (np.pi * alpha / 180) * polar.vtas
        self.wj = np.ones(self.n_panels) * downwash / polar.vtas

        self._cp = None                             # pressure coefficient 
        self._L  = None                             # total Lift


    @property
    def wing (self) -> VLM_Wing:
        return self.polar.wing

    @property
    def Qjj (self) -> np.ndarray:
        return self.polar.wing.Qjj
    
    @property
    def N (self) -> np.ndarray:
        return self.polar.wing.N

    @property
    def A_ges (self) -> np.ndarray:
        return self.polar.wing.A_ges

    @property
    def A_panels (self) -> np.ndarray:
        return self.polar.wing.A_panels

    @property
    def q_dyn (self) -> float:
        """ dynamic pressure """
        return 1.225 / 2.0 * self.polar.vtas ** 2


    @property
    def n_panels (self) -> int:
        return self.polar.wing.n_panels

    @property
    def nx_panels (self) -> int:
        """ number of panels in x direction"""
        return self.polar.wing.nx_panels 


    @property
    def Cp_panels (self) -> np.ndarray:
        """ cp of panel"""
        if self._cp is None: 
            self._cp = self.Qjj.dot(self.wj)
        return self._cp

    @property
    def Fxyz_panels (self) -> np.ndarray:
        """ vorce vector of panel"""
        return self.q_dyn * self.N.T * self.A_panels * self.Cp_panels


    @property
    def L (self) -> float:
        """ Lift of wing"""
        if self._L is None: 
            self._L = self.Qjj.dot(self.wj)
        return self._cp


    def aero_results (self) -> tuple[np.ndarray, dict]:
        """ 
        op point results at y stations of panel stripes
        
        Returns: 
            y_pos:  y position of a panel stripe (middle) 
            vars:   dictionary with values per y_pos   
                    .CL
                    .LIFT
                    .ALPHA 
                    .APLHA_IND        
                    .APLHA_EFF        
        """

        nx  	 = self.nx_panels
        aerogrid = self.wing.aerogrid
        q_dyn    = self.q_dyn

        b_panels    : np.ndarray = aerogrid['b']                                # width 
        l_panels    : np.ndarray = aerogrid['l']                                # depth 
        y_panels    : np.ndarray = aerogrid['offset_l'] [:,1]                   # y_posiition of panels 
        lift_panels : np.ndarray = self.Fxyz_panels [2]

        lift  = []
        cl    = []
        y_pos = []
        alpha_eff = []
        alpha_ind = []

        for i in range (0, self.n_panels, nx):                      # step through panels stripewise 

            lift_stripe = lift_panels [i:(i+nx)].sum() 
            b_stripe    = b_panels [i]
            c_stripe    = l_panels [i:(i+nx)].sum() 

            lift_local  = lift_stripe / b_stripe 
            cl_local    = lift_local / (q_dyn * c_stripe)
            a_l0_local  = 0.0                                       # tbd 
            a_eff_local = (cl_local / (2 * np.pi) + a_l0_local) * 180 / np.pi
            a_ind_local = self.alpha - a_eff_local

            lift.append  (lift_local) 
            cl.append    (cl_local)
            y_pos.append (y_panels[i])
            alpha_eff.append (a_eff_local)
            alpha_ind.append (a_ind_local)

        vars = {}
        vars[OpPoint_Var.CL]        = np.array (cl) 
        vars[OpPoint_Var.LIFT]      = np.array (lift) 
        vars[OpPoint_Var.ALPHA_EFF] = np.array (alpha_eff)              
        vars[OpPoint_Var.ALPHA_IND] = np.array (alpha_ind)         
        vars[OpPoint_Var.ALPHA]     = np.full (len(y_pos), self.alpha)             
        return np.array(y_pos), vars 