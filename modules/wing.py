#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""  

    Wing model with planform, wing sections, airfoils 

    Wing                                - main class of data model 
        |-- WingSection                 - the various stations defined by user 
                |-- Airfoil             - the airfoil at a section
        |-- Planform                    - describes geometry, outline of the wing  
        |     (ellipsoid, trapezoid, straightTE, DXF)
        |-- Flaps                       - flaps handler, creates list of Flap 
                |-- Flap                - single flap - dynamically created based on flap group
        |
        |-- refPlanform                 - a ellipsoid reference planform 
        |-- refPlanform_dxf             - a DXF based reference planform 
        |-- xflr5Exporter               - handles export to Xflr5
                |--Planform_Paneled     - Planform which is paneled in x,y direction 
        |-- flzExporter                 - handles export to FLZ_vortex
                |--Planform_Paneled     - Planform which is paneled in x,y direction 
"""

import os
import numpy as np
import bisect
import sys
import copy
from typing                 import override
from pathlib                import Path


# let python find the other modules in the dir of self  
sys.path.append(Path(__file__).parent)
from base.common_utils      import * 
from base.math_util         import * 
from base.spline            import Bezier
from model.airfoil          import Airfoil, GEO_BASIC, GEO_SPLINE
from model.airfoil_examples import Root_Example, Tip_Example

import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

# ---- Typing -------------------------------------

type Array      = list[float]
type Polyline   = tuple[Array, Array]
type Polylines  = tuple[Array, Array, Array]


# ---- Model --------------------------------------
class Wing:
    """ 

    Main object - holds the model 

    """
    unit = 'mm'

    def __init__(self, parm_filePath):
        """
        Init wing from parameters in parm_filePath
        """

        dataDict = Parameters (parm_filePath).get_dataDict()
        if not dataDict:
            logger.info ('No input data - a default wing will be created')
        else: 
            parm_version = fromDict (dataDict, "pc2_version", 1)
            logger.info (f"Reading wing parameters from '{parm_filePath}' - parameter version {parm_version}")

            if parm_version == 1:
                dataDict = self._convert_parm_file_v2 (dataDict)

        # handler for the realtive path to the paramter file (working directory)
        self.pathHandler = PathHandler (onFile=parm_filePath)

        self.dataDict = dataDict

        self._name            = fromDict (dataDict, "wing_name", "My new Wing")
        self._fuselage_width  = fromDict (dataDict, "fuselage_width", 80.0)

        # attach the Planform 

        self._planform      = Planform (self, dataDict)

        #  reference planforms   

        self._planform_elliptical   = None
        self._planform_ref_pc2      = None
        self._planform_ref_pc2_file = None

        self._planform_panelled     = None

        # will hold the class which manages Xflr5, FLZ export including its parameters

        self._exporterXflr5         = None 
        self._exporterFlz           = None 
        self._exporterDxf           = None 
        self._export_airfoils       = None 

        # miscellaneous parms
        # self._rootRe            = fromDict (dataDict, "rootRe", 400000)
        # self._airfoilNickPrefix = fromDict (dataDict, "airfoilNickPrefix", "JX")
        
        logger.info (str(self)  + ' created')


    def __repr__(self) -> str:
        # overwrite to get a nice print string 
        return f"{type(self).__name__} \'{self.name}\'"



    def _save (self) -> dict:
        """ returns the parameters of self as a dict"""

        dataDict = {}

        toDict (dataDict, "pc2_version", 2)
        toDict (dataDict, "wing_name",          self._name) 
        toDict (dataDict, "fuselage_width",     self._fuselage_width) 

        self._planform._save_to (dataDict)
        # if self._exporterXflr5: 
        #     toDict (dataDict, "xflr5",              self.exporterXflr5._save()) 
        # if self._exporterFlz: 
        #     toDict (dataDict, "flz",                self.exporterFlz._save()) 
        # if self._exporterDxf: 
        #     toDict (dataDict, "dxf",                self.exporterDxf._save()) 
        # if self._exporterAirfoils: 
        #     toDict (dataDict, "airfoils",           self.exporterAirfoils._save()) 

        return dataDict


    def _convert_parm_file_v2 (self, dataDict :dict) -> dict:
        """ convert paramter file from version 1 to version 2"""

        logger.info (f"Converting parameters to version 2.0")

        dict_v2 = {} # copy.deepcopy (dataDict)  

        # wing 

        toDict (dict_v2, "pc2_version",     2)
        toDict (dict_v2, "wing_name",       fromDict (dataDict, "wingName", None))

        halfspan = fromDict (dataDict, "wingspan", 2000) / 2.0
        toDict (dict_v2, "halfspan",        halfspan)

        toDict (dict_v2, "chord_root",      fromDict (dataDict, "rootchord", None))
        toDict (dict_v2, "sweep_angle",     fromDict (dataDict, "hingeLineAngle", None))
        toDict (dict_v2, "fuselage_width",  0.0)

        # planform

        chord_tip =                         fromDict (dataDict, "tipchord", None)
        chord_root =                        fromDict (dataDict, "rootchord", None)
        if chord_tip and chord_root:
            cn_tip = chord_tip / chord_root
        else: 
            cn_tip = 0.25

        chord_style = fromDict (dataDict, "planformType", N_Chord_Bezier.name)

        toDict (dict_v2, "chord_style",   chord_style)

        if chord_style == 'Bezier' or chord_style == 'Bezier TE straight':
            toDict (dict_v2, "p1y",       fromDict (dataDict, "p1x", None))
            toDict (dict_v2, "p1x",       fromDict (dataDict, "p1y", None))
            toDict (dict_v2, "p2y",       fromDict (dataDict, "p2x", None))
            toDict (dict_v2, "p3y",       cn_tip)


        # reference line 

        refDict = {}

        if chord_style == 'Bezier TE straight':
            # legacy planform 
            toDict (dict_v2, "chord_style",   N_Chord_Bezier.name)
            toDict (refDict, "xn_root", 1.0)
            toDict (refDict, "xn_tip", 1.0)

        else:
            # flap hinge line -> reference line 
            f = fromDict (dataDict, "flapDepthRoot", None)
            if f:   toDict (refDict, "xn_root", (100 - f)/100)
            f = fromDict (dataDict, "flapDepthTip", None)
            if f:   toDict (refDict, "xn_tip", (100 - f)/100)


        if chord_style == 'Bezier':
            toDict (refDict, "banana_p1y",fromDict (dataDict, "banana_p1x", None))
            toDict (refDict, "banana_p1x",fromDict (dataDict, "banana_p1y", None))

        if refDict:
            toDict (dict_v2, "chord_reference", refDict) 

        # wing sections 

        sectionsList = fromDict (dataDict, "wingSections", None)
        new_sectionsList = []
        if sectionsList: 
            for i, sectionDict in enumerate (sectionsList):
                new_sectionDict = {}
                position = fromDict (sectionDict, "position", None)
                if position is not None:
                    xn = position / halfspan
                else: 
                    xn = None 
                toDict (new_sectionDict, "xn", xn)
                toDict (new_sectionDict, "cn",                  fromDict (sectionDict, "norm_chord", None))
                toDict (new_sectionDict, "flap_group",          fromDict (sectionDict, "flapGroup", None))
                eitherPosOrChord =                              fromDict (sectionDict, "eitherPosOrChord", None)
                defines_cn = None
                if eitherPosOrChord == True:
                    defines_cn = False
                elif eitherPosOrChord == False:
                    defines_cn = True
                toDict (new_sectionDict, "defines_cn", defines_cn)
                toDict (new_sectionDict, "airfoil",             fromDict (sectionDict, "airfoil", None))

                # flap hinge line 
                if i == 0:
                    f = fromDict (dataDict, "flapDepthRoot", None)
                    if f:   toDict (new_sectionDict, "hinge_cn", (100 - f)/100)

                if chord_style == 'Bezier TE straight':
                    if i == len (sectionsList) - 2:
                        # special case Amokka - take the second last section for hinge definition 
                        f = fromDict (dataDict, "flapDepthTip", None)
                        toDict (new_sectionDict, "hinge_cn", (100 - f)/100) 
                else: 
                    if i == len (sectionsList) - 1:
                        f = fromDict (dataDict, "flapDepthTip", None)
                        if f:   toDict (new_sectionDict, "hinge_cn", (100 - f)/100)                

                new_sectionsList.append (new_sectionDict)

        if new_sectionsList:
            toDict (dict_v2, "wingSections", new_sectionsList) 

        # hinge line 

        if chord_style == 'Bezier TE straight':
            toDict (dict_v2, "hinge_equal_ref_line", False)
        else:
            toDict (dict_v2, "hinge_equal_ref_line", True)

        return dict_v2


    # ---Properties --------------------- 

    @property
    def name(self):  return self._name
    def set_name(self, newName):  self._name = newName


    @property 
    def planform (self) -> 'Planform': 
        return self._planform
    

    @property
    def wingspan(self):
        """ wingspan without fuselage""" 
        return self.planform.span * 2

    @property
    def wingspan_total (self):
        """ wingspan *with* fuselage""" 
        return self.wingspan + self.fuselage_width


    @property
    def chord_root(self): return self.planform.chord_root

    @property
    def chord_tip(self): return self._chord_tip
    def set_chord_tip(self, newChord):
        """ set chord_tip - update tip (last) section with new chord  """
        if (newChord > 1.0):
            self._chord_tip = newChord
            self.wingSections.adjust_to_wing()
            #self.planform.refresh()             #todo e.g. update Bezier curve        


    @property
    def fuselage_width (self) -> float:
        """ width of fuselage"""
        return self._fuselage_width
    
    def set_fuselage_with (self, aVal:float):
        aVal = max(0, aVal)
        aVal = min(self.wingspan/2, aVal)
        self._fuselage_with = aVal 


    @property
    def planform_panelled (self) -> 'N_Planform':
        """ 
        shadow planform for self.planform which represents the panelled planform 
        which is the base for Xflr5 or FLZ export"""

        if self._planform_panelled is None: 
            
            self._planform_panelled = N_Planform (self, dataDict = self.dataDict, 
                                                       chord = N_Chord_Panelled (),
                                                       chord_ref = self.planform.norm.chord_ref )
        return self._planform_panelled


    @property
    def planform_elliptical (self) -> 'N_Planform':
        """ an elliptical reference norm planform having same span and chord reference """

        if self._planform_elliptical is None: 
            
            self._planform_elliptical = N_Planform (self, dataDict = self.dataDict, 
                                                       chord = N_Chord_Elliptical (),
                                                       chord_ref = self.planform.norm.chord_ref )
        return self._planform_elliptical
    

    @property
    def planform_ref_pc2_file (self) -> str:
        """ filename of optional PC2 reference planform"""
        return self._planform_ref_pc2_file

    def set_planform_ref_pc2_file (self, pathFilename : str) -> str:
        self._planform_ref_pc2_file = pathFilename


    @property
    def planform_ref_pc2_name (self) -> str:
        """Name of optional PC2 reference planform"""
        return self.planform_ref_pc2.wing.name if self.planform_ref_pc2 else None

    @property
    def planform_ref_pc2 (self) -> 'Planform':
        """ optional PC2 reference planform"""

        if self._planform_ref_pc2 is None and self._planform_ref_pc2_file: 
            
            self._planform_ref_pc2 = Wing (self._planform_ref_pc2_file).planform 

        return self._planform_ref_pc2
    


    @property
    def halfwingspan (self):    return (self.wingspan / 2)

    @property
    def rootRe(self): return self._rootRe
    def set_rootRe(self, newRe): self._rootRe = newRe

    @property
    def airfoilNickPrefix(self): return self._airfoilNickPrefix
    def set_airfoilNickPrefix(self, newStr): self._airfoilNickPrefix = newStr


    @property
    def exporterXflr5 (self) : 
        """ returns class managing Xflr5 export """
        from export_Xflr5       import Export_Xflr5         # here - otherwise circular errors

        if self._exporterXflr5 is None:                     # init exporter with parameters in sub dictionary
            xflr5Dict           = fromDict (self.dataDict, "xlfr5", "")
            self._exporterXflr5 = Export_Xflr5(self, xflr5Dict) 
        return self._exporterXflr5     

    @property
    def exporterDxf (self): 
        """ returns class managing Dxf export """
        from export_Dxf       import Export_Dxf         # here - otherwise circular errors

        if self._exporterDxf is None:                   # init exporter with parameters in sub dictionary       
            dxfDict           = fromDict (self.dataDict, "dxf", "")
            self._exporterDxf = Export_Dxf(self, dxfDict) 
        return self._exporterDxf     


    @property
    def exporterFlz (self): 
        """ returns class managing FLZ export """
        from export_FLZ  import Export_FLZ              # here - otherwise circular errors

        if self._exporterFlz is None:                   # init exporter with parameters in sub dictionary
            flzDict           = fromDict (self.dataDict, "flz", "")
            self._exporterFlz = Export_FLZ(self, flzDict) 
        return self._exporterFlz     


    @property
    def export_airfoils (self): 
        """ returns export handler for airfoils"""

        from export_airfoils  import Export_Airfoils             # here - otherwise circular errors

        if self._export_airfoils is None:                      # init exporter with parameters in sub dictionary
            airfoilsDict      = fromDict (self.dataDict, "airfoils", "")
            self._export_airfoils = Export_Airfoils(self, airfoilsDict) 
        return self._export_airfoils     


    @property
    def workingDir(self): 
        """returns the current working directory which is the dir of the paramter file"""
        return self.pathHandler.workingDir


    # ---Methods --------------------- 


    def save (self, pathFileName):
        """ store data dict to file pathFileName        
        :Returns: 
            True : if succeded, False if failed"""

        currentDict = self._save()

        saveOk = Parameters (pathFileName).write_dataDict(currentDict)

        if saveOk:
            # keep dataDict for later change detection 
            self.dataDict = currentDict  
            # set the current working Dir to the dir of the new saved parameter file            
            self.pathHandler.set_workingDirFromFile (pathFileName)
        return saveOk


    def hasChanged (self):
        """returns true if the parameters has been changed since last save() of parameters"""

        return self._save() != self.dataDict
  
        



    def t_norm_to_wing_right (self, xn : float|Array|list, yn : float|Array|list) -> ...:
        """
        Transforms normalized coordinates into wing coordinates as right side wing 
            - apply fuselage 
        Args:
            xn,yn: normalized, either float, list or Array to transform 
        Returns:
            x,y: transformed wing coordinates as float or np.array
        """

        # transform from norm to planform
        x,y = self._planform.t_norm_to_plan (xn, yn)

        # move for half fuselage
        x = x + self.fuselage_width / 2

        return x, y 


    def t_norm_to_wing_left (self, xn : float|Array|list, yn : float|Array|list) -> ...:
        """
        Transforms normalized coordinates into wing coordinates as left side wing 
            - apply fuselage 
            - mirror around y-axis 
        Args:
            xn,yn: normalized, either float, list or Array to transform 
        Returns:
            x,y: transformed wing coordinates as float or np.array
        """

        # transform from norm to planform
        x,y = self._planform.t_norm_to_plan (xn, yn)

        # move for half fuselage
        x = x + self.fuselage_width / 2

        # mirror half wing 
        x = -x  

        return x, y 



#-------------------------------------------------------------------------------
# Normalized Chord Reference 
#-------------------------------------------------------------------------------


class N_Chord_Reference: 
    """ 

    The chord reference describes how much of the chord is given to Leading and Trailing edge.

    A chord reference function - returns cr at xn.
    e.g. cr = 0.8 means 80% of chord is towards le and 20% towards te from a horizontal reference 
 
    The Reference function is defined by a quadratic Bezier which allows the planform
     to be bended from root to tip


    Wing
        |-- Planform
            |-- Norm_Planform
                |-- Norm_Chord
                |-- Norm_Chord_Reference
                |-- Norm_WingSections
                |-- Norm_Flaps
    """

    def __init__(self, dataDict: dict = None):


        # chord reference  - init linear Bezier control points for straight line    
       
        px = [0.0,  1.0]
        py = [0.75, 0.75]                                       

        # alternate v 1 bezier parameters from dict  - handle banana

        xn_root   = fromDict (dataDict, "xn_root", None)  

        if xn_root is not None:                            # set no banana
            banana_p1y = fromDict (dataDict, "banana_p1y", None)  
            py[0]   = fromDict (dataDict, "xn_root", py[0])  
            py[1]   = fromDict (dataDict, "xn_tip", py[-1])   
            if banana_p1y: 
                banana_p1x = fromDict (dataDict, "banana_p1x", 0.0)
                py_1_no_banana = interpolate (px[0], px[1], py[0], py[1], banana_p1x)
                py_1_banana    = py_1_no_banana + banana_p1y
                px.insert[1, banana_p1x]
                py.insert[1, py_1_banana]

        # regular bezier parameters from dict  

        else: 
            px   = fromDict (dataDict, "px", px)                  
            py   = fromDict (dataDict, "py", py)                 


        # create Bezier for chord reference function

        self._cr_bezier  : Bezier = Bezier (px, py)
        self._cr_bezier_u = np.linspace(0.0, 1.0, num=20)                     # default Bezier u parameter (for polyline)

    def _as_dict (self) -> dict:
        """ returns a data dict with the paramters of self"""

        d = {}
        toDict (d, "px",        self._cr_bezier.points_x)
        toDict (d, "py",        self._cr_bezier.points_y)
        return d




    def at (self, xn: float, fast=True) -> float:
        """ 
        Chord reference function - returns cr at xn
            e.g. cr = 0.8 means 80% of chord is towards le and 20% towards te
                from a horizontal reference 
            Higher Precision is achieved with interpolation of the curve (fast=False) 
        """

        return self._cr_bezier.eval_y_on_x (xn, fast=fast) 


    def is_no_banana (self) -> bool:
        """ 
        True if chord reference is linear meaning 
        no banana function (Bezier is "obsolete") 
        """ 
        px = self._cr_bezier.points_x
        py = self._cr_bezier.points_y

        if len(px) == 2:
            return True
        else:

            # check if the middle bezier point is on line between the two others
            py_1_interpol = interpolate (px[0], px[2], py[0], py[2], px[1])
            delta_y = abs (py_1_interpol - py[1])
            return  round (delta_y,2) == 0.0 


    def set_is_no_banana (self):
        """ 
        set the reference function to linear meaning 
        no banana function (Bezier is n=2) 
        """ 
        px = self._cr_bezier.points_x
        py = self._cr_bezier.points_y

        if len(px) == 3:

            # remove control point in the middle 
            del px [1]
            del py [1]

            self._cr_bezier.set_points (px, py)


    def polyline (self) -> tuple [Array, Array]:
        """
        chord reference as polyline of cr 
        """
        xn, cr = self._cr_bezier.eval(self._cr_bezier_u) 
        return np.array(xn), np.array(cr) 


    def bezier_as_jpoints (self)  -> list[JPoint]:
        """ 
        chord reference Bezier control points as JPoints with limits and fixed property
            - in normed coordinates 
        """
        jpoints = []
        n = len (self._cr_bezier.points)

        for i, point in enumerate(self._cr_bezier.points):

            jpoint = JPoint (point)           

            if i == 0:                                      
                jpoint.set_x_limits ((0,0))
                jpoint.set_y_limits ((0,1))
                jpoint.set_name ('Root')
            elif i == (n-1):                                    
                jpoint.set_x_limits ((1,1))
                jpoint.set_y_limits ((0,1))
                jpoint.set_name ('Tip')
            elif i == 1:                                     
                jpoint.set_x_limits ((0,1))
                jpoint.set_y_limits ((-1, 1))
                jpoint.set_name ("Banana")
            jpoints.append(jpoint)

        return jpoints


    def bezier_from_jpoints (self, jpoints : list[JPoint]): 
        """ 
        set chord referenc eBezier control points from JPoints which  
        """

        px, py = [], []
        for jpoint in jpoints:
            px.append(jpoint.x )
            py.append(jpoint.y)

        # check if banana point was moved 
        bez_px1 = round (self._cr_bezier.points_x[1], 3)               # avoid numerical issues 
        bez_py1 = round (self._cr_bezier.points_y[1], 3)
        set_no_banana = False
        if self.is_no_banana ():
            if bez_px1 == round(px[1],3) and bez_py1 == round(py[1],3):
                set_no_banana = True 

        # update bezier 
        self._cr_bezier.set_points (px, py)

        # remain in no_banana if banana point wasn't moved
        if set_no_banana:
            self.set_is_no_banana()



#-------------------------------------------------------------------------------
# Normalized chord    
#-------------------------------------------------------------------------------


class N_Chord_Abstract: 
    """ 
    Abstract super class - defines the normalized chord distribition  

    Wing
        |-- Planform
            |-- Norm_Planform
                |-- Norm_Chord
    """

    name            = "Abstract"
    isBezier        = False
    isTrapezoidal   = False
    isElliptical    = False

    chord_defined_by_sections = False                               # e.g trapezoid


    def __init__(self, dataDict: dict = None):
        """main constructor

        Args:       
            dataDict: the initial dataDict for self 
        """

        logger.info (str(self)  + ' created')


    def __repr__(self) -> str:
        return f"<{type(self).__name__}>"


    def transform_norm (self, xn : float|Array|list, yn : float|Array|list) -> ...:
        """ 
        Transforms normalized coordinates into self coordinates
            --> do nothing - dummy for interface
        """
        return xn, yn


    def at (self, xn: float) -> float:
        """ main chord function - to be overriden

        Args:
            xn: normalized x position
        Returns:
            cn: normalized chord
        """
        raise NotImplementedError


    def xn_at (self, cn: float) -> float:
        """ 
        returns xn at normed chord xn - to be overriden
        """
        raise NotImplementedError


    @property
    def at_tip (self, ) -> float:
        """ returns normalized chord at tip """
        self.at (1.0)


    def polyline (self) -> Polyline:
        """ 
        Normalized polyline of chord along xn
            At root it is: cn [0] = 1.0 and cnn[0] = 0.0

        Returns:
            xn: normalized x coordinates
            cn: normalized chord
        """
        raise NotImplementedError



class N_Chord_Bezier (N_Chord_Abstract): 
    """ 
    Bezier based normalized chord distribition  
    """

    name            = "Bezier"
    isBezier        = True
    isTemplate      = True

    description = "Chord based on a Bezier curve function,\n" + \
                  "which is defined by its root and tip tangent"

    # is the chord distribution defined by wing section or vice versa - overwrite 
    chord_defined_by_sections = False          # e.g trapezoid


    def __init__(self, dataDict: dict = None):
        super().__init__()
        """
        Main constructor

        Args:       
            myWing: parent self belongs to 
            dataDict: data dict having paramters for self . Defaults to None.
        """

        # init Cubic Bezier for chord distribution 
       
        px = [0.0, 0.55, 1.0,  1.0]
        py = [1.0, 1.0, 0.55, 0.25]     

        # from dict only the variable point coordinates of Bezier

        px[1]   = fromDict (dataDict, "p1x", px[1])             # root tangent 
        py[1]   = fromDict (dataDict, "p1y", py[1])
        py[2]   = fromDict (dataDict, "p2y", py[2])             # nearly elliptic
        py[3]   = fromDict (dataDict, "p3y", py[3])             # defines tip chord 

        self._bezier = Bezier (px, py)
        self._u = np.linspace(0.0, 1.0, num=100)                # default Bezier u parameter


    def at (self, xn: float, fast=True) -> float:
        """ 
        Main chord function - returns cn at xn
            Normally a linear interpolation is done for fast evaulation (fast=True). 
            Higher Precision is achieved with interpolation of the curve (fast=False) 
        """

        return round (self._bezier.eval_y_on_x (xn, fast=fast),10) 


    def xn_at (self, cn: float, fast=True) -> float:
        """ 
        returns xn at normed chord cn
            Normally a linear interpolation is done for fast evaulation (fast=True). 
            Higher Precision is achieved with interpolation of the curve (fast=False) 
        """

        return round (self._bezier.eval_x_on_y (cn, fast=fast), 10)


    def polyline (self) -> Polyline:
        """ 
        Normalized polyline of chord along xn
            At root it is: cn [0] = 1.0  

        Returns:
            xn: normalized x coordinates
            cn: normalized chord
        """
        xn, cn = self._bezier.eval(self._u) 
        return np.round(xn,10), np.round(cn,10) 


    def bezier_as_jpoints (self, transform_fn = None) -> list[JPoint]: 
        """ 
        Bezier control points as JPoints with limits and fixed property
            - in normed coordinates x..1, y..1
            - transform_fn will transform jpoint to new coordinates 
        """

        jpoints = []

        for i, point in enumerate(self._bezier.points):

            jpoint = JPoint (point)   

            if i == 0:                                      # root 
                jpoint.set_fixed (True)
                jpoint.set_name ('') 
            elif i == 1:                                    # root tangent
                jpoint.set_x_limits ((0,1))
                jpoint.set_y_limits ((0,1))
                jpoint.set_name ('Root Tangent') 
            elif i == 2:                                    # tip tangent
                jpoint.set_x_limits ((1,1))
                jpoint.set_y_limits ((0.2,1))
                jpoint.set_name ('Tip Tangent') 
            else:
                # jpoint.set_fixed (True)                   # tip
                jpoint.set_x_limits ((1,1))
                jpoint.set_y_limits ((0.01,0.5))
                jpoint.set_name ('Tip Chord') 

            jpoints.append(jpoint.as_transformed (transform_fn))

        return jpoints


    def bezier_from_jpoints (self, jpoints : list[JPoint], transform_fn = None): 
        """ 
        set Bezier control points from JPoints
            - transform_fn will transform jpoint coordinates prior to setting Bezier
        """

        px, py = [], []
        for jpoint in jpoints:
            jpoint_trans = jpoint.as_transformed (transform_fn)
            px.append(jpoint_trans.x)
            py.append(jpoint_trans.y)

        self._bezier.set_points (px, py)




class N_Chord_Trapezoid (N_Chord_Abstract):
    """ 
    Trapezoidal chord based on wing section 
    """

    name            = "Trapezoid"
    isTrapezoidal   = True
    style           = "trapezoidal"

    description     = "Chord defined by its wing sections,\n" + \
                      "which have the attribut 'Defines planform'" 

    # is the chord defined by wing section or vice versa - overwrite 
    chord_defined_by_sections = True          # e.g trapezoid


    def __init__(self, planform : 'N_Planform', dataDict: dict = None):
        """
        Create new chord distribution 

        Args:       
            planform: parent self belongs to - Trapezoidal need the wing sections
            dataDict: dictionary with parameters for self 
        """

        self._planform = planform 

        super().__init__ (dataDict=dataDict)


    def polyline (self) -> Polyline:
        """ 
        Normalized polyline of chord along xn
            At root it is: cn [0] = 1.0 

        Returns:
            xn: normalized x coordinates
            cn: normalized chord
        """

        # get the sections which define the chord 

        sections =[]
        section : N_WingSection
        for section in self._planform.wingSections:
            if section.has_fix_pos_and_chord:
                sections.append(section)

        # collect their position and length 
        #        
        xn, cn = [], []
        for section in sections:
            xn.append(section.xn())
            cn.append(section.cn())
        return np.round(xn,10), np.round(cn,10) 


    def at (self, xn: float, fast=True) -> float:
        """ 
        Main chord function - returns cn at xn
        """

        xn_arr, cn_arr = self.polyline()
        cn = np.interp(xn, xn_arr, cn_arr)                      # linear interpolation in polyline

        return round (cn,10) 


    def xn_at (self, cn: float, fast=True) -> float:
        """ 
        returns xn at normed chord cn
        """
        xn_arr, cn_arr = self.polyline()

        # as cn is decreasing along xn the array must be reversed to use np.interp
        xn_arr = np.flip (xn_arr)
        cn_arr = np.flip (cn_arr)

        xn = np.interp(cn, cn_arr, xn_arr)                      # linear interpolation in polyline

        return round (xn,10) 



    # def adjust_planform_to_reference (self):
    #     """
    #     Adjust the relevant wing sections of trapezoid to become close to reference(ellipse)
    #     """
         
    #     y_list, _ = self.wing.wingSections.yn_cn_having_fixed()

    #     for index, yPos in enumerate(y_list):        # skip root and tip 
    #         if not (index == 0 or index == (len(y_list)-1)): 

    #             section = self.wing.wingSections.at_y (yPos)

    #             if section: 
    #                 # go a little left to get chord so ellipsoid is better approx.
    #                 leftPos = y_list [index-1]
    #                 refPos = leftPos + 0.95 *  (yPos - leftPos)
    #                 newChord = self.wing.refPlanform_elli._norm_chord_at(refPos)
    #                 section.set_norm_chord (newChord)
    #             else: 
    #                 raise ValueError ("Section at norm position %f not found" % yPos)




class N_Chord_Panelled (N_Chord_Abstract):
    """ 
    Trapezoidal chord distribution based on wing sections.

    All existing wing sections are taken to approximate the outline with straight line.
    This is the base for panelling a planform  
    """

    name            = "Panelled"
    isPanelled      = True
    style           = "trapezoidal"

    # is the chord defined by wing section or vice versa - overwrite 
    chord_defined_by_sections = True          # e.g trapezoid


    def __init__(self, planform : 'N_Planform', dataDict: dict = None):
        """
        Create new chord distribution 

        Args:       
            planform: parent self belongs to - Trapezoidal need the wing sections
            dataDict: dictionary with parameters for self 
        """

        self._planform = planform 

        super().__init__ (dataDict=dataDict)


    def polyline (self) -> Polyline:
        """ 
        Normalized polyline of chord along xn
            At root it is: cn [0] = 1.0 

        Returns:
            xn: normalized x coordinates
            cn: normalized chord
        """

        sections = self._planform.wingSections
        section : N_WingSection

        xn, cn = [], []
        for section in sections:
            xn.append(section.xn())
            cn.append(section.cn())
        return np.round(xn,10), np.round(cn,10) 


    def at (self, xn: float, fast=True) -> float:
        """ 
        Main chord function - returns cn at xn
        """

        xn_arr, cn_arr = self.polyline()
        cn = np.interp(xn, xn_arr, cn_arr)                      # linear interpolation in polyline

        return round (cn,10) 


    def xn_at (self, cn: float, fast=True) -> float:
        """ 
        returns xn at normed chord cn
        """
        xn_arr, cn_arr = self.polyline()

        # as cn is decreasing along xn the array must be reversed to use np.interp
        xn_arr = np.flip (xn_arr)
        cn_arr = np.flip (cn_arr)

        xn = np.interp(cn, cn_arr, xn_arr)                      # linear interpolation in polyline

        return round (xn,10) 



class N_Chord_Elliptical (N_Chord_Abstract):
    """ 
    Chord based on a pure elliptical function, which can be used as a reference  
        As x,y are normed to 1 the function represents a circle 
    """

    name            = "Elliptical"
    description     = "Chord being pure elliptical"

    isElliptical    = True 


    def at (self, xn: float, fast=True) -> float:
        """ 
        Main chord function - returns cn at xn
        """

        cn = np.sqrt(1.0 - (xn ** 2))                       # Pythagoras
        return round (cn,10) 


    def xn_at (self, cn: float, fast=True) -> float:
        """ 
        returns xn at normed chord cn
        """
        xn = np.sqrt(1.0 - (cn ** 2))                       # Pythagoras
        return round (xn,10) 


    def polyline (self) -> Polyline:
        """ 
        Normalized polyline of ellipticalchord along xn
            At root it is: cn [0]  = 1.0  
            At tip  it is: cn [-1] = 0.0  

        Returns:
            xn: normalized x coordinates
            cn: normalized chord
        """
        alphas = np.linspace (0, 1, 50)
        alphas = alphas * alphas * np.pi /2                     # achieve smaller angles at tip by **2 
        
        xn = np.cos (alphas)
        cn = np.sin (alphas)

        return np.round(xn,10), np.round(cn,10) 



#-------------------------------------------------------------------------------
# Normalized WingSection 
#-------------------------------------------------------------------------------


class N_WingSection : 
    """ 
    WingSection in normalized planform

    A wingSection has (normally) either 
        - a fixed position xn (like root or tip section) 
        - a fixed chord cn    
    """


    def __init__(self, planform : 'N_Planform', dataDict: dict = None):
        super().__init__()
        """
        Main constructor

        Args:       
            dataDict: data dict having paramters for self . Defaults to None.
        """

        self._planform      = planform

        self._xn            = fromDict (dataDict, "xn", None)            # xn position
        self._cn            = fromDict (dataDict, "cn", None)            # cn chord  
        self._defines_cn    = fromDict (dataDict, "defines_cn", False)    # section must have cn and xn 
        self._hinge_cn      = fromDict (dataDict, "hinge_cn", None)      # hinge chord position 
        self._flap_group    = fromDict (dataDict, "flap_group", 1)       # flap group (starting here) 

        # sanity

        if self._xn is None and self._cn is None:
            raise ValueError (f"{self} init - either xn or cn is missing")

        if self._xn is not None: 
            self._xn = np.clip (self._xn, 0.0, 1.0)
            self._xn = round (self._xn, 10)
        if self._cn is not None: 
            self._cn = np.clip (self._cn, 0.01, 0.99)
            self._cn = round (self._cn, 10)

        # sanity root and tip  
        # ... trapezoid planform: section may have both which will define planform  

        if self._planform._norm_chord.chord_defined_by_sections:
            if self._xn == 0.0 or self._cn == 1.0:
                self._xn = 0.0
                self._cn = 1.0
                self._defines_cn = True
            elif self._xn == 1.0:
                if self._cn is None:
                    self._cn = 0.25                                         # take default 
                self._defines_cn = True
            else:
                self._defines_cn = self._xn is not None and self._cn is not None
        else: 
            self._defines_cn = False    

        # create airfoil and load coordinates if exist 

        self._init_airfoil (dataDict = fromDict (dataDict, "airfoil", None))


    def __repr__(self) -> str:

        if (self._xn != None):
            info = f"xn={self._xn}"
        elif (self._cn != None):
            info = f"cn={self._cn:.2f}" 
        else:
            info = ''
        return f"<{type(self).__name__} {info}>"


    def _as_dict (self) -> dict:
        """ returns a data dict with the paramters of self"""

        d = {}
        toDict (d, "xn",            self._xn)
        toDict (d, "cn",            self._cn)
        toDict (d, "defines_cn",    self._defines_cn if self._defines_cn else None)
        toDict (d, "hinge_cn",      self._hinge_cn)
        toDict (d, "flap_group",    self.flap_group)
        return d


    @property
    def workingDir (self) -> str:
       """ current working directory""" 
       self._planform.workingDir 


    def _init_airfoil (self, dataDict = None, pathFileName = None, workingDir=None):
        """read and create airfoil for section """
 
        # read data for airfoil either from dict of parameter file 
        #  or get filepath from user selection 
        # then create new 'Airfoil' 

        try: 
            if dataDict: 
                airfoil = Airfoil.onDict (dataDict, geometry=GEO_BASIC,
                                          workingDir= self.workingDir)
            else: 
                airfoil = Airfoil (pathFileName= pathFileName, geometry=GEO_BASIC,
                                   workingDir=workingDir)
        except:
            airfoil = None

        if airfoil is not None and airfoil.isExisting:
            airfoil.load()
        else:
            if self.isRoot: 
                airfoil = Root_Example()
            elif self.isTip:
                airfoil = Tip_Example()
            else:
                airfoil = Airfoil(name="<strak>", geometry=GEO_BASIC)
                airfoil.set_isBlendAirfoil (True)

        self.airfoil : Airfoil = airfoil 



    @property
    def isRoot (self) -> bool:
        """ True if self is root section"""
        return self._xn == 0.0 

    @property
    def isTip (self) -> bool:
        """ True if self is tip section"""
        return self._xn == 1.0 


    def xn (self) -> float:
        """ 
        xn position of self. If self is based on cn, the position will be evaluated from the planform
        """
        if self._xn is None:
            return self._planform.xn_at (self.cn(), fast=False)
        else: 
            return self._xn
        
    def set_xn (self, aVal):
        """ set new position - will switch self defined 'by pos' """

        if self.isRoot or self.isTip: 
            return
        if aVal is None:
            self._xn = None
        else:  
            aVal = np.clip (aVal, 0.0, 1.0)
            self._xn = round(aVal,10)

        if not self.defines_cn and self._xn is not None:               # reset cn 
            self._cn = None 
        else:                                                       # tapezoid must have both
            if self._xn is None: self._xn = round(self.xn(),10)           
            if self._cn is None: self._cn = round(self.cn(),10) 


    def cn (self) -> float:
        """ 
        chord cn of self. If self is based on xn, the chord will be evaluated from the planform
        """
        if self._cn is None:
            return self._planform.cn_at (self.xn())
        else: 
            return self._cn

    def set_cn (self, aVal):
        """ set new chord - will switch self defined 'by chord' """

        if self.isRoot: 
            return
        if aVal is None:
            self._cn = None
        else:  
            aVal = np.clip (aVal, 0.0, 1.0)
            self._cn = round(aVal,10)

        if not self.defines_cn: 
            if self._cn is not None:                                # reset cn 
                self._xn = None 
        else:                                                       # tapezoid must have both
            if self._xn is None: self._xn = round(self.xn(),10)           
            if self._cn is None: self._cn = round(self.cn(),10) 

    @property
    def has_fix_chord (self) -> bool:
        """ True if chord cn of self is fixed """
        return self._cn is not None

    @property
    def has_fix_pos (self) -> bool:
        """ True if position xn of self is fixed """
        return self._xn is not None

    @property
    def has_fix_pos_and_chord (self) -> bool:
        """ True if position xn and chord cn of self is fixed """
        return self.has_fix_chord and self.has_fix_pos 

    @property 
    def defines_cn (self) -> bool:
        """ section defines chord (e.g. trapezoidal)"""
        return self._defines_cn
    
    def set_defines_cn (self, aBool : bool):
        """ set section defines chord (e.g. trapezoidal)"""

        if not (self.isRoot or self.isTip):

            # sanity - is it allowed in that planform
            if self._planform._norm_chord.chord_defined_by_sections:
                self._defines_cn = aBool
                if aBool: 
                    self.set_cn (self.cn())
                else: 
                    self.set_cn (None)       

        return True


    def index (self) -> int:
        """ index of self with wingSections"""
        return self._planform.wingSections.index (self)  


    def xn_limits (self) -> tuple:
         """ xn limits as tuple of self before touching the neighbour section """
         return self._planform.wingSections.limits_of (self) [0]

    def cn_limits (self) -> tuple:
         """ cn limits as tuple of self before touching the neighbour section """
         return self._planform.wingSections.limits_of (self) [1]


    @property
    def name_short (self) -> str:
        """ very short unique name for wing section like "7"""
        if self.isRoot:
            info = "Root"
        elif self.isTip:
            info = "Tip"
        else:
            info = f"{self.index()}"
        return info


    @property
    def hinge_cn (self) -> float:
        """ relative hinge chord position cn of self"""
        return self._hinge_cn

    def set_hinge_cn (self, aVal : float):
        """ set relative hinge chord position cn of self - or remove with None"""
        if aVal is not None: 
            aVal = np.clip (aVal, 0.0, 1.0)
        self._hinge_cn = aVal 

    def remove_hinge_cn (self):
        """ remove a individual relative hinge chord position cn of self"""
        self._hinge_cn = None

    @property
    def flap_group (self) -> float:
        """ flap group (starting) """
        return self._flap_group if self._flap_group is not None else 1

    def set_flap_group (self, aGroup : int):
        self._flap_group = aGroup 


    def le_te (self) -> tuple [float,float]:
        """ leading and trailing edge normed yn coordinate """
 
        le_yn, te_yn = self._planform.le_te_at (self.xn())
        return le_yn, te_yn


    def line (self) -> Polyline:
        """ self as a poyline x,y within normed planform"""

        xn           = self.xn()
        le_yn, te_yn = self.le_te ()
        return np.array([xn, xn]), np.array([le_yn, te_yn])


    def line_in_chord (self) -> Polyline:
        """ self as a poyline x,y within normed chord """

        xn          = self.xn()
        return np.array([xn, xn]), np.array([self.cn(), 0.0])


    def line_in_chord_ref (self) -> Polyline:
        """ self as a poyline x,y within chord reference which is just y[1]=1"""

        xn          = self.xn()
        return np.array([xn, xn]), np.array([0.0, 1.0])



class N_WingSections (list):
    """ 
    container (list) for norm wing sections of a planform
    
            |-- Norm_Planform
                |-- Norm_Chord
                |-- Norm_WingSections
                    |-- Norm_WingSection

    """

    def __init__ (self, planform: 'N_Planform', sectionsDict: dict = {}):
        super().__init__ ([])

        self._planform = planform

        # create all sections based on sections list in dataDict 
        sections : list[N_WingSection] = []
        for sectionDict in sectionsDict:
            sections.append(N_WingSection (planform, sectionDict))

        # new wing - create default sections
        if not sections:
            logger.info ('Creating example wing sections')
            sections.append(N_WingSection (planform, {"xn": 0.0, "flap_group":1, "hinge_cn":0.70}))
            sections.append(N_WingSection (planform, {"cn": 0.8, "flap_group":2, "hinge_cn":0.70}))
            sections.append(N_WingSection (planform, {"xn": 1.0, "flap_group":2, "hinge_cn":0.75}))

        # sanity
        if not sections[0].isRoot or not sections[-1].isTip:
            raise ValueError ("Wingsections data corrupted")

        logger.info (" %d Wing sections added" % len(sections))

        self.extend (sections)


    @property
    def workingDir (self) -> str:
       """ current working directory""" 
       self._planform.workingDir 
   

    def _as_list_of_dict (self) -> list[dict]:
        """ returns a data dict with the paramters of self"""

        section_list  = []
        section : N_WingSection
        for section in self: 
            section_list.append (section._as_dict ()) 
        return section_list


    def create_after (self, aSection: 'N_WingSection') -> 'N_WingSection' : 
        """
        creates and inserts a new wing section after aSection with a chord in the middle to the next neighbour 

        Return: 
            newSection: the new wingSection
        """

        if isinstance(aSection, WingSection) and not aSection.isTip :

            _, right_sec = self.neighbours_of (aSection)

            new_cn = (aSection.cn() + right_sec.cn()) / 2
            new_flap_group = aSection.flap_group 

            new_section = N_WingSection (self._planform, {"cn": new_cn, "flap_group":new_flap_group})
            self.insert (self.index(aSection) + 1, new_section)

        return new_section


    def create_at (self, xn : float)  -> 'N_WingSection': 
        """
        create and insert a new wing section at pos xn 

        Return: 
            newSection: the new wingSection, None if a new section couldn't be created 
        """

        xn = np.clip (xn, 0.0, 1.0)

        # is there already a section? Return this one 

        exist_sec = self.at_xn (xn)
        if exist_sec: return None 

        new_section = N_WingSection (self._planform, {"xn": xn})
        self.insert (1,new_section)                                     # insert section somewhere
        self.sort_by_xn ()                                               # and bring it in order 

        # set flap group of new to the left neighbour 
        left_sec, _ = self.neighbours_of (new_section) 
        new_section.set_flap_group (left_sec.flap_group)

        return new_section


    def delete (self, aSection: 'N_WingSection') -> N_WingSection: 
        """  delete wing section - return new current if succeeded"""

        if aSection and not aSection.isRoot and not aSection.isTip:
            try:
                index = self.index (aSection)
                self.remove (aSection)
                return self[index-1] 
            except: 
                pass
        return None  


    def do_strak (self, geometry  = None): 
        """
        straks the airfoil of all wing sections having a Strak-Airfoil which is 
        created by blending with its real neighbours

        Args: 
            geometry: optional - the desired geometry of the new straked airfoils 
                                 either GEO_BASIC or GEO_SPLINE
        """
        section: N_WingSection

        for section in self:
            if section.airfoil.isBlendAirfoil: 

                # get the neighbour wing sections  with real airfoils 

                left_sec, right_sec = self.neighbours_having_airfoil(section) 
                blendBy  = (section.cn() - left_sec.cn()) / (right_sec.cn() - left_sec.cn())

                # strak - set new geometry to achieve higher quality with splined airfoils 

                section.airfoil.set_name (left_sec.airfoil.name, reset_original=True)     # name will be <left_name>_blend0.6

                section.airfoil.do_blend (left_sec.airfoil,  right_sec.airfoil, blendBy, geometry)



    def sort_by_xn (self):
        """ 
        Re-sort wing sections to an ascending xn pos. 
        When changing major wing parms sections could become out of sort order when
            they have fixed xn and chord mixed    
        """
        self.sort (key=lambda sec: sec.xn()) 


    def neighbours_of (self, aSection: N_WingSection) -> tuple [N_WingSection, N_WingSection]:
        """
        returns the neighbour before and after a wingSection - if no neighbour return None 
        """
        try:
            index = self.index (aSection) 
        except: 
            return None, None
        
        if index == 0 : 
            left_sec = None
        else:    
            left_sec = self[index - 1] 
        try:
            right_sec = self[index + 1] 
        except: 
            right_sec = None
        return left_sec, right_sec


    def limits_of (self, aSection: N_WingSection) -> tuple [tuple,tuple]: 
        """ 
        xn position and cn chord limits as tuple of self before touching the neighbour section
        """
        xn = aSection.xn()
        cn = aSection.cn()

        if aSection.isRoot or aSection.isTip:                       # fixed 
            return (xn,xn), (cn,cn) 
        else:
            left_sec, right_sec = self.neighbours_of (aSection)     # keep a safety distance to next section
            safety = self[-1].xn() / 500.0 
            if left_sec: 
                left_xn = left_sec.xn()
                left_cn = left_sec.cn()
            else:
                left_xn = xn
                left_cn = cn
            if right_sec: 
                right_xn = right_sec.xn()
                right_cn = right_sec.cn()
            else:
                right_xn = xn
                right_cn = cn
            return (left_xn + safety, right_xn - safety), (left_cn, right_cn)



    def neighbours_having_airfoil (self, aSection: 'WingSection') -> tuple [N_WingSection, N_WingSection]:
        """
        returns the neighbour before and after a wingSection, which are not blendAirfoil
        Needed for 'strak'  - if no neighbour return None 
        """

        try:
            index = self.index (aSection) 
        except: 
            return None, None

        sec: N_WingSection

        #left neighbour 
        if aSection.isRoot: 
            left_sec = None
        else: 
            for sec in reversed(self [0:index]):
                if not sec.airfoil.isBlendAirfoil:
                    left_sec = sec
                    break 

        #right neighbour 
        if aSection.isTip: 
            right_sec = None
        else: 
            for sec in self [index+1: ]:
                if not sec.airfoil.isBlendAirfoil:
                    right_sec = sec
                    break 
        return left_sec, right_sec


    # def y_c (self) -> tuple [list, list]:
    #     """
    #     returns y position and chord of all wing sections as two lists.
    #     """
    #     aSection : WingSection = None
    #     y = []
    #     c = []
    #     for aSection in self:
    #         c.append(aSection.chord)
    #         y.append(aSection.y)
    #     return y, c


    # def xn_cn_having_fixed (self) -> tuple [list, list]:
    #     """
    #     returns the norm pos and chord of all wing sections having these defined.
    #     as two list  yn and  norm_chord - including root and tip 

    #     returns: 
    #         yn list: --  list of normed y positions 
    #         cn list: -- list of normed chords 
    #     """
    #     aSection : WingSection = None
    #     yn = []
    #     cn = []
    #     for aSection in self:
    #         if aSection.hasFixPosChord():
    #             cn.append(aSection._norm_chord)
    #             yn.append  (aSection._y / self.wing.halfwingspan)
    #     return yn, cn



    def at_xn (self, xn : float, tolerance = 0.01) -> 'N_WingSection':
        """
        returns the section at position xn with an allowed 'tolerance' - else None
        """
        section : N_WingSection = None
        for section in self:
            if abs( section.xn() - xn) < tolerance :
                return section
        return None



    # def number_of (self, aSection : WingSection) -> int | None:
    #     """
    #     returns the corrected index 1..n of a wing section in all wing sections
    #     """
    #     try:
    #         number = self.index (aSection) + 1
    #     except: 
    #         number = None
    #     return number



#-------------------------------------------------------------------------------
# Flap   
#-------------------------------------------------------------------------------



class N_Flap:
    """ 
    Outline of a single flap based on flap group in the wing sections  
    """
    def __init__(self, flaps: 'N_Flaps',
                 section_left: N_WingSection, section_right: N_WingSection):
        """
        Main constructor for new flap belonging to a wing 
        """
        self._flaps = flaps

        self.xn_from = section_left.xn()
        self.xn_to   = section_right.xn()

        self.flap_group   = section_left.flap_group
        self.section_name = section_right.name_short


    def __repr__(self) -> str:
        #nice print string of self 
        return f"<{type(self).__name__}>"


    @property         
    def planform (self) -> 'N_Planform':
        return self._flaps._planform


    @property
    def name (self) -> str:
        """ short unique name for flap like '2' """
        return f"{self.flap_group}"
    

    def polyline  (self) -> tuple [Array, Array]:
        """
        polyline y,x of self    
        """
        xn_from = self.xn_from
        xn_to   = self.xn_to
        x,y = [], []

        # we start a TE at xn_from and going clockwise around the flap upto xn_to
        x.append(xn_from)
        y.append(self.planform.le_te_at (xn_from)[1])

        # go to hinge point
        x.append(xn_from)
        y.append(self._flaps.hinge_yn_at(xn_from))

        # go along the hinge 
        x.append(xn_to)
        y.append(self._flaps.hinge_yn_at(xn_to))

        # back to TE 
        x.append(xn_to)
        y.append(self.planform.le_te_at (xn_to)[1])

        # ... finally along TE to starting point back to TE
        xn, le_yn, te_yn = self.planform.le_te_polyline()
        i1 = min(bisect.bisect(te_yn, xn_from)-1, len(te_yn) -2)    # get te coordinates between xn_from and yTo
        i2 = min(bisect.bisect(te_yn, xn_to)  -1, len(te_yn) -2)
        y = np.append (y, np.flip(te_yn[i1:i2+1]))  
        x = np.append (x, np.flip(xn[i1:i2+1]))

        return x, y



    def line_left  (self, x_offset=0) -> tuple [Array, Array]:
        """
        polyline xn, yn of self left side   
        """
        xn_from = self.xn_from + x_offset
        x,y = [], []

        # we start a TE at xn_from and going clockwise around the flap upto xn_to
        x.append(xn_from)
        y.append(self.planform.le_te_at (xn_from)[1])

        # go to hinge point
        x.append(xn_from)
        y.append(self._flaps.hinge_yn_at (xn_from))

        return np.array(x), np.array(y)


    def line_right  (self, x_offset=0) -> tuple [Array, Array]:
        """
        polyline xn, yn of self right side   
        """
        xn_to   = self.xn_to - x_offset
        y,x = [], []

        # we start a TE at xn_from and going clockwise around the flap upto xn_to
        x.append(xn_to)
        y.append(self.planform.le_te_at (xn_to)[1])

        # go to hinge point
        x.append(xn_to)
        y.append(self._flaps.hinge_yn_at (xn_to))

        return np.array(x), np.array(y)


    def line_hinge  (self, x_offset=0) -> tuple [Array, Array]:
        """
        hinge line xn, yn line of self    
        """
        xn_from = self.xn_from + x_offset
        xn_to   = self.xn_to   - x_offset
        x,y = [], []

        # go to hinge point
        x.append(xn_from)
        y.append(self._flaps.hinge_yn_at (xn_from))

        # go along the hinge 
        x.append(xn_to)
        y.append(self._flaps.hinge_yn_at (xn_to))

        return np.array(x), np.array(y)


    def line_te  (self, x_offset=0) -> tuple [Array, Array]:
        """
        trailing edge polyline y,x of self    
        """
        xn_from = self.xn_from + x_offset
        xn_to   = self.xn_to   - x_offset
        y,x = [], []

        # we start a TE at xn_from  
        x.append(xn_from)
        y.append(self.planform.le_te_at (xn_from)[1])

        # ... along TE 
        xn, _, le_yn = self.planform.le_te_polyline()
        i1 = min(bisect.bisect(xn, xn_from)-1, len(xn) -2)    # get te coordinates between xn_from and yTo
        i2 = min(bisect.bisect(xn, xn_to)  -1, len(xn) -2)
        x = np.append (x, xn[i1:i2+1])
        y = np.append (y,le_yn[i1:i2+1])  

        # final TE point 
        x = np.append(x, xn_to)
        y = np.append(y, self.planform.le_te_at(xn_to)[1])
 
        return x,y


    def rel_depth  (self, y_offset=0) -> tuple [Array, Array]:
        """
        relative depth (depth/chord) polyline y,x of self    
        """
        xn_from = self.xn_from + y_offset
        xn_to   = self.xn_to   - y_offset

        y = np.linspace (xn_from, xn_to, 10)
        x = np.zeros (10)
        for i, yi in enumerate (y):
            x[i] = self._flaps.depth_at(yi) / self.planform.chord_at (yi) 

        return y, x


    def center (self) -> tuple [float, float]:
        """ center point of self"""

        xn = (self.xn_to + self.xn_from) / 2

        te_yn    = self.planform.le_te_at(xn)[1]
        hinge_yn = self._flaps.hinge_yn_at(xn)
        yn       = (te_yn + hinge_yn) / 2

        return xn, yn 





class N_Flaps:
    """ 
    Creates dynamically list of Flaps from definitions in wingSections 

    Wing
        |-- Planform
            |-- Norm_Planform
                |-- Norm_Flaps
                    |-- Norm_Flap
    """ 
    def __init__(self, planform: 'N_Planform', dataDict: dict = None):

        self._planform : N_Planform = planform

        self._hinge_equal_ref_line = fromDict (dataDict, "hinge_equal_ref_line", False)  

        self.check_and_correct ()                   # sanity checks of hinge and flap definitions    



    def _save (self) -> dict:
        """ returns the parameters of self as a dict"""

        myDict = {}
        toDict (myDict, "hinge_equal_ref_line",  self._hinge_equal_ref_line) 

        return myDict


    def _get_hinge_points  (self) -> tuple [Array, Array]:
        """
        definition points of hinge line as x,y    
        """
        if self.hinge_equal_ref_line:

            # just take reference polyline 
            x, y=  self._planform.chord_ref_polyline()

        else: 

            # collect all hinge xn definitions in sections
            x, y = [], []
            section : N_WingSection
            for section in self._wingSections:

                if section.hinge_cn is not None: 
                    xn       = section.xn ()
                    hinge_rel_cn = section.hinge_cn
                    le_yn, te_yn = self._planform.le_te_at (xn)

                    x.append (xn)
                    y.append (le_yn + hinge_rel_cn * (te_yn - le_yn))

        # sanity - at least 2 points and strictly increaing ? 

        if len (x) < 2 or not np.all(np.diff(x) > 0):
            logger.warning ("Hinge polyline definition is corrupted - will be reset")
            self.check_and_correct()
            x, y = self._get_hinge_points ()                                # try again 

        return x,y 



    @property
    def _wingSections (self) -> N_WingSections:
        return self._planform.wingSections
        
    @property
    def hinge_equal_ref_line (self) -> bool: 
        """ True if hinge line equals reference line """
        return self._hinge_equal_ref_line
    
    def set_hinge_equal_ref_line (self, aBool : bool):
        self._hinge_equal_ref_line = aBool == True 


    def get (self) -> list[N_Flap]: 
        """
        returns the flap objects based on wing sections flap group
        """
        flapList = []
        if not self._wingSections: return flapList 

        section_start  = self._wingSections[0]
        section : N_WingSection

        for section in self._wingSections:
            if (section.flap_group != section_start.flap_group or \
                section == self._wingSections[-1]) :
                # new flap group or last section -> create flap 
                flapList.append(N_Flap(self, section_start, section))
                section_start = section 

        return flapList
    

    def delete_hinge_point_ok (self, index : int) -> bool:
        """ is delete hinge point having index ok? Return True if succeeded """

        if self.hinge_equal_ref_line: return False                      # only if hinge is defined on its own 

        x, y = self._get_hinge_points() 

        if index == 0: return False                                 # don't delete root point 
        if len(x) == 2: return False                                # at least 2 points needed

        # find section with hinge definition #index 

        i = 0 
        sec : N_WingSection
        for sec in self._wingSections:
            if sec.hinge_cn is not None: 
                if i == index:
                    return True 
                else: 
                    i += 1 
        return False


    def check_and_correct (self):
        """ check consistency of wingSections flap definition and correct if possible"""

        sections = self._wingSections

        # if hinge is not equal ref_line - there must be at least 2 hinge definition sections 

        if not self.hinge_equal_ref_line:
            n = 0 
            for sec in sections: 
                if sec.hinge_cn is not None: n += 1 

            if n < 2:
                root_sec : N_WingSection = sections[0]
                tip_sec  : N_WingSection = sections[-1]
                root_sec.set_hinge_cn = 0.75
                tip_sec.set_hinge_cn = 0.75

        # there must be a new flap group when there is a hinge break 
        
        for i in range (1, len(sections)-1):
            left_sec : N_WingSection = sections[i-1]
            sec      : N_WingSection = sections[i]
 
            if sec.hinge_cn is not None:
                if left_sec.flap_group == sec.flap_group:
                    sec.set_flap_group (left_sec.flap_group+1)


    def hinge_yn_at (self, xn : float) -> float:
        """n of hinge line at xn """  

        xn_arr, yn_arr = self.hinge_polyline ()
        yn = np.interp(xn, xn_arr, yn_arr)                                  # linear interpolation 

        return yn


    def hinge_polyline  (self) -> tuple [Array, Array]:
        """
        hinge line y,x to the very tip   
        """

        x, y = self._get_hinge_points ()

        # if left point isn't at tip, insert additional extrapolated point at tip 

        if x[-1] != 1.0: 
            y_tip = interpolate (x[-2], x[-1], y[-2],y[-1], 1.0)
            x.append (1.0)
            y.append (y_tip)

        return np.array(x), np.array(y)


    def hinge_as_jpoints (self, transform_fn = None)  -> list[JPoint]:
        """ 
        hinge definition points as JPoints with limits and fixed property
            - transform_fn will transform jpoint to new coordinates 
        """

        jpoints = []
        xn_arr, yn_arr = self._get_hinge_points()
        i_tip = len(yn_arr)-1

        for i,xn in enumerate(xn_arr):

            yn = yn_arr[i]

            jpoint = JPoint (xn, yn)                               

            le_yn, te_yn = self._planform.le_te_at (xn)
            jpoint.set_x_limits ((xn, xn))                      # fix 
            jpoint.set_y_limits ((te_yn, le_yn))                # le is max, te is min

            if i == 0:                                          # root fixed
                jpoint.set_name ('Flap Root')
            elif i == i_tip:                                     
                jpoint.set_name ('Flap Tip')
            else:                                           
                jpoint.set_name ('Flap')

            jpoints.append(jpoint.as_transformed (transform_fn))

        return jpoints


    def hinge_from_jpoints (self, jpoints: list[JPoint], transform_fn = None):
        """ 
        set hinge from definition JPoints - updates winSection hinge_xn 
        """

        section : N_WingSection
        for section in self._wingSections:

            found_jpoint = None
            for jpoint in jpoints:

                xn, yn = jpoint.as_transformed (transform_fn).xy
                if xn == section.xn(): 
                    found_jpoint = jpoint
                    break
             
            if found_jpoint: 
                le_yn, te_yn = self._planform.le_te_at (xn) 
                hinge_cn = (yn - le_yn) / (te_yn - le_yn)
                section.set_hinge_cn (hinge_cn) 
            else: 
                section.remove_hinge_cn ()



    def depth_at (self, xn : float, hinge_yn : float = None) -> float:
        """ relative flap depth 0..1 at position xn """

        if hinge_yn is None: 
            hinge_yn = self.hinge_yn_at (xn)
        le_yn, te_yn = self._planform.le_te_at (xn)

        depth = (te_yn - hinge_yn) / (te_yn - le_yn)
        depth = np.clip (depth, 0.0, 1.0)                                   # sanity    
        return depth



    def hinge_rel_cn_polyline  (self) -> tuple [Array, Array]:
        """
        relative hinge position within chord    
        """
        xn_arr, yn_arr = self.hinge_polyline ()

        xn, le_yn, te_yn =  self._planform.le_te_polyline ()
        rel_depth = np.zeros (len(xn))

        for i, xni in enumerate (xn):
            hinge_y = np.interp(xni, xn_arr, yn_arr)                            # linear interpolation 
            rel_depth[i] = (le_yn[i] - hinge_y) / (le_yn[i] - te_yn[i])

        return xn, rel_depth


    def depth_cn_polyline  (self) -> tuple [Array, Array]:
        """
        flap depth in chord distribution polyline   
        """
        hinge_xn, hinge_yn = self.hinge_polyline ()

        xn, le_yn, te_yn =  self._planform.le_te_polyline()
        depth = np.zeros (len(xn))

        for i, xi in enumerate (xn):
            hinge_y = np.interp(xi, hinge_xn, hinge_yn)         # linear interpolation 
            depth[i] = (te_yn[i] - hinge_y) 

        return xn, depth




    # def rel_depth_as_jpoints (self)  -> list[JPoint]:
    #     """ 
    #     relative depth of flaps at section as JPoints with limits and fixed property
    #         x values 0..1
    #         y values scaled to chord - fixed as it is on wingSection
    #     """

    #     jpoints = []
    #     y_arr, x_arr = self.hinge_polyline ()
    #     for i, y in enumerate(y_arr):

    #         x = x_arr[i]
    #         le_x, te_x = self._planform._planform_at (y)
    #         rel_depth = (te_x - x) / (te_x - le_x)

    #         jpoint = JPoint (y, rel_depth)                          # change to display coordinate system       

    #         jpoint.set_x_limits ((y, y))                    # fix
    #         jpoint.set_y_limits ((0, 1))

    #         if i == 0:                                      # root fixed
    #             jpoint.set_name ('Flap Root')
    #         elif i == (len(y_arr)-1):                                     
    #             jpoint.set_name ('Flap Tip')
    #         else:                                           
    #             jpoint.set_name ('Flap')
    #         jpoints.append(jpoint)

    #     return jpoints


    # def rel_depth_from_jpoints (self, jpoints: list[JPoint]):
    #     """ 
    #     set hinge from relative depth definition JPoints - updates winSection hinge_xn 
    #     """

    #     for jpoint in jpoints:

    #         y, x = jpoint.xy
             
    #         section = self._wingSections.at_y (y)

    #         if section is None: 
    #             raise ValueError (f"No section found at y position {jpoint.x}")
    #         else: 
    #             xn = 1.0 - x
    #             section.set_hinge_xn (xn) 


    def insert_hinge_point_at (self, xn : float) -> bool:
        """ 
        try to add a hinge 'break' at a section, which should be at xn
            Return True if successful 
        """ 
        section = self._wingSections.at_xn (xn, tolerance = 0.01) 

        if section is not None: 
            if section.hinge_cn == None:

                # calculate the current hinge position 
                xn_arr, yn_arr = self.hinge_polyline ()

                hinge_y = np.interp(xn, xn_arr, yn_arr)                            # linear interpolation 
                le_x, te_x = self._planform.le_te_at (xn)

                cn = (hinge_y - le_x) / (te_x - le_x)

                # update section with hinge info 
                section.set_hinge_cn (cn)

                # ensure a new flap group will begin at this section 
                self.check_and_correct ()

                return True
        return False 



#-------------------------------------------------------------------------------
# Normalized Planform 
#-------------------------------------------------------------------------------


class N_Planform: 
    """ 

    Normalized planform with a root chord of 1.0 and a span of 1.0 

    Leading and Trailing edge are build based on its 

    Reference Line  
        which describes how much of the chord is given to Leading and Trailing edge
        The Reference Line is defined by a quadratic Bezier which allows 
        the planform to be bended from root to tip


    Wing
        |-- Planform
            |-- Norm_Planform
                |-- Norm_Chord
                |-- Norm_WingSections
                    |-- Norm_WingSection
                |-- Norm_Flaps
                    |-- Norm_Flap
    """

    def __init__(self, planform : 'Planform', 
                 dataDict: dict = None, 
                 chord : N_Chord_Abstract = None, 
                 chord_ref : N_Chord_Reference = None):

        self._planform = planform

        # create Norm_Chord distribution depending on style e.g. 'Bezier'
        if chord is None: 
            chord_style = fromDict (dataDict, "chord_style", 'Bezier')

            if chord_style == 'Bezier':
                self._norm_chord = N_Chord_Bezier (dataDict)
            elif chord_style == 'Trapez':
                self._norm_chord = N_Chord_Trapezoid (self, dataDict)
            else:
                raise ValueError (f"Chord style {chord_style} not supported")
        else: 
            self._norm_chord = chord 

        # init chord reference function either new from dataDict or as argument of self 
        if chord_ref is None:
            self._chord_ref = N_Chord_Reference (dataDict=fromDict(dataDict, "chord_reference", {}))
        else: 
            self._chord_ref = chord_ref 

        # init wing sections 
        self._wingSections    = N_WingSections (self, sectionsDict=fromDict(dataDict, "wingSections", {}))

        # init flaps
        self._flaps           = N_Flaps (self, dataDict)



    def _save_to (self, dataDict : dict) :
        """ save the parameters of self into dataDict"""

        toDict (dataDict, "chord_style",        self._norm_chord.name) 
        toDict (dataDict, "chord_reference",    self._chord_ref._as_dict()) 
        toDict (dataDict, "wingSections",       self._wingSections._as_list_of_dict()) 


    @property
    def name (self) -> str:
        """ name - default name of norm chord"""
        return self._norm_chord.name

    @property
    def chord (self) -> N_Chord_Abstract:
        return self._norm_chord

    @property
    def chord_ref (self) -> N_Chord_Reference:
        """ chord reference function """
        return self._chord_ref

    @property
    def workingDir (self) -> str:
       """ current working directory""" 
       self._planform.workingDir 

    @property
    def cn_is_bezier (self) -> bool:
        """ true if Bezier based chord"""
        return self.chord.isBezier

    @property
    def cn_is_trapezoidal (self) -> bool:
        """ true if trapezoidal chord"""
        return self.chord.isTrapezoidal

    @property
    def wingSections (self) -> N_WingSections:
        """ normed wingSections of self"""
        return self._wingSections

    @property
    def flaps (self) -> N_Flaps:
        """ returns Flaps-Handler which generates flaps of self"""
        return self._flaps

    @property
    def hinge_equal_ref_line (self) -> bool: 
        """ True if hinge line equals reference line """
        return self.flaps.hinge_equal_ref_line
    
    @property
    def _move_y (self) -> float:
        """ dy value of move transform of planform in xn,yn coordinate system"""
        # -> trailing edge at root will be at yn=0.0  
        return - self.chord_ref.at(0) 


    def cn_at (self, xn: float, fast=True) -> float:
        """ 
        normalized chord at xn
        """
        return self.chord.at (xn, fast=fast)


    def cn_polyline (self) -> Polyline:
        """ 
        Polyline of normalized chord distribution cn

        Returns:
            xn: normalized x coordinates
            cn: normalized y coordinates 
        """
        return self.chord.polyline()


    def xn_at (self, cn: float, fast=True) -> float:
        """ 
        returns xn at normed chord xn
        """
        return self.chord.xn_at (cn, fast=fast)



    def t_chord_to_norm (self, xn : float|Array|list, 
                               yn : float|Array|list, 
                               cn : float|Array|list|None  =None) -> ...:
        """
        Transforms chord coordinates into normalized planform coordinates
            - by shiftung yn value dependand chord reference at xn  
            - by shifting yn with a constant value 

        Args:
            xn: x normalized, either float, list or Array to transform 
            yn: normalized chord coordinates, either float, list or Array to transform
            cn: optional chord at xn - for optimization 

        Returns:
            x: transformed x normed planform coordinates as float or np.array
            y: transformed y normed planform coordinates as float or np.array
        """

        # LE yn = 1.0, TE yn = 0.0
        # cr = 1.0 --> 100% LE  

        if isinstance (xn, float):
            cr  = self.chord_ref.at (xn)                    # chord reference function

            if cn is None:
                cn  = self.chord.at(xn)                         # chord

            # apply chord reference function
            le_y =   cn * cr                                
            te_y = - cn * (1-cr)   
            y = interpolate (0.0, 1.0, te_y, le_y, yn)

            # move and flip 
            cr_0 = self.chord_ref.at (0)
            y = - (y - cr_0)
            x = xn

        else: 
            # array loop over each xn as y will be depending on the position 
            x, y = np.array (xn), np.array (yn)

            for i in range (len(xn)):
                if cn is None: 
                    x[i], y[i] = self.t_chord_to_norm (xn[i], yn[i])
                else: 
                    x[i], y[i] = self.t_chord_to_norm (xn[i], yn[i], cn=cn[i])

        return x, y 


    def le_te_at (self, xn: float, fast=True) -> tuple [float, float]:
        """ 
        Main planform function - returns le_yn and te_yn at xn

        At root: le_yn = 1.0 and te_yn = 0.0
        """

        xn, le_yn = self.t_chord_to_norm (xn, 1.0)
        xn, te_yn = self.t_chord_to_norm (xn, 0.0)

        return le_yn, te_yn 


    def le_te_polyline (self) -> Polylines:
        """ 
        Normalized polylines of leading and trailing edge
            At root it is: le_yn [0] = 1.0 and te_yn[0] = 0.0

        Returns:
            xn: normalized x coordinates
            le_yn: normalized y coordinates of leading edge
            te_yn: normalized y coordinates of leading edge
        """

        xn, cn = self.chord.polyline()

        xn, le_yn = self.t_chord_to_norm (xn, np.full (len(xn), 1.0), cn=cn)
        xn, te_yn = self.t_chord_to_norm (xn, np.zeros(len(xn))      , cn=cn)

        return xn, le_yn, te_yn 


    def polygon (self) -> Polyline:
        """ 
        polygon of the planform starting at le_root clockwise 
        """

        xn, le_yn, te_yn = self.le_te_polyline()

        xn = np.append (xn, np.flip (xn))
        xn = np.append (xn, xn[0:1])
        yn = np.append (le_yn, np.flip (te_yn))
        yn = np.append (yn, yn[0:1])
        return xn, yn 


    def chord_ref_polyline (self) -> Polyline:
        """ 
        Polyline of the chord reference function reference 
            which is just a horizontal line at y = cr [0]
            in planform after transformation 

        Returns:
            xn: normalized x coordinates
            yn: normalized y coordinates 
        """

        xn  = np.array([0.0, 1.0])

        _,cr0 = self.t_chord_to_norm (0.0, self.chord_ref.at (0) )
        yn  = np.array([1-cr0, 1-cr0])

        return xn, yn


    def box_polygon (self) -> Polyline:
        """ 
        rectangle polygon x=0..1, y=0..1

        Returns:
            xn: normalized x coordinates
            yn: normalized y coordinates 
        """
        le_yn = 0.0
        te_yn = 1.0
        xn = np.array([0.0,   1.0,   1.0,   0.0,   0.0])
        yn = np.array([le_yn, le_yn, te_yn, te_yn, le_yn])

        return xn, yn


#-------------------------------------------------------------------------------
# Planform 
#-------------------------------------------------------------------------------


class Planform: 
    """ 

    Main object representing the planform of a wing half.
        having a chord at root, span and a sweep angle 

    The planform is transformed into wing data from a normalized planform (0..1) 

    Wing
        |-- Planform
            |-- Norm_Planform
                |-- Norm_Chord
    """

    def __init__(self, 
                 wing : Wing = None, 
                 dataDict: dict = None,
                 chord     : N_Chord_Abstract = None, 
                 chord_ref : N_Chord_Reference = None):
                

        self._wing = wing

        self._span        = fromDict (dataDict, "halfspan", 1000.0) 
        self._chord_root  = fromDict (dataDict, "chord_root", 200.0)
        self._sweep_angle = fromDict (dataDict, "sweep_angle", 1.0)

        # create Norm_Planform with a Norm_Chord distribution  
        
        self._norm = N_Planform (self, dataDict, chord=chord, chord_ref=chord_ref)

         
        self._norm_ref_elliptical = None                        # elliptical reference planform             

    def _save_to (self, dataDict : dict) :
        """ save the parameters of self into dataDict"""

        toDict (dataDict, "halfspan",       self.span) 
        toDict (dataDict, "chord_root",     self.chord_root) 
        toDict (dataDict, "sweep_angle",    self.sweep_angle) 

        self._norm._save_to (dataDict)


    @property
    def wing (self) -> Wing:
        """ wing self belongs to"""
        return self._wing
    
    @property
    def workingDir (self) -> Wing:
        """ working directory of wing"""
        return self.wing.workingDir
    
    @property
    def norm (self) -> N_Planform:
        """ the normed planform of self"""
        return self._norm


    @property
    def span (self) -> float:
        """ wing half span"""
        return self._span 

    def set_span (self, aVal : float):
        """ set span of halfwing"""
        aVal = max ( 0.01, aVal)
        self._span = aVal 


    @property
    def chord_root (self) -> float:
        """ chord at root section"""
        return self._chord_root

    def set_chord_root (self, aVal : float):
        """ set chord at root """
        aVal = max ( 0.01, aVal)
        self._chord_root = aVal 


    @property
    def sweep_angle (self) -> float:
        """ sweep angle of reference line in degrees"""
        return self._sweep_angle

    def set_sweep_angle (self, aVal : float):
        """ set sweep angle of reference line to aVal degrees"""

        aVal = max (-75.0, aVal)
        aVal = min ( 75.0, aVal)
        self._sweep_angle = aVal 


    def t_norm_to_plan (self, xn : float|Array|list, yn : float|Array|list) -> ...:
        """
        Transforms normalized coordinates into planform(wing) coordinates 
            - by scaling with chord and span 
            - by shearing with sweep angle 
        Args:
            xn,yn: normalized, either float, list or Array to transform 
        Returns:
            x,y: transformed planform coordinates as float or np.array
        """

        # sanity - convert list to np.array
        if isinstance (xn, float):
            x, y = xn, yn
        else: 
            x, y = np.array (xn), np.array (yn)

        # scale 
        x = x * self.span
        y = y * self.chord_root


        # do shear 
        shear_factor =  1 / np.tan((90-self.sweep_angle) * np.pi / 180)    # = cotangens
        y = y + x * shear_factor  

        return x, y 


    def t_plan_to_norm (self, x : float|Array|list, y : float|Array|list) -> ...:
        """
        Transforms planform(wing) coordinates into normalized coordinates  
            - reverse of 'transform_norm' 

        Args:
            x,y: planform coordinates - either float, list or Array to transform 
        Returns:
            xn,yn: transformed normalized coordinates  as float or np.array
        """

        # sanity - convert list to np.array
        if isinstance (x, float):
            xn, yn = x, y
        else: 
            xn, yn = np.array (x), np.array (y)

        # undo shear 
        shear_factor =  1 / np.tan((90-self.sweep_angle) * np.pi / 180)    # = cotangens
        yn = yn - xn * shear_factor

        # un-scale 
        xn = xn / self.span
        yn = yn / self.chord_root

        return xn, yn 



    def t_norm_to_span (self, xn : float|Array|list, yn : float|Array|list) -> ...:
        """
        Transforms normalized coordinates xn,yn into x 0..span, yn 0..1 
 
        Args:
            xn, yn: normalized, either float, list or Array to transform 
        Returns:
            x: transformed x span coordinates  
            y: equals yn  
        """

        # sanity - convert list to np.array
        if isinstance (xn, float):
            x, y = xn, yn
        else: 
            x, y = np.array (xn), np.array (yn)

        # scale 
        x = x * self.span

        return x, y 


    def t_span_to_norm (self, x : float|Array|list, yn : float|Array|list) -> ...:
        """
        Transforms spanwise x coordinates x and normalized yn into normed xn,yn  
 
        Args:
            x: as span wise coordinates, either float, list or Array to transform 
            yn: normalized, either float, list or Array to transform 
        Returns:
            xn, yn: normalized 
        """

        # sanity - convert list to np.array
        if isinstance (x, float):
            xn, yn = x, yn
        else: 
            xn, yn = np.array (x), np.array (yn)

        # scale 
        xn = xn / self.span

        return xn, yn 



#-------------------------------------------------------------------------------

class Planform_Paneled (N_Chord_Trapezoid):
    """ 
    Helper (slave) Planform which presents the actual planform as a paneled (trapezoid) version
    Used for Xflr5 and FLZ export   
    """
    style  = "paneled"
    isTemplate    = False

    wingSection_eitherPosOrChord = False   # both pos and chord my be defined by user

    sections_depend_on_planform = False          
    planform_depend_on_sections = True           # sections define the shape of the planform

    shortDescription = "Planform defined by its wing sections,\n" + \
                       "which have a defined position and chord." 

    def __init__(self, myWing: Wing, dataDict: dict = None):
        super().__init__(myWing, dataDict)
        """
        Args:
            :myWing: the wing object self belongs to  
            :dataDict: optional - dictonary with all the data for a valid section
        """
        # read additional parameters of this shapeform 
        self._x_panels    = fromDict (dataDict, "x-panels", 10)
        self._x_dist      = fromDict (dataDict, "x-distribution", "cosine")
        self._y_panels    = fromDict (dataDict, "y-panels", 10)
        self._y_dist      = fromDict (dataDict, "y-distribution", "uniform")
        self._y_minWidth  = fromDict (dataDict, "y-minWidth", 20)
        self._minchord_tip = fromDict (dataDict, "minTipChord", 30)

        self.distribution_fns = {}
        self.distribution_fns["uniform"]= lambda y : y
        self.distribution_fns["-sine"]  = lambda y : np.sin (y     * np.pi/2)
        self.distribution_fns["sine"]   = lambda y : np.sin ((y+2) * np.pi/2) + 1
        self.distribution_fns["cosine"] = lambda y : ((np.cos ((y+1) * np.pi)) + 1) / 2


    def _save (self, dataDict):
        """ stores the variables into the dataDict"""
        toDict (dataDict, "x-panels",       self._x_panels) 
        toDict (dataDict, "x-distribution", self._x_dist) 
        toDict (dataDict, "y-panels",       self._y_panels) 
        toDict (dataDict, "y-panels",       self._y_panels) 
        toDict (dataDict, "y-minWidth",     self._y_minWidth) 
        toDict (dataDict, "minTipChord",    self.min_chord_tip) 


    # ---Properties --------------------- 

    @property
    def x_panels (self):                return self._x_panels
    def set_x_panels (self, val: int):  self._x_panels = int(val)

    @property
    def x_dist (self):                  return self._x_dist
    def set_x_dist (self, val):  
        if val in self.distribution_fns:
            self._x_dist = val

    @property
    def y_panels (self):                return self._y_panels
    def set_y_panels (self, val: int):  self._y_panels = int(val)

    @property
    def y_dist (self):                  return self._y_dist
    def set_y_dist (self, val):  
        if val in self.distribution_fns:
            self._y_dist = val

    @property
    def y_minWidth (self):              return self._y_minWidth
    def set_y_minWidth (self, val):     self._y_minWidth = val

    @property
    def min_chord_tip (self):             
        """ minimum chord at tip when generating panels"""
        return self._min_chord_tip
    
    def set_min_chord_tip (self, val): 

        if not val is None: 
            _, chords = self.wing.wingSections.y_c() 
            # minchord_tip must be less than 90% wing section chord before tip  
            val = min (val, chords[-2] * 0.9)  
            # ... more than tip chord  
            val = max (val, self.wing.chord_tip)  
        self._min_chord_tip = val

    @property
    def isTipCutted (self):                     
        """ is tip cutted due to minchord_tip?""" 

        # sanity check: is minchord_tip between chord at tip and chord tip-1

        _, chords = self.wing.wingSections.y_c()
        return chords[-1] < self._min_chord_tip and chords[-2] > self._min_chord_tip


    # --- Methods --------------------- 

    def _calc_reducedTipPos (self):
        """ 
        returns a reduced tip y position if chord_tip is smaller than minTipCord
         - other tip y position remains halfwingspan 
        """

        min_chord_tip = self._min_chord_tip
        newTipPos   = self.halfwingspan

        # sanity check: is minchord_tip between chord at tip and chord tip-1

        yPosList, chordList = self.wing.wingSections.y_c()
        if chordList[-1] > min_chord_tip or chordList[-2] < min_chord_tip: return self.halfwingspan

        # find the y position having minchord_tip 
        yTip     = yPosList[-1]
        yLeftTip = yPosList[-2]

        firstGuess = yLeftTip + (yTip - yLeftTip) * 0.25 
        bounds     = (yLeftTip, yTip)      
        fn         = lambda y : self.wing.planform.chord_at(y) - min_chord_tip

        try: 
            newTipPos = findRoot (fn, firstGuess , no_improve_thr=10e-5, bounds=bounds) 
        except:
            newTipPos = self.halfwingspan

        return newTipPos


    def _sections_y_chord (self):
        """ returns yPos and chord of all sections as two lists"""

        y, c = self.wing.wingSections.y_c()

        # is there a new tip yPos because of minimum tip chord? 
        if self.isTipCutted:

            reduced_y_tip = self._calc_reducedTipPos()
            y[-1]  = reduced_y_tip                        # set "new" tip 
            c[-1] = self.wing.planform.chord_at(reduced_y_tip)
            
        return y, c
    

    def y_panels_forSection(self, sections_y, iSec):
        """y-panels for section i depending on min panel width"""

        if iSec < len(sections_y)-1:
            y_left = sections_y [iSec]
            y_right  = sections_y [iSec+1]
            dy_sec = y_right - y_left
            # assure a min panel width in y-direction 
            npanels = min ( self.y_panels, round(dy_sec/self.y_minWidth))
            npanels = max (npanels, 1)                                      # ensure at least 1 panel
        else:
            npanels = 0 
        return npanels


    def distribution_fns_names (self):
        """ a list of available distribution functions"""
        return list(self.distribution_fns.keys())


    def _planform_at (self, y):
        """
        the planform represented as trapezoid
        """

        sections_y = self._sections_y_chord() [0]

        for iSec in range(len(sections_y) - 1):
            y_before = sections_y [iSec]
            y_after  = sections_y [iSec+1]
            if y  >= y_before and  y <= y_after:
                le_before, te_before = self.wing.planform._planform_at (y_before)
                le_after, te_after   = self.wing.planform._planform_at (y_after)
                le = interpolate(y_before, y_after, le_before, le_after, y) 
                te = interpolate(y_before, y_after, te_before, te_after, y) 
                return le, te

        # not found ...? 
        raise ValueError ("Could not interpolate '%s'for paneled planform the value " % y)

    def y_panel_lines (self):
        """
        the lines from LE to TE representing the y panels of the planform 
        Returns:
            :y: list of array of the y-stations of the line  
            :x: list of array of x values of LE and TE 
            :deviation: in percent to the actual planform chord 
        """
        lines_y = []
        lines_le_to_te = []  

        deviations = []        

        y_distribution_fn = self.distribution_fns [self.y_dist]
        sections_y = self._sections_y_chord() [0]

        # get le te for all sections 
        sections_le_te = []
        for y_sec in sections_y: 
            # take actual planform .. function (maybe faster) 
            sections_le_te.append(self.wing.planform._planform_at (y_sec))

        # now calc a y line according to y distribution function between sections 
        for iSec in range(len(sections_y) - 1):
            y_left   = sections_y [iSec]
            y_right  = sections_y [iSec+1]
            dy_sec   = y_right - y_left
            le_left  = sections_le_te[iSec][0]
            te_left  = sections_le_te[iSec][1]
            le_right = sections_le_te[iSec+1][0]
            te_right = sections_le_te[iSec+1][1]

            # assure a min panel width in y-direction 
            y_panels = self.y_panels_forSection(sections_y, iSec)

            # line on section will be double
            for d_yn_pan in np.linspace (0, 1, y_panels +1): 
                # ! perform the distribution function
                yn_distrib_pan = y_distribution_fn (d_yn_pan)

                le = interpolate(0.0, 1, le_left, le_right, yn_distrib_pan)
                te = interpolate(0.0, 1, te_left, te_right, yn_distrib_pan)
                yPos = y_left + yn_distrib_pan * dy_sec

                lines_y.append([yPos, yPos])
                lines_le_to_te.append([le, te])

                # calculate the deviation to actual planform upto close to tip 
                if yPos < self.halfwingspan:
                    chord_paneled = te-le
                    le,te  =  self.wing.planform._planform_at (yPos) 
                    chord_actual  = te-le
                    deviation = abs((chord_actual - chord_paneled) / chord_actual) * 100
                else:
                    deviation = 0 
                deviations.append(deviation)

        return lines_y, lines_le_to_te, deviations
    
    def x_panel_lines (self):
        """
        the lines from one section to the next representing the x panels of the planform 
        Returns:
            :y: list of array of the y-stations of the line  
            :x: list of array of x values of LE and TE 
        """
        lines_y = []
        lines_panels_x = []           
        x_distribution_fn = self.distribution_fns [self.x_dist]
        sections_y = self._sections_y_chord() [0]

        # get le te for all sections 
        sections_le_te = []
        for y_sec in sections_y: 
            sections_le_te.append(self.wing.planform._planform_at (y_sec))


        # now calc a x horizontal line according to x distribution function between sections 
        for iSec in range(len(sections_y) - 1):
            y_left = sections_y [iSec]
            y_right  = sections_y [iSec+1]

            le_left  = sections_le_te[iSec][0]
            te_left  = sections_le_te[iSec][1]
            le_right = sections_le_te[iSec+1][0]
            te_right = sections_le_te[iSec+1][1]

            chord_left  = te_left  - le_left
            chord_right = te_right - le_right

            # line on section will be double
            for dxn_pan in np.linspace (0, 1, self.x_panels +1): 
                # ! perform the distribution function
                xn_distrib_pan = x_distribution_fn (dxn_pan)

                x_left  = le_left  + xn_distrib_pan * chord_left
                x_right = le_right + xn_distrib_pan * chord_right
                lines_y.append          ([y_left, y_right])
                lines_panels_x.append   ([x_left, x_right])

        return lines_y, lines_panels_x



#-------------------------------------------------------------------------------
# WingSections  
#-------------------------------------------------------------------------------




class WingSection:
    """ 
    A certain station of wing. Is defined by its Re or fixed by y pos.
    There is always a root section (index 0) and a tip section (last index)   

    Wing
       |-- WingSection
             |-- airfoil
    """
    def __init__(self, myWing: Wing, dataDict: dict = None):
        """
        Main constructor for new section belonging to a wing 

        Args:
            myWing: the wing object self belongs to  
            dataDict: optional - dictonary with all the data for a valid section
        """

        # get initial data from dict 
        self.wing : Wing = myWing

        self._y                 = fromDict (dataDict, "y", None)
        self._norm_chord        = fromDict (dataDict, "norm_chord", None)
        self._eitherPosOrChord  = fromDict (dataDict, "eitherPosOrChord", None)

        self._hinge_xn          = fromDict (dataDict, "hinge_xn", None)
        self._flap_group        = fromDict (dataDict, "flap_group", 1)

        self.isRoot = (self._y == 0.0) 
        self.isTip  = (self._y == self.wing.halfwingspan)

        if self.isRoot:
            self._norm_chord = 1.0
            self._hinge_xn = self._hinge_xn if self._hinge_xn is not None else 0.75

        if self.isTip:
            self._norm_chord = 1.0
            self._hinge_xn = self._hinge_xn if self._hinge_xn is not None else 0.75
 
        if (self._y is None and self._norm_chord is None):
            logger.error (f"{self} Either position or chord / reynolds must be set")
            logger.info  (f"{self} Setting chord to 0.8 of root")
            self._norm_chord    = self.wing.chord_root * 0.8

        # section may have either a position or chord for
        # ... trapezoid planform: section may have both which will defne planform  
        if self._eitherPosOrChord is None:
            self._eitherPosOrChord = self.wing.planform.wingSection_eitherPosOrChord

        if not self.isRootOrTip:
            if self.eitherPosOrChord:
                # either position or chord should be flexibel if planform defines sections
                if (not self._y is None) and (not self._norm_chord is None):
                    self._norm_chord = None
            else:
                # for trapezoid both position and chord must have a value 
                if self._norm_chord is None:    self._norm_chord = self.norm_chord
                if self._y is None:             self._y = self.y


        # create airfoil and load coordinates if exist 
        self._init_airfoil (dataDict = fromDict (dataDict, "airfoil", None))


    def _save (self) -> dict:
        """ returns the parameters of self as a dict"""

        myDict = {}
        toDict (myDict, "y",                self._y) 
        toDict (myDict, "norm_chord",       self._norm_chord) 
        toDict (myDict, "eitherPosOrChord", self.eitherPosOrChord) 
        toDict (myDict, "flap_group",       self.flap_group) 
        toDict (myDict, "hinge_xn",         self.hinge_xn) 
        toDict (myDict, "airfoil",          self.airfoil._save ({})) 

        return myDict


    def _init_airfoil (self, dataDict = None, pathFileName = None, workingDir=None):
        """create airfoil for section """
 
        # read data for airfoil either from dict of parameter file 
        #  or get filepath from user selection 
        # then create new 'Airfoil' 

        try: 
            if dataDict: 
                airfoil = Airfoil.onDict (dataDict, geometry=GEO_BASIC,
                                        workingDir= self.wing.pathHandler.workingDir)
            else: 
                airfoil = Airfoil (pathFileName= pathFileName, geometry=GEO_BASIC,
                                workingDir=workingDir)
        except:
            airfoil = None

        if airfoil is not None and airfoil.isExisting:
            airfoil.load()
        else:
            if self.isRoot: 
                airfoil = Root_Example()
            elif self.isTip:
                airfoil = Tip_Example()
            else:
                airfoil = Airfoil(name="<strak>", geometry=GEO_BASIC)
                airfoil.set_isBlendAirfoil (True)

        self.airfoil : Airfoil = airfoil 


    def __repr__(self) -> str:
        # overwrite class method to get a nice print string of self 
        if (self._y != None):
            info = f"y={self.y}mm"
        else:
            info = f"chord={self.norm_chord:.2f}" 
        return f"<{type(self).__name__} {info}>"

    #-----------------------------------------------------------

    @property
    def y (self):
        """
        y-position of wing section - either from fix position or calculated from chord position
        """
        if (self._y != None):
            pos =  self._y
        elif (self.isRoot):                     # enforce position 
            pos = 0.0
        elif (self.isTip):                      # enforce position
            pos = self.wing.halfwingspan
        elif (self._norm_chord != None): 
            pos = self.wing.planform.find_yFromChord (self._norm_chord * self.wing.chord_root) 
        else:
            pos = None
        return pos 

    def set_y (self, value : float):
        """
        set y-position of wing section to a fixed value - removing rel. chord setting
        """
        if (value == None):
            self._y = None
        elif (value < 0.0 or value > self.wing.halfwingspan):
            logger.error ("wingSection: Position must be inside half wingspan %dmm" %self.wing.halfwingspan)
        elif (self.isRoot or self.isTip):
            logger.error ("wingSection: Do not set root or tip chord via wing section")
        else:
            self._y = value
            if self.eitherPosOrChord:
                self._norm_chord  = None             # can't have fix position *and* relative chord setting
            else: 
                self._norm_chord  = self.norm_chord  # also fix chord (trapezoid)

    @property
    def yn (self):
        """ normed y-position of wing section 0..1
        """
        return self.y / self.wing.halfwingspan

    def set_yn (self, value):
        """
        set normed y-position of wing section 0..1 - removing rel. chord setting
        """
        self.set_y (value * self.wing.halfwingspan)


    @property
    def norm_chord (self):
        """ normalized chord 0..1 - either from property or calculated from position  """
        if   (not self._norm_chord is None): return self._norm_chord
        elif (not self._y is None): 
            return self.wing.planform._norm_chord_at (self.yn) 
        else:
            raise ValueError ("Wingsection: Both position and norm chord are not defined")

    def set_norm_chord (self, value):
        """ set the normaized chord 0..1 - the section will move to a new position   """
        if (value is None and self._y is None):
            logger.error ("wingSection: Can't remove normalized chord. Either position or chord must be defined")
        elif(value < 0.0):
            logger.error ("wingSection: Normalized chord must => 0")
        elif(value > 1.0):
            logger.error ("wingSection: Normalized chord must <= 1")
        elif (self.isRoot or self.isTip):
            logger.error ("wingSection: Do not set root or tip chord via wing section")
        else:
            self._norm_chord = value
            if self.eitherPosOrChord:
                self._y       = None         # remove other values, section is flex
            else: 
                self._y       = self.y    # also fix position (trapezoid)

    @property
    def chord (self):
        """ chord 0..chord_root - calculated from normalized chord """
        return self.norm_chord * self.wing.chord_root

    def set_chord (self, value):
        """ set the chord 0..chord_root - set to normalized chord  """
        self.set_norm_chord (value / self.wing.chord_root)


    @property
    def hinge_xn (self) -> float|None:
        """ hinge position 0..1 definition. At least root and tip must have a value """
        return self._hinge_xn
    def set_hinge_xn (self, aVal : float):
        aVal = max (0.0, aVal) 
        aVal = min (1.0, aVal)
        self._hinge_xn = round(aVal,4) 


    @property
    def flap_group (self):
        """ flap group beginning with this section """
        return self._flap_group
    def set_flap_group (self, value):
        self._flap_group = value

    @property
    def isRootOrTip (self): 
        return self.isRoot or self.isTip

    @property
    def Re (self):
        """ Reynolds number at this section calculated from chord """
        return self.norm_chord * self.wing.rootRe
    def set_Re (self, value):
        """ set Re number of this section - the section will move to a new position  """
        if (not value is None and value > 0) and (not self.isRootOrTip ):
            self.set_norm_chord  (value / self.wing.rootRe)

    @property
    def eitherPosOrChord (self) -> bool: 
        """ self may only be defined by user either by position or by chord.
        Is defined by planform typ. Within a trapezoidal a section may have both   """
        return self._eitherPosOrChord

    def set_eitherPosOrChord (self, aBool : bool):

        if self.isRootOrTip:
            self._eitherPosOrChord = False
        else:
            self._eitherPosOrChord = aBool

            if self._eitherPosOrChord:
                # either position or chord should be flexibel if planform defines sections
                if (not self._y is None) and (not self._norm_chord is None) :
                    self._norm_chord = None
            else:
                # for trapezoid both position and chord must have a value 
                if self._norm_chord is None:    self._norm_chord = self.norm_chord
                if self._y is None:             self._y = self.y


    def isSet_eitherPosOrChord_disabled (self): 
        """ change of fixPosChord of section allowed? only for trapezoid planform  """
        return self.wing.planform.wingSection_eitherPosOrChord or self.isRootOrTip


    @property
    def name (self) -> str:
        """ short unique name for wing section like "Section 7"""
        if self.isRoot:
            info = "Root"
        elif self.isTip:
            info = "Tip"
        else:
            info = "Section %s" % self.number ()
        return info

    @property
    def name_short (self) -> str:
        """ very short unique name for wing section like "7"""
        if self.isRoot:
            info = "Root"
        elif self.isTip:
            info = "Tip"
        else:
            info = f"{self.number()}"
        return info

    @property
    def label (self) -> str:
        """ short unique label for wing section like "7: at 1240mm """
        index = self.number()
        if self.isRoot:
            info = "Root"
        elif self.isTip:
            info = "Tip"
        elif (self._y != None):
            info = "at %.0fmm" %self._y
        elif (self._norm_chord != None):
            info = "with %.0fmm" % (self._norm_chord * self.wing.chord_root)
        elif (self.Re != None):
            info = "Re %.0f" %self.Re
        else:
            info = "flex" 
        return f"{index}: {info}"


    @property
    def has_fixed_y (self):
        """ has wing section a fixed y position within wing? """
        return (not self._y is None)


    @property
    def has_fixed_chord (self):
        """ has wing section a fixed chord within wing? """
        return (not self._norm_chord is None)


    # ---Methods --------------------- 

    def isReDisabled (self):
        """ true if Re calculation is not possible  - e.g. missing Re at root"""
        return (self.wing.rootRe is None or self.wing.rootRe <= 0) or (self.isRootOrTip)


    def adjust_to_wing (self, old_wingspan = None):
        """ adjust to modified wing data - correct position, chord etc 
        Args: oldSpan the wing span before modification
        """
        if self.isTip:
            self._y  = self.wing.halfwingspan  
            self._norm_chord = self.wing.chord_tip / self.wing.chord_root
        elif self.isRoot:
            self._y = 0.0
            self._norm_chord = 1.0 
        elif self.has_fixed_y: 
            if old_wingspan:
                self._y = self._y * self.wing.wingspan / old_wingspan  


    def fixChordAndPosition (self):
        """ sets the chord and yPos (for trapezoid planform at creation)
        """
        if self._y is None:   
            self._y = self.y                  # write calculated value fix into variable    
        if self._norm_chord is None: 
            self._norm_chord = self.norm_chord      # write calculated value fix into variable                
        return 
    

    def releaseFixedChordWithPosition (self):
        """ removes a fixed chord when position is defined
        (for not trapezoid planform at creation)
        """
        if ( self._y is None and self._norm_chord is None):
            raise ValueError ("Wingsection: Both position and norm chord are not defined")
        self._norm_chord = None      # remove fixed 
        return 


    def hasFixPosChord (self) -> bool:
        """ has wing section a fixed position and chord within wing? (for trapezoid planform)
        """
        return self._y is not None and self._norm_chord is not None
    
    def set_hasFixPosChord (self, aBool : bool):
        self.set_eitherPosOrChord (not aBool ) 
    

    def y_limits (self) -> tuple: 
        """ y position limits as tuple of self before touching the neighbour section
        """
        leftSec  : WingSection = None
        rightSec : WingSection = None

        leftSec, rightSec = self.wing.wingSections.neighbours (self) 

        if self.isRoot or self.isTip:
            safety = 0.0                # = fixed
            leftLimit  = self.y
            rightLimit = self.y
        else:
            safety = int (self.wing.halfwingspan / 500.0) 
            if leftSec: 
                leftLimit = leftSec.y
            else:
                leftLimit = self.y
            if rightSec: 
                rightLimit = rightSec.y
            else:
                rightLimit = self.y
        return (leftLimit + safety, rightLimit - safety)


    def yn_limits (self) -> tuple: 
        """ yn position limits as tuple of self before touching the neighbour section
        """
        left, right = self.y_limits()
        return ( left / self.wing.halfwingspan, right / self.wing.halfwingspan)


    def norm_chord_limits (self) -> tuple: 
        """ norm chord limits of self as tuple - must be between left and right neighbour
        """
        leftSec  : WingSection = None
        rightSec : WingSection = None

        leftSec, rightSec = self.wing.wingSections.neighbours (self) 

        if self.isRoot or self.isTip:
            safety = 1.0                # fixed
            upperLimit  = self.norm_chord
            lowerLimit  = self.norm_chord
        else:
            safety = 1.005              # = 0,5% 
            if leftSec: 
                upperLimit = leftSec.norm_chord
            else:
                upperLimit = self.norm_chord
            if rightSec: 
                lowerLimit = rightSec.norm_chord
            else:
                lowerLimit = self.norm_chord
        return lowerLimit * safety, upperLimit / safety 


    def chord_limits (self): 
        """  chord limits of self as tuple - must be between left and right neighbour
        """
        left, right = self.norm_chord_limits ()
        left  = left  * self.wing.chord_root
        right = right * self.wing.chord_root
        return (left, right)


    def Re_limits (self): 
        """  Re limits of self as tuple - must be between left and right neighbour
        """
        left, right = self.norm_chord_limits ()
        left  = left  * self.wing.rootRe
        right = right * self.wing.rootRe
        return (left, right)

    
    def airfoilNickPostfix(self): 
        """ the postfix like '-16' of airfoils nickname
        if there is rootRe take this else use normchord"""

        if self.wing.rootRe >= 1000000:
            return "-%03d" % int(self.Re/10000)
        elif self.wing.rootRe >= 100000:
            return "-%02d" % int(self.Re/10000)
        elif self.wing.rootRe >= 10000:
            return "-%02d" % int(self.Re/1000)
        elif self.wing.rootRe > 0:
            return "-%d" % int(self.Re)
        else:
            return "-%03d" % int(self.norm_chord * 100)
        
    def airfoilNick(self): 
        """ the airfoil nickname like GP-16 from nickname prefix and reynolds (or normchord)"""

        if self.wing.airfoilNickPrefix:
            return self.wing.airfoilNickPrefix + self.airfoilNickPostfix()
        else:
            return None

    def airfoil_canBeRemoved (self):
        return (not self.isRootOrTip) and (not self.airfoil.isStrakAirfoil)
    
    def airfoil_canBeEdited (self):
        return not self.airfoil.isStrakAirfoil
    
    def set_airfoilWithPathFileName (self, pathFileName):
        """ sets a new real Airfoil based on its path and loads it """

        relPathFile = self.wing.pathHandler.relFilePath (pathFileName)
        self._init_airfoil (pathFileName=relPathFile, workingDir= self.wing.workingDir)
        self.airfoil.load()



    def le_te_x (self) -> tuple:
        """ leading and trailing edge x value of self"""
        return self.wing.planform._planform_at (self.y)


    def line (self) -> tuple [list, list]:
        """ wing section as a line from LE to TE  """
        le, te = self.le_te_x ()
        y = self.y
        return [y, y], [le, te]


    def norm_line (self):
        """
        the wing section as a normed line from LE to TE
        Returns:
            y:  array of the y-stations of the line  
            xn: array of normed xn values of LE and TE 
        """
        y  = [self.y, self.y]
        xn_le = self.wing.planform._norm_chord_at (self.yn)
        xn = [xn_le, 0]
        return y, xn


    def number (self) -> int:
        """ number 1 (root) .. n (tip) of self within wingSections"""
        return self.wing.wingSections.number_of (self)


 





#-------------------------------------------------------------------------------

# Main program for testing 
if __name__ == "__main__":

    print ("Current directory: ",os.getcwd())
    filename = "..\\examples\\Amokka-JX\\Amokka-JX.json"
    # filename = ""
    myWing = Wing (filename)

