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
from model.polar_set        import Polar_Definition, Polar_Set
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
        self.parm_filePath = parm_filePath

        self.dataDict = dataDict

        self._name                  = fromDict (dataDict, "wing_name", "My new Wing")
        self._description           = fromDict (dataDict, "description", "This is just an example planform.\nUse 'New' to select another template.")
        self._fuselage_width        = fromDict (dataDict, "fuselage_width", 80.0)

        # polar definitions

        self._polar_definitions     = []
        for def_dict in fromDict (dataDict, 'polar_definitions', []):
            self._polar_definitions.append(Polar_Definition(dataDict=def_dict))

        # attach the Planform 

        self._planform              = Planform (self, dataDict)

        # reference planforms and background image  

        self._planform_elliptical   = None
        self._planform_ref_pc2      = None
        self._reference_pc2_file    = fromDict (dataDict, "reference_pc2_file", None)
        self._background_image      = None

        # paneled version of self planform    

        self._planform_paneled      = None

        # will hold the handler which manages export including its parameters

        self._export_airfoils       = None 
        self._export_xflr5          = None 
        self._export_flz            = None 
        self._export_dxf            = None 

        # miscellaneous parms

        self._airfoil_nick_prefix = fromDict (dataDict, "airfoil_nick_prefix", "JX-")
        self._airfoil_nick_base   = fromDict (dataDict, "airfoil_nick_base", 100)
        
        logger.info (str(self)  + ' created')



    def __repr__(self) -> str:
        # overwrite to get a nice print string 
        return f"{type(self).__name__} \'{self.name}\'"


    def _save (self) -> dict:
        """ returns the parameters of self as a dict"""

        dataDict = {}

        toDict (dataDict, "pc2_version", 2)
        toDict (dataDict, "wing_name",          self._name) 
        toDict (dataDict, "description",        self._description) 
        toDict (dataDict, "fuselage_width",     self._fuselage_width) 
        toDict (dataDict, "airfoil_nick_prefix",self._airfoil_nick_prefix) 
        toDict (dataDict, "airfoil_nick_base",  self._airfoil_nick_base) 
        toDict (dataDict, "reference_pc2_file", self._reference_pc2_file)
        toDict (dataDict, "background_image",   self.background_image._as_dict())

        def_list = []
        for polar_def in self.polar_definitions:
            def_list.append (polar_def._as_dict())
        toDict (dataDict,'polar_definitions', def_list)

        # save planform with all sub objects  

        self._planform._save_to (dataDict)

        toDict (dataDict, "panels",             self.planform_paneled._as_dict()) 
        if self._export_xflr5: 
            toDict (dataDict, "xflr5",          self._export_xflr5._as_dict()) 
        if self._export_flz: 
            toDict (dataDict, "flz",            self._export_flz._as_dict()) 
        if self._export_airfoils: 
            toDict (dataDict, "airfoils",       self._export_airfoils._as_dict()) 
        if self._export_dxf: 
            toDict (dataDict, "dxf",            self._export_dxf._as_dict()) 

        return dataDict


    def _convert_parm_file_v2 (self, dataDict :dict) -> dict:
        """ convert paramter file from version 1 to version 2"""

        logger.info (f"Converting parameters to version 2.0")

        dict_v2 = {} # copy.deepcopy (dataDict)  

        # wing 

        toDict (dict_v2, "pc2_version",     2)
        toDict (dict_v2, "wing_name",       fromDict (dataDict, "wingName", None))
        toDict (dict_v2, "description",     "< add a description >")

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

        chord_style = fromDict (dataDict, "planformType", N_Distrib_Bezier.name)

        chord_dict = {}

        toDict (chord_dict, "chord_style",   chord_style)

        if chord_style == 'Bezier' or chord_style == 'Bezier TE straight':
            toDict (chord_dict, "p1y",       fromDict (dataDict, "p1x", None))
            toDict (chord_dict, "p1x",       fromDict (dataDict, "p1y", None))
            toDict (chord_dict, "p2y",       fromDict (dataDict, "p2x", None))
            toDict (chord_dict, "p3y",       cn_tip)

        toDict (dict_v2, "chord_distribution",  chord_dict)
 
        # reference line 

        refDict = {}

        if chord_style == 'Bezier TE straight':
            # legacy planform 
            toDict (chord_dict, "chord_style",   N_Distrib_Bezier.name)
            toDict (dict_v2, "chord_distribution",  chord_dict)

            toDict (refDict, "p0y", 1.0)
            toDict (refDict, "p1y", 1.0)

        else:
            # flap hinge line -> reference line 
            f = fromDict (dataDict, "flapDepthRoot", None)
            if f:   toDict (refDict, "p0y", (100 - f)/100)
            f = fromDict (dataDict, "flapDepthTip", None)
            if f:   toDict (refDict, "p1y", (100 - f)/100)


        if chord_style == 'Bezier':
            refLineDict = {}
            toDict (refLineDict, "banana_p1y",fromDict (dataDict, "banana_p1x", None))
            toDict (refLineDict, "banana_p1x",fromDict (dataDict, "banana_p1y", None))
            toDict (dict_v2, "reference_line", refLineDict) 

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

                airfoilDict = fromDict (sectionDict, "airfoil", {})
                toDict (new_sectionDict, "airfoil",             fromDict (airfoilDict, "file", None))

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
    def name(self) -> str: 
        """name of wing""" 
        return self._name
    def set_name(self, aStr : str):  self._name = aStr

    @property
    def description (self) -> str: 
        """description of wing""" 
        return self._description if self._description is not None else ''
    def set_description(self, aStr : str):  self._description = aStr

    @property 
    def planform (self) -> 'Planform':
        """ planform object""" 
        return self._planform
    
    @property
    def wingspan (self) -> float:
        """ wingspan including fuselage""" 
        return self.planform.span * 2 + self.fuselage_width

    def set_wingspan (self, aVal : float):
        aVal = clip (aVal, 1, 50000)
        self.planform.set_span ((aVal - self.fuselage_width) / 2.0)


    @property
    def fuselage_width (self) -> float:
        """ width of fuselage"""
        return self._fuselage_width
    
    def set_fuselage_width (self, aVal:float):
        aVal = clip (aVal, 0, self.wingspan/2)
        self._fuselage_width = aVal 

    @property
    def wing_area (self) -> float:
        """ current wing area including fuselage"""
        
        fuselage_area = self.fuselage_width * self.planform.chord_root
        return self.planform.planform_area * 2 + fuselage_area

    @property
    def wing_aspect_ratio (self) -> float:
        """ aspect ratio of wing"""
        return self.wingspan ** 2 / self.wing_area


    @property
    def planform_paneled (self) -> 'Planform_Paneled':
        """ 
        shadow planform for self.planform which represents the panelled planform 
        which is the base for Xflr5 or FLZ export"""

        if self._planform_paneled is None:     
            self._planform_paneled = Planform_Paneled (self, dataDict = fromDict (self.dataDict, "panels", {})) 

        return self._planform_paneled


    @property
    def planform_elliptical (self) -> 'Planform':
        """ an elliptical reference norm planform having same span and chord reference """

        if self._planform_elliptical is None:      
            self._planform_elliptical = Planform (self, dataDict = self.dataDict, 
                                                        chord_style = N_Distrib_Elliptical.name,
                                                        chord_ref = self.planform.n_chord_ref )
        return self._planform_elliptical
    

    @property
    def reference_pc2_file (self) -> str:
        """ filename of optional PC2 reference planform"""
        return self._reference_pc2_file

    def set_reference_pc2_file (self, pathFilename : str) -> str:
        self._reference_pc2_file = pathFilename
        self._planform_ref_pc2   = None                         # reset current ref planform 


    @property
    def planform_ref_pc2_name (self) -> str:
        """Name of optional PC2 reference planform"""
        return self.planform_ref_pc2.wing.name if self.planform_ref_pc2 else None

    @property
    def planform_ref_pc2 (self) -> 'Planform':
        """ optional PC2 reference planform"""

        if self._planform_ref_pc2 is None and self._reference_pc2_file: 
            
            self._planform_ref_pc2 = Wing (self._reference_pc2_file).planform 

        return self._planform_ref_pc2
    
    @property
    def background_image (self) -> 'Image_Definition':
        """ returns the image definition of the background image"""
        if self._background_image is None: 
            self._background_image = Image_Definition (self.workingDir, 
                                                          fromDict (self.dataDict, "background_image", {}))
        return self._background_image

    @property
    def halfwingspan (self):    return (self.wingspan / 2)


    @property
    def airfoil_nick_prefix(self): return self._airfoil_nick_prefix
    def set_airfoil_nick_prefix(self, newStr): self._airfoil_nick_prefix = newStr

    @property
    def airfoil_nick_base(self) -> int:
        """ an integer as the base number at root e.g. 100""" 
        return self._airfoil_nick_base
    
    def set_airfoil_nick_base(self, aNumber : int): 
        try:
            self._airfoil_nick_base = int(aNumber) 
        except: 
            self._airfoil_nick_base = 100 


    @property
    def polar_definitions (self) -> list [Polar_Definition]:
        """ list of actual polar definitions """

        if not self._polar_definitions: 
            self._polar_definitions = [Polar_Definition()]
        return self._polar_definitions


    @property
    def export_xflr5 (self) : 
        """ returns exporter managing Xflr5 export """
        from wing_exports       import Export_Xflr5             # here - otherwise circular errors

        if self._export_xflr5 is None:                          # init exporter with parameters in sub dictionary
            xflr5_dict         = fromDict (self.dataDict, "xlfr5", "")
            self._export_xflr5 = Export_Xflr5 (self, self.planform_paneled, xflr5_dict) 
        return self._export_xflr5     


    @property
    def export_flz (self) : 
        """ returns exporter managing FLZ export """
        from wing_exports       import Export_FLZ               # here - otherwise circular errors

        if self._export_flz is None:                            # init exporter with parameters in sub dictionary
            flz_dict         = fromDict (self.dataDict, "flz", "")
            self._export_flz = Export_FLZ (self, self.planform_paneled, flz_dict) 
        return self._export_flz     


    @property
    def export_dxf (self): 
        """ returns class managing Dxf export """
        from wing_exports       import Export_Dxf               # here - otherwise circular errors

        if self._export_dxf is None:                            # init exporter with parameters in sub dictionary       
            dxf_dict         = fromDict (self.dataDict, "dxf", "")
            self._export_dxf = Export_Dxf(self, dxf_dict) 
        return self._export_dxf     


    @property
    def export_airfoils (self): 
        """ returns exporter managing airfoils export"""
        from wing_exports  import Export_Airfoils               # here - otherwise circular errors

        if self._export_airfoils is None:                       # init exporter with parameters in sub dictionary
            airfoilsDict          = fromDict (self.dataDict, "airfoils", "")
            self._export_airfoils = Export_Airfoils (self, airfoilsDict) 
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
            self.parm_filePath = pathFileName
        return saveOk


    def has_changed (self):
        """returns true if the parameters has been changed since last save() of parameters"""

        # compare json string as dict compare is too sensible 
        new_json = json.dumps (self._save())
        cur_json = json.dumps (self.dataDict)

        return new_json != cur_json
  
        
    def t_plan_to_wing_right (self, x : float|Array|list, y : float|Array|list) -> ...:
        """
        Transforms planform coordinates into wing coordinates as right side wing 
            - apply fuselage 
        Args:
            x,y: planform x,y, either float, list or Array to transform 
        Returns:
            x,y: transformed wing coordinates as float or np.array
        """

        # move for half fuselage
        if isinstance (x,float):
            t_x = x + self.fuselage_width / 2
        else: 
            t_x = np.array(x + self.fuselage_width / 2)

        return t_x, y 


    def t_plan_to_wing_left (self, x : float|Array|list, y : float|Array|list) -> ...:
        """
        Transforms planform coordinates into wing coordinates as right side wing 
            - apply fuselage 
            - mirror around y-axis 
        Args:
            x,y: planform x,y, either float, list or Array to transform 
        Returns:
            x,y: transformed wing coordinates as float or np.array
        """

        # move for half fuselage
        if isinstance (x,float):
            t_x = x + self.fuselage_width / 2
        else: 
            t_x = np.array(x + self.fuselage_width / 2)

        # mirror half wing 
        t_x = -t_x  

        return t_x, y 



#-------------------------------------------------------------------------------
# Normalized Chord Reference 
#-------------------------------------------------------------------------------


class N_Chord_Reference: 
    """ 

    The chord reference describes how much of the chord is given to Leading and Trailing edge.

    A chord reference function - returns cr at xn.
    e.g. cr = 0.8 means 80% of chord is towards le and 20% towards te from a horizontal reference 
 

    Wing
        |-- Planform
            |-- Norm_Planform
                |-- Norm_Distribution
                |-- Norm_Chord_Reference
                |-- Norm_Reference_Line
    """

    def __init__(self, dataDict: dict = None):

        # chord reference  - init linear Bezier control points for straight line    

        # ! self being quadratic Bezier (Banana) deactivated !
        #   Banana is handled with Reference_Line 
       
        px = [0.0,  1.0]
        py = [None, None]                                       

        py[0]   = fromDict (dataDict, "p0y", 0.75)                 
        py[1]   = fromDict (dataDict, "p1y", 0.75)                 

        # create Bezier for chord reference function

        self._cr_bezier  : Bezier = Bezier (px, py)
        self._cr_bezier_u = np.linspace(0.0, 1.0, num=20)                     # default Bezier u parameter (for polyline)


    def _as_dict (self) -> dict:
        """ returns a data dict with the paramters of self"""

        d = {}
        toDict (d, "p0y",        self._cr_bezier.points_y[0])
        toDict (d, "p1y",        self._cr_bezier.points_y[1])
        return d


    @property 
    def cr_root (self) -> float:
        """ cr value at root - typically 0.75"""
        return self._cr_bezier.points_y[0]
    
    def set_cr_root (self, aVal : float):
        px, _ = self._cr_bezier.points[0]
        py    = clip (aVal, 0.0, 1.0)
        self._cr_bezier.set_point (0, px, py)

    @property 
    def cr_tip (self) -> float:
        """ cr value at root - typically 0.75"""
        return self._cr_bezier.points_y[-1]
    
    def set_cr_tip (self, aVal : float):
        px, _ = self._cr_bezier.points[-1]
        py    = clip (aVal, 0.0, 1.0)
        self._cr_bezier.set_point (-1, px, py)



    def at (self, xn: float, fast=True) -> float:
        """ 
        Chord reference function - returns cr at xn
            e.g. cr = 0.8 means 80% of chord is towards le and 20% towards te
                from a horizontal reference 
            Higher Precision is achieved with interpolation of the curve (fast=False) 
        """

        return self._cr_bezier.eval_y_on_x (xn, fast=fast) 


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
                jpoint.set_y_limits ((0,2))
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
        # bez_px1 = round (self._cr_bezier.points_x[1], 3)               # avoid numerical issues 
        # bez_py1 = round (self._cr_bezier.points_y[1], 3)
        # set_no_banana = False
        # if not self.is_banana :
        #     if bez_px1 == round(px[1],3) and bez_py1 == round(py[1],3):
        #         set_no_banana = True 

        # update bezier 
        self._cr_bezier.set_points (px, py)





#-------------------------------------------------------------------------------
# Normalized Reference line 
#-------------------------------------------------------------------------------


class N_Reference_Line: 
    """ 

    The reference line is in normed coordinates a horizontal line at yn=0
    The coord distribution is applied at that line according to chord reference. 

    Optionally the referecne line can be a Bezier curve (banana function) which will 
    curve the planform like a bow 
 
    Wing
        |-- Planform
            |-- Norm_Planform
                |-- Norm_Distribution
                |-- Norm_Chord_Reference
                |-- Norm_Reference_Line
    """

    def __init__(self, dataDict: dict = None):


        # reference line  - init linear Bezier control points for straight line    
       
        px = [0.0, 1.0]
        py = [0.0, 0.0]                                       

        # handle banana

        banana_p1y = fromDict (dataDict, "banana_p1y", None)  
        banana_p1x = fromDict (dataDict, "banana_p1x", 0.0)
        if banana_p1y: 
            px.insert(1, banana_p1x)
            py.insert(1, banana_p1y)

        # create Bezier for chord reference function

        self._ref_bezier  : Bezier = Bezier (px, py)
        self._ref_bezier_u = np.linspace(0.0, 1.0, num=20)             # default Bezier u parameter (for polyline)


    def _as_dict (self) -> dict:
        """ returns a data dict with the paramters of self"""

        d = {}
        if self.is_banana:
            toDict (d, "banana_p1x",        self._ref_bezier.points_x[1])
            toDict (d, "banana_p1y",        self._ref_bezier.points_y[1])
        return d


    def at (self, xn: float | np.ndarray, fast=True) -> float:
        """ 
        reference line yn at xn  (noramlly = 0.0 except if Banana-Bezier)
            Higher Precision is achieved with interpolation of the curve (fast=False) 
        """
        if isinstance (xn, float) or isinstance (xn, int):
            if self._ref_bezier.npoints == 2:                       # optimize straight line 
                return 0.0 
            else: 
                return self._ref_bezier.eval_y_on_x (xn, fast=fast) 
        else: 
            yn = np.zeros (len(xn))
            if self.is_banana:
                for i in range (len(xn)):
                    yn[i] = self.at (xn[i], fast=fast)
            return yn



    def is_straight_line (self) -> bool:
        """ 
        returns True if Bezier is either a Line 
        or the 3 control points define (nearly a line)"""

        if self._ref_bezier.npoints == 2:
            return True
        else:

            px = self._ref_bezier.points_x
            py = self._ref_bezier.points_y

            # check if the middle bezier point is on line between the two others
            py_1_interpol = interpolate (px[0], px[2], py[0], py[2], px[1])
            delta_y = abs (py_1_interpol - py[1])
            return round (delta_y,2) == 0.0 


    @property
    def is_banana (self) -> bool:
        """ 
        True if chord reference is not linear but a curve meaning 
        banana function 
        """ 

        return self._ref_bezier.npoints > 2


    def set_is_banana (self, aBool):
        """ 
        set the reference function to a Bezier curve and not a straight line
          meaning banana function (Bezier is n=2) 
        """ 
        px = self._ref_bezier.points_x
        py = self._ref_bezier.points_y

        if len(px) == 3 and aBool == False:

            # remove control point in the middle -> straight line 
            del px [1]
            del py [1]

            self._ref_bezier.set_points (px, py)

        elif len(px) == 2 and aBool == True:

            # add cotrol point in the middle 
            px_new = 0.5
            py_new = interpolate (px[0], px[1], py[0], py[1], px_new)
            points = self._ref_bezier.points
            points.insert (1, (px_new, py_new))
            self._ref_bezier.set_points (points)


    def polyline (self) -> tuple [Array, Array]:
        """
        chord reference as polyline of cr 
        """

        if self.is_banana:
            xn, yr = self._ref_bezier.eval(self._ref_bezier_u) 
        else:
            xn = [0.0, 1.0]
            yr = [0.0, 0.0]

        return np.array(xn), np.array(yr) 


    def bezier_as_jpoints (self)  -> list[JPoint]:
        """ 
        chord reference Bezier control points as JPoints with limits and fixed property
            - in normed coordinates 
        """
        jpoints = []
        n = len (self._ref_bezier.points)

        for i, point in enumerate(self._ref_bezier.points):

            jpoint = JPoint (point)           

            if i == 0:                                      
                jpoint.set_fixed (True)
                jpoint.set_name ('Root')
            elif i == (n-1):                                    
                jpoint.set_fixed (True)
                jpoint.set_name ('Tip')
            elif i == 1:                                     
                jpoint.set_x_limits ((0,1))
                jpoint.set_y_limits ((-2,2))
                jpoint.set_name ("Banana")
            jpoints.append(jpoint)

        return jpoints


    def bezier_from_jpoints (self, jpoints : list[JPoint]): 
        """ 
        set reference line Bezier control points from JPoints   
        """

        px, py = [], []
        for jpoint in jpoints:
            px.append(jpoint.x )
            py.append(jpoint.y)

        # check if banana point was moved 
        bez_px1 = round (self._ref_bezier.points_x[1], 3)               # avoid numerical issues 
        bez_py1 = round (self._ref_bezier.points_y[1], 3)
        set_no_banana = False
        if not self.is_banana :
            if bez_px1 == round(px[1],3) and bez_py1 == round(py[1],3):
                set_no_banana = True 

        # update bezier 
        self._ref_bezier.set_points (px, py)

        # remain in no_banana if banana point wasn't moved
        if set_no_banana:
            self.set_is_banana (False)



#-------------------------------------------------------------------------------
# Normalized chord    
#-------------------------------------------------------------------------------


class N_Distrib_Abstract: 
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


    def __init__(self):
        """main constructor

        Args:       
            dataDict: the initial dataDict for self 
        """

        logger.info (str(self)  + ' created')


    def __repr__(self) -> str:
        return f"<{type(self).__name__}>"


    def _as_dict (self) -> dict:
        """ returns a data dict with the paramters of self"""

        d = {}
        toDict (d, "chord_style", self.__class__.name)
        return d


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
        return self.at (1.0)


    def polyline (self) -> Polyline:
        """ 
        Normalized polyline of chord along xn
            At root it is: cn [0] = 1.0 and cnn[0] = 0.0

        Returns:
            xn: normalized x coordinates
            cn: normalized chord
        """
        raise NotImplementedError



class N_Distrib_Bezier (N_Distrib_Abstract): 
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
        py = [1.0, 1.0, 0.55, 0.10]     

        # from dict only the variable point coordinates of Bezier

        px[1]   = fromDict (dataDict, "p1x", px[1])             # root tangent 
        py[1]   = fromDict (dataDict, "p1y", py[1])
        py[2]   = fromDict (dataDict, "p2y", py[2])             # nearly elliptic
        py[3]   = fromDict (dataDict, "p3y", py[3])             # defines tip chord 

        self._bezier = Bezier (px, py)
        self._u = np.linspace(0.0, 1.0, num=100)                # default Bezier u parameter


    def _as_dict (self) -> dict:
        """ returns a data dict with the paramters of self"""

        d = super()._as_dict()
        toDict (d, "p1x",       self._bezier.points_x[1])
        toDict (d, "p1y",       self._bezier.points_y[1])
        toDict (d, "p2y",       self._bezier.points_y[2])
        toDict (d, "p3y",       self._bezier.points_y[3])
        return d


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




class N_Distrib_Trapezoid (N_Distrib_Abstract):
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


    def __init__(self, planform : 'Planform'):
        """
        Create new chord distribution 

        Args:       
            planform: parent self belongs to - Trapezoidal need the wing sections
            dataDict: dictionary with parameters for self 
        """

        self._planform = planform 

        super().__init__ ()


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
        section : WingSection
        for section in self._planform.wingSections:
            if section.is_xn_cn_fix:
                sections.append(section)

        # collect their position and length 
        #        
        xn, cn = [], []
        for section in sections:
            xn.append(section.xn)
            cn.append(section.cn)
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




class N_Distrib_Paneled (N_Distrib_Abstract):
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


    def __init__(self, parent_planform : 'Planform', cn_tip_min=None):
        """
        Create new chord distribution 

        Args:       
            parent_planform: parent self belongs to (trapezoidal need the wing sections)
            dataDict: dictionary with parameters for self 
            cn_tip_min: Minimum chord at tip - span will be reduced 
        """

        self._parent_planform    = parent_planform
        self._cn_tip_min  = cn_tip_min

        self._is_cn_tip_min_applied = False                                     # flag for user info 

        super().__init__ ()


    @property
    def cn_tip_min (self) -> float: 
        """ the minimum normed chord at tip (will cut the tip)""" 

        # wing sections can change during lifetime - so dynamic check 
        cn_min = self._parent_planform.wingSections[-1].cn                 # cn of tip 
        cn_max = self._parent_planform.wingSections[1].cn                  # cn of 2nd section to ensure at least 2 sections
        self._cn_tip_min = clip (self._cn_tip_min, cn_min, cn_max)

        return round (self._cn_tip_min,3)                                  # calc of cn may have numerical issues 
    
    def set_cn_tip_min (self, aVal):
        """ set minimum - it can't be smaller than parent tip section cn"""

        cn_min = self._parent_planform.wingSections[-1].cn                 # cn of tip 
        cn_max = self._parent_planform.wingSections[1].cn                  # cn of 2nd section to ensure at least 2 sections
        self._cn_tip_min = clip (aVal, cn_min, cn_max)

    @property
    def is_cn_tip_min_applied (self) -> bool:
        """ True if sections were reduced due to tip_min"""
        return self._is_cn_tip_min_applied


    def polyline (self) -> Polyline:
        """ 
        Normalized polyline of chord along xn
            At root it is: cn [0] = 1.0 

        Returns:
            xn: normalized x coordinates
            cn: normalized chord
        """

        self._is_cn_tip_min_applied = False
        cn_tip_min = self.cn_tip_min

        # retrieve xn, cn of sections from parent chord 

        xn, cn = [], []
        section : WingSection
        for section in self._parent_planform.wingSections:
            if cn_tip_min is None or section.cn >= cn_tip_min :
                xn.append(section.xn)
                cn.append(section.cn)
            else:
                self._is_cn_tip_min_applied = True 

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



class N_Distrib_Elliptical (N_Distrib_Abstract):
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
# WingSection and WingSections
#-------------------------------------------------------------------------------


class WingSection : 
    """ 
    WingSection in planform

    A wingSection has (normally) either 
        - a fixed position xn (like root or tip section) 
        - a fixed chord cn    
    """


    def __init__(self, planform : 'Planform', dataDict: dict = None):
        super().__init__()
        """
        Main constructor

        Args:       
            dataDict: data dict having paramters for self . Defaults to None.
        """

        self._planform    = planform

        self._xn            = fromDict (dataDict, "xn", None)            # xn position
        self._cn            = fromDict (dataDict, "cn", None)            # cn chord  
        self._defines_cn    = fromDict (dataDict, "defines_cn", False)    # section must have cn and xn 
        self._hinge_cn      = fromDict (dataDict, "hinge_cn", None)      # hinge chord position 
        self._flap_group    = fromDict (dataDict, "flap_group", 1)       # flap group (starting here) 

        # sanity

        if self._xn is None and self._cn is None:
            raise ValueError (f"{self} init - either xn or cn is missing")

        if self._xn is not None: 
            self._xn = clip (self._xn, 0.0, 1.0)
            self._xn = round (self._xn, 10)
        if self._cn is not None: 
            self._cn = clip (self._cn, 0.0, 1.0)
            self._cn = round (self._cn, 10)

        # sanity root and tip  

        if self._planform.chord_defined_by_sections:

            # ... trapezoid planform: section may have both which will define planform  

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
    
            # ... bezier: either xn or cn

            self._defines_cn = False 
            # sanity 
            if self._xn is not None:
                self._cn = None 
            if self._xn is None and self._cn is None: 
                raise ValueError ("corrupted wing section data")  

        # create airfoil and load coordinates if exist 

        self._airfoil = self._get_airfoil (pathFileName = fromDict (dataDict, "airfoil", None), workingDir=self.workingDir)


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
        if not self.airfoil.isBlendAirfoil:
            toDict (d, "airfoil",    self.airfoil.pathFileName)
        return d


    @property
    def workingDir (self) -> str:
       """ current working directory""" 
       return self._planform.workingDir 


    def _get_airfoil (self, dataDict = None, pathFileName = None, workingDir=None) -> Airfoil:
        """read and create airfoil for section """
 
        # read data for airfoil either from dict of parameter file 
        #  or get filepath from user selection 
 
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
            if self.is_root: 
                airfoil = Root_Example()
            elif self.is_tip:
                airfoil = Tip_Example()
            else:
                airfoil = Airfoil(name="<strak>", geometry=GEO_BASIC)
                airfoil.set_isBlendAirfoil (True)

        # init polar set of airfoil 

        polar_defs = self._planform.wing.polar_definitions

        airfoil.set_polarSet (Polar_Set (airfoil, polar_def=polar_defs, re_scale=self.cn))

        return airfoil


    @property 
    def airfoil (self) -> Airfoil:
        """ airfoil of wing section"""
        return self._airfoil
    
    def set_airfoil (self, airfoil : Airfoil | str | None):
        """ 
        set new airfoil - 'airfoil' can be 
            - an Airfoil object 
            - None - current airfoil will by a strak airfoil
        """
        if isinstance (airfoil, Airfoil):
            # ensure airfoils path is relative to workingDir - if possible 
            rel_pathFileName = PathHandler(workingDir=self.workingDir).relFilePath (airfoil.pathFileName)
            airfoil.workingDir = self.workingDir
            airfoil.set_pathFileName (rel_pathFileName)

            self._airfoil = airfoil

        elif airfoil is None:
            # remove existing (set as strak airfoil) 
            self._airfoil = self._get_airfoil (pathFileName=None) 


    def set_airfoil_by_path (self, pathFileName : str | None):
        """ 
        set new airfoil by an airfoils pathFileName 
            - if None - current airfoil will by a strak airfoil
        """
        if os.path.isfile (pathFileName):
            # ensure relative path to working dir
            rel_pathFileName = PathHandler(workingDir=self.workingDir).relFilePath (pathFileName)
            self._airfoil = self._get_airfoil (workingDir = self.workingDir, 
                                               pathFileName =rel_pathFileName) 
        elif  pathFileName is None:
            # remove airfoil - set as strak 
            self._airfoil = self._get_airfoil (pathFileName=None) 


    @property
    def airfoil_nick_name (self) -> str:
        """ short nick name of airfoil"""
        prefix = self._planform.wing.airfoil_nick_prefix
        base   = self._planform.wing.airfoil_nick_base
        my_num = int(base * self.cn)
        return f"{prefix}{my_num}"

    @property
    def is_root (self) -> bool:
        """ True if self is root section"""
        return self._xn == 0.0 

    @property
    def is_tip (self) -> bool:
        """ True if self is tip section"""
        return self._xn == 1.0 

    @property
    def is_root_or_tip (self) -> bool:
        """ True if self is tip section"""
        return self.is_root or self.is_tip

    @property
    def xn (self) -> float:
        """ 
        xn position of self. If self is based on cn, the position will be evaluated from the planform
        """
        if self._xn is None:
            return self._planform.xn_at_cn (self.cn, fast=False)
        else: 
            return self._xn
        
    def set_xn (self, aVal: float):
        """ set new position - will switch self defined 'by pos' """

        if self.is_root_or_tip: 
            return
        if aVal is None:
            self._xn = None
        else:  
            aVal = clip (aVal, 0.0, 1.0)
            self._xn = round(aVal,10)

        if not self.defines_cn and self.is_xn_fix:                  # reset cn 
            self._cn = None 
        else:                                                       # tapezoid must have both
            if self._xn is None: self._xn = round(self.xn,10)           
            if self._cn is None: self._cn = round(self.cn,10) 

    @property
    def x (self) -> float:
        """
        x position of self. If self is based on cn, the position will be evaluated from the planform
        """
        return round(self.xn * self._planform.span, 6)
    
    def set_x (self, aVal: float):
        """ set new position - will switch self defined 'by pos' """
        self.set_xn (aVal / self._planform.span)


    @property
    def cn (self) -> float:
        """ 
        chord cn of self. If self is based on xn, the chord will be evaluated from the planform
        """
        if self._cn is None:
            return self._planform.cn_at (self.xn, fast=False, normed=True)
        else: 
            return self._cn

    def set_cn (self, aVal):
        """ set new chord - will switch self defined 'by chord' """

        if self.is_root: 
            return
        if aVal is None:
            self._cn = None
        else:  
            aVal = clip (aVal, 0.0, 1.0)
            self._cn = round(aVal,10)

        if not self.defines_cn: 
            if self.is_cn_fix:                                      # reset xn 
                self._xn = None 
        else:                                                       # tapezoid must have both
            if self._xn is None: self._xn = round(self.xn,10)           
            if self._cn is None: self._cn = round(self.cn,10) 

    @property
    def c (self) -> float:
        """
        chord of self. If self is based on cn, the position will be evaluated from the planform
        """
        return self.cn * self._planform.chord_root

    def set_c (self, aVal: float):
        """ set new chord - will switch self defined 'by chord' """
        self.set_cn (aVal / self._planform.chord_root)

    @property
    def is_cn_fix (self) -> bool:
        """ True if chord cn of self is fixed """
        return self._cn is not None

    @property
    def is_xn_fix (self) -> bool:
        """ True if position xn of self is fixed """
        return self._xn is not None

    @property
    def is_xn_cn_fix (self) -> bool:
        """ True if position xn and chord cn of self is fixed """
        return self.is_cn_fix and self.is_xn_fix 


    @property 
    def defines_cn (self) -> bool:
        """ section defines chord (e.g. trapezoidal)"""
        return self._defines_cn


    def set_defines_cn (self, aBool : bool):
        """ set section defines chord (e.g. trapezoidal)"""

        if not (self.is_root):
            # sanity - is it allowed in that planform
            if self._planform.chord_defined_by_sections:
                self._defines_cn = aBool
                if aBool: 
                    self.set_cn (self.cn)
                else: 
                    self.set_cn (None)       
        return True

    @property
    def is_set_xn_allowed (self) -> bool:
        """ True if setting of xn is allowed"""
        if self.is_root_or_tip:
            return False
        else:
            return True

    @property
    def is_set_cn_allowed (self) -> bool:
        """ True if setting of cn is allowed"""
        if self.is_root:
            return False                                                # chord at root always 1.0 
        elif self.is_tip:
            return self._planform.chord_defined_by_sections             # for trapezoid yes 
        else: 
            return True


    def index (self) -> int:
        """ index of self with wingSections"""
        return self._planform.wingSections.index (self)  


    def xn_limits (self) -> tuple:
         """ xn limits as tuple of self before touching the neighbour section """
         return self._planform.wingSections.xn_cn_limits_of (self) [0]

    def x_limits (self) -> tuple:
         """ x limits as tuple of self before touching the neighbour section """
         xn_limits = self.xn_limits()
         return xn_limits[0] * self._planform.span, xn_limits[1] * self._planform.span

    def cn_limits (self) -> tuple:
         """ cn limits as tuple of self before touching the neighbour section """
         return self._planform.wingSections.xn_cn_limits_of (self) [1]

    def c_limits (self) -> tuple:
         """ c chord limits as tuple of self before touching the neighbour section """
         cn_limits = self.cn_limits()
         return cn_limits[0] * self._planform.chord_root, cn_limits[1] * self._planform.chord_root


    @property
    def name_short (self) -> str:
        """ very short unique name for wing section like "7"""
        if self.is_root:
            info = "Root"
        elif self.is_tip:
            info = "Tip"
        else:
            info = f"{self.index()}"
        return info


    @property
    def hinge_equal_ref_line (self) -> bool:
        """ hinge is defined by reference line (convenience from flaps )"""
        return self._planform.flaps.hinge_equal_ref_line


    @property
    def hinge_cn (self) -> float:
        """ 
        relative hinge chord position cn of self
            - if no hinge positionen is defined, the calculated value from hingle line is taken
        """
        if self.defines_hinge:
            return self._hinge_cn
        else: 
            # calculate hinge position from flap depth 
            _, rel_depth = self._planform.flaps.flap_depth_at (self.x)
            hinge_cn = 1.0 - rel_depth 
            return hinge_cn


    def set_hinge_cn (self, aVal : float):
        """ set relative hinge chord position cn of self """
        if not self.hinge_equal_ref_line:
            aVal = clip (aVal, 0.0, 1.0)
            self._hinge_cn = round (aVal,10) 


    @property
    def hinge_c (self) -> float:
        """ relative hinge chord position c of self in planform  """
        return self.hinge_cn * self.c 


    def set_hinge_c (self, aVal : float):
        """ set relative hinge chord position c of self - or remove with None or -1.0"""
        self.set_hinge_cn (aVal / self.c)


    @property
    def hinge_y (self) -> float:
        """ hinge y position in planform - if no hinge pos is defined return -1.0 """
        le_y, _ = self.le_te()
        y = le_y + self.hinge_c 
        return y


    def set_hinge_y (self, y : float):
        """ set hinge chord position cn by y value within planform"""
        le_y, _ = self.le_te () 
        hinge_c = y - le_y
        hinge_c = clip (hinge_c, 0, self.c)

        self.set_hinge_cn(0) 
        self.set_hinge_cn (hinge_c / self.c) 


    @property
    def defines_hinge (self) -> bool:
        """ True if self defines a hinge position """
        return not (self._hinge_cn is None)


    def set_defines_hinge (self, aBool):
        """ make self a hinge line definer and init hinge_cn"""
        if aBool: 
            self.set_hinge_cn (self.hinge_cn)                           # will calculate the actual value
        else: 
            self._hinge_cn = None


    def hinge_remove (self):
        """ remove a individual hinge position of self"""
        if not self.hinge_equal_ref_line:
            self._hinge_cn = None


    @property
    def flap_group (self) -> float:
        """ flap group (starting) """
        return self._flap_group if self._flap_group is not None else 1

    def set_flap_group (self, aGroup : int):
        self._flap_group = aGroup 


    @property
    def flap_cn (self) -> float:
        """  relative flap depth cn of self like 0.25 """
        return 1.0 - self.hinge_cn 
    

    def set_flap_cn (self, aVal : float):
        """ set relative hinge chord position cn of self"""
        self.set_hinge_cn (1.0 - aVal) 


    @property
    def flap_c (self) -> float:
        """ 
        flap depth c of self like 64.7 (mm) 
        """
        return (1.0 - self.hinge_cn) * self.c 


    def set_flap_c (self, aVal : float):
        """ set relative hinge chord position c of self - or remove with None or -1.0"""
        self.set_hinge_c (self.c - aVal )


    def le_te (self) -> tuple [float,float]:
        """ leading and trailing edge y coordinate """
        le_y, te_y = self._planform.le_te_at (self.x)
        return le_y, te_y


    def line (self) -> Polyline:
        """ self as a poyline x,y within planform"""

        x          = self.x
        le_y, te_y = self.le_te ()
        return np.array([x, x]), np.array([le_y, te_y])


    def line_in_chord (self) -> Polyline:
        """ self as a poyline xn,yn within normed chord """

        xn          = self.xn
        return np.array([xn, xn]), np.array([self.cn, 0.0])


    def line_in_chord_ref (self) -> Polyline:
        """ self as a poyline xn,yn within chord reference which is just y[1]=1"""

        xn          = self.xn
        return np.array([xn, xn]), np.array([0.0, 1.0])




class WingSections (list):
    """ 
    container (list) for wing sections of a planform
    
            |-- Planform
                |-- Norm_Chord
                |-- WingSections
                    |-- WingSection

    """

    def __init__ (self, planform: 'Planform', sectionsDict: dict = {}):
        super().__init__ ([])

        self._planform = planform

        self._strak_done = False

        # create all sections based on sections list in dataDict 
        sections : list[WingSection] = []
        for sectionDict in sectionsDict:
            sections.append(WingSection (planform, sectionDict))

        # new wing - create default sections
        if not sections:
            logger.info ('Creating example wing sections')
            sections.append(WingSection (planform, {"xn": 0.0, "flap_group":1, "hinge_cn":0.70}))
            sections.append(WingSection (planform, {"cn": 0.6, "flap_group":2, "hinge_cn":0.70}))
            sections.append(WingSection (planform, {"xn": 1.0, "flap_group":2, "hinge_cn":0.75}))

        # sanity
        if not sections[0].is_root or not sections[-1].is_tip:
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
        section : WingSection
        for section in self: 
            section_list.append (section._as_dict ()) 
        return section_list


    def create_after (self, aSection: 'WingSection'=None, index=None) -> 'WingSection' : 
        """
        creates and inserts a new wing section after aSection 
            with a chord in the middle to the next neighbour 

        Return: 
            newSection: the new wingSection
        """

        if aSection is None and index is not None:
            aSection = self[index]

        new_section = None 

        if isinstance(aSection, WingSection) and not aSection.is_tip :

            _, right_sec = self.neighbours_of (aSection)

            new_cn = (aSection.cn + right_sec.cn) / 2
            new_flap_group = aSection.flap_group 

            new_section = WingSection (self._planform, {"cn": new_cn, "flap_group":new_flap_group})
            self.insert (self.index(aSection) + 1, new_section)

        return new_section


    def create_at (self, x : float, normed=False)  -> 'WingSection': 
        """
        create and insert a new wing section at pos x 

        Args:
            x:  either x oder xn depending on 'normed' to look for 
            normed: True x value is normed xn 
        Return: 
            newSection: the new wingSection, None if a new section couldn't be created 
        """
        if not normed:
            xn = x / self._planform.span
        else: 
            xn = x

        xn = clip (xn, 0.0, 1.0)

        # is there already a section? Return this one 

        exist_sec = self.at_x (xn, tolerance=0.002, normed=True)
        if exist_sec: return None 

        new_section = WingSection (self._planform, {"xn": xn})
        self.insert (1,new_section)                                     # insert section somewhere
        self.sort_by_xn ()                                               # and bring it in order 

        # set flap group of new to the left neighbour 
        left_sec, _ = self.neighbours_of (new_section) 
        new_section.set_flap_group (left_sec.flap_group)

        return new_section


    def delete (self, aSection: 'WingSection') -> WingSection: 
        """  delete wing section - return new current if succeeded"""

        if aSection and not (aSection.is_root_or_tip ):
            try:
                index = self.index (aSection)
                self.remove (aSection)
                return self[index-1] 
            except: 
                pass
        return None  


    def do_strak (self, geometry_class  = None): 
        """
        straks the airfoil of all wing sections having a Strak-Airfoil which is 
        created by blending with its real neighbours

        Args: 
            geometry: optional - the desired geometry of the new straked airfoils 
                                 either GEO_BASIC or GEO_SPLINE
        """
        section: WingSection

        for section in self:
            if section.airfoil.isBlendAirfoil: 

                # get the neighbour wing sections  with real airfoils 

                left_sec, right_sec = self.neighbours_having_airfoil(section) 
                cn = left_sec.cn
                blendBy  = (section.cn - left_sec.cn) / (right_sec.cn - left_sec.cn)

                # strak - set new geometry to achieve higher quality with splined airfoils 

                section.airfoil.set_name (left_sec.airfoil.name, reset_original=True)     # name will be <left_name>_blend0.6

                section.airfoil.do_blend (left_sec.airfoil,  right_sec.airfoil, blendBy, geometry_class)

                self._strak_done = True 


    @property
    def strak_done (self) -> bool:
        """ was there at least one strak calculation - so e.g. names are generated"""
        return self._strak_done


    def sort_by_xn (self):
        """ 
        Re-sort wing sections to an ascending xn pos. 
        When changing major wing parms sections could become out of sort order when
            they have fixed xn and chord mixed    
        """
        self.sort (key=lambda sec: sec.xn) 


    def neighbours_of (self, aSection: WingSection) -> tuple [WingSection, WingSection]:
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


    def xn_cn_limits_of (self, aSection: WingSection) -> tuple [tuple,tuple]: 
        """ 
        xn position and cn chord limits as tuple of self before touching the neighbour section
        """
        xn = aSection.xn
        cn = aSection.cn

        if aSection.is_tip and aSection.defines_cn:                             # special case trapezoid - tip section defines chord 
            left_sec, right_sec = self.neighbours_of (aSection)     
            return (xn,xn), (0.01, left_sec.cn) 
        if aSection.is_root_or_tip:                                             # normally root and tip fixed 
            return (xn,xn), (cn,cn) 
        else:
            left_sec, right_sec = self.neighbours_of (aSection)                 # keep a safety distance to next section
            safety = self[-1].xn / 500.0 
            if left_sec: 
                left_xn = left_sec.xn
                left_cn = left_sec.cn
            else:
                left_xn = xn
                left_cn = cn
            if right_sec: 
                right_xn = right_sec.xn
                right_cn = right_sec.cn
            else:
                right_xn = xn
                right_cn = cn
            if left_cn < right_cn:
                lower_cn, upper_cn = left_cn, right_cn
            else:  
                lower_cn, upper_cn = right_cn, left_cn
            return (left_xn + safety, right_xn - safety), (lower_cn, upper_cn)



    def neighbours_having_airfoil (self, aSection: 'WingSection') -> tuple [WingSection, WingSection]:
        """
        returns the neighbour before and after a wingSection, which are not blendAirfoil
        Needed for 'strak'  - if no neighbour return None 
        """

        try:
            index = self.index (aSection) 
        except: 
            return None, None

        sec: WingSection

        #left neighbour 
        if aSection.is_root: 
            left_sec = None
        else: 
            for sec in reversed(self [0:index]):
                if not sec.airfoil.isBlendAirfoil:
                    left_sec = sec
                    break 

        #right neighbour 
        if aSection.is_tip: 
            right_sec = None
        else: 
            for sec in self [index+1: ]:
                if not sec.airfoil.isBlendAirfoil:
                    right_sec = sec
                    break 
        return left_sec, right_sec


    def at_x (self, x : float, normed=False, tolerance = 0.01) -> 'WingSection':
        """
        returns the section at position x with an allowed 'tolerance' - else None

        Args:
            x:  either x oder xn depending on 'normed' to look for 
            normed: True x value is normed xn 
            tolerance:  normed tolerance for detection 
        """
        if not normed:
            xn = x / self._planform.span
        else: 
            xn = x

        section : WingSection = None
        for section in self:
            if abs( section.xn - xn) < tolerance :
                return section
        return None


    def refresh_polar_sets (self):
        """ refresh polar set of wingSections airfoil"""

        polar_defs = self._planform.wing.polar_definitions

        section : WingSection
        for section in self:
            airfoil = section.airfoil
            airfoil.set_polarSet (Polar_Set (airfoil, polar_def=polar_defs, re_scale=section.cn))



#-------------------------------------------------------------------------------
# Flap   
#-------------------------------------------------------------------------------




class Flap:
    """ 
    Outline of a single flap based on flap group in the wing sections  
    """
    def __init__(self, flaps: 'Flaps',
                 section_left: WingSection, section_right: WingSection):
        """
        Main constructor for new flap belonging to a wing 
        """
        self._flaps = flaps

        self.x_from = section_left.x
        self.x_to   = section_right.x

        self.flap_group   = section_left.flap_group
        self.section_name = section_right.name_short


    def __repr__(self) -> str:
        #nice print string of self 
        return f"<{type(self).__name__}>"


    @property         
    def planform (self) -> 'Planform':
        return self._flaps._planform


    @property
    def name (self) -> str:
        """ short unique name for flap like '2' """
        return f"{self.flap_group}"
    

    def polygon  (self) -> tuple [Array, Array]:
        """
        polyline x,y of self    
        """
        x,y = [], []

        # we start a TE at xn_from and going clockwise around the flap upto xn_to
        x.append(self.x_from)
        y.append(self.planform.le_te_at (self.x_from)[1])

        # go to hinge point
        x.append(self.x_from)
        y.append(self._flaps.hinge_y_at(self.x_from))

        # go along the hinge 
        x.append(self.x_to)
        y.append(self._flaps.hinge_y_at(self.x_to))

        # back to TE 
        x.append(self.x_to)
        y.append(self.planform.le_te_at (self.x_to)[1])

        # ... finally along TE to starting point back to TE
        x_arr, _, te_y = self.planform.le_te_polyline()
        i1 = min(bisect.bisect(te_y, self.x_from)-1, len(te_y) -2)    # get te coordinates between xn_from and yTo
        i2 = min(bisect.bisect(te_y, self.x_to)  -1, len(te_y) -2)
        y = np.append (y, np.flip(te_y[i1:i2+1]))  
        x = np.append (x, np.flip(x_arr[i1:i2+1]))

        return x, y



    def line_left  (self, x_offset=0) -> tuple [Array, Array]:
        """
        polyline x, y of self left side   
        """
        x_from = self.x_from + x_offset * self.planform.span
        x,y = [], []

        # we start a TE at xn_from and going clockwise around the flap upto xn_to
        x.append(x_from)
        y.append(self.planform.le_te_at (x_from)[1])

        # go to hinge point
        x.append(x_from)
        y.append(self._flaps.hinge_y_at (x_from))

        return np.array(x), np.array(y)


    def line_right  (self, x_offset=0) -> tuple [Array, Array]:
        """
        polyline x, y of self right side   
        """
        x_to   = self.x_to - x_offset * self.planform.span
        y,x = [], []

        # we start a TE at xn_from and going clockwise around the flap upto xn_to
        x.append(x_to)
        y.append(self.planform.le_te_at (x_to)[1])

        # go to hinge point
        x.append(x_to)
        y.append(self._flaps.hinge_y_at (x_to))

        return np.array(x), np.array(y)


    def line_hinge  (self, x_offset=0) -> tuple [Array, Array]:
        """
        hinge line x, y line of self    
        """
        x,y = [], []

        # go to hinge point
        x_from  = self.x_from + x_offset * self.planform.span
        hinge_y = self._flaps.hinge_y_at (x_from)  
        x.append(x_from)
        y.append(hinge_y)

        # go along the hinge 
        x_to    = self.x_to   - x_offset * self.planform.span
        hinge_y = self._flaps.hinge_y_at (x_to)  
        x.append(x_to)
        y.append(hinge_y)

        return np.array(x), np.array(y)


    def line_te  (self, x_offset=0) -> tuple [Array, Array]:
        """
        trailing edge polyline y,x of self    
        """
        x_from = self.x_from + x_offset * self.planform.span
        x_to   = self.x_to   - x_offset * self.planform.span
        y,x = [], []

        # we start a TE at xn_from  
        x.append(x_from)
        y.append(self.planform.le_te_at (x_from)[1])

        # ... along TE 
        x_arr, _, le_y = self.planform.le_te_polyline()
        i1 = min(bisect.bisect(x_arr, x_from)-1, len(x_arr) -2)    # get te coordinates between xn_from and yTo
        i2 = min(bisect.bisect(x_arr, x_to)  -1, len(x_arr) -2)
        x = np.append (x, x_arr[i1:i2+1])
        y = np.append (y, le_y [i1:i2+1])  

        # final TE point 
        x = np.append(x, x_to)
        y = np.append(y, self.planform.le_te_at(x_to)[1])
 
        return x,y


    def center (self) -> tuple [float, float]:
        """ center point of self"""

        x = (self.x_to + self.x_from) / 2

        te_y    = self.planform.le_te_at(x)[1]
        hinge_y = self._flaps.hinge_y_at(x)
        y       = (te_y + hinge_y) / 2

        return x, y 




class Flaps:
    """ 
    Creates dynamically list of Flaps from definitions in wingSections 

    Wing
        |-- Planform
            |-- Flaps
                |-- Norm_Flap
    """ 

    def __init__(self, planform: 'Planform', dataDict: dict = None):

        self._planform : Planform = planform

        self._hinge_equal_ref_line = fromDict (dataDict, "hinge_equal_ref_line", False)  

        self.check_and_correct ()                   # sanity checks of hinge and flap definitions    


    def _as_dict (self) -> dict:
        """ returns a data dict with the paramters of self"""

        d = {}
        toDict (d, "hinge_equal_ref_line",  self._hinge_equal_ref_line) 
        return d


    def _get_hinge_points  (self) -> tuple [Array, Array]:
        """
        definition points of hinge line as x,y    
        """
        if self.hinge_equal_ref_line:

            x, y =  self._planform.ref_polyline()                 # just take reference polyline 

        else: 

            x, y = [], []
            section : WingSection
            for section in self._wingSections:                          # collect all hinge xn definitions in sections
                if section.defines_hinge: 
                    x.append (section.x)
                    y.append (section.hinge_y)

        # sanity - at least 2 points and strictly increasing ? 

        if len (x) < 2 or not np.all(np.diff(x) > 0):
            logger.warning ("Hinge polyline definition is corrupted - will be reset")
            self.check_and_correct()
            x, y = self._get_hinge_points ()                                # try again 

        return x,y 


    def _get_hinge_cn_points  (self) -> tuple [Array, Array]:
        """
        relative normed chord at hinge definition points as xn,cn    
        """
        if self.hinge_equal_ref_line:

            xn, cn =  self._planform.n_chord_ref.polyline()             # just take reference polyline 

        else: 

            xn, cn = [], []
            section : WingSection
            for section in self._wingSections:                          # collect all hinge xn definitions in sections
                if section.defines_hinge: 
                    xn.append (section.xn)
                    cn.append (section.hinge_cn)

        return xn,cn 


    @property
    def _wingSections (self) -> WingSections:
        return self._planform.wingSections
        
    @property
    def hinge_equal_ref_line (self) -> bool: 
        """ True if hinge line equals reference line """
        return self._hinge_equal_ref_line
    
    def set_hinge_equal_ref_line (self, aBool : bool):
        self._hinge_equal_ref_line = aBool == True 


    def get (self) -> list[Flap]: 
        """
        returns the flap objects based on wing sections flap group
        """
        flapList = []
        if not self._wingSections: return flapList 

        # all succeeding sections with the same flap group define a new flap
        #   flap_group == 0 means no flap  

        start_section  = None
        section : WingSection

        for section in self._wingSections:

            if start_section is None and section.flap_group > 0:
                start_section = section
            elif start_section is None and section.flap_group == 0:
                pass
            elif (section.flap_group != start_section.flap_group or section == self._wingSections[-1]) :
                
                flapList.append(Flap(self, start_section, section))
                if section.flap_group > 0:
                    start_section = section 
                else:
                    start_section = None 

        return flapList
    

    def delete_hinge_point_ok (self, index : int) -> bool:
        """ is delete hinge point having index ok? Return True if it would be ok """

        if self.hinge_equal_ref_line: return False                  # only if hinge is defined on its own 

        x, _ = self._get_hinge_points() 

        if index == 0 or len(x) == 2:                               # don't delete root point and min 2 points
            return False                                 

        i = 0 
        sec : WingSection
        for sec in self._wingSections:                               # find section with hinge definition #index 
            if sec.hinge_cn: 
                if i == index:
                    return True 
                else: 
                    i += 1 
        return False


    def check_and_correct (self):
        """ check consistency of wingSections flap definition and correct if possible"""

        sections = self._wingSections

        root_sec : WingSection = sections[0]
        tip_sec  : WingSection = sections[-1]

        # if hinge is not equal ref_line - there must be at least 2 hinge definition sections 

        if not self.hinge_equal_ref_line:
            n = 0 
            for sec in sections: 
                if sec.defines_hinge: n += 1 

            if n < 2:
                root_sec.set_hinge_cn (0.75)
                tip_sec.set_hinge_cn (0.75)

        # there must be a new flap group when there is a hinge break
        #    - but not in "Amokka case" where the second last section defines flap depth 

        for i in range (1, len(sections)-1):
            left_sec : WingSection = sections[i-1]
            sec      : WingSection = sections[i]
 
            if sec.defines_hinge and not (not tip_sec.defines_hinge and i == (len(sections) -2)) :
                if left_sec.flap_group == sec.flap_group:
                    sec.set_flap_group (left_sec.flap_group+1)


    def hinge_cn_at (self, xn : float) -> float:
        """cn of hinge line at normed xn """  

        xn_arr, cn_arr = self._get_hinge_cn_points ()
        cn = np.interp(xn, xn_arr, cn_arr)                                  # linear interpolation 

        return cn
    

    def hinge_y_at (self, x : float) -> float:
        """y of hinge line at x """  

        x_arr, y_arr = self.hinge_polyline ()
        y = np.interp(x, x_arr, y_arr)                                  # linear interpolation 

        return y


    def hinge_polyline  (self) -> tuple [Array, Array]:
        """
        hinge line x,y to the very tip   
        """

        x, y = self._get_hinge_points ()

        # if left point isn't at tip, insert additional extrapolated point at tip 

        span = self._planform.span
        if x[-1] != span: 
            y_tip = interpolate (x[-2], x[-1], y[-2],y[-1], span)
            x.append (span)
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

        section : WingSection
        for section in self._wingSections:

            found_jpoint = None
            for jpoint in jpoints:

                # xn, yn = jpoint.as_transformed (transform_fn).xy
                if jpoint.x == section.x: 
                    found_jpoint = jpoint
                    break
             
            if found_jpoint: 
                section.set_hinge_y (jpoint.y) 
            else: 
                section.hinge_remove ()



    def flap_depth_at (self, x : float, hinge_y : float = None) -> float:
        """ 
        flap depth absolut e.g. 35mm and relative e.g. 0.27 at position x 
            if hinge_y is omitted it is calculated from current hinge line 
        
        """

        if hinge_y is None: 
            hinge_y  = self.hinge_y_at (x) 

        le_y, te_y = self._planform.le_te_at (x)                    # calc real flap depth 
        depth      = te_y - hinge_y
        rel_depth  = depth / (te_y - le_y)  
        rel_depth  = clip (rel_depth, 0.0, 1.0)                     # sanity    

        return depth, rel_depth 


    def flap_cn_polyline  (self) -> tuple [Array, Array]:
        """
        relative flap depth e.g. 0.27 (hinge positio) within chord reference    
        """

        # as the relative flap depth e.g. 0.27 is not a striaght line if the chord reference 
        # is defined by a curve, the relative flap depth has to be interpolated for each point 

        hinge_x, hinge_y = self.hinge_polyline ()

        x, le_y, te_y =  self._planform.le_te_polyline ()
        rel_depth = np.zeros (len(x))

        for i, xi in enumerate (x):
            hinge_yi = np.interp(xi, hinge_x, hinge_y)                            # linear interpolation 
            rel_depth[i] = (le_y[i] - hinge_yi) / (le_y[i] - te_y[i])

        return x / self._planform.span, rel_depth


    def flap_in_chord_polyline  (self) -> tuple [Array, Array]:
        """
        flap depth in chord distribution polyline which is flap depth 0.25 * local cn  
        """
        hinge_x, hinge_y = self.hinge_polyline ()

        x, le_y, te_y =  self._planform.le_te_polyline()
        depth = np.zeros (len(x))

        for i, xi in enumerate (x):
            hinge_yi = np.interp(xi, hinge_x, hinge_y)         # linear interpolation 
            depth[i] = (te_y[i] - hinge_yi) / self._planform.chord_root

        return x / self._planform.span, depth




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


    def insert_hinge_point_at (self, x : float) -> bool:
        """ 
        try to add a hinge 'break' at a section, which should be at x
            Return True if successful 
        """ 
        section = self._wingSections.at_x (x, tolerance = 0.01) 

        if section is not None: 
            if not section.defines_hinge:

                # calculate the current hinge position 
                x_arr, y_arr = self.hinge_polyline ()
                hinge_y = np.interp(x, x_arr, y_arr)                            # linear interpolation 
 
                # update section with hinge info 
                section.set_hinge_y (hinge_y)

                # ensure a new flap group will begin at this section 
                self.check_and_correct ()

                return True
        return False 




#-------------------------------------------------------------------------------
# Planform 
#-------------------------------------------------------------------------------


class Planform: 
    """ 

    Main object representing the planform of a wing half.
        having a chord at root, span and a sweep angle 

    Leading and Trailing edge are build based on its 

        Chord distribution 
            which defines a normalized chord along the (normalized) span 

        Chord reference
            which describes how much of the chord is given to Leading and Trailing edge
            The Chord reference is defined by a quadratic Bezier which allows 
            the planform to be bended from root to tip

    Wing
        |-- Planform
            |-- Norm_Chord
            |-- Norm_Chord_Reference
            |-- WingSections
                |-- WingSection
            |-- Flaps
                |-- Flap

    """

    def __init__(self, 
                 wing : Wing, 
                 dataDict: dict = None,
                 chord_style : str = None, 
                 chord_ref : N_Chord_Reference = None,
                 ref_line : N_Reference_Line = None):
                

        self._wing = wing

        self._span        = fromDict (dataDict, "halfspan", 1200.0) 
        self._chord_root  = fromDict (dataDict, "chord_root", 200.0)
        self._sweep_angle = fromDict (dataDict, "sweep_angle", 1.0)

        self._planform_area = None                                        # will be calculated
        self._planform_mac  = None                                        # mean aerodynamic chord - will be calculated

        # create Norm_Chord distribution depending on style e.g. 'Bezier'

        chord_dict = fromDict(dataDict, "chord_distribution", {})
        
        if chord_style is None: 
            chord_style = fromDict (chord_dict, "chord_style", N_Distrib_Bezier.name)

        if chord_style == N_Distrib_Bezier.name:
            self._n_distrib = N_Distrib_Bezier (chord_dict)

        elif chord_style == N_Distrib_Trapezoid.name:
            self._n_distrib = N_Distrib_Trapezoid (self)

        elif chord_style == N_Distrib_Elliptical.name:
            self._n_distrib = N_Distrib_Elliptical ()

        elif chord_style == N_Distrib_Paneled.name:
            self._n_distrib = N_Distrib_Paneled (self)
        else:
            raise ValueError (f"Chord style {chord_style} not supported")

        # init chord reference function either new from dataDict or as argument of self 

        if chord_ref is None:
            self._n_chord_ref = N_Chord_Reference (dataDict=fromDict(dataDict, "chord_reference", {}))
            self._is_slave    = False
        else: 
            self._n_chord_ref = chord_ref 
            self._is_slave    = True 

        # init reference line either new from dataDict or as argument of self

        if ref_line is None:
            self._n_ref_line = N_Reference_Line (dataDict=fromDict(dataDict, "reference_line", {}))
        else: 
            self._n_ref_line = ref_line 

        # init wing sections 
        
        self._wingSections    = WingSections (self, sectionsDict=fromDict(dataDict, "wingSections", {}))

        # init flaps

        self._flaps           = Flaps (self, dataDict=fromDict(dataDict, "flaps", {}))
         


    def _save_to (self, dataDict : dict) :
        """ save the parameters of self into dataDict"""

        toDict (dataDict, "halfspan",           self.span) 
        toDict (dataDict, "chord_root",         self.chord_root) 
        toDict (dataDict, "sweep_angle",        self.sweep_angle) 

        toDict (dataDict, "chord_distribution", self.n_distrib._as_dict()) 
        toDict (dataDict, "chord_reference",    self.n_chord_ref._as_dict()) 
        toDict (dataDict, "reference_line",     self.n_ref_line._as_dict()) 
        toDict (dataDict, "wingSections",       self.wingSections._as_list_of_dict()) 
        toDict (dataDict, "flaps",              self.flaps._as_dict()) 


    @property
    def wing (self) -> Wing:
        """ wing self belongs to"""
        return self._wing

    @property
    def name (self) -> str:
        """ name - default name of norm chord"""
        return self._n_distrib.name
    
    @property
    def workingDir (self) -> Wing:
        """ working directory of wing"""
        return self.wing.workingDir
    

    @property
    def span (self) -> float:
        """ wing half span"""
        if self._is_slave:
            return self.wing.planform.span
        else: 
            return self._span 

    def set_span (self, aVal : float):
        """ set span of halfwing"""
        aVal = max ( 0.01, aVal)
        self._span = aVal 


    @property
    def chord_root (self) -> float:
        """ chord at root section"""
        if self._is_slave:
            return self.wing.planform.chord_root
        else: 
            return self._chord_root 

    def set_chord_root (self, aVal : float):
        """ set chord at root """
        aVal = max ( 0.01, aVal)
        self._chord_root = aVal 


    @property
    def sweep_angle (self) -> float:
        """ sweep angle of reference line in degrees"""
        if self._is_slave:
            return self.wing.planform.sweep_angle
        else: 
            return self._sweep_angle 

    def set_sweep_angle (self, aVal : float):
        """ set sweep angle of reference line to aVal degrees"""

        aVal = max (-75.0, aVal)
        aVal = min ( 75.0, aVal)
        self._sweep_angle = aVal 

    @property
    def planform_area (self) -> float:
        """ (approximated) planform area"""

        if self._planform_area is None: 
            # this is normally done automatically each time the le, te poyline is calculated
            x, le_y, te_y = self.le_te_polyline ()
            self._planform_area = self._calc_planform_area (x, le_y, te_y)
        return self._planform_area

    @property
    def planform_mac (self) -> float:
        """ (approximated) mean aerodynamic chord"""

        if self._planform_mac is None: 
            # this is normally done automatically each time the le, te poyline is calculated
            x, le_y, te_y = self.le_te_polyline ()
            self._planform_mac = self._calc_mac (x, le_y, te_y)
        return self._planform_mac

    @property
    def chord_defined_by_sections (self) -> bool:
        """ True if planform is defined by sections e.g. trapezoid"""
        return self.n_distrib.chord_defined_by_sections


    @property
    def n_distrib (self) -> N_Distrib_Abstract:
        """ normalized chord distribution object """
        return self._n_distrib

    @property
    def n_chord_ref (self) -> N_Chord_Reference:
        """ normalized chord reference object """
        return self._n_chord_ref

    @property
    def n_ref_line (self) -> N_Reference_Line:
        """ normalized refrence line object """
        return self._n_ref_line

    @property
    def wingSections (self) -> WingSections:
        """ wingSections object (list)"""
        return self._wingSections

    @property
    def flaps (self) -> Flaps:
        """ flaps object which generates flaps"""
        return self._flaps

    @property
    def hinge_equal_ref_line (self) -> bool: 
        """ True if hinge line equals reference line """
        return self.flaps.hinge_equal_ref_line
    

    def cn_at (self, x: float, fast=True, normed=False) -> float:
        """ 
        Normalized chord at x or xn. At root cn is always 1.0 

        Args:
            x:      either x or xn position 
            fast:   True - only a linear interplation is made with low precision
            normed: True - x value is normed xn value
        Returns:
            cn:     normalized chord
        """
        if not normed:
            xn = x / self.span
        else: 
            xn = x
        return self.n_distrib.at (xn, fast=fast)


    def cn_polyline (self, normed=False) -> Polyline:
        """ 
        Polyline of normalized chord distribution cn

        Args:
            normed: True - x value is normed xn value
        Returns:
            x:      either x or xn  coordinates
            cn:     normalized chord values 
        """
        xn, cn = self.n_distrib.polyline()

        if not normed:
            x = xn * self.span
        else: 
            x = xn
        return x, cn


    def xn_at_cn (self, cn: float, fast=True) -> float:
        """ 
        xn at normed chord cn (interpolation)
 
        Args:
            cn:     a normalized chord value 
            fast:   True - only a linear interplation is made with low precision
        Returns:
            xn:     normalized x position
        """
        return self.n_distrib.xn_at (cn, fast=fast)


    def x_at_cn (self, cn: float, fast=True) -> float:
        """ 
        x at normed chord cn (interpolation)
 
        Args:
            cn:     a normalized chord value 
            fast:   True - only a linear interplation is made with low precision
        Returns:
            x:      x position 
        """
        return self.n_distrib.xn_at (cn, fast=fast) * self.span


    def le_te_at (self, x: float) -> tuple [float, float]:
        """ 
        Main planform function - returns le and te y coordinates
            At root: le_y = 0.0 and te_y = chord_root
        """

        xn = x / self.span

        xn, le_yn = self.t_chord_to_norm (xn, 1.0)
        xn, te_yn = self.t_chord_to_norm (xn, 0.0)

        le_y = self.t_yn_to_plan (le_yn, x)
        te_y = self.t_yn_to_plan (te_yn, x)

        return le_y, te_y 


    def c_at (self, x: float, fast=False) -> float:
        """ 
        chord at x. At root c is always chord_root 

        Args:
            x:      x position 
            fast:   True - only a linear interplation is made with low precision
        Returns:
            c:      chord
        """
        xn = x / self.span
        cn = self.n_distrib.at (xn, fast=fast)
        c  = cn * self.chord_root
        return c


    def le_te_polyline (self) -> Polylines:
        """ 
        Polylines of leading and trailing edge
            At root it is: le_y [0] = 0.0 and te_y[0] = chord_root

        Returns:
            x:      x coordinates
            le_y:   y coordinates of leading edge
            te_y:   y coordinates of leading edge
        """

        xn, cn = self.n_distrib.polyline()

        xn, le_yn = self.t_chord_to_norm (xn, np.full (len(xn), 1.0), cn=cn)
        xn, te_yn = self.t_chord_to_norm (xn, np.zeros(len(xn))     , cn=cn)

        x, le_y = self.t_norm_to_plan (xn, le_yn)
        x, te_y = self.t_norm_to_plan (xn, te_yn)

        # as we have the data, calc area 
        self._planform_area = self._calc_planform_area (x, le_y, te_y)
        self._planform_mac  = self._calc_mac (x, le_y, te_y)
        
        return x, le_y, te_y 


    def polygon (self) -> Polyline:
        """ 
        polygon of the planform starting at le_root clockwise 
        """
        x, le_y, te_y = self.le_te_polyline()
        return self._polygon (x, le_y, te_y)
    

    def _polygon (self, x : np.ndarray, le_y : np.ndarray, te_y : np.ndarray) -> Polyline:
        """ 
        polygon of the planform starting at le_root clockwise based on le, te poyline 
        """
        x = np.append (x, np.flip (x))
        x = np.append (x, x[0:1])
        y = np.append (le_y, np.flip (te_y))
        y = np.append (y, y[0:1])

        return x, y 


    def _calc_planform_area (self, x : np.ndarray, le_y : np.ndarray, te_y : np.ndarray):
        """ calc planform area based on le, te polyline """

        # create planform polygon 
        x, y = self._polygon (x, le_y, te_y) 

        # see https://stackoverflow.com/questions/24467972/calculate-area-of-polygon-given-x-y-coordinates
        correction = x[-1] * y[0] - y[-1]* x[0]
        main_area = np.dot(x[:-1], y[1:]) - np.dot(y[:-1], x[1:])
        
        half_area =  0.5*np.abs(main_area + correction)  
        return half_area


    def _calc_mac (self, x : np.ndarray, le_y : np.ndarray, te_y : np.ndarray):
        """calc mean aerodynamic chord """

        # calc integral of square chord along span 
        i_c2 = 0.0 
        for i in range (len(x) -1):
            dx = x[i+1] - x[i]
            c_mean = (te_y[i] - le_y[i] + te_y[i+1] - le_y[i+1]) / 2.0          # chord mean vlaue of dx 
            i_c2 += c_mean **2 * dx

        return i_c2 / self._calc_planform_area (x, le_y, te_y )
 

    def box_polygon (self) -> Polyline:
        """ 
        rectangle polygon x=0..span, y=0..chord root

        Returns:
            x: x coordinates
            y: y coordinates 
        """

        # in normed - ref line is at yn=0 - so correct with chord reference at x=0
        cr_0 = self.n_chord_ref.at (0)
        le_yn = 0.0 - cr_0
        te_yn = 1.0 - cr_0

        xn = np.array([0.0,   1.0,   1.0,   0.0,   0.0])
        yn = np.array([le_yn, le_yn, te_yn, te_yn, le_yn])

        x, y = self.t_norm_to_plan (xn, yn)

        return x, y


    def ref_polyline (self, normed=False) -> Polyline:
        """
        reference within a planform  which is normally a straight line
        except Banana (Bezier quadratic) is applied

        Args:
            normed: True - x value is normed xn value
        Returns:
            x: x coordinates
            y: y coordinates         
        """

        xn, yn = self.n_ref_line.polyline ()

        if normed: 
            return xn,yn
        else:
            # do not apply norm_to_plan as banana would be applied twice 
            return self.t_ref_to_plan (xn, yn)


    def t_chord_to_norm (self, xcn : float|Array|list, 
                               ycn : float|Array|list, 
                               cn : float|Array|list|None  =None) -> ...:
        """
        Transforms chord coordinates into normalized planform coordinates
            - by shiftung yn value dependand chord reference at xn  
        In normalized planform coordinates the reference line will be at yn=0, le_y poitive, te_y negative

        Args:
            xcn: xn normalized, either float, list or Array to transform 
            ycn: normalized chord coordinates, either float, list or Array to transform
            cn:  optional chord at xn - for optimization 

        Returns:
            xn: transformed x normed planform coordinates as float or np.array
            yn: transformed y normed planform coordinates as float or np.array
        """

        # LE ycn = 1.0, TE ycn = 0.0
        # cr = 1.0 --> 100% LE  

        if isinstance (xcn, float):
            cr  = self.n_chord_ref.at (xcn)                          # chord reference function

            if cn is None:
                cn  = self.n_distrib.at(xcn)                           # chord

            # apply chord reference function
            le_yn =   cn * cr                                
            te_yn = - cn * (1-cr)   
            yn = interpolate (0.0, 1.0, te_yn, le_yn, ycn)

            # flip yn 
            yn = - yn 

            yn = round (yn,10)
            xn = round (xcn, 10)

        else: 
            # array loop over each xn as y will be depending on the position 
            xn, yn = np.array (xcn), np.array (ycn)

            for i in range (len(xcn)):
                cni = cn[i] if cn is not None else None
                xn[i], yn[i] = self.t_chord_to_norm (xn[i], yn[i], cn=cni)

        return xn, yn 



    def t_norm_to_plan (self, xn : float|Array|list, yn : float|Array|list) -> ...:
        """
        Transforms normalized coordinates into planform(wing) coordinates 
            - move reference line so le_yn = 0 
            - apply banana
            - scale with chord and span 
            - shear with sweep angle 
        Args:
            xn,yn: normalized, either float, list or Array to transform 
        Returns:
            x,y: transformed planform coordinates as float or np.array
        """

        # sanity - convert list to np.array
        if isinstance (xn, float) or isinstance (xn, int):
            _xn, _yn = float(xn), float(yn)
        else: 
            _xn, _yn = np.array (xn), np.array (yn)

        # move reference line which is in norm at yn=0, so le_y at x=0 will be 0.0 
        cr_0 = self.n_chord_ref.at (0)
        _yn = _yn + cr_0

        # apply banana of reference line 
        _yn = _yn + self.n_ref_line.at (_xn, fast=False)

        # scale 
        x = _xn * self.span
        y = _yn * self.chord_root

        # do shear 
        shear_factor =  1 / np.tan((90-self.sweep_angle) * np.pi / 180)    # = cotangens
        y = y + x * shear_factor  

        return np.round(x,10), np.round(y,10) 



    def t_ref_to_plan (self, xn : float|Array|list, yn : float|Array|list) -> ...:
        """
        Transforms normalized reference line coordinates into planform(wing) coordinates 
            -> same as norm_to_plan *without* applying banana (it would be double banana)
        """

        # sanity - convert list to np.array
        if isinstance (xn, float) or isinstance (xn, int):
            _xn, _yn = float(xn), float(yn)
        else: 
            _xn, _yn = np.array (xn), np.array (yn)

        # move reference line which is in norm at yn=0, so le_y at x=0 will be 0.0 
        cr_0 = self.n_chord_ref.at (0)
        _yn = _yn + cr_0

        # scale 
        x = _xn * self.span
        y = _yn * self.chord_root

        # do shear 
        shear_factor =  1 / np.tan((90-self.sweep_angle) * np.pi / 180)    # = cotangens
        y = y + x * shear_factor  

        return np.round(x,10), np.round(y,10) 




    def t_yn_to_plan (self, yn : float|Array|list, x : float|Array|list, ) -> ...:
        """
        Transforms normalized yn coordinate into planform(wing) y coordinate
            - by scaling with chord  
            - by shearing with sweep angle 
        Args:
            yn: normalized, either float, list or Array to transform 
            x:  either float, list or Array to transform 
        Returns:
            y: transformed planform coordinates as float or np.array
        """

        # sanity - convert list to np.array
        if isinstance (yn, float):
            _xn, _yn = x / self.span, yn
        else: 
            _xn, _yn = np.array (x) / self.span, np.array (yn)

        _, y = self.t_norm_to_plan (_xn, _yn)

        return y



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
            _x, _y = x, y
        else: 
            _x, _y = np.array (x), np.array (y)

        # undo shear 
        shear_factor =  1 / np.tan((90-self.sweep_angle) * np.pi / 180)    # = cotangens
        _y = _y - _x * shear_factor

        # undo scale 
        xn = np.round(_x / self.span, 10)
        yn = np.round(_y / self.chord_root, 10)

        # undo banana of reference line 
        yn = yn - self.n_ref_line.at (xn, fast=False)

        # undo move reference line
        cr_0 = self.n_chord_ref.at (0)
        yn = yn - cr_0

        return xn, yn 




    def t_plan_to_ref (self, x : float|Array|list, y : float|Array|list) -> ...:
        """
        Transforms planform(wing) coordinates into normalized reference line coordinates  
            - reverse of 'transform_ref_to_plan' 
        """

        # sanity - convert list to np.array
        if isinstance (x, float):
            _x, _y = x, y
        else: 
            _x, _y = np.array (x), np.array (y)

        # undo shear 
        shear_factor =  1 / np.tan((90-self.sweep_angle) * np.pi / 180)    # = cotangens
        _y = _y - _x * shear_factor

        # undo scale 
        xn = np.round(_x / self.span, 10)
        yn = np.round(_y / self.chord_root, 10)

        # undo move reference line
        cr_0 = self.n_chord_ref.at (0)
        yn = yn - cr_0

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
            _xn, _yn = xn, yn
        else: 
            _xn, _yn = np.array (xn), np.array (yn)

        # scale 
        y = _yn
        x = np.round (_xn * self.span, 10)

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

        if isinstance (x, float):
            xn, yn = x, yn
        else: 
            # sanity - convert list to np.array
            xn, yn = np.array (x), np.array (yn)

        xn = np.round (xn / self.span, 10)

        return xn, yn 




class Planform_Paneled (Planform): 
    """ 

    Subclass of Planform reperesenting a paneled version of a (parent) planform 

    The chord distribution is a polyline of the wing sections le and te of the parent 

    Panelling is defined by the panel definition paramters 

    """

    def __init__(self, wing : Wing, dataDict: dict = None):


        self._wing = wing
        self._parent_planform = wing.planform

        self._span        = None                                        # will take it from parent 
        self._chord_root  = None
        self._sweep_angle = None


        # create Norm_Chord distribution based on parent planform 

        self._n_distrib     = N_Distrib_Paneled (self._parent_planform, dataDict)

        # main objects from parent

        self._n_chord_ref   = self._parent_planform.n_chord_ref 
        self._n_ref_line    = self._parent_planform.n_ref_line 
        self._wingSections  = self._parent_planform.wingSections
        self._flaps         = self._parent_planform.flaps

        # get panel paramters - x,y are in wing coordinate system (wy is along span)

        self._wy_panels      = fromDict (dataDict, "wy_panels", 8)
        self._wy_dist        = fromDict (dataDict, "wy_distribution", "uniform")

        self._wx_panels      = fromDict (dataDict, "wx_panels", 8)
        self._wx_dist        = fromDict (dataDict, "wx_distribution", "uniform")

        self._width_min      = fromDict (dataDict, "width_min", 0.02)                # min panel width 1%
        self.set_cn_tip_min   (fromDict (dataDict, "cn_tip_min", 0.05))              # min tip chord 10%
        self._cn_diff_max    = fromDict (dataDict, "cn_diff_max", 0.02)              # max cn difference 5%

        self._use_nick_name  = fromDict (dataDict, "use_nick_name", False)           # use airfoil nick name for export%

        self._is_width_min_applied = False                                           # flag for user info 
        self._cn_diff              = 0.0                                             # cuurent max chord difference

        # dict of available panel distribution functions used for x and y  

        self._wy_distribution_fns = {}
        self._wy_distribution_fns["uniform"]= lambda y : y
        self._wy_distribution_fns["-sine"]  = lambda y :  np.sin (y     * np.pi/2)
        self._wy_distribution_fns["sine"]   = lambda y : (np.sin ((y+1) * np.pi/2) - 1.0) * -1.0
        self._wy_distribution_fns["cosine"] = lambda y : (np.cos ((y+1) * np.pi) + 1) / 2

        self._wx_distribution_fns = {}
        self._wx_distribution_fns["uniform"]= lambda y : y
        self._wx_distribution_fns["cosine"] = lambda y : (np.cos ((y+1) * np.pi) + 1) / 2


    def _as_dict (self) -> dict:
        """ returns a data dict with the paramters of self"""
        d = {}
        toDict (d, "wy_panels",         self._wy_panels)
        toDict (d, "wy_distribution",   self._wy_dist)
        toDict (d, "wx_panels",         self._wx_panels)
        toDict (d, "wx_distribution",   self._wx_dist)
        toDict (d, "width_min",         self._width_min)
        toDict (d, "cn_tip_min",        self.cn_tip_min)
        toDict (d, "cn_diff_max",       self.cn_diff_max)
        return d
    

   # ---Properties --------------------- 

    @override
    @property
    def span (self) -> float:
        return self._parent_planform.span 

    @override
    @property
    def chord_root (self) -> float:
        return self._parent_planform.chord_root

    @override
    @property
    def sweep_angle (self) -> float:
        return self._parent_planform.sweep_angle


    def wingSections_reduced (self) -> list[WingSection]:
        """ returns list of wing sections if applicable reduced when cn_tip_min """
        
        if self.is_cn_tip_min_applied:
            nsec = len(self.n_distrib.polyline () [0])      # get effective no of sections fro polyline
            sections = self._wingSections [:nsec]           # reduce 
        else: 
            sections = self._wingSections
        return sections


    @property
    def wx_panels (self):                return self._wx_panels
    def set_wx_panels (self, val: int):  self._wx_panels = int(val)

    @property
    def wx_dist (self):                  return self._wx_dist
    def set_wx_dist (self, val):  
        if val in self._wy_distribution_fns:
            self._wx_dist = val

    @property
    def wy_panels (self):                return self._wy_panels
    def set_wy_panels (self, val: int):  self._wy_panels = int(val)

    @property
    def wy_dist (self):                  return self._wy_dist
    def set_wy_dist (self, val):  
        if val in self._wy_distribution_fns:
            self._wy_dist = val

    @property
    def width_min (self):              return self._width_min
    def set_width_min (self, val):     self._width_min = val

    @property
    def is_width_min_applied (self) -> bool:
        """ True if x_panels were reduced due to width_min"""
        return self._is_width_min_applied

    @property
    def cn_diff (self):                 return self._cn_diff

    @property
    def cn_diff_max (self):             return self._cn_diff_max
    def set_cn_diff_max (self, val):    self._cn_diff_max = val

    @property
    def is_cn_diff_exceeded (self) -> bool:
        """ True if chord difference exceeds setting"""
        return self.cn_diff > self.cn_diff_max


    @property
    def cn_tip_min (self) -> float:             
        """ minimum chord at tip when generating panels"""
        return self._n_distrib.cn_tip_min
    
    def set_cn_tip_min (self, aVal):
        self._n_distrib.set_cn_tip_min (aVal)

    @property
    def is_cn_tip_min_applied (self) -> bool:
        """ True if sections were reduced due to min tip chord"""
        return self._n_distrib.is_cn_tip_min_applied

    @property
    def wy_distribution_fns_names (self):
        """ a list of available distribution functions for wy - along span"""
        return list(self._wy_distribution_fns.keys())

    @property
    def wx_distribution_fns_names (self):
        """ a list of available distribution functions for wx - along chord"""
        return list(self._wx_distribution_fns.keys())

    @property
    def use_nick_name (self) -> bool:
        """ use airfoil nick name for export """
        return self._use_nick_name
    def set_use_nick_name (self, aBool : bool):
        self._use_nick_name = aBool == True


    def _xn_rel_stations (self, wy_panels=None) -> np.ndarray:
        """ 
        Relative xn stations of the panels of a section
         - an alternative wy_panels can be provided 
        """
        wy_panels = wy_panels if wy_panels is not None else self.wy_panels 

        wy_dist_fn = self._wy_distribution_fns [self.wy_dist]
        stations = np.linspace (0, 1, wy_panels +1)
        for i, xni in enumerate(stations):
            stations [i] = wy_dist_fn (xni)
        return np.round (stations,10)


    def _cn_rel_stations (self) -> np.ndarray:
        """ relative cn stations of the panels of a section"""
        wx_dist_fn = self._wy_distribution_fns [self.wx_dist]
        stations = np.linspace (0, 1, self.wx_panels +1)
        for i, yni in enumerate(stations):
            stations [i] = wx_dist_fn (yni)
        return np.round (stations,10)


    def _get_x_stations (self) -> np.ndarray:
        """
        x stations of all panels - optimized for width_min 
        """

        self._is_width_min_applied = False

        # walk along span by section and add x stations 

        xn_sec = self.n_distrib.polyline()[0]                     # take poyline as it can be already reduced

        xn_rel_stations = self._xn_rel_stations()
        xn_stations = np.array ([0.0])

        for isec in range (1, len(xn_sec)):      
            
            section_width   = xn_sec[isec] - xn_sec[isec-1]
            xn_sec_stations =  xn_rel_stations [1:] * section_width 

            # check and correct for min panel width 
            wy_panels = self.wy_panels
            while np.min (np.diff (xn_sec_stations)) < self._width_min and wy_panels > 2:
                wy_panels -= 1
                xn_rel_stations_tmp = self._xn_rel_stations(wy_panels)
                xn_sec_stations     =  xn_rel_stations_tmp [1:] * section_width 

                self._is_width_min_applied = True                           # flag for user info 

            xn_stations = np.append (xn_stations, xn_sec_stations + xn_sec[isec-1])

        return xn_stations * self._parent_planform.span
    

    def y_panel_polylines (self) -> tuple[list[Polyline], list[Polyline]]:
        """
        the lines from one section to the next along the span representing the y panels of the planform 
        Returns:
            :x: list of array of x-value of the line  
            :y: list of array of y-value 
        """

        x_list = []
        y_list = []

        # all xn stations along span - optimized for width min 

        xn_stations = self._get_x_stations () / self.span

        # now build and add for every y station a polyline - same like le, te is build 

        for cn_rel_station in self._cn_rel_stations():

            # array of constant cn values along span 
            cn_arr = np.full(len(xn_stations), cn_rel_station) 

            # transform from norm chord to norm planform 
            xn_arr, yn_arr = self.t_chord_to_norm (xn_stations, cn_arr)

            # transform to plan 
            x_arr, y_arr = self.t_norm_to_plan (xn_arr, yn_arr)

            # build list of poylines 
            x_list.append (x_arr)
            y_list.append (y_arr)

        return x_list, y_list



    def x_panel_polylines (self) -> tuple[list[Polyline], list[Polyline]]:
        """
        the lines from le to te representing the x panels of the planform 
        Returns:
            :x: list of array of x-value of the line  
            :y: list of array of y-value 
        """

        x_list = []
        y_list = []

        # all xn stations along span - optimized for width min 

        xn_stations = self._get_x_stations () / self.span

        # now build and add for every x station a line from le to te 

        for xni in xn_stations:

            xn_arr = np.array([xni, xni])
            cn_arr = np.array([0.0, 1.0])

            # transform from norm chord to norm planform 
            xn_arr, yn_arr = self.t_chord_to_norm (xn_arr, cn_arr)

            # transform to plan 
            x_arr, y_arr = self.t_norm_to_plan (xn_arr, yn_arr)

            # build list of poylines 
            x_list.append (x_arr)
            y_list.append (y_arr)

        return x_list, y_list


    def nx_panels_of_section (self, index : int) -> int:
        """ returns the number of x panels of section having index""" 

        left_sec_x  = self.wingSections [index].x

        # sanity 
        if index < (len(self._wingSections) - 1):
            right_sec_x = self.wingSections [index+1].x
        elif index == (len(self._wingSections) - 1):
            right_sec_x = left_sec_x
        if index > (len(self._wingSections) - 1):
            raise ValueError (f"Index {index} to get wing section is to high")        

        x_stations = self._get_x_stations ()

        npan = 0 
        for x in x_stations:
            if x > left_sec_x and x <= right_sec_x:
                npan += 1
            elif x > right_sec_x:
                break 

        return npan                   


    def c_diff_lines (self) -> list:
        """ returns a list with lines indicating the difference between chord paneled and chord parent"""

        self._cn_diff = 0.0

        x_stations = self._get_x_stations ()
        lines =[]

        for xi in x_stations:

            # actual chords at station 
            c_panel  = self.c_at (xi)
            c_parent = self._parent_planform.c_at (xi)
            c_diff   = c_parent - c_panel

            if c_diff > (self.cn_diff_max * self.chord_root):

                # get leading and trailing edge of paneled and of parent planform 
                le_y, te_y = self.le_te_at (xi)
                le_y_parent, te_y_parent = self._parent_planform.le_te_at (xi)
                
                # line between both at le 
                line_x, line_y = [xi, xi],  [le_y_parent, le_y]
                lines.append ((line_x, line_y))

                # line between both at te 
                line_x, line_y = [xi, xi],  [te_y_parent, te_y]
                lines.append ((line_x, line_y))

            self._cn_diff = max ((c_diff/self.chord_root), self.cn_diff)       # save max difference

        return lines 



    def optimize_cn_diff (self):
        """ insert new sections until chord difference is below max value """

        if not self.is_cn_diff_exceeded: return 

        i_cycle = 1

        while self.c_diff_lines() and i_cycle < 15:            # max iterations 

            sections = self.wingSections

            for i_sec in range (len(sections) - 1):

                # test chord difference in the middle of the section 
                xni = (sections[i_sec].xn + sections[i_sec+1].xn) / 2
                cn_panel  = self._n_distrib.at (xni)
                cn_parent = self._parent_planform.n_distrib.at (xni)
                if (cn_parent - cn_panel) > self.cn_diff_max:

                    # too much difference - insert section at mean cn value 
                    sections.create_after (index=i_sec)
                    break

            i_cycle += 1




class Image_Definition:
    """
    describes the properties of an image which can be used e.g. for background
    """

    def __init__(self, working_dir: str, myDict: dict = None):
 
        self._working_dir         = working_dir       
        self._pathFilename        = fromDict (myDict, "file", None)  

        self._mirrored_horizontal = fromDict (myDict, "mirrored_horizontal", False)
        self._mirrored_vertical   = fromDict (myDict, "mirrored_vertical", False)
        self._rotated             = fromDict (myDict, "rotated", False)
        self._invert              = fromDict (myDict, "invert", False)
        self._remove_red          = fromDict (myDict, "remove_red", False)

        self._black_level         = fromDict (myDict, "black_level", 40)            # 0..255 - take start value 
        # self._white_level         = fromDict (myDict, "white_level", 255) 

        self._point_le            = tuple(fromDict (myDict, "point_le", ( 20,-20))) 
        self._point_te            = tuple(fromDict (myDict, "point_te", (400,-20)))

        self._qimage              = None

        # sanity

        if self._pathFilename:
            if not os.path.isabs (self._pathFilename):                              # build absolute path
                self._pathFilename = PathHandler (workingDir= self._working_dir).fullFilePath (self._pathFilename)
            if not os.path.isfile (self._pathFilename):
                self._pathFilename = None


    def _as_dict (self) -> dict:
        """ returns a data dict with the paramters of self"""

        d = {}
        if self.pathFilename:

            # get relative path to working dir 
            relPath = PathHandler(workingDir= self._working_dir).relFilePath(self.pathFilename)
            toDict (d, "file",                  relPath) 

            toDict (d, "mirrored_horizontal",   self.mirrored_horizontal) 
            toDict (d, "mirrored_vertical",     self.mirrored_vertical) 
            toDict (d, "rotated",               self.rotated) 
            toDict (d, "invert",                self.invert) 
            toDict (d, "remove_red",            self.remove_red) 
            toDict (d, "black_level",           self.black_level) 
            # toDict (d, "white_level",           self.white_level) 
            toDict (d, "point_le",              self.point_le) 
            toDict (d, "point_te",              self.point_te) 
        return d


    @property
    def exists (self) -> bool:
        """ this is a valid iamge definition"""
        return self.pathFilename is not None
    
    @property
    def filename (self) -> str: 
        """ pathFilename of an image e.g. jpg"""
        return os.path.basename (self._pathFilename) if self._pathFilename is not None else None

    @property
    def pathFilename (self) -> str: 
        """ pathFilename of an image e.g. jpg"""
        return self._pathFilename
    
    def set_pathFilename (self, aPath : str):
        self._pathFilename = aPath
        self._qimage = None


    @property
    def mirrored_horizontal (self) -> bool:
        return self._mirrored_horizontal
    def set_mirrored_horizontal (self, aBool : bool):
        self._mirrored_horizontal = aBool
    
    @property
    def mirrored_vertical (self) -> bool:
        return self._mirrored_vertical
    def set_mirrored_vertical (self, aBool : bool):
        self._mirrored_vertical = aBool

    @property
    def rotated (self) -> bool:
        return self._rotated
    def set_rotated (self, aBool : bool):
        self._rotated = aBool

    @property
    def invert (self) -> bool:
        return self._invert
    def set_invert (self, aBool : bool):
        self._invert = aBool

    @property
    def remove_red (self) -> bool:
        return self._remove_red
    def set_remove_red (self, aBool : bool):
        self._remove_red = aBool

    @property
    def black_level (self) -> int:
        return self._black_level
    def set_black_level (self, aInt : int):
        self._black_level = aInt

    @property
    def white_level (self) -> int:
        """ white level - currently fixed to black level """
        return self.black_level + 128

    def set_white_level (self, aInt : int):
        "inactive"
        pass                        

    @property
    def point_le (self) -> tuple:
        return self._point_le
    def set_point_le (self, xy : tuple):
        self._point_le = xy

    @property
    def point_te (self) -> tuple:
        return self._point_te
    def set_point_te (self, xy : tuple):
        self._point_te = xy






#-------------------------------------------------------------------------------

# Main program for testing 
if __name__ == "__main__":

    print ("Current directory: ",os.getcwd())
    filename = "..\\examples\\Amokka-JX\\Amokka-JX.json"
    # filename = ""
    myWing = Wing (filename)

