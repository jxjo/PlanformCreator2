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
        |-- Planform_Mesh               - mesh derived from the planform for export and VLM
"""

import fnmatch
import os
import numpy as np
import numpy.typing as npt
import bisect
import shutil
import copy
from typing                 import override
from pathlib                import Path
from math                   import isclose
from time                   import perf_counter

from airfoileditor.base.math_util           import * 
from airfoileditor.base.spline              import * 
from airfoileditor.base.common_utils        import *

from airfoileditor.model.airfoil            import GEO_BASIC
from airfoileditor.model.polar_set          import Polar_Definition
from airfoileditor.model.xo2_driver         import Worker

from .VLM_wing                              import VLM_Wing
from .planform_mesh                         import Planform_Mesh
from .image_definition                      import Image_Definition
from .planform                              import (Planform, N_Distrib_Bezier,
                                                    N_Distrib_Trapezoid, N_Distrib_Elliptical)


import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


# ---- Typing -------------------------------------

type Array      = npt.NDArray[np.float64]



# ---- Model --------------------------------------

AIRFOILS_DIR_SUFFIX     = "_airfoils"
TEMP_STRAK_DIR          = "strak_temp"
FILENAME_NEW            = "new.pc2"

class Wing:
    """ 

    Main object - holds the model 

    """
    VAR_AIRFOILS_DIR = "${airfoils_dir}"
    STRAK_AIRFOIL_NAME = "<strak>"

    unit = 'mm'

    def __init__(self, parm_filePath : str|None, defaultDir : str|None = None):
        """
        Init wing from parameters in parm_filePath

        Args:
            parm_filePath (str): Path to the parameter file
            defaultDir (str): Default directory if parm_filePath is None or not valid
        """

        if parm_filePath and not os.path.isfile(parm_filePath):
            # non existing pc2 file
            logger.error (f".pc2 file '{parm_filePath}' does not exist (anymore) - creating default wing")
            self.pathHandler   = PathHandler (workingDir=defaultDir)
            self._parm_pathFileName = FILENAME_NEW
            p = {}

        else:

            p = Parameters (parm_filePath)
            if not p:
                logger.info (f'No input data - a default wing will be created in: {defaultDir}')
                # handler for the relative path to the parameter file (working directory)
                self.pathHandler   = PathHandler (workingDir=defaultDir)
                self._parm_pathFileName = FILENAME_NEW
            else: 
                parm_version = fromDict (p, "pc2_version", 1)
                logger.info (f"Reading wing parameters from '{parm_filePath}' (file version: {parm_version})")

                if parm_version == 1:
                    p = self._convert_to_v2 (p)
                elif parm_version == 2:
                    p = self._convert_to_v5 (p)

                # handler for the relative path to the parameter file (working directory)
                self.pathHandler = PathHandler (onFile=parm_filePath)
                self._parm_pathFileName = parm_filePath

        # ensure airfoil dir (tmp dir will be created in strak)
        self.create_airfoils_dir()

        self._parms : Parameters    = p

        self._name                  = p.get ("wing_name", "My new Wing")
        self._description           = p.get ("description", "This is just an example planform.\nUse 'New' to select another template.")
        self._fuselage_width        = p.get ("fuselage_width", 80.0)
        self._mass                  = p.get ("mass", None)
        self._halfspan              = p.get ("halfspan", 1200.0)
        self._chord_root            = p.get ("chord_root", 200.0)
        self._sweep_angle           = p.get ("sweep_angle", 0.0)

        # polar definitions

        self._polar_definitions     = []
        for def_dict in p.get ('polar_definitions', []):
            self._polar_definitions.append(Polar_Definition(dataDict=def_dict))

        # attach the Planform 

        self._planform              = Planform (self, p)

        # reference planforms and background image  

        self._planform_elliptical   = None
        self._ref_pc2_file          = p.get ("reference_pc2_file", None)
        self._background_image      = None

        # mesh derived from self planform

        self._planform_mesh         = None

        # will hold the handler which manages export including its parameters

        self._exporter_airfoils     = None 
        self._exporter_xflr5        = None 
        self._exporter_flz          = None 
        self._exporter_dxf          = None 
        self._exporter_csv          = None 

        # wing for VLM aero calculation    

        self._vlm_wing              = None

        # miscellaneous parms

        self._airfoil_use_nick    = p.get ("airfoil_use_nick", False)
        self._airfoil_nick_prefix = p.get ("airfoil_nick_prefix", "PC2-")
        self._airfoil_nick_base   = p.get ("airfoil_nick_base", 100)

        # if new wing save initial dataDict for change detection on save 

        if self.is_new_wing:
            self._parms = self._save()
        
        logger.info (str(self)  + ' created')



    def __repr__(self) -> str:
        # overwrite to get a nice print string 
        return f"<{type(self).__name__} {self.name}>"


    def _save (self) -> Parameters:
        """ returns the parameters of self as new Parameters"""

        VERSION = 5

        p = Parameters ()

        p.set ("pc2_version", VERSION)
        p.set ("wing_name",          self._name) 
        p.set ("description",        self._description) 
        p.set ("fuselage_width",     self._fuselage_width) 
        if self._mass is not None:
            p.set ("mass",            self._mass)
        p.set ("airfoil_use_nick",   self._airfoil_use_nick)
        p.set ("airfoil_nick_prefix",self._airfoil_nick_prefix) 
        p.set ("airfoil_nick_base",  self._airfoil_nick_base) 
        # Convert reference file path to forward slashes for cross-platform storage
        reference_file = self._ref_pc2_file.replace(os.sep, '/') if self._ref_pc2_file else None
        p.set ("reference_pc2_file", reference_file)
        p.set ("background_image",   self.background_image._as_dict())

        # polar definitions - do not save if there is only a default definition 
        def_list = []
        for polar_def in self.polar_definitions:
            def_dict = polar_def._as_dict()
            if not (len (self.polar_definitions) == 1 and def_dict == Polar_Definition()._as_dict()):
                def_list.append (polar_def._as_dict())
        p.set ("polar_definitions", def_list)

        # save planform with all sub objects  

        self._planform._save_to (p)

        # save exporters

        p.set ("panels", self.planform_mesh._as_dict()) 

        if self._exporter_xflr5:
            p.set ("xflr5", self._exporter_xflr5._as_dict()) 
        if self._exporter_flz:
            p.set ("flz", self._exporter_flz._as_dict()) 
        if self._exporter_airfoils:
            p.set ("airfoils_export", self._exporter_airfoils._as_dict()) 
        if self._exporter_dxf:
            p.set ("dxf", self._exporter_dxf._as_dict()) 
        if self._exporter_csv:
            p.set ("csv", self._exporter_csv._as_dict()) 

        return p


    def _convert_to_v2 (self, dataDict :dict) -> dict:
        """ convert parameter file from version 1 to version 2"""

        logger.info (f"Converting parameters to version 2")

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

        if chord_style == "trapezoidal":
            chord_style = N_Distrib_Trapezoid.name

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


    def _convert_to_v5 (self, dataDict :dict) -> dict:
        """ convert parameter file from version 2 to version 5"""

        logger.info (f"Converting parameters to version 5")

        dict_v5 = copy.deepcopy (dataDict)  

        toDict (dict_v5, "pc2_version", 5)

        # convert paneling info to new structure

        data_panels = fromDict (dataDict, "panels", {})
        if fromDict (data_panels, "wx_panels", None):
            data_trapezoidal = {}
            toDict (data_trapezoidal, "trapezoidal", data_panels)
            dict_v5.pop("panels", None)
            toDict (dict_v5, "panels", data_trapezoidal)

        return dict_v5


    # ---Properties --------------------- 

    @property
    def name(self) -> str: 
        """name of wing""" 
        return self._name
    def set_name(self, aStr : str):  self._name = aStr

    @property
    def is_new_wing(self) -> bool:
        """ True if wing has not been saved yet (new wing) """
        return self.parm_fileName == FILENAME_NEW

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
        return self.halfspan * 2 + self.fuselage_width

    def set_wingspan (self, aVal : float):
        aVal = clip (aVal, 1, 50000)
        self.set_halfspan ((aVal - self.fuselage_width) / 2.0)


    @property
    def halfspan (self) -> float:
        """Wing half-span, excluding the fuselage."""
        return self._halfspan

    def set_halfspan (self, aVal : float):
        self._halfspan = max (0.01, aVal)


    @property
    def chord_root (self) -> float:
        """Root chord shared by this wing's planforms."""
        return self._chord_root

    def set_chord_root (self, aVal : float):
        self._chord_root = max (0.01, aVal)


    @property
    def sweep_angle (self) -> float:
        """Sweep angle shared by this wing's planforms, in degrees."""
        return self._sweep_angle

    def set_sweep_angle (self, aVal : float):
        self._sweep_angle = clip (aVal, -75.0, 75.0)


    @property
    def fuselage_width (self) -> float:
        """ width of fuselage"""
        return self._fuselage_width
    
    def set_fuselage_width (self, aVal:float):
        aVal = clip (aVal, 0, self.wingspan/2)
        self._fuselage_width = aVal 


    def wing_data (self) -> tuple[float, float, float, float, float]:
        """
        derived wing data from geometry
            - all together for performance reasons
        
        Returns:
            area: total wing area including fuselage [mm┬▓]
            ar: aspect_ratio including fuselage
            mac: mean aerodynamic chord [mm]
            np: geometric neutral point in chord direction (x,y) [mm]
        """

        planform_area, mac, mac_le_y, np = self.planform.calc_area_mac_np ()
        fuselage_area = self.fuselage_width * self.planform.chord_root

        wing_area     = planform_area * 2 + fuselage_area 
        wing_ar       = self.wingspan ** 2 / wing_area

        return wing_area, wing_ar, mac, mac_le_y, np


    @property
    def mass (self) -> float:
        """Mass of the wing in kilograms."""
        if self._mass is None:
            # initialize mass from a typical model-aircraft wing loading
            typical_wing_loading = 50.0  # g/dm┬▓
            wing_area, _, _, _, _ = self.wing_data()
            self._mass = wing_area * typical_wing_loading / 10_000_000.0
        return self._mass

    def set_mass (self, aVal: float):
        if aVal is not None:
            aVal = clip (aVal, 0.01, 1000.0)
            self._mass = aVal

    @property
    def wing_loading (self) -> float:
        """Wing loading in g/dm┬▓."""
        wing_area_mm2, _, _, _, _ = self.wing_data()
        if wing_area_mm2 <= 0:
            return 0.0
        return self.mass * 1000.0 * 10_000.0 / wing_area_mm2

    def set_wing_loading (self, aVal: float):
        """ set wing loading in g/dm┬▓ - will set mass accordingly"""
        if aVal is not None:
            aVal = clip (aVal, 1.0, 1000.0)
            wing_area_mm2, _, _, _, _ = self.wing_data()
            if wing_area_mm2 > 0:
                self._mass = aVal * wing_area_mm2 / (1000.0 * 10_000.0)


    @property
    def planform_mesh (self) -> Planform_Mesh:
        """ 
        mesh derived from self.planform as the base for Xflr5, FLZ, and VLM"""

        if self._planform_mesh is None:     
            self._planform_mesh = Planform_Mesh (self.planform, dataDict = fromDict (self._parms, "panels", {})) 

        return self._planform_mesh


    @property
    def planform_elliptical (self) -> 'Planform':
        """ an elliptical reference norm planform having same span and chord reference """

        if self._planform_elliptical is None:      
            self._planform_elliptical = Planform (self, dataDict = self._parms, 
                                                        chord_style = N_Distrib_Elliptical.name,
                                                        chord_ref = self.planform.n_chord_ref )
        return self._planform_elliptical
    

    @property
    def ref_pc2_file (self) -> str:
        """ filename of optional PC2 reference planform"""
        return self._ref_pc2_file

    def set_ref_pc2_file (self, pathFilename : str) -> str:
        if pathFilename is None or os.path.isfile (pathFilename):
            self._ref_pc2_file = pathFilename


    def handle_airfoil_change (self):
        """ handle airfoil change in VLM wing """

        # ensure all wing sections have straked airfoils
        if not self.planform.wingSections.strak_done:
            self.planform.wingSections.do_strak (geometry_class=GEO_BASIC)

        # ensure all wingSections have a polar with the current re
        self.planform.wingSections.refresh_polar_sets (reset=False)

        if self._vlm_wing:
            self._vlm_wing.handle_airfoil_change()



    @property
    def vlm_wing (self) -> VLM_Wing:
        """ wing for VLM aero calculation """

        if self._vlm_wing is None: 

            # create new VLM_Wing - mesh with current wing sections will be created if needed
            self._vlm_wing = VLM_Wing (self)

            # ensure all wing sections have straked airfoils
            if not self.planform.wingSections.strak_done:
                self.planform.wingSections.do_strak (geometry_class=GEO_BASIC)

            # ensure all wingSections have a polar with the current re
            self.planform.wingSections.refresh_polar_sets (reset=False)

        return self._vlm_wing


    def vlm_wing_reset (self):
        """ reset (will init new) VLM wing"""
        self._vlm_wing = None


    @property
    def vlm_data_available (self) -> bool:
        """ check if VLM polar is available"""

        vlm_wing : VLM_Wing = self._vlm_wing
        if vlm_wing is None:
            return False   
        return bool (vlm_wing._polars)


    @property
    def background_image (self) -> 'Image_Definition':
        """ returns the image definition of the background image"""
        if self._background_image is None: 
            self._background_image = Image_Definition (self.workingDir, 
                                                          fromDict (self._parms, "background_image", {}))
        return self._background_image

    @property
    def halfwingspan (self):    return (self.wingspan / 2)


    @property
    def airfoil_use_nick(self) ->  bool: 
        """ True if airfoil nick names shall be used for display and export (default) """
        return self._airfoil_use_nick if self._airfoil_use_nick else False
    def set_airfoil_use_nick(self, aBool : bool): self._airfoil_use_nick = aBool == True

    @property
    def airfoil_nick_prefix(self): 
        """ prefix string for airfoil nick names e.g. 'JX-GP-' """
        return self._airfoil_nick_prefix if self._airfoil_nick_prefix else 'PC2-'
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
    def exporter_xflr5 (self) : 
        """ returns exporter managing Xflr5 export """
        from .wing_exports       import Exporter_Xflr5             # here - otherwise circular errors

        if self._exporter_xflr5 is None:                          # init exporter with parameters in sub dictionary
            xflr5_dict         = fromDict (self._parms, "xflr5", "")
            self._exporter_xflr5 = Exporter_Xflr5 (self, self.planform_mesh, xflr5_dict) 
        return self._exporter_xflr5     


    @property
    def exporter_flz (self) : 
        """ returns exporter managing FLZ export """
        from .wing_exports       import Exporter_FLZ               # here - otherwise circular errors

        if self._exporter_flz is None:                            # init exporter with parameters in sub dictionary
            flz_dict         = fromDict (self._parms, "flz", "")
            self._exporter_flz = Exporter_FLZ (self, self.planform_mesh, flz_dict) 
        return self._exporter_flz     


    @property
    def exporter_dxf (self): 
        """ returns class managing Dxf export """
        from .wing_exports       import Exporter_DXF               # here - otherwise circular errors

        if self._exporter_dxf is None:                            # init exporter with parameters in sub dictionary       
            dxf_dict         = fromDict (self._parms, "dxf", "")
            self._exporter_dxf = Exporter_DXF(self, dxf_dict) 
        return self._exporter_dxf     


    @property
    def exporter_csv (self): 
        """ returns class managing Csv export """
        from .wing_exports       import Exporter_CSV               # here - otherwise circular errors

        if self._exporter_csv is None:                            # init exporter with parameters in sub dictionary       
            csv_dict         = fromDict (self._parms, "csv", "")
            self._exporter_csv = Exporter_CSV(self, csv_dict) 
        return self._exporter_csv     


    @property
    def exporter_airfoils (self): 
        """ returns exporter managing airfoils export"""
        from .wing_exports  import Exporter_Airfoils               # here - otherwise circular errors

        if self._exporter_airfoils is None:                       # init exporter with parameters in sub dictionary
            airfoilsDict          = fromDict (self._parms, "airfoils_export", "")
            self._exporter_airfoils = Exporter_Airfoils (self, airfoilsDict) 
        return self._exporter_airfoils     

    @property
    def parm_pathFileName (self):
        """ path and filename of the parameter file like './my_dir/VJX.pc2' relative to working dir"""
        return self._parm_pathFileName 

    @property
    def parm_fileName (self):
        """ filename of the parameter file like 'VJX.pc2' """
        return os.path.basename(self._parm_pathFileName) if self._parm_pathFileName else ''


    @property
    def parm_fileName_stem (self):
        """ stem of fileName like 'VJX' """
        return Path(self.parm_fileName).stem if self.parm_fileName else ''


    @property
    def parm_pathFileName_abs (self):
        """ absolute path and filename of the parameter file like 'c:/my_dir/VJX.pc2' """
        if self.workingDir:
            pathFileName_abs =  os.path.join(self.workingDir, self.parm_pathFileName)
        else: 
            pathFileName_abs =  self.parm_pathFileName
        
        if not os.path.isabs (pathFileName_abs):
            pathFileName_abs = os.path.abspath(pathFileName_abs)       # will insert cwd 
        return pathFileName_abs


    @property
    def workingDir(self): 
        """directory of the parameter file"""
        return self.pathHandler.workingDir


    @property
    def tmp_dir (self) -> str: 
        """
        directory within wing_airfoils_dir for tmp files like blended airfoils and polars
            returns absolute path e.g. <workingDir>/VJX_airfoils/strak_temp
        """

        tmp_dir = os.path.join (self.airfoils_dir, TEMP_STRAK_DIR)
        return tmp_dir


    @property
    def airfoils_dir_rel (self) -> str: 
        """ 
        directory within working dir for airfoils of this wing 
            returns relative path e.g. ./VJX_airfoils
        """

        if self._parm_pathFileName is None: 
            airfoils_dir = Path(FILENAME_NEW).stem + AIRFOILS_DIR_SUFFIX
        else:
            airfoils_dir = Path(self._parm_pathFileName).stem + AIRFOILS_DIR_SUFFIX 
        return airfoils_dir
    

    @property
    def airfoils_dir (self) -> str: 
        """ 
        directory within working dir for airfoils of this wing 
            returns absolute path e.g. <workingDir>/VJX_airfoils
        """

        airfoils_dir = os.path.join (self.workingDir, self.airfoils_dir_rel)
        return airfoils_dir


    # ---Methods --------------------- 

    def _copy_background_image (self, target_dir : Path) -> bool:
        """ 
        Copy the background image of this wing to newDir
        - only if the current imgae path is relative to the current working dir

        Args:
            target_dir: directory of the new parameter file
        Returns:
            True if succeeded, False if failed
        """

        if not self.background_image.pathFilename:
            return True             # no background image defined - nothing to do 

        target_path = target_dir

        current_dir_path = Path(self.parm_pathFileName_abs).parent
        current_dir_abs  = current_dir_path.resolve(strict=False)
        target_dir_abs   = target_path.resolve(strict=False)

        if os.path.normcase(str(current_dir_abs)) == os.path.normcase(str(target_dir_abs)):
            return True             # same dir - nothing to do 

        source_image_path = Path(self.background_image.pathFilename_abs)
        if not source_image_path.is_file():
            logger.error (f"Cannot copy background image - source file '{self.background_image.pathFilename_abs}' does not exist")
            return False 

        image_rel_path = Path(self.background_image.pathFilename)
        if image_rel_path.is_absolute():
            logger.info (f"Cannot copy background image - source file '{self.background_image.pathFilename}' is absolute path")
            return True             # do not copy absolute path - just keep the path as is

        try:
            new_image_path = target_dir_abs / image_rel_path
            new_image_path.parent.mkdir(parents=True, exist_ok=True)
            if not new_image_path.is_file():
                shutil.copy2(source_image_path, new_image_path)
            logger.info (f"Copied background image '{self.background_image.pathFilename_abs}' to '{target_dir_abs}'")
            return True 

        except Exception as e:
            logger.error (f"Copying background image '{self.background_image.pathFilename_abs}' to '{target_path}' failed: {e}")
            return False



    def _copy_airfoils_dir (self, target_dir : Path) -> bool:
        """ copy the airfoils dir of this wing to target_dir without polars and tmp files
        Args:
            target_dir: absolute directory path for copied airfoil files
        Returns:
            True if succeeded, False if failed
        """

        source_dir = Path(self.airfoils_dir)
        if not source_dir.is_dir():
            logger.error (f"Cannot copy airfoils - source dir '{source_dir}' does not exist")
            return False 

        target_path = target_dir

        if target_path.is_dir() and source_dir.samefile(target_path):
            # same dir - nothing to do 
            return True 

        try:
            if target_path.is_dir():
                shutil.rmtree(target_path, ignore_errors=True)
            target_path.mkdir(parents=True, exist_ok=False)

            # copy all airfoil files except polars and tmp dir
            dat_files = [p.name for p in source_dir.glob('*.dat')]
            bez_files = [p.name for p in source_dir.glob('*.bez')]
            hh_files  = [p.name for p in source_dir.glob('*.hicks')]
            airfoil_files = dat_files + bez_files + hh_files

            for fname in airfoil_files:
                shutil.copy2(source_dir / fname, target_path)
            logger.info (f"Copied airfoils dir '{source_dir}' to '{target_path}'")
            return True 

        except Exception as e:
            logger.error (f"Copying airfoils dir '{source_dir}' to '{target_path}' failed: {e}")
            return False


    def create_airfoils_dir (self): 
        """ create the default dir for airfoils """

        # ensure airfoils dir exists
        if not os.path.isdir (self.airfoils_dir):
            os.makedirs (self.airfoils_dir, exist_ok=True)
    

    def create_tmp_dir (self): 
        """ create dir for temporary files (strak) made during session"""

        # ensure tmp dir exists
        if not os.path.isdir(self.tmp_dir):
            os.makedirs (self.tmp_dir, exist_ok=True)


    def remove_airfoils_dir_not_needed (self): 
        """ 
        remove airfoils dir of this wing including polars and tmp files
            - if it is empty
            - or of a new wing which has not been saved yet
        """

        if os.path.isdir(self.airfoils_dir):
            if self.is_new_wing:
                shutil.rmtree(self.airfoils_dir, ignore_errors=True)
            else:
                try:
                    os.rmdir(self.airfoils_dir)  # only removes if empty
                except OSError:
                    pass  # directory not empty or other error


    def remove_tmp (self): 
        """ remove temporary files made during session"""

        # remove tmp dir of strak airfoils 
        if os.path.isdir(self.tmp_dir):
            shutil.rmtree(self.tmp_dir, ignore_errors=True)

        # remove persisted example airfoil its polar dir 
        for section in self.planform.wingSections:
            if section.airfoil.isExample:
                if os.path.isfile (section.airfoil.pathFileName_abs):
                    os.remove (section.airfoil.pathFileName_abs)

                polarDir = str(Path(section.airfoil.pathFileName_abs).with_suffix('')) + '_polars'
                Worker.remove_polarDir (section.airfoil.pathFileName_abs, polarDir) 


    def save (self, newPathFilename : str | None = None) -> bool:
        """ store data dict to file pathFileName
        Args:
            newPathFilename: optional path and filename of the parameter file absolute or relative to working dir   

        Returns: 
            True : if succeeded, False if failed
        """
        parms = self._save()

        # get new absolute path and filename of the parameter file
        if newPathFilename is None:
            pathFileName_abs = Path(self.parm_pathFileName_abs)
        else:
            new_path = Path(newPathFilename)
            pathFileName_abs = new_path if new_path.is_absolute() else Path(self.workingDir) / new_path
 
        # set new location of parms file and save parms 
        parms.set_pathFileName (str(pathFileName_abs))

        try:
            parms.save()
            save_ok = True
            logger.info (f"{self} saved to '{pathFileName_abs}'")
        except Exception as e:
            logger.error (f"Saving wing parameters to file '{pathFileName_abs}' failed: {e}")
            save_ok = False

        if save_ok:
            # keep dataDict for later change detection 
            self._parms = parms  

            if newPathFilename:

                target_dir  = pathFileName_abs.parent

                # copy airfoils to new airfoils dir if file name changed
                new_airfoils_dir = target_dir / (Path(newPathFilename).stem + AIRFOILS_DIR_SUFFIX)
                self._copy_airfoils_dir (new_airfoils_dir)

                # copy background image only when saving into a new directory
                self._copy_background_image (target_dir)

                # set the current working Dir to the dir of the new saved parameter file            
                self.pathHandler.set_workingDirFromFile (str(pathFileName_abs))
                self._parm_pathFileName = pathFileName_abs.name         # only the file name relative to working dir

                # reinit planform with wing sections having new airfoils 
                self._planform = Planform (self, parms)
                self._planform_mesh = None                              # reset planform mesh
                self._vlm_wing = None                                   # reset VLM wing
                self._background_image = None                           # reset to reload from new working dir and/or location

        return save_ok


    def set_parm_fileName_new (self, fileName_stem : str):
        """ set new file name for the parameter file - only the file name relative to working dir
            used after 'Save As' operation
        Args:
            fileName_stem: new file name like 'my_wing' with extension '.pc2' added automatically
        """

        pathFileName_abs = self.parm_pathFileName_abs

        # first rename airfoils dir if existing 
        new_airfoils_dir = os.path.join (os.path.dirname(pathFileName_abs), 
                                        fileName_stem + AIRFOILS_DIR_SUFFIX)    
        old_airfoils_dir = self.airfoils_dir

        if os.path.isdir(old_airfoils_dir):
            try:
                if os.path.isdir(new_airfoils_dir):
                    shutil.rmtree(new_airfoils_dir, ignore_errors=True)
                shutil.move(old_airfoils_dir, new_airfoils_dir)
            except Exception as e:
                logger.error(f"Renaming airfoils dir '{old_airfoils_dir}' to '{new_airfoils_dir}' failed: {e}")
                return

        # rename param file name

        new_pathFileName_abs = os.path.join (os.path.dirname(pathFileName_abs), fileName_stem + ".pc2")
        if os.path.isfile (pathFileName_abs):
            try:
                os.rename (pathFileName_abs, new_pathFileName_abs)
            except Exception as e:
                logger.error(f"Renaming parameter file '{pathFileName_abs}' to '{new_pathFileName_abs}' failed: {e}")
                # rollback airfoils dir rename
                if os.path.isdir(new_airfoils_dir):
                    try:
                        shutil.move(new_airfoils_dir, old_airfoils_dir)
                    except Exception as e2:
                        logger.error(f"Rolling back airfoils dir rename from '{new_airfoils_dir}' to '{old_airfoils_dir}' failed: {e2}")
                return

            self._parm_pathFileName = fileName_stem + ".pc2"


    def has_changed (self):
        """returns true if the parameters has been changed since last save() of parameters"""

        # compare json string as dict compare is too sensible 
        new_dict = self._save()
        cur_dict = self._parms

        # Option 1: Simple check with detailed comparison if different
        new_json = json.dumps(new_dict, sort_keys=True)
        cur_json = json.dumps(cur_dict, sort_keys=True)
        
        if new_json != cur_json:
            # Find differences
            for key in set(list(new_dict.keys()) + list(cur_dict.keys())):
                if new_dict.get(key) != cur_dict.get(key):
                    logger.debug(f"Changed key '{key}': {cur_dict.get(key)} -> {new_dict.get(key)}")
        
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
# Reference Wing  
#-------------------------------------------------------------------------------


class Scale_Mode (StrEnum):
        
        NO_SCALE = \
                    "No scaling"
        MATCH_SPAN_AND_AREA = \
                    "Match span and area"
        MATCH_ROOT_CHORD_AND_AREA = \
                    "Match root chord and area"
        MATCH_ROOT_CHORD_AND_SPAN = \
                    "Match root chord and span"


class Reference_Wing (Wing):

    def __init__ (self, 
                  parm_filePath: str,
                  parent_wing : Wing,
                  scale_mode = Scale_Mode.MATCH_SPAN_AND_AREA,
                  **kwargs):

        self._parent_wing = parent_wing
        self._area_factor = None
        self._scale_mode  = Scale_Mode.NO_SCALE             # avoid recursion during initialization

        super().__init__(parm_filePath, **kwargs)

        self._scale_mode  = Scale_Mode(scale_mode)


    @property
    def parent_wing (self) -> Wing:
        return self._parent_wing

    @property
    def scale_mode (self) -> Scale_Mode:
        return self._scale_mode

    def set_scale_mode (self, scale_mode: Scale_Mode):
        self._scale_mode = Scale_Mode(scale_mode)

    @property
    def area_factor (self) -> float:
        """ Dimensionless planform area per halfspan-root-chord product"""
        if self._area_factor is None:
            self._area_factor = self.planform.calc_area_mac_np()[0] / (
                super().halfspan * super().chord_root)
        return self._area_factor


    @override
    @property
    def halfspan (self) -> float:
        """ returns halfspan of the reference wing based on the scaling mode """

        if self.scale_mode == Scale_Mode.NO_SCALE:
            return self._halfspan
        elif self.scale_mode == Scale_Mode.MATCH_SPAN_AND_AREA:
            return self.parent_wing.halfspan
        elif self.scale_mode == Scale_Mode.MATCH_ROOT_CHORD_AND_AREA:
            target_area = self.parent_wing.planform.calc_area_mac_np()[0]
            span = target_area / (self._area_factor * self.parent_wing.chord_root)
            return max(0.01, span)
        elif self.scale_mode == Scale_Mode.MATCH_ROOT_CHORD_AND_SPAN:
            return self.parent_wing.halfspan
        raise ValueError (f"Unsupported reference-wing scale mode: {self.scale_mode}")


    @override
    @property
    def chord_root (self) -> float:
        """ returns root chord of the reference wing based on the scaling mode """

        if self.scale_mode == Scale_Mode.NO_SCALE:
            return self._chord_root
        elif self.scale_mode == Scale_Mode.MATCH_SPAN_AND_AREA:
            target_area = self.parent_wing.planform.calc_area_mac_np()[0]
            chord = target_area / (self.area_factor * self.parent_wing.halfspan)
            return max(0.01, chord)
        elif self.scale_mode == Scale_Mode.MATCH_ROOT_CHORD_AND_AREA:
            return self.parent_wing.chord_root
        elif self.scale_mode == Scale_Mode.MATCH_ROOT_CHORD_AND_SPAN:
            return self.parent_wing.chord_root
        raise ValueError (f"Unsupported reference-wing scale mode: {self.scale_mode}")


    @override
    @property
    def polar_definitions (self) -> list [Polar_Definition]:
        """ list of actual polar definitions """

        # take polar definitions from parent wing
        polar_defs_parent = self._parent_wing.polar_definitions

        # in case of different chord, adapt Re to have constant speed assumption
        if self._scale_mode == Scale_Mode.MATCH_SPAN_AND_AREA:  

            polar_defs   = []
            re_factor = self.chord_root / self._parent_wing.chord_root
            for polar_def in polar_defs_parent:
                # create copy of polar definition with adjusted Re
                polar_def_new = Polar_Definition (polar_def._as_dict())
                polar_def_new.set_re_asK (polar_def.re_asK * re_factor)
                polar_defs.append (polar_def_new)
                print (f"Original Re: {polar_def.re_asK}, Adjusted Re: {polar_def_new.re_asK}")
        else:
            polar_defs = polar_defs_parent

        return polar_defs
