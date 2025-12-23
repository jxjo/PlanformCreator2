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

import fnmatch
import os
import numpy as np
import bisect
import shutil
from typing                 import override
from pathlib                import Path
from math                   import isclose

from airfoileditor.base.math_util           import * 
from airfoileditor.base.spline              import * 
from airfoileditor.base.common_utils        import *

from airfoileditor.model.airfoil            import Airfoil, GEO_BASIC
from airfoileditor.model.polar_set          import Polar_Definition, Polar_Set
from airfoileditor.model.airfoil_examples   import Root_Example, Tip_Example
from airfoileditor.model.xo2_driver         import Worker

from .VLM_wing                 import VLM_Wing


import logging
logger = logging.getLogger(__name__)
# logger.setLevel(logging.DEBUG)

# ---- Typing -------------------------------------

type Array      = list[float]
type Polyline   = tuple[Array, Array]
type Polylines  = tuple[Array, Array, Array]


# ---- Model --------------------------------------

VAR_AIRFOILS_DIR        = "${airfoils_dir}"

AIRFOILS_DIR_SUFFIX     = "_airfoils"
TEMP_STRAK_DIR          = "strak_temp"
FILENAME_NEW            = "new.pc2"

STRAK_AIRFOIL_NAME      = "<strak>"


class Wing:
    """ 

    Main object - holds the model 

    """
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
                    p = self._convert_parm_file_v2 (p)

                # handler for the relative path to the parameter file (working directory)
                self.pathHandler = PathHandler (onFile=parm_filePath)
                self._parm_pathFileName = parm_filePath

        # ensure airfoil dir (tmp dir will be created in strak)
        self.create_airfoils_dir()

        self._parms : Parameters    = p

        self._name                  = p.get ("wing_name", "My new Wing")
        self._description           = p.get ("description", "This is just an example planform.\nUse 'New' to select another template.")
        self._fuselage_width        = p.get ("fuselage_width", 80.0)

        # polar definitions

        self._polar_definitions     = []
        for def_dict in p.get ('polar_definitions', []):
            self._polar_definitions.append(Polar_Definition(dataDict=def_dict))

        # attach the Planform 

        self._planform              = Planform (self, p)

        # reference planforms and background image  

        self._planform_elliptical   = None
        self._planform_ref_pc2      = None
        self._reference_pc2_file    = p.get ("reference_pc2_file", None)
        self._background_image      = None

        # paneled version of self planform    

        self._planform_paneled      = None

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

        p = Parameters ()

        p.set ("pc2_version", 2)
        p.set ("wing_name",          self._name) 
        p.set ("description",        self._description) 
        p.set ("fuselage_width",     self._fuselage_width) 
        p.set ("airfoil_use_nick",   self._airfoil_use_nick)
        p.set ("airfoil_nick_prefix",self._airfoil_nick_prefix) 
        p.set ("airfoil_nick_base",  self._airfoil_nick_base) 
        p.set ("reference_pc2_file", self._reference_pc2_file)
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

        p.set ("panels", self.planform_paneled._as_dict()) 

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


    def _convert_parm_file_v2 (self, dataDict :dict) -> dict:
        """ convert parameter file from version 1 to version 2"""

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


    def wing_data (self) -> tuple[float, float, tuple]:
        """
        derived wing data from geometry
            - all together for performance reasons
        
        Returns:
            area: total wing area including fuselage
            ar: aspect_ratio including fuselage
            mac: mean aerodynamic chord
            np: geometric neutral point in chord direction (x,y)
        """

        planform_area, mac, np = self.planform._calc_area_mac_np ()
        fuselage_area = self.fuselage_width * self.planform.chord_root

        wing_area     = planform_area * 2 + fuselage_area 
        wing_ar       = self.wingspan ** 2 / wing_area

        return wing_area, wing_ar, mac, np


    @property
    def planform_paneled (self) -> 'Planform_Paneled':
        """ 
        shadow planform for self.planform which represents the panelled planform 
        which is the base for Xflr5 or FLZ export"""

        if self._planform_paneled is None:     
            self._planform_paneled = Planform_Paneled (self, dataDict = fromDict (self._parms, "panels", {})) 

        return self._planform_paneled


    @property
    def planform_elliptical (self) -> 'Planform':
        """ an elliptical reference norm planform having same span and chord reference """

        if self._planform_elliptical is None:      
            self._planform_elliptical = Planform (self, dataDict = self._parms, 
                                                        chord_style = N_Distrib_Elliptical.name,
                                                        chord_ref = self.planform.n_chord_ref )
        return self._planform_elliptical
    

    @property
    def reference_pc2_file (self) -> str:
        """ filename of optional PC2 reference planform"""
        return self._reference_pc2_file

    def set_reference_pc2_file (self, pathFilename : str) -> str:
        if pathFilename is None or os.path.isfile (pathFilename):
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
    def vlm_wing (self) -> VLM_Wing:
        """ wing for VLM aero calculation """

        if self._vlm_wing is None: 
 
            # ensure all wing sections have straked airfoils
            if not self.planform.wingSections.strak_done:
                self.planform.wingSections.do_strak (geometry_class=GEO_BASIC)

            # ensure all wingSections have a polar with the current re
            self.planform.wingSections.refresh_polar_sets (ensure=False)

            # create new VLM_Wing
            self._vlm_wing = VLM_Wing (self.planform_paneled)

        return self._vlm_wing

    def vlm_wing_reset (self):
        """ reset (will init new) VLM wing"""
        self._vlm_wing = None
        

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
            self._exporter_xflr5 = Exporter_Xflr5 (self, self.planform_paneled, xflr5_dict) 
        return self._exporter_xflr5     


    @property
    def exporter_flz (self) : 
        """ returns exporter managing FLZ export """
        from .wing_exports       import Exporter_FLZ               # here - otherwise circular errors

        if self._exporter_flz is None:                            # init exporter with parameters in sub dictionary
            flz_dict         = fromDict (self._parms, "flz", "")
            self._exporter_flz = Exporter_FLZ (self, self.planform_paneled, flz_dict) 
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

    def copy_airfoils_dir (self, newDir : str) -> bool:
        """ copy the airfoils dir of this wing to newDir without polars and tmp files
        Args:
            newDir: absolute or relative path of the new directory 
        Returns:
            True if succeeded, False if failed
        """

        if not os.path.isdir (self.airfoils_dir):
            logger.error (f"Cannot copy airfoils - source dir '{self.airfoils_dir}' does not exist")
            return False 

        if not os.path.isabs (newDir):
            newDir = os.path.join (self.workingDir, newDir)

        try:
            if os.path.isdir (newDir):
                shutil.rmtree(newDir, ignore_errors=True)
            os.mkdir (newDir)

            # copy all airfoil files except polars and tmp dir
            dat_files = fnmatch.filter(os.listdir(self.airfoils_dir), '*.dat')
            bez_files = fnmatch.filter(os.listdir(self.airfoils_dir), '*.bez')
            hh_files  = fnmatch.filter(os.listdir(self.airfoils_dir), '*.hicks')
            airfoil_files = dat_files + bez_files + hh_files

            for fname in airfoil_files:
                shutil.copy2(os.path.join(self.airfoils_dir,fname), newDir)
            return True 

        except Exception as e:
            logger.error (f"Copying airfoils dir '{self.airfoils_dir}' to '{newDir}' failed: {e}")
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

        # save dict to new file
        if newPathFilename is None:
            pathFileName_abs = self.parm_pathFileName_abs
        else:
            pathFileName_abs = newPathFilename if os.path.isabs(newPathFilename) else os.path.join (self.workingDir, newPathFilename)

        parms.set_pathFileName (pathFileName_abs)

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

                target_dir = os.path.dirname(pathFileName_abs)

                # copy airfoils to new airfoils dir if file name changed
                new_airfoils_dir = os.path.join (target_dir, Path(newPathFilename).stem + AIRFOILS_DIR_SUFFIX)
                copy_ok = self.copy_airfoils_dir (new_airfoils_dir)
                if not copy_ok:
                    logger.error (f"Saving wing parameters succeeded but copying airfoils to new dir '{new_airfoils_dir}' failed")

                # copy background image if existing
                if self.background_image.pathFilename:
                    image_pathFileName     = self.background_image.pathFilename_abs
                    new_image_pathFileName = os.path.join(target_dir,   self.background_image.pathFilename)
                    if not os.path.samefile(image_pathFileName, new_image_pathFileName):
                        shutil.copy2(image_pathFileName, new_image_pathFileName)
                    self._background_image = None                       # reset to reload from new location

                # set the current working Dir to the dir of the new saved parameter file            
                self.pathHandler.set_workingDirFromFile (pathFileName_abs)
                self._parm_pathFileName = os.path.basename(pathFileName_abs)  # only the file name relative to working dir

                # reinit planform with wing sections and new airfoils 
                self._planform = Planform (self, parms)


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
        """ returns a data dict with the parameters of self"""

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
        set chord reference Bezier control points from JPoints   
        """

        px, py = [], []
        for jpoint in jpoints:
            px.append(jpoint.x )
            py.append(jpoint.y)

        # update bezier 
        self._cr_bezier.set_points (px, py)





#-------------------------------------------------------------------------------
# Normalized Reference line 
#-------------------------------------------------------------------------------


class N_Reference_Line: 
    """ 

    The reference line is in normed coordinates a horizontal line at yn=0
    The coord distribution is applied at that line according to chord reference. 

    Optionally the reference line can be a Bezier curve (banana function) which will 
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

        # Compatibility 3.0.0: handle banana (only 1 additional control point)

        banana_p1y = fromDict (dataDict, "banana_p1y", None)  
        banana_p1x = fromDict (dataDict, "banana_p1x", 0.0)
        if banana_p1y: 
            px.insert(1, banana_p1x)
            py.insert(1, banana_p1y)

        # from dict control coordinates of Bezier

        px    = fromDict (dataDict, "px", px)              
        py    = fromDict (dataDict, "py", py)

        # create Bezier for reference line

        self._ref_bezier  : Bezier = Bezier (px, py)
        self._ref_bezier_u = np.linspace(0.0, 1.0, num=50)             # default Bezier u parameter (for polyline)


    def _as_dict (self) -> dict:
        """ returns a data dict with the parameters of self"""

        d = {}
        toDict (d, "px", self._ref_bezier.points_x)
        toDict (d, "py", self._ref_bezier.points_y)
        return d


    def at (self, xn: float | np.ndarray, fast=True) -> float:
        """ 
        reference line yn at xn  (normally = 0.0 except if Banana-Bezier)
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

            # check if the middle bezier points is on line between the two outer
            for i, px_i in enumerate (px [1:-1]):
                py_i = py[i]
                py_i_interpol = interpolate (px[0], px[-1], py[0], py[-1], px_i)
                if not isclose (py_i, py_i_interpol, abs_tol=0.005):
                    return False  
            return True


    @property
    def is_banana (self) -> bool:
        """ 
        True if reference line is not a straight line but a curve meaning 
        banana function 
        """ 
        return self._ref_bezier.npoints > 2


    def set_is_banana (self, aBool):
        """ 
        set the reference line to a Bezier curve and not a straight line
          meaning banana function (Bezier is n>2) 
        """ 
        px = self._ref_bezier.points_x
        py = self._ref_bezier.points_y

        if len(px) > 2 and aBool == False:

            # remove control point in the middle -> straight line 
            px = [px[0], px[-1]]
            py = [py[0], py[-1]]

            self._ref_bezier.set_points (px, py)

        elif len(px) == 2 and aBool == True:

            # add control point in the middle 
            px_new = 0.5
            py_new = interpolate (px[0], px[1], py[0], py[1], px_new)
            points = self._ref_bezier.points
            points.insert (1, (px_new, py_new))
            self._ref_bezier.set_points (points)


    def polyline (self) -> tuple [Array, Array]:
        """
        reference line as polyline of cr 
        """

        if self.is_banana:
            xn, yr = self._ref_bezier.eval(self._ref_bezier_u) 
        else:
            xn = [0.0, 1.0]
            yr = [0.0, 0.0]

        return np.array(xn), np.array(yr) 


    def bezier_as_jpoints (self)  -> list[JPoint]:
        """ 
        reference line Bezier control points as JPoints with limits and fixed property
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
            else:                                     
                jpoint.set_x_limits ((0,1))
                jpoint.set_y_limits ((-2,2))
                jpoint.set_name (f"Banana {i}")
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

        self._ref_bezier.set_points (px, py)




#-------------------------------------------------------------------------------
# Normalized chord    
#-------------------------------------------------------------------------------


class N_Distrib_Abstract: 
    """ 
    Abstract super class - defines the normalized chord distribution  

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
        """ returns a data dict with the parameters of self"""

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
        """ main chord function - to be overridden

        Args:
            xn: normalized x position
        Returns:
            cn: normalized chord
        """
        raise NotImplementedError


    def xn_at (self, cn: float) -> float:
        """ 
        returns xn at normed chord xn - to be overridden
        """
        raise NotImplementedError


    @property
    def at_tip (self, ) -> float:
        """ returns normalized chord at tip """
        return self.at (1.0)


    def set_cn_tip (self, aVal : float):
        """ set normed chord at tip (only for Bezier chord distribution) """   
        # to be overridden if it is supported by subclass
        pass      


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
    Bezier based normalized chord distribution allowing > 4 control points 
        and straight line chord distribution at root  
    """

    name            = "Bezier"
    isBezier        = True
    isTemplate      = True

    description = "Chord based on a Bezier curve function,\n" + \
                  "allowing multi control points "

    # is the chord distribution defined by wing section or vice versa - overwrite 
    chord_defined_by_sections = False          # e.g trapezoid


    def __init__(self, dataDict: dict = None):
        super().__init__()
        """
        Main constructor

        Args:       
            myWing: parent self belongs to 
            dataDict: data dict having parameters for self . Defaults to None.
        """

        # init Cubic Bezier for chord distribution 
       
        px = [0.0, 0.5, 1.0,  1.0]
        py = [1.0, 1.0, 0.8, 0.05]     

        # Compatibility 2.0: from dict only the variable point coordinates of Bezier

        px[1]   = fromDict (dataDict, "p1x", px[1])             # root tangent 
        py[1]   = fromDict (dataDict, "p1y", py[1])
        py[2]   = fromDict (dataDict, "p2y", py[2])             # nearly elliptic
        py[3]   = fromDict (dataDict, "p3y", py[3])             # defines tip chord 


        # from dict control coordinates of Bezier

        px    = fromDict (dataDict, "px", px)              
        py    = fromDict (dataDict, "py", py)

        self._bezier = Bezier (px, py)
        self._u = np.linspace(0.0, 1.0, num=100)                # default Bezier u parameter


    def _as_dict (self) -> dict:
        """ returns a data dict with the parameters of self"""

        d = super()._as_dict()
        toDict (d, "px", self._bezier.points_x)
        toDict (d, "py", self._bezier.points_y)

        return d


    def at (self, xn: float, fast=True) -> float:
        """ 
        Main chord function - returns cn at xn
            Normally a linear interpolation is done for fast evaluation (fast=True). 
            Higher Precision is achieved with interpolation of the curve (fast=False) 
        """

        xn_bezier_start = self._bezier.points_x[0]

        # xn either on straight line or on Bezier 
                           
        if xn < xn_bezier_start:         
            xn_line, cn_line = self.line_from_root ()
            cn = round (np.interp(xn, xn_line, cn_line),10)
        else:    
            cn = round (self._bezier.eval_y_on_x (xn, fast=fast),10) 

        return cn 
    

    def xn_at (self, cn: float, fast=True) -> float:
        """ 
        returns xn at normed chord cn
            Normally a linear interpolation is done for fast evaluation (fast=True). 
            Higher Precision is achieved with interpolation of the curve (fast=False) 
        """

        # sanity check 

        if cn > 1.0: 
            logger.warning (f"{self} cn={cn:.3f} clipped")
            return 0.0  
        elif cn < self._bezier.points_y [-1]:
            logger.warning (f"{self} cn={cn:.3f} clipped")
            return 1.0

        # cn either on straight line or on Bezier 

        cn_bezier_start = self._bezier.points_y[0]

        if cn > cn_bezier_start:         
            xn_line, cn_line = self.line_from_root ()
            xn = round (np.interp(cn, cn_line, xn_line),10)
        else:    
            xn = round (self._bezier.eval_x_on_y (cn, fast=fast), 10)

        return xn

    @override
    def set_cn_tip (self, aVal : float):
        """ set normed chord at tip via Bezier curve """   

        # set Bezier tip control point 
        px, _ = self._bezier.points[-1]
        py    = clip (aVal,0.01,0.9)
        self._bezier.set_point (-1, px, py)

        # ensure y tip tangent control point is > y tip 
        tx, ty = self._bezier.points[-2]
        if ty < py:
            ty = py * 1.01
            self._bezier.set_point (-2, tx, ty)


    def polyline (self) -> Polyline:
        """ 
        Normalized polyline of chord along xn
            At root it is: cn [0] = 1.0  

        Returns:
            xn: normalized x coordinates
            cn: normalized chord
        """
        xn_line, cn_line = self.line_from_root (npoints=20)
        xn_bez,  cn_bez  = self._bezier.eval(self._u) 

        xn = np.append (xn_line, xn_bez)
        cn = np.append (cn_line, cn_bez)

        return np.round(xn,10), np.round(cn,10) 


    def line_from_root (self, npoints=2) -> Polyline:
        """ 
        If Bezier is not starting at root return straight line upto bezier
            npoints can be > 2 to get a curve for polyline (when ref line is banana)

        Returns:
            xn: normalized x coordinates
            cn: normalized chord
        """

        xn_bezier_start = self._bezier.points_x[0]
                                                
        if xn_bezier_start == 0.0:
            xn = np.empty(0)
            cn = np.empty(0)
        else:
            xn = np.linspace (0.0, xn_bezier_start, npoints)
            cn = np.linspace (1.0, self._bezier.points_y[0], npoints)
        return xn, cn


    def bezier_as_jpoints (self, transform_fn = None) -> list[JPoint]: 
        """ 
        Bezier control points as JPoints with limits and fixed property
            - in normed coordinates x..1, y..1
            - transform_fn will transform jpoint to new coordinates 
        """

        jpoints = []
        n = len(self._bezier.points)

        for i, point in enumerate(self._bezier.points):

            jpoint = JPoint (point)   

            if i == 0:                                      # root 
                jpoint.set_x_limits ((0,0.9))
                jpoint.set_y_limits ((0.1,1.0))
                jpoint.set_name ('Bezier Start') 
            elif i == 1:                                    # root tangent
                jpoint.set_x_limits ((0,1))
                jpoint.set_y_limits ((0,1))
                jpoint.set_name ('Start Tangent') 
            elif i == n-2:                                  # tip tangent
                jpoint.set_x_limits ((1,1))
                jpoint.set_y_limits ((0.2,1))
                jpoint.set_name ('End Tangent') 
            elif i == n-1:                                  # tip
                jpoint.set_x_limits ((1,1))
                jpoint.set_y_limits ((0.01,0.9))
                jpoint.set_name ('Tip Chord') 
            else:
                jpoint.set_x_limits ((-0.5,1.5))
                jpoint.set_y_limits ((0.01,1.0))
                jpoint.set_name (f'Free {i}') 

            jpoints.append(jpoint.as_transformed (transform_fn))

        return jpoints


    def bezier_from_jpoints (self, jpoints : list[JPoint], transform_fn = None): 
        """ 
        set Bezier control points from JPoints
            - transform_fn will transform jpoint coordinates prior to setting Bezier
        """

        px, py = [], []
        for i, jpoint in enumerate (jpoints):
            jpoint_trans = jpoint.as_transformed (transform_fn)

            if i == 0 and isclose (jpoint_trans.x, 0.0, abs_tol= 0.005):
                xn = 0.0 
                yn = 1.0 
            elif isclose (jpoint_trans.x, 1.0, abs_tol= 0.005):
                xn = 1.0
                yn = jpoint_trans.y
            else:
                xn = jpoint_trans.x
                yn = jpoint_trans.y

            px.append(xn)
            py.append(yn)

        self._bezier.set_points (px, py)



class N_Distrib_Trapezoid (N_Distrib_Abstract):
    """ 
    Trapezoidal chord based on wing section 
    """

    name            = "Trapezoid"
    isTrapezoidal   = True

    description     = "Chord defined by its wing sections,\n" + \
                      "which have the attribute 'Defines planform'" 

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


    def polyline (self, npoints = 2) -> Polyline:
        """ 
        Normalized polyline of chord along xn (root cn[0]=1.0)
           
        Arguments:
            npoints:    number of points of one line segment

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
                
        xn, cn = np.zeros(0), np.zeros(0)

        sec : WingSection
        for isec, sec in enumerate (sections [:-1]):
            next_sec : WingSection = sections [isec+1]
            xn_sec = np.linspace (sec.xn, next_sec.xn, npoints) 
            cn_sec = np.linspace (sec.cn, next_sec.cn, npoints) 
            xn = np.append(xn, xn_sec)
            cn = np.append(cn, cn_sec)
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


    def __init__(self, parent_planform : 'Planform'):
        """
        Create new chord distribution 

        Args:       
            parent_planform: parent self belongs to (trapezoidal need the wing sections)
        """

        self._parent_planform   = parent_planform
        self._cn_tip_min        = None

        super().__init__ ()


    @property
    def cn_tip_min (self) -> float | None: 
        """ the minimum normed chord at tip (will cut the tip)""" 

        if self._cn_tip_min is not None: 
            # wing sections can change during lifetime - so dynamic check 
            cn_min = self._parent_planform.wingSections[-1].cn                 # cn of tip 
            cn_max = self._parent_planform.wingSections[1].cn                  # cn of 2nd section to ensure at least 2 sections
            self._cn_tip_min = clip (self._cn_tip_min, cn_min, cn_max)
            return round (self._cn_tip_min,2)                                  # calc of cn may have numerical issues 
        else: 
            return None 
    
    def set_cn_tip_min (self, aVal : float | None):
        """ set minimum - it can't be smaller than parent tip section cn"""

        if aVal == 0.0:
            self._cn_tip_min = round(self._parent_planform.wingSections[-1].cn + 0.005,2)     # round up
        elif aVal is not None:
            cn_min = self._parent_planform.wingSections[-1].cn                 # cn of tip 
            cn_max = self._parent_planform.wingSections[1].cn                  # cn of 2nd section to ensure at least 2 sections
            self._cn_tip_min = clip (aVal, cn_min, cn_max)
        else: 
            self._cn_tip_min = None


    def polyline (self) -> Polyline:
        """ 
        Normalized polyline of chord along xn which can be reduced if cn_tip_min is active
            At root it is: cn [0] = 1.0 

        Returns:
            xn: normalized x coordinates
            cn: normalized chord
        """

        # retrieve xn, cn of sections from parent chord 

        xn, cn = [], []
        section : WingSection
        for section in self._parent_planform.wingSections:
            if self.cn_tip_min is None or  (round(section.cn,3) >= self.cn_tip_min) :
                xn.append(section.xn)
                cn.append(section.cn)

        return np.round(xn,10), np.round(cn,10) 


    def at (self, xn: float, fast=True) -> float:
        """ 
        Main chord function - returns cn at xn
        """

        xn_arr, cn_arr = [], []
        for section in self._parent_planform.wingSections:
            xn_arr.append(section.xn)
            cn_arr.append(section.cn)

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
        Normalized polyline of elliptical chord along xn
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
            dataDict: data dict having parameters for self . Defaults to None.
        """

        self._planform    = planform

        self._xn            = fromDict (dataDict, "xn", None)               # xn position
        self._cn            = fromDict (dataDict, "cn", None)               # cn chord  
        self._defines_cn    = fromDict (dataDict, "defines_cn", False)      # section must have cn and xn 
        self._hinge_cn      = fromDict (dataDict, "hinge_cn", None)         # hinge chord position 
        self._flap_group    = fromDict (dataDict, "flap_group", 1)          # flap group (starting here) 
        self._is_for_panels = fromDict (dataDict, "is_for_panels", False)   # extra section for paneling


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

            if self.is_tip:
                if self._cn is None:
                    self._cn = 0.25                                         # take default 
                self._defines_cn = True
            elif self.is_root:
                self._cn = 1.0
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

        # create airfoil and load if exist 

        self._airfoil = None

        file = self.get_airfoil_file (pathFileName = fromDict (dataDict, "airfoil", None), workingDir=self.workingDir)
        self.set_airfoil_by_file (file)                # ensure normalized (copied to airfoils dir if needed)

        self._strak_info = None                             # keep info of strak 


    def __repr__(self) -> str:

        if (self._xn != None):
            info = f"xn={self._xn}"
        elif (self._cn != None):
            info = f"cn={self._cn:.2f}" 
        else:
            info = ''
        return f"<{type(self).__name__} {info}>"


    def _as_dict (self) -> dict:
        """ returns a data dict with the parameters of self"""

        d = {}
        toDict (d, "xn",            self._xn)
        toDict (d, "cn",            self._cn)
        toDict (d, "defines_cn",    self._defines_cn if self._defines_cn else None)
        toDict (d, "hinge_cn",      self._hinge_cn)
        toDict (d, "flap_group",    self.flap_group)
        if self.is_for_panels:
            toDict (d, "is_for_panels", self.is_for_panels)

        if not self.airfoil.isBlendAirfoil and not self.airfoil.isExample:
            # replace with airfoils_dir variable if airfoil is in airfoils dir of wing
            if os.path.samefile(self.airfoil.pathName_abs, self._planform.wing.airfoils_dir):
                pathFileName = f"{VAR_AIRFOILS_DIR}\\{self.airfoil.pathFileName}"
            else:
                # make relative path to working dir if possible
                try:
                    pathFileName = os.path.relpath(self.airfoil.pathFileName_abs, self.workingDir)
                except (ValueError, TypeError):
                    pathFileName = self.airfoil.pathFileName_abs
            toDict (d, "airfoil",    pathFileName)
        return d


    @property
    def workingDir (self) -> str:
       """ current working directory""" 
       return self._planform.workingDir 


    def get_airfoil_file (self, pathFileName : str|None= None, workingDir=None) -> str | None:
        """
        check if airfoil pathFileName exists 
            - if exists return its absolute path
            - if not and self isRoot or isTip create example airfoil file and return its path
            - if not root or tip return None for strak airfoil
        """

        airfoils_dir = self._planform._wing.airfoils_dir

        # replace VAR_AIRFOILS_DIR variable
        if pathFileName is not None and VAR_AIRFOILS_DIR  in pathFileName:
            pathFileName = pathFileName.replace (VAR_AIRFOILS_DIR, self._planform.wing.airfoils_dir)


        # pathFileName either absolute or relative to working dir
        if pathFileName:
            if not os.path.isabs(pathFileName):
                if not os.path.isdir(workingDir):
                    raise ValueError (f"{self} working dir {workingDir} does not exist")
                pathFileName_abs = os.path.join(workingDir, pathFileName)
            else:
                pathFileName_abs = pathFileName

            if os.path.isfile (pathFileName_abs):
                return pathFileName_abs
            else:
                logger.warning (f"{self} airfoil file {pathFileName_abs} not found")

        # create example airfoil if root or tip
        if self.is_root: 
            example_airfoil = Root_Example (workingDir=airfoils_dir)
        elif self.is_tip:
            example_airfoil = Tip_Example (workingDir=airfoils_dir)
        else:
            return None                                 # will be strak airfoil 

        # save to tmp_dir 
        if not os.path.isfile(example_airfoil.pathFileName_abs):
            example_airfoil.save()
            logger.debug (f"{self} save airfoil {example_airfoil.fileName} to {airfoils_dir}")

        return example_airfoil.pathFileName_abs


    @property 
    def airfoil (self) -> Airfoil:
        """ airfoil of wing section"""
        return self._airfoil
    

    def set_airfoil (self, airfoil : Airfoil | None):
        """ 
        set airfoil - ! no checks ! - use get_airfoil_file prior to this method
        """
        
        if isinstance (airfoil, Airfoil) or airfoil is None:
            self._airfoil = airfoil
        else:
            raise TypeError (f"{self} set_airfoil - invalid airfoil type {type(airfoil)}")


    def set_strak_airfoil (self):
        """ set airfoil of self as a strak (dummy) airfoil """

        tmp_dir      = self._planform._wing.tmp_dir
        airfoil = Airfoil (name=STRAK_AIRFOIL_NAME, geometry=GEO_BASIC, workingDir=tmp_dir)
        airfoil.set_isBlendAirfoil (True)

        self.set_airfoil (airfoil)


    def set_airfoil_by_file (self, pathFileName_abs : str|None, into_airfoils_dir = True):
        """ 
        set new airfoil by path
            - if into_airfoils_dir is True, ensure it is in airfoils_dir of wing
            - if pathFileName_abs is None - current airfoil will be a strak airfoil

        Args:
            pathFileName_abs:  absolute path of airfoil file to be set
            into_airfoils_dir: if True the airfoil will be copied in the airfoils dir of the wing
        """

        # strak airfoil 
        if pathFileName_abs is None:
            self.set_strak_airfoil ()
            return 

        # sanity - file must exist
        if not os.path.isfile (pathFileName_abs):
            raise FileNotFoundError (f"{self} airfoil file {pathFileName_abs} not found")

        fileName = os.path.basename (pathFileName_abs)
        
        # is airfoil already in airfoils dir of wing ?
        if into_airfoils_dir:
            workingDir = self._planform.wing.airfoils_dir
            pathFileName_in_airfoil_dir = os.path.join(workingDir, fileName)
            if not os.path.isfile (pathFileName_in_airfoil_dir):
                # copy airfoil file to airfoils dir of wing          
                shutil.copy2 (pathFileName_abs, pathFileName_in_airfoil_dir)
                logger.debug (f"{self} copied airfoil {fileName} to {self._planform.wing.airfoils_dir}")

        else: 
            workingDir = os.path.dirname(pathFileName_abs)

        airfoil = Airfoil (pathFileName= fileName, geometry=GEO_BASIC,
                            workingDir=workingDir)
        airfoil.load()

        # ensure airfoil is normalized (for strak) - if not create tmp airfoil 
        if not airfoil.isNormalized:
            airfoil.normalize(mod_string='_norm')
            logger.debug (f"{self} normalize airfoil {airfoil.fileName}")
            airfoil.saveAs (dir=self._planform.wing.airfoils_dir)

        self.set_airfoil (airfoil)


    @property
    def airfoil_nick_name (self) -> str:
        """ short nick name of airfoil"""
        prefix = self._planform.wing.airfoil_nick_prefix
        base   = self._planform.wing.airfoil_nick_base
        my_num = round(base * self.cn, 0)
        return f"{prefix}{int(my_num)}"

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
        else:                                                       # trapezoid must have both
            if self._xn is None: self._xn = round(self.xn,10)           
            if self._cn is None: self._cn = round(self.cn,10) 

        self.set_is_for_panels (False) 


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

        if self.is_root: return                                     # root is always 1.0 

        if aVal is None:
            self._cn = None
        elif self.is_tip and not self.defines_cn:                   # set tip chord via Bezier
            self._planform.set_cn_tip (aVal)
        else:                                                       # normal case
            aVal = clip (aVal, 0.0, 1.0)
            self._cn = round(aVal,10)

        if not self.defines_cn: 
            if self.is_cn_fix:                                      # reset xn 
                self._xn = None 
        else:                                                       # trapezoid must have both
            if self._xn is None: self._xn = round(self.xn,10)           
            if self._cn is None: self._cn = round(self.cn,10) 

        self.set_is_for_panels (False) 


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
            return True # self._planform.chord_defined_by_sections             # for trapezoid yes 
        else: 
            return True

    @property
    def is_for_panels (self) -> bool:
        """ self is additional created for paneling"""
        return self._is_for_panels
    
    def set_is_for_panels (self, aBool : bool):
        self._is_for_panels = aBool == True 


    def index (self) -> int:
        """ index of self with wingSections"""
        try: 
            index = self._planform.wingSections.index (self)
        except: 
            index = 1
        return index  

    @property
    def id (self) -> str:
        """ 
        unique id of self within planform which is 1,2,3.. for normal sections
        and 1.1, 1.2.. for additional sections for paneling
        """
        # find how many additional sections before self 
        n_main = -1
        n_add = 0
        for sec in self._planform.wingSections: 
            if not sec.is_for_panels:
                n_main += 1 
                n_add   = 0
            else:
                n_add += 1
            if sec == self:
                break

        if not self.is_for_panels:
            return f"{n_main}"  
        else:
            return f"{n_main}.{n_add}"
        

    def xn_limits (self) -> tuple:
         """ xn limits as tuple of self before touching the neighbor section """
         return self._planform.wingSections.xn_cn_limits_of (self) [0]

    def x_limits (self) -> tuple:
         """ x limits as tuple of self before touching the neighbor section """
         xn_limits = self.xn_limits()
         return xn_limits[0] * self._planform.span, xn_limits[1] * self._planform.span

    def cn_limits (self) -> tuple:
         """ cn limits as tuple of self before touching the neighbor section """
         return self._planform.wingSections.xn_cn_limits_of (self) [1]

    def c_limits (self) -> tuple:
         """ c chord limits as tuple of self before touching the neighbor section """
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
            info = self.id
        return info


    @property
    def hinge_equal_ref_line (self) -> bool:
        """ hinge is defined by reference line (convenience from flaps )"""
        return self._planform.flaps.hinge_equal_ref_line


    @property
    def hinge_cn (self) -> float:
        """ 
        relative hinge chord position cn of self
            - if no hinge position is defined, the calculated value from hinge line is taken
        """
        if self.defines_hinge and not self.hinge_equal_ref_line:
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

            self.set_is_for_panels (False) 


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
        """ self as a polyline x,y within planform"""

        x          = self.x
        le_y, te_y = self.le_te ()
        return np.array([x, x]), np.array([le_y, te_y])


    def line_in_chord (self) -> Polyline:
        """ self as a polyline xn,yn within normed chord """

        xn          = self.xn
        return np.array([xn, xn]), np.array([self.cn, 0.0])


    def line_in_chord_ref (self) -> Polyline:
        """ self as a polyline xn,yn within chord reference which is just y[1]=1"""

        xn          = self.xn
        return np.array([xn, xn]), np.array([0.0, 1.0])




class WingSections (list [WingSection]):
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
            sections.append(WingSection (planform, {"xn": 0.0,  "flap_group":1}))
            sections.append(WingSection (planform, {"xn": 0.45, "flap_group":2}))
            sections.append(WingSection (planform, {"cn": 0.65, "flap_group":2}))
            sections.append(WingSection (planform, {"cn": 0.35, "flap_group":2}))
            sections.append(WingSection (planform, {"xn": 1.0,  "flap_group":2}))


        self.extend (sections)

        # final sanity
        self.check_n_repair ()

        logger.info (f"{self} added")


    def __repr__(self) -> str:
        # overwrite to get a nice print string 
        return f"<{type(self).__name__} n={len(self)}>"


    @property
    def workingDir (self) -> str:
        """ current working directory""" 
        return self._planform.workingDir 
   

    def _as_list_of_dict (self) -> list[dict]:
        """ returns a data dict with the parameters of self"""

        self.check_n_repair ()

        section_list  = []
        section : WingSection
        for section in self: 
            section_list.append (section._as_dict ()) 

        return section_list

    @property
    def without_for_panels (self) -> list[WingSection]:
        """ return self without extra sections for paneling"""

        return [section for section in self if not section.is_for_panels]



    def create_after (self, aSection: 'WingSection'=None, index=None, is_for_panels=False) -> 'WingSection' : 
        """
        creates and inserts a new wing section after aSection 
            with a chord in the middle to the next neighbor 
            'is_for_panels' indicates an extra section created just for paneling

        Return: 
            newSection: the new wingSection
        """

        if aSection is None and index is not None:
            aSection = self[index]

        new_section = None 

        if isinstance(aSection, WingSection) and not aSection.is_tip :

            _, right_sec = self.neighbors_of (aSection)

            new_cn = (aSection.cn + right_sec.cn) / 2
            new_flap_group = aSection.flap_group 

            new_section = WingSection (self._planform, 
                                       {"cn": new_cn, 
                                        "flap_group"  :new_flap_group,
                                        "is_for_panels" : is_for_panels})
            self.insert (self.index(aSection) + 1, new_section)

            self.reset_strak()

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

        self.check_n_repair ()                                          # re-sort

        # set flap group of new to the left neighbor 
        left_sec, _ = self.neighbors_of (new_section) 
        new_section.set_flap_group (left_sec.flap_group)

        self.reset_strak()

        return new_section


    def delete (self, aSection: 'WingSection', protect_root_tip=True) -> WingSection: 
        """  delete wing section - return new current if succeeded"""

        if aSection and not (protect_root_tip and aSection.is_root_or_tip):
            try:
                index = self.index (aSection)
                self.remove (aSection)

                self.reset_strak()

                return self[index-1] 
            except: 
                pass
        return None  


    def create_airfoil_for (self, section: WingSection,
                         pathFileName_abs : str | None,
                         into_airfoils_dir = True):
        """ 
        Create and set airfoil for section optionally in airfoils dir
            - assign polarSet to new airfoil
            - reset strak 
        """

        # create and set into section
        new_pathFileName_abs = section.get_airfoil_file (pathFileName_abs)
        section.set_airfoil_by_file (new_pathFileName_abs, into_airfoils_dir = into_airfoils_dir)

        # assign polarSet
        polar_defs = self._planform.wing.polar_definitions
        section.airfoil.set_polarSet (Polar_Set (section.airfoil, polar_def=polar_defs, 
                                                  re_scale=section.cn))

        self.reset_strak ()


    def do_strak (self, geometry_class  = None): 
        """
        straks the airfoil of all wing sections having a Strak-Airfoil which is 
        created by blending with its real neighbors

        Args: 
            geometry: optional - the desired geometry of the new straked airfoils 
                                 either GEO_BASIC or GEO_SPLINE
        """

        self._planform._wing.create_tmp_dir()

        tmp_dir = self._planform._wing.tmp_dir
        polar_defs = self._planform.wing.polar_definitions
        n_straked = 0 

        if not os.path.isdir (tmp_dir):
            os.mkdir(tmp_dir)

        for section in self:

            airfoil = section.airfoil
            if airfoil.isBlendAirfoil: 

                # get the neighbor wing sections  with real airfoils 

                left_sec, right_sec = self.neighbors_having_airfoil(section) 
                left, right = left_sec.airfoil, right_sec.airfoil

                if left_sec.airfoil.name == right_sec.airfoil.name:
                    # both are the same airfoil -> dummy blend 
                    blendBy = 0.0 

                elif right_sec.cn == left_sec.cn:
                    # rectangular planform - no chord difference - take pos difference
                    blendBy  = (section.xn - left_sec.xn) / (right_sec.xn - left_sec.xn)

                else: 
                    # blend value is defined by chord relation to left and right              
                    blendBy  = (section.cn - left_sec.cn) / (right_sec.cn - left_sec.cn)

                blendBy = round (blendBy,2)                             # ensure 1% steps 

                # was it already straked? - skip 

                strak_info = f"{left.name}{right.name}{blendBy}"

                if section._strak_info != strak_info:

                    if blendBy > 0.0:

                        airfoil.do_blend (left, right, blendBy, geometry_class)

                        mods = airfoil.geo.modifications_as_label
                        n_straked += 1

                        name = f"{STRAK_AIRFOIL_NAME} {left.fileName_stem}{mods}" # build long unique name  
                        airfoil.set_name     (name, reset_original=True)  

                        fileName = f"{left.fileName_stem}{mods}_{right.fileName_stem}.dat"    
                        airfoil.set_fileName   (fileName)
                        airfoil.set_workingDir (tmp_dir) 
                        airfoil.set_pathName   ('') 
                        airfoil.set_isModified (False)           # avoid save and polar generation if file already exists

                    else:

                        section.set_airfoil (left.asCopy())       # no blend - take left airfoil
                        airfoil = section.airfoil
                        airfoil.set_isBlendAirfoil (True)

                    airfoil.set_polarSet (Polar_Set (airfoil, polar_def=polar_defs, re_scale=section.cn))

                    self._strak_done = True 
                    section._strak_info = strak_info

        if n_straked > 0:
            logger.info (f"{self} straked {n_straked} airfoils in {tmp_dir}")


    def reset_strak (self): 
        """
        reset all straked airfoil e.g. after airfoil for a section selected
            or section removed
        """

        for section in self:
            if section.airfoil.isBlendAirfoil: 

                section.set_strak_airfoil ()
                self._strak_done    = False 
                section._strak_info = None


    @property
    def strak_done (self) -> bool:
        """ was there at least one strak calculation - so e.g. names are generated"""
        return self._strak_done


    def check_n_repair (self):
        """ sanity check of wingSections  - remove dirty ones"""


        # sanity check for dirty section with cn smaller than tip 
        cn_tip = self[-1].cn
        for section in self [:]: 
            if section._cn and section._cn < cn_tip:
                logger.warning (f"{section} removed - cn {section._cn:.3f} smaller than tip") 
                self.delete (section)        

        # only one section with xn=0.0 and one with xn=1.0
        for section in self [1:-1]: 
            if section.is_root or section.is_tip:
                logger.warning (f"{section} removed - duplicate root/tip section") 
                self.delete (section, protect_root_tip=False)

        # Re-sort wing sections to an ascending xn pos. 
        # When changing major wing parms sections could become out of sort order when
        #    they have fixed xn and chord mixed    

        self.sort (key=lambda sec: sec.xn) 

        # there must be root and tip 
        if not self[0].is_root or not self[-1].is_tip:
            raise ValueError ("Wingsection data corrupted")


    def neighbors_of (self, aSection: WingSection) -> tuple [WingSection, WingSection]:
        """
        returns the neighbor before and after a wingSection - if no neighbor return None 
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
        xn position and cn chord limits as tuple of self before touching the neighbor section
        """
        xn = aSection.xn
        cn = aSection.cn

        if aSection.is_tip and aSection.defines_cn:                             # special case trapezoid - tip section defines chord 
            left_sec, right_sec = self.neighbors_of (aSection)     
            return (xn,xn), (0.01, left_sec.cn) 
        if aSection.is_root_or_tip:                                             # normally root and tip fixed 
            return (xn,xn), (cn,cn) 
        else:
            left_sec, right_sec = self.neighbors_of (aSection)                 # keep a safety distance to next section
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



    def neighbors_having_airfoil (self, aSection: 'WingSection') -> tuple [WingSection, WingSection]:
        """
        returns the neighbor before and after a wingSection, which are not blendAirfoil
        Needed for 'strak'  - if no neighbor return None 
        """

        try:
            index = self.index (aSection) 
        except: 
            return None, None

        left_sec = None
        right_sec = None

        #left neighbor 
        if not aSection.is_root: 
            left_sec = self[0]
            for sec in reversed(self [0:index]):
                if not sec.airfoil.isBlendAirfoil:
                    left_sec = sec
                    break 

        #right neighbor 
        if not aSection.is_tip: 
            right_sec = self[-1]
            for sec in self [index+1: ]:
                if not sec.airfoil.isBlendAirfoil:
                    right_sec = sec
                    break 
        if left_sec is None or right_sec is None:
            pass
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

        for section in self:
            if abs( section.xn - xn) < tolerance :
                return section
        return None


    def refresh_polar_sets (self, ensure=True):
        """ refresh polar set of wingSections airfoil"""

        polar_defs = self._planform.wing.polar_definitions

        for section in self:
            airfoil = section.airfoil
            if airfoil :
                polarSet : Polar_Set = airfoil.polarSet

                if polarSet and isclose (polarSet._re_scale, section.cn, rel_tol=0.01) and not ensure:
                    # there is already a polarSet which is scaled approx.
                    pass
                else:
                    # create new, fresh polarSet
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


    def depth_left (self) -> tuple[float]:
        """ 
        flap depth absolute e.g. 35mm and relative e.g. 0.27 at left side        
        """
        return self._flaps.flap_depth_at (self.x_from)

    def depth_right (self) -> tuple[float]:
        """ 
        flap depth absolute e.g. 35mm and relative e.g. 0.27 at right side        
        """
        return self._flaps.flap_depth_at (self.x_to)



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

        self._hinge_equal_ref_line = fromDict (dataDict, "hinge_equal_ref_line", True)  

        self.check_and_correct ()                   # sanity checks of hinge and flap definitions    


    def _as_dict (self) -> dict:
        """ returns a data dict with the parameters of self"""

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

        for section in self._wingSections.without_for_panels:

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

        # helper sections for paneling have the same flap group like real left wingSection

        flap_group = sections[0].flap_group

        for section in sections[:-1]:
            if section.is_for_panels:
                section.set_flap_group (flap_group)
            else: 
                flap_group = section.flap_group



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



    def flap_depth_at (self, x : float, hinge_y : float = None) -> tuple[float]:
        """ 
        flap depth absolute e.g. 35mm and relative e.g. 0.27 at position x 
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
        relative flap depth e.g. 0.27 (hinge position) within chord reference    
        """

        # as the relative flap depth e.g. 0.27 is not a straight line if the chord reference 
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
        self._sweep_angle = fromDict (dataDict, "sweep_angle", 0.0)

        self._wingSections  = None                                        # early to have property

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
            if chord_style == N_Distrib_Bezier.name:                # only Distrib_Bezier may have Banana
                self._n_ref_line = N_Reference_Line (dataDict=fromDict(dataDict, "reference_line", {}))
            else: 
                self._n_ref_line = N_Reference_Line (dataDict={})
        else: 
            self._n_ref_line = ref_line 

        # init wing sections 
        
        self._wingSections    = WingSections (self, sectionsDict=fromDict(dataDict, "wingSections", {}))

        # init flaps

        self._flaps           = Flaps (self, dataDict=fromDict(dataDict, "flaps", {}))
         
        # late setting of polar sets as chord is needed for reynolds factor 
        
        self._wingSections.refresh_polar_sets (ensure=False)



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


    def set_cn_tip (self, aVal : float):
        """ set normed chord at tip (only for Bezier chord distribution) """        
        self.n_distrib.set_cn_tip (aVal) 


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
        """ normalized reference line object """
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
            fast:   True - only a linear interpolation is made with low precision
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
            fast:   True - only a linear interpolation is made with low precision
        Returns:
            xn:     normalized x position
        """
        return self.n_distrib.xn_at (cn, fast=fast)


    def x_at_cn (self, cn: float, fast=True) -> float:
        """ 
        x at normed chord cn (interpolation)
 
        Args:
            cn:     a normalized chord value 
            fast:   True - only a linear interpolation is made with low precision
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
            fast:   True - only a linear interpolation is made with low precision
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

        return x, le_y, te_y 


    def polygon (self) -> Polyline:
        """ 
        polygon of the planform starting at le_root clockwise 
        """
        x, le_y, te_y = self.le_te_polyline()
        return self._polygon (x, le_y, te_y)
    

    def _polygon (self, x : np.ndarray, le_y : np.ndarray, te_y : np.ndarray) -> Polyline:
        """ 
        polygon of the planform starting at le_root clockwise based on le, te polyline 
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
            c_mean = (te_y[i] - le_y[i] + te_y[i+1] - le_y[i+1]) / 2.0          # chord mean value of dx 
            i_c2 += c_mean **2 * dx

        area = self._calc_planform_area (x, le_y, te_y )
        mac  = i_c2 / area

        return mac
 


    def _calc_area_mac_np (self) -> tuple[float, float, tuple]:
        """calc area, mean aerodynamic chord, neutral point"""

        # http://walter.bislins.ch/blog/index.asp?page=Berechnung%3A+Mittlere+Aerodynamische+Fl%FCgeltiefe+%28MAC%29 

        # get chord along span - for trapezoidal at least 3 points per segment are needed
        if self.n_distrib.isTrapezoidal:
            xn, cn = self.n_distrib.polyline(npoints=10)
        else:
            xn, cn = self.n_distrib.polyline()

        x  = xn * self.span
        c  = cn * self.chord_root

        # t/4 line 
        xn, le_yn = self.t_chord_to_norm (xn, np.full (len(xn), 1.0), cn=cn)
        x, le_y   = self.t_norm_to_plan (xn, le_yn)
        t4_y      = le_y + c / 4
        dt4_y     = np.diff (t4_y)

        # finite rectangle width=dx, height=chord mean value, pos = x mean value 
        dx = np.diff (x)
        dc = np.diff (c)
        x_i    = x [:-1] + dx/2 
        c_mean = c [:-1] + dc/2 
        t4_y_i = t4_y [:-1] + dt4_y / 2 

        # integral for geometric parms 
        area   = np.sum(c_mean     * dx) 
        c_int  = np.sum(c_mean **2 * dx) 
        t4_int = np.sum(c_mean     * dx * t4_y_i)
        x_int  = np.sum(c_mean     * dx * x_i)
        np_x  = x_int  / area

        mac   = c_int  / area
        np_y  = t4_int / area
         
        return area, mac, (np_x, np_y) 
 

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
        except Banana (Bezier) is applied

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
            - by shifting yn value dependant chord reference at xn  
        In normalized planform coordinates the reference line will be at yn=0, le_y positive, te_y negative

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

    Subclass of Planform representing a paneled version of a (parent) planform 

    The chord distribution is a polyline of the wing sections le and te of the parent 

    Panelling is defined by the panel definition parameters 

    """

    def __init__(self, wing : Wing, dataDict: dict = None):


        self._wing = wing
        self._parent_planform = wing.planform

        self._span        = None                                        # will take it from parent 
        self._chord_root  = None
        self._sweep_angle = None


        # create Norm_Chord distribution based on parent planform 

        self._n_distrib     = N_Distrib_Paneled (self._parent_planform)

        # main objects from parent

        self._n_chord_ref   = self._parent_planform.n_chord_ref 
        self._n_ref_line    = self._parent_planform.n_ref_line 
        self._wingSections  = self._parent_planform.wingSections
        self._flaps         = self._parent_planform.flaps

        # get panel parameters - x,y are in wing coordinate system (wy is along span)

        self._wy_panels      = None                             # number of panels along span
        self._wy_dist        = None                             # distribution function along span
        self._wx_panels      = None                             # number of panels along chord
        self._wx_dist        = None                             # distribution function along chord
        self._width_min_targ = None                             # target min panel width 1%
        self._cn_diff_max    = None                             # max cn difference 5%
        self._from_dict (dataDict )

        # dict of available panel distribution functions used for x and y  

        self._wy_distribution_fns = {}
        self._wy_distribution_fns["uniform"]= lambda y : y
        self._wy_distribution_fns["-sine"]  = lambda y :  np.sin (y     * np.pi/2)
        self._wy_distribution_fns["sine"]   = lambda y : (np.sin ((y+1) * np.pi/2) - 1.0) * -1.0
        self._wy_distribution_fns["cosine"] = lambda y : (np.cos ((y+1) * np.pi) + 1) / 2

        self._wx_distribution_fns = {}
        self._wx_distribution_fns["uniform"]= lambda y : y
        self._wx_distribution_fns["cosine"] = lambda y : (np.cos ((y+1) * np.pi) + 1) / 2

        # apply current settings 

        self.optimize ()  



    def _as_dict (self) -> dict:
        """ returns a data dict with the parameters of self"""
        d = {}
        toDict (d, "wy_panels",         self._wy_panels)
        toDict (d, "wy_distribution",   self._wy_dist)
        toDict (d, "wx_panels",         self._wx_panels)
        toDict (d, "wx_distribution",   self._wx_dist)

        toDict (d, "width_min",         self._width_min_targ)                     
        toDict (d, "cn_tip_min",        self._n_distrib.cn_tip_min)
        toDict (d, "cn_diff_max",       self._cn_diff_max)
        return d


    def _from_dict (self, d : dict):
        """ set parameters from data dict """

        if not d: d={}
        self._wy_panels      = fromDict (d, "wy_panels", 8)
        self._wy_dist        = fromDict (d, "wy_distribution", "uniform")
        self._wx_panels      = fromDict (d, "wx_panels", 4)
        self._wx_dist        = fromDict (d, "wx_distribution", "uniform")
        self._width_min_targ = fromDict (d, "width_min", None)                # target min panel width 1%
        self._n_distrib.set_cn_tip_min (fromDict (d, "cn_tip_min",None))      # min tip chord 10%
        self._cn_diff_max    = fromDict (d, "cn_diff_max", None)              # max cn difference 5%


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

        # is there a section having cn < cn_tip_min
        isec_cutted = None 
        for isec, section in enumerate (self.wingSections):
            if self.cn_tip_min and round(section.cn,3) < self.cn_tip_min :
                isec_cutted = isec
                break

        # if yes, reduce wing sections 
        if isec_cutted:
            sections = self._wingSections [:isec_cutted]           
        else: 
            sections = self._wingSections
        return sections


    @property
    def wx_panels (self) -> int:         return self._wx_panels
    def set_wx_panels (self, val: int):  
        self._wx_panels = int(val)

    @property
    def wx_dist (self):                  return self._wx_dist
    def set_wx_dist (self, val):  
        if val in self._wy_distribution_fns:
            self._wx_dist = val


    @property
    def wy_panels (self) -> int:         return self._wy_panels
    def set_wy_panels (self, val: int):  
        self._wy_panels = int(val)


    @property
    def wy_dist (self):                  return self._wy_dist
    def set_wy_dist (self, val):  
        if val in self._wy_distribution_fns:
            self._wy_dist = val


    @property
    def width_min_targ (self):  
        """ minimum panel width - None or e.g. 0.01"""            
        return self._width_min_targ 
    
    def set_width_min_targ (self, val : float |  None):   
        if val == 0.0:
            self._width_min_targ = round(self.width_min_cur + 0.005,2)     # round up
        elif val is not None: 
            self._width_min_targ = clip (val, 0.001, 0.2)
        else: 
            self._width_min_targ = None


    @property
    def width_min_cur (self) -> float:  
        """ current min panel width along wing """
        _, width_min_cur = self._calc_x_stations ()
        return width_min_cur
    

    @property
    def cn_diff (self):
        """ current max. chord difference """  

        cn_diff = 0.0
        for xi in self.x_stations():
            c_diff   = self._parent_planform.c_at (xi) - self.c_at (xi)
            cn_diff = max ((c_diff/self.chord_root), cn_diff)               
        return cn_diff 
    

    @property
    def cn_diff_max (self):
        """ max deviation of chord of paneled ot original planform""" 
        return self._cn_diff_max 

    def set_cn_diff_max (self, val: float | None):    
        if val is not None: 
            self._cn_diff_max = clip (val, 0.0, 0.2)
        else: 
            self._cn_diff_max = None
        self.optimize ()                          


    @property
    def cn_tip_min (self) -> float | None:             
        """ minimum chord at tip when generating panels"""
        return self._n_distrib.cn_tip_min
    
    def set_cn_tip_min (self, aVal):
        self._n_distrib.set_cn_tip_min (aVal)

    @property
    def cn_tip_cur (self) -> float:
        """ current chord of cutted tip """
        return self._n_distrib.polyline()[1][-1]


    @property
    def wy_distribution_fns_names (self):
        """ a list of available distribution functions for wy - along span"""
        return list(self._wy_distribution_fns.keys())

    @property
    def wx_distribution_fns_names (self):
        """ a list of available distribution functions for wx - along chord"""
        return list(self._wx_distribution_fns.keys())


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


    @property
    def cn_rel_stations (self) -> np.ndarray:
        """ relative cn stations of the panels of a section"""
        wx_dist_fn = self._wy_distribution_fns [self.wx_dist]
        stations = np.linspace (0, 1, self.wx_panels +1)
        for i, yni in enumerate(stations):
            stations [i] = wx_dist_fn (yni)
        return np.round (stations,10)


    def x_stations (self) -> np.ndarray:
        """ x stations of all panels - optimized for width_min"""

        x_stations, _ = self._calc_x_stations ()
        return x_stations


    def _calc_x_stations (self) -> tuple[np.ndarray, float]:
        """
        calculate x stations of all panels - optimized for width_min
        Returns: 
            x_stations: list 
            width_min_cur: minimum panel width  (mean value for cosinus oder sinus distribution)  
        """

        width_min_targ = self.width_min_targ if self.width_min_targ else 0.0
        width_min_cur  = 1.0                                    # current width min - to be calculated

        # walk along span by section and add x stations 

        xn_sec = self.n_distrib.polyline()[0]                   # take polyline as it can be already reduced

        xn_rel_stations = self._xn_rel_stations()
        xn_stations = np.array ([0.0])


        for isec in range (1, len(xn_sec)):      
            
            section_width   = xn_sec[isec] - xn_sec[isec-1]
            panel_widths    = np.diff (xn_rel_stations) * section_width

            # check and correct y panels for min panel width 
            wy_panels = self.wy_panels
            while np.mean (panel_widths) < width_min_targ and wy_panels > 2:  # mean for cosinus/sinus 

                wy_panels -= 1
                panel_widths    = np.diff (self._xn_rel_stations(wy_panels)) * section_width


            # calc new x station of panel stripes 
            for width_i in panel_widths [:-1]:
                xn_next = xn_stations[-1] + width_i
                xn_stations = np.append (xn_stations, xn_next)
            xn_stations = np.append (xn_stations, xn_sec[isec])


            width_min_cur = min (width_min_cur, np.mean (panel_widths))

        x_stations = np.round (xn_stations * self._parent_planform.span, 6)

        return x_stations , width_min_cur


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

        npan = 0 
        for x in self.x_stations():
            if x > left_sec_x and x <= right_sec_x:
                npan += 1
            elif x > right_sec_x:
                break 

        return npan                   


    def c_diff_lines (self) -> list:
        """ returns a list with lines indicating the difference between chord paneled and chord parent"""

        lines =[]

        c_diff_max = self.cn_diff_max * self.chord_root if self.cn_diff_max is not None else 0.0 

        for xi in self.x_stations():

            # actual chords at station 
            c_panel  = self.c_at (xi)
            c_parent = self._parent_planform.c_at (xi)
            c_diff   = c_parent - c_panel

            if c_diff > c_diff_max:

                # get leading and trailing edge of paneled and of parent planform 
                le_y, te_y = self.le_te_at (xi)
                le_y_parent, te_y_parent = self._parent_planform.le_te_at (xi)
                
                # line between both at le 
                line_x, line_y = [xi, xi],  [le_y_parent, le_y]
                lines.append ((line_x, line_y))

                # line between both at te 
                line_x, line_y = [xi, xi],  [te_y_parent, te_y]
                lines.append ((line_x, line_y))

        return lines 


    def reset (self):
        """ reset paneling to default options - remove all helper sections """

        self._from_dict ({})
        self.optimize ()


    def optimize (self, recalc_sections=True):
        """ 
        build optimized mesh based on current settings

        recalc_sections = False avoids re-creation of sections and re-strak of airfoils 
        """

        # remove already inserted helper sections if requested

        if recalc_sections:

            for section in self.wingSections [:]: 
                if section.is_for_panels:
                    self.wingSections.delete (section)

            # add new sections to achieve cn_diff_max

            if self._cn_diff_max:
                self._optimize_cn_diff ()

                # new sections need polarSets

                self.wingSections.refresh_polar_sets()


    def _optimize_cn_diff (self):
        """ insert new sections until chord difference is below max value """

        # if not self.is_cn_diff_exceeded: return 

        i_cycle = 1
        section_inserted = True 

        while section_inserted and i_cycle < 15:            # max iterations 

            sections = self.wingSections
            section_inserted = False 

            for i_sec in range (len(sections) - 1):

                # ensure a minimum width of a 'section stripe'

                section_width   = sections[i_sec+1].xn - sections[i_sec].xn 
                panel_width_min = self.width_min_targ if self._width_min_targ else 0.005       # default min panel width 0.05%

                if section_width > (1.5 * panel_width_min):

                    # test chord difference in the middle of the section 

                    xni = (sections[i_sec].xn + sections[i_sec+1].xn) / 2
                    cn_panel  = self._n_distrib.at (xni)
                    cn_parent = self._parent_planform.n_distrib.at (xni)

                    if (cn_parent - cn_panel) > self.cn_diff_max:

                        # too much difference - insert section at mean cn value, indicate as extra panel
                        sections.create_after (index=i_sec, is_for_panels = True)

                        section_inserted = True 
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

        self._point_le            = tuple(fromDict (myDict, "point_le", ( 20,-20))) 
        self._point_te            = tuple(fromDict (myDict, "point_te", (400,-20)))

        self._qimage              = None


    def _as_dict (self) -> dict:
        """ returns a data dict with the parameters of self"""

        d = {}
        if self.pathFilename:

            # ensure relative path to working dir 
            if os.path.isabs (self._pathFilename):
                relPath = PathHandler(workingDir= self._working_dir).relFilePath(self.pathFilename)
            else:
                relPath = self.pathFilename

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
        """ this is a valid image definition"""
        return os.path.isfile (self.pathFilename_abs) if self._pathFilename is not None else False
    
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
    def pathFilename_abs (self) -> str: 
        """ absolute pathFilename of an image e.g. jpg"""
        if self._pathFilename is None:
            return None
        elif os.path.isabs (self._pathFilename):
            return self._pathFilename
        else:
            return PathHandler(workingDir= self._working_dir).fullFilePath(self._pathFilename)


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

