#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""

    Reference wing definitions and persisted PC2 reference-file metadata.

    Reference_PC2_File      - path and scaling configuration for a reference wing
        |-- Reference_Wing   - reference wing loaded from a PC2 file
        |-- Reference_Wing_Elliptical - generated elliptical reference wing
"""

from enum                           import StrEnum
import logging
from pathlib                        import Path
from typing                         import override

from airfoileditor.base.common_utils import Parameters, fromDict
from airfoileditor.model.polar_set   import Polar_Definition

from .planform                       import N_Distrib_Elliptical, WingSections
from .planform_mesh                  import Mesh_Strategy_Smooth
from .wing                           import PARAMETER_FILE_VERSION, Wing
from .VLM_wing                       import VLM_Polar


logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


class Scale_Mode (StrEnum):

    NO_SCALE = "No scaling"
    MATCH_SPAN_AND_AREA = "Span & Area"
    MATCH_ROOT_CHORD_AND_AREA = "Root & Area"
    MATCH_ROOT_CHORD_AND_SPAN = "Root & Span"


class Reference_PC2_File:
    """Metadata and scaling configuration for a reference PC2 file."""

    def __init__(self, 
                 dataDict: dict, 
                 workingDir_path: Path):

        # resolve and check the absolute path of the reference PC2 file

        path        = Path(fromDict(dataDict, "path", ""))
        working_dir = Path(workingDir_path).resolve(strict=False)
        path_abs    = working_dir / path if not path.is_absolute() else path
        path_abs    = path_abs.resolve(strict=False)

        if not path_abs.exists():
            raise FileNotFoundError(f"Reference PC2 file not found: {path_abs}")

        self._path = path
        self._path_abs = path_abs

        # set scale mode 
        
        scale_mode_str = fromDict(dataDict, "scale_mode", Scale_Mode.NO_SCALE.value)
        if scale_mode_str not in {mode.value for mode in Scale_Mode}:
            logger.warning(f"Invalid reference-wing scale mode '{scale_mode_str}' for '{path}'")
            self._scale_mode = Scale_Mode.NO_SCALE
        else:
            self._scale_mode = Scale_Mode(scale_mode_str)

        # no-flap for polars 

        self._no_flap = fromDict(dataDict, "no_flap", False)


    def _as_dict(self, workingDir_path: Path) -> dict:

        working_dir = Path(workingDir_path).resolve(strict=False)
        path_to_save = self.path_abs.resolve(strict=False)
        try:
            path_to_save = path_to_save.relative_to(working_dir)
        except ValueError:
            pass

        d = {"path": path_to_save.as_posix()}

        if self._scale_mode != Scale_Mode.NO_SCALE:
            d["scale_mode"] = self._scale_mode.value
        if self._no_flap:
            d["no_flap"] = self._no_flap
        return d

    @property
    def path(self) -> Path:
        return self._path

    @property
    def path_abs(self) -> Path:
        return self._path_abs

    @property
    def scale_mode(self) -> Scale_Mode:
        """How to scale the reference wing relative to the parent wing."""
        return self._scale_mode

    def set_scale_mode(self, scale_mode: Scale_Mode | str):
        if isinstance(scale_mode, str):
            scale_mode = Scale_Mode(scale_mode)
        self._scale_mode = Scale_Mode(scale_mode)

    @property
    def no_flap(self) -> bool:
        """Whether to suppress the flapped polar for this wing."""
        return self._no_flap

    def set_no_flap(self, no_flap: bool):
        self._no_flap = no_flap



class Reference_Wing (Wing):
    """Reference wing scaled relative to a parent wing."""

    is_reference_wing = True

    def __init__(self, reference_pc2_file: Reference_PC2_File, parent_wing: Wing):

        self._parent_wing = parent_wing
        self._area_normed = None
        self._no_flap     = reference_pc2_file.no_flap
        self._reference_pc2_file = reference_pc2_file

        # Avoid recursion during initialization.
        self._scale_mode = Scale_Mode.NO_SCALE

        super().__init__(reference_pc2_file.path_abs)

        self._area_normed = self.planform.calc_area(normed=True)
        self._scale_mode  = reference_pc2_file.scale_mode


    @override
    def _save(self) -> Parameters:
        """Reference wings are not saved independently."""
        return Parameters()

    @property
    def parent_wing(self) -> Wing:
        return self._parent_wing

    @property
    def name(self) -> str:
        return self.parm_path.name

    @property
    def reference_pc2_file(self) -> Reference_PC2_File:
        return self._reference_pc2_file

    @property
    def scale_mode(self) -> Scale_Mode:
        """Scaling mode of this reference wing."""
        return self._scale_mode

    def set_scale_mode(self, scale_mode: Scale_Mode | str):
        if isinstance(scale_mode, str):
            scale_mode = Scale_Mode(scale_mode)

        self._scale_mode = Scale_Mode(scale_mode)
        self._reference_pc2_file.set_scale_mode(self._scale_mode)

    @property
    def scale_mode_names(self) -> list[str]:
        return [mode.value for mode in Scale_Mode]


    @property
    def no_flap(self) -> bool:
        """Indicates whether polars of the reference wing have no flap set."""
        return self._no_flap

    def set_no_flap(self, no_flap: bool):
        self._no_flap = no_flap
        if self._reference_pc2_file is not None:
            self._reference_pc2_file.set_no_flap(no_flap)


    @override
    @property
    def halfspan(self) -> float:
        """Halfspan of the reference wing based on the scaling mode."""

        if self.scale_mode == Scale_Mode.NO_SCALE:
            return self._halfspan
        if self.scale_mode == Scale_Mode.MATCH_SPAN_AND_AREA:
            return self.parent_wing.halfspan
        if self.scale_mode == Scale_Mode.MATCH_ROOT_CHORD_AND_AREA:
            parent_area_normed = self.parent_wing.planform.calc_area(normed=True)
            span = (parent_area_normed / self._area_normed) * self.parent_wing.halfspan
            return max(0.01, span)
        if self.scale_mode == Scale_Mode.MATCH_ROOT_CHORD_AND_SPAN:
            return self.parent_wing.halfspan
        raise ValueError(f"Unsupported reference-wing scale mode: {self.scale_mode}")


    @override
    @property
    def chord_root(self) -> float:
        """Root chord of the reference wing based on the scaling mode."""

        if self.scale_mode == Scale_Mode.NO_SCALE:
            return self._chord_root
        if self.scale_mode == Scale_Mode.MATCH_SPAN_AND_AREA:
            parent_area_normed = self.parent_wing.planform.calc_area(normed=True)
            chord = (parent_area_normed / self._area_normed) * self.parent_wing.chord_root
            return max(0.01, chord)
        if self.scale_mode in (Scale_Mode.MATCH_ROOT_CHORD_AND_AREA,
                               Scale_Mode.MATCH_ROOT_CHORD_AND_SPAN):
            return self.parent_wing.chord_root
        raise ValueError(f"Unsupported reference-wing scale mode: {self.scale_mode}")


    @override
    @property
    def mass(self) -> float:
        """Mass of the reference wing as mass of parent wing"""
        return self.parent_wing.mass

    def set_mass(self, aVal: float):
        """Set mass of the reference wing - not allowed."""
        logger.warning("Setting mass of the reference wing is not allowed.")
        pass


    def _adapt_parent_polar_def(self, polar_def: Polar_Definition) -> Polar_Definition:
        """Scale / adapt (no_flap) a polar definition of parent wing."""

        polar_def_new = Polar_Definition(polar_def._as_dict())

        # apply scale mode to parent wing
        if self._scale_mode == Scale_Mode.MATCH_SPAN_AND_AREA:
            re_factor = self.chord_root / self._parent_wing.chord_root
            polar_def_new.set_re_asK(polar_def.re_asK * re_factor)

        # apply "no flap" for polar definitions if needed
        if self.no_flap and polar_def.is_flap_set:
            polar_def_new.set_flap_def (None)

        return polar_def_new


    @override
    @property
    def polar_definitions(self) -> list[Polar_Definition]:
        """Polar definitions, scaled to this wing when needed."""

        polar_defs_parent = self._parent_wing.polar_definitions

        return [self._adapt_parent_polar_def(polar_def) for polar_def in polar_defs_parent]


    def vlm_polar_at (self, parent_polar_def: Polar_Definition) -> VLM_Polar:
        """ proxy to get the VLM polar - adapts the parent polar definition to the reference wing."""

        polar_def = self._adapt_parent_polar_def(parent_polar_def)

        # sanity - does the adapted polar definition exist self polar definitions
        for p in self.polar_definitions:
            if p.is_equal_to(polar_def):

                return self.vlm_wing.polar_at(polar_def)

        logger.error(f"Adapted polar definition {polar_def} does not exist in reference wing.")
        return None


    def refresh_wingSections(self):
        """Refresh sections in subclasses; this class keeps its own sections."""
        pass



class Reference_Wing_Elliptical (Reference_Wing):
    """Generated elliptical reference wing."""

    name = "Elliptical"

    def __init__(self, parent_wing: Wing, scale_mode=Scale_Mode.MATCH_SPAN_AND_AREA):

        parameters = Parameters()
        parameters.set("pc2_version", PARAMETER_FILE_VERSION)
        parameters.set("wing_name", self.name)
        parameters.set("description", "This is a pure elliptical reference wing")
        parameters.set("halfspan", 1000.0)
        parameters.set("chord_root", 200.0)
        parameters.set("chord_distribution", {"chord_style": N_Distrib_Elliptical.name})
        parameters.set("wingSections", self._sectionsDict_of(parent_wing))
        parameters.set("panels", {
            "strategy": Mesh_Strategy_Smooth.name,
            Mesh_Strategy_Smooth.name: {"cn_tip_min": 0.1},
        })

        self._parent_wing = parent_wing
        self._area_normed = None
        self._reference_pc2_file = None
        self._scale_mode = Scale_Mode.MATCH_ROOT_CHORD_AND_SPAN
        self._no_flap    = False

        Wing.__init__(self, None, parameters=parameters, defaultDir=parent_wing.workingDir)

        self._area_normed = self.planform.calc_area(normed=True)


    @override
    @property
    def airfoils_dir_rel(self) -> str:
        """Relative airfoil directory of the parent wing."""
        return self._parent_wing.airfoils_dir_rel

    def _sectionsDict_of(self, parent_wing: Wing) -> list[dict]:
        """Relevant real-airfoil sections of the parent wing."""
        parent_sections = parent_wing.planform.wingSections.without_for_panels
        return [section._as_dict() for section in parent_sections if section.has_real_airfoil]


    @override
    def refresh_wingSections(self):
        """Refresh the sections from the parent wing."""

        wing_sections = WingSections(
            self.planform, sectionsDict=self._sectionsDict_of(self._parent_wing))
        wing_sections.check_n_repair()

        self.planform._wingSections = wing_sections

    @override
    def set_scale_mode(self, scale_mode: Scale_Mode):
        """Set the scale mode for the reference wing."""
        # elliptical reference wing always uses MATCH_ROOT_CHORD_AND_SPAN
        pass


    @override
    @property
    def sweep_angle (self) -> float:
        """ always use sweep angle of the parent wing """
        return self._parent_wing.sweep_angle
