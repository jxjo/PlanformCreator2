#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Paneled mesh derived from a planform, using x chordwise and y spanwise."""

from __future__         import annotations
from typing             import TYPE_CHECKING, override

import logging
from time               import perf_counter

import numpy as np

from airfoileditor.base.common_utils import clip, fromDict, toDict

if TYPE_CHECKING:
    from .wing import Planform, WingSection, WingSections


logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


class Mesh_Strategy:
    """Strategy interface for mesh span and edge geometry."""

    name = "abstract"

    is_smooth       = False
    is_trapezoidal  = False

    _wy_distribution_fns = {
        "uniform": lambda y: y,
        "-sine": lambda y: np.sin (y * np.pi / 2),
        "sine": lambda y: (np.sin ((y + 1) * np.pi / 2) - 1.0) * -1.0,
        "cosine": lambda y: (np.cos ((y + 1) * np.pi) + 1) / 2,
    }
    _wx_distribution_fns = {
        "uniform": lambda x: x,
        "cosine": lambda x: (np.cos ((x + 1) * np.pi) + 1) / 2,
    }

    PANEL_WIDTH_MIN       = 0.005                       # Minimum width of a spanwise panel
    SECTION_DIST_MIN      = 0.01                        # Minimum distance between sections


    def __init__ (self, mesh: "Planform_Mesh", dataDict: dict | None = None):
        """Bind this strategy to its mesh and load its configuration."""

        self.mesh = mesh

        self._wx_panels         = None
        self._wx_dist           = None
        self._wy_dist           = None
        self._cn_tip_min        = None
        self._cn_ratio_min      = None
        self._width_min_targ    = None


        self._from_dict (dataDict)


    def _as_dict (self) -> dict:
        """Serialize configuration shared by all mesh strategies."""
        data = {}
        toDict (data, "wx_panels", self._wx_panels)
        toDict (data, "wx_distribution", self._wx_dist)
        toDict (data, "wy_distribution", self._wy_dist)
        toDict (data, "cn_tip_min", self._cn_tip_min)
        toDict (data, "cn_ratio_min", self._cn_ratio_min)
        toDict (data, "width_min", self._width_min_targ)
        return data


    def _from_dict (self, data: dict | None):
        """Load configuration shared by all mesh strategies."""
        if not data:
            data = {}
        self._wx_panels = fromDict (data, "wx_panels", 4)
        self._wx_dist = fromDict (data, "wx_distribution", "uniform")
        self._wy_dist = fromDict (data, "wy_distribution", "uniform")
        self.set_cn_tip_min (fromDict (data, "cn_tip_min", 0.0))
        self.set_cn_ratio_min (fromDict (data, "cn_ratio_min", None))
        self.set_width_min_targ (fromDict (data, "width_min", self.PANEL_WIDTH_MIN))


    def _clean_sections_cn_tip_min (self):
        """ delete all helper sections which are beyond the minimum tip chord """

        if self.cn_tip_min is None:
            return

        wingSections = self.wingSections(all=True)[:]
        for section in wingSections:
            if section.is_for_panels and section.cn < self.cn_tip_min:
                self.mesh.wingSections.remove(section)


    @property
    def wx_panels (self) -> int:
        """Return the number of chordwise mesh panels."""
        return self._wx_panels


    def set_wx_panels (self, value: int):
        """Set the chordwise panel count."""
        self._wx_panels = int (value)


    @property
    def wx_dist (self) -> str:
        """Return the name of the chordwise station distribution."""
        return self._wx_dist


    def set_wx_dist (self, value: str):
        """Select a supported chordwise station distribution."""
        if value in self._wx_distribution_fns:
            self._wx_dist = value


    @property
    def wy_dist (self) -> str:
        """Return the name of the spanwise station distribution."""
        return self._wy_dist


    def set_wy_dist (self, value: str):
        """Select a supported spanwise station distribution."""
        if value in self._wy_distribution_fns:
            self._wy_dist = value


    @property
    def wx_distribution_fns_names (self) -> list[str]:
        """Return the supported chordwise station distribution names."""
        return list (self._wx_distribution_fns.keys ())


    @property
    def width_min_targ (self) -> float | None:
        """Requested minimum panel width, if enabled as fraction of the span."""
        return self._width_min_targ


    def set_width_min_targ (self, value: float | None):
        """Set the target minimum spanwise panel width."""
        if value == 0.0:
            self._width_min_targ = round (self.width_min_cur + self.PANEL_WIDTH_MIN, 2)
        elif value is not None:
            self._width_min_targ = clip (value, 0.001, 0.2)
        else:
            self._width_min_targ = None


    @property
    def width_min_cur (self) -> float:
        """narrowest current panel width as fraction of the span."""
        widths = np.diff (self.y_stations ())
        if len(widths) == 0:
            min_width = 0.0
        else:
            min_width = np.min (widths) / self.mesh.span
        return float (min_width)

    @property
    def wy_distribution_fns_names (self) -> list[str]:
        """Return the supported spanwise station distribution names."""
        return list (self._wy_distribution_fns.keys ())


    @property
    def cn_tip_min (self) -> float | None:
        """Return the normalized chord below which the mesh tip is omitted."""

        if self._cn_tip_min is not None:

            # sanity - ensure cn_tip_min is within the range of the last and second wing sections
            wingSections = self.wingSections(all=True)
            if len (wingSections) <= 2:
                self._cn_tip_min = None
                return None
            cn_min = wingSections[-1].cn
            cn_max = wingSections[1].cn
            self._cn_tip_min = clip (self._cn_tip_min, cn_min, cn_max)
            return round (self._cn_tip_min, 3)
        
        return None


    def set_cn_tip_min (self, value: float | None):
        """Set and constrain the minimum normalized chord retained at the tip."""

        wingSections = self.wingSections(all=True)

        if len (wingSections) <= 2:
            self._cn_tip_min = None
        elif value == 0.0:
            self._cn_tip_min = round (wingSections[-1].cn + 0.005, 2)
        elif value is not None:
            cn_min = wingSections[-1].cn
            cn_max = wingSections[1].cn
            self._cn_tip_min = clip (value, cn_min, cn_max)
        else:
            self._cn_tip_min = None


    @property
    def cn_ratio_min (self) -> float | None:
        """Minimum chord ratio between adjacent sections, if enabled."""
        return self._cn_ratio_min


    def set_cn_ratio_min (self, value: float | None):
        """Set the minimum chord ratio between adjacent sections."""
        self._cn_ratio_min = clip (value, 0.05, 1.0) if value is not None else None


    @property
    def cn_ratio_cur (self) -> float:
        """Return the lowest chord ratio between adjacent retained sections."""

        sections = self.wingSections(all_reduced=True)
        ratios = []
        for index in range (len (sections) - 1):
            cn_left  = sections[index].cn
            cn_right = sections[index + 1].cn
            if cn_left:
                ratios.append (cn_right / cn_left)

        return min (ratios) if ratios else 1.0


    def _apply_cn_ratio_min (self):
        """Insert helper sections until adjacent section chord ratios are high enough."""

        if self.cn_ratio_min is None:
            return

        cycle = 1
        section_inserted = True

        while section_inserted and cycle < 15:

            sections = self.wingSections(all=True)
            section_inserted = False
            for index in range (len (sections) - 1):
                section_width = sections[index + 1].xn - sections[index].xn
                cn_left  = sections[index].cn
                cn_right = sections[index + 1].cn
                if section_width >= 2 * self.SECTION_DIST_MIN and cn_left and cn_right / cn_left < self.cn_ratio_min:
                    self.mesh.wingSections.create_after (index=index, is_for_panels=True)
                    section_inserted = True
                    break
            cycle += 1


    @property
    def cn_tip_cur (self) -> float:
        """Return the normalized chord at the current mesh tip."""
        return self.cn_polyline ()[1][-1]


    @property
    def cn_rel_stations (self) -> np.ndarray:
        """Return normalized chordwise stations for the selected distribution."""
        wx_dist_fn = self._wx_distribution_fns[self.wx_dist]
        stations = np.linspace (0, 1, self.wx_panels + 1)
        for index, xn in enumerate (stations):
            stations[index] = wx_dist_fn (xn)
        return np.round (stations, 10)


    def cn_polyline (self) -> tuple[np.ndarray, np.ndarray]:
        """section positions and normalized chord values -> trapezoidal"""

        raise NotImplementedError


    def cn_at (self, yn: float) -> float:
        """Interpolate the normalized mesh chord at normalized span position ``yn``."""
        yn_arr, cn_arr = self.cn_polyline ()
        return round (np.interp (yn, yn_arr, cn_arr), 10)


    def yn_rel_stations (self, wy_panels: int) -> np.ndarray:
        """Build normalized spanwise stations for the selected distribution."""
        wy_dist_fn = self._wy_distribution_fns[self.wy_dist]
        stations = np.linspace (0, 1, wy_panels + 1)
        for index, yn in enumerate (stations):
            stations[index] = wy_dist_fn (yn)
        return np.round (stations, 10)


    def yn_max (self) -> float:
        """Return the maximum normalized spanwise station for the mesh."""

        if self.cn_tip_min is not None:
            for section in reversed (self.wingSections()):
                if section.cn >= self.cn_tip_min:
                    return section.xn
        return 1.0


    def y_stations (self) -> np.ndarray:
        """Return the spanwise mesh stations in planform coordinates."""
        raise NotImplementedError


    def le_te_polyline (self) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
        """Return spanwise leading- and trailing-edge polylines for the mesh."""
        raise NotImplementedError


    def x_stations_for (self, y_stations : np.ndarray) -> np.ndarray:
        """Return chordwise mesh stations at every spanwise station."""

        cn_stations = self.cn_rel_stations
        y_poly, le_x, te_x = self.le_te_polyline ()
        le_x_stations = np.interp (y_stations, y_poly, le_x)
        te_x_stations = np.interp (y_stations, y_poly, te_x)
        return le_x_stations[:, np.newaxis] + cn_stations * (te_x_stations - le_x_stations)[:, np.newaxis]


    def wingSections (self, only_real = False, all = False, all_reduced = True) -> list[WingSection]:
        """
        list of wing sections depending on argument. Reduced sections are returned by default.

        Args:
            only_real: return only the real sections
            all: return all sections
            all_reduced: return all without the sections beyond cn_tip_min
        """
        if only_real:
            sections =  [section for section in self.mesh.wingSections if not section.is_for_panels]
        elif all:
            sections = self.mesh.wingSections
        elif all_reduced:
            sections = self.mesh.wingSections
            for index, section in enumerate (self.mesh.wingSections):
                if self.cn_tip_min and round (section.cn, 2) < self.cn_tip_min:
                    sections = sections[:index]
                    break
        else:
            sections = []
        return sections



    def ny_panels_of_section (self, index: int) -> int:
        """Return the number of spanwise panels in the indexed wing section."""
        
        left_y = self.wingSections()[index].x
        if index < len (self.wingSections()) - 1:
            right_y = self.wingSections()[index + 1].x
        elif index == len (self.wingSections()) - 1:
            right_y = left_y
        else:
            raise ValueError (f"Index {index} to get wing section is to high")

        return int (np.count_nonzero ((self.y_stations () > left_y) & (self.y_stations () <= right_y)))


    def c_diff_lines (self) -> list:
        """Return chord-deviation line segments, if supported by this strategy."""
        return []


    def reset (self):
        """Reset current strategy settings."""

        self._from_dict ({})



class Mesh_Strategy_Trapezoidal (Mesh_Strategy):
    """Section-wise trapezoidal mesh, compatible with XFLR5 and FLZ."""

    name = "trapezoidal"

    is_trapezoidal  = True

    def __init__ (self, mesh: "Planform_Mesh", dataDict: dict | None = None):
        """Create a trapezoidal strategy with its configuration."""

        self._wy_panels         = None
        self._cn_diff_max       = None

        super().__init__ (mesh, dataDict)


    def _as_dict (self) -> dict:
        """Serialize trapezoidal mesh configuration."""
        data = super()._as_dict ()
        toDict (data, "wy_panels", self._wy_panels)
        toDict (data, "width_min", self._width_min_targ)
        toDict (data, "cn_diff_max", self._cn_diff_max)
        return data


    def _from_dict (self, data: dict | None):
        """Load trapezoidal mesh configuration."""
        super()._from_dict (data)
        if not data:
            data = {}
        self._wy_panels = fromDict (data, "wy_panels", 8)
        self._width_min_targ = fromDict (data, "width_min", 0.015)
        self._cn_diff_max = fromDict (data, "cn_diff_max", 0.02)


    @property
    def wy_panels (self) -> int:
        """Return the requested spanwise panel count per section."""
        return self._wy_panels


    def set_wy_panels (self, value: int):
        """Set the requested spanwise panel count per section."""
        self._wy_panels = int (value)


    @property
    def cn_diff_max (self) -> float | None:
        """Return the enabled maximum normalized chord deviation, if any."""
        return self._cn_diff_max


    def set_cn_diff_max (self, value: float | None):
        """Set the chord-deviation limit and optimize mesh helper sections."""
        self._cn_diff_max = clip (value, 0.0, 0.2) if value is not None else None


    @override
    def cn_polyline (self, reduced=False) -> tuple[np.ndarray, np.ndarray]:
        """
        section positions and normalized chord values -> trapezoidal
        If reduced is True, return only upto wingsection.cn > cn_tip_min
        """
        yn, cn = [], []

        wingSections = self.wingSections (all_reduced=True) if reduced else self.wingSections (all=True)

        for section in wingSections:
            yn.append (section.xn)
            cn.append (section.cn)

        return np.array (yn), np.array (cn)


    def cn_at_y (self, y: float) -> float:
        """Return the normalized trapezoidal chord at a spanwise position."""
        return self.cn_at (y / self.mesh.span)


    def c_at (self, y: float) -> float:
        """Return the trapezoidal chord at a spanwise position."""
        return self.cn_at_y (y) * self.mesh.chord_root

    @property
    def cn_diff (self) -> float:
        """Return the greatest normalized chord deficit relative to the parent."""
        cn_diff = 0.0
        for y in self.y_stations ():
            c_diff = self.mesh.parent_planform.c_at (y) - self.c_at (y)
            cn_diff = max (c_diff / self.mesh.chord_root, cn_diff)
        return cn_diff


    def y_stations (self) -> np.ndarray:
        """Return stations distributed independently within each retained section."""

        self._apply_cn_ratio_min ()

        # optimize deviation - insert helper sections
        if self.cn_diff_max:
            self._optimize_cn_diff ()

        # clean up sections that are beyond the minimum tip chord
        self._clean_sections_cn_tip_min ()

        # get the spanwise stations of the wing sections up to cutted tip (reduced)
        yn_sec = self.cn_polyline (reduced=True)[0]

        # generate panels between the retained sections
        yn_rel_stations = self.yn_rel_stations (self.wy_panels)

        yn_stations = self._apply_width_min (yn_sec, yn_rel_stations)

        return np.round (yn_stations * self.mesh.span, 6)


    def _apply_width_min (self, yn_sec: np.ndarray, yn_rel_stations: np.ndarray) -> np.ndarray:
        """Build section-wise stations, reducing panels in narrow sections."""

        yn_stations = np.array ([0.0])                              # normalized spanwise stations
        width_min_targ = self.width_min_targ if self.width_min_targ else 0.0

        for index in range (1, len (yn_sec)):
            section_width = yn_sec[index] - yn_sec[index - 1]
            panel_widths = np.diff (yn_rel_stations) * section_width
            wy_panels = self.wy_panels

            while np.mean (panel_widths) < width_min_targ and wy_panels > 2:
                wy_panels -= 1
                panel_widths = np.diff (self.yn_rel_stations (wy_panels)) * section_width

            for width in panel_widths[:-1]:
                yn_stations = np.append (yn_stations, yn_stations[-1] + width)
            yn_stations = np.append (yn_stations, yn_sec[index])

        return yn_stations


    def le_te_polyline (self) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
        """Return straight leading and trailing edges through the mesh sections."""

        y, le_x, te_x = [], [], []

        for section in self.wingSections(all_reduced=True):
            y.append(section.x)
            le, te = section.le_te ()
            le_x.append(le)
            te_x.append(te)
        return y, le_x, te_x


    def _optimize_cn_diff (self):
        """Insert helper sections until chord deficit meets the configured limit."""

        cycle = 1
        section_inserted = True

        while section_inserted and cycle < 15:

            sections = self.wingSections(all=True)
            section_inserted = False
            for index in range (len (sections) - 1):
                section_width = sections[index + 1].xn - sections[index].xn
                panel_width_min = self.width_min_targ if self.width_min_targ else self.PANEL_WIDTH_MIN
                if section_width >= 2 * self.SECTION_DIST_MIN and section_width > 1.5 * panel_width_min:
                    yn_mid    = (sections[index].xn + sections[index + 1].xn) / 2
                    cn_mesh   = self.cn_at (yn_mid)
                    cn_parent = self.mesh.parent_planform.n_distrib.at (yn_mid)
                    if cn_parent - cn_mesh > self.cn_diff_max:
                        self.mesh.wingSections.create_after (index=index, is_for_panels=True)
                        section_inserted = True
                        break
            cycle += 1


    def c_diff_lines (self) -> list:
        """Return edge-gap line segments where chord deviation exceeds its limit."""
        
        lines = []
        c_diff_max = self.cn_diff_max * self.mesh.chord_root if self.cn_diff_max is not None else 0.0
        y_poly, le_x, te_x = self.le_te_polyline ()

        for y in self.y_stations ():
            c_panel = self.c_at (y)
            c_parent = self.mesh.parent_planform.c_at (y)
            if c_parent - c_panel > c_diff_max:
                le_x_mesh = np.interp (y, y_poly, le_x)
                te_x_mesh = np.interp (y, y_poly, te_x)
                le_x_parent, te_x_parent = self.mesh.parent_planform.le_te_at (y)
                lines.append (([y, y], [le_x_parent, le_x_mesh]))
                lines.append (([y, y], [te_x_parent, te_x_mesh]))
        return lines



class Mesh_Strategy_Smooth (Mesh_Strategy):
    """Smooth parent-planform geometry with one global spanwise panel count."""

    name = "smooth"

    is_smooth       = True

    def __init__ (self, mesh: "Planform_Mesh", dataDict: dict | None = None):
        """Create a smooth strategy with its configuration."""

        self._wy_panels_span = None
        super().__init__ (mesh, dataDict)


    def _as_dict (self) -> dict:
        """Serialize smooth mesh configuration."""
        data = super()._as_dict ()
        toDict (data, "wy_panels_span", self._wy_panels_span)
        return data


    def _from_dict (self, data: dict | None):
        """Load smooth mesh configuration."""
        if not data:
            data = {}
        super()._from_dict (data)

        self._wy_dist = fromDict (data, "wy_distribution", "-sine")
        self._wy_panels_span = fromDict (data, "wy_panels_span", 30)
        self.set_cn_ratio_min (fromDict (data, "cn_ratio_min", 0.4))


    @property
    def wy_panels_span (self) -> int:
        """Return the total spanwise panel count for the smooth mesh."""
        return self._wy_panels_span


    def set_wy_panels_span (self, value: int):
        """Set the total spanwise panel count for the smooth mesh."""
        npanels = clip (value, 5, 100)
        self._wy_panels_span = int (npanels)


    @override
    def cn_polyline (self, reduced = False) -> tuple[np.ndarray, np.ndarray]:
        """
        normalized planform distribution (cn values along the span).
        If reduced is True, return only upto wingsection.cn > cn_tip_min
        """

        yn, cn = self.mesh.parent_planform.cn_polyline (normed=True)

        if reduced and self.cn_tip_min is not None:
            mask = cn > self.cn_tip_min
            yn = yn[mask]
            cn = cn[mask]
        return yn, cn


    def _apply_width_min (self, yn_stations: np.ndarray) -> np.ndarray:
        """Stretch narrow panels while preserving the mesh root and tip."""

        if self.width_min_targ is None:
            return yn_stations

        panel_widths = np.diff (yn_stations)
        if len (panel_widths) == 0:
            return yn_stations

        yn_range = yn_stations[-1] - yn_stations[0]
        width_max = yn_range / len (panel_widths)
        if self.width_min_targ >= width_max:
            logger.warning (f"{self}: width_min {self.width_min_targ:.3f} cannot be met with {len (panel_widths)} panels")
            return np.linspace (yn_stations[0], yn_stations[-1], len (yn_stations))

        width_min = self.width_min_targ

        # Panels narrower than the target are stretched to the target width.
        # The other panels are shortened proportionally, like a rubber band,
        # so that root and tip remain fixed.
        is_stretched = panel_widths < width_min
        while True:
            width_stretched = np.sum (is_stretched) * width_min
            width_remaining = yn_range - width_stretched
            widths_remaining = panel_widths[~is_stretched]

            if len (widths_remaining) == 0:
                panel_widths[:] = width_min
                break

            panel_widths[is_stretched] = width_min
            panel_widths[~is_stretched] = widths_remaining * width_remaining / np.sum (widths_remaining)

            # A shortened panel may now fall below the target. Lock it and
            # redistribute once more. If no new panel needs locking, stop.
            is_newly_stretched = (panel_widths < width_min) & ~is_stretched
            if not np.any (is_newly_stretched):
                break
            is_stretched |= is_newly_stretched

        yn_adjusted = np.empty_like (yn_stations)
        yn_adjusted[0] = yn_stations[0]
        yn_adjusted[1:] = yn_adjusted[0] + np.cumsum (panel_widths)
        return yn_adjusted
    

    def _align_to_sections (self, yn_stations: np.ndarray) -> np.ndarray:
        """Move stations so that every wing section lies on a panel boundary."""

        sections = self.wingSections (all_reduced=True)
        section_yn = np.array ([section.xn for section in sections])

        if len (section_yn) > len (yn_stations):
            logger.warning (f"{self}: not enough y stations for all wing sections")
            return yn_stations

        # Reserve a different, nearby station for every section. Reserving stations
        # from root to tip ensures that their indices, and therefore their order,
        # remain strictly increasing.
        station_indices = []
        i_first = 0
        for i_section, yn_section in enumerate (section_yn):

            n_sections_remaining = len (section_yn) - i_section - 1
            i_last = len (yn_stations) - n_sections_remaining - 1
            candidate_indices = np.arange (i_first, i_last + 1)
            i_nearest = candidate_indices[np.argmin (np.abs (yn_stations[candidate_indices] - yn_section))]
            station_indices.append (i_nearest)
            i_first = i_nearest + 1

        yn_adjusted = np.copy (yn_stations)

        # Stretch the original station interval between two reserved stations to
        # the corresponding section interval. This is the rubber-band step.
        for i_section in range (len (section_yn) - 1):

            i_start = station_indices[i_section]
            i_end   = station_indices[i_section + 1]
            yn_start = yn_stations[i_start]
            yn_end   = yn_stations[i_end]
            section_start = section_yn[i_section]
            section_end   = section_yn[i_section + 1]

            for i_station in range (i_start, i_end + 1):
                fraction = (yn_stations[i_station] - yn_start) / (yn_end - yn_start)
                yn_adjusted[i_station] = section_start + fraction * (section_end - section_start)

        return yn_adjusted


    def y_stations (self) -> np.ndarray:
        """Return globally distributed stations from root to the retained tip."""

        self._apply_cn_ratio_min ()

        yn_max = self.yn_max() 

        # raw distribution up to y_max
        yn_stations = self.yn_rel_stations (self.wy_panels_span) * yn_max 

        # stretch narrow root and tip panels before fixing section stations
        yn_stations = self._apply_width_min (yn_stations)

        # align stations to wing sections
        yn_stations = self._align_to_sections (yn_stations)

        return np.round (yn_stations * self.mesh.span, 6)


    def le_te_polyline (self) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
        """Return the parent planform's smooth edges up tip."""

        y, le_y, te_y = self.mesh.parent_planform.le_te_polyline ()
        return y, le_y, te_y



class Planform_Mesh:
    """Paneled mesh derived from a planform, using x chordwise and y spanwise."""


    def __init__ (self, parent_planform: Planform, dataDict: dict | None = None):
        """Create a mesh derived from a planform.

        Args:
            parent_planform: Planform that provides geometry and wing sections.
            dataDict: Optional persisted paneling settings.
        """
        self._parent_planform = parent_planform

        # read and build strategies from the provided data dictionary
        self._strategies = {}
        for strategy_class in [Mesh_Strategy_Trapezoidal, Mesh_Strategy_Smooth]:
            name     = strategy_class.name
            data     = fromDict (dataDict, name, {})
            strategy = strategy_class(self, dataDict=data)
            self._strategies[name] = strategy

        # current mesh strategy
        self._strategy : Mesh_Strategy | None = None
        self._from_dict (dataDict)


    def __repr__(self) -> str:
        # overwrite to get a nice print string
        return f"<{type(self).__name__}>"

    def _as_dict (self) -> dict:
        """Serialize mesh settings for the owning wing's parameter data."""
        data = {}
        strategy : Mesh_Strategy 
        for name, strategy in self._strategies.items ():
            toDict (data, name, strategy._as_dict ())
        toDict (data, "strategy", self.strategy.name)
        return data


    def _from_dict (self, data: dict | None):
        """Load mesh settings, using the standard paneling defaults when absent."""

        name = fromDict (data, "strategy", Mesh_Strategy_Trapezoidal.name)
        self.set_strategy_name (name)


    @property
    def parent_planform (self) -> Planform:
        """Return the planform from which this mesh is derived."""
        return self._parent_planform


    @property
    def wingSections (self) -> WingSections:
        """Return the parent planform's sections, including mesh helper sections."""
        return self.parent_planform.wingSections


    @property
    def span (self) -> float:
        """Return the parent planform's half-span in planform coordinates."""
        return self.parent_planform.span


    @property
    def chord_root (self) -> float:
        """Return the parent planform's root chord."""
        return self.parent_planform.chord_root


    def wingSections_reduced (self) -> list[WingSection]:
        """Return sections that remain within the mesh's configured tip extent."""
        return self.strategy.wingSections (all_reduced=True)


    @property
    def strategy_name (self) -> str:
        """Return the active mesh geometry strategy name."""
        return self.strategy.name if self.strategy is not None else ""


    def set_strategy_name (self, value: str):
        """Select a mesh geometry strategy and rebuild its optimized data.

        Args:
            value: Supported strategy name from :attr:`strategy_names`.
        """
        if self.parent_planform.n_distrib.isTrapezoidal:
            value = Mesh_Strategy_Trapezoidal.name
        try:
            self._strategy = self._strategies[value]
        except:
            if self._strategy is None:
                self._strategy = self._strategies[Mesh_Strategy_Trapezoidal.name]


    @property
    def strategy_names (self) -> list[str]:
        """Return the supported mesh geometry strategy names."""
        if self.parent_planform.n_distrib.isTrapezoidal:
            return [Mesh_Strategy_Trapezoidal.name]
        return [Mesh_Strategy_Trapezoidal.name, Mesh_Strategy_Smooth.name]


    @property
    def strategy (self) -> Mesh_Strategy:
        """Return the strategy that defines the current mesh."""
        return self._strategy


    @property
    def strategy_trapezoidal (self) -> Mesh_Strategy_Trapezoidal:
        """Return the persistent trapezoidal mesh strategy."""
        return self._strategies[Mesh_Strategy_Trapezoidal.name]


    @property
    def strategy_smooth (self) -> Mesh_Strategy_Smooth:
        """Return the persistent smooth mesh strategy."""
        return self._strategies[Mesh_Strategy_Smooth.name]


    def create_mesh (self) -> tuple[np.ndarray, np.ndarray]:
        """Return the full mesh as a tuple of spanwise and chordwise stations.

        Returns:
            A tuple containing:
                - Spanwise station coordinates in planform units from root to mesh tip.
                - Chordwise station coordinates at every spanwise station, indexed as ``[y_station, x_station]``.
        """

        t0 = perf_counter ()

        # always remove helper sections before optimizing
        for section in self.wingSections[:]:
            if section.is_for_panels:
                self.wingSections.delete (section)
        wingSections_before = self.wingSections[:] 

        y_stations = self.strategy.y_stations ()
        t1 = perf_counter ()
        x_stations = self.strategy.x_stations_for (y_stations)


        # Refresh polar sets if wing sections have changed
        if wingSections_before != self.wingSections:
            self.wingSections.refresh_polar_sets ()

        logger.debug (f"{self} create_mesh y: {t1 - t0:.5f}s x: {perf_counter () - t1:.5f}s")

        return y_stations, x_stations


    def reset (self):
        """ reset current strategy settings"""

        self.strategy.reset ()

