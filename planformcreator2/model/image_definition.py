#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Definition and settings for a planform background image."""

import os
from pathlib import Path

from airfoileditor.base.common_utils import *


class Image_Definition:
    """
    Describes the properties of an image which can be used e.g. for background.
    """

    def __init__(self, working_dir: str | Path, myDict: dict = None):

        self._working_dir         = Path(working_dir)
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

            working_dir = self._working_dir
            file_path = Path(self._pathFilename)

            if file_path.is_absolute():
                # Convert absolute path to relative only when it points into working_dir.
                abs_path = file_path.resolve(strict=False)
                if abs_path.is_relative_to(working_dir):
                    path_to_store = abs_path.relative_to(working_dir)
                else:
                    path_to_store = abs_path
            else:
                # Keep relative paths relative and normalize lexically to shortest form.
                # Do not force absolute resolution here, because upward relative paths
                # like '../../../Desktop/...' are valid and must retain their meaning.
                path_to_store = Path(os.path.normpath(str(file_path)))

            relPath = path_to_store.as_posix()

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
            path = self._working_dir / self._pathFilename
            return os.path.normpath(str(path))


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