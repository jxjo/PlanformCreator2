#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Stable PyInstaller entry point for AirfoilEditor in PlanformCreator2 builds.

Why this file exists:
- The dual-exe build needs a concrete script path for each executable.
- Referencing this local file keeps the build independent from any external
  AirfoilEditor repository checkout path.
- Runtime behavior matches the regular AirfoilEditor startup function.
"""

import sys

from airfoileditor import app


if __name__ == '__main__':
    sys.exit(app.start())
