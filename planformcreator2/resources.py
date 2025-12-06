#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Resource management helper

Supports both pip-installed packages and PyInstaller frozen executables.
"""

import sys
from pathlib import Path

# subdirectory for resources in PyInstaller frozen executable onedir mode
FROZEN_RESOURCES_SUBDIR   = '_internal/planformcreator2'

# directory for xo2 examples relative to resources directory
TEMPLATES_DIR             = "templates"


def _is_frozen() -> bool:
    """Check if running as PyInstaller frozen executable."""
    return getattr(sys, 'frozen', False)


def _get_resources_dir_pyinstaller() -> Path:
    """Get resources directory for PyInstaller frozen executable."""
    # PyInstaller bundles to _internal directory
    exe_dir = Path(sys.executable).parent
    return exe_dir  / FROZEN_RESOURCES_SUBDIR


def _get_resources_dir_pip() -> Path:
    """ Get resources directory for pip-installed package."""
    return Path(__file__).parent


def _get_resources_dir_dev() -> Path:
    """ Get resources directory for local development"""
    return Path(__file__).parent.parent



def get_resource_path(resource_type: str, *path_parts: str) -> Path:
    """
    Get path to a resource (icon, template, example).
    
    Works for pip packages, dev mode and PyInstaller executables.
    
    Args:
        resource_type: 'icons', 'templates', or 'examples'
        *path_parts: Additional path components (e.g., 'PC2.ico')
    
    Returns:
        Path to the resource
        
    Examples:
        >>> get_resource_path('icons', 'PC2.ico')
        >>> get_resource_path('examples', 'VJX.glide', 'VJX.glide.pc2')
    """
    if _is_frozen():
        # PyInstaller: resources are in _internal directory
        base = _get_resources_dir_pyinstaller()
    else:
        # Pip package: resources are at project root level
        # (icons/, templates/, examples/ next to planformcreator2/)
        base = _get_resources_dir_pip()

        if not (base / resource_type).exists():
            # Development mode: resources are in parent directory
            base = _get_resources_dir_dev()
    
    resource_path = base / resource_type / Path(*path_parts) if path_parts else base / resource_type

    return resource_path


def get_icon_path(icon_file_name: str) -> Path:
    """
    Get path to an icon file.
    """
    if icon_file_name:
        icon_path = get_resource_path('icons', icon_file_name)
        if icon_path.is_file():
            return icon_path
    return None


def get_icons_path() -> Path:
    """Get the icons directory path."""
    return get_resource_path('icons')


def get_template_dir() -> Path:
    """Get the templates directory path."""
    return get_resource_path('templates')


def get_examples_dir() -> Path:
    """Get the examples directory path."""
    return get_resource_path('examples')


def get_assets_dir() -> Path:
    """Get the assets directory path."""
    return get_resource_path('assets')


def get_icon_as_string(icon_name: str) -> str:
    """
    Get icon path as string for compatibility with existing code.
    
    Args:
        icon_name: Icon filename
        
    Returns:
        Path as string
    """
    return str(get_icon_path(icon_name))

