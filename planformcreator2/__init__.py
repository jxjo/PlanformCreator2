"""
    PlanformCreator2 Package
"""

import os
import sys

def resources_dir_pc2():
    """Get root directory for resources like ./assets, ./examples_optimize"""
    
    # in frozen exe (pyinstaller - onedir), this is the _internals dir below the exe dir 
    if getattr(sys, 'frozen', False):
        return os.path.join(os.path.dirname(sys.executable), '_internal')
    
    # in a bundled package this is the package dir equals the dir of self __file__
    resources_dir = os.path.abspath(os.path.dirname(__file__))
    if os.path.isdir(os.path.join(resources_dir, 'templates')):
        return resources_dir

    # in dev this is the main dir of the project equals the parent dir of self __file__
    resources_dir = os.path.dirname (resources_dir)
    if os.path.isdir(os.path.join(resources_dir, 'templates')):
        return resources_dir
    
    return None
