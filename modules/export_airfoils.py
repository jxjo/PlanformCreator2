#!/usr/bin/env pythonbutton_color
# -*- coding: utf-8 -*-

"""  

Handle export of airfoils to file 

"""

import logging

from base.common_utils      import * 
from wing                   import Wing, N_WingSection, N_WingSections
from model.airfoil          import GEO_SPLINE

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


class Export_Airfoils:
    """ 
    Handle export of the current airfoils of wing to a subdirectory
    """
    def __init__(self, wing : Wing, myDict: dict = None):
 
        self.wing       = wing
        self.workingDir = wing.workingDir       
        self._export_dir         = fromDict (myDict, "export_dir", "airfoils")
        self._use_nick           = fromDict (myDict, "use_nick", True)
        self._adapt_te_gap       = fromDict (myDict, "adapt_te_gap", False)
        self._te_gap_mm          = fromDict (myDict, "te_gap_mm", 0.5)

    def _as_dict (self) -> dict:
        """ returns a data dict with the paramters of self"""

        d = {}
        toDict (d, "export_dir",        self._export_dir) 
        toDict (d, "use_nick",          self._use_nick) 
        toDict (d, "adapt_te_gap",         self._adapt_te_gap) 
        toDict (d, "te_gap_mm",         self._te_gap_mm) 
        return d

    @property
    def _chord_root (self) -> float: 
        self.wing.planform.chord_root

    @property
    def _wingSections (self) -> N_WingSections: 
        self.wing.planform.norm.wingSections

    @property
    def export_dir(self):
        """the directory for airfoils export - path is relativ to current or absolute """
        return self._export_dir
    
    def set_export_dir(self, newStr): 
        self._export_dir = PathHandler (workingDir=self.workingDir).relFilePath (newStr) # ensure a valid, relativ path 

    @property
    def baseAndExportDir(self):
        """the directory for airfoil export including current dir """
        return PathHandler (workingDir=self.workingDir).fullFilePath (self.export_dir)

    @property
    def use_nick(self) -> bool: return self._use_nick
    def set_use_nick(self, aBool): self._use_nick = aBool

    @property
    def adapt_te_gap(self) -> bool: return self._adapt_te_gap
    def set_adapt_te_gap(self, aBool): self._adapt_te_gap = aBool

    @property
    def te_gap_mm(self) -> float: return self._te_gap_mm
    def set_te_gap_mm(self, aVal): 
        self._te_gap_mm = aVal


    def do_it (self, toDir : str): 
        """ main entry: start the export to the file defined in self parameters.
        Returns list of airfoil fieNames """

        targetDir = self.baseAndExportDir

        # ensure all airfoils are up to date and splined (quality) 

        self._wingSections.do_strak (geometry=GEO_SPLINE)          

        
        # save airfoils of all sections  

        fileNames  = []
        section : N_WingSection

        for section in self._wingSections:

            if self.use_nick and section.airfoilNick(): 
                newName = section.airfoilNick()
            else: 
                newName = None

            
            if self.adapt_te_gap:                                                   # te gap in mm? if yes scale it to normed
                teGap = self.te_gap_mm / section.cn() * self._chord_root
            else: 
                teGap = None 

        
            pathFileName = section.airfoil.save_copyAs (dir=toDir, destName = newName, teGap = teGap)

            fileNames = fileNames.append (os.path.basename(pathFileName) )


        logger.info ("Airfoils written to " + targetDir) 
        # message = "Airfoils: \n\n" + \
        #           ',  '.join(airfoilList)  + \
        #           "\n\n exported to \n\n" + \
        #           "'" +  targetDir + "'"      
        return fileNames

