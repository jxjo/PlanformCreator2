#!/usr/bin/env python

import numpy as np
import ezdxf
from ezdxf import enums
from common_utils import *
from wing_model import Wing, WingSection, Flap
from airfoil import Airfoil

class Export_Dxf:
    """ 

    Handle export of the planform and airfoils to dxf 
    Additionally export airfoils to file. 

    """
    def __init__(self, wing : Wing, myDict: dict = None):
 
        self.wing       = wing
        self.workingDir = wing.workingDir       
        self._exportDir         = fromDict (myDict, "exportDir", "dxf", msg=False)
        self._useNick           = fromDict (myDict, "useNick", True, msg=False)
        self._setTeGap          = fromDict (myDict, "setTeGap", False, msg=False)
        self._teGap_mm          = fromDict (myDict, "teGap_mm", 0.5, msg=False)
        self._includeAirfoils   = fromDict (myDict, "includeAirfoils", True, msg=False)
        self._exportAirfoils    = fromDict (myDict, "exportAirfoils", True, msg=False)


    def _save (self):
        """ returns the parameters of self in dataDict"""

        myDict = {}
        toDict (myDict, "exportDir",        self._exportDir) 
        toDict (myDict, "useNick",          self._useNick) 
        toDict (myDict, "setTeGap",         self._setTeGap) 
        toDict (myDict, "teGap_mm",         self._teGap_mm) 
        toDict (myDict, "includeAirfoils",  self._includeAirfoils) 
        toDict (myDict, "exportAirfoils",   self._exportAirfoils) 

        return myDict


    @property
    def exportDir(self):
        """the directory for flz export - path is relativ to current or absolute """
        return self._exportDir
    
    def set_exportDir(self, newStr): 
        # insure a valid, relativ path 
        self._exportDir = PathHandler (workingDir=self.workingDir).relFilePath (newStr)

    @property
    def baseAndExportDir(self):
        """the directory for dxf export including current dir """
        return PathHandler (workingDir=self.workingDir).fullFilePath (self.exportDir)

    @property
    def useNick(self) -> bool: return self._useNick
    def set_useNick(self, aBool): self._useNick = aBool

    @property
    def includeAirfoils(self) -> bool: return self._includeAirfoils
    def set_includeAirfoils(self, aBool): self._includeAirfoils = aBool


    @property
    def setTeGap(self) -> bool: return self._setTeGap
    def set_setTeGap(self, aBool): self._setTeGap = aBool

    @property
    def teGap_mm(self) -> float: return self._teGap_mm
    def set_teGap_mm(self, aVal): self._teGap_mm = aVal

    @property
    def exportAirfoils(self) -> bool: return self._exportAirfoils
    def set_exportAirfoils(self, aBool): self._exportAirfoils = aBool

    @property
    def fileName(self): 
        return self.wing.name.strip() +  '_wing.dxf'


    def doIt (self): 
        """ main entry: start the export to the file defined in self parameters.
        Returns a message string what was done """

        if self.setTeGap: 
            teGap = self.teGap_mm
        else: 
            teGap = None

        dxf = Dxf_Artist(self.wing)

        dxf.plot_planform()
        dxf.plot_hingeLine ()
        dxf.plot_wingSections ()
        dxf.plot_flapLines()
        dxf.plot_title ()

        if self.includeAirfoils:
            self.wing.do_strak ()                    # ensure strak airfoils are uptodate 
            dxf.plot_airfoils (teGap_mm=teGap)

        targetDir = self.baseAndExportDir

        if not os.path.exists(targetDir): os.makedirs(targetDir)
        pathFileName = os.path.join (targetDir, self.fileName)
        dxf.save (pathFileName)

        # export airfoils 
        if self.exportAirfoils:
                airfoilList = self.wing.do_export_airfoils (targetDir, useNick=self.useNick, teGap_mm=teGap)
        else:
            airfoilList = []

        if airfoilList: 
            text = 'including airfoils '
        else:
            text = ''

        InfoMsg ("DXF file " + self.fileName + " "+  text + "written to " + targetDir) 
        message = "Wing " + text + "exported as DXF to \n\n'" + targetDir + "'"

        return message



class Dxf_Artist:
    """ 
    - open an dxf document 
    - 'plots' different wing artefacts into dxf document 
    - save dxf document to file   
    """
    def __init__(self, wing : Wing): 

        self.wing = wing
        self.doc = ezdxf.new('R2010')
        self.msp = self.doc.modelspace()


    def _arr_to_poly (self, x,y):
        """ converts the two x,y arrays to an array of points (x,y) """
        poly = []
        for i, x in enumerate(x): 
            poly.append ((x, y[i]))
        return poly


    def _line_mirror (self, points) -> list: 
        """ mirrors a line e.g. le along y that 
         - te point of root will be at 0,0 
         - le point of root will be at 0, rootchord"""
        mirrored_points = np.empty (len(points))
        x_mirror = self.wing.rootchord / 2               # flip x  around half rootchord
        for i, x in enumerate(points):
            mirrored_points [i] = x_mirror - (x - x_mirror)
        return mirrored_points


    def _plot_line_fromPoints (self, pointList ):
        """plots a (poly) line defined by an array of points """

        self.msp.add_lwpolyline (pointList)

    def _plot_line_fromArray (self, x: list , y:list ):
        """plots a (poly) line defined by two arrays x and y """

        self.msp.add_lwpolyline (self._arr_to_poly (x,y))

    # --------  public ----------------

    def plot_planform (self):
        
        y, le, te, = self.wing.planform.lines()

        # mirror the lines along span so that root-te will be at 0,0 
        le = self._line_mirror (le)
        te = self._line_mirror (te)

        # make a single line for the planform contour from 0,0 - root - le - tip - te - root 
        y_c = [0.0]
        x_c = [0.0]
        y_c.extend(y)
        x_c.extend(le)
        y  = np.flip(y)
        te = np.flip(te)
        y_c.extend(y)
        x_c.extend(te)

        # insert into dxf doc
        self._plot_line_fromArray (y_c, x_c)


    def plot_hingeLine (self):

        y, hl = self.wing.planform.hingeLine()
        # mirror the lines along span so that root-te will be at 0,0 
        hl = self._line_mirror (hl)
        # insert into dxf doc
        self._plot_line_fromArray (y, hl)


    def plot_flapLines (self):

        flaps = self.wing.getFlaps()

        flap : Flap
        for i, flap in enumerate (flaps):
            if i < (len (flaps) - 1):                    # no flap line at tip 

                y, fl = flap.lineRight                  # only one line of flap box needed
                fl = self._line_mirror (fl)
                # insert into dxf doc
                self._plot_line_fromArray (y, fl)


    def plot_wingSections (self):
        # plot a little vertical marker line at wing section station above le 
        #  + airfoil nick name

        fontsize = (self.wing.rootchord / 230.0) * 7.0

        sec : WingSection
        for sec in self.wing.wingSections:

            _, le_te = sec.line()               # le from sec line - we have to mirror 
            x_m = self._line_mirror (le_te)[0] + 20  

            y_m = sec.yPos
            p1 = (y_m, x_m)
            p2 = (y_m, x_m + 15) 
            line = [p1,p2]
            self._plot_line_fromPoints (line)

            if sec.airfoilNick():
                airfoilText = "'"+ sec.airfoilNick()+"'" 
                self.msp.add_text(airfoilText, height = fontsize).set_placement(
                                    (y_m, x_m+35), align=enums.TextEntityAlignment.CENTER)

            airfoilText = sec.airfoilName()
            self.msp.add_text(airfoilText, height = fontsize).set_placement(
                                (y_m, x_m+20), align=enums.TextEntityAlignment.CENTER)

    def plot_airfoils (self, teGap_mm = None):
        # plot the airfoils in real size to the left of the planform 
        # an absolute Te gap is set    

        # ! here in dxf and airfoil coordinate system (not wing) !

        airfoil: Airfoil
        sec : WingSection
        for sec in self.wing.wingSections:

            teGap = None 

            # te gap in mm? if yes scale it to normed ... do it
            if not teGap_mm is None and teGap_mm >= 0.0: 
                teGap = teGap_mm / sec.chord
                airfoil = Airfoil.asCopy (sec.airfoil)
                airfoil.set_teGap_perc(teGap * 100)
            else: 
                airfoil = sec.airfoil

            x = airfoil.x
            y = airfoil.y

            # scale to real size 
            x = x * sec.chord
            y = y * sec.chord

            x = x + sec.yPos - sec.chord /4     # center t/4 above ypos of section 

            _, le_te = sec.line()               # le from sec line - we have to mirror 
            y_m = self._line_mirror (le_te)[0] + 20 + 80 
            y = y + y_m                         # shift upward 

            self._plot_line_fromArray (x,y)

            # plot te gap info if it was set 
            if teGap: 
                # just above te a small text marker 
                y_m = y[0] + 20 
                x_m = x[0]
                fontsize = 4 
                text = "Te gap = %.1f mm" % teGap_mm  
                self.msp.add_text(text, height = fontsize).set_placement(
                                (x_m, y_m), align=enums.TextEntityAlignment.CENTER)


   

    def plot_title (self):
        # plot wing name at the bottom
        x_m = - self.wing.rootchord * 0.2
        y_m = 0.0
        fontsize = (self.wing.rootchord / 230.0) * 10.0
        text = self.wing.name
        self.msp.add_text(text, height = fontsize).set_placement(
                            (y_m, x_m), align=enums.TextEntityAlignment.TOP_LEFT)

        x_m = x_m - 20 
        fontsize = (self.wing.rootchord / 230.0) * 6.0
        text = "Generated by Planform Creator 2"
        self.msp.add_text(text, height = fontsize).set_placement(
                            (y_m, x_m), align=enums.TextEntityAlignment.TOP_LEFT)


    def save(self, pathFileName):
        """ save the current dxf document to pathFileName"""  
        
        self.doc.saveas(pathFileName)  



