#!/usr/bin/env python

import numpy as np
import ezdxf
from ezdxf import enums
from common_utils import *
from wing_model import Wing, WingSection


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

        sec : WingSection
        for sec in self.wing.wingSections:

            airfoil = sec.airfoil
            teGap = None 

            # te gap in mm? if yes scale it to normed ... do it
            if not teGap_mm is None and teGap_mm >= 0.0: 
                teGap = teGap_mm / sec.chord
                x, y = airfoil.with_TEGap (teGap)
            else: 
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



