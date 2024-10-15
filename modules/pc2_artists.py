#!/usr/bin/env pythonupper
# -*- coding: utf-8 -*-

"""  

The "Artists" to plot a airfoil object on a pg.PlotItem 

"""

from base.math_util             import derivative1
from base.artist                import *
from base.common_utils          import *
from base.spline                import Bezier

from wing                       import Wing, Planform
from model.airfoil              import Airfoil, Airfoil_Bezier, usedAs, Geometry
from model.airfoil_geometry     import Line, Side_Airfoil_Bezier

from PyQt6.QtGui                import QColor, QBrush, QPen
from PyQt6.QtCore               import pyqtSignal, QObject

import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


# -------- helper functions ------------------------

def _color_airfoil_of (airfoil_type : usedAs) -> QColor:
    """ returns QColor for airfoil depending on its type """

    alpha = 1.0

    if airfoil_type == usedAs.DESIGN:
        color = 'deeppink'
    elif airfoil_type == usedAs.NORMAL:
        color = 'springgreen' # 'aquamarine'
        alpha = 0.9
    elif airfoil_type == usedAs.FINAL:
        color = 'springgreen'
    elif airfoil_type == usedAs.SEED:
        color = 'dodgerblue'
    elif airfoil_type == usedAs.SEED_DESIGN:
        color = 'cornflowerblue'
    elif airfoil_type == usedAs.REF1:                          # used also in 'blend' 
        color = 'lightskyblue'  
        alpha = 0.9
    elif airfoil_type == usedAs.REF2:
        color = 'orange'
        alpha = 0.9
    elif airfoil_type == usedAs.TARGET:
        color = 'cornflowerblue'
        alpha = 1.0
    else:
        color = 'gray'
    qcolor =  QColor (color) 

    if alpha != 1.0:
        qcolor.setAlphaF (alpha) 
    return qcolor


def _linestyle_of (aType : Line.Type) -> QColor:
    """ returns PenStyle for line depending on its type """

    if aType == Line.Type.CAMBER:
        style = style=Qt.PenStyle.DashDotLine
    elif aType == Line.Type.THICKNESS:
        style = style=Qt.PenStyle.DashLine
    elif aType == Line.Type.UPPER:
        style = style=Qt.PenStyle.DotLine
    elif aType == Line.Type.LOWER:
        style = style=Qt.PenStyle.DotLine
    else:
        style = style=Qt.PenStyle.SolidLine

    return style



# -------- concrete sub classes ------------------------


class Planform_Artist (Artist):
    """Plot the planform contour  """


    def __init__ (self, *args, **kwargs):

        self._welcome_text = None                       # a HTML welcome text 
        self._first_time  = True                        # to show welcome message 

        super().__init__ (*args, **kwargs)

 


    def set_current (self, aLineLabel):
        # tries to set a highlighted airfoil to section with name ''aLineLabel' 
        if (not aLineLabel is None and aLineLabel != self._curLineLabel):    # only when changed do something
            self._curLineLabel = aLineLabel
            if self.show:                       # view is switched on by user? 
                self.plot ()


    def set_welcome (self, aText):
        """ set a welcome HTML text, which is schon the first time"""
        if aText != self._welcome_text:
            self._welcome_text = aText 
            self.plot()


    @property
    def wing (self) -> Wing: return self.data_object

    @property
    def planform (self) -> Planform: return self.wing.planform


    def _plot (self): 
    
        color_palette = random_colors (5)

        if self._first_time and self._welcome_text is not None:
            self._plot_text (self._welcome_text, color=QColor(self.COLOR_LEGEND), # fontSize=self.SIZE_NORMAL, 
                                parentPos=(0.05,0.), itemPos=(0.0,0), offset=(30,10))
            self._first_time = False
        else: 
            self._plot_title (self.wing.name, subTitle="my little subtitle" )


        label = f"{self.wing.name}"

                # set color, width, ZValue, symbol style depending on airfoil usage and no of airfoils  

        color = None # _color_airfoil_of (airfoil.usedAs)
        if color is None:  
            color = color_palette [0]

        # default 
        width = 1
        antialias = False
        zValue = 1
        pen = pg.mkPen(color, width=width)

        # plot contour

        brush = pg.mkBrush (color.darker (600))

        y, x = self.planform.linesPolygon()
        self._plot_dataItem  (x, y, name=label, pen = pen, 
                                symbol=None,  
                                fillLevel=0.0, fillBrush=brush, antialias = antialias,
                                zValue=zValue)


