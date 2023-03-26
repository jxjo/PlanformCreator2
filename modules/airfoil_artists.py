#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""  

The "Artists" to plot a airfoil object on a matplotlib axes

"""
import numpy as np
from artist import * 

from common_utils import *
from airfoil import* 

cl_planform         = 'whitesmoke'
ls_curvature        = '-'
ls_deriv3           = '--'
ls_camber           = '--'
ms_points           = dict(marker='o', fillstyle='none', markersize=4)   # marker style for points



# -------- concrete sub classes ------------------------

class Grid_Artist (Artist):
    """shows the grid in the axis
    """
    # plot over loaded because different handling with grid
    def set_show (self, aBool):
        """ suser switch to diaable ploting the data
        """
        self._show = aBool
        self.plot()

    def plot(self):
        # todo improved grid depending on scale  
        self.ax.grid(self.show)
        self.ax.figure.canvas.draw_idle()



class Airfoil_Artist (Artist):
    """Plot the airfoils contour  """

    def __init__ (self, axes, modelFn, onPick=None, show=False, showMarker=True):
        super().__init__ (axes, modelFn, onPick=onPick, show=show, showMarker=showMarker)

        self._points = False                    # show point marker 

    @property
    def points(self): return self._points
    def set_points (self, aBool): self._points = aBool 

    @property
    def _marker_style (self):
        """ the marker style to show points"""
        if self._points: return ms_points
        else:            return dict()


    def _defaultUserTip (self):
        # overwritten in subclass"
        return None                         # 'Click airfoil to select'

    def set_current (self, aLineLabel, figureUpdate=False):
        """ tries to set a highlighted airfoil  to section with name ''aLineLabel' 
        """
        if (not aLineLabel is None and aLineLabel != self._curLineLabel):    # only when changed do something
            self._curLineLabel = aLineLabel
            if self.show:                       # view is switched on by user? 
                self.plot (figureUpdate=figureUpdate)

    @property
    def airfoils (self): 
        return self.model
    
    def _plot (self): 
        """ do plot of airfoils in the prepared axes   
        """

        if not len(self.airfoils) : return 
        # create cycled colors 
        self._set_colorcycle (10, colormap="Paired")          # no of cycle colors - extra color for each airfoil

        # now plot each single airfoil
        airfoil: Airfoil
        for airfoil in self.airfoils:
            if (airfoil.isLoaded):

                legend = ("%s" % (airfoil.name))  

                x = airfoil.x
                y = airfoil.y

                color = self._nextColor()
                if self._curLineLabel == legend:
                    p = self.ax.fill (x, y, facecolor=color, alpha=0.1)
                    self._add(p)
                    linewidth=1.6
                    self._plot_marker (x,y, color, airfoil)
                else:
                    linewidth=0.8

                p = self.ax.plot (x, y, '-', color = color, label=legend, linewidth= linewidth, **self._marker_style)
                self._add(p)

                self._nextColor()                       # in colorycle are pairs 

                if self._pickActive: 
                    self._makeLinePickable (p)

        # activate event for clicking on line 
        if self._pickActive:
            self.ax.figure.canvas.mpl_connect('pick_event', self._on_pick)     

        p = self.ax.legend(labelcolor=cl_labelGrid,)
        self._add(p)
        p.get_frame().set_linewidth(0.0)



    def _plot_marker (self, x,y, color, airfoil):
        # annotate airfoil with name etc. 

        text =  airfoil.name() +  \
               "\n" + \
               "\nThickness  %.2f%%  at  %.2f%%" % (airfoil.maxThickness, airfoil.maxThicknessX) +\
               "\nCamber     %.2f%%  at  %.2f%%" % (airfoil.maxCamber   , airfoil.maxCamberX   ) 

        y = np.amin(y) * 0.3
        x = np.amax(x) * 0.8
        p = self.ax.annotate(text, color=color, 
                xy=(x, y), xycoords='data', ha='left', va= 'top',
                xytext=(0, -50), textcoords='offset points')
        self._add(p)



class Airfoil_Line_Artist (Artist):
    """Superclass for plotting a line like curvature of upper and lower side of an airfoil
    """
    def __init__ (self, axes, modelFn, onPick=None, show=False, showMarker=True):
        super().__init__ (axes, modelFn, onPick=onPick, show=show, showMarker=showMarker)

        self._upper  = True                     # including upper and lower lines 
        self._lower  = True
        self._points = False                    # show point marker 

    def refresh(self, figureUpdate=False, upper=None, lower=None):
        """ overloaded to switch upper/lower on/off"""
        if upper is not None: self._upper = upper
        if lower is not None: self._lower = lower
        super().refresh (figureUpdate=figureUpdate)

    @property
    def upper(self): return self._upper
    def set_upper (self, aBool): self._upper = aBool 

    @property
    def lower(self): return self._lower
    def set_lower (self, aBool): self._lower = aBool 
    
    @property
    def points(self): return self._points
    def set_points (self, aBool): self._points = aBool 

    @property
    def _marker_style (self):
        """ the marker style to show points"""
        if self._points: return ms_points
        else:            return dict()

    
    @property
    def airfoils (self): 
        return self.model
    
    def _plot (self): 
        # to be overloaded
        pass



class Airfoil_Curv_Artist (Airfoil_Line_Artist):
    """Plot curvature (top or bottom) of an airfoil
    """
    def _plot (self): 

        if not len(self.airfoils) : return 
        # create cycled colors 
        self._set_colorcycle (10 , colormap="Paired")         

        airfoil: Airfoil
        for airfoil in self.airfoils:
            if (airfoil.isLoaded):
                color = self._nextColor()
                if self._upper: 
                    line = airfoil.upper.curvature 
                    legend = ("%s" % ("Upper " + line.name))  
                    linewidth=0.8

                    p = self.ax.plot (line.x, line.y, ls_curvature, color = color, label=legend, 
                                      linewidth= linewidth, **self._marker_style)
                    self._add(p)

                color = self._nextColor()
                if self._lower: 
                    line = airfoil.lower.curvature 
                    legend = ("%s" % ("Lower " + line.name))  
                    linewidth=0.8

                    p = self.ax.plot (line.x, line.y, ls_curvature, color = color, label=legend, 
                                      linewidth= linewidth, **self._marker_style)
                    self._add(p)

        if self._upper or self.lower:
            p = self.ax.legend(labelcolor=cl_labelGrid)
            p.get_frame().set_linewidth(0.0)
            self._add(p)


class Airfoil_Deriv3_Artist (Airfoil_Line_Artist):
    """Plot derivative 3 line (top or bottom) of an airfoil
    """  
    def _plot (self): 

        # create cycled colors 
        n = len(self.airfoils)                                      
        if not n: return 
        self._set_colorcycle (10 , colormap="Paired")         

        airfoil: Airfoil
        for airfoil in self.airfoils:
            if (airfoil.isLoaded):
                color = self._nextColor()
                if self._upper: 
                    line = airfoil.upper.deriv3 
                    legend = ("%s" % ("Upper " + line.name))  
                    linewidth=0.8

                    p = self.ax.plot (line.x, line.y, ls_deriv3, color = color, label=legend, 
                                      linewidth= linewidth, **self._marker_style)
                    self._add(p)

                color = self._nextColor()
                if self._lower: 
                    line = airfoil.lower.deriv3 
                    legend = ("%s" % ("Lower " + line.name))  
                    linewidth=0.8

                    p = self.ax.plot (line.x, line.y, ls_deriv3, color = color, label=legend, 
                                      linewidth= linewidth, **self._marker_style)
                    self._add(p)

        if self._upper or self.lower:
            p = self.ax.legend(labelcolor=cl_labelGrid, loc='lower right', bbox_to_anchor=(0.995,0.02))
            # p.get_frame().set_edgecolor(cl_labelGrid)
            p.get_frame().set_linewidth(0.0)
            self._add(p)


class Airfoil_Camber_Artist (Airfoil_Line_Artist):
    """Plot camber line of an airfoil
    """
    @property
    def airfoils (self): 
        return self.model
        
    def _plot (self): 

        # create cycled colors 
        n = len(self.airfoils)                                      
        if not n: return 
        self._set_colorcycle (n, colormap="Paired")          # no of cycle colors (each 2 for upper and lower)

        airfoil: Airfoil
        for airfoil in self.airfoils:
            if (airfoil.isLoaded):
                    line = airfoil.camber
                    legend = ("%s" % (line.name))  
                    color = self._nextColor()
                    linewidth=0.8

                    p = self.ax.plot (line.x, line.y, ls_camber, color = color, label=legend, linewidth= linewidth)
                    self._add(p)

                    self._nextColor()                       # in colorycle are pairs 

