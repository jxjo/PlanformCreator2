#!/usr/bin/env pythonupper
# -*- coding: utf-8 -*-

"""  

The "Artists" to plot a airfoil object on a matplotlib axes

"""
import numpy as np
from artist import Artist, cl_labelGrid

from common_utils import *
from airfoil import* 

cl_planform         = 'whitesmoke'
cl_editing          = 'deeppink'
cl_helperLine       = 'orange'
ls_curvature        = '-'
ls_camber           = '--'
ls_thickness        = ':'
ms_points           = dict(marker='o', fillstyle='none'    , markersize=4)   # marker style for points
ms_points_vline     = dict(marker='|', fillstyle='none'    , markersize=4)   # marker style for points
ms_le               = dict(marker='o'                      , markersize=7)   # marker style for leading edge
ms_warning          = dict(marker='o', color='orange'      , markersize=6)   # marker style for wrong points
ms_leReal           = dict(marker='o', color='limegreen'   , markersize=6)   # marker style for real leading edge
ms_point            = dict(marker='+'                      , markersize=8)   # marker style for just a point



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

                # line style 
                if airfoil.isEdited:
                    color = cl_editing
                else:
                    color = self._nextColor()

                # the marker style to show points
                if self._points and airfoil.isEdited:
                    _marker_style = ms_points_vline
                    linewidth=0.5
                elif self._points:
                    _marker_style = ms_points
                    linewidth=0.5
                    # p = self.ax.plot (airfoil.le, color= color, **ms_le)
                    # self._add(p)
                else:  
                    _marker_style = dict()
                    linewidth=1.0

                # plot airfoil 
                p = self.ax.plot (airfoil.x, airfoil.y, '-', color = color, label="%s" % (airfoil.name), 
                                  linewidth= linewidth, **_marker_style)
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



class Curvature_Artist (Airfoil_Line_Artist):
    """Plot curvature (top or bottom) of an airfoil
    """
    
    def _plot (self): 

        if not len(self.airfoils) : return 
        # create cycled colors 
        self._set_colorcycle (10 , colormap="Paired")         

        airfoilList = self.airfoils
        # airfoilList.append (self.airfoils[0].asNormalized)

        airfoil: Airfoil
        for airfoil in airfoilList:
            if (airfoil.isLoaded):
                color = self._nextColor()
                linewidth=0.8

                if self.upper: 
                    line = airfoil.spline.curv_upper 
                    p = self.ax.plot (line.x, -line.y, ls_curvature, color = color, label=line.name, 
                                      linewidth= linewidth, **self._marker_style)
                    self._add(p)
                    self._plot_marker (line, color, upper=True)

                color = self._nextColor()
                if self.lower: 
                    line = airfoil.spline.curv_lower 
                    p = self.ax.plot (line.x, line.y, ls_curvature, color = color, label=line.name, 
                                      linewidth= linewidth, **self._marker_style)
                    self._add(p)
                    self._plot_marker (line, color, upper=False)

        if self.upper or self.lower:
            p = self.ax.legend(labelcolor=cl_labelGrid)
            p.get_frame().set_linewidth(0.0)
            self._add(p)


    def _plot_marker (self, line : SideOfAirfoil, color, upper=True):
        # annotate reversals of curvature  ... 

        reversals = line.reversals()
        if reversals:
            for i, point in enumerate(reversals): 
                text = "R"
                marker_x = point[0]
                if upper:
                    marker_y = - point[1] - 0.3
                    va = 'bottom'
                else: 
                    marker_y = point[1] + 0.3
                    va = 'top'

                p = self.ax.text (marker_x, marker_y, text, va=va, ha='right', color = color )
                self._add (p) 
                


class Le_Artist (Artist):
    """Plot the airfoils leading edge areacontour  """

    def __init__ (self, axes, modelFn, show=False, showMarker=True):
        super().__init__ (axes, modelFn, show=show, showMarker=showMarker)

        self._points = True                    # show point marker 

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
        """ do plot of airfoils in the prepared axes   
        """

        # create cycled colors 
        self._set_colorcycle (10, colormap="Paired")          # no of cycle colors - extra color for each airfoil

        airfoil : Airfoil

        for airfoil in self.airfoils:
            if (airfoil.isLoaded):

                legend = ("%s" % (airfoil.name))  

                if airfoil.isEdited:
                    color = cl_editing
                else:
                    color = self._nextColor()

                linewidth=0.5
                self._plot_le (airfoil.le, color)
                self._plot_le_angle (airfoil)
                self._plot_le_coordinates (airfoil)

                p = self.ax.plot (airfoil.x, airfoil.y, '-', color = color, label=legend, 
                                  linewidth= linewidth, **self._marker_style)
                self._add(p)

                self._nextColor()                       # in colorycle are pairs 


    def _plot_le (self, le, color):

        # highlight leading edge based on coordinates
        if self.points:
            p = self.ax.plot (le[0], le[1], color=color, **ms_le)
            self._add(p)


    def _plot_le_angle (self, airfoil: Airfoil):

        yLim1, yLim2 = self.ax.get_ylim()

        xLe, yLe = airfoil.le
        iLe = np.nonzero(airfoil.x == xLe)[0][0]
 
        # plot two lines from LE to upper and lower neighbour points 
        xLe_before = airfoil.x [iLe-1]
        yLe_before = airfoil.y [iLe-1]

        # length of lines about 3/4 of axes height
        dy_line = (yLim2 - yLim1)/ 3 

        dx = xLe_before - xLe
        dy = yLe_before - yLe
        x = [xLe, xLe_before + dy_line * dx/dy]
        y = [yLe, yLe_before + dy_line]
        p = self.ax.plot (x,y, color = cl_helperLine, lw=0.7)
        self._add(p)

        # plot angle text 
        text = "%.1f °" % (airfoil.panelAngle_le)

        p = self.ax.annotate(text, (x[1], y[1]), 
                             xytext=(-15, 5), textcoords='offset points', color = cl_helperLine)
        self._add (p)   

        # lower line
        xLe_after = airfoil.x [iLe+1]
        yLe_after = airfoil.y [iLe+1]
        dx = xLe_after - xLe
        dy = yLe_after - yLe
        x = [xLe, xLe_after - dy_line * dx/dy]
        y = [yLe, yLe_after - dy_line]
        p = self.ax.plot (x,y, color = cl_helperLine, lw=0.7)
        self._add(p)



    def _plot_le_coordinates (self, airfoil: Airfoil):

        xLe, yLe = airfoil.le
        if airfoil.isEdited:
            text = "New "
        else:
            text = ""

        text = text + "LE at %.7f, %.7f" % (xLe, yLe)
        p = self.ax.annotate(text, (xLe, yLe), 
                             xytext=(20, -4), textcoords='offset points', color = cl_helperLine)
        self._add (p)   



class Thickness_Artist (Airfoil_Line_Artist):
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
            if (airfoil.isLoaded ):
                    
                    if airfoil.isEdited:
                        color = cl_editing
                    else:
                        color = self._nextColor()
                    linewidth=0.8

                    # plot camber line
                    p = self.ax.plot (airfoil.camber.x, airfoil.camber.y, ls_camber, color = color, 
                                      linewidth= linewidth, **self._marker_style, label="%s" % (airfoil.camber.name))
                    self._add(p)

                    # plot thickness distribution line
                    p = self.ax.plot (airfoil.thickness.x, airfoil.thickness.y, ls_thickness, color = color, 
                                      linewidth= linewidth, **self._marker_style, label="%s" % (airfoil.thickness.name))
                    self._add(p)
                    self._nextColor()                       # in colorycle are pairs 

                    self._plot_max_val(airfoil.maxThicknessX, airfoil.maxThickness, airfoil.isModified, color)
                    self._plot_max_val(airfoil.maxCamberX,    airfoil.maxCamber,    airfoil.isModified, color)

        # show legend 
        p = self.ax.legend(labelcolor=cl_labelGrid,)
        p.get_frame().set_linewidth(0.0)
        self._add(p)


    def _plot_max_val (self, x, y, isNew, color):

        x, y = x /100 , y / 100
        if isNew:
            text = "New "
            color = cl_helperLine
        else:
            text = ""
            color = color
        p = self.ax.plot (x, y, color=color, **ms_point)
        self._add(p)

        if isNew:
            p = self.ax.annotate(text + "%.2f%% at %.2f%%" % (y * 100, x *100), (x, y), 
                                xytext=(3, 3), textcoords='offset points', color = cl_helperLine)
            self._add (p)   


