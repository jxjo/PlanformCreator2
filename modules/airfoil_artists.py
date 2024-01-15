#!/usr/bin/env pythonupper
# -*- coding: utf-8 -*-

"""  

The "Artists" to plot a airfoil object on a matplotlib axes

"""
import numpy as np
import matplotlib.pyplot as plt

from artist             import *
from common_utils       import *

from airfoil            import Airfoil, Airfoil_Bezier
from airfoil            import NORMAL, SEED,SEED_DESIGN, REF1, REF2, DESIGN, FINAL
from airfoil_geometry   import Side_Airfoil, Side_Airfoil_Bezier, Side_Airfoil_HicksHenne
from airfoil_geometry   import Curvature_Abstract, UPPER, LOWER

from spline             import HicksHenne

cl_planform         = 'whitesmoke'
cl_editing          = 'deeppink'
cl_editing_lower    = 'orchid'
cl_helperLine       = 'orange'
ls_curvature        = '-'
ls_curvature_lower  = '--'
ls_difference       = '-.'
ls_camber           = '--'
ls_thickness        = ':'
ms_points           = dict(marker='o', fillstyle='none'    , markersize=4)   # marker style for points
ms_points_selected  = dict(marker='x', fillstyle='none'    , markersize=6)   # marker style for points
ms_le               = dict(marker='o'                      , markersize=7)   # marker style for leading edge
ms_warning          = dict(marker='o', color='orange'      , markersize=6)   # marker style for wrong points
ms_leReal           = dict(marker='o', color='limegreen'   , markersize=6)   # marker style for real leading edge
ms_point            = dict(marker='+'                      , markersize=8)   # marker style for just a point




def _color_airfoil_of (airfoil_type):
    """ returns the plot color for airfoil depending on its type """

    if airfoil_type == DESIGN:
        color = 'deeppink'
    elif airfoil_type == NORMAL:
        color = 'aquamarine'
    elif airfoil_type == FINAL:
        color = 'springgreen'
    elif airfoil_type == SEED:
        color = 'dodgerblue'
    elif airfoil_type == SEED_DESIGN:
        color = 'cornflowerblue'
    elif airfoil_type == REF1:
        color = 'mistyrose'
    elif airfoil_type == REF2:
        color = 'orange'
    else:
        color = None
    return color 



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

    label_with_airfoil_type = False             # include airfoil type in label 

    def __init__ (self, axes, modelFn, **kwargs):
        super().__init__ (axes, modelFn, **kwargs)

        self._points = False                    # show point marker 
        self._show_bezier = True                # draw Bezier control points and helper line
        self._show_hicksHenne = True            # draw hicks henne bumbs 
        self.set_showLegend('extended')         # show  legend with airfoil data 

 
    @property
    def points(self): return self._points
    def set_points (self, aBool): self._points = aBool 

    @property
    def show_bezier(self): return self._show_bezier
    def set_show_bezier (self, aBool): self._show_bezier = aBool 

    @property
    def show_hicksHenne(self): return self._show_hicksHenne
    def set_show_hicksHenne (self, aBool): self._show_hicksHenne = aBool 


    def set_current (self, aLineLabel, figureUpdate=False):
        """ tries to set a highlighted airfoil  to section with name ''aLineLabel' 
        """
        if (not aLineLabel is None and aLineLabel != self._curLineLabel):    # only when changed do something
            self._curLineLabel = aLineLabel
            if self.show:                       # view is switched on by user? 
                self.plot (figureUpdate=figureUpdate)

    
    def _plot (self): 
        """ do plot of airfoils in the prepared axes   
        """
        airfoils = self.model 
        airfoil: Airfoil

        if not len(airfoils) : return 

        # create cycled colors 
        self._set_colorcycle (10, colormap="Paired")          # no of cycle colors - extra color for each airfoil

        # now plot each single airfoil
        airfoil: Airfoil

        for iair, airfoil in enumerate (airfoils):
            if (airfoil.isLoaded):

                color = _color_airfoil_of (airfoil.usedAs)
                if self.label_with_airfoil_type:
                    label = f"{airfoil.usedAs}: {airfoil.name}"
                else: 
                    label = f"{airfoil.name}"

                if airfoil.usedAs == DESIGN:
                    linewidth = 1.0
                else: 
                    linewidth = 0.8

                # the marker style to show points
                if self._points:
                    linewidth=0.5
                    _marker_style = ms_points
                else:  
                    _marker_style = dict()

                p = self.ax.plot (airfoil.x, airfoil.y, '-', color = color, label=label, 
                                  linewidth= linewidth,  **_marker_style)
                self._add (p)

                if airfoil.isBezierBased and self.show_bezier: 
                    if not airfoil.usedAs == DESIGN:            # a seperate artist will show 
                        self.draw_controlPoints (airfoil, self._get_color(p))
                        self._plot_title ('Bezier based', va='top', ha='left', wspace=0.05, hspace=0.05)

                if airfoil.isHicksHenneBased and self.show_hicksHenne: 
                    self.draw_hicksHenne (airfoil, self._get_color(p))
                    self._plot_title ('Hicks Henne based', va='top', ha='left', wspace=0.05, hspace=0.05)

                self._cycle_color()                             # in colorycle are pairs  - move to next

                if self._pickActive: 
                    self._makeObjectPickable (p)

                # print a table for the max values 
                if self.showLegend == 'extended':
                    self._print_values (iair, airfoil, color)
                elif self.showLegend == 'normal':
                    self._print_name (iair, airfoil, color)

        # suppress autoscale because of printed legend 
        self.ax.autoscale(enable=False, axis='y')

        # activate event for clicking on line 
        if self._pickActive: self._connectPickEvent ()


    def draw_controlPoints(self, airfoil: Airfoil_Bezier, color):
        """ draw Bezier control Points of airfoil """

        markersize  = 6
        linewidth   = 0.7
        linestyle   = ':'

        for sideBezier in [airfoil.geo.upper, airfoil.geo.lower]:
            if sideBezier.name == UPPER:
                markerstyle = 6
            else: 
                markerstyle = 7
            x = sideBezier.bezier.points_x
            y = sideBezier.bezier.points_y
            p = self.ax.plot (x,y, linestyle, linewidth=linewidth, marker=markerstyle, markersize=markersize, color=color) 
            self._add(p)


    def draw_hicksHenne (self, airfoil: Airfoil_Bezier, color):
        """ draw Bezier control Points of airfoil """

        linewidth   = 0.7
        linestyle   = '-'

        side : Side_Airfoil_HicksHenne

        for side in [airfoil.geo.upper, airfoil.geo.lower]:
        # side = airfoil.geo.upper

            if side.name == UPPER:
                linestyle   = '--'
                delta_y =  0.1
            else:
                linestyle   = '--'
                delta_y = -0.1

            hh : HicksHenne
            for hh in side.hhs:
                x = side.x 
                y = hh.eval (x) * 10 + delta_y
                p = self.ax.plot (x,y, linestyle, linewidth=linewidth ) 
                self._add(p)


    def _print_name (self, iair, airfoil: Airfoil, color):
        # print airfoil name in upper left corner , position relative in pixel 

        xa = 0.96
        ya = 0.96 
        yoff = - iair * 12 - 12
        if self.label_with_airfoil_type:
            name = f"{airfoil.usedAs}: {airfoil.name}" if airfoil.usedAs else f"{airfoil.name}" 
        else:  
            name = f"{airfoil.name}"

        self._add (print_text   (self.ax, name, 'right', (xa,ya), (0, yoff), color, xycoords='axes fraction'))


    def _print_values (self, iair, airfoil: Airfoil, color):
         # print thickness, camber in a little table in upper left corner , position relative in pixel 
 
        xa = 0.98
        ya = 0.96 

        # header 
        if iair == 0: 
            self._add (print_text (self.ax, 'Thickness', 'right', (xa,ya), (-85, 0), cl_textHeader, xycoords='axes fraction'))
            self._add (print_text (self.ax, 'Camber'   , 'right', (xa,ya), (-25, 0), cl_textHeader, xycoords='axes fraction'))

        # airfoil data 
        if self.label_with_airfoil_type:  
            name = f"{airfoil.usedAs}: {airfoil.name}" if airfoil.usedAs else f"{airfoil.name}" 
        else:  
            name = f"{airfoil.name}"

        geo = airfoil.geo
        xt, t = geo.maxThickX, geo.maxThick 
        xc, c = geo.maxCambX,  geo.maxCamb

        yoff = - iair * 12 - 12
        self._add (print_text   (self.ax, name, 'right', (xa,ya), (-135, yoff), color, xycoords='axes fraction'))
        self._add (print_number (self.ax,  t, 2, (xa,ya), (-100, yoff), cl_text, asPercent=True))
        self._add (print_number (self.ax, xt, 1, (xa,ya), ( -70, yoff), cl_text, asPercent=True))
        self._add (print_number (self.ax,  c, 2, (xa,ya), ( -30, yoff), cl_text, asPercent=True))
        self._add (print_number (self.ax, xc, 1, (xa,ya), (   0, yoff), cl_text, asPercent=True))



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
    name = 'Curvature' 

    def __init__ (self, axes, modelFn, **kwargs):
        super().__init__ (axes, modelFn, **kwargs)

        self.set_showLegend('extended')         # show  legend with airfoil data 


    def _plot (self): 

        nairfoils = len(self.airfoils)
        if nairfoils == 0: return 
        
        airfoil: Airfoil

        for iair, airfoil in enumerate (self.airfoils):
            if (airfoil.isLoaded):

                curv : Curvature_Abstract = airfoil.geo.curvature
                color = _color_airfoil_of (airfoil.usedAs)
                linewidth = 0.8
                label = None
                sides = []
                if self.upper: sides.append (curv.upper)
                if self.lower: sides.append (curv.lower)

                side : Side_Airfoil
                for side in sides:
                    x = side.x
                    y = side.y      # if side.name == UPPER else -side.y
                    alpha = 0.9     # if side.name == UPPER else 0.6
                    ls = ls_curvature if side.name == UPPER else ls_curvature_lower
                    p = self.ax.plot (x, y, ls, color = color, alpha=alpha, label=label, 
                                      linewidth= linewidth, **self._marker_style)
                    self._add(p)
                    self._plot_reversals (side, color)

                    # print a table for the max values 
                    if self.showLegend == 'extended':
                        self._print_values (iair, nairfoils, airfoil.name, side, side.name==UPPER, color)

        self._plot_title (self.name, va='top', ha='center', wspace=0.1, hspace=0.05)



    def _plot_reversals (self, line : Side_Airfoil, color):
        # annotate reversals of curvature  - return number of reversals 

        reversals = line.reversals()
        if reversals:
            for i, point in enumerate(reversals): 
                text = "R"
                marker_x = point[0]
                if point[1] < 0.0:
                    marker_y = point[1] - 0.5
                    va = 'bottom'
                else: 
                    marker_y = point[1] + 0.5
                    va = 'top'

                p = self.ax.text (marker_x, marker_y, text, va=va, ha='center', color = color )
                self._add (p) 


    def _print_values (self, iair, nair, name, curvature: Side_Airfoil, upper: bool, color):
        # print curvature values 

        # print in upper left corner , position relative in pixel 
        xa = 0.87
        if upper: 
            ya = 0.96 
            ypos = 0
        else: 
            ya = 0.04 
            ypos = 12 * nair + 6

        # header 
        if iair == 0: 
            self._add (print_text (self.ax, 'LE'       , 'right', (xa,ya), (  2, ypos), cl_textHeader, xycoords='axes fraction'))
            self._add (print_text (self.ax, 'TE'       , 'right', (xa,ya), ( 38, ypos), cl_textHeader, xycoords='axes fraction'))
            self._add (print_text (self.ax, 'Reversals', 'right', (xa,ya), ( 90, ypos), cl_textHeader, xycoords='axes fraction'))

        # airfoil data + name 
        le_curv = curvature.y[0]
        te_curv = curvature.y[-1]
        nr     = len(curvature.reversals())
        yoff = ypos - iair * 12 - 12

        self._add (print_text   (self.ax, name, 'right', (xa,ya), (-35, yoff), color, alpha=0.8, xycoords='axes fraction'))
        self._add (print_number (self.ax, le_curv, 0, (xa,ya), (  5, yoff), cl_text))
        self._add (print_number (self.ax, te_curv, 1, (xa,ya), ( 40, yoff), cl_text))
        self._add (print_number (self.ax,      nr, 0, (xa,ya), ( 75, yoff), cl_text))



class Curvature_Smooth_Artist (Airfoil_Line_Artist):
    """Plot curvature (top or bottom) of an airfoil
       Select coordinate points to smooth
    """

    def _on_pick (self, event):
        # callback of matplt - having matplt 'event' as argument
        # overloaded to toggle marker 
        try: 
            artist = event.artist
            myMarker = artist.get_marker()
            side     = artist.get_label().split('_')[1]
        except: 
            myMarker = None
            print ("- Pick event couldn't be handled ", event.artist, "callback: ", self._pickCallback)

        if   myMarker == ms_points.get('marker'):
            artist.set (color=cl_userHint, **ms_points_selected)

        elif myMarker == ms_points_selected.get('marker'):
            if side == 'upper':
                color = cl_editing
            else:
                color = cl_editing_lower
            artist.set (color=color, **ms_points)

        super()._on_pick(event)
        self.ax.figure.canvas.draw_idle()

    
    def get_selected(self, side='upper'): 
        """ 
        returns list of selected points of upper or lower side
        Point index is from 0..n starting at LE 
        """

        selected = []
        iside = 0

        for i, artist in enumerate(self._myPlots):
            try:
                myMarker = artist.get_marker()
            except: 
                myMarker = 'None'
            if myMarker != 'None':
                mySide     = artist.get_label().split('_')[1]
                if mySide == side:
                    if myMarker == ms_points_selected.get('marker'):
                        selected.append(iside)
                    iside += 1
        return selected

    
    def _plot (self): 

        if not len(self.airfoils) : return 
        # create cycled colors 
        self._set_colorcycle (10 , colormap="Paired")         

        airfoilList = self.airfoils

        airfoil: Airfoil
        for airfoil in airfoilList:
            if (airfoil.isLoaded):


                linewidth=0.8

                if self.upper: 
                    line = airfoil.geo.curvature.upper
                    if airfoil.isEdited:
                        color = cl_editing
                    else:
                        color = None
                    p = self.ax.plot (line.x, line.y, ls_curvature, color = color, label=line.name, 
                                      linewidth= linewidth, **self._marker_style)
                    self._add(p)

                    if airfoil.isEdited:
                        self._plot_marker (line, self._get_color(p), 'upper')

                if self.lower: 
                    line = airfoil.geo.curvature.lower 
                    if airfoil.isEdited:
                        color = cl_editing_lower
                    else:
                        color = None
                    p = self.ax.plot (line.x, line.y, ls_curvature, color = color, label=line.name, 
                                      linewidth= linewidth, **self._marker_style)
                    self._add(p)

                    if airfoil.isEdited:
                        self._plot_marker (line, self._get_color(p), 'lower')

        # activate event for clicking on line 
        if self._pickActive: self._connectPickEvent ()
        self.show_mouseHelper()


    def show_mouseHelper (self):
        """ show info for section select"""
        p = self.ax.text (0.40, 0.05, 'click points to be removed', color=cl_userHint, fontsize = 'small',
                    transform=self.ax.transAxes, horizontalalignment='left', verticalalignment='bottom')
        self._add(p)


    def _plot_marker (self, line : Side_Airfoil, lineColor, side):
        # plot coordinate points to click   ... 

        for i in range (len(line.x)-1):                     # TE not clickable

            label = '_' + side + '_' + str(i)               # '_' do not appear in legend
            p = self.ax.plot (line.x[i], line.y[i], color = lineColor, label=label, 
                              **ms_points)
            self._add(p)
            self._makeObjectPickable (p)

                

class Difference_Artist (Airfoil_Line_Artist):
    """Plot the y-difference of two airfoils 

        2nd airfoil is Bezier based airfoil 
        1st is reference or original airfoil from where x-stations are taken  
    """

    @property
    def airfoil (self) -> Airfoil_Bezier: 
        return self.airfoils[1] 
    
    @property
    def ref_airfoil (self) -> Airfoil : 
        return self.airfoils[0] 
    

    def _get_difference (self, side_ref: Side_Airfoil, side_actual: Side_Airfoil_Bezier):
        # calculate difference at y-stations of reference airfoil 
        diff  = np.zeros (len(side_ref.x))
        for i, x in enumerate(side_ref.x):
            diff [i] = side_actual.bezier.eval_y_on_x (x, fast=True) - side_ref.y[i]
        return diff 


    def _plot (self): 

        if len(self.airfoils) != 2 : return 

        self.set_showLegend (False)                             # no legend 
        color = _color_airfoil_of (self.airfoil.usedAs)
        linewidth=0.8

        if self.upper:
            x = self.ref_airfoil.geo.upper.x
            y = 10 * self._get_difference (self.ref_airfoil.geo.upper, self.airfoil.geo.upper )
            p = self.ax.plot (x, y, ls_difference, color = color, 
                            linewidth= linewidth, **self._marker_style)
            self._add(p)

        if self.lower:
            x = self.ref_airfoil.geo.lower.x
            y = 10 * self._get_difference (self.ref_airfoil.geo.lower, self.airfoil.geo.lower ) 
            p = self.ax.plot (x, y, ls_difference, color = color, 
                            linewidth= linewidth, **self._marker_style)
            self._add(p)



class Le_Artist (Artist):
    """Plot the airfoils leading edge areacontour  """

    def __init__ (self, axes, modelFn, show=False, showMarker=True):
        super().__init__ (axes, modelFn, show=show, showMarker=showMarker)

        self._points = True                     # show point marker 
        self.set_showLegend (False)             # no legend 


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

                color = _color_airfoil_of (airfoil.usedAs)

                linewidth = 0.5
                
                self._plot_le_angle (airfoil)
                self._plot_le_coordinates (airfoil)

                p = self.ax.plot (airfoil.x, airfoil.y, '-', color = color, 
                                  linewidth= linewidth, **self._marker_style)
                self._add(p)

                self._plot_le (airfoil.geo.le, color)


    def _plot_le (self, le, color):

        # highlight leading edge based on coordinates
        if self.points:
            p = self.ax.plot (le[0], le[1], color=color, **ms_le)
            self._add(p)


    def _plot_le_angle (self, airfoil: Airfoil):

        yLim1, yLim2 = self.ax.get_ylim()

        xLe, yLe = airfoil.geo.le
        iLe = airfoil.geo.iLe
 
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
        text = "%.1f °" % (airfoil.geo.panelAngle_le)

        p = self.ax.annotate(text, (x[1], y[1]), fontsize = 'small',
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

        xLe, yLe = airfoil.geo.le
        if airfoil.isEdited:
            text = "New "
        else:
            text = ""

        text = text + "LE at %.7f, %.7f" % (xLe, yLe)
        p = self.ax.annotate(text, (xLe, yLe), fontsize = 'small',
                             xytext=(20, -4), textcoords='offset points', color = cl_helperLine)
        self._add (p)   



class Thickness_Artist (Airfoil_Line_Artist):
    """Plot thickness, camber line of an airfoil, print max values 
    """
    def __init__ (self, axes, modelFn, **kwargs):
        super().__init__ (axes, modelFn, **kwargs)

        self.set_showLegend(False)         # show  legend with airfoil data 

        
    def _plot (self): 

        airfoils = self.model 
        airfoil: Airfoil

        if not len(airfoils) : return 

        for iair, airfoil in enumerate (airfoils):
            if (airfoil.isLoaded ):

                # use fast airfoil geometry calc
                camber    = airfoil.camber
                thickness = airfoil.thickness
                    
                color = _color_airfoil_of (airfoil.usedAs)
                linewidth=0.8

                # plot camber line
                p = self.ax.plot (camber.x, camber.y, ls_camber, color = color, 
                                    linewidth= linewidth, **self._marker_style)
                self._add(p)

                # plot thickness distribution line
                p = self.ax.plot (thickness.x, thickness.y, ls_thickness, color = color, 
                                    linewidth= linewidth, **self._marker_style)
                self._add(p)

                # plot marker for the max values 
                self._plot_max_val (thickness, airfoil.usedAs, color)
                self._mark_max_val (thickness, color)
                self._plot_max_val (camber, airfoil.usedAs, color)
                self._mark_max_val (camber,    color)


    def _mark_max_val (self, airfoilLine: Side_Airfoil, color):
        # indicate max. value of camber or thickness line 
        p = self.ax.plot (*(airfoilLine.maximum), color=color, **ms_point)
        self._add(p)


    def _plot_max_val (self, airfoilLine: Side_Airfoil, airfoil_type, color):
        # indicate max. value of camber or thickness line 
        x, y = airfoilLine.maximum
        if airfoil_type == DESIGN:
            text = "New "
            color = cl_helperLine
        else:
            text = ""
            color = color
        p = self.ax.plot (x, y, color=color, **ms_point)
        self._add(p)

        if airfoil_type == DESIGN:
            p = self.ax.annotate(text + "%.2f%% at %.1f%%" % (y * 100, x *100), (x, y), fontsize='small',
                                xytext=(3, 3), textcoords='offset points', color = color)
            self._add (p)   



class Bezier_Edit_Artist (Artist):
    """Plot upper and lower Bezier curve - drag control points with mouse    """

    def __init__ (self, axes, modelFn, **kwargs):
        super().__init__ (axes, modelFn, **kwargs)

        self._points = False                       # show point marker 

        self.points_upper_artist   = []            # the artists of bezier control points
        self.points_lower_artist   = []      
        self.helper_upper_artist   = None          # artist to draw helper line 
        self.helper_lower_artist   = None      
        self.bezier_upper_artist   = None          # the artist to draw bezier curve
        self.bezier_lower_artist   = None

        self.thickness_artist      = None
        self.camber_artist         = None
        
        self.set_showLegend(False)  


    def set_points (self, aBool): self._points = aBool 


    def _deleteMyPlots(self):
        super()._deleteMyPlots()
        # clear up 
        self.ax.relim()                            # make sure all the data fits

        self.points_upper_artist   = []            # the artists of bezier control points
        self.points_lower_artist   = []   
        self.helper_upper_artist   = None          # artist to draw helper line 
        self.helper_lower_artist   = None      
        self.bezier_upper_artist   = None          # the artist to draw bezier curve
        self.bezier_lower_artist   = None
        self.thickness_artist      = None
        self.camber_artist         = None

    @property
    def airfoil (self) -> Airfoil_Bezier:
        return self.model


    def _plot (self): 
        """ do plot of bezier control points and bezier curve 
        """

        for sideBezier in [self.airfoil.geo.upper, self.airfoil.geo.lower]:

            # plot bezier control points  

            points_artist = []

            for ipoint, cpoint in enumerate (sideBezier.controlPoints):

                markersize = 6
                if ipoint == 0 or ipoint == (len(sideBezier.controlPoints)-1):
                    markerstyle = '.'
                    markersize = 3
                elif sideBezier.name == UPPER:
                    markerstyle = 6
                else: 
                    markerstyle = 7

                p = self.ax.plot (*cpoint, marker=markerstyle, markersize=markersize, 
                                  color=cl_userHint, animated=True) 
                points_artist.append (self._add(p))

            # plot bezier helper line between control points 
                
            x = sideBezier.bezier.points_x
            y = sideBezier.bezier.points_y
            p = self.ax.plot (x,y, ':', linewidth=0.7, color=cl_userHint, animated=True) 
            helper_artist = self._add(p)

            # plot  bezier curve points 

            if self._points:                         # the marker style to show points
                _marker_style = ms_points
                linewidth= 0.4
            else:  
                _marker_style = dict()
                linewidth= 0.8

            p = self.ax.plot (sideBezier.x, sideBezier.y, linestyle= 'None', linewidth=linewidth,   # 'None' not visible 
                              color=cl_editing, **_marker_style, animated=True ) 
            bezier_artist  = self._add(p) 

            # remind artist - activate dragManager per side 
            if sideBezier.name == UPPER:
                self.points_upper_artist = points_artist
                self.helper_upper_artist = helper_artist
                self.bezier_upper_artist = bezier_artist 
            else: 
                self.points_lower_artist = points_artist
                self.helper_lower_artist = helper_artist
                self.bezier_lower_artist = bezier_artist 
                
            self._dragManagers.append (DragManager (self.ax, points_artist,
                                typeTag = sideBezier.name, 
                                callback_draw_animated  = self.draw_animated,
                                callback_shiftCtrlClick = self.handle_shiftCtrlClick,
                                callback_on_moved       = self._moveCallback)) 

        # connect to draw event for initial plot of the animated artists all together
        self._connectDrawEvent()

        self.show_mouseHelper ()


    def show_mouseHelper (self):
        # show info for section select #
        text = 'ctrl+click to add, shift+click to remove point'
        p = self.ax.text (0.50, 0.05, text, color=cl_userHint, fontsize = 'small',
                    transform=self.ax.transAxes, horizontalalignment='center', verticalalignment='bottom')
        self._add(p)


    def draw_animated (self, artist_onMove=None, iArtist = None , typeTag=None): 
        """ call back when point is moving - draw and update Bezier """

        if artist_onMove is not None and iArtist is not None: 
            
            if typeTag == UPPER:
                side   = self.airfoil.geo.upper
            else: 
                side   = self.airfoil.geo.lower

            iPoint = iArtist 

            # get new coordinates (when dragged) and try to move control point 
            x_try, y_try = artist_onMove.get_xydata()[0]

            # draw all the dependand artists of this side 
            self.draw_animated_side (side, iPoint, x_try, y_try)

            # finally draw all animated artists
            self.draw_animated_artists ()


            
    
            

    def draw_animated_side (self, side : Side_Airfoil_Bezier, iPoint, x, y): 
        """ call back when point is moving - draw and update Bezier """

        if side.name == UPPER:
            artist_onMove = self.points_upper_artist[iPoint]
            bezier_artist = self.bezier_upper_artist
            helper_artist = self.helper_upper_artist
        else:  
            artist_onMove = self.points_lower_artist[iPoint]
            bezier_artist = self.bezier_lower_artist
            helper_artist = self.helper_lower_artist
        
        # set new bezier points  - will be checked for valid x,y 
        x, y = side.move_controlPoint_to(iPoint, x, y)

        # draw the cotrol point which is moved
        artist_onMove.set_xdata(x)
        artist_onMove.set_ydata(y)

        # draw helper line between control points 
        helper_artist.set_xdata(side.bezier.points_x)
        helper_artist.set_ydata(side.bezier.points_y)

        # draw animated Bezier 
        bezier_artist.set_xdata(side.x)
        bezier_artist.set_ydata(side.y)
        bezier_artist.set_linestyle('--')


    def handle_shiftCtrlClick (self, iArtist=None, typeTag=None, event=None):
        """handle shift or control click - if posssible insert new opPoint at eventxy
            - or remove if shift click  
        """
        if not event: return 

        updateBezier = False 
        if typeTag == UPPER:
            points_artist = self.points_upper_artist
            bezier_artist = self.bezier_upper_artist
            helper_artist = self.helper_upper_artist
            sideBezier    = self.airfoil.geo.upper
            markerstyle = 6
        elif typeTag == LOWER: 
            points_artist = self.points_lower_artist
            bezier_artist = self.bezier_lower_artist
            helper_artist = self.helper_lower_artist
            sideBezier   = self.airfoil.geo.lower
            markerstyle = 7
        else: 
            return

        if event.key == 'control' and iArtist is None:                     # no new opPoint on an exiysting point

            # is tnew point for upper points or lower points? 
            # simple check: +y --> upper, -y --> lower 
            if (event.ydata > 0.0 and typeTag == LOWER) or \
               (event.ydata < 0.0 and typeTag == UPPER): 
                return
            
            # new control point - insert according x-coordinate, x,y will be checked
            i_insert, x, y = sideBezier.insert_controlPoint_at (event.xdata, event.ydata)
            if not i_insert is None: 
                # create artist for the new point 
                p = self.ax.plot (x, y, marker=markerstyle, color=cl_userHint, 
                                  markersize=6,  animated=True) 
                self._add(p)
                points_artist.insert (i_insert, p[0])
                updateBezier = True 

        elif event.key == 'shift' and iArtist:

            # remove control Point on which shift-click was made
            i_delete = sideBezier.delete_controlPoint_at (index = iArtist)
            if i_delete is not None:
                artist = points_artist [iArtist]
                self._myPlots.remove(artist)                # remove from my lists 
                points_artist.remove(artist)
                artist.remove()                             # finally matplotlib remove 
                updateBezier = True 

        if updateBezier: 
            # update Bezier now, so redraw wil use the new curve 
            bezier_artist.set_xdata(sideBezier.x)
            bezier_artist.set_ydata(sideBezier.y)
            helper_artist.set_xdata(sideBezier.bezier.points_x)
            helper_artist.set_ydata(sideBezier.bezier.points_y)

            bezier_artist.set_linestyle('--')
            self.draw_animated_artists ()
            bezier_artist.set_linestyle('None')

