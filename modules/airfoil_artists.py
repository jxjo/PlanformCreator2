#!/usr/bin/env pythonupper
# -*- coding: utf-8 -*-

"""  

The "Artists" to plot a airfoil object on a matplotlib axes

"""
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Circle

from artist             import *
from common_utils       import *

from airfoil            import Airfoil, Airfoil_Bezier
from airfoil            import NORMAL, SEED,SEED_DESIGN, REF1, REF2, DESIGN, FINAL
from airfoil_geometry   import Side_Airfoil, Side_Airfoil_Bezier, Side_Airfoil_HicksHenne
from airfoil_geometry   import Curvature_Abstract, UPPER, LOWER, THICKNESS, CAMBER

from spline             import HicksHenne

cl_fill             = 'whitesmoke'
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



# -------- helper functions ------------------------


def _color_airfoil_of (airfoil_type):
    """ returns the plot color for airfoil depending on its type """

    if airfoil_type == DESIGN:
        color = 'deeppink'
    elif airfoil_type == NORMAL:
        color = 'turquoise' # 'aquamarine'
    elif airfoil_type == FINAL:
        color = 'springgreen'
    elif airfoil_type == SEED:
        color = 'dodgerblue'
    elif airfoil_type == SEED_DESIGN:
        color = 'cornflowerblue'
    elif airfoil_type == REF1:                          # used also in 'blend' 
        color = 'lightskyblue'  
    elif airfoil_type == REF2:
        color = 'orange'
    else:
        color = None
    return color 



def _plot_bezier_point_marker (ax, side : Side_Airfoil_Bezier, ipoint, color, animated=False):
    """
    Plot a single marker for a bezier control point
    returns: plt marker artist  
    """

    markersize = 7
    if ipoint == 0 or ipoint == (len(side.controlPoints)-1):
        markerstyle = '.'
        markersize = 3
    elif side.name == UPPER:
        markerstyle = 6
    else: 
        markerstyle = 7

    x,y = side.controlPoints[ipoint]

    if animated: 
        alpha = 1
    else: 
        alpha = 0.5

    p =  ax.plot (x,y , marker=markerstyle, markersize=markersize, 
                 color=color, alpha=alpha, animated=animated) 
    return p



def _plot_bezier_point_number (ax, side : Side_Airfoil_Bezier, ipoint, color, animated=False):
    """
    Plot a single marker for a bezier control point
    returns: plt text artist  
    """

    if side.name == UPPER:
        va = 'bottom'
        yn = 8
    else:
        va = 'top'
        yn = -8

    x,y = side.controlPoints[ipoint]

    p = None 

    if ipoint == 0 :                            # point 0 draw to the left 
        p = ax.annotate(f'{ipoint+1}', (x,y) , va='center', ha='right', fontsize='small',
            xytext=(-10, 0), textcoords='offset points', 
            color = color, backgroundcolor= cl_background, animated=animated)
    elif ipoint > 0: 
        p = ax.annotate(f'{ipoint+1}', (x,y), va=va, ha='center', fontsize='small',
            xytext=(0, yn), textcoords='offset points', 
            color = color, backgroundcolor= cl_background, animated=animated)
    return p


def _plot_side_title (ax : plt.Axes, side : Side_Airfoil):
    """
    Plot info text about bezier curve of one side 
    returns: plt text artist  
    """

    if side.name == UPPER:
        y = 0.88
        va = 'top'
    else:
        y = 0.12
        va = 'bottom'
    x = 0.05 

    if side.isBezier:
        text = f'{side.nPoints} control points'
    elif side.isHicksHenne:
        text = f'{side.nhhs} functions'

    p = ax.text(x,y, text, va=va, ha='left',
                transform= ax.transAxes,  fontsize='small',
                color = cl_textHeader, alpha=1)
    return p 







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


    def __init__ (self, axes, modelFn, **kwargs):
        super().__init__ (axes, modelFn, **kwargs)

        self._show_title = False                        # show title like 'Bezier based' 
        self._points = False                            # show point marker 
        self._show_panels = False                       # show ony panels 
        self._label_with_airfoil_type = False           # include airfoil type in label 
        self._show_shape_function = True                # show Bezier or Hicks Henne shape functions
        self.set_showLegend('normal')                   # show legend with/without airfoil data 

 
    @property
    def points(self): return self._points
    def set_points (self, aBool): 
        self._points = aBool 
        if self._points: 
            self.set_show_panels (False)

    @property
    def label_with_airfoil_type(self): return self._label_with_airfoil_type
    def set_label_with_airfoil_type (self, aBool): self._label_with_airfoil_type = aBool 

    @property
    def show_title(self): return self._show_title
    def set_show_title (self, aBool): self._show_title = aBool 

    @property
    def show_panels(self): return self._show_panels
    def set_show_panels (self, aBool): 
        self._show_panels = aBool 
        if self._show_panels: 
            self.set_points (False)

    @property
    def show_shape_function(self): return self._show_shape_function
    def set_show_shape_function (self, aBool): self._show_shape_function = aBool 


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
        self._set_colorcycle (8, colormap="Set2")          # extra color for each airfoil

        # now plot each single airfoil
        airfoil: Airfoil

        # are there many airfoils - one of them is DESIGN? 
        airfoils_with_design = False 
        for airfoil in airfoils:
            if len(airfoils) > 1 and (airfoil.usedAs == DESIGN):
                airfoils_with_design = True 


        for iair, airfoil in enumerate (airfoils):
            if (airfoil.isLoaded):

                # if there are many airfoils, dimm airfoils which are not DESIGN 
                color = _color_airfoil_of (airfoil.usedAs)
                if color is not None: 
                    if airfoils_with_design and not (airfoil.usedAs == DESIGN):
                        color = adjust_lightness (color,0.7)
                else: 
                    color = self._cycle_color()

                if self.label_with_airfoil_type:
                    label = f"{airfoil.usedAs}: {airfoil.name}"
                else: 
                    label = f"{airfoil.name}"

                # the marker style 
                if self.points:
                    linewidth = 0.5
                    linestyle = 'None'
                    _marker_style = ms_points
                elif self.show_panels:                  # plot black marker to simulate panels        
                    linewidth = 1.5  
                    linestyle = '-'
                    _marker_style = dict(marker='o', fillstyle='full', markersize=4, 
                                         mfc=cl_background, mec=cl_background)  
                else:  
                    linewidth = 1.2  
                    linestyle = '-'
                    _marker_style = dict()  

                p = self.ax.plot (airfoil.x, airfoil.y, color=color, label=label, 
                                  linewidth=linewidth, linestyle=linestyle, **_marker_style)
                self._add (p)

                # plot real le - airfoil must be loaded as GEO_SPLINE!
                # p = self.ax.plot (airfoil.geo.le, linestyle='None', 
                #                   marker='o', fillstyle='full', markersize=6, 
                #                   mfc='red', mec='red')
                # self._add (p)

                # fill airfoil if it's only one 
                if len(airfoils) == 1 and not self.show_panels:                    
                    p = self.ax.fill (airfoil.x, airfoil.y, facecolor=color, alpha=0.1)
                    self._add(p)

                # show Bezier or Hicks Henne shape function
                if self.show_shape_function:
                    if airfoil.isBezierBased: 
                        self.draw_bezier (airfoil, color)
                        if self.show_title: 
                            self._plot_title ('Bezier based', va='top', ha='left', wspace=0.05, hspace=0.05)

                    if airfoil.isHicksHenneBased: 
                        self.draw_hicksHenne (airfoil)
                        if self.show_title: 
                            self._plot_title ('Hicks Henne based', va='top', ha='left', wspace=0.05, hspace=0.05)

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


    def draw_bezier(self, airfoil: Airfoil_Bezier, color):
        """ draw Bezier control Points of airfoil """

        linewidth   = 1
        linestyle   = ':'

        for side in [airfoil.geo.upper, airfoil.geo.lower]:

            # print info text about bezier curve 

            if self.show_title:
                p = _plot_side_title (self.ax, side)
                self._add(p)

            # plot bezier control points with connecting line 

            x = side.bezier.points_x
            y = side.bezier.points_y
            p = self.ax.plot (x,y, linestyle, linewidth=linewidth, color=color, alpha=0.7) 
            self._add(p)

            for ipoint in range (side.nPoints):

                # plot bezier control point marker 

                p = _plot_bezier_point_marker(self.ax, side, ipoint, color)
                self._add(p)

                # print point number  

                p = _plot_bezier_point_number (self.ax, side, ipoint, color)
                self._add(p)

            


    def draw_hicksHenne (self, airfoil: Airfoil_Bezier):
        """ draw hicks henne functions of airfoil """

        linewidth   = 1
        linestyle   = ':'

        side : Side_Airfoil_HicksHenne

        for side in [airfoil.geo.upper, airfoil.geo.lower]:
        # side = airfoil.geo.upper

            if side.name == UPPER:
                delta_y =  0.1
            else:
                delta_y = -0.1

            hh : HicksHenne
            for ih, hh in enumerate(side.hhs):

                # plot hh function 
                x = side.x 
                y = hh.eval (x) 
                p = self.ax.plot (x,y * 10 + delta_y, linestyle, linewidth=linewidth , alpha=1) 
                self._add(p)

                # plot maximum marker 
                x = hh.location
                y = hh.strength  * 10 + delta_y
                color =self._get_color (p) 
                p = self.ax.plot (x, y, color=color, **ms_point)
                self._add(p)

                p = self.ax.annotate(f'{ih+1}  w{hh.width:.2f}', (x, y), fontsize='small',
                    xytext=(3, 3), textcoords='offset points', color = color)
                self._add(p)

            # print info text 

            if self.show_title:    
                p = _plot_side_title (self.ax, side)
                self._add(p)



    def _print_name (self, iair, airfoil: Airfoil, color):
        # print airfoil name in upper left corner , position relative in pixel 

        xa = 0.96
        ya = 0.96 
        sc = get_font_size() / 10                    # scale pos depending on font size 

        yoff = - iair * (12*sc) - 12
        if self.label_with_airfoil_type:
            name = f"{airfoil.usedAs}: {airfoil.name}" if airfoil.usedAs else f"{airfoil.name}" 
        else:  
            name = f"{airfoil.name}"

        self._add (print_text   (self.ax, name, 'right', (xa,ya), (0, yoff), color, xycoords='axes fraction'))


    def _print_values (self, iair, airfoil: Airfoil, color):
         # print thickness, camber in a little table in upper left corner , position relative in pixel 
 
        xa = 0.98
        ya = 0.96 

        sc = get_font_size() / 10                    # scale pos depending on font size 

        # header 
        if iair == 0: 
            self._add (print_text (self.ax, 'Thickness', 'right', (xa,ya), (-85*sc, 0), cl_textHeader, xycoords='axes fraction'))
            self._add (print_text (self.ax, 'Camber'   , 'right', (xa,ya), (-25*sc, 0), cl_textHeader, xycoords='axes fraction'))

        # airfoil data 
        if self.label_with_airfoil_type:  
            name = f"{airfoil.usedAs}: {airfoil.name}" if airfoil.usedAs else f"{airfoil.name}" 
        else:  
            name = f"{airfoil.name}"

        geo = airfoil.geo
        xt, t = geo.maxThickX, geo.maxThick 
        xc, c = geo.maxCambX,  geo.maxCamb

        yoff = - iair * (12*sc) - (12*sc)
        self._add (print_text   (self.ax, name, 'right', (xa,ya), (-135*sc, yoff), color, xycoords='axes fraction'))
        self._add (print_number (self.ax,  t, 2, (xa,ya), (-100*sc, yoff), cl_text, asPercent=True))
        self._add (print_number (self.ax, xt, 1, (xa,ya), ( -70*sc, yoff), cl_text, asPercent=True))
        self._add (print_number (self.ax,  c, 2, (xa,ya), ( -30*sc, yoff), cl_text, asPercent=True))
        self._add (print_number (self.ax, xc, 1, (xa,ya), (   0*sc, yoff), cl_text, asPercent=True))



class Airfoil_Line_Artist (Artist):
    """Superclass for plotting a line like curvature of upper and lower side of an airfoil
    """
    def __init__ (self, axes, modelFn, **kwargs):
        super().__init__ (axes, modelFn, **kwargs)

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

        self._show_derivative = False           # show derivative of curvature 
        self.set_showLegend('extended')         # show legend with/without curvature data 

    @property
    def show_derivative(self): return self._show_derivative
    def set_show_derivative (self, aBool): self._show_derivative = aBool 


    def _plot (self): 

        from math_util  import derivative1

        nairfoils = len(self.airfoils)
        if nairfoils == 0: return 
        
        airfoil: Airfoil

        for iair, airfoil in enumerate (self.airfoils):
            if (airfoil.isLoaded):

                curv : Curvature_Abstract = airfoil.geo.curvature
                color = _color_airfoil_of (airfoil.usedAs)
                linewidth = 1.0 # 0.8
                label = None
                sides = []
                if self.upper: sides.append (curv.upper)
                if self.lower: sides.append (curv.lower)

                side : Side_Airfoil
                for side in sides:
                    x = side.x
                    y = side.y      
                    alpha = 0.7 # 0.9     
                    ls = ls_curvature if side.name == UPPER else ls_curvature_lower
                    p = self.ax.plot (x, y, ls, color = color, alpha=alpha, label=label, 
                                      linewidth= linewidth, **self._marker_style)
                    self._add(p)
                    self._plot_reversals (side, color)

                    # plot derivative1 of curvature ('spikes') only if just one airfoil or DESIGN
                    if self.show_derivative:
                        if nairfoils == 1 or (nairfoils > 1 and airfoil.usedAs == DESIGN):
                            p = self.ax.plot (x, -derivative1(x,y), ls, color = 'red', alpha=alpha, label=label, 
                                            linewidth= 0.8, **self._marker_style)
                            self._add(p)

                    # print a table for the max values 
                    if self.showLegend == 'extended':
                        self._print_values (iair, nairfoils, airfoil.name, side, side.name==UPPER, color)


        self._plot_title (self.name, va='top', ha='center', wspace=0.1, hspace=0.05)

        # print derivative legend 
        if self.show_derivative:
            p = self.ax.text (0.05, 0.05, "Derivative of curvature", color='firebrick' , 
                            fontsize = 'small', transform=self.ax.transAxes, 
                            horizontalalignment='left', verticalalignment='bottom')
            self._add (p)   


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

        sc = get_font_size() / 10                    # scale pos depending on font size 

        # header 
        if iair == 0: 
            self._add (print_text (self.ax, 'LE'    , 'right', (xa,ya), (  2*sc, ypos), cl_textHeader, xycoords='axes fraction'))
            self._add (print_text (self.ax, 'TE'    , 'right', (xa,ya), ( 38*sc, ypos), cl_textHeader, xycoords='axes fraction'))
            self._add (print_text (self.ax, 'Revers', 'right', (xa,ya), ( 80*sc, ypos), cl_textHeader, xycoords='axes fraction'))

        # airfoil data + name 
        le_curv = curvature.y[0]
        te_curv = curvature.y[-1]
        nr     = len(curvature.reversals())
        yoff = ypos - iair * 12 - 12

        if nair > 1:                                # airfoil name only if there are several
            self._add (print_text   (self.ax, name, 'right', (xa,ya), (-35*sc, yoff), color, alpha=0.8, xycoords='axes fraction'))
        self._add (print_number (self.ax, le_curv, 0, (xa,ya), (  5*sc, yoff), cl_text))
        self._add (print_number (self.ax, te_curv, 1, (xa,ya), ( 40*sc, yoff), cl_text))
        self._add (print_number (self.ax,      nr, 0, (xa,ya), ( 68*sc, yoff), cl_text))


                

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

        for airfoil in airfoils:
            if (airfoil.isLoaded ):

                # use fast airfoil geometry calc
                camber    = airfoil.camber
                thickness = airfoil.thickness
                    
                color = _color_airfoil_of (airfoil.usedAs)

                # dimm airfoils which are not DESIGN 
                if  not (airfoil.usedAs == DESIGN):
                    color = adjust_lightness (color,0.6)

                # plot camber line
                p = self.ax.plot (camber.x, camber.y, ls_camber, color = color, 
                                    linewidth= 0.8, **self._marker_style)
                self._add(p)

                # plot thickness distribution line
                p = self.ax.plot (thickness.x, thickness.y, ls_thickness, color = color, 
                                    linewidth= 1.0, **self._marker_style)
                self._add(p)

                # plot marker for the max values 
                self._plot_max_val (thickness, airfoil.usedAs, color)
                self._plot_max_val (camber,    airfoil.usedAs, color)

                # plot le circle 

                le_radius = airfoil.leRadius_perc / 100
                circle = Circle ((le_radius,0), le_radius, linewidth=1, fill=False, 
                                     ec=adjust_lightness(color,1.2))
                p = self.ax.add_artist (circle) 
                self._add(p)



    def _plot_max_val (self, airfoilLine: Side_Airfoil, airfoil_type, color):
        """ indicate max. value of camber or thickness line """

        x, y = airfoilLine.maximum
        if airfoil_type == DESIGN:
            text = ""
            color = cl_helperLine
        else:
            text = ""
            color = color

        # symmetrical and camber? 
        if airfoilLine.name == CAMBER and y == 0.0: 
            p = self.ax.annotate("No camber - symmetrical", (0.3, 0), fontsize='small',
                                xytext=(4, 4), textcoords='offset points', color = color)
            self._add (p) 
        # normal 
        else:  
            # marker 
            p = self.ax.plot (x, y, color=color, **ms_point)        
            self._add(p)
            # values 
            p = self.ax.annotate(text + "%.2f%% at %.1f%%" % (y * 100, x *100), (x, y), fontsize='small',
                                xytext=(4, 4), textcoords='offset points', color = color)
            self._add (p)   



class Thickness_Edit_Artist (Airfoil_Line_Artist):
    """
    Plot thickness, camber line of an airfoil, print max values
    Move highpoints of thickness and camber by mouse  
    """

    def __init__ (self, axes, modelFn, **kwargs):
        super().__init__ (axes, modelFn, **kwargs)


        self.thickness_artist        = None          # artist to thickness line 
        self.thickness_max_artist    = None          # artist to move thickness highpoint 
        self.thickness_marker_artist = None          # artist to move thickness highpoint 
        self.camber_artist           = None          # artist to camber line 
        self.camber_max_artist       = None          # artist to move camber highpoint 
        self.camber_marker_artist    = None          # artist to move camber highpoint 

        self._mouseInfo_wasShown     = False         # user info shown? 

        self.set_showLegend(False)                   # show legend with airfoil data 


    def _deleteMyPlots(self):
        super()._deleteMyPlots()
        
        self.ax.relim()                              # make sure all the data fits

        self.thickness_artist        = None          # artist to thickness line 
        self.thickness_max_artist    = None          # artist to move thickness highpoint 
        self.thickness_marker_artist = None          # artist to move thickness highpoint 
        self.camber_artist           = None          # artist to camber line 
        self.camber_max_artist       = None          # artist to move camber highpoint 
        self.camber_marker_artist    = None          # artist to move camber highpoint 

    @property
    def airfoil (self) -> Airfoil:
        return self.model [0]


        
    def _plot (self): 

        if not self.airfoil.isLoaded: return 

        color = _color_airfoil_of (self.airfoil.usedAs)
        linewidth=1.2

        # thickness line and max point for mouse 

        thickness = self.airfoil.thickness                   # use fast airfoil geometry calc
        p = self.ax.plot (thickness.x, thickness.y, ls_thickness, color = color, 
                            linewidth= linewidth, **self._marker_style, animated=True)
        self.thickness_artist = self._add(p)

        x, y = thickness.maximum
        p = self.ax.plot (x, y, color=cl_userHint, **ms_point, animated=True)        
        self.thickness_max_artist = self._add(p)

        p = self.ax.annotate("%.2f%% at %.1f%%" % (y * 100, x *100), (x, y), fontsize='small',
                               xytext=(4, 4), textcoords='offset points', color = cl_userHint, animated = True)
        self.thickness_marker_artist = self._add (p)   

        self._dragManagers.append (DragManager (self.ax, self.thickness_max_artist,
                    typeTag = thickness.name, 
                    callback_draw_animated  = self.draw_animated,
                    callback_on_moved       = self._moveCallback)) 


        # camber line and max point for mouse 

        camber    = self.airfoil.camber                      # use fast airfoil geometry calc
        p = self.ax.plot (camber.x, camber.y, ls_camber, color = color, 
                            linewidth= linewidth, **self._marker_style, animated=True)
        self.camber_artist = self._add(p)

        x, y = camber.maximum
        p = self.ax.plot (x, y, color=cl_userHint, **ms_point, animated=True)        
        self.camber_max_artist = self._add(p)

        p = self.ax.annotate("%.2f%% at %.1f%%" % (y * 100, x *100), (x, y), fontsize='small',
                               xytext=(4, 4), textcoords='offset points', color = cl_userHint, animated = True)
        self.camber_marker_artist = self._add (p)   

        self._dragManagers.append (DragManager (self.ax, self.camber_max_artist,
                    typeTag = camber.name, 
                    callback_draw_animated  = self.draw_animated,
                    callback_on_moved       = self._moveCallback)) 
        
        # plot le circle 

        le_radius = self.airfoil.leRadius_perc / 100
        circle = Circle ((le_radius,0), le_radius, fill=False, ec=color, alpha=0.8, animated=True)
        p = self.ax.add_artist (circle) 
        self._add(p)


        # connect to draw event for initial plot of the animated artists all together
        self._connectDrawEvent()

        self.show_mouseInfo ()

        # suppress autoscale 
        # self.ax.set_ylim(bottom=0.0)
        self.ax.autoscale(enable=False, axis='y')



    def show_mouseInfo (self):
        """ show user info for mouse helper """

        # show only once 
        if not self._mouseInfo_wasShown:
            text = 'move highpoints with mouse'
            p = self.ax.text (0.50, 0.05, text, color=cl_userHint, fontsize = 'small',
                        transform=self.ax.transAxes, horizontalalignment='center', verticalalignment='bottom')
            self._add(p)
            self._mouseInfo_wasShown = True


    def draw_animated (self, artist_onMove=None, iArtist = None , typeTag=None): 
        """ call back when point is moving - draw and update lines """

        if artist_onMove is not None and iArtist is not None: 
            
            if typeTag == THICKNESS:
                line          = self.airfoil.thickness
                line_artist   = self.thickness_artist
                marker_artist = self.thickness_marker_artist 

            else: 
                line          = self.airfoil.camber
                line_artist   = self.camber_artist
                marker_artist = self.camber_marker_artist 

            # get new coordinates (when dragged) and try to move control point 
            x_try, y_try = artist_onMove.get_xydata()[0]

            x = min (0.8, max (0.1, x_try))

            if typeTag == THICKNESS:
                y = min (0.5, max (0.01, y_try))            # if 0.0 line couldn't be restored
            else:
                y = min (0.5, max (0.001, y_try))           # if 0.0 line couldn't be restored

            # set new max points  - will be checked for valid x,y 
            line.set_maximum (x, y)

            # draw the point which is moved
            artist_onMove.set_xdata(x)
            artist_onMove.set_ydata(y)

            # draw the line 
            line_artist.set_xdata(line.x)
            line_artist.set_ydata(line.y)

            # marker pos update 
            marker_artist.xy = (x,y)
            marker_artist.set_text ("%.2f%% at %.1f%%" % (y * 100, x *100))

            # finally draw all animated artists
            self.draw_animated_artists ()




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

        self._mouseInfo_artist     = None         # user info artist

        self.set_showLegend(False)  


    def set_points (self, aBool): self._points = aBool 


    def _deleteMyPlots(self):
        super()._deleteMyPlots()
        
        self.ax.relim()                            # make sure all the data fits

        self.marker_upper_artist   = []            # the artists of bezier control points
        self.marker_lower_artist   = []   
        self.number_upper_artist   = []            # the artists of bezier control points number
        self.number_lower_artist   = []   
        self.helper_upper_artist   = None          # artist to draw helper line 
        self.helper_lower_artist   = None      
        self.bezier_upper_artist   = None          # the artist to draw bezier curve
        self.bezier_lower_artist   = None

    @property
    def airfoil (self) -> Airfoil_Bezier:
        return self.model


    def _plot (self): 
        """ do plot of bezier control points and bezier curve 
        """

        for side in [self.airfoil.geo.upper, self.airfoil.geo.lower]:

            # plot bezier control points marker and their number  

            marker_artist = []
            number_artist = []
            for ipoint in range (side.nPoints):

                p = _plot_bezier_point_marker (self.ax, side, ipoint, cl_userHint, animated=True)
                marker_artist.append (self._add(p))

                p = _plot_bezier_point_number (self.ax, side, ipoint, cl_userHint, animated=True)
                number_artist.append (self._add(p))

            # plot bezier helper line between control points 
                
            x = side.bezier.points_x
            y = side.bezier.points_y
            p = self.ax.plot (x,y, ':', linewidth=1, color=cl_userHint, alpha=0.7, animated=True) 
            helper_artist = self._add(p)

            # plot  bezier curve points 

            if self._points:                         # the marker style to show points
                _marker_style = ms_points
                linewidth= 0.4
            else:  
                _marker_style = dict()
                linewidth= 0.8

            # prepare plot bezier for animated drawing (dashed line) 
            p = self.ax.plot (side.x, side.y, linestyle= 'None', linewidth=linewidth,   # 'None' not visible 
                              color=cl_editing, **_marker_style, animated=True ) 
            bezier_artist  = self._add(p) 

            # remind artist - activate dragManager per side 
            if side.name == UPPER:
                self.marker_upper_artist = marker_artist
                self.number_upper_artist = number_artist
                self.helper_upper_artist = helper_artist
                self.bezier_upper_artist = bezier_artist 
            else: 
                self.marker_lower_artist = marker_artist
                self.number_lower_artist = number_artist
                self.helper_lower_artist = helper_artist
                self.bezier_lower_artist = bezier_artist 
                
            self._dragManagers.append (DragManager (self.ax, marker_artist,
                                typeTag = side.name, 
                                callback_draw_animated  = self.draw_animated,
                                callback_shiftCtrlClick = self.handle_shiftCtrlClick,
                                callback_on_moved       = self._moveCallback)) 

        # connect to draw event for initial plot of the animated artists all together
        self._connectDrawEvent()

        self.show_mouseInfo ()

        return 



    def show_mouseInfo (self):
        """ show user info for mouse helper """

        # will be removed after first mouse move 
        if self._mouseInfo_artist is None: 
            text = 'move points with mouse, ctrl+click to add, shift+click to remove'
            p = self.ax.text (0.50, 0.05, text, color=cl_userHint, fontsize = 'small',
                        transform=self.ax.transAxes, horizontalalignment='center', verticalalignment='bottom')
            self._mouseInfo_artist = p


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
            self.update_animated_side (side, iPoint, x_try, y_try)

            # finally draw all animated artists
            self.draw_animated_artists ()

            # remove mouse info after first move 
            if self._mouseInfo_artist: 
                self._mouseInfo_artist.remove()  
                self._mouseInfo_artist = False
                      
     

    def update_animated_side (self, side : Side_Airfoil_Bezier, iPoint, x, y): 
        """ update Bezier and plot artists of side """

        if side.name == UPPER:
            artist_onMove = self.marker_upper_artist[iPoint]
            number_onMove = self.number_upper_artist[iPoint]
            bezier_artist = self.bezier_upper_artist
            helper_artist = self.helper_upper_artist
        else:  
            artist_onMove = self.marker_lower_artist[iPoint]
            number_onMove = self.number_lower_artist[iPoint]
            bezier_artist = self.bezier_lower_artist
            helper_artist = self.helper_lower_artist
        
        # set new bezier points  - will be checked for valid x,y 
        x, y = side.move_controlPoint_to(iPoint, x, y)

        # draw the control point which is moved
        number_onMove.xy = (x,y)
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
            marker_artist = self.marker_upper_artist
            number_artist = self.number_upper_artist
            bezier_artist = self.bezier_upper_artist
            helper_artist = self.helper_upper_artist
            side          = self.airfoil.geo.upper
        elif typeTag == LOWER: 
            marker_artist = self.marker_lower_artist
            number_artist = self.number_lower_artist
            bezier_artist = self.bezier_lower_artist
            helper_artist = self.helper_lower_artist
            side          = self.airfoil.geo.lower
        else: 
            return

        if event.key == 'control' and iArtist is None:                     # no new opPoint on an exiysting point

            # is tnew point for upper points or lower points? 
            # simple check: +y --> upper, -y --> lower 
            if (event.ydata > 0.0 and typeTag == LOWER) or \
               (event.ydata < 0.0 and typeTag == UPPER): 
                return
            
            # new control point - insert according x-coordinate, x,y will be checked
            i_insert, x, y = side.insert_controlPoint_at (event.xdata, event.ydata)

            updateBezier = not i_insert is None 

        elif event.key == 'shift' and iArtist:

            # remove control Point on which shift-click was made
            i_delete = side.delete_controlPoint_at (index = iArtist)

            updateBezier = not i_delete is None 

        if updateBezier: 
            # update Bezier now, so redraw wil use the new curve 
            bezier_artist.set_xdata(side.x)
            bezier_artist.set_ydata(side.y)
            helper_artist.set_xdata(side.bezier.points_x)
            helper_artist.set_ydata(side.bezier.points_y)

            # delete marker and their number - and rebuild  

            for artist in marker_artist:
                self._myPlots.remove(artist)                # remove from my lists 
                artist.remove()                             # finally matplotlib remove 
            for artist in number_artist:
                self._myPlots.remove(artist)                # remove from my lists 
                artist.remove()                             # finally matplotlib remove 

            marker_artist.clear()                           # keep list alive 
            number_artist.clear()
            for ipoint in range (side.nPoints):

                p = _plot_bezier_point_marker (self.ax, side, ipoint, cl_userHint, animated=True)
                marker_artist.append (self._add(p))

                p = _plot_bezier_point_number (self.ax, side, ipoint, cl_userHint, animated=True)
                number_artist.append (self._add(p))

            # finally re-draw all 
                
            bezier_artist.set_linestyle('--')
            self.draw_animated_artists ()
            bezier_artist.set_linestyle('None')

