#!/usr/bin/env pythonupper
# -*- coding: utf-8 -*-

"""  

The "Artists" to plot a airfoil object on a matplotlib axes

"""
import numpy as np
from artist import Artist, cl_userHint, cl_labelGrid, DragManager

from common_utils import *
from airfoil2_geometry import SideOfAirfoil_Bezier
from airfoil2          import* 

cl_planform         = 'whitesmoke'
cl_editing          = 'deeppink'
cl_editing_lower    = 'orchid'
cl_helperLine       = 'orange'
ls_curvature        = '-'
ls_difference       = '-.'
ls_camber           = '--'
ls_thickness        = ':'
ms_points           = dict(marker='o', fillstyle='none'    , markersize=4)   # marker style for points
ms_points_selected  = dict(marker='x', fillstyle='none'    , markersize=6)   # marker style for points
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

    def __init__ (self, axes, modelFn, **kwargs):
        super().__init__ (axes, modelFn, **kwargs)

        self._points = False                    # show point marker 

    @property
    def points(self): return self._points
    def set_points (self, aBool): self._points = aBool 


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
        airfoil: Airfoil2

        for airfoil in self.airfoils:
            if (airfoil.isLoaded):

                # line style 
                if airfoil.isEdited:
                    color = cl_editing
                    linewidth = 1.0
                else:
                    color=None
                    linewidth = 0.8

                # the marker style to show points
                if self._points and airfoil.isEdited:
                    _marker_style = ms_points_vline
                    linewidth=0.5
                elif self._points:
                    _marker_style = ms_points
                    linewidth=0.5
                else:  
                    _marker_style = dict()

                # plot airfoil 
                p = self.ax.plot (airfoil.x, airfoil.y, '-', color = color, label="%s" % (airfoil.name), 
                                  linewidth= linewidth, **_marker_style)
                self._add(p)
                if airfoil.isBezierBased: 
                    self.draw_controlPoints (airfoil, self._get_color(p))

                self._cycle_color()                             # in colorycle are pairs  - move to next

                if self._pickActive: 
                    self._makeObjectPickable (p)

        # activate event for clicking on line 
        if self._pickActive: self._connectPickEvent ()


    def draw_controlPoints(self, airfoil: Airfoil2_Bezier, color):
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

        airfoil: Airfoil2
        for airfoil in airfoilList:
            if (airfoil.isLoaded):
                if airfoil.isEdited:
                    color = cl_editing
                else: 
                    color = None
                linewidth=0.8

                if self.upper: 
                    line = airfoil.geo.curvature.upper
                    label = airfoil.name + " - upper"
                    p = self.ax.plot (line.x, line.y, ls_curvature, color = color, label=label, 
                                      linewidth= linewidth, **self._marker_style)
                    self._add(p)
                    self._plot_marker (line, self._get_color(p), upper=True)

                if airfoil.isEdited:
                    color = cl_editing_lower
                else: 
                    color = None
                if self.lower: 
                    line = airfoil.geo.curvature.lower 
                    label = airfoil.name + " - lower"
                    p = self.ax.plot (line.x, line.y, ls_curvature, color = color, label=label, 
                                      linewidth= linewidth, **self._marker_style)
                    self._add(p)
                    self._plot_marker (line, self._get_color(p), upper=False)

        self._plot_title ('Curvature', va='bottom', ha='left')

        if self._myPlots:                     # something plotted? 
            p = self.ax.plot ([], [], ' ', label="R: reversals")
            self._add(p)


    def _plot_marker (self, line : SideOfAirfoil, color, upper=True):
        # annotate reversals of curvature  ... 

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

        airfoil: Airfoil2
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


    def _plot_marker (self, line : SideOfAirfoil, lineColor, side):
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
    def airfoil (self) -> Airfoil2_Bezier: 
        return self.airfoils[1] 
    
    @property
    def ref_airfoil (self) -> Airfoil2 : 
        return self.airfoils[0] 
    

    def _get_difference (self, side_ref: SideOfAirfoil, side_actual: SideOfAirfoil_Bezier):
        # calculate difference at y-stations of reference airfoil 
        diff  = np.zeros (len(side_ref.x))
        for i, x in enumerate(side_ref.x):
            diff [i] = side_actual.bezier.eval_y_on_x (x, fast=True) - side_ref.y[i]
        return diff 

    def _plot (self): 

        if len(self.airfoils) != 2 : return 

        # create cycled colors 
        self._set_colorcycle (10 , colormap="Paired")         

        linewidth=0.8

        if self.upper:
            if self.airfoil.isEdited:
                color = cl_editing
            else: 
                color = None
            x = self.ref_airfoil.geo.upper.x
            y = 10 * self._get_difference (self.ref_airfoil.geo.upper, self.airfoil.geo.upper )
            p = self.ax.plot (x, y, ls_difference, color = color, label="diff upper * 10", 
                            linewidth= linewidth, **self._marker_style)
            self._add(p)

        if self.lower:
            if self.airfoil.isEdited:
                color = cl_editing_lower
            else: 
                color = None
            x = self.ref_airfoil.geo.lower.x
            y = 10 * self._get_difference (self.ref_airfoil.geo.lower, self.airfoil.geo.lower ) 
            p = self.ax.plot (x, y, ls_difference, color = color, label="diff lower * 10", 
                            linewidth= linewidth, **self._marker_style)
            self._add(p)



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

        airfoil : Airfoil2

        for airfoil in self.airfoils:
            if (airfoil.isLoaded):

                label = ("%s" % (airfoil.name))  

                if airfoil.isEdited:
                    color = cl_editing
                else:
                    color = None

                linewidth=0.5
                
                self._plot_le_angle (airfoil)
                self._plot_le_coordinates (airfoil)

                p = self.ax.plot (airfoil.x, airfoil.y, '-', color = color, label=label, 
                                  linewidth= linewidth, **self._marker_style)
                self._add(p)

                self._plot_le (airfoil.geo.le, self._get_color(p))

                self._cycle_color()                       # in colorycle are pairs - move next


    def _plot_le (self, le, color):

        # highlight leading edge based on coordinates
        if self.points:
            p = self.ax.plot (le[0], le[1], color=color, **ms_le)
            self._add(p)


    def _plot_le_angle (self, airfoil: Airfoil2):

        yLim1, yLim2 = self.ax.get_ylim()

        xLe, yLe = airfoil.geo.le
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
        text = "%.1f °" % (airfoil.geo.panelAngle_le)

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



    def _plot_le_coordinates (self, airfoil: Airfoil2):

        xLe, yLe = airfoil.geo.le
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

        airfoil: Airfoil2
        for airfoil in self.airfoils:
            if (airfoil.isLoaded ):
                    
                    if airfoil.isEdited:
                        color = cl_editing
                    else:
                        color = None
                    linewidth=0.8

                    # plot camber line
                    p = self.ax.plot (airfoil.camber.x, airfoil.camber.y, ls_camber, color = color, 
                                      linewidth= linewidth, **self._marker_style, label="%s" % (airfoil.camber.name))
                    self._add(p)

                    color = self._get_color(p) 

                    # plot thickness distribution line
                    p = self.ax.plot (airfoil.thickness.x, airfoil.thickness.y, ls_thickness, color = color, 
                                      linewidth= linewidth, **self._marker_style, label="%s" % (airfoil.thickness.name))
                    self._add(p)
                    self._cycle_color()                      # in colorycle are pairs 

                    self._plot_max_val(airfoil.thickness, airfoil.isModified, color)
                    self._plot_max_val(airfoil.camber,    airfoil.isModified, color)


    def _plot_max_val (self, airfoilLine: SideOfAirfoil, isModified, color):
        # indicate max. value of camber or thickness line 
        x, y = airfoilLine.maximum
        if isModified:
            text = "New "
            color = cl_helperLine
        else:
            text = ""
            color = color
        p = self.ax.plot (x, y, color=color, **ms_point)
        self._add(p)

        if isModified:
            p = self.ax.annotate(text + "%.2f%% at %.1f%%" % (y * 100, x *100), (x, y), 
                                xytext=(3, 3), textcoords='offset points', color = cl_helperLine)
            self._add (p)   



class Bezier_Edit_Artist (Artist):
    """Plot upper and lower Bezier curve - drag control points with mouse    """

    def __init__ (self, axes, modelFn, **kwargs):
        super().__init__ (axes, modelFn, **kwargs)

        self._points = False                       # show point marker 
        self._camber = False                       # show camber and thcikness

        self.points_upper_artist   = []            # the artists of bezier control points
        self.points_lower_artist   = []            
        self.bezier_upper_artist   = None          # the artist to draw bezier curve
        self.bezier_lower_artist   = None

        self.thickness_artist      = None
        self.camber_artist         = None
        
        self.ciddraw               = None

        self.set_showLegend(True)  

    def set_points (self, aBool): self._points = aBool 

    def set_camber (self, aBool): self._camber = aBool 

    def _deleteMyPlots(self):
        super()._deleteMyPlots()
        # clear up 
        self.ax.relim()                       # make sure all the data fits
        self.points_upper_artist   = []            # the artists of bezier control points
        self.points_lower_artist   = []            
        self.bezier_upper_artist   = None          # the artist to draw bezier curve
        self.bezier_lower_artist   = None
        self.thickness_artist      = None
        self.camber_artist         = None

        if self.ciddraw is not None: 
            self.ax.figure.canvas.mpl_disconnect(self.ciddraw)

    @property
    def airfoil (self) -> Airfoil2_Bezier: return self.model

    def _plot (self): 
        """ do plot of bezier control points and bezier curve 
        """

        # plot bezier control points for upper and lower side

        for sideBezier in [self.airfoil.geo.upper, self.airfoil.geo.lower]:

            side_name = sideBezier.name
            points_artist = []

            for ipoint, cpoint in enumerate (sideBezier.controlPoints):

                markersize = 6
                if ipoint == 0 or ipoint == (len(sideBezier.controlPoints)-1):
                    markerstyle = '.'
                    markersize = 3
                elif side_name == UPPER:
                    markerstyle = 6
                else: 
                    markerstyle = 7

                p = self.ax.plot (*cpoint, marker=markerstyle, markersize=markersize, 
                                  color=cl_userHint, animated=True) 
                points_artist.append (self._add(p))
           
            # plot  bezier curve points 

            label = "Bezier" if side_name == UPPER else ''

            # the marker style to show points
            if self._points:
                _marker_style = ms_points
                linewidth= 0.4
            else:  
                _marker_style = dict()
                linewidth= 0.8

            p = self.ax.plot (sideBezier.x, sideBezier.y, '-', linewidth=linewidth, 
                              color=cl_editing, **_marker_style, animated=True, label=label ) 
            bezier_artist  = self._add(p) 

            # remind artist - activate dragManager per side 
            if side_name == UPPER:
                self.points_upper_artist = points_artist
                self.bezier_upper_artist = bezier_artist 
            else: 
                self.points_lower_artist = points_artist
                self.bezier_lower_artist = bezier_artist 
                
            self._dragManagers.append (DragManager (self.ax, points_artist,
                                typeTag = side_name, draw_event=False, 
                                callback_draw_static    = self.draw_static_all,
                                callback_draw_animated  = self.draw_animated_point,
                                callback_shiftCtrlClick = self.handle_shiftCtrlClick,
                                callback_on_moved       = self._moveCallback)) 


        #  thickness and camber line 
        if self._camber:
            camb  = self.airfoil.camber 
            thick = self.airfoil.thickness 
            p = self.ax.plot (thick.x, thick.y, '--', linewidth=0.8, color=cl_editing_lower, 
                                animated=True, label = thick.name ) 
            self.thickness_artist = self._add(p) 

            p = self.ax.plot (camb.x, camb.y, ':', linewidth=0.8, color=cl_editing_lower, 
                                animated=True, label = camb.name ) 
            self.camber_artist  = self._add(p) 


        # connect to draw event for initial plot of the animated artists all together
        self.ciddraw    = self.ax.figure.canvas.mpl_connect('draw_event', self.on_draw)

        self.show_mouseHelper ()


    def show_mouseHelper (self):
        # show info for section select #
        text = 'ctrl+click to add, shift+click to remove point'
        p = self.ax.text (0.50, 0.05, text, color=cl_userHint, fontsize = 'small',
                    transform=self.ax.transAxes, horizontalalignment='center', verticalalignment='bottom')
        self._add(p)


    def on_draw (self, event): 
        """ call back of draw event when self will be drawn"""

        # the initial draw of the points is not done by DragManager because
        # there are 2 DragMans and we need the background *without* any point 
        canvas = self.ax.figure.canvas
        if event is not None:
            if event.canvas != canvas: raise RuntimeError

        # get the current (empty) background of axes with the points
        background = canvas.copy_from_bbox(self.ax.bbox)
        dragMan: DragManager
        for dragMan in self._dragManagers:
            # provide the dragManagers with an empty background image 
            dragMan.set_background(background)
            dragMan.on_draw (None)                  # now draw points 


    def draw_static_all (self, artist_onMove=None, typeTag=None, *_): 
        """ call back to draw all animated artists in static mode excluding artist  onMove"""

        # drwa all artists excluding the one which is on move 
        for artist in self.points_upper_artist + self.points_lower_artist:
            if artist != artist_onMove: 
                self.ax.draw_artist (artist)
        
        # draw static bezier if there is no move 
        if artist_onMove is None:
            self.bezier_upper_artist.set_linestyle('-')
            self.ax.draw_artist (self.bezier_upper_artist)
            self.bezier_lower_artist.set_linestyle('-')
            self.ax.draw_artist (self.bezier_lower_artist)

            if self._camber:
                camb  = self.airfoil.camber 
                thick = self.airfoil.thickness 
                self.thickness_artist.set (xdata=thick.x, ydata=thick.y)
                self.camber_artist.set    (xdata=camb.x,  ydata=camb.y)
                self.ax.draw_artist (self.thickness_artist)
                self.ax.draw_artist (self.camber_artist)

        else:                                   # do not draw Bezier which is currently changed
            if typeTag == UPPER:
                self.bezier_lower_artist.set_linestyle('-')
                self.ax.draw_artist (self.bezier_lower_artist)
            else:
                self.bezier_upper_artist.set_linestyle('-')
                self.ax.draw_artist (self.bezier_upper_artist)


    def draw_animated_point (self, artist_onMove=None, iArtist = None , typeTag=None): 
        """ call back when point is moving - draw and update Bezier """

        if artist_onMove is not None and iArtist is not None: 

            if typeTag == UPPER:
                bezier_artist = self.bezier_upper_artist
                sideBezier   = self.airfoil.geo.upper
            elif typeTag == LOWER: 
                bezier_artist = self.bezier_lower_artist
                sideBezier   = self.airfoil.geo.lower
            else: 
                return
            
            # get new coordinates (when dragged) and try to move control point 
            x_try, y_try = artist_onMove.get_xydata()[0]

            iPoint = iArtist
            # set new bezier points  - will be checked for valid x,y 
            x, y = sideBezier.move_controlPoint_to(iPoint, x_try, y_try)
            
            # draw marker and get new coordinates (when dragged) 
            artist_onMove.set_xdata(x)
            artist_onMove.set_ydata(y)
            self.ax.draw_artist (artist_onMove)

            # draw new Bezier 
            bezier_artist.set_xdata(sideBezier.x)
            bezier_artist.set_ydata(sideBezier.y)
            bezier_artist.set_linestyle('--')
            self.ax.draw_artist (bezier_artist)


    def handle_shiftCtrlClick (self, iArtist=None, typeTag=None, event=None):
        """handle shift or control click - if posssible insert new opPoint at eventxy
                               - or remove if shift click  
        """
        if not event: return 

        updateBezier = False 
        if typeTag == UPPER:
            points_artist = self.points_upper_artist
            bezier_artist = self.bezier_upper_artist
            sideBezier    = self.airfoil.geo.upper
        elif typeTag == LOWER: 
            points_artist = self.points_lower_artist
            bezier_artist = self.bezier_lower_artist
            sideBezier   = self.airfoil.geo.lower
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
                p = self.ax.plot (x, y, marker='o', color=cl_userHint, 
                                  markersize=4, animated=True) 
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

