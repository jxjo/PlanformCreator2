#!/usr/bin/env pythonupper
# -*- coding: utf-8 -*-

"""  

The "Artists" to plot a airfoil object on a matplotlib axes

"""
import numpy as np
from copy import deepcopy
from artist import Artist, cl_userHint, cl_labelGrid, DragManager

from common_utils import *
from spline import Bezier 
from spline_of_airfoil import SideOfAirfoil_Bezier
from airfoil import* 

cl_planform         = 'whitesmoke'
cl_editing          = 'deeppink'
cl_editing_lower    = 'orchid'
cl_helperLine       = 'orange'
ls_curvature        = '-'
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
                else:  
                    _marker_style = dict()
                    linewidth=1.0

                # plot airfoil 
                p = self.ax.plot (airfoil.x, airfoil.y, '-', color = color, label="%s" % (airfoil.name), 
                                  linewidth= linewidth, **_marker_style)
                self._add(p)

                self._nextColor()                       # in colorycle are pairs 

                if self._pickActive: 
                    self._makeObjectPickable (p)

        # activate event for clicking on line 
        if self._pickActive: self._connectPickEvent ()



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

        airfoil: Airfoil
        for airfoil in airfoilList:
            if (airfoil.isLoaded):
                color = self._nextColor()
                linewidth=0.8

                if self.upper: 
                    line = airfoil.spline.curv_upper 
                    p = self.ax.plot (line.x, line.y, ls_curvature, color = color, label=line.name, 
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

        airfoil: Airfoil
        for airfoil in airfoilList:
            if (airfoil.isLoaded):


                linewidth=0.8

                if self.upper: 
                    line = airfoil.spline.curv_upper 
                    if airfoil.isEdited:
                        color = cl_editing
                    else:
                        color = self._nextColor()
                    p = self.ax.plot (line.x, line.y, ls_curvature, color = color, label=line.name, 
                                      linewidth= linewidth, **self._marker_style)
                    self._add(p)

                    if airfoil.isEdited:
                        self._plot_marker (line, color, 'upper')

                if self.lower: 
                    line = airfoil.spline.curv_lower 
                    if airfoil.isEdited:
                        color = cl_editing_lower
                    else:
                        color = self._nextColor()
                    p = self.ax.plot (line.x, line.y, ls_curvature, color = color, label=line.name, 
                                      linewidth= linewidth, **self._marker_style)
                    self._add(p)

                    if airfoil.isEdited:
                        self._plot_marker (line, color, 'lower')

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

                label = ("%s" % (airfoil.name))  

                if airfoil.isEdited:
                    color = cl_editing
                else:
                    color = self._nextColor()

                linewidth=0.5
                self._plot_le (airfoil.le, color)
                self._plot_le_angle (airfoil)
                self._plot_le_coordinates (airfoil)

                p = self.ax.plot (airfoil.x, airfoil.y, '-', color = color, label=label, 
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



class Bezier_Artist (Artist):
    """Plot upper and lower Bezier curve - drag control points with mouse    """

    def __init__ (self, axes, modelFn, **kwargs):
        super().__init__ (axes, modelFn, **kwargs)

        self._points = False                    # show point marker 

        # show mouse helper / allow drag of points 
        self.points_artist    = []            # the artists of opPoints in this view
        self.bezier_artist    = None

        self.set_showLegend(False)  

        # connect to draw event for initial plot of the animated artists all together
        self.ciddraw    = self.ax.figure.canvas.mpl_connect('draw_event', self.on_draw)

    def _deleteMyPlots(self):
        super()._deleteMyPlots()
        # clear up 
        self.ax.relim()                       # make sure all the data fits
        self.points_artist    = []            # the artists of opPoints in this view
        self.bezier_artist     = None

    @property
    def sideBezier (self) -> SideOfAirfoil_Bezier:  return self.model.upper
   
    @property
    def u(self): 
        return np.linspace (0, 1, 100)


    def _plot (self): 
        """ do plot of bezier control points and bezier curve 
        """

        # plot opPoint bezier control points 

        for ipoint, cpoint in enumerate (self.sideBezier.bezier.points):

            if ipoint == 0 or ipoint == (len(self.sideBezier.bezier.points)-1):
                markerstyle = '.'
            else:
                markerstyle = 'o'
            p = self.ax.plot (*cpoint, marker=markerstyle, color=cl_userHint, markersize=5, animated=True) 
            self._add(p)
            self.points_artist.append (p[0])

           
        # plot  bezier curve points 
        
        x, y = self.sideBezier.bezier.eval(self.u)

        p = self.ax.plot (x,y , '-', linewidth=0.8, color= 'red', animated=True) 
        self._add(p)
        (self.bezier_artist,) = p 

        # activate dragManager 
        self._dragManagers.append (DragManager (self.ax, self.points_artist, 
                            callback_draw_animated = self.handle_point_moving,
                            callback_shiftCtrlClick = self.handle_shiftCtrlClick,
                            callback_on_moved=None)) # self._moveCallback



    def on_draw (self, event): 
        """ call back of draw event when self will be drawn"""

        canvas = self.ax.figure.canvas
        if event is not None:
            if event.canvas != canvas: raise RuntimeError

        # get the current (empty) background of axes
        background = canvas.copy_from_bbox(self.ax.bbox)
        dragMan: DragManager
        for dragMan in self._dragManagers:
            # provide the dragManagers with an empty background image 
            dragMan.set_background(background)
            # do initial draw of all artists of dragMan  
            dragMan.on_draw

        # fix scaling as it would conflict the background image
        self.ax.autoscale(enable=False, axis='both')

    def draw_point_static (self, iArtist): 
        """ call back to draw point when it's ot moving"""
        if iArtist is None: 
            self.bezier_artist.set_linestyle('-')
            self.ax.draw_artist (self.bezier_artist)
        else: 
            self.ax.draw_artist (self.points_artist[iArtist])


    def handle_point_moving(self, duringMove=False, iArtist = None ): 
        """ call back when point is moving - draw and update Bezier """

        # just draw if no move 
        if not duringMove:
            if iArtist is None: 
                self.bezier_artist.set_linestyle('-')
                self.ax.draw_artist (self.bezier_artist)
            else: 
                self.ax.draw_artist (self.points_artist[iArtist])
            return

        # get new coordinates (when dragged) and try to move control point 
        x_try, y_try = self.points_artist[iArtist].get_xydata()[0]

        x, y = self.sideBezier.move_controlPoint_to(iArtist, x_try, y_try)
        
        # draw marker and get new coordinates (when dragged) 
        self.points_artist[iArtist].set_xdata(x)
        self.points_artist[iArtist].set_ydata(y)
        self.ax.draw_artist (self.points_artist[iArtist])

        # draw new Bezier 
        x,y = self.sideBezier.bezier.eval(self.u)
        self.bezier_artist.set_xdata(x)
        self.bezier_artist.set_ydata(y)
        self.bezier_artist.set_linestyle('--')
        self.ax.draw_artist (self.bezier_artist)


    def handle_shiftCtrlClick (self, iArtist=None, event=None):
        """handle shift or control click - if posssible insert new opPoint at eventxy
                               - or remove if shift click  
        """
        if not event: return 

        updateBezier = False 

        if event.key == 'control' and iArtist is None:                     # no new opPoint on an exiysting point

            # new control point - insert according x-coordinate, x,y will be checked
            i_insert, x, y = self.sideBezier.insert_controlPoint_at (event.xdata, event.ydata)
            if not i_insert is None: 
                # create artist for the new point 
                p = self.ax.plot (x, y, marker='o', color=cl_userHint, 
                                  markersize=5, animated=True) 
                self._add(p)
                self.points_artist.insert (i_insert, p[0])
                updateBezier = True 

        elif event.key == 'shift' and iArtist:

            # remove control Point on which shift-click was made
            i_delete = self.sideBezier.delete_controlPoint_at (index = iArtist)
            if not i_delete is None:
                self.points_artist [iArtist].remove()       # matplotlib remove 
                del self.points_artist [iArtist]
                updateBezier = True 

        if updateBezier: 
            # update Bezier now, so redraw wil use the new curve 
            x,y = self.sideBezier.bezier.eval(self.u)
            self.bezier_artist.set_xdata(x)
            self.bezier_artist.set_ydata(y)

