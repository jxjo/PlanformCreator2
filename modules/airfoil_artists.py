#!/usr/bin/env pythonupper
# -*- coding: utf-8 -*-

"""  

The "Artists" to plot a airfoil object on a pg.PlotItem 

"""

from base.math_util             import derivative1
from base.artist                import *
from base.common_utils          import *
from base.spline                import Bezier

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


class Movable_Highpoint (Movable_Point):
    """ 
    Represents the highpoint of an airfoil Line object,
    which can be moved to change the highpoint 
    """

    def __init__ (self, 
                  geo : Geometry, 
                  line : Line, 
                  line_plot_item : pg.PlotDataItem, *args, 
                  movable : bool = False,
                  color : QColor|str, 
                  **kwargs):

        # symmetrical and camber? 
        if line.type == Line.Type.CAMBER and geo.isSymmetrical: 
            movable = False 
        # Bezier is changed via control points ? 
        elif geo.isBezier:
            movable = False 

        self.name = 'max '+ line.name
        self._geo = geo
        self._line = line
        self._line_type = line.type
        self._line_plot_item = line_plot_item

        super().__init__(line.highpoint.xy, 
                         color = color, 
                         movable = movable, 
                         show_label_static = movable,
                         **kwargs)


    def label_static (self, *_):

        if self._line.type == Line.Type.CAMBER and self._geo.isSymmetrical: 
            return  "No camber - symmetrical" 
        else:  
            return super().label_static()

    def label_moving (self, *_):

        if self._line.type == Line.Type.CAMBER and self._geo.isSymmetrical: 
            return  "No camber - symmetrical" 
        else:  
            return f"{self.y:.2%} @ {self.x:.2%}"


    def _moving (self, _):
        """ slot - point is moved by mouse """
        # overlaoded to update airfoil geo 
        # update highpoint coordinates
        self._geo.set_highpoint_of (self._line, (self.x, self.y), finished=False)

        # update self xy if we run against limits 
        self.setPos(self._line.highpoint.xy)

        # update line plot item 
        self._line_plot_item.setData (self._line.x, self._line.y)


    def _finished (self, _):
        """ slot - point is move finished """
        # overlaoded to update airfoil geo 
        self._geo.finished_change_of (self._line)

        # update parent plot item 
        self._changed()




class Movable_TE_Point (Movable_Point):
    """ 
    Represents the upper TE point. 
    When moved the upper plot item will be updated
    When finished final TE gap will be set 
    """

    name = "TE gap"

    def __init__ (self, 
                  geo : Geometry, 
                  upper_plot_item : pg.PlotDataItem, 
                  movable = False, 
                  **kwargs):

        # Bezier is changed via control points ? 
        if geo.isBezier:
            movable = False 

        self._geo = geo
        self._upper_plot_item = upper_plot_item

        xy = self._te_point_xy()

        super().__init__(xy, movable=movable, show_label_static = movable,**kwargs)

    
    def _te_point_xy (self): 
        return self._geo.upper.x[-1], self._geo.upper.y[-1]
    

    def _moving (self, _):
        """ slot -point is moved"""

        # update highpoint coordinates
        self._geo.set_te_gap (self.y * 2 , moving=True)

        # update self xy if we run against limits 
        self.setPos(self._te_point_xy())

        # update line plot item 
        self._upper_plot_item.setData (self._geo.upper.x, self._geo.upper.y)


    def _finished (self, _):
        """ slot - point moving is finished"""
        # final highpoint coordinates
        self._geo.set_te_gap (self.y * 2, moving=False)
        self._changed()


    def label_moving (self, *_):

        return f"{self.name}  {self.y*2:.2%} "

    def _label_opts (self, moving=False, hover=False) -> dict:
        """ returns the label options as dict """

        # overloaded to align right 
        if moving or hover:
            labelOpts = {'color': QColor(Artist.COLOR_NORMAL),
                        'anchor': (1,1),
                        'offset': (10, 10)}
        else: 
            labelOpts = {'color': QColor(Artist.COLOR_LEGEND),
                        'anchor': (1,1),
                        'offset': (10, 10)}
        return labelOpts




class Movable_LE_Point (Movable_Point):
    """ 
    Represents the LE radius . 
    When moved the LE radius will be updated
    When finished final LE radius will be set 
    """
    name =  "LE radius"

    def __init__ (self, 
                  geo : Geometry, 
                  circle : pg.ScatterPlotItem, 
                  movable = False, 
                  **kwargs):

        # Bezier is changed via control points ? 
        if geo.isBezier:
            movable = False 

        self._geo = geo
        self._circle_item = circle
        xy = 2 * self._geo.le_radius , 0

        super().__init__(xy, movable=movable, show_label_static = movable, **kwargs)
  

    def _moving (self, _):
        """ slot - when point is moved"""

        # update radius 
        new_radius = self.x / 2
        new_radius = min(0.05,  new_radius)
        new_radius = max(0.002, new_radius)

        # update self xy if we run against limits 
        self.setPos(2 * new_radius,0)

        # update line plot item 
        self._circle_item.setData ([new_radius], [0])
        self._circle_item.setSize (2 * new_radius)


    def _finished (self, _):
        """ slot - point moving is finished"""

        # final highpoint coordinates
        new_radius = self.x / 2
        self._geo.set_le_radius (new_radius)
        self._changed()


    def label_moving (self, *_):

        return f"{self.name}  {self.x/2:.2%} "





class Movable_Side_Bezier (Movable_Bezier):
    """
    pg.PlotCurveItem/UIGraphicsItem which represents 
    an airfoil Side_Bezier. 
    The Bezier curve which can be changed by the controllpoints
    
    Points are implemented with Moveable_Points
    A Moveable_Point can also be fixed ( movable=False).
    See pg.TargetItem for all arguments 

    Callback 'on_changed' will return the (new) list of 'points'

    """
    def __init__ (self, 
                  airfoil : Airfoil_Bezier,
                  side : Side_Airfoil_Bezier,
                  **kwargs):

        self._airfoil = airfoil
        self._side = side 
        points = side.controlPoints_as_points

        if side.isUpper:
            label_anchor = (0,1) 
        else: 
            label_anchor = (0,0)

        super().__init__(points, label_anchor=label_anchor, **kwargs)

    @property
    def bezier (self) -> Bezier:
        """ the Bezier  self is working with """
        # here - take Bezier of the 'side' 
        return self._side.bezier

    @property
    def u (self) -> list:
        """ the Bezier parameter  """
        # here - take Bezier of the 'side' 
        return self._side._u


    def refresh (self):
        """ refresh control points from side control points """

        # update all my movable points at once 
        movable_point : Movable_Bezier_Point
        for i, point_xy in enumerate(self._side.controlPoints): 
            movable_point = self._movable_points[i]
            movable_point.setPos_silent (point_xy)              # silent - no change signal 

        self.setData(*self.points_xy())                         # update self (polyline) 

        # if self._bezier_item is not None:   	                # update bezier item to be dispalyed
        #     x,y = self.bezier.eval (self.u) 
        #     self._bezier_item.setData (x, y)
        #     self._bezier_item.show()


    def scene_clicked (self, ev : MouseClickEvent):
        """ 
        slot - mouse click in scene of self 
            - handle add Bezier point with crtl-click either on upper or lower side
        """ 

        # handle on ctrl-click
        if not (ev.modifiers() & QtCore.Qt.KeyboardModifier.ControlModifier): return  
       
        # get scene coordinates of click pos and map to view box 
        vb : pg.ViewBox = self.getViewBox()
        pos : pg.Point = vb.mapSceneToView(ev.scenePos())
        pos_x = pos.x()
        pos_y = pos.y()

        # typically there are 2 instances of self - upper and lower Bezier 
        if pos_y < 0.0 and self._side.isLower:
            self._add_point (pos_x, pos_y)
        elif pos_y >= 0.0 and self._side.isUpper:
            self._add_point (pos_x, pos_y)


    def _add_point (self, pos_x, pos_y):
       """ add controlpoint to Bezier curve""" 

       index, point = self._side.check_new_controlPoint_at (pos_x, pos_y)
       
       if index is not None: 

            self._side.add_controlPoint (index, point)
            # _finished will do the rest - and init complete refresh
            self._finished_point()
 

    def _delete_point (self, aPoint : Movable_Point):
        """ slot - point is should be deleted """
        # overloaded - don't delete point 1 
        if aPoint.id == 1: return
        super()._delete_point (aPoint)    

        px, py = self.jpoints_xy()
        self._side.set_controlPoints (px, py)   

        # _finished will do the rest - and init complete refresh
        self._finished_point()


    def _finished_point (self, aPoint = None):
        """ slot - point move is finished """

        # overloaded - update airfoil geometry 
        self._airfoil.geo.finished_change_of (self._side)      

        super()._finished_point(aPoint)



# -------- concrete sub classes ------------------------


class Airfoil_Artist (Artist):
    """Plot the airfoils contour  """


    def __init__ (self, *args, 
                  show_points = False,
                  **kwargs):

        self._show_panels = False                       # show ony panels 
        self._show_points = show_points is True         # show coordinate points

        self._label_with_airfoil_type = False           # include airfoil type in label 

        super().__init__ (*args, **kwargs)

 
    @property
    def label_with_airfoil_type(self): return self._label_with_airfoil_type
    def set_label_with_airfoil_type (self, aBool): self._label_with_airfoil_type = aBool 


    @property
    def show_panels(self): return self._show_panels
    def set_show_panels (self, aBool): 
        self._show_panels = aBool 
        if self._show_panels: 
            self.set_show_points (False)


    @property
    def show_points (self): return self._show_points
    def set_show_points (self, aBool):
        """ user switch to show point (marker )
        """
        self._show_points = aBool is True 

        p : pg.PlotDataItem
        for p in self._plots:
            if isinstance (p, pg.PlotDataItem):
                if self.show_points:
                    p.setSymbol('o')
                else: 
                    p.setSymbol(None)
        self.plot()             # do refresh will show leading edge of spline 



    def set_current (self, aLineLabel):
        # tries to set a highlighted airfoil to section with name ''aLineLabel' 
        if (not aLineLabel is None and aLineLabel != self._curLineLabel):    # only when changed do something
            self._curLineLabel = aLineLabel
            if self.show:                       # view is switched on by user? 
                self.plot ()


    @property
    def airfoils (self) -> list [Airfoil]: return self.data_list


    def _plot (self): 
    
        color_palette = random_colors (len(self.airfoils))

        # are there many airfoils - one of them is DESIGN? 

        airfoil: Airfoil
        airfoils_with_design = False 
        for airfoil in self.airfoils:
            if airfoil.usedAsDesign:
                airfoils_with_design = True 
                break

        for iair, airfoil in enumerate (self.airfoils):
            if (airfoil.isLoaded):

                # no legend if it is only one airfoil 

                if len(self.airfoils) == 1:
                    label = None                            # suppress legend 
                else: 
                    if self.label_with_airfoil_type:
                        label = f"{airfoil.usedAs}: {airfoil.name}"
                    else: 
                        label = f"{airfoil.name}"

                # set color, width, ZValue, symbol style depending on airfoil usage and no of airfoils  

                color = _color_airfoil_of (airfoil.usedAs)
                if color is None:  
                    color = color_palette [iair]

                # default 
                width = 1
                antialias = False
                zValue = 1

                if airfoils_with_design:
                    if airfoil.usedAsDesign:
                        width = 2
                        antialias = True
                        zValue = 3
                    else:
                        color = QColor (color).darker (150)
                elif airfoil.usedAs == usedAs.NORMAL:
                        width = 2
                        antialias = True

                pen = pg.mkPen(color, width=width)

                sPen, sBrush, sSize = pg.mkPen(color, width=1), 'black', 7
                s = 'o' if self.show_points else None 

                # plot contour and fill airfoil if it's only one 
                #   use geometry.xy to refelect changes in diesign airfoil

                if len(self.airfoils) == 1: 

                    # if there is only one airfoil, fill the airfoil contour with a soft color tone  
                    brush = pg.mkBrush (color.darker (600))
                    self._plot_dataItem  (airfoil.geo.x, airfoil.geo.y, name=label, pen = pen, 
                                          symbol=s, symbolSize=sSize, symbolPen=sPen, symbolBrush=sBrush, 
                                          fillLevel=0.0, fillBrush=brush, antialias = antialias,
                                          zValue=zValue)
                else: 
                    self._plot_dataItem  (airfoil.geo.x, airfoil.geo.y, name=label, pen = pen, 
                                          symbol=s, symbolSize=sSize, symbolPen=sPen, symbolBrush=sBrush,
                                          antialias = antialias,
                                          zValue=zValue)

                # optional plot of real LE defined by spline 

                if self.show_points and airfoil.geo.isSplined:
                    if airfoil.isNormalized:
                        brushcolor = "limegreen"
                        text = None
                    else: 
                        brushcolor = "yellow"
                        text="LE spline"
                    self._plot_point (airfoil.geo.le_real, color=color, brushColor=brushcolor,
                                      text=text,anchor=(0.5,1) )



class Bezier_Artist (Artist):
    """Plot and edit airfoils Bezier control points """

    sig_bezier_changed     = pyqtSignal()           # bezier curve changed 

    @property
    def airfoils (self) -> list [Airfoil]: return self.data_list

    def refresh_from_side (self, aLinetype):

        p : Movable_Side_Bezier
        for p in self._plots: 
            if p._side.type == aLinetype:
                p.refresh()

    def _plot (self): 
    
        airfoil: Airfoil

        for airfoil in self.airfoils:
            if airfoil.isBezierBased and airfoil.isLoaded:

                color = _color_airfoil_of (airfoil.usedAs)
                movable = airfoil.usedAsDesign

                side : Side_Airfoil_Bezier
                for side in [airfoil.geo.lower, airfoil.geo.upper]:     # paint upper on top 

                    p = Movable_Side_Bezier (airfoil, side, color=color, movable=movable,
                                              on_changed=self.sig_bezier_changed.emit) 
                    self._add(p)
 
                    # connect to mouse click in scene to add a new Bezier control point 

                    if movable:
                        sc : pg.GraphicsScene = p.scene()
                        sc.sigMouseClicked.connect (p.scene_clicked)


    # def draw_hicksHenne (self, airfoil: Airfoil_Bezier):
    #     """ draw hicks henne functions of airfoil """

    #     linewidth   = 1
    #     linestyle   = ':'

    #     side : Side_Airfoil_HicksHenne

    #     for side in [airfoil.geo.upper, airfoil.geo.lower]:
    #     # side = airfoil.geo.upper

    #         if side.name == UPPER:
    #             delta_y =  0.1
    #         else:
    #             delta_y = -0.1

    #         hh : HicksHenne
    #         for ih, hh in enumerate(side.hhs):

    #             # plot hh function 
    #             x = side.x 
    #             y = hh.eval (x) 
    #             p = self.ax.plot (x,y * 10 + delta_y, linestyle, linewidth=linewidth , alpha=1) 
    #             self._add(p)

    #             # plot maximum marker 
    #             x = hh.location
    #             y = hh.strength  * 10 + delta_y
    #             color =self._get_color (p) 
    #             p = self.ax.plot (x, y, color=color, **ms_point)
    #             self._add(p)

    #             p = self.ax.annotate(f'{ih+1}  w{hh.width:.2f}', (x, y), fontsize='small',
    #                 xytext=(3, 3), textcoords='offset points', color = color)
    #             self._add(p)

    #         # print info text 

    #         if self.show_title:    
    #             p = _plot_side_title (self.ax, side)
    #             self._add(p)




class Curvature_Artist (Artist):
    """
    Plot curvature (top or bottom) of an airfoil
    """
    name = 'Curvature' 

    def __init__ (self, *args, show_derivative=False, **kwargs):

        self._show_upper = True                     # show upper side 
        self._show_lower = True                     # show lower side 
        self._show_derivative = show_derivative     # show derivative of curvature 
 
        super().__init__ (*args, **kwargs)

    @property
    def show_upper(self): return self._show_upper
    def set_show_upper (self, aBool): 
        self._show_upper = aBool
        self.plot() 

    @property
    def show_lower(self): return self._show_lower
    def set_show_lower (self, aBool): 
        self._show_lower = aBool 
        self.plot()

    @property
    def show_derivative(self): return self._show_derivative
    def set_show_derivative (self, aBool): 
        self._show_derivative = aBool 
        self.plot()

    @property
    def airfoils (self) -> list [Airfoil]: return self.data_list


    def _plot (self): 

        nairfoils = len(self.airfoils)
        
        airfoil: Airfoil

        for airfoil in self.airfoils:

            color = _color_airfoil_of (airfoil.usedAs)

            sides = []
            if self.show_upper: sides.append (airfoil.geo.curvature.upper)
            if self.show_lower: sides.append (airfoil.geo.curvature.lower)

            side : Line
            for side in sides:
                x = side.x
                y = side.y      
                if side.type == Line.Type.UPPER:
                    pen = pg.mkPen(color, width=1, style=Qt.PenStyle.SolidLine)
                else: 
                    pen = pg.mkPen(color, width=1, style=Qt.PenStyle.DashLine)

                label = f"{side.name} - {airfoil.name}"
                self._plot_dataItem (x, y, name=label, pen=pen)

                # plot derivative1 of curvature ('spikes') 

                if self.show_derivative and (nairfoils == 1 or airfoil.usedAsDesign):
                    pen = QPen (pen)
                    pen.setColor (QColor('red'))
                    name = f"{side.name} - Derivative"
                    self._plot_dataItem (x, -derivative1(x,y), name=name, pen=pen)

                # plot max points at le and te and reversals

                self._plot_le_te_max_point (side, color )
                self._plot_reversals (side, color)



    def _plot_le_te_max_point (self, aSide : Line, color ):
        """ plot the max values at LE and te"""

        point_color = QColor (color).darker (150)

        # le 
        if aSide.isUpper:
            anchor = (0,1)
        else: 
            anchor = (0,0)
        text = f"max {aSide.name} {aSide.max_xy[1]:.0f}"
        self._plot_point (aSide.max_xy, color=point_color, text=text, anchor=anchor,
                          textColor=Artist.COLOR_LEGEND)

        # te 
        if aSide.isUpper:
            anchor = (1,1)
        else: 
            anchor = (1,0)
        text = f"max {aSide.name} {aSide.te[1]:.1f}"
        self._plot_point (aSide.te, color=point_color, text=text, anchor=anchor,
                          textColor=Artist.COLOR_LEGEND)


    def _plot_reversals (self, side : Line, color):
        """ annotate reversals of curvature """

        point_color = QColor (color).darker (150)

        reversals = side.reversals()
        if reversals:
            for reversal_xy in reversals: 
                if side.isUpper:
                    anchor = (1,1.2)
                else: 
                    anchor = (1,-0.2)
                self._plot_point (reversal_xy, color=point_color, size=2, text="R", anchor=anchor,
                                  textColor=Artist.COLOR_LEGEND)


# class Difference_Artist (Airfoil_Line_Artist):
#     """Plot the y-difference of two airfoils 

#         2nd airfoil is Bezier based airfoil 
#         1st is reference or original airfoil from where x-stations are taken  
#     """

#     @property
#     def airfoil (self) -> Airfoil_Bezier: 
#         return self.airfoils[1] 
    
#     @property
#     def ref_airfoil (self) -> Airfoil : 
#         return self.airfoils[0] 
    

#     def _get_difference (self, side_ref: Side_Airfoil, side_actual: Side_Airfoil_Bezier):
#         # calculate difference at y-stations of reference airfoil 
#         diff  = np.zeros (len(side_ref.x))
#         for i, x in enumerate(side_ref.x):
#             diff [i] = side_actual.bezier.eval_y_on_x (x, fast=True) - side_ref.y[i]
#         return diff 


#     def _plot (self): 

#         if len(self.airfoils) != 2 : return 

#         self.set_showLegend (False)                             # no legend 
#         color = _color_airfoil_of (self.airfoil.usedAs)
#         linewidth=0.8

#         if self.upper:
#             x = self.ref_airfoil.geo.upper.x
#             y = 10 * self._get_difference (self.ref_airfoil.geo.upper, self.airfoil.geo.upper )
#             p = self.ax.plot (x, y, ls_difference, color = color, 
#                             linewidth= linewidth, **self._marker_style)
#             self._add(p)

#         if self.lower:
#             x = self.ref_airfoil.geo.lower.x
#             y = 10 * self._get_difference (self.ref_airfoil.geo.lower, self.airfoil.geo.lower ) 
#             p = self.ax.plot (x, y, ls_difference, color = color, 
#                             linewidth= linewidth, **self._marker_style)
#             self._add(p)




class Airfoil_Line_Artist (Artist, QObject):
    """
    Plot thickness, camber line of an airfoil, print max values 
    """

    sig_geometry_changed     = pyqtSignal()          # airfoil data changed 

    @property
    def airfoils (self) -> list [Airfoil]: return self.data_list


    def _plot (self): 

        airfoil: Airfoil

        for iair, airfoil in enumerate (self.airfoils):

            color = _color_airfoil_of (airfoil.usedAs)
            color.setAlphaF (0.8)

            # plot all 'lines' of airfoil 

            for line in airfoil.geo.lines_dict.values():
                
                style = _linestyle_of (line._type)
                if airfoil.usedAsDesign:
                    zValue = 5                                  # plot design on top 
                else:
                    zValue = 4 

                is_upper = line.type == Line.Type.UPPER
                is_lower = line.type == Line.Type.LOWER
                is_upper_lower = is_upper or is_lower

                if iair == 0 and not is_upper_lower:            # line legend only for the first airfoil 
                    name = line.name
                else: 
                    name = None 

                # plot upper and lower only for design (to visualize move highpoint)

                if airfoil.usedAsDesign or not is_upper_lower:
                    pen = pg.mkPen(color, width=1, style=style)
                    p = self._plot_dataItem (line.x, line.y, pen = pen, name = name, zValue=zValue)
                else: 
                    p = None

                # plot its highpoint 

                ph = Movable_Highpoint (airfoil.geo, line, p, 
                                            movable=airfoil.usedAsDesign, color=color,
                                            on_changed=self.sig_geometry_changed.emit )
                self._add (ph) 


                # te gap point for DESIGN 

                if airfoil.usedAsDesign and is_upper:
                    pt = Movable_TE_Point (airfoil.geo, p, 
                                            movable=airfoil.usedAsDesign, color=color,
                                            on_changed=self.sig_geometry_changed.emit )
                    self._add (pt) 


            # plot le circle 

            radius = airfoil.geo.le_radius
            circle_item = self._plot_point (radius, 0, color=color, size=2*radius, pxMode=False, 
                                            style=Qt.PenStyle.DotLine, brushAlpha=0.3, brushColor='black')
            pl = Movable_LE_Point (airfoil.geo, circle_item, 
                                    movable=airfoil.usedAsDesign, color=color,
                                    on_changed=self.sig_geometry_changed.emit )
            self._add(pl) 
