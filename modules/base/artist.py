#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""  

An Artist is responsible for plotting one or more PlotDataItem (pg)
of a given PlotItem 

An Artist is alawys subclassed from Artist_Abstract and enriched with
data aware, semantic functions to plot the data it is intended to do

All PlotItem, ViewBox settings are made 'outside' of an Artist

see: https://pyqtgraph.readthedocs.io/en/latest/getting_started/plotting.html

"""

from typing             import override
from enum               import StrEnum

import numpy as np

import pyqtgraph as pg
from pyqtgraph.Qt       import QtCore
from PyQt6.QtCore       import pyqtSignal

from pyqtgraph.graphicsItems.ScatterPlotItem    import Symbols
from pyqtgraph.graphicsItems.GraphicsObject     import GraphicsObject
from pyqtgraph.GraphicsScene.mouseEvents        import MouseClickEvent


from PyQt6.QtCore       import Qt, QTimer, QObject
from PyQt6.QtGui        import QColor

from base.common_utils  import *
from base.math_util     import JPoint 
from base.spline        import Bezier 

import logging
logger = logging.getLogger(__name__)
# logger.setLevel(logging.DEBUG)

class qcolors (StrEnum):

    EDITABLE      = 'orange' 
    HOVER         = 'deepskyblue'
    ERROR         = 'firebrick'

# -------- common methodes ------------------------



def random_colors (nColors, h_start=0) -> list[QColor]:
    """ 
    returns a list of random QColors which fit together 
    Args:
        h_start: optional Hue start value: 0=red, 2/6=green, 4/6=blue
    """

    # https://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
    golden_ratio = 0.618033988749895
    colors = []

    for i in range (nColors):
        h = h_start + golden_ratio * (i+1)/(nColors+1) 
        h = h % 1.0
        colors.append(QColor.fromHsvF (h, 0.5, 0.95, 1.0) ) #0.5
    return colors


def color_in_series (color : QColor | str, i, n, delta_hue=0.1):
    """ 
    returns the i-th of n colors in a color hsv starting with color upto color + delta_hue
    """

    # sanity 
    if n < 2:
        n = 2
        i = 0  

    if isinstance (color, str): 
        color = QColor (color) 

    start_hue, sat, value, alpha = color.getHsvF ()
    hue = start_hue + i * delta_hue / (n-1) 
    hue = hue % 1.0

    return QColor.fromHsvF (hue, sat, value, alpha)


# -------- pg defaults ------------------------


pg.setConfigOptions(antialias=False)
pg.setConfigOptions(mouseRateLimit=30)



# ---------------------------------------------------------------------------


class Movable_Point (pg.TargetItem):
    """
    Abstract pg.TargetItem/UIGraphicsItem which represents a single, moveable point.
    
    A Moveable_Point reacts on mouse move (drag) and finished to perform 
    modifications of an model object.

    A Moveable_Point can also be fixed ( movable=False).

    See pg.TargetItem for all arguments 

    """

    name = 'Point'                                  # name of self - shown in 'label'

    sigShiftClick = QtCore.Signal(object)           # signal when point is shift clicked

    def __init__ (self, 
                  xy_or_point : tuple | JPoint, 
                  parent = None,                    # to attach self to parent (polyline)
                  name : str = None, 
                  id = None, 
                  symbol = '+',
                  size = None,
                  movable : bool = False,
                  movable_color = None,
                  show_label_static : bool = True, 
                  label_anchor = (0,1),
                  color = "red",
                  on_changed = None,
                  **kwargs):

        # use JPoint to check xy limits 

        if isinstance (xy_or_point, JPoint):
            self._jpoint = xy_or_point
        else: 
            self._jpoint = JPoint (xy_or_point)

        movable = movable and (not self._jpoint.fixed)

        if self._jpoint.name is not None:
            self.name = self._jpoint.name                               # jpooint has precedence 
        elif name is not None:
            self.name = name                                            # take argument
        else:
            pass                                                        # take class name 

        self._id = id
        self._callback_changed = on_changed if callable(on_changed) else None 

        self._show_label_static = show_label_static
        self._label_anchor = label_anchor


        self._symbol_moving  = 'crosshair' 
        self._symbol_hover   = 'crosshair' 
        self._symbol_movable = symbol 
        self._symbol_fixed   = symbol

        # set pen colors and brushes 

        if movable:
            symbol = self._symbol_movable
            size = size if size is not None else 9 

            brush_color = QColor(color) if color else qcolors.EDITABLE
            brush_color = QColor(brush_color.darker(200))

            color = movable_color if movable_color is not None else qcolors.EDITABLE
            hoverBrush = qcolors.HOVER

            pen = pg.mkPen (color, width=1) 
            hoverPen = pg.mkPen (qcolors.HOVER, width=1)

            self._movingBrush =  QColor('black')
            self._movingBrush.setAlphaF (0.3) 

        else: 
            symbol = self._symbol_fixed
            size = size if size is not None else 9 

            penColor = QColor (color).darker (120)

            pen = pg.mkPen (penColor, width=1)
            brush_color = QColor (penColor) # QColor (color)

            hoverPen = pg.mkPen (color, width=1) 
            hoverBrush = QColor(color)

        # init TargetItem  

        super().__init__(pos=self._jpoint.xy, pen= pen, brush = brush_color, 
                         hoverPen = hoverPen, hoverBrush = hoverBrush,
                         symbol=symbol, size=size, movable=movable,
                         label = self.label_static, 
                         labelOpts = self._label_opts(),
                         **kwargs)

        # attach to parent (polyline) 
        if parent is not None: 
            self.setParentItem (parent)

        # points are above other things
        if movable:
            self.setZValue (10)                     # only within same parent item
        else: 
            self.setZValue (2)

        # default callback setup 
        self.sigPositionChanged.connect (self._moving)
        self.sigPositionChangeFinished.connect (self._finished)

    @property
    def xy (self) -> tuple[float]: 
        """ returns x,y coordinates checked against limits"""
        # get coordinates of TargetItem 
        x = self.pos().x()
        y = self.pos().y()  

        # jpoint will take care of the x,y limits 
        self._jpoint.set_xy (x,y)
        xy = self._jpoint.xy

        # and reset maybe corrected x,y, 
        self.setPos (xy) 

        return xy

    @property
    def x (self) -> float: return self.xy[0]

    @property
    def y (self) -> float: return self.xy[1]


    @property
    def id (self): return self._id 


    def set_name (self, aName :str):
        """ set a(dynamic) name of self"""
        self.name = aName
        self._label.valueChanged()                  # force label callback


    def label_static (self, *_) -> str:
        """ the static label - can be overloaded """
        if self._show_label_static:
            return f"{self.name}" 
        else:
            return None

    def label_moving (self, *_):
        """ the label when moving - can be overloaded """
        return f"{self.name} {self.y:.4n}@{self.x:.4n}"


    def label_hover (self, *_):
        """ the label when hovered - can be overloaded """
        return self.label_moving (*_)
        # return f"Point {self.y:.4n}@{self.x:.4n} hovered"


    def _label_opts (self, moving=False, hover=False) -> dict:
        """ returns the label options as dict """

        if moving or hover:
            labelOpts = {'color': QColor(Artist.COLOR_NORMAL),
                        'anchor': self._label_anchor,
                        'offset': (5, 0)}
        else: 
            labelOpts = {'color': QColor(Artist.COLOR_LEGEND),
                        'anchor': self._label_anchor,
                        'offset': (5, 0)}
        return labelOpts


    def _moving (self):
        """ default slot - point is moved by mouse """
        # to be overlaoded 

    def _finished (self):
        """ default slot -  when point move is finished """
        # to be overlaoded 

    def _changed (self): 
        """ handle callback to parent when finished """
        if callable(self._callback_changed):
            # callback / emit signal delayed so we leave the scope of Graphics 
            timer = QTimer()                                
            timer.singleShot(10, self._callback_changed)     # delayed emit 


    # TargetItem overloaded ---------------------------

    @override
    def setPos (self,*args):
        # overridden as change detection of TargetItem is too sensible for numerical issues
        try:
            newPos = pg.Point(*args)
        except: 
            raise TypeError(f"Could not make Point from arguments: {args!r}")

        if (round(self._pos.x(), 6) !=  round(newPos.x(), 6) or
            round(self._pos.y(), 6) !=  round(newPos.y(), 6)): 
            # print ("not equal", self._pos, newPos, self._pos.x()+self._pos.y(), newPos.x()+newPos.y())
            super().setPos(*args)


    def setPos_silent (self, *args): 
        """ same as superclass targetItem.setPos but doesn't signal sigPositionChanged """

        # jpoint will take care of the x,y limits 
        self._jpoint.set_xy (*args)
        xy = self._jpoint.xy

        # used for high speed refresh 
        newPos = pg.Point(xy)
        if (round(self._pos.x(), 6) !=  round(newPos.x(), 6) or
            round(self._pos.y(), 6) !=  round(newPos.y(), 6)): 
            self._pos = newPos
            GraphicsObject.setPos(self, self._pos)            # call grand pa to avoid signal 


    @override
    def mouseClickEvent(self, ev : MouseClickEvent):
        """ pg overloaded - ghandle shift_click """
        if self.movable :
            if ev.modifiers() & QtCore.Qt.KeyboardModifier.ShiftModifier: 
                ev.accept()
                self.sigShiftClick.emit(self) 
        return super().mouseClickEvent(ev)


    @override
    def mouseDragEvent(self, ev):

        super().mouseDragEvent (ev) 

        if ev.isStart() and self.moving:
            self.setLabel (self.label_moving, self._label_opts(moving=True))
            self.setMovingBrush ()
            self.setPath (Symbols[self._symbol_moving])

        if ev.isFinish() and not self.moving:
            self.setLabel (self.label_static, self._label_opts(moving=False))
            self.setPath (Symbols[self._symbol_movable])


    @override
    def hoverEvent(self, ev):
        # overridden to allow mouse hover also for points which are not movable
        if (not ev.isExit()) and ev.acceptDrags(QtCore.Qt.MouseButton.LeftButton):
            self.setMouseHover(True)
        else:
            self.setMouseHover(False)

 

    @override
    def setMouseHover (self, hover: bool):
        # overridden from TargetItem to get hover event for new label 

        if not self.mouseHovering is hover:
            if hover:
                self.setLabel (self.label_hover, self._label_opts(hover=hover))
            else: 
                self.setLabel (self.label_static, self._label_opts(hover=hover))        
                
        super().setMouseHover(hover)


    @override
    def setMovingBrush(self):
        """Set the brush that fills the symbol when moving.
        """
        if self.moving:
            self.currentBrush = self._movingBrush
            self.update()



class Movable_Bezier_Point (Movable_Point):
    """ 
    Represents one control point of a Side_Bezier,
    """

    @override
    def label_moving (self,*_):
        """ label precision depending on value """

        if self.x >= 1000:
            precision_x = 0
        elif self.x >= 100:
            precision_x = 1
        elif self.x >=10:
            precision_x = 2
        else:
            precision_x = 3

        if self.y >= 1000:
            precision_y = 0
        elif self.y >= 100:
            precision_y = 1
        elif self.y >=10:
            precision_y = 2
        else:
            precision_y = 3

        return f"x {self.x:.{precision_x}f}\ny {self.y:.{precision_y}f}"



class Movable_Bezier (pg.PlotCurveItem):
    """
    pg.PlotCurveItem/UIGraphicsItem which represents 
    a Bezier curve which can be changed by the controlpoints
    
    Points are implemented with Movable_Points
    A Movable_Point can also be fixed ( movable=False).
    See pg.TargetItem for all arguments 

    Callback 'on_changed' will return the (new) list of 'points'

    """
    def __init__ (self, 
                  jpoints : list[JPoint], 
                  id = None, 
                  color = None, 
                  movable = False,
                  label_anchor = (0,1),
                  show_static = False,                              # plot also when not in move 
                  movable_point_class = Movable_Bezier_Point,       # to choose an individual Movable_Point
                  on_changed = None, 
                  **kwargs):

        self._callback_changed = on_changed
        self._id = id 
        self.movable = movable 

        # Control jpoints  
        self._jpoints : list[JPoint] = jpoints 
        # ... as movable Bezier points 
        self._movable_points = []
 

        # init polyline of control points as PlotCurveItem
          
        if movable:
            penColor = QColor (qcolors.EDITABLE).darker (120)
        else:
            penColor = QColor (color).darker (150)
        pen = pg.mkPen (penColor, width=1, style=Qt.PenStyle.DotLine)

        super().__init__(*self.jpoints_xy(), pen=pen)

        if movable:
            self.setZValue (10)                     # movable dotted line above other objects 
        else: 
            self.setZValue (5)

        # init control points as Movable_Points 

        symbol = 's'

        for i, jpoint in enumerate (jpoints):

            p = movable_point_class (jpoint, parent=self, name=f"P{str(i)}", id = i, movable=movable, 
                                     color=color, symbol=symbol, size=7, label_anchor=label_anchor,  **kwargs) 
            
            p.sigPositionChanged.connect        (self._moving_point)
            p.sigPositionChangeFinished.connect (self._finished_point)
            p.sigShiftClick.connect             (self._delete_point)
            self._movable_points.append(p)


        # init temp PlotCurve to represent Bezier during move 

        self._bezier = None                             # a helper bezier to show during move 
        self._u = None                                  # u distribution of helper bezier 
        self._bezier_item = None                        # plotItem of bezier 

        if movable or show_static: 

            x,y = self.bezier.eval(self.u)
 
            pen = pg.mkPen (QColor (color), width=1, style=Qt.PenStyle.DashLine)
            self._bezier_item = pg.PlotCurveItem (x,y, pen=pen)
            self._bezier_item.setParentItem (self)

            if not show_static:
 
                self._bezier_item.hide()

    @property
    def id (self):
        """ returns id of self """
        return self._id 


    @property
    def bezier (self) -> Bezier:
        """ the Bezier self is working with - displayed on move  """
        # can be overloaded 
        # here - we use a helper bezier to show during move  
        if self._bezier is None: 
            self._bezier = Bezier (*self.jpoints_xy())
        return self._bezier

    @property
    def u (self) -> list:
        """ the Bezier paramter  """
        # can be overloaded 
        if self._u is None: 
            self._u = np.linspace (0.0, 1.0, 50)                # only 50 points for speed
        return self._u

  

    def jpoints_xy (self) -> tuple[list]:
        """returns coordinates of self_jpoints as x, y lists """
        x, y = [], []
        for p in self._jpoints:
            x.append(p.x)
            y.append(p.y)
        return x, y

    def points_xy (self) -> tuple[list]:
        """returns coordinates of self_movable_points as x, y lists """
        x, y = [], []
        for p in self._movable_points:
            x.append(p.x)
            y.append(p.y)
        return x, y


    @override
    def mouseClickEvent(self, ev : MouseClickEvent):
        """ pg overloaded - handle crtl_click """
        if self.movable :
            if ev.modifiers() & QtCore.Qt.KeyboardModifier.ControlModifier: 
                x = round (ev.pos().x(),6)
                y = round (ev.pos().y(),6)
                added = self._add_point ((x,y))

                if added: 
                    ev.accept()
                else: 
                    ev.ignore()

        return super().mouseClickEvent(ev)


    def _moving_point (self, aPoint : Movable_Point):
        """ slot - point is moved by mouse """
        i = aPoint.id
        self._jpoints[i].set_xy(aPoint.xy)                  # update self point list 
        self.setData(*self.points_xy())                     # update self (polyline) 

        if self._bezier_item:            
            self.bezier.set_points(*self.points_xy())      # update of bezier
            x,y = self.bezier.eval(self.u)
            self._bezier_item.setData (x, y)
            self._bezier_item.show()


    def _add_point (self, xy : tuple) -> bool:
        """ 
        handle add point - will be called when ctrl_click on Bezier
        
        Returns: 
            inserted: True if point was added 
        """

        # to be overridden
        logger.debug (f"Bezier ctrl_click at x={xy[0]} y={xy[1]}")
        return False



    def _delete_point (self, aPoint : Movable_Point):
        """ slot - point should be deleted """

        # a minimum of 2 control points 
        if len(self._jpoints) <= 2: return   

        # remove from list
        i = aPoint.id
        del self._jpoints[i]                                # update self point list 
        px, py = self.jpoints_xy()
        self.setData(px, py)                                # update self (polyline) 

        if self._bezier_item: 
            x,y = self.bezier.eval(self.u)                  # update of bezier
            self._bezier_item.setData (x, y)
            self._bezier_item.show()

        self._finished_point (aPoint)
        if aPoint.scene():                                  # sometimes scene get lost ... (?) 
            aPoint.scene().removeItem(aPoint)               # final delete from scene 


    def _finished_point (self, aPoint):
        """ slot - point move is finished """
        
        if callable(self._callback_changed):
            timer = QTimer()   
            # delayed emit 
            timer.singleShot(10, lambda: self._callback_changed())




# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------



class Artist(QObject):
    """
        Abstract class:
        
        An Artist is responsible for plotting one or more PlotDataItem (pg)
        within a given ViewBox 

        An Artist is alawys subclassed from Artist_Abstract and enriched with
        data aware, semantic functionsto plot the data it is intended to do

        All ViewBox settings are made 'outside' of an Artist

        On init the artist doesn't plot data. It has to be 'plot' or 'refresh' 

    """

    name = "Abstract Artist" 

    SIZE_HEADER         = 14                            # size in pt 
    SIZE_HEADER_SMALL   = 11                            
    SIZE_NORMAL         = 10 

    COLOR_HEADER        = "whitesmoke"
    COLOR_NORMAL        = "silver"
    COLOR_LEGEND        = "darkgray"
    COLOR_HELP          = QColor ('deepskyblue').darker(120)

    show_mouse_helper_default   = True                  # global setting to show mouse helper points

    sig_help_message     = pyqtSignal (object, str)     # new user help message of self 


    def __init__ (self, pi: pg.PlotItem , 
                  getter = None,       
                  show = True,
                  show_mouse_helper = None,
                  show_legend = False):
        """
 
        Args:
            gv: GraphicsView where PlotDataItes will be added 
            get: getter for data_objects (either bound method or objects)  
            show: True: items will be show immidiatly show_points
            show_points: show data points as markers  
        """

        super().__init__()

        self._pi = pi                       # (parent) plotItem)
        self._getter = getter               # bounded method to the model e.g. Wing 

        self._show = show is True           # should self be plotted? 
        self._show_legend = show_legend is True 
        self._show_mouse_helper = show_mouse_helper 

        self._plots = []                    # plots (PlotDataItem) made up to now 

        self._t_fn  = None                  # coordinate transformation function accepting x,y
        self._tr_fn = None                  # reverse transformation function accepting xt,yt


        # do not 'plot' on init
        # self.plot() 


    @override
    def __repr__(self) -> str:
        # get a nice print string 
        text = '' 
        return f"<{type(self).__name__}{text}>"


    # ------- public ----------------------

    @property
    def data_object (self): 
        # to be ooverloaded - or implemented with semantic name 
        if callable(self._getter):
            return self._getter()
        else: 
            return self._getter
        
    @property
    def data_list (self): 
        # to be ooverloaded - or implemented with semantic name        
        if isinstance (self.data_object, list):
            return self.data_object
        else: 
            return [self.data_object]   


    @property
    def show (self): return self._show

    def set_show (self, aBool, refresh=True):
        """
        switch to enable/disable ploting the data
            - refresh=True will immediatly refresh 
        """
        self._show = aBool is True 

        if self.show: 
            if refresh:
                if not self._plots:
                    self.plot()                                 # first time, up to now no plots created ...
                else: 
                    self.refresh()                              # normal refresh 
            else: 
                pass                                            # will be shown with next refresh 
        else:
            p : pg.PlotDataItem
            for p in self._plots:                               # always hide all plot 
                p.hide()

            if self.show_legend:                                # and remove legend 
                self._remove_legend_items ()

            self.set_help_message (None)                        # remove user help message


    @property
    def show_legend (self): return self._show_legend
    def set_show_legend (self, aBool):
        """ user switch to show legend for self plots
        """
        self._show_legend = aBool is True 

        if self.show_legend:
            self.plot()
        else: 
            self._remove_legend_items ()

    @property
    def show_mouse_helper (self):
        """ show mouse helpers of self"""
        if self._show_mouse_helper is None:
            return Artist.show_mouse_helper_default
        else: 
            return self._show_mouse_helper


    def set_show_mouse_helper (self, aBool : bool):
        """ on/off for mouse helper of self - for global setting use class variable"""
        self._show_mouse_helper = aBool == True


    @property
    def t_fn (self):
        """ current active transformation function to transform x,y in view coordinates"""
        if self._t_fn is None: 
            return lambda x,y : (x,y)                   # dummy 1:1 tra<nsformation
        else: 
            return self._t_fn
    
    def set_t_fn (self, transform_fn):
        """ set transformation function to transform x,y in view coordinates"""
        if transform_fn is not None:
            if callable (transform_fn):
                self._t_fn = transform_fn
            else:
                raise ValueError ("transformation function is not callable")
        else:
            self._t_fn = None 


    @property
    def tr_fn (self):
        """ current active reverse transformation function to transform x,y from view coordinates"""
        if self._tr_fn is None: 
            return lambda x,y : (x,y)                   # dummy 1:1 transformation
        else: 
            return self._tr_fn
    
    def set_tr_fn (self, transform_fn):
        """ set reverse transformation function to transform x,y from view coordinates"""
        if transform_fn is not None:
            if callable (transform_fn):
                self._tr_fn = transform_fn
            else:
                raise ValueError ("transformation function is not callable")
        else:
            self._tr_fn = None 


    def set_help_message (self, aMessage : str):
        """ set user help message - signal it parent"""
        self.sig_help_message.emit (self, aMessage)


    def plot (self):
        """the artist will (re)plot - existing plots will be deleted 
        """
        if self.show:

            self._remove_legend_items ()
            self._remove_plots ()

            if self.show_legend and self._pi.legend is None:
                # must be before .plot 
                self._pi.addLegend(offset=(-10,10),  verSpacing=0 )  
                self._pi.legend.setLabelTextColor (self.COLOR_LEGEND)

            if len(self.data_list) > 0:

                self._plot()                        # plot data list 

                if self._plots:
                    logger.debug  (f"{self} of {self._pi} - plot {len(self._plots)} items")
        # else:
        #     self.set_help_message (None)                              # remove help message of self 


    def refresh(self):
        """ refresh self plots by setting new x,y data """

        if self.show:
            self._refresh_plots ()

            if self.show_legend:
                self._remove_legend_items ()
                self._add_legend_items()

        # else:

        #     self.set_help_message (None)                              # remove help message of self 
            # logging.debug (f"{self} refresh")


    # --------------  private -------------

    def _plot (self):
        """ main method to plot the items"""
        # do plot - overwritten in sublass
        pass


    def _plot_dataItem (self, x, y,  
                        name=None, 
                        zValue=1,
                        **kwargs) -> pg.PlotDataItem:
        """ plot DataItem and add it to self._plots etc """

        # (optional) transformation of coordinate 
        xt, yt = self.t_fn (x,y)

        p = pg.PlotDataItem  (xt, yt, **kwargs)

        p.setZValue (zValue)

        self._add (p, name=name)

        return p 
        

    def _plot_point (self, 
                    *args,                     # optional: tuple or x,y
                     symbol='o', color=None, style=Qt.PenStyle.SolidLine, 
                     size=7, pxMode=True, 
                     brushColor=None, brushAlpha=1.0,
                     text=None, textColor=None, textPos=None, anchor=None):
        """ plot point with text label at x, y - text will follow the point """

        if isinstance (args[0], tuple):
            x = args[0][0] 
            y = args[0][1] 
        else: 
            x = args[0]
            y = args[1] 

        # (optional) transformation of coordinate 
        xt, yt = self.t_fn (x,y)

        # pen style
        color = QColor(color) if color else QColor(self.COLOR_NORMAL)
        pen = pg.mkPen (color, style=style)   

        # brush style 
        brushColor = QColor(brushColor) if brushColor else color 
        brushColor.setAlphaF (brushAlpha)
        brush = pg.mkBrush(brushColor) 
        p = pg.ScatterPlotItem  ([xt], [yt], symbol=symbol, size=size, pxMode=pxMode, 
                                 pen=pen, brush=brush)
        p.setZValue(3)                                      # move to foreground 

        # plot label as TextItem 

        if text is not None: 
            color = QColor(textColor) if textColor else QColor(self.COLOR_NORMAL)
            anchor = anchor if anchor else (0, 1)
            t = pg.TextItem(text, color, anchor=anchor)
            t.setZValue(3)                                      # move to foreground 
            # ? attach to parent doesn't work (because of PlotDataItem? )
            textPos = textPos if textPos is not None else (xt,yt)
            t.setPos (*textPos)

            self._add (t)

        return self._add(p) 




    def _plot_text (self, text : str, color=None, fontSize=None, 
                          parentPos = (0.5,0.5),    # pos within PlotItem 
                          itemPos = (0,1),          # box anchor of TextItem 
                          offset = (0,0)            # offet in px 
                          ):
        """ plot text label at fixed position using LabelItem """

        if not text: return 

        fontSize = fontSize if fontSize is not None else self.SIZE_NORMAL
        color = color if color is not None else self.COLOR_NORMAL

        label = pg.LabelItem(text, color=QColor(color), size=f"{fontSize}pt")    

        # addItem to PlotItem doesn't work (would be added to viewbox and scaled)     
        label.setParentItem(self._pi)
        label.anchor(itemPos=itemPos, parentPos=parentPos, offset=offset)
        label.setZValue(5)

        # manuel add to self items 
        self._plots.append(label)


    def _remove_plots (self):
        """ remove self plots from GraphicsView """

        p : pg.PlotDataItem
        for p in self._plots:

            if isinstance (p, pg.LabelItem):
                # in case of LabelItem, p is added directly to the scene via setParentItem
                self._pi.scene().removeItem (p)
            else: 
                # normal case - p is an item of PlotItem 
                self._pi.removeItem (p)

        self._plots = []


    def _add_legend_items (self):
        """ add legend items of self """
        if self._pi.legend is not None:
            p : pg.PlotDataItem
            for p in self._plots:
                if isinstance (p, pg.PlotDataItem) :
                    name = p.name()
                    if name:
                        self._pi.legend.addItem (p, name)


    def _remove_legend_items (self):
        """ removes legend items of self """
        if self._pi.legend is not None:
            for p in self._plots:
                if isinstance (p, pg.PlotDataItem):
                    self._pi.legend.removeItem (p)

            # ... try to rebuild layout of legend because of strange spacing 
            # legend_ncol = self._pi.legend.columnCount
            # self._pi.legend.setColumnCount (legend_ncol+1)
            # self._pi.legend.setColumnCount (legend_ncol)


    def _refresh_plots (self):
        """ set new x,y data into plots"""
        # can be overloaded for high speed refresh 

        self.plot()             # default - normal plot 


    def _add(self, aPlot: pg.PlotDataItem, name = None):
        """ 
        Add new plot item to self plots
            name: ... of item in legend  
        """

        self._pi.addItem (aPlot)
        self._plots.append(aPlot)

        # 'manual' control if aPlot should appear in legend 
        if self.show_legend and name and isinstance (aPlot, pg.PlotDataItem) : 

            # avoid dublicates in legend
            label_exists = False
            for legend_item in self._pi.legend.items:
                label_item = legend_item [1]
                if label_item.text == name:
                    label_exists = True
                    break

            if not label_exists:                             
                self._pi.legend.addItem (aPlot, name)
                aPlot.opts['name'] = name
 
        return aPlot 