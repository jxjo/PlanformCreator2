#!/usr/bin/env pythonupper
# -*- coding: utf-8 -*-

"""  

The "Artists" to plot a airfoil object on a pg.PlotItem 

"""

from base.math_util             import derivative1
from base.artist                import *
from base.common_utils          import *
from base.spline                import Bezier

from wing                       import Wing, Planform, Reference_Line, Planform_Bezier 
from wing                       import Planform_2, Norm_Planform, Norm_Chord_Bezier
from wing                       import WingSections, WingSection
from wing                       import Flaps, Flap
from model.airfoil              import Airfoil, GEO_BASIC

from PyQt6.QtGui                import QColor, QBrush, QPen, QTransform
from PyQt6.QtCore               import pyqtSignal, QObject

import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


# -------- helper functions ------------------------


COLOR_PLANFORM = QColor ('whitesmoke')
COLOR_LE       = QColor ('whitesmoke')
COLOR_TE       = QColor ('coral')

COLOR_BOX       = QColor ('dodgerblue')

COLOR_CHORD    = QColor ('paleturquoise')
COLOR_REF_LINE = QColor ('springgreen')
COLOR_BANANA   = QColor ('khaki')
COLOR_REF_ELLI = QColor ('dodgerblue')
COLOR_SECTION  = QColor ('deeppink')



# -------- Abstract super class ------------------------

class Abstract_Wing_Artist (Artist):
    """ Superclass for all artists proding access the wing obejcts and handle scaling """

    LEFT    = "Left"                                # show left wing
    RIGHT   = "Right"                               # show right wing

    def __init__ (self, *args, wing_half=None, show_mouse_helper=None, **kwargs):

        self._first_time  = True                        # to show welcome message 
        self._wing_half   = wing_half

        if show_mouse_helper is not None: 
            self.show_mouse_helper = show_mouse_helper

        super().__init__ (*args, **kwargs)


    @property
    def wing (self) -> Wing: return self.data_object

    @property
    def planform (self) -> Planform: return self.wing.planform

    @property
    def wingSections (self) -> WingSections: return self.wing.wingSections

    @property
    def flaps (self) -> Flaps: return self.wing.flaps

    @property
    def wing_mode (self) -> bool:
        """ does artist plot half of a complete wing"""
        return self._wing_half is not None


    def _create_transform (self) -> QTransform:
        """ create Transform object for flip and move depending on wing half and fuselage"""
        t = None
        half_fuse = self.wing.fuselage_width / 2
        if self._wing_half == self.RIGHT:
            t = QTransform()
            t.translate (half_fuse,0) 
        elif self._wing_half == self.LEFT:
            t = QTransform.fromScale(-1, 1)             # flip around y-axis
            t.translate (half_fuse,0) 
        return t


    @override
    def plot (self):
        """the artist will (re)plot - existing plots will be deleted 
        """
        # overridden to perform transformation of half wing to wing 
        if self.show:

            super().plot()

            if self.wing_mode:  

                # create transform object for flip and move 
                transform = self._create_transform ()

                if transform:
                    item : pg.PlotItem
                    for item in self._plots:
                        # do transform 
                        item.setTransform (transform)



# -------- concrete sub classes ------------------------


class Planform_Artist_2 (Artist):
    """Plot the planform contour  """

    sig_planform_changed     = pyqtSignal()                 # planform data changed 

    def __init__ (self, *args, show_ref_line=False, **kwargs):

        self._show_ref_line = show_ref_line             # show reference line 

        super().__init__ (*args, **kwargs)

    @property
    def planform (self) -> Planform_2: return self.data_object

    @property
    def show_ref_line (self) -> bool:
        """ show ref line in planform """
        return self._show_ref_line
    
    def set_show_ref_line (self, aBool: bool):
        """ set show ref line"""
        self._show_ref_line = aBool == True
        self.refresh()

    @property
    def wing_mode (self) -> bool:
        """ does artist plot half of a complete wing"""
        return False # self._wing_half is not None


    def _plot (self): 
    
        if self.wing_mode:
            if self._wing_half == self.RIGHT:
                self._plot_title (self.wing.name, subTitle="created by Jochen",  offset=(-80,0) )
        else:
            self._plot_title ("Planform", subTitle="self.planform.style",  offset=(-20,-30) )

        # plot planform le and te 

        x, le_y, te_y = self.planform.le_te_polyline ()

        color = COLOR_PLANFORM
        pen   = pg.mkPen(color, width=2)
        self._plot_dataItem  (x, le_y, name=f"{self.planform}", pen = pen, antialias = True, zValue=2)

        pen   = pg.mkPen(COLOR_TE, width=2)
        self._plot_dataItem  (x, te_y, name=f"{self.planform}", pen = pen, antialias = True, zValue=2)


        if self.show_ref_line:

            # reference line 

            x, y = self.planform.ref_polyline()
            pen   = pg.mkPen(COLOR_REF_LINE, width=2)
            self._plot_dataItem  (x, y, name="Reference Line", pen = pen, antialias = True, zValue=3)

            # movable points of ref line  

            if self.show_mouse_helper:
                # pt = self.Movable_Ref_Root (self._pi, self.planform, movable=True, color=COLOR_REF_LINE,
                #                             on_changed=self.sig_planform_changed.emit)
                # self._add (pt) 
                # pt = self.Movable_Ref_Tip  (self._pi, self.planform, movable=True, color=COLOR_REF_LINE, 
                #                             on_changed=self.sig_planform_changed.emit)
                # self._add (pt) 
                # pt = self.Movable_Ref_Angle (self._pi, self.planform, movable=True, color=COLOR_REF_LINE,
                #                             on_changed=self.sig_planform_changed.emit)
                # self._add (pt) 
                pass



    class Movable_Ref_Abstract (Movable_Point):
        """ Abstract: a point of the reference line or planform to change. """
        name = "Reference Point"

        def __init__ (self, pi : pg.PlotItem, planform : Planform_2, 
                    movable = False, show_ref_line = True, **kwargs):

            self._pi = pi
            self._planform = planform
            self._tmp_planform_item = None 
            self._tmp_ref_line_item = None 
            self._show_ref_line = show_ref_line

            super().__init__(self._point_xy(), movable=movable, 
                             symbol='s', size=8, show_label_static = movable,**kwargs)


        def _point_xy (self) -> tuple: 
            """ x,y coordinates of point """
            raise NotImplementedError


        def _plot_tmp_planform (self):
            """ create or update temp planform dashed outline """

            x, y = self._planform.polygon()

            if self._tmp_planform_item is None:
                pen = pg.mkPen(QColor(COLOR_PLANFORM).darker(150), width=1, style=Qt.PenStyle.DashLine)
                p = pg.PlotDataItem  (x, y, pen = pen, antialias = False)
                p.setZValue (2)
                self._pi.addItem (p)
                self._tmp_planform_item = p
            else:
                self._tmp_planform_item.setData (x,y)


        def _plot_tmp_ref_line (self):
            """ create or update temp dashed reference line """

            if self._show_ref_line:
                x,y = self._planform.ref_polyline()

                if self._tmp_ref_line_item is None:
                    pen = pg.mkPen(QColor(COLOR_REF_LINE).darker(100), width=1, style=Qt.PenStyle.DashLine)
                    p = pg.PlotDataItem  (x, y, pen = pen, antialias = False)
                    p.setZValue (2)

                    self._pi.addItem (p)
                    self._tmp_ref_line_item = p
                else:
                    self._tmp_ref_line_item.setData (x,y)


        @override
        def _finished (self, _):
            """ slot - point moving is finished"""

            if self._tmp_planform_item is not None:
                self._pi.removeItem (self._tmp_planform_item)
                self._tmp_planform_item = None 
            if self._tmp_ref_line_item is not None:
                self._pi.removeItem (self._tmp_ref_line_item)
                self._tmp_ref_line_item = None 

            self._changed()



    class Movable_Ref_Root (Movable_Ref_Abstract):
        """ root point of the reference to change """

        name = "@ Root"

        @override
        def _point_xy (self) -> tuple: 
            """ x,y coordinates of ref point at root """
            x = 0.0
            y = self._ref_line.x_root
            return x,y            


        @override
        def _moving (self, _):
            """ self is moved"""
            self._ref_line.set_x_root (self.y)                  # update ref line angle
            self.setPos(self._point_xy())                       # update point position if we run against limits 

            self._plot_tmp_ref_line()                           # update tmp reference line                
            self._plot_tmp_planform()                           # and planform plot item as dashed line


        @override
        def label_moving (self):
            """ label text during move"""
            return f"{self.name} {self._ref_line.xn_root:.1%} "



    class Movable_Ref_Tip (Movable_Ref_Abstract):
        """ 
        Represents tip point of the reference to change. 
        """

        name = "@ Tip"

        def __init__ (self, *args, **kwargs):
            super().__init__(*args,**kwargs)

            self._xn_tip_org = self._ref_line.xn_tip
            
        @override
        def _point_xy (self) -> tuple: 
            """ x,y coordinates of ref point at tip"""
            y = self._ref_line.x_at_tip
            x = self._planform.halfwingspan
            return x,y            


        @override
        def _moving (self, _):
            """ self is moved"""

            # update point position if we run against limits 
            x_le, x_te = self._planform.planform_at_tip
            point_y = max (x_le, self.y)
            point_y = min (x_te, point_y)
            point_x = self._planform.halfwingspan 
            self.setPos((point_x, point_y))  

            # update x_tip (will change angle) and xn_tip of reference line at the same time 
            self._ref_line.set_x_tip (point_y)                
            xn_tip = (point_y - x_le) / (x_te - x_le)
            self._ref_line.set_xn_tip (xn_tip)

            # update tmp reference line and planform plot item as dashed line
            self._plot_tmp_ref_line()                  
            self._plot_tmp_planform()


        @override
        def label_moving (self):
            """ label text during move"""
            return f"{self.name} {self._ref_line.xn_tip:.1%} "



    class Movable_Banana_Bezier (Movable_Bezier):
        """
        pg.PlotCurveItem/UIGraphicsItem which represents 
        the Bezier of the banana fucntion. 
        The Bezier curve which can be changed by the controllpoints
        
        Points are implemented with Moveable_Points
        A Moveable_Point can also be fixed ( movable=False).
        See pg.TargetItem for all arguments 
        """
        def __init__ (self, pi : pg.PlotItem, ref_line : Reference_Line, **kwargs):

            self._pi = pi
            self._ref_line = ref_line

            super().__init__(ref_line.banana_as_jpoints (), **kwargs)


        @override
        @property
        def u (self) -> list:
            """ the Bezier parameter """
            return self._ref_line._banana_u

        @override
        def _finished_point (self, aPoint):
            """ slot - point move is finished """

            # write back control points into original bezier 
            self._ref_line.banana_from_jpoints (self._jpoints)

            super()._finished_point (aPoint)




class Planform_Box_Artist (Artist):
    """
    Plot box arround planform contour
    Modify chord_root and span 
    """

    sig_planform_changed     = pyqtSignal()                 # planform data changed 

    @property
    def planform (self) -> Planform_2: return self.data_object

    def _plot (self): 
    
        x, y = self.planform.box_polygon ()

        color = COLOR_BOX
        pen   = pg.mkPen(color, width=1, style=Qt.PenStyle.DotLine)

        self._plot_dataItem  (x, y, pen = pen, antialias = False, zValue=1)

        # movable root chord and tip  

        if self.show_mouse_helper:
            pt = self.Movable_Chord_Root    (self._pi, self.planform, color=color,
                                            on_changed=self.sig_planform_changed.emit)
            self._add (pt) 
            pt = self.Movable_Span          (self._pi, self.planform, color=color, 
                                            on_changed=self.sig_planform_changed.emit)
            self._add (pt) 
            pt = self.Movable_Angle         (self._pi, self.planform, color=color, 
                                            on_changed=self.sig_planform_changed.emit)
            self._add (pt) 



    class Movable_Box_Abstract (Movable_Point):
        """ 
        Abstract: A point of box to change planform. 
        """
        name = "Box Point"

        def __init__ (self, pi : pg.PlotItem, planform : Planform_2, 
                      movable = True, **kwargs):

            self._pi = pi
            self._planform = planform

            self._tmp_box_item = None 
            self._tmp_planform_item = None 

            super().__init__(self._point_xy(), movable=movable, label_anchor = (0,0),
                             symbol='s', size=8, show_label_static = movable,**kwargs)


        def _point_xy (self) -> tuple: 
            """ x,y coordinates of point """
            raise NotImplementedError

        @override
        def _moving (self):
            """ self is moved"""
            # override by subclass to set planform
            self.setPos(self._point_xy())                   # update point position              
            self._plot_tmp_box()                            # update tmp lines                 
            self._plot_tmp_planform()


        def _plot_tmp_planform (self):
            """ create or update temp planform dashed outline """

            x, y = self._planform.polygon()

            if self._tmp_planform_item is None:
                pen = pg.mkPen(QColor(COLOR_PLANFORM).darker(150), width=1, style=Qt.PenStyle.DashLine)
                p = pg.PlotDataItem  (x, y, pen = pen, antialias = False)
                p.setZValue (2)

                self._pi.addItem (p)
                self._tmp_planform_item = p
            else:
                self._tmp_planform_item.setData (x,y)


        def _plot_tmp_box (self):
            """ create or update temp dashed reference line """

            x,y = self._planform.box_polygon()

            if self._tmp_box_item is None:
                pen = pg.mkPen(QColor(COLOR_BOX).darker(100), width=1, style=Qt.PenStyle.DashLine)
                p = pg.PlotDataItem  (x, y, pen = pen, antialias = False)
                p.setZValue (2)

                self._pi.addItem (p)
                self._tmp_box_item = p
            else:
                self._tmp_box_item.setData (x,y)


        @override
        def _finished (self, _):
            """ slot - point moving is finished"""

            if self._tmp_box_item is not None:
                self._pi.removeItem (self._tmp_box_item)
                self._tmp_box_item = None 
            if self._tmp_planform_item is not None:
                self._pi.removeItem (self._tmp_planform_item)
                self._tmp_planform_item = None 

            self._changed()



    class Movable_Angle (Movable_Box_Abstract):
        """ Point of the Box to change the angle. """
        name = "Angle"

        @override
        def _point_xy (self) -> tuple: 
            """ x,y coordinates of ref point at tip """
            # upper right corner of box 
            box_x, box_y = self._planform.box_polygon()
            x, y = box_x[2], box_y[2]
            return x, y         

        @override
        def _moving (self, _):
            """ self is moved"""
            angle = np.arctan (self.y /self.x) * 180 / np.pi
            self._planform.set_sweep_angle (angle)          # update chord of planform 
            super()._moving ()


        @override
        def label_moving (self):
            """ label text during move"""
            return f"{self.name}  {self._planform.sweep_angle:.2f}°"


    class Movable_Chord_Root (Movable_Box_Abstract):
        """ Root point of planform to change chord """
        name = "Root Chord"

        @override
        def _point_xy (self) -> tuple: 
            """ x,y coordinates of ref point at root """
            return 0.0, self._planform.chord_root           

        @override
        def _moving (self, _):
            """ self is moved"""
            self._planform.set_chord_root (self.y)          # update chord of planform 
            super()._moving ()

        @override
        def label_moving (self):
            """ label text during move"""
            return f"{self.name} {self._planform.chord_root:.1f}mm "


    class Movable_Span (Movable_Box_Abstract):
        """ Tip point of planform to change span"""
        name = "Half Span"

        @override
        def _point_xy (self) -> tuple: 
            """ x,y coordinates of ref point at tip """
            # middle point of right line
            box_x, box_y = self._planform.box_polygon()
            x = box_x[2]
            y = (box_y[2] + box_y[3]) / 2
            return x, y         

        @override
        def _moving (self, _):
            """ self is moved"""
            self._planform.set_span (self.x)                # update span of planform 
            super()._moving ()

        @override
        def label_moving (self):
            """ label text during move"""
            return f"{self.name} {self._planform.span:.1f}mm "





class Planform_Artist (Abstract_Wing_Artist):
    """Plot the planform contour  """

    sig_planform_changed     = pyqtSignal()                 # planform data changed 

    def __init__ (self, *args, show_ref_line=False, **kwargs):

        self._welcome_text = None                       # a HTML welcome text 
        self._first_time  = True                        # to show welcome message 
        self._show_ref_line = show_ref_line             # show reference line 

        super().__init__ (*args, **kwargs)


    @property
    def show_ref_line (self) -> bool:
        """ show ref line in planform """
        return self._show_ref_line
    
    def set_show_ref_line (self, aBool: bool):
        """ set show ref line"""
        self._show_ref_line = aBool == True
        self.refresh()


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


    def _plot (self): 
    
        if self._first_time and self._welcome_text is not None:
            self._plot_text (self._welcome_text, color=QColor(self.COLOR_LEGEND), # fontSize=self.SIZE_NORMAL, 
                                parentPos=(0.05,0.), itemPos=(0.0,0), offset=(30,10))
            self._first_time = False
        else: 
            if self.wing_mode:
                if self._wing_half == self.RIGHT:
                    self._plot_title (self.wing.name, subTitle="created by Jochen",  offset=(-80,0) )
            else:
                self._plot_title ("Planform", subTitle=self.planform.style,  offset=(-20,-30) )

        # plot planform contour

        color = QColor(COLOR_PLANFORM)
        pen   = pg.mkPen(color, width=2)
        y, x = self.planform.polygon()
        self._plot_dataItem  (y, x, name=f"{self.wing.name}", pen = pen, antialias = True, zValue=2)


        # two little markers to identify straight tip 

        x_le, x_te = self.planform.planform_at_tip
        y          = self.planform.halfwingspan
        color_marker = QColor(COLOR_PLANFORM).darker(150)
        self._plot_point (y,x_le,symbol="_", color=color_marker, size=7)
        self._plot_point (y,x_te,symbol="_", color=color_marker, size=7)

        # movable root chord and tip  

        if self.show_mouse_helper:
            pt = self.Movable_Root_Chord (self._pi, self.planform, 
                                        movable=True, show_ref_line=self.show_ref_line, color=color,
                                        on_changed=self.sig_planform_changed.emit)
            self._add (pt) 
            pt = self.Movable_Tip_Span (self._pi, self.planform, 
                                        movable=True, show_ref_line=self.show_ref_line, color=color, 
                                        on_changed=self.sig_planform_changed.emit)
            self._add (pt) 


        if self.show_ref_line:

            # reference line 

            ref_line = self.planform.reference_line
            x,y   = ref_line.polyline()
            pen   = pg.mkPen(COLOR_REF_LINE, width=2)
            self._plot_dataItem  (x, y, name=ref_line.name, pen = pen, antialias = True, zValue=3)

            # movable points of ref line  

            if self.show_mouse_helper:
                pt = self.Movable_Ref_Root (self._pi, self.planform, movable=True, color=COLOR_REF_LINE,
                                            on_changed=self.sig_planform_changed.emit)
                self._add (pt) 
                pt = self.Movable_Ref_Tip  (self._pi, self.planform, movable=True, color=COLOR_REF_LINE, 
                                            on_changed=self.sig_planform_changed.emit)
                self._add (pt) 
                pt = self.Movable_Ref_Angle (self._pi, self.planform, movable=True, color=COLOR_REF_LINE,
                                            on_changed=self.sig_planform_changed.emit)
                self._add (pt) 

            # banana line - not in wing mode 

            if not self.wing_mode:
                x,y   = ref_line.banana_polyline()
                color = COLOR_BANANA  
                pen   = pg.mkPen(color, width=1)
                self._plot_dataItem  (x, y, name='Banana Bezier', pen = pen, antialias = False, zValue=3)

            # movable Banana Bezier curve   

            if self.show_mouse_helper and self.planform.style == "Bezier":

                pt = self.Movable_Banana_Bezier (self._pi, ref_line, movable=True, color=color.darker(120),
                                            on_changed=self.sig_planform_changed.emit)
                self._add (pt) 



    class Movable_Abstract (Movable_Point):
        """ 
        Abstract: Represents a point of the reference line or planform to change. 
        """
        name = "Reference Point"

        def __init__ (self,
                    pi       : pg.PlotItem, 
                    planform : Planform, 
                    movable = False, 
                    show_ref_line = True, 
                    **kwargs):

            self._pi = pi
            self._planform = planform
            self._tmp_planform_item = None 
            self._tmp_ref_line_item = None 
            self._show_ref_line = show_ref_line

            self._ref_line = planform.reference_line

            super().__init__(self._point_xy(), movable=movable, 
                             symbol='s', size=8, show_label_static = movable,**kwargs)


        def _point_xy (self) -> tuple: 
            """ x,y coordinates of point """
            raise NotImplementedError


        def _plot_tmp_planform (self):
            """ create or update temp planform dashed outline """

            x, y = self._planform.polygon()

            if self._tmp_planform_item is None:

                pen = pg.mkPen(QColor(COLOR_PLANFORM).darker(150), width=1, style=Qt.PenStyle.DashLine)
                p = pg.PlotDataItem  (x, y, pen = pen, antialias = False)
                p.setZValue (2)

                self._pi.addItem (p)
                self._tmp_planform_item = p

            else:

                self._tmp_planform_item.setData (x,y)


        def _remove_tmp_planform (self):
            """ remove temp planform dashed outline """
            if self._tmp_planform_item is not None:
                self._pi.removeItem (self._tmp_planform_item)
                self._tmp_planform_item = None 


        def _plot_tmp_ref_line (self):
            """ create or update temp dashed reference line """

            if self._show_ref_line:
                x,y = self._ref_line.polyline()

                if self._tmp_ref_line_item is None:
                    pen = pg.mkPen(QColor(COLOR_REF_LINE).darker(100), width=1, style=Qt.PenStyle.DashLine)
                    p = pg.PlotDataItem  (x, y, pen = pen, antialias = False)
                    p.setZValue (2)

                    self._pi.addItem (p)
                    self._tmp_ref_line_item = p

                else:
                    self._tmp_ref_line_item.setData (x,y)


        def _remove_tmp_ref_line (self):
            """ remove temp planform dashed outline """
            if self._tmp_ref_line_item is not None:
                self._pi.removeItem (self._tmp_ref_line_item)
                self._tmp_ref_line_item = None 


        @override
        def _finished (self, _):
            """ slot - point moving is finished"""
            self._remove_tmp_planform ()
            self._remove_tmp_ref_line ()
            self._changed()



    class Movable_Ref_Angle (Movable_Abstract):
        """ 
        Represents a point of the reference to change the angle. 
        """

        name = "Angle"

        @override
        def _point_xy (self) -> tuple: 
            """ x,y coordinates of angle point """
            x = self._ref_line._halfwingspan / 5                    # constant x pos
            y = self._ref_line.x_at (x)                             # y pos depends on angle 
            return x,y 


        @override
        def _moving (self, _):
            """ self is moved"""
            
            self._ref_line.set_angle_by_point (self.x,self.y)       # update angle of reference line of planform 

            self.setPos(self._point_xy())                           # update point position if we run against limits           

            self._plot_tmp_ref_line()                               # update tmp reference line item as dashed line             
            self._plot_tmp_planform()                               # update tmp planform plot item as dashed line


        @override
        def label_moving (self):
            """ label text during move"""
            return f"{self.name}  {self._ref_line.angle:.2f}°"



    class Movable_Ref_Root (Movable_Abstract):
        """ 
        Represents root point of the reference to change. 
        """

        name = "@ Root"

        @override
        def _point_xy (self) -> tuple: 
            """ x,y coordinates of ref point at root """
            x = 0.0
            y = self._ref_line.x_root
            return x,y            


        @override
        def _moving (self, _):
            """ self is moved"""
            self._ref_line.set_x_root (self.y)                  # update ref line angle
            self.setPos(self._point_xy())                       # update point position if we run against limits 

            self._plot_tmp_ref_line()                           # update tmp reference line                
            self._plot_tmp_planform()                           # and planform plot item as dashed line


        @override
        def label_moving (self):
            """ label text during move"""
            return f"{self.name} {self._ref_line.xn_root:.1%} "



    class Movable_Ref_Tip (Movable_Abstract):
        """ 
        Represents tip point of the reference to change. 
        """

        name = "@ Tip"

        def __init__ (self, *args, **kwargs):
            super().__init__(*args,**kwargs)

            self._xn_tip_org = self._ref_line.xn_tip
            
        @override
        def _point_xy (self) -> tuple: 
            """ x,y coordinates of ref point at tip"""
            y = self._ref_line.x_at_tip
            x = self._planform.halfwingspan
            return x,y            


        @override
        def _moving (self, _):
            """ self is moved"""

            # update point position if we run against limits 
            x_le, x_te = self._planform.planform_at_tip
            point_y = max (x_le, self.y)
            point_y = min (x_te, point_y)
            point_x = self._planform.halfwingspan 
            self.setPos((point_x, point_y))  

            # update x_tip (will change angle) and xn_tip of reference line at the same time 
            self._ref_line.set_x_tip (point_y)                
            xn_tip = (point_y - x_le) / (x_te - x_le)
            self._ref_line.set_xn_tip (xn_tip)

            # update tmp reference line and planform plot item as dashed line
            self._plot_tmp_ref_line()                  
            self._plot_tmp_planform()


        @override
        def label_moving (self):
            """ label text during move"""
            return f"{self.name} {self._ref_line.xn_tip:.1%} "




    class Movable_Root_Chord (Movable_Abstract):
        """ 
        Represents root point of planform to change chord. 
        """
        name = "Root Chord"

        def __init__ (self, *args, **kwargs):
            super().__init__ (*args, label_anchor = (0,0), **kwargs)

        @override
        def _point_xy (self) -> tuple: 
            """ x,y coordinates of ref point at root """
            return 0.0, self._planform.chord_root           


        @override
        def _moving (self, _):
            """ self is moved"""
            # update chord of planform 
            self._planform.set_chord_root (self.y)                  # update ref line a

            # update point position if we run against limits 
            self.setPos(self._point_xy())                  

            # update tmp reference line and planform plot item as dashed line
            self._plot_tmp_ref_line()                  
            self._plot_tmp_planform()


        @override
        def label_moving (self):
            """ label text during move"""
            return f"{self.name} {self._planform.chord_root:.1f}mm "



    class Movable_Tip_Span (Movable_Abstract):
        """ 
        Represents root point of planform to change chord. 
        """
        name = "Span"

        def __init__ (self, *args, **kwargs):
            super().__init__ (*args, label_anchor = (0,0), **kwargs)


        @override
        def _point_xy (self) -> tuple: 
            """ x,y coordinates of ref point at root """

            _, x_te = self._planform.planform_at_tip
            return self._planform.halfwingspan, x_te           


        @override
        def _moving (self, _):
            """ self is moved"""
            # update chord of planform 
            x_le, x_te = self._planform.planform_at_tip
            # self._planform.set_chord_tip (self.y - x_le)                # update chord at tip
            self._planform.set_halfwingspan (self.x)                    # update halfwingspan

            # update point position if we run against limits 
            self.setPos(self._point_xy())                  

            # update tmp reference line and planform plot item as dashed line
            self._plot_tmp_ref_line()                  
            self._plot_tmp_planform()


        @override
        def label_moving (self):
            """ label text during move"""
            return f"{self.name} {self._planform.halfwingspan:.1f}mm "




    class Movable_Banana_Bezier (Movable_Bezier):
        """
        pg.PlotCurveItem/UIGraphicsItem which represents 
        the Bezier of the banana fucntion. 
        The Bezier curve which can be changed by the controllpoints
        
        Points are implemented with Moveable_Points
        A Moveable_Point can also be fixed ( movable=False).
        See pg.TargetItem for all arguments 
        """
        def __init__ (self, pi : pg.PlotItem, ref_line : Reference_Line, **kwargs):

            self._pi = pi
            self._ref_line = ref_line

            super().__init__(ref_line.banana_as_jpoints (), **kwargs)


        @override
        @property
        def u (self) -> list:
            """ the Bezier parameter """
            return self._ref_line._banana_u

        @override
        def _finished_point (self, aPoint):
            """ slot - point move is finished """

            # write back control points into original bezier 
            self._ref_line.banana_from_jpoints (self._jpoints)

            super()._finished_point (aPoint)



class Chord_Artist (Abstract_Wing_Artist):
    """Plot the chord distribution  """

    sig_planform_changed     = pyqtSignal()                 # planform data changed 

    def _plot (self): 
    
        self._plot_title ("Chord Distribution", subTitle=None, offset=(-20,-30))
        label = "Normed Chord Distribution"

        color = COLOR_CHORD
        pen   = pg.mkPen(color, width=2)
        brush = pg.mkBrush (color.darker (600))

        yn, xn = self.planform.norm_chord_line () 
        y = yn * self.planform.halfwingspan
        self._plot_dataItem  (y, xn, name=label, pen = pen, antialias = True, 
                              fillLevel=0.0, fillBrush=brush,  zValue=1)

        if self.show_mouse_helper and self.planform.style == "Bezier":
 
            # movable Bezier curve   
            pt = self.Movable_Chord_Bezier (self._pi, self.planform, 
                                        movable=True, color=COLOR_CHORD,
                                        on_changed=self.sig_planform_changed.emit)
            self._add (pt) 


    class Movable_Chord_Bezier (Movable_Bezier):
        """
        pg.PlotCurveItem/UIGraphicsItem which represents 
        a Bezier based chord distribution. 
        The Bezier curve which can be changed by the controllpoints
        """
        def __init__ (self, pi : pg.PlotItem, planform : Planform_Bezier, **kwargs):

            self._pi = pi
            self._planform = planform

            super().__init__(planform.bezier_as_jpoints (), **kwargs)

        @override
        @property
        def u (self) -> list:
            """ the Bezier parameter """
            return self._planform._norm_u_points()

        @override
        def _finished_point (self, aPoint):
            """ slot - point move is finished """

            # write back control points into original bezier 
            self._planform.bezier_from_jpoints (self._jpoints)

            super()._finished_point (aPoint)



class Norm_Chord_Artist (Artist):
    """Plot the chord distribution  """

    sig_planform_changed     = pyqtSignal()                 # planform data changed 

    @property
    def planform (self) -> Planform_2: return self.data_object

    def _plot (self): 
    
        self._plot_title ("Chord", subTitle=None, offset=(30,-30))
        label = "Chord Distribution"

        color = COLOR_CHORD
        pen   = pg.mkPen(color, width=2)
        brush = pg.mkBrush (color.darker (600))

        xn, yn = self.planform.normed.cn_polyline () 
        # y = yn * self.planform.span
        self._plot_dataItem  (xn, yn, name=label, pen = pen, antialias = True, 
                              fillLevel=0.0, fillBrush=brush,  zValue=1)

        if self.show_mouse_helper and self.planform.normed.isBezier:
 
            # movable Bezier curve   
            pt = self.Movable_Chord_Bezier (self._pi, self.planform.normed._norm_chord, 
                                        movable=True, color=COLOR_CHORD,
                                        on_changed=self.sig_planform_changed.emit)
            self._add (pt) 


    class Movable_Chord_Bezier (Movable_Bezier):
        """
        pg.PlotCurveItem/UIGraphicsItem which represents 
        a Bezier based chord distribution. 
        The Bezier curve which can be changed by the controllpoints
        """
        def __init__ (self, pi : pg.PlotItem, norm_chord : Norm_Chord_Bezier, **kwargs):

            self._pi = pi
            self._norm_chord = norm_chord

            super().__init__(norm_chord.bezier_as_jpoints (), **kwargs)


        @override
        @property
        def u (self) -> list:
            """ the Bezier u parameter """
            return np.linspace(0.0, 1.0, num=50)

        @override
        def _finished_point (self, aPoint):
            """ slot - point move is finished """

            # write back control points into original bezier 
            self._norm_chord.bezier_from_jpoints (self._jpoints)

            super()._finished_point (aPoint)



class Norm_Ref_Line_Artist (Abstract_Wing_Artist):
    """Plot the normed reference line """

    def _plot (self): 
    
        x,y = self.planform.norm_ref_line ()
        color = QColor (COLOR_REF_LINE).darker (150)
        label = self.planform.reference_line.name

        pen = pg.mkPen(color, width=1, style=Qt.PenStyle.DashLine)

        self._plot_dataItem  (x, y, name=label, pen = pen, antialias = False, zValue=2)



class Norm_Ref_Line_Artist (Abstract_Wing_Artist):
    """Plot the reference line within chord distribution"""

    sig_planform_changed     = pyqtSignal()                 # planform data changed 

    @property
    def planform (self) -> Planform_2: return self.data_object

    def _plot (self): 

        self._plot_title ("Reference Line", subTitle=None, offset=(30,-30))
        label = "Reference Line"

        xn,yn = self.planform.normed.rn_polyline ()

        color = QColor (COLOR_REF_LINE) 
        pen = pg.mkPen(color, width=2)

        ref_item = self._plot_dataItem  (xn, yn, name=label, pen = pen, antialias = True, zValue=2)

        # plot dummy le and te and fill between re line 

        le_x, le_y  = [0.0, 1.0], [0.0, 0.0]
        te_x, te_y  = [0.0, 1.0], [1.0, 1.0]

        pen = pg.mkPen(COLOR_LE, width=2)
        le_item = self._plot_dataItem  (le_x, le_y, pen=pen)
        pen = pg.mkPen(COLOR_TE, width=2)
        te_item = self._plot_dataItem  (te_x, te_y, pen=pen)

        brush = pg.mkBrush (COLOR_LE.darker(800))
        p = pg.FillBetweenItem (ref_item, le_item, brush=brush )
        self._add(p)

        brush = pg.mkBrush (COLOR_TE.darker(500))
        p = pg.FillBetweenItem (ref_item, te_item, brush=brush )
        self._add(p)

        # mouse helper to change ref line Bezier 
        if self.show_mouse_helper and self.planform.normed.isBezier:
 
            # movable Bezier curve   
            pt = self.Movable_Ref_Line_Bezier (self._pi, self.planform.normed, 
                                        movable=True, color=COLOR_REF_LINE,
                                        on_changed=self.sig_planform_changed.emit)
            self._add (pt) 



    class Movable_Ref_Line_Bezier (Movable_Bezier):
        """
        pg.PlotCurveItem/UIGraphicsItem which represents 
        a Bezier based reference line. 
        The Bezier curve which can be changed by the control points
        """
        def __init__ (self, pi : pg.PlotItem, norm_planform : Norm_Planform, **kwargs):

            self._pi = pi
            self._norm_planform = norm_planform

            super().__init__(norm_planform.ref_bezier_as_jpoints (), **kwargs)


        @override
        @property
        def u (self) -> list:
            """ the Bezier u parameter """
            return np.linspace(0.0, 1.0, num=50)

        @override
        def _finished_point (self, aPoint):
            """ slot - point move is finished - write back control points"""
            self._norm_planform.ref_bezier_from_jpoints (self._jpoints)
            super()._finished_point (aPoint)





class Ref_Planform_Artist (Artist):
    """Plot the planform contour  """

    def __init__ (self, *args, show_ref_line=False, **kwargs):

        self._show_ref_line = show_ref_line             # show reference line 

        super().__init__ (*args, **kwargs)

    @property
    def ref_planforms (self) -> list[Planform]:
        """ list of reference planforms"""
        return self.data_list


    @property
    def show_ref_line (self) -> bool:
        """ show ref line in planform """
        return self._show_ref_line
    
    def set_show_ref_line (self, aBool: bool):
        """ set show ref line"""
        self._show_ref_line = aBool == True
        self.refresh()


    def _plot (self): 
    
        planform : Planform 
        for planform in self.ref_planforms:

            label = f"{planform.style}"

            color = COLOR_REF_ELLI
            width = 1
            pen   = pg.mkPen(color, width=width)

            y, x = planform.polygon()
            self._plot_dataItem  (y, x, name=label, pen = pen, antialias = False, zValue=1)




class Ref_Chord_Artist (Artist):
    """Plot the chord distribution of reference planforms"""

    @property
    def ref_planforms (self) -> list[Planform]:
        """ list of reference planforms"""
        return self.data_list

    def _plot (self): 
    
        planform : Planform 
        for planform in self.ref_planforms:

            color = COLOR_REF_ELLI
            pen   = pg.mkPen(color, width=1)

            yn, xn = planform.norm_chord_line () 
            y = yn * planform.halfwingspan
            self._plot_dataItem  (y, xn, name=f"{planform.style}", pen = pen, antialias = False, zValue=1)




class WingSections_Artist (Abstract_Wing_Artist):
    """Plot the wing sections of a planform """

    sig_cur_wingSection     = pyqtSignal (WingSection)          # new current wingSection 
    sig_wingSection_new     = pyqtSignal (WingSection)          # new wingsection inserted 
    sig_wingSection_changed = pyqtSignal ()                     # wingsection data changed 


    def __init__ (self, *args, cur_wingSection_fn=None, norm_chord=False, **kwargs):

        self._cur_wingSection_fn = cur_wingSection_fn           # current wingSection bound method 
        self._norm_chord = norm_chord                           # plot chord as normed

        super().__init__ (*args, **kwargs)

    @property
    def norm_chord (self) -> bool:
        """ true - sections will be plotted with normed chord"""
        return self._norm_chord

    @property
    def cur_wingSection (self) -> WingSection | None: 
        """ returns the current, selected wing section """
        if self._cur_wingSection_fn:
            return self._cur_wingSection_fn()
        else: 
            return None
    

    def _plot (self): 

        color = COLOR_SECTION
        name  = "Wing Sections"                                     # to show in legend

        section : WingSection
        for section in self.wingSections:

            pen   = pg.mkPen(color, width=1)

            if self.norm_chord:
                x,y = section.norm_line ()
            else: 
                x,y = section.line()  

            p = self._plot_dataItem  (x, y,  name=name, pen = pen, antialias = False, zValue=3)

            # plot section name in planform view 

            if not self.wing_mode:
                point_xy = (x[0],y[0]) if self.norm_chord else (x[0],y[0])  # point at le
                anchor   = (0.5,1.5) if section.isTip else (0.5,1.2)        # label anchor 

                self._plot_point (point_xy, color=color, size=0, text=section.name_short, textColor=color, anchor=anchor)

                # highlight current section - add movable points 

                if section == self.cur_wingSection and not (section.isRoot or section.isTip):
                    p.setShadowPen (pg.mkPen(QColor(COLOR_SECTION).darker(150), width=4))


                    if self.show_mouse_helper:
                        # add movable points for move by pos and move by chord 

                        pt = self.Movable_Section (self._pi, section, norm_chord=self.norm_chord, 
                                                move_by_pos=True, name="Move by pos",
                                                movable=True, color=COLOR_SECTION,
                                                on_changed=self.sig_wingSection_changed.emit)
                        self._add (pt) 
                        pt = self.Movable_Section (self._pi, section, norm_chord=self.norm_chord, 
                                                move_by_pos=False, name="Move by chord",
                                                movable=True, color=COLOR_SECTION,
                                                on_changed=self.sig_wingSection_changed.emit)
                        self._add (pt) 

                # make line clickable 

                p.setCurveClickable (True, width=8)
                p.sigClicked.connect (self._section_item_clicked)

                name = None                                             # legend entry only for frst item 

        if not self.wing_mode:
            # make scene cklickable to add wing section 
            #   delayed as during init scene is not yet available
            QTimer().singleShot (10, self._connect_scene_mouseClick)


    def _section_item_clicked (self, item : pg.PlotDataItem,  ev : MouseClickEvent):
        """ a wing section item was clicked - select or delete (shift-click) it"""
    
        section = self.wingSections.at_y (item.xData[0])

        # shift-click - delete section 

        if (ev.modifiers() & QtCore.Qt.KeyboardModifier.ShiftModifier): 
            
            new_current = self.wingSections.delete (section) 
            if new_current: 
                QTimer().singleShot(10, lambda: self.sig_cur_wingSection.emit (section))


        # normal click - select section 

        else: 
            # callback / emit signal delayed so we leave the scope of Graphics
            #  -> refresh will delete items ...
            QTimer().singleShot(10, lambda: self.sig_cur_wingSection.emit (section))     


    def _connect_scene_mouseClick (self): 
        """ connect mouse click in scene to slot"""           

        scene : pg.GraphicsScene = self._pi.scene()
        if scene:  scene.sigMouseClicked.connect (self._scene_clicked)

    @override
    def _remove_plots(self):
        """ overloaded to disconnect older slot when doing refresh"""

        super()._remove_plots()

        scene : pg.GraphicsScene = self._pi.scene()
        if scene:
            try:                            # slot could be not connected up to now
                scene.sigMouseClicked.disconnect (self._scene_clicked)
            except:
                pass


    def _scene_clicked (self, ev : MouseClickEvent):
        """ 
        slot - mouse click in scene of self - handle add wing section with crtl-click 
        """ 

        # handle only ctrl-click
        if not (ev.modifiers() & QtCore.Qt.KeyboardModifier.ControlModifier): return  
       
        # get scene coordinates of click pos and map to view box 

        viewbox : pg.ViewBox = ev.currentItem
        if viewbox == self._pi.vb:                     # is this my view box (or of another plot item)? 
            pos : pg.Point = viewbox.mapSceneToView(ev.scenePos())

            # create new section  and signal ...

            section = self.wing.wingSections.create_at (pos.x())

            if section: 
                QTimer().singleShot(10, lambda: self.sig_wingSection_new.emit (section))     



    class Movable_Section (Movable_Point):
        """ 
        Represents a point of the section line to move either by pos or by chord. 
        """
        name = "Section Point"

        def __init__ (self,
                    pi       : pg.PlotItem, 
                    section : WingSection, 
                    movable = False, 
                    move_by_pos = True,                     # either by pos or by chord '
                    norm_chord = False,                     # either chord distribution or planform 
                    **kwargs):

            self._pi = pi
            self._section = section
            self._tmp_section_item = None 
            self._move_by_pos = move_by_pos
            self._norm_chord = norm_chord     

            self._in_move = False                    # move recursion detection          

            super().__init__(self._point_xy(), movable=movable, 
                             symbol='s', size=8, show_label_static = movable,**kwargs)
            
            # set static brush depending if point to indicate current section dependancy ...

            if (move_by_pos and section.has_fixed_chord) or (not move_by_pos and section.has_fixed_y):
                self.setBrush (QColor("black"))


        def _point_xy (self) -> tuple: 
            """ x,y coordinates of point """

            if self._norm_chord:
                x,y = self._section.norm_line ()
            else:
                x,y = self._section.line()      

            if self._move_by_pos:
                point_xy = (x[0],y[1])                              # point at te 
            else:
                point_xy = (x[0],y[0])                              # point at 0 
                
            return point_xy


        @override
        def _moving (self, _):
            """ self is moved"""

            # as a corrected new point position is set, which will emit a new move signal 
            #   a recursion has to be avoided 
            if self._in_move: return 
            self._in_move = True 

            # check if mouse point is between neighbour sections
            y = self.x
            y_left, y_right = self._section.y_limits()
            y = max (y_left,  y) 
            y = min (y_right, y)

            # update section depending 

            if self._move_by_pos:
                self._section.set_y (y)                     # update section line of  
            else:
                planform = self._section.wing.planform        
                if self._norm_chord:
                    norm_chord = planform.norm_chord_at (y)
                    self._section.set_norm_chord (norm_chord)
                else:
                    chord = planform.chord_at (y)
                    self._section.set_chord (chord)

            # update point position if we run against limits 

            self.setPos(self._point_xy())                  

            # create or update temp section line

            self._plot_tmp_section_line()

            self._in_move = False


        def _plot_tmp_section_line (self):
            """ create or update temp dashed reference line """

            if self._norm_chord:
                x,y = self._section.norm_line ()
            else:
                x,y = self._section.line()      

            if self._tmp_section_item is None:
                pen = pg.mkPen(QColor(COLOR_SECTION).darker(100), width=1, style=Qt.PenStyle.DashLine)
                p = pg.PlotDataItem  (x, y, pen = pen, antialias = False, zValue=2)
                p.setZValue (2)

                self._pi.addItem (p)
                self._tmp_section_item = p

            else:
                self._tmp_section_item.setData (x,y)


        def _remove_tmp_section_line (self):
            """ remove temp planform dashed outline """
            if self._tmp_section_item is not None:
                self._pi.removeItem (self._tmp_section_item)
                self._tmp_section_item = None 


        @override
        def label_static (self) -> str:
            """ the static label depending on fixed pos or chord"""

            if self._move_by_pos:
                if self._section.has_fixed_y:
                    return f"Fixed by pos"
                else:
                    return f"Flex pos"
            else:
                if self._section.has_fixed_chord:
                    return f"Fixed by chord"
                else:
                    return f"Flex chord"        


        @override
        def label_moving (self):
            """ label text during move"""
            if self._move_by_pos:
                lab = f"pos {self._section.y:.1f}mm"
            else:
                if self._norm_chord:
                    lab = f"chord {self._section.norm_chord:.1%}"
                else:
                    lab = f"chord {self._section.chord:.1f}mm"
            return lab

        @override
        def _finished (self, _):
            """ slot - point moving is finished"""
            self._remove_tmp_section_line ()
            self._changed()




class Flaps_Artist (Abstract_Wing_Artist):
    """Plot the flaps in planform """

    sig_flaps_changed     = pyqtSignal()                 # planform data changed 

    def __init__ (self, *args, rel_depth=False, **kwargs):

        self._rel_depth = rel_depth                           # plot chord as normed
        super().__init__ (*args, **kwargs)


    @property
    def rel_depth (self) -> bool:
        """ true - relative depth 0..1 will be plotted"""
        return self._rel_depth


    def _plot (self): 

        flaps = self.wing.flaps.get()
        if not flaps: return 

        colors = random_colors (len(flaps))

        color : QColor = colors[0]
        pen   = pg.mkPen(color, width=2)

        if self.rel_depth:

            # plot relative depth distribution in chord distribution 

            brush_color = color.darker(300)
            brush_color.setAlphaF (0.4)
            brush = pg.mkBrush (brush_color)         
            x,y = self.wing.flaps.rel_depth_polyline ()  
            self._plot_dataItem  (x, y,  pen = pen, antialias = True, name="Relative Flap Depth",
                                    fillLevel=0.0, fillBrush=brush,  zValue=3)
        else:

            # plot single flaps 

            self._plot_flaps (flaps, colors) 

        # moving points at hinge of flaps 

        if self.show_mouse_helper and not self.flaps.hinge_equal_ref_line:
            p = self.Movable_Hinge (self._pi, self.wing.flaps, color=color, rel_depth=self.rel_depth,
                                            movable=True, on_changed=self.sig_flaps_changed.emit)
            self._add(p) 
                


    def _plot_flaps (self, flaps, colors):
        """ plot the single flaps"""

        if self.rel_depth: return                   # not implemented

        flap : Flap
        for i, flap in enumerate (flaps):

            color : QColor = colors[i]
            pen   = pg.mkPen(color, width=1)

            # flap contour left and right side 
            x,y = flap.line_left(y_offset=0.5)  
            self._plot_dataItem  (x, y,  pen = pen, antialias = False, zValue=1)
            x,y = flap.line_right(y_offset=0.5)  
            self._plot_dataItem  (x, y,  pen = pen, antialias = False, zValue=1)

            # hinge and te 
            x,y = flap.line_hinge(y_offset=0.5)  
            p1 = self._plot_dataItem  (x, y,  pen = pen, antialias = False, zValue=1)

            x,y = flap.line_te(y_offset=0.5)  
            p2 = self._plot_dataItem  (x, y,  pen = pen, antialias = False, zValue=1)

            # fill area between hinge and te 
            brush = pg.mkBrush (color.darker(400))
            p = pg.FillBetweenItem (p1, p2, brush=brush)
            self._add (p)

            # plot flap group in the middle of the flap - not in 'wing mode'
            if not self.wing_mode:
                self._plot_point (flap.center(), color=color, size=0, text=flap.name, textColor=color, anchor=(0.5,0.5))



    class Movable_Hinge (pg.PlotDataItem):
        """
        pg.PlotCurveItem/UIGraphicsItem which represents 
        the hinge poyline with its points at wing sections 
        
        Points are implemented with Movable_Points
        """

        class Movable_Hinge_Point (Movable_Point):
            """ Represents hinge movable point """

            def __init__ (self, *args, flaps : Flaps = None, rel_depth=False, **kwargs):
                self._rel_depth = rel_depth
                self._flaps = flaps 
                super().__init__ (*args, **kwargs) 


            @override
            def label_moving (self):
                """ label text during move"""
                if self._rel_depth:
                    rel_depth  = self.y
                    return f"{self.name} {rel_depth:.1%}"
                else:
                    hinge_x    = self.y
                    le_x, te_x = self._flaps._planform._planform_at (self.x)
                    depth      = te_x - hinge_x
                    depth_norm = (te_x - hinge_x) / (te_x - le_x)
                    return f"{self.name} {depth:.1f}mm {depth_norm:.1%}"



        def __init__ (self, 
                    pi : pg.PlotItem, 
                    flaps : Flaps,
                    rel_depth = False,  
                    id = None, 
                    color = None, 
                    movable = False,
                    label_anchor = (0,1),
                    on_changed = None, 
                    **kwargs):

            self._pi = pi
            self._flaps = flaps
            self._rel_depth = rel_depth
            self._callback_changed = on_changed
            self._id = id 
            self.movable = movable 

            # Control jpoints  
            if self._rel_depth:
                self._jpoints : list[JPoint] = self._flaps.rel_depth_as_jpoints ()
            else:
                self._jpoints : list[JPoint] = self._flaps.hinge_as_jpoints ()

            # init polyline of control points as PlotCurveItem and hide 
            pen = pg.mkPen (color, width=1, style=Qt.PenStyle.DotLine)
            
            super().__init__(*self.jpoints_xy(), pen=pen)

            if movable:
                self.setZValue (10)                                 # movable dotted line above other objects 
            else: 
                self.setZValue (5)
           
            self.setCurveClickable (True, width=8)                  # make clickable to add new point 
            self.sigClicked.connect (self._hinge_item_clicked)

            # init control points as Movable_Points 

            for i, jpoint in enumerate (self._jpoints):

                p = self.Movable_Hinge_Point (jpoint, flaps=flaps, rel_depth=rel_depth, id = i, parent=self, 
                                              movable=movable, 
                                              color=color, symbol='s', size=7, label_anchor=label_anchor, **kwargs) 
                
                p.sigPositionChanged.connect        (self._moving_point)
                p.sigPositionChangeFinished.connect (self._finished_point)
                p.sigShiftClick.connect             (self._delete_point)


        @property
        def id (self):
            """ returns id of self """
            return self._id 
    

        def jpoints_xy (self) -> tuple[list]:
            """returns coordinates of self_jpoints as x, y lists """
            x, y = [], []
            for p in self._jpoints:
                x.append(p.x)
                y.append(p.y)
            return x, y


        def _moving_point (self, aPoint : Movable_Point):
            """ slot - point is moved by mouse """
            jpoint : JPoint =  self._jpoints[aPoint.id]
            jpoint.set_y(aPoint.y)                                  # update self point list 

            aPoint.setPos (*jpoint.xy)                              # update Movable point 
            self.setData (*self.jpoints_xy())                       # update self (polyline) 
            self.show()


        def _delete_point (self, aPoint : Movable_Point):
            """ slot - point should be deleted """

            # a minimum of 3 control points 
            if len(self._jpoints) <= 3: return   

            # remove from list
            i = aPoint.id
            del self._jpoints[i]                                # update self point list 
            self.setData (*self.jpoints_xy())                   # update self (polyline) 

            self._finished_point (aPoint)
            if aPoint.scene():                                  # sometimes scene get lost ... (?) 
                aPoint.scene().removeItem(aPoint)               # final delete from scene 


        def _hinge_item_clicked (self, item, ev : MouseClickEvent):
            """ slot - hinge item clicked - try to insert a new point  """

            # handle only ctrl-click
            if not (ev.modifiers() & QtCore.Qt.KeyboardModifier.ControlModifier): return  

            # get y-position on the hinge line 
            viewbox : pg.ViewBox = self.getViewBox ()               
            pos : pg.Point = viewbox.mapSceneToView(ev.scenePos())
            y = pos.x()

            insert_ok = self._flaps.insert_hinge_point_at (y) 

            if insert_ok:
                # if successful, leave self directly without updatung points 
                ev.accept()
                if callable(self._callback_changed):
                    QTimer().singleShot(10, lambda: self._callback_changed())



        def _finished_point (self, aPoint):
            """ slot - point move is finished """
            
            self.hide()
            if self._rel_depth:
                self._flaps.rel_depth_from_jpoints (self._jpoints)
            else: 
                self._flaps.hinge_from_jpoints (self._jpoints)

            if callable(self._callback_changed):
                QTimer().singleShot(10, lambda: self._callback_changed())




class Airfoil_Artist (Abstract_Wing_Artist):
    """Plot the airfoils of a planform """

    def __init__ (self, *args, show_strak=False, real_size=False, mini_mode=False,**kwargs):

        self._show_strak    = show_strak                    # show also straked airfoils 
        self._real_size     = real_size                     # plot airfoils in real size
        self._show_thick    = False                         # show max thickness
        self._mini_mode     = mini_mode                     # mini mode for overview 
        super().__init__ (*args, **kwargs)


    @property
    def show_strak (self) -> bool:
        """ true - show also straked airfoils """
        return self._show_strak
    def set_show_strak (self, aBool : bool):
        self._show_strak = aBool == True
        self.refresh()

    @property
    def real_size (self) -> bool:
        """ true - plot airfoils in real size """
        return self._real_size
    def set_real_size (self, aBool : bool):
        self._real_size = aBool == True
        self.refresh()


    @property
    def show_thick (self) -> bool:
        """ true - plotmax thickness """
        return self._show_thick
    def set_show_thick (self, aBool : bool):
        self._show_thick = aBool == True
        self.refresh()


    def _plot (self): 

        if self._mini_mode:
            self._plot_title ("Airfoils", offset=(-20,+20) )
        else:
            sub = "Real Size" if self.real_size else ""
            self._plot_title ("Airfoils", subTitle=sub,  offset=(-20,-30) )

        # plot airfoil of wing Sections 

        colors = random_colors (len(self.wingSections))

        # strak airfoils if needed
        if self.show_strak:
            self.wingSections.do_strak (geometry=GEO_BASIC)

        section : WingSection
        for i, section in enumerate (self.wingSections):
            airfoil = section.airfoil
            if (airfoil.isLoaded) and not (not self.show_strak and airfoil.isBlendAirfoil):

                # plot contour 

                if self.real_size:
                    le_x = section.line ()[1][0]
                    x = airfoil.x * section.chord + le_x 
                    y = airfoil.y * section.chord 
                else:
                    x = airfoil.x
                    y = airfoil.y

                color : QColor = colors[i]
                if self._mini_mode:
                    color = color.darker(130)  
                    label = f"{airfoil.name}"
                else:    
                    label = f"{airfoil.name} @ {section.name}"   
                pen = pg.mkPen(color, width=1.5)
                antialias = True

                self._plot_dataItem  (x, y, name=label, pen = pen, antialias = antialias, zValue=2)
                
                # plot thickness highpoint with max thickness line 

                if self.show_thick:
                    x,t = airfoil.geo.thickness.highpoint.xy
                    y_u = airfoil.geo.upper.yFn (x)                               # thickness highpoint on upper line 
                    y_l = airfoil.geo.lower.yFn (x)                               # thickness highpoint on lower line 
                    if self.real_size:
                        x   = x *   section.chord + le_x 
                        y_u = y_u * section.chord 
                        y_l = y_l * section.chord 
                        t   = t *   section.chord
                        label = f"{t:.1f}mm @ {x:.0f}mm"
                    else: 
                        label = f"{t:.1%} @ {x:.0%}"

                    self._plot_point (x,y_u, symbol='+', color=color, text=label)

                    line_x = [x,x]
                    line_y = [y_l, y_u]
                    pen = pg.mkPen(color.darker(150), width=1)

                    self._plot_dataItem  (line_x, line_y, pen = pen, antialias = False, zValue=1)






                

