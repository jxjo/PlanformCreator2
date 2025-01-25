#!/usr/bin/env pythonupper
# -*- coding: utf-8 -*-

"""  

The "Artists" to plot a airfoil object on a pg.PlotItem 

"""
from enum                       import Enum

from base.math_util             import interpolate
from base.artist                import *
from base.common_utils          import *

from wing                       import Wing
from wing                       import Planform, N_Distrib_Bezier
from wing                       import WingSection, WingSections 
from wing                       import Flaps, Flap, Image_Definition
from model.airfoil              import Airfoil, GEO_BASIC
from model.polar_set            import *

from PyQt6.QtGui                import QColor, QImage, QBrush, QPen, QTransform
from PyQt6.QtCore               import pyqtSignal

import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


# -------- Colors ------------------------


COLOR_PLANFORM = QColor ('whitesmoke')
COLOR_LE       = QColor ('whitesmoke')
COLOR_TE       = QColor ('coral')

COLOR_BOX      = QColor ('dodgerblue')

COLOR_CHORD    = QColor ('paleturquoise')
COLOR_REF_LINE = QColor ('springgreen')
COLOR_BANANA   = QColor ('khaki')
COLOR_SECTION  = QColor ('deeppink')

COLOR_REF_ELLI = QColor ('dodgerblue')
COLOR_REF_PC2  = QColor ('darkorchid')

COLOR_WARNING  = QColor ('gold')

# -------- coordinate systems ------------------------


class mode (Enum):
    """ enums for coordinate system an artist is working"""

    DEFAULT       = 0                       # no transformation, use default coordinates of artist

    NORM_NORM     = 1                       # x = 0..1      y = 0..1
    NORM_TO_SPAN  = 2                       # x = 0..span   y = 0..1

    PLANFORM_NORM = 3                       # x = 0..1      y = 0..1
    NORM_TO_PLANFORM = 4                    # x = 0..span   y = 0..chord

    WING_LEFT     = 5                       # x = -fuse..-span  y = chord
    WING_RIGHT    = 6                       # x = +fuse..+span  y = chord

    REF_TO_NORM   = 7                       # x = 0..1      y = 0..1 relative within reference 
    REF_TO_SPAN   = 8                       # x = 0..span   y = 0..1 relative within reference 

    REF_TO_PLAN   = 9                       # x = 0..1      y = 0..1 relative within reference 



# -------- Abstract super class ------------------------

class Abstract_Artist_Planform (Artist):
    """ 
    Superclass for planform artists providing
        - access planform objects and handle scaling 
        - common change signals

    """

    sig_planform_changed     = pyqtSignal ()                    # planform data changed 
    sig_wingSection_selected = pyqtSignal (WingSection)       # new current wingSection 
    sig_wingSection_new      = pyqtSignal (WingSection)       # new wingsection inserted 
    sig_wingSection_changed  = pyqtSignal ()                    # wingsection data changed 
    sig_flaps_changed        = pyqtSignal ()                    # flaps hinge line changed


    def __init__ (self, *args, 
                  mode = mode.DEFAULT,                          # coordinate system shall work 
                  **kwargs):

        self._mode  = mode

        super().__init__ (*args, **kwargs)


    @override
    def plot (self):
        """the artist will (re)plot - existing plots will be deleted """

        # override to add coordinate transformations prior to _plot 

        if self._mode == mode.NORM_TO_PLANFORM:  
            self.set_t_fn  (self.planform.t_norm_to_plan)                   # bound method for coordinate transformation
            self.set_tr_fn (self.planform.t_plan_to_norm)                    

        elif self._mode == mode.NORM_TO_SPAN:  
            self.set_t_fn  (self.planform.t_norm_to_span)                   # scale x to span
            self.set_tr_fn (self.planform.t_span_to_norm)            

        elif self._mode == mode.REF_TO_SPAN:  
            self.set_t_fn  (self.planform.t_norm_to_span)                   # scale x to span
            self.set_tr_fn (self.planform.t_span_to_norm)            

        elif self._mode == mode.REF_TO_PLAN:  
            self.set_t_fn  (self.planform.t_ref_to_plan)                    # scale x,y to plan
            self.set_tr_fn (self.planform.t_plan_to_ref)            

        elif self._mode == mode.WING_RIGHT:
            self.set_t_fn  (self.wing.t_plan_to_wing_right)                 # scale span, chord, add fuselage

        elif self._mode == mode.WING_LEFT:
            self.set_t_fn  (self.wing.t_plan_to_wing_left)                  # scale span, chord, add fuselage

        super().plot()


    @property
    def planform (self) -> Planform: 
        return self.data_object

    @property
    def wing (self) -> Wing: 
        return self.planform.wing
    
    @property
    def wingSections (self) -> WingSections:
        return self.planform.wingSections


# -------- concrete sub classes ------------------------



class Ref_Line_Artist (Abstract_Artist_Planform):
    """
    Plot the reference line either
        - mode PLANFORM
    """

    def _plot (self): 

        if self._mode == mode.WING_LEFT or self._mode == mode.WING_RIGHT:
            x, y = self.planform.ref_polyline()
        else:
            x, y = self.planform.n_ref_line.polyline ()

        pen   = pg.mkPen(COLOR_REF_LINE, width=1.5)
        self._plot_dataItem  (x, y, name="Reference Line", pen = pen, antialias = True, zValue=4)

        # mouse helper to change ref line Bezier 

        if self.show_mouse_helper:
 
            # movable Bezier curve   
            pt = self.Movable_Ref_Line_Bezier (self._pi, self.planform, 
                                        t_fn = self.t_fn, tr_fn = self.tr_fn, 
                                        movable=True, color=COLOR_REF_LINE,
                                        on_changed=self.sig_planform_changed.emit)
            self._add (pt) 

            if self.planform.n_ref_line.is_straight_line():
                msg = "Reference line: ctrl-click to add point for 'banana'"
            else: 
                msg = "Reference line: Move Bezier control point to modify, shift-click to remove 'banana' point"
            self.set_help_message (msg)



    class Movable_Ref_Line_Bezier (Movable_Bezier):
        """
        pg.PlotCurveItem representing a Bezier based reference line. 

        The Bezier curve can be changed by the control points.
            - either 2 or 3 points 
            - add/delete 3rd point 
        """

        class Movable_Ref_Line_Point (Movable_Point):
            """ 
            Represents one control point of Movable_Ref_Line_Bezier
                - subclassed to get individual label 
            """

            def __init__ (self, *args, **kwargs):
                super().__init__ (*args, **kwargs)

                self._show_label_static = not self._jpoint.fixed               # label only if movable (banana)


            @override
            def label_static (self, *_) -> str:
                """ the static label - can be overloaded """
                if not self._jpoint.fixed:
                    return f"{self.name}" 
                else:
                    return None

            @override
            def label_moving (self,x,y):
                """ nice label for hover and moving """

                if self._jpoint.fixed:
                    return ""
                else: 
                    return f"{self.name} {y:.1f}mm at {x:.1f}mm"



        def __init__ (self, pi : pg.PlotItem, 
                      planform : Planform,
                      t_fn = None, tr_fn = None,                                # transformation
                      **kwargs):

            self._pi = pi
            self._planform = planform

            self._t_fn, self._tr_fn  = t_fn, tr_fn

            # scale x-coordinate of Bezier jpoints depending on mode 
            jpoints = self._planform.n_ref_line.bezier_as_jpoints ()
            jpoints = JPoint.transform (jpoints, transform_fn = self._t_fn)

            super().__init__(jpoints, movable_point_class=self.Movable_Ref_Line_Point, **kwargs)


        @override
        @property
        def u (self) -> list:
            """ the Bezier u parameter """
            return np.linspace(0.0, 1.0, num=25)            # just a few points for speed 


        @override
        def _add_point (self, xy : tuple):
            """ handle add point - will be called when ctrl_click on Bezier """

            # only a third point may be added 
            if len(self._jpoints) != 2: return   

            # sanity - not too close to end points
            if xy[0] > (self._jpoints[1].x * 0.1) and xy[0] < (self._jpoints[1].x * 0.9):

                self._jpoints.insert (1, JPoint (xy))

                logger.debug (f"Bezier point added at x={xy[0]} y={xy[1]}")

                self._finished_point (None)

                return True
            else:
                return False 
            

        @override
        def _finished_point (self, aPoint):
            """ slot - point move is finished - write back control points"""

            jpoints = JPoint.transform (self._jpoints, transform_fn = self._tr_fn)
            self._planform.n_ref_line.bezier_from_jpoints (jpoints)

            super()._finished_point (aPoint)






class Planform_Box_Artist (Abstract_Artist_Planform):
    """ 
    Plot box arround planform contour (only if show_mouse_helper)
        - Modify chord_root and span 
        - mode PLANFORM
    """

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

            self.set_help_message ("Planform: Use control points of the enclosing box to modify root chord, span and sweep angle")


    class Movable_Box_Point (Movable_Point):
        """ 
        Abstract: A point of box to change planform. 
        """
        name = "Box Point"

        def __init__ (self, pi : pg.PlotItem, 
                      planform : Planform, 
                      movable = True, 
                      **kwargs):

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

            x,y = self._planform.polygon()

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



    class Movable_Angle (Movable_Box_Point):
        """ Point of the Box to change the angle. """
        name = "Angle"

        @override
        def _point_xy (self) -> tuple:  
            box_x, box_y = self._planform.box_polygon()         # upper right corner of box 
            return box_x[1], box_y[1]      

        @override
        def _moving (self, _):
            angle = np.arctan (self.y /self.x) * 180 / np.pi
            self._planform.set_sweep_angle (angle)              # update chord of planform 
            super()._moving ()

        @override
        def label_moving (self, *_):
            return f"{self.name}  {self._planform.sweep_angle:.2f}°"



    class Movable_Chord_Root (Movable_Box_Point):
        """ Root point of planform to change chord """
        name = "Root Chord"

        @override
        def _point_xy (self) -> tuple: 
            box_x, box_y = self._planform.box_polygon()         # middle point of right line
            return box_x[3],  box_y[3] 

        @override
        def _moving (self, _):
            self._planform.set_chord_root (self.y)              # update chord of planform 
            super()._moving ()

        @override
        def label_moving (self, *_):
            return f"{self.name} {self._planform.chord_root:.1f}mm "



    class Movable_Span (Movable_Box_Point):
        """ Tip point of planform to change span"""
        name = "Half Span"

        @override
        def _point_xy (self) -> tuple: 
            box_x, box_y = self._planform.box_polygon()         # middle point of right line
            return box_x[1], box_y[2]       

        @override
        def _moving (self, _):
            self._planform.set_span (self.x)                # update span of planform 
            super()._moving ()

        @override
        def label_moving (self, *_):
            return f"{self.name} {self._planform.span:.1f}mm "




class Norm_Chord_Artist (Abstract_Artist_Planform):
    """
    Plot the normed chord 
        - mode DEFAULT (normed in span)
        - mode NORM
    """

    
    def _plot (self): 

        if self._mode == mode.NORM_NORM:
            x, yn = self.planform.n_distrib.polyline()
            t_fn, tr_fn = self.t_fn, self.tr_fn
        else: 
            x, yn = self.planform.cn_polyline ()
            t_fn, tr_fn = self.planform.t_norm_to_span, self.planform.t_span_to_norm 
 
        name       = "Chord distribution"
        color      = COLOR_CHORD
        pen, brush = pg.mkPen(color, width=2), pg.mkBrush (color.darker (600))

        self._plot_dataItem  (x, yn, pen = pen, name=name, antialias = True, 
                              fillLevel=0.0, fillBrush=brush,  zValue=1)

        # Chord Bezier curve based - move control points  

        if self.show_mouse_helper and self.planform.n_distrib.isBezier:
 
            pt = self.Movable_Chord_Bezier (self._pi, self.planform,
                                            t_fn = t_fn, tr_fn = tr_fn, 
                                            movable=True, color=color,
                                            on_changed=self.sig_planform_changed.emit)
            self._add (pt) 

            self.set_help_message ("Chord distribution: Move Bezier control points to modify")

        # Chord trapezoidal - defined by wing sections

        elif self.planform.n_distrib.isTrapezoidal:

            pass



    class Movable_Chord_Bezier (Movable_Bezier):
        """
        pg.PlotCurveItem/UIGraphicsItem which represents 
        a Bezier based chord distribution. 
        """
        def __init__ (self, pi : pg.PlotItem, 
                      planform = Planform, 
                      t_fn = None, tr_fn = None,                                # transformation
                      **kwargs):

            self._pi = pi
            self._planform = planform
            self._norm_chord : N_Distrib_Bezier = self._planform.n_distrib
            self._t_fn, self._tr_fn  = t_fn, tr_fn

            jpoints = self._norm_chord.bezier_as_jpoints ()
            jpoints = JPoint.transform (jpoints, transform_fn = self._t_fn)

            super().__init__(jpoints, **kwargs)


        @override
        @property
        def u (self) -> list:
            """ the Bezier u parameter array """
            return np.linspace(0.0, 1.0, num=50)

        @override
        def _finished_point (self, aPoint):
            """ slot - point move is finished """
            # write back control points into original bezier 
            self._norm_chord.bezier_from_jpoints (self._jpoints, transform_fn = self._tr_fn)
            super()._finished_point (aPoint)




class Planform_Artist (Abstract_Artist_Planform):
    """
    Plot the Planform
        - mode NORM_PLANFORM
        - mode PLANFORM
        - mode WING_RIGHT, WING_LET
    """

    def __init__ (self, *args, 
                  mode = mode.DEFAULT,
                  as_contour = False,                               #  planform as outline 
                  **kwargs):

        self._as_contour = as_contour           
        super().__init__ (*args, mode = mode, **kwargs)


    @property 
    def as_contour (self):
        """ show planform as one contour - and not le and te separatly"""
        return self._as_contour


    def _plot (self): 

        # plot planform in a single contour 

        if self.as_contour:

            x, y = self.planform.polygon () 

            self._plot_dataItem  (x, y, pen=pg.mkPen(COLOR_LE, width=1.5), antialias=True, zValue=3,
                                    name=f"{self.planform}")
            
        # plot planform by leading and trailing edge 

        else: 
            x, le_y, te_y = self.planform.le_te_polyline () 

            brush_le, brush_te = None, None
            fillLevel = 0 

            # plot le and te 

            self._plot_dataItem  (x, le_y, pen=pg.mkPen(COLOR_LE, width=2), antialias=True, zValue=3,
                                name=f"Leading edge", fillLevel=fillLevel, fillBrush=brush_le)
            
            self._plot_dataItem  (x, te_y, pen=pg.mkPen(COLOR_TE, width=2), antialias=True, zValue=3,
                                name=f"Trailing edge", fillLevel=fillLevel, fillBrush=brush_te)





class Panelling_Artist (Abstract_Artist_Planform):
    """
    Plot the panels of planform
        - mode DEFAULT
    """    

    def _plot (self): 
    
        planform        = self.wing.planform_paneled
        parent_planform = planform._parent_planform

        # plot le, te of parent  

        x, le_y, te_y = parent_planform.le_te_polyline () 

        pen = pg.mkPen (COLOR_PLANFORM.darker(150), width=1, style=Qt.PenStyle.DashLine)
        self._plot_dataItem  (x, le_y, pen=pen, antialias=False, zValue=1, name=f"Planform")        
        self._plot_dataItem  (x, te_y, pen=pen, antialias=False, zValue=1)

        # plot planform by leading and trailing edge 

        x, le_y, te_y = planform.le_te_polyline () 

        self._plot_dataItem  (x, le_y, pen=pg.mkPen(COLOR_LE, width=1.5), antialias=True, zValue=3,
                            name=f"Paneled Leading edge")        
        self._plot_dataItem  (x, te_y, pen=pg.mkPen(COLOR_TE, width=1.5), antialias=True, zValue=3,
                            name=f"Paneled Trailing edge")

        # plot grid of panel x,y lines 

        x_list, y_list = planform.y_panel_polylines ()

        for i in range (len(x_list)):
            x, y = x_list[i], y_list[i]
            self._plot_dataItem  (x, y, pen=pg.mkPen(COLOR_BOX.darker(150), width=1), antialias=False, zValue=1)        

        x_list, y_list = planform.x_panel_polylines ()

        for i in range (len(x_list)):
            x, y = x_list[i], y_list[i]
            self._plot_dataItem  (x, y, pen=pg.mkPen(COLOR_BOX.darker(150), width=1), antialias=False, zValue=1)        


        # plot vertical lines indicating to much delta between paneled chord and parent chord 

        for line in planform.c_diff_lines ():

            x, y = line[0], line[1]
            color = COLOR_WARNING # .darker(50)
            color.setAlphaF (0.5)
            self._plot_dataItem  (x, y, pen=pg.mkPen(color, width=6), name="Chord difference", antialias=False, zValue=1)        




class Norm_Chord_Ref_Artist (Abstract_Artist_Planform):
    """
    Plot chord reference within chord distribution
        - mode SPAN_REF
        - mode NORM_REF    
    """

    def _plot (self): 

        xn,yn = self.planform.n_chord_ref.polyline ()

        color = QColor (COLOR_REF_LINE) 
        pen = pg.mkPen(color, width=2)

        ref_item = self._plot_dataItem  (xn, yn, name="Reference Line", pen = pen, antialias = True, zValue=2)


        if self._mode == mode.REF_TO_SPAN or self._mode == mode.REF_TO_NORM or self._mode == mode.DEFAULT:

            # plot dummy le and te and fill between ref line 

            le_x, le_y  = np.array([0.0, 1.0]), np.array([0.0, 0.0])
            te_x, te_y  = np.array([0.0, 1.0]), np.array([1.0, 1.0])

            pen = pg.mkPen(COLOR_LE, width=2)
            le_item = self._plot_dataItem  (le_x, le_y, pen=pen, name="Leading edge")
            pen = pg.mkPen(COLOR_TE, width=2)
            te_item = self._plot_dataItem  (te_x, te_y, pen=pen, name="Trailing edge")

            brush = pg.mkBrush (COLOR_LE.darker(800))
            p = pg.FillBetweenItem (ref_item, le_item, brush=brush )
            self._add(p)

            brush = pg.mkBrush (COLOR_TE.darker(500))
            p = pg.FillBetweenItem (ref_item, te_item, brush=brush )
            self._add(p)

        # mouse helper to change ref line Bezier 

        if self.show_mouse_helper:
 
            # movable Bezier curve   
            pt = self.Movable_Ref_Chord_Bezier (self._pi, self.planform, 
                                        t_fn = self.t_fn, tr_fn = self.tr_fn, 
                                        movable=True, color=COLOR_REF_LINE,
                                        on_changed=self.sig_planform_changed.emit)
            self._add (pt) 

            self.set_help_message ("Chord reference: Move control points to modify")



    class Movable_Ref_Chord_Bezier (Movable_Bezier):
        """
        pg.PlotCurveItem representing a Bezier based chord reference. 

        The Bezier curve can be changed by the control points.
            - either 2 or 3 points 
            - add/delete 3rd point 
        """

        class Movable_Ref_Chord_Point (Movable_Point):
            """ 
            Represents one control point of Movable_Ref_Chord_Bezier
                - subclassed to get individual label 
            """

            @override
            def label_moving (self,x,y):
                """ nice label for hover and moving """

                if x == 0.0 or x == 1.0:
                    return f"{self.name} {y:.1%}"
                else: 
                    return f"{self.name} {y:.1%} at {x:.1f}mm"



        def __init__ (self, pi : pg.PlotItem, 
                      planform : Planform,
                      t_fn = None, tr_fn = None,                                # transformation
                      **kwargs):

            self._pi = pi
            self._planform = planform

            self._t_fn, self._tr_fn  = t_fn, tr_fn

            # scale x-coordinate of Bezier jpoints depending on mode 
            jpoints = self._planform.n_chord_ref.bezier_as_jpoints ()
            jpoints = JPoint.transform (jpoints, transform_fn = self._t_fn)

            super().__init__(jpoints, movable_point_class=self.Movable_Ref_Chord_Point, **kwargs)


        @override
        @property
        def u (self) -> list:
            """ the Bezier u parameter """
            return np.linspace(0.0, 1.0, num=25)            # just a few points for speed 


        @override
        def _add_point (self, xy : tuple):
            """ handle add point - will be called when ctrl_click on Bezier """
            return False 
            

        @override
        def _finished_point (self, aPoint):
            """ slot - point move is finished - write back control points"""

            jpoints = JPoint.transform (self._jpoints, transform_fn = self._tr_fn)
            self._planform.n_chord_ref.bezier_from_jpoints (jpoints)

            super()._finished_point (aPoint)




class Ref_Planforms_Artist (Abstract_Artist_Planform):
    """
    Plot the planform contour of reference planforms 
        - mode PLANFORM
    """
    def __init__ (self, *args, 
                  show_chord = False,                               #  planform as outline 
                  **kwargs):

        self._show_chord = show_chord           
        self._show_elliptical = True           
        self._show_ref_pc2    = True

        super().__init__ (*args, **kwargs)

    @property
    def show_elliptical (self) -> bool: return self._show_elliptical

    def set_show_elliptical (self, aBool : bool):
        self._show_elliptical = aBool
        self.refresh()

    @property
    def show_ref_pc2 (self) -> bool: return self._show_ref_pc2

    def set_show_ref_pc2 (self, aBool : bool):
        self._show_ref_pc2 = aBool
        self.refresh()


    @property
    def ref_planforms (self) -> list[Planform]:

        refs = []

        if self.show_elliptical:
            refs.append (self.wing.planform_elliptical)
        if self.wing.planform_ref_pc2 and self.show_ref_pc2:
            refs.append (self.wing.planform_ref_pc2)
        return refs


    def _plot (self): 

        planform : Planform 
        for planform in self.ref_planforms:

            if self._show_chord:
                x, y = planform.cn_polyline () 
            else:
                x, y = planform.polygon () 

            if planform.n_distrib.isElliptical:
                color = COLOR_REF_ELLI
                name  = f"Reference {planform.name}" 
            else:  
                color = COLOR_REF_PC2
                name  = f"Reference {self.wing.planform_ref_pc2_name}"

            pen   = pg.mkPen(color, width=1)

            self._plot_dataItem  (x, y, name=name, pen = pen, antialias = False, zValue=1)




class WingSections_Artist (Abstract_Artist_Planform):
    """
    Plot the wing sections either
        - mode NORM
        - mode SPAN_NORM
        - mode NORM_PLANFORM
        - mode PLANFORM
    A section can be selected (current wing section) and
        - moved by x position or by chord           (not 'defines_cn')
        - moved by x position and defining chord    ('defines_cn') 
    """


    def __init__ (self, *args, wingSection_fn=None, 
                  **kwargs):

        self._wingSection_fn = wingSection_fn           # current wingSection bound method

        super().__init__ (*args, **kwargs)


    @property
    def cur_wingSection (self) -> WingSection | None: 
        """ returns the current, selected wing section """
        if self._wingSection_fn:
            return self._wingSection_fn()
        else: 
            return None

    @override
    def set_show (self, aBool : bool, refresh=True):
        """ overridden to disconnect scene click when not shown"""

        if not aBool:
            self._disconnect_scene_mouseClick ()
        super().set_show (aBool, refresh=refresh) 


    def _plot (self): 

        color = COLOR_SECTION
        m     = self._mode 

        # plot all sections

        section : WingSection

        for section in self.wingSections:

            if   m == mode.NORM_NORM or m == mode.NORM_TO_SPAN:
                x,y = section.line_in_chord ()
            elif m == mode.REF_TO_NORM  or m == mode.REF_TO_SPAN:
                x,y = section.line_in_chord_ref ()
            else: 
                x,y = section.line ()

            if section.defines_cn:
                pen   = pg.mkPen(color, width=1.0)
                name  = "Wing Sections fix"                                     
            else:
                pen   = pg.mkPen(color, width=1.0,style=Qt.PenStyle.DashLine)
                name  = "Wing Sections flex"                                    

            p = self._plot_dataItem  (x, y,  name=name, pen = pen, antialias = False, zValue=3)

            # plot section name in different modes 

            if m == mode.NORM_NORM:
                point_xy = (x[1],y[1])                                          # point at te
                anchor = (-0.2,0.8)
            elif m == mode.REF_TO_NORM or m == mode.REF_TO_SPAN:
                point_xy = (x[0],y[0])                                          # point at le
                anchor = (0.5,1.2)                                              # always constant above 
            elif m == mode.WING_RIGHT and section.is_root:                           
                point_xy = (x[0],y[0])                                          # point at x = 0 
                anchor = (1.0,1.2)                                              # shift left
            elif m == mode.WING_LEFT and section.is_root:                           
                point_xy = None                                                 # skip left side root 
            else:
                point_xy = (x[0],y[0])                                          # point at le
                anchor = (0.5,2.0) if section.is_tip else (0.5,1.2)              

            if point_xy:
                self._plot_point (point_xy, color=color, size=0, text=section.name_short, textColor=color, anchor=anchor)

            # highlight current section - add movable points for move by pos and move by chord 

            if section == self.cur_wingSection:

                p.setShadowPen (pg.mkPen(QColor(COLOR_SECTION).darker(200), width=6))

                if self.show_mouse_helper:

                    # mouse helper for position 

                    movable = not (section.is_root_or_tip)
                    pt = self.Movable_Section_Point (self._pi, self.planform, section, 
                                                mode = m, t_fn = self.t_fn, tr_fn = self.tr_fn,
                                                move_by_pos=True, movable=movable,
                                                on_changed=self.sig_wingSection_changed.emit)
                    self._add (pt) 

                    # mouse helper for chord 
                    
                    if m == mode.REF_TO_SPAN or m == mode.REF_TO_NORM:                  # move doesn't make sense  (yn==1)
                        movable = False
                    else:
                        movable = True and not section.is_root_or_tip \
                                  or (section.defines_cn and section.is_tip)            # define tip chord

                    pt = self.Movable_Section_Point (self._pi, self.planform, section, 
                                                mode = m, t_fn = self.t_fn, tr_fn = self.tr_fn,
                                                move_by_pos=False, movable=movable,
                                                on_changed=self.sig_wingSection_changed.emit)
                    self._add (pt) 

            # make line clickable if there is a callback 
            if self._wingSection_fn:
                p.setCurveClickable (True, width=8)
                p.sigClicked.connect (self._section_item_clicked)


        if self.show_mouse_helper and not (m == mode.WING_LEFT or m == mode.WING_RIGHT):
            # make scene clickable to add wing section 
            #   delayed as during init scene is not yet available
            QTimer().singleShot (10, self._connect_scene_mouseClick)


            msg = "Wing Sections: Click to select, move by position or by chord, shift-click to remove, " + \
                                   "ctrl-click to add a section"
            if self.planform.chord_defined_by_sections:
                msg = msg + ", alt-click to toggle 'defines chord'"
            self.set_help_message (msg)
            


    def _section_item_clicked (self, item : pg.PlotDataItem,  ev : MouseClickEvent):
        """
        wing section item was clicked 
            - select
            - delete (shift-click)
            - select & toggle 'defines_cn' (alt-click)"""
    
        x, y   = item.xData[0], item.yData[0]

        section : WingSection
        found = False
        for section in self.wingSections:
            if round(section.x,4) == round(x,4): 
                found = True
                break 

        # sanity 
        if not found:
            logger.warning (f"Wing section at x={x} could be detected")
            return


        # shift-click - delete section 

        if (ev.modifiers() & QtCore.Qt.KeyboardModifier.ShiftModifier): 
            
            new_current = self.wingSections.delete (section) 
            if new_current: 
                QTimer().singleShot(10, lambda: self.sig_wingSection_selected.emit (section))

        # alt-click - toggle defines chord  

        if (ev.modifiers() & QtCore.Qt.KeyboardModifier.AltModifier): 
            
            toggled = section.set_defines_cn (not section.defines_cn) 
            if toggled: 
                QTimer().singleShot(10, lambda: self.sig_wingSection_selected.emit (section))


        # normal click - select section 

        else: 
            # callback / emit signal delayed so we leave the scope of Graphics
            #  -> refresh will delete items ...
            QTimer().singleShot(10, lambda: self.sig_wingSection_selected.emit (section))     


    def _connect_scene_mouseClick (self): 
        """ connect mouse click in scene to slot"""           

        scene : pg.GraphicsScene = self._pi.scene()
        if scene:  scene.sigMouseClicked.connect (self._scene_clicked)


    def _disconnect_scene_mouseClick (self): 
        """ disconnect mouse click in scene to slot"""           

        scene : pg.GraphicsScene = self._pi.scene()
        if scene:
            try:                            # slot could be not connected up to now
                scene.sigMouseClicked.disconnect (self._scene_clicked)
            except:
                pass


    @override
    def _remove_plots(self):
        """ overloaded to disconnect older slot when doing refresh"""

        super()._remove_plots()
        self._disconnect_scene_mouseClick ()


    def _scene_clicked (self, ev : MouseClickEvent):
        """ 
        slot - mouse click in scene of self - handle add wing section with crtl-click 
        """ 

        # handle only ctrl-click
        if not (ev.modifiers() & QtCore.Qt.KeyboardModifier.ControlModifier): return  
       
        # was the scene click in my viewbox?

        if isinstance(ev.currentItem, pg.ViewBox):
            viewbox : pg.ViewBox = ev.currentItem
        else:
            viewbox : pg.ViewBox = ev.currentItem.getViewBox()

        if viewbox == self._pi.vb:                     # is this my view box (or of another plot item)? 
            
            ev.accept()

            # get scene coordinates of click pos and map to view box 

            pos : pg.Point = viewbox.mapSceneToView(ev.scenePos())

            # create new section  and signal ...
            section = self.wingSections.create_at (pos.x())

            if section: 
                QTimer().singleShot(10, lambda: self.sig_wingSection_new.emit (section))     



    class Movable_Section_Point (Movable_Point):
        """ 
        Represents a point of the section line to move either by pos or by chord. 
        """
        name = "Section Point"

        def __init__ (self,
                    pi       : pg.PlotItem, 
                    planform : Planform,
                    section  : WingSection, 
                    movable = True, 
                    move_by_pos = True,                                     # either by pos or by chord '
                    mode = mode.NORM_TO_PLANFORM,                                   # coordinate system
                    t_fn = None, tr_fn = None,                              # transformation
                    **kwargs):

            self._pi = pi
            self._planform = planform 
            self._section = section
            self._tmp_section_item = None 
            self._tmp_outline_item = None 


            # different modi of a section point 
            self._move_by_pos   = move_by_pos
            self._move_by_chord = not move_by_pos if not section.defines_cn else False
            self._define_chord  = not move_by_pos if     section.defines_cn else False   # trapezoid planform

            self._mode = mode 
            self._t_fn, self._tr_fn  = t_fn, tr_fn

            self._in_move = False                                           # move recursion detection          

            super().__init__(self._point_xy(), movable=movable, color=COLOR_SECTION,
                             symbol='s', size=8, show_label_static = movable,**kwargs)
            
            # set static brush depending if point to indicate current section dependancy ...

            if (move_by_pos and section.is_cn_fix) or (not move_by_pos and section.is_xn_fix):
                self.setBrush (QColor("black"))


        def _line_of_section (self) -> tuple:
            """ poyline of section in display coordinate"""

            if self._mode == mode.NORM_NORM or self._mode == mode.NORM_TO_SPAN:
                xn,yn = self._section.line_in_chord ()
            elif self._mode == mode.REF_TO_NORM or self._mode == mode.REF_TO_SPAN:
                xn,yn = self._section.line_in_chord_ref ()
            else: 
                xn,yn = self._section.line ()

            return self._t_fn (xn,yn)                               # transformed coordinates


        def _point_xy (self) -> tuple: 
            """ x,y coordinates of point """

            x,y = self._line_of_section ()

            if self._move_by_pos:
                point_xy = (x[0],y[1])                              # point at te 
            elif self._move_by_chord or self._define_chord:
                point_xy = (x[0],y[0])                              # point at 0                 
            return point_xy


        @override
        def _moving (self, _):
            """ self is moved"""

            # as a corrected new point position is set, which will emit a new move signal 
            #   a recursion has to be avoided 
            if self._in_move: return 
            self._in_move = True 

            xn, yn = self._tr_fn (self.x, self.y)

            # check if mouse point is between neighbour sections
            x = self.x
            left_x, right_x = self._section.x_limits()
            x = np.clip (x, left_x, right_x)
            y = self.y

            if   self._move_by_pos:
                self._section.set_x (x)                             # update section pos

            elif self._move_by_chord:
                cn = self._planform.cn_at (x)
                self._section.set_cn (cn)                           # update section pos by chord

            elif self._define_chord:
                if self._mode == mode.DEFAULT:
                    _, te_y = self._section.le_te ()
                    c  = te_y - y                                   # calculate new c from trailing edge
                    lower_c, upper_c = self._section.c_limits()     # c shouldn't be more than left neighbour
                    c = np.clip (c, lower_c, upper_c)
                    self._section.set_c (c)                         # update section chord
                else: 
                    cn = yn                                         # yn is chord 
                    lower_cn, upper_cn = self._section.cn_limits()  # cn shouldn't be more than left neighbour
                    cn = np.clip (cn, lower_cn, upper_cn)
                    self._section.set_cn (cn)                       # update section chord


            self.setPos(self._point_xy())                           # update point position if we run against limits  

            self._plot_tmp_section_line()                           # create or update temp section line
            self._plot_tmp_outline ()                               # create or update temp chord polyline
            self._in_move = False


        def _plot_tmp_section_line (self):
            """ create or update temp dashed reference line """

            x,y = self._line_of_section ()

            if self._tmp_section_item is None:
                pen = pg.mkPen(QColor(COLOR_SECTION).darker(100), width=1, style=Qt.PenStyle.DashLine)
                p = pg.PlotDataItem  (x, y, pen = pen, antialias = False, zValue=2)
                p.setZValue (2)
                self._pi.addItem (p)
                self._tmp_section_item = p
            else:
                self._tmp_section_item.setData (x,y)


        def _plot_tmp_outline (self):
            """ create or update temp chord dashed outline if section defines chord """

            if not self._section.defines_cn: return 

            if self._mode == mode.REF_TO_SPAN or self._mode == mode.REF_TO_NORM:
                return                                                  # outline doesn't make sense ...

            if self._mode == mode.NORM_NORM or self._mode == mode.NORM_TO_SPAN:
                x,y = self._planform.cn_polyline()
                pen = pg.mkPen(QColor(COLOR_CHORD).darker(150), width=1, style=Qt.PenStyle.DashLine)
            else:
                x,y = self._planform.polygon()
                pen = pg.mkPen(QColor(COLOR_PLANFORM).darker(150), width=1, style=Qt.PenStyle.DashLine)

            if self._tmp_outline_item is None:
                p = pg.PlotDataItem  (x, y, pen = pen, antialias = False)
                p.setZValue (2)
                self._pi.addItem (p)
                self._tmp_outline_item = p
            else:
                self._tmp_outline_item.setData (x,y)


        @override
        def label_static (self, *_) -> str:
            """ the static label depending on fixed pos or chord"""
            if self._section.is_root:
                return ""
            elif self._section.is_tip:
                if self._planform.chord_defined_by_sections and self._define_chord:
                    return "Flex chord"
                else:
                    return "" 
            elif self._move_by_pos:               
                return "Fixed pos"   if self._section.is_xn_fix else "Flex pos"
            else:
                return "Fixed chord" if self._section.is_cn_fix else "Flex chord"


        @override
        def label_moving (self, *_):
            """ label text during move"""
            if self._section.is_root:
                return ""
            elif self._section.is_tip:
                if self._planform.chord_defined_by_sections and self._define_chord:
                    if self._mode == mode.NORM_NORM or self._mode == mode.NORM_TO_SPAN:
                        lab = f"chord {self._section.cn:.1%}"
                    else:
                        lab = f"chord {self._section.c:.1f}mm"
                else:
                    return "" 
            elif self._move_by_pos:
                lab = f"pos {self._section.x:.1f}mm"
            else:
                if self._mode == mode.NORM_NORM or self._mode == mode.NORM_TO_SPAN:
                    lab = f"chord {self._section.cn:.1%}"
                else:
                    lab = f"chord {self._section.c:.1f}mm"
            return lab


        @override
        def _finished (self, _):
            """ slot - point moving is finished"""

            if self._tmp_section_item is not None:
                self._pi.removeItem (self._tmp_section_item)
                self._tmp_section_item = None 
            if self._tmp_outline_item is not None:
                self._pi.removeItem (self._tmp_outline_item)
                self._tmp_outline_item = None 

            self._changed()




class Flaps_Artist (Abstract_Artist_Planform):
    """
    Plot the flaps in  
        - mode PLANFORM
        - mode NORM_TO_SPAN
        - mode SPAN_REF
        - mode NORM_PLANFORM
    """

    def _plot (self): 

        flaps      = self.planform.flaps
        flaps_list = flaps.get()
        if not flaps_list: return 

        colors = random_colors (len(flaps_list), h_start=3/12)
        color : QColor = colors[0]

        if self._mode == mode.NORM_TO_SPAN :

            # plot depth distribution in chord distribution 

            x,y = flaps.flap_in_chord_polyline ()  

            pen   = pg.mkPen(color, width=1, style=Qt.PenStyle.DashLine)
            brush_color = color.darker(200)
            brush_color.setAlphaF (0.4)
            brush = pg.mkBrush (brush_color)   

            self._plot_dataItem  (x, y,  pen = pen, antialias = True, name="Relative Flap Depth",
                                    fillLevel=0.0, fillBrush=brush,  zValue=3)

        elif self._mode == mode.REF_TO_NORM or self._mode == mode.REF_TO_SPAN:

            # plot relative depth distribution in chord distribution 

            x,y = flaps.flap_cn_polyline ()  

            pen   = pg.mkPen(color, width=1, style=Qt.PenStyle.DashLine)
            brush_color = color.darker(200)
            brush_color.setAlphaF (0.4)
            brush = pg.mkBrush (brush_color)   

            self._plot_dataItem  (x, y,  pen = pen, antialias = True, name="Relative Flap Depth",
                                    fillLevel=1.0, fillBrush=brush,  zValue=3)
        else:

            x,y = flaps.hinge_polyline ()  
            pen   = pg.mkPen(color, width=1.5)
            self._plot_dataItem  (x, y,  pen = pen, antialias=True, name="Hinge line", zValue=3)

            # plot single flaps 

            self._plot_flaps (flaps_list, colors) 

            # moving points at hinge of flaps 

            if self.show_mouse_helper and not flaps.hinge_equal_ref_line:

                p = self.Movable_Hinge  (self._pi, self.planform, color=color,
                                        t_fn = self.t_fn, tr_fn = self.tr_fn, 
                                        movable=True, on_changed=self.sig_flaps_changed.emit)
                self._add(p) 

                self.set_help_message ("Flaps: Move Hinge points to modify hinge line, ctrl-click to add, shift-click to remove hinge point")

                


    def _plot_flaps (self, flap_list : list[Flap], colors : list[QColor]):
        """ plot the single flaps"""

        gap = 0.002

        for i, flap in enumerate (flap_list):

            color : QColor = colors[i]
            pen_1   = pg.mkPen(color            , width=0.8)
            pen_2   = pg.mkPen(color.darker(150), width=0.5)

            # flap contour left and right side 
            x,y = flap.line_left (x_offset=gap)  
            self._plot_dataItem  (x, y,  pen = pen_1, antialias = False, zValue=1)
            x,y = flap.line_right(x_offset=gap)  
            self._plot_dataItem  (x, y,  pen = pen_1, antialias = False, zValue=1)

            # hinge and te 
            x,y = flap.line_hinge (x_offset=gap)  
            p1 = self._plot_dataItem  (x, y,  pen = pen_2, antialias = False, zValue=1)

            x,y = flap.line_te (x_offset=gap)  
            p2 = self._plot_dataItem  (x, y,  pen = pen_2, antialias = False, zValue=1)

            # fill area between hinge and te 
            brush = pg.mkBrush (color.darker(400))
            p = pg.FillBetweenItem (p1, p2, brush=brush)
            self._add (p)                                      

            # plot flap group in the middle of the flap - not in 'wing mode'
            if not (self._mode == mode.WING_LEFT or self._mode == mode.WING_RIGHT):
                x, y = flap.center()
                self._plot_point (x,y, color=color, size=0, text=flap.name, textColor=color, anchor=(0.5,0.5))



    class Movable_Hinge (pg.PlotDataItem):
        """
        pg.PlotCurveItem/UIGraphicsItem which represents 
        the hinge poyline with its points at wing sections 
        
        Points are implemented with Movable_Points
        """

        class Movable_Hinge_Point (Movable_Point):
            """ Represents hinge movable point """

            def __init__ (self, *args, 
                          planform = Planform, 
                          mode = mode.NORM_TO_PLANFORM,
                          t_fn = None, tr_fn = None,                                # transformation
                          **kwargs):

                self._mode = mode
                self._planform = planform
                self._t_fn  = t_fn
                self._tr_fn = tr_fn
                self._flaps : Flaps = planform.flaps

                super().__init__ (*args, **kwargs) 


            @override
            def label_moving (self, *_):
                """ label text during move"""

                x, y = self._tr_fn (self.x, self.y)

                depth, rel_depth = self._flaps.flap_depth_at (x, hinge_y = y)  

                return f"{self.name} {depth:.1f}mm {rel_depth:.1%}"



        def __init__ (self, pi : pg.PlotItem, 
                    planform = Planform, 
                    mode = mode.NORM_TO_PLANFORM,
                    t_fn = None, tr_fn = None,                                  # transformation
                    color = None, 
                    movable = False,
                    label_anchor = (0,1),
                    on_changed = None, 
                    **kwargs):

            self._pi = pi
            self._planform = planform
            self._flaps : Flaps = planform.flaps

            self._mode = mode 
            self._color = color 
            self._t_fn, self._tr_fn  = t_fn, tr_fn
            self._callback_changed = on_changed

            # Control jpoints - transform them to current coordinate system   

            if self._mode == mode.NORM_NORM:
                self._jpoints = self._flaps.rel_depth_as_jpoints ()
            else:
                self._jpoints = self._flaps.hinge_as_jpoints (transform_fn = t_fn)

            # init polyline of control points as PlotCurveItem and hide it (pen=None) 

            super().__init__(*self.polyline(), pen=pg.mkPen (self._color, width=0.1, style=Qt.PenStyle.DotLine))            # mouse coordinates will define hinge line  

            self.setZValue (10) if movable else self.setZValue (5)             
            self.setCurveClickable (True, width=8)                  # make clickable to add new point 
            self.sigClicked.connect (self._hinge_item_clicked)

            # init control points as Movable_Points 

            for i, jpoint in enumerate (self._jpoints):

                p = self.Movable_Hinge_Point (jpoint, planform=planform, mode=mode, id = i, parent=self, 
                                              movable=movable, t_fn = t_fn, tr_fn = tr_fn,               # transformation
                                              color=color, symbol='s', size=7, label_anchor=label_anchor, **kwargs) 
                
                p.sigPositionChanged.connect        (self._moving_point)
                p.sigPositionChangeFinished.connect (self._finished_point)
                p.sigShiftClick.connect             (self._delete_point)


        def polyline (self) -> tuple[list]:
            """returns coordinates of self_jpoints as x, y polyline """
            x, y = [], []
            for p in self._jpoints:
                x.append(p.x)
                y.append(p.y)
            
            # if left point isn't at tip, insert additional extrapolated point at tip 
            span = self._planform.span
            if x[-1] != span: 
                y_tip = interpolate (x[-2], x[-1], y[-2],y[-1], span)
                x.append (span)
                y.append (y_tip)

            return np.array(x), np.array(y)


        def _moving_point (self, aPoint : Movable_Point):
            """ slot - point is moved by mouse """
            jpoint : JPoint =  self._jpoints[aPoint.id]
            jpoint.set_y(aPoint.y)                                  # update self point list 

            aPoint.setPos (*jpoint.xy)                              # update Movable point 
            self.setData (*self.polyline())                         # update self (polyline) 
            self.setPen (pg.mkPen (self._color, width=1, style=Qt.PenStyle.DotLine))  # and show it 


        def _delete_point (self, aPoint : Movable_Point):
            """ slot - point should be deleted """

            # a minimum of 3 control points 
            if len(self._jpoints) < 3: return   

            i = aPoint.id

            if self._flaps.delete_hinge_point_ok (i):                 # delete hinge definition at section ok?

                # remove from list
                del self._jpoints[i]                                # update self point list 
                self.setData (*self.polyline())                     # update self (polyline) 

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
            x = pos.x()

            insert_ok = self._flaps.insert_hinge_point_at (x) 

            if insert_ok:
                # if successful, leave self directly without updatung points 
                ev.accept()
                if callable(self._callback_changed):
                    QTimer().singleShot(10, lambda: self._callback_changed())
            else: 
                ev.ignore()


        def _finished_point (self, aPoint):
            """ slot - point move is finished """
            
            self.hide()
            if self._mode == mode.NORM_NORM:
                self._flaps.rel_depth_from_jpoints (self._jpoints)
            else: 
                self._flaps.hinge_from_jpoints (self._jpoints, transform_fn=self._tr_fn)

            if callable(self._callback_changed):
                QTimer().singleShot(10, lambda: self._callback_changed())




class Airfoil_Artist (Abstract_Artist_Planform):
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
        """ true - plot max thickness """
        return self._show_thick
    def set_show_thick (self, aBool : bool):
        self._show_thick = aBool == True
        self.refresh()


    def _plot (self): 


        # plot airfoil of wing Sections 

        colors = random_colors (len(self.wingSections), h_start=0.4)

        # strak airfoils if needed
        if self.show_strak:
            self.wingSections.do_strak (geometry_class=GEO_BASIC)

        section : WingSection

        for i, section in enumerate (self.wingSections):

            airfoil = section.airfoil
            if (airfoil.isLoaded) and not (not self.show_strak and airfoil.isBlendAirfoil):

                if self.real_size:

                    # the coordinate transformation (mode) can't be used because 
                    # airfoils y_coordinates are z-axis in wing...  
                    line_x, line_y = section.line()                         # get section as line 
                    le_x, te_x = line_y[0],  line_y[1]
                    chord = te_x - le_x

                    x, y = airfoil.x * chord + le_x, airfoil.y * chord

                else:

                    x, y = airfoil.x, airfoil.y

                color : QColor = colors[i]
                if self._mini_mode:
                    color = color.darker(130)  
                    label = f"{airfoil.name}"
                else:    
                    label = f"{airfoil.name} @ {section.name_short}"  

                if airfoil.isBlendAirfoil:
                    pen = pg.mkPen(color, width=1.5, style=Qt.PenStyle.DashLine)
                else:
                    pen = pg.mkPen(color, width=1.5)

                self._plot_dataItem  (x, y, name=label, pen = pen, antialias = True, zValue=2)
                
                # plot thickness highpoint with max thickness line 

                if self.show_thick:
                    x,t = airfoil.geo.thickness.highpoint.xy
                    t_x = x
                    y_u = airfoil.geo.upper.yFn (x)                               # thickness highpoint on upper line 
                    y_l = airfoil.geo.lower.yFn (x)                               # thickness highpoint on lower line 
                    if self.real_size:
                        x   = x *   chord + le_x 
                        y_u = y_u * chord 
                        y_l = y_l * chord 
                        t   = t *   chord
                        t_x = t_x * chord 
                        label = f"{t:.1f}mm @ {t_x:.0f}mm"
                    else: 
                        label = f"{t:.1%} @ {t_x:.0%}"

                    self._plot_point (x,y_u, symbol='+', color=color, text=label)

                    line_x = [x,x]
                    line_y = [y_l, y_u]
                    pen = pg.mkPen(color.darker(150), width=1)

                    self._plot_dataItem  (line_x, line_y, pen = pen, antialias = False, zValue=1)



class Airfoil_Name_Artist (Abstract_Artist_Planform):
    """
    Plot the airfoil names 
        - mode NORM
        - mode SPAN_NORM
        - mode NORM_PLANFORM
        - mode PLANFORM
    """
    def __init__ (self, *args, show_strak=False, use_nick_name=False,**kwargs):

        self._show_strak    = show_strak                    # show also straked airfoils 
        self._use_nick_name = use_nick_name                 # take airfoils nick name
        super().__init__ (*args, **kwargs)


    @property
    def show_strak (self) -> bool:
        return self._show_strak
    def set_show_strak (self, aBool : bool):
        self._show_strak = aBool == True
        self.refresh()

    @property
    def use_nick_name (self) -> bool:
        return self._use_nick_name
    def set_use_nick_name (self, aBool : bool):
        self._use_nick_name = aBool == True
        self.refresh()


    def _plot (self): 

        # plot airfoil of wing Sections 

        colors = random_colors (len(self.wingSections), h_start=0.4)

        # transformation function

        m     = self._mode 

        # strak airfoils if needed to get the generated airfoil names

        if self.show_strak and not self.wingSections.strak_done:
            self.wingSections.do_strak (geometry_class=GEO_BASIC)

        # plot all sections

        section : WingSection

        for isec, section in enumerate (self.wingSections):

            if self.show_strak or (not self.show_strak and not section.airfoil.isBlendAirfoil ):

                if   m == mode.NORM_NORM or m == mode.NORM_TO_SPAN:
                    x,y = section.line_in_chord ()
                elif m == mode.REF_TO_NORM  or m == mode.REF_TO_SPAN:
                    x,y = section.line_in_chord_ref ()
                else: 
                    x,y = section.line ()

                # take root setion to get a constant offset for y of label 

                if section.is_root:
                    dy = (y[1] - y[0]) / 5

                # plot sec airfoil name in different modes 

                if self.use_nick_name:
                    name = section.airfoil_nick_name
                else: 
                    name = section.airfoil.name

                point_x = x[0]
                if m == mode.NORM_NORM:
                    point_y = y[1]                                                # point at te
                    anchor = (-0.2,0.8)
                elif m == mode.REF_TO_NORM or m == mode.REF_TO_SPAN:
                    point_y = y[0]                                                # point at le
                    anchor = (0.5,1.2)                                            # always constant above 
                else:
                    point_y = y[0]                                                # point at le
                    anchor = (0.5,1.5)                                            # label anchor 

                point_y -= dy                                                     # plot above le

                color = colors[isec].darker(110)

                self._plot_point (point_x, point_y, color=color, size=0, text=name, textColor=color, anchor=anchor)



class Image_Artist (Abstract_Artist_Planform):
    """
    Plot an image based on a Image_Definition
    """

    sig_scale_point_changed     = pyqtSignal ()                    # planform data changed 


    def __init__ (self, *args, as_background=False, image_def=None, **kwargs):

        self._as_background = as_background                                 # background image mode 

        self._qimage = None 
        self._image_def = image_def
        self._imageItem = None
        self._inverted  = False

        self._point_le : self.Movable_Image_Point = None
        self._point_te : self.Movable_Image_Point = None

        super().__init__ (*args, **kwargs)


    @property
    def img_def (self) -> Image_Definition:
        """ the actual image definition"""
        if self._image_def is not None: 
            return self._image_def
        else:
            return self.wing.background_image

    @property
    def as_background (self) -> bool: 
        """ show image as background, scaled to span """
        return self._as_background

    
    @property
    def imageItem (self) -> pg.ImageItem:
        """ the pg imageItem (nd.array of qimage)"""
        if self._imageItem is None: 
            self._imageItem = pg.ImageItem() # self.image_np
        return self._imageItem


    def _apply_orientation (self):
        """ apply image definition orientation settings """

        # start with a new identity transformation matrix 
        tr = QTransform()               

        # mirror x and y 
        if self.img_def.mirrored_horizontal:
            tr.scale (-1, 1)
            tr.translate (-self.imageItem.width(),0)

        if self.img_def.mirrored_vertical:
            tr.scale (1, -1)
            tr.translate (0,-self.imageItem.height())

        if self.img_def.rotated:
            tr.rotate (-90)

        self.imageItem.setTransform (tr)
        

    def _apply_coloring (self):
        """ apply image definition color settings """

        # apply levels (lower and upper boundary of color value) = brightness & contrast
        self.imageItem.setLevels ([self.img_def.black_level, self.img_def.white_level])  

        # invert pixels
        if self.img_def.invert:
            # https://stackoverflow.com/questions/47382482/inverting-pixels-of-an-rgb-image-in-python
            self.imageItem.image[:, :, :3] = 255 - self.imageItem.image[:, :, :3]

        # remove red from rgb
        if self.img_def.remove_red:
            self.imageItem.image[:, :, 0] = 0


    def _apply_scale_to_planform (self):
        """ move to 0,0 and scale to span"""

        # get current transformation (orientation already applied) 
        tr = self.imageItem.transform()

        # move le-root of image to 0,0
        imp_0_x  = self.img_def.point_le[0]
        imp_0_y  = self.img_def.point_le[1]

        tr = tr * QTransform (1, 0, 0, 1, - imp_0_x, - imp_0_y)

        # scale span of image to planform span 
        img_span = self.img_def.point_te[0] - self.img_def.point_le[0]
        scale = self.planform.span / img_span

        tr = tr * QTransform (scale, 0, 0, scale, 0, 0)

        self.imageItem.setTransform (tr)


    def _create_imageItem (self) -> pg.ImageItem:
        """ create the final, scaled imageItem"""

        self._imageItem = None

        # re-set original qimage into imgaeItem 
        self.imageItem.setImage (pg.functions.imageToArray (self._qimage))

        # apply defined transformations 
        self._apply_orientation ()
        self._apply_coloring ()

        if self.as_background:
            self._apply_scale_to_planform ()


    @override    
    def _remove_plots (self):
        """ remove self plots from GraphicsView """
        super()._remove_plots()
        self._imageItem = None
        self._qimage = None 


    def _plot (self):

        if not self.img_def.exists: return 

        logger.debug (f"{self} reset and plot image [{self.img_def.pathFilename}]")

        # load file as QImage, convert to ImageItem, apply modifications 

        self._qimage = QImage()
        self._qimage.load (self.img_def.pathFilename)
        self._qimage.convertTo (QImage.Format.Format_ARGB32)                    # ensure not an indexed 8bit 

        self._create_imageItem ()

        self.imageItem.setZValue (0)

        # add ImageItem to PltItem (Viewbox)  

        self._add (self.imageItem)

        # movable points for le and tip only in edit mode (not background) 

        if not self.as_background:

            xy   = self.img_def.point_le     
            self._point_le = self.Movable_Image_Point (xy, name="Move to Root of Leading edge", movable=True, size=11,
                                                       on_changed=self._on_point_le_moved)
            self._add (self._point_le)


            xy   = self.img_def.point_te     
            self._point_te = self.Movable_Image_Point (xy, name="Move to very Tip", movable=True, label_anchor=(1.2,1),  size=11,
                                                       on_changed=self._on_point_te_moved)
            self._add (self._point_te)


    def _on_point_le_moved (self):
        self.img_def.set_point_le ((round(self._point_le.x,2), round(self._point_le.y,2)))
        self.sig_scale_point_changed.emit()

    def _on_point_te_moved (self):
        self.img_def.set_point_te ((round(self._point_te.x,2), round(self._point_te.y,2)))
        self.sig_scale_point_changed.emit()

    # -------------------------------------------------


    class Movable_Image_Point (Movable_Point):

        @override
        def _label_opts (self, moving=False, hover=False) -> dict:
            """ returns the label options as dict """

            if moving or hover:
                labelOpts = {'color': QColor("red"),
                            'anchor': self._label_anchor,
                            'offset': (5, 0)}
            else: 
                labelOpts = {'color': QColor("red"),
                            'anchor': self._label_anchor,
                            'offset': (5, 0)}
            return labelOpts

        @override
        def label_moving (self, *_):
            """ the label when moving - can be overloaded """
            return f"{self.y:.4n}@{self.x:.4n}"

        @override
        def _finished (self):
            """ default slot -  when point move is finished """
            self._changed()






class Wing_Data_Artist (Abstract_Artist_Planform):
    """
    Plot some data of the wing
    """

    def _plot (self): 
 
        p0  = (0.15,0.3)
        x1 = 140
        dy = 25

        y = 0 
        self._plot_text ("Wing Span",  parentPos=p0, offset=(0, y))
        self._plot_text (f"{self.wing.wingspan:.0f}", parentPos=p0, itemPos = (1,1), offset=(x1, y))
        self._plot_text ("mm", parentPos=p0, offset=(x1, y))

        y += dy
        self._plot_text ("Wing Area",  parentPos=p0, offset=(0, y))
        self._plot_text (f"{self.wing.wing_area/10000:.2f}", parentPos=p0, itemPos = (1,1), offset=(x1, y))
        self._plot_text ("dm²", parentPos=p0, offset=(x1, y))

        y += dy
        self._plot_text ("Aspect Ratio",  parentPos=p0, offset=(0, y))
        self._plot_text (f"{self.wing.wing_aspect_ratio:.2f}", parentPos=p0, itemPos = (1,1), offset=(x1, y))

        y += dy
        self._plot_text ("Root Chord",  parentPos=p0, offset=(0, y))
        self._plot_text (f"{self.planform.chord_root:.1f}", parentPos=p0, itemPos = (1,1), offset=(x1, y))
        self._plot_text ("mm", parentPos=p0, offset=(x1, y))

        y += dy
        self._plot_text ("MAC",  parentPos=p0, offset=(0, y))
        self._plot_text (f"{self.planform.planform_mac:.1f}", parentPos=p0, itemPos = (1,1), offset=(x1, y))
        self._plot_text ("mm", parentPos=p0, offset=(x1, y))



class Polar_Artist (Abstract_Artist_Planform):
    """Plot the polars of airfoils of wingSections"""

    def __init__ (self, *args, 
                  xyVars = (CD, CL), 
                  show_strak=False, 
                  **kwargs):
        super().__init__ (*args, **kwargs)

        self._show_strak  = show_strak                  # show also straked airfoils 
        self._show_points = False                       # show point marker 
        self._xyVars = xyVars                           # definition of x,y axis

    @property
    def show_strak (self) -> bool:
        """ true - show also straked airfoils """
        return self._show_strak
    def set_show_strak (self, aBool : bool):
        self._show_strak = aBool == True
        self.refresh()


    @property
    def xyVars(self): return self._xyVars
    def set_xyVars (self, xyVars: Tuple[var, var]): 
        """ set new x, y variables for polar """
        self._xyVars = xyVars 
        self.refresh()


    def _plot (self): 
        """ do plot of airfoil polars in the prepared axes  """

        # get airfoil colors - same as Airfoil_Artist

        colors = random_colors (len(self.wingSections), h_start=0.4)

        # plot polars of airfoils

        nPolar_plotted      = 0 
        nPolar_generating = 0                     # is there a polar in calculation 
        error_msg           = []  

        section : WingSection

        for i, section in enumerate (self.wingSections):

            airfoil = section.airfoil
            if (airfoil.isLoaded) and not (not self.show_strak and airfoil.isBlendAirfoil):

                color_airfoil : QColor = colors[i]

                # get / prepare polars 
                 
                polarSet = airfoil.polarSet
                polarSet.load_or_generate_polars ()

                polarSet : Polar_Set = airfoil.polarSet
                polars_to_plot = polarSet.polars

                polar : Polar 
                for iPolar, polar in enumerate(reversed(polars_to_plot)): 

                    # generate increasing color hue value for the polars of an airfoil 
                    color = color_in_series (color_airfoil, iPolar, len(polars_to_plot), delta_hue=0.1)

                    label_airfoil =  f"{airfoil.name} @ {section.name_short}"  

                    self._plot_polar (label_airfoil, polar, color)

                    if not polar.isLoaded: 
                        nPolar_generating += 1
                    elif polar.error_occurred:
                        # in error_msg could be e.g. '<' 
                        error_msg.append (f"'{airfoil.name_to_show} - {polar.name}': {html.escape(polar.error_reason)}")
                    else: 
                        nPolar_plotted += 1

        # show error messages 

        if error_msg:
            text = '<br>'.join (error_msg)          
            self._plot_text (text, color=qcolors.ERROR, itemPos=(0.5,0.5))

        # show generating message 

        if nPolar_generating > 0: 
            if nPolar_generating == 1:
                text = f"Generating polar"
            else: 
                text = f"Generating {nPolar_generating} polars"
            self._plot_text (text, color= "dimgray", fontSize=self.SIZE_HEADER, itemPos=(0.5, 1))




    def _plot_polar (self, label_airfoil : str, polar: Polar, color): 
        """ plot a single polar"""


        # build nice label 

        label = f"{label_airfoil} {polar.re_asK}k" 

        if not polar.isLoaded:
            label = label + ' generating'                       # async polar generation  

        # finally plot 

        antialias = True
        linewidth = 1.0
        zValue    = 1
        pen       = pg.mkPen(color, width=linewidth)

        x,y = polar.ofVars (self.xyVars)

        self._plot_dataItem  (x, y, name=label, pen = pen, 
                                symbol=None, antialias = antialias, zValue=zValue)
