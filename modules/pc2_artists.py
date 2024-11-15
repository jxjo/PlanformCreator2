#!/usr/bin/env pythonupper
# -*- coding: utf-8 -*-

"""  

The "Artists" to plot a airfoil object on a pg.PlotItem 

"""
from enum                       import Enum

from base.math_util             import derivative1
from base.artist                import *
from base.common_utils          import *
from base.spline                import Bezier

from wing                       import Wing, Planform
from wing                       import Planform_2, Norm_Planform, Norm_Chord_Bezier, Norm_Chord_Trapezoidal
from wing                       import Norm_WingSection, Norm_WingSections 
from wing                       import Norm_Flaps, Norm_Flap
from model.airfoil              import Airfoil, GEO_BASIC

from PyQt6.QtGui                import QColor, QBrush, QPen, QTransform
from PyQt6.QtCore               import pyqtSignal, QObject

import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


# -------- Colors ------------------------


COLOR_PLANFORM = QColor ('whitesmoke')
COLOR_LE       = QColor ('whitesmoke')
COLOR_TE       = QColor ('coral')

COLOR_BOX       = QColor ('dodgerblue')

COLOR_CHORD    = QColor ('paleturquoise')
COLOR_REF_LINE = QColor ('springgreen')
COLOR_BANANA   = QColor ('khaki')
COLOR_REF_ELLI = QColor ('dodgerblue')
COLOR_SECTION  = QColor ('deeppink')

# -------- coordinate systems ------------------------


class mode (Enum):
    """ enums for coordinate system an artist is working"""

    DEFAULT       = 0                       # no transformation, use default coordinates of artist

    NORM_NORM     = 1                       # x = 0..1      y = 0..1
    SPAN_NORM     = 2                       # x = 0..span   y = 0..1

    PLANFORM_NORM = 3                       # x = 0..1      y = 0..1
    PLANFORM      = 4                       # x = 0..span   y = 0..chord

    WING_LEFT     = 5                       # x = -fuse..-span  y = chord
    WING_RIGHT    = 6                       # x = +fuse..+span  y = chord

    NORM_REF      = 7                       # x = 0..1      y = 0..1 relative within reference 
    SPAN_REF      = 8                       # x = 0..span   y = 0..1 relative within reference 



# -------- Abstract super class ------------------------

class Artist_Planform (Artist):
    """ 
    Superclass for planform artists providing
        - access planform objects and handle scaling 
        - common change signals

    """

    sig_planform_changed     = pyqtSignal ()                    # planform data changed 
    sig_wingSection_selected = pyqtSignal (Norm_WingSection)    # new current wingSection 
    sig_wingSection_new      = pyqtSignal (Norm_WingSection)    # new wingsection inserted 
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

        if self._mode == mode.PLANFORM:  
            self.set_t_fn  (self.planform.t_norm_to_plan)                   # bound method for coordinate transformation
            self.set_tr_fn (self.planform.t_plan_to_norm)                    

        elif self._mode == mode.SPAN_NORM:  
            self.set_t_fn  (self.planform.t_norm_to_span)                   # scale x to span
            self.set_tr_fn (self.planform.t_span_to_norm)            

        elif self._mode == mode.SPAN_REF:  
            self.set_t_fn  (self.planform.t_norm_to_span)                   # scale x to span
            self.set_tr_fn (self.planform.t_span_to_norm)            

        elif self._mode == mode.WING_RIGHT:
            self.set_t_fn  (self.wing.t_norm_to_wing_right)                 # scale span, chord, add fuselage

        elif self._mode == mode.WING_LEFT:
            self.set_t_fn  (self.wing.t_norm_to_wing_left)                  # scale span, chord, add fuselage

        super().plot()


    @property
    def planform (self) -> Planform_2: 
        return self.data_object

    @property
    def wing (self) -> Wing: 
        return self.planform.wing
    
    @property
    def wingSections (self) -> Norm_WingSections:
        return self.planform.norm.wingSections


# -------- concrete sub classes ------------------------



class Ref_Line_Artist (Artist_Planform):
    """
    Plot the reference line either
        - mode PLANFORM
        - mode NORM_PLANFORM
    """

    def _plot (self): 

        xn, yn = self.planform.norm.chord_ref_polyline()

        pen   = pg.mkPen(COLOR_REF_LINE, width=1.5)
        self._plot_dataItem  (xn, yn, name="Reference Line", pen = pen, antialias = True, zValue=4)



class Planform_Box_Artist (Artist_Planform):
    """ 
    Plot box arround planform contour (only if show_mouse_helper)
        - Modify chord_root and span 
        - mode PLANFORM
        - mode NORM_PLANFORM
    """

    def _plot (self): 

        x, y = self.planform.norm.box_polygon ()

        color = COLOR_BOX
        pen   = pg.mkPen(color, width=1, style=Qt.PenStyle.DotLine)

        self._plot_dataItem  (x, y, pen = pen, antialias = False, zValue=1)

        # movable root chord and tip  

        if self.show_mouse_helper and self._mode == mode.PLANFORM:          # changing span only for planform

            pt = self.Movable_Chord_Root    (self._pi, self.planform, color=color,
                                            t_fn = self.t_fn, tr_fn = self.tr_fn, 
                                            on_changed=self.sig_planform_changed.emit)
            self._add (pt) 
            pt = self.Movable_Span          (self._pi, self.planform, color=color, 
                                            t_fn = self.t_fn, tr_fn = self.tr_fn, 
                                            on_changed=self.sig_planform_changed.emit)
            self._add (pt) 
            pt = self.Movable_Angle         (self._pi, self.planform, color=color, 
                                            t_fn = self.t_fn, tr_fn = self.tr_fn, 
                                            on_changed=self.sig_planform_changed.emit)
            self._add (pt) 



    class Movable_Box_Abstract (Movable_Point):
        """ 
        Abstract: A point of box to change planform. 
        """
        name = "Box Point"

        def __init__ (self, pi : pg.PlotItem, 
                      planform : Planform_2, 
                      movable = True, 
                      t_fn = None, tr_fn = None,                              # transformation
                      **kwargs):

            self._pi = pi
            self._planform = planform

            self._t_fn, self._tr_fn  = t_fn, tr_fn

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

            x,y = self._t_fn(*self._planform.norm.polygon())

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

            x,y = self._t_fn(*self._planform.norm.box_polygon())

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
            box_x, box_y = self._planform.norm.box_polygon()         # upper right corner of box 
            xn, yn = box_x[1], box_y[1]
            return self._t_fn (xn, yn)        

        @override
        def _moving (self, _):
            angle = np.arctan (self.y /self.x) * 180 / np.pi
            self._planform.set_sweep_angle (angle)              # update chord of planform 
            super()._moving ()

        @override
        def label_moving (self):
            return f"{self.name}  {self._planform.sweep_angle:.2f}°"



    class Movable_Chord_Root (Movable_Box_Abstract):
        """ Root point of planform to change chord """
        name = "Root Chord"

        @override
        def _point_xy (self) -> tuple: 
            box_x, box_y = self._planform.norm.box_polygon()         # middle point of right line
            xn = box_x[3]
            yn = box_y[3] 
            return self._t_fn (xn, yn)

        @override
        def _moving (self, _):
            self._planform.set_chord_root (self.y)              # update chord of planform 
            super()._moving ()

        @override
        def label_moving (self):
            return f"{self.name} {self._planform.chord_root:.1f}mm "



    class Movable_Span (Movable_Box_Abstract):
        """ Tip point of planform to change span"""
        name = "Half Span"

        @override
        def _point_xy (self) -> tuple: 
            box_x, box_y = self._planform.norm.box_polygon()         # middle point of right line
            xn = box_x[1]
            yn = (box_y[1] + box_y[2]) / 2
            return self._t_fn (xn, yn)        

        @override
        def _moving (self, _):
            self._planform.set_span (self.x)                # update span of planform 
            super()._moving ()

        @override
        def label_moving (self):
            return f"{self.name} {self._planform.span:.1f}mm "




class Norm_Chord_Artist (Artist_Planform):
    """
    Plot the normed chord 
        - mode SPAN_NORM
        - mode NORM
    """

    @property
    def planform_normed (self) -> Norm_Planform: 
        return self.planform.norm
    
    def _plot (self): 

        xn, yn = self.planform.norm.cn_polyline () 
 
        name       = "Chord distribution"
        color      = COLOR_CHORD
        pen, brush = pg.mkPen(color, width=2), pg.mkBrush (color.darker (600))

        self._plot_dataItem  (xn, yn, pen = pen, name=name, antialias = True, 
                              fillLevel=0.0, fillBrush=brush,  zValue=1)

        # Chord Bezier curve based - move control points  

        if self.show_mouse_helper and self.planform.norm.cn_is_bezier:
 
            pt = self.Movable_Chord_Bezier (self._pi, self.planform,
                                            t_fn = self.t_fn, tr_fn = self.tr_fn, 
                                            movable=True, color=color,
                                            on_changed=self.sig_planform_changed.emit)
            self._add (pt) 

        # Chord trapezoidal - defined by wing sections

        elif self.planform.norm.cn_is_trapezoidal:

            pass




    class Movable_Chord_Bezier (Movable_Bezier):
        """
        pg.PlotCurveItem/UIGraphicsItem which represents 
        a Bezier based chord distribution. 
        """
        def __init__ (self, pi : pg.PlotItem, 
                      planform = Planform_2, 
                      t_fn = None, tr_fn = None,                                # transformation
                      **kwargs):

            self._pi = pi
            self._planform = planform
            self._norm_chord : Norm_Chord_Bezier = self._planform.norm.chord

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




class Planform_Artist (Artist_Planform):
    """
    Plot the Planform
        - mode NORM_PLANFORM
        - mode PLANFORM
        - mode WING_RIGHT, WING_LET
    """

    def __init__ (self, *args, 
                  as_contour = False,                               #  planform as outline 
                  **kwargs):

        self._as_contour = as_contour           
        super().__init__ (*args, **kwargs)


    @property 
    def as_contour (self):
        """ show planform as one contour - and not le and te seperatly"""
        return self._as_contour
    

    def _plot (self): 
    

        # plot planform in a single contour 

        if self.as_contour:

            xn, yn = self.planform.norm.polygon () 

            self._plot_dataItem  (xn, yn, pen=pg.mkPen(COLOR_LE, width=2), antialias=True, zValue=3,
                                    name=f"{self.planform}")
            
        # plot planform by leading and trailing edge 

        else: 
            xn, le_yn, te_yn = self.planform.norm.le_te_polyline () 

            # fill plot depending on mode  

            if self._mode == mode.PLANFORM_NORM:
                brush_le, brush_te = pg.mkBrush (COLOR_LE.darker(800)), pg.mkBrush (COLOR_TE.darker(500))
            else: 
                brush_le, brush_te = None, None

            # plot le and te 

            self._plot_dataItem  (xn, le_yn, pen=pg.mkPen(COLOR_LE, width=2), antialias=True, zValue=3,
                                name=f"Leading edge", fillLevel=0.0, fillBrush=brush_le)
            
            self._plot_dataItem  (xn, te_yn, pen=pg.mkPen(COLOR_TE, width=2),
                                name=f"Trailing edge", fillLevel=0.0, fillBrush=brush_te)




class Norm_Chord_Ref_Artist (Artist_Planform):
    """
    Plot chord reference within chord distribution
        - mode SPAN_REF
        - mode NORM_REF    
    """

    def _plot (self): 

        xn,yn = self.planform.norm.chord_ref.polyline ()

        color = QColor (COLOR_REF_LINE) 
        pen = pg.mkPen(color, width=2)

        label = "Reference Line"

        ref_item = self._plot_dataItem  (xn, yn, name=label, pen = pen, antialias = True, zValue=2)

        # plot dummy le and te and fill between re line 

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
            pt = self.Movable_Ref_Chord_Bezier (self._pi, self.planform.norm, 
                                        t_fn = self.t_fn, tr_fn = self.tr_fn, 
                                        movable=True, color=COLOR_REF_LINE,
                                        on_changed=self.sig_planform_changed.emit)
            self._add (pt) 



    class Movable_Ref_Chord_Bezier (Movable_Bezier):
        """
        pg.PlotCurveItem representing a Bezier based chord reference. 

        The Bezier curve can be changed by the control points.
            - either 2 or 3 points 
            - add/delete 3rd point 
        """
        def __init__ (self, pi : pg.PlotItem, 
                      norm_planform : Norm_Planform,
                      t_fn = None, tr_fn = None,                                # transformation
                      **kwargs):

            self._pi = pi
            self._norm_planform = norm_planform

            self._t_fn, self._tr_fn  = t_fn, tr_fn

            # scale x-coordinate of Bezier jpoints depending on mode 
            jpoints = self._norm_planform.chord_ref.bezier_as_jpoints ()
            jpoints = JPoint.transform (jpoints, transform_fn = self._t_fn)

            super().__init__(jpoints, **kwargs)


        @override
        @property
        def u (self) -> list:
            """ the Bezier u parameter """
            return np.linspace(0.0, 1.0, num=25)


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
            self._norm_planform.chord_ref.bezier_from_jpoints (jpoints)

            super()._finished_point (aPoint)




class Ref_Planforms_Artist (Artist_Planform):
    """
    Plot the planform contour of reference planforms 
        - mode PLANFORM
    """
    def __init__ (self, *args, 
                  show_chord = False,                               #  planform as outline 
                  **kwargs):

        self._show_chord = show_chord           
        super().__init__ (*args, **kwargs)

    @property
    def ref_planforms (self) -> list[Planform_2]:

        refs = []
        refs.append (self.wing.planform_elliptical)
        return refs


    def _plot (self): 

        planform : Planform_2 
        for planform in self.ref_planforms:

            if self._show_chord:
                xn, yn = planform.norm.cn_polyline () 
            else:
                xn, yn = planform.norm.polygon () 

            label = f"Reference {planform.norm.name}"

            color = COLOR_REF_ELLI
            pen   = pg.mkPen(color, width=1)

            self._plot_dataItem  (xn, yn, name=label, pen = pen, antialias = False, zValue=1)




class WingSections_Artist (Artist_Planform):
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


    def __init__ (self, *args, cur_wingSection_fn=None, 
                  **kwargs):

        self._cur_wingSection_fn = cur_wingSection_fn           # current wingSection bound method

        super().__init__ (*args, **kwargs)


    @property
    def cur_wingSection (self) -> Norm_WingSection | None: 
        """ returns the current, selected wing section """
        if self._cur_wingSection_fn:
            return self._cur_wingSection_fn()
        else: 
            return None


    def _plot (self): 

        color = COLOR_SECTION
        m     = self._mode 

        # plot all sections

        section : Norm_WingSection

        for section in self.wingSections:

            if   m == mode.NORM_NORM or m == mode.SPAN_NORM:
                xn,yn = section.line_in_chord ()
            elif m == mode.NORM_REF  or m == mode.SPAN_REF:
                xn,yn = section.line_in_chord_ref ()
            else: 
                xn,yn = section.line ()

            if section.defines_cn:
                pen   = pg.mkPen(color, width=1.0)
                name  = "Wing Sections fix"                                     
            else:
                pen   = pg.mkPen(color, width=1.0,style=Qt.PenStyle.DashLine)
                name  = "Wing Sections flex"                                    

            p = self._plot_dataItem  (xn, yn,  name=name, pen = pen, antialias = False, zValue=3)

            # plot section name in different modes 

            if m == mode.NORM_NORM:
                point_xy = (xn[1],yn[1])                                      # point at te
                anchor = (-0.2,0.8)
            elif m == mode.NORM_REF or m == mode.SPAN_REF:
                point_xy = (xn[0],yn[0])                                      # point at le
                anchor = (0.5,1.2)                                            # always constant above 
            else:
                point_xy = (xn[0],yn[0])                                      # point at le
                anchor = (0.5,2.0) if section.isTip else (0.5,1.2)            # label anchor 

            self._plot_point (point_xy, color=color, size=0, text=section.name_short, textColor=color, anchor=anchor)

            # highlight current section - add movable points for move by pos and move by chord 

            if section == self.cur_wingSection and not (section.isRoot or section.isTip):

                p.setShadowPen (pg.mkPen(QColor(COLOR_SECTION).darker(200), width=6))

                if self.show_mouse_helper:
                    pt = self.Movable_Section (self._pi, self.planform, section, 
                                                mode = m, t_fn = self.t_fn, tr_fn = self.tr_fn,
                                                move_by_pos=True,
                                                on_changed=self.sig_wingSection_changed.emit)
                    self._add (pt) 

                    if m == mode.SPAN_REF or m == mode.NORM_REF:                # move doesn't make sense  (yn==1)
                        movable = False
                    else:
                        movable = True  
                    pt = self.Movable_Section (self._pi, self.planform, section, 
                                                mode = m, t_fn = self.t_fn, tr_fn = self.tr_fn,
                                                move_by_pos=False, movable=movable,
                                                on_changed=self.sig_wingSection_changed.emit)
                    self._add (pt) 

            # make line clickable if there is a callback 
            if self._cur_wingSection_fn:
                p.setCurveClickable (True, width=8)
                p.sigClicked.connect (self._section_item_clicked)


        if not (m == mode.WING_LEFT or m == mode.WING_RIGHT):
            # make scene cklickable to add wing section 
            #   delayed as during init scene is not yet available
            QTimer().singleShot (10, self._connect_scene_mouseClick)


    def _section_item_clicked (self, item : pg.PlotDataItem,  ev : MouseClickEvent):
        """
        wing section item was clicked 
            - select
            - delete (shift-click)
            - select & toggle 'defines_cn' (alt-click)"""
    
        x, y   = item.xData[0], item.yData[0]
        xn, _  = self.tr_fn (x, y)                                      # transform to norm 

        section : Norm_WingSection
        for section in self.wingSections:
            if section.xn() == xn: break 

        # sanity 
        if section.xn() != xn:
            raise ValueError ("WIng section could be detected")


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
       
        # ! #todo
        # current 2 mouse click events arrive - why? 

        # get scene coordinates of click pos and map to view box 
        viewbox : pg.ViewBox = ev.currentItem
        if viewbox == self._pi.vb:                     # is this my view box (or of another plot item)? 
            
            ev.accept()

            pos : pg.Point = viewbox.mapSceneToView(ev.scenePos())
            xn, _ = self.tr_fn (pos.x(), pos.y())

            # create new section  and signal ...
            section = self.wingSections.create_at (xn)

            if section: 
                QTimer().singleShot(10, lambda: self.sig_wingSection_new.emit (section))     



    class Movable_Section (Movable_Point):
        """ 
        Represents a point of the section line to move either by pos or by chord. 
        """
        name = "Section Point"

        def __init__ (self,
                    pi      : pg.PlotItem, 
                    planform : Planform_2,
                    section : Norm_WingSection, 
                    movable = True, 
                    move_by_pos = True,                                     # either by pos or by chord '
                    mode = mode.PLANFORM,                                   # coordinate system
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
            self._define_chord  = not move_by_pos if     section.defines_cn else False

            self._mode = mode 
            self._t_fn, self._tr_fn  = t_fn, tr_fn

            self._in_move = False                                           # move recursion detection          

            super().__init__(self._point_xy(), movable=movable, color=COLOR_SECTION,
                             symbol='s', size=8, show_label_static = movable,**kwargs)
            
            # set static brush depending if point to indicate current section dependancy ...

            if (move_by_pos and section.has_fix_chord) or (not move_by_pos and section.has_fix_pos):
                self.setBrush (QColor("black"))


        def _line_of_section (self) -> tuple:
            """ poyline of section in display coordinate"""

            if self._mode == mode.NORM_NORM or self._mode == mode.SPAN_NORM:
                xn,yn = self._section.line_in_chord ()
            elif self._mode == mode.NORM_REF or self._mode == mode.SPAN_REF:
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
            left_xn, right_xn = self._section.xn_limits()
            xn = np.clip (xn, left_xn, right_xn)


            if   self._move_by_pos:
                self._section.set_xn (xn)                       # update section pos

            elif self._move_by_chord:
                cn = self._section._planform.cn_at (xn)
                self._section.set_cn (cn)                       # update section pos by chord

            elif self._define_chord:
                if self._mode == mode.PLANFORM or self._mode == mode.PLANFORM_NORM:
                    _, te_yn = self._section.le_te ()
                    cn = yn - te_yn                             # calculate new cn from planform
                else: 
                    cn = yn                                     # yn is chord 
                left_cn, right_cn = self._section.cn_limits()   # cn shouldn't be more dann left neighbour
                cn = np.clip (cn, right_cn, left_cn)
                self._section.set_cn (cn)                       # update section chord


            self.setPos(self._point_xy())                       # update point position if we run against limits  

            self._plot_tmp_section_line()                       # create or update temp section line
            self._plot_tmp_outline ()                             # create or update temp chord polyline
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

            if self._mode == mode.SPAN_REF or self._mode == mode.NORM_REF:
                return                                                  # outline doesn't make sense ...

            if self._mode == mode.NORM_NORM or self._mode == mode.SPAN_NORM:
                x,y = self._t_fn(*self._planform.norm.cn_polyline())
                pen = pg.mkPen(QColor(COLOR_CHORD).darker(150), width=1, style=Qt.PenStyle.DashLine)
            else:
                x,y = self._t_fn(*self._planform.norm.polygon())
                pen = pg.mkPen(QColor(COLOR_PLANFORM).darker(150), width=1, style=Qt.PenStyle.DashLine)

            if self._tmp_outline_item is None:
                p = pg.PlotDataItem  (x, y, pen = pen, antialias = False)
                p.setZValue (2)
                self._pi.addItem (p)
                self._tmp_outline_item = p
            else:
                self._tmp_outline_item.setData (x,y)


        @override
        def label_static (self) -> str:
            """ the static label depending on fixed pos or chord"""
            if self._move_by_pos:               
                return "Fixed pos"   if self._section.has_fix_pos   else "Flex pos"
            else:
                return "Fixed chord" if self._section.has_fix_chord else "Flex chord"


        @override
        def label_moving (self):
            """ label text during move"""
            if self._move_by_pos:
                if self._mode == mode.PLANFORM or self._mode == mode.SPAN_NORM:
                    x = self._section.xn() * self._planform.span
                    lab = f"pos {x:.1f}mm"
                else: 
                    lab = f"pos {self._section.xn():.2f}"
            else:
                if self._mode == mode.NORM_NORM or self._mode == mode.SPAN_NORM:
                    lab = f"chord {self._section.cn():.1%}"
                elif self._mode == mode.PLANFORM_NORM:
                    lab = f"chord {self._section.cn():.2f}"
                else:
                    c = self._section.cn() * self._planform.chord_root
                    lab = f"chord {c:.1f}mm"
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




class Flaps_Artist (Artist_Planform):
    """
    Plot the flaps in  
        - mode PLANFORM
        - mode SPAN_NORM
        - mode SPAN_REF
        - mode NORM_PLANFORM
    """

    def _plot (self): 

        flaps      = self.planform.norm.flaps
        flaps_list = flaps.get()
        if not flaps_list: return 

        colors = random_colors (len(flaps_list))
        color : QColor = colors[0]

        if self._mode == mode.NORM_REF or self._mode == mode.SPAN_REF:

            # plot relative depth distribution in chord distribution 

            x,y = flaps.hinge_cn_polyline ()  

            pen   = pg.mkPen(color, width=1, style=Qt.PenStyle.DashLine)
            brush_color = color.darker(300)
            brush_color.setAlphaF (0.4)
            brush = pg.mkBrush (brush_color)   

            self._plot_dataItem  (x, y,  pen = pen, antialias = True, name="Relative Flap Depth",
                                    fillLevel=0.0, fillBrush=brush,  zValue=3)
        else:

            # plot single flaps 

            self._plot_flaps (flaps_list, colors) 

            # moving points at hinge of flaps 

            if self.show_mouse_helper and not self.planform.norm.hinge_equal_ref_line:

                p = self.Movable_Hinge  (self._pi, self.planform, color=color, mode=self._mode, 
                                        t_fn = self.t_fn, tr_fn = self.tr_fn, 
                                        movable=True, on_changed=self.sig_flaps_changed.emit)
                self._add(p) 
                


    def _plot_flaps (self, flaps : list[Norm_Flap], colors):
        """ plot the single flaps"""

        gap = 0.002
        name = "Flaps"

        for i, flap in enumerate (flaps):

            color : QColor = colors[i]
            pen   = pg.mkPen(color, width=1)

            # flap contour left and right side 
            x,y = flap.line_left (x_offset=gap)  
            self._plot_dataItem  (x, y,  pen = pen, name=name, antialias = False, zValue=1)
            x,y = flap.line_right(x_offset=gap)  
            self._plot_dataItem  (x, y,  pen = pen, antialias = False, zValue=1)

            # hinge and te 
            x,y = flap.line_hinge (x_offset=gap)  
            p1 = self._plot_dataItem  (x, y,  pen = pen, antialias = False, zValue=1)

            x,y = flap.line_te (x_offset=gap)  
            p2 = self._plot_dataItem  (x, y,  pen = pen, antialias = False, zValue=1)

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
                          planform = Planform_2, 
                          mode = mode.PLANFORM,
                          t_fn = None, tr_fn = None,                                # transformation
                          **kwargs):

                self._mode = mode
                self._planform = planform
                self._t_fn  = t_fn
                self._tr_fn = tr_fn
                self._flaps : Norm_Flaps = planform.norm.flaps

                super().__init__ (*args, **kwargs) 


            @override
            def label_moving (self):
                """ label text during move"""

                if self._mode == mode.NORM_NORM :
                    rel_depth  = self.y
                    return f"{self.name} {rel_depth:.1%}"
                
                if self._mode == mode.PLANFORM_NORM :
                    xn, hinge_yn = self.x, self.y
                    return f"{self.name} {self._flaps.depth_at(xn, hinge_yn=hinge_yn):.1%}"

                elif self._mode == mode.PLANFORM:
                    xn, hinge_yn = self._tr_fn (self.x, self.y)
                    depth_n  = self._flaps.depth_at(xn, hinge_yn=hinge_yn)
                    _, te_yn = self._planform.norm.le_te_at (xn)                # calc real flap depth 
                    _, y_arr = self._t_fn ([0,0], [hinge_yn, te_yn])            # transform
                    depth    =  y_arr[1] - y_arr[0]     
                    return f"{self.name} {depth:.1f}mm {depth_n:.1%}"



        def __init__ (self, pi : pg.PlotItem, 
                    planform = Planform_2, 
                    mode = mode.PLANFORM,
                    t_fn = None, tr_fn = None,                                  # transformation
                    color = None, 
                    movable = False,
                    label_anchor = (0,1),
                    on_changed = None, 
                    **kwargs):

            self._pi = pi
            self._planform = planform
            self._flaps : Norm_Flaps = planform.norm.flaps

            self._mode = mode 
            self._t_fn, self._tr_fn  = t_fn, tr_fn
            self._callback_changed = on_changed

            # Control jpoints - transform them to current coordinate system   

            if self._mode == mode.NORM_NORM:
                self._jpoints = self._flaps.rel_depth_as_jpoints ()
            else:
                self._jpoints = self._flaps.hinge_as_jpoints (transform_fn = t_fn)

            # init polyline of control points as PlotCurveItem and hide 

            x, y = self.jpoints_xy()                                # coordinates will define hinge line  

            super().__init__(x, y, pen=pg.mkPen (color, width=1, style=Qt.PenStyle.DotLine))

            self.setZValue (10) if movable else self.setZValue (5)             
            self.setCurveClickable (True, width=8)                  # make clickable to add new point 
            self.sigClicked.connect (self._hinge_item_clicked)

            # init control points as Movable_Points 

            for i, jpoint in enumerate (self._jpoints):

                p = self.Movable_Hinge_Point (jpoint, planform=planform, mode=mode, id = i, parent=self, 
                                              movable=movable, 
                                              t_fn = t_fn, tr_fn = tr_fn,               # transformation
                                              color=color, symbol='s', size=7, label_anchor=label_anchor, **kwargs) 
                
                p.sigPositionChanged.connect        (self._moving_point)
                p.sigPositionChangeFinished.connect (self._finished_point)
                p.sigShiftClick.connect             (self._delete_point)



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
            if len(self._jpoints) < 3: return   

            i = aPoint.id

            # not root or tip 
            if i == 0 or i == (len(self._jpoints) -1 ): return

            # remove from list
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
            x, y = pos.x(), pos.y()

            xn, yn = self._tr_fn (x,y)                          # reverse transformation into normed 
            insert_ok = self._flaps.insert_hinge_point_at (xn) 

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




class Planform_Airfoil_Artist (Artist_Planform):
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

        colors = random_colors (len(self.wingSections))

        # strak airfoils if needed
        if self.show_strak:
            self.wingSections.do_strak (geometry=GEO_BASIC)

        section : Norm_WingSection

        for i, section in enumerate (self.wingSections):

            airfoil = section.airfoil
            if (airfoil.isLoaded) and not (not self.show_strak and airfoil.isBlendAirfoil):

                if self.real_size:

                    # the coordinate transformation (mode) can't be used because 
                    # airfoils y_coordinates are z-axis in wing...  
                    line_x, line_y = self.planform.t_norm_to_plan (*section.line())  # get section as line 
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



