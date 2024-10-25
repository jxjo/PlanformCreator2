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
from wing                       import WingSections, WingSection
from model.airfoil              import Airfoil, Airfoil_Bezier, usedAs, Geometry
from model.airfoil_geometry     import Line, Side_Airfoil_Bezier

from PyQt6.QtGui                import QColor, QBrush, QPen
from PyQt6.QtCore               import pyqtSignal, QObject

import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


# -------- helper functions ------------------------


COLOR_PLANFORM = QColor ('whitesmoke')
COLOR_CHORD    = QColor ('paleturquoise')
COLOR_REF_LINE = QColor ('springgreen')
COLOR_REF_ELLI = QColor ('dodgerblue')
COLOR_SECTION  = QColor ('deeppink')



# -------- concrete sub classes ------------------------


class Planform_Artist (Artist):
    """Plot the planform contour  """

    sig_planform_changed     = pyqtSignal()                 # planform data changed 

    def __init__ (self, *args, show_ref_line=False, **kwargs):

        self._welcome_text = None                       # a HTML welcome text 
        self._first_time  = True                        # to show welcome message 
        self._show_ref_line = show_ref_line             # show reference line 

        super().__init__ (*args, **kwargs)

    @property
    def wing (self) -> Wing: return self.data_object

    @property
    def planform (self) -> Planform: return self.wing.planform

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
            self._plot_title ("Planform", subTitle=self.planform.planformType,  offset=(-20,-30) )

        # plot planform contour

        pen   = pg.mkPen(QColor(COLOR_PLANFORM), width=2)
        y, x = self.planform.polyline()
        self._plot_dataItem  (y, x, name=f"{self.wing.name}", pen = pen, antialias = True, zValue=2)

        # two little markers to identify straight tip 

        x_le, x_te = self.planform.planform_at_tip
        y          = self.planform.halfwingspan
        color = QColor(COLOR_PLANFORM).darker(150)
        self._plot_point (y,x_le,symbol="_", color=color, size=7)
        self._plot_point (y,x_te,symbol="_", color=color, size=7)

        # movable root chord and tip  

        if self.show_mouse_helper:
            pt = self.Movable_Root_Chord (self._pi, self.planform, 
                                        movable=True, show_ref_line=self.show_ref_line, color=COLOR_REF_LINE,
                                        on_changed=self.sig_planform_changed.emit)
            self._add (pt) 
            pt = self.Movable_Tip_Span (self._pi, self.planform, 
                                        movable=True, show_ref_line=self.show_ref_line, color=COLOR_REF_LINE, 
                                        on_changed=self.sig_planform_changed.emit)
            self._add (pt) 

        # reference line 

        if self.show_ref_line:

            ref_line = self.planform.reference_line

            x,y   = ref_line.polyline()
            pen   = pg.mkPen(COLOR_REF_LINE, width=2)
            self._plot_dataItem  (y, x, name=ref_line.name, pen = pen, antialias = True, zValue=3)

            # movable points of ref line  

            if self.show_mouse_helper:
                pt = self.Movable_Ref_Root (self._pi, self.planform, 
                                            movable=True, color=COLOR_REF_LINE,
                                            on_changed=self.sig_planform_changed.emit)
                self._add (pt) 
                pt = self.Movable_Ref_Tip  (self._pi, self.planform, 
                                            movable=True, color=COLOR_REF_LINE, 
                                            on_changed=self.sig_planform_changed.emit)
                self._add (pt) 
                pt = self.Movable_Ref_Angle (self._pi, self.planform, 
                                            movable=True, color=COLOR_REF_LINE,
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
                             symbol='d', size=11, show_label_static = movable,**kwargs)


        def _point_xy (self) -> tuple: 
            """ x,y coordinates of point """
            raise NotImplementedError


        @override
        def _moving (self, _):
            """ self is moved"""

            # update reference line of planform 
            # ...

            # update point position if we run against limits 
            self.setPos(self._point_xy())                  

            # update reference line plot item
            # ...

            # create or update temp planform dashed outline 
            self._plot_tmp_planform()


        def _plot_tmp_planform (self):
            """ create or update temp planform dashed outline """

            y, x = self._planform.polyline()

            if self._tmp_planform_item is None:

                pen = pg.mkPen(QColor(COLOR_PLANFORM).darker(150), width=1, style=Qt.PenStyle.DashLine)
                p = pg.PlotDataItem  (y, x, pen = pen, antialias = False)
                p.setZValue (2)

                self._pi.addItem (p)
                self._tmp_planform_item = p

            else:

                self._tmp_planform_item.setData (y,x)


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
                    p = pg.PlotDataItem  (y, x, pen = pen, antialias = False)
                    p.setZValue (2)

                    self._pi.addItem (p)
                    self._tmp_ref_line_item = p

                else:
                    self._tmp_ref_line_item.setData (y,x)


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
            x = self._ref_line.halfwingspan / 5                 # constant x pos
            y = self._ref_line.x_at (x)                         # y pos depends on angle 
            return x,y 


        @override
        def _moving (self, _):
            """ self is moved"""
            x,y = self._point_xy()

            # update angle of reference line of planform 
            dx = x
            dy = self.y - self._ref_line.x_at(0.0)
            angle = np.arctan (dy/dx) * 180 / np.pi
            ref_line : Reference_Line = self._planform.reference_line
            ref_line.set_angle (angle)                    # update ref line angle 

            # update point position if we run against limits 
            self.setPos(self._point_xy())                  

            # update tmp reference line and planform plot item as dashed line
            self._plot_tmp_ref_line()                  
            self._plot_tmp_planform()


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
            y = self._ref_line.polyline() [0][0]
            x = self._ref_line.polyline() [1][0]
            return x,y            


        @override
        def _moving (self, _):
            """ self is moved"""
            # update angle of reference line of planform 
            self._ref_line.set_x_root (self.y)                  # update ref line a

            # update point position if we run against limits 
            self.setPos(self._point_xy())                  

            # update tmp reference line and planform plot item as dashed line
            self._plot_tmp_ref_line()                  
            self._plot_tmp_planform()


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




class Chord_Artist (Artist):
    """Plot the chord distribution  """

    sig_planform_changed     = pyqtSignal()                 # planform data changed 

    @property
    def wing (self) -> Wing: return self.data_object

    @property
    def planform (self) -> Planform: return self.wing.planform

    def _plot (self): 
    
        self._plot_title ("Chord Distribution", subTitle=None, offset=(-20,-30))
        label = f"{self.wing.name}"

        color = COLOR_CHORD
        pen   = pg.mkPen(color, width=2)
        brush = pg.mkBrush (color.darker (600))

        yn, xn = self.planform.norm_chord_line () 
        y = yn * self.planform.halfwingspan
        self._plot_dataItem  (y, xn, name=label, pen = pen, antialias = True, 
                              fillLevel=0.0, fillBrush=brush,  zValue=1)

        if self.show_mouse_helper and self.planform.planformType == "Bezier":
 
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
        
        Points are implemented with Moveable_Points
        A Moveable_Point can also be fixed ( movable=False).
        See pg.TargetItem for all arguments 
        """
        def __init__ (self, pi : pg.PlotItem, planform : Planform_Bezier, **kwargs):

            self._pi = pi
            self._planform = planform

            super().__init__(planform.controlPoints_as_jpoints (), **kwargs)


        @override
        @property
        def u (self) -> list:
            """ the Bezier parameter """
            return self._planform._norm_u_points()

        @override
        def _finished_point (self, aPoint):
            """ slot - point move is finished """

            # write back control points into original bezier 
            self._planform.controlPoints_from_jpoints (self._jpoints)

            super()._finished_point (aPoint)




class Chord_Ref_Line_Artist (Artist):
    """Plot the reference line within chord distribution"""

    @property
    def wing (self) -> Wing: return self.data_object

    @property
    def planform (self) -> Planform: return self.wing.planform

    def _plot (self): 
    
        x,y = self.planform.norm_ref_line ()
        color = QColor (COLOR_REF_LINE).darker (150)
        label = self.planform.reference_line.name

        pen = pg.mkPen(color, width=1, style=Qt.PenStyle.DashLine)

        self._plot_dataItem  (y, x, name=label, pen = pen, antialias = False, zValue=2)




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

            label = f"{planform.planformType}"

            color = COLOR_REF_ELLI
            width = 1
            pen   = pg.mkPen(color, width=width)

            y, x = planform.polyline()
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
            self._plot_dataItem  (y, xn, name=f"{planform.planformType}", pen = pen, antialias = False, zValue=1)




class WingSections_Artist (Artist):
    """Plot the wing sections of a planform """

    sig_cur_wingSection     = pyqtSignal (WingSection)          # new current wingSection 
    sig_wingSection_new     = pyqtSignal (WingSection)          # new wingsection inserted 
    sig_wingSection_changed = pyqtSignal ()                     # wingsection data changed 


    def __init__ (self, *args, cur_wingSection_fn=None, norm_chord=False, **kwargs):

        self._cur_wingSection_fn = cur_wingSection_fn           # current wingSection bound method 
        self._norm_chord = norm_chord                           # plot chord as normed

        super().__init__ (*args, **kwargs)


    @property
    def wing (self) -> Wing: return self.data_object

    @property
    def wingSections (self) -> WingSections: return self.wing.wingSections

    @property
    def planform (self) -> Planform: return self.wing.planform

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

            # plot section name 

            point_xy = (x[0],y[0]) if self.norm_chord else (x[0],y[0])  # point at le
            anchor   = (0.5,1.5) if section.isTip else (0.5,1.2)        # label anchor 

            self._plot_point (point_xy, color=color, size=0, text=section.name_short(), textColor=color, anchor=anchor)

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
        if scene:  scene.sigMouseClicked.disconnect (self._scene_clicked)


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

            self._prcossing_move = False                    # move recursion detection          

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
            if self._prcossing_move: return 
            self._prcossing_move = True 

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

            self._prcossing_move = False


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

