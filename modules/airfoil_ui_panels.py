#!/usr/bin/env pythonbutton_color
# -*- coding: utf-8 -*-

"""  

UI panels 

"""

import logging
from copy                   import copy 


from base.widgets           import * 
from base.panels            import Edit_Panel

from model.airfoil          import Airfoil, usedAs
from model.airfoil_geometry import Geometry, Geometry_Bezier, Curvature_Abstract
from model.airfoil_geometry import Line, Side_Airfoil_Bezier
from model.polar_set        import Polar_Definition
from model.case             import Case_Direct_Design

from airfoil_widgets        import * 
from airfoil_dialogs        import Match_Bezier, Matcher, Edit_Polar_Definition

logger = logging.getLogger(__name__)
# logger.setLevel(logging.DEBUG)



#-------------------------------------------------------------------------------
# Single edit panels    
#-------------------------------------------------------------------------------


class Panel_Airfoil_Abstract (Edit_Panel):
    """ 
    Abstract superclass for Edit/View-Panels of AirfoilEditor
        - has semantics of App
        - connect / handle signals 
    """

    @property
    def myApp (self):
        return self._parent 

    def airfoil (self) -> Airfoil: 

        return self.dataObject

    def geo (self) -> Geometry:
        return self.airfoil().geo
    

    def _set_panel_layout (self, layout = None ):
        """ Set layout of self._panel """
        # overloaded to connect to widgets changed signal

        super()._set_panel_layout (layout=layout)
        for w in self.widgets:
            w.sig_changed.connect (self._on_airfoil_widget_changed)
        for w in self.header_widgets:
            w.sig_changed.connect (self._on_airfoil_widget_changed)


    def _on_airfoil_widget_changed (self, widget):
        """ user changed data in widget"""
        logger.debug (f"{self} {widget} widget changed slot")
        self.myApp.sig_airfoil_changed.emit()


    @property
    def edit_mode (self) -> bool:
        """ panel in edit_mode or disabled ? - from App """
        return self.myApp.edit_mode


    @override
    @property
    def _isDisabled (self) -> bool:
        """ overloaded: only enabled in edit mode of App """
        return not self.edit_mode
    



class Panel_File_View (Panel_Airfoil_Abstract):
    """ File panel with open / save / ... """

    name = 'File'


    @property
    def _shouldBe_visible (self) -> bool:
        """ overloaded: only visible if edit_moder """
        return not self.edit_mode

    @property
    def _isDisabled (self) -> bool:
        """ override: always enabled """
        return False
    

    def _on_airfoil_widget_changed (self, *_ ):
        """ user changed data in widget"""
        # overloaded - do not react on self widget changes 
        pass

    
    def refresh (self):
        super().refresh()

    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 
        Airfoil_Select_Open_Widget (l,r,c, colSpan=2, signal=False, textOpen="&Open",
                                    get=self.airfoil, set=self.myApp.set_airfoil)
        r += 1
        SpaceR (l,r, height=5)
        r += 1
        Button (l,r,c, text="&Modify Airfoil", width=100, 
                set=self.myApp.modify_airfoil, toolTip="Modify geometry, Normalize, Repanel",
                button_style=button_style.PRIMARY)
        r += 1
        SpaceR (l,r, height=2, stretch=0)
        r += 1
        Button (l,r,c, text="&New as Bezier", width=100, 
                set=self.myApp.new_as_Bezier, disable=lambda: self.airfoil().isBezierBased,
                toolTip="Create new Bezier airfoil based on current airfoil")
        r += 1
        SpaceR (l,r, stretch=4)
        r += 1
        Button (l,r,c, text="&Exit", width=100, set=self.myApp.close)
        r += 1
        SpaceR (l,r, height=5, stretch=0)        
        l.setColumnStretch (1,2)
        l.setContentsMargins (QMargins(0, 0, 0, 0)) 

        return l 
 


class Panel_File_Edit (Panel_Airfoil_Abstract):
    """ File panel with open / save / ... """

    name = 'Edit Mode'

    @property
    def _shouldBe_visible (self) -> bool:
        """ overloaded: only visible if edit_moder """
        return self.edit_mode

    @property
    def airfoilg_org (self) -> Airfoil:
        return self.myApp.airfoil_org

    @property
    def case (self) -> Case_Direct_Design:
        return self.myApp.case


    def _init_layout (self): 

        self.set_background_color (color='deeppink', alpha=0.2)

        l = QGridLayout()
        r,c = 0, 0 
        Field (l,r,c, colSpan=3, width=180, get=lambda: self.airfoilg_org.fileName)
        r += 1
        ComboSpinBox (l,r,c, colSpan=2, width=150, get=self.airfoil_fileName, 
                             set=self.set_airfoil_by_fileName,
                             options=self.airfoil_fileNames,
                             signal=False)
        ToolButton   (l,r,c+2, icon=Icon.DELETE, set=self.remove_current_airfoil,
                      disable=lambda: len(self.case.airfoil_designs) <2)
        r += 1
        SpaceR (l,r)
        l.setRowStretch (r,2)
        r += 1
        Button (l,r,c,  text="&Finish ...", width=100, 
                        set=lambda : self.myApp.modify_airfoil_finished(ok=True), 
                        toolTip="Save current airfoil, optionally modifiy name and leave edit mode")
        r += 1
        SpaceR (l,r, height=5, stretch=0)
        r += 1
        Button (l,r,c,  text="&Cancel",  width=100, 
                        set=lambda : self.myApp.modify_airfoil_finished(ok=False),
                        toolTip="Cancel modifications of airfoil and leave edit mode")
        r += 1
        SpaceR (l,r, height=5, stretch=0)
        l.setColumnStretch (3,2)
        l.setContentsMargins (QMargins(0, 0, 0, 0)) 

        return l



    def airfoil_fileName(self) -> list[str]:
        """ fileName of current airfoil without extension"""
        return os.path.splitext(self.airfoil().fileName)[0]


    def airfoil_fileNames(self) -> list[str]:
        """ list of design airfoil fileNames without extension"""

        fileNames = []
        for airfoil in self.case.airfoil_designs:
            fileNames.append (os.path.splitext(airfoil.fileName)[0])
        return fileNames


    def set_airfoil_by_fileName (self, fileName : str):
        """ set new current design airfoil by fileName"""

        airfoil = self.case.get_design_by_name (fileName)
        self.myApp.set_airfoil (airfoil)


    def remove_current_airfoil (self):
        """ remove current design and set new current design airfoil by fileName"""

        next_airfoil = self.case.remove_design (self.airfoil())

        if next_airfoil: 
            self.myApp.set_airfoil (next_airfoil)




class Panel_Geometry (Panel_Airfoil_Abstract):
    """ Main geometry data of airfoil"""

    name = 'Geometry'
    _width  = (350, None)

    @override
    def _add_to_header_layout(self, l_head: QHBoxLayout):
        """ add Widgets to header layout"""

        l_head.addStretch(1)

        # blend with airfoil - currently Bezier is not supported
        Button (l_head, text="&Blend", width=80,
                set=self.myApp.blend_with, 
                hide=lambda: not self.edit_mode or self.airfoil().isBezierBased,
                toolTip="Blend original airfoil with another airfoil")


    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 
        FieldF (l,r,c, lab="Thickness", width=75, unit="%", step=0.2,
                obj=self.geo, prop=Geometry.max_thick,
                disable=lambda: self.airfoil().isBezierBased)
        r += 1
        FieldF (l,r,c, lab="Camber", width=75, unit="%", step=0.2,
                obj=self.geo, prop=Geometry.max_camb,
                disable=lambda: self.airfoil().isBezierBased or self.airfoil().isSymmetrical)
        r += 1
        FieldF (l,r,c, lab="TE gap", width=75, unit="%", step=0.1,
                obj=self.geo, prop=Geometry.te_gap)

        r,c = 0, 2 
        SpaceC (l,c, stretch=0)
        c += 1 
        FieldF (l,r,c, lab="at", width=75, unit="%", step=0.5,
                obj=self.geo, prop=Geometry.max_thick_x,
                disable=lambda: self.airfoil().isBezierBased)
        r += 1
        FieldF (l,r,c, lab="at", width=75, unit="%", step=0.5,
                obj=self.geo, prop=Geometry.max_camb_x,
                disable=lambda: self.airfoil().isBezierBased or self.airfoil().isSymmetrical)
        r += 1
        FieldF (l,r,c, lab="LE radius", width=75, unit="%", step=0.05,
                obj=self.geo, prop=Geometry.le_radius,
                disable=lambda: self.airfoil().isBezierBased)
        r += 1
        SpaceR (l,r)
        r += 1
        Label  (l,r,0,colSpan=4, get=lambda : "Geometry " + self.geo().description, style=style.COMMENT)

        l.setColumnMinimumWidth (0,80)
        l.setColumnMinimumWidth (3,60)
        l.setColumnStretch (5,2)
        return l 



class Panel_Panels (Panel_Airfoil_Abstract):
    """ Panelling information """

    name = 'Panels'
    _width  =  (290, None)

    def _add_to_header_layout(self, l_head: QHBoxLayout):
        """ add Widgets to header layout"""

        l_head.addStretch(1)

        # repanel airfoil - currently Bezier is not supported
        Button (l_head, text="&Repanel", width=80,
                set=self.myApp.repanel_airfoil, hide=lambda: not self.edit_mode,
                disable=lambda: self.geo().isBasic or self.geo().isHicksHenne,
                toolTip="Repanel airfoil with a new number of panels" ) 


    def _init_layout (self):

        l = QGridLayout()

        r,c = 0, 0 
        FieldI (l,r,c, lab="No of panels", disable=True, width=70, style=self._style_panel,
                get=lambda: self.geo().nPanels, )
        r += 1
        FieldF (l,r,c, lab="Angle at LE", width=70, dec=1, unit="°", style=self._style_angle,
                obj=self.geo, prop=Geometry.panelAngle_le)
        SpaceC (l,c+2, width=10, stretch=0)
        Label  (l,r,c+3,width=70, get=lambda: f"at index {self.geo().iLe}")
        r += 1
        FieldF (l,r,c, lab="Angle min", width=70, dec=1, unit="°",
                get=lambda: self.geo().panelAngle_min[0], )
        r += 1
        SpaceR (l,r,height=5)
        r += 1
        Label  (l,r,0,colSpan=4, get=self._messageText, style=style.COMMENT, height=(None,None))

        l.setColumnMinimumWidth (0,80)
        l.setColumnStretch (c+4,1)
        l.setRowStretch    (r-1,2)
        
        return l
 
      
    def refresh(self):
        super().refresh()


    def _on_panelling_finished (self, aSide : Side_Airfoil_Bezier):
        """ slot for panelling (dialog) finished - reset airfoil"""


    def _style_panel (self):
        """ returns style.WARNING if panels not in range"""
        if self.geo().nPanels < 120 or self.geo().nPanels > 260: 
            return style.WARNING
        else: 
            return style.NORMAL


    def _style_angle (self):
        """ returns style.WARNING if panel angle too blunt"""
        if self.geo().panelAngle_le > 175.0: 
            return style.WARNING
        else: 
            return style.NORMAL

    def _messageText (self): 

        text = []
        minAngle, _ = self.geo().panelAngle_min

        if self.geo().panelAngle_le > 175.0: 
            text.append("- Panel angle at LE (%d°) is too blunt" %(self.geo().panelAngle_le))
        if minAngle < 150.0: 
            text.append("- Min. angle of two panels is < 150°")
        if self.geo().panelAngle_le == 180.0: 
            text.append("- Leading edge has 2 points")
        if self.geo().nPanels < 100 or self.geo().nPanels > 200: 
            text.append("- No of panels should be > 100 and < 200")
        
        text = '\n'.join(text)
        return text 



class Panel_LE_TE  (Panel_Airfoil_Abstract):
    """ info about LE and TE coordinates"""

    name = 'LE, TE'

    _width  = 320

    @property
    def _shouldBe_visible (self) -> bool:
        """ overloaded: only visible if geo is not Bezier """
        return not (self.geo().isBezier and self.edit_mode)


    def _add_to_header_layout(self, l_head: QHBoxLayout):
        """ add Widgets to header layout"""

        l_head.addStretch(1)
        Button (l_head, text="&Normalize", width=80,
                set=lambda : self.airfoil().normalize(), signal=True, 
                hide=lambda: not self.edit_mode,
                toolTip="Normalize airfoil to get leading edge at 0,0")


    def _init_layout (self): 

        l = QGridLayout()     
        r,c = 0, 0 
        FieldF (l,r,c, lab="Leading edge", get=lambda: self.geo().le[0], width=75, dec=7, style=lambda: self._style (self.geo().le[0], 0.0))
        r += 1
        FieldF (l,r,c, lab=" ... of spline", get=lambda: self.geo().le_real[0], width=75, dec=7, style=self._style_le_real,
                hide=lambda: not self.edit_mode)
        r += 1
        FieldF (l,r,c, lab="Trailing edge", get=lambda: self.geo().te[0], width=75, dec=7, style=lambda: self._style (self.geo().te[0], 1.0))
        r += 1
        FieldF (l,r,c+1,get=lambda: self.geo().te[2], width=75, dec=7, style=lambda: self._style (self.geo().te[0], 1.0))

        r,c = 0, 2 
        SpaceC (l,c, width=10, stretch=0)
        c += 1 
        FieldF (l,r,c+1,get=lambda: self.geo().le[1], width=75, dec=7, style=lambda: self._style (self.geo().le[1], 0.0))
        r += 1
        FieldF (l,r,c+1,get=lambda: self.geo().le_real[1], width=75, dec=7, style=self._style_le_real,
                hide=lambda: not self.edit_mode)
        r += 1
        FieldF (l,r,c+1,get=lambda: self.geo().te[1], width=75, dec=7, style=lambda: self._style (self.geo().te[1], -self.geo().te[3]))
        r += 1
        FieldF (l,r,c+1,get=lambda: self.geo().te[3], width=75, dec=7, style=lambda: self._style (self.geo().te[3], -self.geo().te[1]))
        # SpaceC (l,c+2)

        r += 1
        SpaceR (l,r, height=5)
        r += 1
        Label  (l,r,0,colSpan=4, get=self._messageText, style=style.COMMENT, height=(None,None))

        l.setColumnMinimumWidth (0,80)
        # l.setColumnStretch (0,1)
        l.setColumnStretch (c+3,1)
        l.setRowStretch    (r-1,2)
        return l


    def _style_le_real (self):
        """ returns style.WARNING if LE spline isn't close to LE"""
        if self.geo().isLe_closeTo_le_real: 
            if self.geo().isBasic:
                return style.NORMAL
            else: 
                return style.NORMAL
        else: 
            return style.WARNING


    def _style (self, val, target_val):
        """ returns style.WARNING if val isn't target_val"""
        if val != target_val: 
            return style.WARNING
        else: 
            return style.NORMAL


    def _messageText (self): 

        text = []
        if not self.geo().isNormalized:
            if self.geo().isSplined and not self.geo().isLe_closeTo_le_real:
                text.append("- Leading edge of spline is not at 0,0")
            else: 
                text.append("- Leading edge is not at 0,0")
        if self.geo().te[0] != 1.0 or self.geo().te[2] != 1.0 : 
           text.append("- Trailing edge is not at 1")
        if self.geo().te[1] != -self.geo().te[3]: 
           text.append("- Trailing not symmetric")

        if not text:
            if self.geo().isSymmetrical: 
                text.append("Airfoil is symmetrical")
            else: 
                text.append("Airfoil is normalized")

        text = '\n'.join(text)
        return text 



class Panel_Bezier (Panel_Airfoil_Abstract):
    """ Info about Bezier curves upper and lower  """

    name = 'Bezier'
    _width  = (180, None)


    # ---- overloaded 

    @property
    def _shouldBe_visible (self) -> bool:
        """ overloaded: only visible if geo is Bezier """
        return self.geo().isBezier
    
    # ----

    def geo (self) -> Geometry_Bezier:
        return super().geo()

    @property
    def upper (self) -> Side_Airfoil_Bezier:
        if self.geo().isBezier:
            return self.geo().upper

    @property
    def lower (self) -> Side_Airfoil_Bezier:
        if self.geo().isBezier:
            return self.geo().lower


    def _init_layout (self):

        l = QGridLayout()

        r,c = 0, 0 
        Label (l,r,c+1, get="Points")

        r += 1
        FieldI (l,r,c,   lab="Upper side", get=lambda: self.upper.nControlPoints,  width=50, step=1, lim=(3,10),
                         set=lambda n : self.geo().set_nControlPoints_of (self.upper, n))
        r += 1
        FieldI (l,r,c,   lab="Lower side",  get=lambda: self.lower.nControlPoints,  width=50, step=1, lim=(3,10),
                         set=lambda n : self.geo().set_nControlPoints_of (self.lower, n))

        r += 1
        SpaceR (l,r, height=10, stretch=2)
        l.setColumnMinimumWidth (0,70)
        l.setColumnStretch (c+2,4)
        
        return l
 


class Panel_Bezier_Match (Panel_Airfoil_Abstract):
    """ Match Bezier functions  """

    name = 'Bezier Match'
    _width  = (370, None)



    @override
    @property
    def _shouldBe_visible (self) -> bool:
        """ overloaded: only visible if geo is Bezier """
        return self.geo().isBezier and self.edit_mode

    # ----

    @property
    def upper (self) -> Side_Airfoil_Bezier:
        if self.geo().isBezier: return self.geo().upper

    @property
    def lower (self) -> Side_Airfoil_Bezier:
        if self.geo().isBezier: return self.geo().lower

    @property
    def curv_upper (self) -> Line:
        if self.geo().isBezier:
            return self.geo().curvature.upper

    @property
    def curv_lower (self) -> Line:
        if self.geo().isBezier:
            return self.geo().curvature.lower

    @property
    def curv (self) -> Curvature_Abstract:
        return self.geo().curvature

    @property
    def target_airfoil (self) -> Airfoil:
        return self.myApp.airfoil_target

    @property
    def target_upper (self) -> Line:
        if self.target_airfoil: return self.target_airfoil.geo.upper

    @property
    def target_lower (self) -> Line:
        if self.target_airfoil: return self.target_airfoil.geo.lower

    @property
    def target_curv_le (self) -> float:
        return self.target_airfoil.geo.curvature.best_around_le

    @property
    def max_curv_te_upper (self) -> Line:
        if self.target_airfoil: return self.target_airfoil.geo.curvature.at_upper_te

    @property
    def max_curv_te_lower (self) -> Line:
        if self.target_airfoil: return self.target_airfoil.geo.curvature.at_lower_te

    def norm2_upper (self): 
        """ norm2 deviation of airfoil to target - upper side """
        if self._norm2_upper is None: 
            self._norm2_upper = Matcher.norm2_deviation_to (self.upper.bezier, self.target_upper) 
        return  self._norm2_upper    


    def norm2_lower (self): 
        """ norm2 deviation of airfoil to target  - upper side """
        if self._norm2_lower is None: 
            self._norm2_lower = Matcher.norm2_deviation_to (self.lower.bezier, self.target_lower)  
        return self._norm2_lower


    # def _add_to_header_layout(self, l_head: QHBoxLayout):
    #     """ add Widgets to header layout"""

    #     l_head.addSpacing (20)
  
    #     Airfoil_Select_Open_Widget (l_head, width=(100,200),
    #                 get=lambda: self.myApp.airfoil_target, set=self.myApp.set_airfoil_target,
    #                 initialDir=self.airfoil(), addEmpty=True)


    # def __init__ (self,*args, **kwargs):
    #     super().__init__(*args,**kwargs)

    #     self.myApp.sig_airfoil_target_changed.connect(self._on_airfoil_target_changed)


    def _init_layout (self):

        self._norm2_upper = None                                # cached value of norm2 deviation 
        self._norm2_lower = None                                # cached value of norm2 deviation 
        self._target_curv_le = None 
        self._target_curv_le_weighting = None

        l = QGridLayout()

        if self.target_airfoil is not None: 

            self._target_curv_le = self.target_airfoil.geo.curvature.best_around_le 

            r,c = 0, 0 
            Label  (l,r,c+1, get="Deviation", width=70)

            r += 1
            Label  (l,r,c,   get="Upper Side")
            FieldF (l,r,c+1, width=60, dec=3, unit="%", get=self.norm2_upper,
                             style=lambda: Match_Bezier.style_deviation (self.norm2_upper()))
            r += 1
            Label  (l,r,c,   get="Lower Side")
            FieldF (l,r,c+1, width=60, dec=3, unit="%", get=self.norm2_lower,
                             style=lambda: Match_Bezier.style_deviation (self.norm2_lower()))

            r,c = 0, 2 
            SpaceC(l,  c, width=5)
            c += 1
            Label (l,r,c, colSpan=2, get="LE curvature TE")
    
            r += 1
            FieldF (l,r,c  , get=lambda: self.curv_upper.max_xy[1], width=40, dec=0, 
                    style=lambda: Match_Bezier.style_curv_le(self._target_curv_le, self.curv_upper))
            FieldF (l,r,c+1, get=lambda: self.curv_upper.te[1],     width=40, dec=1, 
                    style=lambda: Match_Bezier.style_curv_te(self.max_curv_te_upper, self.curv_upper))

            r += 1
            FieldF (l,r,c  , get=lambda: self.curv_lower.max_xy[1], width=40, dec=0, 
                    style=lambda: Match_Bezier.style_curv_le(self._target_curv_le, self.curv_lower))
            FieldF (l,r,c+1, get=lambda: self.curv_lower.te[1],     width=40, dec=1, 
                    style=lambda: Match_Bezier.style_curv_te(self.max_curv_te_lower, self.curv_lower))

            r,c = 0, 5 
            SpaceC (l,  c, width=10)
            c += 1
            r += 1
            Button (l,r,c  , text="Match...", width=70,
                            set=lambda: self._match_bezier (self.upper, self.target_upper, 
                                                            self.target_curv_le, self.max_curv_te_upper))
            r += 1
            Button (l,r,c  , text="Match...", width=70,
                            set=lambda: self._match_bezier (self.lower, self.target_lower, 
                                                            self.target_curv_le, self.max_curv_te_lower))
            c = 0 
            r += 1
            SpaceR (l,r, height=5, stretch=2)
            r += 1
            Label  (l,r,0, get=self._messageText, colSpan=7, height=(40, None), style=style.COMMENT)
            l.setColumnMinimumWidth (0,70)
            l.setColumnStretch (c+6,2)

        else: 
            SpaceR (l,0)
            Label  (l,1,0, get="Select a target airfoil to match...", style=style.COMMENT)
            SpaceR (l,2, stretch=2)
        return l
 

    def _match_bezier (self, aSide : Side_Airfoil_Bezier, aTarget_line : Line, 
                            target_curv_le: float, max_curv_te : float  ): 
        """ run match bezier (dialog) """ 

        matcher = Match_Bezier (self.myApp, aSide, aTarget_line,
                                target_curv_le = target_curv_le,
                                max_curv_te = max_curv_te)

        matcher.sig_new_bezier.connect     (self.myApp.sig_bezier_changed.emit)
        matcher.sig_match_finished.connect (self._on_match_finished)

        # leave button press callback 
        timer = QTimer()                                
        timer.singleShot(10, lambda: matcher.exec())     # delayed emit 
       


    def _on_match_finished (self, aSide : Side_Airfoil_Bezier):
        """ slot for match Bezier finished - reset airfoil"""

        geo : Geometry_Bezier = self.geo()
        geo.finished_change_of (aSide)              # will reset and handle changed  

        self.myApp.sig_airfoil_changed.emit()


    # def _on_airfoil_target_changed (self,refresh):
    #     """ slot for changed target airfoil"""  
    #     if refresh:      
    #         self.refresh(reinit_layout=True)              # refresh will also set new layout 

    @override
    def refresh (self, reinit_layout=False):

        # reset cached deviations
        self._norm2_lower = None
        self._norm2_upper = None 
        super().refresh(reinit_layout)
        

    def _messageText (self): 
        """ user warnings"""
        text = []
        r_upper_dev = Matcher.result_deviation (self.norm2_upper())
        r_lower_dev = Matcher.result_deviation (self.norm2_lower())

        r_upper_le = Matcher.result_curv_le (self._target_curv_le, self.curv_upper)
        r_lower_le = Matcher.result_curv_le (self._target_curv_le, self.curv_lower)
        r_upper_te = Matcher.result_curv_te (self.max_curv_te_upper,self.curv_upper)
        r_lower_te = Matcher.result_curv_te (self.max_curv_te_lower, self.curv_lower)

        is_bad = Matcher.result_quality.BAD
        if r_upper_dev == is_bad or r_lower_dev == is_bad:
           text.append("- Deviation is quite high")
        if r_upper_le == is_bad or r_lower_le == is_bad:
           text.append(f"- Curvature at LE differs too much from target ({int(self._target_curv_le)})")
        if r_upper_te == is_bad or r_lower_te == is_bad:
           text.append("- Curvature at TE is quite high")

        text = '\n'.join(text)
        return text 




class Panel_Polar_Defs (Edit_Panel):
    """ Panel to add, delete, edit polar definitions """

    name = None                                         # suppress header

    _panel_margins = (0, 0, 0, 0)                       # no inset of panel data 
    _main_margins  = (0, 0, 0, 0)                       # margins of Edit_Panel

    sig_polar_def_changed = pyqtSignal()                # polar definition changed 

    MAX_POLAR_DEFS = 5

    # ---------------------------------------------

    @property
    def polar_defs (self) -> list[Polar_Definition]: 
        return self.dataObject

    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 

        for idef, polar_def in enumerate (self.polar_defs):

            #https://docs.python.org/3.4/faq/programming.html#why-do-lambdas-defined-in-a-loop-with-different-values-all-return-the-same-result
            w = CheckBox   (l,r,c  , width=20,  get=lambda p=polar_def: p.active, set=polar_def.set_active)
            w.sig_changed.connect (self._on_polar_def_changed)
            Field      (l,r,c+1, width=(80,None), get=lambda p=polar_def: p.name)

            ToolButton (l,r,c+2, icon=Icon.EDIT,   set=self.edit_polar_def,   id=idef)
            ToolButton (l,r,c+3, icon=Icon.DELETE, set=self.delete_polar_def, id=idef,
                        hide=lambda: len(self.polar_defs) <= 1)
            r += 1

        if len (self.polar_defs) < self.MAX_POLAR_DEFS:
            ToolButton (l,r,c+1, icon=Icon.ADD,   set=self.add_polar_def)

        l.setColumnStretch (c+1,2)

        return l 


    def edit_polar_def (self, id : int):
        """ edit polar definition with index idef"""

        diag = Edit_Polar_Definition (self, self.polar_defs[id])
        diag.exec()

        # sort polar definitions ascending re number 
        self.polar_defs.sort (key=lambda aDef : aDef.re)

        self._on_polar_def_changed ()


    def delete_polar_def (self, id : int):
        """ delete polar definition with index idef"""

        # at least one polar def needed
        if len(self.polar_defs) <= 1: return 

        del self.polar_defs[id]

        self._on_polar_def_changed ()


    def add_polar_def (self):
        """ add a new polar definition"""

        # increase re number for the new polar definition
        if self.polar_defs:
            new_polar_def  = copy (self.polar_defs[-1])
            new_polar_def.set_re (new_polar_def.re + 100000)
            new_polar_def.set_active(True)
        else: 
            new_polar_def = Polar_Definition()

        self.polar_defs.append (new_polar_def)

        # open edit dialog for new def 

        self.edit_polar_def (len(self.polar_defs)-1)


    def _on_polar_def_changed (self):
        """ handle changed polar def - inform parent"""

        # ensure if only 1 polardef, this has to be active 
        if len(self.polar_defs) == 1 and not self.polar_defs[0].active:
            self.polar_defs[0].set_active(True)

        # signal parent - which has to refresh self to apply changed items 
        self.sig_polar_def_changed.emit()


    @override
    def refresh(self, **_):
        """ refreshes all Widgets on self """

        # refresh has to reinit layout for new/deleted items 
        super().refresh (reinit_layout=True)




class Panel_Airfoils (Edit_Panel):
    """ 
    Panel to show active airfoils 
    - add, delete, edit reference airfoils
    
    """

    name = "Airfoils"   

    sig_airfoil_ref_changed      = pyqtSignal(object, object)    # changed reference airfoil 
    sig_airfoils_to_show_changed = pyqtSignal()                  # changed show filter 


    def __init__(self, *args, **kwargs):

        self._n_airfoils  = 0                       # for change detection

        super().__init__(*args, **kwargs)

    # ---------------------------------------------

    @property
    def airfoils (self) -> list[Airfoil]: 
        return self.dataObject


    def _n_REF (self) -> int:
        """ number of reference airfoils"""
        n = 0 
        for airfoil in self.airfoils:
            if airfoil.usedAs == usedAs.REF: n += 1
        return n


    def _DESIGN_in_list (self) -> bool:
        """ true if NORMAL airfoil can be switched on/off"""
        for airfoil in self.airfoils:
            if airfoil.usedAs == usedAs.DESIGN: 
                return False
        return True


    @override
    def _init_layout (self): 

        self._n_airfoils  = len(self.airfoils)                  # for change dection

        l = QGridLayout()
        r,c = 0, 0 
        iRef = 0

        for iair, airfoil in enumerate (self.airfoils):

            #https://docs.python.org/3.4/faq/programming.html#why-do-lambdas-defined-in-a-loop-with-different-values-all-return-the-same-result

            # if airfoil.usedAs == usedAs.DESIGN :
            #     CheckBox    (l,r,c  , width=20, get=self.show_airfoil, set=self.set_show_airfoil, id=iair,
            #                  disable=True)
            #     Field       (l,r,c+1, width=155, get=lambda :self.airfoil(iair).fileName,  
            #                  toolTip=f"Design airfoil {airfoil.name}")
            #     r += 1

            if airfoil.usedAs == usedAs.NORMAL :
                CheckBox    (l,r,c  , width=20, get=self.show_airfoil, set=self.set_show_airfoil, id=iair,
                             disable=lambda: self._DESIGN_in_list())
                Field       (l,r,c+1, width=155, get=lambda i=iair:self.airfoil(i).fileName, 
                             toolTip=f"Original airfoil {airfoil.name}")
                r += 1

            if airfoil.usedAs == usedAs.TARGET:
                CheckBox    (l,r,c  , width=20, get=self.show_airfoil, set=self.set_show_airfoil, id=iair)
                Field       (l,r,c+1, width=155, get=lambda i=iair:self.airfoil(i).fileName, 
                             toolTip=f"Target airfoil {airfoil.name}")
                r += 1

        Label (l,r,c, colSpan=4, get="Reference airfoils", style=style.COMMENT) 
        r += 1

        for iair, airfoil in enumerate (self.airfoils):

            if airfoil.usedAs == usedAs.REF:
                iRef += 1
                CheckBox   (l,r,c  , width=20, get=self.show_airfoil, set=self.set_show_airfoil, id=iair)

                Airfoil_Select_Open_Widget (l,r,c+1, widthOpen=60,
                                get=self.airfoil, set=self.set_airfoil, id=iair,
                                initialDir=self.airfoils[0], addEmpty=False,
                                toolTip=f"Reference airfoil {iRef}")

                ToolButton (l,r,c+2, icon=Icon.DELETE, set=self.delete_airfoil, id=iair)
                r += 1

        # add new reference as long as < max REF airfoils 
        if self._n_REF() < 3:
            Airfoil_Select_Open_Widget (l,r,c+1, widthOpen=60,
                            get=None, set=self.set_airfoil, id=iair+1,
                            initialDir=self.airfoils[0], addEmpty=True,
                            toolTip=f"New reference airfoil {iRef+1}")
            r +=1
        SpaceR (l,r,stretch=0)

        l.setColumnMinimumWidth (c  ,ToolButton._width)
        l.setColumnMinimumWidth (c+2,ToolButton._width)
        l.setColumnStretch (c+3,2)

        return l 


    def airfoil (self, id : int):
        """ get airfoil with index id from list"""
        return self.airfoils[id]

    def set_airfoil (self, new_airfoil : Airfoil|None = None, id : int = None):
        """ set airfoil with index id from list"""

        if new_airfoil is None: return

        if id < len(self.airfoils): 
            cur_airfoil = self.airfoils[id]
        else: 
            cur_airfoil = None                                  # will add new_airfoil 
        self.sig_airfoil_ref_changed.emit(cur_airfoil, new_airfoil)


    def show_airfoil (self, id : int) -> bool:
        """ is ref airfoil with id active"""
        return self.airfoils[id].get_property ("show", True)

    def set_show_airfoil (self, aBool, id : int):
        """ set ref airfoil with index id active"""
        self.airfoils[id].set_property ("show", aBool)
        self.sig_airfoils_to_show_changed.emit()


    def delete_airfoil (self, id : int):
        """ delete ref airfoil with index idef from list"""

        if len(self.airfoils) == 0: return 

        airfoil = self.airfoils[id]

        # only REF airfoils can be deleted 
        if airfoil.usedAs == usedAs.REF:
            self.sig_airfoil_ref_changed.emit (airfoil, None)


    def add_airfoil_ref (self):
        """ add a new ref airfoil"""

        self.airfoils.append (None)

        self._on_airfoil_list_changed ()


    @override
    def refresh (self, reinit_layout=None):
        """ refreshes all Widgets on self """

        if len (self.airfoils) != self._n_airfoils:
            # rebuild layout with new airfoil entries 
            super().refresh (reinit_layout=True)
        else: 
            # normall refresh of widgets
            super().refresh()

