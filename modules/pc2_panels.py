#!/usr/bin/env pythonbutton_color
# -*- coding: utf-8 -*-

"""  

UI panels 

"""

import logging

from PyQt6.QtWidgets        import QDialog
from PyQt6.QtWidgets        import QTabWidget, QMessageBox

from base.widgets           import * 
from base.panels            import Edit_Panel, Container_Panel, MessageBox
from model.airfoil          import Airfoil, GEO_BASIC 

from airfoil_widgets        import Airfoil_Open_Widget

from wing                   import Wing, Planform, WingSections, WingSection, Flaps
from wing                   import N_Distrib_Abstract, N_Chord_Reference, N_Reference_Line

from pc2_dialogs            import Dialog_TextEdit

from AirfoilEditor_subtree.AirfoilEditor import App_Main

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)



#-------------------------------------------------------------------------------
# Tab panel    
#-------------------------------------------------------------------------------



class Tab_Panel (QTabWidget):
    """ 
    Tab Widget as parent for other items 
    """

    name = "Panel"             # will be title 

    _width  = None
    _height = None 


    def __init__(self,  
                 parent=None,
                 width=None, 
                 height=None, 
                 **kwargs):
        super().__init__(parent=parent, **kwargs)

        self._parent = parent

        if width  is not None: self._width = width
        if height is not None: self._height = height

        # set width and height 
        Widget._set_width  (self, self._width)
        Widget._set_height (self, self._height)

        font = self.font() 
        _font = size.HEADER.value
        font.setPointSize(_font[0])
        font.setWeight   (_font[1])  
        self.setFont(font)

        # see https://doc.qt.io/qt-6/stylesheet-examples.html

        if Widget.light_mode:
            tab_style = """
            QTabWidget::pane { /* The tab widget frame */
                border-top:1px solid #ababab;
            }

            QTabWidget::tab-bar {
                left: 400px; /* move to the right by 5px */
            }

            /* Style the tab using the tab sub-control. Note that
                it reads QTabBar _not_ QTabWidget */
            QTabBar::tab {
                /*background: green; */
                border: 1px solid #C4C4C3;
                border-bottom: 0px;                                     /*remove */
                border-top-left-radius: 3px;
                border-top-right-radius: 3px;
                min-width: 25ex;
                padding: 6px;
            }

            QTabBar::tab:!selected {
                margin-top: 2px; /* make non-selected tabs look smaller */
                background: #e5e5e5
            }
                            
            QTabBar::tab:hover {
                background: rgba(255, 255, 255, 0.2) /* rgba(255, 20, 147, 0.1); */              
            }

            QTabBar::tab:selected {
                background: rgba(255, 255, 255, 0.9) /* background: rgba(255, 20, 147, 0.2); */               
            }

            QTabBar::tab:selected {
                /*color: white; */
                color: #303030;
                font-weight: 600;
                border-color: #9B9B9B;
                border-bottom-color: #C2C7CB; /* same as pane color */
            }
            """
 
        else: 

            tab_style = """
            QTabWidget::pane { /* The tab widget frame */
                border-top:1px solid #505050;
            }

            QTabWidget::tab-bar {
                left: 400px; /* move to the right by 5px */
            }

            /* Style the tab using the tab sub-control. Note that
                it reads QTabBar _not_ QTabWidget */
            QTabBar::tab {
                /*background: green; */
                border: 1px solid #505050;  
                border-bottom: 0px;                                     /*remove */
                border-top-left-radius: 3px;
                border-top-right-radius: 3px;
                min-width: 25ex;
                padding: 6px;
            }

            QTabBar::tab:!selected {
                margin-top: 2px; /* make non-selected tabs look smaller */
                color: #D0D0D0;
                background: #353535
            }
                            
            QTabBar::tab:hover {
                background: rgba(255, 255, 255, 0.2) /* rgba(255, 20, 147, 0.1); */             
            }

            QTabBar::tab:selected {
                background: rgba(77, 77, 77, 0.9) /* background: rgba(255, 20, 147, 0.2); */                   
            }

            QTabBar::tab:selected {
                /*color: white; */
                color: #E0E0E0;
                font-weight: 600;
                border-color: #909090;
                border-bottom-color: #C2C7CB;   /* same as pane color */
            }
            """


        self.setStyleSheet (tab_style) 


    def __repr__(self) -> str:
        # overwritten to get a nice print string 
        return f"<Tab_Panel '{self.name}'>"


    def add_tab (self, aWidget : QWidget, name : str = None):
        """ at an item having 'name' to self"""

        if name is None:
            name = aWidget.name

        self.addTab (aWidget, name)


    def set_tab (self, class_name : str):
        """ set the current tab to tab with widgets class name"""

        for itab in range (self.count()):
            if self.widget(itab).__class__.__name__ == class_name:
                self.setCurrentIndex (itab)
                return


    def set_background_color (self, darker_factor : int | None = None,
                                    color : QColor | int | None  = None,
                                    alpha : float | None = None):
        """ 
        Set background color of a QWidget either by
            - darker_factor > 100  
            - color: QColor or string for new color
            - alpha: transparency 0..1 
        """
        set_background (self, darker_factor=darker_factor, color=color, alpha=alpha)



#-------------------------------------------------------------------------------
# Single edit panels    
#-------------------------------------------------------------------------------


class Panel_Planform_Abstract (Edit_Panel):
    """ 
    Abstract superclass for Edit/View-Panels of PlanformCreator2
        - has semantics of App
        - connect / handle signals 
    """

    # import here to avoid circular import errors 
    from PlanformCreator2   import App_Main


    @property
    def myApp (self) -> App_Main:
        return self._parent 

    def wing (self) -> Wing: 
        return self.dataObject

    def planform (self) -> Planform:
        return self.wing().planform
    
    def n_chord (self) -> N_Distrib_Abstract:
        return self.wing().planform.n_distrib

    def n_chord_ref (self) -> N_Chord_Reference:
        return self.wing().planform.n_chord_ref
    
    def n_ref_line (self) -> N_Reference_Line:
        return self.wing().planform.n_ref_line
    
    def flaps (self) -> Flaps:
        return self.wing().planform.flaps

    def wingSections (self) -> WingSections:
        return self.wing().planform.wingSections
    

    def _set_panel_layout (self, layout = None ):
        """ Set layout of self._panel """

        super()._set_panel_layout (layout=layout)

        # overloaded to connect to widgets changed signal
        for w in self.widgets:
            w.sig_changed.connect (self._on_widget_changed)
        for w in self.header_widgets:
            w.sig_changed.connect (self._on_widget_changed)


    def _on_widget_changed (self, widget):
        """ user changed data in widget"""
        logger.debug (f"{self} {widget} widget changed slot")
        self.myApp.sig_planform_changed.emit()




class Panel_File (Panel_Planform_Abstract):
    """ File panel with open / save / ... """

    name = 'File'
    
    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 
        Button (l,r,c, text="&Open", width=100, 
                set=self.myApp.open, toolTip="Open new Planform",
                button_style=button_style.PRIMARY)
        Button (l,r,c+1, text="&New", width=60, 
                set=self.myApp.new, toolTip="Create new Planform")
        
        r += 1
        SpaceR (l,r, height=2, stretch=0)
        r += 1
        Button (l,r,c, text="&Save", width=100, 
                set=self.myApp.save, toolTip="Save Planform to parameter file")
        Button (l,r,c+1, text="Save As", width=60, 
                set=self.myApp.saveAs, toolTip="Save Planform to new parameter file")
        r += 1
        SpaceR (l,r, height=28, stretch=0)
        r += 1
        Button (l,r,c, text="&Exit", width=100, set=self.myApp.close)
        r += 1
        SpaceR (l,r, height=5, stretch=2)        
        l.setColumnStretch (1,2)
        l.setContentsMargins (QMargins(0, 0, 0, 0)) 

        return l 
 


class Panel_Wing (Panel_Planform_Abstract):
    """ Main geometry data of wing"""

    name = 'Wing'
    _width  = (420, None)

    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 
        Field  (l,r,c, lab="Name", colSpan=3,
                obj=self.wing, prop=Wing.name)
        Button (l,r,c+4, text="Descr", width=75, set=self._edit_description)
        r += 1
        FieldF (l,r,c, lab="Wing Span", width=85, unit="mm", step=10, lim=(10, 20000), dec=0,
                obj=self.wing, prop=Wing.wingspan)
        FieldF (l,r,c+3, lab="Fuselage Width", width=75, unit="mm", step=10, lim=(0, 1000), dec=0,
                obj=self.wing, prop=Wing.fuselage_width)
        r += 1
        FieldF (l,r,c, lab="Half Wing Span", width=85, unit="mm", step=10, lim=(10, 10000), dec=0, 
                obj=self.planform, prop=Planform.span)
        r += 1
        FieldF (l,r,c, lab="Root Chord", width=85, unit="mm", step=10, lim=(10, 1000), dec=0, 
                obj=self.planform, prop=Planform.chord_root)
        FieldF (l,r,c+3, lab="Sweep Angle", width=75, unit="°", step=0.1, lim=(-45, 45), dec=1,
                obj=self.planform, prop=Planform.sweep_angle)
        r += 1
        SpaceR (l,r, height=5, stretch=2)        

        l.setColumnMinimumWidth (0,95)
        l.setColumnMinimumWidth (3,90)
        l.setColumnStretch (2,1)
        l.setColumnStretch (5,2)
        return l 


    def _edit_description (self):
        """ open little text editor to edit description"""

        dialog = Dialog_TextEdit (self, self.wing().description, title="Description of Wing", dx=200, dy=-250)
        dialog.exec () 

        if dialog.result() == QDialog.DialogCode.Accepted:
            self.wing().set_description (dialog.new_text)
            self._on_widget_changed (dialog)                   # manual refresh a dialog is not a 'Widget'


class Panel_Chord_Reference (Panel_Planform_Abstract):
    """ Main geometry data of wing"""

    name    = 'Chord Distribution and Reference'
    _width  = (370, None)

    def _init_layout (self): 
        l = QGridLayout()
        r,c = 0, 0 
        Label  (l,r,c,   get="Chord Distribution")
        Label  (l,r,c+1, get=lambda: self.n_chord().name, fontSize=size.HEADER_SMALL)  
        r += 1
        FieldF (l,r,c,   lab="Reference at Root", width=70, unit="%", step=1, lim=(0,100), dec=1,
                obj=self.n_chord_ref, prop=N_Chord_Reference.cr_root)
        FieldF (l,r,c+3, lab="at Tip", width=70, unit="%", step=1, lim=(0,100), dec=1,
                obj=self.n_chord_ref, prop=N_Chord_Reference.cr_tip)
        r += 1
        CheckBox (l,r,c, text="Reference line is a curve (banana)", colSpan=5,
                obj=self.n_ref_line, prop=N_Reference_Line.is_banana,
                hide=lambda: not self.planform().n_distrib.isBezier)    # banana only for Bezier
        r += 1
        CheckBox (l,r,c, text="Flaps hinge line equals Reference line", colSpan=5,  
                  obj=self.flaps, prop=Flaps.hinge_equal_ref_line)     
        r += 1
        SpaceR (l,r, height=5, stretch=2)        

        l.setColumnMinimumWidth (0,110)
        l.setColumnMinimumWidth (2,15)
        l.setColumnMinimumWidth (3,50)
        l.setColumnStretch (5,2)
        return l 




class Panel_Flaps_Hingeline (Panel_Planform_Abstract):
    """ Main geometry data of wing"""

    name = 'Flaps Hinge line'
    _width  = (250, None)

    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 
        CheckBox (l,r,c, text="Hinge line equals reference line",  
                  obj=self.flaps, prop=Flaps.hinge_equal_ref_line)
        r += 1
        SpaceR (l,r, height=5, stretch=2)        

        # l.setColumnMinimumWidth (0,60)
        l.setColumnStretch (2,2)
        return l 



class Panel_WingSection (Panel_Planform_Abstract):
    """ Main geometry data of wing"""

    name = 'Wing Section'
    _width  = (680, None)

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        # ensure header is not disabled (for section is_for_paneling)
        for w in self.header_widgets:
            w.refresh (False)

    @override
    @property
    def _isDisabled (self) -> bool:
        """ overloaded: disabled if section is just for paneling """
        return self._wingSection().is_for_panels


    @override
    def _add_to_header_layout(self, l_head: QHBoxLayout):
        """ add Widgets to header layout"""

        l_head.addSpacing (15) 
        # l_head.addStretch(1)

        ComboBox     (l_head, width=55,
                      get=self._wingSection_name, set=self._set_wingSection_name,
                      options= self._wingSections_list, signal=False )      # do not signal (would be double refresh)
        ToolButton   (l_head, icon=Icon.ADD,    set=self._add_wingSection,
                      disable=lambda: self._wingSection().is_tip,
                      toolTip='Add a new wing section')
        ToolButton   (l_head, icon=Icon.DELETE, set=self._delete_wingSection,
                      disable=lambda: self._wingSection().is_root_or_tip,
                      toolTip='Delete selected wing section')
        l_head.addStretch(3)


    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 

        # 1. column - section settings and airfoil 

        Label  (l,r,c, get=self._section_info, colSpan=5, 
                style=lambda: style.HINT if self._wingSection().is_for_panels else style.COMMENT,
                hide = lambda: self.planform().chord_defined_by_sections) 
        CheckBox (l,r,c, text="Section defines chord", colSpan=5,               # toggle trapezoid or not 
                obj=self._wingSection, prop=WingSection.defines_cn,
                disable=lambda: self._wingSection().is_root_or_tip,
                hide=lambda:  not self.planform().chord_defined_by_sections)     
               
        r += 1
        FieldF (l,r,c,   lab="Position", width=85, unit="mm", step=1, lim=lambda: (0, self.planform().span), dec=1,
                obj=self._wingSection, prop=WingSection.x, disable=lambda: not self._wingSection().is_set_xn_allowed,
                style=lambda: self._style_for_fixed(self._wingSection().is_xn_fix) )
        FieldF (l,r,c+3, lab="of span", width=65, unit="%", step=1, lim=(0, 100), dec=1,
                obj=self._wingSection, prop=WingSection.xn, disable=lambda: not self._wingSection().is_set_xn_allowed)
        r += 1
        FieldF (l,r,c,   lab="Chord", width=85, unit="mm", step=1, lim=lambda:(1, self.planform().chord_root), dec=1,
                obj=self._wingSection, prop=WingSection.c, disable=lambda: not self._wingSection().is_set_cn_allowed,
                style=lambda: self._style_for_fixed(self._wingSection().is_cn_fix))
        FieldF (l,r,c+3, lab="of root", width=65, unit="%", step=1, lim=(1, 100), dec=1,
                obj=self._wingSection, prop=WingSection.cn, disable=lambda: not self._wingSection().is_set_cn_allowed)

        r += 1

        # seperate mini panel for airfoil 

        p_foil = QWidget()
        l_foil = QGridLayout (p_foil)
        Field  (l_foil,0,0,   lab="Airfoil", # width=130,  
                get=lambda: self._wingSection().airfoil.name)

        Airfoil_Open_Widget (l_foil,0,2, asIcon=True, 
                             get=lambda: self._wingSection().airfoil, 
                             set=self._set_airfoil, signal=True)

        ToolButton (l_foil,0,3, icon=Icon.DELETE, set=self._remove_airfoil,
                    toolTip="Remove airfoil - airfoil at section will be blended", 
                    disable=lambda: self._wingSection().airfoil.isBlendAirfoil)
        ToolButton (l_foil,0,4, icon=Icon.AE,   set=self._edit_airfoil,   
                    toolTip="Edit airfoil with the AirfoilEditor", 
                    disable=lambda: self._wingSection().airfoil.isBlendAirfoil)
        
        l_foil.setContentsMargins (QMargins(0, 0, 0, 0)) 
        l_foil.setSpacing (2)
        l_foil.setColumnMinimumWidth (0,70)
        l_foil.setColumnStretch (1,5)
        l.addWidget (p_foil, r, c, 1, 5)

        l.setColumnMinimumWidth (0,70)
        l.setColumnMinimumWidth (2,10)
        l.setColumnMinimumWidth (3,50)
        l.setColumnMinimumWidth (5,50)

        r += 1
        l.setRowStretch (r,5)

        # 2. column - flap and hingle line settings 

        c = 6
        r = 0
        Label  (l,r,c, get="Flaps hinge line is defined by reference line", colSpan=5, style=style.COMMENT,
                hide=lambda: not self.flaps().hinge_equal_ref_line )
        CheckBox (l,r,c, text="Section defines flaps hinge line", colSpan=5,  
                obj=self._wingSection, prop=WingSection.defines_hinge,
                hide=lambda: self.flaps().hinge_equal_ref_line)     
        r += 1
        FieldF (l,r,c,   lab="Flap depth", width=80, unit="mm", step=1, lim=lambda: (0, self.planform().chord_root), dec=1,
                obj=self._wingSection, prop=WingSection.flap_c, specialText='Auto',
                disable=lambda: not self._wingSection().defines_hinge or self._wingSection().hinge_equal_ref_line)
        FieldF (l,r,c+3, lab="of chord", width=65, unit="%", step=1, lim=(-1, 100), dec=1,
                obj=self._wingSection, prop=WingSection.flap_cn, specialText='-',
                disable=lambda: not self._wingSection().defines_hinge or self._wingSection().hinge_equal_ref_line)

        r += 1
        FieldI (l,r,c,   lab="Flap Group", width=50, step=1, lim=(0, 20),  colSpan=2, specialText="-",
                obj=self._wingSection, prop=WingSection.flap_group)

        l.setColumnMinimumWidth (c  ,70)
        l.setColumnMinimumWidth (c+2,10)
        l.setColumnMinimumWidth (c+3,50)

        # ----------

        l.setColumnStretch (12,4)

        return l 


    def _wingSections_list (self) -> list:
        sec_list = [section.name_short for section in self.wingSections()]
        return sec_list 

    def _wingSection (self) -> WingSection:
        """ Dispatcher for current WingSection between Edit and Diagram """
        return self.myApp.wingSection()

    def _wingSection_name (self) -> str:
        return self._wingSection().name_short
    
    def _set_wingSection_name (self, aSection : WingSection| str):
        """ set cur wingSection by name """
        self.myApp.on_wingSection_selected(aSection)


    def _delete_wingSection (self):
        """ delete current wingSection"""
        aSection = self.wingSections().delete (self._wingSection())     
        self.myApp.on_wingSection_selected (aSection)  

    def _add_wingSection (self):
        """ add wingSection after current"""
        aSection = self.wingSections().create_after (self._wingSection()) 
        self.myApp.on_wingSection_selected (aSection)    


    def _style_for_fixed (self, is_fix: bool) -> style:
        """ returns the style for the entry field dependant if xn or cn is fixed"""
        if is_fix: 
            return style.HINT
        else: 
            return style.NORMAL

    def _section_info (self) -> str:
        """ info text about section"""
        if self._wingSection().is_for_panels:
            text = "Section is only for paneling support"
        elif self._wingSection().defines_cn:
            text = "Section defines the planform"
        elif self._wingSection().is_xn_fix:
            text = "Section is at fixed span position"
        elif self._wingSection().is_cn_fix:
            text = "Section has a fixed chord position"
        else: 
            text = ""
        return text 


    def _edit_airfoil (self, airfoil : Airfoil = None):
        """ edit airfoil with AirfoilEditor"""

        if airfoil is None: 
            airfoil = self._wingSection().airfoil

        ae = App_Main (os.path.join(airfoil.workingDir, airfoil.pathFileName), parent=self.myApp)
        ae.sig_closing.connect (self._on_edit_finished) 

        ae.show()


    def _set_airfoil (self, airfoil : Airfoil | None):
        """ set airfoil for section"""

        if airfoil and not airfoil.isNormalized:

            text = f"The airfoil '{airfoil.fileName}' is not normalized,\n" +\
                   f"but this is needed for blending (strak).\n\n" + \
                   f"It will be normalized and saved as a temporary copy."

            msg = MessageBox (self, "Airfoil not normalized", text, Icon (Icon.INFO), min_width=350)
            msg.setStandardButtons(QMessageBox.StandardButton.Ok | MessageBox.StandardButton.Cancel)
            button = msg.exec()

            if button == QMessageBox.StandardButton.Cancel:
                return

        self.wingSections().set_airfoil_for (self._wingSection(), airfoil)

        self._on_widget_changed (None)


    def _remove_airfoil (self):
        """ remove airfoil from section"""
        self.wingSections().set_airfoil_for (self._wingSection(), None)
        self._on_widget_changed (None)


    def _on_edit_finished (self, pathFilename : str):
        """ slot - AirfoilEditor finished with airfoil pathFilename"""

        # create temp Airfoil to check within _set
        try: 
            airfoil = Airfoil (pathFileName=pathFilename, geometry=GEO_BASIC)
            airfoil.load()
        except:
            return

        self._set_airfoil (airfoil) 


    @override
    def _on_widget_changed (self, widget):
        """ user changed data in widget"""
        logger.debug (f"{self} {widget} wing section widget changed")
        self.myApp.sig_wingSection_changed.emit()