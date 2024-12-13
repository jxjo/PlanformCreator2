#!/usr/bin/env pythonbutton_color
# -*- coding: utf-8 -*-

"""  

UI panels 

"""

import logging

from PyQt6.QtWidgets        import QDialog

from base.widgets           import * 
from base.panels            import Edit_Panel, Container_Panel

from airfoil_widgets        import Airfoil_Open_Widget

from wing                   import Wing, Planform, WingSections, WingSection, Flaps
from wing                   import N_Distrib_Abstract, N_Chord_Reference, N_Reference_Line

from pc2_dialogs            import Dialog_TextEdit

from AirfoilEditor_subtree.AirfoilEditor import App_Main

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)



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

        # overloaded to set ccordinate transformation fucntions
        self.set_t_fn  (self.planform().t_norm_to_plan)       # bound method for coordinate transformation
        self.set_tr_fn (self.planform().t_plan_to_norm)     

        super()._set_panel_layout (layout=layout)

        # overloaded to connect to widgets changed signal
        for w in self.widgets:
            w.sig_changed.connect (self._on_planform_widget_changed)
        for w in self.header_widgets:
            w.sig_changed.connect (self._on_planform_widget_changed)


    def _on_planform_widget_changed (self, widget):
        """ user changed data in widget"""
        logger.debug (f"{self} {widget} widget changed slot")
        self.myApp.sig_planform_changed.emit()


    # coordinate transformation ------------------------

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
        SpaceR (l,r, stretch=4)
        r += 1
        Button (l,r,c, text="&Exit", width=100, set=self.myApp.close)
        r += 1
        SpaceR (l,r, height=5, stretch=1)        
        l.setColumnStretch (1,2)
        l.setContentsMargins (QMargins(0, 0, 0, 0)) 

        return l 
 


class Panel_Wing (Panel_Planform_Abstract):
    """ Main geometry data of wing"""

    name = 'Wing'
    _width  = (430, None)

    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 
        Field  (l,r,c, lab="Name", colSpan=3,
                obj=self.wing, prop=Wing.name)
        # ToolButton  (l,r,c+4, icon=Icon.EDIT, set=self._edit_description)
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
        Field  (l,r,c, lab="Airfoil nick prefix", width=70,
                obj=self.wing, prop=Wing.airfoil_nick_prefix)
        FieldI  (l,r,c+3, lab="Airfoil nick base", width=75, step=10, lim=(10, 9990), 
                obj=self.wing, prop=Wing.airfoil_nick_base)
        r += 1
        SpaceR (l,r, height=5, stretch=2)        

        l.setColumnMinimumWidth (0,95)
        l.setColumnMinimumWidth (3,90)
        l.setColumnStretch (2,1)
        l.setColumnStretch (5,2)
        return l 

    def _edit_description (self):
        """ open little text editor to edit description"""

        dialog = Dialog_TextEdit (self.myApp, self.wing().description, title="Edit Description")
        dialog.exec () 

        if dialog.result() == QDialog.DialogCode.Accepted:
            self.wing().set_description (dialog.new_text)
            self._on_planform_widget_changed (dialog)                   # manual refresh a dialog is not a 'Widget'



class Panel_Chord_Reference (Panel_Planform_Abstract):
    """ Main geometry data of wing"""

    name    = 'Chord Distribution and Reference'
    _width  = (410, None)

    def _init_layout (self): 
        l = QGridLayout()
        r,c = 0, 0 
        Label  (l,r,c,   get="Distribution function", style=style.COMMENT)
        Label  (l,r,c+1, get=lambda: self.n_chord().name, fontSize=size.HEADER_SMALL)  
        r += 1
        SpaceR (l,r, height=26, stretch=0)        
        r += 1
        FieldF (l,r,c,   lab="Reference at Root", width=70, unit="%", step=1, lim=(0,100), dec=1,
                obj=self.n_chord_ref, prop=N_Chord_Reference.cr_root)
        FieldF (l,r,c+3, lab="at Tip", width=70, unit="%", step=1, lim=(0,100), dec=1,
                obj=self.n_chord_ref, prop=N_Chord_Reference.cr_tip)
        r += 1
        CheckBox (l,r,c, text="Reference line is a curve (banana)", colSpan=5,
                  obj=self.n_ref_line, prop=N_Reference_Line.is_banana)
        r += 1
        CheckBox (l,r,c, text="Flaps hinge line equals Reference line", colSpan=5,  
                  obj=self.flaps, prop=Flaps.hinge_equal_ref_line)     
        r += 1
        SpaceR (l,r, height=5, stretch=2)        

        l.setColumnMinimumWidth (0,120)
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
    _width  = (700, None)


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
        Label  (l,r,c, get=self._section_info, colSpan=5, style=style.COMMENT)        
        # SpaceR (l,r, height=26)        
        r += 1
        FieldF (l,r,c,   lab="Position", width=85, unit="mm", step=1, lim=(0, self.planform().span), dec=1,
                obj=self._wingSection, prop=WingSection.x, disable=lambda: not self._wingSection().is_set_xn_allowed,
                style=lambda: self._style_for_fixed(self._wingSection().is_xn_fix) )
        FieldF (l,r,c+3, lab="of span", width=65, unit="%", step=1, lim=(0, 100), dec=1,
                obj=self._wingSection, prop=WingSection.xn, disable=lambda: not self._wingSection().is_set_xn_allowed,
                style=lambda: self._style_for_fixed(self._wingSection().is_xn_fix))
        r += 1
        FieldF (l,r,c,   lab="Chord", width=85, unit="mm", step=1, lim=(1, self.planform().span), dec=1,
                obj=self._wingSection, prop=WingSection.c, disable=lambda: not self._wingSection().is_set_cn_allowed,
                style=lambda: self._style_for_fixed(self._wingSection().is_cn_fix))
        FieldF (l,r,c+3, lab="of root", width=65, unit="%", step=1, lim=(1, 100), dec=1,
                obj=self._wingSection, prop=WingSection.cn, disable=lambda: not self._wingSection().is_set_cn_allowed,
                style=lambda: self._style_for_fixed(self._wingSection().is_cn_fix))
        r += 1
        Label  (l,r,c, get="Flaps hinge line is defined by reference line", colSpan=5, style=style.COMMENT,
                hide=lambda: not self.flaps().hinge_equal_ref_line )
        CheckBox (l,r,c, text="Section defines hinge line", colSpan=5,  
                obj=self._wingSection, prop=WingSection.defines_hinge,
                hide=lambda: self.flaps().hinge_equal_ref_line)     
        r += 1
        FieldF (l,r,c,   lab="Flap depth", width=85, unit="mm", step=1, lim=(0, self.planform().span), dec=1,
                obj=self._wingSection, prop=WingSection.flap_c, specialText='Auto',
                disable=lambda: not self._wingSection().defines_hinge or self._wingSection().hinge_equal_ref_line)
        FieldF (l,r,c+3, lab="of chord", width=65, unit="%", step=1, lim=(-1, 100), dec=1,
                obj=self._wingSection, prop=WingSection.flap_cn, specialText='-',
                disable=lambda: not self._wingSection().defines_hinge or self._wingSection().hinge_equal_ref_line)
        r += 1
        SpaceR (l,r, height=5, stretch=2)        

        c = 7
        r = 1
        Field  (l,r,c,   lab="Airfoil",  colSpan=1, # width=(140,None),
                get=lambda: self._wingSection().airfoil.name)
        Airfoil_Open_Widget (l,r,c+2, asIcon=True, set=self._wingSection().set_airfoil, signal=True)
        ToolButton (l,r,c+3, icon=Icon.DELETE, set=self._remove_airfoil,
                    toolTip="Remove airfoil - airfoil will be blended", 
                    disable=lambda: self._wingSection().airfoil.isBlendAirfoil)
        ToolButton (l,r,c+4, icon=Icon.EDIT,   set=self._edit_airfoil,   
                    toolTip="Edit airfoil with the Airfoil Editor", 
                    disable=lambda: self._wingSection().airfoil.isBlendAirfoil)
        r += 1
        Field  (l,r,c,   lab="Airfoil nick", width=None, colSpan=1, 
                get=lambda: self._wingSection().airfoil_nick_name)
        r += 2
        FieldI (l,r,c,   lab="Flap Group", width=50, step=1, lim=(0, 20),  colSpan=2, specialText="-",
                obj=self._wingSection, prop=WingSection.flap_group)

        # c -= 1

        l.setColumnMinimumWidth (0,70)
        l.setColumnMinimumWidth (2,10)
        l.setColumnMinimumWidth (3,50)
        l.setColumnMinimumWidth (5,40)

        # l.setColumnMinimumWidth (6,10)
        l.setColumnMinimumWidth (7,70)
        # l.setColumnMinimumWidth (10,50)
        #l.setColumnStretch (2,1)
        l.setColumnStretch (8,4)
        # l.setColumnStretch (13,1)
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
        if self._wingSection().defines_cn:
            text = "Section defines the planform"
        elif self._wingSection().is_xn_fix:
            text = "Section is at fixed span position"
        elif self._wingSection().is_cn_fix:
            text = "Section is at fixed chord position"
        else: 
            text = ""
        return text 


    def _remove_airfoil (self):
        """ remove airfoil from section"""
        self._wingSection().set_airfoil (None) 
        self._on_planform_widget_changed (None)


    def _edit_airfoil (self):
        """ edit airfoil with AirfoilEditor"""

        ae = App_Main (self._wingSection().airfoil.pathFileName, parent=self.myApp)
        ae.sig_closing.connect (self._on_edit_finished) 

        ae.show()


    def _on_edit_finished (self, pathFilename : str):
        """ slot - AirfoilEditor finished with airfoil pathFilename"""

        self._wingSection().set_airfoil (pathFilename)
        self._on_planform_widget_changed (None)
