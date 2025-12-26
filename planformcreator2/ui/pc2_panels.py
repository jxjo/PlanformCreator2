#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""  

UI panels 

"""

import subprocess
import sys

import logging

from PyQt6.QtWidgets        import QMenu, QDialog, QFileDialog, QMessageBox
from PyQt6.QtGui            import QDesktopServices
from PyQt6.QtCore           import QUrl

from airfoileditor.base.widgets         import * 
from airfoileditor.base.panels          import Edit_Panel, MessageBox, Disabled_Overlay
from airfoileditor.model.airfoil        import Airfoil, GEO_BASIC

from ..model.wing           import Wing, STRAK_AIRFOIL_NAME
from ..model.wing           import (Planform, N_Distrib_Abstract, N_Chord_Reference, N_Reference_Line,
                                    Flaps, WingSections, WingSection)

from ..app_model            import App_Model
from .pc2_dialogs           import Dialog_TextEdit

logger = logging.getLogger(__name__)
# logger.setLevel(logging.WARNING)


# ---------------------------------------------------------------------------


class Panel_Planform_Abstract (Edit_Panel):
    """ 
    Abstract superclass for Edit/View-Panels of PlanformCreator2
        - has semantics of App
        - connect / handle signals 
    """

    sig_toggle_panel_size = pyqtSignal()                # wants to toggle panel size

    MAIN_MARGINS        = (10, 5,20, 5)
    MAIN_MARGINS_MINI   = ( 0, 5,10, 5)
    MAIN_MARGINS_FILE   = (10, 5,10, 5)

    _main_margins = MAIN_MARGINS


    @property
    def app_model (self) -> App_Model:
        return self.dataObject

    @property
    def wing (self) -> Wing:
        return self.app_model.wing 

    @property    
    def planform (self) -> Planform:
        return self.wing.planform
    
    @property    
    def n_chord (self) -> N_Distrib_Abstract:
        return self.wing.planform.n_distrib
    
    @property
    def n_chord_ref (self) -> N_Chord_Reference:
        return self.wing.planform.n_chord_ref
    
    @property
    def n_ref_line (self) -> N_Reference_Line:
        return self.wing.planform.n_ref_line

    @property    
    def flaps (self) -> Flaps:
        return self.wing.planform.flaps

    @property
    def wingSections (self) -> WingSections:
        return self.wing.planform.wingSections

    @property
    def wingSections_to_show (self) -> list[WingSection]:
        """ the wingSections to plot - without extra sections for paneling"""

        return [section for section in self.wingSections if not section.is_for_panels] 


    @override
    def _set_panel_layout (self ):
        """ Set layout of self._panel """
        # overridden to connect to widgets changed signal

        super()._set_panel_layout ()
        for w in self.widgets:
            w.sig_changed.connect (self._on_widget_changed)
        for w in self.header_widgets:
            w.sig_changed.connect (self._on_widget_changed)


    def _on_widget_changed (self, widget):
        """ user changed data in widget"""
        logger.debug (f"{self} {widget} widget changed slot")
        self.app_model.notify_planform_changed ()




# --------------------------------------------------------------------------


class Panel_File (Panel_Planform_Abstract):
    """ File panel with open / save / ... """

    name = 'File'

    sig_open = pyqtSignal()                             # wants to open a planform
    sig_new = pyqtSignal()                              # wants to create new planform
    sig_save = pyqtSignal()                             # wants to save current planform
    sig_exit = pyqtSignal()                             # wants to exit the application
    sig_save_as = pyqtSignal()                          # wants to save current planform as new file
    sig_rename = pyqtSignal()                           # wants to rename current planform
    sig_delete = pyqtSignal()                           # wants to delete current planform
    sig_delete_temp_files = pyqtSignal()                # wants to delete all temp files

    _main_margins = Panel_Planform_Abstract.MAIN_MARGINS_FILE


    @override
    def _add_to_header_layout(self, l_head: QHBoxLayout):
        """ add Widgets to header layout"""

        l_head.addStretch(1)
        ToolButton   (l_head, icon=Icon.EXPAND, set=self.sig_toggle_panel_size.emit,
                      toolTip='Minimize lower panel -<br>Alternatively, you can double click on the lower panels')


    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 
        Button (l,r,c, text="&Open", width=100, 
                set=self.sig_open.emit, toolTip="Open new Planform",
                button_style=button_style.PRIMARY)
        Button (l,r,c+2, text="&New", width=80, 
                set=self.sig_new.emit, toolTip="Create new Planform")
        
        r += 1
        Button (l,r,c, text="&Save", width=100, 
                set=self.sig_save.emit, toolTip="Save Planform to parameter file")
        MenuButton (l,r,c+2, text="More...", width=80, 
                menu=self._more_menu(), 
                toolTip="Choose further actions for this planform")
        r += 1
        SpaceR (l,r, stretch=1)
        r += 1
        Button (l,r,c, text="&Exit", width=100, set=self.sig_exit.emit,)
        l.setColumnMinimumWidth (1,12)
        l.setColumnStretch (3,1)

        return l 


    def _more_menu (self) -> QMenu:
        """ create and return sub menu for 'more' actions"""

        app_name = "PlanformCreator2"

        menue = QMenu ()
        menue.addAction (MenuAction ("Save as...", self, set=self.sig_save_as.emit,
                                     toolTip="Create a copy of the current planform with new name and filename"))
        menue.addAction (MenuAction ("Rename...", self, set=self.sig_rename.emit,
                                     toolTip="Rename name and/or filename of current planform"))
        # menue.addAction (MenuAction ("Delete", self, set=self.app.do_delete,
        #                              toolTip="Delete current planform including all temporary files created by the AirfoilEditor"))
        menue.addAction (MenuAction ("Delete temp files", self, set=self.sig_delete_temp_files.emit,
                                     toolTip=f"Delete temporary files created by {app_name} just to have a clean directoy again"))
        menue.addSeparator ()
        menue.addAction (MenuAction ("Readme on Github", self, set=self._open_AE_url,
                                     toolTip=f"Open the Github README file of {app_name} in a browser"))
        menue.addAction (MenuAction ("Releases on Github", self, set= self._open_releases_url,
                                     toolTip=f"Open the Github page with the actual release of {app_name}"))
        menue.setToolTipsVisible(True)

        return menue


    def _open_releases_url (self):
        """ open Github versions in Browser"""

        link = "https://github.com/jxjo/PlanformCreator2/releases"
        QDesktopServices.openUrl(QUrl(link))


    def _open_AE_url (self):
        """ open Github AirfoilEditor repo in Browser"""

        link = "https://github.com/jxjo/PlanformCreator2"
        QDesktopServices.openUrl(QUrl(link))



class Panel_File_Small (Panel_File):
    """ File panel small height  - just exit """

    
    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 
        Button      (l,r,c, text="&Open", width=100, 
                    set=self.sig_open.emit, toolTip="Open new Planform",
                    button_style=button_style.PRIMARY)
        MenuButton  (l,r,c+2, text="More...", width=80, 
                    menu=self._more_menu(), 
                    toolTip="Choose further actions for this airfoil")
        ToolButton  (l,r,c+3, icon=Icon.COLLAPSE, set=self.sig_toggle_panel_size.emit,
                    toolTip='Maximize lower panel -<br>Alternatively, you can double click on the lower panels')
        r += 1
        Button      (l,r,c, text="&Exit", width=100, set=self.sig_exit.emit)
        l.setColumnMinimumWidth (1,12)
        l.setColumnStretch (2,2)
        return l 
 

    def _more_menu (self) -> QMenu:
        """ create and return sub menu for 'more' actions"""

        menue = super()._more_menu ()
        menue.insertAction (menue.actions()[0], MenuAction ("&Save", self, set=self.sig_save.emit,   
                                    toolTip="Save Planform to parameter file"))
        menue.insertAction (menue.actions()[0], MenuAction ("&New", self, set=self.sig_new.emit,   
                                    toolTip="Create new Planform"))
        return menue



class Panel_Wing (Panel_Planform_Abstract):
    """ Main geometry data of wing"""

    name = 'Wing'

    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 
        Field  (l,r,c, lab="Name", colSpan=3,
                obj=lambda: self.wing, prop=Wing.name)
        Button (l,r,c+4, text="Descr", width=75, set=self._edit_description)
        r += 1
        FieldF (l,r,c, lab="Wing Span", width=85, unit="mm", step=10, lim=(10, 20000), dec=0,
                obj=lambda: self.wing, prop=Wing.wingspan)
        FieldF (l,r,c+3, lab="Fuselage Width", width=75, unit="mm", step=10, lim=(0, 1000), dec=0,
                obj=lambda: self.wing, prop=Wing.fuselage_width)
        r += 1
        FieldF (l,r,c, lab="Half Wing Span", width=85, unit="mm", step=10, lim=(10, 10000), dec=0, 
                obj=lambda: self.planform, prop=Planform.span)
        r += 1
        FieldF (l,r,c, lab="Root Chord", width=85, unit="mm", step=10, lim=(10, 1000), dec=0, 
                obj=lambda: self.planform, prop=Planform.chord_root)
        FieldF (l,r,c+3, lab="Sweep Angle", width=75, unit="°", step=0.1, lim=(-45, 45), dec=1,
                obj=lambda: self.planform, prop=Planform.sweep_angle)
        r += 1
        l.setRowStretch (r,1)

        l.setColumnMinimumWidth (0,95)
        l.setColumnMinimumWidth (2,20)
        l.setColumnMinimumWidth (3,90)
        return l 


    def _edit_description (self):
        """ open little text editor to edit description"""

        dialog = Dialog_TextEdit (self, self.wing.description, title="Description of Wing", 
                                  parentPos=(0.9,0.0), dialogPos=(0.0,1.0))
        dialog.exec () 

        if dialog.result() == QDialog.DialogCode.Accepted:
            self.wing.set_description (dialog.new_text)
            self._on_widget_changed (dialog)                   # manual refresh a dialog is not a 'Widget'



class Panel_Wing_Small (Panel_Wing):
    """ Minimized wing panel """

    _main_margins = Panel_Planform_Abstract.MAIN_MARGINS_MINI

    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 
        FieldF (l,r,c, lab="Half Wing Span", width=85, unit="mm", step=10, lim=(10, 10000), dec=0, 
                obj=self.planform, prop=Planform.span)
        r += 1
        FieldF (l,r,c, lab="Root Chord", width=85, unit="mm", step=10, lim=(10, 1000), dec=0, 
                obj=self.planform, prop=Planform.chord_root)
        FieldF (l,r,c+3, lab="Sweep Angle", width=75, unit="°", step=0.1, lim=(-45, 45), dec=1,
                obj=self.planform, prop=Planform.sweep_angle)
        r += 1
        l.setRowStretch (r,1)

        l.setColumnMinimumWidth (0,95)
        l.setColumnMinimumWidth (2,15)
        l.setColumnMinimumWidth (3,90)
        return l 



class Panel_Chord_Reference (Panel_Planform_Abstract):
    """ Chord Refrerence panel"""

    name    = 'Chord Distribution and Reference'

    def _init_layout (self): 
        l = QGridLayout()
        r,c = 0, 0 
        Label  (l,r,c,   get="Chord Distribution")
        Label  (l,r,c+1, get=lambda: self.n_chord.name, fontSize=size.HEADER_SMALL)  
        r += 1
        FieldF (l,r,c,   lab="Reference at Root", width=70, unit="%", step=1, lim=(0,100), dec=1,
                obj=lambda: self.n_chord_ref, prop=N_Chord_Reference.cr_root)
        FieldF (l,r,c+3, lab="at Tip", width=70, unit="%", step=1, lim=(0,100), dec=1,
                obj=lambda: self.n_chord_ref, prop=N_Chord_Reference.cr_tip)
        r += 1
        CheckBox (l,r,c, text="Reference line is a curve (banana)", colSpan=5,
                obj=lambda: self.n_ref_line, prop=N_Reference_Line.is_banana,
                hide=lambda: not self.planform.n_distrib.isBezier)    # banana only for Bezier
        r += 1
        CheckBox (l,r,c, text="Flaps hinge line equals Reference line", colSpan=5,  
                  obj=lambda: self.flaps, prop=Flaps.hinge_equal_ref_line)     
        r += 1
        l.setRowStretch (r,1)

        l.setColumnMinimumWidth (0,110)
        l.setColumnMinimumWidth (2,15)
        l.setColumnMinimumWidth (3,50)
        return l 



class Panel_Chord_Reference_Small (Panel_Chord_Reference):
    """ Small Chord Reference panel"""

    _main_margins = Panel_Planform_Abstract.MAIN_MARGINS_MINI

    def _init_layout (self): 
        l = QGridLayout()
        r,c = 0, 0 
        Label  (l,r,c,   get="Chord Distribution")
        Label  (l,r,c+1, get=lambda: self.n_chord.name, fontSize=size.HEADER_SMALL)  
        r += 1
        FieldF (l,r,c,   lab="Reference at Root", width=70, unit="%", step=1, lim=(0,100), dec=1,
                obj=lambda: self.n_chord_ref, prop=N_Chord_Reference.cr_root)
        FieldF (l,r,c+3, lab="at Tip", width=70, unit="%", step=1, lim=(0,100), dec=1,
                obj=lambda: self.n_chord_ref, prop=N_Chord_Reference.cr_tip)
        r += 1
        l.setRowStretch (r,1)

        l.setColumnMinimumWidth (0,110)
        l.setColumnMinimumWidth (2,15)
        l.setColumnMinimumWidth (3,50)
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
    def _on_widget_changed (self, widget):
        """ user changed data in widget"""
        logger.debug (f"{self} {widget} widget changed wingSection slot")
        self.app_model.notify_wingSection_changed ()


    @override
    def refresh (self, **kwargs):
        super().refresh(**kwargs)

        # ensure header is not disabled (for section is_for_paneling)
        for w in self.header_widgets:
            w.refresh (False)


    @override
    @property
    def _isDisabled (self) -> bool:
        """ overloaded: disabled if section is just for paneling """
        return self.wingSection.is_for_panels


    @override
    def _add_to_header_layout(self, l_head: QHBoxLayout):
        """ add Widgets to header layout"""

        ComboBox     (l_head, width=55,
                      get=lambda: self.wingSection_name, set=self.set_wingSection_name,
                      options= lambda: self.wingSections_list, signal=False )      # do not signal (would be double refresh)
        ToolButton   (l_head, icon=Icon.ADD,    set=self._add_wingSection,
                      disable=lambda: self.wingSection.is_tip,
                      toolTip='Add a new wing section')
        ToolButton   (l_head, icon=Icon.DELETE, set=self._delete_wingSection,
                      disable=lambda: self.wingSection.is_root_or_tip,
                      toolTip='Delete selected wing section')
        l_head.addStretch(3)


    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 

        # 1. column - section settings and airfoil 

        Label  (l,r,c, get=self._section_info, colSpan=5, 
                style=lambda: style.HINT if self.wingSection.is_for_panels else style.COMMENT,
                hide = lambda: self.planform.chord_defined_by_sections) 
        CheckBox (l,r,c, text="Section defines chord", colSpan=5,               # toggle trapezoid or not 
                obj=lambda: self.wingSection, prop=WingSection.defines_cn,
                disable=lambda: self.wingSection.is_root_or_tip,
                hide=lambda:  not self.planform.chord_defined_by_sections)     
               
        r += 1
        FieldF (l,r,c,   lab="Position", width=85, unit="mm", step=1, lim=lambda: (0, self.planform.span), dec=1,
                obj=lambda: self.wingSection, prop=WingSection.x, 
                disable=lambda: not self.wingSection.is_set_xn_allowed,
                style=self._style_for_xn_fixed)
        FieldF (l,r,c+3, lab="of span", width=65, unit="%", step=1, lim=(0, 100), dec=1,
                obj=lambda:self.wingSection, prop=WingSection.xn, 
                disable=lambda: not self.wingSection.is_set_xn_allowed)
        r += 1
        FieldF (l,r,c,   lab="Chord", width=85, unit="mm", step=1, lim=lambda:(1, self.planform.chord_root), dec=1,
                obj=lambda: self.wingSection, prop=WingSection.c, 
                disable=lambda: not self.wingSection.is_set_cn_allowed,
                style=self._style_for_cn_fixed)
        FieldF (l,r,c+3, lab="of root", width=65, unit="%", step=1, lim=(1, 100), dec=1,
                obj=lambda: self.wingSection, prop=WingSection.cn, 
                disable=lambda: not self.wingSection.is_set_cn_allowed)

        r += 1

        # seperate mini panel for airfoil 

        p_foil = QWidget()
        l_foil = QGridLayout (p_foil)
        Field  (l_foil,0,0,   lab="Airfoil",
                get=lambda: self._airfoil_name,                 # name as strak fileName can be long
                toolTip=lambda: self.wingSection.airfoil.info_as_html)

        ToolButton (l_foil,0,2, icon=Icon.OPEN, set=self._open_airfoil, 
                        toolTip="Select an airfoil for this wing section")
        ToolButton (l_foil,0,3, icon=Icon.DELETE, set=self._remove_airfoil,
                    toolTip="Remove airfoil - airfoil at section will be blended", 
                    disable=lambda: self.wingSection.airfoil.isBlendAirfoil or self.wingSection.is_root_or_tip)
        ToolButton (l_foil,0,4, icon=Icon.AE,   set=self._edit_airfoil,   
                    toolTip="Edit airfoil with the AirfoilEditor", 
                    disable=lambda: self.wingSection.airfoil.isBlendAirfoil)
        
        l_foil.setContentsMargins (QMargins(0, 0, 0, 0)) 
        l_foil.setSpacing (2)
        l_foil.setColumnMinimumWidth (0,70)
        l_foil.setColumnMinimumWidth (1,215)
        l_foil.setColumnStretch (5,5)
        l.addWidget (p_foil, r, c, 1, 7)

        l.setColumnMinimumWidth (0,70)
        l.setColumnMinimumWidth (2,10)
        l.setColumnMinimumWidth (3,50)
        l.setColumnMinimumWidth (5,40)

        r += 1
        l.setRowStretch (r,2)

        # 2. column - flap and hingle line settings 

        c = 6
        r = 0
        Label  (l,r,c, get="Flaps hinge line is defined by reference line", colSpan=5, style=style.COMMENT,
                hide=lambda: not self.flaps.hinge_equal_ref_line )
        CheckBox (l,r,c, text="Section defines flaps hinge line", colSpan=5,  
                obj=lambda: self.wingSection, prop=WingSection.defines_hinge,
                hide=lambda: self.flaps.hinge_equal_ref_line)     
        r += 1
        FieldF (l,r,c,   lab="Flap depth", width=80, unit="mm", step=1, lim=lambda: (0, self.planform.chord_root), dec=1,
                obj=lambda: self.wingSection, prop=WingSection.flap_c, specialText='Auto',
                disable=lambda: not self.wingSection.defines_hinge or self.wingSection.hinge_equal_ref_line)
        FieldF (l,r,c+3, lab="of chord", width=65, unit="%", step=1, lim=(-1, 100), dec=1,
                obj=lambda: self.wingSection, prop=WingSection.flap_cn, specialText='-',
                disable=lambda: not self.wingSection.defines_hinge or self.wingSection.hinge_equal_ref_line)

        r += 1
        FieldI (l,r,c,   lab="Flap Group", width=50, step=1, lim=(0, 20),  colSpan=2, specialText="-",
                obj=lambda: self.wingSection, prop=WingSection.flap_group)

        l.setColumnMinimumWidth (c  ,70)
        l.setColumnMinimumWidth (c+2,10)
        l.setColumnMinimumWidth (c+3,50)
        l.setColumnStretch (c+5,2)
        return l 


    @property
    def wingSections_list (self) -> list[str]:
        sec_list = [section.name_short for section in self.wingSections_to_show]
        return sec_list 

    @property
    def wingSection (self) -> WingSection:
        """ Dispatcher for current WingSection between Edit and Diagram """
        return self.app_model.cur_wingSection

    @property
    def wingSection_name (self) -> str:
        return self.wingSection.name_short
    
    def set_wingSection_name (self, aSection : WingSection| str):
        """ set cur wingSection by name """
        self.app_model.set_cur_wingSection (aSection)


    def _delete_wingSection (self):
        """ delete current wingSection"""
        self.app_model.delete_wingSection (self.wingSection)


    def _add_wingSection (self):
        """ add wingSection after current"""
        self.app_model.create_wingSection ()


    def _style_for_xn_fixed (self) -> style:
        """ returns the style for the entry field dependant if xn fixed"""
        if self.wingSection.is_xn_fix and not self.wingSection.is_root_or_tip: 
            return style.HINT
        else: 
            return style.NORMAL

    def _style_for_cn_fixed (self) -> style:
        """ returns the style for the entry field dependant if cn fixed"""
        if self.wingSection.is_cn_fix and not self.wingSection.is_root_or_tip: 
            return style.HINT
        else: 
            return style.NORMAL


    def _section_info (self) -> str:
        """ info text about section"""
        if self.wingSection.is_for_panels:
            text = "Section is only for paneling support"
        elif self.wingSection.defines_cn:
            text = "Section defines the planform"
        elif self.wingSection.is_xn_fix:
            text = "Section is at fixed span position"
        elif self.wingSection.is_cn_fix:
            text = "Section has a fixed relative chord"
        else: 
            text = ""
        return text 

    @property
    def _airfoil_name (self) -> str:
        """ get airfoil file name or 'strak' at section"""
        airfoil = self.wingSection.airfoil
        return STRAK_AIRFOIL_NAME if airfoil.isBlendAirfoil else airfoil.fileName


    def _open_airfoil (self):
        """ open a new airfoil and load it"""

        airfoil = self.wingSection.airfoil

        filters   = "Airfoil files (*.dat);;Bezier files (*.bez);;Hicks Henne files (*.hicks)"
        directory = self.wing.airfoils_dir if airfoil.isBlendAirfoil else airfoil.pathName_abs

        newPathFilename, _ = QFileDialog.getOpenFileName(self, filter=filters, directory=directory)

        if newPathFilename:                         # user pressed open
            self._set_airfoil_by_file (newPathFilename)


    def _edit_airfoil (self, airfoil : Airfoil = None):
        """ edit airfoil with AirfoilEditor"""

        if airfoil is None: 
            airfoil = self.wingSection.airfoil

        if airfoil.isBlendAirfoil:
            MessageBox.error(self, 'Edit Airfoil', "Cannot edit a blended (strak) airfoil.\nPlease assign a real airfoil first.", min_height=60)
            return
        
        # Run AirfoilEditor as a separate Python process

        try:
            overlay = Disabled_Overlay (self.window())
            self.window().setEnabled(False)                 # Disable main window to simulate modal behavior
            QApplication.processEvents()                    # ensure overlay is shown

            process = subprocess.Popen([sys.executable, '-m', 'airfoileditor', airfoil.pathFileName_abs])
            process.wait()  # Wait for the process to complete (modal behavior)

        except FileNotFoundError as e:
            MessageBox.error(self, 'Edit Airfoil', f"Could not open AirfoilEditor.\n\n{e}", min_height=60)
            return
        except Exception as e:
            MessageBox.error(self, 'Edit Airfoil', f"Error launching AirfoilEditor.\n\n{e}", min_height=60)
            return
        finally:
            self.window().setEnabled(True)                  # Re-enable main window
            overlay.hide()                                  # Hide immediately
            overlay.close()                                 # Close and trigger closeEvent
            overlay.deleteLater()                           # Schedule for deletion

        # After editing, reload the airfoil to reflect any changes - reset strak
        self._set_airfoil_by_file (airfoil.pathFileName_abs)


    def _set_airfoil_by_file (self, pathFileName : str | None):
        """ 
        Set new airfoil for current section by file name 
            - handles copying to airfoils dir and normalization """

        new_pathFileName = self.wingSection.get_airfoil_file (pathFileName)

        if new_pathFileName is None:
            # remove airfoil - will blend strak
            self.wingSections.create_airfoil_for (self.wingSection, None)
            self.app_model.notify_wingSection_changed ()
            return
        
        # Check if airfoil is already in the airfoils directory
        try:
            already_there = os.path.samefile(os.path.dirname(pathFileName), self.wing.airfoils_dir)
        except (FileNotFoundError, OSError):
            already_there = False

        # Check if airfoil can be loaded and is normalized 
        airfoil = Airfoil.onFileType(new_pathFileName, geometry=GEO_BASIC)
        airfoil.load()
        
        if not airfoil.isLoaded:
            MessageBox.error(self, 'Load Airfoil', f"<b>{new_pathFileName}</b> couldn't be loaded.", min_height=60)
            return        
        
        # 1 not normalized -> normalize and copy
        if not airfoil.isNormalized:

            text = f"The airfoil '{airfoil.fileName}' is not normalized,<br>" +\
                f"but this is needed for blending (strak).<br><br>" + \
                f"It will be normalized and copied to <b>./{self.wing.airfoils_dir_rel}</b>."

            msg = MessageBox (self, "Airfoil not normalized", text, Icon (Icon.INFO), min_width=350)
            msg.setStandardButtons(QMessageBox.StandardButton.Ok | MessageBox.StandardButton.Cancel)
            button = msg.exec()

            if button == QMessageBox.StandardButton.Cancel:
                return
            do_copy = True

        # 2 normalized and not in airfoils_dir -> ask copy
        elif not already_there:    

            text = f"'{airfoil.fileName}' will be copied into directory <b>./{self.wing.airfoils_dir_rel}</b><br>" +\
                f"which is the recommended location for airfoils of this planform.<br><br>" + \
                f"Copy airfoil?"
            msg = MessageBox (self, "Copy Airfoil", text, Icon (Icon.INFO), min_width=350)
            msg.setStandardButtons(QMessageBox.StandardButton.Ok | MessageBox.StandardButton.No)
            button = msg.exec()

            do_copy = button == QMessageBox.StandardButton.Ok

        # 3 already there -> no copy
        else:
            do_copy = False

        # do it - will assign polarSet and reset strak 
        self.wingSections.create_airfoil_for (self.wingSection, new_pathFileName, into_airfoils_dir=do_copy)
        self.app_model.notify_wingSection_changed ()



    def _remove_airfoil (self):
        """ remove airfoil from section"""

        self.wingSections.create_airfoil_for (self.wingSection, None)
        self.app_model.notify_wingSection_changed ()



class Panel_WingSection_Small (Panel_WingSection):
    """ Main geometry data of wing"""

    _width  = None
    _panel_margins = (0, 0, 0, 0)

    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 

        # 0. column - select section
        Label        (l,r,c, get="Wing Section",colSpan=2)
        r += 1
        ComboBox     (l,r,c, width=55,
                      get=lambda: self.wingSection_name, set=self.set_wingSection_name,
                      options= lambda: self.wingSections_list, signal=False )      # do not signal (would be double refresh)
        ToolButton   (l,r,c+1, icon=Icon.ADD,    set=self._add_wingSection,
                      disable=lambda: self.wingSection.is_tip,
                      toolTip='Add a new wing section')
        ToolButton   (l,r,c+2, icon=Icon.DELETE, set=self._delete_wingSection,
                      disable=lambda: self.wingSection.is_root_or_tip,
                      toolTip='Delete selected wing section')

        # 1. column - section settings and airfoil 

        r = 0 
        c += 4
               
        FieldF (l,r,c,   lab="Position", width=85, unit="mm", step=1, lim=lambda: (0, self.planform.span), dec=1,
                obj=lambda: self.wingSection, prop=WingSection.x, 
                disable=lambda: not self.wingSection.is_set_xn_allowed,
                style=lambda: self._style_for_fixed(self.wingSection.is_xn_fix) )
        FieldF (l,r,c+3, lab="of span", width=65, unit="%", step=1, lim=(0, 100), dec=1,
                obj=lambda: self.wingSection, prop=WingSection.xn, 
                disable=lambda: not self.wingSection.is_set_xn_allowed)
        r += 1
        FieldF (l,r,c,   lab="Chord", width=85, unit="mm", step=1, lim=lambda:(1, self.planform.chord_root), dec=1,
                obj=lambda: self.wingSection, prop=WingSection.c, 
                disable=lambda: not self.wingSection.is_set_cn_allowed,
                style=lambda: self._style_for_fixed(self.wingSection.is_cn_fix))
        FieldF (l,r,c+3, lab="of root", width=65, unit="%", step=1, lim=(1, 100), dec=1,
                obj=lambda: self.wingSection, prop=WingSection.cn, 
                disable=lambda: not self.wingSection.is_set_cn_allowed)

        r += 1
        l.setRowStretch (r,2)

        l.setColumnMinimumWidth (3,20)
        l.setColumnMinimumWidth (4,60)
        l.setColumnMinimumWidth (6,20)
        l.setColumnMinimumWidth (7,50)
        l.setColumnStretch (c+7,2)
        return l 


