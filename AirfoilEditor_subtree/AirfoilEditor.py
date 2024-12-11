#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    Airfoil Editor 

    Object model overview (a little simplified) 

    App                                         - root frame 
        |-- Panel_Geometry                      - geometry data 
        |-- Panel_Coordinates                   - LE and TE coordinates
                ...                             - ...

        |-- Airfoil_Diagram                     - main airfoil view 
                :
                |-- Airfoil_Diagram_Item        - Pygtgraph Plot item for airfoil contour
                |-- Curvature_Diagram_Item      - Pygtgraph Plot item for airfoil curvature 
                ...                             - ...

        |-- Airfoil                             - airfoil model object 
"""

import os
import sys
import argparse
from pathlib import Path

from PyQt6.QtCore           import QMargins
from PyQt6.QtWidgets        import QApplication, QMainWindow, QWidget, QMessageBox 
from PyQt6.QtWidgets        import QGridLayout, QVBoxLayout, QHBoxLayout
from PyQt6.QtGui            import QCloseEvent

# let python find the other modules in modules relativ to path of self  
sys.path.append(os.path.join(Path(__file__).parent , 'modules'))

from model.airfoil          import Airfoil, usedAs, GEO_SPLINE
from model.airfoil_geometry import Panelling_Spline, Panelling_Bezier
from model.airfoil_examples import Example

from base.common_utils      import * 
from base.panels            import Container_Panel
from base.widgets           import *

from airfoil_widgets        import * 
from airfoil_diagrams       import * 

from airfoil_dialogs        import Airfoil_Save_Dialog
from airfoil_ui_panels      import * 


import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)



#-------------------------------------------------------------------------------
# The App   
#-------------------------------------------------------------------------------

# ------ globals -----

AppName    = "Airfoil Editor"
AppVersion = "2.0.1"


class App_Main (QMainWindow):
    '''
        The AirfoilEditor App

        If parentApp is passed, the AirfoilEditor is called from eg PlanformEditor,
        so it will be modal with a reduced File Menu 
    '''

    name = AppName  

    # Signals 

    sig_airfoil_changed         = pyqtSignal()          # airfoil data changed 

    sig_airfoil_target_changed  = pyqtSignal(bool)      # target airfoil changed 
    sig_bezier_changed          = pyqtSignal(Line.Type) # new bezier during match bezier 
    sig_panelling_changed       = pyqtSignal()          # new panelling

    sig_enter_edit_mode         = pyqtSignal()          # starting modify airfoil
    sig_enter_bezier_mode       = pyqtSignal(bool)      # starting bezier match dialog 
    sig_enter_panelling         = pyqtSignal()          # starting panelling dialog
    sig_enter_blend             = pyqtSignal()          # starting blend airfoil with

    sig_closing                 = pyqtSignal(str)       # the app is closing with an airfoils pathFilename


    def __init__(self, airfoil_file, parent=None):
        super().__init__(parent)

        self._airfoil = None                        # current airfoil 
        self._airfoil_org = None                    # airfoil saved in edit_mode 
        self._airfoil_ref1 = None                   # reference airfoils 
        self._airfoil_ref2 = None  
        self._airfoil_target = None                 # target for match Bezier     

        self._edit_mode = False                     # edit/view mode of app 

        self._data_panel = None 
        self._file_panel = None
        self._diagram_panel = None

        self.parentApp = parent
        self.initial_geometry = None                # window geometry at the beginning

        # if called from other applcation (PlanformCreator) make it modal to this 

        if parent is not None:
            self.setWindowModality(Qt.WindowModality.ApplicationModal)  

        # get icon either in modules or in icons 
        
        self.setWindowIcon (Icon ('AE_ico.ico'))

        # get initial window size from settings

        Settings.belongTo (__file__, nameExtension=None, fileExtension= '.settings')
        geometry = Settings().get('window_geometry', [])
        maximize = Settings().get('window_maximize', False)
        Win_Util.set_initialWindowSize (self, size_frac= (0.80, 0.70), pos_frac=(0.1, 0.1),
                                        geometry=geometry, maximize=maximize)
        
        self._load_panelling_settings ()

        # init airfoil 

        if airfoil_file and (not os.path.isfile (airfoil_file)): 
            QMessageBox.critical (self, self.name , f"\n'{airfoil_file}' does not exist.\nShowing example airfoil.\n")
            airfoil = Example()
            self.move (200,150)                     # messagebox will move main window 
        elif airfoil_file is None : 
            airfoil = Example()
        else:
            airfoil = create_airfoil_from_path(airfoil_file)

        self.set_airfoil (airfoil, silent=True)

        # init main layout of app

        self._data_panel    = Container_Panel (title="Data panel")
        self._file_panel    = Container_Panel (title="File panel", width=240)
        self._diagram_panel = Diagram_Airfoil (self, self.airfoils)

        l_main = self._init_layout() 

        container = QWidget()
        container.setLayout (l_main) 
        self.setCentralWidget(container)

        # connect to signals from diagram

        self._diagram_panel.sig_airfoil_changed.connect  (self.refresh)
        self._diagram_panel.sig_new_airfoil_ref1.connect (self.set_airfoil_ref1)
        self._diagram_panel.sig_new_airfoil_ref2.connect (self.set_airfoil_ref2)

        # connect to signals of self

        self.sig_airfoil_changed.connect (self.refresh)

        # connect signals to slots of diagram

        self.sig_airfoil_changed.connect        (self._diagram_panel.on_airfoil_changed)
        self.sig_airfoil_target_changed.connect (self._diagram_panel.on_target_changed)
        self.sig_bezier_changed.connect         (self._diagram_panel.on_bezier_changed)
        self.sig_panelling_changed.connect      (self._diagram_panel.on_airfoil_changed)

        self.sig_enter_bezier_mode.connect      (self._diagram_panel.on_bezier_mode)
        self.sig_enter_blend.connect            (self._diagram_panel.on_blend_airfoil)
        self.sig_enter_edit_mode.connect        (self._diagram_panel.on_edit_mode)
        self.sig_enter_panelling.connect        (self._diagram_panel.on_enter_panelling)




    def __repr__(self) -> str:
        # overwritten to get a nice print string 
        text = f""  
        return f"<{type(self).__name__}{text}>"


    def _init_layout (self): 
        """ init main layout with the different panels """

        #  ||               lower                         >||
        #  || file panel ||        data panel             >||
        #                 | Geometry  | Coordinates | ... >| 

        l_data = QHBoxLayout()
        l_data.addWidget (Panel_Geometry    (self, self.airfoil))
        l_data.addWidget (Panel_Panels      (self, self.airfoil))
        l_data.addWidget (Panel_LE_TE       (self, self.airfoil))
        l_data.addWidget (Panel_Bezier      (self, self.airfoil))
        l_data.addWidget (Panel_Bezier_Match(self, self.airfoil))
        l_data.addStretch (1)        
        l_data.setContentsMargins (QMargins(0, 0, 0, 0))
        self._data_panel.setLayout (l_data)

        l_file = QHBoxLayout()
        l_file.addWidget (Panel_File_Edit   (self, self.airfoil))
        l_file.addWidget (Panel_File_View   (self, self.airfoil))
        l_file.setContentsMargins (QMargins(0, 0, 0, 0))
        self._file_panel.setLayout (l_file)

        l_lower = QHBoxLayout()
        l_lower.addWidget (self._file_panel)
        l_lower.addWidget (self._data_panel, stretch=1)
        # l_lower.addStretch (1)
        l_lower.setContentsMargins (QMargins(0, 0, 0, 0))
        lower = QWidget ()
        lower.setMinimumHeight(180)
        lower.setMaximumHeight(180)
        lower.setLayout (l_lower)

        # main layout with diagram panel and lower 

        l_main = QVBoxLayout () 
        l_main.addWidget (self._diagram_panel, stretch=2)
        l_main.addWidget (lower)
        l_main.setContentsMargins (QMargins(5, 5, 5, 5))

        return l_main 


    @property
    def edit_mode (self) -> bool: 
        """ True if self is not in view mode"""
        return self._edit_mode


    def modify_airfoil (self):
        """ modify airfoil - switch to edit mode """
        if self.edit_mode: return 

        # enter edit_mode - create working copy as splined airfoil 
        try:                                            # normal airfoil - allows new geometry
            airfoil  = self._airfoil.asCopy (nameExt=None, geometry=GEO_SPLINE)
        except:                                         # bezier or hh does not allow new geometry
            airfoil  = self._airfoil.asCopy (nameExt=None)
        airfoil.useAsDesign()                           # will have another visualization 
        airfoil.normalize(just_basic=True)              # just normalize coordinates - not spline         

        self.set_edit_mode (True, airfoil)       


    def modify_airfoil_finished (self, ok=False):
        """ modify airfoil finished - switch to view mode """

        if not self.edit_mode: return 

        if ok:
            dlg = Airfoil_Save_Dialog (parent=self, getter=self.airfoil)
            ok_save = dlg.exec()
            if not ok_save: return                      # save was cancelled - return to edit mode 

        # leave edit_mode - restore original airfoil 
        if not ok:
            airfoil = self._airfoil_org                 # restore old airfoil 
        else: 
            airfoil = self._airfoil

        airfoil.useAsDesign (False)                     # will have another visualization 
        airfoil.set_isModified (False)                  # just sanity

        self.set_edit_mode (False, airfoil)       


    def new_as_Bezier (self):
        """ create new Bezier airfoil based on current airfoil and switch to edit mode """

        # enter edit_mode - create working copy as splined airfoil 
        airfoil_bez = Airfoil_Bezier.onAirfoil (self._airfoil)
        airfoil_bez.useAsDesign()                           # will have another visualization 

        self.set_edit_mode (True, airfoil_bez)       

        self.set_airfoil_target (None, refresh=False)       # current will be reference for Bezier
        self.sig_enter_bezier_mode.emit(True)


    def set_edit_mode (self, aBool : bool, for_airfoil):
        """ switch edit / view mode """

        if self._edit_mode != aBool: 
            self._edit_mode = aBool
            
            if self._edit_mode:
                self._airfoil_org = self._airfoil       # enter edit_mode - save original 

                # save possible example to file to ease consistent further handling in widgets
                if self._airfoil.isExample: self._airfoil.save()
            else: 
                self._airfoil_org = None                # leave edit_mode - remove original 
                self._airfoil_target = None            

            self.set_airfoil (for_airfoil, silent=True)

            self.sig_enter_edit_mode.emit()
            self.sig_airfoil_changed.emit()             # signal new airfoil 
        

    def refresh(self):
        """ refreshes all child panels of edit_panel """
        self._data_panel.refresh()
        self._file_panel.refresh()


    def airfoil (self) -> Airfoil:
        """ encapsulates current airfoil. Childs should acces only via this function
        to enable a new airfoil to be set """
        return self._airfoil

    def airfoils (self) -> Airfoil:
        """ list of airfoils (current, ref1 and ref2) 
        Childs should acces only via this function to enable a new airfoil to be set """
        airfoils = [self._airfoil]
        if self.airfoil_ref1:       airfoils.append (self.airfoil_ref1)
        if self.airfoil_ref2:       airfoils.append (self.airfoil_ref2)
        if self.airfoil_target:     airfoils.append (self.airfoil_target)
        if self.airfoil_org:        airfoils.append (self.airfoil_org)

        # remove duplicates 
        airfoils = list(dict.fromkeys(airfoils))

        return airfoils


    def set_airfoil (self, aNew : Airfoil , silent=False):
        """ encapsulates current airfoil. Childs should acces only via this function
        to enable a new airfoil to be set """

        self._airfoil = aNew
        logger.debug (f"New airfoil: {aNew.name}")
        self.setWindowTitle (AppName + "  v" + str(AppVersion) + "  [" + self.airfoil().fileName + "]")
        if not silent: 
            self.sig_airfoil_changed.emit ()


    @property
    def airfoil_ref1 (self) -> Airfoil:
        """ airfoil for reference 1"""
        return self._airfoil_ref1
    def set_airfoil_ref1 (self, airfoil: Airfoil | None = None): 
        self._airfoil_ref1 = airfoil 
        if airfoil: airfoil.set_usedAs (usedAs.REF1)


    @property
    def airfoil_ref2 (self) -> Airfoil:
        """ airfoil for reference 2"""
        return self._airfoil_ref2
    def set_airfoil_ref2 (self, airfoil: Airfoil | None = None): 
        self._airfoil_ref2 = airfoil 
        if airfoil: airfoil.set_usedAs (usedAs.REF2)


    @property
    def airfoil_target (self) -> Airfoil:
        """ target airfoil for match Bezier or 2nd airfoil doing Blend"""
        if self._airfoil_target is None: 
            return self._airfoil_org
        else: 
            return self._airfoil_target
    

    def set_airfoil_target (self, airfoil: Airfoil | None = None, refresh=True): 

        if airfoil is not None: 
            airfoil.set_usedAs (usedAs.TARGET)
        elif self._airfoil_target:                                  # reset the current/old target 
            self._airfoil_target.set_usedAs (usedAs.NORMAL) 
        self._airfoil_target = airfoil 
        
        self.sig_airfoil_target_changed.emit(refresh)


    @property
    def airfoil_org (self) -> Airfoil:
        """ the original airfoil during edit mode"""
        return self._airfoil_org


    # --- airfoil functions -----------------------------------------------



    def blend_with (self): 
        """ run blend airfoil with dialog to blend current with another airfoil""" 

        self.sig_enter_blend.emit()

        dialog = Blend_Airfoil (self, self.airfoil(), self.airfoil_org)  

        dialog.sig_airfoil_changed.connect (self.sig_airfoil_changed.emit)
        dialog.sig_airfoil2_changed.connect (self.set_airfoil_target)
        dialog.exec()     

        if dialog.airfoil2 is not None: 
            # do final blend with high quality (splined) 
            self.airfoil().geo.blend (self.airfoil_org.geo, 
                                      dialog.airfoil2.geo, 
                                      dialog.blendBy) 

        self.sig_airfoil_changed.emit()


    def repanel_airfoil (self): 
        """ run repanel dialog""" 

        self.sig_enter_panelling.emit()

        dialog = Repanel_Airfoil (self, self.airfoil().geo)

        dialog.sig_new_panelling.connect (self.sig_panelling_changed.emit)
        dialog.exec()     

        if dialog.has_been_repaneled:
            # finalize modifications 
            self.airfoil().geo.repanel (just_finalize=True)                

        self.sig_airfoil_changed.emit()



    # --- private ---------------------------------------------------------

    def _on_leaving_edit_mode (self) -> bool: 
        """ handle user wants to leave edit_mode"""
        #todo 
        return True 


    def _save_settings (self):
        """ save settings to file """

        # save Window size and position 
        Settings().set('window_geometry', self.normalGeometry ().getRect())
        Settings().set('window_maximize', self.isMaximized())

        # save panelling values 
        Settings().set('spline_nPanels',  Panelling_Spline().nPanels)
        Settings().set('spline_le_bunch', Panelling_Spline().le_bunch)
        Settings().set('spline_te_bunch', Panelling_Spline().te_bunch)

        Settings().set('bezier_nPanels',  Panelling_Bezier().nPanels)
        Settings().set('bezier_le_bunch', Panelling_Bezier().le_bunch)
        Settings().set('bezier_te_bunch', Panelling_Bezier().te_bunch)


    def _load_panelling_settings (self):
        """ load default panelling settings from file """

        nPanels  = Settings().get('spline_nPanels', None)
        le_bunch = Settings().get('spline_le_bunch', None)
        te_bunch = Settings().get('spline_te_bunch', None)

        if nPanels:     Panelling_Spline._nPanels = nPanels
        if le_bunch is not None:    Panelling_Spline._le_bunch = le_bunch
        if te_bunch is not None:    Panelling_Spline._te_bunch = te_bunch

        nPanels  = Settings().get('bezier_nPanels', None)
        le_bunch = Settings().get('bezier_le_bunch', None)
        te_bunch = Settings().get('bezier_te_bunch', None)

        if nPanels:     Panelling_Bezier._nPanels = nPanels
        if le_bunch is not None:    Panelling_Bezier._le_bunch = le_bunch
        if te_bunch is not None:    Panelling_Bezier._te_bunch = te_bunch


    @override
    def closeEvent  (self, event : QCloseEvent):
        """ main window is closed """

        self._save_settings ()

        self.sig_closing.emit (self.airfoil().pathFileName)

        event.accept()


#--------------------------------

if __name__ == "__main__":

    dev_mode = os.path.isdir(os.path.dirname(os.path.realpath(__file__)) +"\\test_airfoils")

    # init logging  

    if dev_mode:   
        init_logging (level= logging.DEBUG)             # INFO, DEBUG or WARNING
    else:                       
        init_logging (level= logging.WARNING)


    # command line arguments? 
    
    parser = argparse.ArgumentParser(prog=AppName, description='View and edit an airfoil')
    parser.add_argument("airfoil", nargs='*', help="Airfoil .dat or .bez file to show")
    args = parser.parse_args()
    if args.airfoil: 
        airfoil_file = args.airfoil[0]
    else: 
        if os.path.isdir(".\\test_airfoilsss"):
            airfoil_dir   =".\\test_airfoils"
            airfoil_files = [os.path.join(airfoil_dir, f) for f in os.listdir(airfoil_dir) if os.path.isfile(os.path.join(airfoil_dir, f))]
            airfoil_files = [f for f in airfoil_files if (f.endswith('.dat') or f.endswith('.bez'))]       
            airfoil_files = sorted (airfoil_files, key=str.casefold)
            airfoil_file = airfoil_files[0]
        else:
            airfoil_file = None

    app = QApplication(sys.argv)
    app.setStyle('fusion')

    # Strange: Without setStyleSheet, reset Widget.setPalette doesn't work .. !?
    # Segoe UI is the font of 'fusion' style 
    # font = QFont ()
    # print (font.defaultFamily(), font.family(), font.families())
    app.setStyleSheet ("QWidget { font-family: 'Segoe UI' }")

    Main = App_Main (airfoil_file)
    Main.show()
    app.exec()

    