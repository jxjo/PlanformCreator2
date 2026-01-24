#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    The Planform Creator 2 App 

    Object model overview (a little simplified) 

    App                                         - Main 
        |-- App_Mode                            - different UI modes like Modify,
            |-- Data_Panel                      - UI lower data panel for mode 
                |-- Panel_Geometry              - UI single panel with fields 

            |-- Planform_Diagram                - UI upper diagram area 
                |-- Chord_Diagram_Item          - UI a single plot item within diagram
                    |-- Chord_Artist            - UI curvature plot artist
            
        |-- App_Model                           - App shell around data model allowing signals
            |-- Wing                            - wing object  
                |-- Planform                    - planform object of wing 
                    |-- WingSections            - wing sections of a planform
                        |-- Airfoil             - airfoil with wing section

"""

import os
import sys
import argparse
import logging

from PyQt6.QtCore           import QMargins, Qt
from PyQt6.QtWidgets        import QApplication, QMainWindow, QWidget 
from PyQt6.QtWidgets        import QGridLayout
from PyQt6.QtGui            import QCloseEvent, QGuiApplication, QIcon

# --- AE modules ---------------
    
from airfoileditor.resources            import get_icons_path as ae_icons_path
from airfoileditor.base.common_utils    import * 
from airfoileditor.base.widgets         import Icon, Widget
from airfoileditor.base.panels          import Tab_Panel, Win_Util
from airfoileditor.base.app_utils       import Settings, check_or_get_initial_file, Run_Checker, Update_Checker

# --- PC2 modules ---------------

PACKAGE_NAME    = "planformcreator2"

# DEV: when running app.py as main, set package path and property to allow relative imports
if __name__ == "__main__":  
    sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    __package__ = PACKAGE_NAME

from .resources              import get_icon_path
from .app_model              import App_Model, Mode_Id
from .app_modes              import Modes_Manager, Mode_Modify

from .ui.pc2_diagrams        import (Diagram_Wing, Diagram_Planform, Diagram_Airfoils, Diagram_Making_Of,
                                    Diagram_Wing_Analysis)


logger = logging.getLogger(__name__)
# logger.setLevel(logging.DEBUG)


#-------------------------------------------------------------------------------
# The App   
#-------------------------------------------------------------------------------

APP_NAME         = "PlanformCreator2"
__version__      = "4.0.2-beta"                            # hatch "version dynamic" reads this version for build


class Main (QMainWindow):
    '''
        App - Main Window 
    '''

    def __init__(self, initial_file):
        super().__init__()

        logger.info (f"Init Main Window")

        self._app_model         = None                        # the app model
        self._modes_manager     = None                        # app modes manager
        self._diagrams_panel    = None                        # main diagrams panel

        # --- init Settings, check for newer app version ---------------

        Settings.set_file (APP_NAME, file_extension= '.settings')

        is_first_run = Run_Checker.is_first_run (__version__)                 # to show Welcome message
        
        Update_Checker (self, APP_NAME, PACKAGE_NAME,  __version__) 


        # --- init App Model ---------------

        self._app_model = App_Model (workingDir_default=Settings.user_data_dir (APP_NAME))
        self._app_model.set_app_info (__version__, "No CHANGE_TEXT", is_first_run)

        # either airfoil file or Xoptfoil2 input file

        pc2_file = check_or_get_initial_file (initial_file)


        # --- init UI ---------------

        # main window style - dark or light mode

        logger.info (f"Initialize UI")
 
        self._set_win_style (ae_icons_path(), 'PC2.ico')
        self._set_win_title ()
        self._set_win_geometry ()

        self._app_model.sig_new_wing.connect (self._set_win_title)          # update title on new wing
        self._app_model.sig_new_mode.connect (self._set_win_title)          # update title on new case

        # app Modes and manager ---------------
        
        modes_manager = Modes_Manager (self._app_model)
        modes_manager.add_mode (Mode_Modify (self._app_model))

        modes_manager.set_mode (Mode_Id.MODIFY, pc2_file)                   # set initial object in app_model
        modes_manager.sig_close_requested.connect (self.close)              # app close requested from mode view

        self._modes_manager = modes_manager                                 # keep as it hosts slots

        # main widgets and layout of app

        diagram     = self._create_diagrams_panel ()                        # main diagrams tab panel
        modes_panel = modes_manager.stacked_modes_panel()                   # stacked widget with mode data panels

        l = QGridLayout () 
        l.addWidget (diagram, 0,0)                              
        l.addWidget (modes_panel, 1,0)        
        l.setRowStretch (0,1)
        l.setRowMinimumHeight (0,400)
        modes_manager.set_height (160, minimized=65)
        
        l.setSpacing (5)
        l.setContentsMargins (QMargins(5, 5, 5, 5))

        main = QWidget()
        main.setLayout (l) 
        self.setCentralWidget (main)

        self._diagrams_panel = diagram

        # --- Enter event loop ---------------

        logger.info (f"{modes_manager.current_mode} ready")



    # --- private ---------------------------------------------------------


    def _set_win_title (self):
        """ set window title with airfoil or case name """

        pc2_file = self._app_model.wing.parm_fileName if self._app_model.wing is not None else "No Wing Loaded"
        self.setWindowTitle (APP_NAME + "  v" + str(__version__) + "  [" + pc2_file + "]")


    def _set_win_style (self, icons_path : Path = None, app_icon_name : str  = None):
        """ 
        Set window style according to settings
        """

        # set resources dir for Icons
        Icon.ICONS_PATH = icons_path

        # get and set app icon  
        app_icon_path = get_icon_path(app_icon_name) 
        if app_icon_path:
            self.setWindowIcon (QIcon (str(app_icon_path)))  

        # set dark or light mode
        scheme_name = Settings().get('color_scheme', Qt.ColorScheme.Unknown.name)   # either unknown (from System), Dark, Light
        QGuiApplication.styleHints().setColorScheme(Qt.ColorScheme[scheme_name])    # set scheme of QT

        # set mode for Widgets
        Widget.light_mode = not (scheme_name == Qt.ColorScheme.Dark.name)    



    def _set_win_geometry (self):
        """ set window geometry from settings """

        app_settings = Settings()                      # load app settings

        geometry = app_settings.get('window_geometry', [])
        maximize = app_settings.get('window_maximize', False)
        Win_Util.set_initialWindowSize (self, size_frac= (0.85, 0.80), pos_frac=(0.1, 0.1),
                                        geometry=geometry, maximize=maximize)


    def _save_app_settings (self):
        """ save application settings to file """

        s = Settings()

        s.set ('window_maximize', self.isMaximized())
        s.set ('window_geometry', self.normalGeometry().getRect())

        wing = self._app_model.wing
        if wing and not wing.is_new_wing: 
            s.set ('last_opened', wing.parm_pathFileName_abs)
        else:
            s.set ('last_opened', None)

        s.set ('current_diagram', self._diagrams_panel.current_tab_name)

        s.save()


    @override
    def closeEvent  (self, event : QCloseEvent):
        """ main window is closed """

        # terminate polar watchdog thread, clean up working dir 
        self._app_model.close()                           

        # save e.g. diagram options 
        self._save_app_settings () 

        event.accept()


    def _create_diagrams_panel (self) -> Tab_Panel:
        """ upper UI main panel - Tab with diagrams"""

        diagrams = []
        diagrams.append (Diagram_Making_Of     (self, self._app_model))
        diagrams.append (Diagram_Wing          (self, self._app_model))
        diagrams.append (Diagram_Planform      (self, self._app_model))
        diagrams.append (Diagram_Airfoils      (self, self._app_model))
        diagrams.append (Diagram_Wing_Analysis (self, self._app_model))
        tab_panel = Tab_Panel (parent=self)
        for diagram in diagrams:
            tab_panel.add_tab(diagram)
        tab_panel.setMinimumHeight(500)

        tab_panel.set_current_tab (Settings().get('current_diagram', None))

        return tab_panel



#--------------------------------

def start ():
    """ start the app """

    # init logging - can be overwritten within a module  

    init_logging (level= logging.INFO)             # INFO, DEBUG or WARNING

    # command line arguments? 
    
    parser = argparse.ArgumentParser(prog=APP_NAME, description='Create a wing planform')
    parser.add_argument("file", nargs='*', help="Project .pc2 file to show")
    args = parser.parse_args()
    if args.file: 
        initial_file = args.file[0]
    else: 
        initial_file = None

    # init Qt Application and style  

    app = QApplication(sys.argv)
    app.setStyle('fusion')

    main = Main (initial_file)
    main.show()
    rc = app.exec()
    return rc 


if __name__ == "__main__":
    
    sys.exit (start())