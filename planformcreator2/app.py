#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    The Planform Creator 2 App 

    Object model overview (a little simplified) 

    App                                         - root frame 
        |-- Panel_File                          - file functions
        |-- Panel_Wing                          - wing data
                ...                             - ...

        |-- Diagram_Wing                        - wing overview diagram 
                |-- Item_Wing                   - Pyqtgraph Plot item for complete wing
                |-- Item_Wing_Airfoils          - Pyqtgraph Plot item for airfoils of wing 
                ...                             - ...
        |-- Diagram_Planform                    - planform diagram
        ...

        |-- Wing                                - Entry to model 
                |-- Planform                    - the planform - main object 
                ...                             - ...
"""

import os
import sys
import argparse
import logging
from importlib.metadata     import version
from packaging.version      import Version                                  # has to be installed

from PyQt6.QtCore           import pyqtSignal, QMargins, Qt
from PyQt6.QtWidgets        import QApplication, QMainWindow, QWidget 
from PyQt6.QtWidgets        import QGridLayout
from PyQt6.QtGui            import QCloseEvent, QGuiApplication


# Check version of installed airfoileditor package

AE_MIN_VERSION   = '4.2.0b2'                                    # min airfoileditor version required
AE_PACKAGE_NAME  = 'airfoileditor'                              # airfoileditor package name

try: 
    airfoileditor_version = version(AE_PACKAGE_NAME)
    if Version(airfoileditor_version) < Version(AE_MIN_VERSION):
        logging.error (f"Installed {AE_PACKAGE_NAME} version {airfoileditor_version} is too old - needed version {AE_MIN_VERSION}")
        sys.exit(0)
except Exception as e:
    logging.error(f"Required package {AE_PACKAGE_NAME} not found")
    sys.exit(0)
    

from airfoileditor.base.common_utils    import * 
from airfoileditor.base.panels          import *
from airfoileditor.base.widgets         import *
from airfoileditor.base.app_utils       import Settings, check_or_get_initial_file, Run_Checker, Update_Checker

from app_model              import App_Model, Mode_Id
from app_modes              import Modes_Manager, Mode_Modify

from ui.pc2_diagrams        import (Diagram_Wing, Diagram_Planform, Diagram_Airfoils, Diagram_Making_Of,
                                    Diagram_Wing_Analysis)


logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


#-------------------------------------------------------------------------------
# The App   
#-------------------------------------------------------------------------------

APP_NAME         = "PlanformCreator2"
PACKAGE_NAME     = "planformcreator2"
__version__      = "4.0_dev"                                    # hatch "version dynamic" reads this version for build



class Main (QMainWindow):
    '''
        App - Main Window 
    '''

    # Qt Signals 

    sig_closing                 = pyqtSignal(str)       # the app is closing with an airfoils pathFilename


    def __init__(self, initial_file):
        super().__init__()

        self._app_model         = None                        # the app model
        self._modes_manager     = None                        # app modes manager
        self._diagrams_panel    = None                        # main diagrams panel

        # --- init Settings, check for newer app version ---------------

        Settings.set_file (APP_NAME, file_extension= '.settings')

        is_first_run = Run_Checker.is_first_run (__version__)                 # to show Welcome message
        
        Update_Checker (self, APP_NAME, PACKAGE_NAME,  __version__) 


        # --- init App Model ---------------

        app_model = App_Model (workingDir_default=Settings.user_data_dir (APP_NAME))

        app_model.set_app_info (__version__, "No CHANGE_TEXT", is_first_run)

        # either airfoil file or Xoptfoil2 input file

        pc2_file = check_or_get_initial_file (initial_file)

        mode_to_start = Mode_Id.MODIFY

        # load initial settings like polar definitions 

        app_model.load_settings ()                                          # global settings 
        app_model.sig_new_wing.connect (self._set_win_title)                # update title on new wing
        app_model.sig_new_mode.connect (self._set_win_title)                # update title on new case

        self._app_model     = app_model                                     # keep for close 

        # --- init UI ---------------

        # main window style - dark or light mode

        logger.info (f"Initialize UI")

        self._set_win_title ()
        self._set_win_style ()
        self._set_win_geometry ()


        # app Modes and manager ---------------
        
        modes_manager = Modes_Manager (app_model)
        modes_manager.add_mode (Mode_Modify     (app_model))

        modes_manager.set_mode (mode_to_start, pc2_file)                    # set initial object in app_model
        modes_manager.sig_close_requested.connect (self.close)              # app close requested from mode view

        self._modes_manager = modes_manager                                 # keep as it hosts slots

        # main widgets and layout of app

        self._diagrams_panel = self._create_diagrams_panel ()               # main diagrams tab panel
        self._modes_data_panel = modes_manager.stacked_modes_panel()        # stacked widget with mode data panels

        l = QGridLayout () 
        l.addWidget (self._diagrams_panel, 0,0)                              
        l.addWidget (self._modes_data_panel, 1,0)        
        l.setRowStretch (0,1)
        l.setRowMinimumHeight (0,400)
        modes_manager.set_height (160, minimized=65)
        
        l.setSpacing (5)
        l.setContentsMargins (QMargins(5, 5, 5, 5))

        main = QWidget()
        main.setLayout (l) 
        self.setCentralWidget (main)

        # --- Enter event loop ---------------

        logger.info (f"{modes_manager.current_mode} ready")



    # --- private ---------------------------------------------------------


    def _set_win_title (self):
        """ set window title with airfoil or case name """

        pc2_file = self._app_model.wing.parm_fileName if self._app_model.wing is not None else "No Wing Loaded"
        self.setWindowTitle (APP_NAME + "  v" + str(__version__) + "  [" + pc2_file + "]")


    def _set_win_style (self):
        """ 
        Set window style according to settings
        """

        self.setWindowIcon (Icon ('AE_ico.ico'))                                    # get icon either in modules or in icons 

        scheme_name = Settings().get('color_scheme', Qt.ColorScheme.Unknown.name)   # either unknown (from System), Dark, Light
        QGuiApplication.styleHints().setColorScheme(Qt.ColorScheme[scheme_name])    # set scheme of QT
        Widget.light_mode = not (scheme_name == Qt.ColorScheme.Dark.name)           # set mode for Widgets


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

        s.save()


    @override
    def closeEvent  (self, event : QCloseEvent):
        """ main window is closed """

        # save airfoil settings in app settings
        #todo
        # self._app_model.save_settings (to_app_settings=True,
        #                                add_key  = self._diagram.name, 
        #                                add_value= self._diagram.settings())

        # terminate polar watchdog thread, clean up working dir 
        self._app_model.close()                            # finish app model

        # save e.g. diagram options 
        self._save_app_settings () 

        event.accept()


    def _create_diagrams_panel (self) -> Tab_Panel:
        """ upper UI main panel - Tab with diagrams"""

        diagrams = []
        diagrams.append (Diagram_Making_Of     (self._app_model))
        diagrams.append (Diagram_Wing          (self._app_model))
        diagrams.append (Diagram_Planform      (self._app_model))
        diagrams.append (Diagram_Airfoils      (self._app_model))
        diagrams.append (Diagram_Wing_Analysis (self._app_model))

        tab_panel = Tab_Panel (self)
        for diagram in diagrams:
            tab_panel.add_tab(diagram)
        tab_panel.setMinimumHeight(500)

        return tab_panel



#--------------------------------

def start ():
    """ start the app """

    # init logging - can be overwritten within a module  

    init_logging (level= logging.INFO)             # INFO, DEBUG or WARNING

    # command line arguments? 
    
    parser = argparse.ArgumentParser(prog=APP_NAME, description='View and modify an airfoil')
    parser.add_argument("airfoil", nargs='*', help="Airfoil .dat or .bez file to show")
    args = parser.parse_args()
    if args.airfoil: 
        initial_file = args.airfoil[0]
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