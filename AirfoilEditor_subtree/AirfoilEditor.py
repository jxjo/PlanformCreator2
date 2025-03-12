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

from PyQt6.QtCore           import QMargins, QThread
from PyQt6.QtWidgets        import QApplication, QMainWindow, QWidget 
from PyQt6.QtWidgets        import QVBoxLayout, QHBoxLayout
from PyQt6.QtGui            import QCloseEvent, QGuiApplication

# let python find the other modules in modules relativ to path of self - ! before python system modules
# common modules hosted by AirfoilEditor 
# sys.path.insert (1,os.path.join(Path(__file__).parent , 'modules'))

# let python find the other modules in modules relativ to path of self  
sys.path.append(os.path.join(Path(__file__).parent , 'modules'))

from model.airfoil          import Airfoil, usedAs
from model.airfoil_geometry import Panelling_Spline, Panelling_Bezier
from model.polar_set        import Polar_Definition, Polar_Set
from model.xo2_driver       import Worker
from model.case             import Case_Direct_Design

from base.common_utils      import * 
from base.panels            import Container_Panel, Win_Util
from base.widgets           import *

from airfoil_widgets        import * 
from airfoil_diagrams       import * 

from airfoil_dialogs        import Airfoil_Save_Dialog, Blend_Airfoil, Repanel_Airfoil
from airfoil_ui_panels      import * 


import logging
logger = logging.getLogger(__name__)
# logger.setLevel(logging.DEBUG)



#-------------------------------------------------------------------------------
# The App   
#-------------------------------------------------------------------------------


APP_NAME         = "Airfoil Editor"
APP_VERSION      = "3.0"


class App_Main (QMainWindow):
    '''
        The Airfoil Editor App
    '''

    name = APP_NAME  

    WORKER_MIN_VERSION          = '1.0.5'

    # Qt Signals 

    sig_new_airfoil             = pyqtSignal()          # new airfoil selected 
    sig_new_design              = pyqtSignal()          # new airfoil design created 
    sig_airfoil_changed         = pyqtSignal()          # airfoil data changed 

    sig_airfoil_target_changed  = pyqtSignal()          # target airfoil changed 
    sig_airfoils_ref_changed    = pyqtSignal()          # list of reference airfoils changed
    sig_bezier_changed          = pyqtSignal(Line.Type) # new bezier during match bezier 
    sig_panelling_changed       = pyqtSignal()          # new panelling
    sig_blend_changed           = pyqtSignal()          # new (intermediate) blend 
    sig_polar_set_changed       = pyqtSignal()          # new polar sets attached to airfoil

    sig_enter_panelling         = pyqtSignal()          # starting panelling dialog
    sig_enter_blend             = pyqtSignal()          # starting blend airfoil with

    sig_closing                 = pyqtSignal(str)       # the app is closing with an airfoils pathFilename


    def __init__(self, airfoil_file, parent=None):
        super().__init__(parent)

        self._airfoil        = None                     # current airfoil 
        self._airfoil_org    = None                     # airfoil saved in edit_mode 
        self._airfoils_ref   = []                       # reference airfoils 
        self._airfoil_target = None                     # target for match Bezier    

        self._polar_definitions = None                  # current polar definitions  
        self._watchdog          = None                  # polling thread for new olars

        self._edit_mode         = False                 # edit/view mode of app 
        self._case              = None                  # design Case holding all designs 

        self._data_panel        = None                  # main panels of app
        self._file_panel        = None
        self._diagram           = None


        # if called from other applcation (PlanformCreator) make it modal to this 

        if parent is not None:
            self.setWindowModality(Qt.WindowModality.ApplicationModal)  

        # get icon either in modules or in icons 
        
        self.setWindowIcon (Icon ('AE_ico.ico'))

        # get initial window size from settings

        Settings.belongTo (__file__, nameExtension=None, fileExtension= '.settings')
        geometry = Settings().get('window_geometry', [])
        maximize = Settings().get('window_maximize', False)
        Win_Util.set_initialWindowSize (self, size_frac= (0.60, 0.70), pos_frac=(0.1, 0.1),
                                        geometry=geometry, maximize=maximize)
        
        # load settings

        self._load_settings ()

        # if no initial airfoil file, try to get last openend airfoil file 

        if not airfoil_file: 
            airfoil_file = Settings().get('last_opened', default=None) 

        airfoil = create_airfoil_from_path(self, airfoil_file, example_if_none=True, message_delayed=True)

        self.set_airfoil (airfoil, silent=True)

        # Worker for polar generation ready? 

        Worker().isReady (os.path.dirname(os.path.abspath(__file__)), min_version=self.WORKER_MIN_VERSION)
        if Worker.ready:
            Worker().clean_workingDir (self.airfoil().pathName)

        # init main layout of app

        self._data_panel    = Container_Panel (title="Data panel")
        self._file_panel    = Container_Panel (title="File panel", width=240)
        self._diagram       = Diagram_Airfoil_Polar (self, self.airfoils, 
                                                     polar_defs_fn = self.polar_definitions,
                                                     diagram_settings= Settings().get('diagram_settings', []))

        l_main = self._init_layout() 

        container = QWidget()
        container.setLayout (l_main) 
        self.setCentralWidget (container)


        # ---- signals and slots --------------------------------------------------------------

        # install watchdog for poars generated by Worker 

        if Worker.ready:
             self._watchdog = Polar_Watchdog (self) 
             self._watchdog.sig_new_polars.connect         (self._diagram.on_new_polars)
             self._watchdog.start()

        # connect diagram signals to slots of self

        self._diagram.sig_airfoil_changed.connect     (self._on_airfoil_changed)
        self._diagram.sig_polar_def_changed.connect   (self.refresh_polar_sets)
        self._diagram.sig_airfoil_ref_changed.connect (self.set_airfoil_ref)

        # connect self signals to slots of self

        self.sig_new_airfoil.connect            (self.refresh)
        self.sig_new_design.connect             (self.refresh)
        self.sig_airfoil_changed.connect        (self._on_airfoil_changed)

        # connect self signals to slots of diagram

        self.sig_new_airfoil.connect            (self._diagram.on_airfoil_changed)
        self.sig_new_design.connect             (self._diagram.on_new_design)
        self.sig_airfoil_changed.connect        (self._diagram.on_airfoil_changed)
        self.sig_airfoil_target_changed.connect (self._diagram.on_target_changed)
        self.sig_bezier_changed.connect         (self._diagram.on_bezier_changed)
        self.sig_panelling_changed.connect      (self._diagram.on_airfoil_changed)
        self.sig_blend_changed.connect          (self._diagram.on_airfoil_changed)
        self.sig_polar_set_changed.connect      (self._diagram.on_polar_set_changed)
        self.sig_airfoils_ref_changed.connect   (self._diagram.on_airfoils_ref_changed)

        self.sig_enter_blend.connect            (self._diagram.on_blend_airfoil)
        self.sig_enter_panelling.connect        (self._diagram.on_enter_panelling)


    @override
    def __repr__(self) -> str:
        return f"<{type(self).__name__}>"


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
        l_main.addWidget (self._diagram, stretch=2)
        l_main.addWidget (lower)
        l_main.setContentsMargins (QMargins(5, 5, 5, 5))

        return l_main 


    @property
    def edit_mode (self) -> bool: 
        """ True if self is not in view mode"""
        return self._edit_mode

    @property
    def case (self) -> Case_Direct_Design:
        """ design case holding all design airfoils"""
        return self._case 


    def modify_airfoil (self):
        """ modify airfoil - switch to edit mode - create Case """
        if self.edit_mode: return 

        # create new Design Case and get/create first design 

        self._case = Case_Direct_Design (self._airfoil)

        first_design = self._case.first_working_design() 

        self.set_edit_mode (True, first_design)       



    def modify_airfoil_finished (self, ok=False):
        """ 
        modify airfoil finished - switch to view mode 
            - ok == False: edit mode was cancelled 
            - ok == True:  user wants to finish 
        """

        remove_designs  = None                              # let case.close decide to remove design dir 

        # sanity
        if not self.edit_mode: return 

        if ok:
            # create new, final airfoil based on actual design and path from airfoil org 
            new_airfoil = self.case.get_final_from_design (self.airfoil_org, self.airfoil())

            # dialog to edit name, chose path, ..

            dlg = Airfoil_Save_Dialog (parent=self, getter=new_airfoil)
            ok_save = dlg.exec()

            if not ok_save: 
                return                          # save was cancelled - return to edit mode 
            else: 
                remove_designs = dlg.remove_designs

        # leave edit_mode  

        if not ok:
            new_airfoil = self._airfoil_org                 # restore original airfoil 

        # close case 

        self.case.close (remove_designs=remove_designs)     # shut down case
        self._case = None                                

        self.set_edit_mode (False, new_airfoil)       


    def new_as_Bezier (self):
        """ create new Bezier airfoil based on current airfoil, create Case, switch to edit mode """

        # create initial Bezier airfoil based on current

        airfoil_bez = Airfoil_Bezier.onAirfoil (self._airfoil)

        # create new Design Case and get/create first design 

        self._case = Case_Direct_Design (airfoil_bez)
        first_design = self._case.first_working_design() 

        self.set_edit_mode (True, first_design)       
        self.set_airfoil_target (None, refresh=False)       # current will be reference for Bezier


    def set_edit_mode (self, aBool : bool, for_airfoil):
        """ switch edit / view mode """

        if self._edit_mode != aBool: 
            self._edit_mode = aBool
            
            if self._edit_mode:

                # save possible example to file to ease consistent further handling in widgets
                if self._airfoil.isExample: self._airfoil.save()
                self._airfoil_org = self._airfoil       # enter edit_mode - save original 

            else: 
                self._airfoil_org = None                # leave edit_mode - remove original 
                self._airfoil_target = None   
                self._case = None         

            self.set_airfoil (for_airfoil, silent=True)

            self.sig_new_airfoil.emit()                 # signal new airfoil 
        

    def refresh(self):
        """ refreshes all child panels of edit_panel """
        self._data_panel.refresh()
        self._file_panel.refresh()


    def refresh_polar_sets (self):
        """ refresh polar sets of all airfoils"""

        for airfoil in self.airfoils():
            airfoil.set_polarSet (Polar_Set (airfoil, polar_def=self.polar_definitions(), only_active=True))

        self.sig_polar_set_changed.emit()


    def airfoil (self) -> Airfoil:
        """ encapsulates current airfoil. Childs should acces only via this function
        to enable a new airfoil to be set """
        return self._airfoil


    def airfoils (self) -> list [Airfoil]:
        """ list of airfoils (current, ref1 and ref2) """
        airfoils = [self._airfoil]
        if self.airfoil_target:     airfoils.append (self.airfoil_target)
        if self.airfoil_org:        airfoils.append (self.airfoil_org)
        if self.airfoils_ref:       airfoils.extend (self.airfoils_ref)

        # remove duplicates 
        airfoils = list(dict.fromkeys(airfoils))

        return airfoils


    def set_airfoil (self, aNew : Airfoil , silent=False):
        """ set new current aurfoil """

        self._airfoil = aNew
        self._airfoil.set_polarSet (Polar_Set (aNew, polar_def=self.polar_definitions(), only_active=True))

        logger.debug (f"Load new airfoil: {aNew.name}")
        self.setWindowTitle (APP_NAME + "  v" + str(APP_VERSION) + "  [" + self.airfoil().fileName + "]")

        if not silent: 
            if self._airfoil.usedAsDesign:
                self.sig_new_design.emit ()                    # new DESIGN - inform diagram
            else:
                self.sig_new_airfoil.emit ()


    def polar_definitions (self) -> list [Polar_Definition]:
        """ list of current polar definitions """

        if not self._polar_definitions: 
            self._polar_definitions = [Polar_Definition()]
        return self._polar_definitions

      
    @property
    def airfoils_ref (self) -> list[Airfoil]:
        """ reference airfoils"""
        return self._airfoils_ref
    
    def set_airfoil_ref (self, cur_airfoil_ref: Airfoil | None,
                               new_airfoil_ref: Airfoil | None):
        """ adds, replace, delete airfoil to the list of reference airfoils"""

        # check if already in list 
        if new_airfoil_ref in self.airfoils_ref: return 

        if new_airfoil_ref:
            new_airfoil_ref.set_polarSet (Polar_Set (new_airfoil_ref, polar_def=self.polar_definitions(), only_active=True))
            new_airfoil_ref.set_usedAs (usedAs.REF)   

        if cur_airfoil_ref:
            # replace or delete existing 
            i = self.airfoils_ref.index (cur_airfoil_ref)
            if new_airfoil_ref:
               self.airfoils_ref[i] = new_airfoil_ref
            else: 
                del self.airfoils_ref [i]
        else:
            # add new  
            self.airfoils_ref.append(new_airfoil_ref)

        self.sig_airfoils_ref_changed.emit()


    @property
    def airfoil_target (self) -> Airfoil:
        """ target airfoil for match Bezier or 2nd airfoil doing Blend"""
        if self._airfoil_target is None: 
            return self._airfoil_org
        else: 
            return self._airfoil_target
    

    def set_airfoil_target (self, airfoil: Airfoil | None = None, refresh=True): 

        if airfoil is not None: 
            airfoil.set_polarSet (Polar_Set (airfoil, polar_def=self.polar_definitions(), only_active=True))
            airfoil.set_usedAs (usedAs.TARGET)
        elif self._airfoil_target:                                  # reset the current/old target 
            self._airfoil_target.set_usedAs (usedAs.NORMAL) 
        self._airfoil_target = airfoil 
        
        self.sig_airfoil_target_changed.emit()              # refresh


    @property
    def airfoil_org (self) -> Airfoil:
        """ the original airfoil during edit mode"""
        return self._airfoil_org


    # --- airfoil functions -----------------------------------------------


    def blend_with (self): 
        """ run blend airfoil with dialog to blend current with another airfoil""" 

        self.sig_enter_blend.emit()

        dialog = Blend_Airfoil (self, self.airfoil(), self.airfoil_org)  

        dialog.sig_blend_changed.connect (self.sig_blend_changed.emit)
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

    def _on_airfoil_changed (self):
        """ slot handle airfoil chnged signal - save new design"""

        if self.airfoil().usedAsDesign: 

            self.case.add_design(self.airfoil())

            self.set_airfoil (self.airfoil())                # new DESIGN - inform diagram       

        self.refresh () 


    def _on_leaving_edit_mode (self) -> bool: 
        """ handle user wants to leave edit_mode"""
        #todo 
        return True 


    def _save_settings (self):
        """ save settings to file """

        # get settings dict to avoid a lot of read/write
        settings = Settings().get_dataDict ()

        # save Window size and position 
        toDict (settings,'window_maximize', self.isMaximized())
        toDict (settings,'window_geometry', self.normalGeometry().getRect())

        # save panelling values 
        toDict (settings,'spline_nPanels',  Panelling_Spline().nPanels)
        toDict (settings,'spline_le_bunch', Panelling_Spline().le_bunch)
        toDict (settings,'spline_te_bunch', Panelling_Spline().te_bunch)

        toDict (settings,'bezier_nPanels',  Panelling_Bezier().nPanels)
        toDict (settings,'bezier_le_bunch', Panelling_Bezier().le_bunch)
        toDict (settings,'bezier_te_bunch', Panelling_Bezier().te_bunch)

        # save airfoils
        airfoil : Airfoil = self.airfoil_org if self.airfoil().usedAsDesign else self.airfoil()

        if not airfoil.isExample:
            toDict (settings,'last_opened', airfoil.pathFileName)

        ref_list = []
        for airfoil in self.airfoils_ref:
            ref_list.append (airfoil.pathFileName)
        toDict (settings,'reference_airfoils', ref_list)

        # save polar definitions 
        def_list = []
        for polar_def in self.polar_definitions():
            def_list.append (polar_def._as_dict())
        toDict (settings,'polar_definitions', def_list)

        # save polar diagram settings 
        toDict (settings,'diagram_settings', self._diagram._as_dict_list())

        Settings().write_dataDict (settings)


    def _load_settings (self):
        """ load default settings from file """

        # panelling 

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

        # polar definitions 

        self._polar_definitions = []
        for def_dict in Settings().get('polar_definitions', []):
            self._polar_definitions.append(Polar_Definition(dataDict=def_dict))

        # reference airfoils 

        for pathFileName in Settings().get('reference_airfoils', []):
            try: 
                airfoil = Airfoil(pathFileName=pathFileName)
                airfoil.load ()
                self.set_airfoil_ref (None, airfoil)
            except: 
                pass


    @override
    def closeEvent  (self, event : QCloseEvent):
        """ main window is closed """

        # remove lost worker input files 
        if Worker.ready:
            Worker().clean_workingDir (self.airfoil().pathName)

        # terminate polar watchdog thread 

        if self._watchdog:
            self._watchdog.requestInterruption ()
            self._watchdog.wait()

        # save e..g diagram options 
        self._save_settings ()

        # inform parent (PlanformCreator) 
        self.sig_closing.emit (self.airfoil().pathFileName)

        event.accept()


# -----------------------------------------------------------------------------


class Polar_Watchdog (QThread):
    """ 
    Long running QThread to check if new polars were generated 
        - signal new polars 

    All current Polar_Tasks are looped frequently.
    Each tries to load new polars from the polar directory.
    If new polars could be loaded, self will signal to parent to refresh diagrams.

    """

    sig_new_polars      = pyqtSignal ()


    def __repr__(self) -> str:
        """ nice representation of self """
        return f"<{type(self).__name__}>"
    
    @override
    def run (self) :
        # Note: This is never called directly. It is called by Qt once the
        # thread environment has been set up. 
        # Thread is started with .start()

        logger.info (f"{self} --> starting soon")
        self.msleep (1000)                                  # initial wait before polling begins 

        while not self.isInterruptionRequested():

            # check for new polars 

            n_new_polars = 0 
            polar_tasks = Polar_Task.get_instances () 

            for task in polar_tasks: 

                n_new_polars += task.load_polars()
                if task.isCompleted():
                    task.finalize()

            # if new polars loaded signal 

            if n_new_polars:

                self.sig_new_polars.emit()
                logger.debug (f"{self} --> {n_new_polars} new polars")

            self.msleep (500)

        return 


#--------------------------------


if __name__ == "__main__":

    dev_mode = os.path.isdir(os.path.dirname(os.path.realpath(__file__)) +"\\test_airfoils")

    # init logging - can be overwritten within a module  

    if dev_mode:   
        init_logging (level= logging.DEBUG)             # INFO, DEBUG or WARNING
    else:                       
        init_logging (level= logging.WARNING)

    # command line arguments? 
    
    parser = argparse.ArgumentParser(prog=APP_NAME, description='View and edit an airfoil')
    parser.add_argument("airfoil", nargs='*', help="Airfoil .dat or .bez file to show")
    args = parser.parse_args()
    if args.airfoil: 
        airfoil_file = args.airfoil[0]
    else: 
        airfoil_file = None

    # init Qt Application and style  

    app = QApplication(sys.argv)
    app.setStyle('fusion')

    # Strange: Without setStyleSheet, reset Widget.setPalette doesn't work .. !?
    # Segoe UI is the font of 'fusion' style 
    app.setStyleSheet ("QWidget { font-family: 'Segoe UI' }")

    # set dark / light mode for widgets depending on system mode 

    scheme = QGuiApplication.styleHints().colorScheme()
    Widget.light_mode = not (scheme == Qt.ColorScheme.Dark)

    Main = App_Main (airfoil_file)
    Main.show()
    app.exec()

    