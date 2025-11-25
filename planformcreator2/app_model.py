#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""

App model and state management for the PlanformCreator2.

- holding stateful data like wing, planform, etc.
- signals for changes in data to inform the UI
- watchdog thread for monitoring polar generation 
- can be notified of changes in planform geometry and other parameters

The App Model is needed as the 'real' model is QObject agnostic and stateless. 

"""

import os
from enum                   import Enum, auto
from typing                 import override
from PyQt6.QtCore           import pyqtSignal, QObject, QThread, QTimer

from airfoileditor.base.common_utils        import Parameters, clip
from airfoileditor.base.app_utils           import Settings
from airfoileditor.model.airfoil_examples   import Example
from airfoileditor.model.xo2_driver         import Worker
from airfoileditor.model.polar_set          import Polar_Task, Polar_Definition, polarType

# --- the real model imports

from model.wing             import Wing, WingSection
from model.VLM_wing         import VLM_OpPoint, VLM_Polar


import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


# -----------------------------------------------------------------------------

class Mode_Id(Enum):
    """ Application Mode Identifiers """
    VIEW      = auto()
    MODIFY    = auto()


# -----------------------------------------------------------------------------



class App_Model (QObject):

    """
    The App Mode is a shell around the Model enriched with App specific objects
    having a state of the current Airfoil
    and provides Signals when data is changed

    """

    WORKER_MIN_VERSION         = '1.0.10'

    sig_new_mode                = pyqtSignal()          # new mode selected
    sig_new_wing                = pyqtSignal()          # new wing loaded

    sig_planform_changed        = pyqtSignal()          # planform geometry changed
    sig_wingSection_selected    = pyqtSignal()          # current wing section selected
    sig_wingSection_changed     = pyqtSignal()          # current wing section changed
    sig_polar_set_changed       = pyqtSignal()          # new polar sets attached to airfoil
    sig_new_polars              = pyqtSignal()          # new polars generated
    sig_paneling_changed        = pyqtSignal()          # paneling definition changed

    sig_vlm_polar_reset         = pyqtSignal()          # VLM resetted - new polar will be generated on demand
    sig_vlm_polar_changed       = pyqtSignal()          # current VLM polar changed
    sig_vlm_opPoint_changed     = pyqtSignal()          # current VLM operating point changed


    def __init__(self, workingDir_default: str = None):
        super().__init__()

        self._workingDir_default= workingDir_default if workingDir_default else os.getcwd()

        self._version           = ""                    # application version
        self._change_text       = ""                    # change text for this version
        self._is_first_run      = False                 # is first run of this version

        self._wing : Wing                   = None      # actual wing model
        self._cur_wingSection : WingSection = None      # current selected wing section

        self._cur_vlm_alpha : float        = None       # current VLM opPoint alpha
        self._cur_vlm_polar : VLM_Polar    = None       # for change detectioncurrent VLM polar
        self._cur_polar_def : Polar_Definition = None   # current polar definition
        self._vlm_alpha_fixed_to_max : bool = False     # is vlm alpha fixed to max lift alpha

        self._mode_id : Mode_Id = None                  # current app mode
        self._is_lower_minimized= False                 # is lower panel minimized

        # set working dir for Example airfoils created
        Example.workingDir_default = workingDir_default   

        # Worker for polar generation and Xoptfoil2 for optimization ready? 
        modulesDir = os.path.dirname(os.path.abspath(__file__))                
        projectDir = os.path.dirname(modulesDir)

        Worker    (workingDir=self.workingDir_default).isReady (projectDir, min_version=self.WORKER_MIN_VERSION)

        # initialize watchdog thread for polars and xo2 state changes
        self._init_watchdog()


    def __repr__(self):
        """ nice representation of self """
        return f"<App_Model {self.wing}>"


    def _init_watchdog (self):
        """ initialize watchdog thread to check for new polars and xo2 state changes """

        self._watchdog = Watchdog (self) 

        # watch for new polars
        self._watchdog.sig_new_polars.connect       (self.sig_new_polars.emit)

        self._watchdog.start()


    def _finish_watchdog (self):
        """ finish watchdog thread """

        if self._watchdog:
            self._watchdog.requestInterruption()
            self._watchdog.wait (2000)                              # wait max 2s for finish
            self._watchdog = None


    def _get_vlm_alpha_max (self) -> float | None:
        """ get alpha max from VLM polar """

        if self.cur_vlm_polar is None:
            return None                                             # no polar yet - wait   
        else: 
            opPoint = self.cur_vlm_polar.opPoint_at_alpha_max ()
            if opPoint is not None: 
                return opPoint.alpha                                # we got it 
            else: 
                return None                                         # maybe polars were not ready  - so wait 


    # --- properties

    def set_app_info (self, version: str, change_text: str, is_first_run: bool):
        """ set application info """
        self._version        = version
        self._change_text    = change_text
        self._is_first_run   = is_first_run


    @property
    def mode_id (self) -> Mode_Id:
        """ current application mode id """
        return self._mode_id
    

    def set_mode (self, mode_id : Mode_Id):
        """ set new application mode id and case """

        # sanity - ensure mode and case are compatible

        ok = False
        if mode_id == Mode_Id.MODIFY:
            ok = True

        if not ok:
            raise ValueError (f"{self} cannot set mode {mode_id}")
        
        self._mode_id = mode_id
        self.sig_new_mode.emit()


    @property
    def is_ready (self) -> bool:
        """ is app model ready to work"""
        return self.wing  

    @property
    def is_mode_view (self) -> bool:
        """ is current mode view """
        return self._mode_id == Mode_Id.VIEW

    @property
    def is_mode_modify (self) -> bool:
        """ is current mode modify """
        return self._mode_id == Mode_Id.MODIFY

    

    @property
    def wing (self) -> Wing:
        """ current wing """
        return self._wing 

    @property
    def cur_wingSection (self) -> WingSection:
        """ Dispatcher for current WingSection between Edit and Diagram """

        # ensure cur_wingSection still exists 
        if self._cur_wingSection: 
            try:
                self._cur_wingSection.index ()
            except: 
                self._cur_wingSection = None

        # handle first time or not existing 
        if self._cur_wingSection is None: 
            real_sections = self.wing.planform.wingSections.without_for_panels

            if len (real_sections) > 2:
                self._cur_wingSection = real_sections[1]              # set second section as initial
            else:        
                self._cur_wingSection = real_sections[0]              # take root

        return self._cur_wingSection 


    def set_cur_wingSection (self, aSection : WingSection | str | None):
        """ set current wing section either by object or by string"""

        if isinstance (aSection, str):
            sec = next((sec for sec in self.wing.planform.wingSections if sec.name_short == aSection), None)
        else: 
            sec = aSection

        self._cur_wingSection = sec
        logger.debug (f"{sec} as current")

        self.sig_wingSection_selected.emit()


    # --- VLM -----

    @property
    def cur_polar_def (self) -> Polar_Definition:
        """ selected T1 polar definition of wingSection at root"""

        new_polar_def = self._cur_polar_def

        # ensure current polar def is in existing polar definitions
        if self._cur_polar_def not in self.polar_definitions_T1:
            new_polar_def = None

        if self._cur_polar_def is None and self.polar_definitions_T1:
            new_polar_def = self.polar_definitions_T1[0] 

        if new_polar_def != self._cur_polar_def:
            self._cur_polar_def = new_polar_def
            logger.debug (f"{self} set cur_polar_def to {new_polar_def}")

            # reset current VLM opPoint 
            self._cur_vlm_alpha = None if self.vlm_alpha_fixed_to_max else 0.0
            self.sig_vlm_opPoint_changed.emit()

        return self._cur_polar_def


    def set_cur_polar_def (self, polar_def : Polar_Definition):
        """ set selected polar definition of wingSection at root"""

        if polar_def != self._cur_polar_def:
            self._cur_polar_def = polar_def
            logger.debug (f"{self} set cur_polar_def to {polar_def}")

            # reset current VLM opPoint 
            self._cur_vlm_alpha = None if self.vlm_alpha_fixed_to_max else 0.0
            self.sig_vlm_opPoint_changed.emit()


    @property
    def polar_definitions_T1 (self) -> list[Polar_Definition]:
        """ available T1 polar definitions of wingSection at root"""

        return [polar_def for polar_def in self.wing.polar_definitions if polar_def.type == polarType.T1]

    @property
    def cur_vlm_polar (self) -> VLM_Polar:
        """ current VLM polar based on cur polar_def at root chord """

        if self.wing.vlm_wing and self.cur_polar_def:
            v = self.cur_polar_def.calc_v_for_chord(self.wing.planform.chord_root)
            new_polar =  self.wing.vlm_wing.polar_at (v) 
            # do change detection for signal  
            if new_polar != self._cur_vlm_polar:
                self._cur_vlm_polar = new_polar
                logger.debug (f"{self} set cur_vlm_polar to {new_polar}")
                self.sig_vlm_polar_changed.emit()
            return new_polar
        else:
            return None

    @property
    def cur_vlm_alpha (self) -> float:
        """ alpha for current vlm opPoint"""
        # if fixed to max, try to get it 
        if self.vlm_alpha_fixed_to_max and self._cur_vlm_alpha is None:
            self._cur_vlm_alpha = self._get_vlm_alpha_max ()

        return self._cur_vlm_alpha if self._cur_vlm_alpha is not None else 0.0
    
    def set_cur_vlm_alpha (self, aVal : float):
        aVal = clip (aVal, -20, 20)
        if self._cur_vlm_alpha != aVal:
            self._cur_vlm_alpha = aVal
            self.sig_vlm_opPoint_changed.emit()

    @property
    def vlm_alpha_fixed_to_max (self) -> bool:
        """ is vlm alpha fixed to max lift alpha """
        return self._vlm_alpha_fixed_to_max
    
    def set_vlm_alpha_fixed_to_max (self, aBool : bool):
        if self._vlm_alpha_fixed_to_max != aBool:
            self._vlm_alpha_fixed_to_max = aBool
            
            if aBool:                                               # try to set to max now  
                self._cur_vlm_alpha = self._get_vlm_alpha_max ()
                self.sig_vlm_opPoint_changed.emit()

    @property
    def cur_vlm_opPoint (self) -> VLM_OpPoint:
        """ current VLM operating point based on alpha """
        if self.cur_vlm_polar:
            return self.cur_vlm_polar.opPoint_at (self.cur_vlm_alpha)
        else:
            return None
    

    def load_wing (self, pathFilename, initial=False): 
        """ creates / loads new wing as current"""

        self._wing = Wing (pathFilename, defaultDir=self.workingDir_default)
        
        self._cur_wingSection = None
        self._cur_vlm_alpha   = None
        self._cur_polar_def   = None

        self.sig_new_wing.emit()


    def cleanup_wing (self, all=False):
        """ close current wing - remove temp files"""

        # remove airfoils dir if new file
        if self.wing.is_new_wing:
            self.wing.remove_airfoils_dir ()

        # remove airfoil strak dir etc...
        if all:
            self.wing.remove_tmp ()

        # remove lost worker input files 
        if Worker.ready:
            Worker().clean_workingDir (self.wing.workingDir)



    def notify_polar_definitions_changed (self):
        """ notify self that polar definitions have changed """

        # as polar definitions could have changed, ensure a new initialized polarSet 
        self.wing.planform.wingSections.refresh_polar_sets (ensure=True)
        self.sig_polar_set_changed.emit()


    def notify_planform_changed (self):
        """ notify self that planform changed """
        self.sig_planform_changed.emit()
        self.notify_paneling_changed ()


    def notify_wingSection_changed (self):
        """ notify self that wing section changed """
        self.sig_wingSection_changed.emit()
        self.notify_paneling_changed ()


    def notify_paneling_changed (self):
        """ notify self that paneling changed """
        self.wing.vlm_wing_reset ()
        self.sig_paneling_changed.emit()

        # init VL recalculation
        if self.vlm_alpha_fixed_to_max:
            self._cur_vlm_alpha = None 

        QTimer.singleShot(0, self.sig_vlm_polar_reset.emit)   # notify after current events processed


    @property
    def workingDir_default (self) -> str: 
        """ default working directory """
        return self._workingDir_default
    
    @property
    def workingDir (self) -> str: 
        """ directory we are currently in (equals dir of wing if loaded) """
        if self.wing and self.wing.workingDir:                                        
            return self.wing.workingDir
        else:
            return self.workingDir_default
        
    @property
    def settings (self) -> Parameters:
        """ current loaded settings """
        return self._settings


    # ---- functions on state

    
    def load_settings (self):
        """ 
        Load and apply either default or individual settings for airfoil like view, polars, ...
        """

        pass



    def save_settings (self, to_app_settings: bool = False,
                               add_key: str = "",
                               add_value = None):
        """ 
        Save settings either to app settings or to settings of current airfoil
            An additional key, value pair (like diagram settings)can be added"""

        # save either in global settings or airfoil individual settings
        if to_app_settings:
            s = Settings()
 
        #todo 
        # cur_widget : Diagram_Abstract = self._panel_diagrams.currentWidget()
        # s.set('current_diagram', cur_widget.__class__.__name__)

        # # save settings of diagrams 
        # diagram : Diagram_Abstract
        # for diagram in self._diagrams:
        #     s_diagram = diagram.settings()
        #     s.set(diagram.name, s_diagram)

        # s.save()


    def close (self):
        """ finish app model """

        self._finish_watchdog()






# -----------------------------------------------------------------------------


class Watchdog (QThread):
    """ 
    Long running QThread to check if there is some new and signal parent
    
        - new polars generated - check Polar.Tasks 
        - Xoptfoil2 state 

    """

    sig_new_polars          = pyqtSignal ()


    def __repr__(self) -> str:
        """ nice representation of self """
        return f"<{type(self).__name__}>"


    @override
    def run (self) :
        # Note: This is never called directly. It is called by Qt once the
        # thread environment has been set up. 
        # Thread is started with .start()

        logger.info (f"Starting WatchdogThread")
        self.msleep (1000)                                  # initial wait before polling begins 

        while not self.isInterruptionRequested():


            # check for new polars 

            n_polars = 0 
            n_new_polars = 0 

            polar_tasks = Polar_Task.get_instances () 

            for task in polar_tasks: 

                n_polars     += task.n_polars
                n_new_polars += task.load_polars()

                if task.isCompleted():
                    task.finalize()
                else:
                    # this ensures, that polars are returned in the order tasks were generated
                    #   and not randomly by worker execution time -> more consistent diagram updates
                    # break
                    pass        # deactivated 

            # if new polars loaded signal 

            if n_new_polars:

                self.sig_new_polars.emit()
                logger.debug (f"{self} --> {n_new_polars} new in {n_polars} polars")

            self.msleep (500)

        return 
