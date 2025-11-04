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


APP_NAME         = "PlanformCreator2"
PACKAGE_NAME     = "planformcreator2"
__version__      = "4.0_dev"                                    # hatch "version dynamic" reads this version for build

AE_MIN_VERSION   = '4.2.0'                                      # min airfoileditor version required
AE_PACKAGE_NAME  = 'airfoileditor'                              # airfoileditor package name

# ---- imports -----

import os
import sys
import argparse
from importlib.metadata     import version
from packaging.version      import Version                                  # has to be installed
from typing                 import TYPE_CHECKING                        # to handle circular imports


from PyQt6.QtCore           import QMargins, QThread, QUrl
from PyQt6.QtWidgets        import (QApplication, QMainWindow, QWidget, QMessageBox, QFileDialog,
                                    QVBoxLayout, QHBoxLayout, QDialog)
from PyQt6.QtGui            import QCloseEvent, QGuiApplication, QDesktopServices

# For VsCode add airfoileditor package path to the setting "python.analysis.extraPaths" (Pylance)


# Check version of installed package and setup imports
try: 
    airfoileditor_version = version(AE_PACKAGE_NAME)
    if Version(airfoileditor_version) < Version(AE_MIN_VERSION):
        print (f"Installed {AE_PACKAGE_NAME} version {airfoileditor_version} is too old - please install at least version {AE_MIN_VERSION}")
        sys.exit(0)

    print(f"Using installed {AE_PACKAGE_NAME} version {airfoileditor_version}")

    if not TYPE_CHECKING:
        from airfoileditor.base.common_utils    import * 
        from airfoileditor.base.panels          import *
        from airfoileditor.base.widgets         import *
        from airfoileditor.model.xo2_driver     import Worker
        from airfoileditor.model.airfoil        import Airfoil, GEO_BASIC 
        from airfoileditor.model.airfoil_examples import Example
        from airfoileditor.base.app_utils       import *

except Exception as e:
    print(f"Required package {AE_PACKAGE_NAME} not found - setting sys.path for local development")
    
    # ---- Dev mode - add local airfoileditor path to sys.path
    airfoileditor_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '../..', 'airfoileditor/modules'))
    sys.path.insert(0, airfoileditor_path)
        
    print(f"Dev mode: local airfoileditor path added: {airfoileditor_path}")

    from base.common_utils      import * 
    from base.panels            import *
    from base.widgets           import *
    from model.xo2_driver       import Worker
    from model.airfoil          import Airfoil, GEO_BASIC 
    from model.airfoil_examples import Example
    from base.app_utils         import *




# ---- setup logging  - init local logger

init_logging (level= logging.INFO)             # INFO, DEBUG or WARNING

import logging
logger = logging.getLogger(__name__)
# logger.setLevel(logging.WARNING)

# ---- import local modules

from wing                   import *
from pc2_diagrams           import *
from pc2_dialogs            import *

 
#------------------------------------------------


#-------------------------------------------------------------------------------
# The App   
#-------------------------------------------------------------------------------

class Main_PC2 (QMainWindow):
    '''
        App - Main Window 
    '''

    WORKER_MIN_VERSION         = '1.0.10'
    TEMPLATE_DIR               = "templates"


   # Signals 

    sig_wing_new                = pyqtSignal()              # new wing loaded 
    sig_planform_changed        = pyqtSignal()              # planform data changed via input fields 
    sig_wingSection_selected    = pyqtSignal()              # new current wing section 
    sig_wingSection_changed     = pyqtSignal()              # current wing section changed
    sig_polar_set_changed       = pyqtSignal()              # new polar sets attached to airfoil
    sig_paneling_changed        = pyqtSignal()              # paneling changed, new wing VLM 


    def __init__(self, pc2_file):
        super().__init__()

        self._panel_data                    = None          # lower main panel
        self._panel_data_small              = None          # lower main panel - minimized version
        self._panel_data_minimized          = False         # is lower panel minimized
        self._panel_diagrams : Tab_Panel    = None          # upper main panel
        self._cur_wingSection               = None          # Dispatcher field between Diagram and Edit
        self._wing : Wing                   = None          # actual wing model
        self._watchdog                      = None          # polling thread for new polars

        # set user settings path, load settings 

        Settings.set_file (APP_NAME, file_extension= '.settings')
        app_settings = Settings()                            # load app settings


        # check for newer version on PyPi 

        update = Update_Checker (APP_NAME, PACKAGE_NAME,  __version__)   
        if update.is_newer_version_available():
            QTimer.singleShot (1000, lambda: update.show_user_info (self))    

        # Worker for polar generation ready?

        workingDir = Settings.user_data_dir (APP_NAME)                  # temp working dir for Worker and Xoptfoil2  
        modulesDir = os.path.dirname(os.path.abspath(__file__))                
        projectDir = os.path.dirname(modulesDir)
        Worker(workingDir=workingDir).isReady (projectDir, min_version=self.WORKER_MIN_VERSION)


        # ---- create 'wing' model -------

        Example.workingDir_default = Settings.user_data_dir (APP_NAME)  # example airfoil workingDir 

        # if no initial pc2 file, try to get last opened pc2 file

        if not pc2_file: 
            pc2_file = app_settings.get('last_opened', default=None)

        if pc2_file and not os.path.isfile (pc2_file):
            logger.error (f"Parameter file '{pc2_file}' doesn't exist")
            app_settings.set('last_opened', None)
            app_settings.save()
            pc2_file = None

        self.load_wing (pc2_file, initial=True)

        # --- init UI ---------------

        logger.info (f"Initialize UI")

        self.setWindowIcon (Icon ('PC2_ico.ico', icon_dir="modules"))  # will look in .\modules for py, in .\_internal\icons for exe

        # Qt color scheme, initial window size from settings      

        scheme_name = app_settings.get('color_scheme', Qt.ColorScheme.Unknown.name)           # either unknown (from System), Dark, Light
        QGuiApplication.styleHints().setColorScheme(Qt.ColorScheme[scheme_name])            # set scheme of QT
        Widget.light_mode = not (scheme_name == Qt.ColorScheme.Dark.name)                   # set mode for Widgets 

        geometry = app_settings.get('window_geometry', [])
        maximize = app_settings.get('window_maximize', False)
        Win_Util.set_initialWindowSize (self, size_frac= (0.70, 0.70), pos_frac=(0.1, 0.1),
                                        geometry=geometry, maximize=maximize)

        # main layout of app

        l = QGridLayout () 

        l.addWidget (self.panel_diagrams,   0,0)
        l.addWidget (self.panel_data_small, 1,0)
        l.addWidget (self.panel_data,       2,0)

        l.setRowStretch (0,1)
        l.setSpacing (5)
        l.setContentsMargins (QMargins(5, 5, 5, 5))

        main = QWidget()
        main.setLayout (l) 
        self.setCentralWidget (main)

        # apply settings to UI

        self._panel_view_minimized = app_settings.get('panel_view_minimized', False)
        self._panel_diagrams.set_tab (app_settings.get('current_diagram', None))

        for diagram in self._diagrams:
            diagram.set_settings (app_settings)

        # ---- signals and slots ------------ 

        # connect signals of self to self

        self.sig_planform_changed.connect           (self.refresh)
        self.sig_wing_new.connect                   (self.refresh)
        self.sig_wingSection_changed.connect        (self.refresh)

        # connect signals of diagram to self

        diagram : Diagram_Abstract
        for diagram in self._diagrams:
            diagram.sig_wingSection_new.connect     (self.on_wingSection_new)
            diagram.sig_wingSection_selected.connect(self.on_wingSection_selected)
            diagram.sig_wingSection_changed.connect (self.refresh)
            diagram.sig_planform_changed.connect    (self.refresh)
            diagram.sig_polar_def_changed.connect   (self.refresh_polar_sets)
            diagram.sig_panel_def_changed.connect   (self.refresh_paneling)

        # connect signals of self to slots of diagrams

        diagram : Diagram_Abstract
        for diagram in self._diagrams:
            self.sig_wingSection_selected.connect   (diagram.on_wingSection_selected)
            self.sig_wingSection_changed.connect    (diagram.on_wingSection_changed)
            self.sig_wing_new.connect               (diagram.on_wing_new)
            self.sig_planform_changed.connect       (diagram.on_planform_changed)
            self.sig_polar_set_changed.connect      (diagram.on_polar_set_changed)
            self.sig_paneling_changed.connect       (diagram.on_paneling_changed)

        # install watchdog for polars generated by Worker 

        if Worker.ready:

            self._watchdog = Watchdog (self) 
            self._watchdog.start()

            for diagram in self._diagrams:
                self._watchdog.sig_new_polars.connect         (diagram.on_new_polars)


        # --- final set of UI  ---------------

        # self.refresh()  

        logger.info (f"Ready for action ...")


    def __repr__(self) -> str:
        # overwritten to get a nice print string 
        text = f""  
        return f"<{type(self).__name__}{text}>"


    def toggle_data_panel_size (self):
        """ toggle between full and small data panel"""

        self._panel_data_minimized = not self._panel_data_minimized
        self.refresh()

    
    @property
    def panel_data (self) -> Container_Panel:
        """ lower UI main panel """

        if self._panel_data is None: 

            l = QHBoxLayout()
            l.addWidget (Panel_File             (self, self.wing, width=250))
            l.addWidget (Panel_Wing             (self, self.wing))
            l.addWidget (Panel_Chord_Reference  (self, self.wing))
            l.addWidget (Panel_WingSection      (self, self.wing))

            self._panel_data = Container_Panel (layout=l, title="Wing Data", height=150,
                                                hide=lambda: self._panel_data_minimized,
                                                doubleClick= self.toggle_data_panel_size,
                                                hint="Double click to minimize")
        return self._panel_data


    @property
    def panel_data_small (self) -> Container_Panel:
        """ lower UI main panel - small version with only file and wing panel"""

        if self._panel_data_small is None: 

            l = QHBoxLayout()
            l.addWidget (Panel_File_Small               (self, self.wing, width=250, has_head=False))
            l.addWidget (Panel_Wing_Small               (self, self.wing, has_head=False))
            l.addWidget (Panel_Chord_Reference_Small    (self, self.wing, has_head=False))
            l.addWidget (Panel_WingSection_Small        (self, self.wing, has_head=False))

            self._panel_data_small = Container_Panel (layout=l, title="Wing Data Small", height=62,
                                                      hide=lambda: not self._panel_data_minimized,
                                                      doubleClick= self.toggle_data_panel_size,
                                                      hint="Double click to maximize")
        return self._panel_data_small


    @property
    def panel_diagrams (self) -> Tab_Panel:
        """ upper UI main panel - Tab with diagrams"""

        if self._panel_diagrams is None: 

            diagrams = []
            diagrams.append (Diagram_Making_Of     (self, self.wing))
            diagrams.append (Diagram_Wing          (self, self.wing))
            diagrams.append (Diagram_Planform      (self, self.wing, self.wingSection))
            diagrams.append (Diagram_Airfoils      (self, self.wing))
            diagrams.append (Diagram_Wing_Analysis (self, self.wing, self.wingSection))

            tab_panel = Tab_Panel (self)
            for diagram in diagrams:
                tab_panel.add_tab(diagram)
            tab_panel.setMinimumHeight(500)

            self._panel_diagrams = tab_panel
            self._diagrams = diagrams

        return self._panel_diagrams


    def wing (self) -> Wing:
        """ encapsulates current wing. Children should access only via this function
        to enable a new wing to be set """
        return self._wing

      
    def wingSection (self) -> WingSection:
        """ Dispatcher for current WingSection between Edit and Diagram """

        # ensure cur_wingSection still exists 
        if self._cur_wingSection: 
            try:
                self._cur_wingSection.index ()
            except: 
                self._cur_wingSection = None

        # handle first time or not existing 
        if self._cur_wingSection is None: 
            real_sections = self.wing().planform.wingSections.without_for_panels

            if len (real_sections) > 2:
                self._cur_wingSection = real_sections[1]              # set second section as initial
            else:        
                self._cur_wingSection = real_sections[0]              # take root


        return self._cur_wingSection 


    def set_wingSection (self, aSection : WingSection | str | None):
        """ set current wing section either by object or by string"""

        if isinstance (aSection, str):
            sec = next((sec for sec in self.wing().planform.wingSections if sec.name_short == aSection), None)
        else: 
            sec = aSection

        self._cur_wingSection = sec
        logger.debug (f"{sec} as current")

        self.sig_wingSection_selected.emit()


    def on_wingSection_new (self, aSection : WingSection):
        """ slot for section signal from diagram"""
        self.set_wingSection (aSection) 
        self.refresh()


    def on_wingSection_selected (self, aSection : WingSection):
        """ slot for section signal from diagram"""
        self.set_wingSection (aSection) 
        self.panel_data.refresh()
        self.panel_data_small.refresh()


    def refresh(self):
        """ refreshes all child panels of edit_panel """
        self.panel_data.refresh()
        self.panel_data_small.refresh()
        self.refresh_paneling()


    def refresh_polar_sets (self):
        """ refresh polar sets of all airfoils in wingSections"""

        # as polar definitions could have changed, ensure a new initialized polarSet 
        self.wing().planform.wingSections.refresh_polar_sets (ensure=True)

        self.sig_polar_set_changed.emit()

    def refresh_paneling (self):
        """ refresh vlm panels"""

        self.wing().vlm_wing_reset ()

        self.sig_paneling_changed.emit()



    @property
    def workingDir (self): 
        """default home directory for output files (e.g. export)
        Currently equals to dir of parameter file """
        return self.wing().workingDir        


    def set_title (self): 
        """ set window title"""

        self.setWindowTitle (APP_NAME + "  v" + str(__version__) + "  [" + self.wing().parm_fileName + "]")


    def _save_app_settings (self):
        """ save settings, eg Window size and position, to file """

        s = Settings()

        # save Window size and position 
        s.set ('window_maximize', self.isMaximized())
        s.set ('window_geometry', self.normalGeometry().getRect())
        s.set ('panel_view_minimized', self._panel_view_minimized)

        # save current tab as classname 
        cur_widget : Diagram_Abstract = self._panel_diagrams.currentWidget()
        s.set('current_diagram', cur_widget.__class__.__name__)

        # save settings of diagrams 
        diagram : Diagram_Abstract
        for diagram in self._diagrams:
            s_diagram = diagram.settings()
            s.set(diagram.name, s_diagram)

        s.save()


    def _on_leaving_planform (self) -> bool:
        """ 
        Do checks and handle if user wants to leave current planform.
        Returns True if leaving ok, False if cancelled 
        """
        leave = True

        if self.wing().has_changed(): 
            message = "The planform has been modified..\n\n" + \
                      "Do you want to save before leaving?"
            button = MessageBox.save(self, f"Leaving {self.wing().parm_fileName}", message)

            if button == QMessageBox.StandardButton.Save:
                # save settings and parameters
                self.save()

            elif button == QMessageBox.StandardButton.Discard:
                # on Discard remove temp dir of airfoil strak etc
                self.wing().remove_tmp ()

            elif button == QMessageBox.StandardButton.Cancel:
                leave = False

        if leave:
            # remove airfoils dir if new file
            if self.wing().parm_fileName == FILENAME_NEW:
                self.wing().remove_airfoils_dir ()

            # remove lost worker input files 
            if Worker.ready:
                Worker().clean_workingDir (self.workingDir)
        return leave


    @override
    def closeEvent  (self, event : QCloseEvent):
        """ main window is closed """


        leave = self._on_leaving_planform ()

        # final actions if not cancelled

        if leave:

            event.accept()

            # save app settings like window size and position
            self._save_app_settings ()

            # terminate polar watchdog thread 
            if self._watchdog:
                self._watchdog.requestInterruption ()
                self._watchdog.wait()           

        else:
            event.ignore()
        


    #------- file functions ----------------

    def new (self):
        """ reset - and start with example definition"""

        if not self._on_leaving_planform ():                                    # changes made? user cancelled?
            return


        # select a new template 
        modulesDir = os.path.dirname(os.path.abspath(__file__))                
        projectDir = os.path.dirname(modulesDir)

        template_dir  = os.path.join (projectDir, self.TEMPLATE_DIR)

        try:
            dialog = Dialog_Select_Template (self, template_dir, parentPos=(0.3, 0.8), dialogPos=(0,1))
            dialog.exec()     
            template_filePathName = dialog.template_file_selected
        except:
            template_filePathName = None

        if template_filePathName and os.path.isfile (template_filePathName):

            # copy template file to user data dir as new file
            workingDir = Settings.user_data_dir (APP_NAME)                      # temp working dir for Worker and Xoptfoil2  
            new_filePathName = os.path.join(workingDir, os.path.basename(template_filePathName))
            shutil.copy2(template_filePathName, new_filePathName)
            logger.info (f"New planform from template '{template_filePathName}' copied to '{new_filePathName}'")

            #load new planform from USER dir
            self.load_wing (new_filePathName)

            # copy background image if existing
            if self.wing().background_image.pathFilename:
                image_pathFileName     = os.path.join(template_dir, self.wing().background_image.pathFilename)
                new_image_pathFileName = os.path.join(workingDir,   self.wing().background_image.pathFilename)
                shutil.copy2(image_pathFileName, new_image_pathFileName)


    def open (self):
        """ open a new wing definition json and load it"""

        if not self._on_leaving_planform ():                                    # changes made? user cancelled?
            return
        
        filters    = "PlanformCreator2 files (*.pc2)"
        workingDir = self.wing().workingDir

        newPathFilename, _ = QFileDialog.getOpenFileName(self, filter=filters, directory=workingDir)

        if newPathFilename:                     

            #leave button callback and refresh in a few ms 
            QTimer().singleShot(10, lambda: self.load_wing (newPathFilename))     # delayed emit 



    def save (self, newPathFilename=None):
        """ save wing data to the action parameter file - if new wing to saveAs"""

        if self.wing().parm_fileName == FILENAME_NEW and newPathFilename is None:
            self.saveAs ()
            return

        ok = self.wing().save (newPathFilename=newPathFilename)
        if ok:
            MessageBox.success (self,"Save Planform", f"<b>{self.wing().parm_fileName}</b> saved", min_height= 60, min_width=150)
        else:
            MessageBox.error   (self,"Save Planform", f"<b>{self.wing().parm_fileName}</b> couldn't be saved", min_height= 60)


    def saveAs (self):
        """ save wing data to a new file and set this as actual"""

        filters   = "PlanformCreator2 files (*.pc2)"
        directory = self.wing().workingDir
        newPathFilename, _ = QFileDialog.getSaveFileName(self, filter=filters, directory=directory)

        if newPathFilename: 
            self.save (newPathFilename = newPathFilename)
            self.set_title ()
            s = Settings()
            s.set('last_opened', newPathFilename)
            s.save()


    def edit_settings (self):
        """ file menu edit settings """

        # dialog = Dialog_Settings (self, name=self.name)
        # self.wait_window (dialog)


    #-------------

    def load_wing (self, pathFilename, initial=False): 
        """ creates / loads new wing as current"""

        self._wing = Wing (pathFilename, defaultDir=Settings.user_data_dir (APP_NAME))
        
        self._cur_wingSection = None

        if self.wing().parm_fileName != FILENAME_NEW:
            s = Settings()
            s.set('last_opened', self.wing().parm_pathFileName_abs)
            s.save()

        self.set_title ()

        if not initial: 
            self.sig_wing_new.emit()



# -----------------------------------------------------------------------------


class Watchdog (QThread):
    """ 
    Long running QThread to check if there is some new and signal parent 
        - new polars generated - check Polar.Tasks 
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

        logger.info (f"Starting Watchdog Thread")
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



#-------------------------------------------------------------------------------
# Single edit panels    
#-------------------------------------------------------------------------------


class Panel_Planform_Abstract (Edit_Panel):
    """ 
    Abstract superclass for Edit/View-Panels of PlanformCreator2
        - has semantics of App
        - connect / handle signals 
    """

    @property
    def app (self) -> Main_PC2:
        return self._app 

    @property    
    def wing (self) -> Wing: 
        return self.dataObject
    
    def planform (self) -> Planform:
        return self.wing.planform
    
    def n_chord (self) -> N_Distrib_Abstract:
        return self.wing.planform.n_distrib

    def n_chord_ref (self) -> N_Chord_Reference:
        return self.wing.planform.n_chord_ref
    
    def n_ref_line (self) -> N_Reference_Line:
        return self.wing.planform.n_ref_line
    
    def flaps (self) -> Flaps:
        return self.wing.planform.flaps

    def wingSections (self) -> WingSections:
        return self.wing.planform.wingSections

    def wingSections_to_show (self) -> list[WingSection]:
        """ the wingSections to plot - without extra sections for paneling"""

        return [section for section in self.wingSections() if not section.is_for_panels] 


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
        self.app.sig_planform_changed.emit()



class Panel_File (Panel_Planform_Abstract):
    """ File panel with open / save / ... """

    name = 'File'

    @override
    def _add_to_header_layout(self, l_head: QHBoxLayout):
        """ add Widgets to header layout"""

        l_head.addStretch(1)
        ToolButton   (l_head, icon=Icon.EXPAND, set=self.app.toggle_data_panel_size,
                      toolTip='Minimize lower panel -<br>Alternatively, you can double click on the lower panels')


    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 
        Button (l,r,c, text="&Open", width=100, 
                set=self.app.open, toolTip="Open new Planform",
                button_style=button_style.PRIMARY)
        Button (l,r,c+2, text="&New", width=80, 
                set=self.app.new, toolTip="Create new Planform")
        
        r += 1
        Button (l,r,c, text="&Save", width=100, 
                set=self.app.save, toolTip="Save Planform to parameter file")
        MenuButton (l,r,c+2, text="More...", width=80, 
                menu=self._more_menu(), 
                toolTip="Choose further actions for this planform")
        r += 1
        SpaceR (l,r, stretch=1)
        r += 1
        Button (l,r,c, text="&Exit", width=100, set=self.app.close)
        l.setColumnMinimumWidth (1,12)
        l.setColumnStretch (3,1)

        return l 
 
    def _more_menu (self) -> QMenu:
        """ create and return sub menu for 'more' actions"""

        menue = QMenu ()
        menue.addAction (MenuAction ("Save as...", self, set=self.app.saveAs,
                                     toolTip="Create a copy of the current planform with new name and filename"))
        menue.addAction (MenuAction ("Rename...", self, set=self._rename,
                                     toolTip="Rename name and/or filename of current planform"))
        # menue.addAction (MenuAction ("Delete", self, set=self.app.do_delete,
        #                              toolTip="Delete current planform including all temporary files created by the AirfoilEditor"))
        menue.addAction (MenuAction ("Delete temp files", self, set=self._delete_temp_files,
                                     toolTip=f"Delete temporary files created by {APP_NAME} just to have a clean directoy again"))
        menue.addSeparator ()
        menue.addAction (MenuAction ("Readme on Github", self, set=self._open_AE_url,
                                     toolTip=f"Open the Github README file of {APP_NAME} in a browser"))
        menue.addAction (MenuAction ("Releases on Github", self, set= self._open_releases_url,
                                     toolTip=f"Open the Github page with the actual release of {APP_NAME}"))
        menue.setToolTipsVisible(True)

        return menue


    def _rename (self):
        """ rename current planform - both name and filename"""

        dialog = Dialog_Rename (self, lambda: self.wing, parentPos=(1.5,-0.5), dialogPos=(0,1))  
        dialog.exec()   

        self.app.load_wing (self.wing.parm_pathFileName_abs)      # reload wing to apply new name / filename


    def _delete_temp_files (self):
        """ delete temporary files created by PC2"""

        text = f"Delete temporary files of <b>{self.wing.parm_fileName}</b>.<br><br>" +\
               f"The planform will be saved and reloaded after this." 

        msg = MessageBox (self, "Delete Temp Files", text, Icon (Icon.INFO), min_width=300)
        msg.setStandardButtons(QMessageBox.StandardButton.Ok | MessageBox.StandardButton.Cancel)
        button = msg.exec()

        if button == QMessageBox.StandardButton.Ok:
      
            self.wing.save ()                                     # save first to ensure actual data for reload
            self.wing.remove_tmp ()
            self.app.load_wing (self.wing.parm_pathFileName_abs)  # reload wing for new strak etc.

            MessageBox.info (self, "Delete Temp Files", "Temporary files removed")


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
                    set=self.app.open, toolTip="Open new Planform",
                    button_style=button_style.PRIMARY)
        MenuButton  (l,r,c+2, text="More...", width=80, 
                    menu=self._more_menu(), 
                    toolTip="Choose further actions for this airfoil")
        ToolButton  (l,r,c+3, icon=Icon.COLLAPSE, set=self.app.toggle_data_panel_size,
                    toolTip='Maximize lower panel -<br>Alternatively, you can double click on the lower panels')
        r += 1
        Button      (l,r,c, text="&Exit", width=100, set=self.app.close)
        l.setColumnMinimumWidth (1,12)
        l.setColumnStretch (2,2)
        return l 
 

    def _more_menu (self) -> QMenu:
        """ create and return sub menu for 'more' actions"""

        menue = super()._more_menu ()
        menue.insertAction (menue.actions()[0], MenuAction ("&Save", self, set=self.app.save,   
                                    toolTip="Save Planform to parameter file"))
        menue.insertAction (menue.actions()[0], MenuAction ("&New", self, set=self.app.new,   
                                    toolTip="Create new Planform"))
        return menue



class Panel_Wing (Panel_Planform_Abstract):
    """ Main geometry data of wing"""

    name = 'Wing'
    _width  = (420, None)

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
                obj=self.planform, prop=Planform.span)
        r += 1
        FieldF (l,r,c, lab="Root Chord", width=85, unit="mm", step=10, lim=(10, 1000), dec=0, 
                obj=self.planform, prop=Planform.chord_root)
        FieldF (l,r,c+3, lab="Sweep Angle", width=75, unit="°", step=0.1, lim=(-45, 45), dec=1,
                obj=self.planform, prop=Planform.sweep_angle)
        r += 1
        l.setRowStretch (r,1)

        l.setColumnMinimumWidth (0,95)
        l.setColumnMinimumWidth (3,90)
        l.setColumnStretch (2,1)
        l.setColumnStretch (5,2)
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

    _width  = None
    _panel_margins = (0, 0, 0, 0)

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
        l.setRowStretch (r,1)

        l.setColumnMinimumWidth (0,110)
        l.setColumnMinimumWidth (2,15)
        l.setColumnMinimumWidth (3,50)
        l.setColumnStretch (5,2)
        return l 



class Panel_Chord_Reference_Small (Panel_Chord_Reference):
    """ Small Chord Reference panel"""

    _width  = None
    _panel_margins = (0, 0, 0, 0)

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
        l.setRowStretch (r,1)

        l.setColumnMinimumWidth (0,110)
        l.setColumnMinimumWidth (2,15)
        l.setColumnMinimumWidth (3,50)
        l.setColumnStretch (5,2)
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
    def refresh (self, **kwargs):
        super().refresh(**kwargs)

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
        Field  (l_foil,0,0,   lab="Airfoil",
                get=self._airfoil_name,                 # name as strak fileName can be long
                toolTip=lambda: self._wingSection().airfoil.info_as_html)

        ToolButton (l_foil,0,2, icon=Icon.OPEN, set=self._open_airfoil, 
                        toolTip="Select an airfoil for this wing section")
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
        l.addWidget (p_foil, r, c, 1, 7)

        l.setColumnMinimumWidth (0,70)
        l.setColumnMinimumWidth (2,10)
        l.setColumnMinimumWidth (3,50)
        l.setColumnMinimumWidth (5,50)

        r += 1
        l.setRowStretch (r,2)

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
        l.setColumnStretch (c+5,2)
        return l 


    def _wingSections_list (self) -> list:
        sec_list = [section.name_short for section in self.wingSections_to_show()]
        return sec_list 

    def _wingSection (self) -> WingSection:
        """ Dispatcher for current WingSection between Edit and Diagram """
        return self.app.wingSection()

    def _wingSection_name (self) -> str:
        return self._wingSection().name_short
    
    def _set_wingSection_name (self, aSection : WingSection| str):
        """ set cur wingSection by name """
        self.app.on_wingSection_selected(aSection)


    def _delete_wingSection (self):
        """ delete current wingSection"""
        aSection = self.wingSections().delete (self._wingSection())     
        self.app.on_wingSection_selected (aSection)  

    def _add_wingSection (self):
        """ add wingSection after current"""
        aSection = self.wingSections().create_after (self._wingSection()) 
        self.app.on_wingSection_selected (aSection)    


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
            text = "Section has a fixed relative chord"
        else: 
            text = ""
        return text 


    def _airfoil_name (self) -> str:
        """ get airfoil file name or 'strak' at section"""
        airfoil = self._wingSection().airfoil
        return STRAK_AIRFOIL_NAME if airfoil.isBlendAirfoil else airfoil.fileName


    def _open_airfoil (self):
        """ open a new airfoil and load it"""

        airfoil = self._wingSection().airfoil

        filters   = "Airfoil files (*.dat);;Bezier files (*.bez);;Hicks Henne files (*.hicks)"
        # inital strak airfoil has yet no fileName - use working dir of wing
        directory = airfoil.pathName_abs if airfoil.fileName else self.wing.workingDir

        newPathFilename, _ = QFileDialog.getOpenFileName(self, filter=filters, directory=directory)

        if newPathFilename:                         # user pressed open

            airfoil_loaded = False

            try: 
                airfoil = Airfoil.onFileType(newPathFilename, geometry=GEO_BASIC)
                airfoil.load()
                airfoil_loaded = airfoil.isLoaded
            except:
                pass

            if airfoil_loaded:
                self._set_airfoil (airfoil)
            else:
                msg     = f"<b>{newPathFilename}</b> couldn't be loaded."
                MessageBox.error   (self,'Load Airfoil', f"{msg}", min_height= 60)


    def _edit_airfoil (self, airfoil : Airfoil = None):
        """ edit airfoil with AirfoilEditor"""

        if airfoil is None: 
            airfoil = self._wingSection().airfoil

        from app import Main

        ae = Main (os.path.join(airfoil.workingDir, airfoil.pathFileName), parent=self.app)
        ae.sig_closing.connect (self._on_edit_finished) 

        ae.show()


    def _set_airfoil (self, airfoil : Airfoil | None):
        """ set airfoil for section"""

        do_copy = True 

        if airfoil: 

            already_copied = os.path.samefile (airfoil.pathName_abs, self.wing.airfoils_dir)

            if not airfoil.isNormalized:

            # 1 not normalized -> normalize and copy

                text = f"The airfoil '{airfoil.fileName}' is not normalized,\n" +\
                    f"but this is needed for blending (strak).\n\n" + \
                    f"It will be normalized and copied to {self.wing.airfoils_dir_rel}/."

                msg = MessageBox (self, "Airfoil not normalized", text, Icon (Icon.INFO), min_width=350)
                msg.setStandardButtons(QMessageBox.StandardButton.Ok | MessageBox.StandardButton.Cancel)
                button = msg.exec()

                if button == QMessageBox.StandardButton.Cancel:
                    return
                
            elif not already_copied:    
            # 2 normalized -> ask copy

                text = f"The airfoil '{airfoil.fileName}' will be copied to {self.wing.airfoils_dir_rel}/.\n" +\
                    f"which is the recommended location for airfoils of this planform\n\n" + \
                    f"Copy airfoil?"
                msg = MessageBox (self, "Airfoil not normalized", text, Icon (Icon.INFO), min_width=350)
                msg.setStandardButtons(QMessageBox.StandardButton.Ok | MessageBox.StandardButton.No)
                button = msg.exec()

                do_copy = button == QMessageBox.StandardButton.Ok

        self.wingSections().set_airfoil_for (self._wingSection(), airfoil, in_airfoils_dir=do_copy)

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
        logger.debug (f"{self}  {self._wingSection()} changed")
        self.app.sig_wingSection_changed.emit()




class Panel_WingSection_Small (Panel_WingSection):
    """ Main geometry data of wing"""

    _width  = None
    _panel_margins = (0, 0, 0, 0)

    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 

        # 0. column - select section
        ComboBox     (l,r,c, width=55,
                      get=self._wingSection_name, set=self._set_wingSection_name,
                      options= self._wingSections_list, signal=False )      # do not signal (would be double refresh)
        ToolButton   (l,r,c+1, icon=Icon.ADD,    set=self._add_wingSection,
                      disable=lambda: self._wingSection().is_tip,
                      toolTip='Add a new wing section')
        ToolButton   (l,r,c+2, icon=Icon.DELETE, set=self._delete_wingSection,
                      disable=lambda: self._wingSection().is_root_or_tip,
                      toolTip='Delete selected wing section')
        Label        (l,r+1,c, get="Wing Section",colSpan=2)

        # 1. column - section settings and airfoil 

        c += 4
               
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
        l.setRowStretch (r,2)

        l.setColumnMinimumWidth (3,20)
        l.setColumnMinimumWidth (4,60)
        l.setColumnMinimumWidth (6,20)
        l.setColumnMinimumWidth (7,50)
        l.setColumnStretch (c+7,2)
        return l 




#--------------------------------

def start ():
    """ start the app """

    # command line arguments? 
    
    pc2_file = ''
    parser = argparse.ArgumentParser(prog=APP_NAME, description='Create a wing planform')
    parser.add_argument("paramterfile", nargs='*', help="Parameter file .pc2")
    args = parser.parse_args()

    if args.paramterfile: 
        pc2_file = args.paramterfile[0]
    else: 
        pc2_file = None 

    # init Qt Application and style  

    app = QApplication(sys.argv)
    app.setStyle('fusion')

    main = Main_PC2 (pc2_file)
    main.show()
    rc = app.exec()
    return rc 



if __name__ == "__main__":
    
    sys.exit (start())