#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    The Planform Creator 2 App 

    Object model overview (a little simplified) 

    tbd.

"""

import os
import sys
import argparse
from pathlib import Path

from PyQt6.QtCore           import QMargins
from PyQt6.QtWidgets        import QApplication, QMainWindow, QWidget, QMessageBox, QFileDialog
from PyQt6.QtWidgets        import QGridLayout, QVBoxLayout, QHBoxLayout
from PyQt6.QtWidgets        import QTabWidget
from PyQt6.QtGui            import QCloseEvent

# let python find the other modules in modules relativ to path of self - ! before python system modules
# common modules hosted by AirfoilEditor 
sys.path.insert (1,os.path.join(Path(__file__).parent , 'AirfoilEditor_subtree/modules'))
# local modules
sys.path.insert (1,os.path.join(Path(__file__).parent , 'modules'))

from wing                   import Wing

from base.common_utils      import * 
from base.panels            import Container_Panel, MessageBox
from base.widgets           import *

# from AirfoilEditor_subtree  import AirfoilEditor

from pc2_panels             import *
from pc2_diagrams           import *
from pc2_dialogs            import *


import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

#------------------------------------------------

APP_NAME     = "Planform Creator 2"
APP_VERSION  = "2.0 beta 2"

TEMPLATE_DIR = "templates"


# --------------------- tmp ----------------------------------------------------


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
                min-width: 40ex;
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
                min-width: 40ex;
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
# The App   
#-------------------------------------------------------------------------------

class App_Main (QMainWindow):

    name = APP_NAME  

   # Signals 

    sig_wing_new                = pyqtSignal()              # new wing loaded 
    sig_planform_changed        = pyqtSignal()              # planform data changed via input fields 
    sig_wingSection_selected    = pyqtSignal()              # current wing section changed


    def __init__(self, pc2_file):
        super().__init__()

        self.initial_geometry   = None                      # window geometry at the bginning
        self._cur_wingSection = None                        # Dispatcher field between Diagram and Edit
        self._pc2_file = ''                                 # paramter file with wing settings  
        self._myWing : Wing = None                          # actual wing model 


        # get icon either in modules or in icons 

        icon = Icon  ('PC2_ico.ico', icon_dir="modules")    # will look in .\modules for py, in .\_internal\icons for exe
        self.setWindowIcon (icon)

        # get initial window size from settings

        Settings.belongTo (__file__, nameExtension=None, fileExtension= '.settings')
        geometry = Settings().get('window_geometry', [])
        maximize = Settings().get('window_maximize', False)
        Win_Util.set_initialWindowSize (self, size_frac= (0.80, 0.70), pos_frac=(0.1, 0.1),
                                        geometry=geometry, maximize=maximize)

        # create the 'wing' model  

        self.load_wing (pc2_file, initial=True)

        # init main layout of app

        self._data_panel    = Container_Panel  (title="Data panel")
        self._file_panel    = Container_Panel  (title="File panel", width=240)

        self._tab_panel     = Tab_Panel        (self)
        self._diagrams      = []

        self._add_diagram (Diagram_Making_Of(self, self.wing))
        self._add_diagram (Diagram_Wing     (self, self.wing))
        self._add_diagram (Diagram_Planform (self, self.wing, self.wingSection))
        self._add_diagram (Diagram_Airfoils (self, self.wing))
        self._add_diagram (Diagram_Panels   (self, self.wing, self.wingSection))

        self._tab_panel.set_tab (Settings().get('current_diagram', Diagram_Making_Of.__name__))

        l_main = self._init_layout() 

        container = QWidget()
        container.setLayout (l_main) 
        self.setCentralWidget(container)

        # connect to signals of self

        self.sig_planform_changed.connect           (self.refresh)
        self.sig_wing_new.connect                   (self.refresh)

        # connect to signals from diagram

        diagram : Diagram_Abstract
        for diagram in self._diagrams:
            diagram.sig_wingSection_new.connect     (self.on_wingSection_selected)
            diagram.sig_planform_changed.connect    (self.refresh)
            diagram.sig_export_airfoils.connect     (self.export_airfoils)
            diagram.sig_export_xflr5.connect        (self.export_xflr5)
            diagram.sig_export_flz.connect          (self.export_flz)
            diagram.sig_launch_flz.connect          (self.launch_flz)
            diagram.sig_export_dxf.connect          (self.export_dxf)

        # connect signals to slots of diagram

        diagram : Diagram_Abstract
        for diagram in self._diagrams:
            self.sig_wingSection_selected.connect   (diagram.on_cur_wingSection_changed)
            self.sig_wing_new.connect               (diagram.on_wing_new)
            self.sig_planform_changed.connect       (diagram.on_planform_changed)



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
        l_data.addWidget (Panel_Wing                (self, self.wing))
        l_data.addWidget (Panel_Chord_Reference     (self, self.wing))
        l_data.addWidget (Panel_WingSection         (self, self.wing))

        l_data.addStretch (1)        
        l_data.setContentsMargins (QMargins(0, 0, 0, 0))
        self._data_panel.setLayout (l_data)

        l_file = QHBoxLayout()
        l_file.addWidget (Panel_File   (self, self.wing))
        l_file.setContentsMargins (QMargins(0, 0, 0, 0))
        self._file_panel.setLayout (l_file)

        l_lower = QHBoxLayout()
        l_lower.addWidget (self._file_panel)
        l_lower.addWidget (self._data_panel, stretch=1)
        l_lower.setContentsMargins (QMargins(0, 0, 0, 0))
        lower = QWidget ()
        lower.setMinimumHeight(190)
        lower.setMaximumHeight(190)
        lower.setLayout (l_lower)

        # main layout with diagram panel and lower 

        l_main = QVBoxLayout () 
        l_main.addWidget (self._tab_panel, stretch=2)
        l_main.addWidget (lower)
        l_main.setContentsMargins (QMargins(5, 5, 5, 5))

        return l_main 

    def _add_diagram (self, aDiagram : Diagram_Abstract):
        """ add a diagram to Tab panel and diagram list """

        self._tab_panel.add_tab (aDiagram)
        self._diagrams.append(aDiagram)


    def wing (self) -> Wing:
        """ encapsulates current wing. Childs should acces only via this function
        to enable a new wing to be set """
        return self._myWing

      
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
            normed_sections = self.wing().planform.wingSections

            if len (normed_sections) > 2:
                self._cur_wingSection = normed_sections[1]              # set second section as initial
            else:        
                self._cur_wingSection = normed_sections[0]              # take root


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


    def on_wingSection_selected (self, aSection : WingSection):
        """ slot for section signal from diagram"""
        self.set_wingSection (aSection) 
        self.refresh()


    def refresh(self):
        """ refreshes all child panels of edit_panel """
        self._data_panel.refresh()
        self._file_panel.refresh()


    @property
    def workingDir (self): 
        """default home directory for output files (e.g. export)
        Currently equals to dir of parameter file """
        return self.wing().workingDir        

    def set_title (self): 
        """ set window title"""

        if self._pc2_file:
            project = self._pc2_file
        else:
            project = "< new >"
        self.setWindowTitle (APP_NAME + "  v" + str(APP_VERSION) + "  [" + project + "]")


    def _save_settings (self):
        """ save settings, eg Window size and position, to file """

        Settings().set('window_geometry', self.normalGeometry ().getRect())
        Settings().set('window_maximize', self.isMaximized())

        # save current tab as classname 
        current_diagram = self._tab_panel.currentWidget().__class__.__name__
        Settings().set('current_diagram',current_diagram)


    @override
    def closeEvent  (self, event : QCloseEvent):
        """ main window is closed """

        self._save_settings ()

        # save changes? 
        if self.wing().has_changed(): 

            message = "The planform has been modified..\n\n" + \
                      "Do you want to save before exit?"
            button = MessageBox.save(self, "Close "+ APP_NAME, message)

            if button == QMessageBox.StandardButton.Save:
                self.save()
                event.accept()
            elif button == QMessageBox.StandardButton.Discard:
                event.accept()
            else:
                event.ignore()
        else:
            event.accept()



        


    #------- file functions ----------------

    def new (self):
        """ reset - and start with example definition"""

        # select a new template 

        template_dir  = os.path.join (os.path.dirname (__file__), TEMPLATE_DIR)

        dialog = Dialog_Select_Template (self, template_dir) 
        dialog.exec()     

        if dialog.template_file_selected:

            #leave button callback and refresh in a few ms 
            QTimer().singleShot(10, lambda: self.load_wing (dialog.template_file_selected))     # delayed emit 



    def open (self):
        """ open a new wing definition json and load it"""

        filters  = "PlanformCreator2 files (*.pc2)"
        newPathFilename, _ = QFileDialog.getOpenFileName(self, filter=filters)

        if newPathFilename:                     

            #leave button callback and refresh in a few ms 
            QTimer().singleShot(10, lambda: self.load_wing (newPathFilename))     # delayed emit 



    def save (self):
        """ save wing data to the action parameter file - if new wing to saveAs"""


        if self._pc2_file:
            ok = self.wing().save(self._pc2_file)
            if ok:
                _, filename = os.path.split(self._pc2_file)
                MessageBox.success (self,"Save Planform", f"{filename} successfully saved", min_height= 60)
            else:
                MessageBox.error   (self,"Save Planform", f"{self._pc2_file} couldn't be saved", min_height= 60)
        else:
            self.saveAs ()


    def saveAs (self):
        """ save wing data to a new file and set this as actual"""

        filters  = "PlanformCreator2 files (*.pc2)"
        newPathFilename, _ = QFileDialog.getSaveFileName(self, filter=filters)

        if newPathFilename: 
            self._pc2_file = PathHandler.relPath (newPathFilename)
            self.save ()
            self.set_title ()
            Settings().set('lastOpenend', self._pc2_file)


    def edit_settings (self):
        """ file menu edit settings """

        # dialog = Dialog_Settings (self, name=self.name)
        # self.wait_window (dialog)
        pass


    #-------------

    def load_wing (self, pathFilename, initial=False): 
        """ creates / loads new wing as current"""

        self._myWing = Wing (pathFilename)
        
        self._cur_wingSection = None

        if pathFilename:
            self._pc2_file = PathHandler.relPath (pathFilename)
            Settings().set('lastOpenend', self._pc2_file)
        else:
            self._pc2_file = ""
        self.set_title ()

        if not initial: 
            self.sig_wing_new.emit()


    def export_xflr5 (self): 
        """ export wing to xflr5"""

        directory = QFileDialog.getExistingDirectory(self,caption="Select directory for export",
                                                     directory=self.wing().export_xflr5.export_dir)
        if directory: 

            self.wing().export_xflr5.set_export_dir (directory)   
            self.wing().export_xflr5.do_it () 
            n_airfoils = self.wing().export_xflr5.n_airfoils

            filename   = self.wing().export_xflr5.filename
            MessageBox.success (self,"Export FLZ", f"'{filename}' and \n{n_airfoils} arfoils exported." )


    def export_flz (self): 
        """ export wing to FLZ"""

        directory = QFileDialog.getExistingDirectory(self,caption="Select directory for export",
                                                     directory=self.wing().export_flz.export_dir)
        if directory: 

            self.wing().export_flz.set_export_dir (directory)   
            self.wing().export_flz.do_it () 

            filename   = self.wing().export_flz.filename
            MessageBox.success (self,"Export FLZ", f"'{filename}' exported." )


    def launch_flz (self): 
        """ export wing to FLZ and launch """

        self.wing().export_flz.do_it ()

        pathFileName = os.path.join (self.wing().export_flz.base_and_export_dir, self.wing().export_flz.filename) 
        try: 
            os.startfile(pathFileName, 'open')
        except: 
            message = "Could not launch FLZ_vortex on exported file: \n\n" + \
                    pathFileName + \
                    "\n\n Is FLZ_vortex neatly installed and associated with file extension '.flz'?"
            MessageBox.error (self,"Export FLZ", str(message))


    def export_airfoils (self):
        """open export airfoils of wing dialog """

        dialog = Dialog_Export_Airfoil (self, self.wing)  
        dialog.exec()     


    def export_dxf (self):
        """open export planform to dxf dialog """

        dialog = Dialog_Export_Dxf (self, self.wing)  
        dialog.exec()     




#--------------------------------

if __name__ == "__main__":


    dev_mode = True

    # init logging  

    if dev_mode:   
        init_logging (level= logging.DEBUG)             # INFO, DEBUG or WARNING
    else:                       
        init_logging (level= logging.WARNING)

    # paramter file as argument?  

    parmFile = ''
    parser = argparse.ArgumentParser(prog=APP_NAME, description='Create a wing planform')
    parser.add_argument("paramterfile", nargs='*', help="Paramter file .pc2")
    args = parser.parse_args()

    if args.paramterfile: 
        parmFile = args.paramterfile[0]
    else: 
        parmFile = Settings().get('lastOpenend', default=None) 

    if parmFile and not os.path.isfile (parmFile):
            logger.error ("Parameter file '%s' doesn't exist" %parmFile )
            Settings().set('lastOpenend', None) 
            parmFile = None


    app = QApplication(sys.argv)
    app.setStyle('fusion')

    # Strange: Without setStyleSheet, reset Widget.setPalette doesn't work .. !?
    # Segoe UI is the font of 'fusion' style 
    # font = QFont ()
    # print (font.defaultFamily(), font.family(), font.families())
    app.setStyleSheet ("QWidget { font-family: 'Segoe UI' }")

    # set dark / light mode for widgets depending on system mode 

    scheme = QGuiApplication.styleHints().colorScheme()
    Widget.light_mode = not (scheme == Qt.ColorScheme.Dark)

    # start app 

    Main = App_Main (parmFile)
    Main.show()
    app.exec()
