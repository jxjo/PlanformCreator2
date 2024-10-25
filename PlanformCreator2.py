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
from PyQt6.QtGui            import QCloseEvent

# let python find the other modules in modules relativ to path of self - ! before python system modules
# common modules hosted by AirfoilEditor 
sys.path.insert (1,os.path.join(Path(__file__).parent , 'AirfoilEditor_subtree/modules'))
# local modules
sys.path.insert (1,os.path.join(Path(__file__).parent , 'modules'))

from wing                   import Wing
from wing                   import Planform, Planform_DXF, Planform_Trapezoidal, Planform_Bezier         

from base.common_utils      import * 
from base.panels            import Container_Panel
from base.widgets           import *

# from AirfoilEditor_subtree  import AirfoilEditor

from pc2_panels             import *
from pc2_diagrams           import *


import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

#------------------------------------------------

AppName    = "Planform Creator 2"
AppVersion = "2.0 beta.1"



#-------------------------------------------------------------------------------
# The App   
#-------------------------------------------------------------------------------

class App_Main (QMainWindow):

    name = AppName  

   # Signals 

    sig_wing_new            = pyqtSignal()              # new wing loaded 
    sig_cur_wingSection     = pyqtSignal()              # current wing section changed




    def __init__(self, paramFile):
        super().__init__()

        self.initial_geometry   = None                  # window geometry at the bginning
        self._cur_wingSection = None                    # Dispatcher field between Diagram and Edit
        self.paramFile = ''                             # paramter file with wing settings  
        self._myWing : Wing = None                      # actual wing model 

        # if paramFile: 
        #     message = f"Loading\n\n{os.path.basename(paramFile)}"
        # else: 
        #     message = "Creating\n\na sample wing"
        # splash_window = ToolWindow(self, message, duration=0)

        # get icon either in modules or in icons 
        
        self.setWindowIcon (Icon ('AE_ico.ico'))

        # get initial window size from settings

        Settings.belongTo (__file__, nameExtension=None, fileExtension= '.settings')
        geometry = Settings().get('window_geometry', [])
        maximize = Settings().get('window_maximize', False)
        Win_Util.set_initialWindowSize (self, size_frac= (0.80, 0.70), pos_frac=(0.1, 0.1),
                                        geometry=geometry, maximize=maximize)

        # create the 'wing' model - with 'splash window'
        self.load_wing (paramFile, initial=True)

        # init main layout of app

        self._data_panel    = Container_Panel  (title="Data panel")
        self._file_panel    = Container_Panel  (title="File panel", width=240)
        self._diagram_panel = Diagram_Planform (self, self.wing, self.cur_wingSection, welcome=self._welcome_message())

        l_main = self._init_layout() 

        container = QWidget()
        container.setLayout (l_main) 
        self.setCentralWidget(container)


        # connect to signals from diagram

        self._diagram_panel.sig_wingSection_new.connect  (self.set_cur_wingSection)

        # connect to signals of self

            # self.sig_airfoil_changed.connect (self.refresh)

        # connect signals to slots of diagram

        self.sig_wing_new.connect        (self._diagram_panel.on_wing_new)
        self.sig_cur_wingSection.connect (self._diagram_panel.on_cur_wingSection_changed)


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
        # l_data.addWidget (Panel_Geometry    (self, self.airfoil))
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
        lower.setMinimumHeight(180)
        lower.setMaximumHeight(180)
        lower.setLayout (l_lower)

        # main layout with diagram panel and lower 

        l_main = QVBoxLayout () 
        l_main.addWidget (self._diagram_panel, stretch=2)
        l_main.addWidget (lower)
        l_main.setContentsMargins (QMargins(5, 5, 5, 5))

        return l_main 


    def wing (self) -> Wing:
        """ encapsulates current wing. Childs should acces only via this function
        to enable a new wing to be set """
        return self._myWing

      
    def cur_wingSection (self) -> WingSection:
        """ Dispatcher for current WingSection between Edit and Diagram """
        if self._cur_wingSection is None: 

            if len (self.wing().wingSections) > 2:
                self._cur_wingSection = self.wing().wingSections[1]     # set second section as initial
            else:        
                self._cur_wingSection = None                            # nothing selected

        return self._cur_wingSection 


    def set_cur_wingSection (self, aSection : WingSection | None):
        """ set current wing section"""
        self._cur_wingSection = aSection
        logger.debug (f"{aSection} as current")

        self.sig_cur_wingSection.emit()


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

        if self.paramFile:
            project = self.paramFile
        else:
            project = "< new >"
        self.setWindowTitle (AppName + "  v" + str(AppVersion) + "  [" + project + "]")


    def _welcome_message (self) -> str: 
        """ returns a HTML welcome message which is shown on first start up """

        # use Notepad++ or https://froala.com/online-html-editor/ to edit 

        message = """
<p><span style="background-color: black">
<span style="font-size: 18pt; color: lightgray; ">Welcome to the <strong>Airfoil<span style="color:deeppink">Editor</span></strong></span></p>
<p><span style="background-color: black">
This is an example airfoil as no airfoil was provided on startup. Try out the functionality with this example airfoil or <strong><span style="color: silver;">Open&nbsp;</span></strong>an existing airfoil.
</span></p>
<p><span style="background-color: black">
You can view the properties of an airfoil like thickness distribution or camber, analyze the curvature of the surface or <strong><span style="color: silver;">Modify</span></strong> the airfoils geometry.<br>
<strong><span style="color: silver;">New as Bezier</span></strong> allows to convert the airfoil into an airfoil which is based on two Bezier curves.
</span></p>
<p><span style="background-color: black">
<span style="color: deepskyblue;">Tip: </span>Assign the file extension '.dat' to the Airfoil Editor to open an airfoil with a double click.
</span></p>
    """
        
        return message


    def _save_settings (self):
        """ save settings, eg Window size and position, to file """

        Settings().set('window_geometry', self.normalGeometry ().getRect())
        Settings().set('window_maximize', self.isMaximized())


    @override
    def closeEvent  (self, event : QCloseEvent):
        """ main window is closed """

        self._save_settings ()


        # save changes? 
        if self.wing().hasChanged(): 

            message = "There are unsaved changes.\n\n" + \
                       "Do you want to save before exit?"
            # mb = Messagebox (self, title="Close "+ AppName, message=message, icon="warning", 
            #                option_1="Yes", option_2="No", option_3="Cancel")
            response = "Yes"

            if response == "Yes":
                self.save()
                event.accept()
            elif response == "No":
                event.accept()
            else:
                pass
        else:
            event.accept()



        


    #------- file functions ----------------

    def new (self):
        """ reset - and start with example definition"""

        text = "The current wing '%s' will be discarded." % self.wing().name
        # msg  = Messagebox(self, title="Create new wing", message=text,
        #           icon="warning", option_2="Cancel", option_1="Ok")            
        # if msg.get() == "Ok":
        #     self.load_wing ("")               # will create default wing


    def open (self):
        """ open a new wing definition json and load it"""

        filters  = "PlanformCreator2 files (*.pc2)"
        newPathFilename, _ = QFileDialog.getOpenFileName(self, filter=filters)

        if newPathFilename:                     
            #leave button callback and refresh in a few ms 
            timer = QTimer()                                
            timer.singleShot(10, lambda: self.load_wing (newPathFilename))     # delayed emit 



    def save (self):
        """ save wing data to the action parameter file - if new wing to saveAs"""

        if self.paramFile:
            saveOk = self.wing().save(self.paramFile)
            # if saveOk:
            #     text = "Wing successfully saved ...    " 
            #     Messagebox(self, title="Save wing", message=text, icon="check", option_1="Ok", width=300, height=150)  
            #     Settings().set('lastOpenend', self.paramFile)
            # else:
            #     text = "Paramteres couldn't be saved to '%s'" % self.paramFile
            #     Messagebox(self, title="Save wing", message=text, icon="cancel", option_1="Close", width=300, height=150)  
        else:
            self.saveAs ()


    def saveAs (self):
        """ save wing data to a new file and set this as actual"""

        filetypes  = [('PC2 files', '*.pc2')]
        # newPathFilename = filedialog.asksaveasfilename(title='Save parameter file',
        #                              initialdir=self.workingDir, filetypes=filetypes,
        #                              defaultextension = '.pc2')
        # if newPathFilename: 
        #     saveOk =  self.wing().save(newPathFilename)
        #     if saveOk: 
        #         self.paramFile = os.path.normpath(newPathFilename)
        #         self.set_title ()
        #         text = "Wing saved to \n\n'%s'" % newPathFilename
        #         Messagebox(self, title="Save wing", message=text, icon="check", option_1="Ok")  
        #         Settings().set('lastOpenend', self.paramFile)
        #     else: 
        #         text = "Wing couldn't be saved to '%s'" % newPathFilename
        #         Messagebox(self, title="Save wing", message=text, icon="cancel", option_1="Ok")  


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
            self.paramFile = PathHandler.relPath (pathFilename)
            Settings().set('lastOpenend', self.paramFile)
        else:
            self.paramFile = ""
        self.set_title ()

        if not initial: 
            self.sig_wing_new.emit()


    # def export_xflr5 (self): 
    #     """ export wing to xflr5"""
    #     self.wait_window (Dialog_Export_Xflr5_Flz (self, self.wing, Xflr5=True))


    # def export_flz (self): 
    #     """ export wing to xflr5"""
    #     self.wait_window (Dialog_Export_Xflr5_Flz (self, self.wing, Flz=True))


    # def export_dxf (self):
    #     """export wing to dxf"""
    #     self.wait_window (Dialog_Export_Dxf (self, wingFn = self.wing))


    # def export_airfoils (self):
    #     """export airfoils of wing"""
    #     self.wait_window (Dialog_Export_Airfoils (self, wingFn = self.wing))


    # def load_reference_dxf (self): 
    #     """ load a dxf planform into the reference_dxf planform"""
    #     current_dxf_path = self.wing().refPlanform_DXF.dxf_pathFilename

    #     dxf_dialog = Dialog_Load_DXF (self, wingFn = self.wing, dxf_Path = current_dxf_path, ref=True, workingDir = self.workingDir) 
    #     self.wait_window (dxf_dialog)

    #     if dxf_dialog.return_OK: 
    #         new_dxf_Path = dxf_dialog.dxf_pathFilename
    #         if new_dxf_Path:                            # dialog returned a valid path 
    #             self.wing().refPlanform_DXF.set_dxfPathFilename (new_dxf_Path) 
    #             fireEvent(self.ctk_root, PLANFORM_CHANGED)
    #         else:                                       # remove dxf reference file - extra code to make it clear
    #             self.wing().refPlanform_DXF.set_dxfPathFilename (None) 
    #             fireEvent(self.ctk_root, PLANFORM_CHANGED)



#--------------------------------

if __name__ == "__main__":


    dev_mode = os.path.isdir(os.path.dirname(os.path.realpath(__file__)) +"\\test_airfoils")

    # init logging  

    if dev_mode:   
        init_logging (level= logging.DEBUG)             # INFO, DEBUG or WARNING
    else:                       
        init_logging (level= logging.WARNING)

    # paramter file as argument?  

    parmFile = ''
    parser = argparse.ArgumentParser(prog=AppName, description='Create a wing planform')
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

    Main = App_Main (parmFile)
    Main.show()
    app.exec()
