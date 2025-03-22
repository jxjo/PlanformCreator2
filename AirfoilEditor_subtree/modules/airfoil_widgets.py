#!/usr/bin/env pythonbutton_color
# -*- coding: utf-8 -*-

"""  

Higher level Widgets to handle Airfoil UI operations like Open and Select

"""

import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.WARNING)

import os
import fnmatch             

from PyQt6.QtCore           import QMargins
from PyQt6.QtWidgets        import QHBoxLayout
from PyQt6.QtWidgets        import QFileDialog, QWidget

from base.widgets           import * 
from base.panels            import MessageBox

from model.airfoil          import Airfoil, Airfoil_Bezier, Airfoil_Hicks_Henne
from model.airfoil          import GEO_BASIC
from model.airfoil_examples import Example


# ----- common methods -----------

def create_airfoil_from_path (parent, pathFilename, example_if_none=False, message_delayed=False) -> Airfoil:
    """
    Create and return a new airfoil based on pathFilename.
        Return None if the Airfoil couldn't be loaded 
        or 'Example' airfoil if example_if_none == True  
    """

    file_found = True
    airfoil_loaded = False
    airfoil = None

    try: 
        extension = os.path.splitext(pathFilename)[1]
        if extension == ".bez":
            airfoil = Airfoil_Bezier (pathFileName=pathFilename)
        elif extension == ".hicks":
            airfoil = Airfoil_Hicks_Henne (pathFileName=pathFilename)
        else: 
            airfoil = Airfoil(pathFileName=pathFilename, geometry=GEO_BASIC)
    except:
        file_found = False

    if file_found:
        airfoil.load()

        if airfoil.isLoaded:                      
            airfoil_loaded = True 
        else: 
            airfoil_loaded = False

    if not file_found or not airfoil_loaded:
        if example_if_none:
            airfoil = Example()
        else: 
            airfoil = None 

        if pathFilename: 
            fileName = os.path.basename (pathFilename)
            if not file_found:
                msg = f"{fileName} does not exist."
            else: 
                msg = f"{fileName} couldn't be loaded."
            if example_if_none:
                example = "\nUsing example airfoil."
            else:
                example= ""
            if message_delayed:
                QTimer.singleShot (100, lambda: MessageBox.error   (parent,'Load Airfoil', f"{msg}{example}", min_height= 60))
            else:
                MessageBox.error   (parent,'Load Airfoil', f"{msg}{example}", min_height= 60)

    return airfoil  


def get_airfoil_files_sameDir (initialDir : Airfoil | str | None): 
    """ 
    Returns list of airfoil file path in the same directory as airf
    All .dat, .bez and .hicks files are collected 
    """

    if initialDir is None: return []

    if isinstance(initialDir, Airfoil):
        airfoil : Airfoil = initialDir
        if airfoil.pathFileName is not None: 
            airfoil_dir = os.path.dirname(initialDir.pathFileName) 
        else:
            airfoil_dir = None 
    else: 
        airfoil_dir = initialDir

    if not airfoil_dir: airfoil_dir = '.'

    if os.path.isdir (airfoil_dir):
        dat_files = fnmatch.filter(os.listdir(airfoil_dir), '*.dat')
        bez_files = fnmatch.filter(os.listdir(airfoil_dir), '*.bez')
        hh_files  = fnmatch.filter(os.listdir(airfoil_dir), '*.hicks')
        airfoil_files = dat_files + bez_files + hh_files
        airfoil_files = [os.path.normpath(os.path.join(airfoil_dir, f)) \
                            for f in airfoil_files if os.path.isfile(os.path.join(airfoil_dir, f))]
        return sorted (airfoil_files, key=str.casefold)
    else:
        return []


# ----- widgets -----------



class Airfoil_Open_Widget (Widget, QWidget):
    """ 
    Button - either Text or Icon to open Airfoil with file select 

    When user successfully selected an airfoil file, 'set' is called with 
    the new <Airfoil> as argument 
    """


    def __init__(self, *args,
                 set = None,                # will set new airfoil
                 text = "Open", 
                 asIcon = False,            # Button as icon 
                 **kwargs):
        super().__init__(*args, set=set, **kwargs)

        # build local HBox layout with the guest widget

        l = QHBoxLayout(self)
        l.setContentsMargins (QMargins(0, 0, 0, 0))
        l.setSpacing (0)
        l.setStretch (0,2)
        if asIcon:
            widget= ToolButton (l, icon=Icon.OPEN, set=self._open, toolTip="Select airfoil")
            self._width = widget._width             # self shall have the same fixed width
        else: 
            widget = Button    (l, text=text, width=self._width, set=self._open, toolTip="Select airfoil")
        self.setLayout (l) 

        # normal widget handling 

        self._get_properties ()
        self._set_Qwidget_static ()
        self._set_Qwidget ()

        # assign widget to parent layout 

        self._layout_add ()


    def _open (self):
        """ open a new airfoil and load it"""

        filters  = "Airfoil files (*.dat);;Bezier files (*.bez);;Hicks Henne files (*.hicks)"

        if isinstance (self._val, Airfoil):
            directory = self._val.pathName_abs
        else:
            directory = None 
        newPathFilename, _ = QFileDialog.getOpenFileName(self, filter=filters, directory=directory)

        if newPathFilename:                         # user pressed open
            airfoil = create_airfoil_from_path (self, newPathFilename)
            if airfoil is not None: 

                #leave button callback and refresh in a few ms 
                timer = QTimer()                                
                timer.singleShot(10, lambda: self._set_value (airfoil))     # delayed emit 




class Airfoil_Select_Open_Widget (Widget, QWidget):
    """ 
    Compound widget to either select or open new airfoil 
        - ComboBox (optional with spin) with files in same directory
        - optional Open button to select new airfoil 
        - optional Delete button to delete current   

    When user successfully selected an airfoil file, 'set' is called with 
    the new <Airfoil> as argument 

    When user deleted, 'set' is called with None 
    """

    _width  = (120, None)

    def __init__(self, *args,
                 get = None,                # get current / initial airfoil 
                 set = None,                # will set new airfoil
                 addEmpty = False,          # add empty entry to fie list 
                 asSpin = False,            # ComboBox with spin buttons 
                 withOpen = True,           # include open button 
                 textOpen = "Open",         # text for Open button 
                 widthOpen = 100,           # width of open text button 
                 initialDir : Airfoil | str | None = None, # either an airfoil or a pathString 
                 **kwargs):
        super().__init__(*args, get=get, set=set, **kwargs)

        self._combo_widget = None 
        self._no_files_here = None
        self._initial_dir = initialDir
        self._addEmpty = addEmpty is True 
        self

        # get initial properties  (cur airfoil) 
        self._get_properties ()

        # build local HBox layout with the guest widget

        l = QHBoxLayout(self)
        l.setContentsMargins (QMargins(0, 0, 0, 0))
        if asSpin: 
            self._combo_widget = ComboSpinBox (l, get=self.airfoil_fileName, 
                                                set=self.set_airfoil_by_fileName, 
                                                options=self.airfoil_fileNames_sameDir,
                                                hide=self.no_files_here,
                                                toolTip=self._toolTip,
                                                signal=False)
        else:             
            self._combo_widget = ComboBox      (l, get=self.airfoil_fileName, 
                                                set=self.set_airfoil_by_fileName, 
                                                options=self.airfoil_fileNames_sameDir,
                                                hide=self.no_files_here, 
                                                toolTip=self._toolTip,
                                                signal=False)
        if withOpen:
            # either text button if nothing is there
            Airfoil_Open_Widget (l, text=textOpen, set=self.set_airfoil, signal=False,
                                 width = widthOpen,
                                 hide=lambda: not self.no_files_here())
            # ... or icon button together with combo box 
            Airfoil_Open_Widget (l, asIcon=True, set=self.set_airfoil, signal=False,
                                 hide=self.no_files_here)
            l.insertStretch (-1)

        l.setContentsMargins (QMargins(0, 0, 0, 0))

        if self.no_files_here():                                # show only open button
            l.setSpacing (0)
            l.setStretch (0,0)
            l.setStretch (2,2)
        else:                                                   # combobox with open 
            l.setSpacing (1)
            l.setStretch (0,2)
            l.setStretch (2,0)

        self.setLayout (l)

        # normal widget handling 

        self._get_properties ()
        self._set_Qwidget_static ()
        self._set_Qwidget ()

        name = self.airfoil_fileName() if self.airfoil_fileName() is not None else ""
        tip = f"{self._toolTip}: {name}" if self._toolTip else f"{name}"
        self._combo_widget.setToolTip (tip)  

        # assign self to parent layout 

        self._layout_add ()

    @override
    def refresh (self, disable=None):

        self._no_files_here = None                              # reset cached vlaue 
        super().refresh(disable) 

        l : QHBoxLayout = self.layout()
        if self.no_files_here():                                # show only open button
            l.setSpacing (0)
            l.setStretch (0,0)
            l.setStretch (2,2)
        else:                                                   # combobox with open 
            l.setSpacing (1)
            l.setStretch (0,2)
            l.setStretch (2,0)


    def no_files_here (self) -> bool:
        """ True if no files to select in combo """

        if self._no_files_here is None: 
            n = len (self.airfoil_fileNames_sameDir())
            if n== 1:
                # there can be a blank or exmaple entry as the first item 
                fileName_first = self.airfoil_fileNames_sameDir()[0]
                no =  fileName_first == ""
            elif n == 0: 
                no =  True
            else: 
                no = False
            self._no_files_here = no 
        return self._no_files_here


    @property
    def airfoil (self) -> Airfoil:
        return self._val
    def set_airfoil (self, anAirfoil : Airfoil):

        # do manual set value and callback to avoid refresh ping-pong
        self._val = anAirfoil 
        self._no_files_here = None                      # reset cached value 

        # refresh the sub widgets -> hide / show 
        w : Widget
        for w in self.findChildren (Widget):
            w.refresh()

        # set tooltip of combobox to show full filename
        self._combo_widget.setToolTip (self.airfoil_fileName())

        # leave self for callback in a few ms 
        timer = QTimer()                                
        timer.singleShot(10, lambda: self._set_value_callback ())  

        # no emit_change   
        pass


    def airfoil_fileName (self) -> str | None:
        return self.airfoil.fileName if self.airfoil is not None else None

    def set_airfoil_by_fileName (self, newFileName): 
        """ set new current airfoil bei filename""" 

        # Empty selected - set airfoil to None
        if not newFileName:
            self.set_airfoil (None)
            return 

        # get full path of new fileName 
        if self.airfoil is None:
            sameDir = self._initial_dir
        else: 
            sameDir = self.airfoil

        for aPathFileName in get_airfoil_files_sameDir (sameDir):
            if newFileName == os.path.basename(aPathFileName):

                if os.path.isfile (aPathFileName):   # maybe it was deleted in meantime 
                     
                    airfoil = create_airfoil_from_path (self, aPathFileName)
                    if airfoil is not None: 
                        self.set_airfoil (airfoil)
                break


    def airfoil_fileNames_sameDir (self): 
        """ list of airfoil filenames in the same dir as current airfoil"""

        if self.airfoil is None:
            sameDir = self._initial_dir
        else: 
            sameDir = self.airfoil

        fileNames = []
        if self._addEmpty: 
            fileNames.append ("")
           
        for aFileName in get_airfoil_files_sameDir (sameDir):
            fileNames.append(os.path.basename(aFileName))

        return fileNames