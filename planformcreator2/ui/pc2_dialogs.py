#!/usr/bin/env pythonbutton_color
# -*- coding: utf-8 -*-

"""  

Dialog windows to handle the different extra functions  

"""

import fnmatch             

from PyQt6.QtWidgets        import QLayout, QDialogButtonBox, QPushButton, QDialogButtonBox
from PyQt6.QtWidgets        import QFileDialog, QTextEdit, QDialog

import pyqtgraph as pg
from pyqtgraph.GraphicsScene.mouseEvents   import MouseClickEvent

from airfoileditor.base.widgets         import * 
from airfoileditor.base.panels          import Dialog, MessageBox 
from airfoileditor.base.diagram         import Diagram, Diagram_Item
from airfoileditor.base.artist          import Artist

from ..model.wing                       import Wing, Planform, Image_Definition, Planform_Paneled, WingSections
from ..model.wing_exports               import Exporter_Airfoils, Exporter_Dxf, Exporter_Xflr5, Exporter_FLZ

from .pc2_artists                       import Image_Artist, Planform_Artist, Ref_Line_Artist, mode

import logging
logger = logging.getLogger(__name__)
# logger.setLevel(logging.DEBUG)


class Dialog_Export_Airfoil (Dialog):
    """ 
    Dialog to export airfoils to a subdirectory
    """

    _width  = 460
    _height = 250

    name = "Export Airfoils"

    def __init__ (self, *args, **kwargs): 

        self._export_btn : QPushButton = None
        self._cancel_btn : QPushButton = None 

        super().__init__ ( *args, **kwargs)

        # connect dialog buttons
        self._cancel_btn.clicked.connect  (self.close)
        self._export_btn.clicked.connect (self._export_airfoils)


    @property
    def wing (self) -> Wing:
        return self.dataObject
    
    @property
    def exporter_airfoils (self) -> Exporter_Airfoils:
        """ the model airfoil exporter"""
        return self.wing.exporter_airfoils


    def _init_layout(self) -> QLayout:

        l = QGridLayout()
        r = 0 
        SpaceR (l, r, stretch=0, height=10) 
        r += 1 
        Field  (l,r,0, width=250, colSpan=2, lab= "To Directory", get=lambda:self.exporter_airfoils.export_dir)
        Button (l,r,4, width=70, text= "Select", set=self._select_directory)
        r += 1
        CheckBox (l,r,1, colSpan=2, text= "Clear directory before export",
                  obj=self.exporter_airfoils, prop=Exporter_Airfoils.clear_export_dir)

        r += 1
        SpaceR (l, r, stretch=0, height=5) 
        r += 1
        CheckBox (l,r,0, colSpan=2, text= "Use airfoils nick name",
                  obj=self.exporter_airfoils, prop=Exporter_Airfoils.use_nick_name)
        r += 1
        CheckBox (l,r,0, colSpan=2, text= "Set a common trailing edge thickness of",
                  obj=self.exporter_airfoils, prop=Exporter_Airfoils.adapt_te_gap)
        FieldF (l,r,2, width=70, unit="mm", step=0.1, lim=(0, 5), dec=1,
                  obj=self.exporter_airfoils, prop=Exporter_Airfoils.te_gap_mm,
                  disable=lambda: not self.exporter_airfoils.adapt_te_gap)
        r += 1
        Label  (l,r,0, colSpan=5, height=50, style=style.COMMENT,
                get="       The common thickness will be achieved, when the exported airfoils\n" +
                    "       will be scaled to their chord length in CAD.\n")
        r += 1
        l.setRowStretch (r,1)
        l.setColumnMinimumWidth (0,80)
        l.setColumnStretch (1,1)
        l.setColumnStretch (3,1)
        l.setColumnStretch (5,2)

        return l


    def _select_directory (self):
        """ select directory for export"""

        directory = QFileDialog.getExistingDirectory(self, caption="Select Export Directory", directory=self.wing.workingDir)

        if directory: 
            self.exporter_airfoils.set_export_dir (directory)                    
            self.refresh()


    def _export_airfoils (self, *_):
        """ do export airfoils"""

        n_airfoils = self.exporter_airfoils.do_it ()
        
        self.close()

        text = f"{n_airfoils} airfoils" if n_airfoils > 1 else  f"{n_airfoils} airfoil"

        MessageBox.success (self,"Export Airfoils", 
                            f"{text} exported to directory<br><br><b>{self.exporter_airfoils.export_dir}</b>")


    @override
    def _on_widget_changed (self):
        """ slot a input field changed - repanel and refresh"""
        self.refresh()


    @override
    def _button_box (self):
        """ returns the QButtonBox with the buttons of self"""

        buttons = QDialogButtonBox.StandardButton.Cancel
        buttonBox = QDialogButtonBox(buttons)

        self._cancel_btn  = buttonBox.button(QDialogButtonBox.StandardButton.Cancel)

        self._export_btn = QPushButton ("&Export", parent=self)
        self._export_btn.setFixedWidth (80)

        buttonBox.addButton (self._export_btn, QDialogButtonBox.ButtonRole.ActionRole)

        return buttonBox 
    



class Dialog_Export_Dxf (Dialog):
    """ 
    Dialog to export dxf including airfoils to a subdirectory
    """

    _width  = 460
    _height = 320

    name = "Export Planform as dxf File"

    def __init__ (self, *args, **kwargs): 

        self._export_btn : QPushButton = None
        self._cancel_btn : QPushButton = None 

        super().__init__ ( *args, **kwargs)

        # connect dialog buttons
        self._cancel_btn.clicked.connect  (self.close)
        self._export_btn.clicked.connect (self._export_dxf)


    @property
    def wing (self) -> Wing:
        return self.dataObject
    
    @property
    def exporter_dxf (self) -> Exporter_Dxf:
        """ the model dxf exporter"""
        return self.wing.exporter_dxf

    @property
    def exporter_airfoils (self) -> Exporter_Airfoils:
        """ the airfoils exporter"""
        return self.exporter_dxf.exporter_airfoils


    def _init_layout(self) -> QLayout:

        l = QGridLayout()
        r = 0 
        Label  (l,r,0, colSpan=5, style=style.COMMENT,
                get="The planform will be exported as a polyline - not as a spline.")
        r += 1
        SpaceR (l, r, height=10) 
        r += 1 
        Field  (l,r,0, width=250, colSpan=2, lab= "To Directory", get=lambda:self.exporter_dxf.export_dir)
        Button (l,r,4, width=70, text= "Select", set=self._select_directory)
        r += 1
        CheckBox (l,r,1, colSpan=2, text= "Clear directory before export",
                  obj=self.exporter_dxf, prop=Exporter_Dxf.clear_export_dir)
        r += 1
        SpaceR (l, r, stretch=0, height=10) 
        r += 1
        CheckBox (l,r,0, colSpan=3, text= "Export airfoils as well into this directory",
                  obj=self.exporter_dxf, prop=Exporter_Dxf.export_airfoils)
        r += 1
        CheckBox (l,r,0, colSpan=3, text= "Use airfoils nick name",
                  obj=self.exporter_airfoils, prop=Exporter_Airfoils.use_nick_name)
        r += 1
        CheckBox (l,r,0, colSpan=2, text= "Set a common trailing edge thickness of",
                  obj=self.exporter_airfoils, prop=Exporter_Airfoils.adapt_te_gap)
        FieldF (l,r,3, width=70, unit="mm", step=0.1, lim=(0, 5), dec=1,
                  obj=self.exporter_airfoils, prop=Exporter_Airfoils.te_gap_mm,
                  disable=lambda: not self.exporter_airfoils.adapt_te_gap)
        r += 1
        Label  (l,r,0, colSpan=5, height=50, style=style.COMMENT,
                get="      The common thickness will be achieved, when the exported airfoils\n" +
                    "      will be scaled to their chord length in CAD.\n")
        r += 1
        l.setRowStretch (r,1)
        l.setColumnMinimumWidth (0,80)
        l.setColumnStretch (1,1)
        l.setColumnStretch (3,1)
        l.setColumnStretch (5,2)

        return l


    def _select_directory (self):
        """ select directory for export"""

        directory = QFileDialog.getExistingDirectory(self, caption="Select Export Directory", directory=self.wing.workingDir)

        if directory: 
            self.exporter_dxf.set_export_dir (directory)                    
            self.refresh()


    def _export_dxf (self, *_):
        """ do export dxf"""

        n_airfoils = self.exporter_dxf.do_it ()
        
        self.close()

        if self.exporter_dxf.export_airfoils:
            plural = "s" if n_airfoils > 1 else ""
            msg = f"Planform {self.exporter_dxf.dxf_filename} and {n_airfoils} airfoil{plural}<br><br>" + \
                  f"exported to <b>{self.exporter_dxf.export_dir}</b>"
        else: 
            msg = f"Planform {self.exporter_dxf.dxf_filename}<br><br>" + \
                  f"exported to <b>{self.exporter_dxf.export_dir}</b>"

        MessageBox.success (self,"Export dxf", msg, min_width=300)


    @override
    def _on_widget_changed (self):
        """ slot a input field changed - repanel and refresh"""
        self.refresh()


    @override
    def _button_box (self):
        """ returns the QButtonBox with the buttons of self"""

        buttons = QDialogButtonBox.StandardButton.Cancel
        buttonBox = QDialogButtonBox(buttons)

        self._cancel_btn  = buttonBox.button(QDialogButtonBox.StandardButton.Cancel)

        self._export_btn = QPushButton ("&Export", parent=self)
        self._export_btn.setFixedWidth (80)

        buttonBox.addButton (self._export_btn, QDialogButtonBox.ButtonRole.ActionRole)

        return buttonBox 
    



class Dialog_Export_Xflr5 (Dialog):
    """ 
    Dialog to export xflr5 xml including airfoils to a subdirectory
    """

    _width  = 460
    _height = 280

    name = "Export Paneled Planform to Xflr5"

    def __init__ (self, *args, **kwargs): 

        self._export_btn : QPushButton = None
        self._cancel_btn : QPushButton = None 

        super().__init__ ( *args, **kwargs)

        # connect dialog buttons
        self._cancel_btn.clicked.connect  (self.close)
        self._export_btn.clicked.connect (self._export_xflr5)


    @property
    def wing (self) -> Wing:
        return self.dataObject
    
    @property
    def exporter_xflr5 (self) -> Exporter_Xflr5:
        """ the model xflr5 exporter"""
        return self.wing.exporter_xflr5

    @property
    def exporter_airfoils (self) -> Exporter_Airfoils:
        """ the airfoils exporter of exporter xflr5"""
        return self.exporter_xflr5.exporter_airfoils


    def _init_layout(self) -> QLayout:

        l = QGridLayout()
        r = 0 
        Label  (l,r,0, colSpan=5, style=style.COMMENT, height=40,
                get="A Xflr5 XML based wing definition file is created from the paneled planform.<br>" +
                    "The involved airfoils will be exported as well into the same directory.")
        r += 1
        SpaceR   (l, r, height=5, stretch=0) 
        r += 1 
        Field    (l,r,0, width=250, colSpan=2, lab= "To Directory", get=lambda:self.exporter_xflr5.export_dir)
        Button   (l,r,4, width=70, text= "Select", set=self._select_directory)
        r += 1
        CheckBox (l,r,1, colSpan=2, text= "Clear directory before export",
                  obj=self.exporter_xflr5, prop=Exporter_Xflr5.clear_export_dir)

        r += 1
        SpaceR   (l, r, stretch=0, height=5) 
        r += 1
        CheckBox (l,r,0, colSpan=2, text= "Use airfoils nick name",
                  obj=self.exporter_airfoils, prop=Exporter_Airfoils.use_nick_name)
        r += 1
        CheckBox (l,r,0, colSpan=2, text= "Set a common flap angle of",
                  obj=self.exporter_airfoils, prop=Exporter_Airfoils.set_flap)
        FieldF   (l,r,2, width=60, step=0.1, lim=(-20,20), dec=1, unit='°', 
                  obj=self.exporter_airfoils, prop=Exporter_Airfoils.flap_angle,
                  disable=lambda: not self.exporter_airfoils.set_flap)
        r += 1
        Label  (l,r,0, colSpan=5, style=style.COMMENT,
                get="      The hinge position will be set according to the flap hinge line",
                hide=lambda: not self.exporter_airfoils.set_flap)
        r += 1
        l.setRowStretch (r,1)
        l.setColumnMinimumWidth (0,80)
        l.setColumnStretch (1,1)
        l.setColumnStretch (3,1)
        l.setColumnStretch (5,2)

        return l


    def _select_directory (self):
        """ select directory for export"""

        directory = QFileDialog.getExistingDirectory(self, caption="Select directory for export",
                                                     directory=self.wing.workingDir)

        if directory: 
            self.exporter_xflr5.set_export_dir (directory)                    
            self.refresh()


    def _export_xflr5 (self, *_):
        """ do export xflr5"""

        n_airfoils = self.exporter_xflr5.do_it ()
        
        self.close()

        plural = "s" if n_airfoils > 1 else ""
        msg = f"Planform {self.exporter_xflr5.xflr5_filename} and {n_airfoils} Airfoil{plural} <br><br>" + \
              f"exported to directory <b>{self.exporter_xflr5.export_dir}</b>"

        MessageBox.success (self,"Export xflr5", msg, min_width=300)


    @override
    def _on_widget_changed (self):
        """ slot a input field changed - repanel and refresh"""
        self.refresh()


    @override
    def _button_box (self):
        """ returns the QButtonBox with the buttons of self"""

        buttons = QDialogButtonBox.StandardButton.Cancel
        buttonBox = QDialogButtonBox(buttons)

        self._cancel_btn  = buttonBox.button(QDialogButtonBox.StandardButton.Cancel)

        self._export_btn = QPushButton ("&Export", parent=self)
        self._export_btn.setFixedWidth (80)

        buttonBox.addButton (self._export_btn, QDialogButtonBox.ButtonRole.ActionRole)

        return buttonBox 
    



class Dialog_Export_FLZ (Dialog):
    """ 
    Dialog to export FLZ vortex file to a subdirectory
    """

    _width  = 460
    _height = 200

    name = "Export Paneled Planform as FLZ vortex File"

    def __init__ (self, *args, **kwargs): 

        self._export_btn : QPushButton = None
        self._cancel_btn : QPushButton = None 

        super().__init__ ( *args, **kwargs)

        # connect dialog buttons
        self._cancel_btn.clicked.connect  (self.close)
        self._export_btn.clicked.connect (self._export_flz)


    @property
    def wing (self) -> Wing:
        return self.dataObject
    
    @property
    def exporter_flz (self) -> Exporter_FLZ:
        """ the model flz exporter"""
        return self.wing.exporter_flz

    @property
    def exporter_airfoils (self) -> Exporter_Airfoils:
        """ the airfoils exporter of exporter flz"""
        return self.exporter_flz.exporter_airfoils


    def _init_layout(self) -> QLayout:

        l = QGridLayout()
        r = 0 
        Label  (l,r,0, colSpan=5, style=style.COMMENT, height=40,
                get="A FLZ vortex project definition file is created from the paneled planform.<br>"+
                    "The involved airfoils are part of the project file.")
        r += 1
        SpaceR (l, r, height=5, stretch=0) 
        r += 1 
        Field  (l,r,0, width=250, colSpan=2, lab= "To Directory", get=lambda:self.exporter_flz.export_dir)
        Button (l,r,4, width=70, text= "Select", set=self._select_directory)
        r += 1
        SpaceR   (l, r, stretch=0, height=10) 
        r += 1
        CheckBox (l,r,0, colSpan=2, text= "Use airfoils nick name",
                  obj=self.exporter_airfoils, prop=Exporter_Airfoils.use_nick_name)
        r += 1
        l.setRowStretch (r,1)
        l.setColumnMinimumWidth (0,80)
        l.setColumnStretch (1,1)
        l.setColumnStretch (3,1)
        l.setColumnStretch (5,2)

        return l


    def _select_directory (self):
        """ select directory for export"""

        directory = QFileDialog.getExistingDirectory(self, caption="Select directory for export",
                                                     directory=self.wing.workingDir)
        if directory: 
            self.exporter_flz.set_export_dir (directory)                    
            self.refresh()


    def _export_flz (self, *_):
        """ do export flz"""

        self.exporter_flz.do_it ()

        self.close()

        msg = f"Paneled Planform exported as {self.exporter_flz.flz_filename}<br><br>" + \
              f"to directory <b>{self.exporter_flz.export_dir}</b>"
        MessageBox.success (self,"Export FLZ", msg)


    @override
    def _on_widget_changed (self):
        """ slot a input field changed"""
        self.refresh()


    @override
    def _button_box (self):
        """ returns the QButtonBox with the buttons of self"""

        buttons = QDialogButtonBox.StandardButton.Cancel
        buttonBox = QDialogButtonBox(buttons)

        self._cancel_btn  = buttonBox.button(QDialogButtonBox.StandardButton.Cancel)

        self._export_btn = QPushButton ("&Export", parent=self)
        self._export_btn.setFixedWidth (80)

        buttonBox.addButton (self._export_btn, QDialogButtonBox.ButtonRole.ActionRole)

        return buttonBox 
    
class Dialog_Export_CSV (Dialog):
    """ 
    Dialog to export paneled planform to CSV file
    """

    _width  = 460
    _height = 200

    name = "Export Paneled Planform as CSV File"

    def __init__ (self, *args, **kwargs): 

        self._export_btn : QPushButton = None
        self._cancel_btn : QPushButton = None 

        super().__init__ ( *args, **kwargs)

        # connect dialog buttons
        self._cancel_btn.clicked.connect (self.close)
        self._export_btn.clicked.connect (self._exporter_csv)


    @property
    def wing (self) -> Wing:
        return self.dataObject
    

    def _init_layout(self) -> QLayout:

        l = QGridLayout()
        r = 0 
        Label  (l,r,0, colSpan=5, style=style.COMMENT, height=40,
                get="The paneled planform will be exported to a CSV file.")

        r += 1
        SpaceR (l, r, height=5, stretch=0) 
        r += 1 
        Field  (l,r,0, width=250, colSpan=2, lab= "To Directory", get=lambda:self.exporter_csv.export_dir)
        Button (l,r,4, width=70, text= "Select", set=self._select_directory)
        r += 1
        SpaceR   (l, r, stretch=0, height=10) 
        
        l.setRowStretch (r,1)
        l.setColumnMinimumWidth (0,80)
        l.setColumnStretch (1,1)
        l.setColumnStretch (3,1)
        l.setColumnStretch (5,2)

        return l

    def _select_directory (self):
        """ select directory for export"""

        directory = QFileDialog.getExistingDirectory(self, caption="Select Export Directory", directory=self.wing.workingDir)

        if directory: 
            self.exporter_csv.set_export_dir (directory)                    
            self.refresh()

    def _exporter_csv (self, *_):
        """ do export csv"""

        self.wing.exporter_csv.do_it ()

        self.close()

        msg = f"Planform exported as {self.wing.exporter_csv.csv_filename}"
        MessageBox.success (self,"Export CSV", msg)


    @override
    def _on_widget_changed (self):
        """ slot a input field changed"""
        self.refresh()


    @override
    def _button_box (self):
        """ returns the QButtonBox with the buttons of self"""

        buttons = QDialogButtonBox.StandardButton.Cancel
        buttonBox = QDialogButtonBox(buttons)

        self._cancel_btn  = buttonBox.button(QDialogButtonBox.StandardButton.Cancel)

        self._export_btn = QPushButton ("&Export", parent=self)
        self._export_btn.setFixedWidth (80)

        buttonBox.addButton (self._export_btn, QDialogButtonBox.ButtonRole.ActionRole)

        return buttonBox

class Dialog_Rename (Dialog):
    """ 
    Dialog to rename a planform file
    """

    _width  = 300
    _height = 120

    name = "Rename Planform"

    def __init__ (self, *args, **kwargs): 

        self._rename_btn : QPushButton = None
        self._cancel_btn : QPushButton = None 

        self._fileName_new : str = ""

        super().__init__ ( *args, **kwargs)

        # connect dialog buttons
        self._cancel_btn.clicked.connect  (self.close)
        self._rename_btn.clicked.connect (self._rename_planform)


    @property
    def wing (self) -> Wing:
        return self.dataObject
    
    @property
    def fileName_new (self) -> str:
        return self._fileName_new
    
    def set_fileName_new (self, fileName_new : str):
        self._fileName_new = fileName_new


    def _init_layout(self) -> QLayout:

        l = QGridLayout()
        r = 0 
        r += 1
        SpaceR (l, r, height=5) 
        r += 1 
        Field  (l,r,0, width=150, lab= "New Name", get=lambda:self.fileName_new, set=self.set_fileName_new)
        r += 1
        SpaceR (l, r, height=5, stretch=2) 
        l.setColumnMinimumWidth (0,80)
        l.setColumnStretch (3,2)

        return l

    def _check_fileName_new (self, fileName_new : str) -> bool:
        """ check if new filename is valid"""

        if not fileName_new.strip():
            MessageBox.error (self, "Rename Planform", "Please enter a valid planform name.")
            return False

        if fnmatch.fnmatch (fileName_new, "*.pc2") :
            MessageBox.error (self, "Rename Planform", "Please enter a planform name without file extension '.pc2'.")
            return False

        if fileName_new.lower() == self.wing.parm_fileName_stem.lower():
            MessageBox.error(self, "Rename Planform", "The new planform name is identical to the current name.")
            return False

        if self.wing.workingDir:
            pathFileName_abs =  os.path.join(self.wing.workingDir, fileName_new + ".pc2")
        else: 
            pathFileName_abs =  fileName_new + ".pc2"
        
        if not os.path.isabs (pathFileName_abs):
            pathFileName_abs = os.path.abspath(pathFileName_abs)       # will insert cwd 

        if os.path.isfile (pathFileName_abs):
            MessageBox.error (self, "Rename Planform", 
                              f"A planform with name <b>{self.fileName_new}</b> already exists.<br>" +
                              "Please choose a different name.")
            return False

        return True


    def _rename_planform (self, *_):
        """ do rename planform"""

        if not self._check_fileName_new (self.fileName_new):
            return
        
        old_name = self.wing.parm_fileName
        self.wing.set_parm_fileName_new (self.fileName_new)

        if old_name != self.wing.parm_fileName:

            self.close()

            msg = f"Planform renamed from <b>{old_name}</b> to <b>{self.wing.parm_fileName}</b>"
            MessageBox.success (self,"Rename Planform", msg, min_width=300)

        else:
            msg = f"Rename planform from <b>{old_name}</b> to <b>{self.wing.parm_fileName}</b> failed."
            MessageBox.error (self,"Rename Planform", msg, min_width=300)
            self.close()



    @override
    def _on_widget_changed (self):
        """ slot a input field changed"""
        self.refresh()


    @override
    def _button_box (self):
        """ returns the QButtonBox with the buttons of self"""

        buttons = QDialogButtonBox.StandardButton.Cancel
        buttonBox = QDialogButtonBox(buttons)

        self._cancel_btn  = buttonBox.button(QDialogButtonBox.StandardButton.Cancel)

        self._rename_btn = QPushButton ("&Rename", parent=self)
        self._rename_btn.setFixedWidth (80)

        buttonBox.addButton (self._rename_btn, QDialogButtonBox.ButtonRole.ActionRole)

        return buttonBox 
    



class Dialog_Edit_Image (Dialog):
    """ 
    Dialog to define image settings for background image
    """

    _width  = (1150, None)
    _height = (670, None)

    name = "Adjust Background Image"

    # --------------------------------------------------------

    class Diagram_Image (Diagram):
        """ The Diagram showing the image  """

        sig_scale_point_changed     = pyqtSignal ()                    # one of the scaler points was changed


        def __init__(self, *args, **kwargs):

            self._image_artist : Image_Artist = None 
            self._plotItem : pg.PlotItem = None  

            super().__init__(*args, **kwargs)

            self.graph_layout.setContentsMargins (10,10,10,10)  # default margins


        @property
        def img_def (self) -> Image_Definition:
            return self._dataObject


        def create_diagram_items (self):
            """ create all plot Items and add them to the layout """

            self._plotItem = pg.PlotItem ()

            self._add_item (self._plotItem, 0, 0)

            # add the artist
            self._image_artist = Image_Artist (self._plotItem, None, image_def=self.img_def, show=True)
            self._image_artist.sig_scale_point_changed.connect  (self.sig_scale_point_changed.emit)


        def setup_viewRange (self):
            """ define view range of this plotItem"""
            
            # setup view range 
            viewBox = self._plotItem.getViewBox()
            viewBox.autoRange (padding=0.05)                   
            viewBox.setAspectLocked()
            viewBox.invertY(True)
            self._plotItem.showGrid(x=False, y=False)
            self._plotItem.showAxis('left', show=False)
            self._plotItem.showAxis('bottom', show=False)


        @override
        def refresh(self):
            """ overridden as we do not have Diagram_Items"""
            self._image_artist.refresh()
            self.setup_viewRange ()


        @override
        def showEvent (self, ev):
            """ overridden as we do not have Diagram_Items"""
            self.refresh()


        @override
        def create_view_panel (self):
            """ no view_panel"""
            pass


    # --------------------------------------------------------

    def __init__ (self, *args, **kwargs): 

        self._ok_btn     : QPushButton = None
        self._close_btn  : QPushButton = None 

        self._diagram : Dialog_Edit_Image.Diagram_Image = None

        super().__init__ ( *args, **kwargs)

        self.setWindowTitle (f"{self.name}   [{self.img_def.filename}]")

        # connect dialog buttons
        self._close_btn.clicked.connect  (self.close)

        # connect scale point moved in diagra 
        self._diagram.sig_scale_point_changed.connect (self.refresh) 

        # initial plot of diagram 
        self._diagram.refresh()  


    @property
    def img_def (self) -> Image_Definition:
            return self.dataObject


    # -------------------------------------------------------------------

    def _init_layout(self) -> QLayout:

        l = QGridLayout()
        r,c = 0,0 
        # SpaceR (l, r, stretch=0, height=5) 
        # r += 1
        Label  (l,r,c, colSpan=10, height=30,
                get="Adjust the image so that it can be used as a background by changing the orientation, colors and scaling.\n")

        # create image diagram 

        self._diagram = self.Diagram_Image (self, self.img_def)
 
        r += 1
        l.addWidget (self._diagram, r, c, 1, 10)        
        l.setRowStretch (r,1)    

        # ---

        SpaceR (l, r, stretch=0, height=20) 
        c += 1
        r += 1 
        Label    (l,r,c,   get="Orientation:")
        CheckBox (l,r,c+1, text= "Mirror horizontal", obj=self.img_def, prop=Image_Definition.mirrored_horizontal)
        CheckBox (l,r,c+3, text= "Mirror vertical",   obj=self.img_def, prop=Image_Definition.mirrored_vertical)
        CheckBox (l,r,c+5, text= "Rotate by 90°",     obj=self.img_def, prop=Image_Definition.rotated)
        Label    (l,r,c+8, get="Leading edge should be at top, planform root should be on the right", style=style.COMMENT)

        r += 1 
        Label    (l,r,c,   get="Coloring:")
        CheckBox (l,r,c+1, text= "Invert color", obj=self.img_def, prop=Image_Definition.invert)
        CheckBox (l,r,c+3, text= "Remove 'red'", obj=self.img_def, prop=Image_Definition.remove_red)
        Label    (l,r,c+5, get="More white")
        Slider   (l,r,c+6, obj=self.img_def, prop=Image_Definition.black_level, step=5, dec=0, lim=(0,130))
        Label    (l,r,c+7, get="More black")
        Label    (l,r,c+8, get="The image should have a dark background with a clearly visible contour", style=style.COMMENT)

        r += 1 
        Label    (l,r,c,   get="Scaling:")
        Label    (l,r,c+1, colSpan=7, get="Move the mouse helper to root of leading edge and to the very tip", style=style.COMMENT)

        l.setColumnMinimumWidth (0,40)
        l.setColumnMinimumWidth (c,  100)
        l.setColumnMinimumWidth (c+2,20)
        l.setColumnMinimumWidth (c+4,20)
        l.setColumnMinimumWidth (8,70)
        l.setColumnStretch (9,4)

        return l


    @override
    def _on_widget_changed (self):
        """ slot a input field changed - repanel and refresh"""
        self.refresh()


    @override
    def refresh (self):
        """ refresh widgets and image"""
        super().refresh()
        self._diagram.refresh ()                          # will reload 


    @override
    def _button_box (self):
        """ returns the QButtonBox with the buttons of self"""

        buttonBox = QDialogButtonBox (QDialogButtonBox.StandardButton.Close) #  | QDialogButtonBox.StandardButton.Cancel)

        self._close_btn  = buttonBox.button(QDialogButtonBox.StandardButton.Close)

        return buttonBox 
    



class Dialog_Select_Template (Dialog):
    """ 
    Dialog to select a template PC2 file to create a new planform
    """

    class Item_Thumbnail (Diagram_Item):
        """ 
        Diagram (Plot) Item for a single wing as Thumbnail
        """

        name        = "Planform"                                # used for link and section header 
        title       = "Wing name"                 
        subtitle    = "dynamic"                                 # will be set dynamically 

        min_width   = 200                                       # min size needed - see below 
        min_height  = 50 

        show_buttons = False                                    # hide resize buttons
        show_coords  = False                                    # hide coordinates

        sig_wing_selected         = pyqtSignal(Wing)            # self was clicked 

        def __init__(self, *args, **kwargs):

            super().__init__(*args, **kwargs)
    
            # set margins (inset) of self 
            self.setContentsMargins ( 300,10,0,10)

            # make scene clickable to add wing section 
            #   delayed as during init scene is not yet available
            QTimer().singleShot (10, self._connect_scene_mouseClick)

        @property
        def wing (self) -> Wing: 
            return self._dataObject

        @property
        def planform (self) -> Planform:
            return self.wing.planform


        @override
        def plot_title(self, **kwargs):
            text_with_br = self.wing.description.replace ("\n", "<br/>")      # textItem needs <br>
            super().plot_title (title=self.wing.name, title_size=Artist.SIZE_NORMAL,
                                subtitle = text_with_br, offset=(10,10), **kwargs)


        @override
        def paint (self, painter, option, widget):           
            """ highlight selected item """
            # overloaded to paint background depending on hover state
            # re-paint is initialized by self.update in hoverEvent
            if self.mouseHovering:
                color = pg.mkColor ("lightskyblue")
                color.setAlphaF (0.4)
                brush = pg.mkBrush(color)
            else: 
                brush = pg.mkBrush("black")
            painter.setBrush(brush)
            painter.drawRect(self.boundingRect())
 
            super().paint(painter, option, widget)


        @override
        def hoverEvent(self, ev):
            super().hoverEvent (ev)
            # will do a re-paint (which will change background) 
            self.update()


        def _connect_scene_mouseClick (self): 
            """ connect mouse click in scene to slot"""           

            scene : pg.GraphicsScene = self.scene()
            if scene:  scene.sigMouseClicked.connect (self._scene_clicked)


        def _scene_clicked (self, ev : MouseClickEvent):
            """ slot - mouse click in scene of self    """ 
        
            # was the scene click in my geometry rectangle?
            if self.geometry().contains (ev._scenePos):
                ev.accept()
                self.sig_wing_selected.emit (self.wing)


        @override
        def setup_artists (self):
            """ create and setup the artists of self"""
            self._add_artist (Planform_Artist       (self, lambda: self.planform, as_contour=True, show_mouse_helper=False))
            self._add_artist (Ref_Line_Artist       (self, lambda: self.planform, mode=mode.REF_TO_PLAN, show_mouse_helper=False))
    

        @override
        def setup_viewRange (self):
            """ define view range of this plotItem"""
            self.viewBox.autoRange (padding=0.05)                   # first ensure best range x,y 
            self.viewBox.setAspectLocked()
            self.viewBox.invertY(True)
            self.showGrid(x=False, y=False)
            self.showAxis('left', show=False)
            self.showAxis('bottom', show=False)


    # --------------------------------------------------------


    class Diagram_Templates (Diagram):
        """ Diagram with a list of planform Thumbnail items   """

        sig_wing_selected         = pyqtSignal(Wing)             # self was clicked 

        def __init__(self, *args, **kwargs):
            super().__init__(*args, **kwargs)

            self.graph_layout.setContentsMargins (10,10,10,10)  # default margins
            self.graph_layout.setVerticalSpacing (10)   
            
            if not self.template_wings: 
                self._message_show ("No template files available")


        @property
        def template_wings (self) -> list [Wing]:
            return self._dataObject


        def create_diagram_items (self):
            """ create all plot Items and add them to the layout """
            for i, wing in enumerate (self.template_wings):
                item = Dialog_Select_Template.Item_Thumbnail (self,wing)
                self._add_item (item, i, 0)

                item.sig_wing_selected.connect (self._on_wing_selected)

        @override
        def create_view_panel (self):
            """ no view_panel"""
            pass

        def _on_wing_selected (self, aWing):
            self.sig_wing_selected.emit (aWing)



    # --- The Dialog ----------------------------------------------------

    _width  = 600
    _height = 650

    name = "Select a Template for a new Wing"

    # --------------------------------------------------------

    def __init__ (self, *args, **kwargs): 

        self._ok_btn     : QPushButton = None
        self._cancel_btn : QPushButton = None 
        self._template_file_selected = None

        self._diagram : Dialog_Select_Template.Diagram_Templates = None

        super().__init__ ( *args, **kwargs)

        # sanity - check template wings exist 
        if self._template_wings() == []:
            MessageBox.error (None, "Select Template", 
                              "No template files found in the template directory.<br>" +
                              "Please copy some .pc2 template files into the template directory:<br><br>" +
                              f"<b>{self.template_dir}</b>")
            raise FileNotFoundError ("No template files found.")


        self._panel.layout().setContentsMargins (QMargins(0, 0, 0, 0))  # no borders in central panel 

        self.setWindowTitle (f"{self.name}")

        # connect dialog buttons
        self._cancel_btn.clicked.connect  (self.close)

        # connect Diagram wing selected
        self._diagram.sig_wing_selected.connect (self._on_wing_selected)

        # initial plot of diagram 
        self._diagram.refresh()  


    @property
    def template_dir (self) -> list [Wing]:
            return self.dataObject

    @property
    def template_file_selected (self) ->str:
        """ the final selected template file"""
        return self._template_file_selected
    

    def _template_wings (self) -> list[Wing]: 
        """ returns a list of template wings from template_dir"""

        wings = []

        if os.path.isdir (self.template_dir):
            pc2_files = fnmatch.filter(os.listdir(self.template_dir), '*.pc2')
            pc2_files = [os.path.normpath(os.path.join(self.template_dir, f)) \
                                for f in pc2_files if os.path.isfile(os.path.join(self.template_dir, f))]
            pc2_files =  sorted (pc2_files, key=str.casefold)
        else:
            pc2_files = []   

        for pc2_file in pc2_files:

            wings.append (Wing (pc2_file))

        return wings


    def _on_wing_selected (self, aWing: Wing):

        self._template_file_selected = aWing._parm_pathFileName
        self.close()


    # -------------------------------------------------------------------

    def _init_layout(self) -> QLayout:

        l = QGridLayout()

        # create image diagram 
        self._diagram = self.Diagram_Templates (self, self._template_wings())
        l.addWidget (self._diagram, 0, 0, 1, 10)        
 
        l.setRowStretch (0,1)    
        l.setColumnStretch (0,1)    

        return l


    @override
    def _button_box (self):
        """ returns the QButtonBox with the buttons of self"""

        buttonBox = QDialogButtonBox (QDialogButtonBox.StandardButton.Cancel) # QDialogButtonBox.StandardButton.Ok  | 

        self._ok_btn     = buttonBox.button(QDialogButtonBox.StandardButton.Ok)
        self._cancel_btn = buttonBox.button(QDialogButtonBox.StandardButton.Cancel)

        return buttonBox 
    

# --------------------------------------------------------


class Dialog_TextEdit (Dialog):

    """ a small text editor """

    _width  = 300
    _height = 150

    name = "Edit"

    def __init__ (self, *args, title : str= None, **kwargs): 

        self._close_btn  : QPushButton = None 

        self._new_text  = None

        super().__init__ ( *args, **kwargs)


        title = title if title is not None else self.name
        self.setWindowTitle (f"{title}")
        self._panel.layout().setContentsMargins (QMargins(0, 0, 0, 0))  # no borders in central panel 

        # connect dialog buttons
        self._close_btn.clicked.connect  (self.close)


    @property
    def text (self) -> str:
        return self.dataObject

    @property
    def new_text (self) -> str:
        """ the edited text when Ok was pressed"""
        return self._new_text
    
    def set_new_text (self, aStr : str):
        self._new_text = aStr

    # -------------------------------------------------------------------

    def _init_layout(self) -> QLayout:

        l = QGridLayout()

        self._qtextEdit = QTextEdit () 

        self._qtextEdit.setPlaceholderText ("Enter text ...")  
        self._qtextEdit.setPlainText (self.text)  

        l.addWidget (self._qtextEdit, 0,0)

        l.setRowStretch (0,1)    
        l.setColumnStretch (0,1)    

        return l


    @override
    def _button_box (self):
        """ returns the QButtonBox with the buttons of self"""

        buttonBox = QDialogButtonBox (QDialogButtonBox.StandardButton.Close) #  | QDialogButtonBox.StandardButton.Cancel)
        self._close_btn  = buttonBox.button(QDialogButtonBox.StandardButton.Close)

        return buttonBox 


    @override 
    def close (self): 
        """ close button clicked """

        self.set_new_text (self._qtextEdit.toPlainText ()) 

        super().close ()

        self.setResult (QDialog.DialogCode.Accepted)



class Dialog_Edit_Paneling (Dialog):
    """ 
    Dialog to edit / define üaneling options of a Paneled Planform
    """

    _width  = 460
    _height = 330

    name = "Define Paneling"

    sig_paneling_changed     = pyqtSignal ()                    # one of the parameters changed

    # --------------------------------------------------------

    def __init__ (self, *args, **kwargs): 

        self._close_btn  : QPushButton = None 

        super().__init__ ( *args, **kwargs)

        # connect dialog buttons
        self._close_btn.clicked.connect  (self.close)
        self._reset_btn.clicked.connect  (self._on_reset)

        # connect widgets change to signal parent
        w : Widget
        for w in self.widgets:
            w.sig_changed.connect (self._on_field_changed)


    @property
    def planform (self) -> Planform_Paneled:
            return self.dataObject

    @property
    def wingSections (self) -> WingSections:
            return self.planform.wingSections

    @property
    def is_parent_trapezoidal (self) -> bool:
        """ True if parent planform is (already) trapezoidal"""

        return self.planform._parent_planform._n_distrib.isTrapezoidal

    # -------------------------------------------------------------------

    def _init_layout(self) -> QLayout:

        l = QGridLayout()   
        r,c = 0, 0 
        Label       (l,r,c, height=40, colSpan=7,
                     get="Define number of panels along chord (x) and per section of wing (y)",
                     style=style.COMMENT)
        r += 1
        c += 1
        FieldI      (l,r,c, width=70, lab="x-Panels", step=1, lim=(1, 20),
                        obj=self.planform, prop=Planform_Paneled.wx_panels)
        r += 1
        Label       (l,r,c, get="x-Distribution")
        ComboBox    (l,r,c+1, width=70,
                        obj=self.planform, prop=Planform_Paneled.wx_dist,
                        options=self.planform.wx_distribution_fns_names)
        r = 1
        FieldI      (l,r,c+4, width=70, lab="y-Panels", step=1, lim=(2, 20),
                        obj=self.planform, prop=Planform_Paneled.wy_panels)
        r += 1
        Label       (l,r,c+4, get="y-Distribution")
        ComboBox    (l,r,c+5, width=70,
                        obj=self.planform, prop=Planform_Paneled.wy_dist,
                        options=self.planform.wy_distribution_fns_names)

        # optimization settings 

        c = 0 
        r += 1
        Label       (l,r,c, height=40, colSpan=5, get="Optimize paneling for an evenly mesh",
                     style=style.COMMENT)            
        r += 1
        c += 1

        # define max. deviation of chord - only if Bezier etc.

        if not self.is_parent_trapezoidal:
            CheckBox    (l,r,c, text="Minimize deviation of chord", colSpan=4,
                                obj=self, prop=Dialog_Edit_Paneling.activated_cn_diff_max)
            FieldF      (l,r,c+4, width=70, step=0.5, lim=(0.5, 50), dec=1, unit="%", 
                            obj=self.planform, prop=Planform_Paneled.cn_diff_max, 
                            hide= lambda: not bool(self.planform.cn_diff_max))
            Label       (l,r,c+5, get=lambda: f"currently {self.planform.cn_diff:.1%}", colSpan=2, 
                            style=style.COMMENT)
            r +=1
        
        # minimum panel width

        CheckBox    (l,r,c, text="Set a minimum panel width", colSpan=4,
                            obj=self, prop=Dialog_Edit_Paneling.activated_width_min_targ)
        FieldF      (l,r,c+4, width=70,  step=0.5, lim=(0.5, 10), dec=1, unit="%", 
                        obj=self.planform, prop=Planform_Paneled.width_min_targ,
                        hide= lambda: not bool(self.planform.width_min_targ))  

        Label       (l,r,c+5, get=lambda: f"currently {self.planform.width_min_cur:.1%}", colSpan=2, 
                            style=style.COMMENT)
        r += 1
        
        # minimum tip chord

        CheckBox    (l,r,c, text="Set a minimum chord for tip", colSpan=4,
                        obj=self, prop=Dialog_Edit_Paneling.activated_cn_tip_min)
        FieldF      (l,r,c+4, width=70, step=1, lim=(1, 50), dec=1, unit="%", 
                        obj=self.planform, prop=Planform_Paneled.cn_tip_min,
                        hide= lambda: not bool(self.planform.cn_tip_min))
        Label       (l,r,c+5, get=lambda: f"currently {self.planform.cn_tip_cur:.1%}", colSpan=2, 
                            style=style.COMMENT)

        l.setColumnMinimumWidth (0,20)
        l.setColumnMinimumWidth (4,40)
        l.setColumnStretch (8,5)

        l.setRowStretch (r+1, 1)
        
        return l 

    @property
    def activated_cn_diff_max (self) -> bool:
        return self.planform.cn_diff_max is not None

    def set_activated_cn_diff_max (self, aBool):
        if aBool and not self.activated_cn_diff_max:
            self.planform.set_cn_diff_max (0.01)
        elif not aBool:
            self.planform.set_cn_diff_max (None)   

    @property
    def activated_cn_tip_min (self) -> bool:
        return self.planform.cn_tip_min is not None

    def set_activated_cn_tip_min (self, aBool):
        if aBool and not self.activated_cn_tip_min:
            self.planform.set_cn_tip_min (0.0)                  # will be recalc
        elif not aBool:
            self.planform.set_cn_tip_min (None)   


    @property
    def activated_width_min_targ (self) -> bool:
        return self.planform.width_min_targ is not None

    def set_activated_width_min_targ (self, aBool):
        if aBool and not self.activated_width_min_targ:
            self.planform.set_width_min_targ (0.0)              # will be recalc
        elif not aBool:
            self.planform.set_width_min_targ (None)   


    def _on_reset (self, *_):
        """ reset paneling to default values"""

        self.planform.reset ()
        self.refresh ()
        self.sig_paneling_changed.emit()                        # refresh diagram


    @override
    def _on_field_changed (self, *_):
        """ slot for widget changes"""
        self.refresh ()                                         # have 'soft' refresh when settings are changed
        self.sig_paneling_changed.emit()                        # refresh diagram


    @override
    def _button_box (self):
        """ returns the QButtonBox with the buttons of self"""

        buttonBox = QDialogButtonBox (QDialogButtonBox.StandardButton.Close) #  | QDialogButtonBox.StandardButton.Cancel)

        self._close_btn  = buttonBox.button(QDialogButtonBox.StandardButton.Close)

        self._reset_btn = QPushButton ("&Reset", parent=self)
        self._reset_btn.setFixedWidth (80)
        self._reset_btn.setToolTip ("Reset paneling to default values")
        buttonBox.addButton (self._reset_btn, QDialogButtonBox.ButtonRole.ResetRole)

        return buttonBox 
    
