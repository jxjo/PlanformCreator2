#!/usr/bin/env pythonbutton_color
# -*- coding: utf-8 -*-

"""  

Dialog windows to handle the different extra functions  

"""

import fnmatch             

from PyQt6.QtWidgets        import QLayout, QDialogButtonBox, QPushButton, QDialogButtonBox
from PyQt6.QtWidgets        import QFileDialog, QTextEdit

import pyqtgraph as pg
from pyqtgraph.GraphicsScene.mouseEvents        import MouseClickEvent

from base.widgets           import * 
from base.panels            import Dialog, MessageBox 
from base.diagram           import Diagram, Diagram_Item

from base.artist            import Artist
from pc2_artists            import Image_Artist, Planform_Artist, Ref_Line_Artist, mode

from wing                   import Wing, Planform, Image_Definition 
from wing_exports           import Export_Airfoils, Export_Dxf

import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


class Dialog_Export_Airfoil (Dialog):
    """ 
    Dialog to export airfoils to a subdirectory
    """

    _width  = 480
    _height = 280

    name = "Export Airfoils"

    def __init__ (self, *args, **kwargs): 

        self._export_btn : QPushButton = None
        self._close_btn  : QPushButton = None 

        super().__init__ ( *args, **kwargs)

        # move window a little to the side 
        p_center = self._parent.rect().center()               # parent should be main window
        pos_x = p_center.x() - 300
        pos_y = p_center.y() + 100            
        self.move(pos_x, pos_y)

        # connect dialog buttons
        self._close_btn.clicked.connect  (self.close)
        self._export_btn.clicked.connect (self._export_airfoils)


    @property
    def wing (self) -> Wing:
        return self.dataObject
    
    @property
    def export_airfoils (self) -> Export_Airfoils:
        """ the model airfoil exporter"""
        return self.wing.export_airfoils


    def _init_layout(self) -> QLayout:

        l = QGridLayout()
        r = 0 
        SpaceR (l, r, stretch=0, height=5) 
        r += 1
        Label  (l,r,0, colSpan=5, height=60,
                get="Export the airfoils of all wing sections to a subdirectory. The trailing edge thickness\n" + 
                    "may be modified to achieve a common thickness of all airfoils in real coordinates.\n")
        r += 1
        SpaceR (l, r, stretch=0, height=10) 
        r += 1 
        Field  (l,r,0, width=250, colSpan=1, lab= "To Directory", get=lambda:self.export_airfoils.export_dir)
        Button (l,r,4, width=70, text= "Select", set=self._select_directory)
        r += 1
        SpaceR (l, r, stretch=0, height=10) 
        r += 1
        CheckBox (l,r,1, colSpan=2, text= "Use airfoils nick name",
                  obj=self.export_airfoils, prop=Export_Airfoils.use_nick_name)
        r += 1
        CheckBox (l,r,1, colSpan=2, text= "Set a common trailing edge thickness of",
                  obj=self.export_airfoils, prop=Export_Airfoils.adapt_te_gap)
        FieldF (l,r,4, width=70, unit="mm", step=0.1, lim=(0, 5), dec=1,
                  obj=self.export_airfoils, prop=Export_Airfoils.te_gap_mm,
                  disable=lambda: not self.export_airfoils.adapt_te_gap)
                # obj=self, prop=Panel_WingSection.x, 
                # disable=lambda: self._wingSection().is_root_or_tip)

        r += 1
        SpaceR (l, r, height=5) 
        l.setColumnMinimumWidth (0,80)
        l.setColumnStretch (5,2)

        return l


    def _select_directory (self):
        """ select directory for export"""

        directory = QFileDialog.getExistingDirectory(self)

        if directory: 
            self.export_airfoils.set_export_dir (directory)                    
            self.refresh()


    def _export_airfoils (self, *_):
        """ do export airfoils"""

        self.export_airfoils.do_it ()
        
        self.close()

        MessageBox.success (self,"Export Airfoils", 
                            f"{self.export_airfoils.n_airfoils} Airfoils exported to directory\n\n{self.export_airfoils.export_dir}")


    @override
    def _on_widget_changed (self):
        """ slot a input field changed - repanel and refresh"""
        self.refresh()


    @override
    def _button_box (self):
        """ returns the QButtonBox with the buttons of self"""

        buttons = QDialogButtonBox.StandardButton.Cancel
        buttonBox = QDialogButtonBox(buttons)

        self._close_btn  = buttonBox.button(QDialogButtonBox.StandardButton.Cancel)

        self._export_btn = QPushButton ("&Export", parent=self)
        self._export_btn.setFixedWidth (80)

        buttonBox.addButton (self._export_btn, QDialogButtonBox.ButtonRole.ActionRole)

        return buttonBox 
    



class Dialog_Export_Dxf (Dialog):
    """ 
    Dialog to export dxf including airfoils to a subdirectory
    """

    _width  = 480
    _height = 320

    name = "Export Planform as Dxf"

    def __init__ (self, *args, **kwargs): 

        self._export_btn : QPushButton = None
        self._close_btn  : QPushButton = None 

        super().__init__ ( *args, **kwargs)

        # move window a little to the side 
        p_center = self._parent.rect().center()               # parent should be main window
        pos_x = p_center.x() - 300
        pos_y = p_center.y() + 100            
        self.move(pos_x, pos_y)

        # connect dialog buttons
        self._close_btn.clicked.connect  (self.close)
        self._export_btn.clicked.connect (self._export_airfoils)


    @property
    def wing (self) -> Wing:
        return self.dataObject
    
    @property
    def export_dxf (self) -> Export_Dxf:
        """ the model dxf exporter"""
        return self.wing.export_dxf

    @property
    def export_airfoils (self) -> Export_Dxf:
        """ the model airfoils exporter"""
        return self.wing.export_airfoils


    def _init_layout(self) -> QLayout:

        l = QGridLayout()
        r = 0 
        # SpaceR (l, r, stretch=0, height=2) 
        # r += 1
        Label  (l,r,0, colSpan=5, height=80, 
                get="Export the planform as a dxf file to a subdirectory. Optionally, the airfoils can\n" + 
                    "be exported to the same directory.\n" + 
                    "Please be aware that the planform is exported as a polyline - not as a spline.")
        r += 1
        SpaceR (l, r, stretch=1, height=10) 
        r += 1 
        Field  (l,r,0, width=250, colSpan=1, lab= "To Directory", get=lambda:self.export_dxf.export_dir)
        Button (l,r,4, width=70, text= "Select", set=self._select_directory)
        r += 1
        SpaceR (l, r, stretch=0, height=10) 
        r += 1
        CheckBox (l,r,0, colSpan=2, text= "Export the airfoils as well",
                  obj=self.export_dxf, prop=Export_Dxf.export_airfoils)
        r += 1
        CheckBox (l,r,1, colSpan=2, text= "Use airfoils nick name",
                  obj=self.export_airfoils, prop=Export_Airfoils.use_nick_name,
                  disable=lambda: not self.export_dxf.export_airfoils)
        r += 1
        CheckBox (l,r,1, colSpan=2, text= "Set a common trailing edge thickness of",
                  obj=self.export_airfoils, prop=Export_Airfoils.adapt_te_gap,
                  disable=lambda: not self.export_dxf.export_airfoils)
        FieldF (l,r,4, width=70, unit="mm", step=0.1, lim=(0, 5), dec=1,
                  obj=self.export_airfoils, prop=Export_Airfoils.te_gap_mm,
                  disable=lambda: not self.export_dxf.adapt_te_gap)

        r += 1
        SpaceR (l, r, height=5) 
        l.setColumnMinimumWidth (0,80)
        l.setColumnStretch (5,2)

        return l


    def _select_directory (self):
        """ select directory for export"""

        directory = QFileDialog.getExistingDirectory(self)

        if directory: 
            self.export_dxf.set_export_dir (directory)                    
            self.refresh()


    def _export_airfoils (self, *_):
        """ do export airfoils"""

        self.export_dxf.do_it ()
        
        self.close()

        if self.export_dxf.export_airfoils:
            msg = f"Planform '{self.export_dxf.filename}' and {self.export_dxf.n_airfoils} Airfoils \n\nexported to directory '{self.export_dxf.export_dir}'"
        else: 
            msg = f"Planform '{self.export_dxf.filename}' \n\nexported to directory '{self.export_dxf.export_dir}'"

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

        self._close_btn  = buttonBox.button(QDialogButtonBox.StandardButton.Cancel)

        self._export_btn = QPushButton ("&Export", parent=self)
        self._export_btn.setFixedWidth (80)

        buttonBox.addButton (self._export_btn, QDialogButtonBox.ButtonRole.ActionRole)

        return buttonBox 
    


class Dialog_Edit_Image (Dialog):
    """ 
    Dialog to define image settings for background image
    """

    _width  = 1150
    _height = 670

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
            return self._getter


        def create_diagram_items (self):
            """ create all plot Items and add them to the layout """

            self._plotItem = pg.PlotItem ()

            self._graph_widget.addItem (self._plotItem, 0, 0, rowspan=1, colspan=1)

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
            self._image_artist.refresh ()
            self.setup_viewRange ()


        @override
        def create_view_panel (self):
            """ no view_panel"""
            pass


    # --------------------------------------------------------

    def __init__ (self, *args, **kwargs): 

        self._ok_btn     : QPushButton = None
        self._close_btn  : QPushButton = None 

        self._diagram : self.Diagram_Image = None

        super().__init__ ( *args, **kwargs)

        # move window a little to the side 
        p_center = self._parent.rect().center()               # parent should be main window
        pos_x = p_center.x() - 500
        pos_y = p_center.y() - 100            
        self.move(pos_x, pos_y)

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

    class Item_Planform_Thumbnail (Diagram_Item):
        """ 
        Diagram (Plot) Item for a single wing as Thumbnail
        """

        name        = "Planform"                                # used for link and section header 
        title       = "Wing name"                 
        subtitle    = "dynamic"                                 # will be set dynamically 

        sig_wing_selected         = pyqtSignal(Wing)             # self was clicked 


        def __init__(self, *args, **kwargs):

            super().__init__(*args, **kwargs)

            self.buttonsHidden = True                           # hide resize buttons

            # set margins (inset) of self 
            self.setContentsMargins ( 300,10,0,10)

            # make scene clickable to add wing section 
            #   delayed as during init scene is not yet available
            QTimer().singleShot (10, self._connect_scene_mouseClick)


        def wing (self) -> Wing: 
            return self._getter

        def planform (self) -> Planform:
            return self.wing()._planform


        @override
        def plot_title(self, **kwargs):

            text_with_br = self.wing().description.replace ("\n", "<br/>")      # textItem needs <br>
            super().plot_title (title=self.wing().name, title_size=Artist.SIZE_NORMAL,
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
                self.sig_wing_selected.emit (self.wing())


        @override
        def setup_artists (self):
            """ create and setup the artists of self"""
            
            self._add_artist (Planform_Artist       (self, self.planform, as_contour=True, show_mouse_helper=False))
            self._add_artist (Ref_Line_Artist       (self, self.planform, mode=mode.REF_TO_PLAN, show_mouse_helper=False))
    

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


    class Diagram_Template_List (Diagram):
        """ Diagram with a list of planform Thumbnail items   """

        sig_wing_selected         = pyqtSignal(Wing)             # self was clicked 


        def __init__(self, *args, template_wings : list [Wing] = [], **kwargs):

            self._template_wings     = template_wings

            super().__init__(*args, **kwargs)

            self.graph_layout.setContentsMargins (10,10,10,10)  # default margins
            self.graph_layout.setVerticalSpacing (10)   
            

            if not self.template_wings: 
                self._message_show ("No template files available")


        @property
        def template_wings (self) -> list [Wing]:
            return self._template_wings


        def create_diagram_items (self):
            """ create all plot Items and add them to the layout """

            for i, wing in enumerate (self.template_wings):
                item = Dialog_Select_Template.Item_Planform_Thumbnail (self, getter=wing)
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

        self._diagram : self.Diagram_Template_List = None

        super().__init__ ( *args, **kwargs)

        # move window a little to the side 
        p_center = self._parent.rect().center()               # parent should be main window
        pos_x = p_center.x() - 500
        pos_y = p_center.y() - 100            
        self.move(pos_x, pos_y)

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

        self._template_file_selected = aWing.parm_filePath
        self.close()


    # -------------------------------------------------------------------

    def _init_layout(self) -> QLayout:

        l = QGridLayout()

        # create image diagram 
        self._diagram = self.Diagram_Template_List (self, template_wings=self._template_wings())
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

        self._ok_btn     : QPushButton = None
        self._cancel_btn : QPushButton = None 

        self._new_text  = None

        super().__init__ ( *args, **kwargs)

        # move window a little to the side 
        p_center = self._parent.rect().center()                         # parent should be main window
        pos_x = p_center.x() - 100
        pos_y = p_center.y() + 300           
        self.move(pos_x, pos_y)

        ittle = title if title is not None else self.name
        self.setWindowTitle (f"{title}")
        self._panel.layout().setContentsMargins (QMargins(0, 0, 0, 0))  # no borders in central panel 

        # connect dialog buttons
        self._cancel_btn.clicked.connect  (self.close)
        self._ok_btn.clicked.connect  (self.accept)


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

        buttonBox = QDialogButtonBox (QDialogButtonBox.StandardButton.Ok  | QDialogButtonBox.StandardButton.Cancel)

        self._ok_btn     = buttonBox.button(QDialogButtonBox.StandardButton.Ok)
        self._cancel_btn = buttonBox.button(QDialogButtonBox.StandardButton.Cancel)

        return buttonBox 


    def accept (self):
        """ ok button clicked"""

        self.set_new_text (self._qtextEdit.toPlainText ()) 

        super().accept()
        self.close()

    def reject (self):
        """ cancel button clicked"""
        super().reject()
        self.close()