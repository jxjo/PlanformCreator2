#!/usr/bin/env pythonbutton_color
# -*- coding: utf-8 -*-

"""  

Diagram (items) for airfoil

"""

import logging

from base.widgets           import * 
from base.diagram           import * 

from PyQt6.QtWidgets        import QFileDialog, QGraphicsLayoutItem

# from model.airfoil          import Airfoil

from model.airfoil          import Airfoil
from model.polar_set        import *
from model.xo2_driver       import Worker

from airfoil_ui_panels      import Panel_Airfoils, Panel_Polar_Defs
from airfoil_artists        import Polar_Artist
from airfoil_diagrams       import Diagram_Item_Polars

from pc2_artists            import *
from pc2_dialogs            import Dialog_Edit_Image
from wing                   import Wing, Planform, Planform_Paneled

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)



#-------------------------------------------------------------------------------
# Abstract PC2 diagram   
#-------------------------------------------------------------------------------


class Diagram_Abstract (Diagram):
    """    
    Abstract superclass for all diagrams providing 
        - common main model objects 
        - signals and slots 
    """

    name   = "Abstract"                                     # will be shown in Tabs 

    sig_planform_changed        = pyqtSignal()              # airfoil data changed in a diagram 
    sig_wingSection_new         = pyqtSignal(WingSection)   # new current wing section selected in diagram 
    sig_wingSection_changed     = pyqtSignal()              # current wing section changed in diagram 
    sig_flaps_changed           = pyqtSignal()              # flaps changed in diagram 

    sig_export_airfoils         = pyqtSignal()              # export airfoils button pressed  
    sig_export_xflr5            = pyqtSignal()              # export xflr5 button pressed  
    sig_export_flz              = pyqtSignal()              # export FLZ button pressed  
    sig_launch_flz              = pyqtSignal()              # launch FLZ button pressed  
    sig_export_dxf              = pyqtSignal()              # export dxf button pressed  


    def __init__(self, parent, wing_fn, wingSection_fn : WingSection = None,  **kwargs):

        self._wingSection_fn = wingSection_fn       # bound method to get currrent wing section

        super().__init__(parent, wing_fn, **kwargs)

        self._viewPanel.setMinimumWidth(240)
        self._viewPanel.setMaximumWidth(240)

        # set spacing between the two items
        self.graph_layout.setContentsMargins (20,10,20,10)  # default margins
        self.graph_layout.setVerticalSpacing (20)


    def wing (self) -> Wing: 
        """ currently active wing"""
        return self._getter()

    def planform (self) -> Planform: 
        """ currently active planform"""
        return self.wing().planform


    def cur_wingSection (self) -> WingSection | None: 
        """ returns the current, selected wing section """
        return self._wingSection_fn()


    # --- public slots ---------------------------------------------------


    def on_wing_new (self):
        """ slot to handle new wing signal """
        self.refresh()                                   


    def on_wingSection_selected (self):
        """ slot to handle new current wing section """
        self.refresh(also_viewRange=False)


    def on_wingSection_changed (self):
        """ slot to handle changed wing section data"""
        self.refresh(also_viewRange=False)


    def on_planform_changed (self):
        """ slot to handle new current wing section """
        self.refresh(also_viewRange=False)


    # --- private slots ---------------------------------------------------

    def _on_planform_changed (self):
        """ slot to handle geometry change made in diagram """

        logger.debug (f"{str(self)} on geometry changed in diagram")
    
        self.refresh (also_viewRange=False)         # refresh other diagram items - keep current viewRange
        self.sig_planform_changed.emit()            # refresh app


    def _on_wingSection_changed (self):
        """ slot to handle section changes made in diagram """

        logger.debug (f"{str(self)} on on_wingSection_changed in diagram")
    
        self.refresh (also_viewRange=False)         # refresh other diagram items - keep current viewRange
        self.sig_wingSection_changed.emit()         # refresh app


    def _on_flaps_changed (self):
        """ slot to handle flaps changes made in diagram """

        logger.debug (f"{str(self)} on _on_flaps_changed in diagram")
    
        self.refresh (also_viewRange=False)         # refresh other diagram items - keep current viewRange
        self.sig_flaps_changed.emit()               # refresh app



#-------------------------------------------------------------------------------
# Diagram Items  
#-------------------------------------------------------------------------------


class Item_Planform (Diagram_Item):
    """ 
    Diagram (Plot) Item for Planform
    """

    name        = "View Planform"                           # used for link and section header 
    title       = "Planform"                 
    subtitle    = "dynamic"                                 # will be set dynamically 

    sig_planform_changed        = pyqtSignal()              # planform data changed in a diagram 


    def __init__(self, *args, wingSection_fn = None, **kwargs):

        self._wingSection_fn = wingSection_fn               # bound method to get currrent wing section

        self._show_airfoils     = False
        self._show_strak        = False
        self._show_bounding_box = True

        super().__init__(*args, **kwargs)

        # set margins (inset) of self 
        self.setContentsMargins ( 0,50,0,20)


    def wing (self) -> Wing: 
        return self._getter()

    def planform (self) -> Planform:
        return self.wing()._planform

    @override
    def plot_title(self, **kwargs):

        return super().plot_title(subtitle = self.planform().name, **kwargs)


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (Planform_Artist       (self, self.planform, show_legend=True))
        self._add_artist (Planform_Box_Artist   (self, self.planform))
        self._add_artist (Ref_Line_Artist       (self, self.planform, mode=mode.REF_TO_PLAN, show_legend=True))
        self._add_artist (WingSections_Artist   (self, self.planform, show=False, show_legend=True,
                                                       wingSection_fn=self._wingSection_fn))
        self._add_artist (Flaps_Artist          (self, self.planform, show=False ,show_legend=True))
        self._add_artist (Airfoil_Name_Artist   (self, self.planform, show=False, show_legend=False))
        self._add_artist (Ref_Planforms_Artist  (self, self.planform, show=False, show_legend=True))
        self._add_artist (Image_Artist          (self, self.planform, show=False, as_background=True))
 

    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.autoRange (padding=0.1)                   # first ensure best range x,y 
        self.viewBox.setAspectLocked()
        self.viewBox.invertY(True)
        self.showGrid(x=True, y=True)


    @property
    def show_airfoils (self) -> bool: 
        return self._show_airfoils
    
    def set_show_airfoils (self, aBool : bool): 
        self._show_airfoils = aBool == True
        self._show_artist (Airfoil_Name_Artist, aBool)
        self.section_panel.refresh()                                # enable straked checkbox 


    @property
    def show_strak (self) -> bool: 
        return self._show_strak
    
    def set_show_strak (self, aBool : bool): 
        self._show_strak = aBool == True
        artist : Airfoil_Name_Artist = self._get_artist (Airfoil_Name_Artist) [0]
        artist.set_show_strak (aBool)


    @property
    def show_bounding_box (self) -> bool: 
        return self._show_bounding_box
    
    def set_show_bounding_box (self, aBool : bool): 
        self._show_bounding_box = aBool == True
        self._show_artist (Planform_Box_Artist, aBool)


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:    
            l = QGridLayout()
            r,c = 0, 0
            CheckBox (l,r,c, text="Bounding Box", 
                      get=lambda: self.show_bounding_box, set=self.set_show_bounding_box) 
            r += 1
            CheckBox (l,r,c, text="Airfoils", 
                      get=lambda: self.show_airfoils, set=self.set_show_airfoils) 
            CheckBox (l,r,c+1, text="also straked", 
                        get=lambda: self.show_strak, set=self.set_show_strak,
                        disable=lambda: not self.show_airfoils)    
                     
            self._section_panel = Edit_Panel (title=self.name, layout=l, height=100, 
                                              switchable=True, hide_switched=False, 
                                              switched_on=self._show,
                                              on_switched=self.setVisible)
        return self._section_panel 




class Item_Chord (Diagram_Item):
    """ 
    Diagram (Plot) Item for normed Chord alon span
    """

    name        = "View Chord Distribution"                 # used for link and section header 
    title       = "Chord Distribution"                 
    subtitle    = ""                                 

    sig_planform_changed        = pyqtSignal()              # planform data changed in a diagram 


    def __init__(self, *args, wingSection_fn = None, **kwargs):

        self._wingSection_fn = wingSection_fn               # bound method to get currrent wing section
        super().__init__(*args, **kwargs)

        # set margins (inset) of self 
        self.setContentsMargins ( 0,50,0,20)


    def wing (self) -> Wing: 
        return self._getter()

    def planform (self) -> Planform:
        return self.wing()._planform


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (Norm_Chord_Artist     (self, self.planform, show_legend=True))
        self._add_artist (WingSections_Artist   (self, self.planform, mode=mode.NORM_TO_SPAN, show=False, show_legend=True,
                                                 wingSection_fn=self._wingSection_fn))
        self._add_artist (Ref_Planforms_Artist  (self, self.planform, show_chord=True,
                                                 show_legend=True, show=False))
        self._add_artist (Flaps_Artist          (self, self.planform, mode=mode.NORM_TO_SPAN, show=False, show_legend=True))


    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.05)
        self.viewBox.setYRange( 0, 1.1, padding=0.08)   

        y_axis : pg.AxisItem = self.getAxis ("left")
        y_axis.setLabel (units="%")
        y_axis.setScale (100)
        self.showGrid(x=True, y=True)


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:    
            l = QGridLayout()
            self._section_panel = Edit_Panel (title=self.name, layout=l, height=40, 
                                              switchable=True, hide_switched=False, 
                                              switched_on=self._show,
                                              on_switched=self.setVisible)
        return self._section_panel 




class Item_Chord_Reference (Diagram_Item):
    """ 
    Diagram (Plot) Item for Chord Reference 
    """

    name        = "View Chord Reference"                    # used for link and section header 
    title       = "Chord Reference"                 
    subtitle    = ""                                 

    sig_planform_changed        = pyqtSignal()              # planform data changed in a diagram 


    def __init__(self, *args, wingSection_fn = None, **kwargs):

        self._wingSection_fn = wingSection_fn               # bound method to get currrent wing section
        super().__init__(*args, **kwargs)

        # set margins (inset) of self 
        self.setContentsMargins ( 0,50,0,20)


    def wing (self) -> Wing: 
        return self._getter()

    def planform (self) -> Planform:
        return self.wing()._planform        

    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (Norm_Chord_Ref_Artist (self, self.planform, mode=mode.REF_TO_SPAN, show_legend=True))
        self._add_artist (WingSections_Artist   (self, self.planform, mode=mode.REF_TO_SPAN, show=False, show_legend=True,
                                                       wingSection_fn=self._wingSection_fn))
        self._add_artist (Flaps_Artist          (self, self.planform, mode=mode.REF_TO_SPAN, show=False, show_legend=True))


    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.05)
        self.viewBox.setYRange( -0.1, 1.0, padding=0.08)       # then set y-Range

        y_axis : pg.AxisItem = self.getAxis ("left")
        y_axis.setLabel (units="%")
        y_axis.setScale (100)
        self.viewBox.invertY(True)

        self.showGrid(x=True, y=True)


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:    
            l = QGridLayout()
            self._section_panel = Edit_Panel (title=self.name, layout=l, height=40, 
                                              switchable=True, hide_switched=False, 
                                              switched_on=self._show,
                                              on_switched=self.setVisible)
        return self._section_panel 




class Item_Panelling (Diagram_Item):
    """ 
    Diagram (Plot) Item to show the panelled planform 
    """

    name        = "Panelling"                               # used for link and section header 
    title       = "Paneled Planform"                 
    subtitle    = ""                                 


    def __init__(self, *args, wingSection_fn = None, **kwargs):

        self._wingSection_fn = wingSection_fn               # bound method to get currrent wing section
        super().__init__(*args, **kwargs)

        # set margins (inset) of self 
        self.setContentsMargins ( 0,50,0,20)


    def wing (self) -> Wing: 
        return self._getter()

    def planform (self) -> Planform:
        return self.wing()._planform


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (Panelling_Artist      (self, self.planform, show_legend=True))
        self._add_artist (WingSections_Artist   (self, self.planform, show=True, show_legend=True,
                                                       wingSection_fn=self._wingSection_fn))
        self._add_artist (Airfoil_Name_Artist   (self, self.planform, show_legend=False))
        

    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.autoRange (padding=0.05)                   # first ensure best range x,y 
        self.viewBox.setAspectLocked()
        self.viewBox.invertY(True)

        self.showGrid(x=False, y=False)
        self.showAxis('left', show=False)
        self.showAxis('bottom', show=True)



class Item_Wing (Diagram_Item):
    """ 
    Diagram (Plot) Item for a complete Wing
    """

    name = "View Wing"                                          # used for link and section header 

    def __init__(self, *args,  **kwargs):

        self._show_ref_line     = True                          # show reference line 
        self._show_wingSections = False
        self._show_flaps        = True
        self._show_airfoils     = False

        super().__init__(*args, **kwargs)

        self.buttonsHidden      = True                          # don't show buttons and coordinates


    def wing (self) -> Wing: 
        return self._getter()

    def planform (self) -> Planform:
        return self.wing()._planform

    @override
    def plot_title(self, **kwargs):

        title = f'<span style="font-size: 18pt; color: whitesmoke">{self.wing().name}</span>'
        text_with_br = self.wing().description.replace ("\n", "<br/>")      # textItem needs <br>
        return super().plot_title (title=title, subtitle = text_with_br, **kwargs)


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""

        # plot right and left wing 

        self._add_artist (Planform_Artist       (self, self.planform, mode=mode.WING_RIGHT, as_contour=True))
        self._add_artist (Ref_Line_Artist       (self, self.planform, mode=mode.WING_RIGHT))
        self._add_artist (Flaps_Artist          (self, self.planform, mode=mode.WING_RIGHT))
        self._add_artist (WingSections_Artist   (self, self.planform, mode=mode.WING_RIGHT, show=False))
        self._add_artist (Airfoil_Name_Artist   (self, self.planform, mode=mode.WING_RIGHT, show=False))

        self._add_artist (Planform_Artist       (self, self.planform, mode=mode.WING_LEFT, as_contour=True))
        self._add_artist (Ref_Line_Artist       (self, self.planform, mode=mode.WING_LEFT))
        self._add_artist (Flaps_Artist          (self, self.planform, mode=mode.WING_LEFT))
        self._add_artist (WingSections_Artist   (self, self.planform, mode=mode.WING_LEFT, show=False))

        # switch off mose helper 

        for artist in self._artists: artist.set_show_mouse_helper (False) 


    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.02)
        self.viewBox.setAspectLocked()
        self.viewBox.invertY(True)
        self.viewBox.enableAutoRange()

        self.showGrid(x=False, y=False)
        self.showAxis('left', show=False)
        self.showAxis('bottom', show=True)


    @property
    def show_wingSections (self) -> bool: 
        return self._show_wingSections

    @property
    def show_ref_line (self) -> bool: 
        return self._show_ref_line
    
    @property
    def show_flaps (self) -> bool: 
        return self._show_flaps

    @property
    def show_airfoils (self) -> bool: 
        return self._show_airfoils

    def set_show_wingSections (self, aBool : bool): 
        self._wingSections = aBool == True
        self._show_artist (WingSections_Artist, aBool)
 
    def set_show_ref_line (self, aBool : bool): 
        self._show_ref_line = aBool == True
        self._show_artist (Ref_Line_Artist, aBool)

    def set_show_flaps (self, aBool : bool): 
        self._show_flaps = aBool == True
        self._show_artist (Flaps_Artist, aBool)
    
    def set_show_airfoils (self, aBool : bool): 
        self._show_airfoils = aBool == True
        self._show_artist (Airfoil_Name_Artist, aBool)


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:    
            l = QGridLayout()
            r,c = 0, 0 
            r += 1
            CheckBox (l,r,c, text="Reference Line", 
                        get=lambda: self.show_ref_line,
                        set=self.set_show_ref_line) 
            r += 1
            CheckBox (l,r,c, text="Wing Sections", 
                        get=lambda: self.show_wingSections, set=self.set_show_wingSections) 
            r += 1
            CheckBox (l,r,c, text="Flaps", 
                        get=lambda: self.show_flaps, set=self.set_show_flaps) 
            r += 1
            CheckBox (l,r,c, text="Airfoils", 
                      get=lambda: self.show_airfoils, set=self.set_show_airfoils) 
            r += 1
            l.setColumnStretch (3,2)
            l.setRowStretch    (r,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height=160, 
                                              switchable=True, hide_switched=True, 
                                              on_switched=self.setVisible)

        return self._section_panel 





class Item_Wing_Airfoils (Diagram_Item):
    """ 
    Diagram (Plot) Item for airfoils of a wing
    """

    name        = "View Airfoils"
    title       = "Airfoils"                       # title of diagram item
    subtitle    = ""

    def __init__(self, *args,  **kwargs):
        super().__init__(*args, **kwargs)

        self.buttonsHidden      = True                          # don't show buttons and coordinates


    def wing (self) -> Wing: 
        return self._getter()

    def planform (self) -> Wing: 
        return self.wing()._planform

    @property
    def airfoil_artist (self) -> Airfoil_Artist:
        return self._get_artist (Airfoil_Artist) [0]


    @override
    def refresh(self):
        """ override to set legend cols"""
        super().refresh()
        self.legend.setColumnCount (3)


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""

        self._add_artist (Airfoil_Artist    (self, self.planform, show_legend=True))




    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.02)
        self.viewBox.setAspectLocked()
        self.viewBox.enableAutoRange()

        self.showGrid(x=False, y=False)
        self.showAxis('left', show=False)
        self.showAxis('bottom', show=True)

        self.setContentsMargins ( 10,10,20,20)


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:    
            l = QGridLayout()
            r,c = 0, 0 
            r += 1
            CheckBox (l,r,c, text="Real size", 
                        get=lambda: self.airfoil_artist.real_size,
                        set=self.airfoil_artist.set_real_size) 
            r += 1
            CheckBox (l,r,c, text="Straked Airfoils", 
                        get=lambda: self.airfoil_artist.show_strak,
                        set=self.airfoil_artist.set_show_strak) 
            r += 1
            l.setColumnStretch (3,2)
            l.setRowStretch    (r,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height=130, 
                                              switchable=True, hide_switched=False, 
                                              on_switched=self.setVisible)

        return self._section_panel 





class Item_Wing_Data (Diagram_Item):
    """ 
    Diagram (Plot) Item to show wing data 
    """

    name        = "View Wing data"
    title       = "Wing Data"                                   # title of diagram item
    subtitle    = ""

    def __init__(self, *args,  **kwargs):
        super().__init__(*args, **kwargs)

        self.buttonsHidden      = True                          # don't show buttons and coordinates

        # QTimer().singleShot (10, self._add_text)
        


    def wing (self) -> Wing: 
        return self._getter()

    def planform (self) -> Planform:
        return self.wing()._planform

    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.02)
        self.viewBox.setAspectLocked()
        self.viewBox.enableAutoRange()

        self.showGrid(x=False, y=False)
        self.showAxis('left', show=False)
        self.showAxis('bottom', show=False)

        self.setContentsMargins ( 10,10,20,20)

    @override
    def setup_artists(self):

        self._add_artist (Wing_Data_Artist (self, self.planform, show_legend=True))


    def _add_text(self):
        
        graphicsLayout = pg.GraphicsLayout (self)
        layout : QGraphicsGridLayout = graphicsLayout.layout

        text = Label (layout,0,0, get="Hallo", width=100)
        layoutItem = self.scene ().addWidget (text)
        if isinstance (layoutItem, QGraphicsLayoutItem):
            print ("yepee")
        layout.addItem (layoutItem,0,0,1,1)
        # label : pg.LabelItem = graphicsLayout.addLabel ("hallooooo", color=QColor(Artist.COLOR_LEGEND), size=f"{Artist.SIZE_NORMAL}pt")
        self.addItem (layout)



class Item_Airfoils (Diagram_Item):
    """ 
    Diagram (Plot) Item for airfoils of a planform
    """

    name        = "Airfoils"                                   
    title       = "Airfoils"                       # title of diagram item
    subtitle    = ""

    def __init__(self, *args, **kwargs):

        super().__init__(*args, **kwargs)

        # set margins (inset) of self 
        self.setContentsMargins ( 0,50,0,20)


    def wing (self) -> Wing: 
        return self._getter()

    def planform (self) -> Wing: 
        return self.wing()._planform


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""

        self._add_artist (Airfoil_Artist    (self, self.planform, show_legend=True))


    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.05)
        self.viewBox.autoRange ()                                         # first ensure best range x,y 
        self.viewBox.setAspectLocked()
        self.viewBox.enableAutoRange(axis=pg.ViewBox.XAxis, enable=True)
        self.showGrid(x=True, y=True)


    @property
    def airfoil_artist (self) -> Airfoil_Artist:
        return self._get_artist (Airfoil_Artist) [0]

    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:    
            l = QGridLayout()
            r,c = 0, 0 
            r += 1
            CheckBox (l,r,c, text="In real size", 
                        get=lambda: self.airfoil_artist.real_size,
                        set=self.airfoil_artist.set_real_size) 
            r += 1
            CheckBox (l,r,c, text="Show straked airfoils", 
                        get=lambda: self.airfoil_artist.show_strak,
                        set=self.airfoil_artist.set_show_strak) 
            r += 1
            CheckBox (l,r,c, text="Show maximum thickness", 
                        get=lambda: self.airfoil_artist.show_thick,
                        set=self.airfoil_artist.set_show_thick) 
            r += 1
            l.setColumnStretch (3,2)
            l.setRowStretch    (r,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height=140, 
                                              switchable=False, hide_switched=False, 
                                              on_switched=self.setVisible)
        return self._section_panel 




class Item_Polars (Diagram_Item):
    """ 
    Diagram (Plot) Item for polars 
    """

    name        = "Polar"                               # used for link and section header 
    title       = None 
    subtitle    = None                                  # optional subtitle 


    def __init__(self, *args, iItem= 1, xyVars=None, **kwargs):

        self._iItem  = iItem
        self.set_xyVars (xyVars)                        # polar vars for x,y axis 

        self._title_item2 = None                        # a second 'title' for x-axis 
        self._autoRange_not_set = True                  # to handle initial no polars to autoRange 

        self.name = f"{self.name} {iItem}"

        super().__init__(*args, **kwargs)

        # set margins (inset) of self 
        self.setContentsMargins ( 0,10,10,20)


    @override
    def plot_title (self):
        """ override to have 'title' at x,y axis"""

        # remove existing title item 
        if isinstance (self._title_item, pg.LabelItem):
            self.scene().removeItem (self._title_item)          # was added directly to the scene via setParentItem
        if isinstance (self._title_item2, pg.LabelItem):
            self.scene().removeItem (self._title_item2)         # was added directly to the scene via setParentItem
       
        # y-axis
        p = pg.LabelItem(self.yVar, color=QColor(Artist.COLOR_HEADER), size=f"{Artist.SIZE_HEADER}pt")    

        p.setParentItem(self)                              # add to self (Diagram Item) for absolute position 
        p.anchor(itemPos=(0,0), parentPos=(0,0), offset=(50,5))
        p.setZValue(5)
        self._title_item = p

        # x-axis
        p = pg.LabelItem(self.xVar, color=QColor(Artist.COLOR_HEADER), size=f"{Artist.SIZE_HEADER}pt")    

        p.setParentItem(self)                              # add to self (Diagram Item) for absolute position 
        p.anchor(itemPos=(1.0,1), parentPos=(0.98,1.0), offset=(0,-40))
        p.setZValue(5)
        self._title_item2 = p

    def wing (self) -> Wing: 
        return self._getter()

    def planform (self) -> Wing: 
        return self.wing()._planform

   

    @property
    def xVar (self) -> var:
        return self._xyVars[0]

    def set_xVar (self, varType : var):
        self._xyVars = (varType, self._xyVars[1])

        artist : Polar_Artist = self._artists [0]
        artist.set_xyVars (self._xyVars)

        self.setup_viewRange ()

        self.plot_title()


    @property
    def yVar (self) -> var:
        return self._xyVars[1]
    def set_yVar (self, varType: var):
        self._xyVars = (self._xyVars[0], varType)

        artist : Polar_Artist = self._artists [0]
        artist.set_xyVars (self._xyVars)

        self.setup_viewRange ()

        self.plot_title ()


    def set_xyVars (self, xyVars : list[str]):
        """ set xyVars from a list of var strings or enum var"""

        if isinstance (xyVars[0], str):
            xVar = var(xyVars[0])
        else: 
            xVar = xVar 
        if isinstance (xyVars[1], str):
            yVar = var(xyVars[1])
        else: 
            yVar = xVar 
        self._xyVars = (xVar, yVar)


    @override
    def refresh(self): 
        """ refresh my artits and section panel """

        if self._autoRange_not_set:
            self._viewRange_set = False                     # ensure refresh will setup_viewRange (autoRange)

        super().refresh()

        return


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""

        a = Polar_Artist     (self, self.planform, xyVars=self._xyVars, show_legend=True)
        self._add_artist (a)


    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.05)

        self.viewBox.autoRange ()                           # ensure best range x,y 

        # it could be that there are initially no polars, so autoRange wouldn't set a range, retry at next refresh
        if  self.viewBox.childrenBounds() != [None,None] and self._autoRange_not_set:
            self._autoRange_not_set = False 

        self.viewBox.enableAutoRange(enable=False)

        self.showGrid(x=True, y=True)

        self._set_legend_position ()                         # find nice legend position 


    def _set_legend_position (self):
        """ try to have a good position for legend depending on xyVars"""

        if self.legend is None:
            # normally Artist adds legend  - here to set legend during init 
            self.addLegend(offset=(-10,10),  verSpacing=0 )  
            self.legend.setLabelTextColor (Artist.COLOR_LEGEND)

        if (self.yVar == CL or self.yVar == ALPHA) and self.xVar == CD:
            self.legend.anchor (itemPos=(1,0.5), parentPos=(1,0.5), offset=(-10,0))     # right, middle 

        elif (self.yVar == GLIDE or self.yVar == SINK) and (self.xVar == ALPHA or self.xVar == CL):
            self.legend.anchor (itemPos=(0.2,1), parentPos=(0.5,1), offset=(0,-20))     # middle, bottom

        elif (self.yVar == CL) and (self.xVar == ALPHA):
            self.legend.anchor (itemPos=(0,0), parentPos=(0,0), offset=(40,10))         # left, top

        else:  
            self.legend.anchor (itemPos=(1,0), parentPos=(1,0), offset=(-10,10))        # right, top 

        # reduce vertical spacing 
        l : QGraphicsGridLayout = self.legend.layout
        l.setVerticalSpacing(-5)



#-------------------------------------------------------------------------------
# Diagrams  
#-------------------------------------------------------------------------------


class Diagram_Planform (Diagram_Abstract):
    """    
    Diagram view to show/plot Planform - Container for diagram items 
    """

    name   = "Planform"                                     # will be shown in Tabs 

    def __init__(self, *args,  **kwargs):

        self._general_panel = None                          # panel with general settings  
        self._export_panel  = None                          # panel with export buttons
        self._show_ref_line = True                          # show reference line 
        self._show_ref_planform_elli = True
        self._show_wingSections = False
        self._show_flaps = False

        super().__init__(*args,  **kwargs)

        # set spacing between the two items
        self.graph_layout.setContentsMargins (20,10,20,10)  # default margins
        self.graph_layout.setVerticalSpacing (20)


    @property
    def show_wingSections (self) -> bool: 
        return self._show_wingSections
    
    def set_show_wingSections (self, aBool : bool): 
        self._show_wingSections = aBool == True
        self._show_artist (WingSections_Artist, show=aBool)


    @property
    def show_ref_line (self) -> bool: 
        return self._show_ref_line
    
    def set_show_ref_line (self, aBool : bool): 
        self._show_ref_line = aBool == True
        self._show_artist (Ref_Line_Artist, aBool)


    @property
    def show_elliptical (self) -> bool: 
        """ return show_elliptical state of first artist"""
        artist : Ref_Planforms_Artist = self._get_artist (Ref_Planforms_Artist) [0]
        return artist.show_elliptical

    def set_show_elliptical (self, aBool : bool):
        artist : Ref_Planforms_Artist
        for artist in self._get_artist (Ref_Planforms_Artist): 
            artist.set_show_elliptical (aBool) 


    @property
    def show_ref_pc2 (self) -> bool: 
        """ return show_ref_pc2 state of first artist"""
        artist : Ref_Planforms_Artist = self._get_artist (Ref_Planforms_Artist) [0]
        return artist.show_ref_pc2

    def set_show_ref_pc2 (self, aBool : bool):
        artist : Ref_Planforms_Artist
        for artist in self._get_artist (Ref_Planforms_Artist): 
            artist.set_show_ref_pc2 (aBool) 


    @property
    def show_flaps (self) -> bool: 
        return self._show_flaps
    
    def set_show_flaps (self, aBool : bool): 
        self._show_flaps = aBool == True
        self._show_artist (Flaps_Artist, aBool)


    @property
    def background_image_artist (self) -> Image_Artist:
        return self._get_artist (Image_Artist) [0]

    def set_show_ref_planforms (self, aBool : bool): 
        self._show_artist (Ref_Planforms_Artist, aBool)


    @property
    def show_mouse_helper (self) -> bool:
        """ show mouse helper for all artists default"""
        return Artist.show_mouse_helper_default
    
    def set_show_mouse_helper (self, aBool : bool):
        """ global set show mouse helper default"""
        Artist.show_mouse_helper_default = aBool == True

        for item in self.diagram_items:
            if aBool: 
                item._help_messages_shown = {}                      # reset list of already shown help messages
            else: 
                item._help_messages = {}                            # reset list to show 
                item._on_help_message (None, None)                  # ensure refresh of messages            

        self.refresh ()                                             # 


    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """

        i = Item_Planform (self, getter=self.wing, wingSection_fn = self._wingSection_fn)
        self._add_item (i, 0, 0)

        i = Item_Chord    (self, getter=self.wing, wingSection_fn = self._wingSection_fn, show=False)
        self._add_item (i, 1, 0)
        i.viewBox.setXLink (Item_Planform.name)

        i = Item_Chord_Reference  (self, getter=self.wing, wingSection_fn = self._wingSection_fn, show=False)
        self._add_item (i, 2, 0)
        i.viewBox.setXLink (Item_Planform.name)

        # generic connect to artist changed signals 

        for item in self.diagram_items:
            artist : Abstract_Artist_Planform
            for artist in item._artists:
                artist.sig_planform_changed.connect     (self._on_planform_changed) 
                artist.sig_wingSection_changed.connect  (self._on_wingSection_changed) 
                artist.sig_flaps_changed.connect        (self._on_flaps_changed) 
                artist.sig_wingSection_new.connect      (self.sig_wingSection_new.emit) 
                artist.sig_wingSection_selected.connect (self.sig_wingSection_new.emit) 


    # --- view section panels ---------------------------------------------------

    @override
    def create_view_panel (self):
        """ 
        creates a view panel to the left of at least one diagram item 
        has a section_panel
        """

        # override to add additional gneral settings panel on top 

        super().create_view_panel ()

        self._viewPanel.layout().insertWidget (0, self.general_panel, stretch=0)
        self._viewPanel.layout().addWidget    (self.export_panel, stretch=0)


    @property 
    def general_panel (self) -> Edit_Panel | None:
        """ additional section panel with commmon settings"""

        if self._general_panel is None:

            l = QGridLayout()
            r,c = 0, 0
            CheckBox (l,r,c, text="Show mouse helper", 
                      get=lambda: self.show_mouse_helper, set=self.set_show_mouse_helper) 
            r += 1
            CheckBox (l,r,c, text="Reference Line", 
                    get=lambda: self.show_ref_line, set=self.set_show_ref_line) 
            r += 1
            CheckBox (l,r,c, text="Wing Sections", 
                      get=lambda: self.show_wingSections, set=self.set_show_wingSections) 
            r += 1
            CheckBox (l,r,c, text="Flaps", 
                      get=lambda: self.show_flaps, set=self.set_show_flaps) 

            l.setColumnStretch (0,2)

            self._general_panel = Edit_Panel (title="Common Options", layout=l, height=(60,None),
                                              switchable=False, switched_on=True)
        return self._general_panel 



    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:
        
            l = QGridLayout()
            r,c = 0, 0
            CheckBox   (l,r,c, text="Elliptical", 
                        get=lambda: self.show_elliptical, set=self.set_show_elliptical) 

            # toggle fields for pc2 reference planform 
            r += 1
            CheckBox   (l,r,c, text="Another Planform",  
                        hide = lambda: bool(self.wing().reference_pc2_file))
            Button     (l,r,c+1, colSpan=2, text="Select", width=50, 
                        set=self._open_planform_ref_pc2, toolTip="Select another PC2 Planform as reference",
                        hide = lambda: bool(self.wing().reference_pc2_file))

            CheckBox   (l,r,c, text=lambda: self.wing().planform_ref_pc2_name, 
                        get=lambda: self.show_ref_pc2, set=self.set_show_ref_pc2, 
                        hide = lambda: not bool(self.wing().reference_pc2_file)) 
            ToolButton (l,r,c+2, icon=Icon.OPEN, 
                        set=self._open_planform_ref_pc2, toolTip="Open new Planform",
                        hide = lambda: not bool(self.wing().reference_pc2_file))

            # toggle fields for background image
            r += 1
            CheckBox   (l,r,c, text="Background Image", get=False, 
                        hide = lambda: bool(self.wing().background_image.filename)) 
            Button     (l,r,c+1, text="Select", width=50, colSpan=2,
                        set=self._open_background_image, toolTip="Open background image as reference",
                        hide = lambda: bool(self.wing().background_image.filename))

            CheckBox   (l,r,c, text=lambda: self.wing().background_image.filename,  
                        get=lambda: self.background_image_artist.show, 
                        set=self.background_image_artist.set_show, 
                        hide = lambda: not bool(self.wing().background_image.filename)) 
            ToolButton (l,r,c+1, icon=Icon.EDIT, 
                        set=self._edit_background_image, toolTip="Edit background image settings",
                        hide = lambda: not bool(self.wing().background_image.filename))
            ToolButton (l,r,c+2, icon=Icon.OPEN, 
                        set=self._open_background_image, toolTip="Open new background image",
                        hide = lambda: not bool(self.wing().background_image.filename))

            l.setColumnStretch (0,3)
            l.setColumnStretch (2,1)

            self._section_panel = Edit_Panel (title="Reference Planforms", layout=l, height=140,
                                              switchable=True, hide_switched=True, switched_on=False, 
                                              on_switched=self.set_show_ref_planforms)

        return self._section_panel 


    @property 
    def export_panel (self) -> Edit_Panel | None:
        """ additional section panel with export buttons"""

        if self._export_panel is None:

            l = QGridLayout()
            r,c = 0, 1
            Button      (l,r,c, text="Export Dxf", width=100,
                         set=self.sig_export_dxf.emit)
            r += 1
            SpaceR      (l,r,10,3)

            l.setColumnMinimumWidth (0,10)
            l.setColumnStretch (2,2)

            self._general_panel = Edit_Panel (title="Export", layout=l, height=(60,None),
                                              switchable=False, switched_on=True)
        return self._general_panel 


    def _open_planform_ref_pc2 (self):
        """ open reference pc2 file """

        filters  = "PlanformCreator2 files (*.pc2)"
        newPathFilename, _ = QFileDialog.getOpenFileName(self, filter=filters,
                                                         caption="Open PlanformCreator file")

        if newPathFilename: 
            self.wing().set_reference_pc2_file (newPathFilename)
            self.refresh ()  


    def _open_background_image (self):
        """ open background image file  """

        filters  = "Image files (*.png *.jpg *.bmp)"
        newPathFilename, _ = QFileDialog.getOpenFileName(self, filter=filters, 
                                                         caption="Open background image")

        if newPathFilename: 
            self.wing().background_image.set_pathFilename (newPathFilename)
            self._edit_background_image ()  
            self.background_image_artist.set_show(True)


    def _edit_background_image (self):
        """ edit settings of background image   """

        self.wing().background_image._qimage = None

        dialog = Dialog_Edit_Image (self, self.wing().background_image)  
        dialog.exec()   

        self.refresh()  






class Diagram_Making_Of (Diagram_Abstract):
    """    
    Diagram view to with several diagram items to show, how a planform is build 
    """

    name   = "Welcome"                                        # will be shown in Tabs 

    def __init__(self, *args, **kwargs):

        self._show_wingSections = False
        self._show_flaps = False

        super().__init__(*args, **kwargs)

        self.graph_layout.setContentsMargins (10,30,20,10)     # default margins
        self.graph_layout.setHorizontalSpacing (80)
        self.graph_layout.setVerticalSpacing (20)

        self._viewPanel.setMinimumWidth(240)
        self._viewPanel.setMaximumWidth(240)


    @override
    @property
    def diagram_items (self) -> list['Item_Making_Of_Abstract']:
        """ list of my diagram items """
        return super().diagram_items


    @property
    def show_wingSections (self) -> bool: 
        return self._show_wingSections
    
    def set_show_wingSections (self, aBool : bool): 
        self._show_wingSections = aBool == True
        self._show_artist (WingSections_Artist, show=aBool)


    @property
    def show_flaps (self) -> bool: 
        return self._show_flaps
    
    def set_show_flaps (self, aBool : bool): 
        self._show_flaps = aBool == True
        self._show_artist (Flaps_Artist, aBool)


    @property
    def show_mouse_helper (self) -> bool:
        return Artist.show_mouse_helper_default
    
    def set_show_mouse_helper (self, aBool : bool):
        Artist.show_mouse_helper_default = aBool == True
        self.refresh ()


    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """

        item = Item_Making_Of_Welcome (self, getter=self.planform)
        self._add_item (item, 0, 0)

        item = Item_Making_Of_Chord (self, getter=self.planform)
        self._add_item (item, 0, 1)

        item = Item_Making_Of_Chord_Reference (self, getter=self.planform)
        self._add_item (item, 0, 2)

        item = Item_Making_Of_Planform (self, getter=self.planform)
        self._add_item (item, 1, 0, colspan=2)

        item = Item_Making_Of_Paneled (self, getter=self.planform)
        self._add_item (item, 1, 2)

        self.graph_layout.setColumnStretchFactor (0,1)
        self.graph_layout.setColumnMinimumWidth (0,400)
        
        self.graph_layout.setColumnStretchFactor (1,3)
        self.graph_layout.setColumnStretchFactor (2,3)

         # generic connect to artist changed signals 

        for item in self.diagram_items:
            artist : Abstract_Artist_Planform
            for artist in item._artists:
                artist.sig_planform_changed.connect     (self._on_planform_changed) 
                artist.sig_wingSection_changed.connect  (self._on_wingSection_changed) 
                artist.sig_flaps_changed.connect        (self._on_flaps_changed) 
                artist.sig_wingSection_new.connect      (self.sig_wingSection_new.emit) 
                artist.sig_wingSection_selected.connect (self.sig_wingSection_new.emit) 


    # --- view section panels ---------------------------------------------------

    @property
    def section_panel (self) -> Edit_Panel:
 
        if self._section_panel is None:

            l = QGridLayout()
            r,c = 0, 0
            CheckBox (l,r,c, text="Show mouse helper", 
                      get=lambda: self.show_mouse_helper, set=self.set_show_mouse_helper) 
            r += 1
            Label    (l,r,c, get="Drag the little helper points\nto modifiy the geometry. ",
                      height=60, style=style.COMMENT).setAlignment(Qt.AlignmentFlag.AlignTop)
            r += 1
            CheckBox (l,r,c, text="Wing Sections", 
                      get=lambda: self.show_wingSections, set=self.set_show_wingSections) 
            r += 1
            Label    (l,r,c, get="Show the wing sections\ndefining e.g. airfoils and flaps",
                      height=60, style=style.COMMENT).setAlignment(Qt.AlignmentFlag.AlignTop)
            r += 1
            CheckBox (l,r,c, text="Flaps", 
                      get=lambda: self.show_flaps, set=self.set_show_flaps) 
            r += 1
            Label    (l,r,c, get="Show flaps defined by \nthe hinge line and flap groups.",
                      height=60, style=style.COMMENT).setAlignment(Qt.AlignmentFlag.AlignTop)
            r += 1
            SpaceR   (l,r,2)
            l.setColumnStretch (0,2)

            self._section_panel = Edit_Panel (title="Diagram Options", layout=l, height=(250,None),
                                              switchable=False, switched_on=True)
        return self._section_panel 





class Diagram_Wing (Diagram_Abstract):
    """    
    Diagram view to show/plot Wing overview - Container for diagram items 
    """

    name   = "Wing"                                        # will be shown in Tabs 

    def __init__(self, *args, **kwargs):

        super().__init__(*args, **kwargs)

        # set spacing between the two items

        self.graph_layout.setContentsMargins (20,30,20,10)  # default margins
        self.graph_layout.setVerticalSpacing (50)


    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """

        item = Item_Wing (self, getter=self.wing)
        self._add_item (item, 0, 0, colspan=2)

        item = Item_Wing_Data (self, getter=self.wing)
        self._add_item (item, 1, 0)

        item = Item_Wing_Airfoils (self, getter=self.wing)
        self._add_item (item, 1, 1)

        self.graph_layout.setColumnStretchFactor (0,2)
        self.graph_layout.setColumnStretchFactor (1,3)

        self.graph_layout.setRowStretchFactor (0,4)
        self.graph_layout.setRowStretchFactor (1,3)




class Diagram_Panels (Diagram_Abstract):
    """    
    Diagram show/plot panelling of planform - Container for diagram items 
    """

    name   = "Panelling"                                    # will be shown in Tabs 

    def __init__(self, *args, **kwargs):

        self._airfoil_panel      = None 
        self._export_panel       = None 

        super().__init__(*args, **kwargs)

        # set spacing between the two items
        self.graph_layout.setContentsMargins (20,10,20,10)  # default margins
        self.graph_layout.setVerticalSpacing (50)


    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """

        i = Item_Panelling (self, getter=self.wing, wingSection_fn = self._wingSection_fn)
        self._add_item (i, 0, 0)

        # generic connect to artist changed signals 

        for item in self.diagram_items:
            artist : Abstract_Artist_Planform
            for artist in item._artists:
                artist.sig_wingSection_changed.connect  (self._on_wingSection_changed) 
                artist.sig_wingSection_new.connect      (self.sig_wingSection_new.emit) 
                artist.sig_wingSection_selected.connect (self.sig_wingSection_new.emit) 


    # --- view section panels ---------------------------------------------------

    @override
    def create_view_panel (self):
        """ 
        creates a view panel to the left of at least one diagram item 
        has a section_panel
        """

        # override to add additional gneral settings panel on top 

        super().create_view_panel ()

        layout : QVBoxLayout = self._viewPanel.layout()

        # insert airfoils before the stretch item towards the bottom
        layout.insertWidget (layout.count()-1, self.airfoil_panel)
        layout.addWidget (self.export_panel)


    def planform_paneled (self) -> Planform_Paneled:
        return self.wing().planform_paneled

    @property
    def airfoil_name_artist (self) -> Airfoil_Name_Artist:
        return self._get_artist (Airfoil_Name_Artist) [0]

    def set_show_airfoil_names (self, aBool : bool):
        self.airfoil_name_artist.set_show (aBool)
 

    @property
    def use_nick_name (self) -> bool:
        """ use nick name in diagram and export"""
        return self.wing().planform_paneled.use_nick_name
    def set_use_nick_name (self, aBool : bool):
        self.wing().planform_paneled.set_use_nick_name(aBool == True)
        self.airfoil_name_artist.set_use_nick_name (aBool)


    def _style_width_min (self):
        """ widget style of with_min dependant of with_min was applied"""
        if self.planform_paneled().is_width_min_applied:
            return style.HINT
        else: 
            return style.NORMAL

    def _style_cn_tip_min (self):
        """ widget style of cn_tip_min dependant of cn_tip_min was applied"""
        if self.planform_paneled().is_cn_tip_min_applied:
            return style.HINT
        else: 
            return style.NORMAL


    def _style_cn_diff_max (self):
        """ widget style of cn_diff_max dependant if actual chord difference exceeds value """
        if self.planform_paneled().is_cn_diff_exceeded:
            return style.WARNING
        else: 
            return style.NORMAL

    def _optimize_cn_diff (self):
        """ optimize sections until cn_diff_max is reached"""

        self.planform_paneled().optimize_cn_diff()
        self.refresh()

        self.sig_planform_changed.emit()            # new sections could have been created


    def _on_field_changed (self, *_):
        """ slot for widget changes"""
        self.refresh (also_viewRange=False)         # have 'soft' refresh when settings are changed


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:   

            # do an initial calculation of panels to get the chord difference value 
            self.planform_paneled().c_diff_lines()

            l = QGridLayout()   
            r,c = 0, 0 
            FieldI      (l,r,c, width=70, lab="No of X-Panels", step=1, lim=(4, 50),
                            obj=self.planform_paneled, prop=Planform_Paneled.wx_panels)
            r += 1
            Label       (l,r,c, get="X-Distribution")
            ComboBox    (l,r,c+1, width=70,
                            obj=self.planform_paneled, prop=Planform_Paneled.wx_dist,
                            options=self.planform_paneled().wx_distribution_fns_names)
            r += 1
            SpaceR      (l,r,stretch=0)
            r += 1
            FieldI      (l,r,c, width=70, lab="No of Y-Panels", step=1, lim=(2, 20),
                            obj=self.planform_paneled, prop=Planform_Paneled.wy_panels)
            r += 1
            Label       (l,r,c, get="Y-Distribution")
            ComboBox    (l,r,c+1, width=70,
                            obj=self.planform_paneled, prop=Planform_Paneled.wy_dist,
                            options=self.planform_paneled().wy_distribution_fns_names)

            # optimization settings 

            r += 1
            Label       (l,r,c, height=50, colSpan=3, get="Optimize Panelling", fontSize=size.HEADER_SMALL)            
            r += 1
            FieldF      (l,r,c, width=70, lab="Min Panel width", step=0.5, lim=(0.5, 10), dec=1, unit="%", 
                            obj=self.planform_paneled, prop=Planform_Paneled.width_min,
                            style=self._style_width_min)
            r += 1
            Label       (l,r,c+1, get="X-Panels are reduced", height=20,
                            style=style.COMMENT, hide=lambda: not self.planform_paneled().is_width_min_applied)
            r += 1
            FieldF      (l,r,c, width=70, lab="Min Tip chord", step=1, lim=(1, 50), dec=1, unit="%", 
                            obj=self.planform_paneled, prop=Planform_Paneled.cn_tip_min,
                            style=self._style_cn_tip_min)
            r += 1
            Label       (l,r,c+1, get="Sections are reduced", height=20,
                            style=style.COMMENT, hide=lambda: not self.planform_paneled().is_cn_tip_min_applied)
            r += 1
            FieldF      (l,r,c, width=70, lab="Max Chord diff", step=1, lim=(0.5, 50), dec=0.5, unit="%", 
                            obj=self.planform_paneled, prop=Planform_Paneled.cn_diff_max, 
                            style=self._style_cn_diff_max)
            r += 1
            Label       (l,r,c+1, get="Currently exceeded", height=20,
                            style=style.COMMENT, hide=lambda: not self.planform_paneled().is_cn_diff_exceeded)
            Label       (l,r,c+1, get=lambda: f"Currently {self.planform_paneled().cn_diff:.1%}", height=20,
                            style=style.COMMENT, hide=lambda: self.planform_paneled().is_cn_diff_exceeded)
            r += 1
            Button      (l,r,c+1, text="Optimize", set= self._optimize_cn_diff,
                            hide=lambda: not self.planform_paneled().is_cn_diff_exceeded,
                            toolTip="Optimize panelling by inserting new sections")
            r += 1
            SpaceR      (l,r)
            l.setColumnMinimumWidth (0,80)
            l.setColumnStretch (2,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height =(360,None),  
                                              switchable=False)
            
            w : Widget
            for w in self._section_panel.widgets:
                w.sig_changed.connect (self._on_field_changed)

        return self._section_panel 


    @property
    def airfoil_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._airfoil_panel is None:
        
            l = QGridLayout()
            r,c = 0, 0
            CheckBox   (l,r,c, colSpan=2, text="Use airfoil nick name", 
                        get=self.use_nick_name,
                        set=self.set_use_nick_name) 
            r +=1
            CheckBox   (l,r,c, colSpan=2, text="Show straked airfoils", 
                        get=self.airfoil_name_artist.show_strak,
                        set=self.airfoil_name_artist.set_show_strak,) 
            r +=1
            SpaceR (l,r)
            l.setColumnStretch (2,2)

            self._airfoil_panel = Edit_Panel (title="Airfoils", layout=l, height=100,
                                              switchable=True, hide_switched=True, switched_on=True, 
                                              on_switched=self.set_show_airfoil_names)

        return self._airfoil_panel 


    @property 
    def export_panel (self) -> Edit_Panel | None:
        """ additional section panel with export buttons"""

        if self._export_panel is None:

            l = QGridLayout()
            r,c = 0, 1
            Button      (l,r,c, text="Export Xflr5", width=100, set=self.sig_export_xflr5.emit)
            r += 1
            SpaceR      (l,r,2,0)
            r += 1
            Button      (l,r,c, text="Export FLZ", width=100, set=self.sig_export_flz.emit)
            r += 1
            SpaceR      (l,r,2,0)
            r += 1
            Button      (l,r,c, text="Launch FLZ", width=100, set=self.sig_launch_flz.emit,
                                hide= not os.name == 'nt')                                  # only Windows
            r += 1
            SpaceR      (l,r,10,1)

            l.setColumnMinimumWidth (0,10)
            l.setColumnStretch (2,2)

            self._export_panel = Edit_Panel (title="Export", layout=l, height=(100,None),
                                              switchable=False, switched_on=True)
        return self._export_panel 



class Diagram_Airfoils (Diagram_Abstract):
    """    
    Diagram view to show/plot airfoils - Container for diagram items 
    """

    name   = "Airfoils"                                         # will be shown in Tabs 

    def __init__(self, *args, **kwargs):

        self._export_panel       = None 
        super().__init__(*args, **kwargs)


    @override
    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """
        self._add_item (Item_Airfoils (self, getter=self.wing), 0, 0)


    @override
    def create_view_panel (self):
        """ 
        creates a view panel to the left of at least one diagram item 
        has a section_panel
        """
        super().create_view_panel ()

        self._viewPanel.layout().addWidget (self.export_panel, stretch=0)


    @property 
    def export_panel (self) -> Edit_Panel | None:
        """ additional section panel with export buttons"""

        if self._export_panel is None:

            l = QGridLayout()
            r,c = 0, 1
            Button      (l,r,c, text="Export Airfoils", width=100,
                         set=self.sig_export_airfoils.emit)
            r += 1
            SpaceR      (l,r,10,3)

            l.setColumnMinimumWidth (0,10)
            l.setColumnStretch (2,2)

            self._export_panel = Edit_Panel (title="Export", layout=l, height=(60,None),
                                              switchable=False, switched_on=True)
        return self._export_panel 





class Diagram_Airfoil_Polar (Diagram_Abstract):
    """    
    Diagram view to show/plot airfoil diagrams - Container for diagram items 
    """


    name   = "Airfoils & Polars"                        # will be shown in Tabs 

    sig_polar_def_changed       = pyqtSignal()          # polar definition changed  


    def __init__(self, *args, polar_defs_fn= None, diagram_settings=[], **kwargs):

        self._polar_panel   = None 
        self._polar_defs_fn = polar_defs_fn 
        self._diagram_settings = diagram_settings

        self._show_operating_points = False             # show polars operating points 

        super().__init__(*args, **kwargs)

        self._viewPanel.setMinimumWidth(240)
        self._viewPanel.setMaximumWidth(240)
 
         # set spacing between the two items
        self.graph_layout.setVerticalSpacing (0)

    # --- save --------------------- 

    def _as_dict_list (self) -> list:
        """ returns a list with data dict of the parameters of diagram items """

        l = []
        item : Diagram_Item_Polars
        for item in self._get_items (Diagram_Item_Polars):
            item_dict = {}
            toDict (item_dict, "xyVars", (item.xVar, item.yVar))

            l.append (item_dict)
        return l


    def _get_items (self, item_classes : Type[Diagram_Item] | list[type[Diagram_Item]]) -> list[Diagram_Item]:
        """get Diagram Items of self having class name(s)

        Args:
            items: class or list of class of Diagram Items to retrieve
        Returns:
            List of Item with this classes
        """
        look_for = [item_classes] if not isinstance (item_classes,list) else item_classes
        result = []
        for item in self.diagram_items:
            if item.__class__ in look_for:
                result.append(item)
        return result 


    # -------------


    @property 
    def polar_defs (self) -> list [Polar_Definition]:
        """ actual polar definitions"""
        return self.wing().polar_definitions


    def airfoils (self) -> list[Airfoil]: 
        """ the airfoil(s) off all wing sections"""

        airfoils = []
        section : WingSection

        for section in self.planform().wingSections:

            airfoil = section.airfoil
            if airfoil.isLoaded and not airfoil.isBlendAirfoil:
                airfoils.append (airfoil)
        
        return airfoils


    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """

        r = 0 

        self._add_item (Item_Airfoils (self, getter=self.wing), r, 0, colspan=2)

        if Worker.ready:
            r += 1

            # create Polar items with init values from settings 

            dataDict = self._diagram_settings[0] if len(self._diagram_settings) > 0 else {"xyVars" : (var.CD,var.CL)}
            xyVars = dataDict ["xyVars"]

            item = Diagram_Item_Polars (self, iItem=1, getter=self.airfoils, xyVars=xyVars, show=False)
            self._add_item (item, r, 0)

            dataDict = self._diagram_settings[1] if len(self._diagram_settings) > 1 else {"xyVars" : (var.CL,var.GLIDE)}
            xyVars = dataDict ["xyVars"]

            item = Diagram_Item_Polars (self, iItem=2, getter=self.airfoils, xyVars=xyVars, show=False)
            self._add_item (item, r, 1)
 

    @override
    def create_view_panel (self):
        """ 
        creates a view panel to the left of at least one diagram item 
        has a section_panel
        """
 
        # build side view panel with the section panels 

        layout = QVBoxLayout()
        layout.setContentsMargins (QMargins(0, 0, 0, 0)) 

        # airfoils panel 

        if self.section_panel is not None: 
            layout.addWidget (self.section_panel,stretch=0)

        # diagram items panel

        for item in self.diagram_items:
            if item.section_panel is not None: 
                layout.addWidget (item.section_panel,stretch=0)

        # polar panel

        layout.addWidget (self.polar_panel)
        
        # stretch add end 

        layout.addStretch (1)

        self._viewPanel = Container_Panel()
        self._viewPanel.setMinimumWidth(180)
        self._viewPanel.setMaximumWidth(250)
        self._viewPanel.setLayout (layout)


    @property 
    def show_operating_points (self) -> bool:
        """ show polar operatins points """
        return self._show_operating_points

    def set_show_operating_points (self, aBool : bool):
        self._show_operating_points = aBool

        artist : Polar_Artist
        for artist in self._get_artist (Polar_Artist):
            artist.set_show_points (aBool) 


    @property
    def polar_panel (self) -> Edit_Panel:
        """ return polar extra panel to admin polar definitions and define polar diagrams"""

        if self._polar_panel is None:
        
            l = QGridLayout()
            r,c = 0, 0

            Label (l,r,c, colSpan=4, get="Polar definitions", style=style.COMMENT) 
            r += 1

            # helper panel for polar definitions 

            p = Panel_Polar_Defs (self, self.polar_defs, height=(None,None),)

            p.sig_polar_def_changed.connect (self.sig_polar_def_changed.emit)

            l.addWidget (p, r, c, 1, 6)
            l.setRowStretch (r,1)

            # polar diagrams variables setting 

            r += 1
            if Worker.ready:
                SpaceR (l,r, height=5, stretch=0) 
                r += 1
                Label (l,r,c, colSpan=4, get="Diagram variables", style=style.COMMENT) 
                r += 1
                for item in self._get_items (Diagram_Item_Polars):

                    Label       (l,r,c,   width=20, get="y")
                    ComboBox    (l,r,c+1, width=60, obj=item, prop=Diagram_Item_Polars.yVar, options=var.list)
                    SpaceC      (l,c+2,   width=15, stretch=0)
                    Label       (l,r,c+3, width=20, get="x")
                    ComboBox    (l,r,c+4, width=60, obj=item, prop=Diagram_Item_Polars.xVar, options=var.list)
                    SpaceC      (l,c+5)
                    r += 1

                SpaceR (l,r, height=10, stretch=0) 
                r += 1
                CheckBox (l,r,c, text="Operating points", colSpan=4,
                                get=lambda: self.show_operating_points, set=self.set_show_operating_points) 
                r += 1
                SpaceR (l,r, height=10, stretch=1)
                r += 1
                Label  (l,r,c, colSpan=6, get=f"Powered by Worker {Worker.version} using Xfoil", style=style.COMMENT, fontSize=size.SMALL)

            else: 
                SpaceR (l,r, height=10) 
                r += 1
                Label (l,r,c, colSpan=4, get="No polars available", style=style.ERROR, fontSize=size.HEADER_SMALL) 
                r += 1
                Label (l,r,c, colSpan=4, get="Worker not ready", style=style.ERROR) 
                r += 1
                SpaceR (l,r, height=5, stretch=0) 
                r += 1
                lab = Label (l,r,c, colSpan=6, get=Worker.ready_msg, style=style.COMMENT, height=(None,100)) 
                lab.setWordWrap(True)
                r += 1
                SpaceR (l,r, height=10, stretch=3) 

            self._polar_panel = Edit_Panel (title="View Polars", layout=l, height=(150,None),
                                              switchable=True, switched_on=False, on_switched=self._on_polars_switched)
        return self._polar_panel 


    # --- public slots ---------------------------------------------------



    def on_new_polars (self):
        """ slot to handle new polars loaded which were generated async by Worker """

        logger.debug (f"{str(self)} on new polars")

        for item in self._get_items (Diagram_Item_Polars):
            item.refresh ()


    def on_polar_set_changed (self):
        """ slot to handle changed polar set signal """

        logger.debug (f"{str(self)} on polar set changed")
        self.refresh(also_viewRange=False)


    def on_airfoils_ref_changed (self):
        """ slot to handle new list of reference airfoils"""

        logger.debug (f"{str(self)} on airfoils ref changed")
        self.refresh(also_viewRange=False)


    # --- private slots ---------------------------------------------------



    def _on_polars_switched (self, aBool):
        """ slot to handle polars switched on/off """

        logger.debug (f"{str(self)} on polars switched")

        for item in self._get_items (Diagram_Item_Polars):
            item.setVisible (aBool)





class Item_Making_Of_Abstract (Diagram_Item):
    """ 
   Abstract Making Of Diagram (Plot) Item 
    """

    sig_planform_changed        = pyqtSignal()              

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.setContentsMargins (0,100,40,20)


    def planform (self) -> Wing: 
        return self._getter()

    @override
    def _on_help_message (self, aArtist :Artist, aMessage: str | None):
        """ slot for help message signal of an artist. show it"""

        # no user help messages 
        pass



class Item_Making_Of_Planform (Item_Making_Of_Abstract):
    """ Making Of Diagram (Plot) Item for Planform  """

    title       = "Planform"                 
    subtitle    = "Chord Distribution and Reference are combined to form the shape. The result is scaled by span and chord.<br> " + \
                  "Finally a sweep angle is applied by shearing the planform."                         

    def setup_artists (self):
        self._add_artist (Planform_Artist     (self, self.planform))
        self._add_artist (Ref_Line_Artist     (self, self.planform, mode=mode.REF_TO_PLAN))
        self._add_artist (Planform_Box_Artist (self, self.planform))
        self._add_artist (WingSections_Artist (self, self.planform, show=False))
        self._add_artist (Flaps_Artist        (self, self.planform, show=False))

    @override
    def setup_viewRange (self):
        self.viewBox.setDefaultPadding (0.10)
        self.viewBox.enableAutoRange(enable=True)
        self.viewBox.setAspectLocked()
        self.viewBox.invertY(True)
        self.showGrid(x=False, y=False)

        self.setContentsMargins (0,70,20,20)




class Item_Making_Of_Paneled (Item_Making_Of_Abstract):
    """ Making Of Diagram (Plot) Item for Planform  """

    title       = "Paneled Planform"                 
    subtitle    = "The palnform is idealized by panels, the number of which <br>" + \
                  "is defined in x and y direction. An optimization can be applied."                         

    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (Panelling_Artist      (self, self.planform))
        self._add_artist (WingSections_Artist   (self, self.planform, show=False))

    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.autoRange (padding=0.05)                   # first ensure best range x,y 
        self.viewBox.setAspectLocked()
        self.viewBox.invertY(True)
        self.viewBox.enableAutoRange()        

        self.showGrid(x=False, y=False)
        self.showAxis('left', show=False)
        self.showAxis('bottom', show=True)

        self.setContentsMargins (20,100,40,20)


class Item_Making_Of_Welcome (Item_Making_Of_Abstract):
    """ Making Of Diagram (Plot) Item with Welcome message  """

    title       = ""                                    # has it's own title 
    subtitle    = None

    def __init__(self, *args, **kwargs):

        super().__init__(*args, **kwargs)

        self.buttonsHidden      = True                          # don't show buttons and coordinates

        # set margins (inset) of self 
        self.setContentsMargins ( 0,20,0,0)

        parentPos = (0.0)                               # parent x starts at PlotItem (including axis)       
        itemPos   = (0,0)
        offset    = (50,5)

        p1 = pg.LabelItem(self._welcome_message(), color=QColor(Artist.COLOR_HEADER), size=f"{Artist.SIZE_HEADER}pt")    

        p1.setParentItem(self)                            # add to self (Diagram Item) for absolute position 
        p1.anchor(itemPos=itemPos, parentPos=parentPos, offset=offset)
        self._title_item = p1


    def _welcome_message (self) -> str: 
        # use Notepad++ or https://froala.com/online-html-editor/ to edit 

        message = """
<span style="font-size: 18pt; color: whitesmoke">Welcome to <strong>Planform<span style="color:deeppink">Creator2</span></strong></span>

<span style="font-size: 10pt; color: darkgray">
<p>
    The app lets you design the planform of a wing either just as a draft<br> 
    for further CAD processing or as the input for Xflr5 and FLZ_vortex for <br>
    aerodynamic assessment of a wing using panel based calculation methods.
    <p> 
    The base element of the Planform Creator is the <span style="color: whitesmoke">Chord Distribution</span>, <br>
    which is defined first and which will significantly determine the aerodynamic <br>
    properties of a wing regardless of how the planform is later distorted <br>
    within the PlanformCreaor, this chord distribution is retained.
    </p> 
    <p> 
    This diagram explains the individual steps of how the final planform <br>
    is constructed out the <span style="color: whitesmoke">Chord Distribution</span> and the <span style="color: whitesmoke">Chord Reference</span>.
    </p> 
    <p> 
    Try out the functionality or <em>Open</em>
    an existing PlanformCreator2 file.
    </p> 
    <p> 
    Use the little <span style="color: orange">mouse helper points</span> in the diagrams to play around ...
    </p> 
</p>
<p>
</span>
"""
        return message


    def setup_artists (self):
        pass

    @override
    def setup_viewRange (self):
        self.viewBox.autoRange ()  
        self.viewBox.setXRange( 0, 1, padding=0.08)    
        self.showAxis('left', show=False)
        self.showAxis('bottom', show=False)
        self.showGrid(x=False, y=False)



class Item_Making_Of_Chord (Item_Making_Of_Abstract):
    """ Making Of Diagram (Plot) Item for Chord distribution  """

    title       = "Chord Distribution"                       # title of diagram item
    subtitle    = "Defines the chord along the span in a normalized system.<br>" + \
                  "Chord at root equals to 100%"

    def setup_artists (self):
        self._add_artist (Norm_Chord_Artist     (self, self.planform, mode=mode.NORM_NORM))
        self._add_artist (WingSections_Artist   (self, self.planform, mode=mode.NORM_NORM, show=False))

    @override
    def setup_viewRange (self):
        self.viewBox.autoRange ()                            
        self.viewBox.setXRange (-0.1, 1.1, padding=0.0)       
        self.viewBox.setYRange (  0, 1.1,  padding=0.1)       

        x_axis : pg.AxisItem = self.getAxis ("bottom")
        x_axis.setLabel (units="%")
        x_axis.setScale (100)

        y_axis : pg.AxisItem = self.getAxis ("left")
        y_axis.setLabel (units="%")
        y_axis.setScale (100)
        self.showGrid(x=False, y=False)



class Item_Making_Of_Chord_Reference (Item_Making_Of_Abstract):
    """ Making Of Diagram (Plot) Item for Chord distribution  """

    title       = "Chord Reference"                 
    subtitle    = "Describes how much of the chord is added to the leading<br>" + \
                  "and to the trailing edge in relation to the reference line."                         

    def setup_artists (self):
        self._add_artist (Norm_Chord_Ref_Artist (self, self.planform))
        self._add_artist (WingSections_Artist   (self, self.planform, mode=mode.REF_TO_NORM, show=False))
        self._add_artist (Flaps_Artist          (self, self.planform, mode=mode.REF_TO_NORM, show=False))

    @override
    def setup_viewRange (self):
        self.viewBox.autoRange ()                            
        self.viewBox.setXRange (-0.1, 1.1, padding=0.0)       
        self.viewBox.setYRange (-0.1, 1.0,  padding=0.1)       

        x_axis : pg.AxisItem = self.getAxis ("bottom")
        x_axis.setLabel (units="%")
        x_axis.setScale (100)

        y_axis : pg.AxisItem = self.getAxis ("left")
        y_axis.setLabel (units="%")
        y_axis.setScale (100)
        self.viewBox.invertY(True)
        self.showGrid(x=False, y=False)



class Item_Making_Of_Wing (Item_Making_Of_Abstract):
    """ Making of Diagram (Plot) Item for a complete Wing
    """

    title       = "Wing"                 
    subtitle    = "The half wing planform is mirrored and a fuselage width added."                         

    @override
    def setup_artists (self):
        self._add_artist (Planform_Artist       (self, self.planform, mode=mode.WING_RIGHT, as_contour=True))
        self._add_artist (Ref_Line_Artist       (self, self.planform, mode=mode.WING_RIGHT))
        self._add_artist (Flaps_Artist          (self, self.planform, mode=mode.WING_RIGHT, show=False))
        self._add_artist (WingSections_Artist   (self, self.planform, mode=mode.WING_RIGHT, show=False))

        self._add_artist (Planform_Artist       (self, self.planform, mode=mode.WING_LEFT, as_contour=True))
        self._add_artist (Ref_Line_Artist       (self, self.planform, mode=mode.WING_LEFT))
        self._add_artist (Flaps_Artist          (self, self.planform, mode=mode.WING_LEFT, show=False))
        self._add_artist (WingSections_Artist   (self, self.planform, mode=mode.WING_LEFT, show=False))

        for artist in self._artists: artist.set_show_mouse_helper (False) 

    @override
    def setup_viewRange (self):
        self.viewBox.setDefaultPadding(0.02)
        self.viewBox.setAspectLocked()
        self.viewBox.invertY(True)
        self.viewBox.enableAutoRange()

        self.showGrid(x=False, y=False)
        self.showAxis('left', show=False)
        self.showAxis('bottom', show=True)

        self.setContentsMargins (50,50,20,20)                   # because no left y-axis
