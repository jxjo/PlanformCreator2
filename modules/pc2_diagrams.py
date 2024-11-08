#!/usr/bin/env pythonbutton_color
# -*- coding: utf-8 -*-

"""  

Diagram (items) for airfoil

"""

import logging

from base.widgets           import * 
from base.diagram           import * 

# from model.airfoil          import Airfoil

from pc2_artists            import *
from wing                   import Wing, Planform_2

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)



#-------------------------------------------------------------------------------
# Diagram Items  
#-------------------------------------------------------------------------------


class Item_Planform (Diagram_Item):
    """ 
    Diagram (Plot) Item for Planform
    """

    name        = "Planform Diagram"                        # used for link and section header 
    title       = "Planform"                 
    subtitle    = "dynamic"                                 # will be set dynamically 

    sig_planform_changed        = pyqtSignal()              # planform data changed in a diagram 


    def __init__(self, *args, ref_planforms_fn = None, cur_wingSection_fn = None, **kwargs):

        self._ref_planforms_fn = ref_planforms_fn
        self._cur_wingSection_fn = cur_wingSection_fn       # bound method to get currrent wing section

        super().__init__(*args, **kwargs)

        # set margins (inset) of self 
        self.setContentsMargins ( 0,30,0,0)


    def wing (self) -> Wing: 
        return self._getter()

    def planform (self) -> Planform_2:
        return self.wing()._planform_2

    def ref_planforms (self) -> list[Planform]:
        """ list of reference planforms to show"""
        if self._ref_planforms_fn:
            return self._ref_planforms_fn ()
        else: 
            return []

    @override
    def plot_title(self, **kwargs):

        return super().plot_title(subtitle = self.planform()._style, **kwargs)


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (Planform_Artist       (self, self.planform))
        self._add_artist (Ref_Line_Artist       (self, self.planform))
        self._add_artist (Planform_Box_Artist   (self, self.planform))
        self._add_artist (WingSections_Artist   (self, self.planform, show=True, mode=mode.PLANFORM))
        # self.flaps_artist          = Flaps_Artist          (self, self.wing,  show_legend=True)
 

    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.autoRange (padding=0.05)                   # first ensure best range x,y 
        # self.viewBox.setDefaultPadding(0.05)
        self.viewBox.setAspectLocked()
        self.viewBox.setXRange(0, self.planform().span, padding=0.05, update=True)
        self.viewBox.invertY(True)
        self.showGrid(x=True, y=True)


    def set_show_wingSections (self, aBool : bool): 
        for artist in self._get_artist (WingSections_Artist):
            artist.set_show (aBool) 
 
    def set_show_ref_line (self, aBool : bool): 
        for artist in self._get_artist (Ref_Line_Artist):
            artist.set_show (aBool) 


    def set_show_flaps (self, aBool : bool): 
        for artist in self._get_artist (Flaps_Artist):
            artist.set_show (aBool) 


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:    
            l = QGridLayout()
            r,c = 0, 0 
            # CheckBox (l,r,c, text="Reference Line", 
            #         get=lambda: self.planform_artist.show_ref_line,
            #         set=self.planform_artist.set_show_ref_line) 
            r += 1
            l.setColumnStretch (3,2)
            l.setRowStretch    (r,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height=40, 
                                              switchable=True, hide_switched=False, 
                                              switched_on=self._show,
                                              on_switched=self.setVisible)
        return self._section_panel 




class Item_Chord (Diagram_Item):
    """ 
    Diagram (Plot) Item for Chord Distribution
    """

    name        = "Chord Diagram"                           # used for link and section header 
    title       = "Chord Distribution"                 
    subtitle    = ""                                 

    sig_planform_changed        = pyqtSignal()              # planform data changed in a diagram 


    def __init__(self, *args, ref_planforms_fn = None, cur_wingSection_fn = None, **kwargs):

        self._ref_planforms_fn = ref_planforms_fn
        self._cur_wingSection_fn = cur_wingSection_fn       # bound method to get currrent wing section
        super().__init__(*args, **kwargs)

        # set margins (inset) of self 
        self.setContentsMargins ( 0,50,0,0)

    def wing (self) -> Wing: 
        return self._getter()

    def planform (self) -> Planform_2:
        return self.wing()._planform_2

    def ref_planforms (self) -> list[Planform]:
        """ list of reference planforms to show"""
        return self._ref_planforms_fn () if self._ref_planforms_fn else []
        

    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (Norm_Chord_Artist     (self, self.planform, mode=mode.NORM_CHORD,
                                                 show_legend=True))

        # self.chord_ref_line_artist = Norm_Ref_Chord_Artist (self, self.wing, show=False, show_legend=True)
        # self.ref_chord_artist      = Ref_Chord_Artist      (self, self.ref_planforms, show_legend=True)
        # self.wingSections_artist   = WingSections_Artist   (self, self.wing, cur_wingSection_fn=self._cur_wingSection_fn,
        #                                                     norm_chord=True, show_legend=True)
        # self.flaps_artist          = Flaps_Artist          (self, self.wing, rel_depth=True, show_legend=True)



    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.05)

        self.viewBox.autoRange ()                           # first ensure best range x,y 
        self.viewBox.setYRange( 0, 1.0, padding=0.08)       # then set y-Range
        # self.viewBox.invertY(True)

        y_axis : pg.AxisItem = self.getAxis ("left")
        y_axis.setLabel (units="%")
        y_axis.setScale (100)

        self.showGrid(x=True, y=True)


    def set_show_wingSections (self, aBool : bool): 
        for artist in self._get_artist (WingSections_Artist):
            artist.set_show (aBool) 


    def set_show_ref_line (self, aBool : bool): 
        for artist in self._get_artist (Ref_Line_Artist):
            artist.set_show (aBool) 


    def set_show_flaps (self, aBool : bool): 
        for artist in self._get_artist (Flaps_Artist):
            artist.set_show (aBool) 


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:    
            l = QGridLayout()
            r,c = 0, 0 
            r += 1
            l.setRowStretch    (r,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height=40, 
                                              switchable=True, hide_switched=False, 
                                              switched_on=self._show,
                                              on_switched=self.setVisible)
        return self._section_panel 



class Item_Wing (Diagram_Item):
    """ 
    Diagram (Plot) Item for a complete Wing
    """

    name = "Wing Diagram"                                   # used for link and section header 

    def __init__(self, *args,  **kwargs):

        self._show_ref_line     = True                          # show reference line 
        self._show_wingSections = False
        self._show_flaps        = True
        self._artists           = []

        super().__init__(*args, **kwargs)


    def wing (self) -> Wing: 
        return self._getter()


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""

        self._artists = []

        # plot right and left wing 

        for half in [Planform_Artist.RIGHT, Planform_Artist.LEFT]:

            self._add_artist( 
                Planform_Artist         (self, self.wing, wing_half=half, show_mouse_helper=False, 
                                         show_ref_line=True, show_legend=False))
            self._add_artist( 
                Flaps_Artist            (self, self.wing, wing_half=half, show_mouse_helper=False, 
                                         show=self._show_flaps, show_legend=False))
            self._add_artist( 
                WingSections_Artist     (self, self.wing, wing_half=half, show_mouse_helper=False, 
                                         show=self._show_wingSections, show_legend=False))


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


    @override
    def refresh_artists (self):
        """ refresh my artists"""

        artist : Artist
        for artist in self._artists:
            artist.refresh()


    @property
    def show_wingSections (self) -> bool: 
        """ show wing sections in diagrams """
        return self._show_wingSections
    
    def set_show_wingSections (self, aBool : bool): 
        self._show_wingSections = aBool == True
        for artist in self._artists:
            if isinstance (artist, WingSections_Artist):
                artist.set_show (aBool)
 

    @property
    def show_ref_line (self) -> bool: 
        """ show ref line """
        return self._show_ref_line
    
    def set_show_ref_line (self, aBool : bool): 
        self._show_ref_line = aBool == True
        for artist in self._artists:
            if isinstance (artist, Planform_Artist):
                artist.set_show_ref_line (aBool)


    @property
    def show_flaps (self) -> bool: 
        """ show flaps """
        return self._show_flaps
    
    def set_show_flaps (self, aBool : bool): 
        self._show_flaps = aBool == True
        for artist in self._artists:
            if isinstance (artist, Flaps_Artist):
                artist.set_show (aBool)


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
            l.setColumnStretch (3,2)
            l.setRowStretch    (r,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height=None, 
                                              switchable=True, hide_switched=True, 
                                              on_switched=self.setVisible)

        return self._section_panel 


    def set_welcome (self, aText : str):
        """ set a Welcome text into the first artist"""
        pass
        # self.airfoil_artist.set_welcome (aText)





class Item_Wing_Airfoils (Diagram_Item):
    """ 
    Diagram (Plot) Item for airfoils of a wing
    """

    name = "Airfoils"

    def __init__(self, *args,  **kwargs):

        self.airfoil_artist : Planform_Airfoil_Artist = None
        super().__init__(*args, **kwargs)


    def wing (self) -> Wing: 
        return self._getter()


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""

        self.airfoil_artist = Planform_Airfoil_Artist (self, self.wing, show_legend=True, real_size=True, mini_mode=True)


    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.02)
        self.viewBox.setAspectLocked()
        self.viewBox.enableAutoRange()

        self.showGrid(x=False, y=False)
        self.showAxis('left', show=False)
        self.showAxis('bottom', show=True)


    @override
    def refresh_artists (self):
        """ refresh my artists"""

        self.airfoil_artist.refresh()


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

            self._section_panel = Edit_Panel (title=self.name, layout=l, height=None, 
                                              switchable=True, hide_switched=False, 
                                              on_switched=self.setVisible)

        return self._section_panel 





class Item_Airfoils (Diagram_Item):
    """ 
    Diagram (Plot) Item for airfoils of a planform
    """

    name = "Airfoils"                                   

    def __init__(self, *args,  **kwargs):

        self.airfoil_artist : Planform_Airfoil_Artist = None
        super().__init__(*args, **kwargs)


    def wing (self) -> Wing: 
        return self._getter()

    def planform (self) -> Wing: 
        return self.wing()._planform_2


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""

        self.airfoil_artist = Planform_Airfoil_Artist (self, self.planform, show_legend=True)


    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.02)
        self.viewBox.autoRange ()                                         # first ensure best range x,y 
        self.viewBox.setAspectLocked()
        self.viewBox.enableAutoRange(axis=pg.ViewBox.XAxis, enable=True)

        self.showGrid(x=True, y=True)


    @override
    def refresh_artists (self):
        """ refresh my artists"""

        self.airfoil_artist.refresh()


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
            CheckBox (l,r,c, text="Thickness", 
                        get=lambda: self.airfoil_artist.show_thick,
                        set=self.airfoil_artist.set_show_thick) 
            r += 1
            l.setColumnStretch (3,2)
            l.setRowStretch    (r,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height=None, 
                                              switchable=True, hide_switched=False, 
                                              on_switched=self.setVisible)
        return self._section_panel 




#-------------------------------------------------------------------------------
# Diagrams  
#-------------------------------------------------------------------------------


class Diagram_Planform (Diagram):
    """    
    Diagram view to show/plot Planform - Container for diagram items 
    """

    sig_planform_changed        = pyqtSignal()              # airfoil data changed in a diagram 
    sig_wingSection_new         = pyqtSignal(WingSection)   # new current wing section selected in diagram 
    sig_wingSection_changed     = pyqtSignal()              # current wing section changed in diagram 
    sig_flaps_changed           = pyqtSignal()              # flaps changed in diagram 

    def __init__(self, parent, wing_fn, cur_wingSection_fn : WingSection,  **kwargs):

        self._cur_wingSection_fn = cur_wingSection_fn       # bound method to get currrent wing section

        self._general_panel = None                          # panel with general settings  
        self._show_ref_line = True                          # show reference line 
        self._show_ref_planform_elli = True
        self._show_wingSections = True
        self._show_flaps = True

        super().__init__(parent, wing_fn, **kwargs)

        self._viewPanel.setMinimumWidth(240)
        self._viewPanel.setMaximumWidth(240)

        # set spacing between the two items
        self._graph_layout.setContentsMargins (20,10,20,10)  # default margins
        self._graph_layout.setVerticalSpacing (30)



    def wing (self) -> Wing: 
        """ currently active wing"""
        return self._getter()


    def cur_wingSection (self) -> WingSection | None: 
        """ returns the current, selected wing section """
        return self._cur_wingSection_fn()


    @property
    def show_ref_planforms (self) -> bool: 
        """ is switch reference aplanforms on """
        if self._section_panel is not None: 
            return self.section_panel.switched_on
        else: 
            return False


    @property
    def show_wingSections (self) -> bool: 
        """ show wing sections in diagrams """
        return self._show_wingSections
    
    def set_show_wingSections (self, aBool : bool): 
        self._show_wingSections = aBool == True
        for item in self.diagram_items:
            item.set_show_wingSections(aBool) 
 


    @property
    def show_ref_line (self) -> bool: 
        """ show ref line """
        return self._show_ref_line
    
    def set_show_ref_line (self, aBool : bool): 
        self._show_ref_line = aBool == True
        for item in self.diagram_items:
            item.set_show_ref_line(aBool) 


    @property
    def show_flaps (self) -> bool: 
        """ show flaps """
        return self._show_flaps
    
    def set_show_flaps (self, aBool : bool): 
        self._show_flaps = aBool == True
        for item in self.diagram_items:
            item.set_show_flaps(aBool)


    @property
    def show_ref_planform_elli (self) -> bool: 
        """ show ref planform elliptical """
        return self._show_ref_planform_elli
    
    def set_show_ref_planform_elli (self, aBool : bool): 
        self._show_ref_planform_elli = aBool == True
        self.refresh()


    @property
    def show_mouse_helper (self) -> bool:
        """ show mouse helper for all artists"""
        return Artist.show_mouse_helper
    
    def set_show_mouse_helper (self, aBool : bool):
        """ global set show mouse helper"""

        Artist.show_mouse_helper = aBool == True
        self.refresh ()


    def ref_planforms (self) -> list[Planform]:
        """ list of reference planforms to show"""
        refs = []

        if self.show_ref_planforms:
            if self.show_ref_planform_elli:
                refs.append (self.wing().refPlanform_elli)

        return refs


    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """

        i = Item_Planform (self, getter=self.wing, ref_planforms_fn=self.ref_planforms, 
                             cur_wingSection_fn = self._cur_wingSection_fn)
        self._add_item (i, 0, 0)

        i = Item_Chord    (self, getter=self.wing, ref_planforms_fn=self.ref_planforms, 
                            cur_wingSection_fn = self._cur_wingSection_fn, show=False)
        self._add_item (i, 1, 0)

        i.viewBox.setXLink (Item_Planform.name)
        # generic connect to artist changed signals 

        for item in self.diagram_items:
            artist : Artist_Planform
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


    @property 
    def general_panel (self) -> Edit_Panel | None:
        """ additional section panel with commmon settings"""

        if self._general_panel is None:

            l = QGridLayout()
            r,c = 0, 0
            CheckBox (l,r,c, text="Show mouse helper", 
                      get=lambda: self.show_mouse_helper, set=self.set_show_mouse_helper) 
            r += 1
            SpaceR   (l,r,10)
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

            l.setColumnStretch (0,2)

            self._general_panel = Edit_Panel (title="Common", layout=l, height=(60,None),
                                              switchable=False, switched_on=True)
        return self._general_panel 



    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:
        
            l = QGridLayout()
            r,c = 0, 0
            CheckBox (l,r,c, text="Purely elliptical", 
                      get=lambda: self.show_ref_planform_elli, set=self.set_show_ref_planform_elli) 

            r += 1
            SpaceR (l,r)
            l.setColumnStretch (0,2)

            self._section_panel = Edit_Panel (title="Reference Planforms", layout=l, height=80,
                                              switchable=True, hide_switched=False, switched_on=False, 
                                              on_switched=self.refresh)

        return self._section_panel 


    # --- public slots ---------------------------------------------------


    def on_wing_new (self):
        """ slot to handle new wing signal """

        logger.debug (f"{str(self)} on_wing_new")

        self.refresh()                                  # refresh view panels and artists 

        item : Diagram_Item
        for item in self.diagram_items:                 # adapt view range to new wing geometry 
            item.setup_viewRange() 


    def on_cur_wingSection_changed (self):
        """ slot to handle new current wing section """

        logger.debug (f"{str(self)} on_cur_wingSection_changed")
        self.refresh()


    def on_wing_changed (self):
        """ slot to handle airfoil changed signal """

        logger.debug (f"{str(self)} on airfoil changed")
        self.refresh()



    # --- private slots ---------------------------------------------------


    def _on_planform_changed (self):
        """ slot to handle geometry change made in diagram """

        logger.debug (f"{str(self)} on geometry changed in diagram")
    
        self.refresh()                              # refresh other diagram items 
        self.sig_planform_changed.emit()            # refresh app


    def _on_wingSection_changed (self):
        """ slot to handle section changes made in diagram """

        logger.debug (f"{str(self)} on on_wingSection_changed in diagram")
    
        self.refresh()                              # refresh other diagram items 
        self.sig_wingSection_changed.emit()         # refresh app


    def _on_flaps_changed (self):
        """ slot to handle flaps changes made in diagram """

        logger.debug (f"{str(self)} on _on_flaps_changed in diagram")
    
        self.refresh()                              # refresh other diagram items 
        self.sig_flaps_changed.emit()               # refresh app




class Diagram_Making_Of (Diagram):
    """    
    Diagram view to with several diagram items to show, how a planform is build 
    """

    def __init__(self, parent, 
                 planform : Planform_2|None = None, 
                 **kwargs):

        if planform is None: 
            self._planform = Planform_2.default ()           # create demo planform
        else: 
            self._planform = planform

        self._show_ref_line = True                           # show reference line 
        self._show_wingSections = False
        self._show_flaps = False

        # Artist.show_mouse_helper = False

        super().__init__(parent, None, **kwargs)

        self._viewPanel.setMinimumWidth(240)
        self._viewPanel.setMaximumWidth(240)

        self._graph_layout.setContentsMargins (20,30,20,10)  # default margins
        self._graph_layout.setHorizontalSpacing (20)
        self._graph_layout.setVerticalSpacing (20)


    def planform (self) -> Planform_2: 
        return self._planform

    @property
    def show_wingSections (self) -> bool: 
        return self._show_wingSections
    
    def set_show_wingSections (self, aBool : bool): 
        self._show_wingSections = aBool == True
        for item in self.diagram_items:
            item.set_show_wingSections(aBool) 


    @property
    def show_ref_line (self) -> bool: 
        return self._show_ref_line
    
    def set_show_ref_line (self, aBool : bool): 
        self._show_ref_line = aBool == True
        for item in self.diagram_items:
            item.set_show_ref_line(aBool) 


    @property
    def show_flaps (self) -> bool: 
        return self._show_flaps
    
    def set_show_flaps (self, aBool : bool): 
        self._show_flaps = aBool == True
        for item in self.diagram_items:
            item.set_show_flaps(aBool) 


    @property
    def show_mouse_helper (self) -> bool:
        return Artist.show_mouse_helper
    
    def set_show_mouse_helper (self, aBool : bool):
        Artist.show_mouse_helper = aBool == True
        self.refresh ()


    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """

        self._graph_widget.ci.addLabel('Normed System', row=0, col=0, angle=-90,  size=f"{12}pt")
        self._graph_layout.setColumnStretchFactor (0,0)

        item = Item_Making_Of_Chord (self, getter=self.planform)
        self._add_item (item, 0, 1)

        item = Item_Making_Of_Ref_Line (self, getter=self.planform)
        self._add_item (item, 0, 2)

        item = Item_Making_Of_Norm_Planform (self, getter=self.planform)
        self._add_item (item, 0, 3)

        self._graph_widget.ci.addLabel('Wing System', row=1, col=0, angle=-90,  size=f"{12}pt")

        item = Item_Making_Of_Planform (self, getter=self.planform)
        self._add_item (item, 1, 1, colspan=3)


        # generic connect to artist changed signals 
        for item in self.diagram_items:
            artist : Artist_Planform
            for artist in item._artists:
                artist.sig_planform_changed.connect         (self.refresh) 
                artist.sig_wingSection_changed.connect      (self.refresh) 


    # --- view section panels ---------------------------------------------------

    @property
    def section_panel (self) -> Edit_Panel:
 
        if self._section_panel is None:

            l = QGridLayout()
            r,c = 0, 0
            CheckBox (l,r,c, text="Show mouse helper", 
                      get=lambda: self.show_mouse_helper, set=self.set_show_mouse_helper) 
            r += 1
            Label    (l,r,c, get="Drag little helper points with the\n mouse to modifiy the geometry. ",
                      height=60, style=style.COMMENT).setAlignment(Qt.AlignmentFlag.AlignTop)
            r += 1
            CheckBox (l,r,c, text="Reference Line", 
                    get=lambda: self.show_ref_line, set=self.set_show_ref_line) 
            r += 1
            Label    (l,r,c, get="Show or hide the chord reference\n ",
                      height=60, style=style.COMMENT).setAlignment(Qt.AlignmentFlag.AlignTop)
            r += 1
            CheckBox (l,r,c, text="Wing Sections", 
                      get=lambda: self.show_wingSections, set=self.set_show_wingSections) 
            r += 1
            Label    (l,r,c, get="Show or hide the wing sections\nwhere airfoils and flaps are assigned",
                      height=60, style=style.COMMENT).setAlignment(Qt.AlignmentFlag.AlignTop)
            r += 1
            CheckBox (l,r,c, text="Flaps", 
                      get=lambda: self.show_flaps, set=self.set_show_flaps) 
            r += 1
            Label    (l,r,c, get="Show or hide flaps which are\ndefined by the hinge line a flap group.",
                      height=60, style=style.COMMENT).setAlignment(Qt.AlignmentFlag.AlignTop)
            r += 1
            SpaceR   (l,r,2)
            l.setColumnStretch (0,2)

            self._section_panel = Edit_Panel (title="Options", layout=l, height=(250,None),
                                              switchable=False, switched_on=True)
        return self._section_panel 


    # --- public slots ---------------------------------------------------

    # --- private slots ---------------------------------------------------





class Diagram_Wing (Diagram):
    """    
    Diagram view to show/plot Wing overview - Container for diagram items 
    """


    def __init__(self, parent, wing_fn, welcome=None, **kwargs):

        super().__init__(parent, wing_fn, **kwargs)

        self._viewPanel.setMinimumWidth(240)
        self._viewPanel.setMaximumWidth(240)

        # set spacing between the two items
        self._graph_layout.setContentsMargins (20,50,20,10)  # default margins
        self._graph_layout.setVerticalSpacing (50)

        # set welcome message into the first diagram item 

        self.diagram_items[0].set_welcome (welcome) 


    def wing (self) -> Wing: 
        """ currently active wing"""
        return self._getter()


    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """

        item = Item_Wing (self, getter=self.wing)
        self._add_item (item, 0, 0, colspan=2)

        item = Item_Wing_Airfoils (self, getter=self.wing)
        self._add_item (item, 1, 1)

        # connect diagram item artist signals 



    # --- view section panels ---------------------------------------------------


    @property 
    def section_panel (self) -> Edit_Panel | None:
        """ section panel with commmon settings"""

        # if self._section_panel is None:

            # l = QGridLayout()
            # r,c = 0, 0
            # # CheckBox (l,r,c, text="Show mouse helper", 
            # #           get=lambda: self.show_mouse_helper, set=self.set_show_mouse_helper) 
            # r += 1
            # SpaceR   (l,r,10)

            # l.setColumnStretch (0,2)

            # self._section_panel = Edit_Panel (title="Common", layout=l, height=(60,None),
            #                                   switchable=False, switched_on=True)
        return self._section_panel 


    # --- public slots ---------------------------------------------------


    def on_wing_new (self):
        """ slot to handle new wing signal """

        logger.debug (f"{str(self)} on_wing_new")

        self.refresh()                                  # refresh view panels and artists 

        item : Diagram_Item
        for item in self.diagram_items:                 # adapt view range to new wing geometry 
            item.setup_viewRange() 


    def on_wing_changed (self):
        """ slot to handle airfoil changed signal """

        logger.debug (f"{str(self)} on airfoil changed")
        self.refresh()



    # --- private slots ---------------------------------------------------



class Diagram_Airfoils (Diagram):
    """    
    Diagram view to show/plot airfoils - Container for diagram items 
    """

    def __init__(self, parent, wing_fn, **kwargs):

        super().__init__(parent, wing_fn, **kwargs)

        self._viewPanel.setMinimumWidth(240)
        self._viewPanel.setMaximumWidth(240)

        # set spacing between the two items
        self._graph_layout.setContentsMargins (20,50,20,10)  # default margins
        self._graph_layout.setVerticalSpacing (50)


    def wing (self) -> Wing: 
        """ currently active wing"""
        return self._getter()


    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """

        i_p = Item_Airfoils (self, getter=self.wing)
        self._add_item (i_p, 0, 0)


        # connect diagram item artist signals 



    # --- view section panels ---------------------------------------------------


    @property 
    def section_panel (self) -> Edit_Panel | None:
        """ section panel with commmon settings"""

        # if self._section_panel is None:

            # l = QGridLayout()
            # r,c = 0, 0
            # # CheckBox (l,r,c, text="Show mouse helper", 
            # #           get=lambda: self.show_mouse_helper, set=self.set_show_mouse_helper) 
            # r += 1
            # SpaceR   (l,r,10)

            # l.setColumnStretch (0,2)

            # self._section_panel = Edit_Panel (title="Common", layout=l, height=(60,None),
            #                                   switchable=False, switched_on=True)
        return self._section_panel 


    # --- public slots ---------------------------------------------------


    def on_wing_new (self):
        """ slot to handle new wing signal """

        logger.debug (f"{str(self)} on_wing_new")

        self.refresh()                                  # refresh view panels and artists 

        item : Diagram_Item
        for item in self.diagram_items:                 # adapt view range to new wing geometry 
            item.setup_viewRange() 


    def on_wing_changed (self):
        """ slot to handle airfoil changed signal """

        logger.debug (f"{str(self)} on airfoil changed")
        self.refresh()


    # --- private slots ---------------------------------------------------



class Item_Making_Of_Abstract (Diagram_Item):
    """ 
   Abstract Making Of Diagram (Plot) Item 
    """


    sig_planform_changed        = pyqtSignal()              

    def __init__(self, *args, **kwargs):

        super().__init__(*args, **kwargs)

        self.setContentsMargins (40,100,40,20)


    def planform (self) -> Wing: 
        return self._getter()


    def set_show_wingSections (self, aBool : bool): 
        for artist in self._get_artist (WingSections_Artist):
            artist.set_show (aBool) 
 
    def set_show_ref_line (self, aBool : bool): 
        for artist in self._get_artist (Ref_Line_Artist):
            artist.set_show (aBool) 

    # def set_show_flaps (self, aBool : bool): 
    #     for artist in self._artists:
    #         if isinstance (artist, Flaps_Artist):
    #             artist.set_show(aBool) 



class Item_Making_Of_Planform (Item_Making_Of_Abstract):
    """ Making Of Diagram (Plot) Item for Planform  """

    title       = "Planform"                 
    subtitle    = "The 'Norm Planform' is scaled with span and chord. " + \
                  "Finally the sweep angle is apllied by shearing the planform"                         

    def setup_artists (self):
        self._add_artist (Planform_Artist     (self, self.planform))
        self._add_artist (Ref_Line_Artist       (self, self.planform))
        self._add_artist (Planform_Box_Artist   (self, self.planform))
        self._add_artist (WingSections_Artist (self, self.planform, show=False, mode=mode.PLANFORM))

    @override
    def setup_viewRange (self):
        self.viewBox.setDefaultPadding (0.15)
        self.viewBox.enableAutoRange(enable=True)
        self.viewBox.setAspectLocked()
        self.viewBox.invertY(True)
        self.showGrid(x=False, y=False)

        self.setContentsMargins (20,50,20,20)




class Item_Making_Of_Chord (Item_Making_Of_Abstract):
    """ Making Of Diagram (Plot) Item for Chord distribution  """

    title       = "Chord Distribution"                  # title of diagram item
    subtitle    = "Defines the chord along the span in a normalized system.<br>" + \
                  "Chord at root equals to 100%"

    def setup_artists (self):
        self._add_artist (Norm_Chord_Artist     (self, self.planform))
        self._add_artist (WingSections_Artist (self, self.planform, show=False, mode=mode.NORM))

    @override
    def setup_viewRange (self):
        self.viewBox.autoRange ()                            
        self.viewBox.setYRange( 0, 1.1, padding=0.05)       
        self.viewBox.setXRange( -0.04, 1.1, padding=0.0)       

        y_axis : pg.AxisItem = self.getAxis ("left")
        y_axis.setLabel (units="%")
        y_axis.setScale (100)
        self.showGrid(x=False, y=False)



class Item_Making_Of_Ref_Line (Item_Making_Of_Abstract):
    """ Making Of Diagram (Plot) Item for Chord distribution  """

    title       = "Chord Reference"                 
    subtitle    = "Describes how much of the chord is related to the leading<br>" + \
                  "and to the trailing edge in relation to the reference line."                         

    def setup_artists (self):
        self._add_artist (Norm_Ref_Chord_Artist  (self, self.planform))

    @override
    def setup_viewRange (self):
        self.viewBox.autoRange ()                            
        self.viewBox.setXRange( -0.04, 1.1, padding=0.0)       

        self.viewBox.setYRange( -0.1, 1.0, padding=0.05)       
        y_axis : pg.AxisItem = self.getAxis ("left")
        y_axis.setLabel (units="%")
        y_axis.setScale (100)
        self.viewBox.invertY(True)

        self.showGrid(x=False, y=False)



class Item_Making_Of_Norm_Planform (Item_Making_Of_Abstract):
    """ Making Of Diagram (Plot) Item for normed Planform """

    title       = "Normed Planform"                 
    subtitle    = "Putting the chord distribution and the chord reference together<br>" + \
                  "creates the planform in a normalized system."                         

    def setup_artists (self):
        self._add_artist (Norm_Planform_Artist  (self, self.planform))
        self._add_artist (Ref_Line_Artist       (self, self.planform, mode=mode.NORM_PLANFORM))
        self._add_artist (WingSections_Artist (self, self.planform, show=False, mode=mode.NORM_PLANFORM))

    @override
    def setup_viewRange (self):
        self.viewBox.setDefaultPadding(0.05)
        self.viewBox.autoRange ()                            
        self.viewBox.enableAutoRange(enable=True)
        # self.viewBox.setAspectLocked()
        self.showGrid(x=False, y=False)
