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
from wing                   import Wing

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)



#-------------------------------------------------------------------------------
# Diagram Items  
#-------------------------------------------------------------------------------


class Item_Planform (Diagram_Item):
    """ 
    Diagram (Plot) Item for Planform
    """

    name = "Planform Diagram"                               # used for link and section header 

    sig_planform_changed        = pyqtSignal()              # planform data changed in a diagram 


    def __init__(self, *args, ref_planforms_fn = None, cur_wingSection_fn = None, **kwargs):

        self._ref_planforms_fn = ref_planforms_fn
        self._cur_wingSection_fn = cur_wingSection_fn       # bound method to get currrent wing section

        self.wingSections_artist = None
        self.planform_artist     = None
        self.ref_planform_artist = None

        super().__init__(*args, **kwargs)


    def wing (self) -> Wing: 
        return self._getter()


    def ref_planforms (self) -> list[Planform]:
        """ list of reference planforms to show"""
        if self._ref_planforms_fn:
            return self._ref_planforms_fn ()
        else: 
            return []


    @override
    def setup_artists (self, initial_show=True):
        """ create and setup the artists of self"""
        
        self.planform_artist = Planform_Artist   (self, self.wing, show=initial_show, 
                                                  show_ref_line=True, show_legend=True)
        self.planform_artist.sig_planform_changed.connect (self.sig_planform_changed.emit)

        self.ref_planform_artist   = Ref_Planform_Artist   (self, self.ref_planforms, show_legend=True)
        self.wingSections_artist   = WingSections_Artist   (self, self.wing, cur_wingSection_fn=self._cur_wingSection_fn,
                                                            norm_chord=False, show=initial_show, show_legend=True)
        self.flaps_artist          = Flaps_Artist          (self, self.wing, norm_chord=False, show=initial_show)


    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.1)

        self.viewBox.autoRange (padding=0.1)                # first ensure best range x,y 
        #a=self.viewBox.viewRange()
        # self.viewBox.setXRange( 0,a[0][1]*1.02, padding=0.02)           # then set x-Range

        self.viewBox.setAspectLocked()

        self.viewBox.invertY(True)
        self.showGrid(x=True, y=True)

        # self.viewBox.enableAutoRange(axis=pg.ViewBox.XAxis, enable=True)


    @override
    def refresh_artists (self):
        """ refresh my artists"""
        self.planform_artist.refresh() 
        self.ref_planform_artist.refresh()
        self.wingSections_artist.refresh()
        self.flaps_artist.refresh()


    def set_show_wingSections (self, aBool : bool): 
        """ switch on/off wing sections"""
        self.wingSections_artist.set_show(aBool) 


    def set_show_ref_line (self, aBool : bool): 
        """ switch on/off reference line"""
        self.planform_artist.set_show_ref_line(aBool) 


    def set_show_flaps (self, aBool : bool): 
        """ switch on/off flaps"""
        self.flaps_artist.set_show(aBool) 


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
                                              on_switched=self.setVisible)

        return self._section_panel 


    def set_welcome (self, aText : str):
        """ set a Welcome text into the first artist"""
        pass
        # self.airfoil_artist.set_welcome (aText)




class Item_Chord (Diagram_Item):
    """ 
    Diagram (Plot) Item for Chord Distribution
    """

    name = "Chord Diagram"                                  # used for link and section header 

    sig_planform_changed        = pyqtSignal()              # planform data changed in a diagram 


    def __init__(self, *args, ref_planforms_fn = None, cur_wingSection_fn = None, **kwargs):

        self._ref_planforms_fn = ref_planforms_fn
        self._cur_wingSection_fn = cur_wingSection_fn       # bound method to get currrent wing section

        self.chord_artist        = None
        self.ref_chord_artist    = None
        self.wingSections_artist = None

        super().__init__(*args, **kwargs)


    def wing (self) -> Wing: 
        return self._getter()


    def ref_planforms (self) -> list[Planform]:
        """ list of reference planforms to show"""

        if self._ref_planforms_fn:
            return self._ref_planforms_fn ()
        else: 
            return []
        

    @override
    def setup_artists (self, initial_show=True):
        """ create and setup the artists of self"""
        
        self.chord_artist     = Chord_Artist          (self, self.wing, show=initial_show, show_legend=True)

        self.chord_ref_line_artist = Chord_Ref_Line_Artist (self, self.wing, show=False, show_legend=True)
        self.ref_chord_artist      = Ref_Chord_Artist      (self, self.ref_planforms, show_legend=True)
        self.wingSections_artist   = WingSections_Artist   (self, self.wing, cur_wingSection_fn=self._cur_wingSection_fn,
                                                            norm_chord=True, show=initial_show, show_legend=True)

        self.chord_artist.sig_planform_changed.connect (self.sig_planform_changed.emit)


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


    @override
    def refresh_artists (self):
        """ refresh my artists"""
        self.chord_artist.refresh() 
        self.chord_ref_line_artist.refresh() 
        self.ref_chord_artist.refresh()
        self.wingSections_artist.refresh()
        # self.flaps_artist.refresh()


    def set_show_wingSections (self, aBool : bool): 
        """ switch on/off wing sections"""
        self.wingSections_artist.set_show(aBool) 


    def set_show_ref_line (self, aBool : bool): 
        """ switch on/off reference line"""
        self.chord_ref_line_artist.set_show (aBool) 

    def set_show_flaps (self, aBool : bool): 
        """ switch on/off flaps"""
        # self.flaps_artist.set_show(aBool) 

    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:    
            l = QGridLayout()
            r,c = 0, 0 
            # CheckBox (l,r,c, text="Reference Line", 
            #         get=lambda: self.chord_ref_line_artist.show,
            #         set=self.chord_ref_line_artist.set_show) 
            r += 1
            l.setColumnStretch (3,2)
            l.setRowStretch    (r,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height=40, 
                                              switchable=True, hide_switched=False, 
                                              on_switched=self.setVisible)

        return self._section_panel 



#-------------------------------------------------------------------------------
# Diagram  
#-------------------------------------------------------------------------------


class Diagram_Planform (Diagram):
    """    
    Diagram view to show/plot Planform - Container for diagram items 
    """


    sig_planform_changed        = pyqtSignal()              # airfoil data changed in a diagram 
    sig_wingSection_new         = pyqtSignal(WingSection)   # new current wing section selected in diagram 
    sig_wingSection_changed     = pyqtSignal()              # current wing section changed in diagram 


    def __init__(self, parent, wing_fn, cur_wingSection_fn : WingSection, welcome=None, **kwargs):

        self._cur_wingSection_fn = cur_wingSection_fn       # bound method to get currrent wing section
        self._item_planform = None                          # the diagram items of self 
        self._item_chord = None                      

        self._general_panel = None                          # panel with general settings  
        self._show_ref_line = True                          # show reference line 
        self._show_ref_planform_elli = True
        self._show_wingSections = True
        self._show_flaps = True

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
        self._item_planform.set_show_wingSections(aBool) 
        self._item_chord.set_show_wingSections(aBool) 
 


    @property
    def show_ref_line (self) -> bool: 
        """ show ref line """
        return self._show_ref_line
    
    def set_show_ref_line (self, aBool : bool): 
        self._show_ref_line = aBool == True
        self._item_planform.set_show_ref_line(aBool) 
        self._item_chord.set_show_ref_line(aBool) 


    @property
    def show_flaps (self) -> bool: 
        """ show flaps """
        return self._show_flaps
    
    def set_show_flaps (self, aBool : bool): 
        self._show_flaps = aBool == True
        self._item_planform.set_show_flaps(aBool) 
        self._item_chord.set_show_flaps(aBool) 


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

        self._item_planform = Item_Planform (self, getter=self.wing, 
                                             ref_planforms_fn=self.ref_planforms, 
                                             cur_wingSection_fn = self._cur_wingSection_fn, show=True)
        self._add_item (self._item_planform, 0, 0)

        self._item_chord    = Item_Chord    (self, getter=self.wing, 
                                             ref_planforms_fn=self.ref_planforms, 
                                             cur_wingSection_fn = self._cur_wingSection_fn, show=True)
        self._add_item (self._item_chord, 1, 0)

        # self._graph_layout.setRowStretchFactor (0,5)
        # self._graph_layout.setRowStretchFactor (1,4)

        # link both items together 
        self._item_chord.setXLink(Item_Planform.name)

        # connect diagram item signals 
        self._item_planform.planform_artist.sig_planform_changed.connect (self._on_planform_changed)

        self._item_planform.wingSections_artist.sig_wingSection_changed.connect (self._on_wingSection_changed)
        self._item_planform.wingSections_artist.sig_wingSection_new.connect (self.sig_wingSection_new.emit)
        self._item_planform.wingSections_artist.sig_cur_wingSection.connect (self.sig_wingSection_new.emit)

        self._item_chord.chord_artist.sig_planform_changed.connect (self._on_planform_changed)

        self._item_chord.wingSections_artist.sig_wingSection_changed.connect (self._on_wingSection_changed)
        self._item_chord.wingSections_artist.sig_cur_wingSection.connect (self.sig_wingSection_new.emit)
        self._item_chord.wingSections_artist.sig_wingSection_new.connect (self.sig_wingSection_new.emit)


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
