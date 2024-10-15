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


class Diagram_Item_Planform (Diagram_Item):
    """ 
    Diagram (Plot) Item for Planform
    """

    name = "View Planform"           # used for link and section header 

    sig_planform_changed         = pyqtSignal()         # planform data changed in a diagram 



    def wing (self) -> Wing: 
        return self._getter()
    

    @override
    def setup_artists (self, initial_show=True):
        """ create and setup the artists of self"""
        
        self.planform_artist = Planform_Artist   (self, self.wing, show=initial_show, show_legend=True)

        # self.line_artist = Airfoil_Line_Artist (self, self.airfoils, show=False, show_legend=True)
        # self.line_artist.sig_geometry_changed.connect (self.sig_geometry_changed.emit)



    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.05)

        self.viewBox.autoRange ()               # first ensure best range x,y 
        self.viewBox.setXRange( 0, 1)           # then set x-Range

        self.viewBox.setAspectLocked()

        self.viewBox.enableAutoRange(axis=pg.ViewBox.YAxis, enable=False)

        self.showGrid(x=True, y=True)


    @override
    def refresh_artists (self):
        pass
        # self.planform_artist.refresh() 


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:    
            l = QGridLayout()
            r,c = 0, 0 
            CheckBox (l,r,c, text="Reference Line", 
                    get=lambda: True) #, set=self.airfoil_artist.set_show_points) 
            r += 1
            l.setColumnStretch (3,2)
            l.setRowStretch    (r,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height=130, 
                                              switchable=True, on_switched=self.setVisible)

        return self._section_panel 


    def set_welcome (self, aText : str):
        """ set a Welcome text into the first artist"""
        pass
        # self.airfoil_artist.set_welcome (aText)



#-------------------------------------------------------------------------------
# Diagram  
#-------------------------------------------------------------------------------


class Diagram_Planform (Diagram):
    """    
    Diagram view to show/plot Planform - Container for diagram items 
    """


    sig_planform_changed         = pyqtSignal()         # airfoil data changed in a diagram 


    def __init__(self, *args, welcome=None, **kwargs):

        self._item_planform = None                   # the diagram items of self 


        super().__init__(*args, **kwargs)

        self._viewPanel.setMinimumWidth(240)
        self._viewPanel.setMaximumWidth(240)

        # set welcome message into the first diagram item 

        self.diagram_items[0].set_welcome (welcome) 
 


    # @property
    # def show_airfoils_ref (self) -> bool: 
    #     """ is switch show_reference_airfoils on """
    #     if self._section_panel is not None: 
    #         return self.section_panel.switched_on
    #     else: 
    #         return False
        
    # def set_show_airfoils_ref (self, aBool : bool): 
    #     self.section_panel.set_switched_on (aBool, silent=True)
    #     self.section_panel.refresh ()
   

    def wing (self) -> Wing: 
        """ currently active wing"""
        return self._getter()


    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """

        self._item_planform = Diagram_Item_Planform (self, getter=self.wing, show=True)
        self._add_item (self._item_planform, 0, 0)

        self._item_planform.sig_planform_changed.connect (self._on_planform_changed)


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:
        
            l = QGridLayout()
            r,c = 0, 0
            r += 1
            SpaceR (l,r)
            l.setColumnStretch (0,2)

            self._section_panel = Edit_Panel (title="Reference Planforms", layout=l, height=(80,None),
                                              switchable=True, switched_on=False, on_switched=self.refresh)

        return self._section_panel 


    # --- public slots ---------------------------------------------------


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
