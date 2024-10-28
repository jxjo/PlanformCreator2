#!/usr/bin/env pythonbutton_color
# -*- coding: utf-8 -*-

"""  

Diagram (items) for airfoil

"""

import logging

from base.widgets           import * 
from base.diagram           import * 

from model.airfoil          import Airfoil

from airfoil_artists        import *
from airfoil_widgets        import Airfoil_Select_Open_Widget

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)



#-------------------------------------------------------------------------------
# Diagram   
#-------------------------------------------------------------------------------



class Diagram_Item_Airfoil (Diagram_Item):
    """ 
    Diagram (Plot) Item for airfoils shape 
    """

    name = "View Airfoil"           # used for link and section header 


    sig_geometry_changed         = pyqtSignal()          # airfoil data changed in a diagram 


    def airfoils (self) -> list[Airfoil]: 
        return self._getter()
    
    def _one_is_bezier_based (self) -> bool: 
        """ is one of airfoils Bezier based? """
        a : Airfoil
        for a in self.airfoils():
            if a.isBezierBased: return True
        return False 


    def _on_enter_panelling (self):
        """ slot user started panelling dialog - show panels """

        # switch on show panels , switch off thciknes, camber 
        self.airfoil_artist.set_show_points (True)
        self.line_artist.set_show (False)
        self.section_panel.refresh() 

        logger.debug (f"{str(self)} _on_enter_panelling")


    def _on_blend_airfoil (self):
        """ slot to handle blend airfoil entered"""

        self.line_artist.set_show (False)           # switch off thickness & camber 
        self.section_panel.refresh()

        logger.debug (f"{str(self)} _on_blend_airfoil")


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self.airfoil_artist = Airfoil_Artist   (self, self.airfoils, show_legend=True)

        self.line_artist = Airfoil_Line_Artist (self, self.airfoils, show=False, show_legend=True)
        self.line_artist.sig_geometry_changed.connect (self.sig_geometry_changed.emit)

        self.bezier_artist = Bezier_Artist (self, self.airfoils)
        self.bezier_artist.sig_bezier_changed.connect (self.sig_geometry_changed.emit)


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
        self.airfoil_artist.refresh() 
        self.line_artist.refresh() 

        # show Bezier shape function when current airfoil is Design and Bezier 
        cur_airfoil : Airfoil = self.airfoils()[0]
        if cur_airfoil.isBezierBased and cur_airfoil.usedAsDesign:
            self.bezier_artist.set_show (True)
        else: 
            self.bezier_artist.refresh() 

    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:    
            l = QGridLayout()
            r,c = 0, 0 
            CheckBox (l,r,c, text="Coordinate points", 
                    get=lambda: self.airfoil_artist.show_points,
                    set=self.airfoil_artist.set_show_points) 
            r += 1
            CheckBox (l,r,c, text="Thickness && Camber", 
                    get=lambda: self.line_artist.show,
                    set=self.line_artist.set_show) 
            r += 1
            CheckBox (l,r,c, text="Shape function (Bezier)", 
                    get=lambda: self.bezier_artist.show,
                    set=self.bezier_artist.set_show,
                    disable=lambda : not self._one_is_bezier_based()) 
            r += 1
            l.setColumnStretch (3,2)
            l.setRowStretch    (r,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height=130, 
                                              switchable=True, on_switched=self.setVisible)

        return self._section_panel 


    def set_welcome (self, aText : str):
        """ set a Welcome text into the first artist"""

        self.airfoil_artist.set_welcome (aText)




class Diagram_Item_Curvature (Diagram_Item):
    """ 
    Diagram (Plot) Item for airfoils curvature 
    """

    name = "View Curvature"

    def __init__(self, *args, **kwargs):

        self._link_x  = False 

        super().__init__(*args, **kwargs)


    def airfoils (self) -> list[Airfoil]: 
        return self.data_list()
    

    @override
    def set_show (self, aBool):
        """ switch on/off artists of self when diagram_item is switched on/off"""
        super().set_show (aBool)

        self.curvature_artist.set_show (aBool)



    @property
    def link_x (self) -> bool:
        """ is x axes linked with View Airfoil"""
        return self._link_x
    def set_link_x (self, aBool):
        """ link x axes to View Airfoil"""
        self._link_x = aBool is True
        if self.link_x:
            self.setXLink(Diagram_Item_Airfoil.name)
        else: 
            self.setXLink(None)

    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self.curvature_artist = Curvature_Artist (self, self.airfoils, show_derivative=False, show_legend=True)


    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.05)

        self.viewBox.autoRange ()               # first ensure best range x,y 
        self.viewBox.setXRange( 0, 1)           # then set x-Range
        self.viewBox.setYRange(-2.0, 2.0)

        self.showGrid(x=True, y=True)


    def refresh_artists (self):
        self.curvature_artist.refresh() 


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:            
            l = QGridLayout()
            r,c = 0, 0 
            CheckBox (l,r,c, text="Upper side", 
                    get=lambda: self.curvature_artist.show_upper,
                    set=self.curvature_artist.set_show_upper) 
            r += 1
            CheckBox (l,r,c, text="Lower side", 
                    get=lambda: self.curvature_artist.show_lower,
                    set=self.curvature_artist.set_show_lower) 
            r += 1
            CheckBox (l,r,c, text="Derivative of curvature", 
                    get=lambda: self.curvature_artist.show_derivative,
                    set=self.curvature_artist.set_show_derivative) 
            r += 1
            SpaceR   (l,r)
            r += 1
            CheckBox (l,r,c, text=f"X axes linked to '{Diagram_Item_Airfoil.name}'", 
                    get=lambda: self.link_x, set=self.set_link_x) 
            r += 1
            l.setColumnStretch (3,2)
            l.setRowStretch    (r,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, 
                                              height=160, switchable=True, switched_on=self._show, 
                                              on_switched=self.setVisible)

        return self._section_panel 




class Diagram_Airfoil (Diagram):
    """    
    Diagram view to show/plot airfoil diagrams - Container for diagram items 
    """


    sig_airfoil_changed         = pyqtSignal()          # airfoil data changed in a diagram 
    sig_new_airfoil_ref1        = pyqtSignal(object)    # new ref1 airfoil  
    sig_new_airfoil_ref2        = pyqtSignal(object)    # new ref2 airfoil  


    def __init__(self, *args, welcome=None, **kwargs):

        self._item_airfoil = None                   # the diagram items of self 
        self._item_curvature = None

        self._bezier_match_first_time = True        # switch to show target airfoil 

        super().__init__(*args, **kwargs)

        self._viewPanel.setMinimumWidth(240)
        self._viewPanel.setMaximumWidth(240)

        # set welcome message into the first diagram item 

        self.diagram_items[0].set_welcome (welcome) 
 

    @property
    def airfoil_ref1 (self) -> Airfoil | None:
        """ ref1 airfoil"""
        for airfoil in self.airfoils():
            if airfoil.usedAs == usedAs.REF1: return airfoil
        
    def set_airfoil_ref1 (self, airfoil: Airfoil | None = None): 
        self.sig_new_airfoil_ref1.emit (airfoil)
        self.refresh ()

    @property
    def airfoil_ref2 (self) -> Airfoil | None:
        """ ref2 airfoil"""
        for airfoil in self.airfoils():
            if airfoil.usedAs == usedAs.REF2: return airfoil

    def set_airfoil_ref2 (self, airfoil: Airfoil | None = None): 
        self.sig_new_airfoil_ref2.emit (airfoil)
        self.refresh ()


    @property
    def airfoil_target (self) -> Airfoil | None:
        """ target airfoil"""
        for airfoil in self.airfoils():
            if airfoil.usedAs == usedAs.TARGET: return airfoil


    @property
    def airfoil_org (self) -> Airfoil | None:
        """ original airfoil only if there is a design airfoil"""
        for airfoil in self.airfoils():
            if airfoil.usedAs == usedAs.DESIGN:
                for airfoil in self.airfoils():                
                    if airfoil.usedAs == usedAs.NORMAL: 
                        return airfoil
                return


    @property
    def show_airfoils_ref (self) -> bool: 
        """ is switch show_reference_airfoils on """
        if self._section_panel is not None: 
            return self.section_panel.switched_on
        else: 
            return False
        
    def set_show_airfoils_ref (self, aBool : bool): 
        self.section_panel.set_switched_on (aBool, silent=True)
        self.section_panel.refresh ()
   

    def airfoils (self) -> list[Airfoil]: 
        """ the airfoil(s) currently to show as list"""
        if not self.show_airfoils_ref:
            airfoils = [self.data_list()[0]]
        else: 
            airfoils = self.data_list()
        return airfoils


    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """

        self._item_airfoil = Diagram_Item_Airfoil (self, getter=self.airfoils)
        self._add_item (self._item_airfoil, 0, 0)

        self._item_airfoil.sig_geometry_changed.connect (self._on_geometry_changed)

        self._item_curvature = Diagram_Item_Curvature (self, getter=self.airfoils, show=False)
        self._add_item (self._item_curvature, 1, 0)


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:
        
            l = QGridLayout()
            r,c = 0, 0
            Field (l,r,c, width=175, get=lambda: self.airfoil_org.fileName if self.airfoil_org else '', 
                            disable=True,
                            hide=lambda: (self.airfoil_org is None) or (self.airfoil_org == self.airfoil_target),
                            toolTip="Original airfoil")
            r += 1
            Field (l,r,c, width=175, get=lambda: self.airfoil_target.fileName if self.airfoil_target else '', 
                            disable=True,
                            hide=lambda: self.airfoil_target is None,
                            toolTip="Target airfoil")
            r += 1
            Airfoil_Select_Open_Widget (l,r,c, widthOpen=60,
                            get=lambda: self.airfoil_ref1, set=self.set_airfoil_ref1,
                            initialDir=self.airfoils()[0], addEmpty=True,
                            toolTip="Reference 1 airfoil")
            r += 1
            Airfoil_Select_Open_Widget (l,r,c, widthOpen=60,
                            get=lambda: self.airfoil_ref2, set=self.set_airfoil_ref2,
                            initialDir=self.airfoils()[0], addEmpty=True,
                            hide=lambda: not self.airfoil_ref1 and not self.airfoil_ref2,
                            toolTip="Reference 2 airfoil")
            r += 1
            SpaceR (l,r)
            l.setColumnStretch (0,2)

            self._section_panel = Edit_Panel (title="Reference Airfoils", layout=l, height=(80,None),
                                              switchable=True, switched_on=False, on_switched=self.refresh)

        return self._section_panel 


    # --- public slots ---------------------------------------------------


    def on_airfoil_changed (self):
        """ slot to handle airfoil changed signal """

        logger.debug (f"{str(self)} on airfoil changed")
        self.refresh()


    def on_bezier_mode (self, is_enter):
        """ slot to handle bezier mode entered signal -> show ref airfoil"""

        # ensure to show target airfoil in bezier 
        if is_enter:
            self.set_show_airfoils_ref (True)
            self.refresh()                          # plot ref airfoils 
            logger.debug (f"{str(self)} on_bezier_mode {is_enter}")


    def on_bezier_changed (self, aSide_type: Line.Type):
        """ slot to handle bezier changes (dureing match bezier"""

        # high speed - make direct call to artist
        self._item_airfoil.bezier_artist.refresh_from_side (aSide_type)


    def on_blend_airfoil (self):
        """ slot to handle blend airfoil entered signal -> show org airfoil"""

        self.set_show_airfoils_ref (True)

        self._item_airfoil._on_blend_airfoil ()


        self.refresh()                          # plot ref airfoils 
        logger.debug (f"{str(self)} on_blend_airfoil")


    def on_edit_mode (self):
        """ slot to handle edit mode entered signal"""

        self.section_panel.refresh()                        # to show additional airfoils in edit 
        logger.debug (f"{str(self)} on_edit_mode")


    def on_target_changed (self, refresh=True):
        """ slot to handle airfoil target changed signal """

        logger.debug (f"{str(self)} on airfoil target changed")

        # is there a target airfoil (match Bezier)? switch ref panel on
        if self.airfoil_target:
            self.set_show_airfoils_ref (True)
        
        if refresh: 
            self.refresh()
        elif self.section_panel is not None:                    # refresh just section panel
            self.section_panel.refresh()

    def on_enter_panelling (self):
        """ slot user started panelling dialog - show panels """

        self._item_airfoil._on_enter_panelling ()



    # --- private slots ---------------------------------------------------


    def _on_geometry_changed (self):
        """ slot to handle geometry change made in diagram """

        logger.debug (f"{str(self)} on geometry changed in diagram")
    
        self.refresh()                          # refresh other diagram items 
        self.sig_airfoil_changed.emit()         # refresh app
