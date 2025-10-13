#!/usr/bin/env pythonbutton_color
# -*- coding: utf-8 -*-

"""  

Diagram (items) for airfoil

"""

import logging

from PyQt6.QtWidgets        import QFileDialog, QGraphicsLayoutItem

from base.widgets         import * 
from base.diagram         import * 
from base.panels          import Container_Panel, MessageBox
from airfoil_diagrams     import Panel_Polar_Defs

from model.airfoil        import Airfoil
from model.polar_set      import *
from model.xo2_driver     import Worker

from wing                   import Wing, Planform, Planform_Paneled
from VLM_wing               import VLM_OpPoint, OpPoint_Var

from pc2_artists            import *
from pc2_dialogs            import (Dialog_Edit_Image, Dialog_Edit_Paneling, Dialog_Export_Xflr5,
                                    Dialog_Export_FLZ, Dialog_Export_Airfoil, Dialog_Export_Dxf)

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
    sig_wingSection_new         = pyqtSignal(WingSection)   # new wing section inserted in diagram 
    sig_wingSection_changed     = pyqtSignal()              # current wing section changed in diagram 
    sig_wingSection_selected    = pyqtSignal(WingSection)   # another wing section selected in diagram 

    sig_flaps_changed           = pyqtSignal()              # flaps changed in diagram 
    sig_polar_def_changed       = pyqtSignal()              # polar definition changed  
    sig_panel_def_changed       = pyqtSignal()              # paneling definition changed


    def __init__(self, parent, wing_fn, wingSection_fn : WingSection = None,  **kwargs):

        self._wingSection_fn = wingSection_fn       # bound method to get currrent wing section

        super().__init__(parent, wing_fn, **kwargs)

        self._viewPanel.setMinimumWidth(250)
        self._viewPanel.setMaximumWidth(250)

        # set spacing between the two items
        self.graph_layout.setContentsMargins (20,10,20,10)  # default margins
        self.graph_layout.setVerticalSpacing (10)


    def _as_dict_list (self) -> list:
        """ returns a list with data dict of the parameters of self"""

        # to override 
        return None


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


    def on_polar_set_changed (self):
        """ slot to handle changed polar definitions """
        # will behandled in subclass which has polars 
        pass

    def on_paneling_changed (self):
        """ slot to handle changed polar definitions """
        # will behandled in subclass which has polars 
        pass

    @override
    def on_new_polars (self):
        """ slot to handle new polars loaded which were generated async by Worker """
        # will behandled in subclass which has polars 
        pass

    # --- common functions ---------------------------------------------------

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

        self.refresh ()        

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


    def __init__(self, *args, wingSection_fn = None, **kwargs):

        self._wingSection_fn = wingSection_fn               # bound method to get currrent wing section

        super().__init__(*args, **kwargs)
  
        self.setContentsMargins ( 0,50,0,20)                # set margins (inset) of self 


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

        self.viewBox.autoRange (padding=0.08)

        self.viewBox.setAspectLocked()
        self.viewBox.invertY(True)
        self.showGrid(x=True, y=True)


    @property
    def airfoil_name_artist (self) -> Airfoil_Name_Artist:
        return self._get_artist (Airfoil_Name_Artist) [0]


    @property
    def show_airfoils (self) -> bool: 
        return self.airfoil_name_artist.show
    
    def set_show_airfoils (self, aBool : bool): 
        self.airfoil_name_artist.set_show (aBool)        
        self.section_panel.refresh()                                # enable blended checkbox 
        self.setup_viewRange()                                      # ensure airfoil names fit in current view 


    @property
    def show_strak (self) -> bool: 
        return self.airfoil_name_artist.show_strak
    
    def set_show_strak (self, aBool : bool): 
        self.airfoil_name_artist.set_show_strak (aBool)


    @property
    def show_bounding_box (self) -> bool: 
        return self._get_artist (Planform_Box_Artist) [0].show
    
    def set_show_bounding_box (self, aBool : bool): 
        self._show_artist (Planform_Box_Artist, aBool)


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:    
            l = QGridLayout()
            r,c = 0, 0
            CheckBox (l,r,c, text="Bounding Box", colSpan=2,
                      get=lambda: self.show_bounding_box, set=self.set_show_bounding_box)
            r += 1
            CheckBox (l,r,c, text="Airfoils", 
                      get=lambda: self.show_airfoils, set=self.set_show_airfoils)
            CheckBox (l,r,c+1, text="Use nick name", colSpan=3,
                        get=lambda: self.airfoil_name_artist.use_nick_name,
                        set=self.airfoil_name_artist.set_use_nick_name,
                        hide=lambda: not self.show_airfoils,
                        toolTip="Airfoils nick name is defined in diagram 'Airfoils'")
            r += 1
            CheckBox (l,r,c+1, text="Also blended", 
                        get=lambda: self.show_strak, set=self.set_show_strak,
                        hide=lambda: not self.show_airfoils)    

            l.setColumnMinimumWidth (0,70)
            l.setColumnStretch (3,5)
            l.setRowStretch    (r+1,2)
                     
            self._section_panel = Edit_Panel (title=self.name, layout=l, height=125, 
                                              switchable=True, hide_switched=True, 
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

        logger.debug (f"{self} setup viewRange")


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




class Item_VLM_Panels (Diagram_Item):
    """ 
    Diagram (Plot) Item to show the VLM panels of the planform  
    """

    name        = "View Paneling"                           # used for link and section header 
    title       = "Paneling"                 
    subtitle    = ""                                 


    def __init__(self, *args, wingSection_fn = None, opPoint_fn = None, **kwargs):

        self._wingSection_fn = wingSection_fn               # bound method to get currrent wing section
        self._opPoint_fn = opPoint_fn                       # bound method to get currrent opPoint

        super().__init__(*args, **kwargs)

        # set margins (inset) of self - there is an extra colum for color bar!
        self.setContentsMargins ( 0,30,10,10)



    def wing (self) -> Wing: 
        return self._getter()

    def planform (self) -> Planform:
        return self.wing()._planform
    
    def opPoint (self) -> VLM_OpPoint:
        return self._opPoint_fn()


    @override
    def plot_title(self, **kwargs):

        n_panels = self.wing().vlm_wing.n_panels
        return super().plot_title(subtitle=f"{n_panels} panels", **kwargs)
    

    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (VLM_Panels_Artist     (self, self.planform, opPoint_fn=self.opPoint, show_legend=True))
        self._add_artist (WingSections_Artist   (self, self.planform, show=True, show_legend=True,
                                                       show_all_sections=True,
                                                       wingSection_fn=self._wingSection_fn))  
        self._add_artist (Airfoil_Name_Artist   (self, self.planform, show=False, show_legend=False))


    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.layout.setColumnMinimumWidth ( 3, 50)              # ensure width for color keeps reserved

        self.viewBox.autoRange (padding=0.08)

        self.viewBox.setAspectLocked()
        self.viewBox.invertY(True)
        self.showGrid(x=True, y=True)

        logger.debug (f"{self} setup viewRange")


    @property 
    def show_cp_in_panels (self) -> bool:
        """ show colored Cp value in panels """
        artists= self._get_artist (VLM_Panels_Artist)
        if artists:
            artist : VLM_Panels_Artist = artists[0]
            return artist.show_cp_in_panels 
        else:
            return False
    
    def set_show_cp_in_panels (self, aBool: bool):
        artists= self._get_artist (VLM_Panels_Artist)
        if artists:
            artist : VLM_Panels_Artist = artists[0]
            artist.set_show_cp_in_panels (aBool)


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:   

            l = QGridLayout()   
            r,c = 0, 0 
            Button     (l,r,c, text="Define Paneling", width=100, colSpan=3,
                        set=self._edit_paneling, toolTip="Define / Edit paneling options")
            r +=1
            SpaceR   (l,r, height=5)
            r += 1
            CheckBox (l,r,c, text="Show Cp in panels", colSpan=3,
                        obj=self, prop=Item_VLM_Panels.show_cp_in_panels,
                        disable=lambda: self.opPoint() is None)
            l.setRowStretch (r+1,3)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height =100,
                                              switched_on=self._show,  
                                              switchable=True, on_switched=self.setVisible)
            
        return self._section_panel 


    def _edit_paneling (self):
        """ dialog to edit paneling paramters  """

        myParent : Diagram_Abstract = self._parent

        # show difference between original and paneled planform before dialog
        panels_artist : VLM_Panels_Artist = self._get_artist(VLM_Panels_Artist)[0]
        panels_artist.set_show_chord_diff (True)

        dialog = Dialog_Edit_Paneling (self.section_panel, self.wing().planform_paneled, dx=50, dy=-50)  

        dialog.sig_paneling_changed.connect (myParent.sig_panel_def_changed.emit)

        # refresh will be done via signal from app

        dialog.exec()   

        panels_artist.set_show_chord_diff (False)



class Item_VLM_Result (Diagram_Item):
    """ 
    Diagram (Plot) Item to show result of aero calculation
    """

    name        = "View Aero Analysis"                      # used for link and section header 
    title       = ""                                        # will be dynamically set                 
    subtitle    = ""                                 

    sig_opPoint_changed       = pyqtSignal()                # polar definition changed  


    def __init__(self, *args, wingSection_fn = None, opPoint_fn = None, **kwargs):

        self._wingSection_fn    = wingSection_fn            # bound method to get currrent wing section

        self._opPoint_fn        = opPoint_fn                # bound method to get currrent opPoint
        self._opPoint_var       = OpPoint_Var.CL
        self._vtas              = None                      # true air speed
        self._opPoint_alpha     = None
        self._polar_def         = None                      # current polar definition of wingSection at root
        self._opPoint_fixed_to_alpha_max = False            # opPoint is fixed at alpha_max 

        super().__init__(*args, **kwargs)

        self.setContentsMargins ( 0,30,50,10)


    def wing (self) -> Wing: 
        return self._getter()


    def planform (self) -> Planform:
        return self.wing()._planform


    def polar (self) -> VLM_Polar:
        """ current VLM polar """
        return self.wing().vlm_wing.polar_at (self.vtas)


    def opPoint (self) -> VLM_OpPoint:
        """ current selected opPoint based on vta and aplha"""
        return self.polar().opPoint_at (self.opPoint_alpha) if self.isVisible() and self.polar()else None


    @override
    def plot_title(self, **kwargs):

        opPoint_label = self.opPoint().name if self.opPoint() else ""
        return super().plot_title(title = f"{self.opPoint_var} along Span", subtitle=opPoint_label, **kwargs)
    

    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (VLM_Result_Artist     (self, self.planform, show_legend=True,
                                                        polar_fn=self.polar,
                                                        opPoint_fn=self.opPoint))

    @override
    def refresh (self):

        if self.isVisible(): 
            
            # ensure current polar def is in existing polar definitions
            if self.polar_def is None or self.polar_def not in self.polar_definitions:
                self._polar_def = self.polar_definitions[0] if self.polar_definitions else None
                self.sig_opPoint_changed.emit()
            super().refresh()


    @override
    def setVisible (self, aBool):
        """ Qt overloaded to signal parent """

        # overridden to check for Worker 
        super().setVisible (aBool and Worker.ready)


    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self._set_yRange ()

        self.showGrid(x=True, y=True)
        self.showAxis('left', show=True)
        self.showAxis('bottom', show=True)


    def _set_yRange (self): 
        """ set y range depending on var """

        # get y data range and adjust new  
        rect = self.viewBox.childrenBoundingRect()
        max_val, min_val = rect.top(), rect.bottom() 
        max_val, min_val = (max_val, min_val) if max_val > min_val else (min_val, max_val)
        max_val, min_val = round(max_val,2), round(min_val,2)

        if self.opPoint_var == OpPoint_Var.ALPHA:
            range_max = 10 if max_val < 10 else (int (max_val/5) + 1) * 5
            range_min = 0  if min_val >= 0 else (int (min_val/5) - 1) * 5
        elif self.opPoint_var == OpPoint_Var.CL:
            range_max = 1  if max_val < 1  else (int (max_val/0.5) + 1) * 0.5
            range_min = 0  if min_val >= 0 else (int (min_val/0.5) - 1) * 0.5
        else:
            range_min = 0.0 if min_val > 0 else min_val
            range_max = max_val * 1.2 if max_val > 0 else 0

        # logger.debug (f"{self} set viewBox yRange {range_min} {range_max}")
        self.viewBox.setYRange (range_min, range_max)


    @property
    def vtas (self) -> float:
        """ true air speed"""
        chord = self.wing().planform.chord_root / 1000
        vtas  = v_from_re (self.polar_def.re, chord) if self.polar_def is not None else 0
        return vtas


    def _format_polar_def (self, polar_def : Polar_Definition) -> str:
        """ format a polar definition for combobox like '400k N7 | 14,6m/s'"""
        return polar_def.name_with_v (self.wing().planform.chord_root)

    @property
    def polar_definitions (self) -> list[Polar_Definition]:
        """ polar definitions of wing - only Polar Type 1 is supported"""
        return [polar_def for polar_def in self.wing().polar_definitions if polar_def.type == polarType.T1]


    @property
    def polar_def_list (self) -> list[str]:
        """ polar definitions as list of formatted strings"""
        return [self._format_polar_def (polar_def) for polar_def in self.wing().polar_definitions]


    @property
    def polar_def (self) -> Polar_Definition:
        """ polar definition of wingSection at root"""

        if self._polar_def is None:

             if self.polar_definitions:
                self._polar_def = self.polar_definitions[0] 

        return self._polar_def

    def set_polar_def (self, polar_def : Polar_Definition):
        """ set polar definition of wingSection at root"""

        if polar_def in self.polar_definitions:
            self._polar_def = polar_def

            # set to new max of this polar 
            self.set_opPoint_fixed_to_alpha_max (self.opPoint_fixed_to_alpha_max, refresh=False)
            self.refresh()


    @property
    def polar_def_name (self) -> str:
        """ display name of the current polar definition """
      
        return self._format_polar_def (self.polar_def) if self.polar_def is not None else None
    
        
    def set_polar_def_name (self, new_polar_def_name): 
        """ set polar definition of wingSection at root by name"""
        for polar_def in self.polar_definitions:
            polar_def_name = self._format_polar_def (polar_def)
            if polar_def_name == new_polar_def_name: 
                self.set_polar_def (polar_def)


    @property
    def opPoint_alpha (self) -> float:
        """ true air speed"""
        return self._opPoint_alpha if self._opPoint_alpha is not None else 2.0

    def set_opPoint_alpha (self, aVal : float, refresh=True):
        self._opPoint_alpha = clip (aVal, -20, 20)
        if refresh:
            self.refresh ()
            self._set_yRange ()
        self.sig_opPoint_changed.emit()


    @property
    def opPoint_fixed_to_alpha_max (self)-> bool:
        """ opPoint is fixed to alpha_max"""
        return self._opPoint_fixed_to_alpha_max

    def set_opPoint_fixed_to_alpha_max (self, aBool : bool, refresh=True):
        if aBool: 
            opPoint = self.polar().opPoint_at_alpha_max ()
            if opPoint is not None: 
                self._opPoint_fixed_to_alpha_max = True
                self.set_opPoint_alpha (opPoint.alpha, refresh=False)               # avoid double refresh
            else: 
                # maybe polars were not ready  - so wait 
                self._opPoint_fixed_to_alpha_max = True
                self.set_opPoint_alpha (0, refresh=False)                           # avoid double refresh
        else: 
            self._opPoint_fixed_to_alpha_max = False
            # self.set_opPoint_alpha (2.0)

        if refresh and self.isVisible():
            self.refresh()
        

    @property
    def opPoint_var (self) -> OpPoint_Var:
        """ variable to show in diagram"""
        return self._opPoint_var

    def set_opPoint_var (self, aVal : OpPoint_Var):
        self._opPoint_var = aVal

        artist : VLM_Result_Artist = self._get_artist (VLM_Result_Artist)[0]
        artist.set_opPoint_var (aVal)

        self.refresh()
        self._set_yRange ()


    @property
    def use_viscous_loop (self) -> OpPoint_Var:
        """ viscous loop for aero calculation"""
        return self.polar().use_viscous_loop

    def set_use_viscous_loop (self, aBool : bool):
        self.polar().set_use_viscous_loop (aBool)
        if self.opPoint_fixed_to_alpha_max:
            self.set_opPoint_fixed_to_alpha_max (True)
        self.refresh()


    @property
    def opPoint_var_list (self) -> OpPoint_Var:
        """ variable to show in diagram"""
        return [OpPoint_Var.LIFT, OpPoint_Var.CL, OpPoint_Var.ALPHA]


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None and Worker.ready:   

            l = QGridLayout()
            r,c = 0, 0
            Label    (l,r,c, colSpan=4, get=f"Choose airfoil T1 polar of root", style=style.COMMENT)
            r += 1
            ComboBox (l,r,c, width=180, obj=self, prop=Item_VLM_Result.polar_def_name, colSpan=4,
                         options=lambda: self.polar_def_list)
            # ToolButton (l,r,c+4, icon=Icon.EDIT, set=self.edit_polar_def,      # global refresh needed!
            #                 toolTip="Change the settings of this polar definition")                              
            r += 1
            SpaceR   (l,r, height=10,stretch=0)
            r += 1
            Label    (l,r,c, colSpan=4, get=f"Define operating point", style=style.COMMENT)
            r += 1
            CheckBox (l,r,c, text="Set close to Alpha max", colSpan=4,
                        obj=self, prop=Item_VLM_Result.opPoint_fixed_to_alpha_max,
                        disable=lambda: self.polar() is None,
                        toolTip="Operating point of wing is set to alpha_max of current polar")
            r +=1
            FieldF   (l,r,c, lab="Alpha", lim=(-20,20), step=0.5, width=60, unit="°", dec=1, 
                        obj=self, prop=Item_VLM_Result.opPoint_alpha,
                        disable=lambda: self.opPoint_fixed_to_alpha_max or self.polar() is None) 
            r += 1
            SpaceR   (l,r, height=10,stretch=0)
            r += 1
            Label    (l,r,c, colSpan=4, get=f"Diagram variable along span", style=style.COMMENT)
            r += 1
            Label    (l,r,c,   width=60, get="Variable")

            ComboBox (l,r,c+1, width=60, obj=self, prop=Item_VLM_Result.opPoint_var, 
                         options=self.opPoint_var_list, colSpan=3,
                         disable=lambda: self.polar() is None,
                         toolTip="Choose variable to show along span in diagram")
            r += 1
            SpaceR   (l,r, height=10, stretch=3)

            # dev mode 
            # r += 1
            # CheckBox (l,r,c, text="Viscous loop (dev)", colSpan=3,
            #             obj=self, prop=Item_VLM_Result.use_viscous_loop)
            # r += 1
            # Button   (l,r,c, text="Export Polar", width=100, set=self._export_polar_opPoint)

            r += 1
            Label    (l,r,c, colSpan=4, get=f"Powered by Panel Aero", style=style.COMMENT, fontSize=size.SMALL)
            l.setColumnStretch (3,2)
            l.setColumnMinimumWidth (0,50)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height =280,
                                              switched_on=self._show,  
                                              switchable=True, on_switched=self.setVisible)   
                 
        elif self._section_panel is None and not Worker.ready:  

            l = QGridLayout()
            r,c = 0, 0
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
            l.setColumnStretch (3,2)
            l.setColumnMinimumWidth (0,50)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height =150,
                                              switched_on=self._show,  
                                              switchable=True, on_switched=self.setVisible)   

        return self._section_panel 


    def reset_settings (self): 
        """ reset section settings (when new wing)"""
        
        self._opPoint_var   = OpPoint_Var.CL
        self._vtas          = None                          # true air speed
        self._opPoint_alpha = None
        self._polar_def     = None
        self._opPoint_fixed_to_alpha_max = False            # opPoint is fixed at alpha_max 

        artist : VLM_Result_Artist = self._get_artist (VLM_Result_Artist)[0]
        artist.set_opPoint_var (self._opPoint_var)


    # def edit_polar_def (self):
    #     """ edit current polar definition"""

    #     ---  needs global refresh ---
    #     from airfoil_dialogs import Polar_Definition_Dialog

    #     chord = self.wing().planform.chord_root

    #     diag = Polar_Definition_Dialog (self.section_panel, self.polar_def, dx=260, dy=-150, 
    #                                     polar_type_fixed=True,
    #                                     fixed_chord=chord)
    #     diag.exec()

    #     self.refresh()


    def _export_polar_opPoint (self):
        """ export a wing polar and current opPoint to csv"""

        polar = self.wing().vlm_wing.polar_at (self.vtas)
        fileName = f"{polar.name}.csv"

        filters  = "Polar csv files (*.csv)"
        newPathFilename, _ = QFileDialog.getSaveFileName(self.section_panel, caption="Export Wing Polar to csv", 
                                                         directory=fileName, filter=filters)
        if newPathFilename: 
            polar.export_to_csv (newPathFilename)

        opPoint = self.opPoint()
        fileName = f"{polar.name} alpha={opPoint.alpha:.1f}.csv"

        filters  = "Polar csv files (*.csv)"
        newPathFilename, _ = QFileDialog.getSaveFileName(self.section_panel, caption="Export opPoint to csv", 
                                                         directory=fileName, filter=filters)
        if newPathFilename: 
            opPoint.export_to_csv (newPathFilename)




class Item_Wing (Diagram_Item):
    """ 
    Diagram (Plot) Item for a complete Wing
    """

    name = "View Wing"                                          # used for link and section header 

    def __init__(self, *args,  **kwargs):
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

        self._add_artist (Neutral_Point_Artist  (self, self.planform, show=False))

        # switch off mose helper 

        for artist in self._artists: artist.set_show_mouse_helper (False) 


    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.autoRange (padding=0.08)

        self.viewBox.setDefaultPadding(0.02)
        self.viewBox.setAspectLocked()
        self.viewBox.invertY(True)
        self.viewBox.enableAutoRange()

        self.showGrid(x=False, y=False)
        self.showAxis('left', show=False)
        self.showAxis('bottom', show=True)


    @property
    def show_wingSections (self) -> bool: 
        return self._get_artist (WingSections_Artist)[0].show

    def set_show_wingSections (self, aBool : bool): 
        self._show_artist (WingSections_Artist, aBool)


    @property
    def show_ref_line (self) -> bool: 
        return self._get_artist (Ref_Line_Artist)[0].show

    def set_show_ref_line (self, aBool : bool): 
        self._show_artist (Ref_Line_Artist, aBool)


    @property
    def show_flaps (self) -> bool: 
        return self._get_artist (Flaps_Artist)[0].show

    def set_show_flaps (self, aBool : bool): 
        self._show_artist (Flaps_Artist, aBool)


    @property
    def airfoil_name_artist (self) -> Airfoil_Name_Artist:
        return self._get_artist (Airfoil_Name_Artist) [0]

    @property
    def show_airfoils (self) -> bool: 
        return self.airfoil_name_artist.show

    def set_show_airfoils (self, aBool : bool): 
        self.airfoil_name_artist.set_show (aBool)
        self.setup_viewRange()                                      # ensure airfoil names fit in current view 
        self.section_panel.refresh()                                # enable nick checkbox 


    @property
    def use_nick_name (self) -> bool: 
        return self.airfoil_name_artist.use_nick_name

    def set_use_nick_name (self, aBool : bool): 
        self.airfoil_name_artist.set_use_nick_name (aBool)


    @property
    def show_np (self) -> bool: 
        return self._get_artist (Neutral_Point_Artist)[0].show
    
    def set_show_np (self, aBool : bool): 
        self._show_artist (Neutral_Point_Artist, aBool)


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:    
            l = QGridLayout()
            r,c = 0, 0 
            r += 1
            CheckBox (l,r,c, text="Reference Line", colSpan=2, 
                        get=lambda: self.show_ref_line,
                        set=self.set_show_ref_line) 
            r += 1
            CheckBox (l,r,c, text="Wing Sections", colSpan=2, 
                        get=lambda: self.show_wingSections, set=self.set_show_wingSections) 
            r += 1
            CheckBox (l,r,c, text="Flaps", colSpan=2, 
                        get=lambda: self.show_flaps, set=self.set_show_flaps) 
            r += 1
            CheckBox (l,r,c, text="Airfoils", 
                      get=lambda: self.show_airfoils, set=self.set_show_airfoils) 
            CheckBox (l,r,c+1, text="Use nick name",  
                        get=lambda: self.use_nick_name,
                        set=self.set_use_nick_name,
                        hide=lambda: not self.show_airfoils,
                        toolTip="Airfoils nick name is defined in diagram 'Airfoils'")
            r += 1
            CheckBox (l,r,c, text="Neutral Point (geometric)", colSpan=3, 
                      get=lambda: self.show_np, set=self.set_show_np) 
            r += 1
            l.setColumnMinimumWidth (0,70)
            l.setColumnMinimumWidth (1,80)
            l.setColumnStretch (3,5)
            l.setRowStretch    (r,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height=180, 
                                              switchable=False, hide_switched=True, 
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
    def airfoil_artist (self) -> Airfoil_Artist:
        return self._get_artist (Airfoil_Artist) [0]


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
            CheckBox (l,r,c, text="Also blended", 
                        get=lambda: self.airfoil_artist.show_strak,
                        set=self.airfoil_artist.set_show_strak) 
            r += 1
            CheckBox (l,r,c, text="Use nick name", 
                        get=lambda: self.airfoil_artist.use_nick_name,
                        set=self.airfoil_artist.set_use_nick_name, 
                        toolTip="Airfoils nick name is defined in diagram 'Airfoils'")
            r += 1
            l.setColumnStretch (3,2)
            l.setRowStretch    (r,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height=130, 
                                              switchable=False, hide_switched=False, 
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

    name        = "View Airfoils"                                   
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

        self.viewBox.autoRange (padding=0.08)                                         # first ensure best range x,y 
        self.viewBox.setAspectLocked()
        self.viewBox.enableAutoRange(axis=pg.ViewBox.XAxis, enable=True)
        self.showGrid(x=True, y=True)


    @property
    def airfoil_artist (self) -> Airfoil_Artist:
        return self._get_artist (Airfoil_Artist) [0]
    
    @property 
    def airfoil_nick_prefix (self) -> str:
        return self.wing().airfoil_nick_prefix
    def set_airfoil_nick_prefix (self, aVal : str):
        self.wing().set_airfoil_nick_prefix (aVal)
        self.refresh()
    
    @property 
    def airfoil_nick_base (self) -> str:
        return self.wing().airfoil_nick_base
    def set_airfoil_nick_base (self, aVal : str):
        self.wing().set_airfoil_nick_base (aVal)
        self.airfoil_artist.refresh()
    
    

    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:    
            l = QGridLayout()
            r,c = 0, 0 
            r += 1
            CheckBox (l,r,c, text="In real size", colSpan=4,
                        get=lambda: self.airfoil_artist.real_size,
                        set=self.airfoil_artist.set_real_size) 
            r += 1
            CheckBox (l,r,c, text="Show maximum thickness", colSpan=4,
                        get=lambda: self.airfoil_artist.show_thick,
                        set=self.airfoil_artist.set_show_thick) 
            r += 1
            CheckBox (l,r,c, text="Use airfoils nick name", colSpan=4,
                        get=lambda: self.airfoil_artist.use_nick_name,
                        set=self.airfoil_artist.set_use_nick_name) 
            r += 1
            Field    (l,r,c+1, lab="Prefix", width=60,
                        obj=self, prop=Item_Airfoils.airfoil_nick_prefix,
                        toolTip="Common name prefix for all airfoils")
            r += 1
            FieldI   (l,r,c+1, lab="Base", width=60, step=10, lim=(10, 9990), 
                        obj=self, prop=Item_Airfoils.airfoil_nick_base,
                        toolTip="<div>Will be multiplied with the relative chord of the airfoils wing section and "+ \
                                "added to the prefix</div>")

            r += 1
            l.setColumnMinimumWidth (0,20)
            l.setColumnMinimumWidth (1,50)
            l.setColumnMinimumWidth (2,70)
            l.setColumnStretch (2,2)
            l.setColumnStretch (3,5)
            l.setRowStretch    (r,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height=(180,None),
                                              switchable=True, hide_switched=True, 
                                              on_switched=self.setVisible)
        return self._section_panel 




class Item_Polars (Diagram_Item):
    """ 
    Diagram (Plot) Item for polars 
    """

    name        = "Polar"                               # used for link and section header 
    title       = None 
    subtitle    = None                                  # optional subtitle 

    sig_xyVars_changed           = pyqtSignal()         # airfoil data changed in a diagram 

    def __init__(self, *args, iItem= 1, xyVars=None, **kwargs):

        self._iItem  = iItem
        self._xyVars = None
        self._xyVars_show_dict = {}                     # dict of xyVars shown up to now 
        self.set_xyVars (xyVars)                        # polar vars for x,y axis 

        self._title_item2 = None                        # a second 'title' for x-axis 
        self._autoRange_not_set = True                  # to handle initial no polars to autoRange 
        self._next_btn    = None
        self._prev_btn    = None 

        self.name = f"{self.name} {iItem}"

        super().__init__(*args, **kwargs)

        # buttons for prev/next diagram 

        ico = Icon (Icon.COLLAPSE,light_mode = False)
        self._prev_btn = pg.ButtonItem(pixmap=ico.pixmap(QSize(52,52)), width=26, parentItem=self)
        self._prev_btn.mode = 'auto'
        self._prev_btn.clicked.connect(self._prev_btn_clicked)  

        ico = Icon (Icon.EXPAND,light_mode = False)
        self._next_btn = pg.ButtonItem(pixmap=ico.pixmap(QSize(52,52)), width=26, parentItem=self)
        self._next_btn.mode = 'auto'
        self._next_btn.clicked.connect(self._next_btn_clicked)       

        self._refresh_prev_next_btn ()

        # set margins (inset) of self 
        self.setContentsMargins ( 0,10,10,20)

    @property 
    def has_reset_button (self) -> bool:
        """ reset view button in the lower left corner"""
        # to be overridden
        return False 

    @override
    def resizeEvent(self, ev):

        # update position next/prev button 
        if self._next_btn is not None:  
            item_height = self.size().height()
            item_width  = self.size().width()

            btn_rect = self.mapRectFromItem(self._next_btn, self._next_btn.boundingRect())
            x = item_width / 2
            y = item_height - btn_rect.height() + 3
            self._next_btn.setPos(x, y)             

            y = 5
            self._prev_btn.setPos(x, y)             

        super().resizeEvent (ev)


    def _handle_prev_next (self, step = 0):
        """ activates prev or next xy view defined by step"""

        try:
            # save current view Range
            self._xyVars_show_dict[self._xyVars] = self.viewBox.viewRect()

            # get index of current and set new 
            l = list (self._xyVars_show_dict.keys())
            i = l.index(self._xyVars) + step
            if i >= 0 :
                self._xyVars = l[i]
                viewRect = self._xyVars_show_dict [self._xyVars]
                self.setup_viewRange (rect=viewRect)                # restore view Range
                self._refresh_artist ()                             # draw new polar
                self.sig_xyVars_changed.emit()                      # update view panel 
            self._refresh_prev_next_btn ()                          # update vsibility of buttons
        except :
            pass


    def _prev_btn_clicked (self):
        """ previous diagram button clicked"""
        # leave scene clicked event as plot items will be removed with new xy vars 
        QTimer.singleShot (10, lambda: self._handle_prev_next (step=-1))


    def _next_btn_clicked (self):
        """ next diagram button clicked"""
        # leave scene clicked event as plot items will be removed with new xy vars 
        QTimer.singleShot (10, lambda: self._handle_prev_next (step=1))


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


    def _add_xyVars_to_show_dict (self):
        """ add actual xyVars and viewRange to the dict of already shown combinations"""
        try:
            self._xyVars_show_dict[self._xyVars] = self.viewBox.viewRect()
            self._refresh_prev_next_btn ()
        except:
            pass


    def _refresh_prev_next_btn (self):
        """ hide/show previous / next buttons"""

        l = list (self._xyVars_show_dict.keys())

        if self._xyVars in l:
            i = l.index(self._xyVars)
            if i == 0:
                self._prev_btn.hide()
            else:
                self._prev_btn.show()
            if i >= (len (self._xyVars_show_dict) - 1):
                self._next_btn.hide()
            else:
                self._next_btn.show()
        else: 
            self._prev_btn.hide()
            self._next_btn.hide()


    def _refresh_artist (self): 
        """ refresh plar artist with new diagram variables"""

        artist : Polar_Artist = self._artists [0]
        artist.set_xyVars (self._xyVars)

        self.plot_title()

   

    @property
    def xVar (self) -> var:
        return self._xyVars[0]

    def set_xVar (self, varType : var):
        """ set x diagram variable"""

        # save current state - here: only if it is already in dict or first time
        if self._xyVars in self._xyVars_show_dict or not self._xyVars_show_dict:
            self._xyVars_show_dict[self._xyVars] = self.viewBox.viewRect()

        self._xyVars = (varType, self._xyVars[1])
        # wait a little until user is sure for new xyVars (prev/next buttons)
        QTimer.singleShot (3000, self._add_xyVars_to_show_dict)

        self._refresh_artist ()
        self.setup_viewRange ()


    @property
    def yVar (self) -> var:
        return self._xyVars[1]
    
    def set_yVar (self, varType: var):
        """ set y diagram variable"""

        # save current state - here: only if it is already in dict or first time
        if self._xyVars in self._xyVars_show_dict or not self._xyVars_show_dict:
            self._xyVars_show_dict[self._xyVars] = self.viewBox.viewRect()

        self._xyVars = (self._xyVars[0], varType)
        # wait a little until user is sure for new xyVars (prev/next buttons)
        QTimer.singleShot (3000, self._add_xyVars_to_show_dict)

        self._refresh_artist ()
        self.setup_viewRange ()


    def set_xyVars (self, xyVars : list[str]):
        """ set xyVars from a list of var strings or enum var"""

        xVar = xyVars[0]
        if not isinstance (xVar, var):
            xVar = var(xVar)
        else: 
            xVar = xVar 

        yVar = xyVars[1]
        if not isinstance (yVar, var):
            yVar = var(yVar)
        else: 
            yVar = yVar 
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
    def setup_viewRange (self, rect=None):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.05)

        if rect is None: 
            self.viewBox.autoRange ()                           # ensure best range x,y 

            # it could be that there are initially no polars, so autoRange wouldn't set a range, retry at next refresh
            if  self.viewBox.childrenBounds() != [None,None] and self._autoRange_not_set:
                self._autoRange_not_set = False 
            self.viewBox.enableAutoRange(enable=False)

            self.showGrid(x=True, y=True)
        else: 
            self.viewBox.setRange (rect=rect, padding=0.0)      # restore view Range

        self._set_legend_position ()                            # find nice legend position 


    def _set_legend_position (self):
        """ try to have a good position for legend depending on xyVars"""

        if self.legend is None:
            # normally Artist adds legend  - here to set legend during init 
            self.addLegend(offset=(-10,10),  verSpacing=0 )  
            self.legend.setLabelTextColor (Artist.COLOR_LEGEND)

        if (self.yVar == var.CL or self.yVar == var.ALPHA) and self.xVar == var.CD:
            self.legend.anchor (itemPos=(1,0.5), parentPos=(1,0.5), offset=(-10,0))     # right, middle 

        elif (self.yVar == var.GLIDE or self.yVar == var.SINK) and (self.xVar == var.ALPHA or self.xVar == var.CL):
            self.legend.anchor (itemPos=(0,0), parentPos=(0,0), offset=(40,10))         # left, top
            # self.legend.anchor (itemPos=(0.2,1), parentPos=(0.5,1), offset=(0,-20))     # middle, bottom

        elif (self.yVar == var.CL) and (self.xVar == var.ALPHA):
            self.legend.anchor (itemPos=(0,0), parentPos=(0,0), offset=(40,10))         # left, top

        else:  
            self.legend.anchor (itemPos=(1,0), parentPos=(1,0), offset=(-10,10))        # right, top 

        # reduce vertical spacing 
        l : QGraphicsGridLayout = self.legend.layout
        l.setVerticalSpacing(0)



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

        super().__init__(*args,  **kwargs)


    @property
    def show_wingSections (self) -> bool: 
        return self._get_artist (WingSections_Artist) [0].show
    
    def set_show_wingSections (self, aBool : bool): 
        self._show_artist (WingSections_Artist, show=aBool)


    @property
    def show_ref_line (self) -> bool: 
        return self._get_artist (Ref_Line_Artist) [0].show
    
    def set_show_ref_line (self, aBool : bool): 
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
        artist = self._get_artist (Flaps_Artist)[0]
        return artist.show
    
    def set_show_flaps (self, aBool : bool): 
        self._show_artist (Flaps_Artist, aBool)
        self._viewPanel.refresh()


    @property
    def show_flap_depth (self) -> bool: 
        artist : Flaps_Artist = self._get_artist (Flaps_Artist)[0]
        return artist.show_depth
    
    def set_show_flap_depth (self, aBool : bool): 
        artist : Flaps_Artist
        for artist in self._get_artist (Flaps_Artist):
            artist.set_show_depth (aBool)


    @property
    def background_image_artist (self) -> Image_Artist:
        return self._get_artist (Image_Artist) [0]

    def set_show_ref_planforms (self, aBool : bool): 
        self._show_artist (Ref_Planforms_Artist, aBool)
                                      


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
                artist.sig_flaps_changed.connect        (self._on_flaps_changed) 
                artist.sig_wingSection_changed.connect  (self._on_wingSection_changed) 
                artist.sig_wingSection_new.connect      (self.sig_wingSection_new.emit) 
                artist.sig_wingSection_selected.connect (self.sig_wingSection_selected.emit) 


    # --- view section panels ---------------------------------------------------

    @override
    def create_view_panel (self):
        """ 
        creates a view panel to the left of at least one diagram item 
        has a section_panel
        """

        super().create_view_panel ()

        layout : QVBoxLayout = self._viewPanel.layout()

        layout.insertWidget (0, self.general_panel, stretch=0)          # general at top
        layout.addStretch   (3)
        layout.addWidget    (self.export_panel, stretch=0)              # export at bottom


    @property 
    def general_panel (self) -> Edit_Panel | None:
        """ additional section panel with commmon settings"""

        if self._general_panel is None:

            l = QGridLayout()
            r,c = 0, 0
            CheckBox (l,r,c, text="Show mouse helper", colSpan=2,
                        get=lambda: self.show_mouse_helper, set=self.set_show_mouse_helper) 
            r += 1
            CheckBox (l,r,c, text="Reference Line",  colSpan=2,
                        get=lambda: self.show_ref_line, set=self.set_show_ref_line) 
            r += 1
            CheckBox (l,r,c, text="Wing Sections",  colSpan=2,
                        get=lambda: self.show_wingSections, set=self.set_show_wingSections) 
            r += 1
            CheckBox (l,r,c, text="Flaps", 
                        get=lambda: self.show_flaps, set=self.set_show_flaps) 
            CheckBox (l,r,c+1, text="Flap depth", 
                        get=lambda: self.show_flap_depth, set=self.set_show_flap_depth,
                        hide=lambda: not self.show_flaps)

            l.setColumnMinimumWidth (0,70)
            l.setColumnStretch (2,5)
            l.setRowStretch (r+1,3)

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
            CheckBox   (l,r,c, text="Another PC2 Planform", colSpan=2,
                        hide = lambda: bool(self.wing().reference_pc2_file))
            Button     (l,r,c+2, colSpan=2, text="Select", width=50, 
                        set=self._open_planform_ref_pc2, toolTip="Select another PC2 Planform as reference",
                        hide = lambda: bool(self.wing().reference_pc2_file))

            CheckBox   (l,r,c, text=lambda: self.wing().planform_ref_pc2_name, 
                        get=lambda: self.show_ref_pc2, set=self.set_show_ref_pc2, 
                        hide = lambda: not bool(self.wing().reference_pc2_file)) 
            ToolButton (l,r,c+2, icon=Icon.OPEN, 
                        set=self._open_planform_ref_pc2, toolTip="Open new PC2 Planform",
                        hide = lambda: not bool(self.wing().reference_pc2_file))
            ToolButton (l,r,c+3, icon=Icon.DELETE, 
                        set=self._remove_planform_ref_pc2, toolTip="Remove PC2 Planform",
                        hide = lambda: not bool(self.wing().reference_pc2_file))


            # toggle fields for background image
            r += 1
            CheckBox   (l,r,c, text="Background Image", get=False, 
                        hide = lambda: bool(self.wing().background_image.filename)) 
            Button     (l,r,c+2, text="Select", width=50, colSpan=2,
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
            ToolButton (l,r,c+3, icon=Icon.DELETE, 
                        set=self._remove_background_image, toolTip="Remove background image",
                        hide = lambda: not bool(self.wing().background_image.filename))

            l.setColumnStretch (0,3)
            l.setRowStretch (r+1,3)

            self._section_panel = Edit_Panel (title="Reference Planforms", layout=l, height=130,
                                              switchable=True, hide_switched=True, switched_on=False, 
                                              on_switched=self.set_show_ref_planforms)

        return self._section_panel 


    @property 
    def export_panel (self) -> Edit_Panel | None:
        """ additional section panel with export buttons"""

        if self._export_panel is None:

            l = QGridLayout()
            r,c = 0, 1
            Button  (l,r,c, text="Export Dxf", width=100, set=self.export_dxf)
            r += 1
            SpaceR      (l,r,10,1)

            l.setColumnMinimumWidth (0,20)
            l.setColumnStretch (2,2)

            self._export_panel = Edit_Panel (title="Export", layout=l, height=(60,None),
                                              switchable=False, switched_on=True)
        return self._export_panel 


    def export_dxf (self):
        """open export planform to dxf dialog """

        dialog = Dialog_Export_Dxf (self, self.wing, parentPos=(0.2,0.7), dialogPos=(0,1))  
        dialog.exec()     



    def _open_planform_ref_pc2 (self):
        """ open reference pc2 file """

        filters  = "PlanformCreator2 files (*.pc2)"
        newPathFilename, _ = QFileDialog.getOpenFileName(self, filter=filters,
                                                         caption="Open PlanformCreator file")

        if newPathFilename: 
            self.wing().set_reference_pc2_file (newPathFilename)
            self.set_show_ref_pc2 (True)
            self.refresh ()  


    def _remove_planform_ref_pc2 (self):
        """ remove reference pc2 file """

        self.wing().set_reference_pc2_file (None)
        self.set_show_ref_pc2 (False)
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


    def _remove_background_image (self):
        """ remove background image file  """

        self.wing().background_image.set_pathFilename (None)
        self.background_image_artist.set_show(False)
        self.refresh ()  


    def _edit_background_image (self):
        """ edit settings of background image   """

        self.wing().background_image._qimage = None

        dialog = Dialog_Edit_Image (self.section_panel, self.wing().background_image, dx=150, dy=-400)  
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
        self.graph_layout.setHorizontalSpacing (20)


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
                artist.sig_wingSection_selected.connect (self.sig_wingSection_selected.emit) 


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




class Diagram_Airfoils (Diagram_Abstract):
    """    
    Diagram view to show/plot airfoil diagrams - Container for diagram items 
    """

    name   = "Airfoils && Polars"                           # will be shown in Tabs 

    def __init__(self, *args, diagram_settings=[], **kwargs):

        self._polar_panel   = None 
        self._general_panel = None                          # panel with general settings  
        self._export_panel  = None

        self._diagram_settings = diagram_settings

        self._show_operating_points = False                 # show polars operating points 
        self._show_strak    = False                         # show blended airfoils
        self._min_re_asK    = 10                            # minimum re number / 1000 to plot 
        self._apply_min_re  = False                         # activate min re rumber 

        super().__init__(*args, **kwargs)

    # --- save --------------------- 

    def _as_dict_list (self) -> list:
        """ returns a list with data dict of the parameters of diagram items """

        l = []
        item : Item_Polars
        for item in self._get_items (Item_Polars):
            item_dict = {}
            toDict (item_dict, "xyVars", (str(item.xVar), str(item.yVar)))

            l.append (item_dict)
        return l


    # -------------

    @override
    def refresh(self, also_viewRange=True): 
        """ 
        override to reset autoRange of polars 
        """

        if self.isVisible() and also_viewRange:

            item : Item_Polars
            for item in self._get_items (Item_Polars):
                item._autoRange_not_set = True

            logger.debug (f"{str(self)} reset autorange for item_polars")

        super().refresh(also_viewRange=also_viewRange)



    def polar_defs (self) -> list [Polar_Definition]:
        """ actual polar definitions"""
        return self.wing().polar_definitions

    @property
    def chord_root (self) -> float:
        """ chord at root section"""
        return self.wing().planform.chord_root


    @override
    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """

        r = 0 

        self._add_item (Item_Airfoils (self, getter=self.wing), r, 0, colspan=2)

        if Worker.ready:
            r += 1

            # create Polar items with init values from settings 

            dataDict = self._diagram_settings[0] if len(self._diagram_settings) > 0 else {"xyVars" : (var.CD,var.CL)}
            xyVars = dataDict ["xyVars"]

            item = Item_Polars (self, iItem=1, getter=self.wing, xyVars=xyVars, show=False)
            item.sig_xyVars_changed.connect (self._on_xyVars_changed)
            self._add_item (item, r, 0)

            dataDict = self._diagram_settings[1] if len(self._diagram_settings) > 1 else {"xyVars" : (var.CL,var.GLIDE)}
            xyVars = dataDict ["xyVars"]

            item = Item_Polars (self, iItem=2, getter=self.wing, xyVars=xyVars, show=False)
            item.sig_xyVars_changed.connect (self._on_xyVars_changed)
            self._add_item (item, r, 1)
 

    @override
    def create_view_panel (self):
        """ 
        creates a view panel to the left of at least one diagram item 
        has a section_panel
        """
        # override to add additional general settings panel on top 

        super().create_view_panel ()

        layout : QVBoxLayout = self._viewPanel.layout()

        layout.insertWidget (0, self.general_panel, stretch=0)
        layout.insertWidget (2, self.polar_panel, stretch=0)

        # export panel at bottom 

        layout.addStretch   (3)
        layout.addWidget    (self.export_panel, stretch=0)


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
    def show_strak (self) -> bool:
        """ show blended airfoils """
        return self._show_strak

    def set_show_strak (self, aBool : bool):
        self._show_strak = aBool

        artist : Polar_Artist
        for artist in self._get_artist ([Polar_Artist, Airfoil_Artist]):
            artist.set_show_strak (aBool) 

    @property 
    def min_re_asK (self) -> int:
        """ minimum re rumber / 1000 to plot  """
        return self._min_re_asK

    def set_min_re_asK (self, aVal : int):
        self._min_re_asK = clip (aVal,1,1000) 
        self._apply_min_re_to_artists ()

    @property 
    def apply_min_re (self) -> bool:
        """ should min_re be applied  """
        return self._apply_min_re

    def set_apply_min_re (self, aBool : bool):
        self._apply_min_re = aBool == True 
        self.polar_panel.refresh()
        self._apply_min_re_to_artists ()


    def _apply_min_re_to_artists (self):
        """ tell artists to apply min_re - update view range """
        artist : Polar_Artist
        for artist in self._get_artist (Polar_Artist):
            artist.set_min_re_asK (self._min_re_asK if self.apply_min_re else 0) 

        for item in self._get_items (Item_Polars):
            item.setup_viewRange ()


    @property 
    def general_panel (self) -> Edit_Panel | None:
        """ additional section panel with commmon settings"""

        if self._general_panel is None:

            l = QGridLayout()
            r,c = 0, 0
            CheckBox (l,r,c, text="Show blended airfoils", 
                      get=lambda: self.show_strak, set=self.set_show_strak) 
            r += 1
            l.setColumnStretch (3,2)
            l.setRowStretch    (r,2)

            self._general_panel = Edit_Panel (title="Common Options", layout=l, height=(60,None),
                                              switchable=False, switched_on=True)
        return self._general_panel 


    @property
    def polar_panel (self) -> Edit_Panel:
        """ return polar extra panel to admin polar definitions and define polar diagrams"""

        if self._polar_panel is None:
        
            l = QGridLayout()
            r,c = 0, 0

            Label (l,r,c, colSpan=6, get="Polar definitions for Root", style=style.COMMENT) 
            r += 1

            # helper panel for polar definitions 

            p = Panel_Polar_Defs (self, self.polar_defs, height=(None,None), width=(None,250),chord_fn=lambda: self.chord_root)

            p.sig_polar_def_changed.connect (self.sig_polar_def_changed.emit)

            l.addWidget (p, r, c, 1, 6)
            l.setRowStretch (r,1)

            # minimum re rumber to plot 

            r += 1
            SpaceR      (l,r, height=5, stretch=0) 
            r += 1
            CheckBox    (l,r,c, text="Minimum Re", colSpan=4,
                            obj=self, prop=Diagram_Airfoils.apply_min_re,
                            toolTip="Apply a minimum Re number for polars in the diagrams")
            FieldF      (l,r,c+4, width=60, step=1, lim=(1, 1000), unit="k", dec=0,
                            obj=self, prop=Diagram_Airfoils.min_re_asK,
                            hide=lambda: not self.apply_min_re)

            # polar diagrams variables setting 

            r += 1
            if Worker.ready:
                SpaceR (l,r, height=5, stretch=0) 
                r += 1
                Label (l,r,c, colSpan=4, get="Diagram variables", style=style.COMMENT) 
                r += 1
                for item in self._get_items (Item_Polars):

                    Label       (l,r,c,   width=20, get="y")
                    ComboBox    (l,r,c+1, width=60, obj=item, prop=Item_Polars.yVar, options=var.values)
                    SpaceC      (l,c+2,   width=15, stretch=0)
                    Label       (l,r,c+3, width=20, get="x")
                    ComboBox    (l,r,c+4, width=60, obj=item, prop=Item_Polars.xVar, options=var.values)
                    SpaceC      (l,c+5)
                    r += 1

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


    @property 
    def export_panel (self) -> Edit_Panel | None:
        """ additional section panel with export buttons"""

        if self._export_panel is None:

            l = QGridLayout()
            r,c = 0, 1
            Button      (l,r,c, text="Export Airfoils", width=100, set=self.export_airfoils)
            r += 1
            SpaceR      (l,r,10,1)

            l.setColumnMinimumWidth (0,20)
            l.setColumnStretch (2,2)

            self._export_panel = Edit_Panel (title="Export", layout=l, height=(60,None),
                                              switchable=False, switched_on=True)
        return self._export_panel 



    def export_airfoils (self):
        """open export airfoils of wing dialog """

        dialog = Dialog_Export_Airfoil (self, self.wing, parentPos=(0.2,0.7), dialogPos=(0,1))  
        dialog.exec()    


    # --- public slots ---------------------------------------------------


    @override
    def on_new_polars (self):
        """ slot to handle new polars loaded which were generated async by Worker """

        logger.debug (f"{str(self)} on new polars")

        for item in self._get_items (Item_Polars):
            item.refresh ()


    @override
    def on_polar_set_changed (self):
        """ slot to handle changed polar set signal """

        logger.debug (f"{str(self)} on polar set changed")

        for item in self._get_items (Item_Polars):
            item.refresh ()

        if self._viewPanel:
            self._viewPanel.refresh()


    # --- private slots ---------------------------------------------------

    def _on_polars_switched (self, aBool):
        """ slot to handle polars switched on/off """

        logger.debug (f"{str(self)} on polars switched")

        for item in self._get_items (Item_Polars):
            item.setVisible (aBool)


    def _on_xyVars_changed (self):
        """ slot to handle change of xyVars made in diagram """

        logger.debug (f"{str(self)} on xyVars changed in diagram")

        self.polar_panel.refresh()



class Diagram_Wing_Analysis (Diagram_Abstract):
    """    
    Diagram show/plot VLM aero results 
    """

    name   = "Wing Analysis"                                # will be shown in Tabs 

    def __init__(self, *args, **kwargs):

        self._airfoil_panel      = None         
        self._export_panel       = None                     # export to xflr5 ...
        self._general_panel      = None                     # panel with general settings  

        super().__init__(*args, **kwargs)


    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """

        i = Item_VLM_Panels (self, getter=self.wing, show=True, 
                             wingSection_fn = self._wingSection_fn, opPoint_fn=self.opPoint)
        self._add_item (i, 0, 0)

        i = Item_Chord (self, getter=self.wing, wingSection_fn = self._wingSection_fn, show=False)
        self._add_item (i, 1, 0)
        i.viewBox.setXLink (Item_VLM_Panels.name)

        i = Item_VLM_Result (self, getter=self.wing, show=False,
                             wingSection_fn = self._wingSection_fn, opPoint_fn=self.opPoint)
        self._add_item (i, 2, 0)
        i.viewBox.setXLink (Item_VLM_Panels.name)

        i.sig_opPoint_changed.connect   (self._on_opPoint_changed)
        i.sig_visible.connect           (self._on_aero_analysis_visible)

        # generic connect to artist changed signals 

        for item in self.diagram_items:
            artist : Abstract_Artist_Planform
            for artist in item._artists:
                artist.sig_planform_changed.connect     (self._on_planform_changed) 
                artist.sig_wingSection_changed.connect  (self._on_wingSection_changed) 
                artist.sig_wingSection_new.connect      (self.sig_wingSection_new.emit) 
                artist.sig_wingSection_selected.connect (self.sig_wingSection_selected.emit) 

        # show wingSections where possible (Item_Chord) 

        for artist in self._get_artist (WingSections_Artist): 
            artist.set_show (True, refresh=False) 


    # --- view section panels ---------------------------------------------------

    @override
    def create_view_panel (self):
        """ 
        creates a view panel to the left of at least one diagram item 
        has a section_panel
        """
        # override to extra position vlm panel 

        layout = QVBoxLayout()
        layout.setContentsMargins (QMargins(0, 0, 0, 0)) 

        for item in self.diagram_items:                                 # paneling panel, chord distribution
            if item.section_panel is not None: 
                layout.addWidget (item.section_panel,stretch=1)

        layout.insertWidget (0, self.general_panel, stretch=0)          # common settings
        layout.addStretch (1)
        layout.addWidget (self.export_panel)                            # export xflr5 panel 

        self._viewPanel = Container_Panel()
        self._viewPanel.setLayout (layout)


    def planform_paneled (self) -> Planform_Paneled:
        return self.wing().planform_paneled

    @property
    def show_wingSections (self) -> bool: 
        artist = self._get_artist (WingSections_Artist)[0]
        return artist.show 
    
    def set_show_wingSections (self, aBool : bool): 
        self._show_artist (WingSections_Artist, show=aBool)
 

    @property
    def show_airfoils (self) -> bool: 
        artists = self._get_artist (Airfoil_Name_Artist)
        return artists[0].show if artists else False  
    
    def set_show_airfoils (self, aBool : bool): 
        self._show_artist (Airfoil_Name_Artist, aBool)
        self.general_panel.refresh()                                # enable blended checkbox 

        item = self._get_items (Item_VLM_Panels)[0]
        item.setup_viewRange()                                      # ensure airfoil names fit in current view 


    def opPoint (self) -> VLM_OpPoint:
        """ current selected opPoint based on vta and aplha"""

        item : Item_VLM_Result = self._get_items (Item_VLM_Result)[0]
        return item.opPoint()


    @property 
    def general_panel (self) -> Edit_Panel | None:
        """ additional section panel with commmon settings"""

        if self._general_panel is None:

            l = QGridLayout()
            r,c = 0, 0
            CheckBox (l,r,c, text="Show mouse helper", 
                      get=lambda: self.show_mouse_helper, set=self.set_show_mouse_helper) 
            r += 1
            CheckBox (l,r,c, text="Wing Sections", 
                      get=lambda: self.show_wingSections, set=self.set_show_wingSections) 
            r += 1
            CheckBox (l,r,c, text="Airfoils", 
                      get=lambda: self.show_airfoils, set=self.set_show_airfoils) 
            l.setColumnStretch (0,2)
            l.setRowStretch (r+1,3)

            self._general_panel = Edit_Panel (title="Common Options", layout=l, height=(60,None),
                                              switchable=False, switched_on=True)
        return self._general_panel 



    @property 
    def export_panel (self) -> Edit_Panel | None:
        """ additional section panel with export buttons"""

        if self._export_panel is None:

            l = QGridLayout()
            r,c = 0, 1
            Button      (l,r,c, text="Export Xflr5", width=100, set=self.export_xflr5)
            r += 1
            SpaceR (l,r, height=2, stretch=0)
            r += 1
            Button      (l,r,c, text="Export FLZ", width=100, set=self.export_flz,
                                hide= not os.name == 'nt')                                  # only Windows
            r += 1
            SpaceR (l,r, height=2, stretch=0)
            r += 1
            Button      (l,r,c, text="Launch FLZ", width=100, set=self.launch_flz,
                                hide= not os.name == 'nt')                                  # only Windows
            r += 1
            SpaceR      (l,r,10,1)
            l.setColumnMinimumWidth (0,20)
            l.setColumnStretch (2,2)

            self._export_panel = Edit_Panel (title="Export", layout=l, height=(120,None),
                                              switchable=False, switched_on=True)
        return self._export_panel 


    # --- public slots ---------------------------------------------------


    def on_new_polars (self):
        """ slot to handle new polars loaded which were generated async by Worker """

        logger.debug (f"{str(self)} on new polars")

        if self.isVisible():
            item = self._get_items (Item_VLM_Panels)[0]
            if item.isVisible(): 
                item.refresh()
            
            item = self._get_items (Item_VLM_Result)[0]
            if item.isVisible():
                item.refresh()
                item.setup_viewRange ()                             # new polars could leed to show first time 


    @override
    def on_wingSection_changed (self):
        """ slot to handle changed wing section data"""

        # overridden to ensure new strak (-> airfoil polar) when wingSection changed 
        if self.isVisible():
            self.wing().planform.wingSections.do_strak()

        super().refresh(also_viewRange=False)


    @override
    def on_wingSection_selected (self):
        """ slot to handle changed wing section data"""

        # overridden to ensure new strak (-> airfoil polar) when wingSection changed 
        if self.isVisible():
            self.wing().planform.wingSections.do_strak()

            # overridden to refresh only wingSection artist 
            for artist in self._get_artist (WingSections_Artist):
                artist.refresh ()


    @override
    def on_planform_changed (self):
        """ slot to handle new current wing section """

        # overridden - refresh via on_paneling_changed
        pass

    def on_paneling_changed (self):
        """ slot to handle changed polar definitions """

        if self.isVisible():

            logger.debug (f"{str(self)} on paneling changed - visible: {self.isVisible()}")

            # overridden to re-calc alpha max
            item : Item_VLM_Result = self._get_items (Item_VLM_Result)[0]
            item.set_opPoint_fixed_to_alpha_max (item.opPoint_fixed_to_alpha_max, refresh=False)

            self.refresh(also_viewRange=False)


    @override
    def on_new_polars (self):
        """ slot to handle new polars loaded which were generated async by Worker """

        if self.isVisible():

            logger.debug (f"{str(self)} on new polars changed - visible: {self.isVisible()}")

            # overridden to re-calc alpha max
            item : Item_VLM_Result = self._get_items (Item_VLM_Result)[0]
            item.set_opPoint_fixed_to_alpha_max (item.opPoint_fixed_to_alpha_max, refresh=False)

            self.refresh(also_viewRange=False)


    @override
    def on_wing_new (self):
        """ slot to handle new wing signal """

        # overridden to ensure reset section variables when new wing 
        item : Item_VLM_Result = self._get_items (Item_VLM_Result)[0]
        item.reset_settings ()

        super().refresh()                                   




    # --- private slots ---------------------------------------------------

    @override
    def _on_planform_changed (self):
        """ slot to handle geometry change made in diagram """

        # refresh will be done via on_paneling_changed

        self.sig_planform_changed.emit()            # refresh app


    def _on_opPoint_changed (self): 
        """ slot to handle new opPOint in VLM_Result item """
        artist = self._get_artist (VLM_Panels_Artist)[0]
        artist.refresh()


    def _on_aero_analysis_visible (self): 
        """ slot to handle activation of aero analysis """
        item = self._get_items (Item_VLM_Panels)[0]
        item.section_panel.refresh()                                    # disable check show cp 


    @override
    def _on_wingSection_changed (self):
        """ slot to handle section changes made in diagram """

        logger.debug (f"{str(self)} on on_wingSection_changed in diagram")
    
        # do not refresh as panelling will also change and refresh 
        self.sig_wingSection_changed.emit()         # refresh app


    def export_xflr5 (self): 
        """ export wing to xflr5"""

        dialog = Dialog_Export_Xflr5 (self, self.wing, parentPos=(0.2,0.7), dialogPos=(0,1))  
        dialog.exec()     


    def export_flz (self): 
        """ export wing to flz"""

        dialog = Dialog_Export_FLZ (self, self.wing, parentPos=(0.2,0.7), dialogPos=(0,1))  
        dialog.exec()     


    def launch_flz (self): 
        """ export wing to FLZ and launch """

        self.wing().export_flz.do_it ()

        pathFileName = os.path.join (self.wing().export_flz.export_dir_abs, self.wing().export_flz.flz_filename) 
        try: 
            os.startfile(pathFileName, 'open')
        except: 
            message = "Could not launch FLZ_vortex on exported file: <br><br>" + \
                    pathFileName + \
                    "<br><br> Is FLZ_vortex neatly installed and associated with file extension '.flz'?"
            MessageBox.error (self,"Export FLZ", str(message))



# -------- Diagram Items -------------------------------------------------


class Item_Making_Of_Abstract (Diagram_Item):
    """ 
   Abstract Making Of Diagram (Plot) Item 
    """

    min_width   = 200                                   # min size needed - see below 
    min_height  = 100 

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.setContentsMargins (20,100,20,20)


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

    title       = "Wing Analysis"                 
    subtitle    = "The planform is idealized by panels being the starting basis <br>" + \
                  "either for aerodynamic assessment of the lift reserves of the wing<br>" + \
                  "or the export to Xflr5 or FLZ_vortex for further processing."                         

    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (VLM_Panels_Artist     (self, self.planform, opPoint_fn=None, show_legend=False))
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
    You can use the app to design the planform of a wing either just <br>
    as a draft for CAD processing or as the input for Xflr5 and FLZ_vortex <br>
    for further aerodynamic assessment.
    <p> 
    The base element of PlanformCreator2 is the <span style="color: whitesmoke">Chord Distribution</span>, <br>
    which is defined first and which will essentially determine the aero<br>
    properties of a wing regardless of how the planform is later distorted <br>
    within the app, this chord distribution is retained.
    </p> 
    <p> 
    This overview shows the individual steps of how the final planform <br>
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
