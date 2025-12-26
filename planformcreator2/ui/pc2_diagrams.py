#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""  

Diagram (items) for airfoil

"""

import logging

from copy                   import deepcopy
from PyQt6.QtWidgets        import QFileDialog, QGraphicsLayoutItem

# ---- base modules from airfoileditor  

from airfoileditor.base.widgets         import * 
from airfoileditor.base.diagram         import * 
from airfoileditor.base.panels          import Container_Panel, MessageBox

from airfoileditor.model.polar_set      import *
from airfoileditor.model.xo2_driver     import Worker
from airfoileditor.ui.util_dialogs      import Polar_Definition_Dialog, Calc_Reynolds_Dialog

# ---- pc2 modules  

from ..model.wing       import Wing, Planform
from ..model.VLM_wing   import VLM_OpPoint, OpPoint_Var, VLM_Wing

from .pc2_artists       import *
from .pc2_dialogs       import (Dialog_Edit_Image, Dialog_Edit_Paneling, Dialog_Export_Xflr5,
                                Dialog_Export_FLZ, Dialog_Export_CSV, Dialog_Export_Airfoil, Dialog_Export_DXF)

from ..app_model        import App_Model

logger = logging.getLogger(__name__)
# logger.setLevel(logging.DEBUG)


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


    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self._viewPanel.setMinimumWidth(250)
        self._viewPanel.setMaximumWidth(250)

        # set spacing between the two items
        self.graph_layout.setContentsMargins (20,10,20,10)  # default margins
        self.graph_layout.setVerticalSpacing (10)

        # connect to app model signals to refresh - only hammer signals here, detailed signals in Item
        self.app_model.sig_new_wing.connect                 (self.refresh)


    @property
    def app_model (self) -> App_Model:
        """ application model """
        return self._dataObject

    @property
    def wing (self) -> Wing: 
        """ active wing"""
        return self.app_model.wing

    @property
    def planform (self) -> Planform: 
        """ active planform"""
        return self.wing.planform

    @property
    def cur_wingSection (self) -> WingSection | None: 
        """ returns the current, selected wing section """
        return self.app_model.cur_wingSection

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



#-------------------------------------------------------------------------------
# Diagram Items  
#-------------------------------------------------------------------------------


class Item_Abstract (Diagram_Item):
    """ 
    Abstract Diagram (Plot) Item for all Diagram Items in PC2
    """

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
  
        self.setContentsMargins ( 0,50,0,20)                # default set margins (inset) of self 

        # connect to app model signals to refresh - new wing is handled in Diagram_Abstract
        self.app_model.sig_planform_changed.connect         (self.refresh)
        self.app_model.sig_wingSection_changed.connect      (self._on_wingSection_changed)
        self.app_model.sig_wingSection_selected.connect     (self._on_wingSection_changed)

    @property
    def app_model (self) -> App_Model:
        """ application model """
        return self._dataObject   

    @property
    def wing (self) -> Wing: 
        return self.app_model.wing
    
    @property
    def cur_wingSection (self) -> WingSection | None: 
        """ returns the current, selected wing section """
        return self.app_model.cur_wingSection   

    @property
    def planform (self) -> Planform:
        return self.wing.planform


    def _on_wingSection_changed (self):
        """ slot when wingSection changed - refresh only relevant artists """

        if self.isVisible_effective():
            artist : WingSections_Artist 
            for artist in self._get_artist (WingSections_Artist):
                logger.debug (f"{self} _on_wingSection_changed refresh {artist}")
                artist.refresh()            


    def _setup_artists_slots (self):
        """ connect signals from artists to app model notify"""
        
        artist : Abstract_Artist_Planform
        for artist in self._artists:
            if isinstance (artist, Abstract_Artist_Planform):   
                artist.sig_planform_changed.connect     (self.app_model.notify_planform_changed) 
                artist.sig_flaps_changed.connect        (self.app_model.notify_planform_changed) 
                artist.sig_wingSection_changed.connect  (self.app_model.notify_wingSection_changed) 
                artist.sig_wingSection_new.connect      (self.app_model.create_wingSection_at ) 
                artist.sig_wingSection_delete.connect   (self.app_model.delete_wingSection) 
                artist.sig_wingSection_selected.connect (self.app_model.set_cur_wingSection)



class Item_Planform (Item_Abstract):
    """ 
    Diagram (Plot) Item for Planform
    """

    name        = "View Planform"                           # used for link and section header 
    title       = "Planform"                 
    subtitle    = "dynamic"                                 # will be set dynamically 


    @override
    def plot_title(self, **kwargs):

        return super().plot_title(subtitle = self.planform.name, **kwargs)


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (Planform_Artist       (self, lambda: self.planform, show_legend=True))
        self._add_artist (Planform_Box_Artist   (self, lambda: self.planform))
        self._add_artist (Ref_Line_Artist       (self, lambda: self.planform, mode=mode.REF_TO_PLAN, show_legend=True))
        self._add_artist (WingSections_Artist   (self, lambda: self.planform, show=False, show_legend=True,
                                                 wingSection_fn=lambda: self.cur_wingSection))
        self._add_artist (Flaps_Artist          (self, lambda: self.planform, show=False ,show_legend=True))
        self._add_artist (Airfoil_Name_Artist   (self, lambda: self.planform, show=False, show_legend=False))
        self._add_artist (Ref_Planforms_Artist  (self, lambda: self.planform, show=False, show_legend=True))
        self._add_artist (Image_Artist          (self, lambda: self.planform, show=False, 
                                                       image_def=lambda: self.wing.background_image,
                                                       as_background=True))
        self._add_artist (Neutral_Point_Artist  (self, lambda: self.planform, show=False))

        # connect signals from artists to app model notify
        self._setup_artists_slots()


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
    def show_np (self) -> bool: 
        return self._get_artist (Neutral_Point_Artist)[0].show
    
    def set_show_np (self, aBool : bool): 
        self._show_artist (Neutral_Point_Artist, aBool)


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
    def airfoil_use_nick (self) -> bool:
        return self.wing.airfoil_use_nick

    def set_airfoil_use_nick (self, aBool : bool):
        self.wing.set_airfoil_use_nick (aBool)
        self.refresh()


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:    
            l = QGridLayout()
            r,c = 0, 0
            CheckBox (l,r,c, text="Bounding Box", colSpan=2,
                      get=lambda: self.show_bounding_box, set=self.set_show_bounding_box)
            r += 1
            CheckBox (l,r,c, text="Neutral Point (geometric)", colSpan=3, 
                      get=lambda: self.show_np, set=self.set_show_np) 
            r += 1
            CheckBox (l,r,c, text="Airfoils", 
                      get=lambda: self.show_airfoils, set=self.set_show_airfoils)
            CheckBox (l,r,c+1, text="Use nick name", colSpan=3,
                        obj=self, prop=Item_Airfoils.airfoil_use_nick,
                        hide=lambda: not self.show_airfoils,
                        toolTip=f"Airfoils nick name is defined in diagram '{Diagram_Airfoils.name}'")
            r += 1
            CheckBox (l,r,c+1, text="Also blended", 
                        get=lambda: self.show_strak, set=self.set_show_strak,
                        hide=lambda: not self.show_airfoils)    

            l.setColumnMinimumWidth (0,70)
            l.setColumnStretch (3,5)
            l.setRowStretch    (r+1,2)
                     
            self._section_panel = Edit_Panel (title=self.name, layout=l, auto_height=True, 
                                              switchable  = True,
                                              switched_on = lambda: self.show,  
                                              on_switched = lambda aBool: self.set_show(aBool))
        return self._section_panel 




class Item_Chord (Item_Abstract):
    """ 
    Diagram (Plot) Item for normed Chord along span
    """

    name        = "View Chord Distribution"                 # used for link and section header 
    title       = "Chord Distribution"                 
    subtitle    = ""                                 


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (Norm_Chord_Artist     (self, lambda: self.planform, show_legend=True))
        self._add_artist (WingSections_Artist   (self, lambda: self.planform, mode=mode.NORM_TO_SPAN, show=False, show_legend=True,
                                                 wingSection_fn=lambda: self.cur_wingSection))
        self._add_artist (Ref_Planforms_Artist  (self, lambda: self.planform, show_chord=True,
                                                 show_legend=True, show=False))
        self._add_artist (Flaps_Artist          (self, lambda: self.planform, mode=mode.NORM_TO_SPAN, show=False, show_legend=True))

        # connect signals from artists to app model notify
        self._setup_artists_slots()


    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.05)

        if not self.xLinked:
            self.viewBox.autoRange()

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
            self._section_panel = Edit_Panel (title=self.name, auto_height=True,
                                              switchable  = True,
                                              switched_on = lambda: self.show,  
                                              on_switched = lambda aBool: self.set_show(aBool))
        return self._section_panel 




class Item_Chord_Reference (Item_Abstract):
    """ 
    Diagram (Plot) Item for Chord Reference 
    """

    name        = "View Chord Reference"                    # used for link and section header 
    title       = "2. Chord Reference"                 
    subtitle    = ""                                 


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (Norm_Chord_Ref_Artist (self, lambda: self.planform, mode=mode.REF_TO_SPAN, show_legend=True))
        self._add_artist (WingSections_Artist   (self, lambda: self.planform, mode=mode.REF_TO_SPAN, show=False, show_legend=True,
                                                       wingSection_fn=lambda: self.cur_wingSection))
        self._add_artist (Flaps_Artist          (self, lambda: self.planform, mode=mode.REF_TO_SPAN, show=False, show_legend=True))

        # connect signals from artists to app model notify
        self._setup_artists_slots()


    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.setDefaultPadding(0.05)

        if not self.xLinked:
            self.viewBox.autoRange()
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
            self._section_panel = Edit_Panel (title=self.name, 
                                              switchable  = True,
                                              switched_on = lambda: self.show,  
                                              on_switched = lambda aBool: self.set_show(aBool))
        return self._section_panel 



class Item_VLM_Panels (Item_Abstract):
    """ 
    Diagram (Plot) Item to show the VLM panels of the planform  
    """

    name        = "View Paneling"                           # used for link and section header 
    title       = "Paneling"                 
    subtitle    = ""                                 


    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        # set margins (inset) of self - there is an extra column for color bar!
        self.setContentsMargins ( 0,30,10,10)

        # connect to app model signals to refresh - new wing is handled in Diagram_Abstract
        self.app_model.sig_paneling_changed.connect     (self.refresh)
        self.app_model.sig_vlm_opPoint_changed.connect  (self.refresh)
        self.app_model.sig_new_polars.connect           (self.refresh)
        self.app_model.sig_vlm_polar_changed.connect    (self.section_panel.refresh)    # cp checkbox enable/disable


    def _is_vlm_data_available (self) -> bool:
        """ check if VLM data is available to plot"""

        vlm_wing : VLM_Wing = self.wing._vlm_wing
        if vlm_wing is None:
            return False   
        return vlm_wing.has_polars() 


    @property
    def cur_vlm_opPoint (self) -> VLM_OpPoint | None: 
        """ returns the current VLM operating point - only if VLM polars are existing"""
        if self._is_vlm_data_available():
            return self.app_model.cur_vlm_opPoint
        else:
            return None
    

    @override
    def plot_title(self, **kwargs):

        n_panels = self.wing.vlm_wing.n_panels
        return super().plot_title(subtitle=f"{n_panels} panels", **kwargs)
    

    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (VLM_Panels_Artist     (self, lambda: self.planform, 
                                                 opPoint_fn=lambda: self.cur_vlm_opPoint, 
                                                 show_legend=True))
        self._add_artist (WingSections_Artist   (self, lambda: self.planform, 
                                                 wingSection_fn=lambda: self.cur_wingSection,
                                                 show=True, show_legend=True, show_all_sections=True))  
        self._add_artist (Airfoil_Name_Artist   (self, lambda: self.planform, 
                                                 show=False, show_legend=False))

        # connect signals from artists to app model notify
        self._setup_artists_slots()


    @override
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        l : QGraphicsGridLayout = self.layout
        l.setColumnMinimumWidth ( 3, 50)              # ensure width for color keeps reserved

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
                        disable=lambda: not self._is_vlm_data_available())
            l.setRowStretch (r+1,3)

            self._section_panel = Edit_Panel (title=self.name, layout=l, auto_height=True,
                                              switchable  = True,
                                              switched_on = lambda: self.show,  
                                              on_switched = lambda aBool: self.set_show(aBool))
            
        return self._section_panel 


    def _edit_paneling (self):
        """ dialog to edit paneling parameters  """


        # show difference between original and paneled planform before dialog
        panels_artist : VLM_Panels_Artist = self._get_artist(VLM_Panels_Artist)[0]
        panels_artist.set_show_chord_diff (True)

        dialog = Dialog_Edit_Paneling (self.section_panel, self.wing.planform_paneled, 
                                       parentPos=(0.9,0.2), dialogPos=(0.0,0.5))  

        dialog.sig_paneling_changed.connect (self.app_model.notify_paneling_changed)

        # refresh will be done via signal from app

        dialog.exec()   

        panels_artist.set_show_chord_diff (False)



class Item_VLM_Result (Item_Abstract):
    """ 
    Diagram (Plot) Item to show result of aero calculation
    """

    name        = "View Aero Analysis"                      # used for link and section header 
    title       = ""                                        # will be dynamically set                 
    subtitle    = ""                                 

    def __init__(self, *args, **kwargs):

        self._vlm_opPoint_var   = OpPoint_Var.CL            # variable to plot along span
        self._is_alpha_fixed_max = False                    # opPoint is fixed at alpha_max 

        super().__init__(*args, **kwargs)

        self.setContentsMargins ( 0,30,50,10)

        # connect to app model signals to refresh - new wing is handled in Diagram_Abstract
        self.app_model.sig_vlm_opPoint_changed.connect  (self.refresh)
        self.app_model.sig_new_polars.connect           (self.refresh)
        self.app_model.sig_vlm_polar_reset.connect      (self.refresh)          # will result in new VLM calculation
        self.app_model.sig_polar_set_changed.connect    (self.section_panel.refresh)  # first step just refresh of panel


    def _is_vlm_data_available (self) -> bool:
        """ check if VLM data is available to plot"""

        vlm_wing : VLM_Wing = self.wing._vlm_wing
        if vlm_wing is None:
            return False   
        return vlm_wing.has_polars() 


    @property
    def vlm_polar (self) -> VLM_Polar:
        """ current VLM polar """
        return self.app_model.cur_vlm_polar


    @property
    def vlm_alpha (self) -> float:
        """ alpha for current vlm opPoint"""
        return self.app_model.cur_vlm_alpha

    def set_vlm_alpha (self, aVal : float):
        self.app_model.set_cur_vlm_alpha (aVal)


    @property
    def vlm_opPoint (self) -> VLM_OpPoint:
        """ current selected opPoint based on vta and alpha"""
        return self.app_model.cur_vlm_opPoint 


    def _format_polar_def (self, polar_def : Polar_Definition) -> str:
        """ format a polar definition for combobox like '400k N7 | 14,6m/s'"""
        return polar_def.name_with_v (self.wing.planform.chord_root)


    @property
    def polar_def_list (self) -> list[str]:
        """ polar definitions as list of formatted strings for combobox"""
        polar_defs = self.app_model.polar_definitions_T1
        return [self._format_polar_def (polar_def) for polar_def in polar_defs]


    @property
    def polar_def_name (self) -> str:
        """ display name of the current polar definition """
        polar_def = self.app_model.cur_polar_def
        return self._format_polar_def (polar_def) if polar_def is not None else None
    
        
    def set_polar_def_name (self, new_polar_def_name): 
        """ set polar definition of wingSection at root by name"""
        polar_defs = self.app_model.polar_definitions_T1
        for polar_def in polar_defs:
            polar_def_name = self._format_polar_def (polar_def)
            if polar_def_name == new_polar_def_name: 
                self.app_model.set_cur_polar_def (polar_def)


    @property
    def alpha_fixed_to_max (self)-> bool:
        """ opPoint is fixed to alpha_max"""
        return self.app_model.vlm_alpha_fixed_to_max

    def set_alpha_fixed_to_max (self, aBool : bool):
        self.app_model.set_vlm_alpha_fixed_to_max (aBool)
        

    @property
    def vlm_opPoint_var (self) -> OpPoint_Var:
        """ variable to show in diagram"""
        return self._vlm_opPoint_var

    def set_vlm_opPoint_var (self, aVal : OpPoint_Var):
        self._vlm_opPoint_var = aVal

        artist : VLM_Result_Artist = self._get_artist (VLM_Result_Artist)[0]
        artist.set_opPoint_var (aVal)

        self.refresh()
        self._set_yRange ()


    @property
    def use_viscous_loop (self) -> OpPoint_Var:
        """ viscous loop for aero calculation"""
        return self.vlm_polar.use_viscous_loop

    def set_use_viscous_loop (self, aBool : bool):
        self.vlm_polar.set_use_viscous_loop (aBool)
        self.refresh()


    @property
    def opPoint_var_list (self) -> OpPoint_Var:
        """ variable to show in diagram"""
        return [OpPoint_Var.LIFT, OpPoint_Var.CL, OpPoint_Var.ALPHA]


    # ---------------

    def plot_title(self, **kwargs):

        opPoint_label = self.vlm_opPoint.name if self.vlm_opPoint else ""
        return super().plot_title(title = f"{self.vlm_opPoint_var} along Span", subtitle=opPoint_label, **kwargs)
    

    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (VLM_Result_Artist     (self, lambda:self.planform, show_legend=True,
                                                        polar_fn=lambda: self.vlm_polar,
                                                        opPoint_fn=lambda: self.vlm_opPoint))

    def setVisible (self, aBool):
        """ Qt overloaded to signal parent """

        # overridden to check for Worker 
        super().setVisible (aBool and Worker.ready)


    def setup_viewRange (self):
        """ define view range of this plotItem"""

        if not self.xLinked:
            self.viewBox.autoRange()

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

        if self.vlm_opPoint_var == OpPoint_Var.ALPHA:
            range_max = 10 if max_val < 10 else (int (max_val/5) + 1) * 5
            range_min = 0  if min_val >= 0 else (int (min_val/5) - 1) * 5
        elif self.vlm_opPoint_var == OpPoint_Var.CL:
            range_max = 1  if max_val < 1  else (int (max_val/0.5) + 1) * 0.5
            range_min = 0  if min_val >= 0 else (int (min_val/0.5) - 1) * 0.5
        else:
            range_min = 0.0 if min_val > 0 else min_val
            range_max = max_val * 1.2 if max_val > 0 else 0

        # logger.debug (f"{self} set viewBox yRange {range_min} {range_max}")
        self.viewBox.setYRange (range_min, range_max)


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None and Worker.ready:   

            l = QGridLayout()
            r,c = 0, 0
            Label    (l,r,c, colSpan=4, get=f"Choose airfoil T1 polar of root", style=style.COMMENT)
            r += 1
            ComboBox (l,r,c, width=None, obj=self, prop=Item_VLM_Result.polar_def_name, colSpan=4,
                         options=lambda: self.polar_def_list)
            ToolButton (l,r,c+4, icon=Icon.ADD, set=self._add_polar_def,
                        disable=lambda: not self._is_vlm_data_available(), # wait until VLM ended
                        toolTip="Add a polar definition <br><br>" +
                                "Currently the polar has to differ in Re number not only Ncrit or Mach "+
                                "to be handled correctly by the VLM viscous calculation - sorry!")                              
            ToolButton (l,r,c+5, icon=Icon.DELETE, set=self._delete_polar_def,
                        disable=lambda: not self._is_vlm_data_available() or  
                                        len(self.polar_def_list) <= 1,           # wait until VLM ended
                        toolTip="Delete this polar definition")                              
            r += 1
            SpaceR   (l,r, height=10,stretch=0)
            r += 1
            Label    (l,r,c, colSpan=4, get=f"Define operating point", style=style.COMMENT)
            r += 1
            CheckBox (l,r,c, text="Set close to Alpha max", colSpan=4,
                        obj=self, prop=Item_VLM_Result.alpha_fixed_to_max,
                        disable=lambda: not self._is_vlm_data_available(),
                        toolTip="Operating point of wing is set to alpha_max of current polar")
            r +=1
            FieldF   (l,r,c, lab="Alpha", lim=(-20,20), step=0.5, width=60, unit="°", dec=1, 
                        obj=self, prop=Item_VLM_Result.vlm_alpha,
                        disable=lambda: self.alpha_fixed_to_max or not self._is_vlm_data_available()) 
            r += 1
            SpaceR   (l,r, height=10,stretch=0)
            r += 1
            Label    (l,r,c, colSpan=4, get=f"Diagram variable along span", style=style.COMMENT)
            r += 1
            Label    (l,r,c,   width=60, get="Variable")

            ComboBox (l,r,c+1, width=60, obj=self, prop=Item_VLM_Result.vlm_opPoint_var, 
                         options=self.opPoint_var_list, colSpan=3,
                         disable=lambda: not self._is_vlm_data_available(),
                         toolTip="Choose variable to show along span in diagram")

            r += 1
            l.setRowStretch (r,1)

            # dev mode 
            # r += 1
            # CheckBox (l,r,c, text="Viscous loop (dev)", colSpan=3,
            #             obj=self, prop=Item_VLM_Result.use_viscous_loop)
            # r += 1
            # Button   (l,r,c, text="Export Polar", width=100, set=self._export_polar_opPoint)

            l.setColumnStretch (3,2)
            l.setColumnMinimumWidth (0,50)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height =260,
                                              switchable  = True,
                                              switched_on = lambda: self.show,  
                                              on_switched = lambda aBool: self.set_show(aBool))

            # patch Panel Aero into head of panel 

            if Worker.ready:
                l_head = self._section_panel._head.layout()
                Label  (l_head, get=f"by Panel Aero", style=style.COMMENT, fontSize=size.SMALL,
                        align=Qt.AlignmentFlag.AlignBottom)


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
                                              switchable  = True,
                                              switched_on = lambda: self.show,  
                                              on_switched = lambda aBool: self.set_show(aBool))
        return self._section_panel 


    def _edit_polar_def (self, polar_def: Polar_Definition = None, silent: bool=False):
        """ edit polar definition - currently only re number"""

        if polar_def is None:   
            polar_def = self.app_model.cur_polar_def

        chord = self.wing.planform.chord_root

        # currently only mini dialog for re number
        dialog = Calc_Reynolds_Dialog (self.section_panel, re_asK=polar_def.re_asK, fixed_chord=chord,
                                       title="Set Reynolds Number for Polar Definition",
                                       parentPos=(1.1, 0.5), dialogPos=(0,0.5))

        dialog.exec()     

        if dialog.has_been_set:
            polar_def.set_re_asK (dialog.re_asK)

        # sort polar definitions ascending re number 
        self.wing.polar_definitions.sort (key=lambda aDef : aDef.re)

        if not silent:
            self.app_model.notify_polar_definitions_changed ()


    def _delete_polar_def (self):
        """ delete current polar definition"""

        if len (self.wing.polar_definitions) <= 1:
            return                                  # cannot delete last polar definition

        polar_def = self.app_model.cur_polar_def
        self.wing.polar_definitions.remove (polar_def)

        # set new current - will signal change 
        new_cur = self.wing.polar_definitions[-1]
        self.app_model.set_cur_polar_def (new_cur)                


    def _add_polar_def (self):
        """ add a new polar definition"""

        # increase re number for the new polar definition
        if self.wing.polar_definitions:
            new_polar_def  = deepcopy (self.wing.polar_definitions[-1])
            new_polar_def.set_is_mandatory (False)                  # parent could have been mandatory
            new_polar_def.set_re (new_polar_def.re + 100000)
            new_polar_def.set_active(True)
        else: 
            new_polar_def = Polar_Definition()

        self.wing.polar_definitions.append (new_polar_def)

        # open edit dialog for new def 
        self._edit_polar_def (polar_def=new_polar_def, silent=True)     # silent to avoid double notify

        # set new current - will signal change 
        self.app_model.set_cur_polar_def (new_polar_def)                



    def _export_polar_opPoint (self):
        """ export a wing polar and current opPoint to csv"""

        fileName = f"{self.vlm_polar.name}.csv"

        filters  = "Polar csv files (*.csv)"
        newPathFilename, _ = QFileDialog.getSaveFileName(self.section_panel, caption="Export Wing Polar to csv", 
                                                         directory=fileName, filter=filters)
        if newPathFilename: 
            self.vlm_polar.export_to_csv (newPathFilename)

        fileName = f"{self.vlm_polar.name} alpha={self.vlm_opPoint.alpha:.1f}.csv"

        filters  = "Polar csv files (*.csv)"
        newPathFilename, _ = QFileDialog.getSaveFileName(self.section_panel, caption="Export opPoint to csv", 
                                                         directory=fileName, filter=filters)
        if newPathFilename: 
            self.vlm_opPoint.export_to_csv (newPathFilename)




class Item_Wing (Item_Abstract):
    """ 
    Diagram (Plot) Item for a complete Wing
    """

    name = "View Wing"                                          # used for link and section header 

    def __init__(self, *args, **kwargs):

        self._show_full_wing = True

        super().__init__(*args, **kwargs)


    @override
    def plot_title(self, **kwargs):

        title = f'<span style="font-size: 18pt; color: whitesmoke">{self.wing.name}</span>'
        description = self.wing.description.replace ("\n", "<br/>")      # textItem needs <br>
        powered = f"<span style='font-size: 8pt; color: #606060'>Created with PC2</span>"
        description = f"{description}{"<br/>" if description else ""}{powered}"

        return super().plot_title (title=title, subtitle = description, **kwargs)


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""

        # plot right and left wing 

        self._add_artist (Planform_Artist       (self, lambda: self.planform, mode=mode.WING_RIGHT, as_contour=True))
        self._add_artist (Ref_Line_Artist       (self, lambda: self.planform, mode=mode.WING_RIGHT))
        self._add_artist (Flaps_Artist          (self, lambda: self.planform, mode=mode.WING_RIGHT))
        self._add_artist (WingSections_Artist   (self, lambda: self.planform, mode=mode.WING_RIGHT, show=False))
        self._add_artist (Airfoil_Name_Artist   (self, lambda: self.planform, mode=mode.WING_RIGHT, show=False))
        self._add_artist (Neutral_Point_Artist  (self, lambda: self.planform, mode=mode.WING_RIGHT, show=False))

        if self.show_full_wing:
            self._add_artist (Planform_Artist       (self, lambda: self.planform, mode=mode.WING_LEFT, as_contour=True))
            self._add_artist (Ref_Line_Artist       (self, lambda: self.planform, mode=mode.WING_LEFT))
            self._add_artist (Flaps_Artist          (self, lambda: self.planform, mode=mode.WING_LEFT))
            self._add_artist (WingSections_Artist   (self, lambda: self.planform, mode=mode.WING_LEFT, show=False))
            self._add_artist (Neutral_Point_Artist  (self, lambda: self.planform, mode=mode.WING_LEFT, show=False))

        # switch off mouse helper 

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
    def show_full_wing (self) -> bool:
        return self._show_full_wing
    
    def set_show_full_wing (self, aBool : bool):
        self._show_full_wing = aBool

        # switch off existing artists
        for artist in self._artists:
            artist.set_show (False)

        # rebuild artists with new mode
        self._artists.clear()
        if self.scene():
            try:                            # remove any connected scene mouse clickedsignals 
                self.scene().sigMouseClicked.disconnect ()
            except:
                pass
        self.setup_artists()
        self.refresh()


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
    def airfoil_use_nick (self) -> bool:
        return self.wing.airfoil_use_nick
    def set_airfoil_use_nick (self, aBool : bool):
        self.wing.set_airfoil_use_nick (aBool)
        self.refresh()

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
            CheckBox (l,r,c, text="Full Wing", colSpan=2, 
                        get=lambda: self.show_full_wing, set=self.set_show_full_wing) 
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
            CheckBox (l,r,c, text="Neutral Point (geometric)", colSpan=3, 
                      get=lambda: self.show_np, set=self.set_show_np) 
            r += 1
            CheckBox (l,r,c, text="Airfoils", 
                      get=lambda: self.show_airfoils, set=self.set_show_airfoils) 
            CheckBox (l,r,c+1, text="Use nick name",  
                        obj=self, prop=Item_Wing.airfoil_use_nick,
                        hide=lambda: not self.show_airfoils,
                        toolTip=f"Airfoils nick name is defined in diagram '{Diagram_Airfoils.name}'")
            r += 1
            l.setColumnMinimumWidth (0,70)
            l.setColumnMinimumWidth (1,80)
            l.setColumnStretch (3,5)

            self._section_panel = Edit_Panel (title=self.name, layout=l, auto_height=True, 
                                              switchable  = True,
                                              switched_on = lambda: self.show,  
                                              on_switched = lambda aBool: self.set_show(aBool))

        return self._section_panel 





class Item_Wing_Airfoils (Item_Abstract):
    """ 
    Diagram (Plot) Item for airfoils of a wing
    """

    name        = "View Airfoils"
    title       = "Airfoils"                       # title of diagram item
    subtitle    = ""


    @override
    def refresh(self):
        """ override to set legend cols"""
        super().refresh()
        if self.legend:
            self.legend.setColumnCount (3)


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""

        self._add_artist (Airfoil_Artist    (self, lambda: self.planform, show_legend=True))


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
    def airfoil_use_nick (self) -> bool:
        return self.wing.airfoil_use_nick
    def set_airfoil_use_nick (self, aBool : bool):
        self.wing.set_airfoil_use_nick (aBool)
        self.refresh()

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
                        obj=self, prop=Item_Airfoils.airfoil_use_nick,
                        toolTip=f"Airfoils nick name is defined in diagram '{Diagram_Airfoils.name}'")
            r += 1
            l.setColumnStretch (3,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, auto_height=True, 
                                              switchable  = True,
                                              switched_on = lambda: self.show,  
                                              on_switched = lambda aBool: self.set_show(aBool))

        return self._section_panel 





class Item_Wing_Data (Item_Abstract):
    """ 
    Diagram (Plot) Item to show wing data 
    """

    name        = "View Wing data"
    title       = "Wing Data"                                   # title of diagram item
    subtitle    = ""


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

        self._add_artist (Wing_Data_Artist (self, lambda: self.planform, show_legend=True))


    def _add_text(self):
        
        graphicsLayout = pg.GraphicsLayout (self)
        layout : QGraphicsGridLayout = graphicsLayout.layout

        text = Label (layout,0,0, get="Hallo", width=100)
        layoutItem = self.scene ().addWidget (text)
        layout.addItem (layoutItem,0,0,1,1)
        self.addItem (layout)



class Item_Airfoils (Item_Abstract):
    """ 
    Diagram (Plot) Item for airfoils of a planform
    """

    name        = "View Airfoils"                                   
    title       = "Airfoils"                       # title of diagram item
    subtitle    = ""


    @override
    def setup_artists (self):
        """ create and setup the artists of self"""

        self._add_artist (Airfoil_Artist    (self, lambda: self.planform, show_legend=True))


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
    def airfoil_use_nick (self) -> bool:
        return self.wing.airfoil_use_nick
    def set_airfoil_use_nick (self, aBool : bool):
        self.wing.set_airfoil_use_nick (aBool)
        self.refresh()

    @property 
    def airfoil_nick_prefix (self) -> str:
        return self.wing.airfoil_nick_prefix
    def set_airfoil_nick_prefix (self, aVal : str):
        self.wing.set_airfoil_nick_prefix (aVal)
        self.refresh()
    
    @property 
    def airfoil_nick_base (self) -> str:
        return self.wing.airfoil_nick_base
    def set_airfoil_nick_base (self, aVal : str):
        self.wing.set_airfoil_nick_base (aVal)
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
                        obj=self, prop=Item_Airfoils.airfoil_use_nick) 
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

            self._section_panel = Edit_Panel (title=self.name, layout=l, auto_height=True,
                                              switchable  = True,
                                              switched_on = lambda: self.show,  
                                              on_switched = lambda aBool: self.set_show(aBool))
        return self._section_panel 




class Item_Polars (Item_Abstract):
    """ 
    Diagram (Plot) Item for polars 
    """

    name        = "Polar"                               # used for link and section header 
    title       = None 
    subtitle    = None                                  # optional subtitle 

    sig_xyVars_changed           = pyqtSignal()         # airfoil data changed in a diagram 

    def __init__(self, *args, iItem= 1, **kwargs):

        self._iItem  = iItem
        self._xyVars = None
        self._xyVars_show_dict = {}                     # dict of xyVars shown up to now 

        self._title_item2 = None                        # a second 'title' for x-axis 
        self._autoRange_not_set = True                  # to handle initial no polars to autoRange 
        self._next_btn    = None
        self._prev_btn    = None 

        self.name = f"{self.name} {iItem}"

        super().__init__(*args, **kwargs)

        # buttons for prev/next diagram 

        p = Icon_Button (Icon.COLLAPSE, parent=self, itemPos=(0.5,0), parentPos=(0.5,0), offset=(0,0) )
        p.clicked.connect(self._btn_prev_next_clicked)  
        self._prev_btn = p

        p = Icon_Button (Icon.EXPAND, parent=self, itemPos=(0.5,1), parentPos=(0.5,1), offset=(0,5) )
        p.clicked.connect(self._btn_prev_next_clicked)
        self._next_btn = p

        self._refresh_prev_next_btn ()

        # set margins (inset) of self 
        self.setContentsMargins ( 0,10,10,20)

        # connect to polar changed signal
        self.app_model.sig_new_polars.connect  (self._on_new_polars)
        self.app_model.sig_polar_set_changed.connect  (self.refresh)


    @override
    def _settings (self) -> dict:
        """ return dictionary of self settings"""
        d = {}

        # axes x, y variables
        toDict (d, "xyVars", (str(self.xVar), str(self.yVar)))
        return d


    @override
    def _set_settings (self, d : dict):
        """ set settings of self from dict """

        # axes x, y variables
        xyVars = d.get('xyVars', None)                          
        if xyVars is not None:
            self.set_xyVars (xyVars)
            self._refresh_artist_xy ()
            self.setup_viewRange ()


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


    def _btn_prev_next_clicked (self, button : Icon_Button):
        """ prev or next buttons clicked"""

        step = -1 if button.icon_name == Icon.COLLAPSE else 1

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
                self._refresh_artist_xy ()                             # draw new polar
                self.sig_xyVars_changed.emit()                      # update view panel 
            self._refresh_prev_next_btn ()                          # update visibility of buttons
        except :
            pass


    def _btn_var_clicked (self, axis, pos : QPoint):
        """ slot - polar var button in diagram clicked - show menu list of variables"""
        menu = QMenu()
       
        # Build popup menu 
        for v in var.list_small():
            action = QAction (v.value, menu)
            action.setCheckable (True)
            if axis == "y":
                action.setChecked (v == self.yVar)
                action.triggered.connect (lambda  checked, v=v: self.set_yVar (v))
            else:
                action.setChecked (v == self.xVar)
                action.triggered.connect (lambda  checked, v=v: self.set_xVar (v))
            menu.addAction (action)
        menu.exec (pos)


    @override
    def plot_title (self):
        """ override to have 'title' at x,y axis"""

        # remove existing title item 
        if isinstance (self._title_item, pg.LabelItem):
            self.scene().removeItem (self._title_item)          # was added directly to the scene via setParentItem
        if isinstance (self._title_item2, pg.LabelItem):
            self.scene().removeItem (self._title_item2)         # was added directly to the scene via setParentItem
       
        # y-axis
        p = Text_Button (self.yVar, parent=self, color=QColor(Artist.COLOR_HEADER), size=f"{Artist.SIZE_HEADER}pt",
                         itemPos=(0,0), parentPos=(0,0), offset=(60,5))
        p.clicked.connect (lambda pos: self._btn_var_clicked("y",pos)) 
        p.setToolTip (f"Select polar variable for y axis")           
        self._title_item = p

        # x-axis
        p = Text_Button (self.xVar, parent=self, color=QColor(Artist.COLOR_HEADER), size=f"{Artist.SIZE_HEADER}pt",
                         itemPos=(1,1), parentPos=(1,1), offset=(-15,-50))
        p.setToolTip (f"Select polar variable for x axis")           
        p.clicked.connect (lambda pos: self._btn_var_clicked("x",pos))            
        self._title_item2 = p


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


    def _refresh_artist_xy (self): 
        """ refresh polar artist with new diagram variables"""

        artist : Polar_Artist = self._artists [0]
        artist.set_xyVars (self._xyVars)

        self.plot_title()


    def _on_new_polars (self):
        """ slot - new polars available - refresh diagram"""

        self._viewRange_set = False                 # ensure new scaling 
        self.refresh()  


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

        self._refresh_artist_xy ()
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

        self._refresh_artist_xy ()
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
    def setup_artists (self):
        """ create and setup the artists of self"""

        a = Polar_Artist     (self, lambda: self.planform, xyVars=self._xyVars, 
                              show_legend=True)
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

        i = Item_Planform (self,self.app_model)
        self._add_item (i, 0, 0, rowStretch=3)

        i = Item_Chord    (self, self.app_model, show=False)
        self._add_item (i, 1, 0, rowStretch=2)
        i.set_desired_xLink_name (Item_Planform.name)
        i.set_xLinked (True)

        i = Item_Chord_Reference  (self, self.app_model, show=False)
        self._add_item (i, 2, 0, rowStretch=2)
        i.set_desired_xLink_name (Item_Planform.name)
        i.set_xLinked (True)


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
        """ additional section panel with common settings"""

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

            self._general_panel = Edit_Panel (title="Common Options", layout=l, auto_height=True,
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
            CheckBox   (l,r,c, text="Another PC2 Planform", 
                        hide = lambda: bool(self.wing.reference_pc2_file))
            Button     (l,r,c+1, text="Select", width=50, colSpan=4,
                        set=self._open_planform_ref_pc2, toolTip="Select another PC2 Planform as reference",
                        hide = lambda: bool(self.wing.reference_pc2_file))

            CheckBox   (l,r,c, text=lambda: self.wing.planform_ref_pc2_name, 
                        get=lambda: self.show_ref_pc2, set=self.set_show_ref_pc2, 
                        hide = lambda: not bool(self.wing.reference_pc2_file)) 
            ToolButton (l,r,c+2, icon=Icon.OPEN, 
                        set=self._open_planform_ref_pc2, toolTip="Open new PC2 Planform",
                        hide = lambda: not bool(self.wing.reference_pc2_file))
            ToolButton (l,r,c+3, icon=Icon.DELETE, 
                        set=self._remove_planform_ref_pc2, toolTip="Remove PC2 Planform",
                        hide = lambda: not bool(self.wing.reference_pc2_file))


            # toggle fields for background image
            r += 1
            CheckBox   (l,r,c, text="Background Image", get=False, 
                        hide = lambda: bool(self.wing.background_image.filename)) 
            Button     (l,r,c+1, text="Select", width=50, colSpan=4,
                        set=self._open_background_image, toolTip="Open background image as reference",
                        hide = lambda: bool(self.wing.background_image.filename))

            CheckBox   (l,r,c, text=lambda: self.wing.background_image.filename,  
                        get=lambda: self.background_image_artist.show, 
                        set=self.background_image_artist.set_show, 
                        hide = lambda: not bool(self.wing.background_image.filename)) 
            ToolButton (l,r,c+1, icon=Icon.EDIT, 
                        set=self._edit_background_image, toolTip="Edit background image settings",
                        hide = lambda: not bool(self.wing.background_image.filename))
            ToolButton (l,r,c+2, icon=Icon.OPEN, 
                        set=self._open_background_image, toolTip="Open new background image",
                        hide = lambda: not bool(self.wing.background_image.filename))
            ToolButton (l,r,c+3, icon=Icon.DELETE, 
                        set=self._remove_background_image, toolTip="Remove background image",
                        hide = lambda: not bool(self.wing.background_image.filename))

            l.setColumnStretch (0,3)

            self._section_panel = Edit_Panel (title="Reference Planforms", layout=l, auto_height=True,
                                              switchable=True, 
                                              switched_on=False, 
                                              on_switched=self.set_show_ref_planforms)

        return self._section_panel 


    @property 
    def export_panel (self) -> Edit_Panel | None:
        """ additional section panel with export buttons"""

        if self._export_panel is None:

            l = QGridLayout()
            r,c = 0, 0
            Button  (l,r,c, text="Export DXF", width=100, set=self.export_dxf)
            l.setColumnStretch (2,2)
            r += 1
            Button      (l,r,c, text="Export CSV", width=100, set=self.export_csv)

            self._export_panel = Edit_Panel (title="Export", layout=l,  
                                             auto_height=True, main_margins = (10, 5,10, 10),
                                             switchable=False, switched_on=True)
        return self._export_panel 


    def export_dxf (self):
        """open export planform to dxf dialog """

        dialog = Dialog_Export_DXF (self, self.wing, parentPos=(0.2,0.7), dialogPos=(0,1))  
        dialog.exec()     


    def export_csv (self):
        """ export wing to csv file"""

        dialog = Dialog_Export_CSV (self, self.wing, parentPos=(0.2,0.7), dialogPos=(0,1))  
        dialog.exec() 


    def _open_planform_ref_pc2 (self):
        """ open reference pc2 file """

        filters  = "PlanformCreator2 files (*.pc2)"
        newPathFilename, _ = QFileDialog.getOpenFileName(self, filter=filters,
                                                         directory=self.app_model.workingDir,
                                                         caption="Open PlanformCreator file")

        if newPathFilename: 
            self.wing.set_reference_pc2_file (newPathFilename)
            self.set_show_ref_pc2 (True)
            self.refresh ()  


    def _remove_planform_ref_pc2 (self):
        """ remove reference pc2 file """

        self.wing.set_reference_pc2_file (None)
        self.set_show_ref_pc2 (False)
        self.refresh ()  

    
    def _open_background_image (self):
        """ open background image file  """

        filters  = "Image files (*.png *.jpg *.bmp)"
        newPathFilename, _ = QFileDialog.getOpenFileName(self, filter=filters, 
                                                         directory=self.app_model.workingDir,
                                                         caption="Open background image")

        if newPathFilename: 
            self.wing.background_image.set_pathFilename (newPathFilename)
            self._edit_background_image ()  
            self.background_image_artist.set_show(True)


    def _remove_background_image (self):
        """ remove background image file  """

        self.wing.background_image.set_pathFilename (None)
        self.background_image_artist.set_show(False)
        self.refresh ()  


    def _edit_background_image (self):
        """ edit settings of background image   """

        self.wing.background_image._qimage = None

        dialog = Dialog_Edit_Image (self.section_panel, self.wing.background_image, 
                                    parentPos=(1.1,0.5), dialogPos=(0,0.5)) 
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

        item = Item_Making_Of_Welcome (self,self.app_model)
        self._add_item (item, 0, 0)

        item = Item_Making_Of_Chord (self, self.app_model)
        self._add_item (item, 0, 1)

        item = Item_Making_Of_Chord_Reference (self, self.app_model)
        self._add_item (item, 0, 2)

        item = Item_Making_Of_Planform (self, self.app_model)
        self._add_item (item, 1, 0, colspan=2)

        item = Item_Making_Of_Paneled (self, self.app_model)
        self._add_item (item, 1, 2)

        self.graph_layout.setRowStretchFactor (0,2)
        self.graph_layout.setRowStretchFactor (1,3)

        self.graph_layout.setColumnStretchFactor (0,1)
        self.graph_layout.setColumnMinimumWidth (0,400)
        
        self.graph_layout.setColumnStretchFactor (1,3)
        self.graph_layout.setColumnStretchFactor (2,3)

    # --- view section panels ---------------------------------------------------

    @property
    def section_panel (self) -> Edit_Panel:
 
        if self._section_panel is None:

            l = QGridLayout()
            r,c = 0, 0
            CheckBox (l,r,c, text="Show mouse helper", 
                      get=lambda: self.show_mouse_helper, set=self.set_show_mouse_helper) 
            r += 1
            Label    (l,r,c, get="Drag the little helper points\nto modify the geometry. ",
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

            self._section_panel = Edit_Panel (title="Diagram Options", layout=l, auto_height=True,
                                              switchable=False)
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

        item = Item_Wing (self, self.app_model)
        self._add_item (item, 0, 0, colspan=2)

        item = Item_Wing_Data (self, self.app_model)
        self._add_item (item, 1, 0)

        item = Item_Wing_Airfoils (self, self.app_model)
        self._add_item (item, 1, 1)

        self.graph_layout.setColumnStretchFactor (0,2)
        self.graph_layout.setColumnStretchFactor (1,4)

        self.graph_layout.setRowStretchFactor (0,4)
        self.graph_layout.setRowStretchFactor (1,3)




class Diagram_Airfoils (Diagram_Abstract):
    """    
    Diagram view to show/plot airfoil diagrams - Container for diagram items 
    """

    name   = "Airfoils"                                     # will be shown in Tabs 

    def __init__(self, *args, **kwargs):

        self._panel_polar   = None 
        self._panel_common  = None                          # panel with general settings  
        self._panel_export  = None

        self._show_operating_points = False                 # show polars operating points 
        self._show_strak    = False                         # show blended airfoils
        self._min_re_asK    = 10                            # minimum re number / 1000 to plot 
        self._apply_min_re  = True                          # activate min re number 

        super().__init__(*args, **kwargs)

        self._apply_min_re_to_artists ()

        # connect to polar set changed signal
        self.app_model.sig_polar_set_changed.connect  (self._viewPanel.refresh)

    # -------------

    @property
    def polar_defs (self) -> list [Polar_Definition]:
        """ actual polar definitions"""
        return self.wing.polar_definitions

    @property
    def chord_root (self) -> float:
        """ chord at root section"""
        return self.wing.planform.chord_root


    @override
    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """

        r = 0 

        self._add_item (Item_Airfoils (self, self.app_model), r, 0, colspan=2,  rowStretch=2)

        if Worker.ready:
            r += 1

            # create Polar items with init default values from  

            item = Item_Polars (self, self.app_model, iItem=1, show=False)
            item._set_settings ({"xyVars" : (var.CD,var.CL)})
            self._add_item (item, r, 0,  rowStretch=3)

            item = Item_Polars (self, self.app_model, iItem=2, show=False)
            item._set_settings ({"xyVars" : (var.CL,var.GLIDE)})
            self._add_item (item, r, 1,  rowStretch=3)
 

    @override
    def create_view_panel (self):
        """ 
        creates a view panel to the left of at least one diagram item 
        has a section_panel
        """
        # override to add additional general settings panel on top 

        super().create_view_panel ()

        layout : QVBoxLayout = self._viewPanel.layout()

        layout.insertWidget (0, self.panel_common, stretch=0)
        layout.insertWidget (2, self.panel_polar, stretch=0)

        # export panel at bottom 

        layout.addStretch   (3)
        layout.addWidget    (self.panel_export, stretch=0)


    @property
    def show_polars (self) -> bool:
        """ show polar diagrams """
        return self._show_item (Item_Polars)

    def set_show_polars (self, aBool : bool, silent=False):
        self._set_show_item (Item_Polars, aBool, silent=silent)


    @property 
    def show_operating_points (self) -> bool:
        """ show polar operating points """
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
        """ minimum re number / 1000 to plot  """
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
        self.panel_polar.refresh()
        self._apply_min_re_to_artists ()


    def _apply_min_re_to_artists (self):
        """ tell artists to apply min_re - update view range """
        artist : Polar_Artist
        for artist in self._get_artist (Polar_Artist):
            artist.set_min_re_asK (self._min_re_asK if self.apply_min_re else 0) 

        for item in self._get_items (Item_Polars):
            item.setup_viewRange ()


    @property 
    def panel_common (self) -> Edit_Panel | None:
        """ additional section panel with common settings"""

        if self._panel_common is None:

            l = QGridLayout()
            r,c = 0, 0
            CheckBox (l,r,c, text="Show blended airfoils", 
                      get=lambda: self.show_strak, set=self.set_show_strak) 
            r += 1
            l.setColumnStretch (3,2)

            self._panel_common = Edit_Panel (title="Common Options", layout=l, auto_height=True,
                                              switchable=False, switched_on=True)
        return self._panel_common 


    @property
    def panel_polar (self) -> Edit_Panel:
        """ return polar extra panel to admin polar definitions and define polar diagrams"""

        if self._panel_polar is None:
        
            l = QGridLayout()
            r,c = 0, 0

            Label (l,r,c, colSpan=6, get="Polar definitions for Root", style=style.COMMENT) 
            r += 1

            # helper panel for polar definitions 

            p = Panel_Polar_Defs (self, lambda: self.polar_defs, 
                                  auto_height=True, width=(None,250),
                                  chord_fn=lambda: self.chord_root)

            p.sig_polar_def_changed.connect (self.app_model.notify_polar_definitions_changed)

            l.addWidget (p, r, c, 1, 6)
            l.setRowStretch (r,1)

            # minimum re number to plot 

            r += 1
            CheckBox    (l,r,c, text="Exclude Re less than", colSpan=4,
                            obj=self, prop=Diagram_Airfoils.apply_min_re,
                            toolTip="Apply a minimum Re number for polars in the diagrams")
            FieldF      (l,r,c+4, width=60, step=10, lim=(1, 1000), unit="k", dec=0,
                            obj=self, prop=Diagram_Airfoils.min_re_asK,
                            hide=lambda: not self.apply_min_re)

            # polar diagrams variables setting 

            r += 1
            if Worker.ready:
                Label (l,r,c, colSpan=5, style=style.COMMENT, height=40,
                       get="To change polar variables, <br>click the axis labels in the diagram.")
                r += 1
                l.setRowStretch (r,3)

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
                l.setRowStretch (r,3)

            self._panel_polar = Edit_Panel (title="View Polars", layout=l, auto_height=True,
                                            switchable  = True, 
                                            switched_on = lambda: self.show_polars, 
                                            on_switched = lambda aBool: self.set_show_polars(aBool))

            # patch Worker version into head of panel 

            if Worker.ready:
                l_head = self._panel_polar._head.layout()
                Label  (l_head, get=f"{Worker.NAME} {Worker.version}", style=style.COMMENT, fontSize=size.SMALL,
                        align=Qt.AlignmentFlag.AlignBottom)

        return self._panel_polar 


    @property 
    def panel_export (self) -> Edit_Panel | None:
        """ additional section panel with export buttons"""

        if self._panel_export is None:

            l = QGridLayout()
            r,c = 0, 0
            Button      (l,r,c, text="Export Airfoils", width=100, set=self.export_airfoils)
            l.setColumnStretch (2,2)

            self._panel_export = Edit_Panel (title="Export", layout=l, 
                                             auto_height=True, main_margins = (10, 5,10, 10),
                                             switchable=False, switched_on=True)
        return self._panel_export 



    def export_airfoils (self):
        """open export airfoils of wing dialog """

        dialog = Dialog_Export_Airfoil (self, self.wing, parentPos=(0.2,0.7), dialogPos=(0,1))  
        dialog.exec()    




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


    @property
    def cur_vlm_opPoint (self) -> VLM_OpPoint:
        """ current selected opPoint based on vta and alpha"""
        return self.app_model.cur_vlm_opPoint
    

    def create_diagram_items (self):
        """ create all plot Items and add them to the layout """

        i = Item_VLM_Panels     (self, self.app_model, show=True)
        self._add_item (i, 0, 0, rowStretch=3)

        i = Item_Chord          (self, self.app_model, show=False)
        self._add_item (i, 1, 0, rowStretch=2)
        i.set_desired_xLink_name (Item_VLM_Panels.name)
        i.set_xLinked (True)

        i = Item_VLM_Result     (self, self.app_model, show=False)
        self._add_item (i, 2, 0, rowStretch=2)
        i.set_desired_xLink_name (Item_VLM_Panels.name)
        i.set_xLinked (True)

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
                layout.addWidget (item.section_panel,stretch=0)

        layout.insertWidget (0, self.general_panel, stretch=0)          # common settings
        layout.addStretch (1)
        layout.addWidget (self.export_panel)                            # export xflr5 panel 

        self._viewPanel = Container_Panel()
        self._viewPanel.setLayout (layout)


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



    @property 
    def general_panel (self) -> Edit_Panel | None:
        """ additional section panel with common settings"""

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

            self._general_panel = Edit_Panel (title="Common Options", layout=l, auto_height=True,
                                              switchable=False, switched_on=True)
        return self._general_panel 



    @property 
    def export_panel (self) -> Edit_Panel | None:
        """ additional section panel with export buttons"""

        if self._export_panel is None:

            l = QGridLayout()
            r,c = 0, 0
            Button      (l,r,c, text="Export Xflr5", width=100, set=self.export_xflr5)
            r += 1
            Button      (l,r,c, text="Export FLZ", width=100, set=self.export_flz)         
            SpaceC (l,c, width=20, stretch=0)
            c += 1
            Button      (l,r,c, text="Launch FLZ", width=100, set=self.launch_flz,hide= not os.name == 'nt')                                 # only Windows, Linux requires some extra investigations
            
            l.setColumnStretch (3,2)

            self._export_panel = Edit_Panel (title="Export", layout=l, 
                                             auto_height=True, main_margins = (10, 5,10, 10),
                                             switchable=False, switched_on=True)
        return self._export_panel 


    # --- public slots ---------------------------------------------------


    @override
    def on_wingSection_changed (self):
        """ slot to handle changed wing section data"""

        # overridden to ensure new strak (-> airfoil polar) when wingSection changed 
        if self.isVisible():
            self.wing.planform.wingSections.do_strak()
            super().refresh(also_viewRange=False)
   
        # overridden to ensure new strak (-> airfoil polar) when wingSection changed 
        if self.isVisible():
            self.wing.planform.wingSections.do_strak()
            super().refresh(also_viewRange=False)


    @override
    def on_wingSection_selected (self):
        """ slot to handle changed wing section data"""

        # overridden to ensure new strak (-> airfoil polar) when wingSection changed 
        if self.isVisible():
            self.wing.planform.wingSections.do_strak()

            # overridden to refresh only wingSection artist 
            for artist in self._get_artist (WingSections_Artist):
                artist.refresh ()


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

        self.wing.exporter_flz.do_it ()

        pathFileName = os.path.join (self.wing.exporter_flz.export_dir_abs, self.wing.exporter_flz.flz_filename) 
        try: 
            os.startfile(pathFileName, 'open')
        except: 
            message = "Could not launch FLZ_vortex on exported file: <br><br>" + \
                    pathFileName + \
                    "<br><br> Is FLZ_vortex neatly installed and associated with file extension '.flz'?"
            MessageBox.error (self,"Export FLZ", str(message))



# -------- Diagram Items -------------------------------------------------


class Item_Making_Of_Abstract (Item_Abstract):
    """ 
   Abstract Making Of Diagram (Plot) Item 
    """

    min_width   = 200                                   # min size needed - see below 
    min_height  = 100 

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.setContentsMargins (20,100,20,20)


    @override
    def _on_help_message (self, aArtist :Artist, aMessage: str | None):
        """ slot for help message signal of an artist. show it"""

        # no user help messages 
        pass



class Item_Making_Of_Planform (Item_Making_Of_Abstract):
    """ Making Of Diagram (Plot) Item for Planform  """

    title       = "3. Planform"                 
    subtitle    = "Chord Distribution and Reference are combined to form the shape. The result is scaled by span and chord.<br> " + \
                  "Finally a sweep angle is applied by shearing the planform."                         

    def setup_artists (self):
        self._add_artist (Planform_Artist     (self, lambda: self.planform))
        self._add_artist (Ref_Line_Artist     (self, lambda: self.planform, mode=mode.REF_TO_PLAN))
        self._add_artist (Planform_Box_Artist (self, lambda: self.planform))
        self._add_artist (WingSections_Artist (self, lambda: self.planform, show=False))
        self._add_artist (Flaps_Artist        (self, lambda: self.planform, show=False))

        # connect signals from artists to app model notify
        self._setup_artists_slots()

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

    title       = "4. Wing Analysis"                 
    subtitle    = "The planform is idealized by panels being the starting basis <br>" + \
                  "either for aerodynamic assessment of the lift reserves of the wing<br>" + \
                  "or the export to Xflr5 or FLZ_vortex for further processing."                         

    @override
    def setup_artists (self):
        """ create and setup the artists of self"""
        
        self._add_artist (VLM_Panels_Artist     (self, lambda: self.planform, opPoint_fn=None, show_legend=False))
        self._add_artist (WingSections_Artist   (self, lambda: self.planform, show=False))
        # connect signals from artists to app model notify
        self._setup_artists_slots()


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

    show_buttons = False
    show_coords  = False

    def __init__(self, *args, **kwargs):

        super().__init__(*args, **kwargs)

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

    title       = "1. Chord Distribution"                       # title of diagram item
    subtitle    = "Defines the chord along the span in a normalized system.<br>" + \
                  "Chord at root equals to 100%"

    def setup_artists (self):
        self._add_artist (Norm_Chord_Artist     (self, lambda: self.planform, mode=mode.NORM_NORM))
        self._add_artist (WingSections_Artist   (self, lambda: self.planform, mode=mode.NORM_NORM, show=False))

        # connect signals from artists to app model notify
        self._setup_artists_slots()


    @override
    def setup_viewRange (self):
        self.viewBox.autoRange ()                            
        self.viewBox.setXRange (-0.1, 1.2, padding=0.0)       
        self.viewBox.setYRange (  0, 1.2,  padding=0.1)       

        x_axis : pg.AxisItem = self.getAxis ("bottom")
        x_axis.setLabel (units="%")
        x_axis.setScale (100)

        y_axis : pg.AxisItem = self.getAxis ("left")
        y_axis.setLabel (units="%")
        y_axis.setScale (100)
        self.showGrid(x=False, y=False)



class Item_Making_Of_Chord_Reference (Item_Making_Of_Abstract):
    """ Making Of Diagram (Plot) Item for Chord distribution  """

    title       = "2. Chord Reference"                 
    subtitle    = "Describes how much of the chord is added to the leading<br>" + \
                  "and to the trailing edge in relation to the reference line."                         

    def setup_artists (self):
        self._add_artist (Norm_Chord_Ref_Artist (self, lambda: self.planform))
        self._add_artist (WingSections_Artist   (self, lambda: self.planform, mode=mode.REF_TO_NORM, show=False))
        self._add_artist (Flaps_Artist          (self, lambda: self.planform, mode=mode.REF_TO_NORM, show=False))

        # connect signals from artists to app model notify
        self._setup_artists_slots()

    @override
    def setup_viewRange (self):
        self.viewBox.autoRange ()                            
        self.viewBox.setXRange (-0.1, 1.2, padding=0.0)       
        self.viewBox.setYRange (-0.1, 1.2,  padding=0.0)       

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
        self._add_artist (Planform_Artist       (self, lambda: self.planform, mode=mode.WING_RIGHT, as_contour=True))
        self._add_artist (Ref_Line_Artist       (self, lambda: self.planform, mode=mode.WING_RIGHT))
        self._add_artist (Flaps_Artist          (self, lambda: self.planform, mode=mode.WING_RIGHT, show=False))
        self._add_artist (WingSections_Artist   (self, lambda: self.planform, mode=mode.WING_RIGHT, show=False))

        self._add_artist (Planform_Artist       (self, lambda: self.planform, mode=mode.WING_LEFT, as_contour=True))
        self._add_artist (Ref_Line_Artist       (self, lambda: self.planform, mode=mode.WING_LEFT))
        self._add_artist (Flaps_Artist          (self, lambda: self.planform, mode=mode.WING_LEFT, show=False))
        self._add_artist (WingSections_Artist   (self, lambda: self.planform, mode=mode.WING_LEFT, show=False))

        for artist in self._artists: artist.set_show_mouse_helper (False) 

        # connect signals from artists to app model notify
        self._setup_artists_slots()


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




class Panel_Polar_Defs (Edit_Panel):
    """ Panel to add, delete, edit polar definitions """

    name = None                                                 # suppress header

    sig_polar_def_changed = pyqtSignal()                        # polar definition changed 


    def __init__(self, *args,
                 chord_fn = None,                               # optional callable: chord length in mm
                 **kwargs):

        self._chord_fn = chord_fn

        # no margins 
        super().__init__(*args, main_margins=(0,0,0,0), panel_margins=(0,0,0,0), **kwargs)

    # ---------------------------------------------

    @property
    def polar_defs (self) -> list[Polar_Definition]: 
        return self.dataObject

    @property
    def chord (self) -> float|None:
        """ Returns the optional chord length in mm"""
        return self._chord_fn () if callable (self._chord_fn) else None


    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 

        for idef, polar_def in enumerate (self.polar_defs):

            #https://docs.python.org/3.4/faq/programming.html#why-do-lambdas-defined-in-a-loop-with-different-values-all-return-the-same-result
            w = CheckBox   (l,r,c  , width=20,  get=lambda p=polar_def: p.active, set=polar_def.set_active,
                            disable=lambda: len(self.polar_defs) == 1, 
                            toolTip="Show/Hide this polar in diagram")  
            w.sig_changed.connect (self._on_polar_def_changed)

            Field      (l,r,c+1, width=(80,None), get=lambda p=polar_def: p.name_with_v(self.chord))

            # either tool buttons 
            if not polar_def.is_mandatory: 
                ToolButton (l,r,c+2, icon=Icon.EDIT,   set=self.edit_polar_def,   id=idef,
                                toolTip="Change the settings of this polar definition")                              
                ToolButton (l,r,c+3, icon=Icon.DELETE, set=self.delete_polar_def, id=idef,
                                toolTip="Delete this polar definition",  
                                hide=lambda p=polar_def: (len(self.polar_defs) <= 1))           # no delete 
             # .. or info label 
            else:
                Label       (l,r,c+2, get=" Case", colSpan=3, style=style.COMMENT )

            r += 1

        if len (self.polar_defs) < Polar_Definition.MAX_POLAR_DEFS: #  and (not self.mode_optimize):
            ToolButton (l,r,c+1, icon=Icon.ADD,  
                            toolTip="Add a new polar definition",  
                            set=self.add_polar_def)

        l.setColumnStretch (c+1,2)
        l.setColumnMinimumWidth (c+2,20)

        return l 


    def edit_polar_def (self, id : int = None, polar_def : Polar_Definition = None):
        """ edit polar definition with index idef"""


        if isinstance (id, int):
            polar_def = self.polar_defs[id]

        diag = Polar_Definition_Dialog (self, polar_def, 
                                        parentPos=(1.1, 0.5), dialogPos=(0,0.5), fixed_chord=self.chord)
        diag.exec()

        # sort polar definitions ascending re number 
        self.polar_defs.sort (key=lambda aDef : aDef.re)

        self._on_polar_def_changed ()


    def delete_polar_def (self, id : int):
        """ delete polar definition with index idef"""

        # at least one polar def needed
        if len(self.polar_defs) <= 1: return 

        del self.polar_defs[id]

        self._on_polar_def_changed ()


    def add_polar_def (self):
        """ add a new polar definition"""

        # increase re number for the new polar definition
        if self.polar_defs:
            new_polar_def  = deepcopy (self.polar_defs[-1])
            new_polar_def.set_is_mandatory (False)                  # parent could have been mandatory
            new_polar_def.set_re (new_polar_def.re + 100000)
            new_polar_def.set_active(True)
        else: 
            new_polar_def = Polar_Definition()

        self.polar_defs.append (new_polar_def)

        # open edit dialog for new def 

        self.edit_polar_def (polar_def=new_polar_def)


    def _on_polar_def_changed (self):
        """ handle changed polar def - inform parent"""

        # ensure if only 1 polar def, this has to be active 
        if len(self.polar_defs) == 1 and not self.polar_defs[0].active:
            self.polar_defs[0].set_active(True)

        # ensure local refresh - as global will only watch for active polars 
        self.refresh()

        # signal parent - which has to refresh self to apply changed items 
        self.sig_polar_def_changed.emit()


    @override
    def refresh(self, reinit_layout=False):
        """ refreshes all Widgets on self """

        super().refresh(reinit_layout=True)                 # always reinit layout to reflect changed polar defs

