#!/usr/bin/env pythonbutton_color
# -*- coding: utf-8 -*-

"""  

Diagram (items) for airfoil

"""

import logging

from base.widgets           import * 
from base.diagram           import * 
from base.panels            import Container_Panel, MessageBox

from PyQt6.QtWidgets        import QFileDialog, QGraphicsLayoutItem

# from model.airfoil          import Airfoil

from model.airfoil          import Airfoil
from model.polar_set        import *
from model.xo2_driver       import Worker

from airfoil_ui_panels      import Panel_Polar_Defs

from pc2_artists            import *
from pc2_dialogs            import Dialog_Edit_Image, Dialog_Edit_Paneling
from wing                   import Wing, Planform, Planform_Paneled

from VLM_wing               import VLM_OpPoint, OpPoint_Var, v_from_re

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

        self.viewBox.autoRange (padding=0.08)

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

        self.setup_viewRange()                                      # ensure airfoil names fit in current view 


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
    def show_colorBar (self) -> bool:
        """ show color bar for Cp"""
        artists = self._get_artist (VLM_Panels_Artist)
        return artists[0].show_colorBar if artists else False
    
    def set_show_colorBar (self, aBool: bool):
        artist : VLM_Panels_Artist = self._get_artist (VLM_Panels_Artist)[0]
        return artist.set_show_colorBar (aBool)


    @property
    def section_panel (self) -> Edit_Panel:
        """ return section panel within view panel"""

        if self._section_panel is None:   

            l = QGridLayout()   
            r,c = 0, 0 
            Button     (l,r,c, text="Paneling Options", width=130, colSpan=3,
                        set=self._edit_paneling, toolTip="Define / Edit paneling options")
            r +=1
            SpaceR   (l,r, height=5)
            r += 1
            CheckBox (l,r,c, text="Show Cp in panels", colSpan=3,
                        obj=self, prop=Item_VLM_Panels.show_colorBar,
                        disable=lambda: self.opPoint() is None)
            r += 1
            SpaceR   (l,r, stretch=3)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height =100,
                                              switched_on=self._show,  
                                              switchable=True, on_switched=self.setVisible)
            
        return self._section_panel 


    def _edit_paneling (self):
        """ dialog to edit paneling paramters  """

        # do an initial calculation of panels to get the chord difference value 
        self.wing().planform_paneled.recalc_cn_diff()

        # show difference between original and paneled planform before dialog
        panels_artist : VLM_Panels_Artist = self._get_artist(VLM_Panels_Artist)[0]
        panels_artist.set_show_chord_diff (True)

        dialog = Dialog_Edit_Paneling (self.section_panel, self.wing().planform_paneled)  

        myParent : Diagram_Abstract = self._parent
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
        self._polar_def_name    = None
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
        return self.polar().opPoint_at (self.opPoint_alpha) if self.isVisible() else None


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

        if self.isVisible(): super().refresh()


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
        chord  = self.wing().planform.chord_root / 1000
        return v_from_re (self.polar_def.re, chord)


    def _format_polar_def (self, polar_def : Polar_Definition) -> str:
        """ format a polar definition for combobox like '400k N7 | 14,6m/s'"""
        chord  = self.wing().planform.chord_root / 1000
        v      = v_from_re (polar_def.re, chord)
        # ncrit  = f"N{polar_def.ncrit:.2f}".rstrip('0').rstrip('.') 
        return f"Re {polar_def.re_asK}k  |  {v}m/s"


    @property
    def polar_def_list (self) -> list[str]:
        """ polar definitions as list of formatted strings"""
        return [self._format_polar_def (polar_def) for polar_def in self.wing().polar_definitions]


    @property
    def polar_def (self) -> Polar_Definition:
        """ polar definition of wingSection at root"""
        polar_defs = self.wing().polar_definitions

        for polar_def in polar_defs:
            if polar_def.name == self._polar_def_name: 
                return polar_def
        return polar_defs[0]


    @property
    def polar_def_name (self) -> str:
        return self.polar_def.name
        
    def set_polar_def_name (self, polar_def_name): 

        for polar_def in self.wing().polar_definitions:
            if polar_def.name == polar_def_name: 
                self._polar_def_name = polar_def_name

                # set to new max of this polar 
                self.set_opPoint_fixed_to_alpha_max (self.opPoint_fixed_to_alpha_max, refresh=False)

                self.refresh()

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
            Label    (l,r,c, colSpan=4, get=f"Choose airfoil polar of root", style=style.COMMENT)
            r += 1
            # Label    (l,r,c,   width=60, get="Polar")
            ComboBox (l,r,c, width=130, obj=self, prop=Item_VLM_Result.polar_def_name, colSpan=4,
                         options=lambda: self.polar_def_list)
            r += 1
            SpaceR   (l,r, height=10,stretch=0)
            r += 1
            Label    (l,r,c, colSpan=4, get=f"Define operating point", style=style.COMMENT)
            r += 1
            CheckBox (l,r,c, text="Set close to Alpha max", colSpan=4,
                        obj=self, prop=Item_VLM_Result.opPoint_fixed_to_alpha_max)
            r +=1
            FieldF   (l,r,c, lab="Alpha", lim=(-20,20), step=0.5, width=60, unit="°", dec=1, 
                        obj=self, prop=Item_VLM_Result.opPoint_alpha,
                        disable=lambda: self.opPoint_fixed_to_alpha_max) 
            # slider bug - dec values 
            # Slider   (l,r,c+2, lim=(-20,20), step=0.5, width=60, dec=1, 
            #             obj=self, prop=Item_VLM_Result.opPoint_alpha,
            #             hide=lambda: self.opPoint_fixed_to_alpha_max) 
            r += 1
            SpaceR   (l,r, height=10,stretch=0)
            r += 1
            Label    (l,r,c, colSpan=4, get=f"Diagram variable along span", style=style.COMMENT)
            r += 1
            Label    (l,r,c,   width=60, get="Variable")

            ComboBox (l,r,c+1, width=60, obj=self, prop=Item_VLM_Result.opPoint_var, 
                         options=self.opPoint_var_list)
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
        self._polar_def_name    = None
        self._opPoint_fixed_to_alpha_max = False            # opPoint is fixed at alpha_max 

        artist : VLM_Result_Artist = self._get_artist (VLM_Result_Artist)[0]
        artist.set_opPoint_var (self._opPoint_var)


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

        self.viewBox.autoRange (padding=0.08)                                         # first ensure best range x,y 
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
            CheckBox (l,r,c, text="Show maximum thickness", 
                        get=lambda: self.airfoil_artist.show_thick,
                        set=self.airfoil_artist.set_show_thick) 
            r += 1
            l.setColumnStretch (3,2)
            l.setRowStretch    (r,2)

            self._section_panel = Edit_Panel (title=self.name, layout=l, height=100, 
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
    def setup_viewRange (self):
        """ define view range of this plotItem"""

        self.viewBox.autoRange (padding=0.05)                           # ensure best range x,y 

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
        self._show_ref_line = True                          # show reference line 
        self._show_ref_planform_elli = True
        self._show_wingSections = False
        self._show_flaps = False

        super().__init__(*args,  **kwargs)

        # set spacing between the two items
        self.graph_layout.setContentsMargins (20,10,20,10)  # default margins
        self.graph_layout.setVerticalSpacing (10)


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

        # override to add additional general settings panel on top 

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
        self.graph_layout.setHorizontalSpacing (20)
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




class Diagram_Airfoil_Polar (Diagram_Abstract):
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
        self._show_strak    = False                         # show straked airfoils
        self._min_re_asK    = 10                            # minimum re number / 1000 to plot 
        self._apply_min_re  = False                         # activate min re rumber 

        super().__init__(*args, **kwargs)

        self._viewPanel.setMinimumWidth(240)
        self._viewPanel.setMaximumWidth(240)
 
         # set spacing between the two items
        self.graph_layout.setVerticalSpacing (0)

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

            item = Item_Polars (self, iItem=1, getter=self.wing, xyVars=xyVars, show=False)
            self._add_item (item, r, 0)

            dataDict = self._diagram_settings[1] if len(self._diagram_settings) > 1 else {"xyVars" : (var.CL,var.GLIDE)}
            xyVars = dataDict ["xyVars"]

            item = Item_Polars (self, iItem=2, getter=self.wing, xyVars=xyVars, show=False)
            self._add_item (item, r, 1)
 

    @override
    def create_view_panel (self):
        """ 
        creates a view panel to the left of at least one diagram item 
        has a section_panel
        """
        # override to add additional general settings panel on top 

        super().create_view_panel ()

        layout = self._viewPanel.layout()
        layout.insertWidget (0, self.general_panel, stretch=0)
        layout.insertWidget (2, self.polar_panel, stretch=0)

        # export panel at bottom 

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
        """ show straked airfoils """
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
            CheckBox (l,r,c, text="Show straked airfoils", 
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

            p = Panel_Polar_Defs (self, self.polar_defs, height=(None,None),)

            p.sig_polar_def_changed.connect (self.sig_polar_def_changed.emit)

            l.addWidget (p, r, c, 1, 6)
            l.setRowStretch (r,1)

            # minimum re rumber to plot 

            r += 1
            SpaceR      (l,r, height=5, stretch=0) 
            r += 1
            CheckBox    (l,r,c, text="Minimum Re", colSpan=4,
                            obj=self, prop=Diagram_Airfoil_Polar.apply_min_re,
                            toolTip="Apply a minimum Re number for polars in the diagrams")
            FieldF      (l,r,c+4, width=60, step=1, lim=(1, 1000), unit="k", dec=0,
                            obj=self, prop=Diagram_Airfoil_Polar.min_re_asK,
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
            Button      (l,r,c, text="Export Airfoils", width=100, set=self.sig_export_airfoils.emit)
            r += 1
            SpaceR      (l,r,10,1)

            l.setColumnMinimumWidth (0,10)
            l.setColumnStretch (2,2)

            self._export_panel = Edit_Panel (title="Export", layout=l, height=(60,None),
                                              switchable=False, switched_on=True)
        return self._export_panel 




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

        # set spacing between the two items
        self.graph_layout.setContentsMargins (20,10,20,10)  # default margins
        self.graph_layout.setVerticalSpacing (10)


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
        self._viewPanel.setMinimumWidth(180)
        self._viewPanel.setMaximumWidth(250)
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
        self.general_panel.refresh()                                # enable straked checkbox 

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

            self._general_panel = Edit_Panel (title="Common Options", layout=l, height=(60,None),
                                              switchable=False, switched_on=True)
        return self._general_panel 



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

            self._export_panel = Edit_Panel (title="Export", layout=l, height=(80,None),
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


# -------- Diagram Items -------------------------------------------------


class Item_Making_Of_Abstract (Diagram_Item):
    """ 
   Abstract Making Of Diagram (Plot) Item 
    """

    min_width   = 200                                   # min size needed - see below 
    min_height  = 100 

    sig_planform_changed        = pyqtSignal()              

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
