#!/usr/bin/env pythonbutton_color
# -*- coding: utf-8 -*-

"""  

UI panels 

"""

import logging

from base.widgets           import * 
from base.panels            import Edit_Panel

from wing                   import Wing, Planform


logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)



#-------------------------------------------------------------------------------
# Single edit panels    
#-------------------------------------------------------------------------------


class Panel_Planform_Abstract (Edit_Panel):
    """ 
    Abstract superclass for Edit/View-Panels of AirfoilEditor
        - has semantics of App
        - connect / handle signals 
    """

    @property
    def myApp (self):
        return self._parent 

    def wing (self) -> Wing: 

        return self.dataObject

    def planform (self) -> Planform:
        return self.wing().planform
    

    def _set_panel_layout (self, layout = None ):
        """ Set layout of self._panel """
        # overloaded to connect to widgets changed signal

        super()._set_panel_layout (layout=layout)
        for w in self.widgets:
            w.sig_changed.connect (self._on_planform_widget_changed)
        for w in self.header_widgets:
            w.sig_changed.connect (self._on_planform_widget_changed)


    def _on_planform_widget_changed (self, widget):
        """ user changed data in widget"""
        logger.debug (f"{self} {widget} widget changed slot")
        self.myApp.sig_airfoil_changed.emit()



class Panel_File (Panel_Planform_Abstract):
    """ File panel with open / save / ... """

    name = 'File'
    

    
    def _init_layout (self): 

        l = QGridLayout()
        r,c = 0, 0 
        Button (l,r,c, text="&Open", width=100, 
                set=self.myApp.open, toolTip="Open new Planform",
                button_style=button_style.PRIMARY)
        Button (l,r,c+1, text="&New", width=60, 
                set=self.myApp.new, toolTip="Create new Planform")
        
        r += 1
        SpaceR (l,r, height=2, stretch=0)
        r += 1
        Button (l,r,c, text="&Save", width=100, 
                set=self.myApp.save, toolTip="Save Planform to parameter file")
        Button (l,r,c+1, text="Save As", width=60, 
                set=self.myApp.saveAs, toolTip="Save Planform to new parameter file")
        r += 1
        SpaceR (l,r, stretch=4)
        r += 1
        Button (l,r,c, text="&Exit", width=100, set=self.myApp.close)
        r += 1
        SpaceR (l,r, height=5, stretch=0)        
        l.setColumnStretch (1,2)
        l.setContentsMargins (QMargins(0, 0, 0, 0)) 

        return l 
 

