#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    Airfoil Editor 

    Object model overview (a little simplified) 

    App                                         - root frame 
        |-- Edit_File_Menu                      - file level functions
        |-- Edit_AirfoilBase                    - main wing data 
        |-- Edit_AirfoilCurvature               - parameters for a specific planform type 
                :
                |-- Widgets                     - wrapper for CTk widgets - get, set their data 
                |-- Field_Widget                - entry field with label and spin buttons
                ...                     - ...

        |-- Diagram_Airfoil                     - standard airfoil view 
                :
                |-- Artists                     - helper to plot a wing object on a matplotlib axes
                ...                     - ...

"""
import os
import sys
import argparse
from pathlib import Path
import fnmatch 

import logging
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec

from tkinter import filedialog
import customtkinter as ctk

# let python find the other modules in modules relativ to path of self  
sys.path.append(os.path.join(Path(__file__).parent , 'modules'))

from common_utils       import * 

from airfoil            import Airfoil, Airfoil_Bezier, Airfoil_Hicks_Henne
from airfoil            import GEO_BASIC, GEO_SPLINE
from airfoil            import NORMAL
from airfoil_geometry   import Geometry, Side_Airfoil_Bezier, UPPER, LOWER
from airfoil_geometry   import Match_Side_Bezier

from airfoil_examples   import Root_Example
from airfoil_artists    import *

from widgets            import *
from ui_base            import *
from artist             import set_font_size

#------------------------------------------------

AppName    = "Airfoil Editor"
AppVersion = "1.3.0"


# tk change events for updating diagrams or vice versa entry fields  

AIRFOIL_NEW                 = "<<AIRFOIL_NEW>>"                
AIRFOIL_CHANGED             = "<<AIRFOIL_CHANGED>>"
AIRFOIL_CHANGED_IN_ARTIST   = "<<AIRFOIL_CHANGED_IN_ARTIST>>"      # airfoil was changed in diagram by mouse 
BEZIER_CHANGED              = "<<BEZIER_CHANGED>>"                 # bezier curve was changed in app
BEZIER_CHANGED_IN_ARTIST    = "<<BEZIER_CHANGED_IN_ARTIST>>"       # bezier curve was changed in artist


def fireEvent(ctk_root : ctk.CTkToplevel, eventType): 
    """ fire event for the current ctk_root toplevel widget """
    if not ctk_root is None: 
        ctk_root.event_generate (eventType) 
        # print ("Event ", eventType)


#-------------------------------------------------------------------------------
# Edit Frames    
#-------------------------------------------------------------------------------


class Edit_Abstract_Airfoil (Edit_Abstract):
    """ 
    Abstract superclass for all the edit airfoil like frames
    """

    def __init__(self, master, airfoilFn, *args, myApp=None, **kwargs):

        self._airfoilFn = airfoilFn               # the function to the wing (indirect because of new wing could be set)
 
        super().__init__(master, *args, myApp=myApp, **kwargs)

        self.ctk_root.bind(AIRFOIL_CHANGED,          self.changed_airfoil, add='+')
        self.ctk_root.bind(AIRFOIL_NEW,              self.changed_airfoil, add='+')

    
    def airfoil(self) -> Airfoil:
        # it's a method - not a property to exchange the wing 
        return self._airfoilFn()
    
    @property
    def geo (self) -> Geometry:
        """ geometry of self.airfoil"""
        return self.airfoil().geo 

    @property
    def myApp (self) -> 'AirfoilEditor':
        """ the top level app self belongs to -can be overloaded for type info"""
        return super().myApp

    def changed_airfoil (self, dummy): 
        self.refresh()




class Edit_File_Menu(Edit_Abstract_Airfoil):
    """ 
    Frame for the high level commands like load, save, ...
    The parent is the App itself
    """
    name = "File"

    def init (self):

        self.myApp : AirfoilEditor

        self.grid_columnconfigure   (0, weight=1)
        self.grid_columnconfigure   (1, weight=0)
        self.grid_columnconfigure   (2, weight=1)

        r,c = 0,0 
        Header_Widget (self,r,c, columnspan=2, lab=self.name, width=80)
        Button_Widget (self,r,c+2, icon_name='settings', sticky="e", padx=(0,5),
                       style=ICON, set=self.myApp.edit_settings,
                       tooltip="Edit App settings")

        # if AirfoilEditor called stand alone offer combobox with input files
        if not self.myApp.isModal:
            r +=1 
            self.add(Option_Widget (self,r,c, get=self.curAirfoilFileName, set=self.set_curAirfoilFileName,
                                            options=self.airfoilFileNames, columnspan=3,
                                            spin=True, spinPos='below', width=100, padx=10, pady=4, sticky = 'ew'))
            r +=2 
            self.add(Blank_Widget (self,r,c, width=30))
            self.add(Button_Widget (self,r,c+1, lab='Open', width=110, pady=20 , set=self.myApp.open))
            self.add(Blank_Widget (self,r,c+2))

        # if called from parentApp offer "Ok and return" and "Cancel"
        if self.myApp.isModal:
            r +=1 
            self.add(Button_Widget (self,r,c+1, lab='Ok & Return', style=PRIMARY,       
                                    width=110, pady=4,  set=self.myApp.ok_return))
            r +=1 
            self.add(Button_Widget (self,r,c+1, lab='Cancel', width=110, pady=20, set=self.myApp.cancel))
 
 
    def refresh(self):
        # refresh typically based on changed events 
        super().refresh()

    def airfoilFileNames (self): 

        fileNames = []
        for aFileName in self.myApp.airfoilFiles:
            fileNames.append(os.path.basename(aFileName))
        return fileNames
    
    def curAirfoilFileName (self): 

        if self.myApp.curAirfoilFile is None: 
            return ''
        else: 
            return os.path.basename(self.myApp.curAirfoilFile)  

    def set_curAirfoilFileName (self, newFileName): 
        # set new current airfoil bei filename 

        self.myApp.refresh_airfoilFiles()

        for aPathFileName in self.myApp.airfoilFiles:
            if newFileName == os.path.basename(aPathFileName):

                if os.path.isfile (aPathFileName) or newFileName == 'Root_Example':   # maybe it was deleted in meantime 
                    self.myApp.loadNewAirfoil (aPathFileName)
                self.refresh()
                break




class Edit_Airfoil_Data(Edit_Abstract_Airfoil):
    """ 
    Frame to edit main data of the airfoil like thickness. This is just the header
    """
    name = "Airfoil"

    def init (self):

        r, c = 0, 0 
        self.add (Header_Widget (self,r,c,   width= 70, columnspan= 2, lab=self.name))
        self.add (Button_Widget (self,r,c+3, lab='Modify geometry', width=110, padx= (40,0), columnspan=4, sticky='w', 
                                 set=self.modify_airfoil, disable=self.is_modify_airfoil_disabled ))

        r += 1
        Blank_Widget (self, r,0)    
        c += 1                                  # left blank colum to inset the fields 
        self.add (Field_Widget  (self,r,c, columnspan= 8 ,lab='Name', obj=self.airfoil, get='name',
                                 width=200, lab_width=80, justify='left'))
        r += 1
        self.add (Field_Widget  (self,r,c,   lab="Thickness", obj=self.airfoil, 
                                get='maxThickness', width=50, lab_width=80, unit="%", dec=2))
        self.add (Field_Widget  (self,r,c+3, lab="at", lab_width=40, obj=self.airfoil, 
                                get='maxThicknessX', width=50, unit="%", dec=1))
        r += 1
        self.add (Field_Widget  (self,r,c,   lab="Camber", obj=self.airfoil, 
                                get='maxCamber', width=50, lab_width=80, unit="%", dec=2))
        self.add (Field_Widget  (self,r,c+3, lab="at", lab_width=40, obj=self.airfoil, 
                                get='maxCamberX', width=50, unit="%", dec=1))
        r += 1
        self.add (Field_Widget  (self,r,c,   lab="TE gap", obj=self.airfoil, 
                                get='teGap_perc', width=50, lab_width=80, unit="%", dec=2))
        self.add (Field_Widget  (self,r,c+3, lab="LE radius", obj=self.airfoil, 
                                get='leRadius_perc', width=50, lab_width=60, unit="%", dec=2))
        r += 1
        self.add (Label_Widget  (self,r,c, padx=5,  pady=(3,3), disable=True,
                                 lab=lambda : "Data " + self.airfoil().geo.description))
 

    def modify_airfoil (self): 
        """ Open thickness and camber dialog - if 'OK' the modified airfoil will be inserted to the list 
        of airfoils and set to current """

        dialog = Dialog_Geometry (self, self._airfoilFn )
        self.wait_window (dialog)

        if dialog.return_OK: 
            self.myApp.set_curAirfoilPathFileName (dialog.return_newAirfoilPathFileName)

    def is_modify_airfoil_disabled (self):
        """ disable modify button"""
        return self.airfoil().isBezierBased or self.airfoil().isHicksHenneBased



class Edit_Curvature(Edit_Abstract_Airfoil):
    """ 
    Frame to edit main data of the airfoil like thickness. This is just the header
    """
    name = "Curvature"

    @property
    def curvature_threshold (self): 
        return self.geo.curvature.upper.threshold
    def _set_curvature_threshold (self, aVal):
        self.geo.curvature.upper.set_threshold(aVal)
        self.geo.curvature.lower.set_threshold(aVal)
    

    def init (self):

        r, c = 0, 0 
        self.add (Header_Widget (self,r,c,   width=90, lab=self.name, columnspan= 2))

        r = 1
        Blank_Widget (self, r,0)                # left blank column to inset the fields 
        c = 1                                  
        self.add (Field_Widget  (self,r,c,   lab="Reversals upper", 
                                 get=lambda: self.geo.curvature.upper.nreversals,
                                 width=50, lab_width=100, set='', dec=0))
       
        r += 1
        self.add (Field_Widget  (self,r,c, lab="lower", 
                                 get=lambda: self.geo.curvature.lower.nreversals, 
                                 width=50, lab_width=60, padx=(20,0), set='', dec=0))
        r += 1
        self.add (Field_Widget  (self,r,c,   lab="... threshold", 
                                 get=lambda: self.curvature_threshold, 
                                 set=self._set_curvature_threshold, event=AIRFOIL_CHANGED,
                                 width=50, lab_width=80, lim=(0,1), dec=2, spin=False, step=0.02))


    def refresh(self): 
        #overloaded to set threshold in airfoils for reversal calculation 
        self._set_curvature_threshold (self.curvature_threshold)
        super().refresh()



class Edit_Specials (Edit_Abstract_Airfoil):
    """ 
    Frame to edit main data of the airfoil like thickness. This is just the header
    """
    name = "Specials"

    def init (self):

        r, c = 0, 0 
        self.add (Header_Widget (self,r,c,   width=90, lab=self.name, columnspan= 2))

        r = 1
        Blank_Widget (self, r,0)                # left blank column to inset the fields 
        c = 1                                  
        self.add (Button_Widget (self,r,c, lab='Airfoil Bezier', width=130, 
                                 padx= (20,0), columnspan=2, sticky='w', 
                                 set=self.airfoil_Bezier ))
        r += 1
        self.add (Button_Widget (self,r,c, lab='Blend Airfoil', width=130, 
                                 padx= (20,0), pady = (20,0), columnspan=2, sticky='w', 
                                 set=self.blend_airfoils ))



    def airfoil_Bezier (self): 
        """ Open dialog for Bezier design - if 'OK' the modified airfoil will be inserted to the list 
        of airfoils and set to current """

        dialog = Dialog_Bezier (self, self._airfoilFn )
        self.wait_window (dialog)

        if dialog.return_OK: 
            self.myApp.set_curAirfoilPathFileName (dialog.return_newAirfoilPathFileName)

    def blend_airfoils (self): 
        """ Open dialog to blend airfoil with another one  """

        dialog = Dialog_Blend_Airfoils (self, self._airfoilFn )
        self.wait_window (dialog)

        if dialog.return_OK: 
            self.myApp.set_curAirfoilPathFileName (dialog.return_newAirfoilPathFileName)




class Edit_Panels(Edit_Abstract_Airfoil):
    """ 
    Frame to edit main data of the airfoil like thickness. This is just the header
    """
    name = "Panels"

    def init (self):

        r, c = 0, 0 
        self.add (Header_Widget (self,r,c, width= 70, lab=self.name, columnspan= 2))
        self.add (Button_Widget (self,r,c+3, lab='Repanel', width=80, padx= (10,0), columnspan=2, sticky='w', 
                                 set=self.repanel_airfoil, text_style=lambda: self.style_repanel(),
                                 disable=self.is_modify_airfoil_disabled ))

        r += 1
        Blank_Widget (self, r,0)    
        c = 1                                  # left blank column to inset the fields 
        self.add (Field_Widget  (self,r,c,   lab="No of panels", get=lambda: self.geo.nPanels,
                                 width=50, lab_width=0, dec=0, text_style=lambda: self.style('no')))
        r += 1
        self.add (Field_Widget  (self,r,c,   lab="Angle at LE", get=lambda: self.geo.panelAngle_le, 
                                 width=50,   lab_width=80, unit="°", dec=1, text_style=lambda: self.style('le_angle')))
        self.add(Label_Widget   (self,r,c+3, columnspan=2, padx= 0,  width=80, 
                                 lab= lambda: "at index %d" % self.geo.iLe))

        r += 1
        self.add (Field_Widget  (self,r,c,   lab="Angle min", get=lambda: self.geo.panelAngle_min[0], 
                                 width=50,   lab_width=80, unit="°", dec=1, text_style=lambda: self.style('min_angle')))
        self.add(Label_Widget   (self,r,c+3, columnspan=2, padx= 0, width=80, 
                                 lab= lambda: "at index %d" % self.geo.panelAngle_min[1]))
        r += 1
        Blank_Widget (self, r,c)
        r += 1
        self.add(Label_Widget   (self,r, c, columnspan=9, padx= 5, text_style = 'Warning',
                                 lab= lambda: self.messageText()))
        

    def messageText (self): 

        text = []
        minAngle, atIndex = self.geo.panelAngle_min

        if self.geo.panelAngle_le > 172.0: 
            text.append("- Panel angle at LE (%d°) is too blunt." %(self.geo.panelAngle_le))
        if minAngle < 150.0: 
            text.append("- Min. angle of two panels is < 150°")
        if self.geo.panelAngle_le == 180.0: 
            text.append("- Leading edge has 2 points")
        if self.geo.nPanels < 160 or self.geo.nPanels > 260: 
            text.append("- No of panels should be > 160 and < 260")
        
        text = '\n'.join(text)
        return text 
    
    def style (self, panel): 

        minAngle, atIndex = self.geo.panelAngle_min

        if   panel =="min_angle":
            if minAngle < 150.0 : return 'Warning'
        elif panel =="le_angle":
            if self.geo.panelAngle_le > 170.0 : return 'Warning'
        elif panel =="no":
            if self.geo.nPanels < 160 or self.geo.nPanels > 260 : return 'Warning'

    def style_repanel (self): 
        """ text style of the repanel button"""
        if len(self.messageText()) > 0: 
            return 'Warning'

    def repanel_airfoil (self): 
        """ Open repanel dialog - if 'OK' the new repaneled airfoil will be insert to the list 
        of airfoils and set to current """

        dialog = Dialog_Repanel (self, self._airfoilFn, myApp=self.myApp )
        self.wait_window (dialog)

        if dialog.return_OK: 
            self.myApp.set_curAirfoilPathFileName (dialog.return_newAirfoilPathFileName)

    def is_modify_airfoil_disabled (self):
        """ disable modify button"""
        return self.airfoil().isBezierBased or self.airfoil().isHicksHenneBased




class Edit_Coordinates(Edit_Abstract_Airfoil):
    """ 
    Frame to edit main data of the airfoil like thickness. This is just the header
    """
    name = "Coordinates"

    def init (self):

        r, c = 0, 0 
        self.add (Header_Widget (self,r,c,   width=95, lab=self.name, columnspan= 2))
        c += 3
        self.add(Button_Widget  (self,r,c, lab='Normalize', width=80, padx= (10,0), columnspan=2, sticky='w', 
                                 set=self.normalize_airfoil, text_style=lambda: self.style_normalize(),
                                 disable=self.is_modify_airfoil_disabled ))

        r = 1
        Blank_Widget (self, r,0)    
        c = 1                                  # left blank column to inset the fields 
        self.add (Field_Widget  (self,r,c,   lab="Leading edge", get=lambda: self.geo.le[0],
                                 width=80, lab_width=90, dec=7, text_style=lambda: self.style('le_x')))
        self.add (Field_Widget  (self,r,c+3,                     get=lambda: self.geo.le[1],
                                 width=80,               dec=7, text_style=lambda: self.style('le_y')))
        r += 1
        self.add (Field_Widget  (self,r,c, lab="Trailing edge", get=lambda: self.geo.te[0],
                                 width=80, lab_width=90, dec=7, text_style=lambda: self.style('te_x')))
        self.add (Field_Widget  (self,r,c+3, get=lambda: self.geo.te[1],
                                 width=80, dec=7, text_style=lambda: self.style('te_y')))
 
        r += 1
        self.add (Field_Widget  (self,r,c, lab=" " , get=lambda: self.geo.te[2],
                                 width=80, lab_width=90, dec=7, text_style=lambda: self.style('te_x')))
        self.add (Field_Widget  (self,r,c+3, get=lambda: self.geo.te[3],
                                 width=80, dec=7, text_style=lambda: self.style('te_y')))

        r += 1
        Blank_Widget (self, r,c)
        r += 1
        self.add(Label_Widget  (self,r, c, columnspan=9, padx= 5, text_style = self.style_normalize,
                                lab= self.messageText))

    def normalize_airfoil (self): 
        """open normalize dialog window - create new noramlized airfoil on demand"""

        dialog = Dialog_Normalize (self, self._airfoilFn )
        self.wait_window (dialog)

        if dialog.return_OK: 
            self.myApp.set_curAirfoilPathFileName (dialog.return_newAirfoilPathFileName)


    def is_modify_airfoil_disabled (self):
        """ disable modify button"""
        return self.airfoil().isBezierBased or self.airfoil().isHicksHenneBased


    def messageText (self): 

        text = []

        if self.geo.le[0] != 0.0 or self.geo.le[1] != 0.0:
            text.append("- Leading edge is not at 0,0")
        if self.geo.te[0] != 1.0 or self.geo.te[2] != 1.0 : 
           text.append("- Trailing edge is not at 1")

        if not text and self.geo.isSymmetrical: 
            text.append("- Airfoil is symmetrical")
        
        text = '\n'.join(text)
        return text 
    

    def style (self, coord): 
        """ text style of entry fields"""

        if   coord =="le_x":
            if self.geo.le[0] != 0.0 : return 'Warning'
        elif coord =="le_y":
            if self.geo.le[1] != 0.0 : return 'Warning'
        elif coord =="te_x":
            if self.geo.te[0] != 1.0 or \
               self.geo.te[2] != 1.0 : return 'Warning'
        elif coord =="te_y":
            if self.geo.te[3] != - self.geo.te[1] : return 'Warning'


    def style_normalize (self): 
        """ text style of the normalize button"""
        if len(self.messageText()) > 0: 
            if not self.geo.isSymmetrical:
                return 'Warning'



#-------------------------------------------------------------------------------
# Diagrams   
#-------------------------------------------------------------------------------



class Diagram_Airfoil (Diagram_Abstract):
    """ 
    plots the airfoil and curvature 
    """
    name = "Airfoils"

    def __init__(self, master, objectFn, view_frame = None, *args,  **kwargs):
        """_summary_

        Args:
            master: master ctk frame 
            projectFn: function to get project
        """

        self._show_points = False                   # switch to show airfoil points
        self._show_upper  = False                   # switch to show upper curvature
        self._show_lower  = False                   # switch to show lower curvature
        self._show_camber = False                   # switch to show camber / thickness
        self._show_shape_function = True            # switch to show Bezier or HH shape function'
        self._show_logScale = True                  # switch to have log scale for curvature 
        self._show_derivative = False               # switch to show derivative of curvature 

        self._log_widget   : Switch_Widget = None   # log switch widget
        self._deriv_widget : Switch_Widget = None   # derivative switch widget
        self._shape_widget : Switch_Widget = None   # shape function switch widget

        self.airfoilArtist   = None
        self.thicknessArtist = None
        self.curvatureArtist = None

        super().__init__(master, objectFn, view_frame = view_frame, *args, **kwargs)


    def airfoils(self):
        """ list with airfoil(s) self is working on """
        return self.mainObjects()


    def create_axes (self):
        """ setup 2 axes for airfoil and its curvature  """

        if self.show_upper or self.show_lower:
            gs = GridSpec(2, 1, height_ratios=[6, 4])
            self.ax1 : plt.Axes = self.figure.add_subplot(gs[0])
            self.ax2 : plt.Axes = self.figure.add_subplot(gs[1])
        else:
            gs = GridSpec(1, 1)
            self.ax1 : plt.Axes = self.figure.add_subplot(gs[0])
            self.ax2 = None

        self.figure.subplots_adjust(left=0.05, bottom=0.06, right=0.98, top=0.96, wspace=None, hspace=0.10)



    def setup_axes(self):
        """ setup axes, axis, artiss for this plot type """

        # airfoil contour 
        self.ax1.tick_params (labelbottom=self.ax2 is None, labelleft=True, labelsize='small')
        self.ax1.autoscale(enable=False, axis='both')
        self.ax1.set_xlim([-0.05,1.05])
        self.ax1.axis('equal')
        self.ax1.grid (visible=True)
        self.ax1.axhline(0, color=cl_labelGrid, linewidth=.5, alpha=0.7)

        # curvature
        if self.ax2:
            self.ax2.tick_params (labelsize='small')
            self.ax2.set_xlim([-0.05,1.05])

            if self.show_logScale:

                # ! linear range from 0..1 will lead to a kink in the curve 
                thresh  = 1 
                self.ax2.set_yscale('symlog', linthresh=thresh, linscale=1) # , linscale=2
                self.ax2.set_ylim([ -100, 1000])
                # mark linear and log y-axis because of kink in y log scale
                x, y = 0.008, 0.5                   # x in axes coordinates
                self.ax2.text (x, y, 'linear', va='center', ha='left', transform=self.ax2.get_yaxis_transform(), 
                                    color = cl_labelGrid, fontsize='small')
                y = 40
                self.ax2.text (x, y, 'log10', va='top',     ha='left', transform=self.ax2.get_yaxis_transform(), 
                                    color = cl_labelGrid, fontsize='small')
                self.ax2.axhline (y= thresh, xmin=0, xmax=0.035, linestyle='dashed', linewidth=0.5, color = cl_labelGrid,)
                self.ax2.axhline (y=-thresh, xmin=0, xmax=0.035, linestyle='dashed', linewidth=0.5, color = cl_labelGrid,)
            else: 
                self.ax2.clear()
                self.ax2.set_ylim([ -1, 1])
            self.ax2.grid (visible=True)
            self.ax2.axhline(0, color=cl_labelGrid, linewidth=.5, alpha=0.7)


    def setup_artists(self):
        """ setup axes, axis, artists for this plot type """

        super().setup_artists()

        # airfoil is list to prepare for multiple airfoils to view 
        
        self.airfoilArtist   = Airfoil_Artist (self.ax1, self.airfoils, show=True)
        self.airfoilArtist.set_points(self.show_points)
        self.airfoilArtist.set_show_shape_function (self.show_shape_function)
        self.airfoilArtist.set_show_title (True)            # title like "Bezier based.." 

        self.thicknessArtist = Thickness_Artist (self.ax1, self.airfoils, show=self.show_camber)
        self.thicknessArtist.set_points (self.show_points)
        

        if self.ax2: 
            self.curvatureArtist = Curvature_Artist (self.ax2, self.airfoils, show=True)
            self.curvatureArtist.set_lower(self.show_lower)
            self.curvatureArtist.set_upper(self.show_upper)
            self.curvatureArtist.set_show_derivative(self.show_derivative)          
            self.curvatureArtist.set_points (self.show_points)
        else: 
            self.curvatureArtist = None

        self.airfoilArtist.set_showLegend ('normal')

    def re_create_axes (self):
        """ re create axes to add/remove curvature diagram """
        # remove current artists
        self.airfoilArtist.set_show(False)
        self.airfoilArtist = None
        self.thicknessArtist.set_show(False)
        self.thicknessArtist = None
        if self.curvatureArtist:
            self.curvatureArtist.set_show(False)
            self.curvatureArtist = None

        # remove axes 
        if self.ax1: 
            self.ax1.remove()
            self.ax1 = None
        if self.ax2: 
            self.ax2.remove()
            self.ax2 = None
                    
        # re_create 
        self.create_axes()
        self.setup_axes()
        self.setup_artists()


    def setup_switches(self, r=0, c=0):
        """ on/off switches for this plot type"""

        r, c = super().setup_switches(r, c)

        r += 1 
        Switch_Widget (self.view_frame,r,c, lab='Grid',       
                       val=True,  set=self.set_show_grid)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Points', 
                       get=lambda: self.show_points, set=self.set_show_points)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Camber',
                       get=lambda: self.show_camber, set=self.set_show_camber)
        r += 1 
        if self.show_shape_function_disabled():                 # switch off, if the first airfoil is a .dat
            self._show_shape_function = False
        self._shape_widget = Switch_Widget (self.view_frame,r,c, lab='Shape function', 
                       get=lambda: self.show_shape_function, set=self.set_show_shape_function,
                       disable=self.show_shape_function_disabled)
        r += 1
        Blank_Widget (self.view_frame,r,c)
        self.view_frame.grid_rowconfigure(r, weight=1)

        r += 1
        Label_Widget  (self.view_frame, r, 0, lab='Curvature')
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Upper side',  
                       get=lambda: self.show_upper, set=self.set_show_upper)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Lower side',  
                       get=lambda: self.show_lower, set=self.set_show_lower)
        r += 1
        self._log_widget = Switch_Widget (self.view_frame,r,c, lab='log scale y', 
                       get=lambda: self.show_logScale, set=self.set_show_logScale,
                       disable=lambda: not (self.show_upper or self.show_lower))
        r += 1
        self._deriv_widget = Switch_Widget (self.view_frame,r,c, lab='Derivative', 
                       get=lambda: self.show_derivative, set=self.set_show_derivative,
                       disable=lambda: not (self.show_upper or self.show_lower))
        return r, c


    # -------- switch call back 

    def set_show_grid (self, aBool): 
        self.ax1.grid (aBool)
        if self.ax2: self.ax2.grid (aBool)
        self.figure.canvas.draw_idle() 

    @property
    def show_points (self) -> bool: return self._show_points
    def set_show_points (self, aBool): 
        self._show_points = True
        self.airfoilArtist.set_points (aBool)
        if self.curvatureArtist:
            self.curvatureArtist.set_points (aBool)
        if self.thicknessArtist:
            self.thicknessArtist.set_points (aBool)
        self.refresh()

    @property
    def show_camber(self) -> bool: return self._show_camber
    def set_show_camber (self, aBool):
        self._show_camber = aBool
        self.thicknessArtist.set_show (aBool)

    @property
    def show_shape_function(self) -> bool: return self._show_shape_function
    def set_show_shape_function (self, aBool):
        self._show_shape_function = aBool
        self.airfoilArtist.set_show_shape_function (aBool)
        self.refresh ()
    def show_shape_function_disabled (self): 
        if len(self.airfoils()) > 0: 
            if self.airfoils()[0].isBezierBased:
                return False
            elif self.airfoils()[0].isHicksHenneBased:
                return False
        return True


    @property
    def show_upper(self) -> bool: return self._show_upper
    def set_show_upper (self, aBool):
        """ show on/off upper curvature """ 
        self._show_upper = aBool
        # show on/off curvature diagram 
        if (not self.ax2 and aBool) or (self.ax2 and not self.show_lower and not self.show_upper):
            self.re_create_axes()
        if self.curvatureArtist:
            self.curvatureArtist.set_upper(aBool)
        if self._log_widget:   self._log_widget.refresh()
        if self._deriv_widget: self._deriv_widget.refresh()
        self.refresh()


    @property
    def show_lower(self) -> bool: return self._show_lower
    def set_show_lower (self, aBool):
        """ show on/off lower curvature""" 
        self._show_lower = aBool
        # show on/off curvature diagram 
        if (not self.ax2 and aBool) or (self.ax2 and not self.show_lower and not self.show_upper):
            self.re_create_axes()
        if self.curvatureArtist:
            self.curvatureArtist.set_lower(aBool)
        if self._log_widget:   self._log_widget.refresh()
        if self._deriv_widget: self._deriv_widget.refresh()
        self.refresh()


    @property
    def show_logScale(self) -> bool: return self._show_logScale
    def set_show_logScale (self, aBool):
        self._show_logScale = aBool
        self.setup_axes()
        self.refresh()

    @property
    def show_derivative(self): return self._show_derivative
    def set_show_derivative (self, aBool): 
        if self.curvatureArtist:
            self._show_derivative = aBool 
            self.curvatureArtist.set_show_derivative(aBool)
            self.refresh()


    # -------- event handler

    def setChangeBindings (self):
        # overloaded
        self.ctk_root.bind(AIRFOIL_CHANGED, self.changed_airfoil, add='+')
        self.ctk_root.bind(AIRFOIL_NEW,     self.changed_airfoil, add='+')

    def changed_airfoil(self, dummy): 
        """ Eventhandler for changes of airfoil"""
        self.refresh()

    # -------- refresh my Artists which are on 'show mode' 

    def refresh(self): 
        # overloaded

        # refresh switches
        if not self._shape_widget is None:                  # subclass mustn't have widget
            self._shape_widget.refresh()

        # refresh artists
        self.airfoilArtist.refresh ()  
        self.thicknessArtist.refresh ()  
        if self.curvatureArtist:
            self.curvatureArtist.refresh ()  

        self.figure.canvas.draw_idle()    # draw ony if Windows is idle!




class Diagram_Airfoil_Bezier (Diagram_Airfoil):
    """ 
    plots an Bezier based airfoil and curvature 
    Allows to modiffy Bezier points
    """
    name = "Bezier Airfoil"

    def __init__(self, master, airfoilOrg : Airfoil, airfoilBezier : Airfoil_Bezier, *args, **kwargs):

        self._airfoilBezier : Airfoil_Bezier = airfoilBezier

        if airfoilOrg.isBezierBased:                    # if original is Bezier don't show per default
            self._show_original   = False 
        else: 
            self._show_original   = True 
        self._show_diff       = False
        self.bezierArtist     = None
        self.diffArtist       = None 

        super().__init__( master, airfoilOrg, *args, **kwargs)


    def airfoils(self):
        """ list with airfoil(s) self is working on """
        if self.show_original:
            airfoils =  super().airfoils()
        else: 
            airfoils =  []
        airfoils.append (self._airfoilBezier)
        return airfoils


    def airfoilBezier (self) -> Airfoil_Bezier:  
        return self._airfoilBezier


    def airfoils_to_diff (self):
        """ two airfoils to show the difference"""
        return [super().airfoils()[0], self.airfoilBezier()] 


    def setup_artists(self):
        """ setup axes, axis, artists for this plot type """

        self.bezierArtist     = Bezier_Edit_Artist (self.ax1, self.airfoilBezier, show=True, 
                                                   onMove=self.changed_bezier_in_artist)
        self.diffArtist       = Difference_Artist (self.ax1, self.airfoils_to_diff, show=False)

        super().setup_artists ()

        self.airfoilArtist.set_show_title (False)           # switch off "Bezier based" of original 
        self.airfoilArtist.set_show_shape_function (False)  # will be shown by bezier artist 


    def re_create_axes (self):
        """ re create axes to add/remove curvature diagram """
        # remove current artists
        self.bezierArtist.set_show(False)
        self.bezierArtist = None
        self.diffArtist.set_show(False)
        self.diffArtist = None
        super().re_create_axes ()


    def setup_switches(self, r=0, c=0):
        """ on/off switches for this plot type"""

        r += 1 
        Switch_Widget (self.view_frame,r,c, lab='Grid',       
                       val=True,  set=self.set_show_grid)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Original airfoil', 
                       get=lambda: self.show_original, set=self.set_show_original)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Points', 
                       get=lambda: self.show_points, set=self.set_show_points)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Camber',
                       get=lambda: self.show_camber, set=self.set_show_camber)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Diff * 10',
                       get=lambda: self.show_diff, set=self.set_show_diff)
        r += 1
        Blank_Widget (self.view_frame,r,c)
        self.view_frame.grid_rowconfigure(r, weight=1)

        r += 1
        Label_Widget  (self.view_frame, r, 0, lab='Curvature')
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Upper side',  
                       get=lambda: self.show_upper, set=self.set_show_upper)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Lower side',  
                       get=lambda: self.show_lower, set=self.set_show_lower)
        r += 1
        self._log_widget = Switch_Widget (self.view_frame,r,c, lab='log scale y', 
                       get=lambda: self.show_logScale, set=self.set_show_logScale,
                       disable=lambda: not (self.show_upper or self.show_lower))
        r += 1
        self._deriv_widget = Switch_Widget (self.view_frame,r,c, lab='Derivative', 
                       get=lambda: self.show_derivative, set=self.set_show_derivative,
                       disable=lambda: not (self.show_upper or self.show_lower))

        return r, c


    # -------- switch call back 

    @property
    def show_original(self) -> bool: return self._show_original
    def set_show_original (self, aBool):
        self._show_original = aBool                         # will filter self.airfoils 
        self.airfoilArtist.refresh(figureUpdate=True)
        if self.curvatureArtist:
            self.curvatureArtist.refresh(figureUpdate=True)

    @property
    def show_diff(self) -> bool: return self._show_diff
    def set_show_diff (self, aBool):
        self._show_diff = aBool
        self.diffArtist.set_show(aBool) 

    # -------- event handler

    # -------- call back from diagram 
    
    def changed_bezier_in_artist(self):
        # call back from diagram 
        if self.curvatureArtist:
            self.curvatureArtist.refresh () 
        self.airfoilArtist.refresh()
        self.diffArtist.refresh() 
        self.figure.canvas.set_cursor = lambda cursor: None     # matplotlib hack: suppress busy cursor
        self.figure.canvas.draw()

        # make splined curves like thickness invalid 
        self.airfoilBezier().reset()  

        # inform entry fields
        fireEvent  (self.ctk_root, BEZIER_CHANGED_IN_ARTIST)               

    # -------- refresh my Artists which are on 'show mode' 

    def refresh(self): 
        # overloaded
        self.bezierArtist.refresh ()  
        self.diffArtist.refresh ()
        super().refresh()



class Diagram_LeTe_Mini (Diagram_Abstract):
    """ 
    plots the leading edge and the trailing edge area in mini format
    """
    name = "LE and TE"


    def airfoils(self):
        """ list with airfoil(s) self is working on """
        return self.mainObjects()


    def create_axes (self):
        """ setup 2 axes for airfoil and its curvature  """

        from matplotlib.gridspec import GridSpec

        gs = GridSpec(2, 2, width_ratios=[2, 4])
        self.ax1 = self.figure.add_subplot(gs[:, 0])            # left, full height - LE
        self.ax2 = self.figure.add_subplot(gs[:-1, -1])         # right upper       - airfoil
        self.ax3 = self.figure.add_subplot(gs[ -1, -1])         # right lower       - TE

        self.figure.subplots_adjust(left=0.02, bottom=0.08, right=0.98, top=0.97, wspace=0.07, hspace=0.15)

    def setup_axes(self):
        """ setup axes, axis, artiss for this plot type """

        # airfoil contour 
        self.ax1.tick_params (labelbottom=True, labelleft=False, labelsize='small')
        self.ax1.set_xlim([-0.01,0.03])
        self.ax1.set_ylim([-0.02,0.02])
        self.ax1.autoscale(enable=False, axis='both')
        self.ax1.grid ()

        self.ax2.tick_params (labelbottom=True, labelleft=False, labelsize='small')
        self.ax2.axis('equal')
        self.ax2.autoscale(enable=True, axis='both')
        self.ax2.grid ()

        self.ax3.tick_params (labelbottom=True, labelleft=False, labelsize='small')
        self.ax3.set_xlim([0.8,1.1])
        self.ax3.set_ylim([-0.04,0.04])
        self.ax3.autoscale(enable=False, axis='x')
        self.ax3.grid ()


    def setup_artists(self):
        """ artists are not set automatically - set from outside """

        self.airfoilArtist  = Airfoil_Artist (self.ax2, self.airfoils, show=True)
        self.airfoilArtist.set_points(True)
        self.airfoilArtist.set_showLegend ('normal')

        self.leArtist  = Le_Artist (self.ax1, self.airfoils, show=True)
        self.teArtist  = Le_Artist (self.ax3, self.airfoils, show=True)


    def setup_switches(self, r=0, c=0):
        """ no switches"""
        return r, c
    
    # -------- event handler

    def setChangeBindings (self):
        # overloaded
        self.ctk_root.bind(AIRFOIL_CHANGED, self.changed_airfoil, add='+')

    def changed_airfoil(self, dummy): 
        """ Eventhandler for changes of airfoil"""
        self.refresh()

    # -------- refresh my Artists which are on 'show mode' 

    def refresh(self): 
        # overloaded
        self.airfoilArtist.refresh()
        self.leArtist.refresh()
        self.teArtist.refresh()

        self.ax1.figure.canvas.draw_idle()    # draw ony if Windows is idle!




class Diagram_Airfoil_Mini (Diagram_Abstract):
    """ 
    plots the airfoil with two free axes in mini format 
    """
    name = "Thickness and Camber"

    def __init__(self, master, objectFn, show_thickness=True, *args, **kwargs):
        super().__init__( master, objectFn, *args, **kwargs)

        self._show_thickness = show_thickness


    def airfoils(self):
        """ list with airfoil(s) self is working on """
        return self.mainObjects()


    def create_axes (self):
        """ setup 2 axes for airfoil, thickness, curvature etc."""

        if self._show_thickness:
            gs = GridSpec(2, 1, height_ratios=[5, 5])
            self.ax1 = self.figure.add_subplot(gs[:-1, :])          # top, full   - airfoil
            self.ax2 = self.figure.add_subplot(gs[ -1, :])          # lower, full - thickness
        else: 
            gs = GridSpec(1, 1)
            self.ax1 = self.figure.add_subplot(gs[:, :])            # full
            self.ax2 = None


        self.figure.subplots_adjust(left=0.02, bottom=0.08, right=0.98, top=0.97, wspace=0.07, hspace=0.15)

    def setup_axes(self):
        """ setup axes, axis, artiss for this plot type """

        # airfoil contour 
        self.ax1.tick_params (labelbottom=True, labelleft=False, labelsize='small')
        self.ax1.autoscale(enable=False, axis='both')
        self.ax1.set_xlim([-0.05,1.05])
        self.ax1.axis('equal')
        self.ax1.grid (visible=True)
        
        if self._show_thickness:
            # additional plot 
            self.ax2.tick_params (labelbottom=True, labelleft=False, labelsize='small')
            self.ax2.autoscale(enable=False, axis='x')
            self.ax2.set_xlim([-0.05,1.05])
            self.ax2.axis('equal')
            self.ax2.grid (visible=True)


    def setup_artists(self):
        """ setup axes, axis, artists for this plot type """
        super().setup_artists()

        # airfoil is list to prepare for multiple airfoils to view         
        self.airfoilArtist = Airfoil_Artist (self.ax1, self.airfoils, show=True)

        if self._show_thickness:
            self.thicknessArtist = Thickness_Artist (self.ax2, self.airfoils, show=True)
        else: 
            self.thicknessArtist = None 


    def setup_switches(self, r=0, c=0):
        """ no switches"""
        return r, c

    # -------- refresh my Artists which are on 'show mode' 

    def refresh(self): 
        # overloaded
        self.airfoilArtist.refresh ()  
        if self._show_thickness:
            self.thicknessArtist.refresh ()  

        self.ax1.figure.canvas.draw_idle()    # draw ony if Windows is idle!




class Diagram_Airfoil_Edit (Diagram_Abstract):
    """ 
    plots the airfoil with two free axes (airfoil and thickness & camber)

    thicknees & camber line can be moved by mouse ) 
    """
    name = "Thickness and Camber"

    def __init__(self, master, objectFn, show_thickness=True, *args, **kwargs):
        super().__init__( master, objectFn, *args, **kwargs)

        self._show_thickness = show_thickness


    def airfoils(self):
        """ list with airfoil(s) self is working on """
        return self.mainObjects()

    @property
    def airfoil_edited (self) -> Airfoil:
        """ the airfoil which can be edited """
        airfoil : Airfoil
        for airfoil in self.airfoils():
            if airfoil.usedAs == DESIGN:
                return airfoil 
        return None

    @property
    def airfoil_original (self) -> Airfoil:
        """ the original airfoil"""
        airfoil : Airfoil
        for airfoil in self.airfoils():
            if airfoil.usedAs != DESIGN:
                return airfoil 
        return None

    def create_axes (self):
        """ setup 2 axes for airfoil, thickness, curvature etc."""

        if self._show_thickness:
            gs = GridSpec(2, 1, height_ratios=[5, 5])
            self.ax1 = self.figure.add_subplot(gs[:-1, :])          # top, full   - airfoil
            self.ax2 = self.figure.add_subplot(gs[ -1, :])          # lower, full - thickness
        else: 
            gs = GridSpec(1, 1)
            self.ax1 = self.figure.add_subplot(gs[:, :])            # full
            self.ax2 = None


        self.figure.subplots_adjust(left=0.02, bottom=0.08, right=0.98, top=0.97, wspace=0.07, hspace=0.15)

    def setup_axes(self):
        """ setup axes, axis, artiss for this plot type """

        # airfoil contour 
        self.ax1.tick_params (labelbottom=True, labelleft=False, labelsize='small')
        self.ax1.autoscale(enable=False, axis='both')
        self.ax1.set_xlim([-0.05,1.05])
        self.ax1.axis('equal')
        self.ax1.grid (visible=True)
        
        # additional plot 
        self.ax2.tick_params (labelbottom=True, labelleft=False, labelsize='small')
        self.ax2.autoscale(enable=False, axis='both')
        self.ax2.set_xlim([-0.05,1.05])
        self.ax2.set_ylim([-0.05, 0.2])
        self.ax2.axis('equal')
        self.ax2.grid (visible=True)


    def setup_artists(self):
        """ setup axes, axis, artists for this plot type """
        super().setup_artists()

        # airfoil is list to prepare for multiple airfoils to view         
        self.airfoilArtist = Airfoil_Artist (self.ax1, self.airfoils, show=True)

        self.thicknessArtist = Thickness_Edit_Artist (self.ax2,[self.airfoil_edited], show=True,
                                                        onMove=self.changed_line_in_artist)
        self.originalArtist  = Thickness_Artist (self.ax2,[self.airfoil_original], show=True)


    def setup_switches(self, r=0, c=0):
        """ no switches"""
        return r, c
    
    # -------- call back from diagram 
    
    def changed_line_in_artist(self):
        # call back from diagram 

        self.airfoil_edited.rebuild_from_thicknessCamber()

        self.airfoilArtist.refresh()
        self.figure.canvas.set_cursor = lambda cursor: None     # matplotlib hack: suppress busy cursor
        self.figure.canvas.draw()

        # inform entry fields
        fireEvent  (self.ctk_root, AIRFOIL_CHANGED_IN_ARTIST)    

    # -------- refresh my Artists which are on 'show mode' 

    def refresh(self): 
        # overloaded
        self.airfoilArtist.refresh ()  
        self.thicknessArtist.refresh ()  
        self.originalArtist.refresh()

        self.ax1.figure.canvas.draw_idle()    # draw ony if Windows is idle!



#-------------------------------------------------------------------------------
# Dialogs for smaller tasks   
#-------------------------------------------------------------------------------

class Dialog_Airfoil_Settings (Dialog_Settings):

    heightFrac = 0.45


    def _add_settings(self, r):

        # Header 
        c = 0 
        
        Label_Widget  (self.edit_frame,r, c, padx= 30, pady=(20,15), sticky = 'nw',
                        lab= "Default values for re-panelling an airfoil")

        # Settings 
        r += 1
        self.add (Field_Widget  (self.edit_frame,r,c, lab="No of panels", lab_width=100, width=80, padx= 50, pady=5,
                                 obj=self, get='nPanels_default', set="set_nPanels_default", dec=0))
        r += 1
        self.add (Field_Widget  (self.edit_frame,r,c, lab="LE bunch", lab_width=100, width=80, padx= 50, pady=5,
                                 obj=self, get='le_bunch_default', set="set_le_bunch_default", dec=2))
        r += 1
        self.add (Field_Widget  (self.edit_frame,r,c, lab="TE bunch", lab_width=100, width=80, padx= 50, pady=5,
                                 obj=self, get='te_bunch_default', set="set_te_bunch_default", dec=2))

        r += 1
        return r
    

    @property
    def nPanels_default (self):
        return fromDict (self.settings_dict, 'nPanels_default', default=Airfoil.nPanels_default, msg=False) 
    def set_nPanels_default (self, newVal):
        if newVal is not None: 
            newVal = max (40,  newVal)
            newVal = min (500, newVal) 
            newVal= int (newVal)
            Airfoil.nPanels_default = newVal 

        toDict(self.settings_dict, 'nPanels_default', newVal)
        self.refresh()


    @property
    def le_bunch_default (self):
        return fromDict (self.settings_dict, 'le_bunch_default', default=Airfoil.le_bunch_default, msg=False) 
    def set_le_bunch_default (self, newVal):
        if newVal is not None: 
            newVal = max (0.0, newVal)
            newVal = min (1.0, newVal) 
            Airfoil.le_bunch_default = newVal 
        toDict(self.settings_dict, 'le_bunch_default', newVal)
        self.refresh()


    @property
    def te_bunch_default (self):
        return fromDict (self.settings_dict, 'te_bunch_default', default=Airfoil.te_bunch_default, msg=False) 
    def set_te_bunch_default (self, newVal):
        if newVal is not None: 
            newVal = max (0.0, newVal)
            newVal = min (1.0, newVal) 
            Airfoil.te_bunch_default = newVal 
        toDict(self.settings_dict, 'te_bunch_default', newVal)
        self.refresh()
    



class Dialog_Airfoil_Abstract (Dialog_Abstract):
    """ 
    Superclass for modal dialog to modify airfoil properties  
    having 

        header_frame    - which is empty 
        diagramm_frame  - ! which is not initialized !
        switches_frame  - for diagram setting  
        input_frame     - which is empty 
        button_frame    - with default buttons SaveAs, Ok, Cancel 

    """
    name       = "Modify airfoil"
    nameExt    = '-mod'                                     # default extension for new airfoil 

    def __init__(self, master, airfoilFn, workingDir=None, *args, **kwargs):

        self.return_newAirfoilPathFileName = None   # return value for parent

        airfoilOrg_in : Airfoil = airfoilFn()

        # make a copy of original airfoil as splined airfoil - if possible (not for Bezier) 

        try: 
            self.airfoilOrg = airfoilOrg_in.asCopy(geometry=GEO_SPLINE) 
        except: 
            self.airfoilOrg = airfoilOrg_in.asCopy() 

        self.airfoilOrg.set_usedAs (NORMAL)

        # and a copy of original airfoil as splined airfoil, normalized - as working copy 

        try:                            # normal airfoil - allows new geometry
            self.airfoil    = airfoilOrg_in.asCopy (nameExt=self.nameExt, geometry=GEO_SPLINE)
        except:                         # bezier or hh does not allow new geometry
            self.airfoil    = airfoilOrg_in.asCopy (nameExt=self.nameExt)

        self.airfoil.set_usedAs (DESIGN)
        self.hasbeen_normalized = False
        if not self.airfoil.isNormalized:                    # also LE of spline at 0,0? 
            self.hasbeen_normalized = self.airfoil.normalize ()    # ensure exact le based on spline

        self.airfoil.set_isModified (False)                  # initial now save needed
        title  = self.name +"   [" + self.airfoilOrg.name + "]"

        super().__init__(master, workingDir=workingDir, title=title, *args, **kwargs)


    @property
    def geo (self) -> Geometry:
        """ geometry of self.airfoil"""
        return self.airfoil.geo

    @property
    def geoOrg (self) -> Geometry:
        """ geometry of self.airfoilOrg"""
        return self.airfoilOrg.geo


    def init (self):
        """ init common frames and buttons"""

        # -------  Setup major sub frames --------------------

        self.header_frame   = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.input_frame    = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.button_frame   = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.view_frame     = ctk.CTkFrame(self.edit_frame, fg_color="transparent", width=10, height=10) 
        self.diagram_frame  = None                                      # diagram - created in subclass 

        self.header_frame.grid (row=0, column=0, columnspan=2, sticky="nwe", padx=0, pady=(10,10))
        self.input_frame.grid  (row=2, column=1, sticky="nwe", padx=40, pady=10)
        self.button_frame.grid (row=3, column=1, sticky="wes", padx=40, pady=(30,20))
        self.view_frame.grid   (row=1, column=0, sticky="nsew")

        self.edit_frame.grid_columnconfigure (0, weight=0, minsize=10)  # view - width overloaded in sub  
        self.edit_frame.grid_columnconfigure (1, weight=5)              # input - width flex
        self.edit_frame.grid_rowconfigure    (1, weight=1)              # input - height flex

        # ------- Default buttons --------------------

        r,c = 0,1  
        self.nameWidget = Field_Widget  (self.button_frame,r,c, lab="New Airfoil name", 
                                 obj=lambda: self.airfoil, get='name', set='set_name',
                                 lab_width=130, width=220, justify='left')
        c += 2
        self.add (Button_Widget (self.button_frame,r,c, lab='Save As...', set=self.save, 
                                 style=SUPTLE, width=90, disable=self._ok_disabled))
        c += 1 
        self.add (Button_Widget (self.button_frame,r,c, lab='Ok', set=self.ok, 
                                 style=PRIMARY, width=90, disable=self._ok_disabled))
        c += 1 
        self.add (Button_Widget (self.button_frame,r,c, lab='Cancel', set=self.cancel, 
                                 width=90))
        c += 1
        self.add (Label_Widget (self.button_frame,r,c, text_style=STYLE_WARNING,
                                lab= lambda: self._save_warning(), width= 110))

        self.button_frame.grid_columnconfigure (9, weight=1)

        # ------- Common events and bindings --------------------
        
        self.change_event = AIRFOIL_CHANGED     # input field will fire this event when data is changed

        self.ctk_root.bind(self.change_event, self.refresh, add='+')


    def _save_warning (self): 
        if self.airfoil and self.airfoil.isModified: 
            return "Airfoil not saved"
        else: 
            return ""


    def save(self): 
        """ save modified airfoil to file """
        
        # ensure user entry in name field is set to airfoil
        self.nameWidget.force_set() 

        filetypes  = [('Airfoil files', '*.dat')]
        initialDir, airfoilFileName = os.path.split(self.airfoilOrg.pathFileName)
        newPathFilename = filedialog.asksaveasfilename(title='Save airfoil',
                                     initialdir=initialDir, filetypes=filetypes,
                                     initialfile=self.airfoil.name,
                                     defaultextension = '.dat')
        if newPathFilename: 

            airfoilPath, airfoilFileName = os.path.split(newPathFilename)
            # maybe user changed name when saving to file? 
            airfoilName = os.path.splitext(airfoilFileName)[0]

            try: 
                self.airfoil.saveAs (airfoilPath, airfoilName)
            except: 
                message = "Airfoil name not valid.\n\nAirfoil could not be saved"
                msg = Messagebox (self, title="Save", message=message, icon="cancel", option_1="Ok")
                msg.get()                # wait until pressed ok

            self.refresh()


    def cancel(self): 
        # changed bindings
        self.ctk_root.unbind(self.change_event)
        super().cancel()


    def _save_fileTypes (self):
        # returns the fileTypes for save message
        return "" 
    
    def _ok_disabled (self):
        # ok and save button disabling 
        # to be overloaded
        return False

    def ok(self): 
        """ saves modified airfoil and returns to parent with filename of the new, modified airfoil"""

        exitDialog = True 

        # ensure user entry in name field is set to airfoil
        self.nameWidget.force_set() 

        # save in directory of original airfoil with new name  
        if self.airfoil.isModified: 
            airfoilDir = os.path.split(self.airfoilOrg.pathFileName)[0]
            if airfoilDir == '': 
                airfoilDirMSG = 'Current directory'
            else:
                airfoilDirMSG = airfoilDir

            try: 
                self.airfoil.saveAs (dir = airfoilDir)
                message = "Airfoil '%s'\n\nsaved %s to\n\ndirectory '%s'" \
                           % (self.airfoil.name, self._save_fileTypes(), airfoilDirMSG )
                msg = Messagebox (self, title="Save", message=message, icon="check", option_1="Ok")
            except: 
                message = "Airfoil name not valid.\n\nAirfoil could not be saved"
                msg = Messagebox (self, title="Save", message=message, icon="cancel", option_1="Ok")
                exitDialog = False
            msg.get()                # wait until pressed ok

        if exitDialog:
            # return value for parent
            self.return_newAirfoilPathFileName = self.airfoil.pathFileName

            # release changed bindings and close
            self.ctk_root.unbind(self.change_event)
            super().ok()




class Dialog_Repanel (Dialog_Airfoil_Abstract):
    """ 
    Dialog to repanel airfoil  
    """
    name       = "Repanel airfoil"
    nameExt    ='-repan'
    widthFrac  = 0.80
    heightFrac = 0.60

    def init (self):
        # init UI ...
        super().init()                          # basic airfoil layout in super 

        # start with a repaneld airfoil  using default values 
        self.airfoil.repanel() 
        super().refresh()                       # airfoil modified - save warninng          

        self.showRepaneled = True 

        # Header 
        c = 0 
        r = 0 
        Header_Widget (self.header_frame,r,c, pady=0, lab= "Repanel Airfoil", sticky = 'nw', width=100)
        
        Switch_Widget (self.header_frame,r,c+1, padx=(30,30), lab='Show repaneled airfoil',
                       get=lambda: self.showRepaneled, set=self.set_showRepaneled)

        Label_Widget  (self.header_frame,r, c+2, padx= 20, sticky = 'nw', columnspan=1,
                        lab= "For repaneling a spline is created with the existing points.\n" + \
                        "The spline will define a new 'real' leading edge of the airfoil contour.",
                        text_style=STYLE_COMMENT)
        Label_Widget  (self.header_frame,r, c+3, padx= 20,  sticky = 'nw', columnspan=1,
                        lab= "The new leading edge devides the airfoil in an upper and lower side,\n" + \
                        "which are repaneled by a cosinus function with a bunch at LE and TE.",
                        text_style=STYLE_COMMENT)

        self.header_frame.grid_columnconfigure (4, weight=1)

        # input fields
        r,c = 0,0
        Blank_Widget (self.input_frame,r,c,  height= 10)
        r +=1 
        self.add (Field_Widget (self.input_frame,r,c, lab="Leading edge bunch", lab_width=130, 
                                width=100, 
                                obj=self.airfoil, get='le_bunch', set='set_le_bunch',
                                event=self.change_event, lim=(0,1), dec=2, spin=True, step=0.02))
        self.add (Label_Widget (self.input_frame,r,c+3, text_style=self.le_bunch_message_style,
                                lab= lambda: self.le_bunch_message(), columnspan = 1, width= 250))

        self.add (Field_Widget (self.input_frame,r,c+4, lab="Trailing edge bunch", lab_width=130, 
                                width=100,
                                obj=self.airfoil, get='te_bunch', set='set_te_bunch',
                                event=self.change_event, lim=(0,1), dec=2, spin=True, step=0.02))

        r +=1 
        Blank_Widget (self.input_frame,r,c,  height= 10)
        r +=1 
        self.add (Field_Widget (self.input_frame,r,c  , lab="No of panels", width=100,
                                obj=self.airfoil, get='nPanelsNew', set='set_nPanelsNew',
                                event=self.change_event, lim=(50,500), dec=0, spin=True, step=10))
        self.add (Label_Widget (self.input_frame,r,c+3  , columnspan = 1, text_style=STYLE_DISABLED,
                                lab= lambda: "equals %d points" % self.airfoil.nPoints))
        
        self.add(Button_Widget (self.input_frame,r,c+4, lab='Change defaults', width=110, columnspan=3, 
                                sticky='w', style=SUPTLE, padx=(3,10),set=self.change_defaults))

        self.input_frame.grid_columnconfigure (8, weight=2)

        # buttons are defined in super class 

        # ----------- setup diagram frame at the end ---------------

        # set specific diagram frame for this dialog 
        self.diagram_frame = Diagram_LeTe_Mini (self.edit_frame, self.airfoilList, size=(7.0,4.0))
        self.diagram_frame.grid(row=1, column=1, sticky="nwes")



    def airfoilList (self):
        if self.showRepaneled: 
            return [self.airfoil]
        else: 
            return [self.airfoilOrg]


    def set_showRepaneled (self, aBool):
        self.showRepaneled = aBool
        self.refresh ('') 


    def le_bunch_message (self): 
        angle = self.geo.panelAngle_le
        if angle > 172.0: 
            text = "Angle at LE is too blunt. Decrease bunch" 
        elif angle < 155.0: 
            text = "Angle at LE is too sharp. Increase bunch"
        else:
            text = "leads to good panel angle at LE"
        return text 
            

    def le_bunch_message_style (self): 
        angle = self.geo.panelAngle_le
        if angle > 170.0 or angle < 155.0: 
            return 'Warning'
        else: 
            return 'Disabled'

    def change_defaults (self):
        """ change panelling defaults by calling Settings dialog """

        self.myApp.edit_settings()
        self.airfoil.repanel()
        self.refresh()

    def refresh(self, *_):

        self.diagram_frame.refresh()
        super().refresh()




class Dialog_Normalize (Dialog_Airfoil_Abstract):
    """ 
    Dialog to normalize airfoil
    """
    name        ='Normalize airfoil'
    nameExt     ='-norm'
    widthFrac   = 0.48
    heightFrac  = 0.32

    def init (self):
        # init UI ...
        super().init()                          # basic airfoil layout in super 

        self.airfoil.set_isModified(True)       # had beed normalized
        super().refresh()                       # airfoil modified - save warninng          

        # Header 
        c = 0 
        r = 0 
        Header_Widget (self.header_frame,r,c, pady=0, lab= "Normalize Airfoil", sticky = 'nw', width=100)
        
        Label_Widget  (self.header_frame,r, c+1, padx= 20, sticky = 'nw', columnspan = 1, 
                        lab= "The airfoil is shifted, rotated and scaled\n" + \
                             "so LE will be at 0,0 and TE is symmetric at 1,y.",
                             text_style=STYLE_COMMENT)
        Label_Widget  (self.header_frame,r, c+2, padx= 10, sticky = 'nw', columnspan = 1,
                        lab= "Within an iteration the leading edge of the spline should\n" + \
                             "get close to 0,0 (to numerical issues mostly not exactly).",
                             text_style=STYLE_COMMENT)

        self.header_frame.grid_columnconfigure (3, weight=1)

        # fields 
        r,c = 0,0 
        Blank_Widget (self.input_frame, r,c)
        r += 1
        self.add (Label_Widget (self.input_frame,r,c+1, padx=0, lab="Normalized x", columnspan = 2))
        self.add (Label_Widget (self.input_frame,r,c+3, padx=0, lab="y"))

        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Leading edge", get=lambda: self.geo.le[0],
                                 width=80, lab_width=130, dec=7))
        self.add (Field_Widget  (self.input_frame,r,c+3, get=lambda: self.geo.le[1],
                                 width=80, dec=7))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" ... of spline", get=lambda: self.geo.le_real[0],
                                 width=80, lab_width=130, dec=7, text_style=lambda: self.style('le_real_x')))
        self.add (Field_Widget  (self.input_frame,r,c+3, get=lambda: self.geo.le_real[1],
                                 width=80, dec=7, text_style=lambda: self.style('le_real_y')))
        r += 1
        Blank_Widget (self.input_frame, r,c, height=15)
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Trailing edge", get=lambda: self.geo.te[0],
                                 width=80, dec=7))
        self.add (Field_Widget  (self.input_frame,r,c+3, get=lambda: self.geo.te[1],
                                 width=80, dec=7))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" " , get=lambda: self.geo.te[2],
                                 width=80, dec=7))
        self.add (Field_Widget  (self.input_frame,r,c+3, get=lambda: self.geo.te[3],
                                 width=80, dec=7))

        # fields original airfoil
        r = 1 
        c = 5 
        self.add (Label_Widget (self.input_frame,r,c+1, padx=0, lab="Original x", columnspan = 2))
        self.add (Label_Widget (self.input_frame,r,c+3, padx=0, lab="y"))

        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" ", get=lambda: self.geoOrg.le[0],
                                 width=80,   lab_width=60, dec=7, text_style=lambda: self.styleOrg('le_x')))
        self.add (Field_Widget  (self.input_frame,r,c+3,          get=lambda: self.geoOrg.le[1],
                                 width=80, dec=7,                 text_style=lambda: self.styleOrg('le_y')))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" ", get=lambda: self.geoOrg.le_real[0],
                                 width=80,   lab_width=60, dec=7, text_style=lambda: self.styleOrg('le_real_x')))
        self.add (Field_Widget  (self.input_frame,r,c+3,          get=lambda: self.geoOrg.le_real[1],
                                 width=80, dec=7,                 text_style=lambda: self.styleOrg('le_real_y')))

        r += 2
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" ", get=lambda: self.geoOrg.te[0],
                                 width=80,   lab_width=60, dec=7, text_style=lambda: self.styleOrg('te_x')))
        self.add (Field_Widget  (self.input_frame,r,c+3,          get=lambda: self.geoOrg.te[1],
                                 width=80,                 dec=7, text_style=lambda: self.styleOrg('te_y')))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" ", get=lambda: self.geoOrg.te[2],
                                 width=80,   lab_width=60, dec=7, text_style=lambda: self.styleOrg('te_x')))
        self.add (Field_Widget  (self.input_frame,r,c+3,          get=lambda: self.geoOrg.te[3],
                                 width=80,                 dec=7, text_style=lambda: self.styleOrg('te_y')))
        r += 1
        Blank_Widget (self.input_frame, r,c)
        self.input_frame.grid_columnconfigure (9, weight=2)
        self.input_frame.grid_rowconfigure (0, weight=1)
        self.input_frame.grid_rowconfigure (r, weight=1)

        
    def style (self, coord): 
        if   coord =="le_x":
            if self.geo.le[0] != 0.0 : return 'Warning'
        elif coord =="le_y":
            if self.geo.le[1] != 0.0 : return 'Warning'
        elif coord =="le_real_x":
            if not self.geo.isNormalized : return 'Warning'
        elif coord =="le_real_y":
            if not self.geo.isNormalized : return 'Warning'
        elif coord =="te_x":
            if self.geo.te[0] != 1.0 or \
               self.geo.te[2] != 1.0 : return 'Warning'
        elif coord =="te_y":
            if self.geo.te[3] != - self.geo.te[1] : return 'Warning'
        
    def styleOrg (self, coord): 
        if   coord =="le_x":
            if self.geoOrg.le[0] != 0.0 : return 'Warning'
        elif coord =="le_y":
            if self.geoOrg.le[1] != 0.0 : return 'Warning'
        elif coord =="le_real_x":
            if not self.geoOrg.isNormalized : return 'Warning'
        elif coord =="le_real_y":
            if not self.geoOrg.isNormalized : return 'Warning'
        elif coord =="te_x":
            if self.geoOrg.te[0] != 1.0 or \
               self.geoOrg.te[2] != 1.0 : return 'Warning'
        elif coord =="te_y":
            if self.geoOrg.te[3] != - self.geoOrg.te[1] : return 'Warning'




class Dialog_Geometry (Dialog_Airfoil_Abstract):
    """ 
    Dialog to change thickness, camber or TE gap of airfoil  
    """
    name        ='Modify airfoil'
    nameExt     ='-mod'
    widthFrac   = 0.65
    heightFrac  = 0.80

    def init (self):
        # init UI ...
        super().init()                              # basic airfoil layout in super 

        # ! see Dialog_Airfoil_Abstract for init of airfoil !

        self.showModified = True 
        self._chord = 200.0                         # sample chord length for showing airfoil parms

        # Header 
        c = 0 
        r = 0 
        Header_Widget (self.header_frame,r,c, pady=0, lab= "Modify geometry", sticky = 'nw', width=100)
        
        Label_Widget  (self.header_frame,r, c+2, padx= (30,15), sticky = 'nw', columnspan=1,
                        lab= "For geometry modifications a spline is created with the existing points.\n" + \
                             "Thickness and camber distribution is evaluated based on this spline.",
                             text_style=STYLE_COMMENT)
        Label_Widget  (self.header_frame,r, c+3, padx= 20,  sticky = 'nw', columnspan=1,
                        lab= "If geometry is modified, the airfoil will be\n" + \
                             "repaneled and normalized to ensure exact results.",
                             text_style=STYLE_COMMENT)

        self.header_frame.grid_columnconfigure (4, weight=1)

        # input fields normalized  airfoil 

        r = 0 
        c = 0 
        if self.hasbeen_normalized: 
            lab = "New airfoil normalized " + self.airfoil.geo.description
        else: 
            lab = "New airfoil " + self.airfoil.geo.description
        Label_Widget (self.input_frame,r,c, padx=0, lab= lab, columnspan = 5,
                      text_style=STYLE_COMMENT)

        r += 1
        Blank_Widget (self.input_frame, r,10, height=5)
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Thickness", obj=self.airfoil, 
                                get='maxThickness', set='set_maxThickness', step=0.1, 
                                spin=True, width=100, lab_width=70, unit="%", dec=2,
                                event=self.change_event))
        self.add (Field_Widget  (self.input_frame,r,c+3, lab="at", lab_width=20, obj=self.airfoil, 
                                get='maxThicknessX', set='set_maxThicknessX', step=0.1, 
                                spin=True, width=100, unit="%", dec=1,
                                event=self.change_event))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Camber", obj=self.airfoil, 
                                get='maxCamber', set='set_maxCamber', disable= 'isSymmetric',  
                                spin=True, width=100, lab_width=70, unit="%", dec=2, step=0.1,
                                event=self.change_event))
        self.add (Field_Widget  (self.input_frame,r,c+3, lab="at", lab_width=20, obj=self.airfoil, 
                                get='maxCamberX', set='set_maxCamberX', disable= 'isSymmetric',  
                                spin=True, width=100, unit="%", dec=1, step=0.1,
                                event=self.change_event))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="TE gap", obj=self.airfoil, 
                                get='teGap_perc', set='set_teGap_perc', step=0.01,
                                spin=True, width=100, lab_width=70, unit="%", dec=2,
                                event=self.change_event))
        self.add (Field_Widget  (self.input_frame,r,c+3, lab="LE radius", obj=self.airfoil, 
                                get='leRadius_perc', set='set_leRadius_perc', step=0.1,
                                spin=True, width=100, lab_width=60, unit="%",  dec=3,
                                event=self.change_event))
        Label_Widget  (self.input_frame,r,c+6 , padx=5, lab= "blend width 10%", columnspan = 1)

        # fields for chord length xy mm 

        r = 0 
        c = 8 
        Label_Widget  (self.input_frame,r,c , padx=5, lab= "... with a chord length of", columnspan = 3)
        self.add (Field_Widget  (self.input_frame,r,c+4, obj=self, get='chord', set='set_chord', 
                                width=50, unit="mm", dec=1))
        r += 1
        Blank_Widget (self.input_frame, r,10, height=5)
        r += 1

        self.add (Field_Widget  (self.input_frame,r,c,   lab="Thickness", 
                                get=lambda: self.airfoil.maxThickness * self.chord / 100,  
                                width=50, lab_width=70, unit="mm", dec=1, disable= True))
        self.add (Field_Widget  (self.input_frame,r,c+3, lab="at", lab_width=20,  
                                get=lambda: self.airfoil.maxThicknessX * self.chord / 100, 
                                width=50, unit="mm", dec=1, disable= True))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Camber",  
                                get=lambda: self.airfoil.maxCamber * self.chord / 100, 
                                width=50, lab_width=70, unit="mm", dec=1, disable= True))
        self.add (Field_Widget  (self.input_frame,r,c+3, lab="at", lab_width=20, obj=self.airfoil, 
                                get=lambda: self.airfoil.maxCamberX * self.chord / 100, 
                                width=50, unit="mm", dec=1, disable= True))

        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="TE gap",  
                                get=lambda: self.airfoil.teGap_perc * self.chord / 100,  
                                width=50, lab_width=70, unit="mm", dec=2, disable= True))

        self.input_frame.grid_columnconfigure (7,  weight=1)
        self.input_frame.grid_columnconfigure (14, weight=2)

        # ---- buttons in super class


        # ----------- setup diagram frame at the end ---------------

        self.diagram_frame = Diagram_Airfoil_Edit (self.edit_frame, self.airfoilList, 
                                                   size=(7.0,6.0))
        self.diagram_frame.grid(row=1, column=1, sticky="nwes")


        # ----------- register change event from diagram / mouse move  ---------------
        self.ctk_root.bind(AIRFOIL_CHANGED_IN_ARTIST, self.refresh, add='+')



    @property                               
    def chord (self):                            # sample chord length 
        return self._chord
    def set_chord (self, aVal): 
        if aVal > 0: self._chord= aVal
        super().refresh()


    def airfoilList (self):
        return [self.airfoilOrg, self.airfoil]


    def refresh(self, dummy):
        self.diagram_frame.refresh()
        super().refresh()




class Dialog_Blend_Airfoils (Dialog_Airfoil_Abstract):
    """ 
    Dialog to blend airfoil with another
    """
    name        ='Blend airfoil'
    nameExt     ='-blended'
    widthFrac   = 0.65
    heightFrac  = 0.65

    def __init__(self, master, airfoilFn):

        
        self.airfoilBlend = None        # airfoil to blend with - start none
        self._blendBy = 0.5 

        super().__init__ (master, airfoilFn)


    def init (self):
        # init UI ...
        super().init()                              # basic airfoil layout in super 

        # ! see Dialog_Airfoil_Abstract for init of airfoil !
        
        self.airfoilOrg.set_geo_strategy (GEO_BASIC)         # use linear interpolation for max speed
        self.airfoil = None 

        self.showModified = True 

        # Header 
        c = 0 
        r = 0 
        Header_Widget (self.header_frame,r,c, pady=0, lab= "Blend Airfoil", sticky = 'nw', width=140)
        
        # Switch_Widget (self.header_frame,r,c+1, padx=(30,30), lab='Show modified airfoil',
        #                get=lambda: self.showModified, set=self.set_showModified)

        Label_Widget  (self.header_frame,r, c+1, padx= 15, sticky = 'nw', columnspan=1,
                        lab= "For blending the interpolated y-coordinates of the\n" + \
                             "airfoils are taken to calculate a new y-coordinate.",
                        text_style=STYLE_COMMENT)
        Label_Widget  (self.header_frame,r, c+2, padx= 20,  sticky = 'nw', columnspan=1,
                        lab= "The involved airfoils will be temporarly \n" + \
                             "repaneled and normalized to ensure exact results.",
                        text_style=STYLE_COMMENT)
        Label_Widget  (self.header_frame,r, c+3, padx= 20,  sticky = 'nw', columnspan=1,
                        lab= "Paneling information is taken from the more weighted airfoil.\n" + \
                             "The final airfoil will be evaluated with a spline for high precision.",
                        text_style=STYLE_COMMENT)

        self.header_frame.grid_columnconfigure (4, weight=1)

        r = 0 
        c = 0 
        lab = "Airfoils normalized " + self.airfoilOrg.geo.description
        Label_Widget (self.input_frame,r,c, padx=0, lab= lab, columnspan = 4,
                      text_style=STYLE_COMMENT)
        c = 2 
        r += 1
        Blank_Widget (self.input_frame, r,c+2, width=25)
        self.add (Label_Widget  (self.input_frame,r,c+3, lab="blendy by", padx=(30,0), width=60, columnspan=1))

        self.add (Label_Widget  (self.input_frame,r,c+5, lab=self._hint_select_airfoil, 
                                 padx=(0,0),text_style=STYLE_HINT ))

        r += 1
        self.add (Field_Widget  (self.input_frame,r,c, width=200, 
                                 get=lambda: self.airfoilOrg.name))
        c += 2
        self.add(Slider_Widget (self.input_frame,r,c, width=200, height=16, padx=(0,10), 
                                lim=(0, 1), step=0.01, columnspan=3, 
                                obj=self, get='blendBy', set='set_blendBy',
                                disable=lambda: self.airfoil is None))
        c += 3
        self.add(Field_Widget  (self.input_frame,r,c, width=200, justify='left', 
                                get=lambda: self.airfoilBlend.name if self.airfoilBlend else None ))
        c += 4
        self.add(Button_Widget (self.input_frame,r,c, lab='Select', width=90, columnspan=2, sticky='w',
                                style=PRIMARY, padx=(3,10),set=self.select_airfoil))

        r += 1
        c = 5
        self.add (Field_Widget  (self.input_frame,r,c, width=100, pady=10,
                                obj=self,  get='blendBy', set='set_blendBy', step=0.1, lim=(0,1),  
                                spin=True, unit="", dec=2,
                                disable=lambda: self.airfoil is None))
        self.input_frame.grid_columnconfigure ( 0,  weight=1)
        self.input_frame.grid_columnconfigure (12,  weight=2)

        # ---- buttons in super class

        # ----------- setup diagram frame at the end ---------------

        self.diagram_frame = Diagram_Airfoil_Mini (self.edit_frame, self.airfoilList, 
                                                   show_thickness= False, size=(5.5,4.0))
        self.diagram_frame.grid(row=1, column=1, sticky="nwes")



    @property                               
    def blendBy (self): return self._blendBy
    def set_blendBy (self, aVal): 
        self._blendBy= aVal
        if self.airfoil and self.airfoilBlend:
            self.airfoil.do_strak (self.airfoilOrg, self.airfoilBlend, self.blendBy, geometry=GEO_BASIC)
        self.refresh()

    def _hint_select_airfoil (self):
        if self.airfoil is None: 
            return "Select airfoil to blend with"
        else: 
            return ""

    def airfoilList (self):
        airfoils = [self.airfoilOrg]
        if self.airfoilBlend: airfoils.append(self.airfoilBlend)
        if self.airfoil:      airfoils.append(self.airfoil)
        return airfoils

    
    def select_airfoil(self):
        """ select airfoil with explorer and load it if possible """

        filetypes  = [('Airfoil files', '*.dat')]
    
        newPathFilename = filedialog.askopenfilename(
                    title='Select airfoil file',
                    initialdir=self.workingDir,
                    filetypes=filetypes)
        if newPathFilename: 
            self.airfoilBlend = Airfoil (pathFileName= newPathFilename, geometry=GEO_BASIC)
            self.airfoilBlend.load()
            self.airfoilBlend.normalize() 
            self.airfoilBlend.set_usedAs (REF1)

            # initial result airfoil 
            self.airfoil = self.airfoilOrg.asCopy (nameExt=self.nameExt, geometry=GEO_BASIC) 
            self.airfoil.set_usedAs (DESIGN)

            # initial blend - will refresh 
            self.set_blendBy (0.5) 


    def refresh(self, *_):
        self.diagram_frame.refresh()
        super().refresh()

    def _ok_disabled(self):
        return self.airfoilBlend is None

    def ok (self): 
        """ save and close"""
        # overloaded to spline final, blended airfoil 
        if self.airfoil: 

            self.airfoil.do_strak (self.airfoilOrg, self.airfoilBlend, self.blendBy, 
                                   geometry=GEO_SPLINE)
            self.airfoil.set_isStrakAirfoil (False)     # suppress auto naming 

        return super().ok()


class Dialog_Bezier (Dialog_Airfoil_Abstract):
    """ 
    Dialog to change thickness, camber or TE gap of airfoil  
    """
    name        ='Design Bezier airfoil'
    nameExt     ='-bezier'

    widthFrac  = 0.82
    heightFrac = 0.80

    def __init__(self, master, airfoilFn):

        self._matcher_upper = None          # controller object to match side         
        self._matcher_lower = None          # controller object to match side     

        self._target_curv_at_le = None      # target curvature at le to achieve  

        super().__init__ (master, airfoilFn)


    @property
    def matcher_upper (self) -> Match_Side_Bezier:
        """ match controler for upper side"""
        if self._matcher_upper is None: 
            geo     = self.airfoil.geo
            geoOrg  = self.airfoilOrg.geo
            self._matcher_upper = Match_Side_Bezier (geo.upper, geoOrg.upper, 
                                            target_curv_at_le=self.target_curv_at_le,
                                            max_te_curv      =geoOrg.curvature.at_upper_te)
        return self._matcher_upper

    @property
    def matcher_lower (self) -> Match_Side_Bezier:
        """ match controler for upper side"""
        if self._matcher_lower is None: 
            geo     = self.airfoil.geo
            geoOrg  = self.airfoilOrg.geo
            self._matcher_lower = Match_Side_Bezier (geo.lower, geoOrg.lower, 
                                            target_curv_at_le=self.target_curv_at_le,
                                            max_te_curv      =geoOrg.curvature.at_lower_te)
        return self._matcher_lower


    def init (self):
        # init UI ...

        super().init()                                              # basic airfoil layout in super 

        # overwrite

        if not self.airfoilOrg.isNormalized:                        # also LE of spline at 0,0? 
            self.airfoilOrg.normalize() 

        if not self.airfoil.isBezierBased:                          # create a dummy bezier airfoil
            self.airfoil= Airfoil_Bezier (name=self.airfoil.name)   # a new sample bezier
            self.set_nPoints(5, UPPER)
            self.set_nPoints(5, LOWER)
            self.airfoil.set_isLoaded(True)                         # treat sample as loaded 
            self.airfoil.set_usedAs (DESIGN)                        # will indicate airfoil when plotted 

        super().refresh()                                           # airfoil modified - save warning          


        # react on changes in diagram made by mouse drag 
        self.ctk_root.bind(BEZIER_CHANGED_IN_ARTIST, self.refresh, add='+')

        # ----------- Header  ----------------

        r,c = 0,0  
        Header_Widget (self.header_frame,r,c, pady=0, lab= "Bezier airfoil", sticky = 'nw', width=170)
        
        Label_Widget  (self.header_frame,r, c+2, padx= (0,15), sticky = 'nw', columnspan=1,
                        lab= "Design a new airfoil based on a Bezier curve for upper and lower side.\n" + 
                             "Control points can be moved, inserted or deleted with the mouse",
                             text_style=STYLE_COMMENT)
        Label_Widget  (self.header_frame,r, c+3, padx= 20,  sticky = 'nw', columnspan=1,
                        lab= "'Match Bezier' will move the control points for a best fit\n" + 
                             "to the original airfoil. 4 - 8 control points are currently supported.",
                             text_style=STYLE_COMMENT)

        Label_Widget  (self.header_frame,r, c+4, padx= 20,  sticky = 'nw', columnspan=1,
                        lab= "Particular attention is paid to achieving same curvature\n" + 
                             "on upper and lower side at leading edge.",
                             text_style=STYLE_COMMENT)

        self.header_frame.grid_columnconfigure (5, weight=1)

        # ----------- Input - with 2 sub frames  ----------------

        r,c = 0,0  
        Header_Widget (self.input_frame,r,c,   padx=0, lab= "Bezier airfoil")
        Label_Widget  (self.input_frame,r,c+1, padx=20, pady=10, columnspan=1,
                       lab= "sides with curvature which should match target")
        r +=1
        bezier_frame = ctk.CTkFrame(self.input_frame, fg_color="transparent")
        bezier_frame.grid(row=r, column=c, columnspan=2, sticky="nwe", padx=(0,10), pady=(0,10))

        self._init_bezier_frame (bezier_frame) 

        r = 0
        c += 2
        Header_Widget (self.input_frame,r,c,   padx=0, lab= f"Target   '{self.airfoilOrg.name}'")
        r +=1
        target_frame = ctk.CTkFrame(self.input_frame, fg_color="transparent")
        target_frame.grid(row=r, column=c, columnspan=2, sticky="nwe", padx=(0,10), pady=(0,10))

        self._init_target_frame (target_frame) 

        self.input_frame.grid_columnconfigure (1, weight=1)
        self.input_frame.grid_columnconfigure (3, weight=4)

         # ---- buttons are in super class - add additional open button 

        Button_Widget (self.button_frame,0,0, lab='Open .bez', set=self.open_bez, style=SECONDARY, 
                       padx=(0,50), width=90)

        # ----------- init Diagram frame at the end ---------------

        self.diagram_frame  = Diagram_Airfoil_Bezier (self.edit_frame, 
                                    self.airfoilOrg, self.airfoil, size=(12,5.2),
                                    view_frame=self.view_frame) 
        self.diagram_frame.grid(row=1, column=1, padx=(5,5), sticky="news")
        # set width of view_frame 
        self.edit_frame.grid_columnconfigure (0, weight=0, minsize=170)

        return


    def _init_bezier_frame (self, frame: ctk.CTkFrame): 
        # init sub frame with data of Bezier airfoil 

        r,c = 0,0  
        Label_Widget  (frame,r,c, padx= 0, lab= "Side", width=80, columnspan=1)
        c +=1
        Label_Widget  (frame,r,c, padx=20, lab= "Ctrl Points", width=60, columnspan=1)
        c +=2
        Blank_Widget  (frame, r,c, width=10)
        c += 1
        Label_Widget  (frame,r,c, padx=10, lab= "LE  curvature  TE", width=90, columnspan=3)
        c +=4
        Blank_Widget  (frame, r,c, width=30)
        c +=1
        Label_Widget  (frame,r,c, padx=10, lab= "Deviation Bezier to target ", width=50, columnspan=2)

        frame.grid_columnconfigure (c+2, weight=1)
        r +=1

        for sideName in [UPPER, LOWER]:

            r += 1
            c = 0 
            self.add (Label_Widget (frame,r,c, padx=(30,0), lab=f"{sideName}", width=60, columnspan=1))
            c +=1
            self.add (Field_Widget  (frame,r,c, width=90, lab_width=100,
                                    get=self.nPoints, set=self.set_nPoints, objId = sideName,
                                    spin=True, step=1, lim=(3,10)))
            c +=3
            self.add (Field_Widget  (frame,r,c, get=self.curv_at_le, objId = sideName,width=50, dec=0))
            c +=2
            self.add (Field_Widget  (frame,r,c, get=self.curv_at_te, objId = sideName,width=50, dec=1))
            c +=3
            self.add (Field_Widget  (frame,r,c, get=self.deviation_norm2, objId = sideName,width=70, dec=5))
            c +=1
            self.add (Button_Widget (frame,r,c, width=100, padx=20, objId = sideName,
                                    lab=f"Match {sideName}", set=self.match_side_bezier, 
                                    disable=self.match_side_bezier_disabled))
        c = 0 
        r += 1
        self.add (Field_Widget  (frame,r,c,   lab="TE gap", obj=self.airfoil, 
                                get='teGap_perc', set='set_teGap_perc', step=0.01,
                                spin=True, width=90, lab_width=70, unit="%", dec=2,
                                event=self.changed_te_gap))
        Label_Widget (frame,r,c+3, padx=0, columnspan=8, text_style=STYLE_DISABLED,
                      lab= "= move last Bezier point up/down (Match will overwrite)")


    def _init_target_frame (self, frame : ctk.CTkFrame): 
        # init sub frame with data of target airfoil 

        r,c = 0,0  
        Label_Widget  (frame,r,c, padx= 0, lab= "Side", width=80, columnspan=1)
        c +=1
        Label_Widget  (frame,r,c, padx=10, lab= "LE  curvature  TE", width=90, columnspan=3)

        r +=1

        for sideName in [UPPER, LOWER]:
            curv = self.airfoilOrg.geo.curvature.side(sideName)
            r += 1
            c = 0 
            self.add (Label_Widget (frame,r,c, padx=(30,0), lab=f"{sideName}", width=60, columnspan=1))
            c += 1
            self.add (Field_Widget  (frame,r,c, val=curv.y[0], width=50, dec=0))
            c +=2
            self.add (Field_Widget  (frame,r,c, val=curv.y[-1],width=50, dec=1))
            c +=2
            self.add (Label_Widget (frame,r,c, padx=(0,0), lab=self.curv_warning, objId=sideName,
                                    columnspan=1, width=20, text_style=STYLE_WARNING))

        self.add (Field_Widget  (frame,r-1,c+1, width=90, lab="LE target will be", lab_width=65,
                        padx=10,  
                        get=lambda: self.target_curv_at_le, set=self.set_target_curv_at_le, 
                        dec=0, spin=True, step=5, lim=(10,1000)))
        
        self.add (Field_Widget  (frame,r,c+1, width=90, lab="Weighting", lab_width=65,
                        padx=10,  
                        get=lambda: self.target_curv_at_le_weighting, set=self.set_target_curv_at_le_weighting, 
                        dec=1, spin=True, step=0.5, lim=(0.1,10)))

        frame.grid_columnconfigure (c+4, weight=1)

        c = 0 
        r += 1
        self.add (Field_Widget  (frame,r,c,   lab="TE gap", width=50, lab_width=70,
                                 obj=self.airfoilOrg, get='teGap_perc', dec=2,))



    def curv_at_le (self, objId):
        """ curvature at le"""
        if objId == UPPER:  curv = self.airfoil.geo.curvature.upper
        else:               curv = self.airfoil.geo.curvature.lower
        return curv.y[0]


    def curv_at_te (self, objId):
        """ curvature at te"""
        if objId == UPPER:  curv = self.airfoil.geo.curvature.upper
        else:               curv = self.airfoil.geo.curvature.lower
        return curv.y[-1]


    @property
    def target_curv_at_le (self):
        """ target curvature at le to achieve"""
        if self._target_curv_at_le is None: 
            return self.airfoilOrg.geo.curvature.best_around_le
        else: 
            return self._target_curv_at_le

    def set_target_curv_at_le (self, aVal):
        """ manual set / overwrite auto target curvature"""
        self._target_curv_at_le = aVal
        self.matcher_upper.set_target_curv_at_le (self.target_curv_at_le) 
        self.matcher_lower.set_target_curv_at_le (self.target_curv_at_le) 


    @property
    def target_curv_at_le_weighting (self):
        """ target curvature at le weighting"""
        return self.matcher_upper._target_curv_at_le_weighting 

    def set_target_curv_at_le_weighting (self, aVal):
        """ manual set / overwrite weighting for target_le """
        self.matcher_upper.set_target_curv_at_le_weighting (aVal) 
        self.matcher_lower.set_target_curv_at_le_weighting (aVal) 


    def curv_warning (self, objId):
        """ warning text if curv around le of target differs on upper and lower """
        warn = []
        at_le = self.airfoilOrg.geo.curvature.at_le
        best  = self.airfoilOrg.geo.curvature.best_around_le
        if objId == UPPER:  
            max  = self.airfoilOrg.geo.curvature.max_upper_le
            bump = self.airfoilOrg.geo.curvature.bump_at_upper_le
        else: 
            max = self.airfoilOrg.geo.curvature.max_lower_le
            bump = self.airfoilOrg.geo.curvature.bump_at_lower_le

        if max != at_le: 
            warn = f"max around LE is {max:.0f}"
        elif bump:
            warn = f"bump close to LE"

        # if best != at_le: 
        #     warn.append(f"target will be {best:.0f}")
        #     warn = ", ".join (warn) 
        return warn


    def deviation_norm2 (self, objId):
        """ norm2 deviation current bezier to target"""
        if objId == UPPER:  norm2 = self.matcher_upper.norm2
        else:               norm2 = self.matcher_lower.norm2
        return norm2


    def nPoints(self, objId):
        """ number of control points upper/lowerfor entry field"""
        if objId == UPPER:  return self.airfoil.geo.upper.nPoints
        else:               return self.airfoil.geo.lower.nPoints


    def set_nPoints(self, nPoints, objId):
        """ set number of control points from entry field"""

        if nPoints < 3 or nPoints > 10: return 

        sideName = objId
        if sideName == UPPER: 
            matcher = self.matcher_upper
        else: 
            matcher = self.matcher_lower
        matcher.bezier.set_npoints (nPoints)
        matcher.set_initial_bezier()

        # update diagram                                        
        self.airfoil.reset()                                    # make splined curves like thickness invalid 
        fireEvent  (self.ctk_root, AIRFOIL_CHANGED)             # update diagram 

        self.refresh()


    def match_side_bezier (self, objId): 
        """ match upper or lower bezier curve to 'original' airfoil """

        sideName = objId
        if sideName == UPPER: 
            matcher = self.matcher_upper
            side    =self.airfoil.geo.upper
        else: 
            matcher = self.matcher_lower
            side    =self.airfoil.geo.lower

        matcher.reset()                                         # reset statistic info 

        #---------- run optimization with nelder mead ---------------------

        messageFn = lambda: (f"Matching Bezier to {sideName} side of \n\n{self.airfoilOrg.name} \n\n\n"
                           + f" {matcher.get_nevals():4d} evaluations") 

        Eval_With_ToolWindow (self, matcher.run, messageFn)

        self.airfoil.reset()                                    # make splined curves like thickness invalid 
        fireEvent  (self.ctk_root, AIRFOIL_CHANGED)             # update diagram 
        self._match_result_info (side, matcher)                 # user info 


    def _match_result_info (self, side: Side_Airfoil_Bezier, matcher: Match_Side_Bezier):
        # info message for user 

        ncp         = matcher.ncp
        ncp_var     = matcher.ncp - 2
        deviation   = matcher.norm2
        niter       = matcher.niter
        max_reached = matcher.max_reached
        ntarget     = matcher.ntarget
        le_diff     = matcher.le_curv_diff
        title = f"Match Bezier {side.name} side"

        good_deviation = deviation < 0.001
        good_le_diff   = le_diff < 10 
         
        if good_deviation and good_le_diff:         
            text = f"Optimization at {ncp_var} control points successful.\n"  
            icon = "check"
        else:
            text = f"Optimization at {ncp_var} control points not too good.\n\n"  
            if not good_deviation:
                text = text + f"y-deviation at {ntarget} test points: {deviation:.5f} \n"
            if not good_le_diff:
                text = text + f"Curvature difference at leading edge: {le_diff:.0f} \n"
            icon = "info"

        if max_reached:
            text = text + f"\nMax number of {niter} iterations exceeded.\n" 

        # user hint 

        hint = None
        if ncp >= 7 and not good_le_diff:
            hint = f"The algorithm could be confused... \nTry again with {ncp-1} control points."
        elif ncp <= 5 and (not good_le_diff or not good_deviation): 
            hint = f"Give it a try with {ncp+1} control points to improve results."
        elif not good_le_diff: 
            hint = f"Adjust LE target curvature to to improve results."

        if hint: 
            text = text + "\n\n" + hint
            height = 230
        else: 
            height = 180

        msg = Messagebox (self, height=height, title=title, message=text, icon=icon, option_1="Ok")
        msg.get()


    def match_side_bezier_disabled (self, objId): 
        # adapt bezier only for 4 and 5 point bezier 

        if objId == UPPER: 
            return self.airfoil.geo.upper.nPoints < 4 or self.airfoil.geo.upper.nPoints > 8 
        else:
            return self.airfoil.geo.lower.nPoints < 4 or self.airfoil.geo.lower.nPoints > 8 


    def open_bez(self): 
        # open & load a bezier curves deinition from file 

        filetypes  = [('Airfoil bezier files', '*.bez')]
        newPathFilename = filedialog.askopenfilename(
                    title='Select airfoil bezier definition file',
                    initialdir=os.getcwd(),
                    filetypes=filetypes)
        if newPathFilename:                    # user pressed open
            ok = self.airfoil.load_bezier(newPathFilename)
            if not ok:
                msg = Messagebox(self, title="Reading Bezier file", 
                                 message= "Could not load '%s'.\n\nError in file format." %newPathFilename,
                                 icon='cancel')
                msg.get()

            else: 
                self._matcher_upper = None      # controller object to match side         
                self._matcher_lower = None      # controller object to match side      

            fireEvent  (self.ctk_root, AIRFOIL_CHANGED)              # update diagram 
            self.title ("Design Bezier airfoil  [" + self.airfoil.name + "]")
            self.nameWidget.refresh()
            self.refresh()

   

    def _save_fileTypes (self):
        # returns the fileTypes for save message
        return "'.dat' and '.bez'" 


    def changed_te_gap (self):
        # callback when te gap was changed in entry field 

        # make splined curves like thickness invalid 
        self.airfoil.reset()     
        fireEvent  (self.ctk_root, AIRFOIL_CHANGED)              # update diagram 
        self.refresh()



#-------------------------------------------------------------------------------
# The App   
#-------------------------------------------------------------------------------

# class AirfoilEditor (ctk.CTkToplevel):
# class AirfoilEditor (ctk.CTk):
class AirfoilEditor ():
    '''

        The AirfoilEditor App

        If parentApp is passed, the AirfoilEditor is called from eg PlanformEditor,
        so it will be modal with a reduced File Menu 
    '''

    name = AppName  

    def __init__(self, airfoilFile, parentApp=None):


        # called from another App? Switch to modal window
        #
        #   self is not a subclass of ctk to support both modal and root window mode 

        self.parentApp = parentApp
        self.ctk_root  = None                       # will be set later 
        self.main      = None                       # the topLevel widget 
        self.diagram   = None                       # diagram widget 
        self.isModal   = not self.parentApp is None
        self.return_OK = True
        self.initial_geometry = None                # window geometry at th ebginning

        # get defaults from settings file 

        Airfoil.nPanels_default  = Settings().get('nPanels_default',  default=Airfoil.nPanels_default)
        Airfoil.le_bunch_default = Settings().get('le_bunch_default', default=Airfoil.le_bunch_default)
        Airfoil.te_bunch_default = Settings().get('te_bunch_default', default=Airfoil.te_bunch_default)

        # create windows - set window size or maximized 

        if self.isModal: 

            main = ctk.CTkToplevel (parentApp)          # modal - inherit ctk mode from parent

            set_initialWindowSize (main, widthFrac=0.80, heightFrac=0.75)
            main.transient (parentApp)
            # bug fix titlebar color https://github.com/TomSchimansky/CustomTkinter/issues/1930
            main.after(20, lambda: main._windows_set_titlebar_color(main._get_appearance_mode()))

            self.return_newAirfoilPathFileName = None   # return value for parent
            self.return_newAirfoilName         = None   # return value for parent

        else: 

            main = ctk.CTk()  

            geometry = Settings().get('window_geometry', None)
            set_initialWindowSize(main, widthFrac=0.85, heightFrac=0.75, geometry=geometry)
            main.after (800,  lambda: dummy_window_resize (main))   # handle matplotlib bug for win scale <> 1.0
            main.after (1000, self.save_win_geometry)               # get geoemtry after startup geometry management

        self.main = main 
        self.ctk_root = main        


        # check and load the passed airfoil(s)

        self.airfoilFiles =[]
        self.curAirfoilFile = None

        if not airfoilFile:                            # show a demo airfoil 
            airfoilFile = "Root_Example"
            self.airfoilFiles.append (airfoilFile)
        else: 
            if os.path.isfile (airfoilFile):
                self.airfoilFiles= self._getAirfoilFiles_sameDir (airfoilFile)
            else:
                ErrorMsg ("'%s' does not exist. AirfoilEditor aborted." % airfoilFile) 
                self.return_OK = False
                self.cancel()
                return
        self.loadNewAirfoil (airfoilFile, initial=True)

        # create main frames        
 
        switches_frame = ctk.CTkFrame     (main, width=180)           # corresponds to grid minsize
        edit_frame     = ctk.CTkFrame     (main, fg_color= 'transparent')
        file_frame     = Edit_File_Menu   (main, self.curAirfoil, myApp=self)

        # maingrid 2 x 2 - diagram on top, file and edit on bottom
        main.grid_rowconfigure   (0, weight=2)
        main.grid_rowconfigure   (1, weight=0)
        main.grid_columnconfigure(0, weight=0, minsize=180)           # corresponds to frame width ...
        main.grid_columnconfigure(1, weight=1)

        switches_frame.grid  (row=0, column=0, pady=(5,5), padx=(5,0), ipady=5,sticky="news")
        file_frame.grid      (row=1, column=0, pady=(0,5), padx=(5,0), ipady=5,sticky="news")
        edit_frame.grid      (row=1, column=1, pady=0,     padx=0,     sticky="nesw")

        # different sections of the edit area 1 x 3 
        edit_frame.grid_rowconfigure      (0, weight=1)
        edit_frame.grid_columnconfigure   (4, weight=1)

        edit_Airfoil_frame    = Edit_Airfoil_Data   (edit_frame, self.curAirfoil, myApp=self)
        edit_Airfoil_frame.grid   (row=0, column=0, pady=(0,5), padx=(5,0), ipadx=10, sticky="news")

        edit_Panels_frame  = Edit_Panels (edit_frame, self.curAirfoil, myApp=self)
        edit_Panels_frame.grid (row=0, column=1, pady=(0,5), padx=(5,0), ipadx=10, sticky="news")

        edit_Coordinates_frame  = Edit_Coordinates (edit_frame, self.curAirfoil, myApp=self)
        edit_Coordinates_frame.grid (row=0, column=2, pady=(0,5), padx=(5,0), ipadx=10, sticky="news")

        edit_Curvature_frame  = Edit_Curvature (edit_frame, self.curAirfoil, myApp=self)
        edit_Curvature_frame.grid (row=0, column=3, pady=(0,5), padx=(5,0), ipadx=10, sticky="news")

        edit_Specials_frame  = Edit_Specials (edit_frame, self.curAirfoil, myApp=self)
        edit_Specials_frame.grid (row=0, column=4, pady=(0,5), padx=(5,5), ipadx=10, sticky="news")


        self.diagram  = Diagram_Airfoil  (main, self.curAirfoil, view_frame=switches_frame)  # , size=[20,8]
        self.diagram.grid   (row=0, column=1, pady=(5,5), padx=(5,5), sticky="news")

        # start App - run mainloop if self is not modal otherise control is at parent

        if not self.isModal: 
            main.protocol("WM_DELETE_WINDOW", self.onExit)  # intercept app close by user 
            main.mainloop() 
        else:
            main.protocol("WM_DELETE_WINDOW", self.cancel)  # intercept app close by user 
            main.focus_force() 
            main.grab_set()
      

    # ------------------

    def curAirfoil (self) -> Airfoil:
        """ encapsulates current airfoil. Childs should acces only via this function
        to enable a new airfoil to be set """
        return self._curAirfoil


    def set_curAirfoil (self, aNew, initial=False):
        """ encapsulates current airfoil. Childs should acces only via this function
        to enable a new airfoil to be set """
        self._curAirfoil = aNew
        # mega alarm - inform everything
        if not initial: 
            fireEvent (self.ctk_root, AIRFOIL_NEW)

    def refresh_airfoilFiles (self):
        """ refreshes the list of airfoil files in current directory"""
        self.airfoilFiles = self._getAirfoilFiles_sameDir (self.curAirfoil().pathFileName)


    def set_curAirfoilPathFileName (self, newPathFileName): 
        # set new current airfoil by pathFileName 

        self.refresh_airfoilFiles()
        
        for aPathFileName in self.airfoilFiles:
            if newPathFileName == aPathFileName:
                self.loadNewAirfoil (aPathFileName)
                break


    def _getAirfoilFiles_sameDir (self, anAirfoilFile): 
        """ 
        Returns the list of airfoilFiles path in the same directory as anAirfoilFile
        All .dat, .bez and .hicks files are collected 
        """

        if os.path.isfile (anAirfoilFile):
            airfoil_dir = os.path.dirname(anAirfoilFile) 
            if airfoil_dir == '': airfoil_dir = '.'
            dat_files = fnmatch.filter(os.listdir(airfoil_dir), '*.dat')
            bez_files = fnmatch.filter(os.listdir(airfoil_dir), '*.bez')
            hh_files  = fnmatch.filter(os.listdir(airfoil_dir), '*.hicks')
            airfoil_files = dat_files + bez_files + hh_files
            airfoil_files = [os.path.normpath(os.path.join(airfoil_dir, f)) \
                                for f in airfoil_files if os.path.isfile(os.path.join(airfoil_dir, f))]
            return sorted (airfoil_files)
        else:
            return []


    def _set_title(self):
        """ sets window title of self """
        self.main.title (AppName + "  v" + str(AppVersion) + "  [" + self.curAirfoil().fileName + "]")


    def edit_settings (self):
        """ file menu edit settings """

        dialog = Dialog_Airfoil_Settings(self.main, name=self.name)
        self.main.wait_window (dialog)

        Airfoil.nPanels_default  = Settings().get('nPanels_default',  default=Airfoil.nPanels_default)
        Airfoil.le_bunch_default = Settings().get('le_bunch_default', default=Airfoil.le_bunch_default)
        Airfoil.te_bunch_default = Settings().get('te_bunch_default', default=Airfoil.te_bunch_default)



    #------- file functions ----------------

    def open (self):
        """ open a new wairfoil and load it"""

        filetypes  = [('Airfoil files', '*.dat'), ('Bezier files', '*.bez'), ('Hicks Henne files', '*.hicks')]
        newPathFilenames = filedialog.askopenfilenames(
                    title='Select one or more airfoils',
                    initialdir=os.getcwd(),
                    filetypes=filetypes)
        if newPathFilenames:                    # user pressed open
            newPathFilenames = list(newPathFilenames)
            if len (newPathFilenames) <= 1:     # ... selected one file 
                self.airfoilFiles= self._getAirfoilFiles_sameDir (newPathFilenames[0])
            else:                               # ... and selected many files 
                self.airfoilFiles = list(newPathFilenames)
            self.loadNewAirfoil (newPathFilenames[0])


    def loadNewAirfoil(self, pathFilename, airfoil= None, initial=False):
        """loads and sets a new airfoil - updates title """

        if airfoil is None: 
            if pathFilename == "Root_Example":
                airfoil = Root_Example()
            else:
                extension = os.path.splitext(pathFilename)[1]
                if extension == ".bez":
                    airfoil = Airfoil_Bezier (pathFileName=pathFilename)
                elif extension == ".hicks":
                    airfoil = Airfoil_Hicks_Henne (pathFileName=pathFilename)
                else: 
                    airfoil = Airfoil(pathFileName=pathFilename, geometry=GEO_BASIC)
            airfoil.set_usedAs (NORMAL)
        airfoil.load()

        if airfoil.isLoaded:                            # could have been error in loading
            self.curAirfoilFile = airfoil.pathFileName
            if self.main:                               # during startup there is no main 
                self.set_curAirfoil (airfoil, initial=initial)
                self._set_title()


    def ok_return(self): 
        """ modal mode: close and hand over current airfoil file to parent app"""
        if self.isModal:
            self.return_OK = True
            self.return_newAirfoilPathFileName = self.curAirfoil().pathFileName
            self.return_newAirfoilName = self.curAirfoil().name
            self.onExit()


    def cancel(self): 
        """ modal mode: close without checks"""
        if self.isModal:
            self.return_OK = False
            self.onExit()


    def save_win_geometry (self): 
        """ save the geometry to compare it when window will be closed """

        if self.main.state() == 'zoomed':
            self.initial_geometry = 'zoomed'
        else: 
            self.initial_geometry = self.main.winfo_geometry()




    def onExit(self): 
        """ interception of user closing the app - check for changes made"""
        self.ctk_root = None

        if not self.isModal:
            # store window pos and size in settings if user changed it 
            if self.main.state() == 'zoomed':
                cur_geometry = 'zoomed'
            else: 
                cur_geometry = self.main.winfo_geometry()
            if cur_geometry != self.initial_geometry:
                Settings().set('window_geometry', cur_geometry)

        self.main.destroy()



#--------------------------------

if __name__ == "__main__":

    # init logging  

    if os.path.isdir(os.path.dirname(os.path.realpath(__file__)) +"\\test_airfoils"):  # dev environment 
        init_logging (level= logging.DEBUG)             # INFO, DEBUG or WARNING
    else:                       
        init_logging (level= logging.WARNING)

    # ctk application settings and scalings prior to init 

    Settings.belongTo (__file__, msg=True)
    apply_scaling_from_settings ()

    # set matpltlib defaults 
        
    set_font_size (Settings().get('plot_font_size', default=10))


    # command line arguments? 
    
    parser = argparse.ArgumentParser(prog=AppName, description='View and edit an airfoil')
    parser.add_argument("airfoil", nargs='*', help="Airfoil .dat or .bez file to show")
    args = parser.parse_args()

    if args.airfoil: 
        airfoil_file = args.airfoil[0]
        if not os.path.isfile (airfoil_file):
            ErrorMsg ("Airfoil file '%s' doesn't exist" %airfoil_file )
            sys.exit(1)
    else: 
        if os.path.isdir(".\\test_airfoils"):
            airfoil_dir   =".\\test_airfoils"
            airfoil_files = [os.path.join(airfoil_dir, f) for f in os.listdir(airfoil_dir) if os.path.isfile(os.path.join(airfoil_dir, f))]
            airfoil_files = [f for f in airfoil_files if f.endswith('.dat')]       
            airfoil_files = sorted (airfoil_files)
            airfoil_file = airfoil_files[0]
            NoteMsg ("No airfoil file as argument. Showing example airfoils in '%s'" %airfoil_dir)
        else:
            NoteMsg ("No airfoil file as argument. Showing example airfoil...")
            airfoil_file = None

    AirfoilEditor (airfoil_file)
    