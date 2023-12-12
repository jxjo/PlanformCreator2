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

import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec

from tkinter import filedialog
import customtkinter as ctk

# let python find the other modules in modules relativ to path of self  
sys.path.append(os.path.join(Path(__file__).parent , 'modules'))

from common_utils       import * 

from airfoil            import Airfoil, Airfoil_Bezier, GEO_BASIC, GEO_SPLINE
from airfoil            import NORMAL
from airfoil_geometry   import Geometry, Side_Airfoil_Bezier, UPPER, LOWER
from airfoil_geometry   import Match_Geo_Bezier, Match_Side_Bezier

from airfoil_examples   import Root_Example
from airfoil_artists    import *

from widgets            import *
from ui_base            import Dialog_Abstract, Edit_Abstract, Diagram_Abstract              
from ui_base            import set_initialWindowSize, Dialog_Settings              


#------------------------------------------------

AppName    = "Airfoil Editor"
AppVersion = "1.1.beta"


# tk change events for updating mainly plots or vice versa 

AIRFOIL_NEW                 = "<<AIRFOIL_NEW>>"                
AIRFOIL_CHANGED             = "<<AIRFOIL_CHANGED>>"
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

        r,c = 0,0 
        Header_Widget (self,r,c, lab=self.name, width=80)
        Button_Widget (self,r,c, icon_name='settings', sticky="e",  style=ICON, 
                       set=self.myApp.edit_settings)

        # if AirfoilEditor called stand alone offer combobox with input files
        if not self.myApp.isModal:
            r +=1 
            self.add(Option_Widget (self,r,c, get=self.curAirfoilFileName, set=self.set_curAirfoilFileName,
                                            options=self.airfoilFileNames,
                                            spin=True, spinPos='below', width=100, padx=10, pady=4, sticky = 'ew',))
            r +=2 
            self.add(Button_Widget (self,r,c, lab='Open',        width=100, pady=4, sticky = 'ew', set=self.myApp.open))

        # if called from parentApp offer "Ok and return" and "Cancel"
        if self.myApp.isModal:
            r +=1 
            self.add(Button_Widget (self,r,c, lab='Ok & Return', style=PRIMARY,       
                                    width=120, pady=4, sticky = 'ew', set=self.myApp.ok_return))
            r +=1 
            self.add(Button_Widget (self,r,c, lab='Cancel', width=120, pady=4, sticky = 'ew', set=self.myApp.cancel))
 
        r +=1 
        Blank_Widget (self,r,c,  height= 50)


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
        self.add (Button_Widget  (self,r,c+3, lab='Modify geometry', width=110, padx= (30,0), columnspan=4, sticky='w', 
                                set=self.modify_airfoil ))

        r += 1
        Blank_Widget (self, r,0)    
        c += 1                                  # left blank colum to inset the fields 
        self.add (Field_Widget  (self,r,c, columnspan= 8 ,lab='Name', obj=self.airfoil, get='name',
                                 width=190, lab_width=80, justify='left'))
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
        r += 1
        self.add (Label_Widget  (self,r,c+1, padx=0,  pady=(5,0), lab=lambda : self.airfoil().geo.description))
 

    def modify_airfoil (self): 
        """ Open thickness and camber dialog - if 'OK' the modified airfoil will be inserted to the list 
        of airfoils and set to current """

        dialog = Dialog_Geometry (self, self._airfoilFn )
        self.wait_window (dialog)

        if dialog.return_OK: 
            self.myApp.add_toAirfoilFiles (dialog.return_newAirfoilPathFileName)




class Edit_Curvature(Edit_Abstract_Airfoil):
    """ 
    Frame to edit main data of the airfoil like thickness. This is just the header
    """
    name = "Curvature"

    @property
    def myApp (self) -> 'AirfoilEditor':
        return super().myApp

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
        self.add (Field_Widget  (self,r,c,   lab="Reversals upper", get=lambda: self.geo.curvature.upper.nreversals,
                                width=50, lab_width=100, set='', dec=0, disable= True))
       
        r += 1
        self.add (Field_Widget  (self,r,c, lab="lower", get=lambda: self.geo.curvature.lower.nreversals, 
                                width=50, lab_width=60, padx=(20,0), set='', dec=0, disable= True))
        r += 1
        self.add (Field_Widget  (self,r,c,   lab="... threshold", get=lambda: self.curvature_threshold, 
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

    @property
    def myApp (self) -> 'AirfoilEditor':
        return super().myApp

    

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
            self.myApp.add_toAirfoilFiles (dialog.return_newAirfoilPathFileName)

    def blend_airfoils (self): 
        """ Open dialog to blend airfoil with another one  """

        dialog = Dialog_Blend_Airfoils (self, self._airfoilFn )
        self.wait_window (dialog)

        if dialog.return_OK: 
            self.myApp.add_toAirfoilFiles (dialog.return_newAirfoilPathFileName)




class Edit_Panels(Edit_Abstract_Airfoil):
    """ 
    Frame to edit main data of the airfoil like thickness. This is just the header
    """
    name = "Panels"

    def init (self):

        r, c = 0, 0 
        self.add (Header_Widget (self,r,c, width= 70, lab=self.name, columnspan= 2))
        self.add (Button_Widget  (self,r,c+3, lab='Repanel', width=80, padx= (10,0), columnspan=2, sticky='w', 
                                 set=self.repanel_airfoil, text_style=lambda: self.style_repanel() ))

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

        dialog = Dialog_Repanel (self, self._airfoilFn )
        self.wait_window (dialog)

        if dialog.return_OK: 
            self.myApp.add_toAirfoilFiles (dialog.return_newAirfoilPathFileName)




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
                                 set=self.normalize_airfoil, text_style=lambda: self.style_normalize() ))

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
        self.add(Label_Widget  (self,r, c, columnspan=9, padx= 5, text_style = 'Warning',
                                lab= lambda: self.messageText()))

    def normalize_airfoil (self): 
        """open normalize dialog window - create new noramlized airfoil on demand"""

        dialog = Dialog_Normalize (self, self._airfoilFn )
        self.wait_window (dialog)

        if dialog.return_OK: 
            self.myApp.add_toAirfoilFiles (dialog.return_newAirfoilPathFileName)


    def messageText (self): 

        text = []

        if self.geo.le[0] != 0.0 or self.geo.le[1] != 0.0:
            text.append("- Leading edge is not at 0,0")
        if self.geo.te[0] != 1.0 or self.geo.te[2] != 1.0 : 
           text.append("- Trailing edge is not at 1")
        
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

        self._show_points = False          # switch to show airfoil points
        self._show_upper  = False          # switch to show upper curvature
        self._show_lower  = False          # switch to show lower curvature
        self._show_camber = False          # switch to show camber / thickness
        self._show_logScale = True        # switch to have log scale for curvature 
        self._log_widget : Switch_Widget = None   # log switch widget

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
            gs = GridSpec(2, 1, height_ratios=[5, 4])
            self.ax1 : plt.Axes = self.figure.add_subplot(gs[0])
            self.ax2 : plt.Axes = self.figure.add_subplot(gs[1])
        else:
            gs = GridSpec(1, 1)
            self.ax1 : plt.Axes = self.figure.add_subplot(gs[0])
            self.ax2 = None

        self.figure.subplots_adjust(left=0.04, bottom=0.06, right=0.98, top=0.97, wspace=None, hspace=0.10)



    def setup_axes(self):
        """ setup axes, axis, artiss for this plot type """

        # airfoil contour 
        self.ax1.tick_params (labelbottom=self.ax2 is None, labelleft=True, labelsize='small')
        self.ax1.autoscale(enable=False, axis='both')
        self.ax1.set_xlim([-0.05,1.05])
        self.ax1.axis('equal')
        self.ax1.grid (visible=True)
        self.ax1.axhline(0, color=cl_labelGrid, linewidth=.5, alpha=0.6)

        # curvature
        if self.ax2:
            self.ax2.tick_params (labelsize='small')
            self.ax2.set_xlim([-0.05,1.05])

            if self.show_logScale:

                # ! linear range from 0..1 will lead to a kink in the curve 
                thresh  = 1 
                self.ax2.set_yscale('symlog', linthresh=thresh, linscale=1) # , linscale=2
                self.ax2.set_ylim([ -1000, 1000])
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


    def setup_artists(self):
        """ setup axes, axis, artists for this plot type """

        super().setup_artists()

        # airfoil is list to prepare for multiple airfoils to view 
        
        self.airfoilArtist   = Airfoil_Artist   (self.ax1,     self.airfoils, show=True)
        self.thicknessArtist = Thickness_Artist (self.ax1,     self.airfoils, show=False)

        if self.ax2: 
            self.curvatureArtist = Curvature_Artist (self.ax2,     self.airfoils, show=True)
            self.curvatureArtist.set_lower(self.show_lower)
            self.curvatureArtist.set_upper(self.show_upper)
        else: 
            self.curvatureArtist = None


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
        self.airfoilArtist.refresh (figureUpdate=True) 
        if self.curvatureArtist:
            self.curvatureArtist.set_points (aBool)
            self.curvatureArtist.refresh (figureUpdate=True) 

    @property
    def show_camber(self) -> bool: return self._show_camber
    def set_show_camber (self, aBool):
        self._show_camber = aBool
        self.thicknessArtist.set_show (aBool)

    @property
    def show_upper(self) -> bool: return self._show_upper
    def set_show_upper (self, aBool):
        self._show_upper = aBool
        # show on/off curvature diagram 
        if (not self.ax2 and aBool) or (self.ax2 and not self.show_lower and not self.show_upper):
            self.re_create_axes()
        if self.curvatureArtist:
            self.curvatureArtist.set_upper(aBool)
        self._log_widget.refresh()
        self.refresh()


    @property
    def show_lower(self) -> bool: return self._show_lower
    def set_show_lower (self, aBool):
        self._show_lower = aBool
        # show on/off curvature diagram 
        if (not self.ax2 and aBool) or (self.ax2 and not self.show_lower and not self.show_upper):
            self.re_create_axes()
        if self.curvatureArtist:
            self.curvatureArtist.set_lower(aBool)
        self._log_widget.refresh()
        self.refresh()


    @property
    def show_logScale(self) -> bool: return self._show_logScale
    def set_show_logScale (self, aBool):
        self._show_logScale = aBool
        self.setup_axes()
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

    def __init__(self, master, airfoilOrg, airfoilBezier, *args, **kwargs):

        self._airfoilBezier : Airfoil_Bezier = airfoilBezier

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
        return r, c


    # -------- switch call back 

    @property
    def show_original(self) -> bool: return self._show_original
    def set_show_original (self, aBool):
        self._show_original = aBool
        self.airfoilArtist.refresh(figureUpdate=True)

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
            gs = GridSpec(2, 1, height_ratios=[5, 4])
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
            self.ax2.autoscale(enable=False, axis='both')
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



#-------------------------------------------------------------------------------
# Dialogs for smaller tasks   
#-------------------------------------------------------------------------------


class Dialog_Airfoil_Abstract (Dialog_Abstract):
    """ 
    Superclass for modal dialog to modify airfoil properties  
    having 

        header_frame    - which is empty 
        diagramm_frame  - ! which is not initialized !
        switches_frame  - for diagram setting - if has_switches_frame = True        
        input_frame     - which is empty 
        button_frame    - with default buttons SaveAs, Ok, Cancel 

    """
    name       = "Modify airfoil"
    nameExt    = '-mod'                                     # default extension for new airfoil 

    def __init__(self, master, airfoilFn, workingDir=None, has_switches_frame=False, *args, **kwargs):

        self.has_switches_frame = has_switches_frame

        # make a copy of original airfoil as splined airfoil

        self.airfoilOrg = Airfoil.asCopy (airfoilFn(), geometry=GEO_SPLINE) 
        self.airfoilOrg.set_usedAs (NORMAL)

        # and a copy of original airfoil as splined airfoil, normalized - as working copy 

        self.airfoil    = Airfoil.asCopy (airfoilFn(), nameExt=self.nameExt, geometry=GEO_SPLINE) 
        self.airfoil.set_usedAs (DESIGN)
        self.hasbeen_normalized = False
        if not self.airfoil.isNormalized:                    # also LE of spline at 0,0? 
            self.hasbeen_normalized = self.airfoil.normalize ()    # ensure exact le based on spline

        title  = self.name +"   [" + self.airfoil.name + "]"

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
        # init UI ...

        # input field will fire this event when data is changed
        self.change_event = AIRFOIL_CHANGED

        self.return_newAirfoilPathFileName = None   # return value for parent

        # -------  Setup frames --------------------

        self.header_frame   = None                  # Header info and remarks
        self.diagram_frame  = None                  # diagram area - created in subclass 
        self.switches_frame = None                  # optional switches for diagram
        self.input_frame    = None                  # input fields 
        self.button_frame   = None                  # auto buttons like Ok or Save

        # main grid 4 x 2  (header , diagram, inputs, default-buttons ) 
        self.header_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.header_frame.grid(row=0, column=0, columnspan=2, sticky="nwe", padx=0, pady=(10,10))

        if self.has_switches_frame:
            self.switches_frame = ctk.CTkFrame(self.edit_frame, width=180, fg_color="transparent")
            self.switches_frame.grid(row=1, column=0, sticky="nsew")
        else:
            self.switches_frame = None

        self.input_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.input_frame.grid(row=2, column=1, sticky="nwe", padx=40, pady=10)

        self.button_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.button_frame.grid(row=3, column=1, sticky="wes", padx=40, pady=(10,20))


        if self.has_switches_frame:
            self.edit_frame.grid_columnconfigure (0, weight=0, minsize=180)
        else:
            self.edit_frame.grid_columnconfigure (0, weight=0, minsize=10)

        self.edit_frame.grid_columnconfigure (1, weight=2)
        self.edit_frame.grid_rowconfigure    (3, weight=1)


        # new airfoil name and default buttons commonly defined  
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
        self.add (Label_Widget (self.button_frame,r,c, text_style='Warning',
                                lab= lambda: self._save_warning(), width= 110))

        self.button_frame.grid_columnconfigure (9, weight=1)

        # changed bindings
        self.ctk_root.bind(self.change_event, self.refresh, add='+')


    def _save_warning (self): 
        if self.airfoil.isModified: 
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

        self.showRepaneled = True 

        # Header 
        c = 0 
        r = 0 
        Header_Widget (self.header_frame,r,c, pady=0, lab= "Repanel Airfoil", sticky = 'nw', width=100)
        
        Switch_Widget (self.header_frame,r,c+1, padx=(30,30), lab='Show repaneled airfoil',
                       get=lambda: self.showRepaneled, set=self.set_showRepaneled)

        Label_Widget  (self.header_frame,r, c+2, padx= 20, sticky = 'nw', columnspan=1,
                        lab= "For repaneling a spline is created with the existing points.\n" + \
                        "The spline will define a new 'real' leading edge of the airfoil contour.")
        Label_Widget  (self.header_frame,r, c+3, padx= 20,  sticky = 'nw', columnspan=1,
                        lab= "The new leading edge devides the airfoil in an upper and lower side,\n" + \
                        "which are repaneled by a cosinus function with a bunch at LE and TE.")

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
        self.add (Label_Widget (self.input_frame,r,c+3  , columnspan = 1, 
                                lab= lambda: "equals %d points" % self.airfoil.nPoints))

        self.input_frame.grid_columnconfigure (8, weight=2)

        # buttons are defined in super class 

        # ----------- setup diagram frame at the end ---------------

        # set specific diagram frame for this dialog 
        self.diagram_frame = Diagram_LeTe_Mini (self.edit_frame, self.airfoilList, size=(7.0,4.0))
        self.diagram_frame.grid(row=1, column=1, sticky="nwe")



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


    def refresh(self, *_):

        self.diagram_frame.refresh()
        super().refresh()




class Dialog_Normalize (Dialog_Airfoil_Abstract):
    """ 
    Dialog to normalize airfoil
    """
    name        ='Normalize airfoil'
    nameExt     ='-norm'
    widthFrac   = 0.5
    heightFrac  = 0.35

    def init (self):
        # init UI ...
        super().init()                          # basic airfoil layout in super 

        # Header 
        c = 0 
        r = 0 
        Header_Widget (self.header_frame,r,c, pady=0, lab= "Normalize Airfoil", sticky = 'nw', width=100)
        
        Label_Widget  (self.header_frame,r, c+1, padx= 20, sticky = 'nw', columnspan = 1, 
                        lab= "The airfoil is shifted, rotated and scaled\n" + \
                             "so LE will be at 0,0 and TE is symmetric at 1,y.")
        Label_Widget  (self.header_frame,r, c+2, padx= 10, sticky = 'nw', columnspan = 1,
                        lab= "Within an iteration the leading edge of the spline should\n" + \
                             "get close to 0,0 (to numerical issues mostly not exactly).")

        self.header_frame.grid_columnconfigure (3, weight=1)

        # fields 
        r,c = 0,0 
        Blank_Widget (self.input_frame, r,c, height=20)
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

        self.input_frame.grid_columnconfigure (9, weight=2)
        self.input_frame.grid_rowconfigure (0, weight=1)

        
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
    widthFrac   = 0.80
    heightFrac  = 0.72

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
        
        Switch_Widget (self.header_frame,r,c+1, padx=(30,30), lab='Show modified airfoil',
                       get=lambda: self.showModified, set=self.set_showModified)

        Label_Widget  (self.header_frame,r, c+2, padx= 15, sticky = 'nw', columnspan=1,
                        lab= "For geometry modifications a spline is created with the existing points.\n" + \
                             "Thickness and camber distribution is evaluated based on this spline.")
        Label_Widget  (self.header_frame,r, c+3, padx= 20,  sticky = 'nw', columnspan=1,
                        lab= "If geometry is modified, the airfoil will be\n" + \
                             "repaneled and normalized to ensure exact results.")

        self.header_frame.grid_columnconfigure (4, weight=1)

        # input fields normalized  airfoil 

        r = 0 
        c = 0 
        if self.hasbeen_normalized: 
            lab = "Working copy normalized " + self.airfoil.geo.description
            style = STYLE_WARNING
        else: 
            lab = "Working copy " + self.airfoil.geo.description
            style = STYLE_HINT
        Label_Widget (self.input_frame,r,c, padx=0, lab= lab, columnspan = 4,
                      text_style=style)

        r += 1
        Blank_Widget (self.input_frame, r,10, height=5)
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Thickness", obj=self.airfoil, 
                                get='maxThickness', set='set_maxThickness', step=0.01, 
                                spin=True, width=95, lab_width=70, unit="%", dec=2,
                                event=self.change_event))
        self.add (Field_Widget  (self.input_frame,r,c+3, lab="at", lab_width=20, obj=self.airfoil, 
                                get='maxThicknessX', set='set_maxThicknessX', step=0.1, 
                                spin=True, width=95, unit="%", dec=1,
                                event=self.change_event))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Camber", obj=self.airfoil, 
                                get='maxCamber', set='set_maxCamber', disable= 'isSymmetric',  
                                spin=True, width=95, lab_width=70, unit="%", dec=2, step=0.01,
                                event=self.change_event))
        self.add (Field_Widget  (self.input_frame,r,c+3, lab="at", lab_width=20, obj=self.airfoil, 
                                get='maxCamberX', set='set_maxCamberX', disable= 'isSymmetric',  
                                spin=True, width=95, unit="%", dec=1, step=0.1,
                                event=self.change_event))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="TE gap", obj=self.airfoil, 
                                get='teGap_perc', set='set_teGap_perc', step=0.01,
                                spin=True, width=95, lab_width=70, unit="%", dec=2,
                                event=self.change_event))

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

        self.diagram_frame = Diagram_Airfoil_Mini (self.edit_frame, self.airfoilList, 
                                                   size=(7.0,5.2))
        self.diagram_frame.grid(row=1, column=1, sticky="nwe")



    @property                               
    def chord (self):                            # sample chord length 
        return self._chord
    def set_chord (self, aVal): 
        if aVal > 0: self._chord= aVal
        super().refresh()


    def airfoilList (self):
        if self.showModified: 
            return [self.airfoilOrg, self.airfoil]
        else: 
            return [self.airfoilOrg]


    def set_showModified (self, aBool):
        self.showModified = aBool
        self.refresh ('') 
                    

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

    def __init__(self, master, airfoilFn, workingDir=None, *args, **kwargs):

        
        self.airfoilBlend = None        # airfoil to blend with - start none
        self._blendBy = 0.5 

        super().__init__ (master, airfoilFn, workingDir=workingDir, *args, **kwargs)


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
                             "airfoils are taken to calculate a new y-coordinate.")
        Label_Widget  (self.header_frame,r, c+2, padx= 20,  sticky = 'nw', columnspan=1,
                        lab= "The involved airfoils will be temporarly \n" + \
                             "repaneled and normalized to ensure exact results.")
        Label_Widget  (self.header_frame,r, c+3, padx= 20,  sticky = 'nw', columnspan=1,
                        lab= "Paneling information is taken from the more weighted airfoil.\n" + \
                             "The final airfoil will be evaluated with a spline for high precision.")

        self.header_frame.grid_columnconfigure (4, weight=1)

        r = 0 
        c = 0 
        lab = "Airfoils normalized " + self.airfoilOrg.geo.description
        style = STYLE_HINT
        Label_Widget (self.input_frame,r,c, padx=0, lab= lab, columnspan = 4,
                      text_style=style)
        c = 2 
        r += 1
        Blank_Widget (self.input_frame, r,c+2, width=25)
        self.add (Label_Widget  (self.input_frame,r,c+3, lab="blendy by", padx=(30,0), width=60, columnspan=1))

        r += 1
        self.add (Field_Widget  (self.input_frame,r,c, width=200, 
                                 get=lambda: self.airfoilOrg.name))
        c += 2
        self.add(Slider_Widget (self.input_frame,r,c, width=200, height=16, padx=(0,10), 
                                lim=(0, 1), step=0.01, columnspan=3, 
                                obj=self, get='blendBy', set='set_blendBy'))
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
                                spin=True, unit="", dec=2))

        self.input_frame.grid_columnconfigure ( 0,  weight=1)
        self.input_frame.grid_columnconfigure (12,  weight=2)

        # ---- buttons in super class

        # ----------- setup diagram frame at the end ---------------

        self.diagram_frame = Diagram_Airfoil_Mini (self.edit_frame, self.airfoilList, 
                                                   show_thickness= False, size=(5.5,4.0))
        self.diagram_frame.grid(row=1, column=1, sticky="nwe")



    @property                               
    def blendBy (self): return self._blendBy
    def set_blendBy (self, aVal): 
        self._blendBy= aVal
        if self.airfoil and self.airfoilBlend:
            self.airfoil.do_strak (self.airfoilOrg, self.airfoilBlend, self.blendBy, geometry=GEO_BASIC)
        self.refresh()


    def airfoilList (self):
        airfoils = [self.airfoilOrg]
        if self.airfoilBlend: airfoils.append(self.airfoilBlend)
        if self.airfoil:      airfoils.append(self.airfoil)
        return airfoils

    
    def select_airfoil(self):
        """ select airfoil with explorer and load it if possible """

        filetypes  = [('dat files', '*.dat')]
    
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
            self.airfoil = Airfoil.asCopy (self.airfoilOrg, nameExt=self.nameExt, geometry=GEO_BASIC) 
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

    widthFrac  = 0.92
    heightFrac = 0.80

    def __init__(self, master, airfoilFn, workingDir=None, *args, **kwargs):

        super().__init__ (master, airfoilFn, workingDir=workingDir, has_switches_frame=True,
                          *args, **kwargs)


    def init (self):
        # init UI ...

        super().init()                                              # basic airfoil layout in super 

        # overwrite

        if not self.airfoilOrg.isNormalized:                        # also LE of spline at 0,0? 
            self.airfoilOrg.normalize() 

        self.airfoil    = Airfoil_Bezier (name=self.airfoil.name, has_joined_sides=True) 
        self.airfoil.set_usedAs (DESIGN)                            # will indicate airfoil when plotted 

        self.showOrg = True 

        # react on changes in diagram made by mouse drag 
        self.ctk_root.bind(BEZIER_CHANGED_IN_ARTIST, self.refresh, add='+')

        # ----------- Input Fields  ----------------

        # Header 
        c = 0 
        r = 0 
        Header_Widget (self.header_frame,r,c, pady=0, lab= "Bezier airfoil", sticky = 'nw', width=150)
        
        Label_Widget  (self.header_frame,r, c+2, padx= 15, sticky = 'nw', columnspan=1,
                        lab= "Design a new airfoil based on a Bezier curve for upper and lower side.\n" + 
                             "Control points can be moved, inserted or deleted with the mouse")
        Label_Widget  (self.header_frame,r, c+3, padx= 20,  sticky = 'nw', columnspan=1,
                        lab= "'Match Bezier' will move the control points for a best fit\n" + 
                             "to the original airfoil (4 - 6 control points are currently supported)")

        self.header_frame.grid_columnconfigure (4, weight=1)

        # input fields normalized  airfoil 

        r,c = 0,0  
        Label_Widget (self.input_frame,r,c, padx=0, lab= "Bezier curve data of new airfoil", columnspan = 2)

        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,  lab="Upper points", 
                                 get=self.nPoints, 
                                 set=self.set_nPoints, objId = UPPER,
                                 spin=True, step=1, lim=(3,10), width=90, lab_width=100))
        self.add (Button_Widget (self.input_frame,r,c+3, width=100, padx=0, 
                                 lab='Match Bezier', set=lambda: self.match_side_bezier(UPPER), 
                                 disable=lambda: self.match_side_bezier_disabled(UPPER)))

        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,  lab="Lower points", 
                                 get=self.nPoints, 
                                 set=self.set_nPoints, objId = LOWER,
                                 spin=True, step=1, lim=(3,10), width=90, lab_width=100))
        self.add (Button_Widget (self.input_frame,r,c+3, width=100, padx=0, 
                                 lab='Match Bezier', set=lambda: self.match_side_bezier(LOWER), 
                                 disable=lambda: self.match_side_bezier_disabled(LOWER)))

        # ---------------
        self.add (Button_Widget (self.input_frame,r,c+4, width=150, padx=0, 
                                 lab='Match Bezier Airfoil', set=self.match_geo_bezier))
        # ---------------


        self.input_frame.grid_columnconfigure (7, weight=1)
        r,c = 0,8
        Label_Widget (self.input_frame,r,c, padx=0, lab= "Geometric data of new airfoil", columnspan = 2)

        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Thickness", obj=self.airfoil, 
                                get='maxThickness', 
                                disable=True, width=65, lab_width=70, unit="%", dec=2))
        self.add (Field_Widget  (self.input_frame,r,c+3, lab="at", lab_width=20, obj=self.airfoil, 
                                get='maxThicknessX',  
                                disable=True, width=65, unit="%", dec=1))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Camber", obj=self.airfoil, 
                                get='maxCamber', 
                                disable=True, width=65, lab_width=70, unit="%", dec=2))
        self.add (Field_Widget  (self.input_frame,r,c+3, lab="at", lab_width=20, obj=self.airfoil, 
                                get='maxCamberX',   
                                disable=True, width=65,  unit="%", dec=1))

        self.input_frame.grid_columnconfigure (15, weight=3)

        r += 1
        c = 0 
        self.add (Field_Widget  (self.input_frame,r,c,   lab="TE gap", obj=self.airfoil, 
                                get='teGap_perc', set='set_teGap_perc', step=0.01,
                                spin=True, width=90, lab_width=70, unit="%", dec=2,
                                event=self.changed_te_gap))
        Label_Widget (self.input_frame,r,c+3, padx=0, lab= "= move last point of Bezier curves up or down", columnspan = 2)

        # ---- buttons are in super class - add additional open button 

        r,c = 0,0 
        Button_Widget (self.button_frame,r,c, lab='Open .bez', set=self.open_bez, style=SECONDARY, 
                       padx=(0,50), width=90)


        # ----------- setup diagram frame at the end ---------------

        self.diagram_frame  = Diagram_Airfoil_Bezier (self.edit_frame, 
                                    self.airfoilOrg, self.airfoil, size=(16,6),
                                    view_frame=self.switches_frame) 
        self.diagram_frame.grid(row=1, column=1, pady=(5,5), padx=(5,5), sticky="news")

 

    def nPoints(self, objId):
        """ number of control points upper/lowerfor entry field"""
        if objId == UPPER:
            return self.airfoil.geo.upper.nPoints
        else: 
            return self.airfoil.geo.lower.nPoints


    def set_nPoints(self, nPoints, objId):
        """ set number of control points from entry field"""

        if nPoints < 3 or nPoints > 10: return 

        curveType = objId
        if curveType == UPPER: 
            side = self.airfoil.geo.upper
            side_target = self.airfoilOrg.geo.upper
        else:
            side = self.airfoil.geo.lower
            side_target = self.airfoilOrg.geo.lower

        side.set_controlPoints_closeTo (side_target, nPoints)

        # update diagram                                        
        self.airfoil.reset()                                    # make splined curves like thickness invalid 
        fireEvent  (self.ctk_root, AIRFOIL_CHANGED)             # update diagram 

        self.refresh()


    def match_geo_bezier (self): 
        """ match bezier curves of joined upper and lower to 'original' airfoil """

        opt = Match_Geo_Bezier (self.airfoil.geo, self.airfoilOrg.geo)
        message = f"Matching Beziers of {UPPER} and {LOWER } side\n\n to \n\n{self.airfoilOrg.name} ..." 

        Eval_With_ToolWindow (self, opt.run, message)

        self.airfoil.set_geo (opt.geo_result)

        # update diagram                                        
        self.airfoil.reset()                                    # make splined curves like thickness invalid 
        fireEvent  (self.ctk_root, AIRFOIL_CHANGED)             # update diagram 

        # user info 
        self._match_result_info (None, opt)


    def match_side_bezier (self, curveType): 
        """ adapt bezier curve to 'original' airfoil """

        if curveType == UPPER: 
            side        = self.airfoil.geo.upper
            side_target = self.airfoilOrg.geo.upper
        else:
            side        = self.airfoil.geo.lower
            side_target = self.airfoilOrg.geo.lower

        #---------- run optimization with nelder mead ---------------------

        opt = Match_Side_Bezier (side, side_target)
        message = f"Matching {curveType} side Bezier\n\n to \n\n{self.airfoilOrg.name} ..." 

        Eval_With_ToolWindow (self, opt.run, message)
        # tool = Eval_With_ToolWindow2 (self, opt.run, message)
        # opt.run ()
        # tool.close()

        side.set_controlPoints (opt.bezier.points)

        # update diagram                                        
        self.airfoil.reset()                                    # make splined curves like thickness invalid 
        fireEvent  (self.ctk_root, AIRFOIL_CHANGED)             # update diagram 

        # user info 
        self._match_result_info (side, opt)


    def _match_result_info (self, side: Side_Airfoil_Bezier, opt: Match_Side_Bezier):
        # info message for user 

        nvar        = opt.nvar
        deviation   = opt.norm2
        niter       = opt.niter
        max_reached = opt.max_reached
        ncp         = opt.ncp
        if side:
            title = f"Match Bezier {side.name} side"
        else: 
            title = f"Match Beziers for {UPPER} and {LOWER} side"

        if   nvar >= 7:
            good_deviation = 0.001
        elif nvar >= 5:
            good_deviation = 0.005
        else: 
            good_deviation = 0.01

        if not max_reached and deviation < good_deviation:         # 0.001
            text = "Optimization of %d control points with %d variables successful. \n\n" %(ncp, nvar) + \
                "y-deviation at check points: %.5f \n\n" %deviation + \
                "Iterations needed: %d" %niter
            icon = "check"
        else:
            text = "Optimization with %d variables not too good. \n\n" %(nvar) + \
                "y-deviation at check points: %.5f \n\n" %deviation 
            if max_reached:
                text = text + "Maximum number of iterations (%d) exceeded." %niter
            else: 
                text = text + "Iterations needed: %d" %niter
            icon = "info"

        msg = Messagebox (self, title=title, message=text, icon=icon, option_1="Ok")
        msg.get()


    def match_side_bezier_disabled (self, curveType): 
        # adapt bezier only for 4 and 5 point bezier 

        if curveType == UPPER: 
            return self.airfoil.geo.upper.nPoints < 4 or self.airfoil.geo.upper.nPoints > 7 
        else:
            return self.airfoil.geo.lower.nPoints < 4 or self.airfoil.geo.lower.nPoints > 7 


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
        self.isModal   = not self.parentApp is None
        self.return_OK = True


        # create windows 

        if self.isModal: 
            # modal - inherit ctk mode from parent
            main = ctk.CTkToplevel (parentApp)
            set_initialWindowSize (main, widthFrac=0.80, heightFrac=0.75)
            main.transient (parentApp)
            # bug fix titlebar color https://github.com/TomSchimansky/CustomTkinter/issues/1930
            main.after(20, lambda: main._windows_set_titlebar_color(main._get_appearance_mode()))

            self.return_newAirfoilPathFileName = None   # return value for parent
            self.return_newAirfoilName         = None   # return value for parent

        else: 
            main = ctk.CTk()  
            set_initialWindowSize(main, widthFrac=0.85, heightFrac=0.75)

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
 
        switches_frame = ctk.CTkFrame     (main, width=180)    # corresponds to grid minsize
        edit_frame     = ctk.CTkFrame     (main, height=300, fg_color= 'transparent')
        file_frame     = Edit_File_Menu   (main, self.curAirfoil, myApp=self)

        # maingrid 2 x 2 - diagram on top, file and edit on bottom
        main.grid_rowconfigure   (0, weight=2)
        main.grid_rowconfigure   (1, weight=0)
        main.grid_columnconfigure(0, weight=0, minsize=180)                     # corresponds to frame width ...
        main.grid_columnconfigure(1, weight=1)

        switches_frame.grid  (row=0, column=0, pady=(5,5), padx=(5,0), ipady=5,sticky="news")
        # diagram_frame.grid   (row=0, column=1, pady=(5,5), padx=(5,5), sticky="news")
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


        diagram_frame  = Diagram_Airfoil  (main, self.curAirfoil, view_frame=switches_frame, size=[20,8])
        diagram_frame.grid   (row=0, column=1, pady=(5,5), padx=(5,5), sticky="news")

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

    def add_toAirfoilFiles (self, aPathFileName):
        """ inserts a new airfoilPathFileName to the list right after current airfoil"""

        try: 
            curIndex = self.airfoilFiles.index (self.curAirfoil().pathFileName)
            
            if not aPathFileName in self.airfoilFiles: 
                self.airfoilFiles.insert (curIndex + 1, aPathFileName)
            self.loadNewAirfoil (aPathFileName)
        except: 
            ErrorMsg ("Could not add %s to airfoil list" % aPathFileName )

    def _getAirfoilFiles_sameDir (self, anAirfoilFile): 
        """ returns the list of airfoilFiles in the same directory as anAirfoilFile"""

        if os.path.isfile (anAirfoilFile):
            airfoil_dir = os.path.dirname(anAirfoilFile)
            airfoil_files = fnmatch.filter(os.listdir(airfoil_dir), '*.dat')
            airfoil_files = [os.path.normpath(os.path.join(airfoil_dir, f)) \
                                for f in airfoil_files if os.path.isfile(os.path.join(airfoil_dir, f))]
            return sorted (airfoil_files)
        else:
            return []


    def _set_title(self):
        """ sets window title of self """
        self.main.title (AppName + "  v" + str(AppVersion) + "  [" + self.curAirfoil().name + "]")


    def edit_settings (self):
        """ file menu edit settings """

        Dialog_Settings(self.main, name=self.name)


    #------- file functions ----------------

    def open (self):
        """ open a new wairfoil and load it"""

        filetypes  = [('Airfoil files', '*.dat')]
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
                airfoil = Airfoil(pathFileName=pathFilename, geometry=GEO_BASIC)
                airfoil.set_usedAs (NORMAL)
        airfoil.load()

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


    def onExit(self): 
        """ interception of user closing the app - check for changes made"""
        self.ctk_root = None
        self.main.destroy()



#--------------------------------

if __name__ == "__main__":

    # init colorama
    just_fix_windows_console()

    # set ctk application settings prior to init 

    Settings.belongTo (__file__, msg=True)

    ctk.set_appearance_mode    (Settings().get('appearance_mode', default='System'))   # Modes:  "System" (standard), "Dark", "Light"
    ctk.set_default_color_theme(Settings().get('color_theme', default='blue'))         # Themes: "blue" (standard), "green", "dark-blue"
    scaling = Settings().get('widget_scaling', default=1.0)
    if scaling != 1.0: 
        ctk.set_widget_scaling(scaling)  # widget dimensions and text size
        NoteMsg ("Font size is scaled to %.2f" %scaling)
    scaling = Settings().get('window_scaling', default=1.0)
    if scaling != 1.0: 
        ctk.set_window_scaling(scaling)  # scaling of window
        NoteMsg ("Window size is scaled to %.2f" %scaling)

    # command line arguments? 
    
    parser = argparse.ArgumentParser(prog=AppName, description='View and edit an airfoil')
    parser.add_argument("airfoil", nargs='*', help="Airfoil .dat file to show")
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

    myApp = AirfoilEditor (airfoil_file)
    
 