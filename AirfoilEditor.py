#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    A Airfoil Viewer 

    Object model overview (a little simplified) 

    App                                         - root frame 
        |-- Edit_File_Menu                      - file level functions
        |-- Edit_AirfoilBase                    - main wing data 
        |-- Edit_AirfoilCurvature               - parameters for a specific planform type 
        |-- Edit_WingSection                    - select and edit a single wing section 
                :
                |-- Widgets                     - wrapper for CTk widgets - get, set their data 
                |-- Field_Widget                - entry field with label and spin buttons
                ...                     - ...

        |-- Diagramms                           - the master to select one of the diagrams
                |-- Airfoil                     - standard airfoil view 
                        :
                        |-- Artists             - helper to plot a wing object on a matplotlib axes
                        |-- PlanformArtist      - plots the planform 
                        ...                     - ...

"""
import os
import sys
import argparse
from pathlib import Path
import fnmatch 

import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from tkinter import filedialog, Frame
import customtkinter as ctk

# let python find the other modules in modules relativ to path of self  
sys.path.append(os.path.join(Path(__file__).parent , 'modules'))

from common_utils       import * 
from airfoil            import *
from airfoil_examples   import Root_Example
from widgets            import *
from artist             import Plot_Toolbar
from ui_base            import Dialog_Abstract, Edit_Abstract, set_initialWindowSize
from airfoil_artists    import *

from spline_of_airfoil  import Bezier, UPPER, LOWER


#------------------------------------------------

AppName    = "Airfoil Editor"
AppVersion = "0.8.4"

#------------------------------------------------

cl_background               = ('#EBEBEB','#242424')                     # background color in diagrams

#   change events for updating mainly plots

AIRFOIL_NEW                 = "<<AIRFOIL_NEW>>"                #tk event types
AIRFOIL_CHANGED             = "<<AIRFOIL_CHANGED>>"


def fireEvent(ctk_root : ctk.CTkToplevel, eventType): 
    """ fire event for the current ctk_root toplevel widget """
    if not ctk_root is None: 
        ctk_root.event_generate (eventType) 
        print ("Event ", eventType)


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
    
    def changed_airfoil (self, dummy): 
        self.refresh()
#-------------------------------------------

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
                                get='maxThicknessX', width=50, unit="%", dec=2))
        r += 1
        self.add (Field_Widget  (self,r,c,   lab="Camber", obj=self.airfoil, 
                                get='maxCamber', width=50, lab_width=80, unit="%", dec=2))
        self.add (Field_Widget  (self,r,c+3, lab="at", lab_width=40, obj=self.airfoil, 
                                get='maxCamberX', width=50, unit="%", dec=2))
        r += 1
        self.add (Field_Widget  (self,r,c,   lab="TE gap", obj=self.airfoil, 
                                get='teGap', width=50, lab_width=80, unit="%", dec=2))
 

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

    def __init__(self, master, airfoilFn, *args, **kwargs):

        self._curvature_threshold = None

        super().__init__(master, airfoilFn, *args, **kwargs)

    @property
    def curvature_threshold (self): 
        return self._curvature_threshold
    def _set_curvature_threshold (self, aVal):
        self._curvature_threshold = aVal
        self.airfoil().curv_upper.set_threshold(aVal)
        self.airfoil().curv_lower.set_threshold(aVal)
    

    def init (self):

        self._curvature_threshold = self.airfoil().curv_upper.threshold

        r, c = 0, 0 
        self.add (Header_Widget (self,r,c,   width=90, lab=self.name, columnspan= 2))
        # self.add (Button_Widget (self,r,c+3, lab='Smooth', width=80, padx= (10,0), columnspan=2, sticky='w', 
        #                          set=self.smooth_airfoil ))

        r = 1
        Blank_Widget (self, r,0)                # left blank column to inset the fields 
        c = 1                                  
        self.add (Field_Widget  (self,r,c,   lab="Reversals upper", get=lambda: len(self.airfoil().curv_upper.reversals()),
                                width=60, lab_width=100, set='', dec=0, disable= True))
       
        self.add (Field_Widget  (self,r,c+3, lab="lower", get=lambda: len(self.airfoil().curv_lower.reversals()), 
                                width=60, lab_width=60, padx=(20,0), set='', dec=0, disable= True))
        r += 1
        self.add (Field_Widget  (self,r,c,   lab="... threshold", get=lambda: self.curvature_threshold, 
                                set=self._set_curvature_threshold, event=AIRFOIL_CHANGED,
                                width=60, lab_width=80, lim=(0,1), dec=2, spin=False, step=0.02))

        r += 1
        self.add (Header_Widget (self,r,0,   width=90, lab='Specials', columnspan= 2))
        r += 1
        self.add (Button_Widget (self,r,c, lab='Airfoil Bezier', width=110, padx= (0,0), columnspan=4, sticky='w', 
                                 set=self.airfoil_Bezier ))



    def refresh(self): 
        #overloaded to set threshold in airfoils for reversal calculation 
        self._set_curvature_threshold (self.curvature_threshold)
        super().refresh()

    def smooth_airfoil (self): 
        """ Open smooth dialog - if 'OK' the new smoothed airfoil will be inserted to the list 
        of airfoils and set to current """

        dialog = Dialog_Smooth (self, self._airfoilFn )
        self.wait_window (dialog)

        if dialog.return_OK: 
            self.myApp.add_toAirfoilFiles (dialog.return_newAirfoilPathFileName)

    def airfoil_Bezier (self): 
        """ Open dialog for Bezier design - if 'OK' the modified airfoil will be inserted to the list 
        of airfoils and set to current """

        dialog = Dialog_Bezier (self, self._airfoilFn )
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
        self.add (Field_Widget  (self,r,c,   lab="No of panels", get=lambda: self.airfoil().nPanels,
                                 width=50, lab_width=0, dec=0, text_style=lambda: self.style('no')))
        r += 1
        self.add (Field_Widget  (self,r,c,   lab="Angle at LE", get=lambda: self.airfoil().panelAngle_le, 
                                 width=50,   lab_width=80, unit="°", dec=0, text_style=lambda: self.style('le_angle')))
        self.add(Label_Widget   (self,r,c+3, columnspan=2, padx= 0,  
                                 lab= lambda: "at index %d" % self.airfoil().iLe))

        r += 1
        self.add (Field_Widget  (self,r,c,   lab="Angle min", get=lambda: self.airfoil().panelAngle_min[0], 
                                 width=50,   lab_width=80, unit="°", dec=0, text_style=lambda: self.style('min_angle')))
        self.add(Label_Widget   (self,r,c+3, columnspan=2, padx= 0,  
                                 lab= lambda: "at index %d" % self.airfoil().panelAngle_min[1]))
        r += 1
        Blank_Widget (self, r,c)
        r += 1
        self.add(Label_Widget   (self,r, c, columnspan=9, padx= 5, text_style = 'Warning',
                                 lab= lambda: self.messageText()))
        


    def messageText (self): 

        text = []
        airfoil = self.airfoil()
        minAngle, atIndex = self.airfoil().panelAngle_min

        if airfoil.panelAngle_le > 172.0: 
            text.append("- Panel angle at LE (%d°) is too blunt." %(self.airfoil().panelAngle_le))
        if minAngle < 150.0: 
            text.append("- Min. angle of two panels is < 150°")
        if airfoil.panelAngle_le == 180.0: 
            text.append("- Leading edge has 2 points")
        if airfoil.nPanels < 160 or airfoil.nPanels > 260: 
            text.append("- No of panels should be > 160 and < 260")
        
        text = '\n'.join(text)
        return text 
    
    def style (self, panel): 

        airfoil = self.airfoil()
        minAngle, atIndex = airfoil.panelAngle_min

        if   panel =="min_angle":
            if minAngle < 150.0 : return 'Warning'
        elif panel =="le_angle":
            if airfoil.panelAngle_le > 170.0 : return 'Warning'
        elif panel =="no":
            if airfoil.nPanels < 160 or airfoil.nPanels > 260 : return 'Warning'

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
        self.add (Field_Widget  (self,r,c,   lab="Leading edge", get=lambda: self.airfoil().le[0],
                                 width=75, lab_width=90, dec=6, text_style=lambda: self.style('le_x')))
        self.add (Field_Widget  (self,r,c+3,                     get=lambda: self.airfoil().le[1],
                                 width=75,               dec=6, text_style=lambda: self.style('le_y')))
        r += 1
        self.add (Field_Widget  (self,r,c,   lab="Trailing edge", get=lambda: self.airfoil().te_fromPoints[0],
                                 width=75, lab_width=90, dec=6, text_style=lambda: self.style('te_x')))
        self.add (Field_Widget  (self,r,c+3,                     get=lambda: self.airfoil().te_fromPoints[1],
                                 width=75,               dec=6, text_style=lambda: self.style('te_y')))
 
        r += 1
        self.add (Field_Widget  (self,r,c,   lab=" " ,            get=lambda: self.airfoil().te_fromPoints[2],
                                 width=75, lab_width=90, dec=6, text_style=lambda: self.style('te_x')))
        self.add (Field_Widget  (self,r,c+3,                      get=lambda: self.airfoil().te_fromPoints[3],
                                 width=75,               dec=6, text_style=lambda: self.style('te_y')))

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
        airfoil = self.airfoil()

        if airfoil.le[0] != 0.0 or airfoil.le[1] != 0.0:
            text.append("- Leading edge is not at 0,0")
        if airfoil.te_fromPoints[0] != 1.0 or airfoil.te_fromPoints[2] != 1.0 : 
           text.append("- Trailing edge is not at 1")
        
        text = '\n'.join(text)
        return text 
    

    def style (self, coord): 
        """ text style of entry fields"""
        airfoil = self.airfoil()

        if   coord =="le_x":
            if airfoil.le[0] != 0.0 : return 'Warning'
        elif coord =="le_y":
            if airfoil.le[1] != 0.0 : return 'Warning'
        elif coord =="te_x":
            if airfoil.te_fromPoints[0] != 1.0 or \
               airfoil.te_fromPoints[2] != 1.0 : return 'Warning'
        elif coord =="te_y":
            if airfoil.te_fromPoints[3] != - airfoil.te_fromPoints[1] : return 'Warning'


    def style_normalize (self): 
        """ text style of the normalize button"""
        if len(self.messageText()) > 0: 
            return 'Warning'



#-------------------------------------------------------------------------------
# Diagrams   
#-------------------------------------------------------------------------------


class Diagram_Abstract(ctk.CTkFrame):
    """ 
    Abstract super class of the specific plot frames like "Diagram_Airfoil"
    """
    name        = "This is the plot super class"
    defaultTip  = "Use switches ...."

    cl_text         = "#DCE4EE"
    cl_labelGrid    = '#B0B0B0'


    def __init__(self, master, airfoilFn, *args, view_frame = None, setActive=True, size= None,  **kwargs):
        super().__init__( master, *args, **kwargs)

        self.view_frame : Frame = view_frame
        self._airfoilFn = airfoilFn

        # root for change events (widgets will have the same toplevel as root)
        self.ctk_root = self.winfo_toplevel()

        # big diagram grid 
        self.grid_columnconfigure(0, weight=1)
        self.grid_rowconfigure(0, weight=1)

        if size: 
            self.figure : plt.Figure = plt.Figure(figsize=size)
        else: 
            self.figure : plt.Figure = plt.Figure()

        # connect tk and pyplot
        self.canvas = FigureCanvasTkAgg(self.figure, self)
        # take background of dark mode for canvas
        self.canvas._tkcanvas.configure(background= cl_background[1])
        self.canvas._tkcanvas.grid (row=0, column=0, pady=0, padx=0, sticky="news")


        # common axes for this diagram
        self.create_axes()
        self.setup_axes ()

        # Create the artists for the diagramm
        self.setup_artists ()

        # init of switches / plots if a frame for the switches is available 
        r,c = 0,1
        if self.view_frame: 
            self.view_frame.grid_columnconfigure(0, weight=1)   # to center switches
            self.view_frame.grid_columnconfigure(2, weight=1)

            r,c = self.setup_Switches (r, c)                       

            r +=1
            Blank_Widget (self.view_frame,r,c)
            self.view_frame.grid_rowconfigure(r, weight=1)
            r +=1
            Label_Widget (self.view_frame, r, 0, lab='Pan and zoom')
            r +=1
            self.toolbar = Plot_Toolbar(self.canvas, self.view_frame, background=ctk.get_appearance_mode())
            self.toolbar.grid (row=r, column=0, columnspan= 3, sticky='ew', padx=(10,10), pady=(0,10))

        # react on changes of model
        self.setChangeBindings ()

        # and finally show it 
        self.refresh()


    @property
    def airfoil(self) -> Airfoil:
        return self._airfoilFn()
    
    # ----- abstract - to overlaod

    def create_axes (self):
        """ setup axes, axis for this plot type """
        self.ax1 : plt.Axes = self.figure.add_subplot()        # the pyplot axes this diagram is plotted
        self.figure.subplots_adjust(left=0.04, bottom=0.07, right=0.98, top=0.99, wspace=None, hspace=None)


    def setup_axes(self):
        """ to overload - setup axes, axis for this plot type """
        pass


    def setup_artists(self):
        """ setup artists for this plot type """

        self.gridArtist = Grid_Artist (self.ax1, None, show=True)
        self.gridArtist.plot()          # force to show first time


    def setup_Switches(self, r=0, c=0):
        """ define on/off switches ffor this plot type"""

        r = 0 
        Header_Widget (self.view_frame,r,0, columnspan=2, lab='View')

        r += 1 
        Switch_Widget (self.view_frame,r,c, padx=10, lab='Grid', 
                       get=lambda: self.gridArtist.show, set=self.gridArtist.set_show)

        return r, c 

    def refresh(self, dummy): 
        # overwrite in sub class
        pass  

    def setChangeBindings (self):
        # overwrite in sub class
        pass      




class Diagram_Airfoil (Diagram_Abstract):
    """ 
    plots the airfoil and curvature  of all wing sections
    """
    name = "Airfoils"

    def create_axes (self):
        """ setup 2 axes for airfoil and its curvature  """

        self.ax1 : plt.Axes = self.figure.add_subplot(2, 1, 1)
        self.ax2 : plt.Axes = self.figure.add_subplot(2, 1, 2)
        self.figure.subplots_adjust(left=0.04, bottom=0.07, right=0.96, top=0.97, wspace=None, hspace=0.15)

    def setup_axes(self, show_logScale=True):
        """ setup axes, axis, artiss for this plot type """

        # airfoil contour 
        self.ax1.tick_params (labelbottom=True, labelleft=True, labelsize='small')
        self.ax1.set_xlim([-0.05,1.05])
        self.ax1.axis('equal')

        # curvature
        self.ax2.tick_params (labelsize='small')
        self.ax2.set_xlim([-0.05,1.05])

        if show_logScale:
            # log y scale to get full range of curvature 
            # suppress warning of a motplotlib bug reported: 
            # https://github.com/matplotlib/matplotlib/issues/26118
            import warnings
            warnings.filterwarnings("ignore", message = "All values for SymLogScale")
            # ! linear range from 0..1 will lead to a kink in the curve 
            thresh  = 1 
            self.ax2.set_yscale('symlog', linthresh=thresh, linscale=1) # , linscale=2
            self.ax2.set_ylim([ -10, 1000])
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

        # airfoil is list to prepare for future multiple airfoils to view 
        
        self.airfoilArtist   = Airfoil_Artist   (self.ax1,     [self._airfoilFn], show=True)
        self.camberArtist    = Thickness_Artist (self.ax1,     [self._airfoilFn], show=False)
        self.curvatureArtist = Curvature_Artist (self.ax2,     [self._airfoilFn], show=True)


    def setup_Switches(self, r=0, c=0):
        """ define on/off switches for this plot type"""

        r, c = super().setup_Switches(r, c)

        self.curvatureArtist.set_upper (True)
        self.curvatureArtist.set_lower (True)

        r += 1
        Switch_Widget (self.view_frame,r,c, padx=10, lab='Points',
                       get=lambda: self.airfoilArtist.points, set=self._set_points)

        r += 1
        Switch_Widget (self.view_frame,r,c, padx=10, lab='Camber', 
                       get=lambda: self.camberArtist.show, set=self.camberArtist.set_show)

        r += 1
        Blank_Widget (self.view_frame,r,c)
        self.view_frame.grid_rowconfigure(r, weight=1)

        r += 1
        Label_Widget  (self.view_frame, r, 0, lab='Curvature')
        r += 1
        Switch_Widget (self.view_frame,r,c, padx=10, lab='Upper side', 
                       get=lambda: self.curvatureArtist.upper, set=self._set_upper)
        r += 1
        Switch_Widget (self.view_frame,r,c, padx=10, lab='Lower side',
                       get=lambda: self.curvatureArtist.lower, set=self._set_lower)
        r += 1
        Switch_Widget (self.view_frame,r,c, padx=10, lab='log scale y',
                       val=True, set=self._set_logScale)

        return r, c


    # -------- switch call back 

    def _set_points (self, aBool):
        self.airfoilArtist.set_points(aBool)
        self.curvatureArtist.set_points(aBool)
        self.refresh()

    def _set_upper (self, aBool):
        self.curvatureArtist.set_upper(aBool)
        self.refresh()

    def _set_lower (self, aBool):
        self.curvatureArtist.set_lower(aBool) 
        self.refresh()

    def _set_logScale (self, aBool):
        self.setup_axes(show_logScale=aBool)
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
        self.camberArtist.refresh ()  
        self.curvatureArtist.refresh ()  

        self.figure.canvas.draw_idle()    # draw ony if Windows is idle!



class Diagram_LeTe_Mini (Diagram_Abstract):
    """ 
    plots the leading edge and the trailing edge area in mini format
    """
    name = "LE and TE"

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
        # self.ax1.axis('equal')
        self.ax1.set_xlim([-0.01,0.03])
        self.ax1.set_ylim([-0.02,0.02])
        self.ax1.autoscale(enable=False, axis='both')
        self.ax1.grid ()

        self.ax2.tick_params (labelbottom=True, labelleft=False, labelsize='small')
        # self.ax2.set_xlim([-0.01,1.01])
        self.ax2.axis('equal')
        # self.ax2.set_ylim([-0.05,0.05])
        self.ax2.autoscale(enable=True, axis='both')
        self.ax2.grid ()

        self.ax3.tick_params (labelbottom=True, labelleft=False, labelsize='small')
        self.ax3.set_xlim([0.8,1.1])
        # self.ax3.axis('equal')
        self.ax3.set_ylim([-0.04,0.04])
        self.ax3.autoscale(enable=False, axis='x')
        self.ax3.grid ()


    def setup_artists(self):
        """ artists are not set automatically - set from outside """
        pass


    def setup_Switches(self, r=0, c=0):
        """ no switches"""
        return r, c

    # -------- event handler

    # refresh handled in App 

    # -------- refresh my Artists which are on 'show mode' 

    def refresh(self): 
        # overloaded

        self.ax1.figure.canvas.draw_idle()    # draw ony if Windows is idle!



class Diagram_Airfoil_Mini (Diagram_Abstract):
    """ 
    plots the airfoil with two free axes in mini format 
    """
    name = "Thickness and Camber"

    def create_axes (self):
        """ setup 2 axes for airfoil, thickness, curvature etc."""

        from matplotlib.gridspec import GridSpec

        gs = GridSpec(2, 1)
        self.ax1 = self.figure.add_subplot(gs[:-1, :])          # top, full   - airfoil
        self.ax2 = self.figure.add_subplot(gs[ -1, :])          # lower, full - thickness

        self.figure.subplots_adjust(left=0.02, bottom=0.08, right=0.98, top=0.97, wspace=0.07, hspace=0.15)

    def setup_axes(self):
        """ setup axes, axis, artiss for this plot type """

        # airfoil contour 
        self.ax1.tick_params (labelbottom=True, labelleft=False, labelsize='small')
        self.ax1.axis('equal')
        self.ax1.autoscale(enable=True, axis='both')
        self.ax1.grid ()

        # additional plot 
        self.ax2.tick_params (labelbottom=True, labelleft=False, labelsize='small')
        self.ax2.axis('equal')
        self.ax2.autoscale(enable=True, axis='both')
        self.ax2.grid ()


    def setup_artists(self):
        """ artists are not set automatically - set from outside """
        pass

    def setup_Switches(self, r=0, c=0):
        """ no switches"""
        return r, c

    # -------- event handler

    # refresh handled in App 

    # -------- refresh my Artists which are on 'show mode' 

    def refresh(self): 
        # overloaded
        self.ax1.figure.canvas.draw_idle()    # draw ony if Windows is idle!



class Diagram_Curvature_Mini (Diagram_Abstract):
    """ 
    plots the airfoil curvature to smooth
    """
    name = "Curvature"

    def create_axes (self):
        """ setup 1 axes for airfoilcurvature """

        from matplotlib.gridspec import GridSpec

        gs = GridSpec(1, 1)
        self.ax1 = self.figure.add_subplot(gs[:, :])            # full   - curv

        self.figure.subplots_adjust(left=0.04, bottom=0.08, right=0.98, top=0.97, wspace=0.07, hspace=0.15)

    def setup_axes(self):
        """ setup axes, axis, artiss for this plot type """

        self.ax1.tick_params (labelsize='small')
        self.ax1.set_xlim([-0.05,1.05])
        self.ax1.grid ()
        # suppress warning of a motplotlib bug reported: 
        # https://github.com/matplotlib/matplotlib/issues/26118
        import warnings
        warnings.filterwarnings("ignore", message = "All values for SymLogScale")
        self.ax1.set_yscale('symlog', linthresh=1)
        self.ax1.set_ylim([ -100, 1000])

    def setup_artists(self):
        """ artists are not set automatically - set from outside """
        pass

    def setup_Switches(self, r=0, c=0):
        """ no switches"""
        return r, c

    # -------- event handler

    # refresh handled in App 

    # -------- refresh my Artists which are on 'show mode' 

    def refresh(self): 
        # overloaded

        self.ax1.figure.canvas.draw_idle()    # draw ony if Windows is idle!



class Edit_File_Menu(Edit_Abstract_Airfoil):
    """ 
    Frame for the high level commands like load, save, ...
    The parent is the App itself
    """
    name = "File"

    def init (self):

        self.grid_columnconfigure   (0, weight=1)

        r,c = 0,0 
        self.add (Header_Widget (self,r,c, lab=self.name, width=80))

        # if AirfoilEditor called stand alone offer combobox with input files
        if not self.myApp.isModal:
            r +=1 
            self.add(Option_Widget (self,r,c, get=self.curAirfoilFileName, set=self.set_curAirfoilFileName,
                                            options=self.airfoilFileNames,
                                            spin=True, spinPos='below', width=150, padx=10, pady=4, sticky = 'ew',))
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

                if os.path.isfile (aPathFileName):                  # maybe it was deleted in meantime 
                    self.myApp.loadNewAirfoil (aPathFileName)
                self.refresh()
                break


#-------------------------------------------------------------------------------
# Dialogs for smaller tasks   
#-------------------------------------------------------------------------------


class Dialog_Airfoil_Abstract (Dialog_Abstract):
    """ 
    Superclass for modal dialog to modify airfoil properties  
    having 

        header_frame    - which is empty 
        diagramm_frame  - ! which is not initialized !
        input_frame     - which is empty 
        button_frame    - with default buttons SaveAs, Ok, Cancel 

    """
    def __init__(self, master, airfoilFn, workingDir=None, nameExt='-mod', *args, **kwargs):
        super().__init__(master, *args, **kwargs)

        self.airfoilOrg : Airfoil = airfoilFn()             # keep the original airfoil 

        self.airfoil  = Airfoil.asCopy (self.airfoilOrg, nameExt=nameExt) 
        self.hasbeen_normalized = False

        if not self.airfoil.isNormalized_highPrec:          # also LE of spline at 0,0? 
            self.hasbeen_normalized = self.airfoil.normalize (highPrec = True)        # ensure exact le based on spline


        # input field will fire this event when data is changed
        self.change_event = AIRFOIL_CHANGED

        self.return_newAirfoilPathFileName = None   # return value for parent

        # main grid 4 x 1  (header , diagram, inputs, default-buttons ) 
        self.header_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.header_frame.grid(row=0, column=0, sticky="nwe", padx=0, pady=(10,10))

        # dummy for diagram
        self.diagram_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent") 
        self.diagram_frame.grid(row=1, column=0, sticky="nwe")

        self.input_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.input_frame.grid(row=2, column=0, sticky="nwe", padx=40, pady=10)

        self.button_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.button_frame.grid(row=3, column=0, sticky="wes", padx=40, pady=(10,20))

        self.edit_frame.grid_columnconfigure (0, weight=1)
        self.edit_frame.grid_rowconfigure    (3, weight=1)


        # new airfoil name and default buttons commonly defined  
        r,c = 0,0  
        self.nameWidget = Field_Widget  (self.button_frame,r,c, lab="New Airfoil name", 
                                 obj=self.airfoil, get='name', set='set_name',
                                 lab_width=130, width=220, justify='left')
        c += 2
        Button_Widget (self.button_frame,r,c, lab='Save As...', set=self.save, width=90)
        c += 1 
        Button_Widget (self.button_frame,r,c, lab='Ok', set=self.ok, style=PRIMARY, width=90)
        c += 1 
        Button_Widget (self.button_frame,r,c, lab='Cancel', set=self.cancel, width=90)

        c += 1
        Label_Widget (self.button_frame,r,c, text_style='Warning',
                                lab= lambda: self._save_warning(), width= 110)

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

            self.airfoil.apply_repanel ()

            try: 
                self.airfoil.saveAs (airfoilPath, airfoilName)
            except: 
                message = "Airfoil name not valid.\n\nAirfoil could not be saved"
                msg = Messagebox (self, title="Save", message=message, icon="cancel", option_1="Ok")
                msg.get()                # wait until pressed ok

            self.refresh('')


    def cancel(self): 
        # changed bindings
        self.ctk_root.unbind(self.change_event)
        super().cancel()


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
                message = "Airfoil '%s'\n\nsaved to\n\n%s" % (self.airfoil.name, airfoilDirMSG )
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


#-------------------------------------------

class Dialog_Repanel (Dialog_Airfoil_Abstract):
    """ 
    Dialog to repanel airfoil  
    """

    width  = 1380
    height = 630

    def __init__(self, master, airfoilFn, *args, **kwargs):
        super().__init__(master, airfoilFn, *args, nameExt='-repan', height=self.height/2, **kwargs)

        # start with a repaneld airfoil  using default values 
        self.airfoil.repanel()               

        self.title ("Repanel airfoil  [" + self.airfoil.name + "]")
        self.showRepaneled = True 

        # set specific diagram frame for this dialog 
        self.diagram_frame = Diagram_LeTe_Mini (self.edit_frame, self.airfoilListFn, size=(7.0,4.0))
        self.diagram_frame.grid(row=1, column=0, sticky="nwe")

        # artists for preview in diagram_frame
        self.airfoilArtist  = Airfoil_Artist (self.diagram_frame.ax2, self.airfoilListFn, show=True)
        self.airfoilArtist.set_points(True)
        self.airfoilArtist.refresh(figureUpdate=True)

        self.leArtist  = Le_Artist (self.diagram_frame.ax1, self.airfoilListFn, show=True)
        self.leArtist.refresh(figureUpdate=True)

        self.teArtist  = Le_Artist (self.diagram_frame.ax3, self.airfoilListFn, show=True)
        self.teArtist.refresh(figureUpdate=True)

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

    # ---- 

    def airfoilListFn (self):
        if self.showRepaneled: 
            return [self.airfoil]
        else: 
            return [self.airfoilOrg]


    def set_showRepaneled (self, aBool):
        self.showRepaneled = aBool
        self.refresh ('') 


    def le_bunch_message (self): 
        angle = self.airfoil.panelAngle_le
        if angle > 172.0: 
            text = "Angle at LE is too blunt. Decrease bunch" 
        elif angle < 155.0: 
            text = "Angle at LE is too sharp. Increase bunch"
        else:
            text = "leads to good panel angle at LE"
        return text 
            

    def le_bunch_message_style (self): 
        angle = self.airfoil.panelAngle_le
        if angle > 170.0 or angle < 155.0: 
            return 'Warning'
        else: 
            return 'Disabled'


    def refresh(self, dummy):
        self.leArtist.refresh(figureUpdate=True)
        self.teArtist.refresh(figureUpdate=True)
        self.airfoilArtist.refresh(figureUpdate=True)
        super().refresh()



#-------------------------------------------

class Dialog_Smooth (Dialog_Airfoil_Abstract):
    """ 
    Dialog to repanel airfoil  
    """

    width  = 1380
    height = 630

    def __init__(self, master, airfoilFn, *args, **kwargs):
        super().__init__(master, airfoilFn, *args, nameExt='-smoothed', height=self.height/2, **kwargs)

        # start with a repaneld airfoil  using default values 
        self.airfoil_before  = Airfoil.asCopy (self.airfoilOrg, nameExt='-before') 
        self.airfoil.set_isEdited(True)               

        self.title ("Smooth airfoil  [" + self.airfoil.name + "]")

        # set specific diagram frame for this dialog 
        self.diagram_frame = Diagram_Curvature_Mini (self.edit_frame, self.airfoilListFn, size=(7.0,4.0))
        self.diagram_frame.grid(row=1, column=0, sticky="nwe")

        # artists for preview in diagram_frame
        self.smoothArtist  = Curvature_Smooth_Artist (self.diagram_frame.ax1, self.airfoilListFn, 
                                                      show=True, onPick=self._pointPicked)
        self.smoothArtist.set_upper (True)
        self.smoothArtist.set_lower (True)
        self.smoothArtist.refresh(figureUpdate=True)


        # Header 
        c = 0 
        r = 0 
        Header_Widget (self.header_frame,r,c, pady=0, lab= "Smooth Airfoil", sticky = 'nw', width=100)
        
        Switch_Widget (self.header_frame,r,c+1, padx=(30,30), lab='Upper Side',
                       get=lambda: self.smoothArtist.upper, set=self._set_upper)
        c += 2
        Switch_Widget (self.header_frame,r,c+1, padx=(0,30), lab='Lower Side',
                       get=lambda: self.smoothArtist.lower, set=self._set_lower)

        Label_Widget  (self.header_frame,r, c+2, padx= 20, sticky = 'nw', columnspan=1,
                        lab= "! Dev playground for smoothing !")

        self.header_frame.grid_columnconfigure (7, weight=1)

        # input fields
        r,c = 0,0
        Blank_Widget (self.input_frame,r,c,  height= 10)
        r +=1 
        self.add (Field_Widget (self.input_frame,r,c, lab="Points to remove", lab_width=100, 
                                width=150, 
                                obj=self, get=lambda: str(self._selectedPoints), set='',
                                event=self.change_event ))

        Button_Widget (self.input_frame,r,c+3, lab='Remove selected', set=self._remove_selected, width=120)
        Button_Widget (self.input_frame,r,c+4, lab='Rebuild airfoil', set=self._rebuild_airfoil, width=120)

        r +=1 
        Blank_Widget (self.input_frame,r,c, width=20, height = 15) 
        r +=1 
        Button_Widget (self.input_frame,r,c, lab='Low res repanel', columnspan = 1,
                       set=self._repanel_low_res, width=120)
        Button_Widget (self.input_frame,r,c+1, lab='High res repanel', columnspan = 1,
                       set=self._repanel_high_res, width=120)


        self.input_frame.grid_columnconfigure (8, weight=2)

        # buttons are defined in super class 

    # ---- 

    def airfoilListFn (self):
        return [self.airfoil]
    
    def _pointPicked (self, aPointLabel):

        self.refresh()

    def _set_upper (self, aBool):
        self.smoothArtist.set_upper(aBool)
        self.smoothArtist.refresh(figureUpdate=True)

    def _set_lower (self, aBool):
        self.smoothArtist.set_lower(aBool) 
        self.smoothArtist.refresh(figureUpdate=True)

    @property
    def _selectedPoints (self):

        sel_upper = self.smoothArtist.get_selected (side='upper')
        sel_lower = self.smoothArtist.get_selected(side='lower')

        iLe = self.airfoil.iLe

        selected = []
        for i, iSel in enumerate (sel_upper):
            selected.append(abs(iSel-iLe)) 
        for i, iSel in enumerate (sel_lower):
            selected.append(abs(iSel+iLe)) 

        return selected  

    def _remove_selected(self): 
        """ remove the selected coordinate points from airfoil"""
        selected = self._selectedPoints
        if selected: 
            x_new = np.delete (self.airfoil.x, selected)
            y_new = np.delete (self.airfoil.y, selected)

            self.airfoil.set_xy(x_new, y_new)
            self.smoothArtist.refresh(figureUpdate=True)
            self.refresh()

    def _rebuild_airfoil(self): 
        """ remove the selected coordinate points from airfoil"""

        iLe = self.airfoil_before.iLe
        x_new = self.airfoil.x
        y_new = self.airfoil.y

        for i, xi in enumerate (self.airfoil_before.x):

            if xi != x_new[i]:                 # is there a x-Coordinate not in airfoil with removed coords
                if i <= iLe:
                    yi_new = self.airfoil.spline.get_y_on ("upper", [xi])
                else:
                    yi_new = self.airfoil.spline.get_y_on ("lower", [xi])
                x_new = np.insert (x_new, i, xi)
                y_new = np.insert (y_new, i, yi_new)

        self.airfoil.set_xy(x_new, y_new)
        self.smoothArtist.refresh(figureUpdate=True)
        self.refresh()

    def _repanel_low_res(self): 
        """ repanel after removing the selected coordinate points from airfoil"""

        x_new, y_new = self.airfoil.spline.get_repaneled (70, 0.995, 0.2)
        self.airfoil.set_xy(x_new, y_new)
        self.airfoil_before  = Airfoil.asCopy (self.airfoil, nameExt='-before') 

        self.smoothArtist.refresh(figureUpdate=True)
        self.refresh()

    def _repanel_high_res(self): 
        """ repanel after removing the selected coordinate points from airfoil"""

        self.airfoil.repanel()
        self.airfoil_before  = Airfoil.asCopy (self.airfoil, nameExt='-before') 

        self.smoothArtist.refresh(figureUpdate=True)
        self.refresh()


#-------------------------------------------


class Dialog_Normalize (Dialog_Airfoil_Abstract):
    """ 
    Dialog to normalize airfoil
    """

    width  = 900
    height = 370

    def __init__(self, master, airfoilFn, *args, **kwargs):
        super().__init__(master, airfoilFn, *args, nameExt='-norm', height=self.height/2, **kwargs)

        # ! see Dialog_Airfoil_Abstract for init of airfoil !

        self.title ("Normalize airfoil  [" + self.airfoil.name + "]")

        self.diagram_frame.grid_remove()                # not neeeded here

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
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Leading edge", get=lambda: self.airfoil.le[0],
                                 width=80, lab_width=130, dec=7))
        self.add (Field_Widget  (self.input_frame,r,c+3, get=lambda: self.airfoil.le[1],
                                 width=80, dec=7))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" ... of spline", get=lambda: self.airfoil.spline.leSpline[0],
                                 width=80, lab_width=130, dec=7))
        self.add (Field_Widget  (self.input_frame,r,c+3, get=lambda: self.airfoil.spline.leSpline[1],
                                 width=80, dec=7))
        r += 1
        Blank_Widget (self.input_frame, r,c, height=15)
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Trailing edge", get=lambda: self.airfoil.te_fromPoints[0],
                                 width=80, dec=7))
        self.add (Field_Widget  (self.input_frame,r,c+3, get=lambda: self.airfoil.te_fromPoints[1],
                                 width=80, dec=7))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" " , get=lambda: self.airfoil.te_fromPoints[2],
                                 width=80, dec=7))
        self.add (Field_Widget  (self.input_frame,r,c+3, get=lambda: self.airfoil.te_fromPoints[3],
                                 width=80, dec=7))

        # fields original airfoil
        r = 1 
        c = 5 
        self.add (Label_Widget (self.input_frame,r,c+1, padx=0, lab="Original x", columnspan = 2))
        self.add (Label_Widget (self.input_frame,r,c+3, padx=0, lab="y"))

        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" ", get=lambda: self.airfoilOrg.le[0],
                                 width=80,   lab_width=60, dec=7, text_style=lambda: self.style('le_x')))
        self.add (Field_Widget  (self.input_frame,r,c+3, get=lambda: self.airfoilOrg.le[1],
                                 width=80, dec=7,                 text_style=lambda: self.style('le_y')))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" ", get=lambda: self.airfoilOrg.spline.leSpline[0],
                                 width=80,   lab_width=60, dec=7, text_style=lambda: self.style('leSpline_x')))
        self.add (Field_Widget  (self.input_frame,r,c+3, get=lambda: self.airfoilOrg.spline.leSpline[1],
                                 width=80, dec=7,                 text_style=lambda: self.style('leSpline_y')))

        r += 2
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" ", get=lambda: self.airfoilOrg.te_fromPoints[0],
                                 width=80,   lab_width=60, dec=7, text_style=lambda: self.style('te_x')))
        self.add (Field_Widget  (self.input_frame,r,c+3,          get=lambda: self.airfoilOrg.te_fromPoints[1],
                                 width=80,                 dec=7, text_style=lambda: self.style('te_y')))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" ", get=lambda: self.airfoilOrg.te_fromPoints[2],
                                 width=80,   lab_width=60, dec=7, text_style=lambda: self.style('te_x')))
        self.add (Field_Widget  (self.input_frame,r,c+3,          get=lambda: self.airfoilOrg.te_fromPoints[3],
                                 width=80,                 dec=7, text_style=lambda: self.style('te_y')))

        self.input_frame.grid_columnconfigure (9, weight=2)
        self.input_frame.grid_rowconfigure (0, weight=1)

        
    def style (self, coord): 
        if   coord =="le_x":
            if self.airfoilOrg.le[0] != 0.0 : return 'Warning'
        elif coord =="le_y":
            if self.airfoilOrg.le[1] != 0.0 : return 'Warning'
        elif coord =="leSpline_x":
            if not self.airfoilOrg.spline.isLe_closeTo_leSpline : return 'Warning'
        elif coord =="leSpline_y":
            if not self.airfoilOrg.spline.isLe_closeTo_leSpline : return 'Warning'
        elif coord =="te_x":
            if self.airfoilOrg.te_fromPoints[0] != 1.0 or \
               self.airfoilOrg.te_fromPoints[2] != 1.0 : return 'Warning'
        elif coord =="te_y":
            if self.airfoilOrg.te_fromPoints[3] != - self.airfoilOrg.te_fromPoints[1] : return 'Warning'


#-------------------------------------------


class Dialog_Geometry (Dialog_Airfoil_Abstract):
    """ 
    Dialog to change thickness, camber or TE gap of airfoil  
    """

    width  = 1170
    height = 770

    def __init__(self, master, airfoilFn, *args, **kwargs):
        super().__init__(master, airfoilFn, *args, nameExt='-mod', height=self.height/2, **kwargs)

        self.airfoil.set_isEdited (True)            # will indicate airfoil when plotted 

        # ! see Dialog_Airfoil_Abstract for init of airfoil !

        self.title ("Modify airfoil  [" + self.airfoil.name + "]")

        self.showModified = True 
        self._chord = 200.0                         # sample chord length for showing airfoil parms

        # set specific diagram frame for this dialog 
        self.diagram_frame = Diagram_Airfoil_Mini (self.edit_frame, self.airfoilListFn, size=(7.0,5))
        self.diagram_frame.grid(row=1, column=0, sticky="nwe")

        # artists for preview in diagram_frame
        self.airfoilArtist = Airfoil_Artist (self.diagram_frame.ax1, self.airfoilListFn, show=True)
        self.airfoilArtist.refresh(figureUpdate=True)

        self.thickArtist   = Thickness_Artist (self.diagram_frame.ax2, self.airfoilListFn, show=True)
        self.thickArtist.refresh(figureUpdate=True)


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
            Label_Widget (self.input_frame,r,c, padx=5, lab= "Working copy normalized for higher precision", columnspan = 3,
                          text_style="Warning")
        else:
            Label_Widget (self.input_frame,r,c, padx=5, lab= "Working copy", columnspan = 2)

        r += 1
        Blank_Widget (self.input_frame, r,10, height=5)
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Thickness", obj=self.airfoil, 
                                get='maxThickness', set='set_maxThickness', step=0.01, 
                                spin=True, width=95, lab_width=70, unit="%", dec=2,
                                event=self.change_event))
        self.add (Field_Widget  (self.input_frame,r,c+3, lab="at", lab_width=20, obj=self.airfoil, 
                                get='maxThicknessX', set='set_maxThicknessX', step=0.1, 
                                spin=True, width=95, unit="%", dec=2,
                                event=self.change_event))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Camber", obj=self.airfoil, 
                                get='maxCamber', set='set_maxCamber', disable= 'isSymmetric',  
                                spin=True, width=95, lab_width=70, unit="%", dec=2, step=0.01,
                                event=self.change_event))
        self.add (Field_Widget  (self.input_frame,r,c+3, lab="at", lab_width=20, obj=self.airfoil, 
                                get='maxCamberX', set='set_maxCamberX', disable= 'isSymmetric',  
                                spin=True, width=95, unit="%", dec=2, step=0.1,
                                event=self.change_event))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="TE gap", obj=self.airfoil, 
                                get='teGap', set='set_teGap', step=0.01,
                                spin=True, width=95, lab_width=70, unit="%", dec=2,
                                event=self.change_event))

        # fields for chord length xy mm 

        r = 0 
        c = 8 
        Label_Widget  (self.input_frame,r,c , padx=5, lab= "... with a chord length of", columnspan = 3)
        self.add (Field_Widget  (self.input_frame,r,c+4, obj=self, get='chord', set='set_chord', 
                                width=50, unit="mm", dec=0))
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
                                get=lambda: self.airfoil.teGap * self.chord / 100,  
                                width=50, lab_width=70, unit="mm", dec=2, disable= True))

        self.input_frame.grid_columnconfigure (7,  weight=1)
        self.input_frame.grid_columnconfigure (14, weight=2)

        # ---- buttons in super class

    @property                               
    def chord (self):                            # sample chord length 
        return self._chord
    def set_chord (self, aVal): 
        if aVal > 0: self._chord= aVal
        super().refresh()


    def airfoilListFn (self):
        if self.showModified: 
            return [self.airfoilOrg, self.airfoil]
        else: 
            return [self.airfoilOrg]


    def set_showModified (self, aBool):
        self.showModified = aBool
        self.refresh ('') 
                    

    def refresh(self, dummy):
        self.thickArtist.refresh(figureUpdate=True)
        self.airfoilArtist.refresh(figureUpdate=True)
        super().refresh()



class Dialog_Bezier (Dialog_Airfoil_Abstract):
    """ 
    Dialog to change thickness, camber or TE gap of airfoil  
    """

    width  = 1300
    height = 850

    def __init__(self, master, airfoilFn, *args, **kwargs):
        super().__init__(master, airfoilFn, *args, nameExt='-bezier', height=self.height/2, **kwargs)

        # ! see Dialog_Airfoil_Abstract for init of airfoil !
        self.airfoil  = Airfoil_Bezier (name="Test-bez") 
        self.airfoil.set_isEdited (True)            # will indicate airfoil when plotted 

        self.title ("Design Bezier airfoil  [" + self.airfoil.name + "]")
        self.showOrg = True 

        # set specific diagram frame for this dialog 
        self.diagram_frame = Diagram_Airfoil_Mini (self.edit_frame, None, size=(7.5,6))
        self.diagram_frame.figure.subplots_adjust(left=0.05, bottom=0.07, right=0.96, top=0.97, wspace=None, hspace=0.15)

        ax1, ax2 = self.diagram_frame.ax1, self.diagram_frame.ax2
        ax1.tick_params (labelbottom=True, labelleft=True, labelsize='small')
        ax2.tick_params (labelbottom=True, labelleft=True, labelsize='small')
        ax2.axis('auto')
        ax2.set_xlim([-0.05,1.05])
        self._set_logScaleY (ax2)


        self.diagram_frame.grid(row=1, column=0, sticky="nwe")

        # artists for preview in diagram_frame
        self.airfoilArtist = Airfoil_Artist (self.diagram_frame.ax1, [self.airfoilOrg], 
                                             show=True)
        self.bezierArtist  = Bezier_Artist  (self.diagram_frame.ax1, self.airfoil, 
                                             show=True, onMove=self.changed_bezier)

        self.curvArtist    = Curvature_Artist (self.diagram_frame.ax2,  [self.airfoil], 
                                             show=True)

        self.airfoilArtist.refresh()
        self.curvArtist.refresh()
        self.bezierArtist.refresh(figureUpdate=True)            # -> draw_idle

        # ----------- Input Fields  ----------------

        # Header 
        c = 0 
        r = 0 
        Header_Widget (self.header_frame,r,c, pady=0, lab= "Bezier airfoil", sticky = 'nw', width=100)
        
        Switch_Widget (self.header_frame,r,c+1, padx=(30,30), lab='Show original airfoil',
                       get=lambda: self.showOrg, set=self.airfoilArtist.set_show)

        Label_Widget  (self.header_frame,r, c+2, padx= 15, sticky = 'nw', columnspan=1,
                        lab= "lore ipsum")
        Label_Widget  (self.header_frame,r, c+3, padx= 20,  sticky = 'nw', columnspan=1,
                        lab= "und weiter ....")

        self.header_frame.grid_columnconfigure (4, weight=1)

        # input fields normalized  airfoil 

        r,c = 0,0  
        Label_Widget (self.input_frame,r,c, padx=0, lab= "New Bezier based airfoil", columnspan = 2)

        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,  lab="Upper points", 
                                obj=self.airfoil.upper, get='nPoints', 
                                spin=True, step=1, disable=True, width=90, lab_width=90))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,  lab="Lower points", 
                                obj=self.airfoil.lower, get='nPoints', 
                                spin=True, step=1, disable=True, width=90, lab_width=90))







        c = 8
        r = 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Thickness", obj=self.airfoil, 
                                get='maxThickness', 
                                disable=True, width=65, lab_width=70, unit="%", dec=2))
        self.add (Field_Widget  (self.input_frame,r,c+3, lab="at", lab_width=20, obj=self.airfoil, 
                                get='maxThicknessX',  
                                disable=True, width=65, unit="%", dec=2))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Camber", obj=self.airfoil, 
                                get='maxCamber', 
                                disable=True, width=65, lab_width=70, unit="%", dec=2))
        self.add (Field_Widget  (self.input_frame,r,c+3, lab="at", lab_width=20, obj=self.airfoil, 
                                get='maxCamberX',   
                                disable=True, width=65,  unit="%", dec=2))
        r += 1
        c = 0 
        self.add (Field_Widget  (self.input_frame,r,c,   lab="TE gap", obj=self.airfoil, 
                                get='teGap', set='set_teGap', step=0.01,
                                spin=True, width=90, lab_width=70, unit="%", dec=2,
                                event=self.changed_te_gap))
        Label_Widget (self.input_frame,r,c+3, padx=0, lab= "will y-move last point of Bezier curve ", columnspan = 2)


        # ---- buttons in super class



    def _set_logScaleY (self, ax): 
        # log y scale to get full range of curvature 
        # suppress warning of a motplotlib bug reported: 
        # https://github.com/matplotlib/matplotlib/issues/26118
        import warnings
        warnings.filterwarnings("ignore", message = "All values for SymLogScale")
        # ! linear range from 0..1 will lead to a kink in the curve 
        thresh  = 1 
        ax.set_yscale('symlog', linthresh=thresh, linscale=1) # , linscale=2
        ax.set_ylim([ -10, 1000])
        # mark linear and log y-axis because of kink in y log scale
        x, y = 0.008, 0.5                   # x in axes coordinates
        ax.text (x, y, 'linear', va='center', ha='left', transform=ax.get_yaxis_transform(), 
                            color = cl_labelGrid, fontsize='small')
        y = 40
        ax.text (x, y, 'log10', va='top',     ha='left', transform=ax.get_yaxis_transform(), 
                            color = cl_labelGrid, fontsize='small')
        ax.axhline (y= thresh, xmin=0, xmax=0.035, linestyle='dashed', linewidth=0.5, color = cl_labelGrid,)
        ax.axhline (y=-thresh, xmin=0, xmax=0.035, linestyle='dashed', linewidth=0.5, color = cl_labelGrid,)


    def changed_bezier(self):
        # callback when bezier point was moved in diagram
        self.airfoil._spline = None  # todo
        self.curvArtist.refresh(figureUpdate=True)
        self.refresh()


    def changed_te_gap (self):
        # callback when te gap was changed in entry field 
        self.airfoil._spline = None  # todo
        self.refresh()
        self.bezierArtist.refresh()
        self.curvArtist.refresh(figureUpdate=True)




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
            set_initialWindowSize (main, widthFrac=0.70, heightFrac=0.65)
            main.transient (parentApp)
            self.return_newAirfoilPathFileName = None   # return value for parent
            self.return_newAirfoilName         = None   # return value for parent

        else: 
            main = ctk.CTk()  
            set_initialWindowSize(main, widthFrac=0.80, heightFrac=0.75)

        self.main = main 
        self.ctk_root = main        

        # self.main.withdraw()
        # splash = SplashWinwow()


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

        switches_frame = ctk.CTkFrame     (main, height=350)
        diagram_frame  = Diagram_Airfoil  (main, self.curAirfoil, fg_color= cl_background[1], view_frame=switches_frame)
        edit_frame     = ctk.CTkFrame     (main, height=300, fg_color= 'transparent')
        file_frame     = Edit_File_Menu   (main, self.curAirfoil, myApp=self)

        # maingrid 2 x 2 - diagram on top, file and edit on bottom
        main.grid_rowconfigure   (0, weight=2)
        main.grid_rowconfigure   (1, weight=0)
        main.grid_columnconfigure(1, weight=1)
        switches_frame.grid  (row=0, column=0, pady=(5,5), padx=(5,0), ipady=5,sticky="news")
        diagram_frame.grid   (row=0, column=1, pady=(5,5), padx=(5,5), sticky="news")
        file_frame.grid      (row=1, column=0, pady=(0,5), padx=(5,0), ipady=5,sticky="news")
        edit_frame.grid      (row=1, column=1, pady=0,     padx=0,     sticky="nesw")

        # different sections of the edit area 1 x 3 
        edit_frame.grid_rowconfigure      (0, weight=1)
        edit_frame.grid_columnconfigure   (3, weight=1)

        edit_Airfoil_frame    = Edit_Airfoil_Data   (edit_frame, self.curAirfoil, myApp=self)
        edit_Airfoil_frame.grid   (row=0, column=0, pady=(0,5), padx=(5,0), ipadx=10, sticky="news")

        edit_Panels_frame  = Edit_Panels (edit_frame, self.curAirfoil, myApp=self)
        edit_Panels_frame.grid (row=0, column=1, pady=(0,5), padx=(5,0), ipadx=10, sticky="news")

        edit_Coordinates_frame  = Edit_Coordinates (edit_frame, self.curAirfoil, myApp=self)
        edit_Coordinates_frame.grid (row=0, column=2, pady=(0,5), padx=(5,0), ipadx=10, sticky="news")

        edit_Curvature_frame  = Edit_Curvature (edit_frame, self.curAirfoil, myApp=self)
        edit_Curvature_frame.grid (row=0, column=3, pady=(0,5), padx=(5,5), ipadx=10, sticky="news")


        # start App - run mainloop if self is not modal otherise control is at parent

        # splash.closeIt()
        # main.deiconify()
        
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
                airfoil = Airfoil(pathFileName=pathFilename)
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
        NoteMsg ("The App is scaled to %.2f" %scaling)

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
            airfoil_files = sorted (airfoil_files)
            airfoil_file = airfoil_files[0]
            NoteMsg ("No airfoil file as argument. Showing example airfoils in '%s'" %airfoil_dir)
        else:
            NoteMsg ("No airfoil file as argument. Showing example airfoil...")
            airfoil_file = None

    myApp = AirfoilEditor (airfoil_file)
    
 