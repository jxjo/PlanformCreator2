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

import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from tkinter import filedialog, Frame
import customtkinter as ctk

# let python find the other modules in modules relativ to path of self  
sys.path.append(os.path.join(Path(__file__).parent , 'modules'))
from modules.common_utils       import * 
from modules.airfoil            import *
from modules.airfoil_examples   import Root_Example
from modules.widgets            import *
from modules.airfoil_artists    import *


#------------------------------------------------

AppName    = "Airfoil Viewer"
AppVersion = "0.6.1"

#------------------------------------------------

cl_background               = '#101010'                     # background color in diagrams

#   change events for updating mainly plots

AIRFOIL_NEW                 = "<<AIRFOIL_NEW>>"                #tk event types
AIRFOIL_CHANGED             = "<<AIRFOIL_CHANGED>>"
AIRFOIL_REPANELED           = "<<AIRFOIL_REPANELED>>"

ctk_root : ctk.CTk = None                                   # root event handler

def fireEvent(eventType): 
    # print ("- fire event from root", eventType)
    if ctk_root: ctk_root.event_generate (eventType) 


#-------------------------------------------------------------------------------
# Edit Frames    
#-------------------------------------------------------------------------------

class Edit_Abstract (ctk.CTkFrame):
    """ 
    Abstract superclass for all the edit like frames
    """

    def __init__(self, master, airfoilFn, *args, **kwargs):
        super().__init__(master, *args, **kwargs)

        self._airfoilFn = airfoilFn               # the function to the wing (indirect because of new wing could be set)
        self.widgets = []

        ctk_root.bind(AIRFOIL_CHANGED,          self.changed_airfoil, add='+')
        ctk_root.bind(AIRFOIL_NEW,              self.changed_airfoil, add='+')

        self.init()
    
    def airfoil(self) -> Airfoil:
        # it's a method - not a property to exchange the wing 
        return self._airfoilFn()
    
    @property
    def myApp (self) -> 'App':
        """ the App self belongs to"""
        return self.winfo_toplevel()

    def init(self):
        # main method - to be overloaded by sub class
        pass

    def add (self, aWidget): 
        # kepp track of the widgets of self to be able to refresh them
        self.widgets.append (aWidget)

    def refresh(self):
        # refresh typically based on changed events 
        for widget in self.widgets:
            if isinstance(widget, Base_Widget): widget.refresh()

    def changed_airfoil(self, dummy): 
        """ Eventhandler for changes of airfoil"""
        self.refresh()      

#-------------------------------------------

class Edit_Airfoil_Data(Edit_Abstract):
    """ 
    Frame to edit main data of the airfoil like thickness. This is just the header
    """
    name = "Airfoil"

    def init (self):

        r, c = 0, 0 
        self.add (Header_Widget (self,r,c,   width= 70, columnspan= 2, lab=self.name))
        self.add (Field_Widget  (self,r,c+2, columnspan= 8 ,lab=None, obj=self.airfoil, get='name', set='set_name',
                                 lab_width=1, event=AIRFOIL_CHANGED, width=255, justify='left'))

        r += 1
        Blank_Widget (self, r,0)    
        c += 1                                  # left blank colum to inset the fields 
        self.add (Field_Widget  (self,r,c,   lab="Thickness", obj=self.airfoil, 
                                get='maxThickness', set='', 
                                spin=True, width=95, lab_width=70, unit="%", dec=2, disable= True))
        self.add (Field_Widget  (self,r,c+3, lab="at", lab_width=20, obj=self.airfoil, 
                                get='maxThicknessX', set='', 
                                spin=True, width=95, unit="%", dec=2, disable= True))
        r += 1
        self.add (Field_Widget  (self,r,c,   lab="Camber", obj=self.airfoil, 
                                get='maxCamber', set='', 
                                spin=True, width=95, lab_width=70, unit="%", dec=2, disable= True))
        self.add (Field_Widget  (self,r,c+3, lab="at", lab_width=20, obj=self.airfoil, 
                                get='maxCamberX', set='', 
                                spin=True, width=95, unit="%", dec=2, disable= True))

        r += 1
        self.add (Field_Widget  (self,r,c,   lab="TE gap", obj=self.airfoil, 
                                get='teGapPercent', set='', 
                                spin=True, width=95, lab_width=70, unit="%", dec=2, disable= True))
        r += 1
        Blank_Widget (self, r,c)

        r += 1
        self.add(Label_Widget  (self,r, c, columnspan=9, padx= 5, text_style = 'Warning',
                                lab= lambda: self.messageText()))
 
    def messageText (self): 
        text = []
        if  self.airfoil().maxThickness != self.airfoil().maxThickness:     # test for nan value 
            text.append("The airfoil has erronous coordinates - can't spline.")
        
        text = '\n'.join(text)
        return text 



class Edit_Curvature(Edit_Abstract):
    """ 
    Frame to edit main data of the airfoil like thickness. This is just the header
    """
    name = "Curvature"

    def __init__(self, master, airfoilFn, *args, **kwargs):

        self._curvature_threshold = None
        self._deriv3_threshold    = None

        super().__init__(master, airfoilFn, *args, **kwargs)

    @property
    def curvature_threshold (self): 
        return self._curvature_threshold
    def _set_curvature_threshold (self, aVal):
        self._curvature_threshold = aVal
        self.airfoil().spline.curv_upper.set_threshold(aVal)
        self.airfoil().spline.curv_lower.set_threshold(aVal)
        # fireEvent (AIRFOIL_CHANGED)
    
    @property
    def deriv3_threshold (self): 
        return self._deriv3_threshold
    def _set_deriv3_threshold (self, aVal):
        self._deriv3_threshold    = aVal
        self.airfoil().spline.deriv3_upper.set_threshold(aVal)
        self.airfoil().spline.deriv3_lower.set_threshold(aVal)
        # fireEvent (AIRFOIL_CHANGED)


    def init (self):

        self._curvature_threshold = self.airfoil().spline.curv_upper.threshold
        self._deriv3_threshold    = self.airfoil().spline.deriv3_upper.threshold

        r, c = 0, 0 
        self.add (Header_Widget (self,r,c,   width=90, lab=self.name, columnspan= 2))
        self.add (Label_Widget  (self,r,c+2, padx=0, width=70, lab='Reversals', columnspan= 2))
        self.add (Label_Widget  (self,r,c+4, padx=0, width=70, lab='Spikes', columnspan= 2))

        c += 6
        self.add(Button_Widget  (self,r,c, lab='Smooth', width=80, padx= 0, columnspan=2, sticky='w', 
                                 set=self.smooth_airfoil ))


        r = 1
        Blank_Widget (self, r,0)                # left blank column to inset the fields 
        c = 1                                  
        self.add (Field_Widget  (self,r,c,   lab="Upper side", get=lambda: len(self.airfoil().spline.curv_upper.reversals()),
                                width=60, lab_width=80, set='', dec=0, disable= True))
        self.add (Field_Widget  (self,r,c+3,   get=lambda: len(self.airfoil().spline.deriv3_upper.reversals()),
                                width=60,               set='', dec=0, disable= True))
        r += 1
        self.add (Field_Widget  (self,r,c, lab="Lower side", get=lambda: len(self.airfoil().spline.curv_lower.reversals()), 
                                width=60, lab_width=80, set='', dec=0, disable= True))
        self.add (Field_Widget  (self,r,c+3, get=lambda: len(self.airfoil().spline.deriv3_lower.reversals()), 
                                width=60,               set='', dec=0, disable= True))
        r += 1
        self.add (Field_Widget  (self,r,c,   lab="... threshold", get=lambda: self.curvature_threshold, 
                                set=self._set_curvature_threshold, event=AIRFOIL_CHANGED,
                                width=60, lab_width=80, lim=(0,1), dec=1, spin=False, step=0.1))
        self.add (Field_Widget  (self,r,c+3, get=lambda: self.deriv3_threshold, 
                                set=self._set_deriv3_threshold, event=AIRFOIL_CHANGED, 
                                width=60,               lim=(0,10), dec=1, spin=False, step=0.2))

    def refresh(self): 
        #overloaded to set threshold in airfoils for reversal calculation 
        self._set_curvature_threshold (self.curvature_threshold)
        self._set_deriv3_threshold (self.deriv3_threshold)
        super().refresh()

    def smooth_airfoil (self): 
        """ Open smooth dialog - if 'OK' the new smoothed airfoil will be inserted to the list 
        of airfoils and set to current """

        dialog = Dialog_Repanel (self, self._airfoilFn )
        self.wait_window (dialog)

        if dialog.return_OK: 
            self.myApp.add_toAirfoilFiles (dialog.return_newAirfoilPathFileName)



class Edit_Panels(Edit_Abstract):
    """ 
    Frame to edit main data of the airfoil like thickness. This is just the header
    """
    name = "Panels"

    def init (self):

        r, c = 0, 0 
        self.add (Header_Widget (self,r,c, width= 70, lab=self.name, columnspan= 2))
        c += 2
        self.add (Field_Widget  (self,r,c, get=lambda: len(self.airfoil().x) -1,
                                 width=50, lab_width=0, dec=0, text_style=lambda: self.style('no')))
        c += 2
        self.add(Button_Widget  (self,r,c, lab='Repanel', width=80, padx= 0, columnspan=2, sticky='w', 
                                 set=self.repanel_airfoil ))

        r += 1
        Blank_Widget (self, r,0)    
        c = 1                                  # left blank column to inset the fields 
        self.add (Field_Widget  (self,r,c,   lab="Angle at LE", get=lambda: self.airfoil().panelAngle_le, 
                                 width=50,   lab_width=80, unit="°", dec=0, text_style=lambda: self.style('le_angle')))
        self.add(Label_Widget   (self,r,c+3, columnspan=2, padx= 0,  
                                 lab= lambda: "at index %d" % self.airfoil().le_i))

        r += 2
        self.add (Field_Widget  (self,r,c,   lab="Angle min", get=lambda: self.airfoil().panelAngle_min[0], 
                                 width=50,   lab_width=80, unit="°", dec=0, text_style=lambda: self.style('min_angle')))
        self.add(Label_Widget   (self,r,c+3, columnspan=2, padx= 0,  
                                 lab= lambda: "at index %d" % self.airfoil().panelAngle_min[1]))
        r += 1
        Blank_Widget (self, r,c, height=42)
        r += 1
        self.add(Label_Widget   (self,r, c, columnspan=9, padx= 5, text_style = 'Warning',
                                 lab= lambda: self.messageText()))
        


    def messageText (self): 

        text = []
        airfoil = self.airfoil()
        minAngle, atIndex = self.airfoil().panelAngle_min

        if airfoil.panelAngle_le > 170.0: 
            text.append("- Panel angle at LE (%d°) is too blunt." %(self.airfoil().panelAngle_le))
        if minAngle < 150.0: 
            text.append("- Min. angle of two panels is < 150°")
        if airfoil.panelAngle_le == 180.0: 
            text.append("- Leading edge has 2 points")
        if airfoil.nPanels < 160 or airfoil.nPanels > 260: 
            text.append("- No of panels should be > 160 and < 260")
        if not text == []:
            text.append("\nRepanel airfoil")
        
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


    def repanel_airfoil (self): 
        """ Open repanel dialog - if 'OK' the new repaneled airfoil will be insert to the list 
        of airfoils and set to current """

        dialog = Dialog_Repanel (self, self._airfoilFn )
        self.wait_window (dialog)

        if dialog.return_OK: 
            self.myApp.add_toAirfoilFiles (dialog.return_newAirfoilPathFileName)



class Edit_Coordinates(Edit_Abstract):
    """ 
    Frame to edit main data of the airfoil like thickness. This is just the header
    """
    name = "Coordinates"

    def init (self):

        r, c = 0, 0 
        self.add (Header_Widget (self,r,c,   width=95, lab=self.name, columnspan= 2))
        c += 3
        self.add(Button_Widget  (self,r,c, lab='Normalize', width=80, padx= (10,0), columnspan=2, sticky='w', 
                                 set=self.normalize_airfoil ))

        # self.add (Label_Widget  (self,r,c+2, padx=0, lab='     x', width=30))
        # self.add (Label_Widget  (self,r,c+4, padx=0, lab='     y', width=30))

        r = 1
        Blank_Widget (self, r,0)    
        c = 1                                  # left blank column to inset the fields 
        self.add (Field_Widget  (self,r,c,   lab="Leading edge", get=lambda: self.airfoil().le[0],
                                 width=80, lab_width=90, dec=7, text_style=lambda: self.style('le_x')))
        self.add (Field_Widget  (self,r,c+3,                     get=lambda: self.airfoil().le[1],
                                 width=80, dec=7,               text_style=lambda: self.style('le_y')))
        r += 1
        self.add (Field_Widget  (self,r,c,   lab="Trailing edge", get=lambda: self.airfoil().te_fromPoints[0],
                                 width=80, lab_width=90, dec=7, text_style=lambda: self.style('te_x')))
        self.add (Field_Widget  (self,r,c+3,                     get=lambda: self.airfoil().te_fromPoints[1],
                                 width=80,               dec=7, text_style=lambda: self.style('te_y')))
 
        r += 1
        self.add (Field_Widget  (self,r,c,   lab=" " ,            get=lambda: self.airfoil().te_fromPoints[2],
                                 width=80, lab_width=90, dec=7, text_style=lambda: self.style('te_x')))
        self.add (Field_Widget  (self,r,c+3,                      get=lambda: self.airfoil().te_fromPoints[3],
                                 width=80,               dec=7, text_style=lambda: self.style('te_y')))

        # r += 1
        # self.add (Field_Widget  (self,r,c,   lab="LE splined", get=lambda: self.airfoil().le[0],
        #                          width=80,   lab_width=90, dec=7, text_style = self.le_splined_style))
        # self.add (Field_Widget  (self,r,c+3,                     get=lambda: self.airfoil().le[1],
        #                          width=80,                 dec=7, text_style = self.le_splined_style))

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
        if not text == []:
            text.append("\nNormalize airfoil")
        
        text = '\n'.join(text)
        return text 
    

    def style (self, coord): 

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


#-------------------------------------------------------------------------------
# Diagrams   
#-------------------------------------------------------------------------------


class Diagram_Abstract(ctk.CTkFrame):
    """ 
    Abstract super class of the specific plot frames like "Plot_Planform"
    """
    name        = "This is the plot super class"
    defaultTip  = "Use switches ...."

    cl_text         = "#DCE4EE"
    cl_labelGrid    = '#B0B0B0'


    def __init__(self, master, airfoilFn, *args, view_frame = None, setActive=True, size= None,  **kwargs):
        super().__init__( master, *args, **kwargs)

        self.view_frame : Frame = view_frame
        self._airfoilFn = airfoilFn

        self.configure(fg_color= cl_background)

        # big diagram grid 
        self.grid_columnconfigure(0, weight=1)
        self.grid_rowconfigure(0, weight=1)
        self.grid(row=0, column=0, sticky="news")

        if size: 
            self.figure : plt.Figure = plt.Figure(figsize=size)
        else: 
            self.figure : plt.Figure = plt.Figure()

        # connect tk and pyplot
        self.canvas = FigureCanvasTkAgg(self.figure, self)
        self.canvas._tkcanvas.grid_columnconfigure(0, weight=1)
        self.canvas._tkcanvas.grid_rowconfigure   (0, weight=1)
        self.canvas._tkcanvas.grid (row=0, column=0, pady=0, padx=0, sticky="news")
        self.canvas._tkcanvas.configure(background= cl_background)


        # common axes for this diagram
        self.create_axes()
        self.setup_axes ()

        # Create the artists for the diagramm
        self.setup_artists ()

        # init of switches / plots if a frame for the switches is available 
        col = 0 
        row = 0 
        if self.view_frame: 
            self.view_frame.grid_columnconfigure(0, weight=1)   # to center switches
            col += 1
            row, col = self.setup_Switches (row=0, col=1)                       

            row +=2
            Label_Widget (self.view_frame, row, 0, lab='Pan and zoom')
            row +=1
            self.toolbar = Plot_Toolbar(self.canvas, self.view_frame)
            self.toolbar.grid (row=row+2, column=0, columnspan= 2, sticky='ew', padx=10, pady=(0,10))

        # react on changes of model
        self.setChangeBindings ()

        # is active frame? control change events 
        self._active            = setActive
        # and finally show it - if "active"
        if self._active: self.refresh()


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


    def setup_Switches(self, row=0, col=0):
        """ define on/off switches ffor this plot type"""

        row = 0 
        Header_Widget (self.view_frame,row,col, lab='View', width=80)

        row += 1 
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Grid', 
                       get=lambda: self.gridArtist.show, set=self.gridArtist.set_show)

        # extra col to center all the switches
        self.view_frame.grid_columnconfigure(col+1, weight=1)
        return row, col 

    def refresh(self, dummy): 
        # overwrite in sub class
        pass  

    def setChangeBindings (self):
        # overwrite in sub class
        pass      




class Diagram_Airfoil (Diagram_Abstract):
    """ 
    plots the airfoils of all wing sections
    """
    name = "Airfoils"

    def create_axes (self):
        """ setup 2 axes for airfoil and its curvature  """

        self.ax1 : plt.Axes = self.figure.add_subplot(2, 1, 1)
        self.ax2 : plt.Axes = self.figure.add_subplot(2, 1, 2)
        self.figure.subplots_adjust(left=0.04, bottom=0.07, right=0.96, top=0.97, wspace=None, hspace=0.15)

    def setup_axes(self):
        """ setup axes, axis, artiss for this plot type """

        # airfoil contour 
        self.ax1.tick_params (labelbottom=True, labelleft=True, labelsize='small')
        self.ax1.set_xlim([-0.05,1.05])
        self.ax1.axis('equal')


        # curvature
        # self.ax2.set_ylabel('curvature', color=self.cl_text)
        self.ax2.tick_params (labelsize='small')
        self.ax2.set_xlim([-0.05,1.05])
        self.ax2.set_ylim([ 5, -5.0])
        self.ax2.set_aspect('auto', 'datalim')
        self.ax2.grid (axis='x')
        self.ax2.axhline(0, color=self.cl_labelGrid, linewidth=0.5)

        self.ax2Twin = self.ax2.twinx()
        # self.ax2Twin.set_ylabel('3rd derivative', color=self.cl_text)
        self.ax2Twin.tick_params (labelsize='small')
        self.ax2Twin.set_ylim([ -100,  100])
        # mirrorax2 = ax2.get_shared_x_axes().get_siblings(ax2)[0]


    def setup_artists(self):
        """ setup axes, axis, artists for this plot type """

        super().setup_artists()

        # airfoil is list to prepare for future multiple airfoils to view 
        
        self.airfoilArtist   = Airfoil_Artist        (self.ax1,     [self._airfoilFn], show=True)
        self.camberArtist    = Airfoil_Camber_Artist (self.ax1,     [self._airfoilFn], show=False)
        # self.curvArtist      = Airfoil_Curv_Artist   (self.ax2,     [self._airfoilFn], show=True)
        self.curvatureArtist = Curvature_Artist      (self.ax2,     [self._airfoilFn], show=True)
        self.deriv3Artist    = Airfoil_Deriv3_Artist (self.ax2Twin, [self._airfoilFn], show=False)


    def setup_Switches(self, row=0, col=0):
        """ define on/off switches for this plot type"""

        row, col = super().setup_Switches(row, col)

        self._set_upper (True) 
        self._set_lower (True) 

        disableNotNormalized = not self.airfoil.isNormalized

        row += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Points',
                       get=lambda: self.airfoilArtist.points, set=self._set_points)

        row += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Camber', 
                       get=lambda: self.camberArtist.show, set=self.camberArtist.set_show)

        row += 1
        Blank_Widget (self.view_frame,row,col)
        self.view_frame.grid_rowconfigure(row, weight=2)

        row += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Upper side', 
                       get=lambda: self.curvatureArtist.upper, set=self._set_upper)
        row += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Lower side',
                       get=lambda: self.curvatureArtist.lower, set=self._set_lower)

        row += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Curvature', 
                       get=lambda: self.curvatureArtist.show, set=self.curvatureArtist.set_show)
        row += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='3rd derivative', 
                       get=lambda: self.deriv3Artist.show, set=self.deriv3Artist.set_show)

        row += 1
        Blank_Widget (self.view_frame,row,col)
        self.view_frame.grid_rowconfigure(row, weight=1)

        return row, col


    # -------- switch call back 

    def _set_points (self, aBool):
        self.airfoilArtist.set_points(aBool)
        self.curvatureArtist.set_points(aBool)
        self.deriv3Artist.set_points(aBool)
        self.refresh()

    def _set_upper (self, aBool):
        self.curvatureArtist.set_upper(aBool)
        self.deriv3Artist.set_upper(aBool)
        self.refresh()

    def _set_lower (self, aBool):
        self.curvatureArtist.set_lower(aBool) 
        self.deriv3Artist.set_lower(aBool)
        self.refresh()

    # -------- event handler

    def setChangeBindings (self):
        # overloaded
        ctk_root.bind(AIRFOIL_CHANGED,          self.changed_airfoil, add='+')
        ctk_root.bind(AIRFOIL_NEW,              self.changed_airfoil, add='+')

    def changed_airfoil(self, dummy): 
        """ Eventhandler for changes of airfoil"""
        self.refresh()

    # -------- refresh my Artists which are on 'show mode' 

    def refresh(self): 
        # overloaded
        self.airfoilArtist.refresh ()  
        self.camberArtist.refresh ()  
        self.deriv3Artist.refresh ()  
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


    def setup_Switches(self, row=0, col=0):
        """ no switches"""
        return row, col

    # -------- event handler

    # refresh handled in App 

    # -------- refresh my Artists which are on 'show mode' 

    def refresh(self): 
        # overloaded

        self.ax1.figure.canvas.draw_idle()    # draw ony if Windows is idle!



class Edit_File_Menu(Edit_Abstract):
    """ 
    Frame for the high level commands like load, save, ...
    The parent is the App itself
    """
    name = "File"

    def init (self):

        self.grid_columnconfigure   (0, weight=1)
        self.add (Header_Widget (self,0,0, lab=self.name, width=80))

        # file selection with buttons - takes 2 rows
        self.add(Option_Widget (self,1,0, get=self.curAirfoilFileName, set=self.set_curAirfoilFileName,
                                          options=self.airfoilFileNames,
                                          spin=True, spinPos='below', width=100, padx=10, pady=4, sticky = 'ew',))
        self.add(Button_Widget (self,3,0, lab='Open',        width=100, pady=4, sticky = 'ew', set=self.myApp.open))
        self.add(Button_Widget (self,4,0, lab='Save As...',  disable=True, width=100, pady=4, sticky = 'ew', set=self.myApp.saveAs))

    def airfoilFileNames (self): 

        fileNames = []
        for aFileName in self.myApp.airfoilFiles:
            fileNames.append(os.path.basename(aFileName))
        return fileNames
    
    def curAirfoilFileName (self): 

        return os.path.basename(self.myApp.curAirfoilFile)  

    def set_curAirfoilFileName (self, newFileName): 
        
        for aPathFileName in myApp.airfoilFiles:
            if newFileName == os.path.basename(aPathFileName):
                self.myApp.loadNewAirfoil (aPathFileName)
                self.refresh()
                break


#-------------------------------------------------------------------------------
# Dialogs for smaller tasks   
#-------------------------------------------------------------------------------


class Dialog_Abstract (ctk.CTkToplevel):
    """ 
    Abstract superclass for windows dialogs 

    self.return_OK is True if user conformed 'ok' 
    """
    width  = 500
    height = 400
    titleText  = "My little title"

    def __init__(self, master, workingDir=None, *args, **kwargs):
        super().__init__(master, *args, **kwargs)

        # the default directory for file activities
        self.workingDir = workingDir

        # the attribute for return ok
        self.return_OK = False

        xPos, yPos = self.centerPosition(self.width, self.height)
        self.geometry("%sx%s+%s+%s" %(self.width, self.height, xPos, yPos))

        self.title (self.titleText)
        self.resizable(False, False)                        # width, height
        self.transient(master)

        # make dialog modal 
        self.focus_force() 
        self.grab_set()
        self.protocol("WM_DELETE_WINDOW", self.destroy)

        self.edit_frame    = ctk.CTkFrame (self) 
        self.grid_rowconfigure    (0, weight=1)
        self.grid_columnconfigure (0, weight=1)
        self.edit_frame.grid    (row=0, column=0, pady=10, padx=10, sticky="nesw")

        self.widgets = []                                   # for refresh logic  


    def ok (self):
        # to over load and do ok actions
        self.return_OK = True
        self.destroy()

    def cancel (self): 
        # to over load and do cancel actions
        self.destroy()

    def add (self, aWidget): 
        self.widgets.append (aWidget)

    def refresh(self):
        for widget in self.widgets:
            if isinstance(widget, Base_Widget): widget.refresh()

    def centerPosition(self, width=None, height=None):
        """ get center of a tkinter window
        """
        self.update_idletasks()

        if width is None:  width  = self.winfo_width()
        if height is None: height = self.winfo_height()

        x = self.winfo_screenwidth() // 2 - width // 2
        y = self.winfo_screenheight() // 2 - int (height / 1.7)
        return x, y



#-------------------------------------------

class Dialog_Repanel (Dialog_Abstract):
    """ 
    Dialog to repanel airfoil  
    """

    width  = 1450
    height = 700

    def __init__(self, master, airfoilFn, *args, **kwargs):
        super().__init__(master, *args, height=self.height/2, **kwargs)

        self.airfoil : Airfoil = airfoilFn()
        self.airfoilSplined  = Airfoil_Splined (self.airfoil) 
        self.airfoilSplined.repanel()               # using default values 

        self.title ("Repanel airfoil  [" + self.airfoil.name + "]")

        self.return_newAirfoilPathFileName = None   # return value for parent

        self.showRepaneled = True 


        # main grid 4 x 1  (preview + edit + buttons) 
        self.header_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.header_frame.grid(row=0, column=0, sticky="nwe", padx=0, pady=(10,10))

        self.diagram_frame = Diagram_LeTe_Mini (self.edit_frame, self.airfoilListFn, size=(8,4.8))
        self.diagram_frame.grid(row=1, column=0, sticky="nwe")

        self.input_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.input_frame.grid(row=2, column=0, sticky="nwe", padx=40, pady=10)

        self.button_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.button_frame.grid(row=3, column=0, sticky="wes", pady=10)

        self.edit_frame.grid_columnconfigure (0, weight=1)
        self.edit_frame.grid_rowconfigure    (3, weight=1)

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
        r = 0 
        c = 0 
        self.add (Field_Widget (self.input_frame,r,c, lab="Leading edge bunch", lab_width=130, 
                                width=100, 
                                obj=self.airfoilSplined, get='le_bunch', set='set_le_bunch',
                                event=AIRFOIL_REPANELED, lim=(0,1), dec=2, spin=True, step=0.02))
        self.add (Label_Widget (self.input_frame,r,c+3, text_style=self.le_bunch_message_style,
                                lab= lambda: self.le_bunch_message(), columnspan = 1, width= 250))

        self.add (Field_Widget (self.input_frame,r,c+4, lab="Trailing edge bunch", lab_width=130, 
                                width=100,
                                obj=self.airfoilSplined, get='te_bunch', set='set_te_bunch',
                                event=AIRFOIL_REPANELED, lim=(0,1), dec=2, spin=True, step=0.02))

        r +=1 
        Blank_Widget (self.input_frame,r,c,  height= 10)
        r +=1 
        self.add (Field_Widget (self.input_frame,r,c  , lab="No of panels", width=100,
                                obj=self.airfoilSplined, get='nPanels', set='set_nPanels',
                                event=AIRFOIL_REPANELED, lim=(50,500), dec=0, spin=True, step=10))
        self.add (Label_Widget (self.input_frame,r,c+3  , columnspan = 1, 
                                lab= lambda: "equals %d points" % self.airfoilSplined.nPoints))

        self.add (Field_Widget  (self.input_frame,r,c+4, columnspan= 3 ,lab="New Airfoil name", 
                                 obj=self.airfoilSplined, get='name', set='set_name',
                                 width=255, justify='left'))
        self.add (Label_Widget (self.input_frame,r,c+8, text_style='Warning',
                                lab= lambda: self.save_warning(), columnspan = 1, width= 110))


        self.input_frame.grid_columnconfigure (8, weight=2)

        # buttons on extra frame 
        r = 0 
        c = 1 
        self.add(Button_Widget (self.button_frame,r,c, lab='Save As...', set=self.save, primary=True, width=100))
        c += 1 
        self.add(Button_Widget (self.button_frame,r,c, lab='Ok', set=self.ok, primary=True, width=100))
        c += 1 
        self.add(Button_Widget (self.button_frame,r,c, lab='Cancel', set=self.cancel, width=100))
        self.button_frame.grid_columnconfigure (0, weight=1)
        self.button_frame.grid_columnconfigure (5, weight=1)

        # changed bindings
        ctk_root.bind(AIRFOIL_REPANELED, self.refresh, add='+')

    # ---- 

    def airfoilListFn (self):
        if self.showRepaneled: 
            return [self.airfoilSplined]
        else: 
            return [self.airfoil]
    
    def set_showRepaneled (self, aBool):
        self.showRepaneled = aBool
        self.refresh ('') 

    def le_bunch_message (self): 

        angle = self.airfoilSplined.panelAngle_le

        if angle > 170.0: 
            text = "Angle at LE is too blunt. Decrease bunch" 
        elif angle < 155.0: 
            text = "Angle at LE is too sharp. Increase bunch"
        else:
            text = "leads to good panel angle at LE"
        return text 
            

    def le_bunch_message_style (self): 

        angle = self.airfoilSplined.panelAngle_le
        if angle > 170.0 or angle < 155.0: 
            return 'Warning'
        else: 
            return 'Disabled'
        
    def save_warning (self): 

        if self.airfoilSplined.isModified: 
            return "Airfoil not saved"
        else: 
            return ""

    def save(self): 
        """ save repaneled airfoil to file """
        filetypes  = [('Airfoil files', '*.dat')]
        initialDir, airfoilFileName = os.path.split(self.airfoilSplined.pathFileName)
        newPathFilename = filedialog.asksaveasfilename(title='Save airfoil',
                                     initialdir=initialDir, filetypes=filetypes,
                                     initialfile=self.airfoilSplined.name,
                                     defaultextension = '.dat')
        if newPathFilename: 
            airfoilPath, airfoilFileName = os.path.split(newPathFilename)
            # maybe user changed name when saving to file? 
            airfoilName = os.path.splitext(airfoilFileName)[0]
            self.airfoilSplined.apply_repanel ()
            self.airfoilSplined.saveAs (airfoilPath, airfoilName)
            self.refresh('')


    def refresh(self, dummy):
        self.leArtist.refresh(figureUpdate=True)
        self.teArtist.refresh(figureUpdate=True)
        self.airfoilArtist.refresh(figureUpdate=True)
        super().refresh()



    def cancel(self): 
        # changed bindings
        ctk_root.unbind(AIRFOIL_REPANELED)
        super().cancel()

    def ok(self): 

        # save in directory of original airfoil with new name  
        if self.airfoilSplined.isModified: 
            airfoilDir = os.path.split(self.airfoilSplined.pathFileName)[0]
            self.airfoilSplined.saveAs (dir = airfoilDir)

            message = "Airfoil '%s'\n\nsaved to\n\n%s" % (self.airfoilSplined.name, airfoilDir )
            msg = Messagebox (self, title="Save", message=message, icon="check", option_1="Ok")
            msg.get()  
                                         # wait until pressed ok

        # return value for parent
        self.return_newAirfoilPathFileName = self.airfoilSplined.pathFileName

        # release changed bindings and close
        ctk_root.unbind(AIRFOIL_REPANELED)
        super().ok()



class Dialog_Normalize (Dialog_Abstract):
    """ 
    Dialog to normalize airfoil
    """

    width  = 700
    height = 350

    def __init__(self, master, airfoilFn, *args, **kwargs):
        super().__init__(master, *args, height=self.height/2, **kwargs)

        self.airfoil : Airfoil = airfoilFn()
        self.airfoilNorm : Airfoil_Normalized = self.airfoil.asNormalized()

        self.title ("Noramlize airfoil  [" + self.airfoil.name + "]")

        self.return_newAirfoilPathFileName = None   # return value for parent


        # main grid 4 x 1  (preview + edit + buttons) 
        self.header_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.header_frame.grid(row=0, column=0, sticky="nwe", padx=0, pady=(10,10))

        self.input_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.input_frame.grid(row=1, column=0, sticky="nwe", padx=40, pady=10)

        self.button_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.button_frame.grid(row=2, column=0, sticky="wes", pady=(30,10))

        self.edit_frame.grid_columnconfigure (0, weight=1)
        self.edit_frame.grid_rowconfigure    (3, weight=1)

        # Header 
        c = 0 
        r = 0 
        Header_Widget (self.header_frame,r,c, pady=0, lab= "Normalize Airfoil", sticky = 'nw', width=100)
        
        Label_Widget  (self.header_frame,r, c+1, padx= 20, sticky = 'nw', columnspan=1,
                        lab= "For normalizing the airfoil is shifted, rotated and scaled\n" + \
                        "so LE will be at 0,0 and TE is symmetric at 1,y.")

        self.header_frame.grid_columnconfigure (2, weight=1)

        # fields normalized
        r = 0 
        c = 0 
        self.add (Label_Widget (self.input_frame,r,c+1, padx=0, lab="Normalized x", columnspan = 2))
        self.add (Label_Widget (self.input_frame,r,c+3, padx=0, lab="y"))

        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Leading edge", get=lambda: self.airfoilNorm.le[0],
                                 width=80, dec=7))
        self.add (Field_Widget  (self.input_frame,r,c+3,                     get=lambda: self.airfoilNorm.le[1],
                                 width=80,                 dec=7))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab="Trailing edge", get=lambda: self.airfoilNorm.te_fromPoints[0],
                                 width=80, dec=7))
        self.add (Field_Widget  (self.input_frame,r,c+3,                      get=lambda: self.airfoilNorm.te_fromPoints[1],
                                 width=80,                 dec=7))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" " ,            get=lambda: self.airfoilNorm.te_fromPoints[2],
                                 width=80, dec=7))
        self.add (Field_Widget  (self.input_frame,r,c+3,                      get=lambda: self.airfoilNorm.te_fromPoints[3],
                                 width=80,                 dec=7))


        # fields original 
        r = 0 
        c = 5 
        self.add (Label_Widget (self.input_frame,r,c+1, padx=0, lab="Original x", columnspan = 2))
        self.add (Label_Widget (self.input_frame,r,c+3, padx=0, lab="y"))

        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" ", get=lambda: self.airfoil.le[0],
                                 width=80,   lab_width=60, dec=7, text_style=lambda: self.style('le_x')))
        self.add (Field_Widget  (self.input_frame,r,c+3, get=lambda: self.airfoil.le[1],
                                 width=80, dec=7,                 text_style=lambda: self.style('le_y')))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" ", get=lambda: self.airfoil.te_fromPoints[0],
                                 width=80,   lab_width=60, dec=7, text_style=lambda: self.style('te_x')))
        self.add (Field_Widget  (self.input_frame,r,c+3,          get=lambda: self.airfoil.te_fromPoints[1],
                                 width=80,                 dec=7, text_style=lambda: self.style('te_y')))
        r += 1
        self.add (Field_Widget  (self.input_frame,r,c,   lab=" ", get=lambda: self.airfoil.te_fromPoints[2],
                                 width=80,   lab_width=60, dec=7, text_style=lambda: self.style('te_x')))
        self.add (Field_Widget  (self.input_frame,r,c+3,          get=lambda: self.airfoil.te_fromPoints[3],
                                 width=80,                 dec=7, text_style=lambda: self.style('te_y')))


        c = 0 
        r +=1 
        Blank_Widget (self.input_frame,r,c,  height= 40)
        r +=1 
        self.add (Field_Widget  (self.input_frame,r,c, columnspan= 6 ,lab="New airfoil name", 
                                 obj=self.airfoilNorm, get='name', set='set_name',
                                 width=255, justify='left'))
        self.add (Label_Widget (self.input_frame,r,c+6, text_style='Warning',
                                lab= lambda: self.save_warning(), columnspan = 2, width= 110))

        self.input_frame.grid_columnconfigure (9, weight=2)

        # buttons on extra frame 
        r = 0 
        c = 1 
        self.add(Button_Widget (self.button_frame,r,c, lab='Save As...', set=self.save, primary=True, width=100))
        c += 1 
        self.add(Button_Widget (self.button_frame,r,c, lab='Ok', set=self.ok, primary=True, width=100))
        c += 1 
        self.add(Button_Widget (self.button_frame,r,c, lab='Cancel', set=self.cancel, width=100))
        self.button_frame.grid_columnconfigure (0, weight=1)
        self.button_frame.grid_columnconfigure (5, weight=1)

        # changed bindings
        ctk_root.bind(AIRFOIL_REPANELED, self.refresh, add='+')

    # ---- 
        
    def style (self, coord): 

        if   coord =="le_x":
            if self.airfoil.le[0] != 0.0 : return 'Warning'
        elif coord =="le_y":
            if self.airfoil.le[1] != 0.0 : return 'Warning'
        elif coord =="te_x":
            if self.airfoil.te_fromPoints[0] != 1.0 or \
               self.airfoil.te_fromPoints[2] != 1.0 : return 'Warning'
        elif coord =="te_y":
            if self.airfoil.te_fromPoints[3] != - self.airfoil.te_fromPoints[1] : return 'Warning'

    def save_warning (self): 

        if self.airfoilNorm.isModified: 
            return "Airfoil not saved"
        else: 
            return ""

    def save(self): 
        """ save normalized airfoil to file """
        filetypes  = [('Airfoil files', '*.dat')]
        initialDir, airfoilFileName = os.path.split(self.airfoilNorm.pathFileName)
        newPathFilename = filedialog.asksaveasfilename(title='Save airfoil',
                                     initialdir=initialDir, filetypes=filetypes,
                                     initialfile=self.airfoilNorm.name,
                                     defaultextension = '.dat')
        if newPathFilename: 
            airfoilPath, airfoilFileName = os.path.split(newPathFilename)
            # maybe user changed name when saving to file? 
            airfoilName = os.path.splitext(airfoilFileName)[0]
            self.airfoilNorm.saveAs (airfoilPath, airfoilName)
            self.refresh('')


    def cancel(self): 
        # changed bindings
        ctk_root.unbind(AIRFOIL_REPANELED)
        super().cancel()


    def ok(self): 

        # save in directory of original airfoil with new name  
        if self.airfoilNorm.isModified: 
            airfoilDir = os.path.split(self.airfoilNorm.pathFileName)[0]
            self.airfoilNorm.saveAs (dir = airfoilDir)

            message = "Airfoil '%s'\n\nsaved to\n\n%s" % (self.airfoilNorm.name, airfoilDir )
            msg = Messagebox (self, title="Save", message=message, icon="check", option_1="Ok")
            msg.get()                       # wait until pressed ok
                                         
        # return value for parent
        self.return_newAirfoilPathFileName = self.airfoilNorm.pathFileName

        # release changed bindings and close
        ctk_root.unbind(AIRFOIL_REPANELED)
        super().ok()



#-------------------------------------------------------------------------------
# The App   
#-------------------------------------------------------------------------------

class App(ctk.CTk):

    name = AppName  

    def __init__(self, airfoilFiles):
        super().__init__(fg_color= cl_background)

        global ctk_root                                 # being event handler

        # create the 'wing' model 
        self.airfoilFiles =[]
        self.curAirfoilFile = None

        if not airfoilFiles:
            self.loadNewAirfoil ("", airfoil=Root_Example() )
        else: 
            if not type(airfoilFiles) == list: 
                self.airfoilFiles.append (airfoilFiles)
            else:
                self.airfoilFiles = airfoilFiles
            self.loadNewAirfoil (self.airfoilFiles[0])

        self.title (AppName + "  v" + str(AppVersion) + "  [" + self.curAirfoil().name + "]")
        # intercept app close by user  
        self.protocol("WM_DELETE_WINDOW", self.onExit)

        # configure customtkinter
        self.geometry("1500x750")
        self.appearance_mode = "Dark"                   
        ctk.set_appearance_mode(self.appearance_mode)   # Modes: "System" (standard), "Dark", "Light"
        ctk.set_default_color_theme("blue")             # Themes: "blue" (standard), "green", "dark-blue"

        # setup event root for the widgets - so there will be a single root -> ctk root
        ctk_root = self
        Base_Widget.ctk_root = self


        # create main frames        
        switches_frame = ctk.CTkFrame     (self, height=350)
        diagram_frame  = Diagram_Airfoil  (self, self.curAirfoil, fg_color= cl_background, view_frame=switches_frame)
        edit_frame     = ctk.CTkFrame     (self, height=200)
        file_frame     = Edit_File_Menu   (self, self.curAirfoil)

        # maingrid 2 x 2 - diagram on top, file and edit on bottom
        self.grid_rowconfigure   (0, weight=0)
        self.grid_rowconfigure   (1, weight=1)
        self.grid_columnconfigure(1, weight=1)
        switches_frame.grid  (row=0, column=0, pady=(0,5), padx=(5,3), ipady=5,sticky="news")
        diagram_frame.grid   (row=0, column=1, pady=0, padx=0, sticky="news")
        file_frame.grid      (row=1, column=0, pady=(0,5), padx=(5,3), ipady=5,sticky="news")
        edit_frame.grid      (row=1, column=1, pady=(0,5), padx=(2,5), sticky="nesw")

        # different sections of the edit area 1 x 3 
        edit_frame.grid_rowconfigure      (0, weight=1)
        edit_frame.grid_columnconfigure   (3, weight=1)

        edit_Airfoil_frame    = Edit_Airfoil_Data   (edit_frame, self.curAirfoil)
        edit_Airfoil_frame.grid   (row=0, column=0, pady=5, padx=5, sticky="news")

        edit_Curvature_frame  = Edit_Panels (edit_frame, self.curAirfoil)
        edit_Curvature_frame.grid (row=0, column=1, pady=5, padx=5, sticky="news")

        edit_Curvature_frame  = Edit_Coordinates (edit_frame, self.curAirfoil)
        edit_Curvature_frame.grid (row=0, column=2, pady=5, padx=5, sticky="news")

        edit_Curvature_frame  = Edit_Curvature (edit_frame, self.curAirfoil)
        edit_Curvature_frame.grid (row=0, column=3, pady=5, padx=5, sticky="news")


    def curAirfoil (self) -> Airfoil:
        """ encapsulates current airfoil. Childs should acces only via this function
        to enable a new airfoil to be set """
        return self._curAirfoil

    def set_curAirfoil (self, aNew):
        """ encapsulates current airfoil. Childs should acces only via this function
        to enable a new airfoil to be set """
        self._curAirfoil = aNew
        # mega alarm - inform everything
        fireEvent (AIRFOIL_NEW)

    def add_toAirfoilFiles (self, aPathFileName):
        """ inserts a new airfoilPathFileName to the list right after current airfoil"""

        try: 
            curIndex = self.airfoilFiles.index (self.curAirfoil().pathFileName)
            if not aPathFileName in self.airfoilFiles: 
                self.airfoilFiles.insert (curIndex+1, aPathFileName)
            self.loadNewAirfoil (aPathFileName)
        except: 
            ErrorMsg ("Could not add %s to airfoil list" % aPathFileName )


    #------- file functions ----------------

    def open (self):
        """ open a new wing definition json and load it"""

        filetypes  = [('Airfoil files', '*.dat')]
        newPathFilenames = filedialog.askopenfilenames(
                    title='Select one or more airfoils',
                    initialdir=os.getcwd(),
                    filetypes=filetypes)
        if newPathFilenames:                       # user pressed open
            self.airfoilFiles = newPathFilenames
            self.loadNewAirfoil (self.airfoilFiles[0])

    def save (self):
        """  """
        pass

    def saveAs (self):
        """  """


    def loadNewAirfoil(self, pathFilename, airfoil= None):
        """loads and sets a new wing model returns - updates title """

        if airfoil is None: 
            airfoil = Airfoil(pathFileName=pathFilename)
        airfoil.load()

        self.curAirfoilFile = airfoil.pathFileName
        self.set_curAirfoil (airfoil)

        self.title (AppName + "  v" + str(AppVersion) + "  [" + airfoil.name + "]")


    def onExit(self): 
        """ interception of user closing the app - check for changes made"""
        global ctk_root

        ctk_root = None
        Base_Widget.ctk_root = None
        self.destroy()

#--------------------------------

if __name__ == "__main__":

    # init colorama
    just_fix_windows_console()

    parser = argparse.ArgumentParser(prog=AppName, description='Show and edit (tbd) an airfoil')
    parser.add_argument("airfoil", nargs='*', help="Airfoil .dat file to show")
    args = parser.parse_args()
    if args.airfoil: 
        airfoil_files = args.airfoil[0]
        if not os.path.isfile (airfoil_files):
            ErrorMsg ("Airfoil file '%s' doesn't exist" %airfoil_files )
            sys.exit(1)
    else: 
        NoteMsg ("No airfoil file as argument. Showing example airfoil...")
        airfoil_files = ".\\examples\\vjx.glide\\JX-GP-055.dat"        # test / demo 
        # myFile = ".\\modules\\test_airfoils\\MH32.dat"        # test / demo 

    airfoil_dir =".\\modules\\test_airfoils"
    airfoil_files = [os.path.join(airfoil_dir, f) for f in os.listdir(airfoil_dir) if os.path.isfile(os.path.join(airfoil_dir, f))]
    airfoil_files = sorted (airfoil_files)
    myApp = App(airfoil_files)
    myApp.mainloop()
 