#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Highlevel abstract base classes for UI like Dialog, Edit frame or DIagram frame

"""
from tkinter import Frame
import customtkinter as ctk
from widgets            import *
from common_utils       import fromDict, toDict

# Diagram abstract 
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg # use matplotlib together with tkinter
from artist             import Plot_Toolbar



#------------------------------------------------

def set_initialWindowSize (tkwindow, widthFrac=0.9, heightFrac=0.8):
    """ set size and position of tkinter window in fraction of screensize"""

    width  = int (tkwindow.winfo_screenwidth()  * widthFrac)
    height = int (tkwindow.winfo_screenheight() * heightFrac) 

    tkwindow.minsize(int(width*0.9), int(height*0.8))
    tkwindow.geometry("%dx%d" %(width, height))

    x = tkwindow.winfo_screenwidth()  // 2 - width // 2
    y = (tkwindow.winfo_screenheight() // 2 - height // 2) // 2

    tkwindow.geometry("+%d+%d" %(x, y))


class Dialog_Abstract (ctk.CTkToplevel):
    """ 
    Abstract superclass for windows dialogs 

    self.return_OK is True if user conformed 'ok' 
    """
    name       = "My little title"
    widthFrac  = 0.75
    heightFrac = 0.70

    def __init__(self, master, workingDir=None, title=None, *args, **kwargs):
        super().__init__(master, *args, **kwargs)

        self.transient(master)

        # bug fix titlebar color https://github.com/TomSchimansky/CustomTkinter/issues/1930
        self.after(10, lambda: self._windows_set_titlebar_color(self._get_appearance_mode()))
        set_initialWindowSize (self, widthFrac=self.widthFrac, heightFrac=self.heightFrac)

        self.title (title if title else self.name)

        # master edit frame for dialog 

        self.edit_frame    = ctk.CTkFrame (self)            
        self.grid_rowconfigure    (0, weight=1)
        self.grid_columnconfigure (0, weight=1)
        self.edit_frame.grid    (row=0, column=0, pady=5, padx=5, sticky="nesw")

        self.widgets = []                                   # for refresh logic  
        self.return_OK = False                              # the attribute for return ok
        self.workingDir = workingDir                        # the default directory for file activities
        self.ctk_root = self.winfo_toplevel()               # root for change events

        # Init UI, widgets, grid

        self.init()

        # Focus on self, grap all user input

        self.wait_visibility()                              # Linux needs window to grap !
        self.grab_set()
        # self.resizable(False, False)                        
        self.focus_set()
        self.protocol("WM_DELETE_WINDOW", self.destroy)


    def init (self):
        """ init UI, widgets, grid - to be overloaded"""
        pass

    

    def ok (self):
        # to over load and do ok actions
        self.return_OK = True
        self.editFrame = None                                
        self.widgets = []                                   # ensure no widgets bound anymore 
        self.destroy()

    def cancel (self): 
        # to over load and do cancel actions
        self.editFrame = None                                
        self.widgets = []                                   # ensure no widgets bound anymore 
        self.destroy()

    def add (self, aWidget): 
        self.widgets.append (aWidget)

    def refresh(self, *_):
        """ refresh (reread) all widgets """
        for widget in self.widgets:
            if isinstance(widget, Base_Widget): widget.refresh()

    def force_set (self):
        """ all widgets write back their value - e.g. on Ok to ensure to have actual data"""
        for widget in self.widgets:
            if isinstance(widget, Base_Widget): widget.force_set()

    def leftTopPosition(self, width=None, height=None):
        """ get center of a tkinter window
        """
        # self.update_idletasks()

        if width is None:  width  = self.winfo_width()
        if height is None: height = self.winfo_height()

        x = self.winfo_screenwidth() // 2 - width // 2
        y = self.winfo_screenheight() // 2 - int (height / 1.7)
        return x, y



#-------------------------------------------


class Edit_Abstract (ctk.CTkFrame):
    """ 
    Abstract superclass for all the edit like frames
    """

    def __init__(self, master, *args, myApp=None, **kwargs):
        super().__init__(master, *args, **kwargs)

        self.widgets = []

        # root for change events (widgets will have the same toplevel as root)
        self.ctk_root = self.winfo_toplevel()
        if myApp:
            self._myApp = myApp
        else:
            self._myApp = self.winfo_toplevel()

        self.init()

    @property
    def myApp (self):
        """ the top level app self belongs to -can be overloaded for type info"""
        return self._myApp

    
    @property
    def workingDir (self): 
        """ the current (default) working for file saves etc. """
        return self.myApp.workingDir


    def init(self):
        # main method - to be overloaded by sub class
        pass

    def add (self, aWidget): 
        # kepp track of the widgets of self to be able to refresh them
        self.widgets.append (aWidget)

    def refresh(self, *_):
        # refresh typically based on changed events 
        for widget in self.widgets:
            if isinstance(widget, Base_Widget): widget.refresh()
            # print ("  - refresh in ", self.__class__.__name__," for %s widgets" % len(self.widgets))



#-------------------------------------------------------------------------------
# Diagram (frame) of an app   
#-------------------------------------------------------------------------------


class Diagram_Abstract(ctk.CTkFrame):
    """ 
    Abstract super class of the specific plot frames like "Plot_Planform"
    """

    name = "This is the diagram super class"

    cl_background  = ('#EBEBEB','#242424')                     # background color in diagrams


    def __init__(self, master, objectFn, *args, view_frame = None, setActive=True, size= None,  **kwargs):
        """Diagramm (frame) - can have different plot and a view frame for diagram switches  

        Args:
            master: parent frame
            objectFn: the model object (or list) self is working on - as method
            view_frame: view frame for the switches of seld .
            setActive: show diagram after init. Defaults to False.
            size: size in inch as tuple . Defaults to None.
        """
        super().__init__( master, *args, **kwargs)

        self._active    = setActive                             # is active frame? control change events 
        self._objectFn  = objectFn                              # main data object 

        self.view_frame : Frame = view_frame

        self.ctk_root = self.winfo_toplevel()                    # root for change events 

        self.configure(fg_color= self.cl_background)

        # big diagram grid 
        self.grid_columnconfigure(0, weight=1)
        self.grid_rowconfigure(0, weight=1)
        self.grid(row=0, column=0, sticky="news")

        # setup canvas for plt
        
        if size:    self.figure : plt.Figure = plt.Figure(figsize=size)
        else:   	self.figure : plt.Figure = plt.Figure(figsize= [20, 8]) 

        self.canvas = FigureCanvasTkAgg(self.figure, self)              # connect tk and pyplot    
        self.canvas._tkcanvas.configure(background= self.cl_background[1])   # take background of dark mode 
        self.canvas._tkcanvas.grid (row=0, column=0, pady=0, padx=0, sticky="news")

        # first setup view area with switches
        
        self.setup_view_frame ()

        # delayed init and show plot axes when all tkinter grid sizing work is done

        if size:                                        # these are mini diagrams --> faster
            self.after (100, self.init_and_show_plot)
        else:                                           # the big app diagrams
            self.after (500, self.init_and_show_plot)

        return


    # -----  data objects

    def mainObject(self):
        " main data object of the diagram"

        # should be encapsulated in sub class 
        return self._objectFn()


    def mainObjects(self):
        """ list with main dataObjects self is working on """

        if callable(self._objectFn):
            obj_or_list = self._objectFn()
        else: 
            obj_or_list = self._objectFn

        if isinstance(obj_or_list, list): 
            return obj_or_list
        else: 
            return [obj_or_list]
        

    # ----- setup - partly to overlaod

    def init_and_show_plot (self):
        """ create and init axes, create artists to plot"""

        # common axes for this diagram
        self.create_axes()
        self.setup_axes ()

        # Create the artists for the diagramm
        self.setup_artists ()

        # make ready for change events 
        self.setChangeBindings ()

        # and finally show it if self is the first to show 
        if self._active: 
            self.after_idle (self.refresh)


    def setup_view_frame (self):
        """ create view area with switches etc."""

        if not self.view_frame: return

        r,c = 0,0
        self.view_frame.grid_columnconfigure(0, weight=0)   # to center switches
        self.view_frame.grid_columnconfigure(2, weight=2)

        Header_Widget (self.view_frame,r,0, columnspan=3, lab='View')

        r,c = self.setup_switches (r, c)                       

        self.setup_toolbar (r)


    def setup_switches(self, r=0, c=0):
        """ to overload - define on/off switches for this plot type"""
        return r, c 


    def setup_toolbar (self, r):
        """ toolbar for pan and zoom"""
        r +=1
        Blank_Widget (self.view_frame,r,0)
        self.view_frame.grid_rowconfigure(r, weight=1)
        r +=1
        Label_Widget (self.view_frame, r, 0, lab='Pan and zoom')
        r +=1
        self.toolbar = Plot_Toolbar(self.canvas, self.view_frame, background=ctk.get_appearance_mode())
        self.toolbar.grid (row=r, column=0, columnspan= 3, sticky='ew', padx=(10,10), pady=(0,10))


    def create_axes (self):
        """ setup axes, axis for this plot type """
        self.ax : plt.Axes = self.figure.add_subplot()        # the pyplot axes this diagram is plotted
        self.figure.subplots_adjust(left=0.04, bottom=0.08, right=0.98, top=0.96, wspace=None, hspace=None)


    def setup_axes(self):
        """ to overload - setup axes, axis for this plot type """
        pass


    def setup_artists(self):
        """ setup artists for this plot type """
        # overwrite in sub class
        pass  


    def refresh(self, dummy): 
        """ refresh all artists"""
        # overwrite in sub class
        if self._active:
            pass  

    # ----- general refresh when getting active view again

    def setActive(self, active: bool):
        """ the Diagram master (Tabview) will activate/deactivate to avoid plot generation
            if self is not visible """
        if active: 
            self._active = True
            self.refresh()
        else: 
            self._active = False

    # ----- change event setup 

    def setChangeBindings (self):
        """ bind self to change events from outside"""
        # overwrite in sub class
        pass      




#------------------------------------------------
# Common apps 
#------------------------------------------------

from  common_utils import Settings

class Dialog_Settings (Dialog_Abstract):
    """ 
    Dialog to edit app settings
    """
    name       = "Edit settings"
    widthFrac  = 0.30
    heightFrac = 0.40

    def __init__(self, master,  name= None, *args, **kwargs):

        title = self.name + "  [" + (name if name else master.name) + "]"

        super().__init__(master, *args, title=title, **kwargs)

    def init (self):

        self.settings_dict = Settings().get_dataDict()

        # Header 
        c = 0 
        r = 0 
        # Header_Widget (self.header_frame,r,c, pady=0, lab= "Normalize Airfoil", sticky = 'nw', width=100)
        
        Label_Widget  (self.edit_frame,r, c, padx= 30, pady=(20,15), sticky = 'nw',
                        lab= "The following will be applied with the next restart")

        # Settings 
        r += 1
        self.add (Combo_Widget  (self.edit_frame,r,c, lab="Appearance mode", lab_width=100, width=100, padx= 50, pady=5,
                                 obj=self, get='appearance_mode', set="set_appearance_mode",
                                 options=self.appearance_modes()))
        r += 1
        self.add (Field_Widget  (self.edit_frame,r,c, lab="Scaling of font size", lab_width=100, width=80, padx= 50, pady=5,
                                 obj=self, get='widget_scaling', set="set_widget_scaling", dec=2))

        r += 1
        self.add (Field_Widget  (self.edit_frame,r,c, lab="Scaling of App size", lab_width=100, width=80, padx= 50, pady=5,
                                 obj=self, get='window_scaling', set="set_window_scaling", dec=2))

        # close  
        r += 1
        self.edit_frame.grid_rowconfigure (r, weight=1)
        r += 1
        Button_Widget (self.edit_frame,r,c, lab='Close', set=self.ok, 
                       columnspan=2, width=100, sticky="w", pady=20, padx=(200,0))


    @property
    def appearance_mode (self):
        return fromDict (self.settings_dict, 'appearance_mode', default='System', msg=False)
    def set_appearance_mode (self, aMode):
        if aMode in self.appearance_modes (): 
            toDict(self.settings_dict, 'appearance_mode', aMode)
        self.refresh()
    def appearance_modes (self):
        return ["System", "Dark", "Light"]
    

    @property
    def widget_scaling (self):
        return fromDict (self.settings_dict, 'widget_scaling', default=1.0, msg=False)
    def set_widget_scaling (self, aVal):
        if not aVal: aVal = 1.0
        aVal = max (0.49, aVal)
        aVal = min (1.51, aVal)
        toDict(self.settings_dict, 'widget_scaling', aVal)
        self.refresh()

    @property
    def window_scaling (self):
        return fromDict (self.settings_dict, 'window_scaling', default=1.0, msg=False)
    def set_window_scaling (self, aVal):
        if not aVal: aVal = 1.0
        aVal = max (0.49, aVal)
        aVal = min (1.51, aVal)
        toDict(self.settings_dict, 'window_scaling', aVal)        
        self.refresh()


    def ok (self):
        # to over load and do ok actions

        self.force_set()

        Settings().write_dataDict (self.settings_dict, dataName='Settings')

        super().ok()                               
