#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Highlevel abstract base classes for UI like Dialog oder EditFrame

"""
import customtkinter as ctk
from widgets            import *
from common_utils       import fromDict, toDict


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
        for widget in self.widgets:
            if isinstance(widget, Base_Widget): widget.refresh()

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
            self.myApp = myApp
        else:
            self.myApp = self.winfo_toplevel()

        self.init()
    
    
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

    # ctk.set_appearance_mode    (Settings().get('appearance_mode', default='System'))   # Modes:  "System" (standard), "Dark", "Light"
    # ctk.set_default_color_theme(Settings().get('color_theme', default='blue'))         # Themes: "blue" (standard), "green", "dark-blue"
    # scaling = Settings().get('widget_scaling', default=1.0)
    # if scaling != 1.0: 
    #     ctk.set_widget_scaling(scaling)  # widget dimensions and text size
    #     NoteMsg ("The App is scaled to %.2f" %scaling)

    def ok (self):
        # to over load and do ok actions
        for widget in self.widgets:
            widget.force_set()
        Settings().write_dataDict (self.settings_dict, dataName='Settings')
        super().ok()                               
