#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Highlevel abstract base classes for UI like Dialog, Edit frame or DIagram frame

"""
import platform
from tkinter import Frame
import customtkinter as ctk
from customtkinter import ScalingTracker
from customtkinter.windows.widgets.scaling import CTkScalingBaseClass

from widgets            import *
from common_utils       import fromDict, toDict, NoteMsg

# Diagram abstract 
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg # use matplotlib together with tkinter
from artist             import Plot_Toolbar


#------- Helper function -----------------------------------------

def set_initialWindowSize (tkwindow : ctk.CTkToplevel,
                           width = None, height = None,  
                           widthFrac = None, heightFrac = None,
                           geometry=None):
    """
    Set size and position of tkinter window in fraction of screensize

    Args:
        tkwindow: tk window to set size 
        width:      width in px  - default 600
        height:     height in px - default 400
        widthFrac:  or width as fraction of screensize
        heightFrac: or height as fraction of screensize
        geometry:   gemetry string like "1551x846+144+67" or "zoomed"
    """

    if geometry is not None and geometry !='zoomed':     # default window size if no settings

        win_scale = ScalingTracker.get_window_scaling(tkwindow)
        if win_scale != 1.0:
            # in case Windows dpi settings are != 100%, ctk would scale geometry by factor win_scale
            #   so the new window would have a new size and pos --> bug
            # so geometry-string is first downscaled and then upscaled to have the original size 
            # tkwindow.geometry(geometry)
            tkwindow.geometry(tkwindow._reverse_geometry_scaling (geometry))

        else:
            tkwindow.geometry(geometry)

    else: 
 
        if widthFrac and heightFrac: 
            width  = int (tkwindow.winfo_screenwidth()  * widthFrac)
            height = int (tkwindow.winfo_screenheight() * heightFrac) 
        
        if width  is None: width  = 600
        if height is None: height = 400

        tkwindow.minsize(int(width*0.9), int(height*0.8))
        tkwindow.geometry("%dx%d" %(width, height))

        x = (tkwindow.winfo_screenwidth()  - width)  // 2
        y = ((tkwindow.winfo_screenheight() - height) // 2 ) // 2.0 # more up (Windows bar)

        tkwindow.geometry("+%d+%d" %(x, y))

        if geometry == 'zoomed':                    # maximize window 
            if platform.system() == 'Windows': 
                # handle ctk bug with zoomed
                # main.state('zoomed')
                tkwindow._state_before_windows_set_titlebar_color = 'zoomed'
            else:                                   # Linux
                tkwindow.attributes('-zoomed', True)


def dummy_window_resize (tkwindow : ctk.CTkToplevel):
    """
    performs a tiny dummy resize of the top level window 
    to handle a matplotlib bug when Windows fontsize <> 100%
    --> the initial plot doesn't handle dpi correctly, a resize is needed
        to adjust the plot to the frame size
    """

    win_scale = ScalingTracker.get_window_scaling(tkwindow)

    # dummy resive only if wqin scale isn't 1.0 
    if win_scale != 1.0:

        geometry = tkwindow.geometry()
        width, height, x, y = CTkScalingBaseClass._parse_geometry_string(geometry)
        width += 1
        geometry = f"{round(width)}x{round(height)}+{x}+{y}"
        tkwindow.geometry(geometry)



def apply_scaling_from_settings ():
    """ reads settings and applies ctk windows scaling """

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


#-------------------------------------------------------------------------------
#  Toolwindow with exec   
#-------------------------------------------------------------------------------



class ToolWindow (ctk.CTkToplevel):
    """ shows a little tool window for a certain duration
    
    Function is executed in a seperate thread to allow long running tasks"""


    def __init__(self, master : ctk.CTkFrame, 
                 message: str,
                 after : int = 0,
                 duration : int = 500, 
                 width: int = 300, height: int = 150):
        """evals functionFn and shows a tool window during excution

        Args:
            master: parent frame 
            functionFn: function to be evluated
            message (str): message text during execution.
            width  (optional): width of tool window. Defaults to 300.
            height (optional): height of tool window. Defaults to 150.
        """
   
        super().__init__(master)

        self._message = message
        self._duration = duration

        bg_color = self._apply_appearance_mode(ctk.ThemeManager.theme["CTkFrame"]["fg_color"])
        fg_color = self._apply_appearance_mode(ctk.ThemeManager.theme["CTkFrame"]["top_fg_color"])
        self.configure (fg_color=fg_color)
        self.configure (bg_color=bg_color)

        set_initialWindowSize (self, width=width, height=height)

        self.overrideredirect(True)                 # make Toolwindow, remove titlebar
        
        # ---------------

        text = self._message() if callable(self._message) else self._message 

        self._msg_widget = ctk.CTkLabel(self,  width=width-40, height=120, text=text, justify="center",
                                          fg_color="transparent", text_color=cl_styles [STYLE_NORMAL]) 
        # self._msg_widget._text_label.configure (wraplength=width *0.8, justify="center")
        self._msg_widget.grid(row=1, column=1, sticky="nwes")
        
        self.grid_columnconfigure(0, weight=0)
        self.grid_columnconfigure(1, weight=2)
        self.grid_columnconfigure(2, weight=0)
        self.grid_rowconfigure(0, weight=2)   
        self.grid_rowconfigure(1, weight=3)   
        self.grid_rowconfigure(2, weight=1)   

        # ---------------

        if after == 0: 
            self._show_it()
        else:  
            self.after (after, self._show_it)


    def _show_it (self):
        """ pop up self for time duration """

        if self.winfo_exists():
            self.grab_set()
        self.attributes('-topmost', 1)
        self.update ()

        if self._duration > 0: 
            self.after (self._duration, self._close)

            # wait until window closed - otherwise parent program would "run away"
            self.master.wait_window(self)


    def _close (self): 
        """close self """
        self.grab_release()
        self.destroy()



class Eval_With_ToolWindow (ctk.CTkToplevel):
    """ evals functionFn and shows a tool window during excution
    
    Function is executed in a seperate thread to allow long running tasks"""


    def __init__(self, master : ctk.CTkFrame, 
                 functionFn,                  
                 message: str, 
                 width: int = 300, height: int = 150):
        """evals functionFn and shows a tool window during excution

        Args:
            master: parent frame 
            functionFn: function to be evluated
            message (str): message text during execution.
            width  (optional): width of tool window. Defaults to 300.
            height (optional): height of tool window. Defaults to 150.
        """
   
        super().__init__(master)

        self._functionFn = functionFn
        self._message = message
        self._thread = None

        bg_color = self._apply_appearance_mode(ctk.ThemeManager.theme["CTkFrame"]["fg_color"])
        fg_color = self._apply_appearance_mode(ctk.ThemeManager.theme["CTkFrame"]["top_fg_color"])
        self.configure (fg_color=fg_color)
        self.configure (bg_color=bg_color)

        set_initialWindowSize (self, width=width, height=height)

        self.overrideredirect(True)                 # make Toolwindow, remove titlebar
        
        # ---------------

        text = self._message() if callable(self._message) else self._message 

        self._msg_widget = ctk.CTkLabel(self,  width=width-40, height=120, text=text, justify="center",
                                          fg_color="transparent", text_color=cl_styles [STYLE_NORMAL]) 
        # self._msg_widget._text_label.configure (wraplength=width *0.8, justify="center")
        self._msg_widget.grid(row=1, column=1, sticky="nwes")
        
        self.grid_columnconfigure(0, weight=0)
        self.grid_columnconfigure(1, weight=2)
        self.grid_columnconfigure(2, weight=0)
        self.grid_rowconfigure(0, weight=2)   
        self.grid_rowconfigure(1, weight=3)   
        self.grid_rowconfigure(2, weight=1)   

        # ---------------

        if self.winfo_exists():
            self.wait_visibility()                      # Linux needs wait ... (thanks Thomas) 
            self.grab_set()
        self.lift()

        # force immediate appearance 
        self.update()

        # start thread and polling 
        # self._run_function ()
        self.after (200, self._run_function)

        # wait until window closed - otherwise parent program would "run away"
        self.master.wait_window(self)



    def _run_function (self):
        """ run the function as a thread ..."""
        import threading

        self._thread = threading.Thread(target=self._functionFn, args=())
        self._thread.start()

        # now poll if thread is still running 
        self.after (300, self._check_running)


    def _check_running (self):
        """ thread still running - if yes - start next poll """
        if self._thread:
            if self._thread.is_alive(): 

                # if message is a function - do live update 
                if callable(self._message):
                    self._msg_widget.configure(text=self._message())
                    self.update()

                # set next poll 
                self.after (300, self._check_running)

            else: 
                self._close()


    def _close (self): 
        """close self """
        self.grab_release()
        self.destroy()


#-------------------------------------------------------------------------------
#  Messagebox   
#-------------------------------------------------------------------------------


class Messagebox(ctk.CTkToplevel):
    """ Message in different styles - inspired vom CTKMessagebox"""

    ICONS = {
            "check": None,
            "cancel": None,
            "info": None,
            "question": None,
            "warning": None
        }

    def __init__(self, master,                  
                 width: int = 400,
                 height: int = 200,
                 title: str = "Messagebox",
                 message: str = "This is a Messagebox!",
                 option_1: str = "Ok",
                 option_2: str = None,
                 option_3: str = None,
                 button_color: str = "default",
                 button_width: int = None,
                 icon: str = "info",             #  "check", "cancel", "info", "question", "warning"
                 font: tuple = None):
        
        super().__init__(master)

        width   = 250 if width  < 250 else width
        height  = 150 if height < 150 else height

        self.after(10)
        set_initialWindowSize (self, width=width, height=height)
        self.transient(master)
        self.resizable(width=False, height=False)
                
        self.title(title)
        self.protocol("WM_DELETE_WINDOW", self.button_event)
        # self.lift()

        
        # ---------------

        self.grid_columnconfigure (0, weight=1)
        self.grid_rowconfigure    (0, weight=1)  

        button_width = button_width if button_width else 80 
        
        if button_color=="default":
            button_color = self._apply_appearance_mode(ctk.ThemeManager.theme["CTkButton"]["fg_color"])
            # second and third button in a darker color 
            button2_color = self._apply_appearance_mode(ctk.ThemeManager.theme["CTkOptionMenu"]["button_color"])
        else:
            button_color = button_color
            button2_color = button_color
                    
        icon_img = self.load_icon(icon, (25,25)) if icon else None                    

        # ---------------

        bg_color = self._apply_appearance_mode(ctk.ThemeManager.theme["CTkFrame"]["fg_color"])
        fg_color = self._apply_appearance_mode(ctk.ThemeManager.theme["CTkFrame"]["top_fg_color"])

        frame_mid = ctk.CTkFrame(self, bg_color=bg_color, fg_color=fg_color)
        frame_mid.grid(row=0, column=0, columnspan=3, sticky="nwes", padx=(0,0), pady=(0,0))
        frame_mid.grid_rowconfigure   (0, weight=1)
        frame_mid.grid_columnconfigure(0, weight=1)
        frame_mid.grid_columnconfigure(1, weight=6)
        frame_mid.grid_columnconfigure(2, weight=1)

        icon_widget = ctk.CTkButton(frame_mid,  width=1, height=100, corner_radius=0, text=None, font=font,
                                            fg_color="transparent", hover=False,  image=icon_img)
        msg_widget  = ctk.CTkButton(frame_mid,  width=1, height=100, corner_radius=0, text=message, font=font,
                                            fg_color="transparent", text_color=cl_styles [STYLE_NORMAL], hover=False,  image=None)
        msg_widget._text_label.configure(wraplength=width *0.8, justify="center")
    
        icon_widget.grid(row=0, column=0, columnspan=1, sticky="nes")
        msg_widget.grid (row=0, column=1, columnspan=2, sticky="nwes")
        
        # ---------------

        if option_1:
            
            frame_bot = ctk.CTkFrame(self, fg_color="transparent")
            frame_bot.grid(row=1, column=0, columnspan=3, sticky="nwes", padx=(0,0))
            frame_bot.grid_columnconfigure((0,4), weight=1)

            button1_widget = ctk.CTkButton(frame_bot, text=option_1, fg_color=button_color,
                                                    width=button_width, height=25, font=font, 
                                                    command=lambda: self.button_event(option_1))
            button1_widget.grid(row=0, column=1, sticky="e", padx=(30,10), pady=10)

            if option_2:
                self.button_2 = ctk.CTkButton(frame_bot, text=option_2, fg_color=button2_color,
                                                        width=button_width, height=25, font=font, 
                                                        command=lambda: self.button_event(option_2))
                self.button_2.grid(row=0, column=2, sticky="e", padx=10, pady=10)
                
            if option_3:
                self.button_3 = ctk.CTkButton(frame_bot, text=option_3, fg_color=button2_color,
                                                        width=button_width, height=25, font=font, 
                                                        command=lambda: self.button_event(option_3))
                self.button_3.grid(row=0, column=3, sticky="e", padx=(10,0), pady=10)

        # --------------

        if self.winfo_exists():
            self.grab_set()


    def load_icon(self, icon, icon_size):
            if icon not in self.ICONS or self.ICONS[icon] is None:
                image_path = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'icons', icon + '.png')
                if not icon_size: icon_size = (25,25) 
                self.ICONS[icon] = ctk.CTkImage(Image.open(image_path), size=icon_size)
            return self.ICONS[icon]         


    def get(self):
        self.master.wait_window(self)
        return self.event

    def show (self):
        """ pops up self it is not used with get (wait_window) - returns self """

        # #todo - not the real solution ... Title bar isn't painted ...
        self.wm_overrideredirect(True)  
        self.deiconify()
        self.after(100)
        self.update_idletasks()
        self.after(100)
        return self

    def closeIt (self): 
        """close self from outside """
        self.grab_release()
        self.destroy()

    def button_event(self, event=None):
        self.grab_release()
        self.destroy()
        self.event = event


#-------------------------------------------------------------------------------
#  Higher level app / control base classes    
#-------------------------------------------------------------------------------


class Dialog_Abstract (ctk.CTkToplevel):
    """ 
    Abstract superclass for windows dialogs 

    self.return_OK is True if user conformed 'ok' 
    """
    name       = "My little title"
    widthFrac  = 0.75
    heightFrac = 0.70

    def __init__(self, master, myApp=None, workingDir=None, title=None, *args, **kwargs):
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

        if myApp:
            self._myApp = myApp
        else:
            self._myApp = self.winfo_toplevel()

        # Init UI, widgets, grid

        self.init()

        # Focus on self, grap all user input

        self.wait_visibility()                              # Linux needs window to grap !
        self.grab_set()
        # self.resizable(False, False)                        
        self.focus_set()
        self.protocol("WM_DELETE_WINDOW", self.destroy)


    @property
    def myApp (self):
        """ the top level app self belongs to -can be overloaded for type info"""
        return self._myApp


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


    @property
    def myApp (self):
        """ the top level app self belongs to -can be overloaded for type info"""
        return self.winfo_toplevel()

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
            self.after_idle (self.setActive, True)


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
        self.toolbar.grid (row=r, column=0, columnspan= 3, sticky='ew', padx=(10,10), pady=(0,0))

        # show mouse coordinates 
        r += 1
        coords = ctk.CTkLabel (self.view_frame, textvariable=self.toolbar.mouse_coords, height=16,
                               text_color=cl_styles [STYLE_DISABLED])
        coords.grid (row=r, column=0, columnspan= 3, sticky='w', padx=(10,10), pady=(2,4))


    def create_axes (self):
        """ setup axes, axis for this plot type """
        self.ax : plt.Axes = self.figure.add_subplot()        # the pyplot axes this diagram is plotted
        self.figure.subplots_adjust(left=0.04, bottom=0.06, right=0.98, top=0.97, wspace=None, hspace=None)
        self.ax.tick_params (labelsize='small')


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
    widthFrac  = 0.25
    heightFrac = 0.40

    def __init__(self, master,  name= None, *args, **kwargs):

        title = self.name + "  [" + (name if name else master.name) + "]"

        super().__init__(master, *args, title=title, **kwargs)

    def init (self):

        self.settings_dict = Settings().get_dataDict()

        # Header 
        c = 0 
        r = 0 
        
        Label_Widget  (self.edit_frame,r, c, padx= 30, pady=(20,15), sticky = 'nw',
                        lab= "The following options will be applied with the next restart")

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

        r += 1
        self.add (Field_Widget  (self.edit_frame,r,c, lab="Plot font size", lab_width=100, width=80, padx= 50, pady=5,
                                 spin=True, dec=0, lim=(8,15), step=1, 
                                 obj=self, get='plot_font_size', set="set_plot_font_size"))

        # allow additional setitngs in sub class
        r = self._add_settings (r+1)

        # close  
        self.edit_frame.grid_rowconfigure (r, weight=1)
        r += 1
        Button_Widget (self.edit_frame,r,c, lab='Close', set=self.ok, 
                       columnspan=2, width=100, sticky="w", pady=20, padx=(200,0))


    def _add_settings (self, r):
        """ add additional settings """
        # to be overloaded
        return r


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

    @property
    def plot_font_size (self):
        return fromDict (self.settings_dict, 'plot_font_size', default=10, msg=False)
    def set_plot_font_size (self, aVal):
        if aVal is None: aVal = 10
        toDict(self.settings_dict, 'plot_font_size', aVal)        
        self.refresh()

    def ok (self):
        # to over load and do ok actions

        self.force_set()

        Settings().write_dataDict (self.settings_dict, dataName='Settings')

        super().ok()                               
