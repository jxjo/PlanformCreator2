#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""  

The "Artists" to plot a wing object on a matplotlib axes


"""
from common_utils import *
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as mc
from matplotlib.collections import PathCollection
from matplotlib.backend_bases import MouseEvent
from matplotlib.backends.backend_tkagg import NavigationToolbar2Tk 

from cycler import cycler  
import colorsys

import tkinter as tk

cl_background       = '#101010'
cl_labelGrid        = '#B0B0B0'    
cl_axes             = '#606060'
cl_text             = '#D0D0D0'
cl_textHeader       = '#808080'
cl_userHint         = '#E0A721'
cl_toolbar          = ('gray85', 'gray35')


# -------- common methodes ------------------------

# helper functions to position values and text 

def print_number (ax : plt.Axes, val, decimals, xy, xytext, color, alpha=0.8, asPercent=False):
    """ print a formatted numer at axes x,y with pixel offset xytext"""

    if asPercent: 
        text = f"{val:.{decimals}%}"  
    else: 
        text = f"{val:.{decimals}f}"  

    p = ax.annotate(text, xy=xy, xytext=xytext, va='top', ha='right',
                            xycoords='axes fraction', textcoords='offset points', fontsize='small',
                            color = color, alpha=alpha)
    return p


def print_text  (ax : plt.Axes , text, ha, xy, xytext, color, alpha=1.0, xycoords='data'):
    """ print a text at axes x,y with pixel offset xytext
        
    xycoords: 'data' (default), 'axes fraction', ... 
        """
    p = ax.annotate(text, xy=xy, xytext=xytext, va='top', ha=ha,
                            xycoords=xycoords, textcoords='offset points', fontsize='small',
                            color = color, alpha=alpha)
    return p


# -------- matplotlib defaults ------------------------

# plt.rcParams.update({'figure.dpi': 180})
plt.rcParams.update({'font.size': 10})                       # default font.size: 10.0
plt.rcParams.update({'font.weight': 'light'})   
plt.rcParams.update({'text.color': cl_text})   

plt.rcParams.update({'figure.facecolor': cl_background})         
plt.rcParams.update({'axes.facecolor': cl_background})      
plt.rcParams.update({'axes.edgecolor': cl_axes})             # axes spines color        
plt.rcParams.update({'xtick.color': cl_labelGrid})   
plt.rcParams.update({'ytick.color': cl_labelGrid})   
plt.rcParams.update({'axes.labelcolor':  cl_labelGrid})  
plt.rcParams.update({'axes.spines.left': True})   
plt.rcParams.update({'axes.spines.bottom': True})   
plt.rcParams.update({'axes.spines.top': True})   
plt.rcParams.update({'axes.spines.right': True})   
plt.rcParams.update({'lines.linewidth': 1.0})               # line width in points

plt.rcParams.update({'legend.fontsize': 'small'})          # fontsiize of legend 

plt.rcParams.update({'axes.grid': False})                   # display grid or not    
plt.rcParams.update({'grid.linewidth': 0.8})                # in points         
plt.rcParams.update({'grid.color': cl_labelGrid})           
plt.rcParams.update({'grid.alpha': 0.2})                    # transparency, between 0.0 and 1.0  


def set_font_size (aSize): 
    """ set the default plot font size"""
    aSize = min (15, aSize) 
    aSize = max ( 8, aSize)
    plt.rcParams.update({'font.size': aSize})


def get_font_size ():
    """ returns the default plot font zize"""
    return plt.rcParams['font.size']


def adjust_lightness(color, amount=1.0):
    """
    Lightens the given color by multiplying by the given amount.
    Input can be matplotlib color string, hex string, or RGB tuple.

    Examples:
    >> lighten_color('g', 0.3)
    >> lighten_color('#F034A3', 0.6)
    >> lighten_color((.3,.55,.1), 0.5)
    """    
    try:
        c = mc.cnames[color]
    except:
        c = color
    c = colorsys.rgb_to_hls(*mc.to_rgb(c))
    return colorsys.hls_to_rgb(c[0], max(0, min(1, amount * c[1])), c[2])



# ---------------------------------------------------------------------------


class Artist():
    """
        Abstract class: The "Artist" to plot a data object on a matplotlib axes

        - these are not MatplotLib artists  

    """

    name = "Abstract Artist" 

    def __init__ (self, axes, modelFn, norm = False, onPick=None, onMove=None,
                  show=False, showMarker=True):
        """
        The Artist to plot a data object on a matplotlib axes
        - these are not MatplotLib artists  

        Args:
            axes: the axes to plot on to 
            modelFn: the object the artist should plot, either bound method or list  
            norm: show in normed 0..1 scale 
            onPick: call back command when line was picked by user - will activate picking. 
            onMove: call back command when point was moved by user 
            show: will show self immadiatly 
            showMarker: show marker info 
        """
        self.ax : plt.Axes = axes
        self._modelFn = modelFn             # we get a bounded method to the model e.g. Wing 

        self._norm = norm                   # plot in normed coordinates
        self._show = show                   # should self be plotted? 
        self.showMarker = showMarker
        self._showLegend = True             # show legend of labels are available
        self._myPlots = []                  # plots (line artists) made up to now 
        self._dragManagers  = []            # DragManagers which are instanciated by self
        self._mouseActive  = False
        self._moveCallback = None
        self._pickActive   = False
        self._pickCallback = None
    
        if onPick:
            self._pickActive = True
            self._mouseActive = True
            self._pickCallback = onPick

        if onMove:
            self._mouseActive = True
            self._moveCallback = onMove

        self._ciddraw      = None           # callback id draw event 
        self._cidpick      = None           # callback id pick event 
        self._curLineLabel = None

        self._xticks = []                   # the axis ticks self added
        self._yticks = []

    # ------- public ----------------------

    @property
    def model (self): 
        """access main data object e.g. wing"""
        # to do - handle many airfoils 
        if isinstance(self._modelFn, list): 
            objectList = []
            for objectFn in self._modelFn:
                if callable(objectFn):
                    modelObject = objectFn()   # objectFn is a bound method to the object
                else:
                    modelObject = objectFn
                objectList.append (modelObject)
            return objectList
        else: 
            if callable(self._modelFn):
                return self._modelFn()          # modelFn is a bound method to the object
            else:
                return self._modelFn
    
    @property
    def show (self): return self._show

    def set_show (self, aBool):
        """ user switch to disable ploting the data
        """
        if (self._show != aBool):               # only when changed do something
            if not aBool:                       # was showed and switched off
                self._deleteMyPlots()           # remove current plot elements
                self._plotLegend()
                self.ax.figure.canvas.draw_idle()
            self._show = aBool
            if aBool:
                self.plot(figureUpdate=True)    # enforce redraw


    def plot (self, figureUpdate=False):
        """the artist will plot its data - redraw canvas only if FigureUpdate=True!
        """
        if self.show:                           # view is switched on by user? 
            self._deleteMyPlots()               # remove current plot elements
            self._plot()                        # repaint everything 
            self._plotLegend()
            if figureUpdate:                    
                self.ax.figure.canvas.draw_idle()    # draw ony if Windows is idle!


    def refresh(self, figureUpdate=False):
        self.plot (figureUpdate = figureUpdate)

    @property
    def norm (self): return self._norm
    """ plot with normed coordinates 0..1"""

    @property
    def abs (self): return not self._norm
    """ plot with absolute coordinates - the opposite of norm """

    def set_norm (self, aBool):
        if (self._norm != aBool):               # only when changed do something
            self._norm = aBool
            self._deleteMyPlots()               # first delete current plots
            self.ax.relim()                     # make sure all the data fits
            self.ax.autoscale()                 # auto-scale on 
            self.plot(figureUpdate=True)        # enforce redraw

    def set_abs (self, aBool):
        self.set_norm (not aBool)


    @property
    def mouseActive (self): return self._mouseActive
    def set_mouseActive (self, aBool): 
        self._mouseActive = aBool
        self.plot(figureUpdate=True)        # enforce redraw

    @property
    def curLineLabel (self): return self._curLineLabel
    """ Label of current line object"""

    @property
    def showLegend (self): return self._showLegend
    def set_showLegend (self, mode):
        """ 
        switch display of axes legend. Different modes are possible 

        mode =  True        matplotlib legend
                False       no legend
               'extended'   an extended legend is created by the artist 
               'normal'     a mini legend version is created by the artist
        """
        self._showLegend = mode

    # --------------  private -------------

    def _plot (self):
        # do plot - overwritten in sublass
        pass

    def _deleteMyPlots(self):
        """ remove all artists and artifacts of self to reset an artist"""

        # remove matplotlib artists 
        for p in self._myPlots:
            try:
                p.remove()
            except:
                print (" -!- ups - artist %s not found" %p)
        self._myPlots = []
    
        self._remove_myticks ()                         # remove ticks self added to axis
        self._disconnect()                              # remove event connections


    def _disconnect (self):
        """ remove all registered event callbacks of self """

        if not self._cidpick is None:  self.ax.figure.canvas.mpl_disconnect(self._cidpick)
        if not self._ciddraw is None:  self.ax.figure.canvas.mpl_disconnect(self._cidpick)

        # remove DragManagers event callbacks 
        dm : DragManager
        for dm in self._dragManagers:
            dm._disconnect()
        self._dragManagers = []


    def _connectDrawEvent (self):
        """ connect matplotlib 'event' with function"""
        self._ciddraw = self.ax.figure.canvas.mpl_connect('draw_event', self._on_draw)

    def _connectPickEvent (self):
        """ installs and connects callback for pick event """ 
        self._cidpick = self.ax.figure.canvas.mpl_connect('pick_event', self._on_pick)     


    def draw_animated_artists (self): 
        """draw all artists of self which are animated"""
        for art in self._myPlots:
            if art.get_animated():
                self.ax.draw_artist (art)



    def _add(self, aPlot):
        """ add matplotlib artist to artists of self"""

        art = None 

        if isinstance (aPlot, PathCollection):      # scatter returns PathCollection 
            art = aPlot
        elif isinstance (aPlot, list):              # .plot returns list - take first element
            art = aPlot [0]
        else:
            art = aPlot
        
        if not art is None: self._myPlots.append(art)

        return art



    def _plotLegend(self):
        """ shows the legend """

        if self.showLegend == True:

            # show the original matplotlib legend

            h, l = self.ax.get_legend_handles_labels()          # are there any lines with labels
            if h: 
                leg = self.ax.legend(h, l, labelcolor=cl_labelGrid)
            else: 
                leg = self.ax.legend([], [])                    # remove legend 
            leg.set_zorder(2)                                   
            leg.get_frame().set_linewidth(0.0)                  # no frame 


    def _plot_title (self, title: str, va='top', ha='left', wspace=0.08, hspace=0.1 ): 
        """ plot a title in self axes
        Args:
            title: title text 
            va: vertical alignment 'top', 'bottom', 'center'
            ha: horizontal alignment  'left' or 'right', 'center'
            wspace: width  space 
            hspace: height space        
        """

        if va == 'top':
            y = 1.0 - hspace
        elif va == 'center':
            y = 0.5
        else:
            y = hspace 

        if ha == 'left':
            x = wspace
        elif ha == 'center':
            x = 0.5 - wspace
        else:
            x = 1.0 - wspace 

        p = self.ax.text (x, y, title, color=cl_text , backgroundcolor= cl_background,
                          fontsize = 'large', transform=self.ax.transAxes, 
                          horizontalalignment=ha, verticalalignment=va)
        self._add (p)   


    def _makeObjectPickable (self, aObject): 
        """ make aLine or point clickable on axes"""

        if isinstance (aObject, list):
            obj = aObject[0]                    # could be matplotlib list 
        else:
            obj = aObject
        try: 
            obj.set_picker (True)
            obj.set_pickradius(5)
        except: 
            pass


    def _on_pick (self, event):
        # callback of matplt - having matplt 'event' as argument
        try: 
            myLabel = event.artist.get_label()
        except: 
            print ("- Pick event couldn't be handled ", event.artist, "callback: ", self._pickCallback)

        # now callback parent with myLabel as argument
        if self._pickCallback:
            # remove '_' which was used to suppress artist to appear in legend 
            if myLabel[0] == '_':
                myLabel = myLabel[1:]
            self._pickCallback(myLabel)


    def _on_draw (self, event): 
        """ call back of draw event when self will be drawn"""

        canvas = self.ax.figure.canvas
        if event is not None:
            if event.canvas != canvas: raise RuntimeError

        # get the current (empty) background of axes with the points
        background = canvas.copy_from_bbox(self.ax.bbox)

        # provide the dragManagers with an empty background image 
        dragMan: DragManager
        for dragMan in self._dragManagers:
            dragMan.set_background(background)
 
        self.draw_animated_artists ()


    def _set_colorcycle (self, nColors = 10, colormap='Set2'):
        """ set the color cycle of self axes to have a nice auto colors when plotting
        nColors define the length of the cycle (until it starts from the beginning)"""

        # https://matplotlib.org/stable/gallery/color/colormap_reference.html
        new_colors = plt.get_cmap(colormap)(np.linspace(0, 1, nColors))
        color_cycler = cycler('color', new_colors)  
        self.ax.set_prop_cycle(color_cycler)

    def _get_color (self, anArtist):
        """ get color of artist anArtist"""
        if isinstance (anArtist, list):            # .plot returns list 
            art = anArtist [0]
        else:
            art = anArtist
        return art.get_color()

    def _cycle_color (self):
        """ move cycler to next color and return this color """
        # https://stackoverflow.com/questions/37890412/increment-matplotlib-color-cycle
        p = self.ax.plot([], [])
        return self._get_color(p)

    
    def _add_xticks (self, ticks):
        """ add ticks list to the axis"""
        self._xticks = np.append(self._xticks,ticks)
        self.ax.set_xticks (np.append(self.ax.get_xticks(), ticks))
    
    def _add_yticks (self, ticks):
        """ add ticks list to the axis"""
        self._yticks = np.append(self._yticks,ticks)
        self.ax.set_yticks (np.append(self.ax.get_yticks(), ticks))

    def _remove_myticks (self):
        """ remove all axis ticks on x and y self had added"""

        if len(self._xticks): 
            ticks = self.ax.get_xticks()
            self.ax.set_xticks(np.setdiff1d(ticks, self._xticks))
            self._xticks= []

        if len(self._yticks): 
            ticks = self.ax.get_yticks()
            self.ax.set_yticks(np.setdiff1d(ticks, self._yticks))
            self._yticks= []


# ----------------------------------------------------------

#    Helper functions 



def autoscale_y(ax : plt.Axes,margin=(0.1, 0.1)):
    """
    This function rescales the y-axis based on the data that is visible given the current xlim of the axis.
    ax -- a matplotlib axes object
    margin -- the fraction of the total height of the y-data to pad the upper and lower ylims
    """

    def get_bottom_top(line):
        xd = line.get_xdata()
        yd = line.get_ydata()
        lo,hi = ax.get_xlim()
        y_displayed = yd[((xd>lo) & (xd<hi))]
        h = np.max(y_displayed) - np.min(y_displayed)
        bot = np.min(y_displayed)-margin*h
        top = np.max(y_displayed)+margin*h
        return bot,top

    lines = ax.get_lines()
    bot,top = np.inf, -np.inf

    for line in lines:
        new_bot, new_top = get_bottom_top(line)
        if new_bot < bot: bot = new_bot
        if new_top > top: top = new_top

    ax.set_ylim(bot,top)




# ----------------------------------------------------------


class Plot_Toolbar(NavigationToolbar2Tk):
    """Base class of all artists"""

    from matplotlib import backend_bases

    # list of toolitems to add to the toolbar, format is:
    # (
    #   text, # the text of the button (often not visible to users)
    #   tooltip_text, # the tooltip shown on hover (where possible)
    #   image_file, # name of the image for the button (without the extension)
    #   name_of_method, # name of the method in NavigationToolbar2 to call
    # )
    toolitems = (
        ('Home', 'Reset original view', 'home', 'home'),
        ('Back', 'Back to previous view', 'back', 'back'),
    #    ('Forward', 'Forward to next view', 'forward', 'forward'),
    #    (None, None, None, None),
        ('Pan',
         'Left button pans, Right button zooms\n'
         'x/y fixes axis, CTRL fixes aspect',
         'move', 'pan'),
        ('Zoom', 'Zoom to rectangle\nx/y fixes axis', 'zoom_to_rect', 'zoom'),
    #    ('Subplots', 'Configure subplots', 'subplots', 'configure_subplots'),
    #    (None, None, None, None),
    #    ('Save', 'Save the figure', 'filesave', 'save_figure'),
      )
    
    backend_bases.NavigationToolbar2.toolitems = toolitems

    def __init__ (self, canvas, view_frame, background="Dark"):
        super().__init__ (canvas, view_frame, pack_toolbar=False)

        if background == "Light":
            backColor = cl_toolbar[0]
        else: 
            backColor = cl_toolbar[1]

        self.config(background=backColor)
        for button in self.winfo_children():
            button.config(background=backColor)

        self.mouse_coords = tk.StringVar(master=self)

 
    def set_message(self, s):
        # suppress coordinates to show in original toolbar 

        self.mouse_coords.set (s) 


# ----------------------------------------------------------


class DragManager:
    """ 
    The DragManager enables moving around matplotlib artist(s) on its axes.
    
    An artist is typically a Line2D object like a line or point
    """

    def __init__(self, ax, animated_artists, bounds=None, 
                 typeTag = None, 
                 callback_draw_animated = None, 
                 callback_shiftCtrlClick = None, 
                 callback_doubleClick = None, 
                 callback_on_moved=None):
        """
        Create a DragManager for an artist 

        Parameters
        ----------
        ax :                Axes - matplotlib axes we are working on 
        animated_artist :   Artist - matplotlib artist like Line2D - either single or list
        bounds :            [(x1,x2),(y1,y2)] - bounds for movement of the artist - either scalar or array
        typeTag :           optional free form tag to identy DragManager in callBacks
        callback_draw_animated : function - optional external method to draw the artist (and do other thinsg)
        callback_shiftCtrlClick : function - callback with coordinates shift or control click
        callback_doubleClick : function - callback with iArtist
        callback_on_moved : function - call with final coordinates after movement
             
        """
        self.ax = ax
        self._typeTag = typeTag
        self.canvas = ax.figure.canvas
        self._callback_on_moved = callback_on_moved
        self._callback_draw_animated = callback_draw_animated
        self._callback_shiftCtrlClick = callback_shiftCtrlClick
        self._callback_doubleClick = callback_doubleClick
        self._shiftCtrlClick = False                # was shift or ctrl press down made?
        self._doubleClick    = False                # was double click made?

        # bounds for movements
        if bounds: 
            self._bounds_x = bounds[0]
            self._bounds_y = bounds[1]
            if isinstance (animated_artists, list):
                if isinstance (self._bounds_x,tuple) or isinstance (self._bounds_y,tuple):
                    raise ValueError ("Dragmanager: Bounds of artist must be list")
        else:
            self._bounds_x = None
            self._bounds_y = None

        self._bg = None

        if isinstance (animated_artists, list):         # could be list or single artist
            self._artists = animated_artists
        else:
            self._artists = [animated_artists]          #
        self._press_xy = None

        # Connect to all the events we need.
        self.cidpress   = self.canvas.mpl_connect('button_press_event', self.on_press)
        self.cidrelease = self.canvas.mpl_connect('button_release_event', self.on_release)
        self.cidmotion  = self.canvas.mpl_connect('motion_notify_event', self.on_motion)
        self.cid_enter  = self.canvas.mpl_connect('axes_enter_event', self.on_enter_event)


    def _draw_animated(self, artist_onMove=None, duringMove=False, iArtMoved=None):
        """Draw the animated artists either via callback for static and animated"""

        if duringMove:                # on move - draw on move only current
            artistMoved = self._artists[iArtMoved]
            # draw and handle all the animated stuff 
            self._callback_draw_animated(artist_onMove=artistMoved, 
                                            iArtist= iArtMoved, typeTag= self._typeTag)
 

    def on_enter_event(self, event): 
        """Callback to register with 'motion_notify_even' which is fired when entering axes"""

        # the tkkinter widget doesn't have initially the fous - ctrl / shift key is lost  
        # print ("enter " , event )
        self.canvas.get_tk_widget().focus_set()


    def set_background(self, aBackground):
        """ background image can also be provided from outside if there are several dragMans """ 
        self._bg = aBackground


    def on_press(self, event : MouseEvent ):
        """Check whether mouse is over us; if so, store some data."""

        if event.inaxes != self.ax: return

        # is one of my artists hit? 
        myArtistHit = False
        iArt = None
        for iArt, myArtist in enumerate (self._artists): 
            contains, attrd = myArtist.contains(event)
            if contains: 
                myArtistHit = True
                break

        if not myArtistHit: iArt = None 

        logging.debug (f"Matplotlib Event:  {event.name}  doubleClick: {event.dblclick}")

        # shift ctrl click not on an artist ? Extra handling 
        if event.key =='control' or event.key =='shift':
            if self._callback_shiftCtrlClick:

                # blank background
                self.canvas.restore_region(self._bg)

                #call back into parent - draw artists with new values 
                self._callback_shiftCtrlClick(iArtist=iArt, typeTag= self._typeTag, event=event)

                # new state of self 
                self._shiftCtrlClick = True
                return 

        if not myArtistHit: return

        # double click extra handling 
        if event.dblclick:
            if self._callback_doubleClick:

                # self.canvas.restore_region(self._bg)            # blank background
                self._callback_doubleClick (iArtist=iArt)       #call back into parent 
                self._doubleClick = True                        # new state of self 
                return 

        # store all relevant data - index of artist, old position, new mouse poisition
        self._press_xy = iArt, myArtist.get_xydata()[0], (event.xdata, event.ydata)


    def on_motion(self, event):
        """Move the artits if the mouse is draged over us."""
        if self._press_xy is None or event.inaxes != self.ax:
            return
        " retrieve data from initial mouse down"
        iArt, (x0, y0), (xpress, ypress) = self._press_xy

        dx = event.xdata - xpress
        dy = event.ydata - ypress
        newx = x0 + dx
        newy = y0 + dy
        if self._bounds_x: 
            if isinstance (self._bounds_x[iArt], tuple):
                bounds_min = self._bounds_x [iArt][0]
                bounds_max = self._bounds_x [iArt][1]
            else: 
                bounds_min = self._bounds_x [0]
                bounds_max = self._bounds_x [1]
            newx = max (newx, bounds_min)
            newx = min (newx, bounds_max)
        if self._bounds_y: 
            if isinstance (self._bounds_y[iArt], tuple):
                bounds_min = self._bounds_y [iArt][0]
                bounds_max = self._bounds_y [iArt][1]
            else: 
                bounds_min = self._bounds_y [0]
                bounds_max = self._bounds_y [1]
            newy = max (newy, bounds_min)
            newy = min (newy, bounds_max)
        # print(f'x0={x0}, xpress={xpress}, event.xdata={event.xdata}, '
        #       f'dx={dx}, x0+dx={x0+dx}')
        self._artists[iArt].set_xdata([newx])
        self._artists[iArt].set_ydata([newy])

        self.canvas.restore_region(self._bg)

        """ draw my artists during move - only the moved artist will be 'duringMove'"""
        self._draw_animated(duringMove = True, iArtMoved=iArt)

        self.canvas.blit(self.ax.bbox)
        self.canvas.flush_events()

    def on_release(self, event : MouseEvent):
        """Clear button press information."""
        if event.button != 1:
            return

        if event.inaxes != self.ax: 
            return

        logging.debug (f"Matplotlib Event:  {event.name}  doubleClick: {event.dblclick}")

        if self._doubleClick:

            self._doubleClick = False

        # does the released button belong to self motion? 
        elif self._press_xy: 
            self.canvas.restore_region(self._bg)

            # callback when move is finished - before redraw - parent could change points
            if not self._callback_on_moved is None:
                self._callback_on_moved() 

            # parent has to take care on refresh 
            #  --> self._draw_animated()

        elif self._shiftCtrlClick:

            # now show artists which were drawn at button down 
            self.canvas.blit(self.ax.bbox)
            self.canvas.flush_events()

            # callback when shiftCtrlClick is finished 
            self._callback_on_moved() 

        # reset state variable         
        self._shiftCtrlClick = False
        self._press_xy = None


    def _disconnect(self):
        """Disconnect all callbacks."""
        self.canvas.mpl_disconnect(self.cidpress)
        self.canvas.mpl_disconnect(self.cidrelease)
        self.canvas.mpl_disconnect(self.cidmotion)
        self.canvas.mpl_disconnect(self.cid_enter)

        self.ax = None
        self.canvas = None
        self._callback_on_moved = None
        self._callback_draw_animated = None
        self._callback_shiftCtrlClick = None
        self._callback_doubleClick = None

