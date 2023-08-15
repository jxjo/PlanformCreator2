#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""  

The "Artists" to plot a wing object on a matplotlib axes


"""
from common_utils import *
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from matplotlib.backends.backend_tkagg import NavigationToolbar2Tk 

from cycler import cycler  

cl_background       = '#101010'
cl_labelGrid        = '#C0C0C0'
cl_text             = '#D0D0D0'
cl_userHint         = '#E0A721'
cl_toolbar          = ('gray85', 'gray35')


class Artist():
    """
    The "Artist" to plot a wing object on a matplotlib axes

        - these are not MatplotLib artists  - 

    Arguments:
        axes --     the plt axes to plot onto
        dataModel -- the object the artist should plot 

    Keyword Arguments:
        onPick --   call back command when line was picked by user - will activate picking :)
        norm --     when implemented will plot in a normed coordinate systeme
    """
    
    # plt.rcParams.update({'figure.dpi': 180})
    plt.rcParams.update({'font.size': 9})                       # default font.size: 10.0
    plt.rcParams.update({'font.weight': 'light'})   
    plt.rcParams.update({'text.color': cl_text})   

    plt.rcParams.update({'figure.facecolor': cl_background})         
    plt.rcParams.update({'axes.facecolor': cl_background})      
    plt.rcParams.update({'axes.edgecolor': cl_labelGrid})       # axes spines color        
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


    def __init__ (self, axes, modelFn, norm = False, onPick=None, onMove=None,
                  show=False, showMarker=True):

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
                objectList.append (objectFn())
            return objectList
        else: 
            try: 
                return self._modelFn()          # modelFn is a bound method to the object
            except:
                return self._modelFn
    
    @property
    def show (self): return self._show

    def set_show (self, aBool):
        """ suser switch to diaable ploting the data
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

    def set_showLegend (self, aBool: bool):
        """ switch display of axes legend - no refresh will be done"""
        self._showLegend = aBool

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
                print (" -!- ups - artist %snot found" %p)
        self._myPlots = []

        # remove registered pick events 
        if not self._cidpick is None:
            self.ax.figure.canvas.mpl_disconnect(self._cidpick)

        # remove DragManagers 
        dm : DragManager
        for dm in self._dragManagers:
            dm.disconnect()
        self._dragManagers = []

        # remove ticks self added to axis
        self._remove_myticks ()


    def _add(self, aPlot):
        """ add matplotlib artist to artists of self"""
        if isinstance (aPlot, list):            # .plot returns list 
            art = aPlot [0]
        else:
            art = aPlot

        self._myPlots.append(art)

    def _plotLegend(self):
        """ shows the legend """

        if self._showLegend:
            # are there any lines with labels
            h, l = self.ax.get_legend_handles_labels()
            if h: 
                leg = self.ax.legend(h, l, labelcolor=cl_labelGrid)
            else: 
                leg = self.ax.legend([], [])        # remove legend 
            leg.set_zorder(2)
            leg.get_frame().set_linewidth(0.0)


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

    def _connectPickEvent (self):
        """ installs and connects callback for pick event """ 
        self._cidpick = self.ax.figure.canvas.mpl_connect('pick_event', self._on_pick)     



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


    def _set_colorcycle (self, nColors = 10, colormap='Set2'):
        """ set the color cycle of self axes to have a nice auto colors when plotting
        nColors define the length of the cycle (until it starts from the beginning)"""

        # https://matplotlib.org/stable/gallery/color/colormap_reference.html
        new_colors = plt.get_cmap(colormap)(np.linspace(0, 1, nColors))
        color_cycler = cycler('color', new_colors)  
        self.ax.set_prop_cycle(color_cycler)


    def _nextColor (self):
        "returns next color in color cycle"
        return next(self.ax._get_lines.prop_cycler)['color']
    
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



def autoscale_y(ax,margin=(0.1, 0.1)):
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


def autoscale_x(ax, margin=(0.1, 0.1)):
    """
    This function rescales the x-axis based on the data that is visible given the current ylim of the axis.
    ax -- a matplotlib axes object
    margin -- the fraction of the total height of the y-data to pad the upper and lower ylims"""

    def get_left_right(line):
        xd = line.get_xdata()
        yd = line.get_ydata()
        lo,hi = ax.get_ylim()
        x_displayed = xd[((yd>lo) & (yd<hi))]
        if len(x_displayed) > 1:                # a normal line 
            h = np.max(x_displayed) - np.min(x_displayed)
            left  = np.min(x_displayed) - margin[0]*h
            right = np.max(x_displayed) + margin[1]*h
        elif len(x_displayed) == 1:              # just a point 
            left  = xd[0] - margin[0] * xd[0]
            right = xd[0] + margin[1] * xd[0]
        else:                                   # empty plot 
            left  = float('inf')
            right = float('-inf')
        return left,right

    lines = ax.get_lines()
    left,right = np.inf, -np.inf

    for line in lines:
        new_left, new_right = get_left_right(line)
        if new_left  < left:  left  = new_left
        if new_right > right: right = new_right

    ax.set_xlim(left,right)




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
    #    ('Back', 'Back to previous view', 'back', 'back'),
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

 
    def set_message(self, s):
        # suppress coordinates to show 
        pass


# ----------------------------------------------------------


class DragManager:
    """ 
    The DragManager enables moving around matplotlib artist(s) on its axes.
    
    An artist is typically a Line2D object like a line or point
    """

    def __init__(self, ax, animated_artists, bounds=None,
                 dependant_artists = None, 
                 callback_draw_animated = None, 
                 callback_shiftCtrlClick = None, 
                 callback_on_moved=None):
        """
        Create a DragManager for an artist 

        Parameters
        ----------
        ax :                Axes - matplotlib axes we are working on 
        animated_artist :   Artist - matplotlib artist like Line2D - either single or list
        dependant_artists : Artists - optional other artists (lists) which are dependand on self moves
        bounds :            [(x1,x2),(y1,y2)] - bounds for movement of the artist - either scalar or array
        callback_draw_animated : function - optional external method to draw the artist (and do other thinsg)
        callback_shiftCtrlClick : function - call with coordinates during double click
        callback_on_moved : function - call with final coordinates after movement
             
        """
        self.ax = ax
        self.canvas = ax.figure.canvas
        self._callback_on_moved = callback_on_moved
        self._callback_draw_animated = callback_draw_animated
        self._callback_shiftCtrlClick = callback_shiftCtrlClick
        self._dependant_artists = dependant_artists

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
        self.ciddraw    = self.canvas.mpl_connect('draw_event', self.on_draw)
        self.cidpress   = self.canvas.mpl_connect('button_press_event', self.on_press)
        self.cidrelease = self.canvas.mpl_connect('button_release_event', self.on_release)
        self.cidmotion  = self.canvas.mpl_connect('motion_notify_event', self.on_motion)

    def _draw_animated(self, duringMove=False, iArtMoved=None):
        """Draw the animated artists."""
 
        for iArt, art in enumerate(self._artists):

            """ only the moved artist will be 'duringMove' for drawing"""
            if duringMove and (iArt == iArtMoved):
                duringMoveiArt = True
            else:
                duringMoveiArt = False

            # use user calback if provided 
            if self._callback_draw_animated: 
                if len(self._artists) > 1: 
                    self._callback_draw_animated(duringMove=duringMoveiArt, iArtist= iArt)
                else:                               # compatibility mode with single artist
                    self._callback_draw_animated(duringMove=duringMove)
            else: 
                self.ax.draw_artist (self._artists[iArt])

        # print('Artist: ', self._artists[iArtist], '  during: ', duringMove)


    def on_draw(self, event):
        """Callback to register with 'draw_event'."""
        if event is not None:
            if event.canvas != self.canvas: raise RuntimeError

        # save background without my artists
        self._bg = self.canvas.copy_from_bbox(self.ax.bbox)

        # initial draw 
        self._draw_animated()

        self.canvas.blit(self.ax.bbox)


    def on_press(self, event):
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

        # dbouble click not on an artist ? Extra handling 
        if event.key =='control' or event.key =='shift':
            if self._callback_shiftCtrlClick:
                self._callback_shiftCtrlClick(iArtist=iArt, event=event )
                return 

        if not myArtistHit: return

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

    def on_release(self, event):
        """Clear button press information."""
        if event.button != 1:
            return

        # does the released button belong to self? 
        if self._press_xy: 
            self._press_xy = None

            # callback when move is finished - before redraw - parent could change points
            if not self._callback_on_moved is None:
                self._callback_on_moved() 

            # self.canvas.draw()

            self.canvas.restore_region(self._bg)

            # draw my artists after movement in their 'static style' 
            self._draw_animated()

            self.canvas.blit(self.ax.bbox)
            # self._bg = None 


    def disconnect(self):
        """Disconnect all callbacks."""
        self.canvas.mpl_disconnect(self.ciddraw)
        self.canvas.mpl_disconnect(self.cidpress)
        self.canvas.mpl_disconnect(self.cidrelease)
        self.canvas.mpl_disconnect(self.cidmotion)




# if __name__ == '__main__':

#     # Test DraggableArtist 
#     #     
#     def on1_finished (): 
#         print("Finished ")

#     def draw_animated(duringMove=False):
#         ax.draw_artist (markerArtist)
#         x2,y2 = markerArtist.get_xydata()[0]
#         x = [0,x2]
#         y = [0,y2]
#         lineArtist.set_xdata(x)
#         lineArtist.set_ydata(y)
#         ax.draw_artist (lineArtist)

#     def draw_animated2(duringMove=False, iArtist=None):

#         if iArtist:
#             ax.draw_artist (markerArtists[iArtist])
#             print ("Artist: ", iArtist)
#         # ax.draw_artist (markerArtist2)

#     fig, ax = plt.subplots()

#     # ax.set_xlim((0, 6))
#     # ax.set_ylim((0, 6))


#     (markerArtist,) =  ax.plot ([2], [2], marker='o', color="red", markersize=8, animated=True) 
#     (lineArtist,) =  ax.plot ([0,2], [0,2],  color="red", animated=True) 
#     dr1 = DragManager(ax, markerArtist, bounds=[(2,5.5),(1,4)], 
#                        dependant_artists = [lineArtist], 
#                        callback_draw_animated = draw_animated,
#                        callback_on_moved=on1_finished)

#     # test array of artists 

#     markerArtists = []
#     for i in range (5):
#         (art,) =  ax.plot ([i], [8-i], marker='o', color="blue", markersize=6, animated=True) 
#         markerArtists.append(art)

#     dr2 = DragManager(ax, markerArtists, bounds=None, 
#                        callback_draw_animated = draw_animated2,
#                        callback_on_moved=on1_finished)


#     plt.show()
