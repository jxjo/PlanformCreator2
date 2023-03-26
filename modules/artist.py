#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""  

The "Artists" to plot a wing object on a matplotlib axes

"""
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import matplotlib.backends.backend_tkagg
from matplotlib.backends.backend_tkagg import NavigationToolbar2Tk 

from cycler import cycler  

import numpy as np

from common_utils import *

cl_background       = '#101010'
cl_labelGrid        = '#B0B0B0'
cl_text             = '#D0D0D0'
cl_userHint         = '#E0A721'
cl_toolbar          = 'gray35'


class Artist():
    """Base class of all artists

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

    plt.rcParams.update({'axes.grid': False})                   # display grid or not    
    plt.rcParams.update({'grid.linewidth': 0.8})                # in points         
    plt.rcParams.update({'grid.color': cl_labelGrid})           
    plt.rcParams.update({'grid.alpha': 0.2})                    # transparency, between 0.0 and 1.0  


    def __init__ (self, axes, modelFn, norm = False, onPick=None, show=False, showMarker=True):

        self.ax : plt.Axes = axes
        self._modelFn = modelFn             # we get a bounded method to the model e.g. Wing 

        self._norm = norm                   # plot in normed coordinates
        self._show = show                   # should self be plotted? 
        self.showMarker = showMarker
        self._myPlots = []                  # plots (line artists) made up to now 
    
        if onPick:
            self._pickActive = True
            self._pickCallback = onPick
        else:
            self._pickActive = False
            self._pickCallback = None

        self._curLineLabel = None

        self.userTip  = self._defaultUserTip ()


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
            self._showUserTip ()
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

    # --------------  private -------------

    def _plot (self):
        # do plot - overwritten in sublass
        pass

    def _deleteMyPlots(self):

        for p in self._myPlots:
            if isinstance (p, list):
                for l in p:                     # ax.plot returns a list of plot items(lines)
                    try:
                        l.remove()
                    except:
                        print (" -!- ups - artist %s not found", l)
            else:
                try:
                    p.remove()
                except:
                    print (" -!- ups - artist %snot found", p)
        self._myPlots = []


    def _add(self, aPlot):
        self._myPlots.append(aPlot)

    def _makeLinePickable (self, aLine): 

        try: 
            aLine[0].set_picker (True)
            aLine[0].set_pickradius(5)
        except: 
            pass


    def _on_pick (self, event):
        # callback of matplt - having matplt 'event' as argument
        try: 
            myLabel = event.artist.get_label()
        except: 
            pass # print ("- Pick event couldn't be handled ", event.artist, "callback: ", self._pickCallback)

        # now callback parent with myLabel as argument
        if self._pickCallback:
            self._pickCallback(myLabel)
        # default remove of user tip 
        if self.userTip:
            self.userTip = ''
            self.plot()


    def _showUserTip (self):

        if self.userTip: 
            p = self.ax.text(0.5, 0.06, self.userTip , color=cl_userHint, 
                            horizontalalignment='center', transform=self.ax.transAxes)
            self._add(p)
 
    def _defaultUserTip (self):
        # default user tip - overwritten in subclass"
        return None


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
    

import tkinter as tk

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

    def __init__ (self, canvas, view_frame):
        super().__init__ (canvas, view_frame, pack_toolbar=False)

        self.config(background=cl_toolbar)
        for button in self.winfo_children():
            button.config(background=cl_toolbar)

 
    def set_message(self, s):
        # suppress coordinates to show 
        pass
