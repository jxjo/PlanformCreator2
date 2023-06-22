#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""  

The "Artists" to plot a wing object on a matplotlib axes

"""
import numpy as np
from common_utils import *
from artist import *
from wing_model import (Wing, Planform, WingSection, Planform_DXF, Flap, 
                        Planform_Bezier, Airfoil, Planform_Paneled)

cl_planform         = 'whitesmoke'
cl_quarter          = 'lightgrey'
cl_pureElliptical   = 'royalblue'
cl_dxf              = 'chocolate'
cl_wingSection_fix  = 'deeppink'
cl_wingSection_flex = 'mediumvioletred'
cl_paneled          = 'steelblue'



# -------- concrete sub classes ------------------------


class Grid_Artist (Artist):
    """shows the grid in the axis
    """
    # plot over loaded because different handling with grid
    def set_show (self, aBool):
        """ suser switch to diaable ploting the data
        """
        self._show = aBool
        self.plot()

    def plot(self):
        # todo improved grid depending on scale  
        self.ax.grid(self.show)
        self.ax.figure.canvas.draw_idle()


class CurrentSection_Artist (Artist):
    """Plot a Marker Symbol at the current (selected) wing sections.
    May plot in real coordinates and normed

    Keyword Arguments:
        norm --   True: plot in a normed coordinate system
    """    
    @property   
    def wing (self) -> Wing:
        return self.model
    @property
    def wingSections (self): 
        return self.wing.wingSections

    def __init__ (self, axes, dataModel, norm = False, onPick=None, show=False):
        super().__init__ (axes, dataModel, norm = norm, onPick=onPick, show=show)

    def set_current (self, aLineLabel, figureUpdate=False):
        """ tries to set a Marker to a line with Label aLabel 
        """
        if (not aLineLabel is None and aLineLabel != self._curLineLabel):    # only when changed do something
            self._curLineLabel = aLineLabel
            if self.show:                       # view is switched on by user? 
                self.plot (figureUpdate=figureUpdate)

    def _plot (self): 
        """ do plot of wing sections in the prepared axes   
        """ 
        section : WingSection

        sectionName = self._curLineLabel
        found = False
        for section in self.wingSections:
            if section.name() == sectionName:
                found = True
                break
        if not found: return 

        if self._norm: 
            y, le_to_te = section.norm_line()
            marker_y = - 0.02                # le_to_te[1] - 0.01 
            delta = 0.001
        else:
            y, le_to_te = section.line()
            marker_y = le_to_te[1] + 10 
            delta = 1

        # a marker triangle close to x-axis
        marker_x = y[0]
        p = self.ax.plot(marker_x, marker_y,  markersize=10, clip_on=False, 
                         marker=6, color = cl_userHint)
        self._add(p)

        # 2 lines left and right of section to highlight section
        ynew = np.asarray(y) + delta
        p = self.ax.plot(ynew, le_to_te, ':', color=cl_userHint)
        self._add (p)              # remind plot to delete 
        ynew = np.asarray(y) - delta
        p = self.ax.plot(ynew, le_to_te, ':', color=cl_userHint)
        self._add (p)              # remind plot to delete 



class Planform_Artist (Artist):
    """Plot the outline of the wing planform.
    """

    def __init__ (self, axes, modelFn, planform=None, **kwargs):
        super().__init__ (axes, modelFn, **kwargs)

        # an alternative planform - not taken from Wing
        self._planform = planform

    @property
    def planform (self) -> Planform:
        if self._planform is None:
            return self.model.planform
        else:
            return self._planform
    
    def _plot(self):
    
        y, leadingEdge, trailingEdge = self.planform.lines()

        self._show_wingData (y, leadingEdge, trailingEdge)

        p = self.ax.plot(y, leadingEdge,  '-', color=cl_planform)
        self._add (p)
        p = self.ax.plot(y, trailingEdge, '-', color=cl_planform)
        self._add (p)
        p = self.ax.fill_between(y, leadingEdge, trailingEdge, facecolor=cl_planform, alpha=0.10)
        self._add (p)

        # hinge line
        y, hinge = self.planform.hingeLine()
        p = self.ax.plot(y, hinge,  '-', linewidth=0.8, color='springgreen')
        self._add (p)

        # plot at last chord and tip 
        section = self.planform.wing.rootSection
        y, le_to_te = section.line()
        p = self.ax.plot(y, le_to_te,  '-', color=cl_planform)
        self._add (p)
        section = self.planform.wing.tipSection
        y, le_to_te = section.line()
        p = self.ax.plot(y, le_to_te,  '-', color=cl_planform)
        self._add (p)

    def _show_wingData (self, y, leadingEdge, trailingEdge):

        area, aspectRatio = self.planform.calc_area_AR (y, leadingEdge, trailingEdge)
        text  = "Wing span %.0f mm\n" % (self.planform.halfwingspan * 2)
        text += "Wing area %.1f dm²\n" % (area * 2 / 10000)
        text += "Aspect ratio %.1f\n" % (aspectRatio)

        p = self.ax.text (0.91, 0.05, text, color=cl_labelGrid, # fontsize = 'small',
                          transform=self.ax.transAxes, 
                          horizontalalignment='left', verticalalignment='bottom')
        self._add (p)   
       


class ChordLines_Artist (Artist):
    """Plot the chordlines t/2 t/4 3t/4 of the wing planform and chord distribution.
    """
    @property
    def planform (self) -> Planform:
        return self.model.planform
    
    def _plot(self):

        if self._norm: 
            y, chord = self.planform.norm_chord_line ()
            quarterChord = chord/4
        else:
            y, leadingEdge, trailingEdge = self.planform.lines()
            quarterChord = leadingEdge + (trailingEdge - leadingEdge)/4

        p = self.ax.plot(y, quarterChord, '--', color= cl_quarter, linewidth=0.7)
        self._add (p)

        if self._norm: 
            halfChord = chord/2
        else:
            halfChord = leadingEdge + (trailingEdge - leadingEdge)/2
        p = self.ax.plot(y, halfChord, '--', color= cl_quarter, linewidth=0.7)
        self._add (p)

        if self._norm: 
            threeQuarterChord = chord * 3 / 4
        else:
            threeQuarterChord = leadingEdge + (trailingEdge - leadingEdge) * 3/4
        p = self.ax.plot(y, threeQuarterChord, '--', color= cl_quarter, linewidth=0.7)
        self._add (p)



class RefPlanform_Artist (Planform_Artist):
    """Plot the outline of the wing reference planform 'PureElliptical'
    """
    color = cl_pureElliptical 

    @property
    def refPlanform (self) -> Planform :
        return self.model.refPlanform

    def _plot(self):
        y, leadingEdge, trailingEdge = self.refPlanform.lines()
        p = self.ax.plot(y, leadingEdge,  color=self.color)
        self._add (p)              # remind plot to delete 
        p = self.ax.plot(y, trailingEdge, color=self.color)
        self._add (p)              # remind plot to delete 

        self.plot_markers (y, leadingEdge)

    def plot_markers (self, y, leadingEdge): 
        def closest(list, Number):
            aux = []
            for valor in list:
                aux.append(abs(Number-valor))
            return aux.index(min(aux))
        # print a line label with dxf filename 
        iclosest = closest (y, 0.7 * y[-1])         # pos at 90% span 
        marker_y = y[iclosest] 
        marker_x = leadingEdge[iclosest] - 15
        text = self.refPlanform.planformType
        p = self.ax.text (marker_y, marker_x, text , color = self.color, ha='left', va='bottom')
        self._add (p)   


class RefPlanform_DXF_Artist (Planform_Artist):
    """Plot the outline of the dxf reference planform 
    """
    color = cl_dxf  
    @property
    def refPlanform_DXF (self) -> Planform_DXF :
        if self._planform is None:
            return self.model.refPlanform_DXF
        else:
            return self._planform

    def _plot(self):

        # is there a DXF planform? 
        if (self.refPlanform_DXF): 
            y, leadingEdge, trailingEdge = self.refPlanform_DXF.lines ()
            if y != []: 
                p = self.ax.plot(y, leadingEdge,  label='DXF contour', color=self.color)
                self._add (p)              # remind plot to delete 
                p = self.ax.plot(y, trailingEdge, label='DXF contour', color=self.color)
                self._add (p)              # remind plot to delete 

                # rootline 
                yr = [y[0],y[0]]
                xr = [leadingEdge[0],trailingEdge[0]]
                p = self.ax.plot(yr, xr, color=self.color)
                self._add (p)              # remind plot to delete 

                # hinge line? 
                yh, xh = self.refPlanform_DXF.hingeLine_dxf()
                if yh != []:
                    p = self.ax.plot(yh, xh, color=self.color)
                    self._add (p)              # remind plot to delete 

                if self.showMarker: 
                    self.plot_markers (y, leadingEdge)

    def plot_markers (self, y, leadingEdge): 

        def closest(list, Number):
            aux = []
            for valor in list:
                aux.append(abs(Number-valor))
            return aux.index(min(aux))
        
        # print a line label with dxf filename 
        iclosest = closest (y, 0.82 * y[-1])         # 90% span 
        marker_y = y[iclosest] 
        marker_x = leadingEdge[iclosest] - 15

        text = self.refPlanform_DXF.dxf_filename()
        p = self.ax.text (marker_y, marker_x, text , color = self.color, ha='left', va='bottom')
        self._add (p)   


class PaneledPlanform_Artist (Planform_Artist):
    """Plot the outline of the paneled planform for Xflr5 or FLZ export
    """
    color = cl_paneled 

    def __init__ (self, axes, dataModel, paneledPlanform, **kwargs):
        super().__init__ (axes, dataModel, **kwargs)

        self._paneledPlanform = paneledPlanform

    @property
    def planform (self) -> Planform :
        return self.model.planform
    @property
    def paneledPlanform (self) -> Planform_Paneled :
        return self._paneledPlanform
    @property
    def wingSections (self): 
        return self.model.wingSections

    def _plot(self):

        # le and te of the original planform 
        y, leadingEdge, trailingEdge = self.planform.lines()
        lw = 0.7
        p = self.ax.plot(y, leadingEdge,  '--', lw=lw, color=cl_planform)
        self._add (p)
        p = self.ax.plot(y, trailingEdge, '--', lw=lw, color=cl_planform)
        self._add (p)

        # y-panel lines 
        lw = 0.7
        ls ='-'
        lines_y, lines_le_to_te, deviations = self.paneledPlanform.y_panel_lines()

        for iLine in range(len(lines_y)):
            y = lines_y[iLine]
            le_to_te = lines_le_to_te [iLine]
            if deviations[iLine] > 5:            # highlight y with too much deviation from actual 
                lw = 1.5
                color = cl_userHint
            else:
                lw = 0.7
                color = self.color
            p = self.ax.plot(y, le_to_te, color=color, linewidth=lw)
            self._add (p)              # remind plot to delete 

        # x-panel lines 
        lines_y, lines_panels_x = self.paneledPlanform.x_panel_lines()

        for iLine in range(len(lines_y)):
            y = lines_y[iLine]
            line_x = lines_panels_x [iLine]
            p = self.ax.plot(y, line_x, color=self.color, linewidth=lw)
            self._add (p)              # remind plot to delete 

        # wing sections
        section : WingSection
        lw = 1
        ls ='-'

        for section in self.wingSections:
            y, le_to_te = section.line()
            p = self.ax.plot(y, le_to_te, color=cl_wingSection_fix, linestyle=ls, linewidth=lw)
            self._add (p)              # remind plot to delete 
            self.plot_markers (y, le_to_te, section)


    def plot_markers (self, y, le_to_te, section: WingSection): 

        # plot section name 
        offset = 10
        top_y = y[0] 
        if section.isTip: 
            top_x = le_to_te[0] - 4 * offset
        else: 
            top_x = le_to_te[0] - offset

        if section.isRoot:
            label = "Root"
        elif section.isTip:
            label = "Tip"
        else:
            label = str(section.wing.wingSectionIndexOf (section))

        p = self.ax.text (top_y, top_x, "%s" % label, ha='center', va='bottom',
                          color = cl_wingSection_fix )
        self._add (p)   


class Chord_Artist (Artist):
    """Plot the chord distribution of a planform.
    """
    color = cl_planform     

    def set_mouseActive (self, aBool): 
        # overloaded for user tip 
        if aBool:
            self.userTip = 'Drag tangent endpoints with mouse'
        else: 
            self.userTip = '' 
        super().set_mouseActive(aBool)


    def __init__ (self, axes, modelFn, **kwargs):
        super().__init__ (axes, modelFn, **kwargs)

        # show mouse helper / allow drag of points 
        self.p1_marker_artist = None
        self.p1_line_artist = None
        self.p2_marker_artist = None
        self.p2_line_artist = None
        self.chord_line_artist = None 
        self.chord_fill_artist = None 

        # DragManager instances
        self._dm1 = None
        self._dm2 = None


    def _deleteMyPlots(self):
        # overloaded to remove mouse bindung of DragManager"
    
        if not self._dm1 is None:
            self._dm1.disconnect()
            self._dm1 = None 
        if not self._dm2 is None:
            self._dm2.disconnect()
            self._dm1 = None 
        super()._deleteMyPlots ()
    
    @property
    def planform (self) -> Planform :
        return self.model.planform
    
    def chord_line (self):
        # will be overwritten by subclasses
        return self.model.planform.norm_chord_line ()    
    
    def _plot (self): 
        
        y, chord = self.chord_line ()

        # draw chord - if mouse is active animate=True has to be set!
        p = self.ax.plot(y, chord, '-', label = 'Chord distribution', color=self.color, animated=self.mouseActive)
        self._add(p)
        (self.chord_line_artist,) = p 

        # p = self.ax.fill_between(y, 0, chord, facecolor=self.color, alpha=0.10, animated=True)
        # self._add(p)
        # (self.chord_fill_artist,) = p 

        if self.mouseActive and isinstance(self.planform, Planform_Bezier): 

            self.show_mouseHelper_bezier (self.model.planform)

            # make p1,2 of Bezier draggable - install callback when move is finished
            self._dm1 = DragManager (self.ax, self.p1_marker_artist, 
                                    bounds=[(0.1, 0.95),(0.8, 1.1)], 
                                    dependant_artists = [self.p1_line_artist], 
                                    callback_draw_animated = self.draw_animated_p1,
                                    callback_on_moved=self._moveCallback)
                                    # callback_on_moved=self.on_p1_moved)
            self._dm2 = DragManager (self.ax, self.p2_marker_artist, 
                                    bounds=[(1, 1),(0.05, 0.95)], 
                                    dependant_artists = [self.p2_line_artist], 
                                    callback_draw_animated = self.draw_animated_p2,
                                    callback_on_moved=self._moveCallback)


    def show_mouseHelper_bezier (self, planform: Planform): 
        """ show the helper points and lines for bezier curve definition """

        # ! animated=True must be set for all artists moving around !

        p = self.ax.plot (planform._py[0:2], planform._px[0:2], '--', linewidth=0.5, color= cl_userHint, animated=True) 
        self._add(p)
        (self.p1_line_artist,) = p 
        p = self.ax.plot (planform._py[0]+0.002, planform._px[0], marker='s', color=cl_userHint, markersize=5 )
        self._add(p)
        p = self.ax.plot (planform._py[1], planform._px[1], marker='o', color=cl_userHint, markersize=8, animated=True )
        self._add(p)
        (self.p1_marker_artist,) = p 

        p = self.ax.plot (planform._py[2:], planform._px[2:], '--', linewidth=0.5, color= cl_userHint)    
        self._add(p)
        (self.p2_line_artist,) = p 
        p = self.ax.plot (planform._py[3], planform._px[3], marker='s', color=cl_userHint, markersize=5 )
        self._add(p)
        p = self.ax.plot (planform._py[2], planform._px[2], marker='o', color=cl_userHint, markersize=8 )
        self._add(p)
        (self.p2_marker_artist,) = p 


    def draw_animated_p1(self, duringMove=False): 
        """ call back when bezier point 1 was moved"""

        # draw marker and get new coordinates (when dragged) 
        self.ax.draw_artist (self.p1_marker_artist)
        x1,y1 = self.p1_marker_artist.get_xydata()[0]

        # set new endpoint for tangent line and draw line 
        x = self.p1_line_artist.get_xdata()
        y = self.p1_line_artist.get_ydata()
        x[-1] = x1
        y[-1] = y1
        self.p1_line_artist.set_xdata(x)
        self.p1_line_artist.set_ydata(y)
        self.ax.draw_artist (self.p1_line_artist)

        # update planform - ! wing coordinate system
        self.planform.set_p1x (y1)  
        self.planform.set_p1y (x1)  
        self.draw_animated_chord(duringMove=duringMove)  


    def draw_animated_p2(self, duringMove=False): 
        """ call back when bezier point 2 was moved"""

        # draw marker and get new coordinates (when dragged) 
        self.ax.draw_artist (self.p2_marker_artist)
        x2,y2 = self.p2_marker_artist.get_xydata()[0]

        # set new endpoint for tangent line and draw line 
        x = self.p2_line_artist.get_xdata()
        y = self.p2_line_artist.get_ydata()
        x[0] = x2
        y[0] = y2
        self.p2_line_artist.set_xdata(x)
        self.p2_line_artist.set_ydata(y)
        self.ax.draw_artist (self.p2_line_artist)

        # update planform - ! wing coordinate system
        self.planform.set_p2x (y2)
        self.draw_animated_chord(duringMove=duringMove)  


    def draw_animated_chord (self, duringMove=False):

        y, chord = self.chord_line ()

        # because of animate=True the artist has to be provided with actual data...
        self.chord_line_artist.set_xdata(y)
        self.chord_line_artist.set_ydata(chord)
        self.chord_line_artist.set_xdata(y)
        self.chord_line_artist.set_ydata(chord)
        if duringMove:
            self.chord_line_artist.set_linestyle(':')
        else:
            self.chord_line_artist.set_linestyle('-')
        # and plotted with special commans
        self.ax.draw_artist (self.chord_line_artist)



# ----------------------------------


class RefChord_Artist (Chord_Artist):
    """Plot the chord distribution of a planform.
    """
    color = cl_pureElliptical
    def chord_line (self):
        return self.model.refPlanform.norm_chord_line ()


class RefChord_DXF_Artist (Chord_Artist):
    """Plot the chord distribution of a planform.
    """
    color = cl_dxf
    def chord_line (self):
        return self.model.refPlanform_DXF.norm_chord_line ()


class Sections_Artist (Artist):
    """Plot the wing sections as a vertical line. Add markers and text etc.
    May plot in real coordinates and normed

    Keyword Arguments:
        norm --   True: plot in a normed coordinate system
    """
    def _defaultUserTip (self):
        # overwritten in subclass"
        return 'Click section to select'

    @property   
    def wing (self) -> Wing:
        return self.model
    @property
    def wingSections (self): 
        return self.wing.wingSections
    
    def _plot (self): 
        """ do plot of wing sections in the prepared axes   
        """
        section : WingSection

        # now plot each single section
        for section in self.wingSections:
            if self._norm: 
                y, le_to_te = section.norm_line()
                le_to_te [1] =  0.08
            else:
                y, le_to_te = section.line()

            if section.hasFixedPosition():
                color = cl_wingSection_fix
                linewidth= 1.5
                linestyle='solid'
            else:
                color = cl_wingSection_flex
                linewidth = 1
                linestyle='dashed'

            label = section.name()

            if not (section.isRootOrTip and self._norm): 
                p = self.ax.plot(y, le_to_te, color=color, label=label, linestyle=linestyle, 
                                linewidth=linewidth)
                self._add (p)              # remind plot to delete 

                if self._pickActive: 
                    self._makeLinePickable (p)

                self.plot_markers (y, le_to_te, section)

        # activate event for clicking on line 
        if self._pickActive:
            self.ax.figure.canvas.mpl_connect('pick_event', self._on_pick)


    def plot_markers (self, y, le_to_te, section: WingSection): 

        sectionFix = section.hasFixedPosition()

        # section chord - print along chord
        if self._norm:
            # if section.isRoot: return               # no norm_chord for root
            text = "%.2f" % (section.norm_chord)
            marker_x = (le_to_te[0] + le_to_te[1]) * 0.6
            marker_y = y[0] + 0.007
        else: 
            text = "%.0fmm" % section.chord
            marker_x = le_to_te[1] - (le_to_te[1] - le_to_te[0]) * 0.6
            marker_y = y[0] + 6

        if not sectionFix:                          # fixed chord 
            text = "fix\n" + text 

        color = cl_wingSection_fix

        p = self.ax.text (marker_y, marker_x, text, ha='left',color = color )
        self._add (p)   

        # section pos at bottom  
        if not section.isRootOrTip: 
            if self._norm:
                marker_y = -0.05                            # in axis coordinates
                text = "%.2f" % (section.norm_yPos)
                if sectionFix:                                  # fixed position 
                    text = "fix\n\n" + text 
            else: 
                marker_y = 0.02                             # in axis coordinates
                text = "%.0fmm" % section.yPos
                if sectionFix:                                  # fixed position 
                    text = "fix\n" + text 

            color = cl_wingSection_fix
            marker_x = y[0]                                 # in data coordinates

            p = self.ax.text (marker_x, marker_y, text, color=color, # fontsize = 'small',
                            transform=self.ax.get_xaxis_transform(), 
                            horizontalalignment='center', verticalalignment='bottom')
            self._add (p)   

        # section name above le
        marker_top_y = y[0] 
        if self._norm:
            offset = - 0.03
        else:
            offset = 10
        marker_top_x = le_to_te[0] - offset

        if section.isRoot:
            label = "Root"
        elif section.isTip:
            label = "Tip"
            marker_top_x = le_to_te[0] - 4 * offset
        else:
            label = str(section.wing.wingSectionIndexOf (section))

        p = self.ax.text (marker_top_y, marker_top_x, "%s" % label, ha='center', va='bottom',
                          color = cl_wingSection_fix, fontsize = 'x-large' )
        self._add (p)   



#-----------------------------------------------

class Flap_Artist (Artist):
    """Plot the flaps as a filley polygon. Add markers and text etc.
       May plot in real coordinates 
    """
    def wing (self) -> Wing:
        return self.model

    def _plot (self): 
        """ do plot of wing sections in the prepared axes   
        """

        flaps = self.wing().getFlaps()

        n = len(flaps)                   # Number of colors
        if n < 2: n= 2
         # create cycled colors 
        self._set_colorcycle (n, colormap='summer')                # no of cycle colors - extra color for each airfoil

        flap : Flap
        for flap in flaps:   

            p = self.ax.plot(flap.y, flap.x, label="Flap group %d" %flap.flapGroup, color = cl_planform, linewidth=0.5)
            self._add(p)

            p = self.ax.fill(flap.y, flap.x, linewidth=3, alpha=0.3)
            self._add(p)

            color = p[0].get_facecolor()                      # get last cycler color 
            self._plot_markers (flap, color)

        # self.ax.legend()

        
    def  _plot_markers (self, flap : Flap, color): 

        # flapDepth
        self._text_flapDepth (flap.lineLeft, flap.depthLeft)
        if flap.lineRight [0][1] == self.wing().halfwingspan:    
            # plot extra depth at tip
            self._text_flapDepth (flap.lineRight, flap.depthRight)
        # if (flap.lineRight [0][1] - flap.lineLeft [0][1]) > self.wing().halfwingspan /10 :    
        #     # plot extra depth in the middle if thre is space
        #     line  = ((flap.lineLeft [0] + flap.lineRight[0]) / 2.0 , 
        #              (flap.lineLeft [1] + flap.lineRight[1]) / 2.0) 
        #     depth = (flap.depthLeft + flap.depthRight) / 2.0
        #     self._text_flapDepth (line, depth)

        # flapGroup
        yBase = (flap.lineLeft [0][0] + flap.lineRight [0][1]) / 2  # middle of flap span
        xBase = (flap.lineLeft [1][0] + flap.lineRight [1][1]) / 2  # middle of flap chord
        p = self.ax.text (yBase, xBase, "%d" % (flap.flapGroup) , 
                          color = cl_text, fontsize = 'x-large' )
        self._add(p)

    def _text_flapDepth (self, line, flapDepth):
        yBase = line [0][0]
        xBase = (line [1][0] + line [1][1]) / 2  # middle of flap chord
        p = self.ax.text (yBase + 5, xBase, "%.1f %%" % (flapDepth * 100) , 
                        color = cl_text, fontsize = 'small' )
        self._add(p)



class Airfoil_Artist (Artist):
    """Plot the airfoils of the wing sections 
    May plot in real coordinates and normed

    Additional arguments:
        strak --   True: also show straked airfoils 
    """
    def __init__ (self, axes, dataModel, strak=False, **kwargs):
        super().__init__ (axes, dataModel, **kwargs)

        self._strak = strak

    def _defaultUserTip (self):
        # overwritten in subclass"
        return 'Click airfoil to select'

    def set_current (self, aLineLabel, figureUpdate=False):
        """ tries to set a highlighted airfoil  to section with name ''aLineLabel' 
        """
        if (not aLineLabel is None and aLineLabel != self._curLineLabel):    # only when changed do something
            self._curLineLabel = aLineLabel
            if self.show:                       # view is switched on by user? 
                self.plot (figureUpdate=figureUpdate)

    def set_strak (self, aBool):
        """ show straked airfoils """
        self._strak = aBool
        if self.show:                       # view is switched on by user? 
            self.plot(figureUpdate=True)

    @property
    def wing (self) -> Wing:
        return self.model
    @property
    def wingSections (self): 
        return self.wing.wingSections
    
    def _plot (self): 
        """ do plot of wing sections in the prepared axes   
        """
        
        n = 0                                       # Number of colors
        for section in self.wing.wingSections:
            airfoil = section.airfoil
            if (airfoil.isLoaded) and not (not self._strak and airfoil.isStrakAirfoil): n += 1
        if not n: return 

         # create cycled colors 
        self._set_colorcycle (n)                # no of cycle colors - extra color for each airfoil

        # plot title
        if not self._norm: 
            text = "Airfoils in wing sections"
            self._add(self.ax.text(.05,.9, text, fontsize ='large', ha='left', transform=self.ax.transAxes))

        section : WingSection

        # now plot each single section
        for section in self.wing.wingSections:
            airfoil = section.airfoil
            if (airfoil.isLoaded) and not (not self._strak and airfoil.isStrakAirfoil):

                legend = ("%s:  %s" % (section.name(), airfoil.name))  

                if self._norm:
                    x = airfoil.x
                    y = airfoil.y
                else:
                    # x = 0 at quarter chord 
                    x = airfoil.x * section.chord - section.chord / 4
                    y = airfoil.y * section.chord 

                color = next(self.ax._get_lines.prop_cycler)['color']
                if self._curLineLabel == legend:
                    if not self._norm:                     # for norm it would be too much colorr confusion
                        p = self.ax.fill (x, y, facecolor=color, alpha=0.1)
                        self._add(p)
                    linewidth=1.6
                    self._plot_marker (x,y, color, section)
                else:
                    linewidth=0.8

                p = self.ax.plot (x, y, '-', color = color, label=legend, linewidth= linewidth)
                self._add(p)

                if self._pickActive: 
                    self._makeLinePickable (p)

        # activate event for clicking on line 
        if self._pickActive:
            self.ax.figure.canvas.mpl_connect('pick_event', self._on_pick)     
        if not self._norm: 
            self._plot_zero_marker (self.wing.rootchord)

        # self.ax.legend()


    def _plot_marker (self, x,y, color, section: WingSection):
        # annotate airfoil with name etc. 

        airfoil = section.airfoil 
        text = "'"+ section.airfoilNick() + "' - " + section.airfoilName() + "  @" + section.name() + \
               "\n" + \
               "\nThickness  %.2f%%  at  %.2f%%" % (airfoil.maxThickness, airfoil.maxThicknessX) +\
               "\nCamber     %.2f%%  at  %.2f%%" % (airfoil.maxCamber   , airfoil.maxCamberX   ) 

        y = np.amin(y) * 0.3
        x = np.amax(x) * 0.8
        p = self.ax.annotate(text, color=color, 
                xy=(x, y), xycoords='data', ha='left', va= 'top',
                xytext=(0, -50), textcoords='offset points')
        self._add(p)


    def  _plot_zero_marker (self, rootChord): 

        # draw a vertical line at x=0 to indicate quarter chord 
        # yfrom = - rootChord / 15
        # yto   = - rootChord / 25
        yfrom = 0.1
        yto   = 0.3
        p = self.ax.plot ([0,0],[yfrom,yto], color=  cl_labelGrid, 
                          transform=self.ax.get_xaxis_transform(), 
                          linestyle="dashed", linewidth=0.7)
        self._add(p) 

        y = 0.054
        p = self.ax.text(0 , y,  "quarter chord" , color=cl_labelGrid, 
                        transform=self.ax.get_xaxis_transform(), 
                        horizontalalignment='center', verticalalignment= "bottom")
        self._add(p) 


class AirfoilName_Artist (Artist):
    """shows the airfoil name in planform view """

    @property   
    def wing (self) -> Wing:
        return self.model
    @property
    def wingSections (self): 
        return self.wing.wingSections
    
    def _plot (self): 
        """ do plot of wing sections in the prepared axes   
        """
        if self.norm: return                    # normalized view not supported 

         # create cycled colors 
        n= len(self.wing.wingSections)
        self._set_colorcycle (n)                # no of cycle colors - extra color for each airfoil

        section : WingSection
        for section in self.wingSections:
            y, le_to_te = section.line()
            self.plot_markers (y, le_to_te, section)
            

    def plot_markers (self, y, le_to_te, section: WingSection): 
        # print airfoil name and nickname below the planform 

        marker_y = 0.97                         # in axis coordinates
        marker_x = y[0]                         # in data coordinates
        text = "'"+ section.airfoilNick() + "'" + "\n" + section.airfoilName()

        color = next(self.ax._get_lines.prop_cycler)['color']
        p = self.ax.text (marker_x, marker_y, text, color=color, 
                          transform=self.ax.get_xaxis_transform(), 
                          horizontalalignment='center', verticalalignment='top')
        self._add (p)   
