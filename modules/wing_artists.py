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


# ----------------------------------


class CurrentSection_Artist (Artist):
    """Plot a Marker Symbol at the current (selected) wing sections.
       May plot in real coordinates and normed

    Keyword Arguments:
        norm --   True: plot in a normed coordinate system
    """    
    def __init__ (self, axes, modelFn, **kwargs):
        super().__init__ (axes, modelFn, **kwargs)

        # yPos limits for movement of section 
        self._leftLimit  = None
        self._rightLimit = None

        # show mouse helper / allow drag of points 
        self.section_line_artist = None
        self.chord_marker_artist = None
        self.chord_marker_anno   = None
        self.pos_marker_artist   = None
        self.pos_marker_anno     = None

    @property   
    def wing (self) -> Wing:
        return self.model

    @property
    def wingSections (self): 
        return self.wing.wingSections

    @property 
    def curSection (self) -> WingSection: 
        # find current section based on name 
        sectionName = self._curLineLabel
        sec : WingSection
        for sec in self.wingSections:
            if sec.name() == sectionName:
                return sec
        return None


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

        if self.curSection is None: 
            return
        else: 
            # coordinates of line and limits for movement by mouse 
            if self._norm: 
                y_sec, x_sec =  self.curSection.norm_line()
                self._leftLimit, self._rightLimit = self.curSection.limits_norm_yPos ()
            else:
                y_sec, x_sec =  self.curSection.line()
                self._leftLimit, self._rightLimit = self.curSection.limits_yPos ()

        # plot current section as a thick line 
        p = self.ax.plot(y_sec, x_sec, '-', color=cl_wingSection_fix,  linewidth=2.5)
        self._add (p)              # remind plot to delete 

        if self.mouseActive and not self.curSection.isRootOrTip:        # don't move root or tip

            self.show_mouseHelper (self.curSection, y_sec, x_sec)

            # make section points draggable - install callback when move is finished
            self._dragManagers.append (DragManager (self.ax, self.chord_marker_artist, 
                                        bounds=[None,None], 
                                        dependant_artists = [self.section_line_artist, self.chord_marker_anno], 
                                        callback_draw_animated = self.draw_animated_byChord,
                                        callback_on_moved=self._moveCallback))
            self._dragManagers.append (DragManager (self.ax, self.pos_marker_artist, 
                                        bounds=[None,None], 
                                        dependant_artists = [self.section_line_artist, self.pos_marker_anno], 
                                        callback_draw_animated = self.draw_animated_byPos,
                                        callback_on_moved=self._moveCallback))
            

    def show_mouseHelper (self, section, y_sec, x_sec): 
        """ show the helper points for section movement"""

        # ! animated=True must be set for all artists moving around !
        # highlight section

        p = self.ax.plot (y_sec, x_sec, '--', linewidth=0.5, color= cl_wingSection_fix, animated=True) 
        self._add(p)
        (self.section_line_artist,) = p 

        # upper marker at le - move "chord"

        p = self.ax.plot(y_sec [0], x_sec[0],  markersize=8, clip_on=False, 
                         marker=11 , color = cl_userHint, 
                         animated=True, pickradius=10)
        self._add(p)
        (self.chord_marker_artist,) = p 

        p = self.ax.annotate('move by chord', color=cl_userHint, backgroundcolor= cl_background,
                        xy=(y_sec [0], x_sec[0]), ha='left', va= 'top', fontsize = 'small',
                        xytext=(8, -5), textcoords='offset points', animated=True)
        self._add(p)
        self.chord_marker_anno = p 

        # lower marker at te - move "pos"

        x = y_sec[1]
        if self.norm:
            y = 0.022
            xytext=(8, 0)
            va = 'bottom'
            ha = 'left'
        else:
            y = x_sec[1]
            xytext= (8, -12)
            va= 'top'
            ha='left'

        p = self.ax.plot(x, y,  markersize=8, clip_on=False, 
                         marker=6, color = cl_userHint, 
                         animated=True, pickradius=10)
        self._add(p)
        (self.pos_marker_artist,) = p 

        p = self.ax.annotate('move by pos', color=cl_userHint, backgroundcolor= cl_background,
                        xy=(x, y), ha=ha, va= va, fontsize = 'small', annotation_clip=False,
                        xytext=xytext, textcoords='offset points', animated=True)
        self._add(p)
        self.pos_marker_anno = p 


    def draw_animated_byPos(self, duringMove=False): 
        """ call back when point for new position was moved"""

        # just draw marker if no move 
        if not duringMove:
            self.ax.draw_artist (self.pos_marker_artist)
            self.ax.draw_artist (self.pos_marker_anno)
            return

        # get new coordinates (when dragged) 
        xm,ym = self.pos_marker_artist.get_xydata()[0]

        # is new pos between left and right section? 
        new_yPos = xm
        new_yPos = max (self._leftLimit,  new_yPos)
        new_yPos = min (self._rightLimit, new_yPos)
        # new_yPos = round(new_yPos,1)

        # update new section pos
        if self.norm: 
            self.curSection.set_norm_yPos (new_yPos)
            y_sec, x_sec =  self.curSection.norm_line()
        else:
            self.curSection.set_yPos (new_yPos)
            y_sec, x_sec =  self.curSection.line()

        # draw new section line 
        self.section_line_artist.set_xdata(y_sec)
        self.section_line_artist.set_ydata(x_sec)
        self.ax.draw_artist (self.section_line_artist)

        # draw marker - keep marker on trailing 
        if self.norm: 
            x, y =  y_sec[1], 0.022
        else:
            x, y =  y_sec[1],x_sec[1]
        self.pos_marker_artist.set_xdata(x)
        self.pos_marker_artist.set_ydata(y)
        self.ax.draw_artist (self.pos_marker_artist)

        self.pos_marker_anno.xy =  (x,y)
        self.pos_marker_anno.set ( text="%.2f" % x)
        self.ax.draw_artist (self.pos_marker_anno)



    def draw_animated_byChord(self, duringMove=False): 
        """ call back when point for new chord length was moved"""

        # just draw marker if no move 
        if not duringMove:
            self.ax.draw_artist (self.chord_marker_artist)
            self.ax.draw_artist (self.chord_marker_anno)
            return

        # get new coordinates (when dragged) 
        xm,ym = self.chord_marker_artist.get_xydata()[0]
         
        # is new pos between left and right section 
        new_yPos = xm
        new_yPos = max (self._leftLimit,  new_yPos)
        new_yPos = min (self._rightLimit, new_yPos)

        # get chord at this position 
        if self.norm: 
            new_yPos_norm = new_yPos
        else:
            new_yPos_norm = new_yPos / self.wing.halfwingspan
        new_norm_chord = self.wing.planform.norm_chord_function(new_yPos_norm, fast=False) 
        # update new section pos
        self.curSection.set_norm_chord (new_norm_chord)
        if self.norm: 
            y_sec, x_sec =  self.curSection.norm_line()
        else:
            y_sec, x_sec =  self.curSection.line()

        # draw new section line 
        self.section_line_artist.set_xdata(y_sec)
        self.section_line_artist.set_ydata(x_sec)
        self.ax.draw_artist (self.section_line_artist)

        # draw marker - keep marker on leading edge 
        self.chord_marker_artist.set_xdata(y_sec[0])
        self.chord_marker_artist.set_ydata(x_sec[0])
        self.ax.draw_artist (self.chord_marker_artist)

        self.chord_marker_anno.xy =  (y_sec[0],x_sec[0])
        if self.norm: 
            self.chord_marker_anno.set ( text="%.2f" % self.curSection.norm_chord)
        else:
            self.chord_marker_anno.set ( text="%.1f" % self.curSection.chord)
        self.ax.draw_artist (self.chord_marker_anno)


# ----------------------------------


class Planform_Artist (Artist):
    """Plot the outline of the wing planform.
    """

    def __init__ (self, axes, modelFn, planform=None, **kwargs):
        super().__init__ (axes, modelFn, **kwargs)

        # an alternative planform - not taken from Wing
        self._planform = planform

        # show mouse helper / allow drag of points 
        self.p1_marker_artist = None
        self.p1_marker_anno = None
        self.p1_line_artist = None
        self.banana_line_artist = None 
        self.planform_line_artist = None 


    @property
    def planform (self) -> Planform:
        if self._planform is None:
            return self.model.planform
        else:
            return self._planform


    def _plot(self):
    
        y, x = self.planform.linesPolygon()
        p = self.ax.plot(y, x,  '-', color=cl_planform)
        self._add (p)
        (self.planform_line_artist,) = p 

        # hinge line
        yh, hinge = self.planform.hingeLine()
        p = self.ax.plot(yh, hinge,  '-', linewidth=0.8, color='springgreen')
        self._add (p)

        # plot banana line with mouse helper 
        if self.mouseActive and self.planform.planformType == "Bezier": 

            self.show_mouseHelper_banana (self.planform)

            # make p1,2 of Bezier draggable - install callback when move is finished
            bounds_x = ( 0.1 * self.planform.halfwingspan, 0.9 * self.planform.halfwingspan)
            bounds_y = (-0.2 * self.planform.rootchord,    0.2 * self.planform.rootchord)
            self._dragManagers.append (DragManager (self.ax, self.p1_marker_artist, 
                                        bounds=[bounds_x, bounds_y], 
                                        dependant_artists = [self.banana_line_artist, self.p1_marker_anno], 
                                        callback_draw_animated = self.draw_animated_p1,
                                        callback_on_moved=self._moveCallback))

        self._show_wingData(y, x)


    def show_mouseHelper_banana (self, planform: Planform_Bezier): 
        """ show the helper points and lines for bezier curve definition """

        # ! animated=True must be set for all artists moving around !
        x, y = planform.banana_line()
        p = self.ax.plot (x,y , '--', linewidth=0.5, color= cl_userHint, animated=True) 
        self._add(p)
        (self.banana_line_artist,) = p 

        # end points of banana bezier
        p = self.ax.plot (x[0], y[0],   marker=(3, 0, 0), fillstyle='none', color=cl_userHint, markersize=8 )
        self._add(p)
        p = self.ax.plot (x[-1], y[-1], marker=(3, 0, 0), fillstyle='none', color=cl_userHint, markersize=8 )
        self._add(p)

        # drag marker 
        x = planform.banana_p1y * planform.halfwingspan
        y = planform.banana_p1x * planform.rootchord
        p = self.ax.plot (x, y, marker='o', color=cl_userHint, markersize=6, animated=True )
        self._add(p)
        (self.p1_marker_artist,) = p 

        p = self.ax.annotate('banana', color=cl_userHint, fontsize = 'small',
                            xy=(x, y), ha='left', va= 'bottom',
                            xytext=(5, 3), textcoords='offset points', animated=True)
        self._add(p)
        self.p1_marker_anno = p 


    def draw_animated_p1(self, duringMove=False): 
        """ call back when bezier point 1 was moved"""

        # just draw planform if no move 
        if not duringMove:
            self.planform_line_artist.set_linestyle('-')
            self.ax.draw_artist (self.planform_line_artist)
            self.ax.draw_artist (self.p1_marker_artist)
            self.ax.draw_artist (self.p1_marker_anno)
            self.ax.draw_artist (self.banana_line_artist)
            return

        # draw marker and get new coordinates (when dragged) 
        self.ax.draw_artist (self.p1_marker_artist)
        x1,y1 = self.p1_marker_artist.get_xydata()[0]

        # update planform banana - ! wing coordinate system
        norm_y1 = x1 / self.planform.halfwingspan
        norm_x1 = y1 /  self.planform.rootchord

        self.planform : Planform_Bezier
        self.planform.set_banana_p1x (norm_x1)  
        self.planform.set_banana_p1y (norm_y1)  

        # because of animate=True the artist has to be provided with actual data...
        y, banana_x = self.planform.banana_line()
        self.banana_line_artist.set_xdata(y)
        self.banana_line_artist.set_ydata(banana_x)
        self.ax.draw_artist (self.banana_line_artist)

        # update planform outline  
        y, x = self.planform.linesPolygon()
        self.planform_line_artist.set_xdata(y)
        self.planform_line_artist.set_ydata(x)
        self.planform_line_artist.set_linestyle(':')
        # and plotted with special command
        self.ax.draw_artist (self.planform_line_artist)

        # update annotation text   
        self.p1_marker_anno.xy =  (x1,y1)
        self.p1_marker_anno.set ( text="height %.2f  pos %.2f" % (norm_x1, norm_y1))
        self.ax.draw_artist (self.p1_marker_anno)


    def _show_wingData (self, x, y):
        """ x,y coordinates of the closed polygon"""

        area, aspectRatio = self.planform.calc_area_AR (x,y)
        text  = "Wing span %.0f mm\n" % (self.planform.halfwingspan * 2)
        text += "Wing area %.1f dm²\n" % (area * 2 / 10000)
        text += "Aspect ratio %.1f\n" % (aspectRatio)

        p = self.ax.text (0.89, 0.05, text, color=cl_labelGrid, # fontsize = 'small',
                          transform=self.ax.transAxes, 
                          horizontalalignment='left', verticalalignment='bottom')
        self._add (p)   
       

# ----------------------------------


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

    def __init__ (self, axes, modelFn, **kwargs):
        super().__init__ (axes, modelFn, **kwargs)

        # show mouse helper / allow drag of points 
        self.p1_marker_artist  = None
        self.p1_marker_anno    = None
        self.p1_line_artist    = None
        self.p2_marker_artist  = None
        self.p2_marker_anno    = None
        self.p2_line_artist    = None
        self.chord_line_artist = None 

    
    @property
    def planform (self) -> Planform :
        return self.model.planform
    
    def chord_line (self):
        return self.model.planform.norm_chord_line ()    
    
    def _plot (self): 

        y, chord = self.chord_line ()

        p = self.ax.plot(y, chord, '-', color=self.color)
        self._add(p)
        (self.chord_line_artist,) = p 

        if self.mouseActive and isinstance(self.planform, Planform_Bezier): 

            self.show_mouseHelper_bezier (self.model.planform)

            # make p1,2 of Bezier draggable - install callback when move is finished
            self._dragManagers.append (DragManager (self.ax, self.p1_marker_artist, 
                                        bounds=[(0.1, 0.95),(0.6, 1.0)], 
                                        dependant_artists = [self.p1_line_artist, self.p1_marker_anno], 
                                        callback_draw_animated = self.draw_animated_p1,
                                        callback_on_moved=self._moveCallback))
            self._dragManagers.append (DragManager (self.ax, self.p2_marker_artist, 
                                        bounds=[(1, 1),(0.05, 0.95)], 
                                        dependant_artists = [self.p2_line_artist, self.p2_marker_anno], 
                                        callback_draw_animated = self.draw_animated_p2,
                                        callback_on_moved=self._moveCallback))


    def show_mouseHelper_bezier (self, planform: Planform): 
        """ show the helper points and lines for bezier curve definition """

        # ! animated=True must be set for all artists moving around !

        # Bezier p1 tangent line
        p = self.ax.plot (planform._py[0:2], planform._px[0:2], '--', linewidth=0.5, color= cl_userHint, animated=True) 
        self._add(p)
        (self.p1_line_artist,) = p 
        p = self.ax.plot (planform._py[0]+0.002, planform._px[0], marker=(3, 0, 0), fillstyle='none', color=cl_userHint, markersize=8 )
        self._add(p)

        # Bezier p1 marker and annotation
        p = self.ax.plot (planform._py[1], planform._px[1], marker='o', color=cl_userHint, markersize=6, animated=True )
        self._add(p)
        (self.p1_marker_artist,) = p 
        p = self.ax.annotate('root tangent', color=cl_userHint, fontsize='small',
                            xy=(planform._py[1], planform._px[1]), ha='left', va='center',
                            xytext=(8, 0), textcoords='offset points', animated=True)
        self._add(p)
        self.p1_marker_anno = p 

        # Bezier p2 tangent line
        p = self.ax.plot (planform._py[2:], planform._px[2:], '--', linewidth=0.5, color= cl_userHint)    
        self._add(p)
        (self.p2_line_artist,) = p 
        p = self.ax.plot (planform._py[3], planform._px[3],marker=(3, 0, 0), fillstyle='none', color=cl_userHint, markersize=8 )
        self._add(p)

        # Bezier p2 marker and annotation
        p = self.ax.plot (planform._py[2], planform._px[2], marker='o', color=cl_userHint, markersize=6)
        self._add(p)
        (self.p2_marker_artist,) = p 
        p = self.ax.annotate('tip tangent', color=cl_userHint, fontsize='small',
                            xy=(planform._py[2], planform._px[2]), ha='center', va= 'bottom',
                            xytext=(0, 5), textcoords='offset points', animated=True)
        self._add(p)
        self.p2_marker_anno = p 



    def draw_animated_p1(self, duringMove=False): 
        """ call back when bezier point 1 was moved"""

        self.planform : Planform_Bezier
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

        # update annotation
        if duringMove: 
            angle  = self.planform.tangentAngle_root
            length = self.planform.tangentLength_root
            self.p1_marker_anno.xy =  (x1,y1)
            self.p1_marker_anno.set ( text="angle %.1f  length %.2f" % (angle, length))
        self.ax.draw_artist (self.p1_marker_anno)


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

        # update annotation
        if duringMove: 
            angle  = self.planform.tangentAngle_tip
            length = self.planform.tangentLength_tip
            self.p2_marker_anno.xy =  (x2,y2)
            self.p2_marker_anno.set ( text="angle %.1f  length %.2f" % (angle, length))
        self.ax.draw_artist (self.p2_marker_anno)


    def draw_animated_chord (self, duringMove=False):

        y, chord = self.chord_line ()

        # because of animate=True the artist has to be provided with actual data...
        self.chord_line_artist.set_xdata(y)
        self.chord_line_artist.set_ydata(chord)
        if duringMove:
            self.chord_line_artist.set_linestyle(':')
        else:
            self.chord_line_artist.set_linestyle('-')
        # and plotted with special command
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
            else:
                y, le_to_te = section.line()

            color = cl_wingSection_fix
            linewidth= 1.0
            linestyle='solid'

            label = section.name()

            # if not (section.isRootOrTip and self._norm): 
            p = self.ax.plot(y, le_to_te, color=color, label=label, linestyle=linestyle, 
                            linewidth=linewidth)
            self._add (p)              # remind plot to delete 

            if self._pickActive: 
                self._makeLinePickable (p)

            self.plot_markers (y, le_to_te, section)

        # activate event for clicking on line 
        if self._pickActive: 
            self._connectPickEvent ()
            if self.mouseActive:      
                self.show_mouseHelper ()


    def show_mouseHelper (self):
        """ show info for section select"""

        if len(self.wingSections) > 2:
            iSec = -(-len(self.wingSections) // 2) - 1                # trick to round up

            if self.norm:
                y_sec, x_sec = self.wingSections [iSec].norm_line()
            else:
                y_sec, x_sec = self.wingSections [iSec].line()

            p = self.ax.annotate('click to select', color=cl_userHint, fontsize = 'small',
                    xy=(y_sec [0], (x_sec[0] + x_sec[1])/3), ha='left', va= 'top',
                    xytext=(6, 0), textcoords='offset points')
            self._add(p)



    def plot_markers (self, y, le_to_te, section: WingSection): 

        sectionFix = section.hasFixedPosition()

        # section chord - print along chord
        if self._norm:
            # if section.isRoot: return               # no norm_chord for root
            text = "%.2f" % (section.norm_chord)
            marker_x = (le_to_te[0] + le_to_te[1]) * 0.40
            marker_y = y[0] + 0.007
        else: 
            text = "%.0f" % section.chord
            marker_x = le_to_te[1] - (le_to_te[1] - le_to_te[0]) * 0.40
            marker_y = y[0] + 6

        if not sectionFix:                          # fixed chord 
            text = text + " fix"

        color = cl_wingSection_fix

        p = self.ax.text (marker_y, marker_x, text, ha='left',color = color, rotation=90 )
        self._add (p)   

        # section pos at bottom  
        if not section.isRootOrTip: 
            if self._norm:
                marker_y = 0.06                             # in axis coordinates
                text = "%.2f" % (section.norm_yPos)
                if sectionFix:                              # fixed position 
                    text = "fix\n" + text 
            else: 
                marker_y = 0.02                             # in axis coordinates
                text = "%.0f" % section.yPos
                if sectionFix:                              # fixed position 
                    text = "fix\n" + text 

            color = cl_wingSection_fix
            marker_x = y[0]                                 # in data coordinates

            p = self.ax.text (marker_x, marker_y, text, color=color, backgroundcolor= cl_background, 
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
        if self._pickActive: self._connectPickEvent ()

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
