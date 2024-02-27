#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    The Planform Creator 2 App 

    Object model overview (a little simplified) 

    App                                         - root frame 
        |-- Edit_File_Menu                      - file level functions
                |-- Dialog_Export_Xflr5_Flz     - handle export to Xflr5 and FLZ_vortex 
                |-- Dialog_Export_Dxf           - handle export to DXF file 
                |-- Dialog_Load_DXF             - import DXF as reference or a main planform  
        |-- Edit                                - parent frame for  edit sub frames 
                |-- Edit_Wing                   - main wing data 
                |-- Edit_Wing_Planform          - parameters for a specific planform type 
                |-- Edit_WingSection            - select and edit a single wing section 
                        :
                        |-- Widgets             - wrapper for CTk widgets - get, set their data 
                        |-- Field_Widget        - entry field with label and spin buttons
                        |-- Header_Widget       - a page header  
                        ...                     - ...

        |-- Diagrams                            - the master to select one of the diagrams
                |-- Diagram_Planform            - the planform (outline) of a half wing 
                |-- ChordDistribution           - normalized chord distribution of the wing
                |-- Airfoils                    - the airfoils at the wing sections 
                        :
                        |-- Artists             - helper to plot a wing object on a matplotlib axes
                        |-- PlanformArtist      - plots the planform 
                        |-- SectionArtist       - plots the wing sections 
                        ...                     - ...

"""
import os
import sys
import argparse
from pathlib import Path

import matplotlib.pyplot as plt

from tkinter import filedialog
import customtkinter as ctk

# let python find the other modules in modules relativ to path of self  
sys.path.append(os.path.join(Path(__file__).parent , 'modules'))

from common_utils       import * 
from wing_model         import Planform, Planform_Bezier_StraightTE, \
                                       Planform_DXF, Planform_Trapezoidal, Planform_Bezier         

from ui_base            import *        
from widgets            import * 
from wing_artists       import *


#------------------------------------------------

AppName    = "Planform Creator 2"
AppVersion = "1.2.1"

#------------------------------------------------

cl_background               = ('#EBEBEB','#242424')                     # background color in diagrams

#   change events for updating mainly plots

WING_NEW                    = "<<WING_NEW>>"                #tk event types
WING_CHANGED                = "<<WING_CHANGED>>"             
WING_CHANGED_BY_MOUSE       = "<<WING_CHANGED_BY_MOUSE>>"
CHORD_CHANGED               = "<<CHORD_CHANGED>>"             
CHORD_CHANGED_BY_MOUSE      = "<<CHORD_CHANGED_BY_MOUSE>>"             
PLANFORM_CHANGED            = "<<PLANFORM_CHANGED>>"
PLANFORM_CHANGED_BY_MOUSE   = "<<PLANFORM_CHANGED_BY_MOUSE>>"
SECTION_CHANGED             = "<<SECTION_CHANGED>>"
CURRENT_SECTION_CHANGED     = "<<CURRENT_SECTION_CHANGED>>"
DIAGRAM_SECTION_SELECTED    = "<<DIAGRAM_SECTION_SELECTED>>"
AIRFOIL_CHANGED             = "<<AIRFOIL_CHANGED>>"
PANELS_CHANGED              = "<<PANELS_CHANGED>>"


def fireEvent(ctk_root : ctk.CTkToplevel, eventType): 
    """ fire event for the current ctk_root toplevel widget """
    if not ctk_root is None: 
        ctk_root.event_generate (eventType) 


#-------------------------------------------------------------------------------
# Edit Frames    
#-------------------------------------------------------------------------------

class Edit(ctk.CTkFrame):
    """ 
    Master frame for user input 
    """
    # - holds the current data object wing, planform, wing secttion
    # - Handles the interaction between the different sub frames
    # - informs the diagrams about changes 
    #
    def __init__(self, master : 'App', wingFn, *args, **kwargs):
        super().__init__(master, *args, **kwargs)

        self.myApp   = master
        self._wingFn = wingFn

        # root for change events (widgets will have the same toplevel as root)
        self.ctk_root = self.winfo_toplevel()

        self.curSection = self.wing.wingSections[0]

        self.editWing_frame          = Edit_Wing               (self, wingFn, width=300)
        self.editPlanformType_frame  = Edit_Planform_Master    (self, wingFn, width=500)
        self.editSectionMaster_frame = Edit_WingSection_Master (self, wingFn)

        # edit maingrid 1 x 4  with subframes

        self.grid_rowconfigure      (0, weight=1)
        self.grid_columnconfigure   (0, weight=0)
        self.grid_columnconfigure   (1, weight=0) #, minsize=400
        self.grid_columnconfigure   (2, weight=0) #, minsize=400
        self.grid_columnconfigure   (3, weight=1, minsize=400)

        self.editWing_frame.grid          (row=0, column=1, pady=(0,5), padx=(5,0), sticky="news")
        self.editPlanformType_frame.grid  (row=0, column=2, pady=(0,5), padx=(5,0), sticky="news")
        self.editSectionMaster_frame.grid (row=0, column=3, pady=(0,5), padx=(5,5), sticky="news")

        # major bindings for change management

        self.ctk_root.bind(WING_NEW,                 self.new_wing, add='+')
        self.ctk_root.bind(WING_CHANGED,             self.changed_wing, add='+')
        self.ctk_root.bind(CHORD_CHANGED,            self.changed_chord, add='+')
        self.ctk_root.bind(CHORD_CHANGED_BY_MOUSE,   self.changed_chord, add='+')
        self.ctk_root.bind(PLANFORM_CHANGED,         self.changed_planform, add='+')
        self.ctk_root.bind(SECTION_CHANGED,          self.changed_section, add='+')

        self.ctk_root.bind(PLANFORM_CHANGED_BY_MOUSE,self.changed_planform_by_mouse, add='+')
        self.ctk_root.bind(WING_CHANGED_BY_MOUSE,    self.changed_wing, add='+')

        # bindings from plot diagrams 
        self.ctk_root.bind(DIAGRAM_SECTION_SELECTED,self.sectionSelected_in_diagram, add='+')

    # ------ properties ------

    @property
    def wing(self) -> Wing:
        return self._wingFn()
    
    @property
    def planform(self) -> Planform:
        return self.wing.planform

    # ------ event handler ---

    def new_wing (self, event): 
        """ Eventhandler for a new wing being set"""
        # refresh all 
        self.editWing_frame.refresh()
        self.editPlanformType_frame.refresh()    
        self.editSectionMaster_frame.reset()     

    def changed_wing (self, event): 
        """ Eventhandler for changes of major wing data"""
        # doesn't change planform data
        # could change wing sections (a new dxf file which could change hinge and flaps)
        self.editWing_frame.refresh()
        self.editSectionMaster_frame.refresh_current()    # e.g. rootchord or re at root ... 

    def changed_chord (self, event): 
        """ Eventhandler for changes of major wing data"""
        # doesn't change planform data
        # could change / move wing sections
        if self.planform.sections_depend_on_planform:
            # refesh fields of current section
            self.editSectionMaster_frame.refresh_current()    # e.g. section position (wingSpan) 
            self.editPlanformType_frame.refresh()             # Bezier parameters


    def changed_planform_by_mouse (self, event): 
        """ Eventhandler for changes of planform by mouse in diagram e.g. banana"""

        self.editPlanformType_frame.refresh()                   # banana parameters      


    def changed_planform (self, event): 
        """ Eventhandler for changes of planform data"""

        self.editSectionMaster_frame.changed_planform()          # 'Defines Planform' (trapezoid) 


    def changed_section (self, event): 
        """ Eventhandler for changes of section data"""
        # doesn't change wing data
        # could change planform
        if self.planform.planform_depend_on_sections:
            self.editPlanformType_frame.refresh()   # e.g. chord changes section position 
        # refresh the other fields of current section
        self.editSectionMaster_frame.refresh_current()    # e.g. section position (wingSpan) 

    def sectionSelected_in_diagram (self, event): 
        """ Eventhandler for changes of the current wing section selection in diagrams"""
        # refresh the wing section edit frame with new section
        self.editSectionMaster_frame.set_curSection (self.myApp.curWingSectionName(), inEvent=True)


#-------------------------------------------


class Edit_Abstract_Wing (Edit_Abstract):
    """ 
    Abstract superclass for all the edit like frames
    """

    def __init__(self, master, wingFn, *args, **kwargs):

        self._wingFn = wingFn               # the function to the wing (indirect because of new wing could be set)

        super().__init__(master, *args, **kwargs)

    def wing(self) -> Wing:
        # it's a method - not a property to exchange the wing 
        return self._wingFn()
    
    def planform(self) -> Planform:
        return self.wing().planform
    
        
#-------------------------------------------

class Edit_Wing(Edit_Abstract_Wing):
    """ 
    Frame to edit main data of the wing like wingspan. This is just the header
    """
    name = "Wing"

    def init (self):

        self.grid_rowconfigure      (1, weight=1)
        self.grid_columnconfigure   (4, weight=1)
               
        self.add (Header_Widget (self,0,0, lab=self.name, width=105))
        self.add (Field_Widget  (self,0,1, lab=None, obj=self.wing, get='name', set='set_name',
                                 lab_width=1, event=WING_CHANGED, width=200, justify='left'))


        self.dataFrame = Edit_Wing_Data (self, self._wingFn, fg_color='transparent')
        self.dataFrame.grid (row=1, column=0, columnspan=5, pady=0, padx=(10,5), sticky="news")

    def refresh(self):
        # overloaded to refresh also child frame with data
        super().refresh()
        self.dataFrame.refresh()

        
#-------------------------------------------

class Edit_Wing_Data (Edit_Abstract_Wing):
    """ 
    Frame to edit main data of the wing like wingspan ...
    """
    name = "Wing"

    def init (self):

        unit = self.wing().unit

        r = 0         
        self.add (Field_Widget  (self,r,0, lab="Wing span",     obj=self.wing, get='wingspan', set='set_wingspan',
                                 event=WING_CHANGED, lim=(100,20000), dec=1, spin= True, step=10, unit=unit))
        r += 1
        self.add (Field_Widget  (self,r,0, lab="Chord at root", obj=self.wing, get='rootchord', set='set_rootchord',
                                 event=WING_CHANGED, lim=(10,1000), dec=1, spin=True, step=1, unit=unit))
        self.add (Field_Widget  (self,r,3, lab="Chord at tip",  obj=self.wing, get='tipchord', set='set_tipchord',
                                 event=CHORD_CHANGED,               
                                 lim=(1,800), dec=1, spin=True, step=1, unit=unit))
        r += 1
        self.add (Field_Widget  (self,r,0, lab="Flap at root",  obj=self.wing, get='flapDepthRoot', set='set_flapDepthRoot',
                                 event=WING_CHANGED, lim=(0,80), dec=1, spin=True, step=0.1, unit='%'))
        self.add (Field_Widget  (self,r,3, lab="Flap at tip",   obj=self.wing, get='flapDepthTip', set='set_flapDepthTip',
                                 event=WING_CHANGED,            
                                 lim=(0,80), dec=1, spin=True, step=0.1, unit='%'))
        r += 1
        self.add (Field_Widget  (self,r,0, lab="Hinge angle",   obj=self.wing, get='hingeAngle', set='set_hingeAngle',
                                 event=WING_CHANGED, lim=(-10,45), dec=2, spin=True, step=0.1, unit="°"))
        r += 1
        Blank_Widget (self,r,0, width=20, height = 15) 
        r += 1
        self.add (Field_Widget  (self,r,0, lab="Re at root",    obj=self.wing, get='rootRe', set='set_rootRe',
                                 event=WING_CHANGED, dec=0, spin=True, step=1000))

        # a mini frame to bring the two nick fields together
        self.nick_frame = ctk.CTkFrame(self, fg_color="transparent")
        self.nick_frame.grid    (row=r, column=3, columnspan=3, sticky="nwes")

        self.add (Field_Widget  (self.nick_frame,0,0, lab="Airfoils nick", obj=self.wing, get='airfoilNickPrefix', set='set_airfoilNickPrefix',
                                 event=SECTION_CHANGED, width= 65))
        self.add (Field_Widget  (self.nick_frame,0,3,  obj=self.wing().rootSection, get='airfoilNickPostfix', 
                                 disable=True, width= 40))
        


#-------------------------------------------

class Edit_Planform_Master(Edit_Abstract_Wing):
    """ 
    Master/Header frame for planform type specific sub frames
    """
    name = "Planform"

    def init (self):

        self.currentPlanform_frame = None

        # main grid:  header - sub Frame for specific planform 
        self.grid_columnconfigure   (2, weight=1)
        self.grid_rowconfigure      (3, weight=1)

        self.add(Header_Widget (self,0,0, lab=self.name, width=105))
        self.add(Option_Widget (self,0,1, get='planformType', set = 'set_planformType',
                                width = 150, options=Planform.allTemplatePlanformTypes(),
                                event=PLANFORM_CHANGED))

        self.add(Label_Widget  (self,1, 0, lab= self.shortDescription))
        self.add(Blank_Widget  (self,2, 0))

        # set inital planform to edit 
        self.set_planform_frame (self.wing().planformType)

    def shortDescription (self): 
        return self.wing().planform.shortDescription

    @property
    def planformType (self): 
        return self.wing().planformType

    def set_planformType (self, aType):
        """ set a new planform selected by user
        """

        if self.wing().planformType != aType:
            try:
                self.wing().set_planformType (aType) 
                handled = True
            except:
                handled = self.handle_Planform_DXF_notValid ()

            if handled: 
                self.set_planform_frame (aType)
            self.refresh()                      # refresh planform description

    def set_planform_frame (self, newPlanformType):
        """ 
        set the edit planform frame for a specific planform
        if it's already set - do nothing
        """
        curFrame = self.currentPlanform_frame

        if curFrame is None or newPlanformType != curFrame.name:
            if not curFrame is None:
                curFrame.grid_forget()              # remove current 
                curFrame.destroy()

            if(newPlanformType =="Bezier"): 
                curFrame = Edit_Planform_Bezier(self, self._wingFn, fg_color='transparent')
            elif(newPlanformType =="trapezoidal"): 
                curFrame = Edit_Planform_Trapezoid(self, self._wingFn, fg_color='transparent')
            elif(newPlanformType =="Bezier TE straight"): 
                curFrame = Edit_Planform_Bezier_StraightTE(self, self._wingFn, fg_color='transparent')
            elif(newPlanformType =="DXF file"): 
                curFrame = Edit_Planform_DXF(self, self._wingFn, fg_color='transparent')
            else:
                raise ValueError ("No Edit_Frame for planform type ", newPlanformType)

            self.currentPlanform_frame = curFrame
            curFrame.grid (row=3, columnspan=3, pady=0, padx=(10,5), sticky="news")


    def handle_Planform_DXF_notValid (self): 

        # the current dxf planform doesn't have a valid planform 
        # aks for a new dxf file

        dxf_dialog = Dialog_Load_DXF (self, wingFn = self.wing, dxf_Path=None, ref=False, workingDir = self.workingDir) 

        self.wait_window (dxf_dialog)
   
        dxf_Path = dxf_dialog.dxf_pathFilename
        if dxf_Path:  
            self.wing().set_planform (Planform_DXF( self.wing(), dxf_Path= dxf_Path, ref = False))
            handled = True
            fireEvent (self.ctk_root, WING_CHANGED)                   # update hinge, flaps
        else: 
            handled = False

        return handled
    
    def refresh(self):
        super().refresh()
        # new wing - also new planformType? 
        self.set_planform_frame (self.planformType)
        self.currentPlanform_frame.refresh()



#-------------------------------------------


class Edit_Planform_Bezier(Edit_Abstract_Wing):
    """ 
    Frame to edit the parameters of a Bezier based planform
    """
    name = Planform_Bezier.planformType

    def planform(self) -> Planform_Bezier:
        return self.wing().planform

    def init(self):

        self.grid_columnconfigure   (0, weight=0)
        self.grid_rowconfigure      (6, weight=1)

        r = 0 
        self.add (Field_Widget  (self,r,0, lab="Root tangent", obj=self.planform, 
                                    get='tangentAngle_root', set='set_tangentAngle_root', unit='°',
                                    event=CHORD_CHANGED, lim=(-20,0), dec=1, spin=True, step=0.1))
        self.add (Field_Widget  (self,r,3, lab="length", lab_width=60, obj=self.planform, 
                                    get='tangentLength_root', set='set_tangentLength_root',
                                    event=CHORD_CHANGED, lim=(0.1,1), dec=2, spin=True, step=0.01))
        r += 1
        self.add (Field_Widget  (self,r,0, lab="Tip tangent", obj=self.planform, 
                                    get='tangentAngle_tip', unit='°',
                                    event=CHORD_CHANGED, dec=1, spin=True))
        self.add (Field_Widget  (self,r,3, lab="length", lab_width=60, obj=self.planform, 
                                    get='tangentLength_tip', set='set_tangentLength_tip',
                                    event=CHORD_CHANGED, lim=(0.1,1), dec=2, spin=True, step=0.01))
        r += 1
        self.add(Button_Widget (self,r,1, lab='Set elliptical', width=110, columnspan=2, 
                                    style=SUPTLE, set=self.set_elliptical, padx=2, pady=(5,15)))

        r += 1
        self.add (Field_Widget  (self,r,0, lab="Banana height", obj=self.planform, 
                                    get='banana_p1x', set='set_banana_p1x',  
                                    event=CHORD_CHANGED, lim=(-0.1,0.1),dec=2, spin=True, step=0.01))
        self.add (Field_Widget  (self,r,3, lab="position", lab_width=60, obj=self.planform, 
                                    get='banana_p1y', set='set_banana_p1y',
                                    event=CHORD_CHANGED, lim=(0.1,0.9), dec=2, spin=True, step=0.01))
        

    def set_elliptical (self): 
        """ set bezier planform t be elliptical"""
        self.planform().set_elliptical()            # the 'logic' is handled in model object Planform_Bezier
        self.refresh ()                             # refresh all widgets of self (push)
        fireEvent (self.ctk_root, CHORD_CHANGED)    # refresh diagram (separated from edit frames)


class Edit_Planform_Trapezoid (Edit_Abstract_Wing):
    """ 
    Frame to edit the parameters of a trapezoid planform
    """
    name = Planform_Trapezoidal.planformType

    def planform(self) -> Planform_Trapezoidal:
        return self.wing().planform

    def init(self):

        self.grid_columnconfigure   (0, weight=0)
        self.grid_columnconfigure   (1, weight=1)
        Blank_Widget (self,0,0,  height= 20)

        self.add(Button_Widget (self,1,0, lab='Adapt', width=70, set=self.adaptSections ))
        self.add(Label_Widget  (self,1,1, lab='wing sections to elliptical planform'))

    def adaptSections (self):
        # changes chord of sections to best fit to reference (elliptical) 
        self.planform().adjust_planform_to_reference()
        fireEvent (self.ctk_root, PLANFORM_CHANGED)


#-------------------------------------------


class Edit_Planform_Bezier_StraightTE (Edit_Abstract_Wing):
    """ 
    Frame to edit the parameters of a Bezier planform with straight TE
    """
    name = Planform_Bezier_StraightTE.planformType

    def planform(self) -> Planform_Bezier_StraightTE:
        return self.wing().planform

    def init(self):

        r = 0 

        self.add (Field_Widget  (self,r,0, lab="Root tangent", obj=self.planform, 
                                    get='tangentAngle_root', set='set_tangentAngle_root', unit='°',
                                    event=CHORD_CHANGED, lim=(-20,10), dec=1, spin=True, step=0.5))
        self.add (Field_Widget  (self,r,3, lab="length", lab_width=60, obj=self.planform, 
                                    get='tangentLength_root', set='set_tangentLength_root',
                                    event=CHORD_CHANGED, lim=(0.1,1), dec=2, spin=True, step=0.01))
        r += 1
        self.add (Field_Widget  (self,r,0, lab="Tip tangent", obj=self.planform, 
                                    get='tangentAngle_tip', unit='°',
                                    event=CHORD_CHANGED, dec=1, spin=True, step=0.5))
        self.add (Field_Widget  (self,r,3, lab="length", lab_width=60, obj=self.planform, 
                                    get='tangentLength_tip', set='set_tangentLength_tip',
                                    event=CHORD_CHANGED, lim=(0.1,1), dec=2, spin=True, step=0.01))


#-------------------------------------------

class Edit_Planform_DXF (Edit_Abstract_Wing):
    """ 
    Frame to edit the parameters of a planform based on DXF
    """
    name = Planform_DXF.planformType

    def init(self):

        # self.grid_columnconfigure   (0, weight=1)
        self.grid_columnconfigure   (2, weight=1)
        self.grid_rowconfigure      (0, weight=0)
        self.grid_rowconfigure      (5, weight=1)

        self.add(Field_Widget  (self,1,0, lab='Current file',   width= 150, get= self.dxf_filename))
        self.add(Button_Widget (self,1,2, lab='Select',         width=90,   set= self.open_dxf_file ))


        Blank_Widget (self,3,0)
        self.add(Label_Widget  (self,4,0, lab=lambda: self.planform().infoText ))
        Blank_Widget (self,5,0)

    def dxf_filename(self): 
        return self.planform().dxf_filename()

    def open_dxf_file (self):

        current_dxf_path = self.planform().dxf_pathFilename

        dxf_dialog = Dialog_Load_DXF (self, wingFn = self.wing, dxf_Path = current_dxf_path,  workingDir = self.workingDir) 

        self.wait_window (dxf_dialog)

        if dxf_dialog.return_OK:
            new_dxf_Path = dxf_dialog.dxf_pathFilename        
            if new_dxf_Path:                                    # dialog returned a valid path 
                self.wing().planform.set_dxfPathFilename (new_dxf_Path) 
                self.wing().planform.assignToWing()             # take over hinge, flap

                self.refresh()
                fireEvent (self.ctk_root, WING_CHANGED)



class Edit_WingSection_Master(Edit_Abstract_Wing):
    """ 
    Master frame for wingSection - to select, add, delete a wing section
    Is parent for a single wing section frame
    """
    name = "Section"

    @property
    def myApp (self) -> 'App':
        return super().myApp
    
    def init(self):

        self.curSectionFrame = None

        # get initial section from app 
        self.curSection = None
        self.set_curSection (self.myApp.curWingSectionName(), initial=True)

        # main grid:  header frame + - sub Frame for one section  
        hfrm = ctk.CTkFrame(self, fg_color='transparent')
        self.grid_columnconfigure   (1, weight=1)
        self.grid_rowconfigure      (1, weight=1)

        self.add(Header_Widget (hfrm,0,0, lab=self.name, width=105))

        self.add(Option_Widget (hfrm,0,1,   get=self.curSectionName, set=self.set_curSection,
                                            spin=True, width=100, options=self.sectionNames))
        self.add(Button_Widget (hfrm,0,2,   lab=' Add ',  width=50,  set=self.addSection,
                                            disable=self.addDisabled))
        self.add(Button_Widget (hfrm,0,3,   lab='Delete',   width=50, set=self.deleteSection,
                                            disable=self.deleteDisabled))

        hfrm.grid (row=0, column=0, pady=0, padx=0, sticky="w")


    def curSectionName(self): return self.curSection.name()
    def sectionNames(self): 
        return [s.name() for s in self.wing().wingSections]   
    
    def deleteSection (self):
        """ delete the current, selected wing Section""" 
        if not self.deleteDisabled():
            # remind the neighbour before this section 
            leftSec, rightSec = self.wing().getNeighbourSectionsOf (self.curSection)
            self.wing().deleteSection (self.curSection)
            fireEvent (self.ctk_root, SECTION_CHANGED)         # update diagram
            # delete done - set option list to old neighbour 
            self.set_curSection(leftSec.name())

    def deleteDisabled (self):
        return self.curSection.isRoot or self.curSection.isTip
    

    def addSection (self):
        """ add a section after current Section""" 
        if not self.addDisabled():
            newSection = self.wing().createSectionAfter (self.curSection)
            fireEvent (self.ctk_root, SECTION_CHANGED)         # update diagram
            self.set_curSection(newSection.name())

    def addDisabled (self):
        return self.curSection.isTip

    def set_curSection (self, aName, inEvent=False, initial=False):
        """ 
        set the current section to edit in child frame
        if it's already set - do nothing
        """
        if aName is None: return

        self.curSection = self.get_SectionFromName (aName)

        # create new frame for current (new) wing section 
        if not self.curSectionFrame:
            self.curSectionFrame = Edit_WingSection(self, self._wingFn, lambda: self.curSection, fg_color='transparent')
            self.curSectionFrame.grid (row=1, column=0, pady=0, padx=(10,5), sticky="nwes")
        else:
            # self.curSectionFrame.set_wingSection (self.curSection)
            self.curSectionFrame.refresh()
            self.refresh()                          # update prev/next buttons  / option widget

        # inform diagram 
        self.myApp.set_curWingSectionName(aName)
        if not inEvent and not initial:                         # avoid event ping pong
            fireEvent (self.ctk_root, CURRENT_SECTION_CHANGED)    


    def get_SectionFromName (self, aName) -> WingSection:
        """ return the section with "aName"
        """
        for sec in self.wing().wingSections:
            if (sec.name() == aName): return sec
        raise ValueError ("Wing section not found: ", aName )
    
    def changed_planform (self): 
        """ refresh when planform changed """

        if self.curSectionFrame is not None:
            self.curSectionFrame.grid_forget()              # remove current 
            self.curSectionFrame.destroy()
        self.curSectionFrame = Edit_WingSection(self, self._wingFn, lambda: self.curSection, fg_color='transparent')
        self.curSectionFrame.grid (row=1, column=0, pady=0, padx=(10,5), sticky="nwes")


    def refresh_current (self):
        # refresh data of current section
        self.curSectionFrame.refresh()

    def reset (self):
        """reset everything - needed when a new wing is loaded """
        self.set_curSection (self.wing().wingSections[0].name())


class Edit_WingSection(Edit_Abstract_Wing):
    """ 
    Frame to edit the parameters of a single wing section 
    """
    name = "Wing Section"

    def __init__(self, master, wingFn, curSectionFunction, *args, **kwargs):

        self._curSectionFunction = curSectionFunction
        super().__init__(master, wingFn, *args, **kwargs)


    def wingSection (self) -> WingSection:
        return self._curSectionFunction()


    def init (self):

        unit = self.wingSection().wing.unit

        # In this frame the dataObject (wingSection) changes during lifetime.
        # There the values can't be accessd via a 'bound method' in 'set' of the widget,
        # but is has to be a string which is evaluated during runtime

        self.add(Field_Widget  (self,1,0, lab="Position", obj=self.wingSection, get= 'yPos', set='set_yPos',
                                                lim='limits_yPos', dec=1, spin=True, step=2, unit=unit,
                                                disable='isRootOrTip', event=SECTION_CHANGED))
        self.add(Field_Widget  (self,1,3, lab="Position rel.", obj=self.wingSection, get='norm_yPos', set='set_norm_yPos',
                                                lim='limits_norm_yPos', dec=3, spin=True, step=0.01, unit='%',
                                                disable='isRootOrTip', event=SECTION_CHANGED))

        self.add(Field_Widget  (self,2,0, lab="Chord", obj=self.wingSection, get='chord', set='set_chord',
                                                lim='limits_chord', dec=1, spin=True, step=0.5, unit=unit,
                                                disable='isRootOrTip', event=SECTION_CHANGED))
        self.add(Field_Widget  (self,2,3, lab="Chord rel.", obj=self.wingSection, get='norm_chord', set='set_norm_chord',
                                                lim='limits_normChord', dec=3, spin=True, step=0.01, unit='%',
                                                disable='isRootOrTip', event=SECTION_CHANGED))

        self.add(Field_Widget  (self,3,0, lab="Reynolds", obj=self.wingSection ,get='Re', set='set_Re',
                                                lim='limits_Re', dec=0, spin=True, step=1000,
                                                disable='isReDisabled', event=SECTION_CHANGED))

        if self.wing().planform.planform_depend_on_sections:
            self.add(Switch_Widget (self,3,3, lab="Defines planform", obj=self.wingSection , padx=5, columnspan=3,
                                                get='hasFixPosChord', set='set_hasFixPosChord',
                                                disable='isSet_eitherPosOrChord_disabled', event=SECTION_CHANGED))

        Blank_Widget (self,4,0, width=20, height = 10) 

        self.add(Field_Widget  (self,5,0, lab="Airfoil", width=110, get=lambda: self.wingSection().airfoil.name, 
                                disable=True, event=SECTION_CHANGED))
        
        self.add(Button_Widget (self,5,2, lab='Select', width=60, columnspan=2, sticky='w',style=SUPTLE, 
                                padx=(3,10),set=self.select_airfoil ))
        self.add(Button_Widget (self,5,2, lab='Edit',   width=60, columnspan=2, sticky='e', style=SUPTLE,
                                set=self.edit_airfoil, disable=self.edit_airfoil_disable ))
        self.add(Button_Widget (self,5,4, lab='Remove', width=60, columnspan=3, sticky='w', style=SUPTLE,
                                set=self.remove_airfoil, disable=self.remove_airfoil_disable ))

        self.add(Field_Widget  (self,6,0, lab="Airfoil nick", obj=self.wingSection ,get='airfoilNick', 
                                                disable=True, width=110))
        Blank_Widget (self,7,0, width=20, height = 10) 
        self.add(Field_Widget  (self,8,0, lab="Flap group", obj=self.wingSection ,get='flapGroup', set='set_flapGroup',
                                                lim=(0,9), dec=0, spin=True, step=1,
                                                disable='isTip', event=SECTION_CHANGED))

        self.grid_columnconfigure   (5, weight=1)

    
    def select_airfoil(self):
        """ select airfoil with explorer and load it if possible """

        filetypes  = [('dat files', '*.dat')]
    
        newPathFilename = filedialog.askopenfilename(
                    title='Select airfoil file',
                    initialdir=self.workingDir,
                    filetypes=filetypes)
        if newPathFilename: 
            self.wingSection().set_airfoilWithPathFileName(newPathFilename)
            self.refresh()
            fireEvent (self.ctk_root, AIRFOIL_CHANGED)

    def edit_airfoil(self):
        """ edit airfoil with AirfoilEditor - a new, modified airfoil may be returned"""

        from AirfoilEditor import AirfoilEditor

        relPathFileName = self.wingSection().airfoil.pathFileName
        ph = PathHandler (workingDir=self.workingDir)
        absPathFileName = ph.fullFilePath (relPathFileName)

        dialog = AirfoilEditor (absPathFileName, parentApp=self.myApp)

        if dialog.return_OK:                    # no early abort
            self.wait_window (dialog.main)
    
            if dialog.return_OK:                # user pressed ok
                relPathFileName = ph.relFilePath(dialog.return_newAirfoilPathFileName)
                self.wingSection().set_airfoilWithPathFileName(relPathFileName)
                self.refresh()
                fireEvent (self.ctk_root, AIRFOIL_CHANGED)
        else: 
            Messagebox (self,title="Edit airfoil", message= "Airfoil\n\n'%s'\n\ndoesn't exist anymore."%absPathFileName,
                        icon="cancel", option_1="Close")


    def edit_airfoil_disable (self):
        return not self.wingSection().airfoil_canBeEdited()


    def remove_airfoil(self):

        if self.wingSection().airfoil_canBeRemoved():
            self.wingSection().set_airfoilWithPathFileName(None)
            self.refresh()
            fireEvent (self.ctk_root, AIRFOIL_CHANGED)
        
    def remove_airfoil_disable (self):
        return not self.wingSection().airfoil_canBeRemoved()


#-------------------------------------------------------------------------------
# Diagrams   
#-------------------------------------------------------------------------------

class Diagrams(ctk.CTkTabview):
    """ 
    Master frame for diagrams - childs are the different diagram frames 
    """
    def __init__(self, master, wingFn, *args, **kwargs):
        super().__init__(master, *args, text_color="white", **kwargs)
        """
        Args:
            :master: frame self belongs to
        """
        
        self.diagrams  = []
        diagramClasses = [Diagram_Wing, Diagram_Planform, 
                          Diagram_ChordDistribution, Diagram_Airfoils] 

        # Get all subclasses of Plot_Frame and create a tab for each 
        for iDia, diaClass in enumerate (diagramClasses):

            tab_frame = self.add("   " + diaClass.name + "   ")
            tab_frame.grid_rowconfigure(0, weight=1)

            # plot area
            plot_frame=  ctk.CTkFrame(tab_frame, fg_color="transparent") 
            plot_frame.grid_columnconfigure(0, weight=1)
            plot_frame.grid_rowconfigure(0, weight=1)
            
            # settings and switches frame 
            if diaClass == Diagram_Wing:                # full wing diagram doesn't have view frame
                tab_frame.grid_columnconfigure(0, weight=1)
                view_frame = None 
                plot_frame.grid (row=0, column=0, padx=(0,0), pady=0, sticky='wens')
            else: 
                tab_frame.grid_columnconfigure(0, weight=0, minsize=160)
                tab_frame.grid_columnconfigure(1, weight=1)
                view_frame=  ctk.CTkFrame(tab_frame)    
                view_frame.grid (row=0, column=0, padx=(0, 5), pady=(0,0), sticky='news')
                plot_frame.grid (row=0, column=1, padx=(0,0), pady=0, sticky='wens')

            
            # create the diagram in the plot frame and set first diagram active (show it)
            self.diagrams.append (diaClass (plot_frame, wingFn, setActive=(iDia==0), 
                                            view_frame=view_frame))

        # set size of tab view titles
        self._segmented_button.configure(font=("", 16))
        self.configure(command=self.newTabSelected)

         
    def newTabSelected (self):
        # activate the new selected frame for updates 

        newName = self.get()                                # current from CTkTabview
        diagram     : Diagram_Abstract = None
        new_diagram : Diagram_Abstract = None

        for diagram in self.diagrams:
            diagram.setActive (False)                       # first deactivate all
            if (diagram.name.strip() == newName.strip()): 
                new_diagram = diagram

        if new_diagram: new_diagram.setActive(True)         #  activate new one 

    


#-------------------------------------------------------------------------------
# Full Wing    
#-------------------------------------------------------------------------------


class Diagram_Wing (Diagram_Abstract):
    """ 
    Frame to full wing (left and right) 
    """
    name = "Wing"

    @property
    def myApp (self) -> 'App':
        return super().myApp

    def wing(self) -> Wing:
        " main data object of the diagram"
        return self._objectFn()


    def create_axes (self):
        """ setup axes, axis for this plot type """
    
        self.ax : plt.Axes = self.figure.add_subplot()        # the pyplot axes this diagram is plotted
        self.figure.subplots_adjust(left=0.03, bottom=0.06, right=0.98, top=0.96, wspace=None, hspace=None)
        self.ax.tick_params (labelsize='small')

        self.ax.set_xticks ([])
        self.ax.set_yticks ([])

        self.ax.grid (visible=True)


    def setup_axes (self):
        """ setup axes, axis for this plot type """

        limy = self.ax.get_ylim()
        if limy[0] < limy[1]: 
            self.ax.invert_yaxis() 
        self.ax.axis('equal')
        # self.ax.relim()
        self.ax.autoscale(enable=True, axis='Both')
        # self.ax.margins(x=0.02, y= 0.1)


    def setup_artists (self):
        """ setup artists for this plot type """

        super().setup_artists()
        self.wingArtist     = Wing_Artist  (self.ax, self.wing, show=True)
        # self.dxfArtist          = RefPlanform_DXF_Artist(self.ax, self.wing)

    # -------- switch call back 

    # -------- event handler

    def setChangeBindings (self):
        # overloaded
        self.ctk_root.bind(WING_NEW,                 self.refresh, add='+')
        self.ctk_root.bind(WING_CHANGED,             self.refresh, add='+')
        self.ctk_root.bind(CHORD_CHANGED,            self.refresh, add='+')
        self.ctk_root.bind(PLANFORM_CHANGED,         self.refresh, add='+')
        self.ctk_root.bind(SECTION_CHANGED,          self.refresh, add='+')
        self.ctk_root.bind(AIRFOIL_CHANGED,          self.refresh, add='+')

    # -------- Callbacks from Artists 

    # -------- refresh my Artists which are on 'show mode' 

    def refresh(self,*_): 
        # overloaded
        if self._active:
            self.wingArtist.refresh ()  
            # self.dxfArtist.refresh      () 

            # self.setup_axes()                       # maybe the axis limits changed
            self.ax.relim()
            self.ax.autoscale_view()

            self.ax.figure.canvas.draw_idle()     # draw ony if Windows is idle!



#-------------------------------------------------------------------------------
# Planform   
#-------------------------------------------------------------------------------


class Diagram_Planform (Diagram_Abstract):
    """ 
    Frame to plot planform 
    """
    name = "Planform"

    @property
    def myApp (self) -> 'App':
        return super().myApp

    def wing(self) -> Wing:
        " main data object of the diagram"
        return self._objectFn()


    def create_axes (self):
        """ setup axes, axis for this plot type """
    
        # overloaded to have no ticks for this diagram
        super().create_axes()
        self.ax.set_xticks ([])
        self.ax.set_yticks ([])

        self.ax.grid (visible=True)


    def setup_axes (self):
        """ setup axes, axis for this plot type """

        limy = self.ax.get_ylim()
        if limy[0] < limy[1]: 
            self.ax.invert_yaxis() 
        self.ax.axis('equal')
        self.ax.relim()
        self.ax.autoscale(enable=True, axis='Both')
        self.ax.margins(x=0.06, y= 0.1)


    def setup_artists (self):
        """ setup artists for this plot type """

        super().setup_artists()
        self.planformArtist     = Planform_Artist       (self.ax, self.wing, show=True, onMove=self.changed_by_mouse)
        self.chordLinesArtist   = ChordLines_Artist     (self.ax, self.wing)
        self.sectionsArtist     = Sections_Artist       (self.ax, self.wing, show=True, onPick=self.sectionPicked)     
        self.curSectionArtist   = CurrentSection_Artist (self.ax, self.wing, show=True, onMove=self.section_by_mouse)
        self.airfoilNameArtist  = AirfoilName_Artist    (self.ax, self.wing)     
        self.flapArtist         = Flap_Artist           (self.ax, self.wing)
        self.referenceArtist    = RefPlanform_Artist    (self.ax, self.wing)
        self.dxfArtist          = RefPlanform_DXF_Artist(self.ax, self.wing)


    def setup_switches (self, r = 0, c = 0 ):
        """ define on/off switches for this plot types"""

        r,c = super().setup_switches(r,c)

        # the whole plot work will be done by the artists
        r += 1 
        Switch_Widget (self.view_frame,r,c, lab='Grid', val=True,  set=self._set_grid)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Mouse helper', val=True, set=self._set_mouseHelpers)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Wing sections', val=True, set=self._set_showSections)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Airfoil names', val=False, set=self._set_showAirfoilNames)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Flaps', val=False, set=self._set_showFlaps)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Chord lines', val=False, set=self._set_showChord)

        r += 1
        Label_Widget (self.view_frame,r,c, lab='Reference planforms', pady=(20,10))
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Elliptical', val=False, set=self._set_showReference)

        r += 1
        p = Switch_Widget (self.view_frame,r,c, lab='From DXF', val=False, set=self._set_showRefDXF,
                           disable=lambda: not self.wing().refPlanform_DXF.isValid)
        self._dxf_widget = p 
        return r,c 


    # -------- switch call back 

    def _set_grid (self, aBool): 
        self.ax.grid (aBool)
        self.figure.canvas.draw_idle() 

    def _set_showAirfoilNames (self, aBool):
        self.airfoilNameArtist.set_show(aBool)
        self.airfoilNameArtist.refresh (figureUpdate=True) 

    def _set_showFlaps (self, aBool):
        self.flapArtist.set_show(aBool)
        self.flapArtist.refresh (figureUpdate=True) 

    def _set_showChord (self, aBool):
        self.chordLinesArtist.set_show(aBool)
        self.chordLinesArtist.refresh (figureUpdate=True) 

    def _set_showReference (self, aBool):
        self.referenceArtist.set_show(aBool)
        self.referenceArtist.refresh (figureUpdate=True) 

    def _set_showRefDXF (self, aBool):
        self.dxfArtist.set_show(aBool)
        self.dxfArtist.refresh (figureUpdate=True) 

    def _set_mouseHelpers (self, aBool): 

        self.planformArtist.set_mouseActive (aBool)
        self.curSectionArtist.set_mouseActive (aBool)
        self.sectionsArtist.set_mouseActive (aBool)

    def _set_showSections (self, aBool): 

        self.curSectionArtist.set_show (aBool)
        self.sectionsArtist.set_show (aBool)


    # -------- event handler

    def setChangeBindings (self):
        # overloaded
        self.ctk_root.bind(WING_NEW,                 self.wing_new, add='+')
        self.ctk_root.bind(WING_CHANGED,             self.changed_wing, add='+')
        self.ctk_root.bind(CHORD_CHANGED,            self.changed_chord, add='+')
        self.ctk_root.bind(PLANFORM_CHANGED,         self.changed_planform, add='+')
        self.ctk_root.bind(SECTION_CHANGED,          self.changed_sections, add='+')
        self.ctk_root.bind(AIRFOIL_CHANGED,          self.changed_airfoil, add='+')
        self.ctk_root.bind(CURRENT_SECTION_CHANGED,  self.changed_currentSection, add='+')

    def wing_new (self, *_): 
        """ Eventhandler for new wing """
        self.changed_wing ()
        self.changed_currentSection ()

    def changed_wing (self, *_): 
        """ Eventhandler for changes of the complete wing - like wing span """
        self.refresh()

    def changed_chord (self, *_): 
        """ Eventhandler for changes of chord"""
        self.refresh()

    def changed_planform (self, *_): 
        """ Eventhandler for changes of planform"""
        self.refresh()

    def changed_airfoil (self, *_): 
        """ Eventhandler for changes of planform"""
        self.refresh_sections() 

    def changed_sections (self, *_): 
        """ Eventhandler for changes of planform"""
        if self.wing().planform.planform_depend_on_sections:
            self.refresh()
        self.refresh_sections() 

    def changed_currentSection(self, *_): 
        """" eventhandler - current has changed from outside """
        if self._active:
            #retrieve new section name from App 
            self.curSectionArtist.set_current (self.myApp.curWingSectionName(), figureUpdate=False) 
            self.refresh_sections() 

    # -------- Callbacks from Artists 

    def changed_by_mouse(self): 
        """" planform (banana) or wing (hinge) was changed with mouse """
        self.changed_planform()
        fireEvent (self.ctk_root, WING_CHANGED_BY_MOUSE)
        fireEvent (self.ctk_root, PLANFORM_CHANGED_BY_MOUSE)

    def section_by_mouse(self): 
        """" current section was moved with mouse """
        self.changed_sections()
        fireEvent (self.ctk_root, DIAGRAM_SECTION_SELECTED)


    def sectionPicked (self, aSectionLabel):
        # callback method - the user pciked a wing section in the plot

        "fire event only if new section was selected"
        if aSectionLabel != self.curSectionArtist.curLineLabel:

            self.myApp.set_curWingSectionName(aSectionLabel)
            fireEvent (self.ctk_root, DIAGRAM_SECTION_SELECTED)

            self.curSectionArtist.set_current (aSectionLabel, figureUpdate=True)  

            self.ax.figure.canvas.draw_idle()     # draw ony if Windows is idle!


    # -------- refresh my Artists which are on 'show mode' 

    def setActive(self, active: bool):
        # overloaded to set active section 
        if active: 
            self.curSectionArtist.set_current (self.myApp.curWingSectionName(), figureUpdate=False)    
        super().setActive(active)


    def refresh(self): 
        # overloaded
        if self._active:

            self._dxf_widget.refresh()

            self.planformArtist.refresh ()  
            self.chordLinesArtist.refresh ()  
            self.sectionsArtist.refresh ()  
            self.curSectionArtist.refresh()
            self.airfoilNameArtist.refresh ()  
            self.referenceArtist.refresh() 
            self.dxfArtist.refresh      () 
            self.flapArtist.refresh     () 

            self.setup_axes()                       # maybe the axis limits changed

            self.ax.figure.canvas.draw_idle()     # draw ony if Windows is idle!
            # print ("  - refresh in ", self.__class__.__name__," for active artists")

    def refresh_sections(self): 
        if self._active:
            self.sectionsArtist.refresh ()  
            self.airfoilNameArtist.refresh ()  
            self.flapArtist.refresh     () 
            self.curSectionArtist.refresh()

            self.ax.figure.canvas.draw_idle()    # draw ony if Windows is idle!
            # print ("  - refresh sections in ", self.__class__.__name__," for active artists")




class Diagram_Planform_Paneled (Diagram_Abstract):
    """ 
    Frame to plot planform in a simple, mini Version for panel previews . 
    """
    name = "Planform paneled"

    def __init__(self, master, objectFn, paneledPlanform : Planform_Paneled, *args, size= None,  **kwargs):

        self._paneledPlanform    = paneledPlanform             # planform with panels to show 

        super().__init__(master, objectFn, *args, size= size,  **kwargs)


    def wing(self) -> Wing:
        " main data object of the diagram"
        return self._objectFn()


    def create_axes (self):
        """ setup axes, axis for this plot type """
        self.ax : plt.Axes = self.figure.add_subplot(frameon=False)        # the pyplot axes this diagram is plotted
        self.figure.subplots_adjust(left=0.01, bottom=0.0, right=0.99, top=1, wspace=None, hspace=None)


    def setup_axes (self):
        """ setup axes, axis for this plot type """
        self.ax.set_ylim([self.wing().rootchord, 0.0])
        self.ax.set_xlim([-0.05 * self.wing().halfwingspan, self.wing().halfwingspan * 1.05])
        self.ax.axis('equal')
        self.ax.set_xticks([], [])
        self.ax.set_yticks([], [])


    def setup_artists (self):
        """ artists afor this view """
        self.panelArtist = PaneledPlanform_Artist (self.ax, self.wing, self._paneledPlanform, show=True)


    def refresh(self): 
        self.panelArtist.refresh(figureUpdate=True)




class Diagram_Planform_DXF (Diagram_Abstract):
    """ 
    Frame to plot planform in a simple, mini Version for dxf previews . 
    """
    name = "Planform paneled"

    def __init__(self, master, objectFn, planform_dxf : Planform_DXF, *args, size= None,  **kwargs):

        self._planform_dxf    = planform_dxf             # planform with panels to show 

        super().__init__(master, objectFn, *args, size= size,  **kwargs)


    def wing(self) -> Wing:
        " main data object of the diagram"
        return self._objectFn()


    def create_axes (self):
        """ setup axes, axis for this plot type """
        self.ax : plt.Axes = self.figure.add_subplot(frameon=False)        # the pyplot axes this diagram is plotted
        self.figure.subplots_adjust(left=0.01, bottom=0.0, right=0.99, top=1, wspace=None, hspace=None)


    def setup_axes (self):
        """ setup axes, axis for this plot type """
        self.ax.set_ylim([self.wing().rootchord, 0.0])
        self.ax.set_xlim([-0.05 * self.wing().halfwingspan, self.wing().halfwingspan * 1.05])
        self.ax.axis('equal')
        self.ax.set_xticks([], [])
        self.ax.set_yticks([], [])


    def setup_artists (self):
        """ artists for this diagram """
        self.planformArtist = RefPlanform_DXF_Artist (self.ax, self.wing, show=True, showDetail=True, 
                                                      showMarker=False, planform=self._planform_dxf)

    def refresh(self): 
        self.planformArtist.refresh(figureUpdate=True)



#-------------------------------------------------------------------------------
# Chord   
#-------------------------------------------------------------------------------

class Diagram_ChordDistribution (Diagram_Abstract):
    """ 
    Frame to plot chord distribution
    """
    name = "Chord distribution"

    @property
    def myApp (self) -> 'App':
        return super().myApp

    def wing(self) -> Wing:
        " main data object of the diagram"
        return self._objectFn()


    def create_axes (self):
        """ setup axes, axis for this plot type """
    
        # overloaded to have no ticks for this diagram
        super().create_axes()
        self.ax.set_xticks ([])
        self.ax.set_yticks ([])

        self.ax.grid (visible=True)


    def setup_axes(self):
        """ setup axes, axis, artiss for this plot type """
        self.ax.set_ylim([ 0.0, 1.19])
        self.ax.set_xlim([ 0.0, 1.06])


    def setup_artists(self):
        """ setup axes, axis, artists for this plot type """

        super().setup_artists()
        self.chordArtist        = Chord_Artist          (self.ax, self.wing, show=True, onMove=self.chord_by_mouse)
        self.chordLinesArtist   = ChordLines_Artist     (self.ax, self.wing, norm=True, )
        self.curSectionArtist   = CurrentSection_Artist (self.ax, self.wing, norm=True, onMove=self.section_by_mouse)
        self.sectionsArtist     = Sections_Artist       (self.ax, self.wing, norm=True, onPick=self.sectionPicked)
        self.referenceArtist    = RefChord_Artist       (self.ax, self.wing, norm=True)
        self.dxfArtist          = RefChord_DXF_Artist   (self.ax, self.wing, norm=True)


    def setup_switches(self, r=0, c=0):
        """ define on/off switches ffor this plot type"""

        r,c = super().setup_switches(r, c)

        r += 1 
        Switch_Widget (self.view_frame,r,c, lab='Grid', val=True,  set=self._set_grid)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Mouse helper', val=True, set=self._set_mouseHelpers)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Wing sections', val=False, set=self._set_showSections)

        r += 1
        Label_Widget (self.view_frame,r,c, lab='Reference planforms', pady=(20,10))
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Elliptical', val=False, set=self._set_showReference)
        r += 1
        p = Switch_Widget (self.view_frame,r,c, lab='From DXF', val=False, set=self._set_showRefDXF,
                           disable=lambda: not self.wing().refPlanform_DXF.isValid)
        self._dxf_widget = p 

        return r,c 


    # -------- switch call back 

    def _set_grid (self, aBool): 
        self.ax.grid (aBool)
        self.figure.canvas.draw_idle() 

    def _set_mouseHelpers (self, aBool): 
        self.chordArtist.set_mouseActive (aBool)
        self.curSectionArtist.set_mouseActive (aBool)
        self.sectionsArtist.set_mouseActive (aBool)

    def _set_showSections (self, aBool): 
        self.curSectionArtist.set_show (aBool)
        self.sectionsArtist.set_show (aBool)

    def _set_showReference (self, aBool):
        self.referenceArtist.set_show(aBool)
        self.referenceArtist.refresh (figureUpdate=True) 

    def _set_showRefDXF (self, aBool):
        self.dxfArtist.set_show(aBool)
        self.dxfArtist.refresh (figureUpdate=True) 


    # -------- event handler

    def setChangeBindings (self):
        # overloaded
        self.ctk_root.bind(WING_NEW,                 self.wing_new, add='+')
        self.ctk_root.bind(CHORD_CHANGED,            self.changed_chord, add='+')
        self.ctk_root.bind(PLANFORM_CHANGED,         self.changed_planform, add='+')
        self.ctk_root.bind(SECTION_CHANGED,          self.changed_sections, add='+')
        self.ctk_root.bind(CURRENT_SECTION_CHANGED,  self.changed_currentSection, add='+')

    def wing_new (self, *_): 
        """ Eventhandler for new wing """
        self.changed_chord ()
        self.changed_currentSection ()

    def changed_chord (self, *_): 
        """ Eventhandler for changes of chord"""
        self.refresh()

    def changed_planform (self, *_): 
        """ Eventhandler for changes of planform"""

        if self._active:
            self.dxfArtist.refresh ()           # a new ref dxf could have been loaded
        self.refresh_sections() 

    def changed_sections (self, *_): 
        """ Eventhandler for changes of planform"""
        if self.wing().planform.planform_depend_on_sections:
            self.refresh()
        self.refresh_sections() 

    def changed_currentSection(self, *_): 
        """" eventhandler - current has changed from outside """
        if self._active:
            #retrieve new section name from App 
            self.curSectionArtist.set_current (self.myApp.curWingSectionName(), figureUpdate=True)  

    # -------- Callbacks from Artits 

    def chord_by_mouse(self): 
        """" chord distribution was changed with mouse """
        self.changed_chord()
        fireEvent (self.ctk_root, CHORD_CHANGED_BY_MOUSE)


    # -------- refresh my Artists which are on 'show mode' 

    def setActive(self, active: bool):
        # overloaded to set active section 
        if active: 
            self.curSectionArtist.set_current (self.myApp.curWingSectionName(), figureUpdate=False)    
        super().setActive(active)


    def refresh(self): 
        # overloaded
        if self._active:

            self._dxf_widget.refresh()

            self.chordArtist.refresh ()  
            self.chordLinesArtist.refresh ()  
            self.sectionsArtist.refresh ()  
            self.curSectionArtist.refresh()
            self.referenceArtist.refresh () 
            self.dxfArtist.refresh () 
            self.ax.figure.canvas.draw_idle()    # draw ony if Windows is idle!
            # print ("  - refresh in ", self.__class__.__name__," for active artists")

    def refresh_sections(self): 
        if self._active:
            self.sectionsArtist.refresh ()  
            self.curSectionArtist.refresh()
            self.ax.figure.canvas.draw_idle()    # draw ony if Windows is idle!
            # print ("  - refresh sections in ", self.__class__.__name__," for active artists")

    # -------- Callbacks from Artists 

    def section_by_mouse(self): 
        """" current section was moved with mouse """
        self.changed_sections("")
        fireEvent (self.ctk_root, DIAGRAM_SECTION_SELECTED)


    def sectionPicked (self, aSectionLabel):
        # call method - the user pciked a wing section in the plot
        self.myApp.set_curWingSectionName(aSectionLabel)
        fireEvent (self.ctk_root, DIAGRAM_SECTION_SELECTED)
        self.curSectionArtist.set_current (aSectionLabel, figureUpdate=True)  



#-------------------------------------------------------------------------------
# Airfoil   
#-------------------------------------------------------------------------------

class Diagram_Airfoils (Diagram_Abstract):
    """ 
    plots the airfoils of all wing sections
    """
    name = "Airfoils"
    _show_strakedAirfoils = False

    @property
    def myApp (self) -> 'App':
        return super().myApp

    def wing(self) -> Wing:
        " main data object of the diagram"
        return self._objectFn()


    def setup_axes(self):
        """ setup axes, axis, artiss for this plot type """
        self.ax.axis('equal')
        self.ax.relim()
        self.ax.autoscale(enable=True, axis='Both')

        self.ax.grid (visible=True)


    def setup_artists(self):
        """ setup axes, axis, artists for this plot type """

        super().setup_artists()
        self.airfoilArtist      = Airfoil_Artist (self.ax, self.wing, show=True, norm=True, 
                                                  strak=False, onPick=self.airfoilPicked)


    def setup_switches(self, r=0, c=0):
        """ define on/off switches for this plot type"""

        r,c =super().setup_switches(r, c)

        r += 1 
        Switch_Widget (self.view_frame,r,c, lab='Grid', val=True, set=self._set_grid)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='as wing section', val=False, set=self._set_show_abs)
        r += 1
        Switch_Widget (self.view_frame,r,c, lab='Straked airfoils', val=False, set=self._set_show_strakedAirfoils)
        return r,c 


    # -------- switch call back 

    def _set_grid (self, aBool): 
        self.ax.grid (aBool)
        self.figure.canvas.draw_idle() 

    def _set_show_strakedAirfoils (self, aBool):

        if aBool: 
            self.wing().do_strak()                    # recalculate strak airfoils 
            self.airfoilArtist.set_strak (True)
            self.refresh()
        else: 
            self.airfoilArtist.set_strak (False)
        self._show_strakedAirfoils = aBool 

    def _set_show_abs (self, aBool): 
        self.airfoilArtist.set_abs (aBool)
        self.airfoilArtist.refresh (figureUpdate=True) 



    # -------- event handler

    def setChangeBindings (self):
        # overloaded
        self.ctk_root.bind(WING_NEW,                 self.wing_new, add='+')
        self.ctk_root.bind(AIRFOIL_CHANGED,          self.changed_airfoil, add='+')
        # these could change chord of wingsection (abs size of airfoil) or strak airfoil
        self.ctk_root.bind(WING_CHANGED,             self.changed_airfoil, add='+')
        self.ctk_root.bind(CHORD_CHANGED,            self.changed_airfoil, add='+')
        self.ctk_root.bind(PLANFORM_CHANGED,         self.changed_airfoil, add='+')
        self.ctk_root.bind(SECTION_CHANGED,          self.changed_airfoil, add='+')

        self.ctk_root.bind(CURRENT_SECTION_CHANGED,  self.changed_currentSection, add='+')

    def wing_new (self, *_): 
        """ Eventhandler for new wing """
        self._set_show_strakedAirfoils (False)
        self.changed_airfoil ()
        self.changed_currentSection ()

    def changed_airfoil(self, *_): 
        """ Eventhandler for changes of airfoil"""
        if self._active:
            self.airfoilArtist.set_strak (False)            # switch off - strak airfoils could have changed
            self.refresh()

    def changed_currentSection(self, *_): 
        """" eventhandler - current has changed from outside """
        if self._active:
            #retrieve new section name from App 
            self.airfoilArtist.set_current (self.myApp.curWingSectionName(), figureUpdate=True)  

    # -------- refresh my Artists which are on 'show mode' 

    def setActive(self, active: bool):
        # overloaded to set active section 
        if active: 
            self.airfoilArtist.set_current (self.myApp.curWingSectionName(), figureUpdate=True)  
        super().setActive(active)

    def refresh(self): 
        # overloaded
        if self._active:
            self.airfoilArtist.refresh ()  
            self.ax.figure.canvas.draw_idle()    # draw ony if Windows is idle!

    # -------- pick object in axes handling  

    def airfoilPicked (self, aAirfoilLabel):
        # call method - the user pciked a wing section in the plot
        self.airfoilArtist.set_current (aAirfoilLabel, figureUpdate=True)  
        sectionName = aAirfoilLabel.split("@")[1].strip()
        if sectionName: 
            self.myApp.set_curWingSectionName(sectionName)
            fireEvent (self.ctk_root, DIAGRAM_SECTION_SELECTED)
        else: 
            ErrorMsg ("Missing section name in airfoil label")


#-------------------------------------------------------------------------------
# Dialogs for smaller tasks   
#-------------------------------------------------------------------------------


class Dialog_Load_DXF (Dialog_Abstract):
    """ 
    Import dxf file to be overlay or template planform 

    Returns in self.dxf_pathFilename if the user selected a valid file 
    """
    name  = "Import dxf file"
    widthFrac  = 0.65
    heightFrac = 0.45

    def __init__(self, master, *args, wingFn = None, dxf_Path= None, ref:bool = False, **kwargs):

        self.wingFn = wingFn
        self.wing : Wing = wingFn()
        self.dxf_pathFilename = None                    # the return value 
        self.tmpPlanform = Planform_DXF( self.wing, dxf_Path= dxf_Path, ref = ref)
        self.tmpPlanform.adaptHingeAngle = False        # show the original hinge angle 

        super().__init__(master, *args, **kwargs)


    def init (self):
        # init UI 

        if self.tmpPlanform.dxf_isReference:
            shortDescription = "The wing contour in the dxf file will be used as " +\
                               "a reference overlay for the active planform." 
            headerText = "DXF contour as reference"
        else:
            shortDescription = "The chord distribution of the dxf wing will be the base of the current planform." 
            headerText = "DXF chord distribution for planform"

        frame = self.edit_frame
        frame.grid_columnconfigure (5, weight=1)

        r = 0 
        Header_Widget (frame,r,0, lab=headerText, width=110, columnspan=3)
        r +=1  
        Label_Widget  (frame,r,0, lab= lambda: shortDescription)
        r +=1  
        Blank_Widget  (frame,r,0)

        r +=1  
        self.add(Field_Widget  (frame,r,0, lab='DXF file', padx=(10,0),width= 220, lab_width=180,
                                columnspan=2, get= self.tmpPlanform.dxf_filename))
        self.add(Button_Widget (frame,r,4, lab='Select', width=90, set=self.open_dxf_file ))
        self.add(Button_Widget (frame,r,5, lab='Remove', width=70, set=self.remove_dxf_file, 
                                disable=self.remove_dxf_disable ))

        r +=1  
        Blank_Widget  (frame,r,0)
        r +=1  
        self.add(Label_Widget (frame,r,0, lab=lambda: self.tmpPlanform.infoText, columnspan=1, width=180, sticky = "ew" ))

        r +=1  
        Blank_Widget (frame,r,0)

        r +=1  
        self.add(Button_Widget (frame,r,1, lab='Ok', style=PRIMARY, set=self.ok, width=100, padx= 0, 
                                disable= self.ok_disable ))
        self.add(Button_Widget (frame,r,2, lab='Cancel',set=self.cancel, width= 100))

        r +=1  
        Blank_Widget  (frame,r,3)


        # ----------- setup diagram frame at the end ---------------

        # show a little dxf preview
        self.diagram_frame = Diagram_Planform_DXF (frame, self.wingFn, self.tmpPlanform, size=(4.5,2.5))
        self.diagram_frame.grid(row=5, column=1, columnspan= 5, padx=(0,15))
        frame.grid_columnconfigure (5, weight=1)
        frame.grid_rowconfigure (5, weight=1)


    def open_dxf_file (self):

        filetypes  = [('dxf files', '*.dxf')]
        newPathFilename = filedialog.askopenfilename(
                    title='Open dxf file',
                    initialdir=self.workingDir,
                    filetypes=filetypes)

        if newPathFilename: 
            self.tmpPlanform.set_dxfPathFilename (newPathFilename)
            self.refresh()
            self.diagram_frame.refresh()

    def remove_dxf_file (self):
            self.tmpPlanform.set_dxfPathFilename ("")
            self.refresh()
            self.diagram_frame.refresh()

    def remove_dxf_disable (self):
            disable =  not self.tmpPlanform.dxf_pathFilename
            return disable

    def ok (self):
        # to over load and do ok actions
        if self.tmpPlanform.isValid: 
            self.dxf_pathFilename = self.tmpPlanform.dxf_pathFilename  # the return value 
        super().ok()

    def ok_disable (self):
        " activate ok-Button"
        # in case dxf is reference, it's also possible to remove the dxf reference
        # else a valid dxf must be choosen 
        disable = not self.tmpPlanform.isValid and not self.tmpPlanform.dxf_isReference
        return disable

    def cancel(self): 
        super().cancel()



class Dialog_Export_Xflr5_Flz (Dialog_Abstract):
    """ 
    Export planform as paneled for Xflr5 oder FLZ 

    """
    name       = "Export to ..."
    widthFrac  = 0.70
    heightFrac = 0.62

    def __init__(self, master, wingFn, Xflr5=False, Flz=False, *args, **kwargs):

        self.wingFn = wingFn
        self.wing : Wing = wingFn()

        if Xflr5:
            self.exporter = self.wing.exporterXflr5
            self.mode = "Xflr5"
        elif Flz:
            self.exporter = self.wing.exporterFlz
            self.mode = "FLZ_vortex"
        else:
            return  
        self.paneledPlanform = self.exporter.paneledPlanform

        super().__init__(master, *args, **kwargs)


    def init (self):
        # init UI 

        # main grid 3 x 1  (preview + edit + buttons) 
        self.input_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent") 
        self.input_frame.grid(row=1, column=0, sticky="nwe")

        self.button_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.button_frame.grid(row=2, column=0, sticky="wes", pady=10)

        self.edit_frame.grid_columnconfigure (0, weight=1)
        self.edit_frame.grid_rowconfigure    (0, weight=1, minsize=400)
        self.edit_frame.grid_rowconfigure    (2, weight=1)


        # header with hints 
        r = 0 
        c = 0 
        Header_Widget (self.input_frame,r,c, lab= self.mode+" export", width=110, pady=(7,20), columnspan=2)
        hint = self.check_y_deviation() 
        if hint: 
            self.add (Label_Widget  (self.input_frame,r,c+2, lab=hint, sticky="w", 
                                     columnspan=8, text_style='hint'))

        # entry fields 
        r +=1  
        self.add (Field_Widget  (self.input_frame,r,c, lab="x-panels", width=100, padx=(20,0),
                                 obj=self.paneledPlanform, get='x_panels', set='set_x_panels',
                                 event=PANELS_CHANGED, lim=(1,50), dec=0, spin=True, step=1))
        self.add (Combo_Widget  (self.input_frame,r,c+3, lab="x-distribution", width=90,
                                 obj=self.paneledPlanform, get='x_dist', set='set_x_dist',
                                 options=self.paneledPlanform.distribution_fns_names(),
                                 event=PANELS_CHANGED ))

        r +=1  
        self.add (Field_Widget  (self.input_frame,r,c, lab="y-panels", width=100, padx=(20,0),
                                 obj=self.paneledPlanform, get='y_panels', set='set_y_panels',
                                 event=PANELS_CHANGED, lim=(1,50), dec=0, spin=True, step=1))
        self.add (Combo_Widget  (self.input_frame,r,c+3, lab="y-distribution",  width=90,
                                 obj=self.paneledPlanform, get='y_dist', set='set_y_dist',
                                 options=self.paneledPlanform.distribution_fns_names(),
                                 event=PANELS_CHANGED ))
        r +=1  
        self.add (Field_Widget  (self.input_frame,r,c, lab="Min panel width", width=100, padx=(20,0), lab_width=110,
                                 obj=self.paneledPlanform, get='y_minWidth', set='set_y_minWidth',
                                 event=PANELS_CHANGED, lim=(1,40), dec=0, spin=True, step=1, unit=self.wing.unit))
        text = "Good value could be 20 %s" %(self.wing.unit)
        self.add (Label_Widget  (self.input_frame,r,c+3, lab=text, padx=(0,0)))
        r +=1  
        self.add (Field_Widget  (self.input_frame,r,c, lab="Min tip chord", width=100, padx=(20,0),
                                 obj=self.paneledPlanform, get='minTipChord', set='set_minTipChord',
                                 event=PANELS_CHANGED, lim=(1,100), dec=0, spin=True, step=1, unit=self.wing.unit))
        self.add (Label_Widget  (self.input_frame,r,c+3, lab=self.tipChord_text, padx=(0,0),
                                 text_style=self.tipChord_text_style))

        r = 1 
        c = 7 
        self.input_frame.grid_columnconfigure (6, weight=1)
        self.input_frame.grid_columnconfigure (10, weight=2)

        self.add(Field_Widget  (self.input_frame,r,c, lab=self.mode+ " directory", obj=self.exporter, get='baseAndExportDir', set='',
                                width=220, disable=True, justify='left'))
        self.add(Button_Widget (self.input_frame,r,c+2, lab='Select', width=60, sticky='w', set=self.select_dir ))
        self.add(Switch_Widget (self.input_frame,r+1,c, lab='Use airfoil nick names for airfoils', 
                                columnspan=2, padx=0, 
                                obj=self.exporter, get='useNick', set='set_useNick'))
 

        r = 0 
        c = 1 
        self.add(Button_Widget (self.button_frame,r,c, lab='Export', set=self.ok, style=PRIMARY, width=100))
        if self.mode == "FLZ_vortex": 
            c += 1 
            self.add(Button_Widget (self.button_frame,r,c, lab='Launch FLZ', set=self.launch_Flz, width=100,
                                    disable=self.launch_Flz_disabled))
        c += 1 
        self.add(Button_Widget (self.button_frame,r,c, lab='Close', set=self.cancel, width=100))
        self.button_frame.grid_columnconfigure (0, weight=1)
        self.button_frame.grid_columnconfigure (4, weight=1)

        # changed bindings
        self.ctk_root.bind(PANELS_CHANGED, self.refresh, add='+')


        # ----------- setup diagram frame at the end ---------------

        # set specific diagram frame for this dialog 
        self.diagram_frame = Diagram_Planform_Paneled (self.edit_frame, self.wingFn, 
                                                      self.paneledPlanform, size=(4.2,3.4))
        self.diagram_frame.grid(row=0, column=0, sticky="nwes")


    def tipChord_text (self): 
        """ user hint for tipChord"""
        if self.paneledPlanform.minTipChord > self.wing.tipchord:
            return "Tip will be cutted to 'Min tip chord'"
        else: 
            return "Tip chord of wing is %d %s" %(self.wing.tipchord, self.wing.unit)

    def tipChord_text_style (self): 
        """ style for user hint for tipChord"""

        if self.paneledPlanform.minTipChord > self.wing.tipchord:
            return STYLE_WARNING
        else: 
            return STYLE_COMMENT


    def select_dir(self):
        " open dialog for directory selection"

        newDir = filedialog.askdirectory(
                    title='Select directory for export',
                    initialdir=self.exporter.baseAndExportDir)
        if newDir:
            self.exporter.set_exportDir (newDir)
            super().refresh()


    def check_y_deviation (self): 
        # build a user message if the deviation of the paneled wing to the actual wing is too high

        # in case of a trapezoidal planform there is per se no deviation 
        if self.wing.planform.planform_depend_on_sections: return

        threshold = 5                           # deviation in percent

        _, _, deviations = self.paneledPlanform.y_panel_lines()
        if max(deviations) > threshold:
            hint = "The deviation to the actual chord at the highlighted y-stations is more than %d%%.\n" % threshold + \
                   "Maybe you want to add sections to improve quality of the wing analysis?"
        else: 
            hint = None   
        return hint

    def refresh(self, *_):
        self.diagram_frame.refresh()
        super().refresh()

    def cancel(self): 
        # changed bindings
        self.ctk_root.unbind(PANELS_CHANGED)
        super().cancel()

    def launch_Flz(self): 
        """try to open FLZ_vortex on the exportet file"""
        
        pathFileName = os.path.join (self.exporter.baseAndExportDir, self.exporter.fileName) 
        
        message = self.exporter.doIt()
        try: 
            os.startfile(pathFileName, 'open')
        except: 
            message = "Could not launch FLZ_vortex on exported file: \n\n" + \
                    pathFileName + \
                    "\n\n Is FLZ_vortex neatly installed and associated with file extension '.flz'?"
            Messagebox (self, title="Launch FLZ_vortex", message=message, icon="cancel", option_1="Ok")


    def launch_Flz_disabled (self):
        """ FLZ only on Windows"""

        return not os.name == 'nt'

    def ok(self): 

        # do the export 
        message = self.exporter.doIt()
        msg = Messagebox (self, title=self.mode + " export", message=message, icon="check", option_1="Ok")
        msg.get()                               # wait until pressed ok
        # release changed bindings and close
        self.ctk_root.unbind(PANELS_CHANGED)
        super().ok()



class Dialog_Export_Dxf (Dialog_Abstract):
    """ 
    Export wing / planform as dxf to file  

    """
    name       = "Export DXF"
    widthFrac  = 0.40
    heightFrac = 0.30

    def __init__(self, master, *args, wingFn = None,  **kwargs):

        self.wingFn = wingFn
        self.wing : Wing = wingFn()
        self.exporter = self.wing.exporterDxf

        super().__init__(master, *args, **kwargs)


    def init (self):
        # init UI 

        # main grid 3 x 1  (header + edit + buttons) 
        self.header_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.header_frame.grid(row=0, column=0, sticky="we")

        self.input_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.input_frame.grid(row=1, column=0, sticky="nwes", padx=10, pady=20)

        self.button_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.button_frame.grid(row=2, column=0, sticky="wes", pady=10)

        self.edit_frame.grid_columnconfigure (0, weight=1)
        self.edit_frame.grid_rowconfigure    (1, weight=1)

        # header with hints 
        r = 0 
        c = 0 
        Header_Widget (self.header_frame,r,c, lab="Dxf Export", width=90, pady=(7,20))
        hint = self.hint_onlyPolylines () 
        if hint: 
            self.add (Label_Widget  (self.header_frame,r,c+1, lab=hint, sticky="w", 
                                     columnspan=8, text_style='hint'))
        self.header_frame.grid_columnconfigure (c+2, weight=1)

        # entry fields 
        r = 0 
        c = 0 
        self.add(Field_Widget  (self.input_frame,r,c, lab="Dxf directory", obj=self.exporter, get='baseAndExportDir', set='',
                                width=280, columnspan=2, disable=True))
        self.add(Button_Widget (self.input_frame,r,c+3, lab='Select', width=60, sticky='w', set=self.select_dir ))


        r += 1 
        Blank_Widget           (self.input_frame,r,c, width=50) 

        r += 1 
        self.add(Switch_Widget (self.input_frame,r,c+1, lab='Include airfoils in DXF', 
                                columnspan=2, padx=0, 
                                obj=self.exporter, get='includeAirfoils', set='set_includeAirfoils'))      
       
        r += 1 
        self.add(Switch_Widget (self.input_frame,r,c+1, lab='Use airfoils nick name ', 
                                columnspan=2, padx=0, 
                                obj=self.exporter, get='useNick', set='set_useNick'))      
        r += 1 
        self.add(Switch_Widget (self.input_frame,r,c+1, lab='Export airfoils into directory', 
                                columnspan=2, padx=0, 
                                obj=self.exporter, get='exportAirfoils', set='set_exportAirfoils'))      
       
        r += 1 
        self.add(Switch_Widget (self.input_frame,r,c+1, lab='Set a common airfoil TE thickness of', 
                                columnspan=2, padx=0, 
                                obj=self.exporter, get='setTeGap', set='set_setTeGap'))      

        self.teWidget = Field_Widget (self.input_frame,r,c+3, lab="", lab_width= 30, width=90,
                                 obj=self.exporter, get='teGap_mm', set='set_teGap_mm',
                                 lim=(0,2), dec=1, spin=True, step=0.1, unit='mm')
        self.add(self.teWidget) 


        self.add(Button_Widget (self.button_frame,0,1, lab='Export', set=self.ok, style=PRIMARY, width=100))
        self.add(Button_Widget (self.button_frame,0,2, lab='Cancel', set=self.cancel, width=100))
        self.button_frame.grid_columnconfigure (0, weight=1)
        self.button_frame.grid_columnconfigure (3, weight=1)


    def select_dir(self):
        " open dialog for directory selection"

        newDir = filedialog.askdirectory(
                    title='Select directory for export',
                    initialdir=self.exporter.baseAndExportDir)
        if newDir:
            self.exporter.set_exportDir (newDir)
            super().refresh()


    def hint_onlyPolylines (self): 

        if self.wing.planform.planform_depend_on_sections: 
            hint = None              # in case of a trapezoidal planform no problem ;-)  
        else: 
            nLines = len(self.wing.planform._norm_y_points ())
            hint = "Note: Leading and trailing edge will be approximated by %d straight lines.\n" % nLines + \
                   "Therefore the dxf contour shouldn't be used for final CAD design - use splines.\n" + \
                   "Also the airfoils in dxf are drawn as polylines. Use 'Import .dat' in CAD for pefect quality"
        return hint

    def cancel(self): 
        # changed bindings
        self.ctk_root.unbind(PANELS_CHANGED)
        super().cancel()

    def ok(self): 

        # update model if user pressed directly ok after typing teGap 
        self.teWidget.CTk_callback()

        # do the export 
        message = self.exporter.doIt()
        msg = Messagebox (self, title=self.name, message=message, icon="check", option_1="Ok")
        msg.get()                               # wait until pressed ok

        # release changed bindings
        self.ctk_root.unbind(PANELS_CHANGED)
        super().ok()



class Dialog_Export_Airfoils (Dialog_Abstract):
    """ 
    Export wing / planform as dxf to file  

    """
    name       = "Export Airfoils"
    widthFrac  = 0.40
    heightFrac = 0.25

    def __init__(self, master, *args, wingFn = None,  **kwargs):

        self.wingFn = wingFn
        self.wing : Wing = wingFn()

        super().__init__(master, *args, **kwargs)


    def init (self):
        # init UI 
        self.exporter = self.wing.exporterAirfoils

        # main grid 3 x 1  (header + edit + buttons) 
        self.header_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.header_frame.grid(row=0, column=0, sticky="we")

        self.input_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.input_frame.grid(row=1, column=0, sticky="nwes", padx=10, pady=20)

        self.button_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.button_frame.grid(row=2, column=0, sticky="wes", pady=15)

        self.edit_frame.grid_columnconfigure (0, weight=1)
        self.edit_frame.grid_rowconfigure    (1, weight=1)

        # header with hints 
        r = 0 
        c = 0 
        Header_Widget (self.header_frame,r,c, lab="Airfoil export", width=110, pady=(7,10))
        hint =  "A common trailing edge thickness in mm can be set \n" + \
                "which will modify the airfoils based on their chord in the wing."
        self.add (Label_Widget  (self.header_frame,r,c+1, lab=hint, sticky="w", 
                                    columnspan=8, text_style='hint'))
        self.header_frame.grid_columnconfigure (c+2, weight=1)

        # entry fields 
        r = 0 
        c = 0 
        self.add(Field_Widget  (self.input_frame,r,c, lab="Airfoils directory", obj=self.exporter, get='baseAndExportDir', set='',
                                lab_width=105, width=380, columnspan=4, disable=True))
        self.add(Button_Widget (self.input_frame,r,c+5, lab='Select', width=60, sticky='w', set=self.select_dir ))

        r += 1 
        Blank_Widget           (self.input_frame,r,c, width=50) 
              
        r += 1 
        self.add(Switch_Widget (self.input_frame,r,c+1, lab='Use airfoils nick name ', 
                                columnspan=2, padx=0, 
                                obj=self.exporter, get='useNick', set='set_useNick'))      
        r += 1 
        self.add(Switch_Widget (self.input_frame,r,c+1, lab='Set a common airfoil TE thickness of', 
                                columnspan=2, padx=0, 
                                obj=self.exporter, get='setTeGap', set='set_setTeGap'))      
        self.teWidget = Field_Widget (self.input_frame,r,c+3, lab="", lab_width= 30, width=90,
                                 obj=self.exporter, get='teGap_mm', set='set_teGap_mm',
                                 lim=(0,2), dec=1, spin=True, step=0.1, unit='mm')
        self.add(self.teWidget) 


        self.add(Button_Widget (self.button_frame,0,1, lab='Export', set=self.ok, style=PRIMARY, width=100))
        self.add(Button_Widget (self.button_frame,0,2, lab='Cancel', set=self.cancel, width=100))
        self.button_frame.grid_columnconfigure (0, weight=1)
        self.button_frame.grid_columnconfigure (3, weight=1)


    def select_dir(self):
        " open dialog for directory selection"

        newDir = filedialog.askdirectory(
                    title='Select directory for export',
                    initialdir=self.exporter.baseAndExportDir)
        if newDir:
            self.exporter.set_exportDir (newDir)
            super().refresh()

    def cancel(self): 
        # changed bindings
        self.ctk_root.unbind(PANELS_CHANGED)
        super().cancel()

    def ok(self): 

        # update model if user pressed directly ok after typing teGap 
        self.teWidget.CTk_callback()

        # do the export 
        message = self.exporter.doIt()
        msg = Messagebox (self, title=self.name, message=message, icon="check", option_1="Ok")
        msg.get()                               # wait until pressed ok

        # release changed bindings
        self.ctk_root.unbind(PANELS_CHANGED)
        super().ok()



class Edit_File_Menu(Edit_Abstract_Wing):
    """ 
    Frame for the high level commands like load, save, ...
    The parent is the App itself
    """
    name = "File"

    def init (self):

        self.myApp : App
        
        self.grid_columnconfigure   (0, weight=1)
        Header_Widget (self,0,0, lab=self.name, width=80)
        Button_Widget (self,0,0, icon_name='settings', sticky='e', style=ICON, set=self.myApp.edit_settings)

        Button_Widget (self,1,0, lab='New',         width=100, pady=4, sticky = '', set=self.myApp.new)
        Button_Widget (self,2,0, lab='Open',        width=100, pady=4, sticky = '', set=self.myApp.open)
        Button_Widget (self,3,0, lab='Save',        width=100, pady=4, sticky = '', set=self.myApp.save)
        Button_Widget (self,4,0, lab='Save As...',  width=100, pady=4, sticky = '', set=self.myApp.saveAs)

        Button_Widget (self,5,0, lab='Import Dxf',  width=100, pady=4, sticky = '', set=self.myApp.load_reference_dxf)

        self.option_export = Option_Widget (self,6,0, width = 100, pady=4, sticky = '',
                                            get=self.exportDisplayValue, set = self.set_exportType,
                                            options=self.exportChoices())


    def exportChoices (self):       return ["to Xflr5","to FLZ_vortex", "to DXF", "Airfoils"]

    def exportDisplayValue (self):  return "Export..."

    def set_exportType (self, aType):
        self.option_export.refresh()
        if aType == "to Xflr5":         self.myApp.export_xflr5 ()
        if aType == "to FLZ_vortex":    self.myApp.export_flz ()
        if aType == "to DXF":           self.myApp.export_dxf ()
        if aType == "Airfoils":         self.myApp.export_airfoils ()



#-------------------------------------------------------------------------------
# The App   
#-------------------------------------------------------------------------------

class App(ctk.CTk):

    name = AppName  

    def __init__(self, paramFile):
        super().__init__()

        if paramFile: 
            message = f"Loading\n\n{os.path.basename(paramFile)}"
        else: 
            message = "Creating\n\na sample wing"
        splash_window = ToolWindow(self, message, duration=0)

        # setup event root - so there will be a single root -> ctk root
        self.ctk_root = self

        # settings file handler
        self.settings = Settings()

        # create the 'wing' model - with 'splash window'
        self.paramFile = '' 
        self._myWing : Wing = None
        self.load_wing (paramFile)

        # init UI for new wing 
        self.activate_wing (initial=True)

        # initial size of app window 
        set_initialWindowSize(self, widthFrac=0.92, heightFrac=0.8)

        self._curWingSectionName = None                 # Dispatcher field between Diagram and Edit

        # intercept app close by user  
        self.protocol("WM_DELETE_WINDOW", self.onExit)

        # create main frames        
        diagram_frame = Diagrams        (self, self.wing, border_width=0, corner_radius=0, 
                                         fg_color=Diagram_Abstract.cl_background ) 
        edit_frame    = Edit            (self, self.wing, height=250, fg_color= 'transparent')
        file_frame    = Edit_File_Menu  (self, self.wing, width=200)

        # maingrid 2 x 2 - diagram on top, file and edit on bottom
        self.grid_rowconfigure   (0, weight=1)
        self.grid_rowconfigure   (1, weight=0, minsize=250)
        self.grid_columnconfigure(0, weight=0, minsize=160)
        self.grid_columnconfigure(1, weight=1)
        diagram_frame.grid (row=0, column=0, columnspan=2, pady=(0,5), padx=5, sticky="news")
        file_frame.grid    (row=1, column=0,               pady=(0,5), padx=(5,0), ipady=5,sticky="news")
        edit_frame.grid    (row=1, column=1,               pady=0,     padx=0, sticky="nesw")

        # close splash window again after a while (tkinter draws UI) 
        self.after (800, splash_window._close)


    def wing (self) -> Wing:
        """ encapsulates current wing. Childs should acces only via this function
        to enable a new wing to be set """
        return self._myWing

    
    def curWingSectionName (self):
        """ Dispatcher for current WingSection between Edit and Diagram """
        if self._curWingSectionName is None: 

            # set second section as initial
            self._curWingSectionName = self.wing().wingSections[1].name()        

        return self._curWingSectionName 
    
    def set_curWingSectionName (self, aName):
        """ Dispatcher for current WingSection between Edit and Diagram """
        self._curWingSectionName = aName


    @property
    def workingDir (self): 
        """default home directory for output files (e.g. export)
        Currently equals to dir of parameter file """
        return self.wing().workingDir        

    def set_title (self): 
        """ set window title"""

        if self.paramFile:
            project = self.paramFile
        else:
            project = "< new >"
        self.title (AppName + "  v" + str(AppVersion) + "  [" + project + "]")


    #------- file functions ----------------

    def new (self):
        """ reset - and start with example definition"""

        text = "The current wing '%s' will be discarded." % self.wing().name
        msg  = Messagebox(self, title="Create new wing", message=text,
                  icon="warning", option_2="Cancel", option_1="Ok")            
        if msg.get() == "Ok":
            self.load_wing ("")               # will create default wing
            self.activate_wing ()

    def open (self):
        """ open a new wing definition json and load it"""

        filetypes  = [('Planform Creator 2 files', '*.pc2')]
        newPathFilename = filedialog.askopenfilename(
                    title='Select new wing definition',
                    initialdir=self.workingDir,
                    filetypes=filetypes)

        if newPathFilename:                     # user pressed open
            # load new wing into model 
            self.load_wing (newPathFilename)

            # init UI with new wing 
            self.activate_wing ()


    def save (self):
        """ save wing data to the action parameter file - if new wing to saveAs"""

        if self.paramFile:
            saveOk = self.wing().save(self.paramFile)
            if saveOk:
                text = "Wing successfully saved ...    " 
                Messagebox(self, title="Save wing", message=text, icon="check", option_1="Ok", width=300, height=150)  
                self.settings.set('lastOpenend', self.paramFile)
            else:
                text = "Paramteres couldn't be saved to '%s'" % self.paramFile
                Messagebox(self, title="Save wing", message=text, icon="cancel", option_1="Close", width=300, height=150)  
        else:
            self.saveAs ()


    def saveAs (self):
        """ save wing data to a new file and set this as actual"""

        filetypes  = [('PC2 files', '*.pc2')]
        newPathFilename = filedialog.asksaveasfilename(title='Save parameter file',
                                     initialdir=self.workingDir, filetypes=filetypes,
                                     defaultextension = '.pc2')
        if newPathFilename: 
            saveOk =  self.wing().save(newPathFilename)
            if saveOk: 
                self.paramFile = os.path.normpath(newPathFilename)
                self.set_title ()
                text = "Wing saved to \n\n'%s'" % newPathFilename
                Messagebox(self, title="Save wing", message=text, icon="check", option_1="Ok")  
                self.settings.set('lastOpenend', self.paramFile)
            else: 
                text = "Wing couldn't be saved to '%s'" % newPathFilename
                Messagebox(self, title="Save wing", message=text, icon="cancel", option_1="Ok")  

    def edit_settings (self):
        """ file menu edit settings """

        Dialog_Settings(self)


    #-------------

    def load_wing (self, pathFilename): 
        """ creates / loads new wing as current"""
        self._myWing = Wing (pathFilename)

    def activate_wing (self, initial=False):
        """ activates new wing in App / UI """

        pathFilename = self._myWing.paramFilePath
        if pathFilename:
            self.paramFile = PathHandler.relPath (pathFilename)
            self.settings.set('lastOpenend', self.paramFile)
        else:
            self.paramFile = ""
        self.set_title ()

        if not initial: 
            # mega alarm - inform everything
            fireEvent (self.ctk_root, WING_NEW)


    def export_xflr5 (self): 
        """ export wing to xflr5"""
        self.wait_window (Dialog_Export_Xflr5_Flz (self, self.wing, Xflr5=True))


    def export_flz (self): 
        """ export wing to xflr5"""
        self.wait_window (Dialog_Export_Xflr5_Flz (self, self.wing, Flz=True))


    def export_dxf (self):
        """export wing to dxf"""
        self.wait_window (Dialog_Export_Dxf (self, wingFn = self.wing))


    def export_airfoils (self):
        """export airfoils of wing"""
        self.wait_window (Dialog_Export_Airfoils (self, wingFn = self.wing))


    def load_reference_dxf (self): 
        """ load a dxf planform into the reference_dxf planform"""
        current_dxf_path = self.wing().refPlanform_DXF.dxf_pathFilename

        dxf_dialog = Dialog_Load_DXF (self, wingFn = self.wing, dxf_Path = current_dxf_path, ref=True, workingDir = self.workingDir) 
        self.wait_window (dxf_dialog)

        if dxf_dialog.return_OK: 
            new_dxf_Path = dxf_dialog.dxf_pathFilename
            if new_dxf_Path:                            # dialog returned a valid path 
                self.wing().refPlanform_DXF.set_dxfPathFilename (new_dxf_Path) 
                fireEvent(self.ctk_root, PLANFORM_CHANGED)
            else:                                       # remove dxf reference file - extra code to make it clear
                self.wing().refPlanform_DXF.set_dxfPathFilename (None) 
                fireEvent(self.ctk_root, PLANFORM_CHANGED)

    def onExit(self): 
        """ interception of user closing the app - check for changes made"""

        if self.wing().hasChanged(): 

            message = "There are unsaved changes.\n\n" + \
                       "Do you want to save before exit?"
            mb = Messagebox (self, title="Close "+ AppName, message=message, icon="warning", 
                           option_1="Yes", option_2="No", option_3="Cancel")
            response = mb.get()

            if response == "Yes":
                self.save()
                self.destroy()
            elif response == "No":
                self.destroy() 
            else:
                pass
        else:
            self.destroy()


#--------------------------------

if __name__ == "__main__":


    InfoMsg("Starting %s ..." % App.name)

    # set ctk application settings prior to init 

    Settings.belongTo (__file__, msg=True)

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

    # paramter file as argument?  

    parmFile = ''
    parser = argparse.ArgumentParser(prog=AppName, description='Create a wing planform')
    parser.add_argument("paramterfile", nargs='*', help="Paramter file .pc2")
    args = parser.parse_args()

    if args.paramterfile: 
        parmFile = args.paramterfile[0]
    else: 
        parmFile = Settings().get('lastOpenend', default=None) 

    if parmFile and not os.path.isfile (parmFile):
            ErrorMsg ("Parameter file '%s' doesn't exist" %parmFile )
            Settings().set('lastOpenend', None) 
            parmFile = None

    myApp = App(parmFile)
    myApp.mainloop()
 