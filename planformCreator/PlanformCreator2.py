#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""

                The Planform Creator App 

"""
import os
from tkinter import filedialog, messagebox
import customtkinter as ctk

import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg # use matplotlib together with tkinter

from model.wing_model   import Planform, Planform_Elliptical, Planform_Elliptical_StraightTE, \
                               Planform_DXF, Planform_Trapezoidal, Except_Planform_DXF_notValid, \
                               InfoMsg
from widgets            import * 
from wing_artist        import *


#------------------------------------------------

cl_background       = '#101010'                 # background color in diagrams


#   change events for updating mainly plots

WING_NEW                    = "<<WING_NEW>>"             #tk event types
WING_CHANGED                = "<<WING_CHANGED>>"             #tk event types
CHORD_CHANGED               = "<<CHORD_CHANGED>>"             
PLANFORM_CHANGED            = "<<PLANFORM_CHANGED>>"
SECTION_CHANGED             = "<<SECTION_CHANGED>>"
CURRENT_SECTION_CHANGED     = "<<CURRENT_SECTION_CHANGED>>"
DIAGRAMM_SECTION_SELECTED   = "<<DIAGRAMM_SECTION_SELECTED>>"
AIRFOIL_CHANGED             = "<<AIRFOIL_CHANGED>>"

ctk_root : ctk.CTk = None

def fireEvent(eventType): 
    print ("- fire event from root", eventType)
    if ctk_root: ctk_root.event_generate (eventType) 



class Edit(ctk.CTkFrame):
    """ 
    Master frame for user input 
    """
    # - holds the current data object wing, planform, wing secttion
    # - Handles the interaction between the different sub frames
    # - informs the diagramms about changes 
    #
    def __init__(self, master, wingFn, *args, **kwargs):
        super().__init__(master, *args, **kwargs)

        self.myApp   = master
        self._wingFn = wingFn

        self.curSection = self.wing.wingSections[0]

        self.editWing_frame          = Edit_Wing               (self, wingFn, width=300)
        self.editPlanform_frame      = Edit_Wing_PlanformType  (self, wingFn, width=500)
        self.editSectionMaster_frame = Edit_WingSection_Master (self, wingFn)

        # edit maingrid 1 x 4  with subframes

        self.grid_rowconfigure      (0, weight=1)
        self.grid_columnconfigure   (0, weight=0)
        self.grid_columnconfigure   (1, weight=0, minsize=400)
        self.grid_columnconfigure   (2, weight=0, minsize=400)
        self.grid_columnconfigure   (3, weight=1, minsize=400)

        self.editWing_frame.grid          (row=0, column=1, pady=5, padx=5, sticky="news")
        self.editPlanform_frame.grid      (row=0, column=2, pady=5, padx=5, sticky="news")
        self.editSectionMaster_frame.grid (row=0, column=3, pady=5, padx=5, sticky="news")

        # major bindings for change management

        ctk_root.bind(WING_NEW,                 self.new_wing, add='+')
        ctk_root.bind(WING_CHANGED,             self.changed_wing, add='+')
        ctk_root.bind(CHORD_CHANGED,            self.changed_chord, add='+')
        ctk_root.bind(PLANFORM_CHANGED,         self.changed_planform, add='+')
        ctk_root.bind(SECTION_CHANGED,          self.changed_section, add='+')

        # bindings from plot diagrams 
        ctk_root.bind(DIAGRAMM_SECTION_SELECTED,self.sectionSelected_in_diagram, add='+')

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
        self.editPlanform_frame.refresh()    
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

    def changed_planform (self, event): 
        """ Eventhandler for changes of planform data"""

        # doesn't change wing data
        # doesn't change wing sections - only changed_chord does 
    
    def changed_section (self, event): 
        """ Eventhandler for changes of section data"""
        # doesn't change wing data
        # could change planform
        if self.planform.planform_depend_on_sections:
            self.editPlanform_frame.refresh()   # e.g. chord changes section position 
        # refresh the other fields of current section
        self.editSectionMaster_frame.refresh_current()    # e.g. section position (wingSpan) 

    def sectionSelected_in_diagram (self, event): 
        """ Eventhandler for changes of the current wing section selection in diagrams"""
        # refresh the wing section edit frame with new section
        myApp : App = self.winfo_toplevel()
        self.editSectionMaster_frame.set_curSection (myApp.curWingSectionName(), inEvent=True)


#-------------------------------------------


class Edit_Abstract (ctk.CTkFrame):
    """ 
    Abstract superclass for all the planform type specific edit frames
    """

    def __init__(self, master, wingFn, *args, **kwargs):
        super().__init__(master, *args, **kwargs)

        self._wingFn = wingFn
        self.widgets = []
        self.init()
    
    @property
    def wing(self) -> Wing:
        return self._wingFn()
    
    @property
    def planform(self) -> Planform:
        return self.wing.planform


    def init(self):
        # overloaded by sub class
        pass

    def add (self, aWidget): 
        self.widgets.append (aWidget)

    def refresh(self):
        for widget in self.widgets:
            if isinstance(widget, Base_Widget): widget.refresh()
        print ("  - refresh in ", self.__class__.__name__," for %s widgets" % len(self.widgets))

        
#-------------------------------------------

class Edit_Wing(Edit_Abstract):
    """ 
    Frame to edit main data of the wing like wingspan. This is just the header
    """
    name = "Wing"

    def init (self):

        self.grid_rowconfigure      (1, weight=1)
               
        self.add (Header_Widget (self,0,0, lab=self.name, width=80))

        self.dataFrame = Edit_Wing_Data (self, self._wingFn, fg_color='transparent')
        self.dataFrame.grid (row=1, columnspan=2, pady=0, padx=(10,5), sticky="news")

    def refresh(self):
        # overloaded to refresh also child frame with data
        super().refresh()
        self.dataFrame.refresh()


        
#-------------------------------------------

class Edit_Wing_Data (Edit_Abstract):
    """ 
    Frame to edit main data of the wing like wingspan ...
    """
    name = "Wing"

    def init (self):

        unit = self.wing.unit
                
        self.add (Field_Widget  (self,1,0, lab="Name",              get=lambda  : self.wing.name, 
                                 event=WING_CHANGED,                set=lambda a: self.wing.set_name(a),
                                 width=140, columnspan= 2))
        self.add (Field_Widget  (self,2,0, lab="Wing span",         get=lambda  : self.wing.wingspan, 
                                 event=WING_CHANGED,                set=lambda a: self.wing.set_wingspan(a),
                                 lim=(100,20000), dec=1, spin= True, step=10, unit=unit))
        self.add (Field_Widget  (self,2,3, lab="Hinge angle",       get=lambda  : self.wing.hingeAngle, 
                                 event=WING_CHANGED,                set=lambda a: self.wing.set_hingeAngle(a),
                                 lim=(-5,45), dec=1, spin=True, step=0.1, unit="°"))
        self.add (Field_Widget  (self,3,0, lab="Chord at root",     get=lambda  : self.wing.rootchord, 
                                 event=WING_CHANGED,                set=lambda a: self.wing.set_rootchord(a),
                                 lim=(10,500), dec=1, spin=True, step=1, unit=unit))
        self.add (Field_Widget  (self,3,3, lab="Chord at tip",      get=lambda  : self.wing.tipchord, 
                                 event=CHORD_CHANGED,               set=lambda a: self.wing.set_tipchord(a),
                                 lim=(1,500), dec=1, spin=True, step=1, unit=unit))


        self.add (Field_Widget  (self,4,0, lab="Flap at root",      get=lambda  : self.wing.flapDepthRoot, 
                                 event=PLANFORM_CHANGED,            set=lambda a: self.wing.set_flapDepthRoot(a),
                                 lim=(0,50), dec=1, spin=True, step=0.5, unit='%'))
        self.add (Field_Widget  (self,4,3, lab="Flap at tip",       get=lambda  : self.wing.flapDepthTip, 
                                 event=PLANFORM_CHANGED,            set=lambda a: self.wing.set_flapDepthTip(a),
                                 lim=(0,50), dec=1, spin=True, step=0.5, unit='%'))
        
        Blank_Widget (self,5,0, width=20, height = 15) 

        self.add (Field_Widget  (self,6,0, lab="Re at root",  get=lambda: self.wing.rootReynolds, 
                                 event=SECTION_CHANGED,             set=lambda a: self.wing.set_rootReynolds(a),
                                 lim=(10,10000000), dec=0, spin=True, step=1000))
        self.add (Field_Widget  (self,6,3, lab="Airfoils nick", get=lambda  : self.wing.airfoilNickBase, 
                                 event=SECTION_CHANGED, width= 60,  set=lambda a: self.wing.set_airfoilNickBase(a)))


#-------------------------------------------

class Edit_Wing_PlanformType(Edit_Abstract):
    """ 
    Master frame for planform type specific sub frames
    """
    name = "Planform"

    def init (self):

        self.currentPlanform_frame = None

        # main grid:  header - sub Frame for specific planeform 
        self.grid_columnconfigure   (2, weight=1)
        self.grid_rowconfigure      (3, weight=1)

        self.add(Header_Widget (self,0,0, lab=self.name, width=110))
        self.add(Option_Widget (self,0,1, get=lambda: self.wing.planformType, set = self.set_planformType,
                                                    width = 150,
                                                    options=Planform.allTemplatePlanformTypes(),
                                                    event=PLANFORM_CHANGED))

        self.add(Label_Widget  (self,1, 0, lab= self.shortDescription))
        self.add(Blank_Widget  (self,2, 0, height = 10))

        # set inital planform to edit 
        self.set_planform_frame (self.wing.planformType)

    def shortDescription (self): 
        return self.wing.planform.shortDescription


    def set_planformType (self, aType):
        """ set a new planform selected by user
        """
        if self.wing.planformType != aType:
            try: 
                self.wing.set_planformType (aType) 
                handled = True
            except Except_Planform_DXF_notValid:
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

            if(newPlanformType =="elliptical"): 
                curFrame = Edit_Planform_Elliptical(self, self._wingFn, fg_color='transparent')
            elif(newPlanformType =="trapezoidal"): 
                curFrame = Edit_Planform_Trapezoid(self, self._wingFn, fg_color='transparent')
            elif(newPlanformType =="elliptical TE straight"): 
                curFrame = Edit_Planform_Elliptical_StraightTE(self, self._wingFn, fg_color='transparent')
            elif(newPlanformType =="DXF file"): 
                curFrame = Edit_Planform_DXF(self, self._wingFn, fg_color='transparent')
            else:
                raise ValueError ("No Edit_Frame for planform type ", newPlanformType)

            self.currentPlanform_frame = curFrame
            curFrame.grid (row=3, columnspan=3, pady=0, padx=(10,5), sticky="news")


    def handle_Planform_DXF_notValid (self): 

        # the current dxf planform doesn't have a valid planform 
        # aks for a new dxf file

        dxf_dialog = Dialog_Load_DXF (self, wing = self.wing, dxf_Path=None, ref=False) 

        self.wait_window (dxf_dialog)

        dxf_Path = dxf_dialog.dxf_pathFilename
        if dxf_Path:  
            self.wing.planform =  Planform_DXF( self.wing, dxf_Path= dxf_Path, ref = False)
            handled = True
            fireEvent (WING_CHANGED)                   # update hinge, flaps
        else: 
            handled = False

        return handled


#-------------------------------------------


class Edit_Planform_Elliptical(Edit_Abstract):
    """ 
    Frame to edit the parameters of a elliptical planform
    """
    name = Planform_Elliptical.planformType

    @property
    def planform(self) -> Planform_Elliptical:
        return self.wing.planform

    def init(self):

        self.grid_columnconfigure   (0, weight=0)
        self.grid_rowconfigure      (6, weight=1)


        self.add (Field_Widget  (self,0,0, lab="Tip belly",         get=lambda  : self.planform.ellipseTipBelly, 
                                    event=CHORD_CHANGED,            set=lambda a: self.planform.set_ellipseTipBelly(a),
                                    lim=(0,1), dec=2, spin=True, step=0.05))
        self.add (Field_Widget  (self,0,3,    lab="Tip belly width",get=lambda  : self.planform.ellipseBellyWidth, 
                                    event=CHORD_CHANGED,            set=lambda a: self.planform.set_ellipseBellyWidth(a),
                                    lim=(0,1), dec=2, spin=True, step=0.05))
        self.add (Field_Widget  (self,1,0,    lab="Ellipse shift",  get=lambda  : self.planform.ellipseShift, 
                                    event=CHORD_CHANGED,            set=lambda a: self.planform.set_ellipseShift(a),
                                    lim=(0,0.5), dec=2, spin=True, step=0.05))
        self.add (Field_Widget  (self,2,0,    lab="Ellipse correction",get=lambda: self.planform.ellipseCorrection,
                                    event=CHORD_CHANGED,            set=lambda a: self.planform.set_ellipseCorrection(a),
                                    lim=(-1,1), dec=2, spin=True, step=0.05))

        self.add (Field_Widget  (self,3,0,    lab="LE correction",  get=lambda  : self.planform.leCorrection,
                                    event=PLANFORM_CHANGED,         set=lambda a: self.planform.set_leCorrection(a),
                                    lim=(-1,1), dec=2, spin=True, step=0.05))
        


class Edit_Planform_Trapezoid (Edit_Abstract):
    """ 
    Frame to edit the parameters of a trapezoid planform
    """
    name = Planform_Trapezoidal.planformType

    @property
    def planform(self) -> Planform_Trapezoidal:
        return self.wing.planform

    def init(self):

        self.grid_columnconfigure   (0, weight=1)
        self.grid_columnconfigure   (2, weight=1)
        Blank_Widget (self,0,0,  height= 20)

        self.add(Label_Widget  (self,1,0, lab='Adapt wing sections to reference'))
        self.add(Button_Widget (self,1,1, lab='Adapt', width=60, set=self.adaptSections ))

    def adaptSections (self):
        # changes chord of sections to best fit to reference (elliptical) 
        self.planform.adjust_planeform_to_reference()
        fireEvent (PLANFORM_CHANGED)


#-------------------------------------------


class Edit_Planform_Elliptical_StraightTE (Edit_Abstract):
    """ 
    Frame to edit the parameters of a elliptical planform
    """
    name = Planform_Elliptical_StraightTE.planformType

    @property
    def planform(self) -> Planform_Elliptical_StraightTE:
        return self.wing.planform

    def init(self):

        self.add (Field_Widget  (self,0,0,    lab="Tip belly",   get=lambda: self.planform.ellipseTipBelly, 
                                    set=self.planform.set_ellipseTipBelly,
                                    lim=(0,1), dec=2, spin=True, step=0.05,
                                    event=CHORD_CHANGED))
        self.add (Field_Widget  (self,0,3,    lab="Tip belly width",   get=lambda: self.planform.ellipseBellyWidth, 
                                    set=self.planform.set_ellipseBellyWidth,
                                    lim=(0,1), dec=2, spin=True, step=0.05,
                                    event=CHORD_CHANGED))
        self.add (Field_Widget  (self,1,0,    lab="Ellipse shift",   get=lambda: self.planform.ellipseShift, 
                                    set=self.planform.set_ellipseShift,
                                    lim=(0,0.5), dec=2, spin=True, step=0.05,
                                    event=CHORD_CHANGED))
        self.add (Field_Widget  (self,2,0,    lab="Ellipse correction",   get=lambda: self.planform.ellipseCorrection,
                                    set=self.planform.set_ellipseCorrection,
                                    lim=(-1,1), dec=2, spin=True, step=0.05,
                                    event=CHORD_CHANGED))


#-------------------------------------------

class Edit_Planform_DXF (Edit_Abstract):
    """ 
    Frame to edit the parameters of a planform based on DXF
    """
    name = Planform_DXF.planformType

    @property
    def planform(self) -> Planform_DXF:
        return self.wing.planform


    def init(self):

        # self.grid_columnconfigure   (0, weight=1)
        self.grid_columnconfigure   (2, weight=1)
        self.grid_rowconfigure      (0, weight=0)
        self.grid_rowconfigure      (5, weight=1)

        self.add(Field_Widget  (self,1,0, lab='Current file',   width= 120, get= self.dxf_filename))
        self.add(Button_Widget (self,1,2, lab='Select',         width=90,   set=self.open_dxf_file ))


        Blank_Widget (self,3,0,  height= 5)
        self.add(Label_Widget  (self,4,0, lab=lambda: self.planform.infoText ))
        Blank_Widget (self,5,0,  height= 10)

    def dxf_filename(self): 
        return self.planform.dxf_filename()

    def open_dxf_file (self):

        wing = self.planform.wing
        current_dxf_path = self.planform.dxf_pathFilename()

        dxf_dialog = Dialog_Load_DXF (self, wing = wing, dxf_Path = current_dxf_path) 

        self.wait_window (dxf_dialog)

        new_dxf_Path = dxf_dialog.dxf_pathFilename
        if new_dxf_Path:                            # dialog returned a valid path 
            wing.planform.set_dxf_pathFilename (new_dxf_Path) 
            wing.planform.assignToWing()            # take over hinge, flap

            self.refresh()
            fireEvent (WING_CHANGED)



class Edit_WingSection_Master(Edit_Abstract):
    """ 
    Master frame for wingSection - to select, add, delete a wing section
    Is parent for a single wing section frame
    """
    name = "Sections"

    def init(self):

        # set first section as initial 
        self.curSection : WingSection = self.wing.wingSections[1]
        self.curSectionFrame = None

        # main grid:  header frame + - sub Frame for one section  
        hfrm = ctk.CTkFrame(self, fg_color='transparent')
        self.grid_columnconfigure   (1, weight=1)
        self.grid_rowconfigure      (1, weight=1)

        self.add(Header_Widget (hfrm,0,0, lab=self.name, width=100))

        self.add(Option_Widget (hfrm,0,1,   get=self.curSectionName, set=self.set_curSection,
                                            spin=True, width=100, options=self.sectionNames))
        self.add(Button_Widget (hfrm,0,2,   lab=' Add ',  width=50, set=self.addSection,
                                            disable=self.addDisabled))
        self.add(Button_Widget (hfrm,0,3,   lab='Delete',   width=50, set=self.deleteSection,
                                            disable=self.deleteDisabled))

        # Blank_Widget  (hfrm,0, 4, width=60)
        hfrm.grid (row=0, column=0, pady=0, padx=0, sticky="nwes")

        # init and set section data frame to current section 
        self.set_curSection (self.curSection.name())


    def curSectionName(self): return self.curSection.name()
    def sectionNames(self): 
        return [s.name() for s in self.wing.wingSections]   
    
    def deleteSection (self):
        """ delete the current, selected wing Section""" 
        if not self.deleteDisabled():
            # remind the neighbour before this section 
            leftSec, rightSec = self.wing.getNeighbourSectionsOf (self.curSection)
            self.wing.deleteSection (self.curSection)
            fireEvent (SECTION_CHANGED)         # update diagram
            # delete done - set option list to old neighbour 
            self.set_curSection(rightSec.name())

    def deleteDisabled (self):
        return self.curSection.isRoot or self.curSection.isTip
    

    def addSection (self):
        """ add a section after current Section""" 
        if not self.addDisabled():
            newSection = self.wing.createSectionAfter (self.curSection)
            fireEvent (SECTION_CHANGED)         # update diagram
            self.set_curSection(newSection.name())

    def addDisabled (self):
        return self.curSection.isTip

    def set_curSection (self, aName, inEvent=False):
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
        myApp : App = self.winfo_toplevel()
        myApp.set_curWingSectionName(aName)
        if not inEvent:                         # avoid event ping pong
            fireEvent (CURRENT_SECTION_CHANGED)    


    def get_SectionFromName (self, aName) -> WingSection:
        """ return the section with "aName"
        """
        for sec in self.wing.wingSections:
            if (sec.name() == aName): return sec
        raise ValueError ("Wing section not found: ", aName )


    def refresh_current (self):
        # refresh data of current section
        self.curSectionFrame.refresh()

    def reset (self):
        """reset everything - needed when a new wing is loaded """
        self.set_curSection (self.wing.wingSections[0].name())


class Edit_WingSection(Edit_Abstract):
    """ 
    Frame to edit the parameters of a single wing section 
    """
    name = "Wing Section"

    def __init__(self, master, wingFn, curSectionFunction, *args, **kwargs):

        self._curSectionFunction = curSectionFunction
        super().__init__(master, wingFn, *args, **kwargs)


    @property
    def wingSection (self) -> WingSection:
        return self._curSectionFunction()


    def init (self):

        unit = self.wingSection.wing.unit

        # special case! 
        # In this frame the dataObject (wingSection) changes during lifetime.
        # There the values can't be accessd via a 'bound method' in 'set' of the widget,
        # but is has to be a string which is evaluated during runtime

        self.add(Field_Widget  (self,1,0, lab="Position", obj=lambda: self.wingSection, get= 'yPos', set='set_yPos',
                                                lim='limits_yPos', dec=1, spin=True, step=2, unit=unit,
                                                disable='isRootOrTip', event=SECTION_CHANGED))
        self.add(Field_Widget  (self,1,3, lab="Relative Position", obj=lambda: self.wingSection, get='norm_yPos', set='set_norm_yPos',
                                                lim=(0.0,1.0), dec=2, spin=True, step=0.01, unit='%',
                                                disable='isRootOrTip', event=SECTION_CHANGED))
        # self.add(Label_Widget  (self,1,3, lab=self.fixedPositionText))

        self.add(Field_Widget  (self,2,0, lab="Chord", obj=lambda: self.wingSection, get='chord', set='set_chord',
                                                lim='limits_Chord', dec=1, spin=True, step=0.5, unit=unit,
                                                disable='isRootOrTip', event=SECTION_CHANGED))
        self.add(Field_Widget  (self,2,3, lab="Relative chord", obj=lambda: self.wingSection, get='norm_chord', set='set_norm_chord',
                                                lim='limits_normChord', dec=2, spin=True, step=0.01, unit='%',
                                                disable='isRootOrTip', event=SECTION_CHANGED))

        self.add(Field_Widget  (self,3,0, lab="Reynolds", obj=lambda: self.wingSection ,get='Re', set='set_Re',
                                                lim='limits_Re', dec=0, spin=True, step=1000,
                                                disable=False, event=SECTION_CHANGED))
        #                                        disable='isReDisabled', event=SECTION_CHANGED))

        # self.add(Label_Widget  (self,3,3, lab=self.fixedPosAndChordText))

        Blank_Widget (self,4,0, width=20, height = 10) 
        self.add(Field_Widget  (self,5,0, lab="Airfoil", get=lambda: self.wingSection.airfoilName(), set='',
                                                disable=True, event=SECTION_CHANGED))
        
        self.add(Button_Widget (self,5,2, lab='Select', width=70, set=self.select_airfoil ))
        self.add(Button_Widget (self,5,3, lab='Remove', width=70, set=self.remove_airfoil, 
                                disable=self.remove_airfoil_disable ))

        Blank_Widget (self,6,0, width=20, height = 10) 
        self.add(Field_Widget  (self,7,0, lab="Flap group", obj=lambda: self.wingSection ,get='flapGroup', set='set_flapGroup',
                                                lim=(0,9), dec=0, spin=True, step=1,
                                                disable='isTip', event=SECTION_CHANGED))

        self.grid_columnconfigure   (5, weight=1)


    def fixedPositionText (self): 
        text = ""
        if self.wingSection.hasFixedPosition():
            if   self.wingSection.isRoot: text = "Root "
            elif self.wingSection.isTip:  text = "Tip "
            text +="fixed"
        return text

    def fixedPosAndChordText (self): 
        text = ""
        if self.wingSection.hasFixedPositionAndChord():
            text ="fixed also chord"
        return text
    
    def select_airfoil(self):
        """ select airfoil with explorer and load it if possible """

        filetypes  = [('dat files', '*.dat')]
        pathFile   = self.wingSection.airfoil.pathFileName
        initialDir = os.path.dirname(pathFile) if pathFile != None else ''

        newPathFilename = filedialog.askopenfilename(
                    title='Select airfoil file',
                    initialdir=initialDir,
                    filetypes=filetypes)

        if newPathFilename: 
            self.wingSection.set_airfoilWithPathFileName(newPathFilename)
            self.refresh()
            fireEvent (AIRFOIL_CHANGED)

    def remove_airfoil(self):

        if self.wingSection.airfoil_canBeRemoved():
            self.wingSection.set_airfoilWithPathFileName(None)
            self.refresh()
            fireEvent (AIRFOIL_CHANGED)
        
    def remove_airfoil_disable (self):
        return not self.wingSection.airfoil_canBeRemoved()


#--------------- Diagramm Plots --------------------------

class Diagrams(ctk.CTkTabview):
    """ 
    Master frame for diagrams - childs are the different plot frames 
    """
    def __init__(self, master, wingFn, *args, **kwargs):
        super().__init__(master, *args, **kwargs)
        """
        Args:
            :master: frame self belongs to
        """
        
        self._wingFn = wingFn

        self.myPlot_frames = []
        # Get all subclasses of Plot_Frame and create a tab for each 
        for plot_cls in Diagram_Abstract.__subclasses__():
            tab_frame = self.add("   " + plot_cls.name + "   ")
            tab_frame.grid_columnconfigure(0, weight=1)
            tab_frame.grid_rowconfigure(0, weight=1)

            self.myPlot_frames.append (plot_cls (tab_frame,  self._wingFn, diagram_master=self))

        # set size of tab view titles
        self._segmented_button.configure(font=("", 16))
        self.configure(fg_color=cl_background)
        self.configure(command=self.newTabSelected)
        self.newTabSelected ()              

    def newTabSelected (self):
        # activate the new selected frame for updates 
        newName = self.get()
        plot_frame : Diagram_Abstract = None
        for plot_frame in self.myPlot_frames:
            plot_frame.setActive (False)                    # first deactivate all
            if (plot_frame.name.strip() == newName.strip()): 
                newPlot_frame = plot_frame

        if newPlot_frame: newPlot_frame.setActive(True)    #  eactivate new one 

    @property
    def wing(self) -> Wing:
        return self._wingFn()



#---------------- plot frames ----------------------

class Diagram_Abstract(ctk.CTkFrame):
    """ 
    Abstract super class of the specific plot frames like "Plot_Planform"
    """
    name = "This is the plot super class"
    defaultTip = "Use switches to adapt diagram view"

    def __init__(self, master, wingFn, *args, diagram_master = None,  **kwargs):
        super().__init__( master, *args, **kwargs)

        self.master : Diagrams = diagram_master
        self._wingFn = wingFn

        self.configure(fg_color= cl_background)

        self.grid_columnconfigure(0, weight=1)
        self.grid_rowconfigure(0, weight=1)
        self.grid(row=0, column=0, sticky="news")

        self.figure : plt.Figure = plt.Figure()

        # connect tk and pyplot
        self.canvas = FigureCanvasTkAgg(self.figure, self)
        self.canvas._tkcanvas.grid_columnconfigure(0, weight=1)
        self.canvas._tkcanvas.grid (row=0, column=0, pady=0, padx=0, sticky="news")
        self.canvas._tkcanvas.configure(background= cl_background)

        # add an info  view frame below the plot 
        self.view_frame = ctk.CTkFrame (self, fg_color= cl_background)
        self.view_frame.grid(row=1, column=0, pady=0, padx=0, sticky="ew")


        self.axes : plt.Axes = self.figure.add_subplot()        # the pyplot axes this diagram is plotted
        self.setup_axes ()
        self.setup_artists ()

        # Define Frame below plot with all the on/offswitches 
        # Create the artists for the diagramm

        # grid on / off is always available 
        col = 0 
        Switch_Widget (self.view_frame,0,col, lab='Grid', 
                       get=lambda: self.gridArtist.show, set=self.gridArtist.set_show)
        
        col += 1
        self.view_frame.grid_columnconfigure(col, weight=1)

        # add user tip label 
        col += 1
        self.tip = ctk.CTkLabel(self.view_frame, text=self.defaultTip, text_color ="goldenrod2")
        self.tip.grid(row=0, column=col, padx=30, pady= 0, sticky='w')

        self.setup_Switches (col=col)                       # init of switches / plots

        self.view_frame.grid_columnconfigure(10, weight=2)  # center switches in middle 


        self.setChangeBindings ()
        self._active            = False                     # is active frame? control change events 


    @property
    def wing(self) -> Wing:
        return self._wingFn()
    
    # ----- abstract - to overlaod

    def setup_axes(self):
        """ setup axes, axis for this plot type """

        self.figure.subplots_adjust(left=0.03, bottom=0.07, right=0.98, top=0.97, wspace=None, hspace=None)


    def setup_artists(self):
        """ setup artists for this plot type """
        self.gridArtist = Grid_Artist (self.axes, self._wingFn, show=True)
        self.gridArtist.plot()          # force to show first time


    def setup_Switches(self, col=0):
        """ define on/off switches ffor this plot type"""
        pass

    def refresh(self, dummy): 
        # overwrite in sub class
        if self._active:
            pass  

    # ----- view Switches

    def showTip(self, myTip): 
        self.tip.configure(text=myTip)

    # ----- general refresh when getting active view again

    def setActive(self, active: bool):
        # the Diagramm master (Tabview) will activate/deactivate to avoid plot generation
        #   if self is not visible
        if active: 
            self._active = True
            print ("Actived - refresh artists: ", self)
            self.refresh()
        else: 
            self._active = False

    # ----- change event setup 

    def setChangeBindings (self):
        # overwrite in sub class
        pass      


#-------------------------------------------------------------------------------
# Planform   
#-------------------------------------------------------------------------------

class Diagram_Planform (Diagram_Abstract):
    """ 
    Frame to plot planform 
    """
    name = "Planform"

    def setup_axes (self):
        """ setup axes, axis for this plot type """
        super().setup_axes ()

        self.axes.set_ylim([self.wing.rootchord, 0.0])
        self.axes.set_xlim([-0.05 * self.wing.halfwingspan, self.wing.halfwingspan * 1.05])
        self.axes.axis('equal')


    def setup_artists (self):
        """ setup artists for this plot type """

        super().setup_artists()
        self.planformArtist     = Planform_Artist (self.axes, self._wingFn, show=True)
        self.chordLinesArtist   = ChordLines_Artist (self.axes, self._wingFn, show=False)
        self.curSectionArtist   = CurrentWingSection_Artist (self.axes, self._wingFn, show=True)
        self.sectionsArtist     = WingSections_Artist (self.axes, self._wingFn, onPick=self.sectionPicked)     
        self.flapArtist         = Flap_Artist (self.axes, self._wingFn)
        self.referenceArtist    = RefPlanform_Artist (self.axes, self._wingFn)
        self.dxfArtist          = RefPlanform_DXF_Artist (self.axes, self._wingFn)


    def setup_Switches (self, col = 0):
        """ define on/off switches for this plot types"""

        # the whole plot work will be done by the artists
        # plot the real planform on top     
        col += 1
        Switch_Widget (self.view_frame,0,col, lab='Planform', 
                get=lambda: self.planformArtist.show, set=self.planformArtist.set_show)
        col += 1
        Switch_Widget (self.view_frame,0,col, lab='Chord lines', 
                get=lambda: self.chordLinesArtist.show, set=self.chordLinesArtist.set_show)
        col += 1
        Switch_Widget (self.view_frame,0,col, lab='Wing sections', 
                get=lambda: self.sectionsArtist.show, set=self.sectionsArtist.set_show)
        col += 1
        Switch_Widget (self.view_frame,0,col, lab='Current section', 
                get=lambda: self.curSectionArtist.show, set=self.curSectionArtist.set_show)
        col += 1
        Switch_Widget (self.view_frame,0,col, lab='Flaps', 
                get=lambda: self.flapArtist.show, set=self.flapArtist.set_show)
        col += 1
        Switch_Widget (self.view_frame,0,col, lab='Reference', 
                get=lambda: self.referenceArtist.show, set=self.referenceArtist.set_show)

        if (self.wing.refPlanform_DXF): 
            col += 1
            Switch_Widget (self.view_frame,0,col, lab='Reference DXF', 
                    get=lambda: self.dxfArtist.show, set=self.dxfArtist.set_show)
            
        # --- some info  - currently temp 

        self.axes.text(.95,.9, self.wing.name, fontsize ='x-large', ha='right', transform=self.axes.transAxes)


    # -------- event handler

    def setChangeBindings (self):
        # overloaded
        ctk_root.bind(WING_NEW,                 self.wing_new, add='+')
        ctk_root.bind(WING_CHANGED,             self.changed_wing, add='+')
        ctk_root.bind(CHORD_CHANGED,            self.changed_chord, add='+')
        ctk_root.bind(PLANFORM_CHANGED,         self.changed_planform, add='+')
        ctk_root.bind(SECTION_CHANGED,          self.changed_sections, add='+')
        ctk_root.bind(CURRENT_SECTION_CHANGED,  self.changed_currentSection, add='+')

    def wing_new (self, dummy): 
        """ Eventhandler for new wing """
        self.axes.clear()
        self.changed_wing (dummy)
        self.changed_currentSection (dummy)

    def changed_wing (self, dummy): 
        """ Eventhandler for changes of the complete wing - like wing span """
        # self.axes.clear()
        self.setup_axes()                       # maybe the axis limits changed
        self.gridArtist.plot()
        self.refresh()
        self.refresh_sections() 

    def changed_chord (self, dummy): 
        """ Eventhandler for changes of chord"""
        self.refresh()
        self.refresh_sections() 

    def changed_planform (self, dummy): 
        """ Eventhandler for changes of planform"""
        self.refresh()
        self.refresh_sections() 

    def changed_sections (self, dummy): 
        """ Eventhandler for changes of planform"""
        if self.wing.planform.planform_depend_on_sections:
            self.refresh()
        self.refresh_sections() 

    def changed_currentSection(self, dummy): 
        """" eventhandler - current has changed from outside """
        if self._active:
            #retrieve new section name from App 
            myApp : App = self.winfo_toplevel()
            self.curSectionArtist.set_current (myApp.curWingSectionName(), figureUpdate=True)  


    # -------- refresh my Artists which are on 'show mode' 

    def setActive(self, active: bool):
        # overloaded to set active section 
        if active: 
            myApp : App = self.winfo_toplevel()
            self.curSectionArtist.set_current (myApp.curWingSectionName(), figureUpdate=False)    
        super().setActive(active)


    def refresh(self): 
        # overloaded
        if self._active:
            self.planformArtist.refresh ()  
            self.chordLinesArtist.refresh ()  
            self.curSectionArtist.refresh()
            self.referenceArtist.refresh() 
            self.dxfArtist.refresh      () 
            self.flapArtist.refresh     () 
            self.axes.figure.canvas.draw_idle()    # draw ony if Windows is idle!
            print ("  - refresh in ", self.__class__.__name__," for active artists")

    def refresh_sections(self): 
        if self._active:
            self.sectionsArtist.refresh ()  
            self.flapArtist.refresh     () 
            self.curSectionArtist.refresh()

            self.axes.figure.canvas.draw_idle()    # draw ony if Windows is idle!
            print ("  - refresh sections in ", self.__class__.__name__," for active artists")

    # -------- pick object in axes handling  

    def sectionPicked (self, aSectionLabel):
        # callback method - the user pciked a wing section in the plot
        myApp : App = self.winfo_toplevel()
        myApp.set_curWingSectionName(aSectionLabel)
        fireEvent (DIAGRAMM_SECTION_SELECTED)
        self.curSectionArtist.set_current (aSectionLabel, figureUpdate=True)  



#-------------------------------------------------------------------------------
# Chord   
#-------------------------------------------------------------------------------

class Diagram_ChordDistribution (Diagram_Abstract):
    """ 
    Frame to plot chord distribution
    """
    name = "Chord distribution"

    def setup_axes(self):
        """ setup axes, axis, artiss for this plot type """
        super().setup_axes ()

        self.axes.set_ylim([ 0.0, 1.1])
        self.axes.set_xlim([-0.1, 1.1])
        self.axes.text(.95,.9, self.wing.name, fontsize ='x-large', ha='right', transform=self.axes.transAxes)

    def setup_artists(self):
        """ setup axes, axis, artists for this plot type """

        super().setup_artists()
        self.chordArtist        = Chord_Artist (self.axes, self._wingFn, show=True)
        self.chordLinesArtist   = ChordLines_Artist (self.axes, self._wingFn, norm=True, show=False)
        self.curSectionArtist   = CurrentWingSection_Artist (self.axes, self._wingFn, norm=True, show=True)
        self.sectionsArtist     = WingSections_Artist (self.axes, self._wingFn, show=True, norm=True,
                                                       onPick=self.sectionPicked)
        self.referenceArtist    = RefChord_Artist (self.axes, self._wingFn, norm=True)
        self.dxfArtist          = RefChord_DXF_Artist (self.axes, self._wingFn, norm=True)


    def setup_Switches(self, col=0):
        """ define on/off switches ffor this plot type"""

        # col += 1
        # Switch_Widget  (self.view_frame,0,col, lab='Chord distribution', 
        #                 get=lambda: self.chordArtist.show, set=self.chordArtist.set_show)
        col += 1
        Switch_Widget (self.view_frame,0,col, lab='Chord lines', 
                get=lambda: self.chordLinesArtist.show, set=self.chordLinesArtist.set_show)
        col += 1
        Switch_Widget  (self.view_frame,0,col, lab='Wing sections', 
                        get=lambda: self.sectionsArtist.show, set=self.sectionsArtist.set_show)
        col += 1
        Switch_Widget (self.view_frame,0,col, lab='Current section', 
                get=lambda: self.curSectionArtist.show, set=self.curSectionArtist.set_show)
        col += 1
        Switch_Widget  (self.view_frame,0,col, lab='Reference', 
                        get=lambda: self.referenceArtist.show, set=self.referenceArtist.set_show)
        if (self.wing.refPlanform_DXF.isValid): 
            col += 1
            Switch_Widget  (self.view_frame,0,col, lab='Reference DXF', 
                            get=lambda: self.dxfArtist.show, set=self.dxfArtist.set_show)


    # -------- event handler

    def setChangeBindings (self):
        # overloaded
        ctk_root.bind(WING_NEW,                 self.wing_new, add='+')
        ctk_root.bind(CHORD_CHANGED,            self.changed_chord, add='+')
        ctk_root.bind(PLANFORM_CHANGED,         self.changed_planform, add='+')
        ctk_root.bind(SECTION_CHANGED,          self.changed_sections, add='+')
        ctk_root.bind(CURRENT_SECTION_CHANGED,  self.changed_currentSection, add='+')

    def wing_new (self, dummy): 
        """ Eventhandler for new wing """
        self.changed_chord (dummy)
        self.changed_currentSection (dummy)

    def changed_chord (self, dummy): 
        """ Eventhandler for changes of chord"""
        self.refresh()
        self.refresh_sections() 

    def changed_planform (self, dummy): 
        """ Eventhandler for changes of planform"""
        # only sections here
        self.refresh_sections() 

    def changed_sections (self, dummy): 
        """ Eventhandler for changes of planform"""
        if self.wing.planform.planform_depend_on_sections:
            self.refresh()
        self.refresh_sections() 

    def changed_currentSection(self, dummy): 
        """" eventhandler - current has changed from outside """
        if self._active:
            #retrieve new section name from App 
            myApp : App = self.winfo_toplevel()
            self.curSectionArtist.set_current (myApp.curWingSectionName(), figureUpdate=True)  

    # -------- refresh my Artists which are on 'show mode' 

    def setActive(self, active: bool):
        # overloaded to set active section 
        if active: 
            myApp : App = self.winfo_toplevel()
            self.curSectionArtist.set_current (myApp.curWingSectionName(), figureUpdate=False)    
        super().setActive(active)


    def refresh(self): 
        # overloaded
        if self._active:
            self.chordArtist.refresh ()  
            self.chordLinesArtist.refresh ()  
            self.sectionsArtist.refresh ()  
            self.curSectionArtist.refresh()
            self.referenceArtist.refresh () 
            self.dxfArtist.refresh () 
            self.axes.figure.canvas.draw_idle()    # draw ony if Windows is idle!
            print ("  - refresh in ", self.__class__.__name__," for active artists")

    def refresh_sections(self): 
        if self._active:
            self.sectionsArtist.refresh ()  
            self.curSectionArtist.refresh()
            self.axes.figure.canvas.draw_idle()    # draw ony if Windows is idle!
            print ("  - refresh sections in ", self.__class__.__name__," for active artists")
  
    # -------- pick object in axes handling  

    def sectionPicked (self, aSectionLabel):
        # call method - the user pciked a wing section in the plot
        myApp : App = self.winfo_toplevel()
        myApp.set_curWingSectionName(aSectionLabel)
        fireEvent (DIAGRAMM_SECTION_SELECTED)
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

    def setup_axes(self):
        """ setup axes, axis, artiss for this plot type """
        super().setup_axes ()

        self.axes.axis('equal')


    def setup_artists(self):
        """ setup axes, axis, artists for this plot type """

        super().setup_artists()
        self.airfoilArtist      = Airfoil_Artist (self.axes, self._wingFn, show=True, norm=True, 
                                                  strak=False, onPick=self.airfoilPicked)


    def setup_Switches(self, col=0):
        """ define on/off switches ffor this plot type"""
        col += 1
        Switch_Widget (self.view_frame,0,col, lab='In real size', 
                       get=lambda: self.airfoilArtist.abs, set=self.airfoilArtist.set_abs)
        col += 1
        Switch_Widget (self.view_frame,0,col, lab='Straked airfoils', 
                       get=self.show_strakedAirfoils, set=self.set_show_strakedAirfoils)


    # -------- switch call back 

    def show_strakedAirfoils (self): 

        return self._show_strakedAirfoils

    def set_show_strakedAirfoils (self, aBool):
        # handled not in Artist to allow a re-gnerating of straked airfoils 

        if aBool: 
            self.wing.do_strak()                    # recalculate strak airfoils 
            self.airfoilArtist.set_strak (True)
            self.refresh()
        else: 
            self.airfoilArtist.set_strak (False)
        self._show_strakedAirfoils = aBool 

    # -------- event handler

    def setChangeBindings (self):
        # overloaded
        ctk_root.bind(WING_NEW,                 self.wing_new, add='+')
        ctk_root.bind(AIRFOIL_CHANGED,          self.changed_airfoil, add='+')
        # these could change chord of wingsection (abs size of airfoil) or strak airfoil
        ctk_root.bind(WING_CHANGED,             self.changed_airfoil, add='+')
        ctk_root.bind(CHORD_CHANGED,            self.changed_airfoil, add='+')
        ctk_root.bind(PLANFORM_CHANGED,         self.changed_airfoil, add='+')
        ctk_root.bind(SECTION_CHANGED,          self.changed_airfoil, add='+')

        ctk_root.bind(CURRENT_SECTION_CHANGED,  self.changed_currentSection, add='+')

    def wing_new (self, dummy): 
        """ Eventhandler for new wing """
        self.set_show_strakedAirfoils (False)
        self.changed_airfoil (dummy)
        self.changed_currentSection (dummy)

    def changed_airfoil(self, dummy): 
        """ Eventhandler for changes of airfoil"""
        if self._active:
            self.airfoilArtist.set_strak (False)            # switch off - strak airfoils could have changed
            self.refresh()

    def changed_currentSection(self, dummy): 
        """" eventhandler - current has changed from outside """
        if self._active:
            #retrieve new section name from App 
            myApp : App = self.winfo_toplevel()
            self.airfoilArtist.set_current (myApp.curWingSectionName(), figureUpdate=True)  
            print ("  - set current airfoil in ", self.__class__.__name__," for airfoil artists")

    # -------- refresh my Artists which are on 'show mode' 

    def setActive(self, active: bool):
        # overloaded to set active section 
        if active: 
            myApp : App = self.winfo_toplevel()
            self.airfoilArtist.set_current (myApp.curWingSectionName(), figureUpdate=True)  
            print ("  - set current in ", self.__class__.__name__," for active artists")
        super().setActive(active)

    def refresh(self): 
        # overloaded
        if self._active:
            self.airfoilArtist.refresh ()  
            self.axes.figure.canvas.draw_idle()    # draw ony if Windows is idle!
            print ("  - refresh in ", self.__class__.__name__," for active artists")

    # -------- pick object in axes handling  

    def airfoilPicked (self, aAirfoilLabel):
        # call method - the user pciked a wing section in the plot
        self.airfoilArtist.set_current (aAirfoilLabel, figureUpdate=True)  
        sectionName = aAirfoilLabel.split(":")[0]
        myApp : App = self.winfo_toplevel()
        myApp.set_curWingSectionName(sectionName)
        fireEvent (DIAGRAMM_SECTION_SELECTED)





#-------------------------------------------------------------------------------
# Dialogs for smaller tasks   
#-------------------------------------------------------------------------------

class Dialog_Abstract (ctk.CTkToplevel):
    """ 
    Abstract superclass for windows dialogs 
    """
    width  = 500
    height = 400
    titleText  = "My little title"

    def __init__(self, master, *args, **kwargs):
        super().__init__(master, *args, **kwargs)

        # self.deiconify()
        xPos, yPos = self.centerPosition()
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
        self.edit_frame.grid    (row=0, column=0, pady=20, padx=20, sticky="nesw")

        self.widgets = []                                   # for refresh logic  


    def ok (self):
        # to over load and do ok actions
        self.destroy()

    def cancel (self): 
        # to over load and do cancel actions
        self.destroy()

    def add (self, aWidget): 
        self.widgets.append (aWidget)

    def refresh(self):
        for widget in self.widgets:
            if isinstance(widget, Base_Widget): widget.refresh()

    def centerPosition(self):
        """ get center of a tkinter window
        """
        self.update_idletasks()
        width  = self.winfo_width()
        height = self.winfo_height()
        frm_width = self.winfo_rootx() - self.winfo_x()
        win_width = width + 2 * frm_width
        titlebar_height = self.winfo_rooty() - self.winfo_y()
        win_height = height + titlebar_height + frm_width
        x = self.winfo_screenwidth() // 2 - win_width // 2
        y = self.winfo_screenheight() // 2 - int (win_height / 1.7)
        return x, y


class Dialog_Load_DXF (Dialog_Abstract):
    """ 
    Import dxf file to be overlay or template planform 
    """
    width  = 500
    height = 300
    titleText  = "Import dxf file"

    def __init__(self, master, *args, wing:Wing = None, dxf_Path= None, ref:bool = False, **kwargs):
        super().__init__(master, *args, **kwargs)

        frame = self.edit_frame
        self.dxf_pathFilename = None                    # the return value 
        self.tmpPlanform = Planform_DXF( wing, dxf_Path= dxf_Path, ref = ref)

        if self.tmpPlanform.dxf_isReference:
            shortDescription = "The wing contour in the dxf file will be used as \n" +\
                               "a reference overlay for the active planform." 
            headerText = "DXF contour as reference"
        else:
            shortDescription = "The dxf wing contour will be the base of the current planform." 
            headerText = "DXF contour for planform"


        r = 0 
        Header_Widget (frame,r       ,0, lab=headerText, width=110, columnspan=3)
        Label_Widget  (frame,r:=r+1,0, lab= lambda: shortDescription)
        Blank_Widget  (frame,r:=r+1,0, height = 10)

        self.add(Field_Widget  (frame,r:=r+1,0, lab='DXF file', width= 120, get= self.tmpPlanform.dxf_filename))
        self.add(Button_Widget (frame,r       ,2, lab='Select', width=90, set=self.open_dxf_file ))
        self.add(Button_Widget (frame,r       ,3, lab='Remove', width=70, set=self.remove_dxf_file, 
                                disable=self.remove_dxf_disable ))

        Blank_Widget (frame,r:=r+1,0,  height= 5)
        self.add(Label_Widget (frame,r:=r+1,0, lab=lambda: self.tmpPlanform.infoText ))
        Blank_Widget (frame,r:=r+1,0,  height= 10)
        frame.grid_rowconfigure (r, weight=1)

        self.add(Button_Widget (frame,r:=r+1,1, lab='Ok',    set=self.ok,     width=100,
                                disable= lambda: not self.tmpPlanform.isValid ))
        self.add(Button_Widget (frame,r     ,2, lab='Cancel',set=self.cancel, width= 100))

        Blank_Widget  (frame,r     ,3, height= 10)
        frame.grid_columnconfigure (3, weight=1)


    def open_dxf_file (self):

        filetypes  = [('dxf files', '*.dxf')]
        pathFile   = self.tmpPlanform.dxf_pathFilename()
        initialDir = os.path.dirname(pathFile) if pathFile != None else ''

        newPathFilename = tk.filedialog.askopenfilename(
                    title='Open dxf file',
                    initialdir=initialDir,
                    filetypes=filetypes)

        if newPathFilename: 
            self.tmpPlanform.set_dxf_pathFilename (newPathFilename)
            self.refresh()

    def remove_dxf_file (self):
            self.tmpPlanform.set_dxf_pathFilename ("")
            self.refresh()

    def remove_dxf_disable (self):
            disable =  not self.tmpPlanform.dxf_pathFilename ()
            return disable

    def ok (self):
        # to over load and do ok actions
        if self.tmpPlanform.isValid: 
            self.dxf_pathFilename = self.tmpPlanform.dxf_pathFilename ()  # the return value 
        self.destroy()

    def cancel(self): 
        super().cancel()


#-------------------------------------------------------------------------------
# The App   
#-------------------------------------------------------------------------------

#-------------------------------------------


class Edit_File_Menu(Edit_Abstract):
    """ 
    Frame for the high level commands like load, save, ...
    The parent is the App itself
    """
    name = "File"

    def __init__(self, master, wingFn, *args, **kwargs):

        # special here - self is child of App - needed for callbacks
        self.myApp : App  = master

        super().__init__(master, wingFn, *args, **kwargs)


    def init (self):

        self.grid_columnconfigure   (0, weight=0)
        self.grid_rowconfigure      (7, weight=1)

        Header_Widget (self,0,0, lab=self.name)

        Button_Widget (self,1,0, lab='New',         width=100, set=self.myApp.new)
        Button_Widget (self,2,0, lab='Open',        width=100, set=self.myApp.open)
        Button_Widget (self,3,0, lab='Save',        width=100, disable=True)
        Button_Widget (self,4,0, lab='Save As...',  width=100, disable=True)
        Button_Widget (self,5,0, lab='Import',      width=100, set=self.load_dxf)

        # Option_Widget (self,6,0, val="",            width = 100,
        #                                            set = self.set_importType,
        #                                            options=self.importChoices())

    def importChoices (self): 
        return ["Import ...", "Dxf as reference", "Maybe other things"]

    def set_importType (self, aType):
        if aType == "Dxf as reference":
            self.load_dxf() 


    def load_dxf (self): 
        """ load a dxf planform into the reference_dxf planform"""
        current_dxf_path = self.wing.refPlanform_DXF.dxf_pathFilename()

        dxf_dialog = Dialog_Load_DXF (self, wing = self.wing, dxf_Path = current_dxf_path, ref=True) 

        self.wait_window (dxf_dialog)
        new_dxf_Path = dxf_dialog.dxf_pathFilename
        if new_dxf_Path:                            # dialog returned a valid path 
            self.wing.refPlanform_DXF.set_dxf_pathFilename (new_dxf_Path) 
            fireEvent(PLANFORM_CHANGED)



class App(ctk.CTk):

    def __init__(self, myWing: Wing):
        super().__init__()

        global ctk_root                         # being event handler

        self._myWing = myWing

        # configure customtkinter
        self.appearance_mode = "Dark" # Modes: "System" (standard), "Dark", "Light"
        ctk.set_appearance_mode(self.appearance_mode)    # Modes: "System" (standard), "Dark", "Light"
        ctk.set_default_color_theme("blue") # Themes: "blue" (standard), "green", "dark-blue"

        # set window title
        self.title("Planform Creator [" + self.wing().name + "]")

        # maximize the window using state property
        # self.state('zoomed')

        # register events which will be fired when editing the planform to update views
        ctk_root = self
        Base_Widget.ctk_root = self
        self.event_add(CHORD_CHANGED,'None')
        self.event_add(PLANFORM_CHANGED,'None')
        self._curWingSectionName = None         # Dispatcher field between Diagramm and Edit

        # create main frames        border_width= 1, ,border_width= 1
        diagram_frame = Diagrams        (self, self.wing, fg_color= cl_background)
        edit_frame    = Edit            (self, self.wing, height=500)
        file_frame    = Edit_File_Menu  (self, self.wing, width=200)

        # maingrid 2 x 2 - diagram on top, edit on bottom
        self.grid_rowconfigure   (0, weight=1)
        self.grid_rowconfigure   (1, weight=1)
        self.grid_columnconfigure(1, weight=1)
        diagram_frame.grid (row=0, column=0, columnspan = 2, pady=0, padx=0, sticky="news")
        file_frame.grid    (row=1, column=0, pady=0, padx=0, sticky="nesw")
        edit_frame.grid    (row=1, column=1, pady=0, padx=0, sticky="nesw")


    def wing (self):
        """ encapsulates current wing. Childs should acces only via this function
        to enable a new wing to be set """
        return self._myWing

    def set_wing (self, aNewWing):
        """ encapsulates current wing. Childs should acces only via this function
        to enable a new wing to be set """
        self._myWing = aNewWing
        # mega alarm - inform everything
        fireEvent (WING_NEW)

    
    def curWingSectionName (self):
        """ Dispatcher for current WingSection between Edit and Diagramm """
        return self._curWingSectionName 
    
    def set_curWingSectionName (self, aName):
        """ Dispatcher for current WingSection between Edit and Diagramm """
        self._curWingSectionName = aName


    def new (self):
        """ reset - and start with example ddefinition"""

        msg =  "The current wing '%s' will be discarded." % self.wing().name
        answer = messagebox.askokcancel("Create new wing",msg , icon="warning")
        
        if answer == True:
            newWing = Wing.onFile ('')
            if newWing:
                self.set_wing (newWing)
        

    def open (self):
        """ open a new wing definition json and load it"""

        filetypes  = [('PlaneformCreator2 files', '*.json')]
        # pathFile   = self.wingSection.airfoil.pathFileName
        # initialDir = os.path.dirname(pathFile) if pathFile != None else ''
        initialDir = "."

        newPathFilename = tk.filedialog.askopenfilename(
                    title='Select new wing definition',
                    initialdir=initialDir,
                    filetypes=filetypes)

        if newPathFilename: 
            newWing = Wing.onFile (newPathFilename)
            if newWing:
                self.set_wing (newWing)


if __name__ == "__main__":

    # init colorama
    just_fix_windows_console()

    # myWing = Wing.onFile (".\\examples\\vjx.glide\\VJX.glide.json")
    myWing = Wing.onFile (".\\examples\\Amokka-JX\\Amokka-JX.json")
    # myWing = Wing.onFile ("")

    InfoMsg("Starting  User Interface...")

    myApp = App(myWing)
    myApp.mainloop()
 