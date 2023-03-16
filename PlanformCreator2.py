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

        |-- Diagramms                           - the master to select one of the diagrams
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
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg # use matplotlib together with tkinter

from tkinter import filedialog, Frame
import customtkinter as ctk
from modules.common_utils       import * 
from modules.wing_model         import Planform, Planform_Elliptical, Planform_Elliptical_StraightTE, \
                                       Planform_DXF, Planform_Trapezoidal                          
from modules.widgets            import * 
from modules.wing_artist        import *


#------------------------------------------------

AppName    = "Planform Creator 2"
AppVersion = "0.51"

#------------------------------------------------

cl_background               = '#101010'                     # background color in diagrams

#   change events for updating mainly plots

WING_NEW                    = "<<WING_NEW>>"                #tk event types
WING_CHANGED                = "<<WING_CHANGED>>"             
CHORD_CHANGED               = "<<CHORD_CHANGED>>"             
PLANFORM_CHANGED            = "<<PLANFORM_CHANGED>>"
SECTION_CHANGED             = "<<SECTION_CHANGED>>"
CURRENT_SECTION_CHANGED     = "<<CURRENT_SECTION_CHANGED>>"
DIAGRAMM_SECTION_SELECTED   = "<<DIAGRAMM_SECTION_SELECTED>>"
AIRFOIL_CHANGED             = "<<AIRFOIL_CHANGED>>"
PANELS_CHANGED              = "<<PANELS_CHANGED>>"

ctk_root : ctk.CTk = None                                   # root event handler

def fireEvent(eventType): 
    # print ("- fire event from root", eventType)
    if ctk_root: ctk_root.event_generate (eventType) 


#------------------------------------------------


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
        self.editPlanformType_frame  = Edit_Wing_PlanformType  (self, wingFn, width=500)
        self.editSectionMaster_frame = Edit_WingSection_Master (self, wingFn)

        # edit maingrid 1 x 4  with subframes

        self.grid_rowconfigure      (0, weight=1)
        self.grid_columnconfigure   (0, weight=0)
        self.grid_columnconfigure   (1, weight=0, minsize=400)
        self.grid_columnconfigure   (2, weight=0, minsize=400)
        self.grid_columnconfigure   (3, weight=1, minsize=400)

        self.editWing_frame.grid          (row=0, column=1, pady=5, padx=5, sticky="news")
        self.editPlanformType_frame.grid  (row=0, column=2, pady=5, padx=5, sticky="news")
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

    def changed_planform (self, event): 
        """ Eventhandler for changes of planform data"""

        # doesn't change wing data
        # doesn't change wing sections - only changed_chord does 
    
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
        myApp : App = self.winfo_toplevel()
        self.editSectionMaster_frame.set_curSection (myApp.curWingSectionName(), inEvent=True)


#-------------------------------------------


class Edit_Abstract (ctk.CTkFrame):
    """ 
    Abstract superclass for all the edit like frames
    """

    def __init__(self, master, wingFn, *args, **kwargs):
        super().__init__(master, *args, **kwargs)

        self._wingFn = wingFn               # the function to the wing (indirect because of new wing could be set)
        self.widgets = []
        self.init()
    
    def wing(self) -> Wing:
        # it's a method - not a property to exchange the wing 
        return self._wingFn()
    
    def planform(self) -> Planform:
        return self.wing().planform
    
    @property
    def myApp (self) -> 'App':
        """ the App self belongs to"""
        return self.winfo_toplevel()
    
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

    def refresh(self):
        # refresh typically based on changed events 
        for widget in self.widgets:
            if isinstance(widget, Base_Widget): widget.refresh()
        # print ("  - refresh in ", self.__class__.__name__," for %s widgets" % len(self.widgets))

        
#-------------------------------------------

class Edit_Wing(Edit_Abstract):
    """ 
    Frame to edit main data of the wing like wingspan. This is just the header
    """
    name = "Wing"

    def init (self):

        self.grid_rowconfigure      (1, weight=1)
        self.grid_columnconfigure   (4, weight=1)
               
        self.add (Header_Widget (self,0,0, lab=self.name, width=80))
        self.add (Field_Widget  (self,0,1, lab=None, obj=self.wing, get='name', set='set_name',
                                 lab_width=1, event=WING_CHANGED, width=140))


        self.dataFrame = Edit_Wing_Data (self, self._wingFn, fg_color='transparent')
        self.dataFrame.grid (row=1, column=0, columnspan=5, pady=0, padx=(10,5), sticky="news")

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

        unit = self.wing().unit
                
        self.add (Field_Widget  (self,2,0, lab="Wing span",     obj=self.wing, get='wingspan', set='set_wingspan',
                                 event=WING_CHANGED, lim=(100,20000), dec=1, spin= True, step=10, unit=unit))
        self.add (Field_Widget  (self,2,3, lab="Hinge angle",   obj=self.wing, get='hingeAngle', set='set_hingeAngle',
                                 event=WING_CHANGED, lim=(-5,45), dec=1, spin=True, step=0.1, unit="°"))
        self.add (Field_Widget  (self,3,0, lab="Chord at root", obj=self.wing, get='rootchord', set='set_rootchord',
                                 event=WING_CHANGED, lim=(10,500), dec=1, spin=True, step=1, unit=unit))
        self.add (Field_Widget  (self,3,3, lab="Chord at tip",  obj=self.wing, get='tipchord', set='set_tipchord',
                                 event=CHORD_CHANGED,               
                                 lim=(1,500), dec=1, spin=True, step=1, unit=unit))

        self.add (Field_Widget  (self,4,0, lab="Flap at root",  obj=self.wing, get='flapDepthRoot', set='set_flapDepthRoot',
                                 event=PLANFORM_CHANGED, lim=(0,50), dec=1, spin=True, step=0.5, unit='%'))
        self.add (Field_Widget  (self,4,3, lab="Flap at tip",   obj=self.wing, get='flapDepthTip', set='set_flapDepthTip',
                                 event=PLANFORM_CHANGED,            
                                 lim=(0,50), dec=1, spin=True, step=0.5, unit='%'))
        
        Blank_Widget (self,5,0, width=20, height = 15) 

        self.add (Field_Widget  (self,6,0, lab="Re at root",    obj=self.wing, get='rootRe', set='set_rootRe',
                                 event=WING_CHANGED, lim=(0,10000000), dec=0, spin=True, step=1000))

        # a mini frame to bring the two nick fields together
        self.nick_frame = ctk.CTkFrame(self, fg_color="transparent")
        self.nick_frame.grid    (row=6, column=3, columnspan=3, sticky="nwes")

        self.add (Field_Widget  (self.nick_frame,0,0, lab="Airfoils nick", obj=self.wing, get='airfoilNickPrefix', set='set_airfoilNickPrefix',
                                 event=SECTION_CHANGED, width= 60))
        self.add (Field_Widget  (self.nick_frame,0,3,  obj=self.wing().rootSection, get='airfoilNickPostfix', 
                                 disable=True, width= 40))
        


#-------------------------------------------

class Edit_Wing_PlanformType(Edit_Abstract):
    """ 
    Master/Header frame for planform type specific sub frames
    """
    name = "Planform"

    def init (self):

        self.currentPlanform_frame = None

        # main grid:  header - sub Frame for specific planform 
        self.grid_columnconfigure   (2, weight=1)
        self.grid_rowconfigure      (3, weight=1)

        self.add(Header_Widget (self,0,0, lab=self.name, width=110))
        self.add(Option_Widget (self,0,1, get='planformType', set = 'set_planformType',
                                width = 150, options=Planform.allTemplatePlanformTypes(),
                                event=PLANFORM_CHANGED))

        self.add(Label_Widget  (self,1, 0, lab= self.shortDescription))
        self.add(Blank_Widget  (self,2, 0, height = 10))

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

        dxf_dialog = Dialog_Load_DXF (self, wingFn = self.wing, dxf_Path=None, ref=False) 

        self.wait_window (dxf_dialog)
   
        dxf_Path = dxf_dialog.dxf_pathFilename
        if dxf_Path:  
            self.wing().planform =  Planform_DXF( self.wing(), dxf_Path= dxf_Path, ref = False)
            handled = True
            fireEvent (WING_CHANGED)                   # update hinge, flaps
        else: 
            handled = False

        return handled
    
    def refresh(self):
        super().refresh()
        # new wing - also new planformType? 
        self.set_planform_frame (self.planformType)
        self.currentPlanform_frame.refresh()



#-------------------------------------------


class Edit_Planform_Elliptical(Edit_Abstract):
    """ 
    Frame to edit the parameters of a elliptical planform
    """
    name = Planform_Elliptical.planformType

    def planform(self) -> Planform_Elliptical:
        return self.wing().planform

    def init(self):

        self.grid_columnconfigure   (0, weight=0)
        self.grid_rowconfigure      (6, weight=1)

        self.add (Field_Widget  (self,0,0, lab="Tip belly",       obj=self.planform, get='ellipseTipBelly', set='set_ellipseTipBelly',
                                    event=CHORD_CHANGED, lim=(0,1), dec=2, spin=True, step=0.05))
        self.add (Field_Widget  (self,0,3, lab="Tip belly width", obj=self.planform, get='ellipseBellyWidth', set='set_ellipseBellyWidth',
                                    event=CHORD_CHANGED, lim=(0,1), dec=2, spin=True, step=0.05))
        self.add (Field_Widget  (self,1,0, lab="Ellipse shift",   obj=self.planform,  get='ellipseShift', set='set_ellipseShift',
                                    event=CHORD_CHANGED, lim=(0,0.5), dec=2, spin=True, step=0.05))
        self.add (Field_Widget  (self,2,0, lab="Ellipse correction",obj=self.planform, get='ellipseCorrection', set='set_ellipseCorrection',
                                    event=CHORD_CHANGED, lim=(-1,1), dec=2, spin=True, step=0.05))
        self.add (Field_Widget  (self,3,0, lab="LE correction",   obj=self.planform, get='leCorrection', set='set_leCorrection',
                                    event=PLANFORM_CHANGED, lim=(-1,1), dec=2, spin=True, step=0.05))
        


class Edit_Planform_Trapezoid (Edit_Abstract):
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
        fireEvent (PLANFORM_CHANGED)


#-------------------------------------------


class Edit_Planform_Elliptical_StraightTE (Edit_Abstract):
    """ 
    Frame to edit the parameters of a elliptical planform
    """
    name = Planform_Elliptical_StraightTE.planformType

    def planform(self) -> Planform_Elliptical_StraightTE:
        return self.wing().planform

    def init(self):

        self.add (Field_Widget  (self,0,0, lab="Tip belly",       obj=self.planform, get='ellipseTipBelly', set='set_ellipseTipBelly',
                                 lim=(0,1),   dec=2, spin=True, step=0.05, event=CHORD_CHANGED))
        self.add (Field_Widget  (self,0,3, lab="Tip belly width", obj=self.planform, get='ellipseBellyWidth', set='set_ellipseBellyWidth',
                                 lim=(0,1),   dec=2, spin=True, step=0.05, event=CHORD_CHANGED))
        self.add (Field_Widget  (self,1,0, lab="Ellipse correct.",obj=self.planform, get='ellipseCorrection', set='set_ellipseCorrection',
                                 lim=(-1,1),  dec=2, spin=True, step=0.05, event=CHORD_CHANGED))


#-------------------------------------------

class Edit_Planform_DXF (Edit_Abstract):
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


        Blank_Widget (self,3,0,  height= 5)
        self.add(Label_Widget  (self,4,0, lab=lambda: self.planform().infoText ))
        Blank_Widget (self,5,0,  height= 10)

    def dxf_filename(self): 
        return self.planform().dxf_filename()

    def open_dxf_file (self):

        current_dxf_path = self.planform().dxf_pathFilename()

        dxf_dialog = Dialog_Load_DXF (self, wingFn = self.wing, dxf_Path = current_dxf_path) 

        self.wait_window (dxf_dialog)

        if dxf_dialog.return_OK:
            new_dxf_Path = dxf_dialog.dxf_pathFilename        
            if new_dxf_Path:                                    # dialog returned a valid path 
                self.wing().planform.set_dxfPathFilename (new_dxf_Path) 
                self.wing().planform.assignToWing()             # take over hinge, flap

                self.refresh()
                fireEvent (WING_CHANGED)



class Edit_WingSection_Master(Edit_Abstract):
    """ 
    Master frame for wingSection - to select, add, delete a wing section
    Is parent for a single wing section frame
    """
    name = "Section"

    def init(self):

        # set first section as initial 
        self.curSection : WingSection = self.wing().wingSections[1]
        self.curSectionFrame = None

        # main grid:  header frame + - sub Frame for one section  
        hfrm = ctk.CTkFrame(self, fg_color='transparent')
        self.grid_columnconfigure   (1, weight=1)
        self.grid_rowconfigure      (1, weight=1)

        self.add(Header_Widget (hfrm,0,0, lab=self.name, width=100))

        self.add(Option_Widget (hfrm,0,1,   get=self.curSectionName, set=self.set_curSection,
                                            spin=True, width=100, options=self.sectionNames))
        self.add(Button_Widget (hfrm,0,2,   lab=' Add ',  width=50,  set=self.addSection,
                                            disable=self.addDisabled))
        self.add(Button_Widget (hfrm,0,3,   lab='Delete',   width=50, set=self.deleteSection,
                                            disable=self.deleteDisabled))

        hfrm.grid (row=0, column=0, pady=0, padx=0, sticky="w")

        # init and set section data frame to current section 
        self.set_curSection (self.curSection.name())


    def curSectionName(self): return self.curSection.name()
    def sectionNames(self): 
        return [s.name() for s in self.wing().wingSections]   
    
    def deleteSection (self):
        """ delete the current, selected wing Section""" 
        if not self.deleteDisabled():
            # remind the neighbour before this section 
            leftSec, rightSec = self.wing().getNeighbourSectionsOf (self.curSection)
            self.wing().deleteSection (self.curSection)
            fireEvent (SECTION_CHANGED)         # update diagram
            # delete done - set option list to old neighbour 
            self.set_curSection(leftSec.name())

    def deleteDisabled (self):
        return self.curSection.isRoot or self.curSection.isTip
    

    def addSection (self):
        """ add a section after current Section""" 
        if not self.addDisabled():
            newSection = self.wing().createSectionAfter (self.curSection)
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
        self.myApp.set_curWingSectionName(aName)
        if not inEvent:                         # avoid event ping pong
            fireEvent (CURRENT_SECTION_CHANGED)    


    def get_SectionFromName (self, aName) -> WingSection:
        """ return the section with "aName"
        """
        for sec in self.wing().wingSections:
            if (sec.name() == aName): return sec
        raise ValueError ("Wing section not found: ", aName )


    def refresh_current (self):
        # refresh data of current section
        self.curSectionFrame.refresh()

    def reset (self):
        """reset everything - needed when a new wing is loaded """
        self.set_curSection (self.wing().wingSections[0].name())


class Edit_WingSection(Edit_Abstract):
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
                                                lim=(0.0,1.0), dec=2, spin=True, step=0.01, unit='%',
                                                disable='isRootOrTip', event=SECTION_CHANGED))

        self.add(Field_Widget  (self,2,0, lab="Chord", obj=self.wingSection, get='chord', set='set_chord',
                                                lim='limits_Chord', dec=1, spin=True, step=0.5, unit=unit,
                                                disable='isRootOrTip', event=SECTION_CHANGED))
        self.add(Field_Widget  (self,2,3, lab="Chord rel.", obj=self.wingSection, get='norm_chord', set='set_norm_chord',
                                                lim='limits_normChord', dec=2, spin=True, step=0.01, unit='%',
                                                disable='isRootOrTip', event=SECTION_CHANGED))

        self.add(Field_Widget  (self,3,0, lab="Reynolds", obj=self.wingSection ,get='Re', set='set_Re',
                                                lim='limits_Re', dec=0, spin=True, step=1000,
                                                disable='isReDisabled', event=SECTION_CHANGED))
        self.add(Field_Widget  (self,3,3, lab="Airfoil nick", obj=self.wingSection ,get='airfoilNick', 
                                                disable=True, width=85))


        Blank_Widget (self,4,0, width=20, height = 10) 
        self.add(Field_Widget  (self,5,0, lab="Airfoil", obj=self.wingSection, get='airfoilName', set='',
                                                disable=True, event=SECTION_CHANGED))
        
        self.add(Button_Widget (self,5,2, lab='Select', width=60, columnspan=2, sticky='w', set=self.select_airfoil ))
        self.add(Button_Widget (self,5,2, lab='Remove', width=60, columnspan=2, sticky='e', set=self.remove_airfoil, 
                                disable=self.remove_airfoil_disable ))

        Blank_Widget (self,6,0, width=20, height = 10) 
        self.add(Field_Widget  (self,7,0, lab="Flap group", obj=self.wingSection ,get='flapGroup', set='set_flapGroup',
                                                lim=(0,9), dec=0, spin=True, step=1,
                                                disable='isTip', event=SECTION_CHANGED))

        self.grid_columnconfigure   (5, weight=1)

    
    def select_airfoil(self):
        """ select airfoil with explorer and load it if possible """

        filetypes  = [('dat files', '*.dat')]
        initialDir = self.workingDir if self.workingDir is not None else os.getcwd()

        newPathFilename = filedialog.askopenfilename(
                    title='Select airfoil file',
                    initialdir=initialDir,
                    filetypes=filetypes)
        if newPathFilename: 
            newRelPathFilename = os.path.relpath(newPathFilename, start = self.workingDir)
            self.wingSection().set_airfoilWithPathFileName(newRelPathFilename, workingDir=self.workingDir)
            self.refresh()
            fireEvent (AIRFOIL_CHANGED)

    def remove_airfoil(self):

        if self.wingSection().airfoil_canBeRemoved():
            self.wingSection().set_airfoilWithPathFileName(None)
            self.refresh()
            fireEvent (AIRFOIL_CHANGED)
        
    def remove_airfoil_disable (self):
        return not self.wingSection().airfoil_canBeRemoved()


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
        # handle the view for the switches 
        self._view_dict: dict[str, ctk.CTkFrame] = {}
         

        self.myPlot_frames = []
        # Get all subclasses of Plot_Frame and create a tab for each 
        for plot_cls in Diagram_Abstract.__subclasses__():
            if plot_cls != Diagram_Planform_Mini: 
                tab_frame = self.add("   " + plot_cls.name + "   ")
                tab_frame.grid_columnconfigure(0, weight=1)
                tab_frame.grid_rowconfigure(1, weight=1)

                # switches
                view_frame=  ctk.CTkFrame(tab_frame, fg_color="transparent")
                view_frame.grid (row=0, column=0, padx=(5, 5), pady=5, sticky='we')

                # plot area
                plot_frame=  ctk.CTkFrame(tab_frame, fg_color="transparent")
                plot_frame.grid (row=1, column=0, padx=(0,0), pady=0, sticky='wens')
                plot_frame.grid_columnconfigure(0, weight=1)
                plot_frame.grid_rowconfigure(0, weight=1)
                
                # create the diagram in the plot frame
                self.myPlot_frames.append (plot_cls (plot_frame,  self._wingFn,
                                        view_frame=view_frame))

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
    defaultTip = "Use switches ...."

    def __init__(self, master, wingFn, *args, view_frame = None, size= None,  **kwargs):
        super().__init__( master, *args, **kwargs)

        self.view_frame : Frame = view_frame
        self._wingFn = wingFn

        self.configure(fg_color= cl_background)

        # big diagram grid 
        self.grid_columnconfigure(0, weight=1)
        self.grid_rowconfigure(0, weight=1)
        self.grid(row=0, column=0, sticky="news")

        if size: 
            self.figure : plt.Figure = plt.Figure(figsize=size)
        else: 
            self.figure : plt.Figure = plt.Figure()

        # connect tk and pyplot
        self.canvas = FigureCanvasTkAgg(self.figure, self)
        self.canvas._tkcanvas.grid_columnconfigure(0, weight=1)
        self.canvas._tkcanvas.grid_rowconfigure   (0, weight=1)
        self.canvas._tkcanvas.grid (row=0, column=0, pady=0, padx=0, sticky="news")
        self.canvas._tkcanvas.configure(background= cl_background)

        # common axes for this diagram
        self.create_axes()
        self.setup_axes ()

        # Create the artists for the diagramm
        self.setup_artists ()

        # init of switches / plots if a frame for the switches is available 
        if self.view_frame: 
            col = 0 
            self.view_frame.grid_columnconfigure(0, weight=1)   # to center switches
            col += 1
            self.setup_Switches (row=0, col=1)                       

        # react on changes of model
        self.setChangeBindings ()
        self._active            = False                     # is active frame? control change events 


    @property
    def wing(self) -> Wing:
        return self._wingFn()
    
    # ----- abstract - to overlaod

    def create_axes (self):
        """ setup axes, axis for this plot type """
        self.axes : plt.Axes = self.figure.add_subplot()        # the pyplot axes this diagram is plotted
        self.figure.subplots_adjust(left=0.04, bottom=0.07, right=0.98, top=0.99, wspace=None, hspace=None)


    def setup_axes(self):
        """ to overload - setup axes, axis for this plot type """
        pass


    def setup_artists(self):
        """ setup artists for this plot type """
        self.gridArtist = Grid_Artist (self.axes, self._wingFn, show=True)
        self.gridArtist.plot()          # force to show first time


    def setup_Switches(self, row=0, col=0):
        """ define on/off switches ffor this plot type"""
        # grid on / off is always available 
        # row += 1 
        # Blank_Widget  (self.view_frame,row, 0, height=5)
        # self.view_frame.grid_rowconfigure (row, weight=1)

        col += 1 
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Grid', 
                       get=lambda: self.gridArtist.show, set=self.gridArtist.set_show)

        # extra col to center all the switches
        col += 1 
        self.view_frame.grid_columnconfigure(col, weight=1)
     

    def refresh(self, dummy): 
        # overwrite in sub class
        if self._active:
            pass  

    # ----- general refresh when getting active view again

    def setActive(self, active: bool):
        # the Diagramm master (Tabview) will activate/deactivate to avoid plot generation
        #   if self is not visible
        if active: 
            self._active = True
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

        limy = self.axes.get_ylim()
        if limy[0] < limy[1]: 
            self.axes.invert_yaxis() 
        self.axes.axis('equal')
        self.axes.relim()
        self.axes.autoscale(enable=True, axis='Both')


    def setup_artists (self):
        """ setup artists for this plot type """

        super().setup_artists()
        self.planformArtist     = Planform_Artist (self.axes, self._wingFn, show=True)
        self.chordLinesArtist   = ChordLines_Artist (self.axes, self._wingFn, show=False)
        self.curSectionArtist   = CurrentSection_Artist (self.axes, self._wingFn, show=True)
        self.sectionsArtist     = Sections_Artist (self.axes, self._wingFn, show=True,onPick=self.sectionPicked)     
        self.airfoilNameArtist  = AirfoilName_Artist (self.axes, self._wingFn, show=False)     
        self.flapArtist         = Flap_Artist (self.axes, self._wingFn)
        self.referenceArtist    = RefPlanform_Artist (self.axes, self._wingFn)
        self.dxfArtist          = RefPlanform_DXF_Artist (self.axes, self._wingFn)


    def setup_Switches (self, row = 0, col = 0 ):
        """ define on/off switches for this plot types"""

        # the whole plot work will be done by the artists
        # plot the real planform on top     
        # Switch_Widget (self.view_frame,row,0, padx=10, lab='Planform', 
        #         get=lambda: self.planformArtist.show, set=self.planformArtist.set_show)
        row = 0 
        col += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Chord lines', 
                get=lambda: self.chordLinesArtist.show, set=self.chordLinesArtist.set_show)
        col += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Wing sections', 
                get=lambda: self.sectionsArtist.show, set=self.sectionsArtist.set_show)

        col += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Current section', 
                get=lambda: self.curSectionArtist.show, set=self.curSectionArtist.set_show)
        col += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Airfoil names', 
                get=lambda: self.airfoilNameArtist.show, set=self.airfoilNameArtist.set_show)
        col += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Flaps', 
                get=lambda: self.flapArtist.show, set=self.flapArtist.set_show)
        col += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Reference elliptical', 
                get=lambda: self.referenceArtist.show, set=self.referenceArtist.set_show)

        if (self.wing.refPlanform_DXF): 
            col += 1
            Switch_Widget (self.view_frame,row,col, padx=10, lab='Reference DXF', 
                    get=lambda: self.dxfArtist.show, set=self.dxfArtist.set_show)
        
        super().setup_Switches(row,col)


    # -------- event handler

    def setChangeBindings (self):
        # overloaded
        ctk_root.bind(WING_NEW,                 self.wing_new, add='+')
        ctk_root.bind(WING_CHANGED,             self.changed_wing, add='+')
        ctk_root.bind(CHORD_CHANGED,            self.changed_chord, add='+')
        ctk_root.bind(PLANFORM_CHANGED,         self.changed_planform, add='+')
        ctk_root.bind(SECTION_CHANGED,          self.changed_sections, add='+')
        ctk_root.bind(AIRFOIL_CHANGED,          self.changed_airfoil, add='+')
        ctk_root.bind(CURRENT_SECTION_CHANGED,  self.changed_currentSection, add='+')

    def wing_new (self, dummy): 
        """ Eventhandler for new wing """
        self.axes.clear()
        self.changed_wing (dummy)
        self.changed_currentSection (dummy)

    def changed_wing (self, dummy): 
        """ Eventhandler for changes of the complete wing - like wing span """
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

    def changed_airfoil (self, dummy): 
        """ Eventhandler for changes of planform"""
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
            self.sectionsArtist.refresh ()  
            self.curSectionArtist.refresh()
            self.airfoilNameArtist.refresh ()  
            self.referenceArtist.refresh() 
            self.dxfArtist.refresh      () 
            self.flapArtist.refresh     () 

            self.setup_axes()                       # maybe the axis limits changed

            self.axes.figure.canvas.draw_idle()     # draw ony if Windows is idle!

            # print ("  - refresh in ", self.__class__.__name__," for active artists")

    def refresh_sections(self): 
        if self._active:
            self.sectionsArtist.refresh ()  
            self.airfoilNameArtist.refresh ()  
            self.flapArtist.refresh     () 
            self.curSectionArtist.refresh()

            self.axes.figure.canvas.draw_idle()    # draw ony if Windows is idle!
            # print ("  - refresh sections in ", self.__class__.__name__," for active artists")

    # -------- pick object in axes handling  

    def sectionPicked (self, aSectionLabel):
        # callback method - the user pciked a wing section in the plot
        myApp : App = self.winfo_toplevel()
        myApp.set_curWingSectionName(aSectionLabel)
        fireEvent (DIAGRAMM_SECTION_SELECTED)
        self.curSectionArtist.set_current (aSectionLabel, figureUpdate=True)  



class Diagram_Planform_Mini (Diagram_Abstract):
    """ 
    Frame to plot planform in a simple, mini Version for previews etc. 
    """
    name = "Planform"

    def create_axes (self):
        """ setup axes, axis for this plot type """
        self.axes : plt.Axes = self.figure.add_subplot(frameon=False)        # the pyplot axes this diagram is plotted
        self.figure.subplots_adjust(left=0.01, bottom=0.0, right=0.99, top=1, wspace=None, hspace=None)

    def setup_axes (self):
        """ setup axes, axis for this plot type """
        self.axes.set_ylim([self.wing.rootchord, 0.0])
        self.axes.set_xlim([-0.05 * self.wing.halfwingspan, self.wing.halfwingspan * 1.05])
        self.axes.axis('equal')
        self.axes.set_xticks([], [])
        self.axes.set_yticks([], [])


    def setup_artists (self):
        """ artists are not set automatically - set from outside """
        pass

    def setup_Switches (self, row = 0, col=0):
        """ no switches"""
        pass

    def setChangeBindings (self):
        # overloaded
        pass

    def changed_planform (self, dummy): 
        """ Eventhandler for changes of planform"""
        pass

    def setActive(self, active: bool):
        # overloaded to set active section 
        pass

    def refresh(self): 
        self.axes.figure.canvas.draw_idle()    # draw ony if Windows is idle!


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
        self.axes.set_ylim([ 0.0, 1.1])
        self.axes.set_xlim([ 0.0, 1.1])
        self.axes.text(.95,.9, self.wing.name, fontsize ='x-large', ha='right', transform=self.axes.transAxes)

    def setup_artists(self):
        """ setup axes, axis, artists for this plot type """

        super().setup_artists()
        self.chordArtist        = Chord_Artist (self.axes, self._wingFn, show=True)
        self.chordLinesArtist   = ChordLines_Artist (self.axes, self._wingFn, norm=True, show=False)
        self.curSectionArtist   = CurrentSection_Artist (self.axes, self._wingFn, norm=True, show=True)
        self.sectionsArtist     = Sections_Artist (self.axes, self._wingFn, show=True, norm=True,
                                                       onPick=self.sectionPicked)
        self.referenceArtist    = RefChord_Artist (self.axes, self._wingFn, norm=True)
        self.dxfArtist          = RefChord_DXF_Artist (self.axes, self._wingFn, norm=True)


    def setup_Switches(self, row=0, col=0):
        """ define on/off switches ffor this plot type"""

        # Switch_Widget  (self.view_frame,row,0, lab='Chord distribution', 
        #                 get=lambda: self.chordArtist.show, set=self.chordArtist.set_show)
        row = 0 
        col += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Chord lines', 
                get=lambda: self.chordLinesArtist.show, set=self.chordLinesArtist.set_show)
        col += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Wing sections', 
                        get=lambda: self.sectionsArtist.show, set=self.sectionsArtist.set_show)
        col += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Current section', 
                get=lambda: self.curSectionArtist.show, set=self.curSectionArtist.set_show)
        col += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Reference elliptical', 
                        get=lambda: self.referenceArtist.show, set=self.referenceArtist.set_show)
        if (self.wing.refPlanform_DXF.isValid): 
            col += 1
            Switch_Widget  (self.view_frame,row,col, padx=10, lab='Reference DXF', 
                            get=lambda: self.dxfArtist.show, set=self.dxfArtist.set_show)

        super().setup_Switches(row, col)

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
            # print ("  - refresh in ", self.__class__.__name__," for active artists")

    def refresh_sections(self): 
        if self._active:
            self.sectionsArtist.refresh ()  
            self.curSectionArtist.refresh()
            self.axes.figure.canvas.draw_idle()    # draw ony if Windows is idle!
            # print ("  - refresh sections in ", self.__class__.__name__," for active artists")
  
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
        self.axes.axis('equal')


    def setup_artists(self):
        """ setup axes, axis, artists for this plot type """

        super().setup_artists()
        self.airfoilArtist      = Airfoil_Artist (self.axes, self._wingFn, show=True, norm=True, 
                                                  strak=False, onPick=self.airfoilPicked)


    def setup_Switches(self, row=0, col=0):
        """ define on/off switches for this plot type"""
        row = 0 
        col += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='In real size', 
                       get=lambda: self.airfoilArtist.abs, set=self.airfoilArtist.set_abs)
        col += 1
        Switch_Widget (self.view_frame,row,col, padx=10, lab='Straked airfoils', 
                       get=self.show_strakedAirfoils, set=self.set_show_strakedAirfoils)

        super().setup_Switches(row, col)

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

    # -------- refresh my Artists which are on 'show mode' 

    def setActive(self, active: bool):
        # overloaded to set active section 
        if active: 
            myApp : App = self.winfo_toplevel()
            self.airfoilArtist.set_current (myApp.curWingSectionName(), figureUpdate=True)  
        super().setActive(active)

    def refresh(self): 
        # overloaded
        if self._active:
            self.airfoilArtist.refresh ()  
            self.axes.figure.canvas.draw_idle()    # draw ony if Windows is idle!

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

    self.return_OK is True if user conformed 'ok' 
    """
    width  = 500
    height = 400
    titleText  = "My little title"

    def __init__(self, master, *args, **kwargs):
        super().__init__(master, *args, **kwargs)

        # the attribute for return ok
        self.return_OK = False

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
        self.edit_frame.grid    (row=0, column=0, pady=10, padx=15, sticky="nesw")

        self.widgets = []                                   # for refresh logic  


    def ok (self):
        # to over load and do ok actions
        self.return_OK = True
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


#-------------------------------------------


class Dialog_Load_DXF (Dialog_Abstract):
    """ 
    Import dxf file to be overlay or template planform 

    Returns in self.dxf_pathFilename if the user selected a valid file 
    """
    width  = 700
    height = 350
    titleText  = "Import dxf file"

    def __init__(self, master, *args, wingFn = None, dxf_Path= None, ref:bool = False, **kwargs):
        super().__init__(master, *args, **kwargs)

        frame = self.edit_frame
        self.dxf_pathFilename = None                    # the return value 
        self.tmpPlanform = Planform_DXF( wingFn(), dxf_Path= dxf_Path, ref = ref)
        self.tmpPlanform.adaptHingeAngle = False        # show the original hinge angle 

        if self.tmpPlanform.dxf_isReference:
            shortDescription = "The wing contour in the dxf file will be used as " +\
                               "a reference overlay for the active planform." 
            headerText = "DXF contour as reference"
        else:
            shortDescription = "The chord distribution of the dxf wing will be the base of the current planform." 
            headerText = "DXF chord distribution for planform"

        frame.grid_columnconfigure (5, weight=1)

        r = 0 
        Header_Widget (frame,r,0, lab=headerText, width=110, columnspan=3)
        r +=1  
        Label_Widget  (frame,r,0, lab= lambda: shortDescription)
        r +=1  
        Blank_Widget  (frame,r,0, height = 10)

        r +=1  
        self.add(Field_Widget  (frame,r,0, lab='  DXF file', columnspan=2, width= 220, get= self.tmpPlanform.dxf_filename))
        self.add(Button_Widget (frame,r,3, lab='Select', width=90, set=self.open_dxf_file ))
        self.add(Button_Widget (frame,r,4, lab='Remove', width=70, set=self.remove_dxf_file, 
                                disable=self.remove_dxf_disable ))

        r +=1  
        Blank_Widget (frame,r,0,  height= 15)
        r +=1  
        self.add(Label_Widget (frame,r,0, lab=lambda: self.tmpPlanform.infoText, columnspan=2, sticky = "ew" ))

        # show a little dxf preview
        self.diagram_frame = Diagram_Planform_Mini (frame,  wingFn, size=(3.5,1.2))
        self.diagram_frame.grid(row=r, column=2, columnspan= 4)
        self.diagram_axes = self.diagram_frame.axes
        self.planformArtist = RefPlanform_DXF_Artist (self.diagram_frame.axes, wingFn(), show=True, 
                                                      showMarker=False, planform=self.tmpPlanform)
        self.planformArtist.refresh(figureUpdate=True)

        r +=1  
        Blank_Widget (frame,r,0, height= 10)
        frame.grid_rowconfigure (r, weight=1)

        r +=1  
        self.add(Button_Widget (frame,r,2, lab='Ok', primary=True, set=self.ok, width=100,
                                disable= self.ok_disable ))
        self.add(Button_Widget (frame,r,3, lab='Cancel',set=self.cancel, width= 100))

        r +=1  
        Blank_Widget  (frame,r     ,3, height= 10)

        self.diagram_frame.setActive(True)


    def open_dxf_file (self):

        filetypes  = [('dxf files', '*.dxf')]
        initialDir = self.tmpPlanform.wing.paramDir

        newPathFilename = filedialog.askopenfilename(
                    title='Open dxf file',
                    initialdir=initialDir,
                    filetypes=filetypes)

        if newPathFilename: 
            self.tmpPlanform.set_dxfPathFilename (newPathFilename)
            self.refresh()
            self.planformArtist.refresh(figureUpdate=True)

    def remove_dxf_file (self):
            self.tmpPlanform.set_dxfPathFilename ("")
            self.refresh()
            self.planformArtist.refresh(figureUpdate=True)

    def remove_dxf_disable (self):
            disable =  not self.tmpPlanform.dxf_pathFilename ()
            return disable

    def ok (self):
        # to over load and do ok actions
        if self.tmpPlanform.isValid: 
            self.dxf_pathFilename = self.tmpPlanform.dxf_pathFilename ()  # the return value 
        super().ok()

    def ok_disable (self):
        " activate ok-Button"
        # in case dxf is reference, it's also possible to remove the dxf reference
        # else a valid dxf must be choosen 
        disable = not self.tmpPlanform.isValid and not self.tmpPlanform.dxf_isReference
        return disable

    def cancel(self): 
        super().cancel()


#-------------------------------------------


class Dialog_Export_Xflr5_Flz (Dialog_Abstract):
    """ 
    Export planform as paneled for Xflr5 oder FLZ 

    """
    width  = 1000
    height = 470
    titleText  = "Export to ..."

    def __init__(self, master, wingFn, Xflr5=False, Flz=False, workingDir=None, *args, **kwargs):
        super().__init__(master, *args, height=self.height/2, **kwargs)

        self.wing : Wing = wingFn()
        self.workingDir = workingDir

        if Xflr5:
            self.exporter = self.wing.xflr5Exporter
            self.mode = "Xflr5"
        elif Flz:
            self.exporter = self.wing.flzExporter
            self.mode = "FLZ_vortex"
        else:
            return  
        self.exporter.set_baseDir (self.workingDir)
        self.paneledPlanform = self.exporter.paneledPlanform

        # main grid 3 x 1  (preview + edit + buttons) 

        self.diagram_frame = Diagram_Planform_Mini (self.edit_frame, wingFn, size=(4,2.4))
        self.diagram_frame.grid(row=0, column=0, sticky="nwe")

        self.input_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.input_frame.grid(row=1, column=0, sticky="nwe")

        self.button_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.button_frame.grid(row=2, column=0, sticky="wes", pady=10)

        self.edit_frame.grid_columnconfigure (0, weight=1)
        self.edit_frame.grid_rowconfigure    (0, weight=1)
        self.edit_frame.grid_rowconfigure    (2, weight=1)

        # artists for preview
        self.diagram_axes = self.diagram_frame.axes
        self.panelArtist    = PaneledPlanform_Artist (self.diagram_frame.axes, 
                                wingFn, self.paneledPlanform, show=True)
        self.panelArtist.refresh(figureUpdate=True)

        # header with hints 
        r = 0 
        c = 0 
        Header_Widget (self.input_frame,r,c, lab= self.mode+" export", width=110, pady=(7,20), columnspan=2)
        hint = self.check_y_deviation() 
        if hint: 
            self.add (Label_Widget  (self.input_frame,r,c+2, lab=hint, sticky="w", 
                                     columnspan=8, text_color='hint'))

        # entry fields 
        r +=1  
        self.add (Field_Widget  (self.input_frame,r,c, lab="  x-panels", width=80,
                                 obj=self.paneledPlanform, get='x_panels', set='set_x_panels',
                                 event=PANELS_CHANGED, lim=(1,50), dec=0, spin=True, step=1))
        self.add (Combo_Widget  (self.input_frame,r,c+3, lab="x-distribution", width=90,
                                 obj=self.paneledPlanform, get='x_dist', set='set_x_dist',
                                 options=self.paneledPlanform.distribution_fns_names(),
                                 event=PANELS_CHANGED ))

        r +=1  
        self.add (Field_Widget  (self.input_frame,r,c, lab="  y-panels", width=80,
                                 obj=self.paneledPlanform, get='y_panels', set='set_y_panels',
                                 event=PANELS_CHANGED, lim=(1,50), dec=0, spin=True, step=1))
        self.add (Combo_Widget  (self.input_frame,r,c+3, lab="y-distribution",  width=90,
                                 obj=self.paneledPlanform, get='y_dist', set='set_y_dist',
                                 options=self.paneledPlanform.distribution_fns_names(),
                                 event=PANELS_CHANGED ))
        r +=1  
        self.add (Field_Widget  (self.input_frame,r,c, lab="  y min width", width=80,
                                 obj=self.paneledPlanform, get='y_minWidth', set='set_y_minWidth',
                                 event=PANELS_CHANGED, lim=(1,20), dec=0, spin=True, step=1, unit="mm"))

        r = 1 
        c = 7 
        self.input_frame.grid_columnconfigure (6, weight=1)
        self.input_frame.grid_columnconfigure (10, weight=2)

        self.add(Field_Widget  (self.input_frame,r,c, lab=self.mode+ " directory", obj=self.exporter, get='baseAndExportDir', set='',
                                width=180, disable=True))
        self.add(Button_Widget (self.input_frame,r,c+2, lab='Select', width=60, sticky='w', set=self.select_dir ))
        self.add(Switch_Widget (self.input_frame,r+1,c, lab='Use airfoil nick names for airfoils', 
                                columnspan=2, padx=0, 
                                obj=self.exporter, get='useNick', set='set_useNick'))
 

        r = 0 
        c = 1 
        self.add(Button_Widget (self.button_frame,r,c, lab='Export', set=self.ok, primary=True, width=100))
        if Flz: 
            c += 1 
            self.add(Button_Widget (self.button_frame,r,c, lab='Launch FLZ', set=self.launch_Flz, width=100,
                                    disable=self.launch_Flz_disabled))
        c += 1 
        self.add(Button_Widget (self.button_frame,r,c, lab='Cancel', set=self.cancel, width=100))
        self.button_frame.grid_columnconfigure (0, weight=1)
        self.button_frame.grid_columnconfigure (4, weight=1)

        # changed bindings
        ctk_root.bind(PANELS_CHANGED, self.refresh, add='+')


    def select_dir(self):
        " open dialog for directory selection"

        initialDir = os.path.normpath (os.path.join (self.workingDir, self.exporter.exportDir))
        newDir = filedialog.askdirectory(
                    title='Select directory for export',
                    initialdir=initialDir)
        if newDir:
            # store only relativ path 
            self.exporter.set_exportDir (os.path.relpath(newDir, start = self.workingDir))
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

    def refresh(self, dummy):
        self.panelArtist.refresh(figureUpdate=True)
        super().refresh()

    def cancel(self): 
        # changed bindings
        ctk_root.unbind(PANELS_CHANGED)
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
        ctk_root.unbind(PANELS_CHANGED)
        super().ok()


class Dialog_Export_Dxf (Dialog_Abstract):
    """ 
    Export wing / planform as dxf to file  

    """
    width  = 640
    height = 300
    titleText  = "Export DXF"

    def __init__(self, master, *args, wingFn = None,  **kwargs):
        super().__init__(master, *args, height=self.height/2, **kwargs)

        self.wing : Wing = wingFn()

        # main grid 3 x 1  (header + edit + buttons) 
        self.header_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.header_frame.grid(row=0, column=0, sticky="we")

        self.input_frame = ctk.CTkFrame(self.edit_frame, fg_color="transparent")
        self.input_frame.grid(row=1, column=0, sticky="nwes", padx=10, pady=40)

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
                                     columnspan=8, text_color='hint'))
        self.header_frame.grid_columnconfigure (c+2, weight=1)

        # entry fields 
        r = 0 
        c = 0 
        self.add(Field_Widget  (self.input_frame,r,c, lab="DXF file ", width=180,
                                lab_width=100, 
                                obj=self.wing, get='dxfPathFileName', disable=True))
        self.add(Button_Widget (self.input_frame,r,c+3, lab='Select', width=60, sticky='w', 
                                set=self.select_file ))

        r += 1 
        Blank_Widget           (self.input_frame,r,c, width=50, height = 15) 

        r += 1 
        self.add(Switch_Widget (self.input_frame,r,c+1, lab='Include airfoils in DXF', 
                                columnspan=2, padx=0, 
                                obj=self.wing, get='dxfAirfoilsToo', set='set_dxfAirfoilsToo'))      
       
        self.add (Field_Widget (self.input_frame,r,c+3, lab="TE thickness", width=90,
                                 obj=self.wing, get='dxfAirfoilsTeGap', set='set_dxfAirfoilsTeGap',
                                 lim=(0,2), dec=1, spin=True, step=0.1, unit='mm'))


        self.add(Button_Widget (self.button_frame,0,1, lab='Export', set=self.ok, primary=True, width=100))
        self.add(Button_Widget (self.button_frame,0,2, lab='Cancel', set=self.cancel, width=100))
        self.button_frame.grid_columnconfigure (0, weight=1)
        self.button_frame.grid_columnconfigure (3, weight=1)


    def select_file(self):
        " open dialog for file selection"

        newFile = filedialog.asksaveasfilename(
                    title='Select directory for DXF export',
                    filetypes  = [('dxf files', '*.dxf')],
                    initialdir=self.wing.paramDir,
                    initialfile=self.wing.dxfPathFileName, defaultextension=".dxf")
        if newFile:
            # store only relativ path 
            self.wing.set_dxfPathFileName (newFile)
            super().refresh()


    def hint_onlyPolylines (self): 

        if self.wing.planform.planform_depend_on_sections: 
            hint = None              # in case of a trapezoidal planform no problem ;-)  
        else: 
            nLines = len(self.wing.planform._norm_y_points ())
            hint = "Note: Leading and trailing edge will be approximated by %d straight lines.\n" % nLines + \
                   "Therefore the dxf contour shouldn't be used for final CAD design - use splines."
        return hint


    def refresh(self, dummy):
        self.panelArtist.refresh(figureUpdate=True)
        super().refresh()

    def cancel(self): 
        # changed bindings
        ctk_root.unbind(PANELS_CHANGED)
        super().cancel()

    def ok(self): 
        # release changed bindings
        ctk_root.unbind(PANELS_CHANGED)
        super().ok()





class Edit_File_Menu(Edit_Abstract):
    """ 
    Frame for the high level commands like load, save, ...
    The parent is the App itself
    """
    name = "File"

    def init (self):

        self.grid_columnconfigure   (0, weight=1)
        self.add (Header_Widget (self,0,0, lab=self.name, width=80))

        Button_Widget (self,1,0, lab='New',         width=100, pady=4, sticky = '', set=self.myApp.new)
        Button_Widget (self,2,0, lab='Open',        width=100, pady=4, sticky = '', set=self.myApp.open)
        Button_Widget (self,3,0, lab='Save',        width=100, pady=4, sticky = '', set=self.myApp.save)
        Button_Widget (self,4,0, lab='Save As...',  width=100, pady=4, sticky = '', set=self.myApp.saveAs)

        self.option_import = Option_Widget (self,5,0, width = 100, padx=(10,0), pady=4, 
                                            get=self.importDisplayValue, set = self.set_importType,
                                            options=self.importChoices())
        self.option_export = Option_Widget (self,6,0, width = 100, padx=(10,0), pady=4, 
                                            get=self.exportDisplayValue, set = self.set_exportType,
                                            options=self.exportChoices())

    def importChoices (self):      return ["Dxf as reference", "Other things"]
    def importDisplayValue (self): return "Import..."
    def set_importType (self, aType):
        self.option_import.refresh()
        if aType == "Dxf as reference":  self.myApp.load_reference_dxf() 

    def exportChoices (self):       return ["to Xflr5","to FLZ_vortex", "to DXF", "(Airfoils)"]
    def exportDisplayValue (self):  return "Export..."
    def set_exportType (self, aType):
        self.option_export.refresh()
        if aType == "to Xflr5":         self.myApp.export_xflr5 ()
        if aType == "to FLZ_vortex":    self.myApp.export_flz ()
        if aType == "to DXF":           self.myApp.export_dxf ()
        if aType == "to FLZ_vortex":  pass
        if aType == "Airfoils":  pass



#-------------------------------------------------------------------------------
# The App   
#-------------------------------------------------------------------------------

class App(ctk.CTk):

    name = AppName  

    def __init__(self, paramFile):
        super().__init__(fg_color= cl_background)

        global ctk_root                                 # being event handler

        # create the 'wing' model 
        self.paramFile = '' 
        self.loadNewWing (paramFile)

        # configure customtkinter
        self.appearance_mode = "Dark"                   
        ctk.set_appearance_mode(self.appearance_mode)   # Modes: "System" (standard), "Dark", "Light"
        ctk.set_default_color_theme("blue")             # Themes: "blue" (standard), "green", "dark-blue"

        # maximize the window using state property
        # self.state('zoomed')


        # setup event root for the widgets - so there will be a single root -> ctk root
        ctk_root = self
        Base_Widget.ctk_root = self
        self._curWingSectionName = None                 # Dispatcher field between Diagramm and Edit

        # intercept app close by user  
        self.protocol("WM_DELETE_WINDOW", self.onExit)

        # create main frames        
        diagram_frame = Diagrams        (self, self.wing, fg_color= cl_background)
        edit_frame    = Edit            (self, self.wing, height=500)
        file_frame    = Edit_File_Menu  (self, self.wing)

        # maingrid 2 x 2 - diagram on top, file and edit on bottom
        self.grid_rowconfigure   (0, weight=1)
        self.grid_rowconfigure   (1, weight=1)
        self.grid_columnconfigure(1, weight=1)
        diagram_frame.grid (row=0, column=0, columnspan=2, pady=0, padx=0, sticky="news")
        file_frame.grid    (row=1, column=0,               pady=(0,5), padx=(5,3), ipady=5,sticky="news")
        edit_frame.grid    (row=1, column=1,               pady=(0,5), padx=(2,5), sticky="nesw")



    def wing (self) -> Wing:
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

    @property
    def paramDir (self): 
        """directory of parameter file - which is default dir """
        if self.paramFile: 
            return os.path.dirname(self.paramFile)
        else: 
            return "."                                  # currentDir 

    @property
    def workingDir (self): 
        """default home directory for output files (e.g. export)
        Currently equals paramDir """
        return self.paramDir        

    #------- file functions ----------------

    def new (self):
        """ reset - and start with example ddefinition"""

        text = "The current wing '%s' will be discarded." % self.wing().name
        msg  = Messagebox(self, title="Create new wing", message=text,
                  icon="warning", option_2="Cancel", option_1="Ok")            
        if msg.get() == "Ok":
            self.loadNewWing ("")               # will create default winf


    def open (self):
        """ open a new wing definition json and load it"""

        filetypes  = [('Planform Creator 2 files', '*.json')]
        newPathFilename = filedialog.askopenfilename(
                    title='Select new wing definition',
                    initialdir=self.paramDir,
                    filetypes=filetypes)

        if newPathFilename:                     # user pressed open
            self.loadNewWing (newPathFilename)


    def save (self):
        """ save wing data to the action parameter file - if new wing to saveAs"""

        if self.paramFile:
            self.wing().save(self.paramFile)
            text = "Wing successfully saved ...    " 
            Messagebox(self, title="Save wing", message=text, icon="check", option_1="Ok", width=300, height=150)  
        else:
            self.saveAs ()


    def saveAs (self):
        """ save wing data to a new file and set this as actual"""

        filetypes  = [('PC2 files', '*.json')]
        newPathFilename = filedialog.asksaveasfilename(title='Save parameter file',
                                     initialdir=self.paramDir, filetypes=filetypes,
                                     defaultextension = '.json')
        if newPathFilename: 
            ret =  self.wing().save(newPathFilename)
            if ret == 0: 
                self.paramFile = os.path.normpath(newPathFilename)
                self.title("Planform Creator 2  [" + self.paramFile + "]")
                text = "Wing saved to \n\n'%s'" % newPathFilename
                Messagebox(self, title="Save wing", message=text, icon="check", option_1="Ok")  
            else: 
                text = "Wing couldn't be saved to '%s'" % newPathFilename
                Messagebox(self, title="Save wing", message=text, icon="cancel", option_1="Ok")  


    def loadNewWing(self, pathFilename):
        """loads and sets a new wing model returns - updates title """
        
        self.set_wing (Wing (pathFilename))

        if pathFilename:
            self.paramFile = os.path.relpath(pathFilename, start = os.getcwd())
            project = self.paramFile
        else:
            self.paramFile = ""
            project = "< new >"

        self.title (AppName + "  v" + str(AppVersion) + "  [" + project + "]")


    def export_xflr5 (self): 
        """ export wing to xflr5"""
        self.wait_window (Dialog_Export_Xflr5_Flz (self, self.wing, Xflr5=True, workingDir=self.workingDir))


    def export_flz (self): 
        """ export wing to xflr5"""
        self.wait_window (Dialog_Export_Xflr5_Flz (self, self.wing, Flz=True, workingDir=self.workingDir))


    def export_dxf (self):
        """export wing to dxf"""

        export_dialog = Dialog_Export_Dxf (self, wingFn = self.wing) 
        self.wait_window (export_dialog)

        if export_dialog.return_OK:

            message = self.wing().export_toDxf () 
            Messagebox (self, title="DXF export", message=message, icon="check", option_1="Ok")


    def load_reference_dxf (self): 
        """ load a dxf planform into the reference_dxf planform"""
        current_dxf_path = self.wing().refPlanform_DXF.dxf_pathFilename()

        dxf_dialog = Dialog_Load_DXF (self, wingFn = self.wing, dxf_Path = current_dxf_path, ref=True) 
        self.wait_window (dxf_dialog)

        if dxf_dialog.return_OK: 
            new_dxf_Path = dxf_dialog.dxf_pathFilename
            if new_dxf_Path:                            # dialog returned a valid path 
                self.wing().refPlanform_DXF.set_dxfPathFilename (new_dxf_Path) 
                fireEvent(PLANFORM_CHANGED)
            else:                                       # remove dxf reference file - extra code to make it clear
                self.wing().refPlanform_DXF.set_dxfPathFilename (None) 
                fireEvent(PLANFORM_CHANGED)

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

    # init colorama
    just_fix_windows_console()

    mySettings = ".\\examples\\vjx.glide\\VJX.glide.json"
    # mySettings = ".\\examples\\Amokka-JX\\Amokka-JX.json"
    # mySettings = ""

    InfoMsg("Starting  User Interface...")

    myApp = App(mySettings)
    myApp.mainloop()
 