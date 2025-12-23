#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""

The different operating modes of the app like View_Mode and Modify_Mode

The modes are controlled by the Modes_Manager 

"""
import      os
import      shutil

from typing                 import override

from PyQt6.QtCore           import Qt, pyqtSignal, QObject, QTimer, QMargins
from PyQt6.QtWidgets        import QHBoxLayout, QMessageBox, QStackedWidget, QFileDialog

from .app_model             import App_Model, Mode_Id
from .model.wing            import Wing, FILENAME_NEW

from airfoileditor.base.widgets     import style, Icon       
from airfoileditor.base.panels      import Container_Panel, Toaster, MessageBox       
from airfoileditor.base.app_utils   import Settings


from .resources             import get_template_dir
from .ui.pc2_dialogs        import Dialog_Rename, Dialog_Select_Template
from .ui.pc2_panels         import (Panel_File, Panel_File_Small, Panel_Wing, Panel_Wing_Small,
                                    Panel_Chord_Reference, Panel_Chord_Reference_Small,
                                    Panel_WingSection, Panel_WingSection_Small)


import logging
logger = logging.getLogger(__name__)
# logger.setLevel(logging.DEBUG)


# ------------------------------------------------------------------------------


class Data_Panel (Container_Panel):
    """ 
    Base class for data panels used in different modes. 
    - support double click to minimize/maximize
    - ensure refresh when visible
    """
    
    def __init__ (self, *args, **kwargs):
        super().__init__(*args, **kwargs)


        self.set_doubleClick (self.mode.toggle_minimized)
        self.add_hint ("Double click to minimize/maximize")

        self.app_model.sig_new_wing.connect             (self.refresh)
        self.app_model.sig_planform_changed.connect     (self.refresh)
        self.app_model.sig_wingSection_changed.connect  (self.refresh)
        self.app_model.sig_wingSection_selected.connect (self.refresh)

    @property
    def mode (self) -> 'Mode_Abstract':
        return self._app 

    @property
    def app_model (self) -> App_Model:
        return self.dataObject   

    @override
    def refresh (self, always: bool = False):
        """ refresh panels of self - only when visible  """

        if self.isVisible() or always:
            super().refresh()


# ------------------------------------------------------------------------------


class Mode_Abstract (QObject):
    """
    Abstract base class for different application modes.

    A Mode provides the lower data panel of the UI and has methods for
        - entering and exiting the mode.
        - App level functions specific to the mode.

    It is managed by the Modes_Manager.
    """

    mode_id  : Mode_Id = None                                   # the id of the mode

    sig_exit_requested          = pyqtSignal()                  # signal to request mode and app exit
    sig_leave_requested         = pyqtSignal()                  # signal to request mode leave
    sig_switch_mode_requested   = pyqtSignal(Mode_Id, object)   # signal to request mode switch
    sig_toggle_minimized        = pyqtSignal()                  # signal to request toggle of minimized state


    def __init__(self, app_model: App_Model):
        super().__init__()

        self._app_model = app_model

        self._panel:       Data_Panel = None
        self._panel_small: Data_Panel = None

        self._stacked_panel:  QStackedWidget  = None


    def __repr__(self):
        """ nice representation of self """
        return f"<{self.__class__.__name__}>"


    @property
    def app_model (self): 
        """ app data model """
        return self._app_model


    def _toast_message (self, msg, toast_style = style.HINT):
        """ show toast message """
        
        Toaster.showMessage (self.stacked_panel, msg, corner=Qt.Corner.BottomLeftCorner, margin=QMargins(10, 10, 10, 5),
                             toast_style=toast_style)

    @property
    def panel(self):
        return self._panel

    @property
    def panel_small(self):
        return self._panel_small

    @property
    def stacked_panel(self) -> QStackedWidget:
        """ Get the stacked widget containing both data panels. """

        if self._stacked_panel is None:
            self._stacked_panel = QStackedWidget()
            self._stacked_panel.addWidget (self.panel)
            self._stacked_panel.addWidget (self.panel_small)

            # set initial visibility
            self.set_current_panel(refresh=False)

        return self._stacked_panel


    def on_enter(self, initial_arg = None):
        """ 
        Actions to perform when entering the mode. 
            Override in subclasses if needed. 
            Call super().on_enter() to ensure mode_id is set in app_model.
        """

        on_str = f" with arg {initial_arg}" if initial_arg is not None else ""   
        logger.debug(f"Entering {self.__class__.__name__} {on_str}")


    def on_leave(self):
        """ Actions to perform when exiting the mode. Override in subclasses if needed. """
        logger.debug(f"Exiting {self.__class__.__name__}")


    def prepare_check_enter(self, on_arg=None) -> object:
        """ Check if the mode can be entered. Prepare and Return initial object. """
        # to be overridden in subclasses if needed.
        return on_arg
    
    @property
    def current_panel(self) -> Data_Panel:
        """ Get the currently visible data panel. """
        return self.stacked_panel.currentWidget() 


    def set_current_panel (self, minimized: bool = False, refresh=True):
        """ Set self and all modes to minimized or normal state. """

        if minimized:
            new_current = self.panel_small
        else:
            new_current = self.panel

        self.stacked_panel.setCurrentWidget (new_current)

        if refresh:
            new_current.refresh(always=True)


    def toggle_minimized (self):
        """ Toggle the minimized state of the data panel. """
        self.sig_toggle_minimized.emit ()



    # ---- User actions ----

    def cancel (self):
        """ User action: Cancel current mode and request exit. """
 
        QTimer.singleShot (0, self.sig_leave_requested.emit)    # leave after current events processed


    def finish (self):
        """ User action: Finish current mode and request exit. """

        QTimer.singleShot (0, self.sig_leave_requested.emit)    # leave after current events processed


    def switch_mode (self, mode_id: Mode_Id, on_arg=None):
        """ User action: Switch to another mode. """
         
        QTimer.singleShot (0, lambda: self.sig_switch_mode_requested.emit (mode_id, on_arg))



# ------------------------------------------------------------------------------


class Mode_Modify (Mode_Abstract):
    """
    Application mode for modifying airfoils.
    """

    mode_id = Mode_Id.MODIFY                       # the id of the mode

    @property 
    def wing (self):
        """ current wing from app state """
        return self.app_model.wing
    

    def prepare_check_enter(self, on_arg = None ) -> Wing | None:
        """ Check if the mode can be entered. Override in subclasses if needed. """

        return on_arg
        # info if airfoil is flapped 


    def on_enter(self, pc2_file: str):
        """ Actions to perform when entering the mode. """

        self.app_model.set_mode (self.mode_id)
        self.app_model.load_wing (pc2_file)
        super().on_enter()


    def on_leave(self):
        """ Actions to perform when exiting the mode.  """

        super().on_leave()


    def _on_leaving_planform (self) -> bool:
        """ 
        Do checks and handle if user wants to leave current planform.
        Returns True if leaving ok, False if cancelled 
        """

        leave = True
        clean_up_all = False

        if self.wing.has_changed(): 
            message = "The planform has been modified..\n\n" + \
                      "Do you want to save before leaving?"
            button = MessageBox.save(self.stacked_panel, f"Leaving {self.wing.parm_fileName}", message)

            if button == QMessageBox.StandardButton.Save:
                if self.wing.is_new_wing:
                    leave = self.save_as ()                 # save as new file - or cancel
                else:
                    self.save()                             # save parameter file

            elif button == QMessageBox.StandardButton.Discard:
                # on Discard remove temp dir of airfoil strak etc
                clean_up_all = True

            elif button == QMessageBox.StandardButton.Cancel:
                leave = False

        if leave:
            self.app_model.cleanup_wing (all = clean_up_all)

        return leave


    def open (self):
        """ open a new wing definition and load it"""

        if not self._on_leaving_planform ():                                    # changes made? user cancelled?
            return
        
        filters    = "PlanformCreator2 files (*.pc2)"
        workingDir = self.wing.workingDir

        newPathFilename, _ = QFileDialog.getOpenFileName(self.stacked_panel, filter=filters, directory=workingDir)

        if newPathFilename:                     
            self.app_model.load_wing (newPathFilename)


    def save (self):
        """ save wing data to the action parameter file - if new wing to saveAs"""

        if self.wing.is_new_wing:
            self.save_as ()
            return

        ok = self.wing.save ()
        if ok:
            self._toast_message (f"Planform {self.wing.parm_fileName} saved", toast_style=style.GOOD)
        else:
            MessageBox.error   (self.stacked_panel,"Save Planform", f"<b>{self.wing.parm_fileName}</b> couldn't be saved", min_height= 60)


    def save_as (self) ->bool: 
        """ 
        Save current airfoil as ...
        Returns True if saved, False if cancelled or error"""

        filters   = "PlanformCreator2 files (*.pc2)"
        directory = self.wing.workingDir
        newPathFilename, _ = QFileDialog.getSaveFileName(self.stacked_panel, filter=filters, directory=directory)

        if not newPathFilename: 
            return

        ok = self.wing.save (newPathFilename = newPathFilename)
        if ok:
            self._toast_message (f"Planform saved as {newPathFilename}", toast_style=style.GOOD)
            self.app_model.notify_fileName_changed()            # refresh title ...
        else:
            MessageBox.error   (self.stacked_panel,"Save Planform", f"<b>{newPathFilename}</b> couldn't be saved", min_height= 60)

            self.app_model.load_wing (newPathFilename)          # reload old wing
            return



    def new (self):
        """ reset - and start with example definition"""

        if not self._on_leaving_planform ():                                    # changes made? user cancelled?
            return

        # select a new template 
        template_dir = str(get_template_dir())

        try:
            dialog = Dialog_Select_Template (self.stacked_panel, template_dir, parentPos=(0.3, 0.0), dialogPos=(0,1.2))
            dialog.exec()     
            template_filePathName = dialog.template_file_selected
        except FileNotFoundError:
            MessageBox.error (self.stacked_panel, "New Planform", f"Template directory '{template_dir}' not found.")
            template_filePathName = None

        if template_filePathName and os.path.isfile (template_filePathName):

            #load new planform from template and save in USER dir
            self.app_model.load_wing (template_filePathName, as_new=True)




    def rename (self): 
        """ rename current airfoil as ..."""

        dialog = Dialog_Rename (self.stacked_panel, lambda: self.wing, 
                                parentPos=(0.3,-0.6), dialogPos=(0,1))  
        dialog.exec()   

        self.app_model.load_wing (self.wing.parm_pathFileName_abs)      # reload wing to apply new name / filename



    def delete (self): 
        """ delete current airfoil ..."""

        # airfoil = self.app_model.airfoil

        # if not os.path.isfile (airfoil.pathFileName_abs): return 

        # text = f"Airfoil <b>{airfoil.fileName}</b> including temporary files will be deleted."
        # button = MessageBox.warning (self.stacked_panel, "Delete airfoil", text)

        # if button == QMessageBox.StandardButton.Ok:

        #     self.delete_temp_files (silent=True)
        #     os.remove (airfoil.pathFileName_abs)                               # remove airfoil

        #     self._toast_message (f"Airfoil {airfoil.fileName} deleted", toast_style=style.GOOD)
        #     logger.info (f"Airfoil {airfoil.fileName} deleted")

        #     next_airfoil = get_next_airfoil_in_dir (airfoil, example_if_none=True)
        #     self.app_model.set_airfoil (next_airfoil)                           # try to set on next airfoil

        #     if next_airfoil.isExample:
        #        button = MessageBox.info (self.stacked_panel, "Delete airfoil", "This was the last airfoil in the directory.<br>" + \
        #                                        "Showing Example airfoil") 


    def delete_temp_files (self): 
        """ delete all temp files and directories of current airfoil ..."""

        text = f"Delete temporary files of <b>{self.wing.parm_fileName}</b>.<br><br>" +\
               f"The planform will be saved and reloaded after this." 

        msg = MessageBox (self.stacked_panel, "Delete Temp Files", text, Icon (Icon.INFO), min_width=300)
        msg.setStandardButtons(QMessageBox.StandardButton.Ok | MessageBox.StandardButton.Cancel)
        button = msg.exec()

        if button == QMessageBox.StandardButton.Ok:
      
            self.wing.save ()                                     # save first to ensure actual data for reload
            self.wing.remove_tmp ()
            self.app_model.load_wing (self.wing.parm_pathFileName_abs)  # reload wing for new strak etc.

            MessageBox.info (self.stacked_panel, "Delete Temp Files", "Temporary files removed")


    def exit (self):
        """ User action: leave current mode and app. """

        leave = self._on_leaving_planform ()            # changes made? user cancelled?

        if leave:
            # still new wing? - remove temp dir of airfoil strak etc
            if self.wing.is_new_wing:
                self.app_model.cleanup_wing (all = True)

            self.sig_exit_requested.emit ()

    
    @property
    def panel (self) -> Data_Panel:
        """ lower UI main panel - modify mode """

        if self._panel is None: 

            l = QHBoxLayout()

            p = Panel_File (self, self.app_model, width=250, lazy=True)
            p.sig_toggle_panel_size.connect     (self.toggle_minimized)
            p.sig_open.connect                  (self.open)
            p.sig_new.connect                   (self.new)
            p.sig_save.connect                  (self.save)
            p.sig_save_as.connect               (self.save_as)
            p.sig_rename.connect                (self.rename)
            p.sig_delete.connect                (self.delete)
            p.sig_delete_temp_files.connect     (self.delete_temp_files)
            p.sig_exit .connect                 (self.exit)
            l.addWidget (p)

            l.addWidget (Panel_Wing                 (self, self.app_model, lazy=True))
            l.addWidget (Panel_Chord_Reference      (self, self.app_model, lazy=True))
            l.addWidget (Panel_WingSection          (self, self.app_model, lazy=True))

            self._panel = Data_Panel (self, self.app_model, layout = l)

        return self._panel


    @property
    def panel_small (self) -> Data_Panel:
        """ lower UI view panel - small version"""

        if self._panel_small is None: 

            l = QHBoxLayout()

            p = Panel_File_Small (self, self.app_model, width=250, lazy=True, has_head=False)
            p.sig_toggle_panel_size.connect     (self.toggle_minimized)
            p.sig_open.connect                  (self.open)
            p.sig_new.connect                   (self.new)
            p.sig_save.connect                  (self.save)
            p.sig_save_as.connect               (self.save_as)
            p.sig_rename.connect                (self.rename)
            p.sig_delete.connect                (self.delete)
            p.sig_delete_temp_files.connect     (self.delete_temp_files)
            p.sig_exit .connect                 (self.exit)
            l.addWidget (p)

            l.addWidget (Panel_Wing_Small               (self, self.app_model, lazy=True, has_head=False))
            l.addWidget (Panel_Chord_Reference_Small    (self, self.app_model, lazy=True, has_head=False))
            l.addWidget (Panel_WingSection_Small        (self, self.app_model, lazy=True, has_head=False))

            self._panel_small = Data_Panel (self, self.app_model, layout=l)

        return self._panel_small



# ------------------------------------------------------------------------------


class Modes_Manager (QObject):
    """
    Manages the different application modes.

    The Modes Manager provides a stacked data panel which presents the lower part of the UI
    according to the current mode.
    """

    sig_close_requested = pyqtSignal()                      # signal to request app close


    def __init__(self, app_model: App_Model):
        super().__init__()

        self._modes_dict  = {}
        self._app_model   = app_model

        self._modes_panel : QStackedWidget = None

        self._height            = 250                       # default height of modes panel
        self._height_minimized  = 150                       # default height of modes panel when minimized

        s = Settings()
        self._is_minimized = s.get('lower_panel_minimized', False)


    def _switch_mode_panel (self, mode: Mode_Abstract):
        """ switch stacked widget to panel of mode """

        if not (mode.mode_id in self._modes_dict):
            logger.error (f"Mode {mode} not registered in Mode_Manager.")

        # set small oder normal  
        mode.set_current_panel (self._is_minimized, refresh=True)

        # switch stacked widget to new mode panel
        self._modes_panel.setCurrentWidget (mode.stacked_panel)

        # setting of the actual width of the modes panel after switching needs to be done
        # after all current events are processed so that the size hint of the new panel is valid
        QTimer.singleShot (100, self._set_min_width)        # set min width after current events processed


    def _set_min_width (self):
        """ set minimum width of modes panel according to current mode panel """

        # because QStackedWidget takes the width of the widest widget of all its children,
        # we need to set the minimum width of the stacked widget to the minimum width of the current mode panel

        if self.current_mode is not None:
            self._modes_panel.setMinimumWidth (0)
            self._modes_panel.adjustSize()
            min_width = self.current_mode.current_panel.calc_min_width()
            self._modes_panel.setMinimumWidth (min_width)


    @property
    def current_mode (self) -> Mode_Abstract:
        """ Get the current active mode. """

        mode_id = self._app_model.mode_id
        if mode_id is None:
            return None
        elif mode_id in self._modes_dict:
            return self._modes_dict[mode_id]
        else:
            logger.warning(f"Mode {mode_id} not registered in Mode_Manager.")
            return None     
       

    def add_mode(self, mode: Mode_Abstract):
        """ add a mode to the manager """

        mode_id = mode.mode_id

        if not mode_id in self._modes_dict:

            self._modes_dict [mode_id] = mode                        # register mode

            # connect to signals of Mode
            mode.sig_exit_requested.connect         (self.exit)
            mode.sig_leave_requested.connect        (self.leave_mode)
            mode.sig_switch_mode_requested.connect  (self.switch_mode)
            mode.sig_toggle_minimized.connect       (self.toggle_minimized)


    def set_mode (self, mode_id: Mode_Id, on_arg=None):
        """ set initial mode """

        if mode_id not in self._modes_dict:
            logger.error(f"Mode {mode_id} not registered in Mode_Manager.")
            return

        if self._app_model.mode_id is not None:
            logger.error("Initial mode already set in App_Model.")
            return

        mode : Mode_Abstract = self._modes_dict [mode_id]

        # prepare new mode and argument to work on
        arg =  mode.prepare_check_enter (on_arg)

        mode.on_enter(arg)                                              # set mode
        mode.set_current_panel (self._is_minimized)                     # select the right panels small or not



    def stacked_modes_panel (self) -> QStackedWidget:
        """ build all modes panels and return them as stacked widget """

        if self._modes_panel is not None:
            return self._modes_panel
        
        self._modes_panel = QStackedWidget()

        # collect all modes and their panels
        mode : Mode_Abstract
        for mode_id, mode in self._modes_dict.items():
            self._modes_panel.addWidget (mode.stacked_panel)                 # add mode's data panels to stacked widget

        # switch stacked widget to current mode panel
        if self.current_mode is not None:
            self._modes_panel.setCurrentWidget (self.current_mode.stacked_panel)

        return self._modes_panel


    def switch_mode (self, new_mode_id: Mode_Id, on_arg=None):
        """ switch to given mode """

        if not new_mode_id in self._modes_dict:
            logger.error(f"Mode {new_mode_id} not registered in Mode_Manager.")
            return

        if self._app_model.mode_id is None:
            logger.error("Cannot switch modes: No initial mode set")
            return

        # leave current mode
        self.current_mode.on_leave()

        # prepare new mode and argument to work on
        new_mode   : Mode_Abstract = self._modes_dict [new_mode_id]
        new_arg =  new_mode.prepare_check_enter (on_arg)
        if new_arg is None:  return                                         # cannot enter mode         
        
        # enter new mode, switch data panels
        new_mode.on_enter(new_arg)                                          # prepare enter new mode
        self._switch_mode_panel (new_mode)                                  # switch stacked widget - make visible



    def leave_mode (self):
        """ leave current mode and return to view mode """

        self.switch_mode (Mode_Id.VIEW)


    def exit(self):
        """ exit mode and close app if in view mode """

        if self.current_mode is not None:
            self.current_mode.on_leave()

        s = Settings()
        s.set('lower_panel_minimized', self._is_minimized)
        s.save()

        self.sig_close_requested.emit()             # close app if view mode finished


    def set_height (self, height: int, minimized: int|None = None):
        """ set height of modes panel """

        self._height = height
        self._height_minimized = minimized if minimized is not None else self._height_minimized

        self._set_minimized (self._is_minimized)                     # apply height change


    def _set_minimized (self, minimized: bool):
        """ set minimized state of modes panel """

        self._is_minimized = minimized

        if self.current_mode is not None:
            self.current_mode.set_current_panel (minimized)         # set panel small or normal

        # set to predefined height
        height = self._height_minimized if minimized else self._height
        self._modes_panel.setFixedHeight (height)    


    def toggle_minimized (self):
        """ toggle minimized state of modes panel """

        if self.current_mode is not None:
            self._set_minimized (not self._is_minimized)

            # adjust new min with of data panel
            self._set_min_width()        


