#!/usr/bin/env pythonbutton_color
# -*- coding: utf-8 -*-

"""  

All abstract diagram items to build a complete diagram view 

"""

import logging

from typing             import override

from PyQt6.QtCore       import QSize, QMargins, pyqtSignal, QTimer
from PyQt6.QtWidgets    import QLayout, QGridLayout, QVBoxLayout, QHBoxLayout, QGraphicsGridLayout
from PyQt6.QtWidgets    import QMainWindow, QWidget, QWidgetItem
from PyQt6.QtGui        import QPalette, QIcon

import pyqtgraph as pg                          # import PyQtGraph after PyQt6
from pyqtgraph          import icons

from base.panels        import Edit_Panel
from base.widgets       import ToolButton, Icon

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)



class Diagram (QWidget):
    """ 
    Abstract container panel/widget which may hold
     - serveral PlotItems 
     - each having optional view panel sections (to the left) 

    see: https://pyqtgraph.readthedocs.io/en/latest/getting_started/plotting.html

    """

    width  = (800, None)                # (min,max) 
    height = (400, None)                # (min,max)

    def __init__(self, parent, getter = None, **kwargs):
        super().__init__(parent, **kwargs)

        self._getter = getter
        self._myApp  = parent
        self._section_panel = None 

        # create graphics widget 

        self._graph_widget = pg.GraphicsLayoutWidget (parent=self, show=True, size=None, title=None)
        self._graph_layout.setContentsMargins (20,20,20,10)  # default margins

        # remove default context menu of scene

        self._graph_widget.ci.scene().contextMenu = []


        # create all plot items and setup layout with them  

        self.create_diagram_items () 

        #  add a message view box at bottom   

        self._message_vb = pg.ViewBox()
        self._graph_layout.addItem (self._message_vb, 5, 0)
        self._message_vb.hide()
    

        # create optional view panel add the left side 

        self._viewPanel   = None
        self.create_view_panel ()

        # build layout with view panel and graphics

        l_main = QHBoxLayout()
        l_main.setContentsMargins (QMargins(0, 0, 0, 0))
        # l_main.setSpacing (0)

        if self._viewPanel is not None: 
            l_main.addWidget (self._viewPanel)
        l_main.addWidget (self._graph_widget, stretch=10)

        self.setLayout (l_main)


    def __repr__(self) -> str:
        # overwritten to get a nice print string 
        text = '' 
        return f"<{type(self).__name__}{text}>"


    def data_list (self): 
        # to be overloaded - or implemented with semantic name   

        if callable(self._getter):
            obj = self._getter()
        else: 
            obj = self._getter     

        return obj if isinstance (obj, list) else [obj]


    @property
    def myApp (self): 
        return self._myApp
    
    @property
    def diagram_items (self) -> list['Diagram_Item']:
        """ list of my diagram items """
        items = []
        for i in range (self._graph_layout.count()):
            item = self._graph_layout.itemAt(i)
            if isinstance (item, Diagram_Item):
                items.append (item)
        return items


    @property 
    def _graph_layout (self) -> QGraphicsGridLayout:
        """ returns QLayout of self"""
        return self._graph_widget.ci.layout


    @property
    def section_panel (self) -> Edit_Panel | None:
        """ small section panel representing self in view panel"""
        # overload for constructor 
        return self._section_panel
    

    def refresh(self): 
        """ refresh all childs (Diagram_Items) of self"""

        if self.isVisible():
            item : Diagram_Item
            for item in self.diagram_items:
                item.refresh() 

            if self.section_panel:
                self.section_panel.refresh()

    @override
    def showEvent (self, ev):
        """ QShowEvent - self becomes visible  - Tab switch or first time"""

        # refresh plotItems - if it is first time they are intially plotted
        item : Diagram_Item
        for item in self.diagram_items:
            item.refresh() 

        super().showEvent (ev) 



    def create_diagram_items ():
        """ create all plot Items and add them to the layout """
        # to be overlaoded 
        # like ... 
        #   item = Airfoil_Diagram_Item (self, getter=self.airfoil)
        #   self._add_item (item, 0, 0)
        pass


    def _add_item (self, anItem: 'Diagram_Item', row, col, rowspan=1, colspan=1):
        """ adds a diagram item to self graphic layout """

        self._graph_widget.addItem (anItem, row, col, rowspan=rowspan, colspan=colspan)

        anItem.sig_visible.connect (self._on_item_visible)


    def create_view_panel (self):
        """ 
        creates a view panel to the left of at least one diagram item 
        has a section_panel
        """

        # build side view panel with the section panels 

        layout = QVBoxLayout()
        layout.setContentsMargins (QMargins(0, 0, 0, 0)) 
        for item in self.diagram_items:
            if item.section_panel is not None: 
                layout.addWidget (item.section_panel,stretch=1)

        # add section panel of self (master) 

        if self.section_panel is not None: layout.addWidget (self.section_panel,stretch=0)
        
        layout.addStretch (1)

        self._viewPanel = QWidget()
        self._viewPanel.setMinimumWidth(180)
        self._viewPanel.setMaximumWidth(250)
        self._viewPanel.setLayout (layout)


    def _on_item_visible (self, aBool):
        """ slot to handle actions when switch on/off of diagram items"""

        nItems = len([item for item in self.diagram_items if item.isVisible()])

        if nItems == 0: 
            self._message_show ("No diagram items selected ")
        # the GraphicsLayout gets confused, if all items were switched off
        #     and then switched on again - recalc layout
        if aBool: 
            if nItems > 0:
                self._message_clear()

                # also remove a welcome message
                self.diagram_items[0].set_welcome (None) 



    def _message_show (self, aMessage):
        """ shows aMessage in the middle of the diagram """
        text = pg.TextItem(aMessage, anchor=(0.5,0))
        text.setPos(0, 0)
        self._message_vb.addItem (text)
        self._message_vb.show()


    def _message_clear (self):
        """ shows aMessage in the middle of the diagram """
        self._message_vb.clear()
        self._message_vb.hide()




class Diagram_Item (pg.PlotItem):
    """ 
    Abstract PlotItem  

    see: https://pyqtgraph.readthedocs.io/en/latest/getting_started/plotting.html

    """

    name = "Abstract Diagram_Item"              # used for link and section header

    # Signals 
    sig_visible = pyqtSignal(bool)              # when self is set to show/hide 


    def __init__(self, parent, 
                 getter = None, 
                 show = True,                   # show initially 
                 **kwargs):

        super().__init__(name=self.name,        # to link view boxes 
                         **kwargs)

        self._parent : Diagram = parent
        self._getter = getter
        self._show   = show 
        self._section_panel = None 

        ## Set up additional control button to reset range 

        self._vb_state_changed = False 
        ico = Icon (Icon.RESETVIEW,light_mode = False)
        self._resetBtn = pg.ButtonItem(pixmap=ico.pixmap(QSize(52,52)), width=16, parentItem=self)
        self._resetBtn.mode = 'auto'
        self._resetBtn.clicked.connect(self._resetBtn_clicked)
        timer = QTimer()                                
        timer.singleShot(50, self._reset_prepare)           # delayed when all initial drawing done 

        # setup artists - must be override

        self.setup_artists ()

        # setup view range - must be override

        self._viewRange_set = False
        # self.setup_viewRange ()

        # setup view range - must be override
        
        self.setup_axis()

        # allow only view box context menu  (not plot item) 

        self.setMenuEnabled(enableMenu=False, enableViewBoxMenu=True)

        # initial show or hide - use super() - avoid refresh
 
        super().setVisible (show) 



    def __repr__(self) -> str:
        # overwritten to get a nice print string 
        text = '' 
        return f"<{type(self).__name__}{text}>"
    

    @override
    def setVisible (self, aBool):
        """ Qt overloaded to signal parent """
        super().setVisible (aBool)

        self.set_show (aBool)

        self.sig_visible.emit (aBool)

        logger.debug  (f"{self} - setVisible {aBool}")


    def set_show (self, aBool):
        """ switch on/off artists of self"""
        # to override
        self._show = aBool

        if aBool: 
            self.refresh_artists ()


    @override
    def close (self):
        # PlotItem override to remove button
        self._resetBtn.setParent (None) 
        self._resetBtn = None
        super().close()


    @override
    def resizeEvent(self, ev):
        # PlotItem override to remove button
        if self._resetBtn is None:  ## already closed down
            return
        btnRect = self.mapRectFromItem(self._resetBtn, self._resetBtn.boundingRect())
        y = self.size().height() - btnRect.height()
        self._resetBtn.setPos(20, y+3)            # right aside autoBtn
        super().resizeEvent (ev)


    @override
    def updateButtons(self):
        # PlotItem override to show/hide reset button
        super().updateButtons ()
        try:
            if self.mouseHovering and not self.buttonsHidden and self._vb_state_changed: #  and not all(self.vb.autoRangeEnabled()):
                self._resetBtn.show()
            else:
                self._resetBtn.hide()
        except RuntimeError:
            pass  # this can happen if the plot has been deleted.


    def _resetBtn_clicked(self):
        """ reset button was clicked - set initial view range"""

        if self._vb_state_changed:
            self.setup_viewRange ()
            self._vb_state_changed = False


    def _reset_prepare (self):
        """ prepare reset button - save initial range"""
        self.sigRangeChanged.connect(self._viewRangeChanged)


    def _viewRangeChanged (self): 
        """ slot - view Range changed"""
        self._vb_state_changed = True 


    @property
    def viewBox (self) -> pg.ViewBox:
        """ viewBox of self""" 
        return self.getViewBox()

    def data_object (self): 
        # to be ooverloaded - or implemented with semantic name 
        if callable(self._getter):
            return self._getter()
        else: 
            return self._getter


    def data_list (self): 
        # to be overloaded - or implemented with semantic name        
        if isinstance (self.data_object(), list):
            return self.data_object()
        else: 
            return [self.data_object()]   


    def refresh(self): 
        """ refresh my artits and section panel """

        if self.section_panel is not None: 
            # refresh artists only if self section is switched on 
            if self.section_panel.switched_on:
                self.refresh_artists()          # first artist and then panel 

            self.section_panel.refresh()

        else: 
            self.refresh_artists() 

        # initial, deferred setup of viewRange 

        if not self._viewRange_set:
            self.setup_viewRange ()
            self._viewRange_set = True


    def setup_artists (self):
        """ create and setup the artists of self"""
        # must be implemented by subclass
        
        logger.warning (f"{self} no artists are setup")


    def setup_viewRange (self):
        """ define view range of this plotItem"""
        # must be implemented by subclass
        
        logger.warning (f"{self} no view range defined")


    def setup_axis (self):
        """ setup axis of of this plotItem"""
        # can be implemented by subclass
        
        # define a constant width 
        yaxis : pg.AxisItem = self.getAxis ('left')
        yaxis.setWidth (40)


    def refresh_artists (self): 
        """ refresh the artists of self"""
        # must be implmented by subclass
        pass


    @property
    def section_panel (self) -> Edit_Panel | None:
        """ small section panel representing self in view panel"""
        # overload for constructor 
        return  self._section_panel
    
