import os
import unittest
from types import SimpleNamespace
from unittest.mock import Mock

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

import pyqtgraph as pg
from PyQt6.QtCore import QPointF, Qt
from PyQt6.QtTest import QTest
from PyQt6.QtWidgets import QApplication

from airfoileditor.base.math_util import JPoint
from planformcreator2.ui.pc2_artists import Movable_Polyline


class TestMovablePolylineClick(unittest.TestCase):

    def test_ctrl_click_is_accepted_only_when_point_is_added(self):
        for added in (False, True):
            with self.subTest(added=added):
                position = SimpleNamespace(x=lambda: 5.0, y=lambda: 6.0)
                event = SimpleNamespace(
                    button=lambda: Qt.MouseButton.LeftButton,
                    modifiers=lambda: Qt.KeyboardModifier.ControlModifier,
                    pos=lambda: position,
                    accept=Mock(), ignore=Mock(),
                )
                polyline = SimpleNamespace(
                    clickable=True,
                    mouseShape=lambda: SimpleNamespace(contains=lambda _: True),
                    _add_point=Mock(return_value=added),
                )

                Movable_Polyline.mouseClickEvent(polyline, event)

                polyline._add_point.assert_called_once_with(5.0, 6.0)
                self.assertEqual(event.accept.call_count, int(added))
                self.assertEqual(event.ignore.call_count, int(not added))


    def test_rejected_click_reaches_scene_unaccepted(self):
        app = QApplication.instance() or QApplication([])
        widget = pg.PlotWidget()
        widget.resize(500, 400)
        widget.show()
        widget.setXRange(0, 10, padding=0)
        widget.setYRange(-5, 5, padding=0)
        polyline = Movable_Polyline(
            [JPoint((0, 0)), JPoint((10, 0))], movable=True, color="green")
        widget.addItem(polyline)
        polyline._add_point = Mock(return_value=False)
        clicked = []
        widget.scene().sigMouseClicked.connect(lambda event: clicked.append(event.isAccepted()))
        app.processEvents()

        scene_pos = widget.getPlotItem().vb.mapViewToScene(QPointF(5, 0))
        position = widget.mapFromScene(scene_pos)
        QTest.mouseMove(widget.viewport(), position)
        app.processEvents()
        QTest.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton,
                         Qt.KeyboardModifier.ControlModifier, position)
        app.processEvents()

        polyline._add_point.assert_called_once()
        self.assertEqual(clicked, [False])
        widget.close()


if __name__ == "__main__":
    unittest.main()
