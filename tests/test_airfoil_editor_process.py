import os
import sys
import unittest
from unittest.mock import Mock, patch

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PyQt6 import sip
from PyQt6.QtCore import QProcess
from PyQt6.QtWidgets import QApplication, QWidget

from planformcreator2.ui.pc2_panels import AirfoilEditor_Launcher


class TestAirfoilEditorShutdown(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.app = QApplication.instance() or QApplication([])

    def test_destroying_window_stops_editor_without_completion_callback(self):
        window = QWidget()
        launcher = AirfoilEditor_Launcher(window)
        edited = Mock()
        launcher.edited.connect(edited)
        start = QProcess.start
        errors = []
        exception_hook = sys.excepthook
        sys.excepthook = lambda *args: errors.append(args)
        try:
            with patch("planformcreator2.ui.pc2_panels._is_frozen", return_value=False), \
                 patch.object(QProcess, "start",
                              lambda process, program, arguments: start(
                                  process, sys.executable,
                                  ["-c", "import time; time.sleep(60)"])):
                launcher.start("example.dat")
            process = launcher._process
            self.assertTrue(process.waitForStarted(3000))
            pid = process.processId()
            self.assertGreater(pid, 0)
            stopped = []
            wait_for_finished = process.waitForFinished

            def wait(timeout):
                result = wait_for_finished(timeout)
                stopped.append((result, process.state()))
                return result

            with patch.object(process, "waitForFinished", side_effect=wait):
                sip.delete(window)
            self.assertEqual(stopped, [(True, QProcess.ProcessState.NotRunning)])
            edited.assert_not_called()
            self.assertEqual(errors, [])
        finally:
            sys.excepthook = exception_hook
            if not sip.isdeleted(window):
                sip.delete(window)

    def test_completion_restores_window_and_emits_path(self):
        for exit_code in [0, 2]:
            with self.subTest(exit_code=exit_code):
                window = QWidget()
                launcher = AirfoilEditor_Launcher(window)
                edited = Mock()
                launcher.edited.connect(edited)
                start = QProcess.start
                try:
                    with patch("planformcreator2.ui.pc2_panels._is_frozen", return_value=False), \
                         patch("planformcreator2.ui.pc2_panels.MessageBox.error") as error, \
                         patch.object(QProcess, "start",
                                      lambda process, program, arguments: start(
                                          process, sys.executable,
                                          ["-c", f"import sys; sys.exit({exit_code})"])):
                        launcher.start("example.dat")
                        self.assertFalse(window.isEnabled())
                        self.assertTrue(launcher._process.waitForFinished(3000))
                        self.assertTrue(window.isEnabled())
                        self.assertIsNone(launcher._process)
                        self.assertIsNone(launcher._overlay)
                        if exit_code == 0:
                            edited.assert_called_once_with("example.dat")
                            error.assert_not_called()
                        else:
                            edited.assert_not_called()
                            error.assert_called_once()
                finally:
                    sip.delete(window)

    def test_failed_start_restores_window_and_reports_error(self):
        window = QWidget()
        launcher = AirfoilEditor_Launcher(window)
        start = QProcess.start
        try:
            with patch("planformcreator2.ui.pc2_panels._is_frozen", return_value=False), \
                 patch("planformcreator2.ui.pc2_panels.MessageBox.error") as error, \
                 patch.object(QProcess, "start",
                              lambda process, program, arguments: start(
                                  process, "nonexistent-airfoil-editor-executable", [])):
                launcher.start("example.dat")
                if launcher._process is not None:
                    launcher._process.waitForStarted(3000)
                self.assertTrue(window.isEnabled())
                self.assertIsNone(launcher._process)
                error.assert_called_once()
        finally:
            sip.delete(window)
