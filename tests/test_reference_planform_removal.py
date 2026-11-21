import json
import tempfile
import unittest
from pathlib import Path

from PyQt6.QtCore import QObject

from planformcreator2.app_model import App_Model
from planformcreator2.model.wing import Wing
from planformcreator2.model.wing_reference import Reference_Wing_Elliptical



class TestReferencePlanformRemoval(unittest.TestCase):

    def setUp(self):
        """Create an app model without starting its worker or watchdog."""
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.working_dir = Path(self.directory.name)
        self.model = App_Model.__new__(App_Model)
        QObject.__init__(self.model)
        self.model._wing = Wing(
            None, parameters={"pc2_version": 5}, defaultDir=self.directory.name)
        self.model._ref_wing = None
        self.notifications = []
        self.model.sig_ref_wing_changed.connect(lambda: self.notifications.append(True))


    def add_reference(self, filename, wing_name):
        """Add a real PC2 reference with an independently specified wing name."""
        path = self.working_dir / filename
        path.write_text(json.dumps({"pc2_version": 5, "wing_name": wing_name}),
                        encoding="utf-8")
        self.model.add_ref_pc2_file(str(path))


    def test_removal_uses_filename_not_internal_wing_name(self):
        """Remove a reference even when its internal name is not its filename."""
        self.add_reference("Spencer.pc2", "Spencer")
        self.add_reference("next.pc2", "Another wing")
        self.model.set_ref_wing_filename("Spencer.pc2")
        self.notifications.clear()

        self.model.remove_ref_pc2_file()

        self.assertEqual(self.model.ref_wing_filename, "next.pc2")
        self.assertEqual(self.model.ref_wings_filename, ["next.pc2", "Elliptical"])
        self.assertTrue(self.notifications)
        self.assertTrue((self.working_dir / "Spencer.pc2").is_file())


    def test_removal_selects_next_reference_in_order(self):
        """Select the following entry, including the elliptical fallback."""
        for filename in ["first.pc2", "middle.pc2", "last.pc2"]:
            self.add_reference(filename, "Shared wing name")

        for filename, next_filename in [
            ("middle.pc2", "last.pc2"),
            ("last.pc2", "Elliptical"),
            ("first.pc2", "Elliptical"),
        ]:
            with self.subTest(filename=filename):
                self.model.set_ref_wing_filename(filename)
                self.model.remove_ref_pc2_file()
                self.assertEqual(self.model.ref_wing_filename, next_filename)
                self.assertNotIn(filename, self.model.ref_wings_filename)

        self.assertEqual(self.model.wing.ref_pc2_files, [])
        self.assertIsInstance(self.model.ref_wing, Reference_Wing_Elliptical)


    def test_pc2_wing_named_elliptical_can_be_removed(self):
        """Do not confuse an internal wing name with the built-in reference."""
        self.add_reference("custom.pc2", "Elliptical")

        self.model.remove_ref_pc2_file()

        self.assertEqual(self.model.wing.ref_pc2_files, [])
        self.assertIsInstance(self.model.ref_wing, Reference_Wing_Elliptical)


    def test_no_selection_and_builtin_reference_are_not_removed(self):
        """Leave the collection and selection unchanged for non-PC2 references."""
        for selection in [None, "Elliptical"]:
            with self.subTest(selection=selection):
                self.model.set_ref_wing_filename(selection)
                reference = self.model.ref_wing
                self.notifications.clear()

                self.model.remove_ref_pc2_file()

                self.assertIs(self.model.ref_wing, reference)
                self.assertEqual(self.model.ref_wings_filename, ["Elliptical"])
                self.assertEqual(self.notifications, [])


if __name__ == "__main__":
    unittest.main()
