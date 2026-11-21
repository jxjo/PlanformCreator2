import inspect
import tempfile
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import PropertyMock, patch

from planformcreator2.app_model import App_Model
from planformcreator2.model.wing import Wing
from planformcreator2.model.wing_reference import (
    Reference_PC2_File, Reference_Wing, Reference_Wing_Elliptical, Scale_Mode)


class TestReferencePC2File(unittest.TestCase):

    def make_wing(self, directory, parameters):
        class WingWithoutSave(Wing):
            is_reference_wing = True

        with patch("planformcreator2.model.wing.Planform"), \
             patch("planformcreator2.model.wing.Planform_Mesh"):
            return WingWithoutSave(None, parameters=parameters, defaultDir=str(directory))

    def test_relative_path_resolves_and_serializes_relative_to_wing(self):
        with tempfile.TemporaryDirectory() as directory:
            working_dir = Path(directory)
            pc2_path = working_dir / "reference.pc2"
            pc2_path.touch()

            pc2_file = Reference_PC2_File(
                {"path": "reference.pc2", "scale_mode": Scale_Mode.MATCH_SPAN_AND_AREA.value},
                working_dir,
            )

            self.assertEqual(pc2_file.path_abs, pc2_path.resolve())
            self.assertEqual(pc2_file.scale_mode, Scale_Mode.MATCH_SPAN_AND_AREA)
            self.assertEqual(
                pc2_file._as_dict(working_dir),
                {"path": "reference.pc2", "scale_mode": Scale_Mode.MATCH_SPAN_AND_AREA.value},
            )

    def test_external_path_serializes_absolute_and_invalid_mode_defaults(self):
        with tempfile.TemporaryDirectory() as directory, tempfile.TemporaryDirectory() as external:
            working_dir = Path(directory)
            pc2_path = Path(external) / "reference.pc2"
            pc2_path.touch()
            pc2_file = Reference_PC2_File(
                {"path": str(pc2_path), "scale_mode": "invalid"}, working_dir)

            self.assertEqual(pc2_file.scale_mode, Scale_Mode.NO_SCALE)
            self.assertEqual(pc2_file._as_dict(working_dir), {
                "path": pc2_path.resolve().as_posix(),
            })

    def test_missing_path_raises(self):
        with tempfile.TemporaryDirectory() as directory:
            with self.assertRaises(FileNotFoundError):
                Reference_PC2_File({"path": "missing.pc2"}, Path(directory))

    def test_wing_loads_plural_entries_and_skips_missing_files(self):
        with tempfile.TemporaryDirectory() as directory:
            working_dir = Path(directory)
            pc2_path = working_dir / "reference.pc2"
            pc2_path.touch()
            wing = self.make_wing(working_dir, {
                "pc2_version": 5,
                "reference_pc2_files": [
                    {"path": "reference.pc2", "scale_mode": Scale_Mode.MATCH_SPAN_AND_AREA.value},
                    {"path": "missing.pc2", "scale_mode": Scale_Mode.NO_SCALE.value},
                ],
            })

            self.assertEqual(len(wing.ref_pc2_files), 1)
            self.assertEqual(wing.ref_pc2_files[0].path_abs, pc2_path.resolve())
            self.assertEqual(wing.ref_pc2_files[0].scale_mode, Scale_Mode.MATCH_SPAN_AND_AREA)

    def test_wing_loads_legacy_single_file_key(self):
        with tempfile.TemporaryDirectory() as directory:
            working_dir = Path(directory)
            pc2_path = working_dir / "legacy.pc2"
            pc2_path.touch()
            wing = self.make_wing(working_dir, {
                "pc2_version": 5,
                "reference_pc2_file": "legacy.pc2",
            })

            self.assertEqual(len(wing.ref_pc2_files), 1)
            self.assertEqual(wing.ref_pc2_files[0].path_abs, pc2_path.resolve())
            self.assertEqual(wing.ref_pc2_files[0].scale_mode, Scale_Mode.NO_SCALE)

    def test_wing_save_serializes_paths_relative_to_working_directory(self):
        with tempfile.TemporaryDirectory() as directory:
            working_dir = Path(directory)
            pc2_path = working_dir / "reference.pc2"
            pc2_path.touch()
            wing = self.make_wing(working_dir, {
                "pc2_version": 5,
                "reference_pc2_files": [{"path": str(pc2_path)}],
            })
            wing._planform._save_to = lambda parameters: None
            wing._planform_mesh._as_dict = lambda: {}

            with patch("planformcreator2.model.wing.Image_Definition") as image_definition:
                image_definition.return_value._as_dict.return_value = {}
                saved = wing._save()

            self.assertEqual(saved.get("reference_pc2_files"), [{
                "path": "reference.pc2",
            }])

    def test_wing_collection_keeps_entries_and_preserves_order(self):
        with tempfile.TemporaryDirectory() as directory:
            working_dir = Path(directory)
            first_path = working_dir / "first.pc2"
            second_path = working_dir / "second.pc2"
            first_path.touch()
            second_path.touch()

            first = Reference_PC2_File({"path": str(first_path)}, working_dir)
            second = Reference_PC2_File({"path": str(second_path)}, working_dir)
            wing = object.__new__(Wing)
            wing._working_dir = working_dir
            wing._ref_pc2_files = [first]

            wing.update_ref_pc2_files(second)

            self.assertIs(wing.ref_pc2_files[0], first)
            self.assertIs(wing.ref_pc2_files[1], second)
            self.assertEqual(
                wing.ref_wing_filenames, ["first.pc2", "second.pc2", "Elliptical"])

            wing.update_ref_pc2_files(second, remove=True)
            self.assertEqual(wing.ref_wing_filenames, ["first.pc2", "Elliptical"])

    def test_reference_wing_retains_file_object_and_syncs_scale_mode(self):
        with tempfile.TemporaryDirectory() as directory:
            pc2_path = Path(directory) / "reference.pc2"
            pc2_path.touch()
            pc2_file = Reference_PC2_File({"path": str(pc2_path)}, Path(directory))
            parent_wing = object()
            planform = SimpleNamespace(
                calc_area=lambda normed: 1.0,
                wingSections=SimpleNamespace(refresh_polar_sets=lambda: None),
            )
            wing_init_args = []

            def fake_wing_init(instance, parm_file_path, **kwargs):
                wing_init_args.append(parm_file_path)
                instance._parm_pathFileName = parm_file_path

            with patch.object(Wing, "__init__", fake_wing_init), \
                 patch.object(Reference_Wing, "planform", new_callable=PropertyMock, return_value=planform):
                reference_wing = Reference_Wing(pc2_file, parent_wing)

            self.assertIs(reference_wing.reference_pc2_file, pc2_file)
            self.assertEqual(wing_init_args, [pc2_file.path_abs])
            self.assertEqual(reference_wing.scale_mode, Scale_Mode.NO_SCALE)

            reference_wing.set_scale_mode(Scale_Mode.MATCH_ROOT_CHORD_AND_AREA)
            self.assertEqual(pc2_file.scale_mode, Scale_Mode.MATCH_ROOT_CHORD_AND_AREA)

    def test_reference_wing_requires_file_object_without_scale_override(self):
        parameters = inspect.signature(Reference_Wing).parameters

        self.assertEqual(list(parameters), ["reference_pc2_file", "parent_wing"])
        self.assertIs(parameters["reference_pc2_file"].default, inspect.Parameter.empty)

    def test_elliptical_reference_uses_generated_parameters_without_pc2_file(self):
        with tempfile.TemporaryDirectory() as directory:
            parent_wing = SimpleNamespace(
                _parms={},
                workingDir=directory,
                planform=SimpleNamespace(
                    wingSections=SimpleNamespace(without_for_panels=[])),
            )
            planform = SimpleNamespace(
                calc_area=lambda normed: 1.0,
                wingSections=SimpleNamespace(refresh_polar_sets=lambda: None),
            )
            wing_init_args = []

            def fake_wing_init(instance, parm_file_path, **kwargs):
                wing_init_args.append(parm_file_path)
                instance._parm_pathFileName = "new.pc2"

            with patch.object(Wing, "__init__", fake_wing_init), \
                 patch.object(Reference_Wing, "planform", new_callable=PropertyMock, return_value=planform):
                reference_wing = Reference_Wing_Elliptical(parent_wing)

            self.assertEqual(wing_init_args, [None])
            self.assertIsNone(reference_wing.reference_pc2_file)
            self.assertEqual(
                reference_wing.scale_mode, Scale_Mode.MATCH_ROOT_CHORD_AND_SPAN)

    def test_app_model_updates_and_selects_stored_entry_objects(self):
        with tempfile.TemporaryDirectory() as directory:
            working_dir = Path(directory)
            first_path = working_dir / "first.pc2"
            second_path = working_dir / "second.pc2"
            first_path.touch()
            second_path.touch()

            wing = object.__new__(Wing)
            wing._working_dir = working_dir
            wing._ref_pc2_files = []
            app_model = App_Model.__new__(App_Model)
            app_model._wing = wing
            app_model._ref_wing = None
            app_model.notify_ref_wing_changed = lambda: None

            def make_reference(pc2_file, parent_wing):
                return SimpleNamespace(
                    name=pc2_file.path.name,
                    filename=pc2_file.path.name,
                    scale_mode=pc2_file.scale_mode,
                    reference_pc2_file=pc2_file,
                    set_scale_mode=pc2_file.set_scale_mode,
                    vlm_wing_reset=lambda: None,
                )

            with patch("planformcreator2.app_model.Reference_Wing", side_effect=make_reference) as factory:
                app_model.add_ref_pc2_file(str(first_path))
                first_entry = wing.ref_pc2_files[0]
                self.assertIs(factory.call_args.args[0], first_entry)

                app_model.add_ref_pc2_file(str(second_path))
                second_entry = wing.ref_pc2_files[1]
                self.assertIs(factory.call_args.args[0], second_entry)
                self.assertIs(wing.ref_pc2_files[0], first_entry)

                app_model.set_ref_wing_filename("first.pc2")
                self.assertIs(factory.call_args.args[0], first_entry)
                self.assertIs(wing.get_ref_pc2_file("first.pc2"), first_entry)

                app_model.set_ref_wing_scale_mode(Scale_Mode.MATCH_ROOT_CHORD_AND_AREA)
                self.assertEqual(first_entry.scale_mode, Scale_Mode.MATCH_ROOT_CHORD_AND_AREA)

                app_model.remove_ref_pc2_file()

            self.assertEqual(app_model.ref_wing_filename, "second.pc2")
            self.assertEqual([item.path.name for item in wing.ref_pc2_files], ["second.pc2"])


if __name__ == "__main__":
    unittest.main()