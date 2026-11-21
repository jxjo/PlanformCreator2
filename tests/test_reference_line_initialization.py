import tempfile
import unittest

from planformcreator2.model.planform import (
    N_Reference_Line_Bezier, N_Reference_Line_Polyline,
)
from planformcreator2.model.wing import Wing


class TestReferenceLineInitialization(unittest.TestCase):

    def test_polyline_loads_before_section_repair_and_round_trips(self):
        parameters = {
            "pc2_version": 5,
            "chord_distribution": {"chord_style": "Trapezoid"},
            "wingSections": [
                {"xn": 0.0, "cn": 1.0},
                {"xn": 0.7, "cn": 0.5},
                {"xn": 0.3, "cn": 0.8},
                {"xn": 0.8, "cn": 0.1},
                {"xn": 1.0, "cn": 0.25},
            ],
            "reference_line": {"points": [[1, 0.2], [2, -0.1], [3, 0.3]]},
        }
        with tempfile.TemporaryDirectory() as directory:
            wing = Wing(None, parameters=parameters, defaultDir=directory)
            planform = wing.planform
            ref_line = planform.n_ref_line

            self.assertIsInstance(ref_line, N_Reference_Line_Polyline)
            self.assertEqual([section.xn for section in planform.wingSections],
                             [0.0, 0.3, 0.7, 1.0])
            self.assertEqual(ref_line.point_at_section(planform.wingSections[1]), -0.1)
            self.assertEqual(ref_line.point_at_section(planform.wingSections[2]), 0.2)
            self.assertEqual(ref_line._as_dict(), {"points": [(1, -0.1), (2, 0.2)]})

            saved = {}
            planform._save_to(saved)
            saved["pc2_version"] = 5
            loaded = Wing(None, parameters=saved, defaultDir=directory).planform
            self.assertEqual(loaded.n_ref_line._as_dict(), ref_line._as_dict())

            direct = N_Reference_Line_Polyline(loaded, dataDict={"points": [[1, 0.4]]})
            self.assertEqual(direct.point_at_section(loaded.wingSections[1]), 0.4)
            self.assertEqual(N_Reference_Line_Polyline(loaded)._as_dict(), {"points": []})


    def test_duplicate_kink_positions_keep_points_and_produce_valid_hinge(self):
        parameters = {
            "pc2_version": 5,
            "chord_distribution": {"chord_style": "Trapezoid"},
            "wingSections": [
                {"xn": 0.0, "cn": 1.0},
                {"xn": 0.5, "cn": 0.7},
                {"xn": 0.5, "cn": 0.6},
                {"xn": 1.0, "cn": 0.25},
            ],
            "reference_line": {"points": [[2, 0.1], [1, 0.2]]},
        }
        with tempfile.TemporaryDirectory() as directory:
            wing = Wing(None, parameters=parameters, defaultDir=directory)
            planform = wing.planform
            ref_line = planform.n_ref_line

            xn, yn = ref_line.polyline()
            self.assertEqual(xn.tolist(), [0.0, 0.5, 1.0])
            self.assertEqual(yn.tolist(), [0.0, 0.1, 0.0])
            hinge_x, _ = planform.flaps.hinge_polyline()
            self.assertEqual(hinge_x.tolist(), [0.0, 600.0, 1200.0])
            self.assertEqual(ref_line._as_dict(), {"points": [(1, 0.2), (2, 0.1)]})

            saved = {}
            planform._save_to(saved)
            saved["pc2_version"] = 5
            loaded = Wing(None, parameters=saved, defaultDir=directory).planform
            self.assertEqual(loaded.n_ref_line.polyline()[1].tolist(), yn.tolist())
            self.assertEqual(loaded.n_ref_line._as_dict(), ref_line._as_dict())

            loaded.wingSections[2].set_xn(0.65)
            xn, yn = loaded.n_ref_line.polyline()
            self.assertEqual(xn.tolist(), [0.0, 0.5, 0.65, 1.0])
            self.assertEqual(yn.tolist(), [0.0, 0.2, 0.1, 0.0])


    def test_bezier_and_elliptical_initialization(self):
        with tempfile.TemporaryDirectory() as directory:
            for style, expected in (
                ("Bezier", {"px": [0.0, 0.5, 1.0], "py": [0.0, 0.2, 0.0]}),
                ("Elliptical", {"px": [0.0, 1.0], "py": [0.0, 0.0]}),
            ):
                with self.subTest(style=style):
                    wing = Wing(None, parameters={
                        "pc2_version": 5,
                        "chord_distribution": {"chord_style": style},
                        "reference_line": {"px": [0.0, 0.5, 1.0], "py": [0.0, 0.2, 0.0]},
                    }, defaultDir=directory)
                    self.assertIsInstance(wing.planform.n_ref_line, N_Reference_Line_Bezier)
                    self.assertEqual(wing.planform.n_ref_line._as_dict(), expected)


if __name__ == "__main__":
    unittest.main()
