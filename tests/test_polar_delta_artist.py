import os
from types import SimpleNamespace
from unittest.mock import Mock

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

import numpy as np

from planformcreator2.model.VLM_wing import VLM_Polar_Comparison, VLM_Var
from planformcreator2.ui.pc2_artists import VLM_Polar_Delta_Artist


def test_insert_zero_crossings_interpolates_each_sign_change():
    x = np.array([0.0, 1.0, 2.0, 3.0])
    delta = np.array([-1.0, 1.0, 2.0, -2.0])

    actual_x, actual_delta = VLM_Polar_Delta_Artist._insert_zero_crossings(x, delta)

    np.testing.assert_allclose(actual_x, [0.0, 0.5, 1.0, 2.0, 2.5, 3.0])
    np.testing.assert_array_equal(actual_delta, [-1.0, 0.0, 1.0, 2.0, 0.0, -2.0])


def test_insert_zero_crossings_does_not_cross_nan_gaps():
    x = np.array([0.0, 1.0, 2.0, 3.0])
    delta = np.array([-1.0, np.nan, np.nan, 1.0])

    actual_x, actual_delta = VLM_Polar_Delta_Artist._insert_zero_crossings(x, delta)

    np.testing.assert_array_equal(actual_x, x)
    np.testing.assert_equal(actual_delta, delta)


def test_delta_data_explains_reference_polar_without_operating_points():
    polar = SimpleNamespace(is_ready=True, opPoints=[object()])
    ref_polar = SimpleNamespace(is_ready=True, opPoints=[])
    artist = SimpleNamespace(
        vlm_polar=polar,
        ref_vlm_polar=ref_polar,
        delta_vars=(VLM_Var.WING_ALPHA, VLM_Var.WING_CL),
        _plot_text=Mock(),
        SIZE_NORMAL=10,
    )

    VLM_Polar_Delta_Artist._plot(artist)

    artist._plot_text.assert_called_once_with(
        "Reference wing polar has no operating points",
        color="dimgray", fontSize=10, itemPos=(0.5, 1))


def test_comparison_identifies_relative_deviation_variables():
    assert VLM_Polar_Comparison.is_relative_deviation(VLM_Var.WING_CD)
    assert VLM_Polar_Comparison.is_relative_deviation(VLM_Var.WING_GLIDE)
    assert not VLM_Polar_Comparison.is_relative_deviation(VLM_Var.WING_CL)
