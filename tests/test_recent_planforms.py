import json
import os
from unittest.mock import Mock

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

import pytest
from PyQt6.QtWidgets import QApplication, QMessageBox, QToolButton, QWidget

from airfoileditor.base.app_utils import Recent_Files, Settings
from planformcreator2 import app_modes
from planformcreator2.app_model import App_Model, Mode_Id
from planformcreator2.app_modes import Mode_Modify
from planformcreator2.ui.pc2_panels import Panel_File, Panel_File_Small


@pytest.fixture(scope="module")
def qapp():
    app = QApplication.instance() or QApplication([])
    yield app


@pytest.fixture
def model(qapp, tmp_path, monkeypatch):
    monkeypatch.setattr(Settings, "_pathFileName", str(tmp_path / "settings.json"))
    monkeypatch.setattr("planformcreator2.app_model.Worker", Mock())
    monkeypatch.setattr(App_Model, "_init_watchdog", lambda self: None)
    instance = App_Model(workingDir_default=str(tmp_path))
    instance.load_wing(None)
    return instance


@pytest.fixture
def mode(model, monkeypatch):
    instance = Mode_Modify(model)
    instance._stacked_panel = QWidget()
    monkeypatch.setattr(instance, "_toast_message", Mock())
    yield instance
    instance._stacked_panel.close()
    instance._stacked_panel.deleteLater()


def saved_wing(tmp_path, name):
    path = tmp_path / name
    path.write_text(json.dumps({"pc2_version": 5, "wing_name": path.stem}),
                    encoding="utf-8")
    return path


def test_model_history_is_shared_persistent_and_migrates_legacy(model, tmp_path):
    path = saved_wing(tmp_path, "legacy.pc2")
    settings = Settings()
    settings.set("last_opened", str(path))
    settings.set("unrelated", 42)
    settings.save()

    assert model.recent_files is model.recent_files
    assert model.recent_files.paths == [path]
    assert Recent_Files().paths == [path]
    assert Settings().get("unrelated") == 42
    model.recent_files.clear()
    assert model.recent_files.paths == []
    assert Settings().get("last_opened") == str(path)


def test_open_records_but_internal_load_templates_and_references_do_not(model, tmp_path):
    first = saved_wing(tmp_path, "first.pc2")
    second = saved_wing(tmp_path, "second.pc2")
    template = saved_wing(tmp_path, "template.pc2")

    model.remember_wing(None)
    model.remember_wing(model.wing)
    assert model.recent_files.paths == []
    model.open_wing(first)
    model.load_wing(second)
    assert model.recent_files.paths == [first]
    model.add_ref_pc2_file(str(first))
    model.remember_wing(model.ref_wing)
    assert model.recent_files.paths == [first]
    model.load_wing(template, as_new=True)
    model.remember_wing(model.wing)
    assert model.wing.is_new_wing
    assert model.recent_files.paths == [first]


def test_history_deduplicates_limits_and_forgets(model, tmp_path):
    paths = [saved_wing(tmp_path, f"wing{i}.pc2") for i in range(12)]
    for path in paths:
        model.open_wing(path)
    assert model.recent_files.paths == list(reversed(paths))[:15]
    model.open_wing(paths[4])
    assert model.recent_files.paths[0] == paths[4]
    assert model.recent_files.paths.count(paths[4]) == 1
    model.forget_wing_file(str(paths[4]))
    assert paths[4] not in model.recent_files.paths
    model.forget_wing_file(paths[5])
    assert paths[5] not in model.recent_files.paths


def test_startup_records_initial_file(mode, model, tmp_path):
    path = saved_wing(tmp_path, "startup.pc2")
    mode.on_enter(str(path))
    assert model.mode_id == Mode_Id.MODIFY
    assert model.recent_files.paths == [path]


@pytest.mark.parametrize("panel_cls", [Panel_File, Panel_File_Small])
def test_split_buttons_and_dynamic_recent_menu(mode, model, tmp_path, panel_cls):
    other = saved_wing(tmp_path, "a&b.pc2")
    current = saved_wing(tmp_path, "current.pc2")
    model.open_wing(other)
    model.open_wing(current)
    panel = panel_cls(mode, model)
    try:
        buttons = [button for button in panel.findChildren(QToolButton)
                   if button.text() == "&Open"]
        assert len(buttons) == 1
        button = buttons[0]
        assert button.popupMode() == QToolButton.ToolButtonPopupMode.MenuButtonPopup
        assert not button.autoRaise()
        assert button.width() == 100
        opened, recent = Mock(), Mock()
        panel.sig_open.connect(opened)
        panel.sig_open_recent.connect(recent)
        button.click()
        opened.assert_called_once_with()
        recent.assert_not_called()

        panel._recent_menu.aboutToShow.emit()
        action, = panel._recent_menu.actions()
        assert action.text() == "a&&b.pc2"
        assert action.toolTip() == str(other)
        action.trigger()
        recent.assert_called_once_with(str(other))
        opened.assert_called_once_with()

        model.recent_files.clear()
        panel._recent_menu.aboutToShow.emit()
        action, = panel._recent_menu.actions()
        assert action.text() == "No other recent planforms"
        assert not action.isEnabled()
    finally:
        panel.close()
        panel.deleteLater()


@pytest.mark.parametrize("accepted", [False, True])
def test_recent_open_honors_confirmation(mode, model, tmp_path, monkeypatch, accepted):
    path = saved_wing(tmp_path, "other.pc2")
    original = model.wing
    confirm = Mock(return_value=accepted)
    monkeypatch.setattr(mode, "confirm_close", confirm)
    mode.open_recent(str(path))
    confirm.assert_called_once_with()
    if accepted:
        assert model.wing.parm_path_abs == path
        assert model.recent_files.paths == [path]
    else:
        assert model.wing is original
        assert model.recent_files.paths == []


def test_missing_recent_is_removed_without_leaving_current(mode, model, tmp_path, monkeypatch):
    path = saved_wing(tmp_path, "missing.pc2")
    model.recent_files.add(path)
    path.unlink()
    original = model.wing
    confirm, error = Mock(), Mock()
    monkeypatch.setattr(mode, "confirm_close", confirm)
    monkeypatch.setattr(app_modes.MessageBox, "error", error)
    mode.open_recent(str(path))
    confirm.assert_not_called()
    error.assert_called_once()
    assert model.wing is original
    assert model.recent_files.paths == []


@pytest.mark.parametrize("selected", [False, True])
def test_open_dialog_records_only_selection(mode, model, tmp_path, monkeypatch, selected):
    path = saved_wing(tmp_path, "opened.pc2")
    monkeypatch.setattr(mode, "confirm_close", Mock(return_value=True))
    monkeypatch.setattr(app_modes.QFileDialog, "getOpenFileName",
                        Mock(return_value=(str(path) if selected else "", "")))
    mode.open()
    assert model.recent_files.paths == ([path] if selected else [])


@pytest.mark.parametrize("success", [False, True])
@pytest.mark.parametrize("save_as", [False, True])
def test_save_records_only_success(mode, model, tmp_path, monkeypatch, success, save_as):
    path = saved_wing(tmp_path, "saved.pc2")
    model.load_wing(path)
    original = model.wing
    monkeypatch.setattr(model.wing, "save", Mock(return_value=success))
    monkeypatch.setattr(app_modes.MessageBox, "error", Mock())
    monkeypatch.setattr(app_modes.QFileDialog, "getSaveFileName",
                        Mock(return_value=(str(path), "")))
    result = mode.save_as() if save_as else mode.save()
    assert result is success
    assert model.wing is original
    assert model.recent_files.paths == ([path] if success else [])


def test_cancelled_save_as_blocks_recent_open(mode, model, tmp_path, monkeypatch):
    path = saved_wing(tmp_path, "other.pc2")
    original = model.wing
    monkeypatch.setattr(model.wing, "has_changed", lambda: True)
    monkeypatch.setattr(app_modes.MessageBox, "save",
                        Mock(return_value=QMessageBox.StandardButton.Save))
    monkeypatch.setattr(app_modes.QFileDialog, "getSaveFileName",
                        Mock(return_value=("", "")))
    mode.open_recent(str(path))
    assert model.wing is original
    assert model.recent_files.paths == []


def test_failed_save_blocks_recent_open(mode, model, tmp_path, monkeypatch):
    current = saved_wing(tmp_path, "current.pc2")
    other = saved_wing(tmp_path, "other.pc2")
    model.open_wing(current)
    original = model.wing
    monkeypatch.setattr(model.wing, "has_changed", lambda: True)
    monkeypatch.setattr(model.wing, "save", lambda: False)
    monkeypatch.setattr(app_modes.MessageBox, "save",
                        Mock(return_value=QMessageBox.StandardButton.Save))
    monkeypatch.setattr(app_modes.MessageBox, "error", Mock())
    mode.open_recent(str(other))
    assert model.wing is original
    assert model.recent_files.paths == [current]


def test_new_wing_save_as_records_destination(mode, model, tmp_path, monkeypatch):
    path = tmp_path / "saved.pc2"
    monkeypatch.setattr(app_modes.QFileDialog, "getSaveFileName",
                        Mock(return_value=(str(path), "")))
    assert mode.save() is True
    assert path.is_file()
    assert model.wing.parm_path_abs == path
    assert model.recent_files.paths == [path]


@pytest.mark.parametrize("renamed", [False, True])
def test_rename_forgets_old_path_only_after_success(mode, model, tmp_path, monkeypatch, renamed):
    old_path = saved_wing(tmp_path, "old.pc2")
    new_path = tmp_path / "renamed.pc2"
    model.open_wing(old_path)

    def rename():
        if renamed:
            model.wing.set_parm_fileName_new(new_path.stem)

    monkeypatch.setattr(app_modes, "Dialog_Rename", Mock(return_value=Mock(exec=rename)))
    mode.rename()
    assert model.recent_files.paths == ([new_path] if renamed else [old_path])


@pytest.mark.parametrize("compact", [False, True])
def test_both_mode_panels_connect_recent_signal(mode, monkeypatch, compact):
    opened = Mock()
    monkeypatch.setattr(mode, "open_recent", opened)
    container = mode.panel_small if compact else mode.panel
    try:
        panel = container.findChild(Panel_File)
        assert panel is not None
        panel.sig_open_recent.emit("recent.pc2")
        opened.assert_called_once_with("recent.pc2")
    finally:
        container.close()
        container.deleteLater()
