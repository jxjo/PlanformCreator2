#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Windows build helper for PlanformCreator2.

This script replaces complex batch logic with a small, readable Python driver.
It supports:
- building the shared dual-exe PyInstaller bundle
- building the NSIS installer
- running both steps in sequence
"""

from __future__ import annotations

import argparse
import importlib.metadata
import shutil
import subprocess
import sys
import time
from pathlib import Path

try:
    from packaging.version import Version
except ImportError:  # pragma: no cover - packaging should be available in build env
    Version = None


APP_NAME        = "PlanformCreator2"
DESCRIPTION     = "An interactive wing planform design tool"
PC2_ICON_NAME   = "PC2.ico"
AE_ICON_NAME    = "AE.ico"


def _ensure_airfoileditor_version(package_version: str) -> None:
    """Fail if installed AirfoilEditor is older than PlanformCreator2 version."""

    try:
        ae_version = importlib.metadata.version("airfoileditor")
    except importlib.metadata.PackageNotFoundError as exc:
        raise RuntimeError(
            "AirfoilEditor is not installed in the current Python environment. "
            "Install or upgrade it before building."
        ) from exc

    if Version is None:
        raise RuntimeError(
            "Python package 'packaging' is required for version comparison. "
            "Install it with: pip install packaging"
        )

    if Version(ae_version) < Version(package_version):
        raise RuntimeError(
            f"AirfoilEditor version {ae_version} is older than PlanformCreator2 version {package_version}. "
            "Please upgrade AirfoilEditor before building."
        )

    print(f"Version check OK: airfoileditor {ae_version} >= planformcreator2 {package_version}")


def _dual_spec_text(repo_root: Path, pc2_icon_path: Path, ae_icon_path: Path) -> str:
    """Return the generated PyInstaller spec for a shared dual-exe onedir build."""

    repo_root_text = repo_root.as_posix()
    pc2_icon_text = pc2_icon_path.as_posix()
    ae_icon_text = ae_icon_path.as_posix()

    return f"""# -*- mode: python ; coding: utf-8 -*-
from pathlib import Path
from PyInstaller.utils.hooks import collect_data_files, collect_submodules


REPO_ROOT = Path({repo_root_text!r})


datas = [
    (str(REPO_ROOT / 'icons'), 'planformcreator2/icons'),
    (str(REPO_ROOT / 'templates'), 'planformcreator2/templates'),
]
datas += collect_data_files('airfoileditor')
hiddenimports = collect_submodules('airfoileditor')


a_pc2 = Analysis(
    [str(REPO_ROOT / 'planformcreator2.py')],
    pathex=[str(REPO_ROOT / 'planformcreator2')],
    binaries=[],
    datas=datas,
    hiddenimports=hiddenimports,
    hookspath=[],
    hooksconfig={{}},
    runtime_hooks=[],
    excludes=['matplotlib', 'numpy.tests', 'PyQt6.QtWebEngine', 'charset_normalizer.md__mypyc'],
    noarchive=False,
    optimize=0,
)
pyz_pc2 = PYZ(a_pc2.pure)


a_ae = Analysis(
    [str(REPO_ROOT / 'dev' / 'airfoileditor_launcher.py')],
    pathex=[str(REPO_ROOT / 'planformcreator2')],
    binaries=[],
    datas=[],
    hiddenimports=[],
    hookspath=[],
    hooksconfig={{}},
    runtime_hooks=[],
    excludes=['matplotlib', 'numpy.tests', 'PyQt6.QtWebEngine', 'charset_normalizer.md__mypyc'],
    noarchive=False,
    optimize=0,
)
pyz_ae = PYZ(a_ae.pure)


exe_pc2 = EXE(
    pyz_pc2,
    a_pc2.scripts,
    [],
    exclude_binaries=True,
    name='PlanformCreator2',
    debug=False,
    bootloader_ignore_signals=False,
    strip=False,
    upx=True,
    console=False,
    disable_windowed_traceback=False,
    argv_emulation=False,
    target_arch=None,
    codesign_identity=None,
    entitlements_file=None,
    icon=[{pc2_icon_text!r}],
)


exe_ae = EXE(
    pyz_ae,
    a_ae.scripts,
    [],
    exclude_binaries=True,
    name='AirfoilEditor',
    debug=False,
    bootloader_ignore_signals=False,
    strip=False,
    upx=True,
    console=False,
    disable_windowed_traceback=False,
    argv_emulation=False,
    target_arch=None,
    codesign_identity=None,
    entitlements_file=None,
    icon=[{ae_icon_text!r}],
)


coll = COLLECT(
    exe_pc2,
    exe_ae,
    a_pc2.binaries,
    a_pc2.datas,
    a_ae.binaries,
    strip=False,
    upx=True,
    upx_exclude=[],
    name='PlanformCreator2',
)
"""


def _run(cmd: list[str], cwd: Path, capture_output: bool = False) -> str:
    print(f"> {' '.join(cmd)}")
    result = subprocess.run(
        cmd,
        cwd=cwd,
        text=True,
        capture_output=capture_output,
        check=False,
    )
    if result.returncode != 0:
        if result.stdout:
            print(result.stdout)
        if result.stderr:
            print(result.stderr)
        raise RuntimeError(f"Command failed with exit code {result.returncode}: {' '.join(cmd)}")
    return result.stdout.strip() if capture_output else ""


def _project_metadata(repo_root: Path) -> tuple[str, str]:
    package_name = _run(["hatch", "project", "metadata", "name"], cwd=repo_root, capture_output=True)
    package_version = _run(["hatch", "project", "metadata", "version"], cwd=repo_root, capture_output=True)
    return package_name, package_version


def _replace_dir_with_retry(src: Path, dst: Path, attempts: int = 6, delay_s: float = 0.8) -> None:
    """Replace dst with src, handling transient Windows file locks."""

    last_exc: Exception | None = None

    for i in range(1, attempts + 1):
        try:
            _remove_dir_if_exists(dst, attempts=1, delay_s=delay_s)
            src.rename(dst)
            return
        except FileNotFoundError as exc:
            # Source or destination can race with external tools (indexer/AV).
            last_exc = exc
            if not src.exists():
                raise RuntimeError(f"Build output source folder disappeared: {src}") from exc
            print(f"Rename attempt {i}/{attempts} failed (path race), retrying...")
            time.sleep(delay_s)
        except PermissionError as exc:
            last_exc = exc
            print(f"Rename attempt {i}/{attempts} failed (permission denied), retrying...")
            time.sleep(delay_s)

    # Fallback: copy then remove source, sometimes succeeds when rename keeps failing.
    try:
        if dst.exists():
            shutil.rmtree(dst)
        shutil.copytree(src, dst)
        shutil.rmtree(src)
        return
    except Exception as exc:  # pragma: no cover - defensive fallback
        if last_exc is not None:
            raise RuntimeError(f"Failed to move build output after retries: {last_exc}") from exc
        raise


def _remove_dir_if_exists(path: Path, attempts: int = 6, delay_s: float = 0.8) -> None:
    """Best-effort directory removal with retries for transient Windows locks/races."""

    for i in range(1, attempts + 1):
        try:
            if not path.exists():
                return
            shutil.rmtree(path)
            return
        except FileNotFoundError:
            # Directory vanished between exists() and rmtree().
            return
        except PermissionError:
            print(f"Remove attempt {i}/{attempts} failed (permission denied), retrying...")
            time.sleep(delay_s)

    # Last attempt: allow exception to propagate with original traceback.
    if path.exists():
        shutil.rmtree(path)


def _resolve_pyinstaller_output_dir(dist_dir: Path) -> Path:
    """Return the produced onedir folder, handling case differences in names."""

    candidates = [
        dist_dir / APP_NAME,
        dist_dir / APP_NAME.lower(),
    ]
    for candidate in candidates:
        if candidate.is_dir():
            return candidate

    raise FileNotFoundError(
        f"PyInstaller output directory not found in {dist_dir}. "
        f"Checked: {', '.join(str(c) for c in candidates)}"
    )


def build_exe(repo_root: Path) -> None:
    package_name, package_version = _project_metadata(repo_root)

    win_exe_dir = f"{package_name}-{package_version}_win_exe"
    dist_dir = repo_root / "dist"
    final_out = dist_dir / win_exe_dir

    launcher = repo_root / "dev" / "airfoileditor_launcher.py"
    if not launcher.exists():
        raise FileNotFoundError(f"AirfoilEditor launcher not found: {launcher}")

    print()
    print("------ Create Windows exe using PyInstaller")
    print(f"App             : {APP_NAME}")
    print(f"Package name    : {package_name}")
    print(f"Package version : {package_version}")
    print(f"Output folder   : {win_exe_dir}")

    pc2_icon_path = repo_root / "icons" / PC2_ICON_NAME
    if not pc2_icon_path.is_file():
        raise FileNotFoundError(f"PlanformCreator2 icon not found: {pc2_icon_path}")

    ae_icon_path = repo_root / "icons" / AE_ICON_NAME
    if not ae_icon_path.is_file():
        print(f"Note: AirfoilEditor icon not found: {ae_icon_path}. Falling back to {pc2_icon_path}.")
        ae_icon_path = pc2_icon_path
    print(f"PC2 icon        : {pc2_icon_path}")
    print(f"AE icon         : {ae_icon_path}")

    temp_dir = repo_root / "dev" / "_pyi_tmp"
    spec_path = temp_dir / "dual_build.spec"
    temp_dir.mkdir(parents=True, exist_ok=True)
    spec_path.write_text(_dual_spec_text(repo_root, pc2_icon_path, ae_icon_path), encoding="utf-8")

    _remove_dir_if_exists(dist_dir / APP_NAME)
    _remove_dir_if_exists(dist_dir / APP_NAME.lower())

    _run(
        [
            "pyinstaller",
            "--noconfirm",
            "--log-level=INFO",
            "--distpath",
            "dist",
            str(spec_path),
        ],
        cwd=repo_root,
    )

    pyinstaller_out = _resolve_pyinstaller_output_dir(dist_dir)

    airfoileditor_exe = pyinstaller_out / "AirfoilEditor.exe"
    if not airfoileditor_exe.exists():
        raise FileNotFoundError(f"AirfoilEditor executable not found after build: {airfoileditor_exe}")

    readme_pdf = repo_root / "README.pdf"
    if readme_pdf.exists():
        shutil.copy2(readme_pdf, pyinstaller_out / "README.pdf")

    _remove_dir_if_exists(final_out)

    _replace_dir_with_retry(pyinstaller_out, final_out)

    print()
    print("------ Finished successfully")
    print(f"Created: {final_out}")


def build_installer(repo_root: Path) -> None:
    package_name, package_version = _project_metadata(repo_root)

    win_exe_dir = f"{package_name}-{package_version}_win_exe"
    installer_name = f"{package_name}-{package_version}_win_setup.exe"

    bundle_dir = repo_root / "dist" / win_exe_dir
    pc2_exe = bundle_dir / f"{APP_NAME}.exe"
    ae_exe = bundle_dir / "AirfoilEditor.exe"

    print()
    print("------ Create Windows installer")
    print(f"App             : {APP_NAME}")
    print(f"Package name    : {package_name}")
    print(f"Package version : {package_version}")
    print(f"Bundle folder   : {bundle_dir}")
    print(f"Installer name  : {installer_name}")

    if not pc2_exe.exists():
        raise FileNotFoundError(f"PyInstaller output not found: {pc2_exe}")

    if not ae_exe.exists():
        raise FileNotFoundError(f"AirfoilEditor executable not found: {ae_exe}")

    if shutil.which("makensis") is None and shutil.which("makensis.exe") is None:
        raise FileNotFoundError("makensis.exe not found in PATH")

    _run(
        [
            "makensis.exe",
            "/V3",
            f"/DVERSION={package_version}",
            f"/DAPP_NAME={APP_NAME}",
            f"/DPACKAGE_NAME={package_name}",
            f"/DWIN_EXE_DIR={win_exe_dir}",
            f"/DDESCRIPTION={DESCRIPTION}",
            f"/DICON_NAME={PC2_ICON_NAME}",
            f"/DINSTALLER_NAME={installer_name}",
            "dev\\win_installer.nsi",
        ],
        cwd=repo_root,
    )

    print()
    print("------ Installer finished successfully")
    print(f"Created: {repo_root / 'dist' / installer_name}")


def _repo_root_from_script() -> Path:
    return Path(__file__).resolve().parents[1]


def main() -> int:
    parser = argparse.ArgumentParser(description="Windows build helper for PlanformCreator2")
    parser.add_argument(
        "target",
        choices=["check", "exe", "installer", "all"],
        help="Build target to run",
    )

    args = parser.parse_args()
    repo_root = _repo_root_from_script()

    if not (repo_root / "pyproject.toml").exists():
        print(f"ERROR: pyproject.toml not found in {repo_root}")
        return 2

    try:
        _, package_version = _project_metadata(repo_root)
        _ensure_airfoileditor_version(package_version)

        if args.target == "check":
            return 0

        if args.target in ("exe", "all"):
            build_exe(repo_root)

        if args.target in ("installer", "all"):
            build_installer(repo_root)

    except Exception as exc:
        print()
        print(f"ERROR: {exc}")
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
