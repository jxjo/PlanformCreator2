## Creating a new release 

A short guide for developers how to create a new release

### Bump Version 
- Manually set `__version__` in app.py. This will be read by Hatch ...

### Test package and win_exe generation

- Run `dev/package_build.bat` to create package wheel
  - Ensure pytest is not running with argument `-m "not slow"` to include long running tests
  - Check name of wheel in directory `./dist`
  - Local installation of the package in a new virtual environment  
    ```
    python -m venv .venv
    .venv\scripts\activate
    pip install <local_wheel_in_dist>
    .venv\scripts\deactivate
    ```

- Run `win_build_exe.bat` to create Windows -onedir exe 
  - Ensure pyinstaller option `--noconsole` is set to have no console window during runtime
  - Check Pyinstaller -onedir result in `./dist`

- Run `win_build_installer.bat` to build Windows installer
  - Run ...setup.exe and install app on Desktop 
  - Uninstall app 

### Finishing Development

- Check still any `logger.setLevel(logging.DEBUG)` active (not commented out)
- Last look on 'Spell Checker' output to fix typos

### Preparing Sources

- Final check `__version__` in app.py
- Update `README.md` - check version, text changes, images still actual, dependencies
- Update `README_PyPi.md` - check version, text changes, images still actual, dependencies
- Create `README.pdf` for Windows exe distribution using 'Markdown PDF: Export' extension
- Update `CHANGELOG.md` 
- Final commit and push of dev branch

---

### Merging dev into main on GitHub

- 'Compare and pull request'
- 'Squash and merge' 
- Delete dev branch

### Update Local Dev

- Switch to main 
- Pull main
- Check

--- 

### Package and win_exe generation

- Run `dev/package_build.bat` to create package wheel using Hatch 
  - Ensure pytest is not running with argument `-m "not slow"` to include long running tests
  - Check name of wheel in directory `./dist`

- Run `win_build_exe.bat` to create Windows -onedir exe 
  - Ensure pyinstaller option `--noconsole` is set to have no console window during runtime
  - Check Pyinstaller -onedir result in `./dist`

- Run `win_build_installer.bat` to build Windows installer
  - Run ...setup.exe and install app on Desktop 
  - Uninstall app 

### Package upload to Pypi 

- Run `dev/package_to_PyPI.bat` to upload package wheel to PyPi using Hatch
  - Ensure pytest is not running with argument `-m "not slow"` to include long running tests
  - Check https://pypi.org/project/airfoileditor/ that new version is available

### New Release on Github

- 'Draft a new release' - version tag like 'v4.2.0' - target 'main'
- Copy release information from CHANGELOG.md
- Copy ...win_setup.exe to GitHub attachments 
- 'Set as the latest release'
- 'Publish'

---

### Release information on RCN

---
Done!
