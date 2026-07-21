# Changelog

All notable changes to this project will be documented in this file.

## 4.3.2

### Fixed 

- App: Closing via the window close button now handles unsaved changes the same way as the Exit button.
- Wing Analysis: Simple trapezoid planforms are now paneled correctly.
- Wing Analysis: VLM now distinguishes airfoil polars that share the same Re number but differ in additional properties such as flap setting.
- Airfoils: Polars near "Re excluded" no longer overwrite each other.
- Airfoils: Airfoil nicknames are now set and refreshed correctly.
- Planform: Background images now switch off correctly.
- Planform: After editing, the background image is shown again reliably.
- Planform: Background image file paths are now stored as relative paths when possible.
- Planform: Airfoil names are now shown when a wing section is added.
- AirfoilEditor: Opening AirfoilEditor now shows a hint that the app is disabled.

### Added

- Airfoils: Airfoil camber lines can now be shown optionally.

### Thanks

- Special thanks to Christian for his huge support in finding bugs and improving the app.




## 4.3.1

### Fixed

- Windows installer: AirfoilEditor.exe is now installed beside PlanformCreator2.exe, so AirfoilEditor can be opened from PC2.
- AirfoilEditor 4.3.1: Change signal in modeless dialogs was lost.


## 4.3.0

This release is primarily a port to AirfoilEditor v4.3.0, which includes major refactoring.

### Added

- DXF export:
  - Export Bezier-based planforms as B-splines (Thanks to Michael!)
  - Export Bezier-based airfoils as B-splines, and .dat-based airfoils as cubic splines


### Fixed

- Various minor bugs and usability issues


## 4.0.2

### Added

- VLM: use airfoil polars with forced transition for an improved alpha0, cl_max determination
- Based on AirfoilEditor 4.2.5

### Fixed

- VLM: sometimes airfoils were not straked
- VLM: revised cl_max determination with Xfoil outlier detection


## 4.0.1

This is just a maintenance release with a bunch of bug fixes.

### Added

- Export wing sections data as CSV file (Thanks to Thomas!)
- Switch airfoil polar diagrams  
- Show full or half wing  

### Fixed

- Fix several issues in 'Save as'
- Fix refresh of VLM results
- More minor fixes ...

### Changed

- Requires AirfoilEditor 4.2.3 having additional fixes 



## 4.0

Major release with extensive refactoring and improved installation options. 
The app is now available as a package on PyPI and can be installed via pip or a Windows installer.

### Added

- New planforms are created in User data directory
- More menu options including Save as, Rename, Delete temporary files
- New project airfoil directory '<project_name>_airfoils' which makes it easier to exchange projects with other users
- Maximize / minimize the lower data panel to achieve a larger diagram area
- Export to Xflr5 allows to set a flap angle at the exported airfoils 
- Export DXF with revised parameters
- Airfoil polar diagram variables can be changed directly in the polar diagram  
- Create new polars directly in VLM analysis

### More
- A lot of minor improvements and bug fixes


## 3.1.2

This is just a maintenance release 

### Fixed

- Blending Airfoil: In many cases only linear interpolation was used to calculate the new, straked airfoil. This led to inaccurate results.
- Handle not existing reference file
- VLM Analysis: Fix 'Polar not found' message
- Some more ...

## 3.1.1

### Added

* Airfoil polars along wing span
* VLM based calculation of lift distribution 
* Determination of the span position that fails when Cl_max is reached
* Bezier chord distribution with additional control points
* Reference line to be freeform Bezier (not just banana)


## 3.0 

### Added

* Airfoil polars along wing span
* VLM based calculation of lift distribution 
* Determination of the span position that fails when Cl_max is reached
* Bezier chord distribution with additional control points
* ... many minor enhancements 
