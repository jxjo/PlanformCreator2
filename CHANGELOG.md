# Changelog

All notable changes to this project will be documented in this file.


## 4.0.2

This is just a maintenance release with a bunch of bug fixes.

### Added

- VLM: use airfoil polars with forced transition to get better alpha0 of airfoil

### Fixed

- VLM: sometimes airfoils were not straked


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
