![PC2](https://github.com/jxjo/PlanformCreator2/blob/main/images/PC2_logo.png?raw=true)

# v4.3.2

**PlanformCreator2** (PC2) is an app for designing wing planforms, with a focus on model planes.

#### Planform Design

* Define a planform based on a chord distribution and a reference line along wing span
* Import image of an existing wing as a background image for design
* Add wing sections with fixed position or relative chord length
* Define flaps hinge line and flaps
* Export planform as DXF for use in CAD

#### Airfoils and Polars

* Assign airfoils to wing sections and edit them using the [AirfoilEditor](https://github.com/jxjo/AirfoilEditor)
* Generate blended airfoils for intermediate wing sections ('strak')
* View polars of the airfoil based on xfoil polar generation

#### Wing Analysis

* Auto paneling of the planform as preparation for aerodynamic calculation
* VLM based lift calculation
* Determine critical sections where cl_max of airfoils will be reached
* Export wing definition to Xflr5 and FLZ_vortex for further analysis

## Quick Start

* Python package: `pip install planformcreator2`, then run `planformcreator2`.
* Linux/macOS note: for polar generation and full analysis, compile and install `worker` (see [Xoptfoil2 installation](https://github.com/jxjo/Xoptfoil2#Installation)).


More information is available in the project repository: [PlanformCreator2 on GitHub](https://github.com/jxjo/PlanformCreator2).

Release information: [CHANGELOG](https://github.com/jxjo/PlanformCreator2/blob/main/CHANGELOG.md).
