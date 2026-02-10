![PC2](https://github.com/jxjo/PlanformCreator2/blob/main/images/PC2_logo.png?raw=true)

### Version 4.0.2

---

**PlanformCreator2** - short PC2 - is an app to design the planform of a wing focusing on model sailplanes. 

#### Planform Design 

* Define a planform based on a chord distribution and a reference line along wing span
* Import image of an existing wing as a background image for design
* Add wing sections with fixed position or relative chord length
* Define flaps hinge line and flaps 
* Export planform as dxf file for use in CAD

#### Airfoils and Polars
* Define airfoil at wing sections and edit its properties using the [AirfoilEditor](https://github.com/jxjo/AirfoilEditor)
* Generate blended airfoils for intermediate wing sections ('strak')
* View polars of the airfoil based on xfoil polar generation

#### Wing Analysis
* Auto paneling of the planform as preparation for aerodynamic calculation 
* VLM based lift calculation 
* Determine critical sections where cl_max of airfoils will be reached
* Export wing definition to Xflr5 and FLZ_vortex for further analysis


The app, developed in Python with the Qt UI framework, runs on Windows, Linux, and MacOS. 
Linux and MacOS users are required to compile Worker (polar generation) from the [Xoptfoil2](https://github.com/jxjo/Xoptfoil2) project.

Find more info about the **PlanformCreator2** on [Github](https://github.com/jxjo/PlanformCreator2).

---

Find Release Information in [CHANGELOG](https://github.com/jxjo/PlanformCreator2/blob/main/CHANGELOG.md).
