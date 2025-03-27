
![PC2](images/PC2_logo.png "Logo")


# v3.1.1

**PlanformCreator2** - short PC2 - is an app to design the planform of a wing focusing on model sailplanes. 

#### Planform Design 

* Define a planform based on a chord distribution and a reference line along wing span
* Import image of an existing wing as a background image for design
* Add wing sections with fixed position or relative chord length
* Define flaps hinge line and flaps 
* Export planform as dxf file for use in CAD

#### Airfoils and Polars
* Define airfoil at wing sections and edit its properties using the [Airfoil Editor](https://github.com/jxjo/AirfoilEditor)
* Generate blended airfoils for intermediate wing sections ('strak')
* :new:  v3: View polars of the airfoil based on xfoil polar generation

#### Wing Analysis
* Auto panelling of the planform as preparation for aerodynamic calculation 
* :new:  v3: VLM based lift calculation 
* :new:  v3: Determine critical sections where cl_max of airfoils will be reached
* Export wing definition to Xflr5 and FLZ_vortex for further analysis

The project was inspired by the 'Planform Creator' being part of [The Strak Machine](https://github.com/Matthias231/The-Strak-Machine).

## Basic concepts

The idea behind **PlanformCreator2** is to have a tool to design a new wing with a more graphical, 'playful' approach. It tries to fill the gap between CAD based design and aerodynamic analysis tools like Xflr5.

<img src="images/PC2_usage.png" width="800" />

Within the early design process of a wing **PlanformCreator2** is the master of 'truth'. It provides the data for the aerodynamic analysis. A first lift and lift failure analysis of the wing is part of PC2. 
Further analysis and performance assessments are made with external apps. 


When the results are satisfying the planform and the envolved airfoils are exported as dxf to a CAD program to continue with the final 3D design e.g. for a mold of the wing. 

In contrast to a "paint program", the planform is defined by parameters such as 'span', 'root depth' or "sweep angle". The chord distribution along the span is controlled by a mathematical function via parameters.
The advantage is that the parameters can be changed independently of each other, allowing a quick approach to the desired wing planform.



## Designing a planform 

'Form follows Function' - this is especially true in PC2: The planform of a wing results from a combination of functions and parameters. 

### Chord distribution and Chord Reference
Most important and usually defined first, is the chord distribution along the wing span. The geometric chord distribution directly and significantly determines the lift distribution along the span. The local lift coefficient depends on the local effective angle of attack (influenced by the wing shape) and on the local Reynolds number, which changes proportionally to the local chord.

Two different "types" are available for defining the chord distribution:
- **Bezier curve**: Using a start tangent at the root and a end tangent at the wing tip, the curve of the chord distribution is defined by dragging the control points with the mouse.<br>
:new:  v3: Additional Bezier control points may be defined to achieve a wide range of chord distributions<br>
:new:  v3: An area with constant chord can be defined by moving the start point of Bezier towards tip
- **Trapezoid**: For the definition of a (multiple) trapezoid wing. The chord is defined by the chord length of the individual wing sections.

In PC2, the chord distribution is always displayed in normalized form. Both span and chord length range from 0 to 1. This allows chord distributions - even of different wing designs - to be compared with each other without distortion.

The second main definition is the 'Chord Reference' which defines how much of the chord is assigned towards leading and trailing edge along a virtual 'Chord Reference Line'.  

<img src="images/chord_distribution_reference.png" width="800" />

By combining these two functions, scaling the result to a halfwing span and optionally shearing the result by a 'Sweep Angle' the final planform is created

<img src="images/planform_by_chord.png" width="800" />


#### Variations of the Chord Reference 

The concept of "Chord Reference" is quite powerful to create variations of a planform. As the chord distribution is constant, all variations will have the same area and almost identical aerodynamic properties in first approximation. 

The following diagram shows the impact of different chord references while keeping the chord distribution constant.
 
<img src="images/planform_variations.png" width="800" />



#### Everything banana?
A little speciality is the so called "banana function". It allows to bent the wing in spanwise direction like a banana without changing chord distribution and reference. The result can be a planform like the popular 'bow-style' flying wings. 

Another use case for applying the 'banana function' is to finetune the flap depth alogn the wing span. As the flap hinge line (see further below) is a straight line, benting the planform will directly influence the flap depth. 

<img src="./images/planform_banana.png" width="800" />

### Background image

If you want to recreate an wing based on an existing image, you may load this image as a background image into the app. Having a planform as contour line in the background, it is - most of the times - straight forward to define the chord distribution and the chord reference for a best fit.

There is a little image editor to scale the image and adapt the colors to a 'dark mode' of the image which is best suited as a background. 

Scaling of the image is done by moving the two scale markers to the leading edge of the root and to the very tip point.  

<img src="images/background_image.png" width="800" />



### Wing sections

Once the planform has been defined, another artifact comes into play: 'wing sections'. A wing section has two main tasks:
* Define the position of an airfoil within a planform
* Define the position of the flap hinge line and the start of a 'flap group'.

A wing always has at least 2 wing sections - a root and a tip section. In between, any number of wing sections can be created. There are two modes how the location of a wing section can be defined: 
- by span position: The wing section always remains in this relative span position, even if the wing span or chord distribution is changed. Positioning by span position is helpful for example to define flaps.
- chord length: The wing section is tied to a certain relative chord. When changing the chord distribution, the wing section will move around in order to always have the same chord length. This is especially useful to have an airfoil at a certain chord length (= Reynolds number) within the wing.

<img src="images/wing_sections.png" width="800" />

<sup>In this example, wing section '1' at a fixed position is used to define the width of flap and aileron, while section '2' is the home of airfoil 'JX-GP-055' ensuring a certain chord. </sup>

A special case arises with trapezoidal planforms. Here wing sections are also used to define the chord distribution by having both: a fixed chord and a fixed span position. 

<img src="images/wing_sections_trapezoidal.png" width="800" />

<sup>In this trapezoidal planform, wing sections 'root', '1' and 'tip' define the planform. Wing section '2' is just a helper section to have an additional flap at this position. </sup>



### Hinge Line and Flap Depth

Flaps are defined by their 'hinge line' and their bounding wing sections to the left and to the right. A wing section can have a 'flap group' assigned. All subsequent wing sections with the same flap group will define a single flap.

In many cases the hinge line equals the 'reference line' of the planform. This is also the default case when defining flaps. 

More flexibility is achieved through a separate definition of the hinge line with a hinge point at the root section and the tip section. 
In certain cases, for example with a curved reference line, kinks in the hinge line can be defined at a wing section.

<img src="images/flap_hinge_line.png" width="500" />

### Export as dxf file 
Once the wing design has been completed, the new wing can be transferred to a CAD program as a dxf file. The used airfoils are optionally inserted into the drawing and / or additionally exported as a .dat file.

A nice feature is to define a common trailing edge thickness (trailing edge gap) in mm for all airfoils involved.
So no manual post processing for the airfoils has to be done in CAD. 

<img src="images/dxf_view.png" width="800" />


The generated planform shouldn't be used directly for a 3D construction in the CAD program, since the contour line is approximated by many small straight line pieces (polyline). It is recommended to place a spline over the leading and trailing edge in the respective CAD program.


## Airfoils and Polars

### Assignment of Airfoils

A wing section can either be assigned a fixed airfoil or the airfoil can be left open. In this case, a 'strak' (or 'blended') airfoil is created for this wing section by blending the left and right neighboring airfoils. The 'blending ratio' is derived from the ratio of the respective chord length of the wing sections.

For the root and the tip section an airfoil is mandatory. Initially example airfoils are assigned to these two sections.

<img src="images/airfoil_names.png" width="800" />

<sup>Wing sections with the corresponding airfoils. The airfoil at 'section 2' is created by blending JX-GP-100 and JX-GP-055.</sup>


The airfoils can either be viewed in normal, normed scale or in their 'real' scale within their wing sections.

Included in the **PlanformCreator2** is the  [AirfoilEditor](https://github.com/jxjo/AirfoilEditor) which allows the typical modifications of an airfoil during wing design. 

In the final stage of the design process the airfoils, including the generated 'straked' airfoils, can be exported as a .dat file. The optional setting of a continuous trailing edge thickness ('Te gap') in mm is very practical and makes the often necessary manual reworking of the airfoils in CAD superfluous.


### Airfoil Polars

To generate the polars of an airfoil the **PlanformCreator2** uses the Worker tool of the [Xoptfoil2 project](https://jxjo.github.io/Xoptfoil2). One of the Worker actions is the multi-threaded creation of a polar set using Xfoil.

Within the app a polar is generated 'lazy' - meaning at the moment when the polar should be displayed - and asynchronous in a background task. Each polar is stored in a single file having the Xfoil polar format allowing to import the polar into Xflr5. 

For polar generation the `auto_range` feature of the Worker is applied which optimizes the alpha range of the polar to show the complete T1 polar from `cl_min` to `cl_max` of the airfoil. 

A polar of a specific Re number is defined for the root wing section. For the other wing sections the Re number is automatically adjusted based on the local chord. Afterwards the polars of all wing sections will be generated as one set.

![Polars](images/airfoil_polars.png "Screenshot of Polar Generation")
<sub>Airfoil and Polar view with the auto generated, Re number adjusted polars of the airfoils at the defined wing sections. </sub>

The polars along wingspan can be helpful to have a first assessment of the airfoil ‘strak’ especially towards tip with its lower Re numbers.

## Wing Analysis

### Panel Generation

The basis for an aerodynamic analysis of a wing is the idealization with panels.
PC2 supports this paneling step with some helpful features. 

The paneled planform can either be exported to Xflr5 and FLZ_vortex or used for the integrated aero analysis, as described below.

The first step for paneling is the definition of the number of x and y-panels of the sections. In the case of a curved leading or trailing edge this can lead to considerable geometrical deviations between the original contour and the idealized which will lead to high inaccuracies of the aerodynamic calculation.


<img src="./images/panelling_step_1.png" width="800" />

<sup>Initial panelling of a planform having a single wing section '1' between root and tip. The deviation between the original contour and the trapezoidal contour, marked with yellow lines, is way to high to achieve valid results in aerodynamic calculation. </sup>


Next, a mesh optimization can be applied based on the parameters:

* Minimum panel width – the number of y panels per section will be adapted to achieve a uniform panel width along span
* Minimum chord deviation to planform – additional wing sections will be inserted automatically until the deviation of the section trapezoids to the original planform is below the defined threshold
* Minimum chord at tip – this cuts the tip in such a way that a Re number is achieved that leads to meaningful Xfoil results.

![PC2](images/panelling_steps.png "Panelling steps")

<sup>Auto-optimized panelling to reduce deviation between original and idealized contour. The maximum deviation of chord is now below 2% and good enough to achieve reasonable results.</sup>

As for airfoil polar generation, panel generation is ‘lazy’: A new mesh is initialized each time the planform is changed by the user.


#### Export to Xflr5

When using PC2 together with Xlfr5 the major faciliation beside the definition of wing segements, is the automatic generation of all intermediate airfoils needed in Xflr5 at all wing sections. No further geometric work has to be done for wing definition and airfoils. 

Please read [the short description](doc/PC2_export_to_Xflr5.md) of how it works.


#### Export to FLZ_vortex


PC2 generates a ready to use FLZ_vortex project file which can be loaded as a new 'flight scene' ('Flugszene') or FLZ_vortex can be launched directly from PC2 having the project file loaded. For direct launch the file extension .flz must be assigned to the app FLZ_vortex. 

Do not forget to adjust the plane mass before running a calculation.

### VLM Analysis

PC2 has an integrated VLM (Vortex lattice method) module to calculate the lift distribution along wing span based on the previously carried paneling of the wing.

<img src="./images/vlm_cp_panel.png" width="800" />

The main objective of the aero analysis in PC2 is to find the region of the wing, which will fail when the angle of attack is increased towards stall. For this assessment the 2D airfoil polar data and the 3D lift aero data is combined in a 'viscous loop'.

The 2D airfoil polar data is taken from the polar definitions as described in the section before. The airfoil polars are evaluated for each wing section. Note: For wing analysis only T1 polars (constant speed) are supported.

The 'viscous loop' calculates the effective Cl and Alpha for each panel station along wing span.

<img src="./images/vlm_viscous_loop.png" width="400" />

The maximum possible angle of attack (close to stall) is automatically evaluated and the critical area is marked within the paneling view and in the lift distribution view:

<img src="images/vlm_cl_max.png" width="800" />

<sup>Simple wing example showing the critical wing regions where local Cl reaches cl max of the airfoil.</sup>

By activating the chord distribution view, the chord and thus the area in which the wing will fail first can be changed interactively.

<img src="images/vlm_cl_max_with_chord.png" width="800" />

<sup>Interactive change in the chord distribution and thus the area in which the wing tends to stall first</sup>


## The App - a quick look 
  
An attempt was made to provide a simple, intuitive UI to be able to 'playfully' explore the program and the design of a new wing. 

![PC2](images/PC2_app.png "PlanformCreator2")

- The upper tabs are used to switch between the different main views. 
- Within the panel on the left the settings for current diagram can be customized. 

- At the bottom left are the usual file and action functions. 

- In the lower main panel the input or modification of the planform parameters takes place. The most important parameters can alternatively be changed directly in the diagram using the little mouse helper squares.

To get to know the app for the first time, take a look at the 'Welcome' panel. You'll find some further explanations and can start to modify the sample planform right away. The 'New' function shows a selection of predefined templates which can be a good starting point for your project.

In the 'examples' folder, you'll find some full flavored PC2 projects. Just 'Open' the respective PC2 file. 



## Software Aspects

`PlanformCreator2` is developed in  [Python](https://www.python.org/) using [PyQt6](https://pypi.org/project/PyQt6/) which wraps and extends the Qt UI framework and [PyQtGraph](https://www.pyqtgraph.org/) which wraps the QT Graphics framework. 

The main building blocks of the app are
* Model - containing all geometry and math helper routines to create and modify a wing planform. The model is independent of the UI.

* VLM - for calculation of the lift distribution the Python VLM implementation [Panel Aero](https://github.com/DLR-AE/PanelAero) is used as the core module - thanks to Arne Voß (DLR)   

* UI-Framework - base classes and a little framework to ease the implementation of forms based on `Widgets` and `Diagrams`.
Plots in `Diagrams` are handled by `Artists` where each of them viszualizes certain data aspects of a planform. The base classes are imported from the  [Airfoil Editor](https://github.com/jxjo/AirfoilEditor) project as a Git subtree

* Application - `App_Main` and view `Panels` to handle presentation and user interaction 


##  Installation

### Windows only

A pre-build Windows App is available in the [releases section](https://github.com/jxjo/PlanformCreator2/releases), which can be downloaded, unzipped and run.   

### Windows and Linux without a Python virtual environment

Make sure that you have installed the latest Python version. 

Download python sources from the [releases section](https://github.com/jxjo/PlanformCreator2/releases) or clone the repository. 

Install the additional modules needed:

```
pip3 install pyqt6
pip3 install numpy
pip3 install pyqtgraph 
pip3 install termcolor
pip3 install ezdxf
```
Run the app:

```
python3 PlanformCreator2.py
```

### Windows and Linux using a Python virtual environment

If you are are running other python applications on your PC, it is recommended to create a virtual environment for the PlanformCreator2 not to influence the other installations with the additional PC2 modules.

Make sure that you have installed the latest Python version.

Download python sources from the [releases section](https://github.com/jxjo/PlanformCreator2/releases) or clone the repository. 

Install the additional modules needed.

```
# change to the PC2 directory
python3 -m venv venv                        # create virtual environment
. venv/bin/activate                         # for Windows: 'venv\bin\activate'
pip3 install pyqt6
pip3 install numpy
pip3 install pyqtgraph 
pip3 install termcolor
pip3 install ezdxf
```
Run the app:

```
. venv/bin/activate                         # for Windows: venv\bin\activate  
python3 PlanformCreator2.py
. venv/bin/deactivate                       # for Windows: venv\bin\deactivate' 
```

---
Have fun! ;-)

jochen@jxjo.de
