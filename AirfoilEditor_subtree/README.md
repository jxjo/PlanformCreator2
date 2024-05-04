# Airfoil Editor


The `AirfoilEditor` shows detailed information of an airfoil and allows to modify its main geometry parameters. 

![PC2](images/AirfoilEditor_App.png "Screenshot of the AirfoilEditor ")

Main features: 

* View an airfoil and browse through the airfoils of its subdirectory
* View the curvature of the airfoil surface
* Repanel and normalize the airfoil
* Modify the geometry parameters thickness, camber and their high points 
* Set trailing edge gap 
* Merge an airfoil with another airfoil 
* Create a Bezier based 'copy' of an airfoil 

The driver for this app was to overcome some of the artefacts using xfoils geometry routines (for example used in Xflr5) when trying to create geometric 'high quality' airfoils. The focus of the app is on pure geometry work with airfoils. 

## Basic concepts

The `AirfoilEditor` implements different "strategies" to represent the geometry of an airfoil:

- 'Linear interpolation' -  Based on the point coordinates coming from the airfoils '.dat' file, intermediate points are evaluated with a simple linear interpolation. This is used for fast preview and basic operations.
- 'Cubic spline interpolation' - A cubic spline is built based on the point coordinates coming from the airfoils '.dat' file. The spline allows to evlauate intermediate points with high precision 
nelder mead
- 'Bezier curve' - An airfoil is represented by two Bezier curves for upper and lower side of the airfoil. A nelder mead optimization allows to approximate the Bezier curves to an existing airfoil.
- 'Hicks Henne' - Hicks Henne bump functions are applied to a "seed airfoil" to achieve a new shape (in development for Xoptfoil2)  

The spline interpolation is used to find the position of the 'real' leading edge, which may differ from the leading edge of the coordinates (which is the point with the smallest x-value). When 'normalizing' the airfoil, the 'real' leading edge is taken in an iteration to rotate, stretch and move the airfoil to become 0,0 - 1,0 normalized.

For thickness and camber geometry operations the airfoil (spline) is splitted into two new splines representing thickness and camber distribution. For moving the highpoint of either thickness or camber a mapping spline for the airfoil coordinates is used quite similar to the approach implemented in xfoil. After these operations the airfoil is rebuild out of thickness and camber. 

Repaneling is based on a modified cosinus distribution of the airfoil points on the arc of the spline. This differs from the xfoil approach but the repanel shows are 'nice' behaviour in aero calculation. 

As an exmaple for the modification functionality of the app, the dialog for repaneling is shown:  

![PC2](images/AirfoilEditor_Repanel.png "Screenshot of Repaneling within AirfoilEditor")
<sup>Dialog for repaneling of an airfoil. Recommendations are given for 'healthy' panel angles.  </sup>
</p>

### Curvature 

On of the major views on an airfoil in the Airfoil Editor is the curvature of the airfoils surface. It allows a quick assessment of the surface quality and to detect artefacts like a 'spoiler' at the trailing edge which is quite common. 

As the curvature changes from very high values at the leading edge to very low values towards the trailing edge, a logarithmic scale can be applied in the diagram to improve overview.  


## Bezier based airfoils 

Beside '.dat'-files the Airfoil Editor seamlessly displays '.bez'-Files defining an Bezier based airfoil. 

![PC2](images/AirfoilEditor_bezier1.png "Screenshot of Bezier curve definition")

A '.bez'-file defines the x,y coordinates of the Bezier control points and looks like: 
```
JX-GT-15
Top Start
 0.0000000000  0.0000000000
 0.0000000000  0.0120189628
 0.0681109425  0.1240586151
 0.6435307964  0.0748001854
 1.0000000000  0.0000000000
Top End
Bottom Start
 0.0000000000  0.0000000000
 0.0000000000 -0.0222920000
 0.3333333333 -0.0240468000
 1.0000000000  0.0000000000
Bottom End
````


A little bit hidden is the feature to define a (new) airfoil based on two Bezier curves for upper and lower side. The Bezier editor allows to move the control points of the curve by mouse.

The 'Match' function performs a best match of the Bezier curve to an existing airfoil. For this a simplex optimization (Nelder Mead) is performed to 
- minimize the norm2 deviation between the Bezier curve and the target airfoil
- align the curvature of the Bezier curve at leading and trailing to the targets curvature.  


![PC2](images/AirfoilEditor_bezier.png "Screenshot of Bezier curve definition")
<sup>Dialog for Bezier curve approximation. In this example the upper Bezier curve having 6 control points was 'matched' to the target airfoil at 4 controil points (Leading andtrailing edge are fixed). </sup>


## Hicks-Henne based airfoils 

Hicks-Henne “bump” functions are applied to a base aerofoil and add a linear combination of single-signed sine functions to deform its upper and lower surfaces to create a new aerofoil shape.
They are used in the airfoil optimizer Xoptfoil2 as an alternative to Bezier curves to create new airfoil designs. 

The Airfoil Editor allows to visualize the Hicks-Henne functions which were applied to an airfoil. For this a special file format '.hicks' is used to interchange with Xoptfoil2.

![PC2](images/AirfoilEditor_Hicks-Henne.png "Screenshot of Hicks-Henne based airfoil")
<sup>Visualization of the Hicks-Henne bump functions, which were applied to the upper and lower side of the airfoil</sup>

##  Install

The 'Airfoil Editor' is part of the 'Planform Creator 2' repository. 
A pre-build Windows-Exe of both apps is available in the releases section https://github.com/jxjo/PlanformCreator2/releases  

or 

Download python sources from https://github.com/jxjo/PlanformCreator2/releases or Clone the repository 

and Install 

```
pip3 install numpy
pip3 install matplotlib
pip3 install customtkinter
pip3 install termcolor
pip3 install colorama
pip3 install ezdxf
```

> [!TIP]
 For Windows: Use the "Open with ..." Explorer command to connect the 'AirfoilEditor.exe' to file extension '.dat'. Later a double click on an airfoil .dat-file will open the AirfoilEditor and you can browse through the files in the directory (if you are using the Python version, create a little batch job to open the .dat file)  


Have fun!
