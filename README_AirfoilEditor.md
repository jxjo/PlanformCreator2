# Airfoil Editor



## Airfoil Editor

The `AirfoilEditor` is part of PLanformCreator2 and shows more detailed information of an airfoil like thickness and camber distribution or curvature of the surface. 

![PC2](images/AirfoilEditor_App.png "Screenshot of the AirfoilEditor ")


Operations like repaneling, normalizing and changing geometry parameters make extensive use of splines to achieve high precision results. 

![PC2](images/AirfoilEditor_Repanel.png "Screenshot of Repaneling within AirfoilEditor")


A new airfoil may be designed based on two Bezier curves for upper and lower side. The control points of the Bezier curve can be automatically adapted for a best fit to a seed airfoil. 

![PC2](images/AirfoilEditor_bezier.png "Screenshot of Bezier curve definition")


###  Install

A pre-build Windows-Exe is available in the releases section https://github.com/jxjo/PlanformCreator2/releases  

or 

Download python sources which can also be found there https://github.com/jxjo/PlanformCreator2/releases

or 

Clone the repository 

and 

Install 

```
pip3 install numpy
pip3 install matplotlib
pip3 install customtkinter
pip3 install termcolor
pip3 install colorama
pip3 install ezdxf
```

 
Have fun!
