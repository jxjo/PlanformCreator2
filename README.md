# Planeform Creator 2

-- Work in Progress --

An app to design a wing focusing on model sailplanes 

* Define the planform based on elliptical chord distribution functions
* Major parameter is the hinge line of the wing
* Add an arbitrary number of wing sections with fixed position or relative chord length
* Generate blended airfoils for intermediate wing sections ('strak')
* Import dxf outline of a wing either as template for a new wing or as reference for an existing wing
* Export wing definition to Xflr5 
* Export wing definition to FLZ_vortex (to come)

Inspired and partially based on the 'Planeform Creator' being part of [The Strak Machine](https://github.com/Matthias231/The-Strak-Machine) - Thanks Matthias!  



![PC2](images/Screenshot.png "First screenshot")


![PC2](images/Screenshot_Xflr5-Export.png "Screenshot of Xflr5 Export")

###  Install

```
pip3 install numpy
pip3 install matplotlib
pip3 install customtkinter
pip3 install termcolor
pip3 install colorama
pip3 install ezdxf
```


Have fun!