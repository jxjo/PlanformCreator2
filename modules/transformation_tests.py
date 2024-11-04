"""
"""

import os
import sys
from pathlib import Path
import numpy as np

import pyqtgraph as pg
from pyqtgraph.Qt import QtCore

sys.path.append(Path(__file__).parent)
sys.path.insert (1,os.path.join('..' , 'AirfoilEditor_subtree/modules'))
# sys.path.insert (1,os.path.join(Path(__file__).parent , 'AirfoilEditor_subtree/modules'))
from base.spline    import Bezier
from wing           import Norm_Chord_Bezier, Norm_Planform, Planform2

app = pg.mkQApp("Plotting Example")
win  = pg.GraphicsLayoutWidget(show=True, title="Basic plotting examples")
win.resize(1200,600)
l = win.ci
l.setContentsMargins (10,10,30,20)  # default margins
l.layout.setVerticalSpacing (30)
l.layout.setHorizontalSpacing (30)
pg.setConfigOptions(antialias=True)

l.addLabel("Making of a Planform", col=1, size=f"{14}pt")
l.nextRow()


# -------------------------------------------------------------------

# Put vertical label on left side
l.addLabel('Normed System', angle=-90,  size=f"{12}pt")


# --- chord distribution 

pi1  = l.addPlot(title="chord distribution")
pi1.getAxis ('left').setWidth (30)
pi1.getViewBox().setRange (xRange=( 0,1), yRange=( 0,1), padding=0.1)

norm_chord = Norm_Chord_Bezier()
xn, cn = norm_chord.polyline()

pi1.plot (xn,cn) 


# reference bezier  

pi2  = l.addPlot(title="chord reference ")
pi2.getViewBox().setRange (xRange=( 0,1), yRange=( 0,1), padding=0.1)
pi2.getAxis ('left').setWidth (30)

norm_planform = Norm_Planform(norm_chord)
rxn, rn = norm_planform.rn_polyline()

pi2.plot (rxn,rn, pen='green') 


# --- planform reference applied  

pi3 = l.addPlot(title="reference applied")
pi1.getViewBox().setRange (xRange=( 0,1), yRange=( 0,1), padding=0.1)
pi3.getAxis ('left').setWidth (30)


# planform 
xn, le_yn, te_yn = norm_planform.le_te_polyline()

# ref line
r_xn, r_yn = norm_planform.ref_polyline()


pi3.plot (r_xn,r_yn, pen='green') 
pi3.plot (xn,le_yn, pen='red') 
pi3.plot (xn,te_yn, pen='yellow') 

# -------------------------------------------------------------------

win.nextRow ()

# Put vertical label on left side
l.addLabel('Wing System', angle=-90, size=f"{12}pt")

# --- scale and flip and move

pi4 = l.addPlot(title="scale xy, flip y, move y ")
pi4.getAxis ('left').setWidth (30)
# pi4.getViewBox().setRange (xRange=( 0,900), padding=0.1)
pi4.getViewBox().invertY(True)
pi4.getViewBox().setAspectLocked()

planform = Planform2 (norm_planform, chord_root=200, span=800, sweep_angle=0)

x, le_y, te_y = planform.le_te_polyline ()
# scale 
chord = 200
span  = 800

x     = xn    * span
y_le  = le_yn * chord
y_te  = te_yn * chord
ref_x = r_xn  * span
ref_y = r_yn  * chord

# flip and move
flip_y = -1
move_y = y_le[0]
y_le  = y_le  * flip_y + move_y
y_te  = y_te  * flip_y + move_y
ref_y = ref_y * flip_y + move_y

# bounding box 
box_x = np.array([0.0, 0.0,   span,  span, 0.0])
box_y = np.array([0.0, chord, chord, 0.0,  0.0])

pi4.plot(x , le_y, pen='red')
pi4.plot(x , te_y, pen='yellow')
pi4.plot(ref_x, ref_y, pen='green')
pi4.plot(box_x, box_y, pen='blue')


# --- shear  

angle = 5

pi6 = l.addPlot(title=f"shear by angle {angle}° ")
pi6.getViewBox().setAspectLocked()
pi6.getViewBox().invertY(True)
# pi6.getViewBox().setRange (xRange=( 0,900), padding=0.1)
pi6.getAxis ('left').setWidth (30)

# do shear 

shear_factor =  1 / np.tan((90-angle) * np.pi / 180)    #cotangens

y_le  = y_le    + x * shear_factor  
y_te  = y_te    + x * shear_factor
ref_y = ref_y   + ref_x * shear_factor
box_y = box_y   + box_x * shear_factor

# calc sweep angle at 25%

pi6.plot(x , y_le, pen='red')
pi6.plot(x , y_te, pen='yellow')
pi6.plot(ref_x, ref_y, pen='green')
pi6.plot(box_x, box_y, pen='blue')


if __name__ == '__main__':
    pg.exec()
