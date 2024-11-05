"""
"""

import os
import sys
from pathlib import Path

import pyqtgraph as pg
from PyQt6.QtGui                import QColor

sys.path.append(Path(__file__).parent)
sys.path.insert (1,os.path.join('..' , 'AirfoilEditor_subtree/modules'))
from base.spline    import Bezier
from wing           import Norm_Chord_Bezier, Norm_Planform, Planform_2



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

pi2 = l.addPlot(title="chord reference ")
pi2.getViewBox().setRange (xRange=( 0,1), yRange=( 0,1), padding=0.1)
pi2.getAxis ('left').setWidth (30)

norm_planform = Norm_Planform(norm_chord)
rxn, rn = norm_planform.rn_polyline()

le_x    = [0.0, 1.0]
le_y    = [0.0, 0.0]

te_x    = [0.0, 1.0]
te_y    = [1.0, 1.0]

ref_item = pi2.plot (rxn,rn, pen=pg.mkPen(color='springgreen', width=1.5)) 
le_item  = pi2.plot (le_x,le_y, pen='red', antialias=False) 
te_item = pi2.plot (te_x,te_y, pen='yellow', antialias=False) 

# fill area between ref and le, te 
brush = pg.mkBrush (QColor('red').darker(800))
pi2.addItem (pg.FillBetweenItem (ref_item, le_item, brush=brush ))

brush = pg.mkBrush (QColor('yellow').darker(800))
pi2.addItem (pg.FillBetweenItem (ref_item, te_item, brush=brush ))


# --- planform reference applied  

pi3 = l.addPlot(title="reference applied")
pi1.getViewBox().setRange (xRange=( 0,1), yRange=( 0,1), padding=0.1)
pi3.getAxis ('left').setWidth (30)


# planform 
xn, le_yn, te_yn = norm_planform.le_te_polyline()

# ref line
r_xn, r_yn = norm_planform.ref_polyline()

# box
box_xn, box_yn = norm_planform.box_polygon ()


ref_item = pi3.plot (r_xn,r_yn, pen='springgreen') 
le_item  = pi3.plot (xn,le_yn, pen='red') 
te_item  = pi3.plot (xn,te_yn, pen='yellow') 
pi3.plot (box_xn, box_yn, pen='blue')

# fill area between ref and le, te 
brush = pg.mkBrush (QColor('red').darker(800))
pi3.addItem (pg.FillBetweenItem (ref_item, le_item, brush=brush ))

brush = pg.mkBrush (QColor('yellow').darker(800))
pi3.addItem (pg.FillBetweenItem (ref_item, te_item, brush=brush ))

# wing sections
sections = norm_planform.wingSections
for section in sections:

    cn = section.cn() 
    xn = [section.xn(), section.xn()]
    yn = [0.0, cn]
    pi1.plot (xn, yn, pen='deeppink')


    le_yn, te_yn = section.le_te ()
    xn = [section.xn(), section.xn()]
    yn = [le_yn, te_yn]
    pi3.plot (xn, yn, pen='deeppink')




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

planform = Planform_2 (norm_planform, chord_root=200, span=800, sweep_angle=0)

x, le_y, te_y = planform.le_te_polyline ()
box_x, box_y  = planform.box_polygon ()
ref_x, ref_y  = planform.ref_polyline ()


pi4.plot(x , le_y, pen='red')
pi4.plot(x , te_y, pen='yellow')
pi4.plot(ref_x, ref_y, pen='springgreen')
pi4.plot(box_x, box_y, pen='blue')


# --- shear  

planform = Planform_2.default()
angle = 5

pi6 = l.addPlot(title=f"shear by angle {angle}° ")
pi6.getViewBox().setAspectLocked()
pi6.getViewBox().invertY(True)
# pi6.getViewBox().setRange (xRange=( 0,900), padding=0.1)
pi6.getAxis ('left').setWidth (30)

planform.set_sweep_agnle (10)

x, le_y, te_y = planform.le_te_polyline ()
box_x, box_y  = planform.box_polygon ()
ref_x, ref_y  = planform.ref_polyline ()

pi6.plot(x , le_y, pen='red')
pi6.plot(x , te_y, pen='yellow')
pi6.plot(ref_x, ref_y, pen='springgreen')
pi6.plot(box_x, box_y, pen='blue')

# wing sections
# sections = planform.wingSections ()
for section in planform.wingSections ():
    pi6.plot (*section.polyline(), pen='deeppink')



if __name__ == '__main__':
    pg.exec()
