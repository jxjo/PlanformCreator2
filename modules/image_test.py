"""
"""

import os
import sys
from pathlib import Path

import numpy as np
import pyqtgraph as pg
from PyQt6.QtGui            import QColor, QTransform, QImage
from PyQt6.QtCore           import QRectF


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

l.addLabel("Test background image", col=0, size=f"{14}pt")
l.nextRow()


# -------------------------------------------------------------------

norm_chord = Norm_Chord_Bezier()
xn, cn = norm_chord.polyline()

norm_planform = Norm_Planform(norm_chord)
planform = Planform_2 (norm_planform, chord_root=200, span=1300, sweep_angle=0)


# -------------------------------------------------------------------

win.nextRow ()

# --- scale and flip and move

pi4 = l.addPlot(title="scale xy, flip y, move y ")
pi4.getAxis ('left').setWidth (30)
# pi4.getViewBox().setRange (xRange=( 0,900), padding=0.1)
viewBox = pi4.getViewBox()
viewBox.invertY(True)
viewBox.setAspectLocked()

x, le_y, te_y = planform.le_te_polyline ()
box_x, box_y  = planform.box_polygon ()
ref_x, ref_y  = planform.ref_polyline ()


item = pi4.plot(x , le_y, pen='red')
item.setZValue (10)
pi4.plot(x , te_y, pen='yellow')
pi4.plot(ref_x, ref_y, pen='springgreen')
pi4.plot(box_x, box_y, pen='blue')

# ------ image --------------------

## Create image item

# img = pg.ImageItem(np.full((200, 200), 200))

qimage = QImage()
qimage.load ("..\images\DXF_export.png")

qimage.invertPixels ()
size = qimage.size()

img_arr = pg.functions.imageToArray (qimage)
img = pg.ImageItem(img_arr)
# Example: Transformed display of ImageItem
# tr = QTransform()  # prepare ImageItem transformation:
# tr.scale(4.0,4.0)       # scale horizontal and vertical axes
# # tr.translate(-1.5, -1.5) # move 3x3 image to locate center at axis origin
# img.setTransform(tr) # assign transform


img.setZValue (-100)
img.setLevels([10, 160])
# img = pg.ImageItem(np.zeros((200,200)))
viewBox.addItem(img)

## Set initial view bounds
# viewBox.setRange(QRectF(0, 0, 200, 200))


if __name__ == '__main__':
    pg.exec()
