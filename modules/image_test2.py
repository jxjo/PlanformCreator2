"""
Display a non-uniform image.
This example displays 2-d data as an image with non-uniformly
distributed sample points.
"""

import numpy as np

import pyqtgraph as pg
from pyqtgraph.graphicsItems.NonUniformImage import NonUniformImage
from PyQt6.QtGui            import  QImage


app = pg.mkQApp("NonUniform Image Example")
pg.setConfigOptions(antialias=False)

win = pg.GraphicsLayoutWidget()
win.show()
win.resize(1000, 800)
win.setWindowTitle('pyqtgraph example: Non-uniform Image')

p = win.addPlot(title="Power Losses [W]", row=0, col=0)
hist = pg.HistogramLUTItem(orientation="horizontal",levelMode='mono')
viewBox = p.getViewBox()
viewBox.invertY(True)
viewBox.setAspectLocked()

# p.setMouseEnabled(x=False, y=False)

win.nextRow()
win.addItem(hist)

qimage = QImage()
qimage.load ("..\images\PC2_usecase.png")

qimage.invertPixels ()
size = qimage.size()

img_arr = pg.functions.imageToArray (qimage)
image = pg.ImageItem(img_arr)

print (image.getLevels())
print (image.getLevels())

image.setZValue(-1)
p.addItem(image)

# green - orange - red
# cmap = pg.ColorMap([0.0, 0.5, 1.0], [(74, 158, 71), (255, 230, 0), (191, 79, 76)])
# hist.gradient.setColorMap(cmap)
hist.setImageItem(image)
image.setLevels ([100,450])
hist.setLevels (min=100, max=450)

p.showGrid(x=True, y=True)


# elevate the grid lines
p.axes['bottom']['item'].setZValue(1000)
p.axes['left']['item'].setZValue(1000)

if __name__ == '__main__':
    pg.exec()
