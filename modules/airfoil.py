#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""

    Airfoil and operations on it 

"""

import os
import numpy as np
from common_utils import * 


class Airfoil:
    """ 

    Airfoil object to handle a airfoil direct related things  

    """

    isStrakAirfoil  = False
    isInterpolated  = False 
    isExample       = False                     # vs. Example_Airfoil 

    def __init__(self, pathFileName = None, name = None, workingDir= None):
        """
        Main constructor for new Airfoil

        Args:
            :pathFileName: optional - string of existinng airfoil path and name \n
            :name: optional - name of airfoil - no checks performed 
        """

        self.pathFileName = None
        self.workingDir   = workingDir if workingDir is not None else ''
        self.name         = name if name is not None else ''
        self.sourceName = None                     # the long name out of the two blended airfoils (TSrakAirfoil)

        self._x   = None
        self._y   = None

        if (pathFileName is not None): 
            if os.path.isabs (pathFileName):
                checkPath = pathFileName
            else:
                checkPath = os.path.join (self.workingDir, pathFileName)
            if not os.path.isfile(checkPath):
                ErrorMsg ("Airfoil file \'%s\' does not exist. Couldn\'t create Airfoil" % checkPath)
                self.name = "-- ? --"
            else:
                self.pathFileName = pathFileName
                self.name = os.path.splitext(os.path.basename(self.pathFileName))[0]
        elif (not name):
            self.name = "-- ? --"


    @classmethod
    def onDict(cls, dataDict, workingDir = None):
        """
        Alternate constructor for new Airfoil based on dictionary 

        Args:
            :dataDict: dictionary with "name" and "file" keys
            :workingDir: home of dictionary (paramter file) 
        """
        pathFileName  = fromDict(dataDict, "file", None)
        name          = fromDict(dataDict, "name", None)
        return cls(pathFileName = pathFileName, name = name, workingDir=workingDir)

    def _save (self, airfoilDict):
        """ stores the variables into the dataDict - returns the filled dict"""
        # will be overloaded 
        toDict (airfoilDict, "file",    self.pathFileName) 
        return airfoilDict
 

    def __repr__(self) -> str:
        # overwrite to get a nice print string 
        info = f"\'{self.name}\'"
        return f"{type(self).__name__} {info}"

    @property
    def x (self): return self._x
        
    @property
    def y (self): return self._y
        

    @property
    def isExisting (self):
        return not self.pathFileName is None
    
    @property
    def isLoaded (self):
        return self._x is not None and len(self._x) > 10
    
    @property
    def isNormalized (self):
        
        if self._x is None: return False
        if self._x[0] != 1.0 or self._x[-1] != 1.0: return False
        if self._y[0] != - self._y[-1]: return False
        ile = np.argmin (self._x)
        if self._x[ile] != 0.0 or self._y[ile] != 0.0: return False
        return True

    #-----------------------------------------------------------

    def set_name (self, newName):
        """
        Set der name of the airfoil 
        Note: 
            This will not rename an existing airfoil (file). Use rename instead...
        """

        self.name = newName


    def set_pathFileName (self,fullPath):
        """
        Set der fullpaths of airfoils location and file \n
        ! This will not move or copy the airfoil physically - use clone instead

        Args:
            :newName: String like '..\myAirfoils\JX-GT-15.dat'
        """

        if (os.path.isfile(fullPath)):
            self.pathFileName = fullPath
        else:
            ErrorMsg ("Airfoil \'%s\' does not exist. Couldn\'t be set" % fullPath)

    @property
    def fileName (self):
        """
        Get filename of airfoil 

        Returns: 
            String like 'JX-GT-15.dat'
        """
        if not self.pathFileName is None: 
            return os.path.basename(self.pathFileName) 
        else:
            return None

        

    def load (self, fromPath = None):
        """
        Loads airfoil coordinates from file. 
        pathFileName must be set before or fromPath must be defined.
        Load doesn't change self pathFileName
        """    

        if fromPath and os.path.isfile (fromPath):
            sourcePathFile = fromPath
        elif self.isExisting and not self.isLoaded: 
            sourcePathFile = os.path.join(self.workingDir, self.pathFileName)
        else:
            sourcePathFile = None 

        if sourcePathFile:
            #InfoMsg ("Reading airfoil from file: %s" % sourcePathFile)
            f = open(sourcePathFile, 'r')
            file_lines = f.readlines()
            f.close()
            self._loadLines(file_lines)


    def _loadLines (self, file_lines):
        # read the lines of the airfoil file into self x,y

        x = []
        y = []
        for i, line in enumerate(file_lines):
            if (i > 0): 
                splitline = line.strip().split(" ",1)
                x.append (float(splitline[0].strip()))
                y.append (float(splitline[1].strip()))
        self._x = np.asarray (x)
        self._y = np.asarray (y)


    def saveAs (self, dir = None, destName = None):
        """
        save self to to destPath and destName and set new values to self
        if both destPath and name are not set, it's just a save to current directory
        """        
        newPathFileName = self.copyAs (dir, destName)
        self.pathFileName =  newPathFileName
        if destName: 
            self.name = destName


    def copyAs (self, dir = None, destName = None):
        """
        Write a copy of self to destPath and destName (the airfoil can be renamed).
        Self remains with its current values 
        return: 
            newPathFileName from dir and destName 
        """        

        if not destName:
            if self.isStrakAirfoil:
                destName = self.sourceName              # strak: take the long name of the two airfoils
            else:
                destName = self.name

        if dir and not os.path.isdir (dir):
            os.mkdir(dir)
            newPathFileName = os.path.join (dir, destName) + '.dat'
        else: 
            newPathFileName = destName + '.dat'

        with open(newPathFileName, 'w+') as file:
            file.write("%s\n" % destName)
            for i in range (len(self.x)):
                file.write("%.7f %.7f\n" %(self.x[i], self.y[i]))
            file.close()

        return newPathFileName
    

    def with_TEGap (self, newGap, xBlend = 0.8):
        """ returns self x,y coordinates with a new te gap.
         The procedere is based on xfoil allowing to define a blending distance from le.

        Arguments: 
            newGap:   in y-coordinates - typically 0.01 or so 
            xblend:   the blending distance from trailing edge 0..1 - Default 0.8
        Returns: 
            x,y:      np coordinate arrays with new Te 
        """

        # currently le must be at 0,0 - te must be at 1,gap/2 (normalized airfoil) 
        if not self.isNormalized: 
            ErrorMsg ("Airfoil '%s' not normalized. Te gap can't be set." % self.name)
            return self.x,self.y
        
        x = np.copy (self.x) 
        y = np.copy (self.y) 
        xBlend = min( max( xBlend , 0.0 ) , 1.0 )

        gap = y[0] - y[-1]
        dgap = newGap - gap 
        ile = np.argmin (x)

        # go over each point, changing the y-thickness appropriately
        for i in range(len(x)):
            # thickness factor tails off exponentially away from trailing edge
            if (xBlend == 0.0): 
                tfac = 0.0
                if (i == 0 or i == (len(x)-1)):
                    tfac = 1.0
            else:
                arg = min ((1.0 - x[i]) * (1.0/xBlend -1.0), 15.0)
                tfac = np.exp(-arg)

            if i <= ile: 
                y[i] = y[i] + 0.5 * dgap * x[i] * tfac # * gap 
            else:
                y[i] = y[i] - 0.5 * dgap * x[i] * tfac # * gap   
        return x,y 


    def plot(self, x=None, y=None):
        """
        Plot the airfoil for testing 
        Alternative coordinates can be supplied 
        """
        import matplotlib.pyplot as plt

        if x is None: x = self.x
        if y is None: y = self.y

        if (x is None or len(x) == 0): 
            ErrorMsg ("No coordinates to plot")
            return 

        fig = plt.figure()
        plt.style.use('seaborn-v0_8-ticks')
        fig.set_figwidth (fig.get_figwidth()  * 2 )     # enlarge window because of 4 plots
        plt.subplots_adjust(left=0.10, bottom=0.10, right=0.95, top=0.90, wspace=None, hspace=None)

        ax = fig.add_subplot(1, 1, 1)
        ax.set_xlim([0.0, 1.0])
        ax.set_xlabel('x')
        ax.set_ylabel('y')
        ax.axis('equal')
        ax.set_title (self.name)
        ax.grid()

        ax.plot(x, y, '-', marker='o', lw=1, fillstyle='none', markersize=4)

        plt.show()    



class Airfoil_Interpolated (Airfoil):
    """ Airfoil which coordinates are (can be) interpolated using scipy.interpolate"""

    isInterpolated  = True 
    isStrakAirfoil  = False
    
    @classmethod
    def fromAirfoil (cls, anAirfoil: Airfoil):
        """
        Alternate constructor for new interpolated Airfoil based on another"""
    
        pathFileName  = anAirfoil.pathFileName
        name          = anAirfoil.name

        return cls(pathFileName = pathFileName, name = name, anAirfoil = anAirfoil)
    
    def __init__(self, pathFileName = '', name = '', anAirfoil : Airfoil = None):
        """
        Main constructor for new Airfoil

        Args:
            :pathFileName: optional - string of existinng airfoil path and name \n
            :name: optional - name of airfoil - no checks performed 
        """
        super().__init__()

        self._x_upper = None
        self._y_upper = None
        self._x_lower = None
        self._y_lower = None
        self._y_upper_interp = None
        self._y_lower_interp = None
        self._x   = None
        self._y   = None

        if anAirfoil is not None: 
            if anAirfoil.isLoaded: 
                self._x = np.asarray (anAirfoil.x)
                self._y = np.asarray (anAirfoil.y) 
                self.pathFileName = anAirfoil.pathFileName
                self.name         = anAirfoil.name + " -interpolated-"
            else: 
                raise ValueError ("Airfoil "+ anAirfoil.name + "not loaded")
        elif pathFileName: 
            self.load()
        else:
            self.name = "<strak>"
            self.pathFileName = None

        if self._x is not None:
            if np.size(self._x) > 20 and np.size(self._x) == np.size(self._y):

                self._splitUpperLower ()
                self._y_upper_interp = self._get_interpol_fn ( self._x_upper, self._y_upper)
                self._y_lower_interp = self._get_interpol_fn ( self._x_lower, self._y_lower)

    @property
    def x (self):
        "x coordinates - rebuild from upper and lower "
        if self._x_upper is None: 
            return []
        else:
            return np.concatenate ((np.flip(self._x_upper), self._x_lower[1:]))
        
    @property
    def y (self):
        "y coordinates - rebuild from upper and lower "
        if self._y_upper is None: 
            return []
        else:
            return np.concatenate ((np.flip(self._y_upper), self._y_lower[1:]))

    @property
    def isLoaded (self):
        return self._x_upper is not None
    

    def _splitUpperLower (self):
        """ split self._x,y into upper and lower coordinates """

        if not self.isNormalized:
            raise ValueError ("Airfoil '" + self.name + "' isn't normalized. Cannot split.")
        
        iLe = np.argmin (self._x) 
        self._x_upper = np.flip (self._x [0: iLe + 1])
        self._y_upper = np.flip (self._y [0: iLe + 1])
        self._x_lower = self._x[iLe:]
        self._y_lower = self._y[iLe:]

    def _get_interpol_fn (self, x, y):

        from scipy.interpolate import interp1d

        return interp1d(x, y, kind='cubic', bounds_error=False, fill_value="extrapolate") 
    
    def _x_distributed (self, xfacStart, xfacEnd, nPoints):

        xfacStart = max(0.0, xfacStart)
        xfacStart = min(2.0, xfacStart)
        xfacEnd   = max(0.0, xfacEnd)
        xfacEnd   = min(2.0, xfacEnd)

        if xfacStart >= xfacEnd: raise ValueError ("Airfoil x-distribution: start > end")

        newX = np.sin (np.pi / 2 * (np.linspace(xfacStart, xfacEnd, nPoints) +3.0)) 

        # normalize to 0..1
        xmin = np.amin(newX)
        xmax = np.amax(newX) 
        newX = (newX - xmin) / (xmax-xmin)

        # round - ensure 0.0 .. 1.0 
        newX = np.around (newX,10) 
        return newX
         

    def set_x_upper_lower (self,x): 
        """set new values (array) for upper and lower x - evaluate y with interpolation"""

        self._x_upper = x
        self._x_lower = x
        self._y_upper = self._y_upper_interp(x)
        self._y_lower = self._y_lower_interp(x)
         


class Airfoil_Straked (Airfoil_Interpolated):
    """ Airfoil which is straked (blended) from it's neighbours"""

    isStrakAirfoil = True

    def __init__ (self):
        super().__init__()

        self.name = "<strak>" 
        self.sourceName = None          # the long name out of the two belended airfoils


    def _save (self, airfoilDict):
        """ stores the variables into the dataDict - returns the filled dict"""
        toDict (airfoilDict, "name",  self.name) 
        return airfoilDict


    def do_strak (self, airfoil_in1 : Airfoil, airfoil_in2:Airfoil, blendBy):
        """ straks (blends) self out of two airfoils to the left and right
        depending on the blendBy factor"""

        if airfoil_in1.isInterpolated: 
            airfoil1 = airfoil_in1
        else:
            airfoil1 = Airfoil_Interpolated.fromAirfoil(airfoil_in1)

        if airfoil_in2.isInterpolated: 
            airfoil2 = airfoil_in2
        else:
            airfoil2 = Airfoil_Interpolated.fromAirfoil(airfoil_in2)
    
        if blendBy <= 0.5:
            x_ref_upper = airfoil1._x_upper
            x_ref_lower = airfoil1._x_lower
        else:
            x_ref_upper = airfoil2._x_upper
            x_ref_lower = airfoil2._x_lower

        self._x_upper = x_ref_upper
        self._y_upper = (1 - blendBy) * airfoil1._y_upper + \
                             blendBy  * airfoil2._y_upper_interp(x_ref_upper)

        self._x_lower = x_ref_lower
        self._y_lower = (1 - blendBy) * airfoil1._y_lower + \
                             blendBy  * airfoil2._y_lower_interp(x_ref_lower)
        
        self._x = np.concatenate ((np.flip(self._x_upper), self._x_lower[1:]))
        self._y = np.concatenate ((np.flip(self._y_upper), self._y_lower[1:]))
        
        self.sourceName = airfoil_in1.name + ("_with_%.2f_" % blendBy) + airfoil_in2.name

    def do_strak_Worker  (self, leftAir : Airfoil, rightAir:Airfoil, blendBy):
        """ straks (blends) self out of two airfoils to the left and right."""
        # import shutil

        # tmpDir = "~tmp"

        # leftPathFile  = leftAir.copyAs  (dir=tmpDir)
        # rightPathFile = rightAir.copyAs (dir=tmpDir)

        # newName = leftAir.name + ("_with_%.2f_" % blendBy) + rightAir.name

        # result = XfoilWorker().blendAirfoils(leftPathFile, rightPathFile,blendBy, newName)

        # if result == 0:
        #     newPathFile = os.path.join(tmpDir,newName) + '.dat'
        #     self.load (fromPath=newPathFile)

        #     self.sourceName = os.path.splitext(os.path.basename(newPathFile))[0]
        # else: 
        #     ErrorMsg ("'xfoil_worker' couldn't be executed.")

        # shutil.rmtree(tmpDir)

        return

from scipy.interpolate import splprep, splev, interp1d, sproot

class SplineOfAirfoil: 
    """ Spline representation of airfoil"""


    def __init__ (self, x,y):


        # thickness and camber 
        self._thickness = None                  # array of thickness distribution
        self._camber    = None                  # array of camber distribution
        self._xthick    = None                  # x values for thickness and camber

        # B-spline representation of an N-D curve.
        s = 0.0                                 # no smoothing 
        k = 3                                   # oder of spline - cubic
        self._u    = None                       # the arc positionen around airfoil 0..1
        self._tck  = 0                          # spline parameters - see scipy splprep 
        self._x = x  #test
        self._tck, self._u = splprep([x, y], s=s, k=k)

        # leading edge 
        self.iLe = np.argmin (x)            # index of LE in x,y and u 
        self.uLe = self._u [self.iLe]             # u value at LE 

        # for i in range(len(x)):
        #     print (i, x[i], y[i], self._u[i])


    @property
    def deriv1 (self): 
        """ return derivate 1 of spline at knots"""

        #  splev returns dx/du and dy/du at knot. 
        dx, dy = splev(self._u, self._tck, der=1)
        #  derivative 1 (gradient) is dy/dx = dy/du / dx/du


        return dy/dx

    @property
    def angle (self): 
        """ return the angle in degrees at knots"""
        return np.arctan (self.deriv1) * 180 / np.pi

    @property
    def curvature (self): 
        " return the curvature at knots 0..npoints"

        dx, dy   = splev(self._u, self._tck, der=1)
        ddx, ddy = splev(self._u, self._tck, der=2)

        deriv2 = dx * ddy - dy * ddx

        # get curvature from derivative 2
        n = dx**2 + dy**2
        curv = deriv2 / n**(3./2.)
        return curv 


    def thickness_camber (self): 
        """returns thickness and camber distribution.
        
        Note: 
        
        It's an approximation as thickness is just the sum of y_upper(x) and y_lower(x)
        and camber is just the mean value y_upper(x) and y_lower(x)
        
        """

        # get a new high res distribution for upper and lower 
        u_new_upper = np.linspace (self.uLe, 0.0, 400)
        u_new_lower = np.linspace (self.uLe, 1.0, 400)
        
        x_upper, y_upper = splev (u_new_upper, self._tck, der= 0)
        x_lower, y_lower = splev (u_new_lower, self._tck, der= 0)

        x_upper = np.asarray(x_upper).round(10)
        y_upper = np.asarray(y_upper).round(10)
        x_lower = np.asarray(x_lower).round(10)
        y_lower = np.asarray(y_lower).round(10)

        # take x-distrib of upper and interpolate y_lower with these values
        y_lower = interp1d(x_lower, y_lower,  kind='cubic') (x_upper)
        y_lower = np.asarray(y_lower).round(10)
        x_lower = x_upper

        # thickness and camber can now easily calculated 
        self._thickness = ((y_upper - y_lower)      ).round(10) 
        self._camber    = ((y_upper + y_lower) / 2.0).round(10)
        self._xthick    = x_upper 

        # for i in range(len(x_upper)):
        #     print (i, self._thickness[i], self._camber[i])

        return self._xthick, self._thickness, self._camber 

 
    def get_maxThickness (self): 
        """returns max. thickness (normed) and its position along x"""
        if self._thickness is None: self.thickness_camber()

        # todo interpolate t_max on spline 
        tmax   = np.max(self._thickness)
        tmax_x = self._xthick[np.argmax (self._thickness)]
        return tmax, tmax_x

    def get_maxCamber (self): 
        """returns max. camber (normed) and its position along x"""

        if self._camber is None: self.thickness_camber()

        # todo interpolate c_max on spline 
        cmax   = np.max(self._camber)
        cmax_x = self._xthick[np.argmax (self._camber)]
        return cmax, cmax_x

    def _x_cos_distributed (self, xfacStart, xfacEnd, nPoints):

        xfacStart = max(0.0, xfacStart)
        xfacStart = min(2.0, xfacStart)
        xfacEnd   = max(0.0, xfacEnd)
        xfacEnd   = min(2.0, xfacEnd)

        if xfacStart >= xfacEnd: raise ValueError ("Airfoil x-distribution: start > end")

        newX = np.sin (np.pi / 2 * (np.linspace(xfacStart, xfacEnd, nPoints) +3.0)) 

        # normalize to 0..1
        xmin = np.amin(newX)
        xmax = np.amax(newX) 
        newX = (newX - xmin) / (xmax-xmin)

        # round - ensure 0.0 .. 1.0 
        newX = np.around (newX,10) 
        return newX



# Main program for testing -----------------------------------

def cubicSplineTest ():

    import matplotlib.pyplot as plt
    from airfoil_examples import Root_Example, Tip_Example
    
    air1 = Tip_Example()
    air = Airfoil_Interpolated.fromAirfoil(air1)

    y = air.y
    x = air.x

    spl = SplineOfAirfoil (x,y) 
    x_upper, t, c = spl.thickness_camber()

    print ("Thickness: ", spl.get_maxThickness())
    print ("Camber:    ", spl.get_maxCamber())

    fig, axa = plt.subplots(3, 1, figsize=(16,8))
    ax1 = axa[0]
    ax2 = axa[1]
    ax3 = axa[2]
    ax1.axis('equal')
    ax1.set_ylim([ -0.2,  0.2])
    # ax1.set_xlim([ 0.0, 1.1])

    ax2.set_ylim([ -20,  20])
    ax3.set_ylim([ -1,  1])

    ax1.grid(True)
    ax2.grid(True)
    ax3.grid(True)

    ax1.plot(x, y, '-', label='x y')
    ax1.plot(x_upper, t, '-.', label='thickness')
    ax1.plot(x_upper, c, '-', label='camber')
    ax2.plot (x,spl.angle, label='Angle')
    ax3.plot (x,spl.curvature, label='Curvature')

    ax1.legend()
    ax2.legend()
    ax3.legend()
    plt.show()



def blendTest():

    from airfoil_examples import Root_Example, Tip_Example
    air1 = Root_Example()
    air1.load()
    air2 = Tip_Example()
    air2.load()

    airStrak = Airfoil_Straked.fromAirfoil(air1)
    airStrak.do_strak (air1, air2, 0.5)

    airNewX = Airfoil_Interpolated.fromAirfoil(air1)


    fig = plt.figure()
    plt.style.use('seaborn-v0_8-ticks')
    fig.set_figwidth (fig.get_figwidth()  * 2 )     # enlarge window because of 4 plots
    plt.subplots_adjust(left=0.10, bottom=0.10, right=0.95, top=0.90, wspace=None, hspace=None)

    ax = fig.add_subplot(1, 1, 1)
    ax.set_xlim([0.0, 1.0])
    ax.axis('equal')
    # ax.set_title (self.name)
    ax.grid()

    ax.plot(air1.x, air1.y, '-', marker='o', lw=1, fillstyle='none', markersize=4, label=air1.name)
    ax.plot(air2.x, air2.y, '-', marker='o', lw=1, fillstyle='none', markersize=4, label=air2.name)
    ax.plot(airStrak.x, airStrak.y, '-', marker='o', lw=1, fillstyle='none', markersize=4, label=airStrak.name)
    newX = airNewX._x_distributed (0.0, 1.0, 100)
    airNewX.set_x_upper_lower (newX)
    ax.plot(airNewX.x, airNewX.y, '-', marker='o', lw=1, fillstyle='none', markersize=6, label=air1.name + " new 0.0, 1.0, 50")
    newX = airNewX._x_distributed (0.0, 2.0, 100)
    airNewX.set_x_upper_lower (newX)
    ax.plot(airNewX.x, airNewX.y, '-', marker='o', lw=1, fillstyle='none', markersize=6, label=air1.name + " new 0.0, 2.0, 50")
    newX = airNewX._x_distributed (0.0, 1.3, 100)
    airNewX.set_x_upper_lower (newX)
    ax.plot(airNewX.x, airNewX.y, '-', marker='o', lw=1, fillstyle='none', markersize=6, label=air1.name + " new 0.0, 1.3, 50")

    air1.saveAs()
    air2.saveAs()
    airStrak.saveAs()
    airNewX.name = airNewX.name + " newX"
    airNewX.saveAs()
    ax.legend()
    plt.show()    



if __name__ == "__main__":

    # from worker_driver import XfoilWorker
    from airfoil_examples import Root_Example
    import matplotlib.pyplot as plt


    # ---- Test -----
    # loadFromFile = False

    # blendTest()

    cubicSplineTest()
    # myAirfoil = Root_Example()
    # print ("New airfoil created: ", myAirfoil)
    # myAirfoil.load()

    # myInterpol = Airfoil_Interpolated.fromAirfoil(myAirfoil)
    # myInterpol.plot()

    # fig = plt.figure()
    # plt.style.use('seaborn-v0_8-ticks')
    # fig.set_figwidth (fig.get_figwidth()  * 2 )     # enlarge window because of 4 plots

    # ax = fig.add_subplot(1, 1, 1)
    # ax.axis('equal')
    # ax.set_title (myAirfoil.name)
    # ax.grid()


    # gap = 0.04

    # for gap in np.linspace (0.0, 0.08, 3):
    #     for xBlend in np.linspace (0.1, 1, 4):
    #         x, y = myAirfoil.with_TEGap (gap,xBlend)
    #         ax.plot(x, y, '-', label="gap=%.2f xBlend=%.2f" % (gap, xBlend))

    #     # myAirfoil.plot(x=x, y=y)
    # ax.legend()
    # plt.subplots_adjust(left=0.10, bottom=0.10, right=0.95, top=0.90, wspace=None, hspace=None)
    # plt.show()    

    # print ("Starting polar generation")
    # myAirfoil.polarSet.load_or_generatePolars ([200000, 220000, 270000, 500000])

    # print ("Generated or loaded polars: ", myAirfoil.polarSet.polars)
    # myAirfoil.polarSet.plot()



