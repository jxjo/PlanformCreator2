#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""

    Airfoil and operations on it 

"""
from typing import Type
import logging

import os
from pathlib import Path
import numpy as np
from math_util import * 
from common_utils import * 
from airfoil_geometry import Geometry_Splined, Geometry, Geometry_Bezier, Geometry_HicksHenne
from airfoil_geometry import Side_Airfoil, Side_Airfoil_Bezier, UPPER, LOWER


# geometry soecification 

GEO_BASIC  = Geometry
GEO_SPLINE = Geometry_Splined

# airfoil types for some usage semantics in application 

NORMAL      = ""
SEED        = "Seed"
SEED_DESIGN = "Seed of design"
REF1        = "Reference 1" 
REF2        = "Reference 2" 
DESIGN      = "Design"
FINAL       = "Final"

AIRFOIL_TYPES = [NORMAL, SEED, SEED_DESIGN, REF1, REF2, DESIGN, FINAL]


#--------------------------------------------------------------------------

class Airfoil:
    """ 

    Airfoil object to handle a airfoil direct related things  

    """
    isStrakAirfoil      = False
    isEdited            = False
    isExample           = False                      # vs. Example_Airfoil 
    isBezierBased       = False
    isHicksHenneBased   = False

    # defaults for panelling - can be overwritten from Settings 

    nPanels_default     = 160                       # repanel: no of panels 
    le_bunch_default    = 0.86                      # repanel: panel bunch at leading edge
    te_bunch_default    = 0.7   	                # repanel: panel bunch at trailing edge


    def __init__(self, x= None, y = None, name = None,
                 geometry : Type[Geometry]  = GEO_SPLINE, 
                 pathFileName = None,  workingDir= None):
        """
        Main constructor for new Airfoil

        Args:
            pathFileName: optional - string of existinng airfoil path and name 
            name: optional         - name of airfoil - no checks performed 
            x,y: optional          - the coordinates of airfoil 
            geometry: optional     - the geometry staretegy either GEO_BASIC, GEO_SPLNE...
            workingDir: optional   - base directoty where pathFileName is relative 
        """

        self.pathFileName = None
        if workingDir is not None:
            self.workingDir = os.path.normpath (workingDir)
        else: 
            self.workingDir   = ''
        self._name        = name if name is not None else ''
        self.sourceName   = None                 # long name out of the two blended airfoils (TSrakAirfoil)

        if not x is None: x = x if isinstance(x,np.ndarray) else np.asarray (x)
        self._x     = x
        if not y is None: y = y if isinstance(y,np.ndarray) else np.asarray (y)
        self._y     = y  

        self._isModified     = False
        self._isEdited       = False 
        self._isStrakAirfoil = False             # is self blended from two other airfoils 

        self._geometryClass  = geometry          # geometry startegy 
        self._geo            = None              # selfs instance of geometry

        self._nPanelsNew     = None              # repanel: no of panels - init via default
        self._le_bunch       = None              # repanel: panel bunch at leading edge
        self._te_bunch       = None   	         # repanel: panel bunch at trailing edge

        self._polarSets      = None              # polarSets which are defined from outside 

        self._usedAs         = None              # usage type of airfoil used by app <- AIRFOIL_TYPES
        self._propertyDict   = {}                # multi purpose extra properties for an AIirfoil


        # pathFileName must exist if no coordinates were given 

        if (pathFileName is not None) and  (x is None or y is None): 
            pathFileName = os.path.normpath(pathFileName)       # make all slashes to double back
            if os.path.isabs (pathFileName):
                checkPath = pathFileName
            else:
                checkPath = os.path.join (self.workingDir, pathFileName)
            if not os.path.isfile(checkPath):

                ErrorMsg ("Airfoil file '%s' does not exist. Couldn't create Airfoil" % checkPath)
                self._name = "-- Error --"
            else:
                self.pathFileName = pathFileName
                self._name = os.path.splitext(os.path.basename(self.pathFileName))[0]  # load will get the real name

        elif (pathFileName is not None) : 
                self.pathFileName = pathFileName

        elif (not name):
            self._name = "-- ? --"


    @classmethod
    def onDict(cls, dataDict, workingDir = None, geometry : Type[Geometry]  = None):
        """
        Alternate constructor for new Airfoil based on dictionary 

        Args:
            :dataDict: dictionary with "name" and "file" keys
            :workingDir: home of dictionary (paramter file) 
        """
        pathFileName  = fromDict(dataDict, "file", None)
        name          = fromDict(dataDict, "name", None)
        return cls(pathFileName = pathFileName, name = name, 
                   geometry = geometry, workingDir = workingDir)


    @classmethod
    def onDictKey (cls, dataDict, key, workingDir = None):
        """
        Alternate constructor for new Airfoil based on data dictionary and key
        Returns None if key doesn't exist in dataDict 

        Args:
            :dataDict: dictionary with key
            :key: key in dataDict
            :workingDir: home of dictionary (paramter file) 
        """
        pathFileName  = fromDict(dataDict, key, None)
        if pathFileName is not None: 
            return cls(pathFileName = pathFileName, workingDir=workingDir)
        else: 
            return None 
        

    @classmethod
    def onFileType(cls, pathFileName, workingDir = None):
        """
        Alternate constructor for new Airfoil based on its file type

            '.dat'      - returns Airfoil 
            '.bez'      - returns Airfoil_Bezier 
            '.hicks'    - returns Airfoil_Hicks_Henne 

        Args:
            pathFileName: string of existinng airfoil path and name
            workingDir: optional working dir (if path is relative)
        """

        ext = os.path.splitext(pathFileName)[1]

        if ext == '.dat': 
            return Airfoil (pathFileName=pathFileName, workingDir=workingDir)
        elif ext == '.bez': 
            return Airfoil_Bezier (pathFileName=pathFileName, workingDir=workingDir)
        elif ext == '.hicks': 
            return Airfoil_Hicks_Henne (pathFileName=pathFileName, workingDir=workingDir)
        else:
            raise ValueError (f"Unknown file extension '{ext}' for new airfoil")



    def _save (self, airfoilDict):
        """ stores the variables into the dataDict - returns the filled dict"""
        
        if self.isStrakAirfoil:
            toDict (airfoilDict, "name", self.name) 
        else:
            toDict (airfoilDict, "file", self.pathFileName) 
        return airfoilDict
 

    def __repr__(self) -> str:
        # overwritten to get a nice print string 
        info = f"\'{self.name}\'"
        return f"{type(self).__name__} {info}"


    # ----------  Properties ---------------

    @property
    def x (self): return self._x

    @property
    def y (self): return self._y


    @property
    def geo (self) -> Geometry:
        """ the geometry strategy of self"""
        if self._geo is None: 
            self._geo = self._geometryClass (self.x, self.y)
        return self._geo
    
    def set_geo_strategy (self, geometry):
        """ set new geometry strategy of self
        Args: 
            geometry: either GEO_BASIC or GEO_SPLIne
        """
        if geometry != GEO_BASIC and geometry != GEO_SPLINE: return 

        if self._geometryClass != geometry:
           self._geometryClass = geometry
           self._geo = None 


    def set_xy (self, x, y):
        """ set new coordinates """

        if not x is None: 
            x = x if isinstance(x,np.ndarray) else np.asarray (x)
            x = np.round(x,7)
        if not y is None: 
            y = y if isinstance(y,np.ndarray) else np.asarray (y)
            y = np.round(y,7)

        self._x     = x
        self._y     = y  

        self._geo    = None

        self.set_isModified (True)


    @property
    def name (self): return self._name 
    def set_name (self, newName):
        """  Set name of the airfoil 
        Note:  This will not rename an existing airfoil (file). Use rename instead...
        """
        self._name = newName
        self.set_isModified (True)

    @property
    def name_short (self):
        """ name of airfoil shortend at the beginning to 23 chars"""
        if len(self.name) <= 23:    return self.name
        else:                       return "..." + self.name[-20:]
            
    @property
    def hasPolarSets (self):
        """does self has polarSets (which are set from 'outside')
        """
        return self._polarSets is not None
    
    def polarSets (self):
        """ returns  PolarSets of self"""

        if self._polarSets is None:
            return []
        else: 
            return self._polarSets

    def set_polarSets (self, polarSets): 
        """ polarSets must be set from outside - Airfoil doesn't know about this """
        self._polarSets = polarSets


    @property
    def isEdited (self): return self._isModified
    """ True if airfoil is being edited, modified, ..."""
        # currently equals to isModified ...
    def set_isEdited (self, aBool): 
        self.set_isModified (aBool) 


    @property
    def isModified (self): return self._isModified
    """ True if airfoil is being modifiied, ..."""
    def set_isModified (self, aBool): 
        self._isModified = aBool 

    @property
    def isExisting (self):
        return not self.pathFileName is None


    @property
    def isLoaded (self):
        return self._x is not None and len(self._x) > 10
    
    @property
    def isNormalized (self):
        """ is LE at 0,0 and TE at 1,.. ?"""
        return self.geo.isNormalized
    
    @property
    def isStrakAirfoil (self):
        """ is self blended out of two other airfoils"""
        return self._isStrakAirfoil
    def set_isStrakAirfoil (self, aBool): 
        self._isStrakAirfoil = aBool
    
    @property
    def nPanels (self): 
        """ number of panels """
        return self.geo.nPanels
      
    @property
    def nPoints (self): 
        """ number of coordinate points"""
        return self.geo.nPoints 

    @property
    def teGap_perc (self): 
        """ trailing edge gap in %"""
        return  self.geo.teGap * 100
    
    def set_teGap_perc (self, newGap : float): 
        """ set trailing edge gap to new value %
        
        Args: 
            newGap: te gap in percent %
        """
        newGap = max(0.0, newGap)
        newGap = min(5.0, newGap)
        self.geo.set_teGap (newGap / 100)
        self.set_xy(*self.geo.xy)


    @property
    def leRadius_perc (self):
        """ leading edge radius in % which is the reciprocal of curvature at le """
        return self.geo.leRadius * 100
    
    def set_leRadius_perc (self, newRadius: float): 
        """ set leading edge radius in % which is the reciprocal of curvature at le """
        newRadius = min(10,   newRadius)
        newRadius = max(0.1,  newRadius)
        factor = newRadius / self.leRadius_perc
        self.geo.set_leRadius (factor)
        self.rebuild_from_thicknessCamber()


    @property
    def maxThickness (self): 
        """ max thickness in %"""
        return self.geo.maxThick * 100
    def set_maxThickness(self,newVal): 
        """ set max thickness in %"""
        if newVal < 0.5: newVal = 0.5               # do not allow thickness < 0,5% 
        self.geo.set_maxThick (newVal/100.0)
        self.rebuild_from_thicknessCamber()


    @property
    def maxThicknessX (self): 
        """ max thickness x-position in %"""
        return self.geo.maxThickX * 100
    def set_maxThicknessX(self,newVal): 
        """ set max thickness x-position in %"""
        self.geo.set_maxThickX (newVal/100.0)
        self.rebuild_from_thicknessCamber()


    @property
    def maxCamber (self): 
        """ max camber in %"""
        return self.geo.maxCamb * 100
    def set_maxCamber(self,newVal): 
        """ set max camber in %"""
        self.geo.set_maxCamb (newVal/100.0)
        self.rebuild_from_thicknessCamber()


    @property
    def maxCamberX (self): 
        """ max camber x-position in %"""
        #todo remove
        return self.geo.maxCambX * 100
    def set_maxCamberX(self,newVal): 
        """ set max camber x-position in %"""
        self.geo.set_maxCambX (newVal/100.0)
        self.rebuild_from_thicknessCamber()


    def rebuild_from_thicknessCamber(self):
        """ 
        rebuilds self out of thickness and camber distribution in Geometry
        which were modified directly eg. with mouse - avoid re-spline 
        """

        self.geo._rebuild_from_thicknessCamber ()        # new build of x,y in geo

        self._x = self.geo.x
        self._y = self.geo.y

        self.geo.set_xy_org (self._x, self._y)          # update the copy of x,y in geo 


    @property
    def isSymmetric(self):
        """ true if max camber is 0.0 - so it's a symmetric airfoil"""
        return self.geo.maxCamb == 0.0

    @property
    def camber (self) -> 'Side_Airfoil': 
        """ camber line as Line object"""
        return self.geo.camber 

    @property
    def thickness (self) -> 'Side_Airfoil': 
        """ thickness distribution as Line object """
        return self.geo.thickness

    @property
    def nPanelsNew (self): 
        """ number of panels when being repaneled"""
        if self._nPanelsNew is None: 
            return self.nPanels_default
        else: 
            return self._nPanelsNew 
    def set_nPanelsNew (self, newVal): 
        """ set number of panels and repanel"""
        newVal = max (40,  newVal)
        newVal = min (500, newVal) 
        self._nPanelsNew = int (newVal)
        self.repanel()

    @property
    def le_bunch (self): 
        """ leading edge bunch of panels"""
        if self._le_bunch is None: 
            return self.le_bunch_default
        else: 
            return self._le_bunch
    def set_le_bunch (self, newVal): 
        """ set leading edge bunch of panels and repanel"""
        self._le_bunch = newVal
        self.repanel()

    @property
    def te_bunch (self): 
        """ trailing edge bunch of panels"""
        if self._te_bunch is None: 
            return self.te_bunch_default
        else: 
            return self._te_bunch
    def set_te_bunch (self, newVal): 
        """ set trailing edge bunch of panels and repanel"""
        self._te_bunch = newVal
        self.repanel()

    @property
    def usedAs (self):
        """ usage type of self like DESIGN <- Airfoil_Types"""
        return self._usedAs
    def set_usedAs (self, aType): 
        if aType in AIRFOIL_TYPES:
            self._usedAs = aType
    

    #-----------------------------------------------------------


    def set_pathFileName (self,fullPath, noCheck=False):
        """
        Set der fullpaths of airfoils location and file \n
        ! This will not move or copy the airfoil physically - use copyAs instead

        Args:
            :fullPath: String like '..\\myAirfoils\\JX-GT-15.dat'
            :noCheck:  = TRUE - no check if fullpath exists - default FALSE 
        """

        if noCheck or (os.path.isfile(fullPath)):
            self.pathFileName = fullPath
        else:
            ErrorMsg ("Airfoil \'%s\' does not exist. Couldn\'t be set" % fullPath)

    @property
    def fileName (self):
        """
        filename of airfoil like 'JX-GT-15.dat'
        """
        if not self.pathFileName is None: 
            return os.path.basename(self.pathFileName) 
        else:
            return None

    @property
    def pathName (self):
        """
        directory pathname of airfoil like '..\\myAirfoils\\'
        """
        if not self.pathFileName is None: 
            return os.path.dirname(self.pathFileName) 
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
            f = open(sourcePathFile, 'r')
            file_lines = f.readlines()
            f.close()
            self._name, self._x, self._y = self._loadLines(file_lines)


    def _loadLines (self, file_lines):

        # returns the name and x,y (np array) of the airfoil file 

        x = []
        y = []
        xvalPrev = -9999.9
        yvalPrev = -9999.9

        for i, line in enumerate(file_lines):
            if (i > 0): 
                splitline = line.strip().split()               # will remove all extra spaces
                if len(splitline) == 1:                        # couldn't split line - try tab as separator
                    splitline = line.strip().split("\t",1)
                if len(splitline) >= 2:                     
                    xval = float(splitline[0].strip())
                    yval = float(splitline[1].strip())
                    if xval == xvalPrev and yval == yvalPrev:   # avoid duplicate, dirty coordinates
                        WarningMsg ("Airfoil '%s' has duplicate coordinates - skipped." % self._name)
                    else: 
                        x.append (xval)
                        y.append (yval) 
                    xvalPrev = xval 
                    yvalPrev = yval 
            else: 
                name = line.strip()

        return name, np.asarray (x), np.asarray (y)


    def save (self):
        """basic save of self to its pathFileName
        """
        if self.isLoaded: 
            self._write_to_file ()
            self.set_isModified (False)


    def saveAs (self, dir = None, destName = None):
        """
        save self to to destPath and destName and set new values to self
        if both destPath and name are not set, it's just a save to current directory

        Returns: 
            newPathFileName from dir and destName 
        """     
        if destName: 
            self.set_name (destName)  

        # create dir if not exist - build new airfoil filename
        if dir: 
            if not os.path.isdir (dir):
                os.mkdir(dir)
            self.set_pathFileName (os.path.join (dir, self.name) + '.dat', noCheck=True)

        self.save()
        self.set_isModified (False)
        return self.pathFileName



    def save_copyAs (self, dir = None, destName = None, teGap=None ):
        """
        Write a copy of self to destPath and destName (the airfoil can be renamed).
        Self remains with its current values.
        Optionally a new teGap may be defined for the exported airfoil  

        Args: 
            dir: -optional- new directory for the airfoil 
            destName: - optional- new name
            teGap: -optional- new TE gap in x,y coordinates 

        Returns: 
            newPathFileName from dir and destName 
        """        

        # te gap name extension 
        if teGap is not None: 
            teText = '_te=%.2f' % (teGap * 100)                 # te thickness in percent
        else: 
            teText = ''
        destName = destName + teText if destName else None 

        # determine (new) airfoils name  if not provided
        if not destName:
            if self.isStrakAirfoil:
                destName = self.sourceName                     # strak: take the long name of the two airfoils
            else:
                if self.name: 
                    destName = self.name    
                else:
                    if self.fileName:
                        destName = Path(self.fileName).stem        # cut '.dat'
                    else: 
                        raise ValueError ("Destination name of airfoil couldn't be evaluated")

        # create dir if not exist - build airfoil filename
        if dir: 
            if not os.path.isdir (dir):
                os.mkdir(dir)
            newPathFileName = os.path.join (dir, destName) + '.dat'
        else: 
            newPathFileName = destName + '.dat'

        # create temp new airfoil 
        if not self.isLoaded: self.load()

        airfoil = self.asCopy (name=destName, pathFileName=newPathFileName)

        if teGap is not None: 
            airfoil.set_teGap_perc (teGap * 100)

        # save it to file 
        airfoil.save ()

        return airfoil.pathFileName


    def asCopy (self, pathFileName = None, 
                name=None, nameExt=None,
                geometry=None) -> 'Airfoil':
        """
        returns a copy of self 

        Args:
            pathFileName: optional - string of existinng airfoil path and name 
            name: optional         - name of airfoil - no checks performed 
            nameExt: -optional     - will be appended to self.name (if name is not provided)
            geometry: optional     - the geometry staretegy either GEO_BASIC, GEO_SPLNE...
        """
        if pathFileName is None and name is None: 
            pathFileName = self.pathFileName

        if name is None:
            name = self.name + nameExt if nameExt else self.name

        geometry = geometry if geometry else self._geometryClass

        airfoil =  Airfoil (x = np.copy (self.x), y = np.copy (self.y), 
                            name = name, pathFileName = pathFileName, 
                            geometry = geometry )
        return airfoil 


    def _write_to_file (self):
        """ writes .dat file of to self.pathFileName"""

        with open(self.pathFileName, 'w+') as file:
            file.write("%s\n" % self.name)
            for i in range (len(self.x)):
                file.write("%.7f %.7f\n" %(self.x[i], self.y[i]))
            file.close()


    def repanel (self): 
        """
        Repanel self with the current values of nPointsNew, le_ and te_bunch."""

        # new panel distribution for upper and lower 
        if not self.geo.isBasic: 
            self.geo.repanel (nPanels= self.nPanelsNew, 
                              le_bunch= self.le_bunch, te_bunch = self.te_bunch)
            
            self.set_xy (*self.geo.xy)
        else: 
            raise ValueError (f"{self} may not be repaneled")


    def normalize (self):
        """
        Shift, rotate, scale airfoil so LE is at 0,0 and TE is symmetric at 1,y
        Returns True/False if normalization was done  
        """

        normalized = self.geo.normalize() 

        if normalized:

            # load new coordinates from geo 
            self.set_xy (self.geo.x, self.geo.y)
        return normalized 


    def do_strak (self, airfoil1 : 'Airfoil', airfoil2 : 'Airfoil', blendBy : float,
                  geometry = None ):
        """ straks (blends) self out of two airfoils to the left and right
        depending on the blendBy factor
        
        Args: 
            geometry: optional - geo strategy for strak - either GEO_BASIC or GEO_SPLINE
        """
    
        # sanity - both airfoils must be loaded 
        if not airfoil1.isLoaded:
            raise ValueError ("Airfoil '" + airfoil1.name + "' isn't loaded. Cannot strak.")
        if not airfoil2.isLoaded:
            raise ValueError ("Airfoil '" + airfoil2.name + "' isn't loaded. Cannot strak.")

        if blendBy < 0.0: raise ValueError ("blendyBy must be >= 0.0")
        if blendBy > 1.0: raise ValueError ("blendyBy must be <= 1.0")

        # other geo strategy? 
        if not geometry is None and geometry != self._geometryClass:
            geo = geometry (self.x, self.y)
        else:
            geo = self.geo 

        geo.strak (airfoil1.geo, airfoil2.geo, blendBy)

        self.set_xy (*geo.xy)
        self.sourceName = airfoil1.name + ("_blended_%.2f_" % blendBy) + airfoil2.name
        self.set_isStrakAirfoil (True)



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


#------------------------------------------------------

class Airfoil_Bezier(Airfoil):
    """ 

    Airfoil based on Bezier curves for upper and lower side 

    """

    isBezierBased  = True


    def __init__(self, name = None, pathFileName=None, workingDir= None):
        """
        Main constructor for new Bezier Airfoil

        Args:
            pathFileName: optional - string of existinng airfoil path and name 
            name: optional - name of airfoil - no checks performed 
        """
        super().__init__( name = name, pathFileName=pathFileName, workingDir=workingDir)

        self._geometryClass  = Geometry_Bezier      # geometry startegy 
        self._isLoaded       = False                # bezier definition loaded? 

    @property
    def pathFileName_bezier (self) -> str: 
        """ pathfileName of the Bezier definition file """
        if self.pathFileName:  
            return os.path.splitext(self.pathFileName)[0] + ".bez"
        else: 
            return None 

    @property
    def isLoaded (self): 
        # overloaded
        return self._isLoaded
    def set_isLoaded (self, aBool: bool):
        self._isLoaded = aBool

    @property
    def geo (self) -> Geometry_Bezier:
        """ the geometry strategy of self"""
        if self._geo is None: 
            self._geo = Geometry_Bezier ()
        return self._geo

    def set_geo (self, geometry: Geometry_Bezier):
        """ set new geometry bezier object  """
        # do nothing as only Geometry_Bezier is supported
        pass


    def set_xy (self, x, y):
        """ Bezier - do nothing """

        # overloaded - Bezier curve in Geometry is master of data 
        pass


    def set_newSide_for (self, curveType, px,py): 
        """creates either a new upper or lower side in self"""
        self.geo.set_newSide_for (curveType, px,py)
        self.set_isModified (True)

    @property
    def x (self):
        # overloaded  - take from bezier 
        return self.geo.x

    @property
    def y (self):
        # overloaded  - take from bezier 
        return self.geo.y

    # -----------------

    def reset (self): 
        """ make child curves like thickness or camber invalid """
        self.geo._reset_lines()


    def load (self):
        """
        Overloaded: Loads bezier definition instead of .dat from file" 
        """    

        if self.isExisting and not self.isLoaded: 
            sourcePathFile = os.path.join(self.workingDir, self.pathFileName)
        else:
            sourcePathFile = None 

        return self.load_bezier(fromPath=sourcePathFile)
    


    def load_bezier (self, fromPath=None):
        """
        Loads bezier definition from file. 
        pathFileName must be set before or fromPath must be defined.
        Load doesn't change self pathFileName
        """    

        if fromPath is None: 
            fromPath = self.pathFileName

        with open(fromPath, 'r') as file:            

            file_lines = file.readlines()

        # format of bezier airfoil file 
        # <airfoil name> 
        # Top Start
        # 0.0000000000000000 0.0000000000000000
        # ...
        # 1.0000000000000000 0.0000000000000000
        # Top End
        # Bottom Start
        # ...
        # Bottom End
        new_name = 'Bezier_Airfoil'                         # defalut name 

        try: 
            px, py = [], []
            for i, line in enumerate(file_lines):
                if i == 0:
                    new_name = line.strip()
                else: 
                    line = line.lower()
                    if "start" in line:
                        if "top" in line: 
                            curveType = UPPER
                        else:
                            curveType = LOWER 
                        px, py = [], []
                    elif "end" in line:
                        if not px : raise ValueError("Start line missing")
                        if "top"    in line and curveType == LOWER: raise ValueError ("Missing 'Bottom End'")  
                        if "bottom" in line and curveType == UPPER: raise ValueError ("Missing 'Bottom Top'") 
                        self.set_newSide_for (curveType, px,py)
                    else:     
                        splitline = line.strip().split()
                        if len(splitline) == 1:                        # couldn't split line - try tab as separator
                            splitline = line.strip().split("\t")
                        if len(splitline) >= 2:                     
                            px.append (float(splitline[0].strip()))
                            py.append (float(splitline[1].strip()))
        except ValueError as e:
            ErrorMsg ("While reading Bezier file '%s': %s " %(fromPath,e )) 
            return 0 
         
        self._name = new_name
        self._isLoaded = True 

        logging.debug (f"Bezier definition for {self.name} loaded")

        return True  

        

    def _write_to_file (self):
        # writes x,y to file in 

        #  .dat-format (normal airfoil write 
        super()._write_to_file ()

        #  .bez-format for CAD etc and 

        # filename - remove .dat - add .bez 
        with open(self.pathFileName_bezier, 'w+') as file:

            # airfoil name 
            file.write("%s\n" % self.name)

            file.write("Top Start\n" )
            for p in self.geo.upper.controlPoints:
                file.write("%13.10f %13.10f\n" %(p[0], p[1]))
            file.write("Top End\n" )

            file.write("Bottom Start\n" )
            for p in self.geo.lower.controlPoints:
                file.write("%13.10f %13.10f\n" %(p[0], p[1]))
            file.write("Bottom End\n" )

            file.close()


    def asCopy (self, pathFileName = None, 
                name=None, nameExt=None,
                geometry=None) -> 'Airfoil':
        """
        returns a copy of self 

        Args:
            pathFileName: optional - string of existinng airfoil path and name 
            name: optional         - name of airfoil - no checks performed 
            nameExt: -optional     - will be appended to self.name (if name is not provided)
            geometry: - not supported - 
        """
        # overloaded as Bezier needs a special copy, no other geometry supported

        if pathFileName is None and name is None: 
            pathFileName = self.pathFileName

        if name is None:
            name = self.name + nameExt if nameExt else self.name

        if geometry is not None: 
            raise ValueError ("Airfoil_Bezier does not support new geometry class")

        airfoil =  Airfoil_Bezier (name = name, pathFileName = pathFileName)

        airfoil.geo.upper.set_controlPoints (self.geo.upper.controlPoints)
        airfoil.geo.lower.set_controlPoints (self.geo.lower.controlPoints)
        airfoil.set_isLoaded (True)

        return airfoil 


#------------------------------------------------------

class Airfoil_Hicks_Henne(Airfoil):
    """ 

    Airfoil based on a seed airfoil and hicks henne bump (hh) functions for upper and lower side 

    """

    isHicksHenneBased  = True


    def __init__(self, name = None, pathFileName=None, workingDir= None):
        """
        Main constructor for new Airfoil

        Args:
            pathFileName: optional - string of existinng airfoil path and name \n
            name: optional - name of airfoil - no checks performed 
        """
        super().__init__( name = name, pathFileName=pathFileName, workingDir=workingDir)

        self._geometryClass  = Geometry_HicksHenne  # geometry strategy 
        self._isLoaded       = False                # hicks henne definition loaded? 

    @property
    def pathFileName_hh (self) -> str: 
        """ pathfileName of the hh definition file """
        if self.pathFileName:  
            return os.path.splitext(self.pathFileName)[0] + ".hicks"
        else: 
            return None 

    @property
    def isLoaded (self): 
        # overloaded
        return self._isLoaded
    def set_isLoaded (self, aBool: bool):
        self._isLoaded = aBool


    @property
    def geo (self) -> Geometry_HicksHenne:
        """ the geometry strategy of self"""
        return self._geo

    def set_geo (self, geometry: Geometry_Bezier):
        """ set new geometry hicks henne object  """
        if not isinstance (geometry, self._geometryClass): return 
        self._geo = geometry  


    def set_xy (self, x, y):
        """ hh - do nothing """

        # overloaded - hh geometry is master of data 
        pass

    @property
    def x (self):
        # overloaded  - take from geometry hh 
        return self.geo.x

    @property
    def y (self):
        # overloaded  - take from geometry hh  (seed airfoil and hicks henne are added)
        return self.geo.y

    # -----------------

    def reset (self): 
        """ make child curves like thickness or camber invalid """
        self.geo._reset_lines()


    def load (self):
        """
        Overloaded: Loads hicks henne definition instead of .dat from file" 
        """    

        if self.isExisting and not self.isLoaded: 
            sourcePathFile = os.path.join(self.workingDir, self.pathFileName)
        else:
            sourcePathFile = None 

        return self.load_hh(fromPath=sourcePathFile)


    def load_hh (self, fromPath=None):
        """
        Loads hicks henne definition from file. 
        """    

        if fromPath is None: 
            fromPath = self.pathFileName

        name, seed_name, seed_x, seed_y, top_hhs, bot_hhs = self._read_hh_file (fromPath)

        self.set_hh_data (name, seed_name, seed_x, seed_y, top_hhs, bot_hhs)



    def set_hh_data (self, name, seed_name, seed_x, seed_y, top_hhs, bot_hhs): 
        """ set all data needed for a Hicks Henne airfoil"""

        self.set_name (name)

        if seed_name: 

            seed_foil = Airfoil (x=seed_x, y=seed_y, name=seed_name)

            if seed_foil.isLoaded: 
                self._geo = Geometry_HicksHenne (seed_foil.x, seed_foil.y)
                self._geo.upper.set_hhs (top_hhs)
                self._geo.lower.set_hhs (bot_hhs)

                self._isLoaded = True 
                # logging.debug (f"Hicks Henne definition for {self.name} loaded")
            else: 
                ErrorMsg (f"Hicks Henne seed airfoil {seed_name} couldn't be loaded ")



    def _read_hh_file (self, fromPath):
        """
        reads hicks henne definition from file. 
        """    

        from spline import HicksHenne

        with open(fromPath, 'r') as file:            

            file_lines = file.readlines()

        
        # format of bezier airfoil file 

        # <airfoil name> 
        # Top Start
        # 0.000strength000000000 0.0000location0000000  0.0000width0000000
        # ...
        # Top End
        # Bottom Start
        # ... 
        # Bottom End
        # Seedfoil Start 
        # 'seed airfoil name'
        #  1.000000 0.000000
        #  ...      ...

        name = ''                                # name of airfoil  
        seed_name = ''                           # name of seed airfoil 
        x,y = [], []                             # x,y of sedd
        top_hhs = []                             # array of hh functions 
        bot_hhs = []
        curveType = None

        try: 
            hhs = []
            for i, line in enumerate(file_lines):

                line_low = line.lower()

                if i == 0: 

                    name = line.strip()

                elif "seedfoil start" in line_low:

                    seed_name, x, y = self._loadLines (file_lines [i+1:])

                elif "start" in line_low:

                    if "top" in line_low: 
                        curveType = UPPER
                    else:
                        curveType = LOWER 
                    hhs = []

                elif "end" in line_low:

                    if not curveType : raise ValueError("Start line missing")
                    if "top"    in line_low and curveType == LOWER: raise ValueError ("Missing 'Bottom End'")  
                    if "bottom" in line_low and curveType == UPPER: raise ValueError ("Missing 'Bottom Top'") 

                    if curveType == LOWER:
                        bot_hhs = hhs
                    else: 
                        top_hhs = hhs 
                    curveType = None

                else:     

                    splitline = line.split()
                    if len(splitline) == 3:                     
                        strength = float(splitline[0].strip())
                        location = float(splitline[1].strip())
                        width    = float(splitline[2].strip())
                        hhs.append (HicksHenne (strength, location, width ))
        except ValueError as e:
            ErrorMsg ("While reading Hicks Henne file '%s': %s " %(fromPath,e ))   
         
        return name, seed_name, x, y, top_hhs, bot_hhs   




# ------------ test functions - to activate  -----------------------------------


# def test_adapt_bezier (): 

#     import matplotlib.pyplot as plt
#     from airfoil_examples import Root_Example, Tip_Example

#     fig, ax1 = plt.subplots(1, 1, figsize=(16,6))
#     ax1.grid(True)
#     ax1.axis("equal")

#     air      = Airfoil_Bezier ()
#     air_org  = Airfoil_Bezier ()
#     air_seed = Root_Example()

#     air.upper.adapt_bezier_to (air_seed.upper)
#     air.lower.adapt_bezier_to (air_seed.lower)

#     ax1.plot(air.x,      air.y,      label="Bezier optimized")
#     ax1.plot(air_seed.x, air_seed.y, label=air_seed.name)
#     ax1.plot(air_org.x,  air_org.y,  label="Bezier default")

#     ax1.legend()
#     plt.show()

#     pass



if __name__ == "__main__":

    # test_adapt_bezier()
    pass  
