#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""

    Airfoil and operations on it 

"""
import os
from typing                 import Type, override
from enum                   import StrEnum
from pathlib                import Path

import numpy as np

from base.math_util         import * 
from base.common_utils      import * 
from model.airfoil_geometry import Geometry_Splined, Geometry, Geometry_Bezier, Geometry_HicksHenne
from model.airfoil_geometry import Line, Side_Airfoil_Bezier

import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


#-------------------------------------------------------------------------------
# enums   
#-------------------------------------------------------------------------------

class usedAs (StrEnum):
    """ airfoil types for some usage semantics in application """
    NORMAL      = ""
    SEED        = "Seed"
    SEED_DESIGN = "Seed of design"
    REF1        = "Reference 1" 
    REF2        = "Reference 2" 
    DESIGN      = "Design"
    TARGET      = "Target"
    FINAL       = "Final"

# geometry specification 

GEO_BASIC  = Geometry
GEO_SPLINE = Geometry_Splined




#--------------------------------------------------------------------------

class Airfoil:
    """ 

    Airfoil object to handle a airfoil direct related things  

    """
    isBlendAirfoil      = False
    isEdited            = False
    isExample           = False                      # vs. Example_Airfoil 
    isBezierBased       = False
    isHicksHenneBased   = False


    def __init__(self, x= None, y = None, name = None,
                 geometry : Type[Geometry]  = None, 
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
        self._name          = name if name is not None else ''
        self._name_org      = None                 # will hold original name for modification label
        self._fileName_org  = None                 # will hold original fileName 
        self.sourceName     = None                 # long name out of the two blended airfoils (TSrakAirfoil)

        if not x is None: x = x if isinstance(x,np.ndarray) else np.asarray (x)
        self._x     = x
        if not y is None: y = y if isinstance(y,np.ndarray) else np.asarray (y)
        self._y     = y  

        self._isModified     = False
        self._isEdited       = False 
        self._isBlendAirfoil = False             # is self blended from two other airfoils 

        if geometry is None: 
            self._geometryClass  = GEO_SPLINE          # geometry startegy 
        else:
            self._geometryClass  = geometry          # geometry startegy 
        self._geo            = None              # selfs instance of geometry

        self._nPanelsNew     = None              # repanel: no of panels - init via default
        self._le_bunch       = None              # repanel: panel bunch at leading edge
        self._te_bunch       = None   	         # repanel: panel bunch at trailing edge

        self._polarSet       = None              # polarSet which is defined from outside 

        self._usedAs         = usedAs.NORMAL     # usage type of airfoil used by app <- AIRFOIL_TYPES
        self._propertyDict   = {}                # multi purpose extra properties for an AIirfoil


        # pathFileName must exist if no coordinates were given 

        if (pathFileName is not None) and  (x is None or y is None): 
            pathFileName = os.path.normpath(pathFileName)       # make all slashes to double back
            if os.path.isabs (pathFileName):
                checkPath = pathFileName
            else:
                checkPath = os.path.join (self.workingDir, pathFileName)
            if not os.path.isfile(checkPath):
                self._name = "-- Error --"
                raise ValueError ("Airfoil file '%s' does not exist. Couldn't create Airfoil" % checkPath)
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
    def onFileType(cls, pathFileName, workingDir = None, geometry : Type[Geometry]  = None):
        """
        Alternate constructor for new Airfoil based on its file type

            '.dat'      - returns Airfoil 
            '.bez'      - returns Airfoil_Bezier 
            '.hicks'    - returns Airfoil_Hicks_Henne 

        Args:
            pathFileName: string of existinng airfoil path and name
            workingDir: optional working dir (if path is relative)
            geometry : geometry tyoe - only for .dat files 
        """

        ext = os.path.splitext(pathFileName)[1]

        if ext == '.dat': 
            return Airfoil (pathFileName=pathFileName, workingDir=workingDir, geometry=geometry)
        elif ext == '.bez': 
            return Airfoil_Bezier (pathFileName=pathFileName, workingDir=workingDir)
        elif ext == '.hicks': 
            return Airfoil_Hicks_Henne (pathFileName=pathFileName, workingDir=workingDir)
        else:
            raise ValueError (f"Unknown file extension '{ext}' for new airfoil")



    def _save (self, airfoilDict):
        """ stores the variables into the dataDict - returns the filled dict"""
        
        if self.isBlendAirfoil:
            toDict (airfoilDict, "name", self.name) 
        else:
            toDict (airfoilDict, "file", self.pathFileName) 
        return airfoilDict
 

    def __repr__(self) -> str:
        # overwritten to get a nice print string 
        info = f"'{self.name}'"
        return f"<{type(self).__name__} {info}>"


    def _handle_geo_changed (self):
        """ callback from geometry when it was changed (by user) """

        # load new coordinates from modified geometry 
        self._x = self.geo.x
        self._y = self.geo.y

        # set new name
        if not self._name_org : self._name_org = self.name

        self.set_name (self._name_org + self.geo.modifications_as_label)

        # set new filename 
        if self._fileName_org is None: self._fileName_org = self.fileName
        fileName_without = os.path.splitext(self._fileName_org)[0]
        fileName_ext     = os.path.splitext(self._fileName_org)[1]
        self.set_fileName (fileName_without + self.geo.modifications_as_label + fileName_ext)

        self.set_isModified (True)
        logging.debug (f"{self} - geometry changed: {self.geo.modifications_as_label} ")


    # ----------  Properties ---------------

    @property
    def x (self): return self._x

    @property
    def y (self): return self._y


    @property
    def geo (self) -> Geometry:
        """ the geometry strategy of self"""
        if self._geo is None: 
            self._geo = self._geometryClass (self.x, self.y,
                                             onChange = self._handle_geo_changed)
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
    def name (self): 
        """ name of airfoil - when it is modified including modifications description"""
        return self._name 
        
    def set_name (self, newName, reset_original=False):
        """  Set name of the airfoil. 'reset_original' will also overwrite original filename  
        Note:  This will not rename an existing airfoil (file)...
        """
        self._name = newName

        if not self._name_org or reset_original: 
            self._name_org = self.name

        self.set_isModified (True)

    @property
    def name_short (self):
        """ name of airfoil shortend at the beginning to 23 chars"""
        if len(self.name) <= 23:    return self.name
        else:                       return "..." + self.name[-20:]


    @property
    def polarSet (self):
        """ Property which is set from outside - Airfoil doesn't know about it... """ 
        return self._polarSet 
    def set_polarSet (self, aPolarSet):
        self._polarSet = aPolarSet


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
    def isBlendAirfoil (self):
        """ is self blended out of two other airfoils"""
        return self._isBlendAirfoil
    def set_isBlendAirfoil (self, aBool): 
        self._isBlendAirfoil = aBool
    
    @property
    def nPanels (self): 
        """ number of panels """
        return self.geo.nPanels
      
    @property
    def nPoints (self): 
        """ number of coordinate points"""
        return self.geo.nPoints 

        
    @property
    def isSymmetrical(self):
        """ true if max camber is 0.0 - so it's a symmetric airfoil"""
        return self.geo.isSymmetrical
    

    @property
    def usedAs (self):
        """ usage type (enum usedAs) of self like DESIGN"""
        return self._usedAs
    def set_usedAs (self, aType): 
        if aType in usedAs:
            self._usedAs = aType

    @property
    def usedAsDesign (self): 
        """ short for self used as DESIGN """ 
        return self._usedAs == usedAs.DESIGN

    def useAsDesign (self, aBool=True): 
        """ set usedAs property to DESIGN"""
        if aBool: 
            self.set_usedAs (usedAs.DESIGN)
        else: 
            self.set_usedAs (usedAs.NORMAL)

    #-----------------------------------------------------------


    def set_pathFileName (self,fullPath, noCheck=False):
        """
        Set fullpaths of airfoils location and file \n
        ! This will not move or copy the airfoil physically - use copyAs instead
        """
        if noCheck or (os.path.isfile(fullPath)):
            self.pathFileName = fullPath
        else:
            raise ValueError ("Airfoil \'%s\' does not exist. Couldn\'t be set" % fullPath)


    def set_pathName (self, aDir : str, noCheck=False):
        """
        Set fullpaths of airfoils directory \n
        ! This will not move or copy the airfoil physically
        """
        if noCheck or (os.path.isdir(aDir)):
            self.pathFileName = os.path.join (aDir, self.fileName)
        else:
            raise ValueError ("Directory \'%s\' does not exist. Couldn\'t be set" % aDir)

    @property
    def fileName (self):
        """ filename of airfoil like 'JX-GT-15.dat' """
        if self.pathFileName is None: return '' 
        return os.path.basename(self.pathFileName) 

    def set_fileName (self, aFileName : str):
        """ set new fileName """
        if not aFileName: return 

        self.pathFileName = os.path.join (self.pathName, aFileName)

    def set_name_from_fileName (self):
        """ set current fileName as name of airfoil """
        self.set_name (os.path.splitext(self.fileName)[0])

    def set_fileName_from_name (self):
        """ set current fileName as name of airfoil """
        self.set_fileName (self.name + os.path.splitext(self.fileName)[1]) 


    @property
    def pathName (self):
        """
        directory pathname of airfoil like '..\\myAirfoils\\'
        """
        if not self.pathFileName is None: 
            return os.path.dirname(self.pathFileName) 
        else:
            return ''

    @property
    def pathName_abs (self):
        """
        absolute directory pathname of airfoil like '\\root\\myAirfoils\\'
        """
        if not self.pathFileName is None: 
            return os.path.dirname(os.path.abspath(self.pathFileName))
        else:
            return os.path.dirname(os.getcwd())


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
                        logging.warning ("Airfoil '%s' has duplicate coordinates - skipped." % self._name)
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
            self._write_dat_to_file ()
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



    def save_copyAs (self, dir = None, destName = None, te_gap=None ):
        """
        Write a copy of self to destPath and destName (the airfoil can be renamed).
        Self remains with its current values.
        Optionally a new te_gap may be defined for the exported airfoil  

        Args: 
            dir: -optional- new directory for the airfoil 
            destName: - optional- new name
            te_gap: -optional- new TE gap in x,y coordinates 

        Returns: 
            newPathFileName from dir and destName 
        """        

        # determine (new) airfoils name  if not provided
        if not destName:
            if self.isBlendAirfoil:
                destName = self.sourceName                     # Blend: take the long name of the two airfoils
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

        if te_gap is not None: 
            airfoil.geo.set_te_gap (te_gap)

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


    def _write_dat_to_file (self):
        """ writes .dat file of to self.pathFileName"""

        # ensure extension .dat (in case of Bezier) 
        pathFileName =  os.path.splitext(self.pathFileName)[0] + ".dat"

        with open(pathFileName, 'w+') as file:
            file.write("%s\n" % self.name)
            for i in range (len(self.x)):
                file.write("%.7f %.7f\n" %(self.x[i], self.y[i]))
            file.close()


    def normalize (self, just_basic=False):
        """
        Shift, rotate, scale airfoil so LE is at 0,0 and TE is symmetric at 1,y
        Returns True/False if normalization was done 

        'just_basic' will only normalize coordinates - not based on spline 
        """
        return self.geo.normalize(just_basic=just_basic)  


    def do_blend (self, airfoil1 : 'Airfoil', airfoil2 : 'Airfoil', blendBy : float,
                  geometry = None ):
        """ blends self out of two airfoils to the left and right
        depending on the blendBy factor
        
        Args: 
            geometry: optional - geo strategy for blend - either GEO_BASIC or GEO_SPLINE
        """
    
        # sanity - both airfoils must be loaded 
        if not airfoil1.isLoaded:
            raise ValueError ("Airfoil '" + airfoil1.name + "' isn't loaded. Cannot blend.")
        if not airfoil2.isLoaded:
            raise ValueError ("Airfoil '" + airfoil2.name + "' isn't loaded. Cannot blend.")

        if blendBy < 0.0: raise ValueError ("blendyBy must be >= 0.0")
        if blendBy > 1.0: raise ValueError ("blendyBy must be <= 1.0")

        # other geo strategy? 
        if not geometry is None and geometry != self._geometryClass:
            geo = geometry (self.x, self.y)
        else:
            geo = self.geo 

        geo.blend (airfoil1.geo, airfoil2.geo, blendBy)

        self.set_isBlendAirfoil (True)





#------------------------------------------------------

class Airfoil_Bezier(Airfoil):
    """ 

    Airfoil based on Bezier curves for upper and lower side 

    """

    isBezierBased  = True


    def __init__(self, name = None, pathFileName=None, workingDir= None,
                 cp_upper = None,
                 cp_lower = None):
        """
        Main constructor for new Bezier Airfoil

        Args:
            pathFileName: optional - string of existinng airfoil path and name 
            name: optional - name of airfoil - no checks performed 
        """
        super().__init__( name = name, pathFileName=pathFileName, workingDir=workingDir)

        self._geometryClass  = Geometry_Bezier      # geometry startegy 
        self._isLoaded       = False                # bezier definition loaded? 

        if cp_upper is not None: 
            self.geo.upper.set_controlPoints (cp_upper)
        if cp_lower is not None: 
            self.geo.lower.set_controlPoints (cp_lower)


    @staticmethod
    def onAirfoil (anAirfoil : Airfoil):
        """
        Alternate constructor for new Airfoil based on another airfoil 

        The new Bezier airfoil will just have a rough estimation for the Bezier curves,
        which have to be optimized with 'match bezier'
        """

        # new name and filename
        name = anAirfoil.name + '_bezier'
 
        # get estimated controlpoints for upper and lower 
        cp_upper = Side_Airfoil_Bezier.estimated_controlPoints (anAirfoil.geo.upper, 5)
        cp_lower = Side_Airfoil_Bezier.estimated_controlPoints (anAirfoil.geo.lower, 5)

        airfoil_new =  Airfoil_Bezier (name=name, cp_upper=cp_upper, cp_lower=cp_lower)

        # new pathFileName
        fileName_without = os.path.splitext(anAirfoil.fileName)[0]
        fileName_ext     = os.path.splitext(anAirfoil.fileName)[1] 
        pathFileName = os.path.join (anAirfoil.pathName, fileName_without + '_bezier' + fileName_ext)
        airfoil_new.set_pathFileName (pathFileName, noCheck=True)

        airfoil_new.set_isLoaded (True)

        return airfoil_new 


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
            self._geo = self._geometryClass (onChange = self._handle_geo_changed)
        return self._geo


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
                            side = Line.Type.UPPER
                        else:
                            side = Line.Type.LOWER 
                        px, py = [], []
                    elif "end" in line:
                        if not px : raise ValueError("Start line missing")
                        if "top"    in line and side == Line.Type.LOWER: raise ValueError ("Missing 'Bottom End'")  
                        if "bottom" in line and side == Line.Type.UPPER: raise ValueError ("Missing 'Bottom Top'") 
                        self.set_newSide_for (side, px,py)
                    else:     
                        splitline = line.strip().split()
                        if len(splitline) == 1:                        # couldn't split line - try tab as separator
                            splitline = line.strip().split("\t")
                        if len(splitline) >= 2:                     
                            px.append (float(splitline[0].strip()))
                            py.append (float(splitline[1].strip()))
        except ValueError as e:
            logging.error ("While reading Bezier file '%s': %s " %(fromPath,e )) 
            return 0 
         
        self._name = new_name
        self._isLoaded = True 

        logging.debug (f"Bezier definition for {self.name} loaded")

        return True  

    @override
    def save (self):
        """
        Basic save of self to its pathFileName
            - .bez file 
            - .dat file 
        """
        if self.isLoaded: 
            self._write_dat_to_file ()
            self._write_bez_to_file ()
            self.set_isModified (False)


    def _write_bez_to_file (self):
        """ write Bezier data to bez file """
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

        airfoil =  Airfoil_Bezier (name = name, pathFileName = pathFileName,
                                   cp_upper = self.geo.upper.controlPoints,
                                   cp_lower = self.geo.lower.controlPoints)
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
        if self._geo is None: 
            self._geo = self._geometryClass (onChange = self._handle_geo_changed)
        return self._geo


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

        self._name = name                       # don't use set_ (isModified)

        if seed_name and len(seed_x) and len(seed_y): 

            seed_foil = Airfoil (x=seed_x, y=seed_y, name=seed_name)

            if seed_foil.isLoaded: 
                self._geo = Geometry_HicksHenne (seed_foil.x, seed_foil.y)
                self._geo.upper.set_hhs (top_hhs)
                self._geo.lower.set_hhs (bot_hhs)

                self._isLoaded = True 
                logging.debug (f"Hicks Henne definition for {self.name} loaded")
            else: 
                logging.error (f"Hicks Henne seed airfoil {seed_name} couldn't be loaded ")
        else: 
            raise ValueError (f"Hicks Henne seed airfoil data missing for {name}")



    def _read_hh_file (self, fromPath):
        """
        reads hicks henne definition from file. 
        """    

        from base.spline import HicksHenne

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
        side = None

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
                        side = Line.Type.UPPER
                    else:
                        side = Line.Type.LOWER 
                    hhs = []

                elif "end" in line_low:

                    if not side : raise ValueError("Start line missing")
                    if "top"    in line_low and side == Line.Type.LOWER: raise ValueError ("Missing 'Bottom End'")  
                    if "bottom" in line_low and side == Line.Type.UPPER: raise ValueError ("Missing 'Bottom Top'") 

                    if side == Line.Type.LOWER:
                        bot_hhs = hhs
                    else: 
                        top_hhs = hhs 
                    side = None

                else:     

                    splitline = line.split()
                    if len(splitline) == 3:                     
                        strength = float(splitline[0].strip())
                        location = float(splitline[1].strip())
                        width    = float(splitline[2].strip())
                        hhs.append (HicksHenne (strength, location, width ))
        except ValueError as e:
            logging.error ("While reading Hicks Henne file '%s': %s " %(fromPath,e ))   
         
        return name, seed_name, x, y, top_hhs, bot_hhs   




# ------------ test functions - to activate  -----------------------------------

if __name__ == "__main__":

    pass  
