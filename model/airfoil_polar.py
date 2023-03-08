#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Airfoil and polar objects 

- helper functions 

"""

import os
from common_utils import * 
import numpy as np

from worker import XfoilWorker


class Airfoil:
    """ 

    Airfoil object to handle a airfoil direct related things  

    """

    isStrakAirfoil = False
    isBlended = False       # remove
    isExample = False

    def __init__(self, pathFileName = '', name = ''):
        """
        Main constructor for new Airfoil

        Args:
            :pathFileName: optional - string of existinng airfoil path and name \n
            :name: optional - name of airfoil - no checks performed 
        """

        self.pathFileName = None
        self.name = name
        self.sourceName = None                     # the long name out of the two blended airfoils (TSrakAirfoil)

        self.x = []
        self.y = []
        self.polarSet = polarSet(self)

        if (pathFileName): 
            if not os.path.isfile(pathFileName):
                ErrorMsg ("Airfoil file \'%s\' does not exist. Couldn\'t create Airfoil" % pathFileName)
            else:
                self.pathFileName = pathFileName
                self.name = os.path.splitext(os.path.basename(self.pathFileName))[0]
        elif (not name):
            self.name = "-- ? --"



    @classmethod
    def onDict(cls, dataDict):
        """
        Alternate constructor for new Airfoil based on dictionary 

        Args:
            :dataDict: dictionary with "name" and "file" keys
        """
        pathFileName  = fromDict(dataDict, "file", None)
        name          = fromDict(dataDict, "name", None)
        return cls(pathFileName = pathFileName, name = name)

    def _save (self, airfoilDict):
        """ stores the variables into the dataDict - returns the filled dict"""
        # will be overloaded 
        toDict (airfoilDict, "file",    self.pathFileName) 
 

    def __repr__(self) -> str:
        # overwrite to get a nice print string 
        if (self.isBlended): info = f"Blended \'{self.name}\'"
        else:                info = f"\'{self.name}\'"
        return f"{type(self).__name__} {info}"

    @property
    def isExisting (self):
        return not self.pathFileName is None
    
    @property
    def isLoaded (self):
        return len(self.x) > 10

    #-----------------------------------------------------------

    def set_name (self, newName):
        """
        Set der name of the airfoil 

        Args:
            :newName: String like 'JX-GT-15'
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


    def polars (self):
        """
        Get polars of this airfoil from the polarSet

        Returns: 
            :List: of polar instances 
        """        
        return self.polarSet.polars() 
        

    def load (self, fromPath = None):
        """
        Loads airfoil coordinates from file. 
        pathFileName must be set before or fromPath must be defined.
        Load doesn't change self pathFileName
        """    

        if fromPath and os.path.isfile (fromPath):
            sourcePathFile = fromPath
        elif self.isExisting and not self.isLoaded: 
            sourcePathFile = self.pathFileName
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
        self.x = np.asarray (x)
        self.y = np.asarray (y)


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
        if dir and not os.path.isdir (dir):
            os.mkdir(dir)

        if not destName:
            if self.isStrakAirfoil:
                destName = self.sourceName              # strak: take the long name of the two airfoils
            else:
                destName = self.name

        newPathFileName = os.path.join (dir, destName) + '.dat'

        with open(newPathFileName, 'w+') as file:
            file.write("%s\n" % destName)
            for i in range (len(self.x)):
                file.write("%.7f %.7f\n" %(self.x[i], self.y[i]))
            file.close()

        return newPathFileName



    def plot(self):
        """
        Plot the airfoil for testing 
        """
        import matplotlib.pyplot as plt

        if (self.x == []): 
            ErrorMsg ("No coordinates to plot")
            return 

        fig = plt.figure()
        plt.style.use('seaborn-v0_8-ticks')
        fig.set_figwidth (fig.get_figwidth()  * 1.5)     # enlarge window because of 4 plots

        ax = fig.add_subplot(1, 1, 1)
        ax.set_xlim([0.0, 1.0])
        ax.set_xlabel('x')
        ax.set_ylabel('y')
        ax.axis('equal')
        ax.set_title (self.name)
        ax.grid()

        ax.plot(self.x, self.y, '-', color='grey')

        plt.subplots_adjust(left=0.10, bottom=0.10, right=0.95, top=0.90, wspace=None, hspace=None)
        plt.show()    


class Strak_Airfoil (Airfoil):
    """ Airfoil which is straked (blended) from it's neighbours"""

    isStrakAirfoil = True

    def __init__ (self):
        super().__init__()

        self.name = "<strak>" 
        self.sourceName = None          # the long name out of the two belended airfoils


    def _save (self, airfoilDict):
        """ stores the variables into the dataDict - returns the filled dict"""
        toDict (airfoilDict, "name",  self.name) 


    def do_strak (self,myChord, leftAir : Airfoil, leftChord, rightAir:Airfoil, rightChord ):
        """ straks (blends) self out of two airfoils to the left and right.
        depending on its chordlength compared to the real neighbours."""

        tmpDir = "tmp"
        blendBy  = (myChord - leftChord) / (rightChord - leftChord)

        leftPathFile  = leftAir.copyAs  (dir=tmpDir)
        rightPathFile = rightAir.copyAs (dir=tmpDir)

        newName = leftAir.name + ("_with_%.2f_" % blendBy) + rightAir.name

        result = XfoilWorker().blendAirfoils(leftPathFile, rightPathFile,blendBy, newName)

        if result == 0:
            newPathFile = os.path.join(tmpDir,newName) + '.dat'
            self.load (fromPath=newPathFile)

            self.sourceName = os.path.splitext(os.path.basename(newPathFile))[0]
        else: 
            ErrorMsg ("'xfoil_worker' couldn't be executed.")

        return


#------------------------------------------------------------------------------

class polarSet:
    """ 
    Manage the polars of an airfoil   

    airfoil 
        --> polarSet 
            --> polar   

    """
    def __init__(self, myAirfoil: Airfoil):
        """
        Main constructor for new polarset which belongs to an airfoil 

        Args:
            :myAirfoil: the airfoil object it belongs to  
        """
        self.__airfoil = myAirfoil 
        self.polars = []

        # set also default values for polar generation
        self.ncrit = 7
        self.valRange = [-3, 13, 0.25]
        self.spec_al = True
        self.polarType = 'T1'

    #---------------------------------------------------------------

    def load_or_generatePolars (self, reNumbers: list):
        """ 
        Either loads or (if not already exist) generate polars of myAirfoil for all renumbers.
        The polars are appended to the polarSet.
        Default values for ncrit, spec_al und range will be used    
        
        Args: 
            :reNumbers: array of ReNumbers (float)
        """

        reNumberNotExist = reNumbers
        generatedPolars = []
        loadedPolars = []
        myWorker = XfoilWorker()

        # which polars are already generated?
        polarFilesExist = myWorker.get_existingPolarFiles (self.__airfoil.pathFileName, reNumbers)

        # import directly already existung polars
        for polarFile in polarFilesExist:
            newPolar = polar(self)
            newPolar.import_FromFile(polarFile)
            loadedPolars.append (newPolar)
            if (reNumberNotExist): reNumberNotExist.remove (newPolar.re)
        
        # are there still missing reNumbers? Generate 
        if (reNumberNotExist != []): 
            generatedPolars = self.__generatePolars (reNumberNotExist)

        self.polars.extend (loadedPolars)
        self.polars.extend (generatedPolars)

        return  loadedPolars + generatedPolars


    def __generatePolars (self, reNumbers: list):
        """ 
        Generate polars of myAirfoil for all renumbers.
        Does not append them to polarSet!
        Default values for ncrit, spec_al und range will be used    
        
        Args: 
            :reNumbers: array of ReNumbers (float)
        """
        polarFiles = []
        generatedPolars = []

        myWorker = XfoilWorker()

        # let worker generate new polar files 
        myAirfoilFileName = self.__airfoil.fileName
        polarFiles = myWorker.generatePolars (myAirfoilFileName, reNumbers)

        # append new polar to my polars
        for polarFile in polarFiles:
            newPolar = polar(self)
            newPolar.import_FromFile(polarFile)
            generatedPolars.append (newPolar)

        return generatedPolars


    def plot(self):
        """
        Plot the polars of polarset for testing 
        """
        import matplotlib.pyplot as plt
        from cycler import cycler

        myPolars = self.polars
        if (myPolars == []): 
            ErrorMsg ("No polars to plot")
            return

        plt.rc('axes', prop_cycle= cycler('color', ['c', 'm', 'y', 'k']))  # shaded polars
        plt.style.use('seaborn-v0_8-ticks')
        fig = plt.figure()
        fig.set_figwidth (fig.get_figwidth()  * 1.5)     # enlarge window because of 4 plots
        fig.set_figheight(fig.get_figheight() * 1.5)     # enlarge window because of 4 plots

        ((ax1, ax2), (ax3, ax4)) = fig.subplots(2, 2)

        ax1.set_xlim([0.0025, 0.04])
        ax1.set_xlabel('cd')
        ax1.set_ylabel('cl')
        ax1.grid()
        for polar in myPolars:  ax1.plot(polar.cd, polar.cl, '-', label='%s' %polar)
        ax1.legend()

        ax2.set_ylabel('cl')
        ax2.set_xlabel('alpha')
        ax2.grid()
        for polar in myPolars:  ax2.plot(polar.alpha, polar.cl, '-', label='%s' %polar)

        ax3.set_ylabel('cl/cd')
        ax3.set_xlabel('cl')
        ax3.grid()
        for polar in myPolars:  ax3.plot(polar.cl, polar.clcd , '-', label='%s' %polar)
        
        ax4.set_ylabel('cm')
        ax4.set_xlabel('cl')
        ax4.grid()
        for polar in myPolars:  ax4.plot(polar.cl, polar.cm, '-', label='%s' %polar)

        plt.subplots_adjust(left=0.07, bottom=0.07, right=0.97, top=0.97, wspace=0.2, hspace=0.2)
        plt.show()    


#------------------------------------------------------------------------------

class polar:
    """ 
    A single polar of an airfoil created bei Xfoil(Worker)  

    airfoil 
        --> polarSet 
            --> polar   
    """
    def __init__(self, mypolarSet: polarSet):
        """
        Main constructor for new polar which belongs to a polarset 

        Args:
            :mypolarSet: the polarSet object it belongs to  
        """
        self.polarSet = mypolarSet
        self.alpha = []
        self.cl = []
        self.cd = []
        self.clcd = []
        self.cm = [] 
        self.cd = [] 
        self.cdp = [] 
        self.xtrt = []
        self.xtrb = []

        self.re = 0
        self.ncrit = 0 

    def __repr__(self) -> str:
        # overwrite to get a nice print string wie polarType and Re
        return self.polarSet.polarType + ' '+ type(self).__name__ + ' Re=%d' % self.re

    #--------------------------------------------------------

    def import_FromFile(self, polarPathFileName):
        """
        Read data for self from an Xfoil polar file  

        Args:
            :polarPathFileName: the full path and filename of polarfile
        """
        BeginOfDataSectionTag = "-------"
        airfoilNameTag = "Calculated polar for:"
        reTag = "Re ="
        ncritTag = "Ncrit ="
        parseInDataPoints = 0
        InfoMsg("importing polar %s..." %polarPathFileName)

        fpolar = open(polarPathFileName)

        # parse all lines
        for line in fpolar:

            # scan for airfoil-name
            if  line.find(airfoilNameTag) >= 0:
                splitline = line.split(airfoilNameTag)
                self.airfoilname = splitline[1].strip()
            # scan for Re-Number and ncrit
            if  line.find(reTag) >= 0:
                splitline = line.split(reTag)
                splitline = splitline[1].split(ncritTag)

                re_string    = splitline[0].strip()
                splitstring = re_string.split("e")
                faktor = float(splitstring[0].strip())
                Exponent = float(splitstring[1].strip())
                self.re = faktor * (10**Exponent)

                ncrit_string = splitline[1].strip()
                self.ncrit = float(ncrit_string.strip())

            # scan for start of data-section
            if line.find(BeginOfDataSectionTag) >= 0:
                parseInDataPoints = 1
            else:
                # get all Data-points from this line
                if parseInDataPoints == 1:
                    # split up line detecting white-spaces
                    splittedLine = line.split(" ")
                    # remove white-space-elements, build up list of data-points
                    dataPoints = []
                    for element in splittedLine:
                        if element != '':
                            dataPoints.append(element)
                    self.alpha.append(float(dataPoints[0]))
                    self.cl.append(float(dataPoints[1]))
                    self.cd.append(float(dataPoints[2]))
                    self.cdp.append(float(dataPoints[3]))
                    self.cm.append(float(dataPoints[4]))
                    self.xtrt.append(float(dataPoints[5]))
                    self.xtrb.append(float(dataPoints[6]))
                    if (self.cd [-1] != 0.0):
                        self.clcd.append (self.cl [-1] / self.cd [-1])
                    else:
                        self.clcd = 0.0 

        fpolar.close()




# Main program for testing -----------------------------------

if __name__ == "__main__":

    from .worker import XfoilWorker

    # ---- Test -----
    # loadFromFile = False

    # myAirfoil = 
    # print ("New airfoil created: ", myAirfoil)
    # myAirfoil.load()
    # myAirfoil.plot()

    # print ("Starting polar generation")
    # myAirfoil.polarSet.load_or_generatePolars ([200000, 220000, 270000, 500000])

    # print ("Generated or loaded polars: ", myAirfoil.polarSet.polars)
    # myAirfoil.polarSet.plot()



