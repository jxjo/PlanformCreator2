#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Airfoil and polar objects 

- helper functions 

"""

import os
import numpy as np

from common_utils import * 
from airfoil import * 
from worker_driver import XfoilWorker   


#------------------------------------------------------------------------------

class PolarSet:
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
        self._airfoil = myAirfoil 
        self.polars = []

        # set also default values for polar generation
        self.ncrit = 7
        self.valRange = [-4, 13, 0.25]
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
        polarFilesExist = myWorker.get_existingPolarFiles (self._airfoil.pathFileName, reNumbers,
                                    polarType = self.polarType, ncrit = self.ncrit)

        # import directly already existung polars
        for polarFile in polarFilesExist:
            newPolar = Polar(self)
            newPolar.import_FromFile(polarFile)
            loadedPolars.append (newPolar)
            if (reNumberNotExist): reNumberNotExist.remove (newPolar.re)
        
        # are there still missing reNumbers? Generate 
        if (reNumberNotExist != []): 
            generatedPolars = self._generatePolars (reNumberNotExist)

        self.polars.extend (loadedPolars)
        self.polars.extend (generatedPolars)

        return  loadedPolars + generatedPolars


    def _generatePolars (self, reNumbers: list):
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
        polarFiles = myWorker.generatePolars (self._airfoil.pathFileName, reNumbers, 
                                              polarType = self.polarType, ncrit = self.ncrit, valRange = self.valRange)

        # append new polar to my polars
        for polarFile in polarFiles:
            newPolar = Polar(self)
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
        for polar in myPolars:  ax3.plot(polar.cl, polar.glide , '-', label='%s' %polar)
        
        ax4.set_ylabel('cm')
        ax4.set_xlabel('cl')
        ax4.grid()
        for polar in myPolars:  ax4.plot(polar.cl, polar.cm, '-', label='%s' %polar)

        plt.subplots_adjust(left=0.07, bottom=0.07, right=0.97, top=0.97, wspace=0.2, hspace=0.2)
        plt.show()    


#------------------------------------------------------------------------------

class Polar:
    """ 
    A single polar of an airfoil created bei Xfoil(Worker)  

    airfoil 
        --> polarSet 
            --> polar   
    """
    def __init__(self, mypolarSet: PolarSet):
        """
        Main constructor for new polar which belongs to a polarset 

        Args:
            :mypolarSet: the polarSet object it belongs to  
        """
        self.polarSet = mypolarSet

        self._opPoints = []                     # the single opPoins of self
        self._alpha = []
        self._cl = []
        self._cd = []
        self._cm = [] 
        self._cd = [] 
        self._xtrt = []
        self._xtrb = []

        self.re = 0
        self.ncrit = 0 

    def __repr__(self) -> str:
        # overwrite to get a nice print string wie polarType and Re
        return self.polarSet.polarType + ' Re=%dk ncrit=%d' % (int(self.re/1000), self.ncrit)

    #--------------------------------------------------------

    @property
    def opPoints (self) -> list:
        """ returns the sorted list of opPoints of self """
        return self._opPoints
    
    def set_opPoints (self, opPoints_new):
        """ set list of opPoints of self """
         
        nPoints = len(opPoints_new)
        if nPoints == 0: return 

        self._alpha = [0] * nPoints
        self._cl    = self._alpha.copy()
        self._cd    = self._alpha.copy()
        self._glide = self._alpha.copy()
        self._cm    = self._alpha.copy()
        self._cd    = self._alpha.copy()
        self._xtrt  = self._alpha.copy()
        self._xtrb  = self._alpha.copy()

        # fill the cached array 
        op : OpPoint
        for i, op in enumerate(opPoints_new):
            self._alpha[i] = op.alpha
            self._cl[i]    = op.cl
            self._cd[i]    = op.cd
            self._glide[i] = op.glide
            self._cm[i]    = op.cm
            self._cd[i]    = op.cd
            self._xtrt[i]  = op.xtrt
            self._xtrb[i]  = op.xtrb
    

    @property
    def alpha (self) -> list:
        return self._alpha
    
    @property
    def cl (self) -> list:
        return self._cl
    
    @property
    def cd (self) -> list:
        return self._cd
    
    @property
    def glide (self) -> list:
        return self._glide
    
    @property
    def cm (self) -> list:
        return self._cm
    
    @property
    def xtrt (self) -> list:
        return self._xtrt
    
    @property
    def xtrb (self) -> list:
        return self._xtrb
    


    #--------------------------------------------------------

    def import_FromFile(self, polarPathFileName):
        """
        Read data for self from an Xfoil polar file  

        Args:
            :polarPathFileName: the full path and filename of polarfile
        """

        opPoints = []

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
                    op = OpPoint ()
                    op.alpha = float(dataPoints[0])
                    op.cl = float(dataPoints[1])
                    op.cd = float(dataPoints[2])
                    # cdp = float(dataPoints[3])
                    op.cm = float(dataPoints[4])
                    op.xtrt = float(dataPoints[5])
                    op.xtrb = float(dataPoints[6])

                    opPoints.append(op)
        fpolar.close()

        # guarantee opPoints are sorted based on spec 
        if opPoints[0].spec == 'alpha':
            opPoints.sort (key= lambda op: op.alpha)
        else: 
            opPoints.sort (key= lambda op: op.cl)

        self.set_opPoints (opPoints)



class OpPoint:
    """ 
    A single (operating) point of a polar of an airfoil   

    airfoil 
        --> polarSet 
            --> polar   (1..n) 
                --> opPoint  (1..n) 
    """
    def __init__(self):
        """
        Main constructor for new opPoint 

        Args:
            :-
        """
        self.spec   = 'alpha'                   # self based on 'alpha' or 'cl'
        self.valid  = True                      # has it converged during xfoil calculation
        self.alpha  = None
        self.cl     = None
        self.cd     = None
        self.cm     = None 
        self.xtrt   = None                      # transition top side
        self.xtrb   = None                      # transition bot side

    @property
    def glide (self): 
        if self.cd and self.cl:                 # cd != 0.0  
            return self.cl/self.cd  
        else: 
            return 0.0 


# Main program for testing -----------------------------------


if __name__ == "__main__":

    # from worker_driver import XfoilWorker
    from airfoil_examples import Root_Example
    import matplotlib.pyplot as plt


    # ---- Test -----
    # loadFromFile = False
    myAirfoil = Root_Example()
    print ("Airfoil loaded: ", myAirfoil)
    myAirfoil.saveAs (dir='test')

    print ("Starting polar generation")
    myPolarSet = PolarSet (myAirfoil)
    myPolarSet.load_or_generatePolars ([200000, 300000, 400000, 600000])

    print ("Generated or loaded polars: ", myPolarSet.polars)
    myPolarSet.plot()



