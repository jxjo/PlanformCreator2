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

def blendTest():

    from airfoil_examples import Root_Example, Tip_Example
    air1 = Root_Example()
    air1.load()
    air2 = Tip_Example()
    air2.load()

    airStrak = Airfoil_Interpolated.fromAirfoil(air1)
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

    from worker_driver import XfoilWorker
    from airfoil_examples import Root_Example
    import matplotlib.pyplot as plt


    # ---- Test -----
    # loadFromFile = False
    blendTest()
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



