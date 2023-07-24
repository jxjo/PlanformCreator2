#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Proxy to xfoil_worker to execute common tasks

- blending wo airfoils 
- generating polars
- set TE gap
- smoothing

If xfoil_worker isn't in path-environment the path of workers location must be set 

   XfoilWorker.exePath = "xy\z\"
"""
import os

class XfoilWorker:

    exePath = ''
    workerName = 'xfoil_worker'

    _ckeckPath = os.path.dirname(__file__)
    if os.path.isfile(os.path.join(_ckeckPath, workerName +'.exe')) : 
        exePath = _ckeckPath 
        print (" - Xfoil_worker found in: ", _ckeckPath)
    else:
        print (" - No Xfoil_worker found in: ", _ckeckPath)
        _ckeckPath = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'xfoil_worker')
        if os.path.isfile(os.path.join(_ckeckPath, workerName +'.exe')) : 
            exePath = _ckeckPath 
            print (" - Xfoil_worker found in: ", _ckeckPath)
        else: 
            print (" - No Xfoil_worker found in: ", _ckeckPath, " - using OS search path")

    #def __init__(self):

    def blendAirfoils(self, airfoilFileName, withAirfoilFileName, byPercent, blendedAirfoil=''):
        """ blend one airfoil with another one """ 

        return (self.__execute_Worker ("blend", actionArg=str(byPercent), airfoil1=airfoilFileName, 
                                airfoil2=withAirfoilFileName, outname=blendedAirfoil))


    def setTEGap (self, airfoilFileName, TEGapInPercent, newAirfoil=''):
        """ set the trailing edge of airfoil""" 

        action = 'te=%.2f' % TEGapInPercent
        return (self.__execute_Worker ("set", actionArg=action, airfoil1=airfoilFileName, 
                                 outname=newAirfoil))


    def smooth (self, airfoil, newAirfoil=''):

        return (self.__execute_Worker ("smooth", airfoil1=airfoil, outname=newAirfoil))
        
    def showHelp(self):
        
        return (self.__execute_Worker ("help"))


    def generatePolars ( self, airfoilPathFileName, reNumbers: list[float], 
                            polarType = 'T1',
                            spec = 'alpha', 
                            valRange= [-3, 12, 0.25],
                            ncrit = 7):
        """ 
        Generate polars for airfoilPathFileName in directory of airfoil

        Args:
            :airfoilPathFileName: the airfoil path like 'myAirfoils\JX-GT-15.dat'  \n
            :reNumbers: a list of Re numbers                                   \n
            :optional:
                'polarType' either 1 for T1 polar or 2 ...                      \n
                'spec_al' True - polar is based on alpha values, False on cl    \n
                'valRange' [startValue, endValue, step] of polar according to spec_al \n
                'ncrit' ncrit of xfoil calculation                              \n
        """ 

        if (polarType == 'T2'):
            polarTypeNo = 2
        else: 
            polarTypeNo = 1

        if spec == 'alpha':
            spec_al = True
        else: 
            spec_al = False

        if os.path.isfile(airfoilPathFileName): 

            # a temporary input file for polar generation is created
            tmpInpFile = self.__generate_polarInputFile ( airfoilPathFileName, reNumbers, 
                                                          polarTypeNo, spec_al, valRange, ncrit) 

            # ... so xfoil_worker has some input to work on ...
            result = self.__execute_Worker ("polar", airfoil1=airfoilPathFileName, 
                                                     inputfile=tmpInpFile)
            if (tmpInpFile != ''): os.remove (tmpInpFile)

            # get all polar files in polar subdirectory 
            return self.get_existingPolarFiles (airfoilPathFileName, reNumbers, 
                            polarType = polarType, ncrit = ncrit)

        else: 
            raise Exception ("generatePolars: airfoil file %s doesn't exist" % airfoilPathFileName )


    def get_existingPolarFiles (self, airfoilPathFileName, reNumbers, polarType=None, ncrit=None):
        """ 
        Get exsting polar files of airfoilPathFileName 

        Args:
            :airfoilPathFileName: the airfoil path like 'myLib\JX-GT-15.dat'   \n
            :reNumbers: - optional Filter - - check only for this re numbers
            :polarType: - optional Filter - either T1 or T2                 \n
            :ncrit: - optional Filter -  ncrit of xfoil calculation           \n
        """ 
        from glob import glob
        import re

        # get all polar files in subdirectory
        polarDir = re.sub('.dat', '', airfoilPathFileName) + '_polars'
        polarFiles = [] 

        for file in glob (os.path.join(polarDir,"T*.txt")):
            if (reNumbers == []):
                matched = True
            else: 
                matched = False
                for reNum in reNumbers: 
                    reString = 'Re%.3f' % (reNum / 10**6)
                    if (file.find(reString) > 0): 
                        matched = True

            if (polarType and (file.find(polarType) == -1)): 
                matched = False
            if (ncrit and (file.find('_N'+ ('%.1f' % ncrit)) == -1)): 
                matched = False
            
            if matched:  polarFiles.append(file)

        return polarFiles


    def cd_airfoilDir ( self, airfoilPathFileName):
        """ 
        change directory to directory of airfoil 

        Args:
            :airfoilPathFileName: the airfoil path like 'myAirfoils\JX-GT-15.dat'  \n
        Returns:
            :airfoilFile: the filename of airfoil like 'JX-GT-15.dat'
            :curDir: the current directory (to return) later to
        """ 

        curDir = os.getcwd()
        airfoilPath, airfoilFileName = os.path.split(airfoilPathFileName)

        if (airfoilFileName != ''): 
            raise Exception ("No airfoil file to change to")

        if (airfoilPath != ''): 
            try: 
                os.chdir (airfoilPath)
            except: 
                print ("directory %s does not exist" % airfoilPath )

        return airfoilFileName, curDir


# ---------- Private --------------------------------------

    def __execute_Worker (self, action, actionArg='', airfoil1='', airfoil2='', outname='', inputfile=''):

        workerArgs = ''

        if (action != ''): 
            if (action == 'help'): 
                workerArgs += ' -h '
            else: 
                workerArgs += ' -w ' + action
        else: 
            print('Error: action is mandatory')
            return 1

        # change to dir of airfoil1 for execution
        airfoil1Path, airfoil1FileName = os.path.split(airfoil1)
        airfoil2FileName = airfoil2

        if (airfoil1Path != ''):
            curDir = os.getcwd()
            try: 
                os.chdir (airfoil1Path)
            except: 
                print ("directory %s does not exist" % airfoil1Path )
            # in this case also strip airfoil2 
            if (airfoil2 != ''):
                airfoil2Path, airfoil2FileName = os.path.split(airfoil2)

        # info inputfile is in a dir - strip dir from path - local execution 
        if (inputfile != ''):
            inpufilePath, localInputfile = os.path.split(inputfile)
        else: 
            localInputfile = '' 

        if (actionArg != ''): workerArgs += ' '     + actionArg
        if (airfoil1  != ''): workerArgs += ' -a '  + airfoil1FileName
        if (airfoil2  != ''): workerArgs += ' -a2 ' + airfoil2FileName
        if (outname   != ''): workerArgs += ' -o '  + outname
        if (inputfile != ''): workerArgs += ' -i '  + localInputfile

        result = os.system(os.path.join (XfoilWorker.exePath, XfoilWorker.workerName) + workerArgs)

        if (airfoil1Path != ''): os.chdir (curDir)

        return  result

    def __generate_polarInputFile ( self, airfoilPathFileName, reNumbers, polarType, spec_al, 
                                          valRange: list[float], ncrit):
        """ Generate a temporary polar input file for worker like this 

        &polar_generation
            generate_polars = .true.
            type_of_polar = 1
            op_mode = 'spec-al'
            op_point_range = -2.6, 11.0, 0.5
            polar_reynolds  = 230000, 300000, 400000
        /
        &xfoil_run_options
            ncrit = 7.0
        /
        :return: filename of input file  """

        airfoilPath, airfoilFileName = os.path.split(airfoilPathFileName)
        tmpFileName = 'tmpPolar.inp'

        # generate tmp in dir of airfoil 
        tmpFilePath = os.path.join (airfoilPath, tmpFileName)
        tmpfile = open(tmpFilePath, 'w+')

        tmpfile.write ("&polar_generation\n")
        tmpfile.write ("  generate_polars = .true.\n")
        tmpfile.write ("  type_of_polar = %d\n" % polarType)  
        if spec_al:  tmpfile.write ("  op_mode = 'spec-al'\n") 
        else:        tmpfile.write ("  op_mode = 'spec-cl'\n") 
        tmpfile.write ("  op_point_range = %.2f , %.2f , %.2f \n" % (valRange[0], valRange[1], valRange[2])) 
        tmpfile.write ("  polar_reynolds  = %s\n" % (', '.join(str(e) for e in reNumbers))) 
        tmpfile.write ("/\n")

        tmpfile.write ("&xfoil_run_options\n")
        tmpfile.write ("  ncrit = %.1f\n" % ncrit)  
        tmpfile.write ("/\n")

        tmpfile.close()

        return tmpFilePath              

# -------------- End --------------------------------------


# Main program for testing 
if __name__ == "__main__":

    pass 