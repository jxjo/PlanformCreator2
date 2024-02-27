#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Common Utility functions for convinience
"""

from termcolor import colored
import os
from pathlib import Path
import json
import platform

if platform.system() == 'Windows': 
    from colorama import just_fix_windows_console
    just_fix_windows_console()                          # colored terminal output for WIndows  


#------------------------------------------------------------------------------
# Colored messages
#------------------------------------------------------------------------------

print_disabled = False
trace_disabled = True


def my_print(message):
    if print_disabled:
        return
    else:
       print(message)

def InfoMsg(message):
    my_print(colored(' - ' + message, 'white', attrs=["dark"]))

def ErrorMsg(message):
    my_print(colored('Error: ', 'red') + message)

def WarningMsg(message):
    my_print(colored('Warning: ', 'yellow') + message)

def NoteMsg(message):
    my_print(colored('Note: ', 'cyan') + message)

def TraceMsg(message):
    if (not trace_disabled):
        my_print(colored('Trace ' + message, 'white', attrs=["dark"]))

def DoneMsg():
    my_print("Done.\n")


#------------------------------------------------------------------------------
# Dictonary handling
#------------------------------------------------------------------------------

def fromDict(dict : dict, key, default='no default', msg=True):
    """
    returns a value from dict. If ky is not in the dict and there is no default value an error
    will be raised 

    Args:
        :dict: the dictonary to look in \n
        :key: the key to look for       \n
        :default: the value if key is missing
        :msg: if True a log message will be printed when a value is missing 
    """
    preferedType = None

    if default != 'no default':
        if isinstance (default, float):
           preferedType = float
        elif isinstance (default, bool):
            preferedType = bool
        elif isinstance (default, int):
            preferedType = int

    try:
        value = dict[key]
        if preferedType == float:
            value = float(value)
        elif preferedType == int:
            value = int(value)
        elif preferedType == bool:
            value = bool(value)
    except:
        if default == 'no default':
            value = None
            ErrorMsg('Mandatory parameter \'%s\' not specified'  % key)
        else:
            value = default 
            if value and msg:
                NoteMsg('Parameter \'%s\' not specified, using default-value \'%s\'' % (key, str(value)))
    return value


def toDict(dict : dict, key, value):
    """
    writes t0 the parameter dictionary. If 'value' is None the key is not written 
    """
    if not value is None: 
        # limit decimals in file 
        if isinstance  (value, float):
            value = round (value,6)
        dict [key] = value
    else: 
        # remove key from dictionary  - so default values will be used 
        dict.pop(key, None)

        
#------------------------------------------------------------------------------
# Settings and Paramter file 
#------------------------------------------------------------------------------

class Parameters ():
    """ Handles a parameter file with a json structure representing a dictionary of paramteres""" 

    def __init__ (self, paramFilePath):

        self._paramFilePath = paramFilePath

    def get_dataDict (self, msg=True):
        """
        returns the complete dataDict of self
        """
        dataDict = {}
        if self._paramFilePath:
            try:
                paramFile = open(self._paramFilePath)
                try:
                    dataDict = json.load(paramFile)
                    paramFile.close()
                except ValueError as e:
                    ErrorMsg("Invalid json expression '%s' in parameter file '%s'" % (e, self._paramFilePath))
                    paramFile.close()
                    dataDict = {}
            except:
                if msg: 
                    NoteMsg ("Paramter file %s not found" % self._paramFilePath)
        return dataDict


    def write_dataDict (self, aDict, dataName='Parameters'):
        """ writes data dict to file
        
        :Returns: 
            True : if succeded, False if failed"""

        try:
            paramFile = open(self._paramFilePath, 'w')
        except:
            ErrorMsg("Failed to open file %s" % self._paramFilePath)
            return False

        # save parameter dictionary to .json-file
        try:
            json.dump(aDict, paramFile, indent=2, separators=(',', ':'))
            paramFile.close()
            InfoMsg ("%s saved to %s" % (dataName, self._paramFilePath))
            return True

        except ValueError as e:
            ErrorMsg("Invalid json expression '%s'. Failed to save data to %s'" % (e, self._paramFilePath))
            paramFile.close()
            return False


class Settings (Parameters):
    """ Handles a named setting file with a json structure representing a dictionary of paramteres""" 

    settingsFilePath = None                     # the filePath of the settings

    def __init__ (self, appName='', nameExtension='', fileExtension= '.json', msg=False):
        """ 
        object to handle i/o of settings parameters 
        
        Settings file is defined with class method 'belongsTo'
        """

        super().__init__(self.settingsFilePath)


    @classmethod
    def belongTo (cls, belongsToPath, nameExtension='', fileExtension= '.json', msg=False):
        """ static set of the file the settings will belong to 
        
        Args:
            :belongsToPath: file path of the python module self will belong to 
            :nameExtension: ... will be appended to appName - default '_settings'       \n
            :fileExtension: ... of the settings file - default 'json'       \n
            :msg: True -an info message will be printed        \n
        """

        appName = os.path.splitext(os.path.basename(belongsToPath))[0]

        if nameExtension:
            paramFile = appName + nameExtension + fileExtension
        else:
            paramFile = appName + '_settings' + fileExtension

        # get directory where 'belongTo' is located
        script_dir  = os.path.dirname(os.path.realpath(belongsToPath))

        cls.settingsFilePath = os.path.join(script_dir, paramFile)

        if msg: 
            InfoMsg ("Reading settings from %s" % cls.settingsFilePath)


    @property
    def filePath (self): return self._paramFilePath

    def get(self, key, default='no default', msg=False):
        """
        returns the value of 'key' from settings

        Args:
            :key: the key to look for       \n
            :default: the value if key is missing
            :msg: if True a log message will be printed when a value is missing 
        """
        dataDict = self.get_dataDict (msg=False)
        return fromDict(dataDict, key, default=default, msg=msg)

    def set(self, key, value):
        """
        sets 'key' with 'value' into settings

        Args:
            :key: the key to look for       \n
            :default: the value if key is missing
            :msg: if True a log message will be printed when a value is missing 
        """
        dataDict = self.get_dataDict ()

        toDict(dataDict, key, value)
        self.write_dataDict (dataDict, dataName='Settings')



#------------------------------------------------------------------------------
# File, Path handling 
#------------------------------------------------------------------------------

class PathHandler(): 
    """ handles relative Path of actual files to a workingDir """

    def __init__ (self, workingDir=None, onFile=None): 
        """  Pathhandler for working directory either from 'workinfDir' directly or based 'onFile' """
        self._workingDir = None

        if workingDir is not None: 
           self.workingDir = workingDir 
        elif onFile is not None:
           self.set_workingDirFromFile (onFile)

    @classmethod
    def relPath(cls, pathFilename, start = None):
        """Return a relative version of a path - like os.path.relpath - but checks for same drive

        Args:
            :start: start dir - default: cwd (current working Dir)
        """
        if start is None: start = os.getcwd()

        if Path(pathFilename).anchor == Path(start).anchor: 
            relPath = os.path.relpath(pathFilename, start = start)
        else: 
            relPath = pathFilename
        return relPath  


    @property 
    def workingDir (self):
        return self._workingDir if (not self._workingDir is None) else ''
    
    @property 
    def workingDir_name (self): 
        """ the directory name of workingDir """
        return os.path.basename(os.path.normpath(self.workingDir))
    
    @workingDir.setter
    def workingDir (self, newDir):

        if newDir is None: 
            self._workingDir = os.getcwd ()      # if no directory set, take current working Dir 
        elif not newDir: 
            self._workingDir = os.getcwd ()      # if no directory set, take current working Dir 
        elif not os.path.isdir(newDir): 
            os.makedirs(newDir)  
            self._workingDir = os.path.normpath(newDir)    
        else: 
            self._workingDir = os.path.normpath(newDir) 

    def set_workingDirFromFile (self, aFilePath):

        if aFilePath is None: 
            self.workingDir = None
        elif not aFilePath: 
            self.workingDir = os.getcwd ()      # if no directory set, take current working Dir 
        else: 
            self.workingDir = os.path.dirname(aFilePath) 
            if not os.path.isdir(self.workingDir):
                os.makedirs(self.workingDir)

    def relFilePath (self, aFilePath):
        """ returns the relative path of aFilePath to the workingDir of self"""
        if aFilePath is None: 
            return None
        else: 
            try: 
                relPath =  os.path.normpath(os.path.relpath(aFilePath, start = self.workingDir))
                if len(relPath) > len(aFilePath): 
                    return aFilePath                # relPath would be more complicated
                else: 
                    return relPath 
            except:                                 # aFilePath is on different drive 
                return aFilePath 
    
    def fullFilePath (self, aRelPath):
        """ returns the full path of relative aRelPath and the workingDir of self"""

        if aRelPath is None: 
            return self.workingDir
        else: 
            if os.path.isabs (aRelPath): 
                # maybe we can make a rel path out of it? 
                newPath = self.relFilePath (aRelPath)
                if os.path.isabs (newPath):
                    return aRelPath                 # we surrender - it's absolute
                else: 
                    aRelPath = newPath              # now we have a real real path 
            return os.path.normpath(os.path.join (self.workingDir, aRelPath))
