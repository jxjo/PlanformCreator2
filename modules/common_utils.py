#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Common Utility functions for convinience
"""

from termcolor import colored
from colorama import just_fix_windows_console
import os


#------------------------------------------------------------------------------
# Colored messages
#------------------------------------------------------------------------------

print_disabled = False
trace_disabled = True

# init colorama
just_fix_windows_console()


def my_print(message):
    if print_disabled:
        return
    else:
       print(message)

def InfoMsg(message):
    my_print(colored('Info: ' + message, 'white', attrs=["dark"]))

def ErrorMsg(message):
    my_print(colored('\nError: ', 'red') + message)

def WarningMsg(message):
    my_print(colored('\nWarning: ', 'yellow') + message+ '\n')

def NoteMsg(message):
    my_print(colored('Note: ', 'cyan') + message)

def TraceMsg(message):
    if (not trace_disabled):
        my_print(colored('Trace ' + message, 'white', attrs=["dark"]))

def DoneMsg():
    my_print("Done.\n")


#------------------------------------------------------------------------------
# geo utils
#------------------------------------------------------------------------------

def interpolate(x1, x2, y1, y2, x):
    try:
        y = ((y2-y1)/(x2-x1)) * (x-x1) + y1
    except:
        ErrorMsg("Division by zero, x1:%f, x2:%f" % (x1, x2))
        y = 0.0
    return y


#------------------------------------------------------------------------------
# Dictonary handling
#------------------------------------------------------------------------------

def fromDict(dict, key, default='no default', msg=True):
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
        elif isinstance (default, int):
            preferedType = int

    try:
        value = dict[key]
        if preferedType == float:
            value = float(value)
        elif preferedType == int:
            value = int(value)
    except:
        if default == 'no default':
            value = None
            ErrorMsg('Mandatory parameter \'%s\' not specified'  % key)
        else:
            value = default 
            if value and msg:
                NoteMsg('Parameter \'%s\' not specified, using default-value \'%s\'' % (key, str(value)))
    return value

def toDict(dict, key, value):
    """
    writes t0 the parameter dictionary. If 'value' is None the key is not written 
    """
    if not value is None: 
        # limit decimals in file 
        if isinstance  (value, float):
            value = round (value,6)
        dict [key] = value


        
#------------------------------------------------------------------------------
# Fiel, Path handling 
#------------------------------------------------------------------------------

class PathHandler(): 
    """ handles relative Path of actual files to a workingDir """

    def __init__ (self, workingDir=None, onFile=None): 

        self._workingDir = None

        if workingDir is not None: 
           self.workingDir = workingDir 
        elif onFile is not None:
           self.set_workingDirFromFile (onFile)
           

    @property 
    def workingDir (self):
        return self._workingDir 
    
    @workingDir.setter
    def workingDir (self, newDir):

        if newDir is None: 
            self._workingDir = os.getcwd ()      # if no directory set, take current working Dir 
        elif not newDir: 
            self._workingDir = os.getcwd ()      # if no directory set, take current working Dir 
        elif not os.path.isdir(newDir): 
            raise ValueError (self.__name__+ ": '%s' is not a directory" % newDir)
        else: 
            self._workingDir = os.path.normpath(newDir) 

    def set_workingDirFromFile (self, aFilePath):

        if aFilePath is None: 
            self.workingDir = None
        elif not aFilePath: 
            self._workingDir = os.getcwd ()      # if no directory set, take current working Dir 
        else: 
            if not os.path.isfile(aFilePath): 
                raise ValueError (self.__class__.__name__+ ": '%s' is not a file" % aFilePath)
            else: 
                self.workingDir = os.path.dirname(aFilePath) 

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
