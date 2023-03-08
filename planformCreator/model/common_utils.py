#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Common Utility functions for convinience
"""

from termcolor import colored
from colorama import just_fix_windows_console


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
    my_print(' - ' + colored(message, 'white', attrs=["dark"]))

def ErrorMsg(message):
    my_print(colored('\nError: ', 'red') + message)

def WarningMsg(message):
    my_print(colored('\nWarning: ', 'yellow') + message+ '\n')

def NoteMsg(message):
    my_print(colored('Note: ', 'cyan') + message)

def TraceMsg(message):
    if (not trace_disabled):
        my_print(colored(' - ' + message, 'white', attrs=["dark"]))

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
