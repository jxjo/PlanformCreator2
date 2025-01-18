#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Proxies to Xoptfoil2 and Worker 

If the programs aren't in path-environment or in 'assets directory'
the path to program location location must be set 

   Worker.exePath = "xy/z/"
"""


import os
from tempfile       import NamedTemporaryFile
from glob           import glob
from io             import StringIO

import re
import time 
import shutil
import logging
import datetime 

from subprocess     import Popen, run, PIPE
if os.name == 'nt':                                 # startupinfo only available in windows environment  
    from subprocess import STARTUPINFO, CREATE_NEW_CONSOLE, STARTF_USESHOWWINDOW, CREATE_NO_WINDOW

from base.common_utils          import *

import logging
logger = logging.getLogger(__name__)
# logger.setLevel(logging.WARNING)

# Windows - popen - startupinfo codes
# https://learn.microsoft.com/de-de/windows/win32/api/winuser/nf-winuser-showwindow

SW_NORMAL = 1 
SW_MINIMIZE = 6 

EXE_DIR_WIN    = 'assets/windows'                   # directory of exe files 
EXE_DIR_UNIX   = 'assets/linux'                    

TMP_INPUT_NAME = 'tmp~'                             # temporary input file (~1 will be appended)
TMP_INPUT_EXT  = '.inp'

#------- Helper function -----------------------------------------#

def is_younger (filePath, age_in_seconds): 
    """ returns true if filePathName is younger than age_in_seconds """

    younger = False 

    if os.path.isfile (filePath):
        now         = datetime.datetime.now()
        last_update = datetime.datetime.fromtimestamp(os.path.getmtime(filePath))
        tdelta = now - last_update
        seconds = tdelta.total_seconds()
        younger =  seconds < age_in_seconds

    return younger


def file_in_use (filePath):
    """ returns True if file is in use by another process"""
    
    in_use = False

    if os.path.exists(filePath):
        try:
            os.rename(filePath, filePath)
        except OSError as e:
            logger.warning (f"File {filePath} in use by another process")
            in_use = True 

    return in_use 



#------------------------------------------------------------------------------------

class X_Program:
    """ 
    Abstract superclass - Proxy to execute eg Xoptfoil2 and Worker 
    
        self will be executed in 'workingDir' which must be set if is not current dir
    """
    name        = 'my_program'
    version     = ''                                       # version of self - will be set in isReady
    exe_dir     = None                                     # where to find .exe 
    ready       = False                                    # is Worker ready to work 
    ready_msg   = ''                                       # ready or error message 


    def __init__ (self, workingDir = None):

        if workingDir and not os.path.isdir (workingDir):
            raise ValueError (f"Working directory '{workingDir}' does not exist" )
        else:    
            self.workingDir = workingDir

        self._popen : Popen     = None                      # instance of subprocess when async
        self._returncode        = 0                         # returncode when async finished 
        self._pipe_error_lines  = None                      # errortext lines from stderr when finished
        self._pipe_out_lines    = []                        # output lines from stdout when finished

        self._tmpInpFile        = None                      # tmpfile of this instance, to be deleted 


    def __repr__(self) -> str:
        """ nice representation of self """
        return f"<{type(self).__name__}"

   
    def isReady (self, parent_file : str, min_version : str = '') -> bool:
        """ 
        checks if self is available with min_version.

        Args: 
            parent_file: pathFilename of the parent (app) which will use self
            min_version: check fpr min version number 
        """


        # ready already checked? 

        if self.ready: return True

        # find .exe

        version_ok = False
        ready_msg  = None 
        cls = self.__class__

        if self.exe_dir is None: 

            exe_dir, ready_msg = self._get_exe_dir (parent_file)

            if exe_dir is None:                                        # self not found anywhere
                cls.ready_msg = ready_msg
                logger.warning (ready_msg)
                return 
            else:     
                cls.exe_dir = exe_dir
                logger.debug (ready_msg)

        # try to execute with -h help argument to get version 

        try: 
            returncode, pipe_output, _ = self._execute ('-h', run_async=False, capture_output=True)
        except: 
            returncode = 1

        # extract version and check version
        if returncode == 0 and pipe_output:
            outLines = pipe_output.split('\n')
            for line in outLines: 
                words = line.split()

                # first word is program name 
                if len (words) >=1 and words[0] == self.name:

                    # second word is version - compare single version numbers
                    cls.version = words[1]
                    version_ok = True
                    min_nums = min_version.split(".") 

                    if len (min_nums[0]) > 0 :
                        cur_nums = self.version.split(".")
                        for i in range(len(min_nums)): 
                            try: 
                                cur_num = int(cur_nums[i])
                            except: 
                                cur_num = 0 
                            if cur_num < int(min_nums[i]):
                                version_ok = False
            
            if not version_ok:
                cls.ready_msg = f"Wrong version {self.version} (need {min_version})"
            else: 
                cls.ready = True
                cls.ready_msg = f"Ready"

        else: 
            cls.ready_msg = f"{self.name} couldn't be executed"      

        if self.ready: 
            logging.info (f"{self.name} {self.version} {self.ready_msg}" )
        else: 
            logging.error (f"{self.ready_msg}" )

        return self.ready
    

    def isRunning (self) -> bool:
        """ still running when async - otherwise False"""

        isRunning = False
        if self._popen is not None: 

            self._popen.poll ()                 # check return code

            if self._popen.returncode is None: 

                isRunning = True
                # logger.debug (f"==> {self.name} running" )

            else: 

                self._returncode       = self._popen.returncode
                self._pipe_error_lines = self._popen.stderr.readlines() if self._returncode else []
                self._pipe_out_lines.extend(self._popen.stdout.readlines() if self._popen.stdout else [])

                if self._returncode:
                    logger.error (f"==> {self.name} returncode: {self._returncode} - {''.join (self._pipe_error_lines)}")
                logger.debug (f"==> {self.name} finished {''.join (self._pipe_out_lines)}")
                 
                self._popen = None              # close down process instance 

        return isRunning


    def remove_tmp_inp_file (self, iretry = 0):
        """ os remove of temporary input file"""

        if self._tmpInpFile:            # remove tmpfile of worker 
            try: 
                os.remove(self._tmpInpFile) 
            except OSError as exc: 
                if iretry < 5: 
                    iretry +=1
                    logger.debug (f"{self} Could not delete tmp input file '{self._tmpInpFile}' - Retry {iretry}")
                    time.sleep (0.1)
                    self.remove_tmp_inp_file (iretry=iretry)
                else: 
                    logger.error (f"Could not delete tmp input file '{self._tmpInpFile}' - {exc}")
            self._tmpInpFile = None 


    @property
    def finished_returncode (self): 
        """ returncode of subprocess when finished"""
        return self._returncode


    @property
    def finished_errortext (self): 
        """ errortext from subprocess in case returncode !=0 """

        # scan stderr and stdout for a line with 'Error: ' 

        text = None
        line : str
        if self._pipe_error_lines:
            for line in self._pipe_error_lines:
                _,_,text = line.partition ("Error: ")
                if text: return text 
        if self._pipe_out_lines:
            for line in self._pipe_out_lines:
                _,_,text = line.partition ("Error: ")
                if text: return text 
        return text


    def reset_error (self): 
        """ reset an eventual error code and text """
        self._returncode = 0 
        self._pipe_error_lines = None 


    def finalize (self):
        """ do cleanup actions """

        self.remove_tmp_inp_file ()


    def terminate (self):
        """ terminate os process of self"""

        if self.isRunning():
            self._popen.terminate()
            self._popen = None 


    # ---------- Private --------------------------------------

    def _execute (self, args = None, run_async : bool  = True, 
                  capture_output : bool =False):
        """execute self in workingDir either sync or async

        Args:
            args: arguments of subprocess
            run_async: run async subprocess. Defaults to True.
            capture_output: capture output in pipe. Defaults to False.
             > doesn't work  > input_stream: optional textstream piped into stdin 
        Returns:
            returncode: = 0 if no error
            pipe_output: piped stdout textstream 
            pipe_error: piped stderr textstream 
        """

        popen       = None                                  # subprocess Popen instance
        pipe_result = None                                  # captured stdout result text stream
        pipe_error  = None                                  # captured stderr error text stream
        returncode  = 0 
        input_stream = None

        if self.workingDir:
            curDir = os.getcwd()
            os.chdir (self.workingDir)

        exe = os.path.join (self.exe_dir, self.name)

        # build list of args needed by popen 

        if isinstance (args, list):
            arg_list = [exe] + args
        elif isinstance (args, str):
            arg_list = [exe, args]
        else: 
            arg_list = [exe]

        # run either sync or async 

        if run_async:

            # uses subproccess Popen instance to start a subprocess

            if capture_output:
                stdout = PIPE                               # output is piped to suppress window 
                stderr = PIPE                               # Xoptfoil will write error to stderr
                # needed when running as pyinstaller .exe 
                # https://stackoverflow.com/questions/7006238/how-do-i-hide-the-console-when-i-use-os-system-or-subprocess-call/7006424#7006424
                if os.name == 'nt':
                    flags = CREATE_NO_WINDOW    
                else: 
                    flags  = 0                              # posix must be 0 
                startupinfo = self._get_popen_startupinfo (SW_NORMAL)  
            else: 
                stdout = None 
                stderr = PIPE                               # Xoptfoil will write error to stderr
                if os.name == 'nt':
                    flags  = CREATE_NEW_CONSOLE             # a new console is created (Xoptfoil) 
                else: 
                    flags  = 0                              # posix must be 0 
                startupinfo = self._get_popen_startupinfo (SW_MINIMIZE)  
            if input_stream:
                raise ValueError ("Input stream for subprocess not implemented")
            
                # ... the problem is, that Fortran doesn't make a rewind on input streams
                #     only on redirected input files ...
                # stdin = PIPE
                # intext = input_stream.getvalue()
            else: 
                stdin = None 

            popen = Popen (arg_list, creationflags=flags, text=True, **startupinfo, 
                             stdin=stdin, stdout=stdout, stderr=stderr)  

            logger.debug (f"==> run {self.name}: '{args}'")

            popen.poll()                            # update returncode 

            if popen.returncode is None:            # seems up and running
                returncode = 0          
            else: 
                returncode = popen.returncode 
                pipe_result = popen.stdout
                pipe_error  = popen.stderr 
                popen = None                        # remove process instance 

        else: 

            # uses subproocess run which returns a CompletedProcess instance 

            if capture_output:
                # needed when running as pyinstaller .exe 
                # https://stackoverflow.com/questions/7006238/how-do-i-hide-the-console-when-i-use-os-system-or-subprocess-call/7006424#7006424
                if os.name == 'nt':
                    flags = CREATE_NO_WINDOW    
                else: 
                    flags  = 0                      # posix must be 0       

            completed_process = run (arg_list, text=True, 
                                      input=input_stream, capture_output=capture_output, creationflags=flags)

            returncode  = completed_process.returncode
            pipe_result = completed_process.stdout
            pipe_error  = completed_process.stderr

            if returncode:
                logger.error (f"==> run sync {self.name}: '{completed_process}'")
            else: 
                logger.debug (f"==> run sync {self.name}: '{args}'")

        if self.workingDir: 
            os.chdir (curDir)

        # keep for later poll 
        self._popen = popen 
        self._returncode = returncode
        if pipe_error:
            if isinstance (pipe_error, str):                # 'run' returns string 
                pipe_error = StringIO(pipe_error) 
            self._pipe_error_lines = pipe_error.readlines()
            pipe_error.seek(0)                              # rewind to the start
        else: 
            self._pipe_error_lines = None

        return  returncode, pipe_result, pipe_error


    def _get_popen_startupinfo (self, show : int):
        """ returns popen startinfo parm to eg. minimize shell window - only windows"""

        if os.name == 'nt':
            if show != SW_NORMAL:
                startupinfo = STARTUPINFO()  
                startupinfo.dwFlags |= STARTF_USESHOWWINDOW       
                startupinfo.wShowWindow = show
                return dict(startupinfo=startupinfo)
        return dict(startupinfo=None) 


    def _get_exe_dir (self, parent_file : str): 
        """
        trys to find path to call programName
        
        If found, returns exePath and ready_msg
        If not, return None and ready_msg (error)"""

        parent_dir = os.path.dirname(os.path.realpath(parent_file))

        exe_dir  = None
        ready_msg = None 

        if os.name == 'nt':
            assets_dir = EXE_DIR_WIN
        else: 
            assets_dir = EXE_DIR_UNIX  

        assets_dir = os.path.normpath (assets_dir)  
        check_dir  = os.path.join (parent_dir , assets_dir)

        if os.path.isfile(os.path.join(check_dir, self.name +'.exe')) : 
            exe_dir  = os.path.abspath(check_dir) 
            ready_msg = f"{self.name} found in: {exe_dir}"
        else: 
            exe_path = shutil.which (self.name)  
            if exe_path: 
                exe_dir = os.path.dirname (exe_path)
                ready_msg = f"{self.name} using OS search path to execute: {exe_dir}"
            else: 
                ready_msg = f"{self.name} not found either in '{check_dir}' nor via OS search path" 
        return exe_dir, ready_msg



# ------------------------------------------------------------



class Xoptfoil2 (X_Program):
    """ 
    Proxy to execute Xoptfoil2
    
        self will be executed in 'workingDir' which must be set if is not current dir
        The 'inputfile' must be in 'workingDir' 
    """

    name        = 'Xoptfoil2'
    RUN_CONTROL = 'run_control'                     # file name of control file 
    STILL_ALIVE = 10                                # max. age in seconds of run_control

    @property
    def run_control_filePath (self):
        """ returns filePath of run_control"""
        if self.workingDir: 
            return os.path.join(self.workingDir, Xoptfoil2.RUN_CONTROL)
        else:
            return Xoptfoil2.RUN_CONTROL


    def run (self, outname:str, input_file:str=None, seed_airfoil:str =None):
        """ run self async 

        Args:
            outname: output name for generated airfoil
            inputfile: name of input file. Defaults to 'outname'.inp.
            seed_airfoil: optional seedairfoil filename.
        Returns: 
            returncode: = 0 - no errors (which could be retrieved via 'finished_errortext' )
        """

        args = []
        if seed_airfoil  : args.extend(['-a', seed_airfoil])
        if outname       : args.extend(['-o', outname])  
        
        if input_file is None: input_file = outname + '.inp'
        args.extend(['-i', input_file]) 

        # add 'mode' option - will write error to stderr
        args.extend(['-m', 'ao']) 

        returncode, _, _ = self._execute (args=args, run_async=True)

        return returncode


    def isRunning (self) -> bool:
        """ 
        - still running?    ... process when async
                            ... otherwise check run_control file if self was started from outside 
        - update nSteps, nDesings"""

        if self._popen:                         # started program myself as process
            running = super().isRunning ()
        else:                                   # Xoptfoil was started from outside 
            running = is_younger (self.run_control_filePath,  Xoptfoil2.STILL_ALIVE)

        if not running and os.path.isfile (self.run_control_filePath):
            # remove old run_control 
            os.remove (self.run_control_filePath)
 
        return running 



    def get_progress (self):
        """ returns no of steps, no of designs and objective function when running otherwise 0,0, 1.0 """

        # format of run_control 
            # !stop
            # !run-info; step: 3; design: 3; fmin:  0.9919478

        steps   = 0 
        designs = 0 
        objFun  = 1.0 
        lines = []

        if os.path.isfile (self.run_control_filePath):
            with open(self.run_control_filePath, 'r') as file:
                lines = file.readlines()
                file.close()

        for line in lines: 
            infos = line.split(";")
            if len(infos) == 4: 
                try: 
                    steps   = int(infos[1].split(":")[1])
                    designs = int(infos[2].split(":")[1])
                    objFun  = float(infos[3].split(":")[1])
                except: 
                    pass

        return steps, designs, objFun


    def stop (self):
        """
        tries to stop self with a 'stop' command in 'run_control' file 
            self must run in 'workingDir'
        """


        with open(self.run_control_filePath, 'w+') as file:
            file.write("stop")
            file.close()


# ------------------------------------------------------------



class Worker (X_Program):
    """ proxy to execute Worker commands"""

    name    = 'Worker'

    # -- static methods --------------------------------------------

    @staticmethod
    def remove_polarDir (airfoilPath, polarDir, only_if_older = False):
        """ 
        deletes polar directory of airfoilPathFileName
        If only_if_older the directory is not removed if it is younger than the airfoil
        """ 

        # sanity check 
        if not os.path.isdir(polarDir): return 

        remove = True 

        if only_if_older and os.path.isfile(airfoilPath):

            # compare datetime of airfoil file and polar dir 
            ts = os.path.getmtime(polarDir)                 # file modification timestamp of a file
            polarDir_dt = datetime.datetime.fromtimestamp(ts)        # convert timestamp into DateTime object

            ts = os.path.getmtime(airfoilPath)              # file modification timestamp of a file
            airfoil_dt = datetime.datetime.fromtimestamp(ts)         # convert timestamp into DateTime object

            # add safety seconds (async stuff?) 
            if (airfoil_dt < (polarDir_dt + datetime.timedelta(seconds=2))):
                remove = False 

        if remove: 
            shutil.rmtree(polarDir, ignore_errors=True)


    @staticmethod
    def get_existingPolarFile (airfoil_pathFileName, 
                               polarType : str, re :float, ma : float, ncrit : float):
        """ 
        Get pathFileName of polar file if it exists 
        """ 

        polar_pathFileName = None

        # build name of polar dir from airfoil file 
        polarDir = str(Path(airfoil_pathFileName).with_suffix('')) + '_polars'

        # remove a maybe older polarDir
        Worker.remove_polarDir (airfoil_pathFileName, polarDir, only_if_older=True)    

        # no polarDir - no polarFile 
        if os.path.isdir (polarDir):            

            # build the typical xfoil filename like T1_Re0.500_M0.00_N7.0.txt
            polar_fileName     =  f'{polarType}_Re{re/1000000:.3f}_M{ma:.2f}_N{ncrit:.1f}.txt'
            polar_pathFileName = os.path.join (polarDir, polar_fileName)

            # check if polar file exists
            if not os.path.isfile (polar_pathFileName):
                polar_pathFileName = None

        return polar_pathFileName 

    #---------------------------------------------------------------


    def showHelp(self):
        
        return (self._execute_Worker ("help"))


    def check_inputFile (self, inputFile=None):
        """ checks if self is available with version ..."""

        ready, _ = self.isReady()
        if not ready: return 1, self.name + " not ready"

        error_text = ""
        args = ['-w', 'check-input', '-i', inputFile]

        returncode, pipe_output, pipe_error = self._execute (args, run_async=False, capture_output=True)

        if returncode != 0 and pipe_error:

            # worker output should something like ...
            #  Worker   -check-input jx-gt-10v3.inp
            #  - Processing input
            #    - Reading input jx-gt-10v3.inp
            #    - Output prefix jx-gt-10v3
            #  Error: max_speed should be between 0.001 and 0.5

            out_lines = pipe_output.split('\n')
            for line in out_lines:
                error_text = line.partition("Error:")[2].strip()
                if error_text != '': break 
            if error_text == '':
                raise ValueError ("Errortext not found in Workers")

        return returncode, error_text 


    def generate_polar (self, airfoilPathFileName, 
                        polarType : str, 
                        re : float | list, 
                        ma : float | list, 
                        ncrit : float,
                        autoRange = True, spec = 'alpha', valRange= [-3, 12, 0.25], 
                        nPoints=None, run_async = True):
        """ 
        Generate polar for airfoilPathFileName in directory of airfoil.
        Returns of polar pathFile. If async returns None!
        """ 

        if (polarType == 'T2'):
            polarTypeNo = 2
        else: 
            polarTypeNo = 1

        if spec == 'alpha':
            spec_al = True
        else: 
            spec_al = False

        if not isinstance (re, list):
            re = [re]
        if not isinstance (ma, list):
            ma = [ma]

        if os.path.isfile(airfoilPathFileName): 

            # a temporary input file for polar generation is created
            # when async a text stream is created which will be piped ...
            self._tmpInpFile = self._generate_polarInputFile (airfoilPathFileName, 
                                        re, ma,
                                        polarTypeNo, ncrit,
                                        autoRange, spec_al, valRange,
                                        nPoints=nPoints) 

            # ... so Worker has some input to work on ...

            returncode = self._execute_Worker ("polar", airfoil1=airfoilPathFileName, 
                                                     inputfile=self._tmpInpFile, 
                                                     run_async=run_async)
            
            if returncode: 
                raise RuntimeError (f"Worker polar generation failed for {airfoilPathFileName}")

            if run_async:
                # it will take some seconds until files are available 
                polar_pathFileName = None
                # tmpfile will be removed when finsiehd
            else:
                # get the polar files in polar subdirectory 
                polar_pathFileName = self.get_existingPolarFile (airfoilPathFileName, 
                                                    polarType, re, ma , ncrit)
                
                self.remove_tmp_inp_file ()                 #  remove tmpfile of worker

                return polar_pathFileName
            
        else: 
            raise Exception ("generatePolar - airfoil file %s doesn't exist" % airfoilPathFileName )



    def clean_workingDir (self, workingDir):
        """ 
        deletes temporary (older) files Worker creates 
        """ 
        if os.path.isdir(workingDir):

            # remove tmp input files of polar generation 
            match_path = os.path.join (workingDir, f"{TMP_INPUT_NAME}*{TMP_INPUT_EXT}")
            
            for f in glob(match_path):
                os.remove(f)


# ---------- Private --------------------------------------

    def _execute_Worker (self, action, actionArg='', airfoil1='', airfoil2='', 
                         outname='', inputfile='', run_async=False):

        # change to dir of airfoil1 for execution
        airfoil1Path, airfoil1FileName = os.path.split(airfoil1)
        airfoil2FileName = airfoil2

        if (airfoil1Path != ''):
            self.workingDir = airfoil1Path
            # in this case also strip airfoil2 
            if (airfoil2 != ''):
                airfoil2Path, airfoil2FileName = os.path.split(airfoil2)
        else:
            self.workingDir = None


        # info inputfile is in a dir - strip dir from path - local execution 
        if (inputfile != ''):
            inpufilePath, localInputfile = os.path.split(inputfile)
        else: 
            localInputfile = '' 

        args = []

        if (action != ''): 
            if (action == 'help'): 
                args.extend(['-h'])
            else:                  
                args.extend(['-w', action])
                if (actionArg): args.extend([actionArg]) 
        else: 
            raise ValueError ('action for worker is mandatory')

        if (airfoil1 ): args.extend(['-a',  airfoil1FileName])
        if (airfoil2 ): args.extend(['-a2', airfoil2FileName])
        if (outname  ): args.extend(['-o',  outname]) 
        if (inputfile): args.extend(['-i',  localInputfile])

        # if async - capture output to pipe - so its in background
        if run_async:
            capture = True
        else:
            capture = False

        returncode, _, _ = self._execute (args, run_async=run_async, capture_output=capture)

        return  returncode



    def _generate_polarInputFile (self, airfoilPathFileName, 
                                  reNumbers : list[float], maNumbers : list[float],
                                  polarType : int, ncrit : float,  
                                  autoRange : bool, spec_al: bool, valRange: list[float], 
                                  nPoints = None):
        """ Generate a temporary polar input file for worker like this 

        &polar_generation
            generate_polars = .true.
            polar_reynolds  = 230000, 300000, 400000
            polar_mach      = 0.0, 0.2, 0.5
            type_of_polar = 1
            auto_Range = .true.
            op_mode = 'spec-al'
            op_point_range = -2.6, 11.0, 0.5
        /
        &xfoil_run_options
            ncrit = 7.0
        /
        :return: pathFilename of input file  """

        tmpFilePath = None

        airfoilPath, airfoilFileName = os.path.split(airfoilPathFileName)

        # create tmp input file 

        with NamedTemporaryFile(mode="w", delete=False, dir=airfoilPath, prefix=TMP_INPUT_NAME, suffix=TMP_INPUT_EXT) as tmp:

            tmp.write ("&polar_generation\n")
            tmp.write ("  type_of_polar = %d\n" % polarType)  
            tmp.write ("  polar_reynolds  = %s\n" % (', '.join(str(e) for e in reNumbers))) 
            if max(maNumbers) > 0.0: 
                tmp.write ("  polar_mach  = %s\n" % (', '.join(str(e) for e in maNumbers))) 
            if autoRange:
                tmp.write ("  auto_range = .true.\n") 
                if valRange[2]:                                 # write only increment
                    tmp.write ("  op_point_range = , , %.2f \n" % (valRange[2])) 
            else:
                if spec_al:  tmp.write ("  op_mode = 'spec-al'\n") 
                else:        tmp.write ("  op_mode = 'spec-cl'\n") 
                tmp.write ("  op_point_range = %.2f , %.2f , %.2f \n" % (valRange[0], valRange[1], valRange[2])) 
            tmp.write ("/\n")

            tmp.write ("&xfoil_run_options\n")
            tmp.write ("  ncrit = %.1f\n" % ncrit)  
            tmp.write ("/\n")

            if nPoints is not None: 
                tmp.write ("&paneling_options\n")
                tmp.write (f"  npoint = {int(nPoints)}\n")  
                tmp.write ("/\n")

            tmpFilePath = tmp.name

        return tmpFilePath              


# -------------- End --------------------------------------


# Main program for testing 
if __name__ == "__main__":

    pass 