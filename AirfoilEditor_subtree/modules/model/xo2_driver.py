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
from pathlib        import Path

import time 
import shutil
import logging
import datetime 

from subprocess     import Popen, run, PIPE
if os.name == 'nt':                                 # startupinfo only available in windows environment  
    from subprocess import STARTUPINFO, CREATE_NEW_CONSOLE, STARTF_USESHOWWINDOW, CREATE_NO_WINDOW

# from base.common_utils          import *

import logging
logger = logging.getLogger(__name__)
# logger.setLevel(logging.WARNING)


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

        self._tmp_inpFile       = None                      # tmpfile of this instance, to be deleted 


    def __repr__(self) -> str:
        """ nice representation of self """
        return f"<{type(self).__name__}"

   
    def isReady (self, project_dir : str, min_version : str = '') -> bool:
        """ 
        checks if self is available with min_version.

        Args: 
            project_dir: directory where there should be ./assets/... 
            min_version: check fpr min version number 
        """


        # ready already checked? 

        if self.ready: return True

        # find .exe

        version_ok = False
        ready_msg  = None 
        cls = self.__class__

        if self.exe_dir is None: 

            exe_dir, ready_msg = self._get_exe_dir (project_dir)

            if exe_dir is None:                                        # self not found anywhere
                cls.ready_msg = ready_msg
                logger.warning (ready_msg)
                return 
            else:     
                cls.exe_dir = exe_dir
                logger.debug (ready_msg)

        # try to execute with -h help argument to get version 

        try: 
            returncode = self._execute ('-h', capture_output=True)
        except: 
            returncode = 1

        # extract version and check version
        if returncode == 0 :
            for line in self._pipe_out_lines: 
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

            else: 

                self._returncode       = self._popen.returncode

                new_lines = self._popen.stderr.readlines() if self._popen.stderr else []
                new_lines = [x.rstrip() for x in new_lines]                 # remove \r
                self._pipe_error_lines.extend(new_lines)
                
                new_lines = self._popen.stdout.readlines() if self._popen.stdout else []
                new_lines = [x.rstrip() for x in new_lines]
                self._pipe_out_lines.extend(new_lines)

                if self._returncode:
                    logger.error (f"==> {self.name} returncode: {self._returncode} - {'\n'.join (self._pipe_error_lines)}")
                logger.debug (f"==> {self.name} finished {'\n'.join (self._pipe_out_lines)}")
                 
                self._popen = None              # close down process instance 

        return isRunning


    def remove_tmp_file (self, iretry = 0):
        """ os remove of temporary input file"""

        if self._tmp_inpFile:            # remove tmpfile of worker 
            try: 
                os.remove(self._tmp_inpFile) 
            except OSError as exc: 
                if iretry < 5: 
                    iretry +=1
                    logger.debug (f"{self} Could not delete tmp input file '{self._tmp_inpFile}' - Retry {iretry}")
                    time.sleep (0.1)
                    self.remove_tmp_file (iretry=iretry)
                else: 
                    logger.error (f"Could not delete tmp input file '{self._tmp_inpFile}' - {exc}")
            self._tmp_inpFile = None 


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


    def finalize (self):
        """ do cleanup actions """

        self.remove_tmp_file ()


    def terminate (self):
        """ terminate os process of self"""

        if self.isRunning():
            self._popen.terminate()
            self._popen = None 


    # ---------- Private --------------------------------------

    def _execute (self, args = [], workingDir = None, capture_output : bool =False):
        """sync execute self in workingDir 

        Args:
            args: arguments of subprocess as list of strings
            capture_output: capture output in pipe. Defaults to False.
        Returns:
            returncode: = 0 if no error
        """

        returncode  = 0 
        self._pipe_out_lines    = []                        # errortext lines from stderr when finished
        self._pipe_error_lines  = []                        # output lines from stdout when finished

        # build list of args needed by subprocess.run 

        exe = os.path.join (self.exe_dir, self.name) 

        if isinstance (args, list):
            arg_list = [exe] + args
        elif isinstance (args, str):
            arg_list = [exe] + [args]
        else: 
            arg_list = [exe]

        try:

            # uses subproocess run which returns a completed process instance 

            if workingDir:
                curDir = os.getcwd()
                os.chdir (workingDir)

            if capture_output:
                # needed when running as pyinstaller .exe 
                # https://stackoverflow.com/questions/7006238/how-do-i-hide-the-console-when-i-use-os-system-or-subprocess-call/7006424#7006424
                if os.name == 'nt':
                    flags = CREATE_NO_WINDOW    
                else: 
                    flags  = 0                      # posix must be 0       

            logger.debug (f"==> {self.name} run sync: '{args}'")

            process = run (arg_list, text=True, 
                                    capture_output=capture_output, creationflags=flags)

            returncode  = process.returncode

            if returncode:
                logger.error (f"==> {self.name} ended: '{process}'")

            # finished - nice output strings 

            if process.stderr:  
                self._pipe_error_lines = process.stderr.split ("\n")
                logger.error (f"==> {self.name} stderr: {"\n".join (self._pipe_error_lines)}")

            if capture_output and process.stdout: 
                self._pipe_out_lines = process.stdout.split ("\n")
                # logger.debug (f"==> {self.name} stdout: {"\n".join (self._pipe_out_lines)}")

        except FileNotFoundError as exc:

            returncode = 1
            self._pipe_error_lines = str(exc)

            logger.error (f"==> exception {self.name}: {exc}")

        finally: 

            if workingDir: os.chdir (curDir)

        return  returncode


    def _execute_async (self, args = [], workingDir = None, capture_output : bool =False):
        """async execute self in workingDir 

        Args:
            args: arguments of subprocess as list of strings
            capture_output: capture output in pipe. Defaults to False.
        Returns:
            returncode: = 0 if no error
        """

        returncode  = 0 
        self._pipe_out_lines    = []                        # errortext lines from stderr when finished
        self._pipe_error_lines  = []                        # output lines from stdout when finished

        # build list of args needed by subprocess.run 

        exe = os.path.join (self.exe_dir, self.name) 

        if isinstance (args, list):
            arg_list = [exe] + args
        elif isinstance (args, str):
            arg_list = [exe] + [args]
        else: 
            arg_list = [exe]
        # run either sync or async 

        try:

            # uses subproccess Popen instance to start a subprocess

            if workingDir:
                curDir = os.getcwd()
                os.chdir (workingDir)

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

            logger.debug (f"==> {self.name} run async: '{args}'")

            popen = Popen (arg_list, creationflags=flags, text=True, **startupinfo, 
                                stdout=stdout, stderr=stderr)  

            popen.poll()                            # update returncode

            returncode  = popen.returncode

            if returncode:
                logger.error (f"==> {self.name} ended: '{popen}'")

                self._pipe_error_lines = popen.stderr.readlines()
                logger.error (f"==> {self.name} stderr: {"\n".join (self._pipe_error_lines)}")

            # keep for later poll 
            self._popen = popen 

        except FileNotFoundError as exc:

            returncode = 1
            self._pipe_error_lines = str(exc)

            logger.error (f"==> exception {self.name}: {exc}")

        finally: 

            if workingDir: os.chdir (curDir)

        return  returncode



    def _get_popen_startupinfo (self, show : int):
        """ returns popen startinfo parm to eg. minimize shell window - only windows"""

        if os.name == 'nt':
            if show != SW_NORMAL:
                startupinfo = STARTUPINFO()  
                startupinfo.dwFlags |= STARTF_USESHOWWINDOW       
                startupinfo.wShowWindow = show
                return dict(startupinfo=startupinfo)
        return dict(startupinfo=None) 


    def _get_exe_dir (self, project_dir : str): 
        """
        trys to find path to call programName
        
        If found, returns exePath and ready_msg
        If not, return None and ready_msg (error)"""

        exe_dir  = None
        ready_msg = None 

        if os.name == 'nt':
            assets_dir = EXE_DIR_WIN
        else: 
            assets_dir = EXE_DIR_UNIX  

        assets_dir = os.path.normpath (assets_dir)  
        check_dir  = os.path.join (project_dir , assets_dir)

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
                               polarType : str, re :float, ma : float, ncrit : float) -> str:
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


    def check_inputFile (self, inputFile=None):
        """ uses Worker to check an Xoptfoil2"""

        ready, _ = self.isReady()
        if not ready: return 1, self.name + " not ready"

        error_text = ""
        args = ['-w', 'check-input', '-i', inputFile]

        returncode = self._execute (args, capture_output=True)

        if returncode != 0:

            # worker output should something like ...
            #  Worker   -check-input jx-gt-10v3.inp
            #  - Processing input
            #    - Reading input jx-gt-10v3.inp
            #    - Output prefix jx-gt-10v3
            #  Error: max_speed should be between 0.001 and 0.5

            for line in self._pipe_out_lines:
                error_text = line.partition("Error:")[2].strip()
                if error_text != '': break 
            if error_text == '':
                raise ValueError ("Errortext not found in Workers")

        return returncode, error_text 


    def generate_polar (self, airfoil_pathFileName, 
                        polarType : str, 
                        re : float | list, 
                        ma : float | list, 
                        ncrit : float,
                        autoRange = True, spec = 'alpha', valRange= [-3, 12, 0.25], 
                        nPoints=None, run_async = True) -> int:
        """ 
        Generate polar for airfoilPathFileName in directory of airfoil.
        Returncode = 0 if successfully started (async) or finish (sync)
        """ 

        if not os.path.isfile(airfoil_pathFileName): 
            raise ValueError (f"{self}: Airfoil '{airfoil_pathFileName}' does not exist")

        if (polarType == 'T2'):
            polarTypeNo = 2
        else: 
            polarTypeNo = 1

        if spec == 'alpha':
            spec_al = True
        else: 
            spec_al = False

        if not isinstance (re, list): re = [re]
        if not isinstance (ma, list): ma = [ma]

        # a temporary input file for polar generation is created
        self._tmp_inpFile = self._generate_polar_inputFile (airfoil_pathFileName, 
                                    re, ma, polarTypeNo, ncrit, autoRange, spec_al, valRange,
                                    nPoints=nPoints) 
        if not self._tmp_inpFile:
            raise RuntimeError (f"{self.name} polar generation failed: Couldn't create input file")

        # build args for worker 

        args, workingDir = self._build_worker_args ('polar',airfoil1=airfoil_pathFileName, 
                                                    inputfile=self._tmp_inpFile)

        # .execute either sync or async

        if run_async: 

            returncode = self._execute_async (args, capture_output=True, workingDir=workingDir)

        else:

            returncode = self._execute       (args, capture_output=True, workingDir=workingDir)

            self.remove_tmp_file (self._tmp_inpFile)         
        
        if returncode: 
            raise RuntimeError (f"Worker polar generation failed for {airfoil_pathFileName}")
            



    def clean_workingDir (self, workingDir):
        """ 
        deletes temporary (older) files Worker creates in workingDir
        """ 
        if os.path.isdir(workingDir):

            # remove tmp input files of polar generation 
            match_path = os.path.join (workingDir, f"{TMP_INPUT_NAME}*{TMP_INPUT_EXT}")
            
            for f in glob(match_path):
                os.remove(f)


# ---------- Private --------------------------------------



    def _build_worker_args (self, action, actionArg='', airfoil1='', airfoil2='', outname='', inputfile=''):
        """ return worker args as list of strings and working dir extracted from airfoil1"""

        airfoil1_dir, airfoil1_fileName = os.path.split(airfoil1)
        _,            airfoil2_fileName = os.path.split(airfoil2)


        if (airfoil1_dir != ''):
            self.workingDir = airfoil1_dir
            # in this case also strip airfoil2 
            if (airfoil2 != ''):
                _, airfoil2_fileName = os.path.split(airfoil2)
        else:
            self.workingDir = None

        # info inputfile is in a dir - strip dir from path - local execution 
        if (inputfile != ''):
            _, local_inputfile = os.path.split(inputfile)
        else: 
            local_inputfile = '' 

        args = []

        if (action != ''): 
            if (action == 'help'): 
                args.extend(['-h'])
            else:                  
                args.extend(['-w', action])
                if (actionArg): args.extend([actionArg]) 
        else: 
            raise ValueError ('action for worker is mandatory')

        if (airfoil1 ): args.extend(['-a',  airfoil1_fileName])
        if (airfoil2 ): args.extend(['-a2', airfoil2_fileName])
        if (outname  ): args.extend(['-o',  outname]) 
        if (inputfile): args.extend(['-i',  local_inputfile])

        return  args, airfoil1_dir



    def _generate_polar_inputFile (self, airfoilPathFileName, 
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

    # init logging 

    logging.basicConfig(format='%(levelname)-8s- %(message)s', 
                        level=logging.DEBUG)  # DEBUG or WARNING


    Worker().isReady (project_dir="..\\..", min_version='1.0.3')

    if Worker.ready:

        worker = Worker()

        if os.path.isfile ('..\\..\\test_airfoils\\MH 30.dat'):
            airfoil = '..\\..\\test_airfoils\\MH 30.dat'
        elif os.path.isfile ('MH 30.dat'):
            airfoil = 'MH 30.dat'
        else: 
            logger.error (f"Airfoil file 'MH 30.dat' not found")
            exit()

        # build name of polar dir from airfoil file 
        polarDir = str(Path(airfoil).with_suffix('')) + '_polars'

        # ------- sync test ---------------------------------------------

        try: 
            worker.generate_polar (airfoil, 'T1', 700000, 0.0, 8.0, run_async=False)
            logger.info ("\n".join (worker._pipe_out_lines))

            polar_file = worker.get_existingPolarFile (airfoil, 'T1', 700000, 0.0, 8.0)

            if polar_file:
                logger.info  (f"polar file found: {polar_file}")
            else: 
                logger.error (f"polar file not found")

            worker.finalize ()
            worker.remove_polarDir (airfoil, polarDir)

        except ValueError as exc:
            logger.error (f"{exc}")
        except RuntimeError as exc:
            # logger.error (f"Polar failed: {exc}")
            logger.error (f"{worker}: {worker.finished_errortext}")

        

        # ------- async test ---------------------------------------------

        worker = Worker()

        try: 
            worker.generate_polar (airfoil, 'T1', 700000, 0.0, 8.0, run_async=True)

            secs = 0 
            while worker.isRunning ():
                time.sleep (0.5)
                secs += 0.5
                logger.debug (f"{worker} waiting: {secs}s")

            if worker.finished_returncode == 0:
                logger.info ("\n".join (worker._pipe_out_lines))

                polar_file = worker.get_existingPolarFile (airfoil, 'T1', 700000, 0.0, 8.0)

                if polar_file:
                    logger.info  (f"polar file found: {polar_file}")
                else: 
                    logger.error (f"polar file not found")
            else: 
                logger.error (f"{worker}: {worker.finished_errortext}")

            worker.finalize ()
            worker.remove_polarDir (airfoil, polarDir)

        except ValueError as exc:
            logger.error (f"{exc}")
        except RuntimeError as exc:
            # logger.error (f"Polar failed: {exc}")
            logger.error (f"{worker}: {worker.finished_errortext}")
