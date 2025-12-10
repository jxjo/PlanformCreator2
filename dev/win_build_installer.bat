@echo off
rem ========================================================================
rem Build Windows Installer using NSIS
rem 
rem Prerequisites:
rem   1. Install NSIS from https://nsis.sourceforge.io/
rem   2. Run win_build_exe.bat first to create the PyInstaller build
rem ========================================================================

setlocal
set CUR_DIR=%cd%
set APP_NAME=PlanformCreator2
set DESCRIPTION="An interactive wing planform design tool"
set ICON_NAME=PC2.ico

if not exist pyproject.toml cd ..
if not exist pyproject.toml goto error_no_project

echo.
echo ------  Create Windows Installer ...
echo.

rem ---- Get package version using hatch

for /f "delims=" %%i in ('hatch project metadata name') do set PACKAGE_NAME=%%i
for /f "delims=" %%i in ('hatch project metadata version') do set PACKAGE_VERSION=%%i

set WIN_EXE_DIR=%PACKAGE_NAME%-%PACKAGE_VERSION%_win_exe
set INSTALLER_NAME=%PACKAGE_NAME%-%PACKAGE_VERSION%_win_setup.exe

echo App              : %APP_NAME%
echo Desciption       : %DESCRIPTION%
echo Icon             : %ICON_NAME%
echo Package name     : %PACKAGE_NAME%
echo Package version  : %PACKAGE_VERSION%
echo Pyinstaller exe  : %WIN_EXE_DIR%
echo Installer name   : %INSTALLER_NAME%

rem ---- Check if PyInstaller build exists

if not exist "dist\%WIN_EXE_DIR%\%APP_NAME%.exe" (
    echo.
    echo ERROR: PyInstaller build %APP_NAME%.exe not found!
    echo Please run win_build_exe.bat first to create the executable.
    echo.
    pause
    goto end
)

rem ---- Check if NSIS is installed
where makensis.exe >nul 2>&1
if %errorlevel% neq 0 (
    echo.
    echo ERROR: NSIS not found!
    echo Please install NSIS from https://nsis.sourceforge.io/
    echo Make sure makensis.exe is in your PATH.
    echo.
    pause
    goto end
)

rem ---- Run NSIS to create installer
echo.
echo ------ Creating installer with NSIS...
echo.

pause

makensis.exe /V3 ^
    /DVERSION=%PACKAGE_VERSION% ^
    /DAPP_NAME=%APP_NAME% ^
    /DPACKAGE_NAME=%PACKAGE_NAME% ^
    /DWIN_EXE_DIR=%WIN_EXE_DIR% ^
    /DDESCRIPTION=%DESCRIPTION% ^
    /DICON_NAME=%ICON_NAME% ^
    /DINSTALLER_NAME=%INSTALLER_NAME% ^
    dev\win_installer.nsi

if %errorlevel% equ 0 (
    echo.
    echo --------------------------------------------------------------------
    echo Installer created: dist\%APP_NAME%_%PACKAGE_VERSION%_setup.exe
    echo --------------------------------------------------------------------
    echo.
) else (
    echo.
    echo ERROR: NSIS compilation failed!
    echo.
)

pause
goto end

:error_no_project
echo.
echo ERROR: pyproject.toml not found!
echo Please run this script from the dev directory or project root.
echo.
pause

:end
cd %CUR_DIR%
