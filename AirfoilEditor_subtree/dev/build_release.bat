@echo off

set APP_VERSION=3.0
set WIN_RELEASE=AirfoilEditor_%APP_VERSION%_win

echo.
echo ------ Packaging AirfoilEditor windows runtime into %WIN_RELEASE%
echo.
echo.  ? build_win_exe.bat run ?  (in .\dist is actual build version)  
echo.  ? README.pdf ready ?       (in VSCode export readme.md as pdf)
echo.
echo.
pause

cd ..

rem ----- clean dist folder ------------

if exist dist\AirfoilEditor\Example.dat       del dist\AirfoilEditor\example.dat  /q
if exist dist\AirfoilEditor\Example_polars    rd  dist\AirfoilEditor\Example_polars  /s /q
if exist dist\AirfoilEditor\_internal\AirfoilEditor.settings del dist\AirfoilEditor\_internal\AirfoilEditor.settings  /q

pause 

rem ----- clean releases folder ------------

if     exist releases\%WIN_RELEASE% rd releases\%WIN_RELEASE% /s /q
if     exist releases\%WIN_RELEASE%.zip del releases\%WIN_RELEASE%.zip  /q

mkdir "releases\%WIN_RELEASE%"

rem ----- copy files ------------

if not exist Readme.pdf goto end
xcopy README.pdf                   releases\%WIN_RELEASE%\   /i /q

xcopy dist\AirfoilEditor           releases\%WIN_RELEASE%\   /s /i /q

pause 

rem ----- zip with powershell ------------
cd releases
powershell Compress-Archive %WIN_RELEASE%\* %WIN_RELEASE%.zip
cd ..
dir releases\%WIN_RELEASE% 
pause
:end