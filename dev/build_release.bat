@echo off

set APP_VERSION=3.1.1
set WIN_RELEASE=PlanformCreator2_%APP_VERSION%_win

echo.
echo ------ Packaging PlanformCreator2 windows runtime into %WIN_RELEASE%
echo.
echo.  ? build_win_exe.bat run ?  (in .\dist is actual build version)  
echo.  ? README.pdf ready ?       (in VSCode export readme.md as pdf)
echo.
echo.
pause

cd ..

rem ----- clean dist folder ------------

if exist dist\PlanformCreator2\Root_Example.dat       del dist\PlanformCreator2\Root_Example.dat  /q
if exist dist\PlanformCreator2\Tip_Example.dat       del dist\PlanformCreator2\Tip_Example.dat  /q
if exist dist\PlanformCreator2\_internal\PlanformCreator2.settings del dist\PlanformCreator2\_internal\PlanformCreator2.settings  /q
if exist dist\PlanformCreator2\airfoil_straks_tmp rd dist\PlanformCreator2\airfoil_straks_tmp  /s /q

if     exist releases\%WIN_RELEASE%     rd  releases\%WIN_RELEASE% /s /q
if     exist releases\%WIN_RELEASE%.zip del releases\%WIN_RELEASE%.zip  /q

mkdir releases\%WIN_RELEASE%


rem --- xcopy templates    

xcopy examples\Amokka-JX\*.pc2     releases\%WIN_RELEASE%\examples\Amokka-JX\*.*   /i /q
xcopy examples\Amokka-JX\*.dat     releases\%WIN_RELEASE%\examples\Amokka-JX\*.*   /i /q
xcopy examples\VJX.glide\*.pc2     releases\%WIN_RELEASE%\examples\VJX.glide\*.*   /i /q
xcopy examples\VJX.glide\*.dat     releases\%WIN_RELEASE%\examples\VJX.glide\*.*   /i /q

xcopy README.pdf                   releases\%WIN_RELEASE%\               /i /q

xcopy dist\PlanformCreator2        releases\%WIN_RELEASE%\   /s /i /q

rem ----- zip with powershell ------------

cd releases
powershell Compress-Archive %WIN_RELEASE%\* %WIN_RELEASE%.zip
cd ..
dir releases\%WIN_RELEASE% 
pause