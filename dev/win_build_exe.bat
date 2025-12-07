@echo off

setlocal
set CUR_DIR=%cd%
set APP_NAME=PlanformCreator2
set DIST_DIR=dist
set ICON_NAME=PC2.ico


if not exist pyproject.toml cd ..
if not exist pyproject.toml goto end

echo.
echo ------  Create Windows exe using Pyinstaller  ...
echo.

rem ---- Get package version using hatch

for /f "delims=" %%i in ('hatch project metadata name') do set PACKAGE_NAME=%%i
for /f "delims=" %%i in ('hatch project metadata version') do set PACKAGE_VERSION=%%i

set WIN_EXE_DIR=%PACKAGE_NAME%-%PACKAGE_VERSION%_win_exe

echo App              : %APP_NAME%
echo Icon             : %ICON_NAME%
echo Package name     : %PACKAGE_NAME%
echo Package version  : %PACKAGE_VERSION%
echo In directory     : %DIST_DIR%
echo Pyinstaller exe  : %WIN_EXE_DIR%

echo.
echo ------ Pyinstaller: Build %APP_NAME% 
echo.

pause

rem needed for pyinstaller to avoid "WARNING: lib not found: api-ms-win-crt ..." 
setlocal
set PATH=%PATH%;C:\Windows\System32\downlevel

rem to show missing imports: 					--debug imports ^
rem also look in airfoileditor for imports!: 	--paths airfoileditor ^
rem more infos during build:		 			--log-level=INFO
rem suppress console  	--noconsole    ^
pyinstaller --noconfirm --log-level=INFO  --onedir --noconsole ^
	--icon=./icons/%ICON_NAME% ^
	--paths %PACKAGE_NAME% ^
    --add-data="./icons;%PACKAGE_NAME%/icons" ^
    --add-data="./templates;%PACKAGE_NAME%/templates" ^
    --add-data="./examples;%PACKAGE_NAME%/examples" ^
    --collect-data airfoileditor ^
	--runtime-tmpdir="mySuperTemp" ^
	--exclude-module matplotlib ^
	-n %APP_NAME% ^
    %PACKAGE_NAME%.py 

rem ---- copy README into dir 

echo.
echo ------ copy README.pdf into %DIST_DIR%\%APP_NAME%
echo.

if exist README.pdf copy README.pdf %DIST_DIR%\%APP_NAME%

rem ---- rename target
 
echo.
echo ------ rename %APP_NAME% to %WIN_EXE_DIR%
echo.

cd %DIST_DIR%

if exist %WIN_EXE_DIR% rd /S /Q %WIN_EXE_DIR%
if exist %WIN_EXE_DIR% (
	echo Error: %WIN_EXE_DIR% couldn't be deleted
	goto end
)
ren %APP_NAME% %WIN_EXE_DIR%

rem - no more zip 
goto finished

rem ---- zip directory 

echo.
echo ------ zip %WIN_EXE_DIR% into %WIN_EXE_DIR%.zip
echo.
pause

if exist %WIN_EXE_DIR%.zip del %WIN_EXE_DIR%.zip
powershell Compress-Archive %WIN_EXE_DIR%\* %WIN_EXE_DIR%.zip

:finished
echo.
echo ------ Finished successfully! 
echo.

:end
cd %CUR_DIR%
pause
