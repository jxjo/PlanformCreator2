@echo off

setlocal
set CUR_DIR=%cd%
set APP_NAME=PlanformCreator2
set DIST_DIR=dist
set ICON_NAME=PC2.ico


if not exist pyproject.toml cd ..
if not exist pyproject.toml goto end

rem ---- get package name and version with hatch https://hatch.pypa.io/latest/cli/reference/

hatch project metadata name > tmpFile 
set /p PACKAGE_NAME= < tmpFile 
hatch project metadata version > tmpFile 
set /p PACKAGE_VERSION= < tmpFile 
del tmpFile 

set WIN_EXE_DIR=%PACKAGE_NAME%-%PACKAGE_VERSION%_win_exe

rem ---- run pyinstaller 

echo.
echo ------ Pyinstaller: Build ...win.exe on %PACKAGE_NAME% %PACKAGE_VERSION% in %DIST_DIR%
echo.

pause

rem needed for pyinstaller to avoid "WARNING: lib not found: api-ms-win-crt ..." 
setlocal
set PATH=%PATH%;C:\Windows\System32\downlevel
rem to show missing imports: 			--debug imports ^
rem also look in modules for imports!: 	--paths modules ^
rem more infos during build:		 	--log-level=INFO
rem suppress console  					--noconsole    ^
pyinstaller --noconfirm --log-level=INFO  --onedir  ^
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

rem ---- zip directory 

echo.
echo ------ zip %WIN_EXE_DIR% into %WIN_EXE_DIR%.zip
echo.
pause

if exist %WIN_EXE_DIR%.zip del %WIN_EXE_DIR%.zip
powershell Compress-Archive %WIN_EXE_DIR%\* %WIN_EXE_DIR%.zip

echo.
echo ------ Finished successfully! 
echo.

:end
cd %CUR_DIR%
pause