@echo off
echo.
echo Make onedir exe with pyinstaller in .\dist
echo.
echo ! Dont't forget to increase version number in source file ! 
echo.

echo - Running Pytest  for *-test.py
echo.
cd ..
Pytest

pause

rem this is needed for pyinstaller to avoid "WARNING: lib not found: api-ms-win-crt ..." 
set PATH=%PATH%;C:\Windows\System32\downlevel

rem to show missing imports: 			--debug imports ^
rem also look in modules for imports!: 	--paths modules ^
rem more infos during build:		 	--log-level=INFO

echo.
echo - Building AirfoilEditor.exe  in dist\AirfoilEditor
echo.
rem suppress console  	--noconsole    ^
pyinstaller --noconfirm --log-level=INFO  --onedir  --noconsole   ^
	--icon=./modules/AE_ico.ico ^
	--paths modules ^
    --add-data="./modules/base/icons;./icons" ^
    --add-data="./modules/AE_ico.ico;./icons" ^
    --add-data="./assets/windows/worker.exe;./assets/windows" ^
	--runtime-tmpdir="mySuperTemp" ^
	--exclude-module matplotlib ^
    AirfoilEditor.py 

echo.
pause 

