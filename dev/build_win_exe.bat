@echo off
echo.
echo Make onedir exe with pyinstaller in .\dist
echo.
echo ! Dont't forget to increase version number in source file ! 
echo.
pause

cd ..

rem this is needed for pyinstaller to avoid "WARNING: lib not found: api-ms-win-crt ..." 
set PATH=%PATH%;C:\Windows\System32\downlevel


echo.
echo - Building PlanformCreator2.exe  in dist\PlanformCreator2
echo.
rem to show missing imports: 			--debug imports ^
rem also look in modules for imports!: 	--paths modules ^
rem more infos during build:		 	--log-level=INFO
rem suppress console  					--noconsole    ^
pyinstaller --noconfirm --log-level=INFO  --onedir --noconsole   ^
	--icon=./modules/PC2_ico.ico ^
	--paths="modules" ^
	--paths="AirfoilEditor_subtree/modules" ^
    --add-data="./modules/PC2_ico.ico;./icons" ^
    --add-data="./AirfoilEditor_subtree/modules/base/icons;./icons" ^
    --add-data="./templates;./templates" ^
	--runtime-tmpdir="mySuperTemp" ^
    PlanformCreator2.py 

echo.
pause 
cd dev 

