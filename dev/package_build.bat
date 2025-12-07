@echo off

set CUR_DIR=%cd%

if not exist pyproject.toml cd ..
if not exist pyproject.toml goto end

rem ---- get package name and version with hatch https://hatch.pypa.io/latest/cli/reference/

hatch project metadata name > tmpFile 
set /p PACKAGE_NAME= < tmpFile 
hatch project metadata version > tmpFile 
set /p PACKAGE_VERSION= < tmpFile 
del tmpFile 

rem ---- run Pytest  for test_*.py
rem          exclude slow test like polar with -m "not slow"

echo ------ Pytest %PACKAGE_NAME% %PACKAGE_VERSION% 
echo.

rem Pytest tests\  -m "not slow"
Pytest tests\  

rem ---- build package - wheel and sdist 

echo.
echo ------ Packaging %PACKAGE_NAME% %PACKAGE_VERSION% into .\dist 
echo.
pause

hatch clean
hatch build

echo.
echo ------ Finished successfully! 
echo.

:end
cd %CUR_DIR%
pause
