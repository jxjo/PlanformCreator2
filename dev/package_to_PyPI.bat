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

rem ---- upload package - wheel and sdist 

echo.
echo ------ Upload %PACKAGE_NAME% %PACKAGE_VERSION% wheel and sdist to PyPI 
echo.

dir dist /a:-d |find "%PACKAGE_NAME%"
echo.
pause
echo.

hatch publish --user __token__ 
echo.

:end
cd %CUR_DIR%
pause
