@echo off

set CUR_DIR=%cd%

if not exist pyproject.toml cd ..
if not exist pyproject.toml goto end

echo.
echo ------  Upload package to PyPi using Hatch  ...
echo.

rem ---- get package name and version with hatch https://hatch.pypa.io/latest/cli/reference/

for /f "delims=" %%i in ('hatch project metadata name') do set PACKAGE_NAME=%%i
for /f "delims=" %%i in ('hatch project metadata version') do set PACKAGE_VERSION=%%i

echo Package name     : %PACKAGE_NAME%
echo Package version  : %PACKAGE_VERSION%
echo From directory   : %cd%\dist

echo.
dir dist /a:-d |find "%PACKAGE_NAME%-%PACKAGE_VERSION%"
echo.
pause

rem ---- upload package - wheel and sdist 

echo.
hatch publish --user __token__ 
echo.

:end
cd %CUR_DIR%
pause
