@echo off

set CUR_DIR=%cd%

if not exist pyproject.toml cd ..
if not exist pyproject.toml goto end

echo.
echo ------  Build package using Hatch  ...
echo.

rem ---- get package name and version with hatch https://hatch.pypa.io/latest/cli/reference/

for /f "delims=" %%i in ('hatch project metadata name') do set PACKAGE_NAME=%%i
for /f "delims=" %%i in ('hatch project metadata version') do set PACKAGE_VERSION=%%i

echo Package name     : %PACKAGE_NAME%
echo Package version  : %PACKAGE_VERSION%
echo In directory     : %cd%\dist

rem ---- run Pytest  for test_*.py
rem          exclude slow test like polar with -m "not slow"

rem ---- build package - wheel and sdist 

echo.
echo ------ Packaging into %cd%\dist 
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
