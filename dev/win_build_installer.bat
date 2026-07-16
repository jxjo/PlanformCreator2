@echo off
setlocal
set CUR_DIR=%cd%

if not exist pyproject.toml cd ..
if not exist pyproject.toml goto error_no_project

py -3 dev\win_build.py installer
if %errorlevel% neq 0 goto end

goto end

:error_no_project
echo.
echo ERROR: pyproject.toml not found!
echo Please run this script from the dev directory or project root.
echo.

:end
cd %CUR_DIR%
pause
