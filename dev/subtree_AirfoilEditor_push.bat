@echo off
echo.
echo Push Subtree AirfoilEditor
echo.
pause
cd ..
git subtree push --prefix AirfoilEditor_subtree https://github.com/jxjo/AirfoilEditor main
cd dev
pause