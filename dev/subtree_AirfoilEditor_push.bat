@echo off
echo.
echo Push Subtree AirfoilEditor
echo.
pause
cd ..
git subtree pull --prefix AirfoilEditor_subtree https://github.com/jxjo/AirfoilEditor main --squash
cd dev
pause