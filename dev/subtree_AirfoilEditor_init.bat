@echo off
echo.
echo Init Subtree AirfoilEditor
echo.
pause
cd ..
git subtree add --prefix AirfoilEditor_subtree https://github.com/jxjo/AirfoilEditor main --squash
cd dev
pause