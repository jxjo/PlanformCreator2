@echo off
echo.
echo Pull Subtree AirfoilEditor
echo.
pause
cd ..
git subtree pull --prefix AirfoilEditor_subtree https://github.com/jxjo/AirfoilEditor v3_dev --squash
cd dev
pause