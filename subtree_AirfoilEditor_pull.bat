@echo off
echo.
echo Pull Subtree AirfoilEditor
echo.
pause
git subtree pull --prefix AirfoilEditor_subtree https://github.com/jxjo/AirfoilEditor main --squash
pause