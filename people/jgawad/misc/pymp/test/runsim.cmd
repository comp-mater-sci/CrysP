@echo off
echo Running simulation
if "%1" == "" exit /b 1
tr -s [:cntrl:] > %1
echo %CD% >> %1
