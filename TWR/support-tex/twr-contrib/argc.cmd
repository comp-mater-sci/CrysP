@echo off
rem $Id$

rem Algorithm: count the parameters that are passed to the script, return this number as errorlevel
rem 
:argc
setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION 
set /A cnt=0 
for %%i in (%*) do set /A cnt=!cnt! + 1
exit /B !cnt!
goto :eof

