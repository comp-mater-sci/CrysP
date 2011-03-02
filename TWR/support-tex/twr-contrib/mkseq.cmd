@echo off
rem $Id$

rem Algorithm: create a sequence of strings
rem Parameters:  VARNAME start step end [prefix] [suffix]
rem 

set VARNAME=%1
set %VARNAME%=

rem seq -f "%4%%g%5" %1 %2 %3

setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION 
set X=
for /F "tokens=*" %%A in ('seq -f "%5%%g%6" %2 %3 %4') do (
	set X=!X! %%A
)

echo %X%

endlocal
rem echo %X%
