@echo off
rem $Id$

setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION

call support_path.cmd
rem check number of parameters
call argc.cmd %*
set NARGS=!ERRORLEVEL!
if !NARGS! LSS 4 goto noparams

set CFILE=%1
set FIRSTSTEP=%2
set LASTSTEP=%3
set PREFIX=%4
set SUFFIX=.c
rem Detect inputs that cannot be digested by versch (up to 9 files can be processed...)

rem Create a sequence of names
set STEPFILES=
for /F "tokens=*" %%A in ('seq -f "!PREFIX!%%g!SUFFIX!" !FIRSTSTEP! !LASTSTEP!') do (
	set STEPFILES=!STEPFILES! %%A
)
echo !STEPFILES!

call calcDiff.cmd !CFILE! !STEPFILES!

goto :eof

:noparams
echo The script calculates ODF difference for a given C-file and range of C-files.
echo calcDiffSteps C-file step_start step_end stepprefix 
exit /B 1
goto :eof



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
