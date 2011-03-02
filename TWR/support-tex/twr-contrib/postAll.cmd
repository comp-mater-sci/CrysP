@echo off
rem $Id$

setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION
rem Import settings
call support_path.cmd

if "%1" == "" goto noparams
if "%2" == "" goto noparams
if "%3" == "" goto noparams

set PREFIX=%1
set CLASS=%2
set ROTATION=%3

rem Shift 3 positions
for /L %%i in (1,1,3) do (
	shift /1
)
rem Build  the comment 
set COMMENT=%1
shift /1
for %%i in (%1 %2 %3 %4 %5 %6 %7 %8 %9) do (
	set COMMENT=!COMMENT! %%i
	shift /1
)
rem parameters 9 - 18	
for %%i in (%1 %2 %3 %4 %5 %6 %7 %8 %9) do (
	set COMMENT=!COMMENT! %%i
	shift /1
)
rem echo !COMMENT!

rem get number of points
for /F "tokens=1" %%A in ('tail -n 1 %PREFIX%_map.txt') do set NSTEPS=%%A

echo %NSTEPS% to be processed

rem goto :eof

if  "%NSTEPS%" LEQ "0" (
	set ERRMSG=Cannot detect number of steps
	goto :error
)


if "%ROTATION%" == "0" (
	set OUTPREFIX=step
) else (
	set OUTPREFIX=rstep
)

rem echo %PREFIX%
rem echo %CLASS%
rem echo %ROTATION%
rem echo %OUTPREFIX%

if NOT EXIST %PREFIX%.cur (
	set ERRMSG=CUR file %PREFIX%.cur not found.
	goto :error
)

call cur2rotps.cmd %NSTEPS% %PREFIX%.cur %CLASS% %ROTATION% !COMMENT!
if %ERRORLEVEL%  GTR 0 (
	set ERRMSG=cur2rotps failed.
	goto :error
)
call allps2pdf.cmd

call steps2ti %OUTPREFIX% 1 %NSTEPS%

goto :eof

:error
echo %ERRMSG%
goto :eof

:noparams
echo.
echo %0 input_prefix  symmetry_class rotation_angle [comment ...]
echo.
goto :eof

