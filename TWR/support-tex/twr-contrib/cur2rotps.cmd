@echo off
rem $Id$

setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION
rem Import settings
call support_path.cmd

if "%1" == "" goto noparams
if "%2" == "" goto noparams
if "%3" == "" goto noparams
if "%4" == "" goto noparams


set PREFIX=step

set MAXSTEP=%1
set CURFILE=%2
set CLASS=%3
set ROTATION=%4

rem Shift 4 positions
for /L %%i in (1,1,4) do (
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

call cur2c %MAXSTEP% %CURFILE% %CLASS% %COMMENT%

if "%ROTATION%" == "0" (
	set OUTPREFIX=step
) else (
	call rotsteps %PREFIX% 1 %MAXSTEP%  %ROTATION%  %CLASS%  %COMMENT%
	set OUTPREFIX=rstep
	
)
call steps2ps %OUTPREFIX%  1  %MAXSTEP%


goto stop 

:noparams
echo Usage:
echo  %0 max_steps curfile class rotation_angle  [comment ...]
echo Note that total comment length should not exceed 40 characters.
:stop