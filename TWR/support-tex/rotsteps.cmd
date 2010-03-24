@echo off

setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION
rem Import settings
call support_path.cmd

if "%1" == "" goto noparams
if "%2" == "" goto noparams
if "%3" == "" goto noparams
if "%4" == "" goto noparams

rem Check the prequisities
rem Try to satisfy file dependencies:
set DEPLIST=WAGNER.B04
for %%f in ( %DEPLIST% ) do (
	if not exist %%f copy %TEXSUPPORT_DATA%\%%f %%f
	if not exist %%f goto depfailed
)

set RETCODE=0

set PREFIX=%1
set FSTEP=%2
set LSTEP=%3
set ROTANGLE=%4
set SYMCLASS=%5
rem Shift 5 positions
for /L %%i in (1,1,5) do (
	shift /1
)
rem Build  the comment 
set COMMENT=%1
for %%i in (%2 %3 %4 %5 %6 %7 %8 %9) do (
	set COMMENT=!COMMENT! %%i
)	

for /L %%i in (%FSTEP%, 1, %LSTEP%) do (

echo 00.0        00.0      %ROTANGLE%               PHI1/PHI/PHI2 >  rottex%%i.i01
if "%SYMCLASS%" == "B"  echo    1    2                                IMPOSED IMAG, IMPOSED IDN  >>  rottex%%i.i01
if "%SYMCLASS%" == "F"  echo    2    1                                IMPOSED IMAG, IMPOSED IDN  >>  rottex%%i.i01
echo %COMMENT% Step %%i   >>  rottex%%i.i01

rottex.exe rottex%%i.i01 rottex%%i.l01 WAGNER.B04 %PREFIX%%%i.c R%PREFIX%%%i.c
)

del /Q /F R%PREFIX%*.001

goto stop

:noparams
echo Usage: %0 prefix step_start step_end phi2angle symmetry{B or F}  [comment ...]
goto stop

:depfailed
echo Cannot satisfy the prerequisities:
echo %DEPLIST%
set RETCODE=1
goto stop

:stop
exit /B %RETCODE%

