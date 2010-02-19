@echo off

setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION

if "%1" == "" goto noparams
if "%2" == "" goto noparams
if "%3" == "" goto noparams
if "%4" == "" goto noparams

if not exist WAGNER.B04 goto nofiles

set PREFIX=%1
set COMMENT=%6 %7 %8 %9

for /L %%i in (%2, 1, %3) do (

echo 00.0        00.0      %4               PHI1/PHI/PHI2 >  rottex%%i.i01
if "%5" == "B"  echo    1    2                                IMPOSED IMAG, IMPOSED IDN  >>  rottex%%i.i01
if "%5" == "F"  echo    2    1                                IMPOSED IMAG, IMPOSED IDN  >>  rottex%%i.i01
echo %COMMENT% Step %%i   >>  rottex%%i.i01

rottex.exe rottex%%i.i01 rottex%%i.l01 WAGNER.B04 %PREFIX%%%i.c R%PREFIX%%%i.c
)

del /F R%PREFIX%.001

goto stop

:noparams
echo Usage: %0 prefix step_start step_end phi2angle symmetry{B or F}  [comment ...]
goto stop

:nofiles
echo One of the files doesn't exist:
echo WAGNER.B04
goto stop



:stop

