@echo off
rem $Id$

if "%1" == "" goto noparams
if "%2" == "" goto noparams
if "%3" == "" goto noparams
rem if "%4" == "" goto noparams


rem enable extensions
SETLOCAL ENABLEDELAYEDEXPANSION
SETLOCAL ENABLEEXTENSIONS

set PREFIX=%1
set RSTART=%2
set REND=%3
set RSTEP=%4
set OUTPREFIX=%5


@rem convert -trim -density 300x300 step10.ps  -depth 3 -quality 90 -resize 1857x679 -resample 150x150  step10.png
set LIST=


for /L %%i in (%RSTART%,%RSTEP%,%REND%) do (
	set LIST=!LIST! !PREFIX!%%i.png
)
convert -loop 0 !LIST! -depth 3 -delay 30 %OUTPREFIX%.gif

goto stop

:noparams
echo The script converts a set of PNG files (prefixed by the parameter FILEPREFIX) into animated GIF file. 
echo It can also generate a GIF animation.
echo Syntax:
echo convSteps.cmd FILEPREFIX step_start step_stop step animation_file_prefix
goto stop

:stop
