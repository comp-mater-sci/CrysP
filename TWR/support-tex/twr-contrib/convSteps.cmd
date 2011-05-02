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

set ANIMATION=%4


@rem convert -trim -density 300x300 step10.ps  -depth 3 -quality 90 -resize 1857x679 -resample 150x150  step10.png
set LIST=

set SEDFILE=tmprmpage.sed
rem Construct SED expression file
rem NOTE: there are two "^" marks - the first one is interpreted by echo command.  
echo N > %SEDFILE%
echo /^^\/Helvetica findfont    185 scalefont setfont.* 1846 M  270.0 R (PAGE) show -270.0 R/d >> %SEDFILE%
echo /^^\/Helvetica findfont    185 scalefont setfont.* 1023 M  270.0 R ( 1) show -270.0 R/d >> %SEDFILE%


for /L %%i in (%2,1,%3) do (
	set TMPPS=tmp_step%%i.ps
	set OUTPREFIX=!PREFIX!%%i
	echo !OUTPREFIX!
	set LIST=!LIST! !PREFIX!%%i.png
	rem Prepare "filtered" postscript file
	sed -f %SEDFILE% !PREFIX!%%i.ps > !TMPPS!
	rem Convert to png
	call convPlot.cmd !TMPPS! !OUTPREFIX!
	rem Sanitize after each step: 
	del /f !TMPPS!
)
rem Clean-up
del /f %SEDFILE%

if NOT "%ANIMATION%" == "" (
echo %ANIMATION%.gif
rem create the animation
convert -loop 0 !LIST! -depth 3 -delay 30 %ANIMATION%.gif
)

goto stop

:noparams
echo The script converts a set of postscript files (prefixed by the parameter FILEPREFIX) into a set of PNG files. 
echo It can also generate a GIF animation.
echo Syntax:
echo convSteps.cmd FILEPREFIX step_start step_stop [animation_file_prefix]
goto stop

:stop
