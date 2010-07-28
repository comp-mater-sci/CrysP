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
set OUTPREFIX=%PREFIX%%%i
echo !OUTPREFIX!
set LIST=!LIST! %PREFIX%%%i.png
sed -f %SEDFILE% %PREFIX%%%i.ps > tmp_step%%i.ps

convert -density 300x300 tmp_step%%i.ps -trim  -units PixelsPerInch -density 300 -quality 100 -depth 3 +repage !OUTPREFIX!.png
convert  !OUTPREFIX!.png -crop 1859x550+0+129  +repage -quality 100 -depth 3  !OUTPREFIX!_u.png
convert  !OUTPREFIX!.png -crop 1869x625+0+54  +repage -quality 100 -depth 3  !OUTPREFIX!_l.png


rem convert -density 600x600 !OUTPREFIX!.ps -trim  -units PixelsPerInch -density 300 -quality 90 -depth %BITDEPTH% +repage -density 300 !OUTPREFIX!.png
rem convert  !OUTPREFIX!.png -crop 3715x1127+0+230 -depth %BITDEPTH%  +repage !OUTPREFIX!_u.png
rem convert  !OUTPREFIX!.png -crop 3715x1257+0+100 -colors 8 -depth !BITDEPTH! -define png:bit-depth=8 -density 200 +repage !OUTPREFIX!_l.png


)
del /f tmp_step*.ps
rem convert -loop 0 %LIST% -trim -depth 3 animation.gif

if NOT "%ANIMATION%" == "" (
echo %ANIMATION%.gif
convert -loop 0 %LIST% -depth 3 -delay 30 %ANIMATION%.gif
rem create the animation
)

goto stop

:noparams
echo The script converts a set of postscript files (prefixed by the parameter FILEPREFIX) into a set of PNG files. 
echo It can also generate a GIF animation.
echo Syntax:
echo convSteps.cmd FILEPREFIX step_start step_stop [animation_file_prefix]
goto stop

:stop
