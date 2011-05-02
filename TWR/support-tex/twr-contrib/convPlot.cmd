@echo off
rem $Id$

rem enable extensions
SETLOCAL ENABLEDELAYEDEXPANSION
SETLOCAL ENABLEEXTENSIONS


if "%1" == "" goto noparams
if "%2" == "" (
	set OUTPREFIX=%1
) else (
	set OUTPREFIX=%2
)
if "%3" == "" (
	set BITDEPTH=1  
) else (
	set BITDEPTH=%3  
)	

if "%4" == "" (
	set QUALITY=default
) else (
if "%4" == "default" (
	set QUALITY=default
) else (
if "%4" == "high" (
	set QUALITY=high
) else (
rem Bad value of the parameter, show help text.
goto :noparams
)
)
)

set PLOTFILE=%~n1

if not exist "%PLOTFILE%.ps" goto noinputfile

rem For future development: it might be useful to play with extras:
rem set EXTRAS=-colors 8 -define png:bit-depth=8

goto !QUALITY!

:high
rem High quality: rasterization with 600 dpi
convert -density 600x600 %PLOTFILE%.ps -trim  -units PixelsPerInch -density 300 -quality 90 -depth %BITDEPTH% +repage !OUTPREFIX!.png
convert  !OUTPREFIX!.png -crop 3738x1127+0+230 -depth %BITDEPTH%  +repage !OUTPREFIX!_u.png
convert  !OUTPREFIX!.png -crop 3738x1257+0+100 -depth %BITDEPTH%  +repage !OUTPREFIX!_l.png
convert  !OUTPREFIX!_u.png -trim -depth %BITDEPTH%  +repage !OUTPREFIX!_trim.png
goto stop

:default
rem Standard quality: rasterization with 300 dpi
convert -density 300x300 %PLOTFILE%.ps -trim  -units PixelsPerInch -density 300 -quality 100 -depth %BITDEPTH% +repage !OUTPREFIX!.png
convert  !OUTPREFIX!.png -crop 1859x550+0+129  +repage -quality 100 -depth 3  !OUTPREFIX!_u.png
convert  !OUTPREFIX!.png -crop 1869x625+0+54  +repage -quality 100 -depth 3  !OUTPREFIX!_l.png
convert  !OUTPREFIX!_u.png -trim -depth %BITDEPTH%  +repage !OUTPREFIX!_trim.png
goto stop

:noinputfile
echo Error: input file %PLOTFILE% not found. 
goto stop

:noparams
echo Convert PS to  a high resolution png format. Bitdepth 1 is imposed by default. 
echo Syntax:
echo convPlotBw.cmd postscript_fname [output_prefix]  [bitdepth] [quality]
echo Parameters: 
echo postscript_fname  - filename (with or without extension) of ODF postscript file  
echo output_prefix - prefix of the output file
echo bitdepth - bitdepth used in the output PNG files (default is 1 - suitable for BW).
echo quality - controls image resolution that is used in rendering PS to raster PNG (default or high)
set RETCODE=0
goto stop


:stop
exit /B %RETCODE%
