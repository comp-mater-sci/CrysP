@echo off
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
set PLOTFILE=%1

if not exist "%PLOTFILE%.ps" goto noinputfile



convert -density 600x600 %PLOTFILE%.ps -trim  -units PixelsPerInch -density 300 -quality 90 -depth %BITDEPTH% +repage %OUTPREFIX%.png
convert  %OUTPREFIX%.png -crop 3715x1127+0+230 -depth %BITDEPTH%  +repage %OUTPREFIX%_u.png
convert  %OUTPREFIX%.png -crop 3715x1257+0+100 -depth %BITDEPTH%  +repage %OUTPREFIX%_l.png
convert  %OUTPREFIX%_u.png -trim -depth %BITDEPTH%  +repage %OUTPREFIX%_trim.png

goto stop

:noinputfile
echo Error: input file %PLOTFILE% not found. 
goto stop

:noparams
echo Convert PS to  a high resolution png format. Bitdepth 1 is imposed by default. 
echo Syntax:
echo convPlotBw.cmd postscript_fname [output_prefix]  [bitdepth]
echo Parameters: 
echo postscript_fname  - filename (without extension) of ODF postscript file  
echo output_prefix - prefix of the output file
echo bitdepth - bitdepth used in the output PNG files (default is 1 - suitable for BW).
set RETCODE=0
goto stop


:stop
exit /B %RETCODE%
