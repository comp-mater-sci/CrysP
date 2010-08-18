@echo off
rem enable extensions
SETLOCAL ENABLEDELAYEDEXPANSION
SETLOCAL ENABLEEXTENSIONS

if "%1" == "" goto noparams
if "%2" == "" goto noparams
if "%3" == "" goto noparams
rem if "%4" == "" goto noparams

set INPFNAME=%1
set FRAMEMAX=%2
set ANIMPREFIX=%3

if "%4" == "" (
set DELAY=3
) else (
set DELAY=%4
)
rem Set bounding box and crop region
if not "%5" == "" (
set BB=%5
set CROP=-crop !BB!
)

set PREFIX=cframe
echo Creating bitmap frames (it may take quite a while)...
convert  -density 300x300 -units PixelsPerInch +map %INPFNAME%[0-%FRAMEMAX%] !CROP! -density 300 -quality 90 +repage %PREFIX%-%%%d.png

for /L %%i in (0,1,%FRAMEMAX%) do (
set OUTPREFIX=%PREFIX%%%i
rem echo !OUTPREFIX!
set LIST=!LIST! %PREFIX%-%%i.png
)
echo Frame list:
echo !LIST!

echo Creating animation...
convert !LIST! -delay %DELAY% %ANIMPREFIX%.gif


goto :EOF


:noparams
echo This script will convert PDF file containing set of frames into GIF animation
echo.
echo Syntax:
echo convFrames input_pdf max_frame output_prefix [delay(=3)] [bounding_box]
echo.
echo Note that frame count starts with 0, thus setting max_frame=0 will select the first frame.
echo Examples of bounding box: 
echo 1400x780+60+130 
echo 1100x750+300+150
goto :EOF 