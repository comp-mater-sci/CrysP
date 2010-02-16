@echo off

if "%1" == "" goto noparams

rem enable extensions
SETLOCAL ENABLEDELAYEDEXPANSION
SETLOCAL ENABLEEXTENSIONS


@rem convert -trim -density 300x300 step10.ps  -depth 3 -quality 90 -resize 1857x679 -resample 150x150  step10.png
set LIST=

set SEDFILE=tmprmpage.sed
rem Construct SED expression file
rem NOTE: there are two "^" marks - the first one is interpreted by echo command.  
echo N > %SEDFILE%
echo /^^\/Helvetica findfont    185 scalefont setfont.* 1846 M  270.0 R (PAGE) show -270.0 R/d >> %SEDFILE%
echo /^^\/Helvetica findfont    185 scalefont setfont.* 1023 M  270.0 R ( 1) show -270.0 R/d >> %SEDFILE%



for /L %%i in (1,1,%1) do (
echo %%i
set LIST=!LIST! step%%i.png
sed -f %SEDFILE% step%%i.ps > cstep%%i.ps
convert -trim -density 300x300 cstep%%i.ps  -depth 3 -quality 90 -density 300 -units PixelsPerInch step%%i.png

)
rem convert -loop 0 %LIST% -trim -depth 3 animation.gif

rem create the animation
convert -loop 0 %LIST% -depth 3 animation.gif

goto stop

:noparams

echo convSteps.cmd numberofSteps
goto stop

:stop
