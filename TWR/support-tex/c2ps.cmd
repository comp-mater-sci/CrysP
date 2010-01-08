rem TO CALCULATE AN ODF FROM C-COEFFICIENTS, THEN PLOT IT.
rem FIRST ARGUMENT = C-COEFFICIENTS FILE

@echo off

if "%1" == "" goto noparams
rem Check prequisities
rem if not exist pltodf_c.i01 goto nofiles
if not exist pltodf_o.i01 goto nofiles
if not exist color goto nofiles
if not exist plotter goto nofiles
if not exist create6.b01 goto nofiles
if not exist create6.b02   goto nofiles

rem build pltodf_c.i01
echo     0                    IEVOD: if =0: Ordinary case  >  pltodf_c.i01


calcodf pltodf_c.i01 create6.b01 create6.b02 %1.c aodf.001
odfplt pltodf_o.i01 pltodf_o.l01 aodf.001 %1.001

rem plotscr vga color %1.001

PLOTPSC.EXE plotter color %1.001

move p01.ps %1.ps

goto stop

echo off

:noparams
echo The script requires one parameter: name of .C file (without extension)
echo Use file pltodf_o.i01 to customize the output.
exit /B 1
goto stop

:nofiles
echo One of the files doesn't exist:
echo color plotter create6.b01 create6.b02 pltodf_o.i01
goto stop

:stop
