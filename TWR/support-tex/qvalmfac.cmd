rem TO CALCULATE AN ODF FROM C-COEFFICIENTS, THEN PLOT IT.
rem FIRST ARGUMENT = C-COEFFICIENTS FILE

@echo off

if "%1" == "" goto noparams
rem Check prequisities


call mfactor1 2 1 %1.C

mv mfactor.l01 mfactor_%1.L01

goto stop

echo off

:noparams
echo The script requires one parameter: name of .C file (without extension)
echo You must also have the YLP directory content available.
goto stop

:nofiles
echo One of the files doesn't exist:
echo color plotter create6.b01 create6.b02 pltodf_o.i01
goto stop

:stop
