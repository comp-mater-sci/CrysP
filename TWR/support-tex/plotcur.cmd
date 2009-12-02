@echo off

if "%1" == "" goto noparams
if "%2" == "" goto noparams
if "%3" == "" goto noparams

if "%4" == "B" echo    1    2    4    1 IMAG, IDN, IDM, IPR(O: no listing 1: listing)   > fcurodf.i01
if "%4" == "F" echo    2    1    4    1 IMAG, IDN, IDM, IPR(O: no listing 1: listing)   > fcurodf.i01



del /Q resp.txt


echo    %1                 Number of extraction runs   >> fcurodf.i01


for /L %%I in (1,1,%1) do (
	echo    %%I                           Number of input bloc 1st run   >> fcurodf.i01
        echo LMAX    22  7.00  >> fcurodf.i01
        echo %2 step %%I   >> fcurodf.i01
        echo step%%I.C >> resp.txt
)

FCURODF.EXE fcurodf.i01 fcurodf.l01 %3 create6.b01 create6.b02 < resp.txt

for /L %%I in (1,1,%1) do (
     call c2ps.cmd step%%I
     ps2pdf  step%%I.ps
)


goto stop

:noparams
echo Usage: %0 number_of_runs comment  curfile symmetry{B or F} 
goto stop

:stop