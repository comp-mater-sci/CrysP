@echo off


setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION


if "%1" == "" goto noparams
if "%2" == "" goto noparams
if "%3" == "" goto noparams

if not exist create6.b01 goto nofiles
if not exist create6.b02   goto nofiles

if not exist %2 goto noinput

if "%3" == "B" echo    1    2    4    1 IMAG, IDN, IDM, IPR(O: no listing 1: listing)   > fcurodf.i01
if "%3" == "F" echo    2    1    4    1 IMAG, IDN, IDM, IPR(O: no listing 1: listing)   > fcurodf.i01

set COMMENT=%4 %5 %6 %7 %8 %9



if exist resp.txt  del /Q resp.txt


echo    %1                 Number of extraction runs   >> fcurodf.i01


for /L %%I in (1,1,%1) do (
	echo    %%I                           Number of input bloc 1st run   >> fcurodf.i01
        echo LMAX    22  7.00  >> fcurodf.i01
        echo %COMMENT% step %%I   >> fcurodf.i01
        echo step%%I.C >> resp.txt
)

FCURODF.EXE fcurodf.i01 fcurodf.l01 %2 create6.b01 create6.b02 < resp.txt

goto stop

:nofiles
echo Please check the presence of one or more of the following files:
echo create6.b01 create6.b02
goto stop

:noinput
echo Input file %2 not found.
goto stop

:noparams
echo Usage: %0 number_of_runs  curfile symmetry{B or F} [comment ...]
echo.
echo Requirements: create6.b01 create6.b02
goto stop

:stop