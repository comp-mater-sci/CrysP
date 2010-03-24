@echo off


setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION

call support_path.cmd

if "%1" == "" goto noparams
if "%2" == "" goto noparams
if "%3" == "" goto noparams

set RETCODE=0

if not exist %2 goto noinput

set MAXSTEP=%1
set CURFILE=%2
set CLASS=%3

@rem Try to satisfy file dependencies:
set DEPLIST=create6.b01 create6.b02
for %%f in ( %DEPLIST%  ) do (
	if not exist %%f copy %TEXSUPPORT_DATA%\%%f %%f
	if not exist %%f goto depfailed
)


if "%CLASS%" == "B" echo    1    2    4    1 IMAG, IDN, IDM, IPR(O: no listing 1: listing)   > fcurodf.i01
if "%CLASS%" == "F" echo    2    1    4    1 IMAG, IDN, IDM, IPR(O: no listing 1: listing)   > fcurodf.i01


rem Shift 3 positions
for /L %%i in (1,1,3) do (
	shift /1
)
rem Build  the comment 
set COMMENT=%1
for %%i in (%2 %3 %4 %5 %6 %7 %8 %9) do (
	set COMMENT=!COMMENT! %%i
	shift /1
)
rem parameters 9 - 18	
for %%i in (%1 %2 %3 %4 %5 %6 %7 %8 %9) do (
	set COMMENT=!COMMENT! %%i
	shift /1
)

if exist resp.txt  del /Q resp.txt


echo    %MAXSTEP%                 Number of extraction runs   >> fcurodf.i01


for /L %%I in (1,1,%MAXSTEP%) do (
	echo    %%I                           Number of input bloc 1st run   >> fcurodf.i01
        echo LMAX    22  7.00  >> fcurodf.i01
        echo !COMMENT! step %%I   >> fcurodf.i01
        echo step%%I.C >> resp.txt
)

FCURODF.EXE fcurodf.i01 fcurodf.l01 %CURFILE% create6.b01 create6.b02 < resp.txt

goto stop

:nofiles
echo Please check the presence of one or more of the following files:
echo %DEPLIST%
set RETCODE=1
goto stop

:depfailed
echo Cannot satisfy file dependencies:
echo %DEPLIST%
set RETCODE=1
goto stop


:noinput
echo Input file %2 not found.
set RETCODE=1
goto stop

:noparams
echo Usage: %0 number_of_runs  curfile symmetry{B or F} [comment ...]
echo.
echo Requirements: create6.b01 create6.b02
set RETCODE=1
goto stop

:stop
exit /B %RETCODE%