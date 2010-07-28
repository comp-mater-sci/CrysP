@echo off

rem enable extensions
SETLOCAL ENABLEDELAYEDEXPANSION
SETLOCAL ENABLEEXTENSIONS

if "%1" == "" goto noparams
rem Check prequisities

for %%I in (%*) do (
	call mfactor1 2 1 %%~nI.C
	set LSTFILE=mfactor_%%~nI.L01 
	set DATFILE=mfqrs_%%~nI.dat
	echo %%~nxI : !LSTFILE! !DATFILE!
	mv -f mfactor.l01 !LSTFILE!
	echo #alpha q r M > !DATFILE!
	grep "^ ALFA" !LSTFILE! | gawk  "{print $2, $4, $6, $8}" >> !DATFILE!
)

goto stop

echo off

:noparams
echo The script requires a list of C-file names (at least one).
echo Filenames can be given with or without extension.
echo The output is written to the files suffixed with: mfactor_ and mfqrs_
echo You must also have the YLP directory content available.
goto stop

:stop
