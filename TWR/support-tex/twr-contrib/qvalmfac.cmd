rem @echo off
rem $Id$

rem enable extensions
SETLOCAL ENABLEDELAYEDEXPANSION
SETLOCAL ENABLEEXTENSIONS

rem Import settings
call support_path.cmd

if "%1" == "" goto noparams
rem Check prequisities
set SLIPSYSTEM=%1


if "%SLIPSYSTEM%" == "bcc" (
	set SLIPPAR=2
) else if "%SLIPSYSTEM%" == "fcc" (
		set SLIPPAR=1
	)
	
if "%SLIPPAR%" == "" (
	echo Cannot determine slip system.
)

rem Check the prequisities
rem Try to satisfy file dependencies:
set DEPLIST=MFACTOR.I0%SLIPPAR% YL3.I0%SLIPPAR% YL2MFAC.B0%SLIPPAR%
for %%f in ( %DEPLIST%  ) do (
	if not exist %%f copy %TEXSUPPORT_DATA%\%%f %%f
	if not exist %%f (
		set MISSINGFILE=%%f
		goto depfailed
	)
)


for %%I in (%2 %3 %4 %5 %6 %7 %8 %9) do (
	call mfactor1 %SLIPPAR% 1 %%~nI.C
	set LSTFILE=mfactor_%%~nI.L0%SLIPPAR% 
	set DATFILE=mfqrs_%%~nI.dat
	echo %%~nxI : !LSTFILE! !DATFILE!
	move /Y mfactor.l0%SLIPPAR% !LSTFILE!
	echo #alpha q r M > !DATFILE!
	gawk  "/^ ALFA/ {print $2, $4, $6, $8}" !LSTFILE! >> !DATFILE!
)

goto stop

echo off

:noparams
echo Syntax:
echo qvalmfac slipsystems  C-file1 [C-file2 ...]
echo.
echo Definition of slip systems: bcc or fcc
echo The script requires a list of C-file names (at least one, up to eight).
echo Filenames can be given with or without extension.
echo The output is written to the files suffixed with: mfactor_ and mfqrs_
echo You must also have the YLP directory content available.
goto stop


:depfailed
echo Missing dependency file: %MISSINGFILE%
goto stop

:stop
