@echo off
rem $Id$

setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION
rem Import settings
call support_path.cmd

if "%1" == "" goto noparams
if "%2" == "" goto noparams
if "%3" == "" goto noparams
if "%4" == "" goto noparams
if "%5" == "" goto noparams

set INPFILE=%1
set DIRPREFIX=%2
set PREFIX=%3
set CLASS=%4
set PLTFILE=%5

rem Shift 5 positions
for /L %%i in (1,1,5) do (
	shift /1
)
rem Build  the comment 
set COMMENT=%1
shift /1
for %%i in (%1 %2 %3 %4 %5 %6 %7 %8 %9) do (
	set COMMENT=!COMMENT! %%i
	shift /1
)
rem parameters 9 - 18	
for %%i in (%1 %2 %3 %4 %5 %6 %7 %8 %9) do (
	set COMMENT=!COMMENT! %%i
	shift /1
)

echo !COMMENT!

if NOT EXIST %PLTFILE% (
	set ERRMSG=Cannot find plot file %PLTFILE%
	goto :error
)

set TMPFILE=lpostAll.tmp

for /F "tokens=4,5 delims=," %%i in (%INPFILE%) do (
	echo %%i > !TMPFILE!
	for /F "usebackq tokens=1 delims= " %%I in (`sed -e "s/ //g" !TMPFILE!`) do (
		set SHORTCOMMENT=%%I
		set DIR=%DIRPREFIX%%%I
	)
	rm -f !TMPFILE!
	echo.
	echo %%j
	echo.
	rem Absent rotation field: 
	if "%%j" == "" (
		set ROTATION=0.0
	) else (
		set ROTATION=%%j
	)
	set PRECOMMENT=!SHORTCOMMENT!
	set PWD=!CD!
	
	echo Directory: !DIR!
	echo Rotation: !ROTATION!

	copy /Y %PLTFILE%  !DIR!
	cd !DIR!
	call postAll.cmd elem %CLASS% !ROTATION!  !PRECOMMENT! !COMMENT! 
	cd !PWD!
)

goto :eof

:error
echo %ERRMSG%
goto :eof

:noparams
echo.
echo %0 input_file directory_prefix prefix symmetry_class ODF_plot_file
echo.
echo Example:
echo %0 loclist_A.txt  out_  elem  F  PLTODF_O.I01 [comment ...]
goto :eof

:noparams

