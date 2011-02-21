@echo off
rem $Id$

setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION

call support_path.cmd
rem check number of parameters
call argc.cmd %*
set NARGS=!ERRORLEVEL!
if !NARGS! LSS 2 goto noparams
rem Verify user input, check existence of the files
for %%i in (%*) do (
	if not exist %%i (
		set MSG=Error: file %%i does not exist.
		goto :error
	)
)
set CFILE=%1
set TICFILE=%1
shift

set /A NDIFFS=!NARGS! - 1
set TMPCNF=calcDiff.tmp.1
set TMPFNAMES=calcDiff.tmp.2
set TMPVERSCH=calcDiff.tmp.3
set TMPTI=calcDiff.tmp.4
set OUTFNAME=calcDiff.txt


rem Workaround: versch cannot accept more than 9 files in one "batch" - one-digit number of files to be processed is allowed.
echo !CFILE! > !TMPFNAMES!
set CNT=0
set isfirst=T
:outerloop
if not "%1" == "" (
	call argc.cmd %1 %2 %3 %4 %5 %6 %7 %8 %9
	set NFILES=!ERRORLEVEL!
	if !NFILES! GTR 0 (
		rem echo Start of batch !CNT!
		rem Prepare control file for versch
		echo !CFILE! > !TMPCNF!
		echo !NFILES! >> !TMPCNF!
		for %%i in (%1 %2 %3 %4 %5 %6 %7 %8 %9) do (
					echo %%i >> !TMPFNAMES!
					echo %%i >> !TMPCNF!
				)
		rem Call versch
		call versch  < !TMPCNF! > nul
		rem Call calcTI
		call calcTI.cmd !TICFILE! %1 %2 %3 %4 %5 %6 %7 %8 %9 | gawk "{print $NF}" > !TMPTI!
		rem Merge the data
		if "!isfirst!" == "T" (
			call paste !TMPFNAMES! !TMPTI! VERSCH.L01 > !OUTFNAME!
			set TICFILE=
			set isfirst=F
		) else (
			rem skip the first line in VERSCH output
			call tail -n +2 VERSCH.L01 > !TMPVERSCH!
			call paste !TMPFNAMES! !TMPTI! !TMPVERSCH! >> !OUTFNAME!
		)
	)
	rem Cleanup before the next iteration 
	del /f /q !TMPCNF! !TMPTI!
	if exist !TMPVERSCH! del /f /q !TMPVERSCH!
	rem The file TMPFNAMES _must_ be removed
	del /f /q !TMPFNAMES!
	set /A CNT=CNT + 1
	rem Shift by 9 positions
	for %%i in (%1 %2 %3 %4 %5 %6 %7 %8 %9) do shift
	goto :outerloop
)



rem Display the results	
call cat !OUTFNAME!


goto :eof

:error
echo !MSG! 
exit /B 1
goto :eof


:noparams
echo The script requires a list of C-file names (at least two) with extensions.
echo calcDiff  base-C-File C-files ...
echo. 
echo Output format:
echo filename  texture_index  ODF_difference  comment
exit /B 1
goto :eof

