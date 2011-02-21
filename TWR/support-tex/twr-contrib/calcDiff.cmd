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

rem Prepare control file for versch
set /A NDIFFS=!NARGS! - 1
set TMPCNF=calcDiff.tmp.1
set TMPFNAMES=calcDiff.tmp.2
set OUTFNAME=calcDiff.txt
set isfirst=T
for %%i in (%*) do (
		if "!isfirst!" == "T" (
			echo %%i > !TMPFNAMES!
			echo %%i > !TMPCNF!
			echo !NDIFFS! >>  !TMPCNF!
			set isfirst=F
		) else (
			echo %%i >> !TMPFNAMES!
			echo %%i >> !TMPCNF!
		)
		
)
rem Call versch
call versch  < !TMPCNF! > nul
rem Merge the data
call paste !TMPFNAMES! VERSCH.L01 > !OUTFNAME!
call cat !OUTFNAME!

rem Cleanup
del /f /q !TMPFNAMES!  !TMPFNAMES!

goto :eof

:error
echo !MSG! 
exit /B 1
goto :eof


:noparams
echo The script requires a list of C-file names (at least two) with extensions.
echo calcDiff 
exit /B 1
goto :eof

