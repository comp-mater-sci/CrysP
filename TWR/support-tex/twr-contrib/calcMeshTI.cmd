@echo off
rem $Id$

rem enable extensions
SETLOCAL ENABLEDELAYEDEXPANSION
SETLOCAL ENABLEEXTENSIONS

call support_path.cmd
rem check number of parameters
call argc.cmd %*
set NARGS=!ERRORLEVEL!
if !NARGS! LSS 2 goto noparams

rem Verify user input, check existence of the files
REM for %%i in (%*) do (
	REM if not exist %%i (
		REM set MSG=Error: file %%i does not exist.
		REM goto :error
	REM )
REM )


set SRCDIR=%1
set OUTFILE=%2

set TMPFILE1=calcMeshTI.tmp.1
set TMPFILE2=calcMeshTI.tmp.2
set TMPFILE3=calcMeshTI.tmp.3


set TMPDIR=calcMeshTI.dir

if exist %TMPDIR% (
	del /Q /F %TMPDIR%\*
) else (
	mkdir %TMPDIR%
)


set CWD=%CD%



set NSTEP=1
rem Obrain fully qualified path
for %%d in (%SRCDIR%) do (
	set FSRCDIR=%%~fd
	rem echo %%~fd
)

cd %TMPDIR%
	
for %%f in (%FSRCDIR%\*.cur) do (
	echo %%f %%~nf
	
	cp %%f .
	call cur2c 1 %%~nf.cur F %%~nf > nul
	printc step1.c > nul
	gawk "/TEXTURE INDEX/ {print $7}" PRINTC.L01 >> %TMPFILE1% 
	echo  %%~nf >> %TMPFILE2%
	del /Q %%~nf.cur
	
	set /A NSTEP=!NSTEP!+1
	
)
sed -e "s/_/ /" %TMPFILE2% > %TMPFILE3%
paste %TMPFILE3% %TMPFILE1%  > %OUTFILE%

cp  %OUTFILE% %CWD%
cd %CWD%

goto :eof

:noparams
echo This script calculates texture indices for a set of CUR files. 
echo It is assumed that the CUR files follow the naming convention: token_point.cur
echo Hint: use preTI script to generate such files from locations.
echo.
echo %0 input_directory output_file
goto :eof

:error
goto :eof