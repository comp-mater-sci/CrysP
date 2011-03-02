@echo off
rem $Id$

rem enable extensions
setlocal ENABLEDELAYEDEXPANSION ENABLEEXTENSIONS

rem Import settings
call support_path.cmd
rem check number of parameters
call argc.cmd %*
set NARGS=!ERRORLEVEL!
if !NARGS! LSS 3 goto noparams

set DIRPREFIX=%1
set EXPPREFIX=%2

set TMPLIST=
for %%t in (%3 %4 %5 %6 %7 %8 %9) do (
	set TMPLIST=!TMPLIST! %%t
)


set PWD=%CD%

for /D %%i in ( !DIRPREFIX!* ) do (
	echo.
	echo.
	echo Processing %%~fi
	echo.
	for /D %%d in ( !TMPLIST! ) do (
		echo cp -rf %%d %%i
		cp -rf %%d %%i
	)
	
	set WD=%%~fi
	echo Entering !WD!
	cd !WD!
	
	rem Determine file prefix
	if exist rstep1.c (
		set PREFIX=rstep
	) else (
		set PREFIX=step
	)
	
	echo Prefix: !PREFIX!


	
	
	rem Check number of steps
	for /F "tokens=*" %%A in ('gawk "END {print NR}" elem_map.txt') do set NSTEPS=%%A
	
	echo Number of steps: !NSTEPS!
	
	for /D %%d in ( !TMPLIST! ) do (
		echo copy /Y !PREFIX!*.c %%~fd
		copy /Y !PREFIX!*.c %%~fd
		rem Check if extra files should be processed.
		if not "!EXPPREFIX!" == "-" (
			if exist !WD!\*!EXPPREFIX!*.c (
				echo Extra files files:  !WD!\*!EXPPREFIX!*.c
				echo copy /Y !WD!\*!EXPPREFIX!*.c %%~fd
				copy /Y !WD!\*!EXPPREFIX!*.c %%~fd
			) else (
				echo No extra files in !WD!
			)
		)
		
		echo Entering %%~fd
		cd %%~fd
		
		ls !PREFIX!*.c
		
		for %%K in (*.c) do (
			echo Converting %%K  %%~nK
			call c2ps %%~nK
			call ps2pdf %%~nK.ps
		 )
		 
		rem Perform conversions to PNG
		echo Starting conversion to PNG and GIF
		rem convSteps.cmd FILEPREFIX step_start step_stop [animation_file_prefix]
		call convSteps !PREFIX! 1 !NSTEPS! %%~ni
		if not "!EXPPREFIX!" == "-" (
			for %%K in (*!EXPPREFIX!*.c) do (
				echo Converting %%K  %%~nK
				call convPlot %%~nK
			)
		)
		
		echo Going back to !WD!
		cd !WD!
	)
	echo Current dir: !CD!
	echo !CD!
	echo %PWD%
	echo Going back to %PWD%
	cd %PWD%
) 
goto :eof

:noparams

echo. 
echo makePlots directory_prefix  ^<extra_C_prefix ^| - ^> template_dir1 [template_dir2 ...] 
echo.
