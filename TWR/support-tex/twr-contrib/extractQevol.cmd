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

set PATTERN=%1
set PREFIX=%2
set SUFFIX=%3
set DEFMAPFILE=%PREFIX%_defmap.txt
set QFILE=%PREFIX%.q3d

rem Check pre-requisities
for %%r in (%QFILE% %DEFMAPFILE%) do (
	rem echo %%r
	if not exist  %%r (
			set MISSING=%%r
			goto nofile
	)
)

set TMPFILE=%PREFIX%_%SUFFIX%.qstep
set OUTFILE=%PREFIX%_%SUFFIX%.qevol

grep  %PATTERN% %QFILE% > %TMPFILE%
paste %DEFMAPFILE% %TMPFILE% | gawk "{print $1,$2,$3,$4,$6,$7,$8}" > %OUTFILE%

goto :eof

:noparams

echo This script can extract strain evolution of q-values from .q3d files.
echo. 
echo extractQevol angle_pattern prefix suffix
echo.
echo angle_pattern must be enclosed in double apostrophies
echo.

goto :eof

:nofile
echo Cannot find necessary input file: %MISSING%
goto :eof