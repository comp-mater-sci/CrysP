@echo off
rem $Id$

setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION

if "%1" == "" goto noparams
if "%2" == "" goto noparams
if "%3" == "" goto noparams


set PREFIX=%1
set LOGFILE=%PREFIX%_steps2ps.log
set OUTFILE=%PREFIX%.tim
if not exist %LOGFILE% goto nofile

set TMPFILE=%PREFIX%_steps2ps.tmp
set TMPFILE1=%PREFIX%_steps2ps.tmp1
set TMPFILE2=%PREFIX%_steps2ps.tmp2

if exist %TMPFILE% del %TMPFILE%
if exist %TMPFILE1% del %TMPFILE1%

for /L %%i in (%2,1,%3)  do (
	echo %%i >> %TMPFILE%
	printc %PREFIX%%%i.c | gawk "/TEXTURE/ {print $3}" >> %TMPFILE1%
)
gawk "/MAXIMAL VALUE OF ODF WAS/ {print $6}" %LOGFILE% > %TMPFILE2%
echo #id TI ODFMAX | tee %OUTFILE%
paste %TMPFILE% %TMPFILE1% %TMPFILE2% | tee -a %OUTFILE%

del /Q %TMPFILE%  %TMPFILE1%  %TMPFILE2% 
goto stop

:noparams
echo Usage: %0 prefix step_start step_end
goto stop

:nofile
echo file %LOGFILE% not found.
goto stop

:stop
