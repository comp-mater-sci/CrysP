@echo off
setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION

if "%1" == "" goto noparams
if "%2" == "" goto noparams
if "%3" == "" goto noparams


set PREFIX=%1
set LOGFILE=%PREFIX%_steps2ps.log

for /L %%i in (%2,1,%3)  do (
	call c2ps %PREFIX%%%i | grep "MAXIMAL VALUE OF ODF WAS" | tee -a %LOGFILE%
)
del /F %PREFIX%*.001

goto stop

:noparams
echo Usage: %0 prefix step_start step_end
goto stop

goto stop

:stop
