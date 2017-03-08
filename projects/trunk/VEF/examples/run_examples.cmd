@echo off
rem $Id$

setlocal ENABLEDELAYEDEXPANSION
setlocal ENABLEEXTENSIONS

if not defined VEF_ROOT (
	echo.
	echo Error: cannot run the examples. 
	echo Please set VEF_ROOT environment variable to resolve this problem.
	exit /B 2
)

set ALAMDMC=!VEF_ROOT!\bin\alamDMC.exe

set LOGFILE=run_examples.log

for %%m in (UDSA ASR QRS YLD EWC ADP) do (
	for %%x in (*_%%m.cfg) do (
		echo %%x
		!ALAMDMC! %%m %%x
		if ERRORLEVEL 1 (
			echo FAILURE: %%m %%x >> %LOGFILE%
		) else (
			echo SUCCESS: %%m %%x >> %LOGFILE%
		)
		
	)
)