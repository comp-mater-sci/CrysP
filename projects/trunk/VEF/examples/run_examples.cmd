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

for %%m in (UDSA ASR QRS YLD EWC) do (
	for %%x in (*_%%m.cfg) do (
		echo %%x
		start /NODE 0 /AFFINITY 0x1 /B /WAIT !ALAMDMC! %%m %%x
	)
)