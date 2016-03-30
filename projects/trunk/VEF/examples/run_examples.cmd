@echo off
rem $Id$

SETLOCAL ENABLEDELAYEDEXPANSION
SETLOCAL ENABLEEXTENSIONS
set VEF_ROOT=..
set ALAMDMC=..\alamDMC\Release\alamDMC.exe

for %%m in (UDSA ASR QRS YLD EWC) do (
	for %%x in (*_%%m.cfg) do (
		echo %%x
		start /NODE 0 /AFFINITY 0x1 /B /WAIT !ALAMDMC! %%m %%x
	)
)