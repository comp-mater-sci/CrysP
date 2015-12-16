@echo off
rem $Id$

SETLOCAL ENABLEDELAYEDEXPANSION
SETLOCAL ENABLEEXTENSIONS

set ALAMDMC=..\Release\alamDMC.exe


set TESTCASE=%%x
for %%m in (UDSA ASR QRS Yld EWC) do (
	for %%x in (alam%%m_*.cfg) do (
		echo %%x
		start /NODE 0 /AFFINITY 0x1 /B /WAIT !ALAMDMC! %%m %%x
	)
)