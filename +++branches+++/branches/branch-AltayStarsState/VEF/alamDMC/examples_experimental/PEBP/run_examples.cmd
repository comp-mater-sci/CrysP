@echo off
rem $Id$

SETLOCAL ENABLEDELAYEDEXPANSION
SETLOCAL ENABLEEXTENSIONS

set ALAMDMC=..\..\Release\alamDMC.exe
set TESTCASE=DC06F250
rem Run the regular test cases 
for %%m in (UDSA ASR QRS Yld) do (
	set CONFIG=alam%%m_!TESTCASE!.cfg
	echo !CONFIG!

	!ALAMDMC! %%m !CONFIG!
)
rem Run the test cases that depend on PEBP state file
for %%m in (TSA ASR QRS Yld) do (
	set CONFIG=alam%%m_state_!TESTCASE!.cfg
	echo !CONFIG!

	!ALAMDMC! %%m !CONFIG!
)