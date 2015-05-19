@echo off
rem $Id$

SETLOCAL ENABLEDELAYEDEXPANSION
SETLOCAL ENABLEEXTENSIONS

set ALAMDMC=..\Release\alamDMC.exe
set TESTCASE=AKDQFF

for %%m in (UDSA ASR QRS Yld) do (
	set CONFIG=alam%%m_!TESTCASE!.cfg
	echo !CONFIG!

	!ALAMDMC! %%m !CONFIG!
)