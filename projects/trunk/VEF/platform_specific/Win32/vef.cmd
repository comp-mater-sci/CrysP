@echo off
:: $Id$
:: 
:: The only line that needs to be adjusted: VEF_ROOT
set VEF_ROOT=c:\temp\VEF

:: The first command line argument sets the VEF_ROOT variable
if not "%1" == "" (
	set VEF_ROOT=%1
)
:: There is no need to touch any of the lines below unless you
:: know what you are doing.
title Virtual Experimentation Framework
echo Setting-up the VEF...
:: load 
call %VEF_ROOT%\mtmfhmport.cmd %VEF_ROOT%
if ERRORLEVEL 1 (
	echo This system does not meet the minimal requirements of the VEF.
	echo Some components of the VEF may not work properly.
)
set PATH=%VEF_ROOT%\bin;%PATH%
set PYTHONPATH=%VEF_ROOT%\python;%PYTHONPATH%

echo The VEF is now initialized.

goto :eof

:errmsg
echo Error conditions have occured during initialization of the VEF.
