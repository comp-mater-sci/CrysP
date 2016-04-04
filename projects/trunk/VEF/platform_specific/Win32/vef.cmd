@echo off
:: $Id$
:: 
:: The only line that needs to be adjusted: VEF_ROOT
set VEF_ROOT=c:\temp\VEF

:: There is no need to touch any of the lines below unless you
:: know what you are doing.
title Virtual Experimentation Framework
:: load 
call %VEF_ROOT%\mtmfhmport.cmd

set PATH=%VEF_ROOT%\bin;%PATH%
set PYTHONPATH=%VEF_ROOT%\python;%PYTHONPATH%

