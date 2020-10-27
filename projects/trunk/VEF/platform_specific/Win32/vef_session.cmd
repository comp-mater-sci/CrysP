@echo off
:: $Id$
:: 
:: The only line that needs to be adjusted: VEF_ROOT
set VEF_ROOT=%CD%

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
set SLIS_ROOT=%VEF_ROOT%

echo checking the Conda environment
call conda env list | findstr Conda_VEF_env
if ERRORLEVEL 1 (
	echo conda environment not yet present: creating environment
	call conda env create -f %VEF_ROOT%\python\Conda_VEF_env.yml
) else (
	call conda env update --name Conda_VEF_env --file %VEF_ROOT%\python\Conda_VEF_env.yml --prune > nul
)
call conda activate Conda_VEF_env

cmd \k

goto :eof

:errmsg
echo Error conditions have occured during initialization of the VEF.
