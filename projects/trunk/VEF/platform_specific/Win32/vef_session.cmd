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

:: checking conda path
:: If the conda command is available there is no problem, otherwise
:: the standard installation folders %userprofile%\[ana|mini]conda3\condabin
:: are checked and added to the path if present.
where conda 2> nul
if "%ERRORLEVEL%" EQU "0" goto :condaavail
echo no conda
:: test for miniconda
echo ;%path%; | find /c /I ";%userprofile%\miniconda3\condabin;"
if NOT EXIST %userprofile%\miniconda3\condabin goto :anacondatest
if "%ERRORLEVEL%" EQU "0" goto :anacondatest
set PATH=%PATH%;%userprofile%\miniconda3\condabin;
where conda
if "%ERRORLEVEL%" EQU "0" goto :condaavail
:: test for anaconda
:anacondatest
if NOT EXIST %userprofile%\anaconda3\condabin goto :condaerror
echo ;%path%; | find /c /I ";%userprofile%\anaconda3\condabin;"
if "%ERRORLEVEL%" EQU "0" goto :condaerror
set PATH=%PATH%;%userprofile%\anaconda3\condabin;
where conda
if "%ERRORLEVEL%" EQU "1" goto :condaerror

:condaavail
echo checking the Conda environment
call conda env list | findstr Conda_VEF_env
if ERRORLEVEL 1 (
	echo conda environment not yet present: creating environment
	call conda env create -f %VEF_ROOT%\python\Conda_VEF_env.yml
) else (
	echo Checking the conda environment...
	call conda env update --name Conda_VEF_env --file %VEF_ROOT%\python\Conda_VEF_env.yml --prune > nul
)
call conda activate Conda_VEF_env
cmd \k

goto :eof

:errmsg
echo Error conditions have occured during initialization of the VEF.
goto :eof
:condaerror
echo conda error: the conda command is not available.
echo Download miniconda from http://conda.pydata.org/miniconda.html
echo or add the directory containing the conda.bat file to your path