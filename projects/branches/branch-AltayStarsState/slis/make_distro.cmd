:: $Id: make_distro.cmd 3419 2020-10-07 15:08:15Z Dries.DeSamblanx $

setlocal ENABLEDELAYEDEXPANSION
setlocal ENABLEEXTENSIONS

set SLIS_DIR=c:\temp\VEF

:: The first command line argument sets the VEF_ROOT variable
if not "%1" == "" (
	set SLIS_DIR=%1
)
@echo.
@echo Making distribution in %SLIS_DIR%
@echo.

:: mkdir behaves as 'mkdir -p' with extensions enabled
mkdir %SLIS_DIR%\lib\x64 %SLIS_DIR%\lib\Win32
copy x64\Release-DLL\libslis.dll %SLIS_DIR%\lib\x64
copy Win32\Release-DLL\libslis.dll %SLIS_DIR%\lib\Win32

::
:: Python
::
call activate conda_VEF_env
python -OO -m compileall -b python
call conda deactivate

set PYSLIS=python\pyslis

mkdir %SLIS_DIR%\%PYSLIS%

copy %PYSLIS%\*.pyc %SLIS_DIR%\%PYSLIS%

xcopy /I /E /Y config %SLIS_DIR%\config

:: requirements replaced with conda environment
:: cat python\requirements.txt >> %SLIS_DIR%\python\requirements.txt

:: Utilities
copy python\scripts\*.pyc %SLIS_DIR%\scripts

::
:: Bootstrap for utilities
::
mkdir %SLIS_DIR%\scripts
copy platform_specific\win32\scripts\* %SLIS_DIR%\scripts

