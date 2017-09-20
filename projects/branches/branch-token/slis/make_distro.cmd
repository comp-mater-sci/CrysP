:: $Id$

setlocal ENABLEDELAYEDEXPANSION
setlocal ENABLEEXTENSIONS

set VEF_DIR=c:\temp\VEF

:: The first command line argument sets the VEF_ROOT variable
if not "%1" == "" (
	set VEF_DIR=%1
)
@echo.
@echo Making distribution in %VEF_DIR%
@echo.

:: mkdir behaves as 'mkdir -p' with extensions enabled
mkdir %VEF_DIR%\lib\x64 %VEF_DIR%\lib\Win32
copy x64\Release-DLL\libslis.dll %VEF_DIR%\lib\x64
copy Win32\Release-DLL\libslis.dll %VEF_DIR%\lib\Win32

::
:: Python
::
python -OO -m compileall python

set PYSLIS=python\pyslis

mkdir %VEF_DIR%\%PYSLIS%

copy %PYSLIS%\*.pyo %VEF_DIR%\%PYSLIS%

xcopy /I /E /Y config %VEF_DIR%\config

cat python\requirements.txt >> %VEF_DIR%\python\requirements.txt

:: Utilities
copy python\scripts\*.pyo %VEF_DIR%\scripts

::
:: Bootstrap for utilities
::
mkdir %VEF_DIR%\scripts
copy platform_specific\win32\scripts\* %VEF_DIR%\scripts

