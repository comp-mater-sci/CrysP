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

mkdir %VEF_DIR%\bin
copy alamDMC\Release\alamDMC.exe %VEF_DIR%\bin

::
:: Python
::
python -OO -m compileall python

mkdir %VEF_DIR%\python\pyvef
mkdir %VEF_DIR%\python\pyvef\configurators

copy python\pyvef\*.pyo %VEF_DIR%\python\pyvef
copy python\pyvef\configurators\*.pyo %VEF_DIR%\python\pyvef\configurators

mkdir %VEF_DIR%\scripts

copy python\scripts\*.pyo %VEF_DIR%\scripts

cat python\requirements.txt >> %VEF_DIR%\python\requirements.txt

::
:: Platform-specific
::
copy platform_specific\win32\scripts\* %VEF_DIR%\scripts
copy platform_specific\Win32\vef.cmd %VEF_DIR%

::
:: Manual
::

pushd manual && doxygen & popd

set DOC_DIR=%VEF_DIR%\user_manual
mkdir %DOC_DIR%
xcopy /i /s /y manual\user_manual\html %DOC_DIR%\html
copy manual\user_manual.html %VEF_DIR%

::
:: Examples
::
xcopy /i /s /y examples %VEF_DIR%\examples

::
:: Data
::
xcopy /i /s /y data %VEF_DIR%\data

::
:: MTM-FHMport
::
set MTMFHM_DIR=..\MTM-FHMport
if exist %MTMFHM_DIR%\make_distro.cmd (
	@echo.
	@echo Setting up MTM-FHMport
	pushd %MTMFHM_DIR% && call make_distro.cmd %VEF_DIR% & popd
)