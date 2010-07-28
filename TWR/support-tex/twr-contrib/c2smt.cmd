@echo off
rem $Id$

if "%1" == "" goto noparams
if "%2" == "" goto noparams

setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION
rem Import settings
call support_path.cmd


rem Check prequisities
rem Check the prequisities
rem Try to satisfy file dependencies:
set DEPLIST=WAGNER.B04 create6.b01 create6.b02
for %%f in ( %DEPLIST% ) do (
	if not exist %%f copy %TEXSUPPORT_DATA%\%%f %%f
	if not exist %%f goto depfailed
)

set RETCODE=0

if not exist %1.c goto nofiles

printc.exe %1.c

move printc.l01 printc_%1.lst
rem build rottex.i01
echo 00.0        00.0      00.00               PHI1/PHI/PHI2 >  rottex.i01
echo    2    1                                IMPOSED IMAG, IMPOSED IDN  >>  rottex.i01
echo %1F.triclinic    >>  rottex.i01
rem build pltodf_c.i01
echo     0                    IEVOD: if =0: Ordinary case  >  pltodf_c.i01

rem Create odflam input file
echo %2             Number of Selectors (Max. 12996) > odflam.i01

rottex.exe rottex.i01 rottex.l01 WAGNER.B04 %1.c %1F.c

printc.exe %1F.c

move printc.l01 printc_%1F.lst


calcodf.exe pltodf_c.i01 CREATE6.B01 CREATE6.B02 %1F.C %1F.001


odflam.exe odflam.i01 odflam.l01 %1F.smt %1F.tx0 %1F.001

goto stop

:noparams
echo This script will extract a discrete set of orientations from a C-file and will store in the SMT format.
echo Syntax:
echo c2smt.cmd FILEPREFIX norient
echo Parameters: 
echo FILEPREFIX  - filename (without extension) of C-format texture representations 
echo norient - number of discrete orientations (grains) to be exported from C-file.
set RETCODE=0
goto stop


:nofiles
echo One of the files doesn't exist:
set RETCODE=1
goto stop

:depfailed
echo Cannot satisfy the prerequisities:
echo %DEPLIST%
set RETCODE=1
goto stop

:stop
exit /B %RETCODE%

