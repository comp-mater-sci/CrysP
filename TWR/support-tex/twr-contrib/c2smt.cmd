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

rem construct input filename
rem FNAME will contain bare filename
set FNAME=%~n1
if "%~x1" == "" (
	set INPFNAME=!FNAME!.c
) else (
	set	INPFNAME=%1
)
rem Check if file exists
if not exist !INPFNAME! (
	set MISSINGFILE=!INPFNAME!
	goto nofile
)
rem construct output filename
set POUTFNAME=!FNAME!F
set OUTFNAME=!POUTFNAME!.c

printc.exe !INPFNAME!

move printc.l01 printc_!FNAME!.lst
rem build rottex.i01
echo 00.0        00.0      00.00               PHI1/PHI/PHI2 >  rottex.i01
echo    2    1                                IMPOSED IMAG, IMPOSED IDN  >>  rottex.i01
echo !POUTFNAME!.triclinic    >>  rottex.i01
rem build pltodf_c.i01
echo     0                    IEVOD: if =0: Ordinary case  >  pltodf_c.i01

rem Create odflam input file
echo %2             Number of Selectors (Max. 12996) > odflam.i01

rottex.exe rottex.i01 rottex.l01 WAGNER.B04 !INPFNAME! !OUTFNAME!

printc.exe !OUTFNAME!

move printc.l01 printc_!POUTFNAME!.lst


calcodf.exe pltodf_c.i01 CREATE6.B01 CREATE6.B02 !OUTFNAME! !POUTFNAME!.001


odflam.exe odflam.i01 odflam.l01 !POUTFNAME!.smt !POUTFNAME!.tx0 !POUTFNAME!.001

goto stop

:noparams
echo This script will extract a discrete set of orientations from a C-file and will store in the SMT format.
echo Syntax:
echo c2smt.cmd FILEPREFIX norient
echo Parameters: 
echo FILEPREFIX  - filename (with or without extension) of C-format texture representations 
echo norient - number of discrete orientations (grains) to be exported from C-file.
set RETCODE=0
goto stop


:nofiles
echo One of the files doesn't exist: !MISSINGFILE!
set RETCODE=1
goto stop

:depfailed
echo Cannot satisfy the prerequisities:
echo %DEPLIST%
set RETCODE=1
goto stop

:stop
exit /B %RETCODE%

