@echo off
rem $Id$

setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION

rem Import settings
call support_path.cmd

if "%1" == "" goto noparams

set RETCODE=1
rem construct filename
set FNAME=%~n1
if "%~x1" == "" (
	set INPFNAME=!FNAME!.c
) else (
	set	INPFNAME=%1
)
rem Check if file exists
if not exist !INPFNAME! goto nofile

rem Check the prequisities
rem Try to satisfy file dependencies:
set DEPLIST=pltodf_o.i01 color plotter create6.b01 create6.b02
for %%f in ( %DEPLIST%  ) do (
	if not exist %%f copy %TEXSUPPORT_DATA%\%%f %%f
	if not exist %%f goto depfailed
)

set RETCODE=0

rem build pltodf_c.i01 if not found.
if not exist pltodf_c.i01 (
	echo     0                    IEVOD: if =0: Ordinary case  >  pltodf_c.i01
)


calcodf pltodf_c.i01 create6.b01 create6.b02 !INPFNAME! aodf.001
odfplt pltodf_o.i01 pltodf_o.l01 aodf.001 !FNAME!.001 | tee !FNAME!_c2ps.log

PLOTPSC.EXE plotter color !FNAME!.001

move p01.ps !FNAME!.ps

if exist !FNAME!.001 del /Q /F !FNAME!.001

goto stop

echo off

:noparams
echo This utility will plot C-coefficient file to postscript file.
echo The script requires one parameter: name of .C file (with or without extension).
echo Use file pltodf_o.i01 to customize the output.
set RETCODE=1
goto stop

:nofile
echo File !INPFNAME! not found.
set RETCODE=1
goto stop

:depfailed
echo Cannot satisfy the prerequisities:
echo %DEPLIST%
set RETCODE=1
goto stop

:stop
exit /B %RETCODE%