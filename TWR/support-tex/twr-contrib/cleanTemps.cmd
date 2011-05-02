@echo off
rem $Id$

setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION
rem Import settings
call support_path.cmd

if "%1" == "" goto noparams

set FINDCMD=\utils\gnuwin32\bin\find.exe
set RMCMD=rm

if "%1" == "plttmp" (
call :plttmp 
) else (
call :unknown
)
goto :eof

rem 
:plttmp
for %%P in (create6 AODF) do (
	%FINDCMD% . -name %%P* -exec %RMCMD% {} ; 
)
goto :eof

:unknown
echo Unknown target name
goto :eof

:noparams
echo Perform a cleaning action to the current directory.
echo Syntax:
echo.
echo clean target
echo.
echo Possible targets:
echo    plttmp - clean temporaries that remain after conversions from C files to PS/PDF/PNG/GIF
goto :eof


