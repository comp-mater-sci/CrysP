@echo off
rem $Id$

if "%2" == "" (
set OUTPUT=mapping.txt
) else (
set OUTPUT=%2
)

if "%3" == "" (
set MOUTPUT=step_mapping.txt
) else (
set MOUTPUT=%3
)

if "%1" == "" goto noparam


if not exist %1 goto nofile

gawk "BEGIN{i=0;pl=0.0;}{print $1, $2, pl, pl;}" %1 > %OUTPUT%

gawk "BEGIN{i=0;pl=0.0;}{print $1, $2}" %1 > %MOUTPUT%

goto :eof

:noparam
echo This script will create step mapping file
echo One parameter is required, name of map file.
echo.
echo Usage:
echo makemapping mapfile [slave_mapping_file] [master_mapping_file]
echo.
echo Default names for output files:
echo     slave_mapping_file:  %OUTPUT%
echo     master_mapping_file: %MOUTPUT%
goto :eof

:nofile
echo Input file not found.
goto :eof



