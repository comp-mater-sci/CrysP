@echo off
rem $Id$

if "%1" == "" goto noparam
if "%2" == "" (
set OUTPUT=%~n1.qrs
) else (
set OUTPUT=%2
)


tail -n 365 %1  | head -n  361 | gawk "{print  $4, $5, $6, $7;}" > %OUTPUT%

goto :eof

:noparam
echo This script will extract rqs data from LS3 file.
echo One parameter is required, name of LS3 file
echo Usage:
echo rqs input_prefix output_file

goto :eof



