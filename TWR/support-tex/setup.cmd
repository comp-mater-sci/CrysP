@echo off

rem enable extensions
SETLOCAL ENABLEDELAYEDEXPANSION
SETLOCAL ENABLEEXTENSIONS

set packagedir=%CD%

set shellname=mtmfhm-twrcontrib.cmd
set masterbindir=%packagedir%\Master\ODF\ODFEXE
set supportdir=%packagedir%\twr-contrib
set supportconf=%supportdir%\support_path.cmd
set supportcompatdir=%supportdir%\compat\bin
set supportbindir=%supportdir%\bin

echo Setting toolset root as %packagedir%
echo Setting twr-support as %supportdir%

echo Customizing shell...
rem create customized shell
echo @set PATH=%masterbindir%;%supportdir%;%supportbindir%;%supportcompatdir%;%%PATH%% > %shellname%
rem Create support tools configuration file
echo @echo off  > %supportconf%
echo @rem Path to support tools   >> %supportconf%
echo set TEXSUPPORT_INSTALL=%supportdir%   >> %supportconf%
echo @rem Path to the directory containing common datasets >> %supportconf%
echo set TEXSUPPORT_DATA=%%TEXSUPPORT_INSTALL%%\data >> %supportconf%
echo set PATH=%supportcompatdir%;%%PATH%%  >> %supportconf%
rem Create link file
%supportdir%\setup\mklink.vbs %packagedir%\%shellname%
