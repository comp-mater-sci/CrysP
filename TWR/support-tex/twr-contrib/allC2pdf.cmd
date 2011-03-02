@echo off
rem $Id$

SETLOCAL ENABLEDELAYEDEXPANSION ENABLEEXTENSIONS

for %%K in (*.c) do (
	echo %%K  %%~nK
	c2ps %%~nK
	ps2pdf %%~nK.ps
) 