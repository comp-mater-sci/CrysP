@echo off
rem $Id$

set PWD=%CD%
for %%K in (*.c) do (
	echo %%K  %%~nK
	c2ps %%~nK
	ps2pdf %%~nK.ps
) 