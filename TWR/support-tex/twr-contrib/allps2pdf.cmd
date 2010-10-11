@echo off
rem $Id$

set PWD=%CD%
for %%K in (*.ps) do (
	echo %%K  %%~nK.pdf
	ps2pdf %%~nK.ps %%~nK.pdf
) 