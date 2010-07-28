@echo off
rem $Id$

call c2ps %1 %2 %3

if NOT ERRORLEVEL 1 ps2pdf %~n1.ps


