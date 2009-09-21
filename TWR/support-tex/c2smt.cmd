@echo off

if "%1" == "" goto noparams

rem Check prequisities
rem if not exist pltodf_c.i01 goto nofiles
rem if not exist rottex.i01 goto nofiles
if not exist WAGNER.B04 goto nofiles
if not exist odflam.i01 goto nofiles
if not exist create6.b01 goto nofiles
if not exist create6.b02   goto nofiles



printc.exe %1.c

move printc.l01 printc_%1.lst
rem build rottex.i01
echo 00.0        00.0      00.00               PHI1/PHI/PHI2 >  rottex.i01
echo    2    1                                IMPOSED IMAG, IMPOSED IDN  >>  rottex.i01
echo %1F.triclinic    >>  rottex.i01
rem build pltodf_c.i01
echo     0                    IEVOD: if =0: Ordinary case  >  pltodf_c.i01


rottex.exe rottex.i01 rottex.l01 WAGNER.B04 %1.c %1F.c

printc.exe %1F.c

move printc.l01 printc_%1F.lst


calcodf.exe pltodf_c.i01 CREATE6.B01 CREATE6.B02 %1F.C %1F.001


odflam.exe odflam.i01 odflam.l01 %1F.smt %1F.tx0 %1F.001

goto stop

:noparams
echo This script requires one parameter: filename (without extension) of C-format texture representations.
echo Use file odflam.i01 to specify the number of orientations to select.
goto stop

:nofiles
echo One of the files doesn't exist:
echo odflam.i01 create6.b01 create6.b02 WAGNER.B04
goto stop



:stop