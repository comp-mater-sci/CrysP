SETLOCAL ENABLEDELAYEDEXPANSION
SETLOCAL ENABLEEXTENSIONS

set PREFIX=elem_uniax45

set FLIST=

for /L %%n in (1,1,16) do (
	set FILE=!PREFIX!_%%n_bbc2008vef.dat
	
	echo Increment%%n > !FILE!.tmp
	
	tail +3  !FILE! >> !FILE!.tmp
	
	set FLIST=!FLIST! !FILE!.tmp 
)

echo !FLIST!

paste !FLIST! > !PREFIX!_bbc2008vef.all
