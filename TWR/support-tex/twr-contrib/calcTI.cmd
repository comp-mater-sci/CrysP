@echo off

if "%1" == "" goto noparams

rem Make sure that it is sane here.
for %%f in (calcTI?.tmp) do (
	del %%f
)

for %%i in (%*) do (
	echo %%i >> calcTI1.tmp
	printc %%i > calcTIlst.tmp 
	head -n 2  calcTIlst.tmp | tail -1 >> calcTI2.tmp
	grep "TEXTURE INDEX" calcTIlst.tmp | cut -d= -f2 >> calcTI3.tmp
)
rem Merge the results
paste calcTI1.tmp calcTI2.tmp calcTI3.tmp

rem Cleanup
for %%f in (calcTI?.tmp calcTIlst.tmp) do (
	del %%f
)

goto stop

:noparams
echo The script requires a list of C-file names (at least one) with extensions.
exit /B 1
goto stop

:stop
