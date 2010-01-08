@echo off

if "%1" == "" goto noparams


for %%f in (calcTI?.tmp) do (
	del %%f
)

for %%i in (%*) do (
	echo %%i >> calcTI1.tmp
	printc %%i | grep "TEXTURE INDEX" | cut -d= -f2 >> calcTI2.tmp
)

paste calcTI1.tmp calcTI2.tmp

goto stop

:noparams
echo The script requires a list of C-file names (at least one) with extensions.
exit /B 1
goto stop

:stop
