echo. > elem_all.str

for /L %%i in (1,1,20) do (
cat elem_%%i.str >> elem_all.str
echo. >> elem_all.str
echo. >> elem_all.str

)