
if "%1" == "" goto noparams

set OUTDIR=%1

copy odf_a.i01 %OUTDIR%
copy odf_q.i01 %OUTDIR%
copy odf_o.i01 %OUTDIR%
copy odf_p.i01 %OUTDIR%



copy \odf\c_even.001 %OUTDIR%\c_even.001
copy \odf\c.001 %OUTDIR%\c.001
copy \odf\aodf.001 %OUTDIR%\aodf.001
copy \odf\plot.001 %OUTDIR%\ploto.001
copy \odf\plot.002 %OUTDIR%\plotp.001
copy \odf\odf_a.l01 %OUTDIR%\odf_a.l01
copy \odf\odf_q.l01 %OUTDIR%\odf_q.l01
copy \odf\odf_o.l01 %OUTDIR%\odf_o.l01
copy \odf\odf_p.l01 %OUTDIR%\odf_p.l01

goto stop

:noparams
echo This script requires one parameter: location of target directory.

:stop