@echo off

set PDFLIST=pdflist.txt
set MD5SUMLIST=pdflist_md5sum.txt
set SHA1SUMLIST=pdflist_sha1sum.txt

echo Generating the list of PDFs...

\utils\gnuwin32\bin\find.exe . -name "*.pdf" > %PDFLIST%


echo Calculating MD5 and SHA1 hashes...

del /Q %MD5SUMLIST% %SHA1SUMLIST%


FOR /F "tokens=*" %%i in (%PDFLIST%) do (
rem echo %%i 
md5sum "%%i" >> %MD5SUMLIST%
sha1sum "%%i" >> %SHA1SUMLIST%
)