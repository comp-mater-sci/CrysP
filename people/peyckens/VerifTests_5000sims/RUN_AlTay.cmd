mkdir rev
copy CTL\*.* rev\
cd rev

rem --- svnserverC ---
copy 0Al-5000_CR60.cfg Main.CTL
D:\svnserverC\projects\trunk\AlTay\AlTay\Win32\Release\AlTay.exe

del  *.TWN *.CUB *.LST *.RPT *.SDV
rem --- svnserverC ---
pause




