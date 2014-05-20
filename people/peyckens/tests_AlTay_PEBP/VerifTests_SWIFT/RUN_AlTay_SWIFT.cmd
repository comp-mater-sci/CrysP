cd newSWIFT

rem SWIFT-K test simulations::::::::::::::::
copy KOST2_FC.i01 Main.ctl
D:\svnserverB\MTM\branches\AlTaySub\AlTay\Release\altay.exe

copy KOST2_AL.i01 Main.ctl
D:\svnserverB\MTM\branches\AlTaySub\AlTay\Release\altay.exe


rem SWIFT-S test simulations::::::::::::::::
copy KOST3_FC.i01 Main.ctl
D:\svnserverB\MTM\branches\AlTaySub\AlTay\Release\altay.exe

copy KOST3_AL.i01 Main.ctl
D:\svnserverB\MTM\branches\AlTaySub\AlTay\Release\altay.exe


rem reference simulations with VOCE that also have initial TAU=100::::
copy KOST1_FC.i01 Main.ctl
D:\svnserverB\MTM\branches\AlTaySub\AlTay\Release\altay.exe

copy KOST1_AL.i01 Main.ctl
D:\svnserverB\MTM\branches\AlTaySub\AlTay\Release\altay.exe

del *.TWN

pause