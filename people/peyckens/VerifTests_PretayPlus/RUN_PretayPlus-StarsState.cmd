mkdir rev
copy CTL\*.CTL rev
copy DAT\*.DAT rev
cd rev

copy wenk.CTL pretay.CTL
C:\twrmtm\projects\branches\branch-AltayStarsState\AlTay\PretayPlus\Release\PretayPlus.exe wenk.dat wenk.l01 wenk.pre
copy fcc.CTL pretay.CTL
C:\twrmtm\projects\branches\branch-AltayStarsState\AlTay\PretayPlus\Release\PretayPlus.exe fcc.dat fcc.l01 fcc.pre
copy fcct.CTL pretay.CTL
C:\twrmtm\projects\branches\branch-AltayStarsState\AlTay\PretayPlus\Release\PretayPlus.exe fcct.dat fcct.l01 fcct.pre
copy bcc.CTL pretay.CTL
C:\twrmtm\projects\branches\branch-AltayStarsState\AlTay\PretayPlus\Release\PretayPlus.exe bcc.dat bcc.l01 bcc.pre
copy bcc2.CTL pretay.CTL
C:\twrmtm\projects\branches\branch-AltayStarsState\AlTay\PretayPlus\Release\PretayPlus.exe bcc2.dat bcc2.l01 bcc2.pre

del *.CTL *.DAT
pause




