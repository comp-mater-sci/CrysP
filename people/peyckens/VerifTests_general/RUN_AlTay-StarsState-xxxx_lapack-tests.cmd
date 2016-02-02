
rem set "FOLDER_EXTENSION=SLIPRAT"
rem set "FOLDER_EXTENSION=SLIPRAT_LAPACK_ENABLED"
set "FOLDER_EXTENSION=sliprateSVD_"

set "MODE=Debug"
set "MOD=D"
rem set "MODE=Release"
rem set "MOD=R"

set "CTL_extension=CTL_StarsState_lapack-tests"

set "ALTAY_EXE=C:\twrmtm\projects\branches\branch-AltayStarsState\AlTay\AlTay\x64\%MODE%\AlTay.exe"
               
mkdir rev_x64%MOD%_%FOLDER_EXTENSION%
copy %CTL_extension%\*.CTL rev_x64%MOD%_%FOLDER_EXTENSION%
cd rev_x64%MOD%_%FOLDER_EXTENSION%

copy fcc12_AL.CTL Main.CTL
%ALTAY_EXE%
copy bcc12_AL.CTL Main.CTL
%ALTAY_EXE%
copy bcc24_AL.CTL Main.CTL
%ALTAY_EXE%
copy bcc48_AL.CTL Main.CTL
%ALTAY_EXE%
copy diftau_AL.CTL Main.CTL
%ALTAY_EXE%

copy fcc12_FC.CTL Main.CTL
%ALTAY_EXE%
copy bcc12_FC.CTL Main.CTL
%ALTAY_EXE%
copy bcc24_FC.CTL Main.CTL
%ALTAY_EXE%
copy bcc48_FC.CTL Main.CTL
%ALTAY_EXE%
copy diftau_FC.CTL Main.CTL
%ALTAY_EXE%

del  *.CUB *.h5
pause




