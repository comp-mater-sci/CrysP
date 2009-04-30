# Microsoft Developer Studio Project File - Name="alamel" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=alamel - Win32 DebugGmetex
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "alamel.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "alamel.mak" CFG="alamel - Win32 DebugGmetex"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "alamel - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "alamel - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "alamel - Win32 DebugGmetex" (based on "Win32 (x86) Console Application")
!MESSAGE "alamel - Win32 ReleaseGmetex" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "alamel - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /assume:buffered_io /compile_only /math_library:fast /nologo /transform_loops /tune:p6p /warn:nofileopt /fast /fpp:"/ansi"
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "alamel - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /browser /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt /fpp:"/ansi"
# SUBTRACT F90 /fpp
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept

!ELSEIF  "$(CFG)" == "alamel - Win32 DebugGmetex"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "alamel___Win32_DebugGmetex"
# PROP BASE Intermediate_Dir "alamel___Win32_DebugGmetex"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "DebugGmetex"
# PROP Intermediate_Dir "DebugGmetex"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /browser /check:bounds /compile_only /debug:full /define:"GMETEX_COMM" /nologo /traceback /warn:argument_checking /warn:nofileopt /fpp:"/ansi"
# ADD F90 /browser /check:bounds /compile_only /debug:full /define:"GMETEX_COMM" /nologo /traceback /warn:argument_checking /warn:nofileopt /fpp:"/ansi"
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept /libpath:"C:\work\ipctests\Debug"
# ADD LINK32 kernel32.lib gmetexCommShm.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept /libpath:"C:\work\TWRMTMProject\code\trunk\gmetex\Debug"

!ELSEIF  "$(CFG)" == "alamel - Win32 ReleaseGmetex"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "alamel___Win32_ReleaseGmetex"
# PROP BASE Intermediate_Dir "alamel___Win32_ReleaseGmetex"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "ReleaseGmetex"
# PROP Intermediate_Dir "ReleaseGmetex"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /define:"GMETEX_COMM" /nologo /warn:nofileopt /fast /fpp:"/ansi"
# ADD F90 /compile_only /define:"GMETEX_COMM" /nologo /warn:nofileopt /fast /fpp:"/ansi"
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib gmetexCommShm.lib /nologo /subsystem:console /machine:I386 /libpath:"C:\work\TWRMTMProject\code\trunk\gmetex\Release"

!ENDIF 

# Begin Target

# Name "alamel - Win32 Release"
# Name "alamel - Win32 Debug"
# Name "alamel - Win32 DebugGmetex"
# Name "alamel - Win32 ReleaseGmetex"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\Alg0.for
# End Source File
# Begin Source File

SOURCE=.\Alg1.for
# End Source File
# Begin Source File

SOURCE=.\Alg2.for
# End Source File
# Begin Source File

SOURCE=.\Dynfil.for
# End Source File
# Begin Source File

SOURCE=.\Lp.for
# End Source File
# Begin Source File

SOURCE=.\Main1.for
# End Source File
# Begin Source File

SOURCE=.\Pancak2.for
NODEP_F90_PANCA=\
	".\ReleaseGmetex\MICROSTR.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Sliprat.for
# End Source File
# Begin Source File

SOURCE=.\Stelsel.for
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
