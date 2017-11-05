# Microsoft Developer Studio Project File - Name="lisp_examples" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=lisp_examples - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "lisp_examples.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "lisp_examples.mak" CFG="lisp_examples - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "lisp_examples - Win32 Release" (based on\
 "Win32 (x86) External Target")
!MESSAGE "lisp_examples - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "lisp_examples - Win32 Release"

# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Cmd_Line "NMAKE /f lisp_examples.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "lisp_examples.exe"
# PROP BASE Bsc_Name "lisp_examples.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Cmd_Line "NMAKE /f lisp_examples.mak"
# PROP Rebuild_Opt "/a"
# PROP Target_File "lisp_examples.exe"
# PROP Bsc_Name "lisp_examples.bsc"
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "lisp_examples - Win32 Debug"

# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Cmd_Line "NMAKE /f lisp_examples.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "lisp_examples.exe"
# PROP BASE Bsc_Name "lisp_examples.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Cmd_Line "e:\fb\script\lisp\examples\mk.bat debug"
# PROP Rebuild_Opt ""
# PROP Target_File "e:\fb\script\lisp\examples\runlisp.exe"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "lisp_examples - Win32 Release"
# Name "lisp_examples - Win32 Debug"

!IF  "$(CFG)" == "lisp_examples - Win32 Release"

!ELSEIF  "$(CFG)" == "lisp_examples - Win32 Debug"

!ENDIF 

# Begin Group "lisp"

# PROP Default_Filter ""
# Begin Source File

SOURCE=lisp\append.lsp
# End Source File
# Begin Source File

SOURCE=lisp\factorial.lsp
# End Source File
# Begin Source File

SOURCE=lisp\hanoi.lsp
# End Source File
# Begin Source File

SOURCE=lisp\hello.lsp
# End Source File
# Begin Source File

SOURCE=lisp\lisp.lsp
# End Source File
# End Group
# Begin Source File

SOURCE=bindfunc.bas
# End Source File
# Begin Source File

SOURCE=hello.bas
# End Source File
# Begin Source File

SOURCE=lispdemo.bas
# End Source File
# Begin Source File

SOURCE=makefile
# End Source File
# Begin Source File

SOURCE=.\mk.bat
# End Source File
# Begin Source File

SOURCE=printout.bas
# End Source File
# Begin Source File

SOURCE=runlisp.bas
# End Source File
# End Target
# End Project
