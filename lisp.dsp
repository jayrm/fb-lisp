# Microsoft Developer Studio Project File - Name="lisp" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=lisp - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "lisp.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "lisp.mak" CFG="lisp - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "lisp - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "lisp - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "lisp - Win32 Release"

# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Cmd_Line "NMAKE /f lisp.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "lisp.exe"
# PROP BASE Bsc_Name "lisp.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Cmd_Line "NMAKE /f lisp.mak"
# PROP Rebuild_Opt "/a"
# PROP Target_File "lisp.exe"
# PROP Bsc_Name "lisp.bsc"
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "lisp - Win32 Debug"

# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Cmd_Line "NMAKE /f lisp.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "lisp.exe"
# PROP BASE Bsc_Name "lisp.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Cmd_Line "e:\fb\script\lisp\mk.bat debug"
# PROP Rebuild_Opt ""
# PROP Target_File "e:\fb\script\lisp\lisp.exe"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "lisp - Win32 Release"
# Name "lisp - Win32 Debug"

!IF  "$(CFG)" == "lisp - Win32 Release"

!ELSEIF  "$(CFG)" == "lisp - Win32 Debug"

!ENDIF 

# Begin Group "src"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\src\lisp.bas
# End Source File
# Begin Source File

SOURCE=.\src\lisp_ctx.bas
# End Source File
# Begin Source File

SOURCE=.\src\lisp_ctx.bi
# End Source File
# Begin Source File

SOURCE=.\src\lisp_eval.bas
# End Source File
# Begin Source File

SOURCE=.\src\lisp_eval.bi
# End Source File
# Begin Source File

SOURCE=.\src\lisp_funcs.bas
# End Source File
# Begin Source File

SOURCE=.\src\lisp_funcs.bi
# End Source File
# Begin Source File

SOURCE=.\src\lisp_int.bas
# End Source File
# Begin Source File

SOURCE=.\src\lisp_int.bi
# End Source File
# Begin Source File

SOURCE=.\src\lisp_lexer.bas
# End Source File
# Begin Source File

SOURCE=.\src\lisp_lexer.bi
# End Source File
# Begin Source File

SOURCE=.\src\lisp_object.bas
# End Source File
# Begin Source File

SOURCE=.\src\lisp_object.bi
# End Source File
# Begin Source File

SOURCE=.\src\lisp_objects.bas
# End Source File
# Begin Source File

SOURCE=.\src\lisp_objects.bi
# End Source File
# Begin Source File

SOURCE=.\src\lisp_parser.bas
# End Source File
# Begin Source File

SOURCE=.\src\lisp_parser.bi
# End Source File
# Begin Source File

SOURCE=.\src\lisp_runtime.bi
# End Source File
# Begin Source File

SOURCE=.\src\lisp_runtime_console.bas
# End Source File
# Begin Source File

SOURCE=.\src\lisp_runtime_data.bas
# End Source File
# Begin Source File

SOURCE=.\src\lisp_runtime_list.bas
# End Source File
# Begin Source File

SOURCE=.\src\lisp_runtime_math.bas
# End Source File
# Begin Source File

SOURCE=.\src\lisp_runtime_prog.bas
# End Source File
# Begin Source File

SOURCE=.\src\lisp_runtime_system.bas
# End Source File
# Begin Source File

SOURCE=.\src\makefile
# End Source File
# End Group
# Begin Group "inc"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\inc\lisp.bi
# End Source File
# Begin Source File

SOURCE=.\inc\lisp_err.bi
# End Source File
# End Group
# Begin Group "examples"

# PROP Default_Filter ""
# Begin Group "lisp"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\examples\lisp\append.lsp
# End Source File
# Begin Source File

SOURCE=.\examples\lisp\hanoi.lsp
# End Source File
# Begin Source File

SOURCE=.\examples\lisp\hello.lsp
# End Source File
# Begin Source File

SOURCE=.\examples\lisp\lisp.lsp
# End Source File
# Begin Source File

SOURCE=.\examples\lisp\testsuite.lsp
# End Source File
# End Group
# Begin Source File

SOURCE=.\examples\bindfunc.bas
# End Source File
# Begin Source File

SOURCE=.\examples\hello.bas
# End Source File
# Begin Source File

SOURCE=.\examples\lispdemo.bas
# End Source File
# Begin Source File

SOURCE=.\examples\makefile
# End Source File
# Begin Source File

SOURCE=.\examples\printout.bas
# End Source File
# Begin Source File

SOURCE=.\examples\runlisp.bas
# End Source File
# Begin Source File

SOURCE=.\examples\temp.lsp
# End Source File
# Begin Source File

SOURCE=.\examples\test.bas
# End Source File
# Begin Source File

SOURCE=.\examples\test.lsp
# End Source File
# End Group
# Begin Source File

SOURCE=.\changelog.txt
# End Source File
# Begin Source File

SOURCE=".\dist-win32.lst"
# End Source File
# Begin Source File

SOURCE=.\license.txt
# End Source File
# Begin Source File

SOURCE=.\mk.bat
# End Source File
# Begin Source File

SOURCE=.\mkdist.bat
# End Source File
# Begin Source File

SOURCE=.\notes.txt
# End Source File
# Begin Source File

SOURCE=.\readme.txt
# End Source File
# End Target
# End Project
