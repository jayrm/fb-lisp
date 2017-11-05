call c:\batch\setpath.bat FBWIN
e:
cd \fb\script\lisp\examples

if /i "%1" == "clean" goto CLEAN
if /i "%1" == "debug" goto DEBUG
make
goto END

:CLEAN
make clean
goto END

:DEBUG
make DEBUG=1
goto END

:END
