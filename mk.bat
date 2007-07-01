call c:\batch\setpath.bat fb0170
e:
cd \fb\script\lisp\src

if "%1" == "clean" goto CLEAN
if "%1" == "debug" goto DEBUG
make
cd \fb\script\lisp\examples
make
cd ..
goto END

:CLEAN
make clean
cd \fb\script\lisp\examples
make clean
cd ..
goto END

:DEBUG
make DEBUG=1
cd \fb\script\lisp\examples
make DEBUG=1
cd ..
goto END

:END
