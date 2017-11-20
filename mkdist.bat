@echo off
if "%1" == "" goto HELP

cd ..

echo making directories

mkdir .\lisp\dist
mkdir .\lisp\dist\%1

echo Removing previous files

del .\lisp\dist\%1\*.zip
del .\lisp\dist\%1\*.gz

echo making distribution

zip .\lisp\dist\%1\lisp-%1-win32.zip @lisp\dist-win32.lst

cd lisp

goto END
:HELP
echo.mkdist yyyy.mm.dd
echo.writes zip file in .\lisp\dist\yyyy.mm.dd\lisp-yyyy.mm.dd-win32.zip
:END
