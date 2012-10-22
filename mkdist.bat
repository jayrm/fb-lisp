@echo off
if "%1" == "" goto HELP

cd ..

echo making directories

mkdir h:\upload\lisp
mkdir h:\upload\lisp\dist
mkdir h:\upload\lisp\dist\%1

echo Removing previous files

del h:\upload\lisp\dist\%1\*.zip
del h:\upload\lisp\dist\%1\*.gz

echo making distribution

wzzip -ex -P @lisp\dist-win32.lst h:\upload\lisp\dist\%1\lisp-%1-win32.zip

cd lisp

goto END
:HELP
echo mkdist yyyy.mm.dd
:END
