	LISP Evaluator for FreeBASIC 
	Copyright (c) 2007 Jeffery R. Marshall.

	An embedded LISP interpreter written entirely in FreeBASIC for use 
	with FreeBASIC applications.


LICENSE

	See the 'license.txt' file in the same folder as this readme.txt


COMPILING

	To compile the library, use the following commands:

	cd src
	make


INSTALLING

	There is no install script.  To use, either copy the library and
	include files to an appropriate directory, or include the paths
	to ./lib and ./inc as search paths.

	To use the library as-is, only the following files are needed

	./inc/lisp.bi
	./inc/lisp_err.bi
	./lib/liblisp.a

	To use the library as well as extend the built-in functions, all
	of the include files from ./src are also needed.


INTRINSIC FUNCTIONS

	(* <number> <number>...)
	(+ <number> <number>...)
	(- <number> <number>...)
	(/ <number> <number>...)
	(/= <atom> <atom>...)
	(< <atom> <atom>...)
	(<= <atom> <atom>...)
	(= <atom> <atom>...)
	(> <atom> <atom>...)
	(>= <atom> <atom>...)
	(and <expr>...)
	(atom <item>)
	(car <list>)
	(cdr <list>)
	(cond (expr1 [expr2])...)
	(cons expr1 expr2)
	(defun <sym> <arglist> <expr>...)
	(eval <expr>)
	(garbage-collect)
	(gc)
	(if expr then-expr else-expr...)
	(length <list>)
	(list expr1...)
	(not <expr>)
	(null <expr>)
	(or <expr>...)
	(princ <expr>...)
	(progn <expr>... )
	(quote <expr>)
	(set <sym> <expr>)
	(setf <sym1> <expr1> [<sym2> <expr2>]...)
	(setq <sym1> <expr1> [<sym2> <expr2>]...)
	(unless <expr> else-expr...)
	(when <expr> then-expr...)
	(while <expr> exprs...)


EXAMPLE USAGE IN FREEBASIC

	#include "lisp.bi"

	DIM lsp AS LispModule, expr AS STRING

	expr = "(lisp expression)"
	lsp.eval( expr )


EXAMPLE PROGRAMS

	The directory ./examples have a few example example programs. To
	compile all of the example programs, use:

	cd examples
	make

	Each example also has its own instructions for comiling and usage
	in the .bas source file.


WHY DID I WRITE THIS LIBRARY?

	My main motivation for writing this library was that I wanted to make a
	non-trivial object oriented program in FreeBASIC to see what the
	experience was like.  Overall, it mostly didn't suck.  Except just as in
	C++, much effort needs to go in to managing memory.  I spent probably
	25% of my time hunting down a memory leak.  And this was with careful
	planning of object life-times from the beginning.  In the end it was a
	stupid mistake that was causing the memory leak, but, isn't that always
	the case?

	Otherwise, I like the LISP language.  And I thought to create something
	that could be used as a script/macro language for use in my programs.
	Also with the OO design of the library, it is possible to instance
	completely seperate LispModule objects that can execute independant of
	each other.


OBTAINING THE ORIGINAL SOURCE

	You should be able to find this source at http://www.coderjeff.ca


ACKNOWLEDGEMENTS

	Sandro Sigala - That a C version of a simple LISP evaluator existed and
	was available under an Open Source license saved huge amounts time.  
	Thanks to Sandro, creating this library took only a minimal amount of 
	time.
	

EOF
