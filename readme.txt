	LISP Evaluator for FreeBASIC 
	Copyright (c) 2007-2008 Jeffery R. Marshall.

	An embedded LISP interpreter written entirely in FreeBASIC for use 
	with FreeBASIC applications.


0. INTRODUCTION

	My main motivation for writing this is I like the idea of LISP as a
	script or macro language that can be embedded in my programs.

	I also wanted to make a	non-trivial object oriented program in 
	FreeBASIC. And the design of this LISP evaluator is exactly that.  
	Through the top-level object LispModule, it is possible to instance
	two or more	completely seperate evaluators.  The only thing shared 
	between the instances is the string text for error messages.

	In writing the library, the experience didn't completely suck, except
	in debugging it was just like C++, spending a significant smount of 
	time chasing down a memory leak due to the manual managed memory 
	allocations. And this was with careful planning of object life-times 
	from the beginning.  In the end it was a stupid mistake that was 
	causing the memory leak, but, isn't that always	the case?

	Using this library is probably not a good way to learn LISP.  Use a real
	LISP compiler or interpreter for that.


1. LICENSE

	See the 'license.txt' file in the same folder as this readme.txt


2. COMPILING

	To compile the library, use the following commands:

	cd src
	make


3. INSTALLING

	There is no install script.  To use, either copy the library and
	include files to an appropriate directory, or include the paths
	to ./lib and ./inc as search paths.

	To use the library as-is, only the following files are needed

	./inc/lisp.bi
	./inc/lisp_err.bi
	./lib/liblisp.a

	To use the library as well as extend the built-in functions, all
	of the include files from ./src are also needed.


4. INTRINSIC FUNCTIONS

	Following is a list of the built-in functions available from the library.
	
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
	(append <expr>...)
	(apply function [list])
	(atom <item>)
	(car <list>)
	(cdr <list>)
	(cond (expr1 [expr2])...)
	(cons <expr1> <expr2>)
	(consp <expr>)
	(defun <sym> <arglist> <expr>...)
	(elt <list> <index>)
	(eval <expr>)
	(garbage-collect)
	(gc)
	(if expr then-expr else-expr...)
	(integerp <expr>)
	(lambda expr...)
	(last <list>)
	(length <list>)
	(list <expr1>...)
	(listp <expr>)
	(mapcar <function> <list1...listn>)
	(not <expr>)
	(nth <index> <list>)
	(null <expr>)
	(numberp <expr>)
	(or <expr>...)
	(princ <expr>...)
	(progn <expr>... )
	(quote <expr>)
	(set <sym> <expr>)
	(setf <sym1> <expr1> [<sym2> <expr2>]...)
	(setq <sym1> <expr1> [<sym2> <expr2>]...)
	(stringp <expr>)
	(unless <expr> else-expr...)
	(when <expr> then-expr...)
	(while <expr> exprs...)
	(zerop <expr>)


5. EXAMPLE USAGE IN FREEBASIC

	#include "lisp.bi"
	using LISP

	DIM lsp AS LispModule, expr AS STRING


	expr = "(lisp expression)"
	lsp.eval( expr )


6. EXAMPLE PROGRAMS

	The directory ./examples have a few example example programs. To
	compile all of the example programs, use:

	cd examples
	make

	Each example also has its own instructions for comiling and usage
	in the .bas source file.


7. OBTAINING THE ORIGINAL SOURCE

	Sources whould be at my own webpage here: http://www.coderjeff.ca


8. ACKNOWLEDGEMENTS

	Sandro Sigala - That a C version of a simple LISP evaluator existed and
	was available under an Open Source license saved huge amounts time.  
	Thanks to Sandro for slisp-1.2, creating this library took only a minimal
	amount of time.
	

EOF
