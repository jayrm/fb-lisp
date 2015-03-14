/'
 * 
 * Copyright (c) 2007-2008 Jeffery R. Marshall.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
'/

#include once "lisp_runtime.bi"

namespace LISP

import_lisp_function( quote, args )
import_lisp_function( eval, args )
import_lisp_function( progn, args )
import_lisp_function( defun, args )

import_lisp_function( cond, args )
import_lisp_function( if, args )
import_lisp_function( unless, args )
import_lisp_function( when, args )
import_lisp_function( while, args )

import_lisp_function( apply, args )
import_lisp_function( mapcar, args )


'' ---------------------------------------------------------------------------
''
sub bind_runtime_prog( byval functions as LISP_FUNCTIONS ptr )

	BIND_FUNC( functions, "quote", quote )          '' prog
	BIND_FUNC( functions, "eval", eval )        '' prog
	BIND_FUNC( functions, "progn", progn )      '' prog
	BIND_FUNC( functions, "defun", defun )      '' prog

	BIND_FUNC( functions, "cond", cond )        '' prog
	BIND_FUNC( functions, "if", if )            '' prog
	BIND_FUNC( functions, "unless", unless )    '' prog
	BIND_FUNC( functions, "when", when )        '' prog
	BIND_FUNC( functions, "while", while )      '' prog

	BIND_FUNC( functions, "apply", apply )      '' prog
	BIND_FUNC( functions, "mapcar", mapcar )    '' prog

end sub

'' ---------------------------------------------------------------------------
'' (quote expr)
''
define_lisp_function( quote, args )
	function = _CAR( args )
end_lisp_function()

'' ---------------------------------------------------------------------------
'' (eval expr)
''
define_lisp_function( eval, args )
	function = _EVAL(_EVAL(_CAR( args )))
end_lisp_function()

'' ---------------------------------------------------------------------------
'' (progn expr... )
''
define_lisp_function( progn, args )
	function = _PROGN( args )
end_lisp_function()

'' ---------------------------------------------------------------------------
'' (defun name (arglist) expr...)
''
'' returns the name of the function defined
''
define_lisp_function( defun, args )

	_OBJ(p1) = _CAR(args)			'' name
	_OBJ(p2) = _CAR(_CDR(args))		'' argument list
	_OBJ(p3) = _CDR(_CDR(args))		'' function body
	_OBJ(lexpr) = any				'' the defined lambda expression

	'' !!! FIXME: allow overriding built-ins

	if( p1->dtype <> OBJECT_TYPE_IDENTIFIER ) then
		_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )		
		function = _NIL_
		exit function
	end if

	'' Built-in?
	if( ctx->functions->find( p1->value.id ) <> NULL ) then
		_RAISEERROR( LISP_ERR_CANT_REDEFINE_BUILTIN )		
		function = _NIL_
		exit function
	end if
	
	lexpr = _NEW(OBJECT_TYPE_CONS)
	lexpr->value.cell.car = _NEW(OBJECT_TYPE_IDENTIFIER)
	lexpr->value.cell.car->value.id = lisp.strdup("lambda")
	lexpr->value.cell.cdr = _NEW(OBJECT_TYPE_CONS)
	lexpr->value.cell.cdr->value.cell.car = p2
	lexpr->value.cell.cdr->value.cell.cdr = p3

	_SET(p1, lexpr)

	function = p1

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (cond (expr1 [expr2])...)
''
'' requires (progn ...)
''
define_lisp_function( cond, args)

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any
	_OBJ(p3) = any

	do
		p1 = _CAR(p)
		p2 = _EVAL(_CAR(p1))
		if( p2 <> _NIL_ ) then
			p3 = _CDR(p1)
			if( p3 <> _NIL_ ) then
				function = _CALL_BY_NAME( progn, p3 )
			else
				function = p2
			end if
			exit function
		end if
		p = _CDR(p)
	loop while (p <> _NIL_)

	function = _NIL_

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (if expr then-expr else-expr...)
''
'' requires (progn ...)
''
define_lisp_function( if, args)

	_OBJ(p1) = _CAR(args)
	_OBJ(p2) = _CAR(_CDR(args))
	_OBJ(p3) = _CDR(_CDR(args))

	if( _EVAL(p1) <> _NIL_ ) then
		function = _EVAL(p2)
	else
		function = _CALL_BY_NAME( progn, p3 )
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (unless expr else-expr...)
''
'' requires (progn ...)
''
define_lisp_function( unless, args)

	_OBJ(p1) = _CAR(args)
	_OBJ(p2) = _CDR(args)

	if( _EVAL(p1) = _NIL_ ) then
		function = _CALL_BY_NAME( progn, p2 )
	else
		function = _NIL_
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (when expr then-expr...)
''
'' requires (progn ...)
''
define_lisp_function( when, args )

	_OBJ(p1) = _CAR(args)
	_OBJ(p2) = _CDR(args)

	if( _EVAL(p1) <> _NIL_ ) then
		function = _CALL_BY_NAME( progn, p2 )
	else
		function = _NIL_
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (while expr exprs...)
''
'' requires (progn ...)
''
define_lisp_function( while, args )

	_OBJ(p1) = _CAR(args)
	_OBJ(p2) = _CDR(args)

	while (_EVAL(p1) <> _NIL_)
		_CALL_BY_NAME( progn, p2)
	wend

	function = _NIL_

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (apply function [list])
''
define_lisp_function( apply, args )

	_OBJ(f) = _EVAL(_CAR(args))
	_OBJ(a) = _CAR(_CDR(args))

	'' function name?
	if( f = _NIL_ ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
		exit function
	end if

	'' is identifier?
	if( _IS_IDENTIFIER(f) = FALSE ) then
		_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
		function = _NIL_
		exit function
	end if

	'' execute
	function = ctx->evaluator->call_by_name( f->value.id, _EVAL(a) )

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (mapcar <function> <list1...listn>)
''
define_lisp_function( mapcar, args)

	dim n as integer = _LENGTH(args) - 1
	dim l as integer = 0

	if( n <= 0 ) then
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )
		function = _NIL_
		exit function
	end if

	_OBJ(f) = _EVAL(_CAR(args))
	_OBJ(a) = _CDR(args)
	
	'' function name?
	if( f = _NIL_ ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
		exit function
	end if

	'' is identifier?
	if( _IS_IDENTIFIER(f) = FALSE ) then
		_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
		function = _NIL_
		exit function
	end if

	'' Create an object array to store each list argument
	'' and determine the length of the shortest list
	_OBJ(p)(0 to n-1)

	for i as integer = 0 to n-1
		p(i) = _EVAL(_CAR(a))
		a = _CDR(a)
		if( i = 0 ) then
			l = _LENGTH(p(i))
		else
			dim tmplen as integer = _LENGTH(p(i))
			if( tmplen < l ) then
				l = tmplen
			end if
		end if
	next

	'' Shortest list has no elements?
	if( l <= 0 ) then
		function = _NIL_
		exit function
	end if

	'' at this point we know
	'' - we have n lists (from args)
	'' - we have to use l elements from each list

	'' the results
	_OBJ(res_first) = NULL
	_OBJ(res_prev) = NULL
	_OBJ(res_p1)

	'' get the j'th element from the i'th list

	'' iterate through to the length of the lists
	for j as integer = 0 to l-1

		_OBJ(first) = NULL
		_OBJ(prev) = NULL
		_OBJ(p1)

		'' iterate through each list and build a (temporary) argument
		'' list to pass to the function
		for i as integer = 0 to n-1
			p1 = _NEW( OBJECT_TYPE_CONS )
			p1->value.cell.car = _EVAL(_CAR( p(i) ))
			if( first = NULL ) then
				first = p1
			end if
			if( prev <> NULL ) then
				prev->value.cell.cdr = p1
			end if
			prev = p1
		next

		'' call the function
		_OBJ(res1) = ctx->evaluator->call_by_name( f->value.id, first ) 
		
		'' append results to res
		res_p1 = _NEW( OBJECT_TYPE_CONS )
		res_p1->value.cell.car = res1
		if( res_first = NULL ) then
			res_first = res_p1
		end if
		if( res_prev <> NULL ) then
			res_prev->value.cell.cdr = res_p1
		end if
		res_prev = res_p1

		'' next element
		for i as integer = 0 to n-1
			p(i) = _CDR(p(i))
		next
	next

	if( res_first = NULL ) then
		function = _NIL_
	else
		function = res_first
	end if
		
end_lisp_function()


end namespace
