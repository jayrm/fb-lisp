/'
 * 
 * Copyright (c) 2007-2017 Jeffery R. Marshall.  All rights reserved.
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

import_lisp_function( add, args )     '' +
import_lisp_function( sub, args )     '' -
import_lisp_function( mul, args )     '' *
import_lisp_function( div, args )     '' /

import_lisp_function( abs, args )

import_lisp_function( and, args )
import_lisp_function( or, args )
import_lisp_function( not, args )
import_lisp_function( null, args )    '' !!! FIXME:

import_lisp_function( incr, args )    '' 1+
import_lisp_function( decr, args )    '' 1-

import_lisp_function( bitnot, args )  '' ~

'' ---------------------------------------------------------------------------
''
sub bind_runtime_math( byval functions as LISP_FUNCTIONS ptr )

	BIND_FUNC( functions, "+", add )        '' math (tests/mathops.lsp)
	BIND_FUNC( functions, "-", sub )        '' math (tests/mathops.lsp)
	BIND_FUNC( functions, "*", mul )        '' math (tests/mathops.lsp)
	BIND_FUNC( functions, "/", div )        '' math (tests/mathops.lsp)

	BIND_FUNC( functions, "abs", abs )      '' math (tests/mathops.lsp)

	BIND_FUNC( functions, "and", and )      '' math-logical (tests/bool_op.lsp)
	BIND_FUNC( functions, "or", or )        '' math-logical (tests/bool_op.lsp)
	BIND_FUNC( functions, "not", not )      '' math-logical (tests/bool_op.lsp)

	BIND_FUNC( functions, "null", not )     '' data-logical !!! FIXME:

	BIND_FUNC( functions, "1+", incr )      '' math (tests/mathops.lsp)
	BIND_FUNC( functions, "1-", decr )      '' math (tests/mathops.lsp)

	BIND_FUNC( functions, "~", bitnot )     '' math

end sub

'' ---------------------------------------------------------------------------
'' (+ expr... )
''
define_lisp_function( add, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(res) = any

	res = _NEW( OBJECT_TYPE_INTEGER )

	do
		p1 = _EVAL(_CAR(p))
		if( _IS_NUMBER(p1) ) then
			*res += *p1
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			exit do
		end if
		p = _CDR(p)
	loop while( p <> _NIL_ )
	
	function = res

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (- expr... )
''
define_lisp_function( sub, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(res) = any

	res = _NEW( OBJECT_TYPE_INTEGER )

	do
		p1 = _EVAL(_CAR(p))
		if( _IS_NUMBER(p1) ) then
			if( (p = args) and (_CDR(p) <> _NIL_)) then
				res->dtype = p1->dtype
				res->value = p1->value
			else
				*res -= *p1
			end if
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			exit do
		end if
		p = _CDR(p)
	loop while( p <> _NIL_ )
	
	function = res

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (* expr... )
''
define_lisp_function( mul, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(res) = any

	res = _NEW( OBJECT_TYPE_INTEGER )

	do
		p1 = _EVAL(_CAR(p))
		if( _IS_NUMBER(p1) ) then
			if(p = args) then
				res->dtype = p1->dtype
				res->value = p1->value
			else
				*res *= *p1
			end if
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			exit do
		end if
		p = _CDR(p)
	loop while( p <> _NIL_ )
	
	function = res

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (/ expr... )
''
define_lisp_function( div, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(res) = any

	res = _NEW( OBJECT_TYPE_INTEGER )

	do
		p1 = _EVAL(_CAR(p))
		if( _IS_NUMBER(p1) ) then
			if(p = args) then
				res->dtype = p1->dtype
				res->value = p1->value
			else
				if( p1->IsZero() ) then
					_RAISEERROR( LISP_ERR_DIVISION_BY_ZERO )
				else
					*res /= *p1
				end if
			end if
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			exit do
		end if
		p = _CDR(p)
	loop while( p <> _NIL_ )
	
	function = res

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (abs number)
''
define_lisp_function( abs, args )

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else
		_OBJ(p) = _EVAL(_CAR(args))
		if( _IS_NUMBER(p) ) then
			function = _NEW_REAL( p->abs_number() )
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			function = _NIL_
		end if
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (and expr...)
''
define_lisp_function( and, args)

	_OBJ(p) = args
	_OBJ(p1) = any

	function = _NIL_

	do
		p1 = _EVAL(_CAR(p))
		if( p1 = _NIL_ ) then
			exit function
		end if
		p = _CDR(p)
	loop while ( p <> _NIL_ )

	function = _T_

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (or expr...)
''
define_lisp_function( or, args)

	_OBJ(p) = args
	_OBJ(p1) = any

	function = _NIL_

	do
		p1 = _EVAL(_CAR(p))
		if( p1 <> _NIL_ ) then
			function = _T_
			exit function
		end if
		p = _CDR(p)
	loop while ( p <> _NIL_ )

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (not expr)
'' (null expr)  !!! FIXME: does (null ...) belong here?
''
define_lisp_function( not, args)

	_OBJ(p) = _EVAL(_CAR(args))

	if( p <> _NIL_ ) then
		function = _NIL_
	else
		function = _T_
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (~ <number>)
''
define_lisp_function( bitnot, args)

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else
		_OBJ(p) = _EVAL(_CAR(args))
		if( _IS_INTEGER(p) ) then
			function = _NEW_INTEGER( not p->value.int )
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			function = _NIL_
		end if
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (1+ <number>)
''
define_lisp_function( incr, args)

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else
		_OBJ(p) = _EVAL(_CAR(args))
		if( _IS_INTEGER(p) ) then
			function = _NEW_INTEGER( p->value.int + 1 )
		elseif( _IS_REAL(p) ) then
			function = _NEW_REAL( p->value.flt + 1.0 )
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			function = _NIL_
		end if
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (1- <number>)
''
define_lisp_function( decr, args)

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else
		_OBJ(p) = _EVAL(_CAR(args))
		if( _IS_INTEGER(p) ) then
			function = _NEW_INTEGER( p->value.int - 1 )
		elseif( _IS_REAL(p) ) then
			function = _NEW_REAL( p->value.flt - 1.0 )
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			function = _NIL_
		end if
	end if

end_lisp_function()

end namespace
