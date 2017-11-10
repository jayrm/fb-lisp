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

import_lisp_function( car, args )
import_lisp_function( cdr, args )
import_lisp_function( cons, args )
import_lisp_function( list, args )

import_lisp_function( append, args )
import_lisp_function( length, args )
import_lisp_function( nth, args )
import_lisp_function( elt, args )
import_lisp_function( last, args )

'' ---------------------------------------------------------------------------
''
sub bind_runtime_list( byval functions as LISP_FUNCTIONS ptr )

	BIND_FUNC( functions, "car", car )          '' list (tests/list.lsp)
	BIND_FUNC( functions, "cdr", cdr)           '' list (tests/list.lsp)
  	BIND_FUNC( functions, "cons", cons )        '' list (tests/list.lsp)
	BIND_FUNC( functions, "list", list )        '' list (tests/list.lsp)

	BIND_FUNC( functions, "append", append )    '' list (tests/list.lsp)
	BIND_FUNC( functions, "length", length )    '' list (tests/list.lsp)
	BIND_FUNC( functions, "nth", nth )          '' list (tests/list.lsp)
	BIND_FUNC( functions, "elt", elt )          '' list (tests/list.lsp)
	BIND_FUNC( functions, "last", last )        '' list (tests/list.lsp)

end sub

'' ---------------------------------------------------------------------------
'' lisp-syntax: (car <list>)
'' lisp-syntax: (car <cons>)
''
define_lisp_function( car, args )

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
		exit function
	end if

	function = _CAR( _EVAL( _CAR( args ) ) )

end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (cdr <list>)
'' lisp-syntax: (cdr <cons>)
''
define_lisp_function( cdr, args )

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
		exit function
	end if

	function = _CDR( _EVAL( _CAR( args )))

end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (cons <expr1> expr2)
''
define_lisp_function( cons, args )

	_OBJ(p) = any

	if( _LENGTH(args) < 1 ) then
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )
		function = _NIL_
		exit function
	end if

	p = _NEW( OBJECT_TYPE_CONS )

	p->value.cell.car = _EVAL(_CAR(args))
	p->value.cell.cdr = _EVAL(_CAR(_CDR(args)))

	function = p

end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (list expr...)
''
define_lisp_function( list, args )

	_OBJ(p) = args
	_OBJ(first) = NULL
	_OBJ(prev) = NULL
	_OBJ(p1)

	if( p = _NIL_ ) then
		function = _NIL_
		exit function
	end if

	do
		p1 = _NEW( OBJECT_TYPE_CONS )
		p1->value.cell.car = _EVAL(_CAR(p))
		if( first = NULL ) then
			first = p1
		end if
		if( prev <> NULL ) then
			prev->value.cell.cdr = p1
		end if
		prev = p1
		p = _CDR(p)
	loop while (p <> _NIL_ )

	if( first = NULL ) then
		function = _NIL_
	else
		function = first
	end if
end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (append <expr>...)
''
define_lisp_function( append, args )

	'' !!! FIXME: first arg must be list
	'' !!! FIXME: all but last arg must be a list

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(first) = NULL
	_OBJ(prev) = NULL

	if( p = _NIL_ ) then
		'' no arguments so return nil
		function = _NIL_
		exit function
	end if

	'' iterate through the arguments
	while( p <> _NIL_ )

		'' current argument
		p1 = _EVAL(_CAR(p))

		'' iterate through the elements of the current list
		_OBJ(e) = p1
		_OBJ(e1) = any

		while( e <> _NIL_ )
			'' add elements to the list
			e1 = _NEW( OBJECT_TYPE_CONS )
			if( _IS_CONS(e) ) then
				e1->value.cell.car = _COPY(_CAR(e))
			else
				e1->value.cell.car = _COPY(e)
			end if

			if( first = NULL ) then
				first = e1
			end if
			if( prev <> NULL ) then
				prev->value.cell.cdr = e1
			end if
			prev = e1

			'' next element
			if( _IS_CONS(e) ) then
				e = _CDR(e)
			else
				e = p1
				exit while
			end if
		wend

		'' next argument
		p = _CDR(p)
	wend

	if( first = NULL ) then
		first = _NIL_
	else
		function = first
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (length <list>)
''
define_lisp_function( length, args )

	_OBJ(p) = args
	_OBJ(p1) = any

	dim count as integer = 0

	'' no arguments?
	if( p = _NIL_ ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
		exit function
	end if

	'' more than one argument?
	p1 = _CDR(args)
	if( p1 <> _NIL_ ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
		exit function
	end if

	'' first argument
	p = _EVAL(_CAR(p))

	'' not nil?
	if( p <> _NIL_ ) then

		'' not a list?
		if( _IS_CONS( p ) = FALSE ) then
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			function = _NIL_
			exit function
		end if

		'' count the elements
		while( _IS_CONS( p ) )
			count += 1
			p = _CDR(p)
		wend
	
	end if

	function = _NEW_INTEGER( count )

end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (nth <index> <list>)
''
define_lisp_function( nth, args )

	if( _LENGTH(args) <> 2 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
		exit function
	end if

	_OBJ(p1) = _EVAL(_CAR(args))
	_OBJ(p2) = _EVAL(_CAR(_CDR(args)))

	if( _IS_INTEGER(p1) ) then
		dim i as integer = p1->value.int
		if( i < 0 ) then
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
		else
			if( p2 = _NIL_ ) then
				function = _NIL_
			elseif( _IS_CONS(p2) ) then
				while( p2 <> _NIL_ and i > 0 )
					p2 = _CDR(p2)
					i -= 1
				wend
				if( p2 <> _NIL_ ) then
					function = _CAR(p2)
				else
					function = _NIL_
				end if
			else
				_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
				function = _NIL_
			end if
		end if
	else
		_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
		function = _NIL_
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (elt <list> <index>)
''
define_lisp_function( elt, args )

	if( _LENGTH(args) <> 2 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
		exit function
	end if

	_OBJ(p2) = _EVAL(_CAR(args))
	_OBJ(p1) = _EVAL(_CAR(_CDR(args)))

	if( _IS_INTEGER(p1) ) then
		dim i as integer = p1->value.int
		if( i < 0 ) then
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
		else
			if( p2 = _NIL_ ) then
				function = _NIL_
			elseif( _IS_CONS(p2) ) then
				while( p2 <> _NIL_ and i > 0 )
					p2 = _CDR(p2)
					i -= 1
				wend
				if( p2 <> _NIL_ ) then
					function = _CAR(p2)
				else
					function = _NIL_
				end if
			else
				_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
				function = _NIL_
			end if
		end if
	else
		_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
		function = _NIL_
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (last <list>)
''
define_lisp_function( last, args )

	if( _LENGTH(args) <> 1 ) then
		function = _NIL_
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		exit function
	end if

	_OBJ(p) = _EVAL(_CAR(args))

	if( p = _NIL_ ) then
		function = _NIL_
	elseif( not _IS_CONS(p) ) then
		function = _NIL_
		_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
	else
		dim i as integer = _LENGTH(p) - 1
		if( i >= 0 ) then
			if( _IS_CONS(p) ) then
				while( p <> _NIL_ and i > 0 )
					p = _CDR(p)
					i -= 1
				wend
				if( p <> _NIL_ ) then
					function = p
				else
					function = _NIL_
				end if
			else
				_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
				function = _NIL_
			end if
		else
			function = _NIL_
		end if
	end if

end_lisp_function()

end namespace
