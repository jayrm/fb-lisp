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

import_lisp_function( gc, args )
import_lisp_function( mem, args )
import_lisp_function( load, args )
import_lisp_function( read, args )
import_lisp_function( lineno, args )

'' ---------------------------------------------------------------------------
''
sub bind_runtime_system( byval functions as LISP_FUNCTIONS ptr )

	BIND_FUNC( functions, "gc", gc )                '' system
	BIND_FUNC( functions, "garbage-collect", gc )   '' system
	BIND_FUNC( functions, "mem", mem )              '' system
	BIND_FUNC( functions, "load", load )            '' system
	BIND_FUNC( functions, "read", read )            '' system
	BIND_FUNC( functions, "lineno", lineno )        '' system

end sub

'' ---------------------------------------------------------------------------
'' (gc)
'' (garbage-collect)
''
define_lisp_function( gc, args )

	function = ctx->objects->garbage_collect()

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (mem)
''
define_lisp_function( mem, args )

	_OBJ(r) = any
	r = _CONS( _NEW_INTEGER( ctx->objects->mem_used() ), _
	    _CONS( _NEW_INTEGER( ctx->objects->mem_free() ), _NIL_ ) _
		)

	function = r

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (getsymbols)
''
define_lisp_function( getsymbols, args )

	_OBJ(r) = any
	
	r = _CONS( _NEW_INTEGER( ctx->objects->mem_used() ), _
	    _CONS( _NEW_INTEGER( ctx->objects->mem_free() ), _NIL_ ) _
		)

	function = r

end_lisp_function()

'' ---------------------------------------------------------------------------
''
'' requires (princ-object ...)
''
function eval_text( byval ctx as LISP_CTX ptr, byref text as const string, byref filename as const string ) as LISP_OBJECT ptr


	'' !!! FIXME: this needs to move to evaluator?!!
	'' !!! FIXME: echo and show results should be system variable

	dim p1 as LISP_OBJECT ptr
	dim p2 as LISP_OBJECT ptr

	ctx->lexer->push( filename )
	ctx->lexer->settext( text )

	p2 = _NIL_

	do
		p1 = ctx->parser->parse( )

		if( p1 = NULL ) then
			exit do
		end if

		if( ctx->ErrorCode ) then
			exit do
		end if

		if( ctx->EchoInput ) then
			ctx->PrintOut( "<<=== " )
			ctx->evaluator->call_by_name( "princ-object", p1 )
			ctx->PrintOut( !"\n" )
		end if

		p2 = ctx->evaluator->eval( p1 )

		if( ctx->ErrorCode ) then
			exit do
		end if

		if( ctx->ShowResults ) then
			ctx->PrintOut( "====> " )
			ctx->evaluator->call_by_name( "princ-object", p2 )
			ctx->PrintOut( !"\n" )
		end if

	loop

	ctx->lexer->pop()

	function = p2

end function

'' ---------------------------------------------------------------------------
'' (load "filename")
''
define_lisp_function( load, args )

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
	else
		_OBJ(p) = _EVAL(_CAR(args))
		if( _IS_STRING(p) ) then

			dim filename as string

			filename = *p->value.str

			dim h as integer = freefile
			dim text as string

			if( open( filename for input access read as #h ) = 0 ) then
				close #h
				if( open( filename for binary access read as #h ) = 0 ) then
					text = space( lof( h ))
					get #h,,text
					close #h

					function = eval_text( ctx, text, filename )

				else
					_RAISEERROR( LISP_ERR_IO_ERROR )
					function = _NIL_
					exit function
				end if

			else

				_RAISEERROR( LISP_ERR_FILE_NOT_FOUND )
				function = _NIL_
				exit function

			end if

			exit function
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
		end if
	end if

	function = _NIL_

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (read string)
''
define_lisp_function( read, args )

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
	else
		_OBJ(p) = _EVAL(_CAR(args))
		if( _IS_STRING(p) ) then
			dim text as string
			text = *p->value.str
			function = eval_text( ctx, text, "" )
			exit function
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
		end if
	end if

	function = _NIL_

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (lineno)
''
define_lisp_function( lineno, args )

	if( _LENGTH(args) <> 0 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else
		function = _NEW_INTEGER( ctx->lexer->lineno() )
	end if

end_lisp_function()

end namespace
