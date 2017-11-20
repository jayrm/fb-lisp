/'
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

#include once "lisp_int.bi"
#include once "lisp_err.bi"
#include once "lisp_ctx.bi"
#include once "lisp_lexer.bi"
#include once "lisp_object.bi"
#include once "lisp_objects.bi"
#include once "lisp_parser.bi"
#include once "lisp_funcs.bi"
#include once "lisp_eval.bi"

#include once "lisp_runtime.bi"

namespace LISP

'' !!! FIXME: this requires that console lib be linked
declare function dump_helper( byval ctx as LISP_CTX ptr, byval args as LISP_OBJECT ptr, byval indent as integer ) as LISP_OBJECT ptr
'' !!!

'' ---------------------------------------------------------------------------
'' ERROR MESSAGES
'' ---------------------------------------------------------------------------

dim shared ErrMessages( 0 to LISP_ERRS - 1 ) as zstring ptr = { _
	@"OK", _
	@"Function not defined", _
	@"Invalid argument", _
	@"Setting value of NIL object", _
	@"Getting CDR of non CONS", _
	@"Getting CAR of non CONS", _
	@"Unexpected '.'", _
	@"Unexpected ')'", _
	@"Expected ')'", _
	@"Unexpected token", _
	@"Wrong number of arguments", _
	@"Division by zero", _
	@"Argument type mismatch", _
	@"Too few arguments", _
	@"Unable to redefine built-in function", _
	@"File not found", _
	@"I/O error" _
}

'' ---------------------------------------------------------------------------
'' LISP EXECUTION CONTEXT
'' ---------------------------------------------------------------------------

''
constructor LISP_CTX()

	lexer = new LISP_LEXER( @this )
	objects = new LISP_OBJECTS( @this )
	parser = new LISP_PARSER( @this )
	functions = new LISP_FUNCTIONS( @this )
	evaluator = new LISP_EVAL( @this )

	bind_runtime_console( functions )
	bind_runtime_data( functions )
	bind_runtime_list( functions )
	bind_runtime_math( functions )
	bind_runtime_prog( functions )
	bind_runtime_system( functions )

	ErrorCode = 0
	ErrorText = ""
	ErrorFile = ""
	ErrorLine = 0
	ErrorColumn = 0

end constructor

''
destructor LISP_CTX()

	ErrorText = ""

	delete evaluator
	delete functions
	delete parser
	delete objects
	delete lexer

end destructor

''
function LISP_CTX.GetErrorText() as string
	dim res as string

	if( ErrorCode >= 0 and ErrorCode < LISP_ERRS ) then
		res = *ErrMessages( ErrorCode )
	else
		res = "Unknown error"
	end if
	if( ErrorText > "" ) then
		res &= ", '" & ErrorText & "'."
	else
		res &= "."
	end if

	function = res
end function

''
sub LISP_CTX.ResetError( )
	ErrorCode = 0
	ErrorText = ""
	ErrorFile = ""
	ErrorLine = 0
	ErrorColumn = 0
end sub

''
sub LISP_CTX.RaiseError( byval e_code as LISP_ERROR, byref e_text as const string )

	'' !!! FIXME: only take the first error, or generate a list of errors

	ErrorCode = e_code
	ErrorText = e_text
	ErrorFile = lexer->filename
	ErrorLine = lexer->lineno + 1
	ErrorColumn = lexer->column + 1

end sub

''
sub LISP_CTX.RaiseWarning( byval e_code as LISP_ERROR, byref e_text as const string )

	'' !!! FIXME: allow warnings to be ignored

	ErrorCode = e_code
	ErrorText = e_text
	ErrorFile = lexer->filename
	ErrorLine = lexer->lineno + 1
	ErrorColumn = lexer->column + 1
	
end sub

''
sub LISP_CTX.PrintOut( byref s as const string )
	if( print_cb ) then
		print_cb(s)
	else
		print s;
	end if
end sub

''
function LISP_CTX.Dump( byval args as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	function = dump_helper( @this, args, 0 )
end function

'' requires (princ-object ...)
''
function LISP_CTX.Eval( byref text as const string ) as LISP_ERROR

	dim p1 as LISP_OBJECT ptr
	dim p2 as LISP_OBJECT ptr

	'' !!! FIXME: echo and show results should be set by system vars

	ResetError( )

	lexer->settext( text )

	do
		p1 = parser->parse( )

		if( p1 = NULL ) then
			exit do
		end if

		if( ErrorCode ) then
			exit do
		end if

		if( EchoInput ) then
			PrintOut( "<<= " )
			evaluator->call_by_name( "princ-object", p1 )
			PrintOut( !"\n" )
		end if

		p2 = evaluator->eval( p1 )

		if( ErrorCode ) then
			exit do
		end if

		if( ShowResults ) then
			PrintOut( "==> " )
			evaluator->call_by_name( "princ-object", p2 )
			PrintOut( !"\n" )
		end if

		GarbageCollect()

	loop

	function = ErrorCode

end function

''
function LISP_CTX.Load( byref filename as const string ) as LISP_ERROR

	dim h as integer = freefile
	dim x as string
	if( open( filename for input access read as #h ) = 0 ) then
		close #h
		if( open( filename for binary access read as #h ) = 0 ) then
			x = space( lof( h ))
			get #h,,x
			close #h
			lexer->push( filename )
			function = Eval( x )
			lexer->pop()
		else
			RaiseError( LISP_ERR_IO_ERROR, filename )
			function = LISP_ERR_IO_ERROR
		end if
	else
		RaiseError( LISP_ERR_FILE_NOT_FOUND, filename )
		function = LISP_ERR_FILE_NOT_FOUND
	end if

end function

''
sub LISP_CTX.GarbageCollect( )
	objects->garbage_collect()
end sub

end namespace
