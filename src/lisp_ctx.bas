/'
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

#include once "lisp_int.bi"
#include once "lisp_err.bi"
#include once "lisp_ctx.bi"
#include once "lisp_lexer.bi"
#include once "lisp_object.bi"
#include once "lisp_objects.bi"
#include once "lisp_parser.bi"
#include once "lisp_funcs.bi"
#include once "lisp_eval.bi"

namespace LISP

'' from "lisp_funcs*.bas"
declare sub bind_intrinsic_funcs1( byval ctx as LISP_FUNCTIONS ptr )
declare sub bind_intrinsic_funcs2( byval ctx as LISP_FUNCTIONS ptr )
declare sub bind_intrinsic_funcs3( byval ctx as LISP_FUNCTIONS ptr )

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

	bind_intrinsic_funcs1( functions )
	bind_intrinsic_funcs2( functions )
	bind_intrinsic_funcs3( functions )

	ErrorText = ""
	ErrorLine = 0
	ErrorColumn = 0
	ErrorCode = 0

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
sub LISP_CTX.RaiseError( byval e_code as LISP_ERROR, byref e_text as string )

	'' FIXME: only take the first error, or generate a list of errors

	ErrorCode = e_code
	ErrorLine = lexer->lineno + 1
	ErrorColumn = lexer->column + 1
	ErrorText = e_text

end sub

''
sub LISP_CTX.RaiseWarning( byval e_code as LISP_ERROR, byref e_text as string )

	'' FIXME: allow warnings to be ignored

	ErrorCode = e_code
	ErrorLine = lexer->lineno + 1
	ErrorColumn = lexer->column + 1
	ErrorText = e_text

end sub

end namespace
