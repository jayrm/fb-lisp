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

#include once "lisp_int.bi"
#include once "lisp_err.bi"
#include once "lisp_ctx.bi"
#include once "lisp_lexer.bi"
#include once "lisp_object.bi"
#include once "lisp_objects.bi"
#include once "lisp_parser.bi"
#include once "lisp_funcs.bi"

namespace LISP

#define _NIL_ parent->objects->NIL_
#define _T_   parent->objects->T_

'' ---------------------------------------------------------------------------
'' PARSER_CTX
'' ---------------------------------------------------------------------------

''
type LISP_PARSER_CTX

	declare constructor( )
	declare constructor( byval parent_ctx as LISP_CTX ptr )
	declare destructor( )

	parent as LISP_CTX ptr

	lexer as LISP_LEXER ptr
	objects as LISP_OBJECTS ptr
	functions as LISP_FUNCTIONS ptr

	token_id as LISP_TOKEN_ID

	declare function parse_form() as LISP_OBJECT ptr
	declare function parse_object( byval havetoken as integer ) as LISP_OBJECT ptr
	declare function parse_quote() as LISP_OBJECT ptr

end type

''
private constructor LISP_PARSER_CTX( byval parent_ctx as LISP_CTX ptr )

	parent = parent_ctx

	lexer = parent_ctx->lexer
	objects = parent_ctx->objects
	functions = parent_ctx->functions

	token_id = 0

end constructor

''
private destructor LISP_PARSER_CTX( )

	parent = NULL

	lexer = NULL
	objects = NULL
	functions = NULL

end destructor

''
private function LISP_PARSER_CTX.parse_form() as LISP_OBJECT ptr

	dim as LISP_OBJECT ptr p = any
	dim as LISP_OBJECT ptr first = NULL
	dim as LISP_OBJECT ptr prev = NULL

	do
		token_id = lexer->gettoken()

		select case token_id
		case LISP_TK_EOF
			parent->RaiseWarning( LISP_ERR_EXPECTED_RIGHT_PARA )
			exit do

		case LISP_TK_RIGHT_PARA
			exit do

		case LISP_TK_DOT
			token_id = lexer->gettoken()

			if( prev = NULL ) then
				parent->RaiseError( LISP_ERR_UNEXPECTED_DOT )
				exit do
			end if

			prev->value.cell.cdr = parse_object( TRUE )

			token_id = lexer->gettoken()

			if( token_id <> LISP_TK_RIGHT_PARA ) then
				parent->RaiseError( LISP_ERR_UNEXPECTED_RIGHT_PARA )
				exit do
			end if

			exit do

		end select

		p = objects->new_object( OBJECT_TYPE_CONS )

		if( first = NULL ) then
			first = p
		end if

		if( prev <> NULL ) then
			prev->value.cell.cdr = p
		end if

		p->value.cell.car = parse_object( TRUE )

		prev = p

	loop

	if( first = NULL ) then
		function = _NIL_
	else
		function = first
	end if

end function

''
private function LISP_PARSER_CTX.parse_quote() as LISP_OBJECT ptr

	dim as LISP_OBJECT ptr p = any

	p = objects->new_object( OBJECT_TYPE_CONS )
	
	p->value.cell.car = objects->new_object( OBJECT_TYPE_IDENTIFIER )
	p->value.cell.car->value.id = lisp.strdup( "quote" )
	p->value.cell.cdr = objects->new_object( OBJECT_TYPE_CONS )
	p->value.cell.cdr->value.cell.car = parse_object( FALSE )

	function = p

end function

''
private function LISP_PARSER_CTX.parse_object( byval havetoken as integer ) as LISP_OBJECT ptr

	dim as LISP_OBJECT ptr p = NULL
	dim as LISP_INTEGER i = any
	dim as LISP_REAL r = any

	if( havetoken = FALSE ) then
		token_id = lexer->gettoken()
	end if

	select case token_id
	case LISP_TK_EOF
		''

	case LISP_TK_COMMENT
		''

	case LISP_TK_LEFT_PARA
		p = parse_form()

	case LISP_TK_SINGLE_QUOTE
		p = parse_quote()

	case LISP_TK_IDENTIFIER
		
		if( lcase(*lexer->token) = "t" ) then
			p = _T_

		elseif( lcase(*lexer->token) = "nil" ) then
			p = _NIL_

		else
			p = objects->find_identifier(lexer->token)
			if( p = NULL ) then
				p = objects->new_object( OBJECT_TYPE_IDENTIFIER )
				p->value.id = lisp.strdup(lexer->token)
			end if

		end if

	case LISP_TK_INTEGER
		i = val( *lexer->token )

		p = objects->find_integer(i)
		if( p = NULL ) then
			p = objects->new_object( OBJECT_TYPE_INTEGER )
			p->value.int = i
		end if

	case LISP_TK_REAL
		r = val( *lexer->token )

		p = objects->find_real(r)
		if( p = NULL ) then
			p = objects->new_object( OBJECT_TYPE_REAL )
			p->value.flt = r
		end if
		
	case LISP_TK_STRING

		p = objects->find_string( lexer->token )
		if( p = NULL ) then
			p = objects->new_object( OBJECT_TYPE_STRING )
			p->value.str = lisp.strdup( lexer->token )
		end if

	case else
		parent->RaiseError( LISP_ERR_UNEXPECTED_TOKEN, *lexer->token )

	end select

	function = p

end function


'' ---------------------------------------------------------------------------
'' PARSER
'' ---------------------------------------------------------------------------

''
constructor LISP_PARSER( byval parent_ctx as LISP_CTX ptr )
	ctx = new LISP_PARSER_CTX( parent_ctx )
end constructor

''
destructor LISP_PARSER( )
	delete ctx
end destructor

''
function LISP_PARSER.parse( ) as LISP_OBJECT ptr
	function = ctx->parse_object( FALSE )
end function

end namespace
