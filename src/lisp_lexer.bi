#ifndef __LISP_LEXER_BI__
#define __LISP_LEXER_BI__

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

namespace LISP

	''
	enum LISP_TOKEN_ID
		LISP_TK_INVALID
		LISP_TK_COMMENT
		LISP_TK_INTEGER
		LISP_TK_REAL
		LISP_TK_STRING
		LISP_TK_IDENTIFIER

		LISP_TK_LEFT_PARA
		LISP_TK_RIGHT_PARA
		LISP_TK_DOT
		LISP_TK_SINGLE_QUOTE
		LISP_TK_CHAR

		LISP_TK_EOF
	end enum

	''
	type LISP_LEXER_CTX_ as LISP_LEXER_CTX

	''
	type LISP_LEXER

		declare constructor( byval parent_ctx as LISP_CTX ptr )
		declare destructor( )

	public:
		declare sub settext( byref buffer as const string ) 
		declare sub setfile( byref f as const string )
		declare function gettoken( ) as LISP_TOKEN_ID
		declare function token() as zstring ptr
		
		declare function filename() as string
		declare function lineno() as integer
		declare function column() as integer

		declare sub push( byref f as const string )
		declare sub pop()

	private:
		ctx as LISP_LEXER_CTX_ ptr

	end type

end namespace

#endif
