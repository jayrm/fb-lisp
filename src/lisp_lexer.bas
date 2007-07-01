/'
 * 
 * Copyright (c) 2007 Jeffery R. Marshall.  All rights reserved.
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

''
#define LEX_CHAR_EOF -1

'' ---------------------------------------------------------------------------
'' LEXER_CTX
'' ---------------------------------------------------------------------------

type LISP_LEXER_CTX

	DECLARE_DEBUG_ALLOCATOR()

	declare constructor( )
	declare constructor( byval parent_ctx as LISP_CTX ptr )
	declare destructor( )

	parent as LISP_CTX ptr

	buffer as string		'' The buffer to lex/parse
	lineno as integer		'' current line number
	column as integer		'' current column number
	index0 as integer		'' last marked index
	index1 as integer		'' current index in the buffer

	token_id as LISP_TOKEN_ID    '' id of last token found
	token as string			     '' text of last token found

	declare sub settext( byref buffer as string ) 
	declare function gettoken( ) as LISP_TOKEN_ID

	declare function getchar( ) as integer
	declare function peekchar( ) as integer
	declare function peekchar( byval index as integer ) as integer
	declare function getcomment( ) as LISP_TOKEN_ID
	declare function getidentifier( ) as LISP_TOKEN_ID
	declare function getnumber( ) as LISP_TOKEN_ID
	declare function getstring( ) as LISP_TOKEN_ID

end type

DEFINE_DEBUG_ALLOCATOR( LISP_LEXER_CTX )

''
private constructor LISP_LEXER_CTX( byval parent_ctx as LISP_CTX ptr )

	parent = parent_ctx

	buffer = ""
	lineno = 0
	column = 0
	index0 = 0
	index1 = 0

	token_id = 0
	token = ""

end constructor

''
private destructor LISP_LEXER_CTX( )

	buffer = ""
	token = ""

end destructor

''
private function LISP_LEXER_CTX.getchar() as integer
	if( index1 < len(buffer ) ) then
		function = buffer[index1]
		index1 += 1
		column += 1
	else
		function = LEX_CHAR_EOF
	end if
end function

''
private function LISP_LEXER_CTX.peekchar() as integer
	if( index1 < len(buffer ) ) then
		function = buffer[index1]
	else
		function = LEX_CHAR_EOF
	end if
end function

''
private function LISP_LEXER_CTX.peekchar( byval index as integer) as integer
	if( index1 + index < len(buffer) ) then
		function = buffer[index1 + index]
	else
		function = LEX_CHAR_EOF
	end if
end function

''
private function LISP_LEXER_CTX.getcomment() as LISP_TOKEN_ID
	dim c as integer = any

	'' ';'?
	c = getchar()

	do
		c = getchar()
		select case c
		case 13
			lineno += 1
			column = 0
			exit do
		case LEX_CHAR_EOF
			exit do
		end select
	loop

	token = mid( buffer, index0 + 1, index1 - index0 )
	function = LISP_TK_COMMENT

end function

''
private function LISP_LEXER_CTX.getnumber() as LISP_TOKEN_ID

	dim as integer c = any

	function = LISP_TK_INVALID

	c = peekchar()

	'' '+', '-'
	if( c = 43 or c = 45 ) then
		c = getchar()
		c = peekchar()
	end if

	'' [0-9]
	while( c >= 48 and c <= 57 )
		c = getchar()
		c = peekchar()
	wend

	'' '.'
	if( c = 46 ) then

		c = getchar()
		c = peekchar()

		'' [0-9]
		while( c >= 48 and c <= 57 )
			c = getchar()
			c = peekchar()
		wend

		'' [DEde]
		if( c = 68 or c = 69 or c = 100 or c = 101 ) then
			c = getchar()	'' [DEde]
			c = peekchar()
		end if

		'' '+', '-'		
		if( c = 43 or c = 45 ) then
			c = getchar()
			c = peekchar()
		end if

		'' [0-9]
		while( c >= 48 and c <= 57 )
			c = getchar()
			c = peekchar()
		wend

		function = LISP_TK_REAL

	else
		function = LISP_TK_INTEGER

	end if

	token = mid( buffer, index0 + 1, index1 - index0 )

end function

''
private function LISP_LEXER_CTX.getidentifier( ) as LISP_TOKEN_ID

	dim c as integer = any

	do
		c = peekchar()
		select case c

		'' [-/+*%<>=&A-Za-z_0-9]
		case 45, 47, 43, 42, 37, 60, 62, 61, 38, 65 to 90, 97 to 122, 95, 48 to 57
			c = getchar()
		case else
			exit do
		end select
	loop

	token = mid( buffer, index0 + 1, index1 - index0 )
	function = LISP_TK_IDENTIFIER

end function

''
private function LISP_LEXER_CTX.getstring( ) as LISP_TOKEN_ID

	dim c as integer = any

	'' '"'
	c = getchar()

	do
		c = getchar()

		select case c
		
		'' '\'
		case 92
			c = getchar()
			select case c
			case 97  '' a => alert
				token &= chr(7)

			case 98  '' b => backspace
				token &= chr(8)

			case 101 '' e => escape
				token &= chr(27)

			case 102 '' f => form feed
				token &= chr(12)

			case 110 '' n => line feed
				token &= chr(10)

			case 114 '' r => carriage return
				token &= chr(13)

			case 116 '' t => horizontal tab
				token &= chr(9)

			case 118 '' v => vertical tab
				token &= chr(11)

			case else
				token &= chr(c)

			end select

		case 13
			token &= chr(c)
			lineno += 1
			column = 0

		case 34
			exit do

		case LEX_CHAR_EOF
			exit do

		case else
			token &= chr(c)

		end select

	loop

	function = LISP_TK_STRING
	
end function

''
private function LISP_LEXER_CTX.gettoken( ) as LISP_TOKEN_ID

	dim as integer c
	dim as LISP_TOKEN_ID tk

	function = LISP_TK_EOF

	token = ""

	do
		index0 = index1
		c = peekchar()
	
		select case c

		case LEX_CHAR_EOF
			exit do

		'' '\n'
		case 13
			c = getchar()
			lineno += 1
			column = 0

		'' ' ', '\f', '\t', '\v', '\r'
		case 32, 12, 9, 11, 10
			c = getchar()

		'' ';' comment
		case 59
			function = getcomment()

		'' '?'
		case 63 
			c = getchar()
			c = getchar()
			token = ltrim(str(c))
			function = LISP_TK_INTEGER
			exit do

		'' '-'
		case 45 
			c = peekchar(1)
			select case c
			case 48 to 57 '' '0'-'9'
				function = getnumber()
				exit do

			case else
				function = getidentifier()
				exit do

			end select

		'' [0-9]
		case 48 to 57
			function = getnumber()
			exit do

		'' [-/+*%<>=&A-Za-z_]
		case 45, 47, 43, 42, 37, 60, 62, 61, 38, 65 to 90, 97 to 122, 95
			function = getidentifier()
			exit do
		
		'' single_quote, '(', ')', '.'
		case 39, 40, 41, 46

			index0 = index1
			c = getchar()
			token = chr( c )
			select case c
			case 39
				function = LISP_TK_SINGLE_QUOTE
			case 40
				function = LISP_TK_LEFT_PARA
			case 41
				function = LISP_TK_RIGHT_PARA
			case 46
				function = LISP_TK_DOT
			end select
			
			exit do

		case 34 '' '\"'
			function = getstring()
			exit do

		case else
			function = LISP_TK_CHAR
			token = chr(c)
			exit do

		end select

	loop

end function


'' ---------------------------------------------------------------------------
'' LEXER
'' ---------------------------------------------------------------------------

DEFINE_DEBUG_ALLOCATOR( LISP_LEXER )

''
constructor LISP_LEXER( byval parent_ctx as LISP_CTX ptr )
	ctx = new LISP_LEXER_CTX( parent_ctx )
end constructor

''
destructor LISP_LEXER( )
	delete ctx
end destructor

''
sub LISP_LEXER.settext( byref text as string ) 

	ctx->buffer = text
	ctx->index0 = 0
	ctx->index1 = 0

	ctx->token = ""

end sub

''
function LISP_LEXER.gettoken( ) as LISP_TOKEN_ID
	function = ctx->gettoken()
end function

''
function LISP_LEXER.token() as zstring ptr
	function = strptr( ctx->token )
end function

function LISP_LEXER.lineno() as integer
	function = ctx->lineno
end function

function LISP_LEXER.column() as integer
	function = ctx->column
end function
