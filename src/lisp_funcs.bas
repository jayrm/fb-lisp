/'
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
#include once "lisp_object.bi"
#include once "lisp_objects.bi"
#include once "lisp_eval.bi"

#include once "lisp_funcs.bi"

namespace LISP

'' ---------------------------------------------------------------------------
'' FUNCTION DEFINITION
'' ---------------------------------------------------------------------------

type FUNCTION_DEF

	declare constructor()
	declare constructor( byval id as zstring ptr )
	declare destructor()
	
	id as zstring ptr
	func as LISP_FUNCTION
	nxt as FUNCTION_DEF ptr

end type

''
private constructor FUNCTION_DEF( )
	id = NULL
	func = NULL
	nxt = NULL
end constructor

''
private constructor FUNCTION_DEF( byval nameid as zstring ptr )
	id = lisp.strdup( nameid )
	func = NULL
	nxt = NULL
end constructor

''
private destructor FUNCTION_DEF()
	lisp.strdel( id )
end destructor

'' ---------------------------------------------------------------------------
'' FUNCTIONS_CTX
'' ---------------------------------------------------------------------------

type LISP_FUNCTIONS_CTX

	declare constructor( )
	declare constructor( byval parent_ctx as LISP_CTX ptr )
	declare destructor( )

	parent as LISP_CTX ptr 
	objects as LISP_OBJECTS ptr		'' alias for parent->objects
	functions as FUNCTION_DEF ptr

	declare function find( byval id as zstring ptr ) as FUNCTION_DEF ptr
	declare function bind( byval id as zstring ptr, byval func as LISP_FUNCTION ) as integer

end type

''
private constructor LISP_FUNCTIONS_CTX( byval parent_ctx as LISP_CTX ptr )

	parent = parent_ctx

	objects = parent_ctx->objects
	functions = NULL

end constructor

''
private destructor LISP_FUNCTIONS_CTX( )

	parent = NULL
	objects = NULL

	'' delete list of bound functions
	while( functions )
		dim f as FUNCTION_DEF ptr = functions->nxt
		delete functions
		functions = f
	wend

end destructor

''
private function LISP_FUNCTIONS_CTX.find( byval id as zstring ptr ) as FUNCTION_DEF ptr
	
	dim f as FUNCTION_DEF ptr = any

	function = NULL

	f = functions
	while( f )
		if( *id = *f->id ) then
			function = f
			exit function
		end if
		f = f->nxt
	wend 
	
end function

''
private function LISP_FUNCTIONS_CTX.bind( byval id as zstring ptr, byval func as LISP_FUNCTION ) as integer
	
	dim f as FUNCTION_DEF ptr = any 

	function = FALSE

	if( id = NULL ) then
		exit function
	elseif ( len(*id) = 0 ) then
		exit function
	end if

	f = find( id )

	if( f = NULL ) then
		f = new FUNCTION_DEF( id )
		f->nxt = functions
		functions = f
	end if
	
	f->func = func

	function = TRUE

end function

'' ---------------------------------------------------------------------------
'' FUNCTIONS
'' ---------------------------------------------------------------------------

''
constructor LISP_FUNCTIONS( byval parent_ctx as LISP_CTX ptr )
	ctx = new LISP_FUNCTIONS_CTX( parent_ctx )
end constructor

''
destructor LISP_FUNCTIONS()
	delete ctx
end destructor

''
function LISP_FUNCTIONS.bind( byval id as zstring ptr, byval func as LISP_FUNCTION ) as integer
	function = ctx->bind( id, func )
end function

''
function LISP_FUNCTIONS.find( byval id as zstring ptr ) as LISP_FUNCTION
	dim f as FUNCTION_DEF ptr = any 
	f = ctx->find( id )
	if( f ) then
		function = f->func
	else
		function = NULL
	end if
end function

end namespace