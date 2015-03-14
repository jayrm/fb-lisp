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
#include once "lisp_object.bi"
#include once "lisp_objects.bi"
#include once "lisp_eval.bi"
#include once "lisp_funcs.bi"

namespace LISP

#define _NIL_ parent->objects->NIL_
#define _T_   parent->objects->T_

'' ---------------------------------------------------------------------------
'' EVAL_CTX
'' ---------------------------------------------------------------------------

''
type LISP_EVAL_CTX

	declare constructor( )
	declare constructor( byval parent_ctx as LISP_CTX ptr )
	declare destructor( )

	parent as LISP_CTX ptr				'' parent execution context

	objects as LISP_OBJECTS ptr			'' alias for parent->objects
	functions as LISP_FUNCTIONS ptr		'' alias for parent->functions
	
	'' built-ins
	declare function car( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	declare function cdr( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	declare function cons( byval a as LISP_OBJECT ptr, byval a as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	declare function list_length( byval p as LISP_OBJECT ptr ) as integer
	declare function copy( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr

	declare function progn( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	declare function eval_cons( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	declare function eval_func( byval p as LISP_OBJECT ptr, byval args as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	declare function eval( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	declare function call_by_name( byval nameid as zstring ptr, byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr

end type

''
private constructor LISP_EVAL_CTX( byval parent_ctx as LISP_CTX ptr )

	parent = parent_ctx

	objects = parent_ctx->objects
	functions = parent_ctx->functions

end constructor

''
private destructor LISP_EVAL_CTX( )

	parent = NULL

	objects = NULL
	functions = NULL

end destructor

''
private function LISP_EVAL_CTX.eval_func( byval p as LISP_OBJECT ptr, byval args as LISP_OBJECT ptr ) as LISP_OBJECT ptr

	dim as LISP_OBJECT ptr p1 = any, p2 = any, p3 = any, p4 = any, p5 = any
	dim as integer n = any, i = any
	dim as LISP_OBJECT ptr ptr save_objs = any, eval_objs = any
	
	p1 = car(p)					'' lambda (expected)
	p2 = car(cdr(p))			'' parameters
	p3 = args					'' arguments

	function = _NIL_

	if( p1 = _NIL_ ) then
		'' Error?
		exit function	
	end if

	if( p1->dtype <> OBJECT_TYPE_IDENTIFIER ) then
		'' Error?
		exit function
	end if

	if( *p1->value.id <> "lambda" ) then
		'' Error?
		exit function
	end if
	
	n = list_length( p2 )
	if( n <> list_length( p3 ) ) then
		parent->RaiseError( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		exit function
	end if

	if( n > 0 ) then
		eval_objs = new LISP_OBJECT ptr [n]
		save_objs = new LISP_OBJECT ptr [n]
	end if

	'' evaluate the arguments and save to a temporary list
	for i = 0 to n - 1
		p5 = eval(car(p3))
		eval_objs[i] = p5
		p3 = cdr(p3)
	next

	'' save the old variable values and bind arguments to parameters
	for i = 0 to n - 1
		p4 = car(p2)
		save_objs[i] = objects->get_object( p4 )
		objects->set_object( p4, eval_objs[i] )
		p2 = cdr(p2)
	next

	p4 = cdr(cdr(p))			'' function body
	p5 = progn( p4 )

	'' restore argument variables
	p2 = car(cdr(p))			'' parameters
	for i = 0 to n - 1
		p4 = car(p2)
		objects->set_object( p4, save_objs[i] )
		p2 = cdr(p2)
	next
	
	function = p5

end function

''
private function LISP_EVAL_CTX.eval_cons( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	
	dim as LISP_OBJECT ptr p1 = any, p2 = any, p3 = any

	p1 = car( p )

	if( list_length(p) > 1 ) then
		p2 = cdr( p )
	else
		p2 = _NIL_
	end if

	if( p1 <> _NIL_ ) then
		if( p1->dtype = OBJECT_TYPE_IDENTIFIER ) then

			if( *p1->value.id = "lambda" ) then
				function = p
				exit function
			end if
			
			'' Built-in function?
			dim func as LISP_FUNCTION = any
			func = functions->find( p1->value.id )
			if( func ) then
				function = func( parent, p2 )
				exit function
			end if

			'' User defined function?
			p3 = objects->get_object( p1 )
			if( p3 <> _NIL_ ) then
				function = eval_func( p3, p2 )
				exit function
			end if

			'' !!! FIXME: allow overriding built-ins or error when tryin
			'' (possibly by check user-def'ed functions first)

			parent->RaiseError( LISP_ERR_FUNCTION_NOT_DEFINED, *p1->value.id )
							
		end if
	end if

	function = _NIL_

end function

'' ---------------------------------------------------------------------------
'' BUILT-IN FUNCTIONS
'' ---------------------------------------------------------------------------

''
private function LISP_EVAL_CTX.progn( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr

	dim p1 as LISP_OBJECT ptr = p
	dim p2 as LISP_OBJECT ptr = _NIL_

	while ( p1 <> _NIL_ )
		p2 = eval(car(p1))
		p1 = cdr(p1)
	wend

	function = p2
	
end function

''
private function LISP_EVAL_CTX.eval( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	
	select case p->dtype
	case OBJECT_TYPE_IDENTIFIER
		function = objects->get_object( p )

	case OBJECT_TYPE_CONS
		function = eval_cons( p )

	case else
		function = p

	end select

end function

''
private function LISP_EVAL_CTX.call_by_name( byval nameid as zstring ptr, byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	
	dim func as LISP_FUNCTION = any

	'' Built-in function?
	func = functions->find( nameid )
	if( func ) then
		function = func( parent, p )
		exit function
	end if

	'' User defined function?
	dim f as LISP_OBJECT ptr = objects->find_identifier( nameid )
	if( f <> NULL ) then
		dim fp as LISP_OBJECT ptr = objects->get_object( f )
		if( fp <> _NIL_ ) then
			function = eval_func( fp, p )
			exit function
		end if
	end if

	'' !!! FIXME: allow overriding built-ins or error when trying
	'' (possibly by check user-def'ed functions first)

	parent->RaiseError( LISP_ERR_FUNCTION_NOT_DEFINED, *nameid )
	function = _NIL_
	
end function

''
private function LISP_EVAL_CTX.car( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr

	if( p->dtype = OBJECT_TYPE_CONS ) then
		function = p->value.cell.car
	elseif( p->dtype = OBJECT_TYPE_NIL ) then
		function = _NIL_
	else
		parent->RaiseWarning( LISP_ERR_CAR_OF_NON_CONS )
		function = _NIL_
	end if

end function

''
private function LISP_EVAL_CTX.cdr( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr

	if( p->dtype = OBJECT_TYPE_CONS ) then
		function = p->value.cell.cdr
	elseif( p->dtype = OBJECT_TYPE_NIL ) then
		function = _NIL_
	else
		parent->RaiseWarning( LISP_ERR_CDR_OF_NON_CONS )
		function = _NIL_
	end if

end function

''
private function LISP_EVAL_CTX.cons( byval a as LISP_OBJECT ptr, byval b as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	function = objects->new_cons(a,b)
end function

''
private function LISP_EVAL_CTX.list_length( byval p as LISP_OBJECT ptr ) as integer

	dim count as integer = 0

	while( p <> _NIL_ and p->dtype = OBJECT_TYPE_CONS )
		count += 1
		p = cdr(p)
	wend

	function = count

end function

''
private function LISP_EVAL_CTX.copy( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	function = objects->copy_object( p )
end function

'' ---------------------------------------------------------------------------
'' EVALUATOR
'' ---------------------------------------------------------------------------

constructor LISP_EVAL( byval parent_ctx as LISP_CTX ptr )
	ctx = new LISP_EVAL_CTX( parent_ctx )
end constructor

''
destructor LISP_EVAL()
	delete ctx
end destructor

''
function LISP_EVAL.progn( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	function = ctx->progn( p )
end function

''
function LISP_EVAL.eval( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	function = ctx->eval( p )
end function

''
function LISP_EVAL.call_by_name( byval nameid as zstring ptr, byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	function = ctx->call_by_name( nameid, p )
end function

''
function LISP_EVAL.car( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	function = ctx->car( p )
end function

''
function LISP_EVAL.cdr( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	function = ctx->cdr( p )
end function

''
function LISP_EVAL.cons( byval a as LISP_OBJECT ptr, byval b as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	function = ctx->cons( a, b )
end function
''
function LISP_EVAL.length( byval p as LISP_OBJECT ptr ) as integer
	function = ctx->list_length(p)
end function

''
function LISP_EVAL.copy( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	function = ctx->copy( p )
end function


end namespace
