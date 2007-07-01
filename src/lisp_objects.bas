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
 *
 *
 * Copyright (c) 1997-2001 Sandro Sigala.  All rights reserved.
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

#define _NIL_ ctx->NIL_
#define _T_	  ctx->T_

'' ---------------------------------------------------------------------------
'' OBJECTS_CTX
'' ---------------------------------------------------------------------------

''
type LISP_OBJECT_PAIR

	DECLARE_DEBUG_ALLOCATOR()

	nameid as LISP_OBJECT ptr
	value as LISP_OBJECT ptr
	nxt as LISP_OBJECT_PAIR ptr

end type

DEFINE_DEBUG_ALLOCATOR( LISP_OBJECT_PAIR )

''
type LISP_OBJECTS_CTX

	DECLARE_DEBUG_ALLOCATOR()

	declare constructor( byval parent_ctx as LISP_CTX ptr )
	declare destructor( )

	parent as LISP_CTX ptr

	object_lstfree as LISP_OBJECT ptr
	object_cntfree as integer
	object_lstused as LISP_OBJECT ptr
	object_cntused as integer

	object_lstpair as LISP_OBJECT_PAIR ptr

	NIL_ as LISP_OBJECT_ ptr
	T_ as LISP_OBJECT_ ptr

	gc_id as integer

	declare sub garbage_collect()
	declare sub gc_do_collection()
	declare sub gc_tag_tree( byval p as LISP_OBJECT ptr )
	declare sub gc_tag_whole_tree()

end type

DEFINE_DEBUG_ALLOCATOR( LISP_OBJECTS_CTX )

''
private constructor LISP_OBJECTS_CTX( byval parent_ctx as LISP_CTX ptr )

	parent = parent_ctx

	object_lstfree = NULL
	object_cntfree = 0
	object_lstused = NULL
	object_cntused = 0

	object_lstpair = NULL

	gc_id = 0

end constructor

#macro DELETE_LIST( n, p, nxt )
	while( n )
		p = n->nxt
		delete n
		n = p
	wend
#endmacro

''
private destructor LISP_OBJECTS_CTX( )

	dim as LISP_OBJECT ptr p = any
	DELETE_LIST( object_lstfree, p, nxt )
	DELETE_LIST( object_lstused, p, nxt )

	dim as LISP_OBJECT_PAIR ptr pp = any
	DELETE_LIST( object_lstpair, pp, nxt )

end destructor

''
private sub LISP_OBJECTS_CTX.gc_tag_tree( byval p as LISP_OBJECT ptr )

	if( p->usage_id = gc_id ) then
		exit sub
	end if

	p->usage_id = gc_id

	if( p->dtype = OBJECT_TYPE_CONS) then
		gc_tag_tree(p->value.cell.car)
		gc_tag_tree(p->value.cell.cdr)
	end if

end sub

''
private sub LISP_OBJECTS_CTX.gc_tag_whole_tree()

	dim p as LISP_OBJECT_PAIR ptr = any

	p = object_lstpair
	while( p )
		gc_tag_tree( p->nameid )
		gc_tag_tree( p->value )
		p = p->nxt
	wend

end sub

''
private sub LISP_OBJECTS_CTX.gc_do_collection()

	dim as LISP_OBJECT ptr p = any
	dim as LISP_OBJECT ptr new_object_lstused = T_
	dim as LISP_OBJECT ptr nxt = any

	gc_tag_whole_tree()

	p = object_lstused
	while( p <> NULL and p <> T_ )
		nxt = p->nxt

		if( p->usage_id <> gc_id ) then

			if( p->dtype = OBJECT_TYPE_STRING ) then

				lisp.strdel( p->value.str )

			elseif( p->dtype = OBJECT_TYPE_IDENTIFIER ) then

				lisp.strdel( p->value.id )

			end if

			p->dtype = OBJECT_TYPE_INVALID

			p->nxt = object_lstfree
			object_lstfree = p

			object_cntfree += 1
			object_cntused -= 1

		else

			p->nxt = new_object_lstused
			new_object_lstused = p

		end if

		p = nxt
	wend

	object_lstused = new_object_lstused

end sub

''
private sub LISP_OBJECTS_CTX.garbage_collect( )
	gc_id += 1

	if( gc_id = &h7fffffff ) then
		gc_id = 1
	end if

	gc_do_collection()
end sub

'' ---------------------------------------------------------------------------
'' OBJECTS
'' ---------------------------------------------------------------------------

DEFINE_DEBUG_ALLOCATOR( LISP_OBJECTS )

''
constructor LISP_OBJECTS( byval parent_ctx as LISP_CTX ptr )

	ctx = new LISP_OBJECTS_CTX( parent_ctx )

	'' NOTE: The first two objects added must be NIL, and then T
	'' the GC depends on this order

	ctx->NIL_ = new_object( OBJECT_TYPE_NIL )
	ctx->T_ = new_object( OBJECT_TYPE_T )

end constructor

''
destructor LISP_OBJECTS( )

	delete ctx

end destructor

''
function LISP_OBJECTS.NIL_() as LISP_OBJECT ptr
	function = ctx->NIL_
end function

''
function LISP_OBJECTS.T_() as LISP_OBJECT ptr
	function = ctx->T_
end function

''
function LISP_OBJECTS.new_object() as LISP_OBJECT ptr
	dim p as LISP_OBJECT ptr

	if( ctx->object_lstfree = NULL ) then
		p = new LISP_OBJECT
	else
		p = ctx->object_lstfree
		ctx->object_lstfree = ctx->object_lstfree->nxt
		ctx->object_cntfree -= 1
	end if

	p->nxt = ctx->object_lstused
	ctx->object_lstused = p

	ctx->object_cntused += 1

	p->usage_id = 0

	function = p
end function

''
function LISP_OBJECTS.new_object( byval dtype as LISP_OBJECT_TYPE ) as LISP_OBJECT ptr

	dim p as LISP_OBJECT ptr

	p = new_object()

	p->dtype = dtype
	select case p->dtype
	case OBJECT_TYPE_INTEGER
		p->value.int = 0
	case OBJECT_TYPE_REAL
		p->value.flt = 0
	case OBJECT_TYPE_STRING
		p->value.str = NULL
	case OBJECT_TYPE_CONS
		p->value.cell.car = _NIL_
		p->value.cell.cdr = _NIL_
	end select

	function = p

end function

''
function LISP_OBJECTS.new_object( byval dtype as LISP_OBJECT_TYPE, byval init_value as LISP_INTEGER ) as LISP_OBJECT ptr
	dim tmp as LISP_OBJECT ptr
	tmp = new_object( dtype )
	tmp->value.int = init_value
	function = tmp
end function

''
function LISP_OBJECTS.new_object( byval dtype as LISP_OBJECT_TYPE, byval init_value as LISP_REAL ) as LISP_OBJECT ptr
	dim tmp as LISP_OBJECT ptr
	tmp = new_object( dtype )
	tmp->value.flt = init_value
	function = tmp
end function

''
function LISP_OBJECTS.find_identifier( byval s as zstring ptr ) as LISP_OBJECT ptr
	dim p as LISP_OBJECT ptr = any

	p = ctx->object_lstused
	while( p )
		if( p->dtype = OBJECT_TYPE_IDENTIFIER ) then
			if( *p->value.id = *s ) then
				function = p
				exit function
			end if
		end if
		p = p->nxt
	wend

	function = NULL

end function

''
function LISP_OBJECTS.find_string( byval s as zstring ptr ) as LISP_OBJECT ptr
	dim p as LISP_OBJECT ptr = any

	p = ctx->object_lstused
	while( p )
		if( p->dtype = OBJECT_TYPE_STRING ) then
			if( *p->value.str = *s ) then
				function = p
				exit function
			end if
		end if
		p = p->nxt
	wend

	function = NULL

end function

''
function LISP_OBJECTS.find_integer( byval num as LISP_INTEGER ) as LISP_OBJECT ptr
	dim p as LISP_OBJECT ptr = any

	p = ctx->object_lstused
	while( p )
		if( p->dtype = OBJECT_TYPE_INTEGER ) then
			if( p->value.int = num ) then
				function = p
				exit function
			end if
		end if
		p = p->nxt
	wend

	function = NULL

end function

''
function LISP_OBJECTS.find_real( byval num as LISP_REAL ) as LISP_OBJECT ptr
	dim p as LISP_OBJECT ptr = any

	p = ctx->object_lstused
	while( p )
		if( p->dtype = OBJECT_TYPE_REAL ) then
			if( p->value.flt = num ) then
				function = p
				exit function
			end if
		end if
		p = p->nxt
	wend

	function = NULL

end function

''
sub LISP_OBJECTS.set_object( byval nameid as LISP_OBJECT ptr, byval value as LISP_OBJECT ptr )

	dim p as LISP_OBJECT_PAIR ptr

	if( nameid->value.id = NULL ) then
		exit sub
	end if

	p = ctx->object_lstpair
	while( p )
		if( p->nameid->value.id <> NULL ) then
			if( *nameid->value.id = *p->nameid->value.id ) then
				p->value = value
				exit sub
			end if
		end if
		p = p->nxt
	wend

	p = new LISP_OBJECT_PAIR
	p->nxt = ctx->object_lstpair
	ctx->object_lstpair = p
	p->nameid = nameid
	p->value = value

end sub

''
function LISP_OBJECTS.get_object( byval nameid as LISP_OBJECT ptr ) as LISP_OBJECT ptr

	dim p as LISP_OBJECT_PAIR ptr

	p = ctx->object_lstpair
	while( p )
		if( p->nameid->value.id <> NULL ) then
			if( *nameid->value.id = *p->nameid->value.id ) then
				function = p->value
				exit function
			end if
		end if
		p = p->nxt
	wend

	function = _NIL_

end function

''
function LISP_OBJECTS.garbage_collect() as LISP_OBJECT ptr
	ctx->garbage_collect()	
	function = _T_
end function
