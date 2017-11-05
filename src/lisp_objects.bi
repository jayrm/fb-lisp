#ifndef __LISP_OBJECTS_BI__
#define __LISP_OBJECTS_BI__

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
	type LISP_OBJECTS_CTX_ as LISP_OBJECTS_CTX

	''
	type LISP_OBJECTS

		declare constructor( byval parent_ctx as LISP_CTX ptr )
		declare destructor( )

	public:

		declare function NIL_() as LISP_OBJECT ptr
		declare function T_() as LISP_OBJECT ptr
		
		declare function new_object( ) as LISP_OBJECT ptr
		declare function new_object( byval dtype as LISP_OBJECT_TYPE ) as LISP_OBJECT ptr
		declare function new_object( byval dtype as LISP_OBJECT_TYPE, byval init_value as LISP_INTEGER ) as LISP_OBJECT ptr
		declare function new_object( byval dtype as LISP_OBJECT_TYPE, byval init_value as LISP_REAL ) as LISP_OBJECT ptr
		declare function new_cons( byval first as LISP_OBJECT ptr, byval rest as LISP_OBJECT ptr ) as LISP_OBJECT ptr

		declare function copy_object( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr

		declare sub set_object( byval nameid as LISP_OBJECT ptr, byval value as LISP_OBJECT ptr )
		declare function get_object( byval nameid as LISP_OBJECT ptr ) as LISP_OBJECT ptr

		declare function find_identifier( byval s as zstring ptr ) as LISP_OBJECT ptr
		declare function find_string( byval s as zstring ptr ) as LISP_OBJECT ptr
		declare function find_integer( byval num as LISP_INTEGER ) as LISP_OBJECT ptr
		declare function find_real( byval num as LISP_REAL ) as LISP_OBJECT ptr

		declare function garbage_collect() as LISP_OBJECT ptr
		declare function mem_used() as integer
		declare function mem_free() as integer

	private:	
		ctx as LISP_OBJECTS_CTX_ ptr

	end type

end namespace

#endif
