#ifndef __LISP_OBJECT_BI__
#define __LISP_OBJECT_BI__

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
	enum LISP_OBJECT_TYPE
		OBJECT_TYPE_INVALID
		OBJECT_TYPE_NIL
		OBJECT_TYPE_T
		OBJECT_TYPE_INTEGER
		OBJECT_TYPE_REAL
		OBJECT_TYPE_IDENTIFIER
		OBJECT_TYPE_STRING
		OBJECT_TYPE_CONS
	end enum

	type LISP_OBJECT_ as LISP_OBJECT

	'' 
	type LISP_CELL
		car as LISP_OBJECT_ ptr
		cdr as LISP_OBJECT_ ptr
	end type

	''
	type LISP_INTEGER as integer

	''
	type LISP_REAL as double

	''
	union LISP_OBJECT_VALUE
		id as zstring ptr
		str as zstring ptr
		int as LISP_INTEGER
		flt as LISP_REAL
		cell as LISP_CELL
	end union

	''
	type LISP_OBJECT
		declare constructor()
		declare destructor()

		dtype as LISP_OBJECT_TYPE
		value as LISP_OBJECT_VALUE
		nxt as LISP_OBJECT ptr
		usage_id as integer

		declare operator += ( byref rhs as LISP_OBJECT )
		declare operator -= ( byref rhs as LISP_OBJECT )
		declare operator *= ( byref rhs as LISP_OBJECT )
		declare operator /= ( byref rhs as LISP_OBJECT )

		declare function IsZero() as integer

		declare function compare_integer( byref rhs as LISP_OBJECT ) as integer
		declare function compare_integer( byval rhs as LISP_INTEGER ) as integer
		declare function compare_real( byref rhs as LISP_OBJECT ) as integer
		declare function compare_real( byval rhs as LISP_REAL ) as integer
		declare function compare_string( byref rhs as LISP_OBJECT ) as integer
		declare function compare_string( byval rhs as zstring ptr ) as integer
		declare operator cast() as double
		declare operator cast() as single
		declare operator cast() as integer

		declare function abs_number() as double

	end type

	declare operator = ( byref lhs as LISP_OBJECT, byref rhs as LISP_OBJECT ) as integer
	declare operator < ( byref lhs as LISP_OBJECT, byref rhs as LISP_OBJECT ) as integer
	declare operator > ( byref lhs as LISP_OBJECT, byref rhs as LISP_OBJECT ) as integer
	declare operator <> ( byref lhs as LISP_OBJECT, byref rhs as LISP_OBJECT ) as integer
	declare operator >= ( byref lhs as LISP_OBJECT, byref rhs as LISP_OBJECT ) as integer
	declare operator <= ( byref lhs as LISP_OBJECT, byref rhs as LISP_OBJECT ) as integer

end namespace

#endif