#ifndef __LISP_EVAL_BI__
#define __LISP_EVAL_BI__

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

namespace LISP

	type LISP_EVAL_CTX_ as LISP_EVAL_CTX

	type LISP_EVAL

		declare constructor( )
		declare constructor( byval parent_ctx as LISP_CTX ptr )
		declare destructor()

	public:
		declare function progn( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
		declare function eval( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
		declare function call_by_name( byval nameid as zstring ptr, byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr

		declare function car( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
		declare function cdr( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr
		declare function length( byval p as LISP_OBJECT ptr ) as integer
		declare function copy( byval p as LISP_OBJECT ptr ) as LISP_OBJECT ptr

	private:
		ctx as LISP_EVAL_CTX_ ptr

	end type

end namespace

#endif
