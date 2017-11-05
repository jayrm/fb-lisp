#ifndef __LISP_BI__
#define __LISP_BI__

/'
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

#include once "lisp_err.bi"

#inclib "lisp"

namespace LISP

	''
	type LISP_CTX_ as LISP_CTX
	type LISP_FUNCTIONS_ as LISP_FUNCTIONS

	type LISP_PRINT_CALLBACK as sub( byref s as const string )

	''
	type LispModule

		declare constructor( )
		declare constructor( byref rhs as LispModule )
		declare destructor( )

	public:
		declare function Eval( byref text as string ) as integer
		declare function Load( byref filename as string ) as integer
		declare sub GarbageCollect()

		declare property EchoInput() as integer
		declare property EchoInput( byval flag as integer )
		declare property ShowResults() as integer
		declare property ShowResults( byval flag as integer )

		declare sub ResetError()
		declare function ErrorCode() as integer
		declare function ErrorText() as string
		declare function ErrorLine() as integer
		declare function ErrorColumn() as integer

		declare property Functions() as LISP_FUNCTIONS_ ptr

		declare sub SetPrintCallBack( byval cb as LISP_PRINT_CALLBACK )
		declare function GetPrintCallBack() as LISP_PRINT_CALLBACK
		declare sub PrintOut( byref s as const string )

	private:
		ctx as LISP_CTX_ ptr

	end type

end namespace

#endif
