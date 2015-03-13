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
#include once "lisp_lexer.bi"
#include once "lisp_object.bi"
#include once "lisp_objects.bi"
#include once "lisp_parser.bi"
#include once "lisp_funcs.bi"
#include once "lisp_eval.bi"

#include once "lisp.bi"

#include once "lisp_runtime.bi"

namespace LISP

'' ---------------------------------------------------------------------------
'' USER API
'' ---------------------------------------------------------------------------

'' !!! FIXME: Add copy constructor / reference counting

''
constructor LispModule( )
	ctx = new LISP_CTX
end constructor

''
destructor LispModule( )
	delete ctx
	ctx = NULL
end destructor

''
function LispModule.Eval( byref text as string ) as integer
	function = ctx->Eval( text )
end function

''
function LispModule.Load( byref filename as string ) as integer
	function = ctx->Load( filename )
end function

''
sub LispModule.GarbageCollect()
	ctx->objects->garbage_collect()
end sub

''
sub LispModule.ResetError()
	ctx->ResetError()
end sub

''
function LispModule.ErrorCode() as integer
	function = ctx->ErrorCode
end function

''
function LispModule.ErrorText() as string
	function = ctx->GetErrorText()
end function

''
function LispModule.ErrorLine() as integer
	function = ctx->ErrorLine
end function

''
property LispModule.EchoInput() as integer
	property = ctx->EchoInput
end property

''
property LispModule.EchoInput( byval flag as integer )
	ctx->EchoInput = flag
end property

''
property LispModule.ShowResults() as integer
	property = ctx->ShowResults
end property

''
property LispModule.ShowResults( byval flag as integer )
	ctx->ShowResults = flag
end property

''
property LispModule.Functions() as LISP_FUNCTIONS_ ptr
	property = ctx->functions
end property

''
sub LispModule.SetPrintCallBack( byval cb as LISP_PRINT_CALLBACK )
	ctx->print_cb = cb
end sub

''
function LispModule.GetPrintCallBack() as LISP_PRINT_CALLBACK
	function = ctx->print_cb
end function

''
sub LispModule.PrintOut( byref s as const string )
	ctx->PrintOut( s )
end sub

end namespace
