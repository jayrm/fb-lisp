#ifndef __LISP_RUNTIME_BI__
#define __LISP_RUNTIME_BI__

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
#include once "lisp_object.bi"
#include once "lisp_objects.bi"
#include once "lisp_funcs.bi"
#include once "lisp_eval.bi"

namespace LISP

	'' ---------------------------------------------------------------------------
	'' HELPER MACROS
	'' for creating FreeBASIC functions that can be bound to a LispModule 
	'' ---------------------------------------------------------------------------

	#define _RAISEERROR               ctx->RaiseError
	#define _RAISEWARNING             ctx->RaiseWarning
	#define _NIL_                     ctx->objects->NIL_
	#define _T_                       ctx->objects->T_
	#define _CAR                      ctx->evaluator->car
	#define _CDR                      ctx->evaluator->cdr
	#define _EVAL                     ctx->evaluator->eval
	#define _CALL(proc,args)          ctx->evaluator->execute( #proc, args )
	#define _LENGTH                   ctx->evaluator->length
	#define _NEW                      ctx->objects->new_object
	#define _NEW_INTEGER(i)           ctx->objects->new_object( OBJECT_TYPE_INTEGER, i )
	#define _NEW_REAL(f)              ctx->objects->new_object( OBJECT_TYPE_REAL, f )
	#define _SET                      ctx->objects->set_object
	#define _OBJ(n)                   dim n as LISP_OBJECT ptr
	#define _IS_INTEGER(p)            (p->dtype = OBJECT_TYPE_INTEGER)
	#define _IS_REAL(p)               (p->dtype = OBJECT_TYPE_REAL)
	#define _IS_NUMBER(p)             ((p->dtype = OBJECT_TYPE_INTEGER) or (p->dtype = OBJECT_TYPE_REAL))
	#define _IS_STRING(p)             (p->dtype = OBJECT_TYPE_STRING)
	#define _IS_CONS(p)               (p->dtype = OBJECT_TYPE_CONS)

	#macro import_lisp_function( proc )
		declare function F_##proc( byval ctx as LISP_CTX ptr, byval args as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	#endmacro

	#macro define_lisp_function( proc, args )
		private function F_##proc( byval ctx as LISP_CTX ptr, byval args as LISP_OBJECT ptr ) as LISP_OBJECT ptr
	#endmacro

	#macro end_lisp_function()
		end function
	#endmacro

	#define BIND_FUNC( f, name, proc ) f->bind( @name, @F_##proc )

end namespace

#endif
