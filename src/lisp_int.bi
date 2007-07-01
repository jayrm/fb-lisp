#ifndef __LISP_INT_BI__
#define __LISP_INT_BI__

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

#ifndef NULL
#define NULL 0
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE -1
#endif

namespace LISP
	declare function strdup( byval s as zstring ptr ) as zstring ptr
	declare sub strdel( byval s as zstring ptr )

#ifdef DEBUG
	declare function allocate( byval size as uinteger, byval file as zstring ptr, byval lineno as integer ) as any ptr
	declare function callocate( byval size as uinteger, byval file as zstring ptr, byval lineno as integer ) as any ptr
	declare sub deallocate( byval buf as any ptr, byval file as zstring ptr, byval lineno as integer )

#else
	declare function allocate( byval size as uinteger ) as any ptr
	declare function callocate( byval size as uinteger ) as any ptr
	declare sub deallocate( byval buf as any ptr )

#endif

	declare sub memcheck()

end namespace

#include once "lisp_debug.bi"

#endif
