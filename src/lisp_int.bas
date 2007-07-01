#include once "lisp_int.bi"
#include once "crt.bi"

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

#ifdef DEBUG

	type MEM_T
		p as any ptr
		inuse as integer
		file as string
		lineno as integer
		nxt as MEM_T ptr
	end type

	dim shared memlist as MEM_T ptr = NULL

	'' 
	private function find_address( byval p as any ptr ) as MEM_T ptr

		dim a as MEM_T ptr = memlist
		while(a)
			if( a->p = p ) then
				function = a
				exit function
			end if
			a = a->nxt
		wend

		function = NULL

	end function

	''
	private sub add_address( byval p as any ptr, byval file as zstring ptr, byval lineno as integer )

		dim a as MEM_T ptr = any
		
		ASSERT( p <> NULL )

		a = find_address( p )
		if( a ) then
			''print "--- REUSE: ";
		else
			''print "--- ALLOC: ";
			a = new MEM_T
		end if
		a->p = p
		a->inuse = 1
		a->lineno = lineno
		a->file = *file
		a->nxt = memlist
		memlist = a

		''print hex( a->p ) & ", (" & a->file & ", " & a->lineno & ")"
			
	end sub 

	''
	private sub del_address( byval p as any ptr, byval file as zstring ptr, byval lineno as integer )

		dim a as MEM_T ptr = any
		ASSERT( p <> NULL )
		a = find_address( p )
		if(a) then
			if( a->inuse ) then
				a->inuse = 0
				''print "--- FREE: " & hex( a->p ) & ", (" & a->file & ", " & a->lineno & ")"
			else
				print "<<< TRIED TO DOUBLE FREE MEMORY " & hex( a->p ) & ", (" & a->file & ", " & a->lineno & ")"
				print " - FROM (" & *file & ", " & lineno & ") >>>"
			end if
		else
			print "<<< TRIED TO FREE INVALID MEMORY FROM (" & a->file & ", " & a->lineno & ") >>>"
		end if

	end sub

	''
	private sub do_memcheck()

		dim a as MEM_T ptr = memlist

		while( a )
			if( a->inuse ) then
				print "<<< MEMORY LEAKED AT " & hex( a->p ) & ", (" & a->file & ", " & a->lineno & ") >>>"
			end if
			a = a->nxt
		wend

		a = memlist		
		while( a )
			memlist = memlist->nxt
			delete a
			a = memlist
		wend

	end sub

	''
	namespace LISP

		''
		function allocate( byval size as uinteger, byval file as zstring ptr, byval lineno as integer ) as any ptr
			dim p as any ptr = ..allocate( size )
			ASSERT( p <> NULL )
			add_address( p, file, lineno )
			function = p
		end function

		''
		function callocate( byval size as uinteger, byval file as zstring ptr, byval lineno as integer ) as any ptr
			dim p as any ptr = ..callocate( size )
			ASSERT( p <> NULL )
			add_address( p, file, lineno )
			function = p
		end function

		''
		sub deallocate( byval buf as any ptr, byval file as zstring ptr, byval lineno as integer )
			ASSERT( buf <> NULL )
			del_address( buf, file, lineno )
			..deallocate( buf )
		end sub

		''
		sub memcheck()
			do_memcheck
		end sub

		''
		function strdup( byval s as zstring ptr ) as zstring ptr
			dim p as zstring ptr = any
			p = lisp.allocate( len(*s) + 1, __FILE__, __LINE__ )
			ASSERT( p <> NULL )
			*p = *s
			function = p
		end function

		''
		sub strdel( byval s as zstring ptr )
			ASSERT( s <> NULL )
			lisp.deallocate s, __FILE__, __LINE__
		end sub

	end namespace

#else

	namespace LISP

		''
		function allocate( byval size as uinteger ) as any ptr
			function = ..allocate( size )
		end function

		''
		function callocate( byval size as uinteger ) as any ptr
			function = ..callocate( size )
		end function

		''
		sub deallocate( byval buf as any ptr )
			..deallocate( buf )
		end sub

		''
		sub memcheck()
		end sub

		''
		function strdup( byval s as zstring ptr ) as zstring ptr
			dim p as zstring ptr
			p = lisp.allocate( len(*s) + 1 )
			if( p ) then
				*p = *s
			end if
			function = p
		end function

		''
		sub strdel( byval s as zstring ptr )
			if( s ) then
				lisp.deallocate s
			end if
		end sub

	end namespace

#endif
