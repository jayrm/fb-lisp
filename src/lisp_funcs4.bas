/'
 * 
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

#include once "lisp_runtime.bi"

namespace LISP

'' ---------------------------------------------------------------------------
'' (abs number)
''
define_lisp_function( abs, args )

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else
		_OBJ(p) = _EVAL(_CAR(args))
		if( _IS_NUMBER(p) ) then
			function = _NEW_REAL( p->number_abs() )
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
		end if
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (load "filename")
''
define_lisp_function( load, args)

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
	else
		_OBJ(p) = _EVAL(_CAR(args))
		if( _IS_STRING(p) ) then
			dim h as integer = freefile
			dim x as string
			if( open( *p->value.str for input access read as #h ) = 0 ) then
				close #1
				if( open( *p->value.str for binary access read as #h ) = 0 ) then
					x = space( lof( 1 ))
					get #1,,x
					close #1

					'' !!! insert 'x' in to the lexer buffer

					function = _NIL_ '' _T_
					exit function
				end if 
			end if
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
		end if
	end if

	function = _NIL_

end_lisp_function()

'' ---------------------------------------------------------------------------
''
sub bind_intrinsic_funcs4( byval functions as LISP_FUNCTIONS ptr )

	BIND_FUNC( functions, "abs", abs )
	BIND_FUNC( functions, "load", load )

end sub

end namespace
