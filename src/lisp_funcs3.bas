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
'' (length <list>)
''
define_lisp_function( length, args )

	_OBJ(p) = args

	dim count as integer = 0

	while(( p <> _NIL_ ) and ( _IS_CONS( p ) <> FALSE ))
		count += 1
		p = _CDR(p)
	wend

	function = _NEW_INTEGER( count )

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (= expr... )
''
define_lisp_function( eq_atom, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any

	function = _NIL_

	p1 = _EVAL(_CAR(p))
	p = _CDR(p)

	if( p = _NIL_ ) then
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )
		exit function
	end if

	while( p <> _NIL_ )
		p2 = _EVAL(_CAR(p))

		if( *p1 = *p2 ) then
			function = _T_
		else
			exit while
		end if

		p1 = p2
		p = _CDR(p)
	wend

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (< expr... )
''
define_lisp_function( lt_atom, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any

	function = _NIL_

	p1 = _EVAL(_CAR(p))
	p = _CDR(p)

	if( p = _NIL_ ) then
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )
		exit function
	end if

	while( p <> _NIL_ )
		p2 = _EVAL(_CAR(p))

		if( *p1 < *p2 ) then
			function = _T_
		else
			exit while
		end if

		p1 = p2
		p = _CDR(p)
	wend

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (> expr... )
''
define_lisp_function( gt_atom, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any

	function = _NIL_

	p1 = _EVAL(_CAR(p))
	p = _CDR(p)

	if( p = _NIL_ ) then
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )
		exit function
	end if

	while( p <> _NIL_ )
		p2 = _EVAL(_CAR(p))

		if( *p1 > *p2 ) then
			function = _T_
		else
			exit while
		end if

		p1 = p2
		p = _CDR(p)
	wend

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (/= expr... )
''
define_lisp_function( ne_atom, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any

	function = _NIL_

	p1 = _EVAL(_CAR(p))
	p = _CDR(p)

	if( p = _NIL_ ) then
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )
		exit function
	end if

	while( p <> _NIL_ )
		p2 = _EVAL(_CAR(p))

		if( *p1 <> *p2 ) then
			function = _T_
		else
			exit while
		end if

		p1 = p2
		p = _CDR(p)
	wend

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (>= expr... )
''
define_lisp_function( ge_atom, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any

	function = _NIL_

	p1 = _EVAL(_CAR(p))
	p = _CDR(p)

	if( p = _NIL_ ) then
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )
		exit function
	end if

	while( p <> _NIL_ )
		p2 = _EVAL(_CAR(p))

		if( *p1 >= *p2 ) then
			function = _T_
		else
			exit while
		end if

		p1 = p2
		p = _CDR(p)
	wend

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (<= expr... )
''
define_lisp_function( le_atom, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any

	function = _NIL_

	p1 = _EVAL(_CAR(p))
	p = _CDR(p)

	if( p = _NIL_ ) then
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )
		exit function
	end if

	while( p <> _NIL_ )
		p2 = _EVAL(_CAR(p))

		if( *p1 <= *p2 ) then
			function = _T_
		else
			exit while
		end if

		p1 = p2
		p = _CDR(p)
	wend

end_lisp_function()

'' ---------------------------------------------------------------------------
''
sub bind_intrinsic_funcs3( byval functions as LISP_FUNCTIONS ptr )

	BIND_FUNC( functions, "length", length )

	BIND_FUNC( functions, "=", eq_atom )
	BIND_FUNC( functions, "<", lt_atom )
	BIND_FUNC( functions, ">", gt_atom )
	BIND_FUNC( functions, "/=", ne_atom )
	BIND_FUNC( functions, ">=", ge_atom )
	BIND_FUNC( functions, "<=", le_atom )

end sub

end namespace
