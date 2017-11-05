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

#include once "lisp_runtime.bi"

namespace LISP

import_lisp_function( set, args )
import_lisp_function( setq, args )
import_lisp_function( setf, args )
import_lisp_function( atom, args )
import_lisp_function( eq, args )
import_lisp_function( equal, args )

import_lisp_function( listp, args )
import_lisp_function( consp, args )
import_lisp_function( integerp, args )
import_lisp_function( numberp, args )
import_lisp_function( zerop, args )
import_lisp_function( stringp, args )

import_lisp_function( eq_atom, args )
import_lisp_function( lt_atom, args )
import_lisp_function( gt_atom, args )
import_lisp_function( ne_atom, args )
import_lisp_function( ge_atom, args )
import_lisp_function( le_atom, args )


'' ---------------------------------------------------------------------------
''
sub bind_runtime_data( byval functions as LISP_FUNCTIONS ptr )

	BIND_FUNC( functions, "set", set )              '' data
	BIND_FUNC( functions, "setq", setq )            '' data
	BIND_FUNC( functions, "setf", setf )            '' data
	BIND_FUNC( functions, "atom", atom )            '' data
	BIND_FUNC( functions, "eq", eq )                '' data
	BIND_FUNC( functions, "equal", equal )          '' data

	BIND_FUNC( functions, "listp", listp )          '' data
	BIND_FUNC( functions, "consp", consp )          '' data
	BIND_FUNC( functions, "integerp", integerp )    '' data
	BIND_FUNC( functions, "numberp", numberp )      '' data
	BIND_FUNC( functions, "zerop", zerop )          '' data
	BIND_FUNC( functions, "stringp", stringp )      '' data

	BIND_FUNC( functions, "=", eq_atom )    '' math
	BIND_FUNC( functions, "<", lt_atom )    '' math
	BIND_FUNC( functions, ">", gt_atom )    '' math
	BIND_FUNC( functions, "/=", ne_atom )   '' math
	BIND_FUNC( functions, ">=", ge_atom )   '' math
	BIND_FUNC( functions, "<=", le_atom )   '' math

end sub

'' ---------------------------------------------------------------------------
'' (set name value)
''
define_lisp_function( set, args )

	_OBJ(p1) = _EVAL(_CAR(args))
	_OBJ(p2) = _EVAL(_CAR(_CDR(args)))
	if( p1 = _NIL_ ) then
		_RAISEERROR( LISP_ERR_SETTING_VALUE_OF_NIL_OBJECT )
	elseif( _IS_IDENTIFIER( p1 ) = FALSE ) then
		_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
	else
		_SET( p1, p2 )
	end if

	function = p2

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (setf name value...)
''
define_lisp_function( setf, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any

	do
		p1 = _CAR(p)
		p2 = _EVAL(_CAR(_CDR(p)))

		if( p1 = _NIL_ ) then
			_RAISEERROR( LISP_ERR_SETTING_VALUE_OF_NIL_OBJECT )
		elseif( _IS_IDENTIFIER( p1 ) ) then
			_SET( p1, p2 )
		elseif( _IS_CONS( p1 ) ) then
			'' !!! FIXME: This does not work
			_DUMP( p1 )
			p1->value.cell.car = p2
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
		end if
		p = _CDR(_CDR(p))
	loop while ( p <> _NIL_ )

	function = p2

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (setq name value...)
''
define_lisp_function( setq, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any

	do
		p1 = _CAR(p)
		p2 = _EVAL(_CAR(_CDR(p)))

		if( p1 = _NIL_ ) then
			_RAISEERROR( LISP_ERR_SETTING_VALUE_OF_NIL_OBJECT )
		elseif( _IS_IDENTIFIER( p1 ) = FALSE ) then
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
		else
			_SET( p1, p2 )
		end if
		p = _CDR(_CDR(p))
	loop while ( p <> _NIL_ )

	function = p2

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (atom expr)
''
define_lisp_function( atom, args )

	_OBJ(p) = args

	p = _EVAL(_CAR(args))

	if( _IS_CONS(p) ) then
		function = _NIL_
	else
		function = _T_
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (eq <expr1> <expr2>)
''
define_lisp_function( eq, args )


	_OBJ(p1) = _EVAL(_CAR(args))
	_OBJ(p2) = _EVAL(_CAR(_CDR(args)))

	function = _NIL_

	if (p1 = p2) then
		function = _T_
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
''
private function equal_helper _
	( _
		byval ctx as LISP_CTX ptr, _
		byval p1 as LISP_OBJECT ptr, _
		byval p2 as LISP_OBJECT ptr _
	) as integer

	function = FALSE

	if (p1 = p2) then
		function = TRUE

	elseif( _IS_CONS(p1) and _IS_CONS(p2) ) then
		if( equal_helper( ctx, p1->value.cell.car, p2->value.cell.car ) ) then
			if( equal_helper( ctx, p1->value.cell.cdr, p2->value.cell.cdr ) ) then
				function = TRUE
			end if
		end if
		
	elseif( _IS_CONS(p1) or _IS_CONS(p2) ) then
		'' not equal, fall through

	elseif( p1->dtype = p2->dtype ) then

		select case p1->dtype
		case OBJECT_TYPE_IDENTIFIER
			if( *p1->value.id = *p2->value.id ) then
				function = TRUE
			end if

		case OBJECT_TYPE_INTEGER
			if( p1->value.int = p2->value.int ) then
				function = TRUE
			end if

		case OBJECT_TYPE_REAL
			if( p1->value.flt = p2->value.flt ) then
				function = TRUE
			end if

		case OBJECT_TYPE_STRING
			if( *p1->value.str = *p2->value.str ) then
				function = TRUE
			end if

		end select

	end if

end function

'' ---------------------------------------------------------------------------
'' (equal <expr1> <expr2>)
''
define_lisp_function( equal, args )

	_OBJ(p1) = _EVAL(_CAR(args))
	_OBJ(p2) = _EVAL(_CAR(_CDR(args)))

	if( equal_helper( ctx, p1, p2 ) ) then
		function = _T_
	else
		function = _NIL_
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (listp <expr>)
''
define_lisp_function( listp, args)

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else
		_OBJ(p) = _EVAL(_CAR(args))
		if( _IS_CONS( p ) ) then
			function = _T_
		elseif( p = _NIL_ ) then
			function = _T_
		else
			function = _NIL_
		end if
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (consp <expr>)
''
define_lisp_function( consp, args)

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else
		_OBJ(p) = _EVAL(_CAR(args))
		if( _IS_CONS( p ) ) then
			function = _T_
		else
			function = _NIL_
		end if
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (integerp <expr>)
''
define_lisp_function( integerp, args)

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else
		_OBJ(p) = _EVAL(_CAR(args))
		if( _IS_INTEGER(p) ) then
			function = _T_
		else
			function = _NIL_
		end if
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (numberp <expr>)
''
define_lisp_function( numberp, args)

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else
		_OBJ(p) = _EVAL(_CAR(args))
		if( _IS_NUMBER(p) ) then
			function = _T_
		else
			function = _NIL_
		end if
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (zerop <expr>)
''
define_lisp_function( zerop, args)

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else
		_OBJ(p) = _EVAL(_CAR(args))
		if( _IS_NUMBER(p) ) then
			if( *p = 0 ) then
				function = _T_
			else
				function = _NIL_
			end if
		else
			function = _NIL_
		end if
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (stringp <expr>)
''
define_lisp_function( stringp, args)

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else
		_OBJ(p) = _EVAL(_CAR(args))
		if( _IS_STRING( p ) ) then
			function = _T_
		else
			function = _NIL_
		end if
	end if

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
			function = _NIL_
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
			function = _NIL_
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
			function = _NIL_
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
			function = _NIL_
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
			function = _NIL_
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
			function = _NIL_
			exit while
		end if

		p1 = p2
		p = _CDR(p)
	wend

end_lisp_function()

end namespace
