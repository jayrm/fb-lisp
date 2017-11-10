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
import_lisp_function( eql, args )
import_lisp_function( equal, args )
import_lisp_function( equalp, args )

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
	BIND_FUNC( functions, "atom", atom )            '' data (tests/atom.lsp)

	BIND_FUNC( functions, "eq", eq )                '' data (tests/eq.lsp)
	BIND_FUNC( functions, "eql", eql )              '' data (tests/eq.lsp)
	BIND_FUNC( functions, "equal", equal )          '' data (tests/eq.lsp)
	BIND_FUNC( functions, "equalp", equalp )        '' data (tests/eq.lsp)

	BIND_FUNC( functions, "listp", listp )          '' data (tests/types.lsp)
	BIND_FUNC( functions, "consp", consp )          '' data (tests/types.lsp)
	BIND_FUNC( functions, "integerp", integerp )    '' data (tests/types.lsp)
	BIND_FUNC( functions, "numberp", numberp )      '' data (tests/types.lsp)
	BIND_FUNC( functions, "zerop", zerop )          '' data (tests/types.lsp)
	BIND_FUNC( functions, "stringp", stringp )      '' data (tests/types.lsp)

	BIND_FUNC( functions, "=", eq_atom )            '' math (tests/mathcomp.lsp)
	BIND_FUNC( functions, "<", lt_atom )            '' math (tests/mathcomp.lsp)
	BIND_FUNC( functions, ">", gt_atom )            '' math (tests/mathcomp.lsp)
	BIND_FUNC( functions, "/=", ne_atom )           '' math (tests/mathcomp.lsp)
	BIND_FUNC( functions, ">=", ge_atom )           '' math (tests/mathcomp.lsp)
	BIND_FUNC( functions, "<=", le_atom )           '' math (tests/mathcomp.lsp)

end sub

'' ---------------------------------------------------------------------------
'' lisp-syntax: (set <expr1> <expr2>)
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
'' lisp-syntax: (setf <sym1> <expr1> [<sym2> <expr2>]...)
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
			p1->value.cell.car = p2
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
		end if
		p = _CDR(_CDR(p))
	loop while ( p <> _NIL_ )

	function = p2

end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (setq <sym1> <expr1> [<sym2> <expr2>]...)
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
'' lisp-syntax: (atom <expr>)
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
'' lisp-syntax: (eq <expr1> <expr2>)
''
'' return T if <expr1> and <expr2> are 
'' - the exact same data (stored at same memory address)
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
private function eql_helper _
	( _
		byval ctx as LISP_CTX ptr, _
		byval p1 as LISP_OBJECT ptr, _
		byval p2 as LISP_OBJECT ptr _
	) as integer

	function = FALSE

	if (p1 = p2) then
		function = TRUE

	elseif( p1->dtype = p2->dtype ) then

		select case p1->dtype
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
'' lisp-syntax: (eql <expr1> <expr2>)
''
'' return T if <expr1> and <expr2> are both
'' - integer and same value
'' - real as same value
'' - string and same case sensitive value
'' - otherwise use (eq)
''
define_lisp_function( eql, args )

	_OBJ(p1) = _EVAL(_CAR(args))
	_OBJ(p2) = _EVAL(_CAR(_CDR(args)))

	if( eql_helper( ctx, p1, p2 ) ) then
		function = _T_
	else
		function = _NIL_
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
'' lisp-syntax: (equal <expr1> <expr2>)
''
'' return T if <expr1> and <expr2> are
'' - are both integer and same value
'' - are both real as same value
'' - are both string and same case sensitive value
'' - otherwise use (eq)

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
''
private function equalp_helper _
	( _
		byval ctx as LISP_CTX ptr, _
		byval p1 as LISP_OBJECT ptr, _
		byval p2 as LISP_OBJECT ptr _
	) as integer

	function = FALSE

	if (p1 = p2) then
		function = TRUE

	elseif( _IS_CONS(p1) and _IS_CONS(p2) ) then
		if( equalp_helper( ctx, p1->value.cell.car, p2->value.cell.car ) ) then
			if( equalp_helper( ctx, p1->value.cell.cdr, p2->value.cell.cdr ) ) then
				function = TRUE
			end if
		end if
		
	elseif( _IS_CONS(p1) or _IS_CONS(p2) ) then
		'' not equal, fall through

	elseif( _IS_NUMBER(p1) and _IS_NUMBER(p2) ) then
		if( *p1 = *p2 ) then
			function = TRUE
		end if

	elseif( p1->dtype = p2->dtype ) then

		select case p1->dtype
		case OBJECT_TYPE_IDENTIFIER
			if( *p1->value.id = *p2->value.id ) then
				function = TRUE
			end if
		case OBJECT_TYPE_STRING
			if( lcase(*p1->value.str) = lcase(*p2->value.str) ) then
				function = TRUE
			end if

		end select

	end if

end function

'' ---------------------------------------------------------------------------
'' lisp-syntax: (equalp <expr1> <expr2>)
''
'' return T if <expr1> and <expr2> are
'' - are both number and same value
'' - are both string and same case insensitive value
'' - otherwise use (eq)

define_lisp_function( equalp, args )

	_OBJ(p1) = _EVAL(_CAR(args))
	_OBJ(p2) = _EVAL(_CAR(_CDR(args)))

	if( equalp_helper( ctx, p1, p2 ) ) then
		function = _T_
	else
		function = _NIL_
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (listp <expr>)
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
'' lisp-syntax: (consp <expr>)
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
'' lisp-syntax: (integerp <expr>)
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
'' lisp-syntax: (numberp <expr>)
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
'' lisp-syntax: (zerop <expr>)
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
'' lisp-syntax: (stringp <expr>)
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
'' lisp-syntax: (= <atom> atom...)
''
define_lisp_function( eq_atom, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any

	function = _NIL_

	select case _LENGTH(args)
	case 0
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )

	case 1
		'' special case - always returns T

		p1 = _EVAL(_CAR(p))
		if( _IS_NUMBER(p1) ) then
			function = _T_
		else
			_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
		end if

	case 2
		'' special case - typical usage

		p1 = _EVAL(_CAR(p))
		p2 = _EVAL(_CAR(_CDR(p)))

		if( _IS_NUMBER(p1) andalso _IS_NUMBER(p2) ) then
			if( *p1 = *p2 ) then
				function = _T_
			end if
		else
			_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
		end if
		
	
	case else
		'' general case

		_OBJ(n) = _CALL_BY_NAME( list, args )

		'' get the first number
		p1 = _CAR(n)
		p = _CDR(n)
		p2 = _CAR(p)

		if( not _IS_NUMBER(p1) ) then
			_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
			exit select
		end if

		'' test remaining numbers
		while( p2 <> _NIL_ )

			if( not _IS_NUMBER(p2) ) then
				_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
				exit select
			end if

			if( *p1 <> *p2 ) then
				exit select
			end if

			p = _CDR(p)
			p2 = _CAR(p)
		wend

		function = _T_

	end select

end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (< <atom> atom...)
''
define_lisp_function( lt_atom, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any

	function = _NIL_

	if( p = _NIL_ ) then
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )
		exit function
	end if

	p = _CALL_BY_NAME( list, args )

	p1 = _CAR(p)
	p = _CDR(p)

	if( not _IS_NUMBER(p1) ) then
		_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
		exit function
	end if

	while( p <> _NIL_ )

		p2 = _CAR(p)

		if( not _IS_NUMBER(p2) ) then
			_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
			exit function
		end if

		if( *p1 < *p2 ) then
		else
			exit function
		end if
		
		p1 = p2
		p = _CDR(p)

	wend

	function = _T_

end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (> <atom> atom...)
''
define_lisp_function( gt_atom, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any

	function = _NIL_

	if( p = _NIL_ ) then
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )
		exit function
	end if

	p = _CALL_BY_NAME( list, args )

	p1 = _CAR(p)
	p = _CDR(p)

	if( not _IS_NUMBER(p1) ) then
		_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
		exit function
	end if

	while( p <> _NIL_ )

		p2 = _CAR(p)

		if( not _IS_NUMBER(p2) ) then
			_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
			exit function
		end if

		if( *p1 > *p2 ) then
		else
			exit function
		end if
		
		p1 = p2
		p = _CDR(p)

	wend

	function = _T_

end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (/= <atom> atom...)
''
'' returns T if all number values are unique
'' returns T if only 1 number
'' returns NIL if any numbers are equal value
define_lisp_function( ne_atom, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any

	function = _NIL_

	select case _LENGTH(args)
	case 0
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )

	case 1
		p1 = _EVAL(_CAR(p))
		if( _IS_NUMBER(p1) ) then
			function = _T_
		else
			_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
		end if

	case 2
		p1 = _EVAL(_CAR(p))
		p2 = _EVAL(_CAR(_CDR(p)))

		if( _IS_NUMBER(p1) andalso _IS_NUMBER(p2) ) then
			if( *p1 <> *p2 ) then
				function = _T_
			end if
		else
			_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
		end if

	case else
		'' general case

		_OBJ(n) = _CALL_BY_NAME( list, args )

		'' n is the list of numbers
		p = n

		'' use p to iterate the numbers and check for non-numeric types
		while( p <> _NIL_ )
			p1 = _CAR(p)
			if( not _IS_NUMBER(p1) ) then
				_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
				exit select
			end if
			p = _CDR(p)
		wend

		'' iterate the list, and each sub-list testing if any value is equal
		'' n is the list to iterate
		'' p1 is first item in list n
		'' p is the rest of the items in list n
		'' p2 is current item in list p
		while( n <> _NIL_ )
			p1 = _CAR(n)
			p = _CDR(n)
			while( p <> _NIL_ )
				p2 = _CAR(p)
				if( *p1 = *p2 ) then
					exit select
				end if
				p = _CDR(p)
			wend
			n = _CDR(n)
		wend

		function = _T_

	end select

end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (>= <atom> atom...)
''
define_lisp_function( ge_atom, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any

	function = _NIL_

	if( p = _NIL_ ) then
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )
		exit function
	end if

	p = _CALL_BY_NAME( list, args )

	p1 = _CAR(p)
	p = _CDR(p)

	if( not _IS_NUMBER(p1) ) then
		_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
		exit function
	end if

	while( p <> _NIL_ )

		p2 = _CAR(p)

		if( not _IS_NUMBER(p2) ) then
			_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
			exit function
		end if

		if( *p1 >= *p2 ) then
		else
			exit function
		end if
		
		p1 = p2
		p = _CDR(p)

	wend

	function = _T_


end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (<= <atom> atom...)
''
define_lisp_function( le_atom, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any

	function = _NIL_

	if( p = _NIL_ ) then
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )
		exit function
	end if

	p = _CALL_BY_NAME( list, args )

	p1 = _CAR(p)
	p = _CDR(p)

	if( not _IS_NUMBER(p1) ) then
		_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
		exit function
	end if

	while( p <> _NIL_ )

		p2 = _CAR(p)

		if( not _IS_NUMBER(p2) ) then
			_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
			exit function
		end if

		if( *p1 <= *p2 ) then
		else
			exit function
		end if
		
		p1 = p2
		p = _CDR(p)

	wend

	function = _T_


end_lisp_function()

end namespace
