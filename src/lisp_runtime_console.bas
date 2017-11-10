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

import_lisp_function( princ, args )
import_lisp_function( princ_object, args )
import_lisp_function( princ_string, args )
import_lisp_function( dump, args )

'' ---------------------------------------------------------------------------
''
sub bind_runtime_console( byval functions as LISP_FUNCTIONS ptr )

	BIND_FUNC( functions, "princ", princ )                  '' console
	BIND_FUNC( functions, "princ-object", princ_object )    '' console
	BIND_FUNC( functions, "princ-string", princ_string )    '' console
	BIND_FUNC( functions, "dump", dump )                    '' console

end sub

'' ---------------------------------------------------------------------------
'' lisp-syntax: (princ_string <string>)
''
define_lisp_function( princ_string, args )

	'' !!! FIXME: create unescape function

	_OBJ(p) = args

	dim res as string, ch as byte ptr = any

	ch = p->value.str

	res &= chr(34)

	while( *ch )
		
		select case *ch
		case 7
			res &= $"\a"
		case 8
			res &= $"\b"
		case 12
			res &= $"\f"
		case 9
			res &= $"\t"
		case 11
			res &= $"\v"
		case 13
			res &= $"\r"
		case 10
			res &= $"\n"
		case 27
			res &= $"\e"
		case else
			res &= chr(*ch)
		end select
		ch += 1
	wend

	res &= chr(34)

	_PRINT( res )

	function = p

end_lisp_function()


'' ---------------------------------------------------------------------------
'' lisp-syntax: (princ_object <item>)
''
'' requires (princ-string ...)
''
define_lisp_function( princ_object, args )

	_OBJ(p) = args
	_OBJ(p1) = any

	select case p->dtype
	case OBJECT_TYPE_NIL
		_PRINT( "nil" )

	case OBJECT_TYPE_T
		_PRINT( "t" )

	case OBJECT_TYPE_IDENTIFIER
		_PRINT( *p->value.id )

	case OBJECT_TYPE_STRING
		_CALL_BY_NAME( princ-string, p )

	case OBJECT_TYPE_INTEGER
		_PRINT( str(p->value.int) )

	case OBJECT_TYPE_REAL
		_PRINT( str(p->value.flt) )

	case OBJECT_TYPE_CONS
		_PRINT( "(" )
		p1 = p
		do
			_CALL_BY_NAME( princ-object, p1->value.cell.car )
			p1 = p1->value.cell.cdr
			if( p1 <> _NIL_ ) then
				_PRINT( " " )
				if( p1->dtype <> OBJECT_TYPE_CONS ) then
					_PRINT( ". " )
					_CALL_BY_NAME( princ-object, p1 )
					exit do
				end if
			elseif( p1->dtype <> OBJECT_TYPE_CONS ) then
				exit do
			else
				exit do
			end if
		loop
		_PRINT( ")" )
	end select

	function = p

end_lisp_function()

'' ---------------------------------------------------------------------------
'' lisp-syntax: (princ <expr>...)
''
'' requires (princ-string ...)
''
define_lisp_function( princ, args )

	_OBJ(p) = args
	_OBJ(p1) = any

	do
		p1 = _EVAL(_CAR(p))
		if( _IS_STRING(p1) ) then
			_PRINT( *p1->value.str )
			'' !!! FIXME: _CALL_BY_NAME( princ-string, p1 )
		else
			_CALL_BY_NAME( princ-object, p1 )
		end if
		p = _CDR(p)
	loop while ( p <> _NIL_ )

	function = p1

end_lisp_function()

'' ---------------------------------------------------------------------------
''
public function dump_helper _
	( _
		byval ctx as LISP_CTX ptr, _
		byval args as LISP_OBJECT ptr, _
		byval indent as integer _
	) as LISP_OBJECT ptr

	_OBJ(p) = args
	_OBJ(p1) = any

	_PRINT( space(indent * 2 ) )

	if( p = NULL ) then
		_PRINT( "NULL" )

	else
		select case p->dtype
		case OBJECT_TYPE_INVALID
			_PRINT( "OBJECT_TYPE_INVALID" )

		case OBJECT_TYPE_NIL
			_PRINT( "OBJECT_TYPE_NIL" )

		case OBJECT_TYPE_T
			_PRINT( "OBJECT_TYPE_T" )

		case OBJECT_TYPE_INTEGER
			_PRINT( "OBJECT_TYPE_INTEGER:" & p->value.int )

		case OBJECT_TYPE_REAL
			_PRINT( "OBJECT_TYPE_REAL:" & p->value.flt )

		case OBJECT_TYPE_IDENTIFIER
			_PRINT( "OBJECT_TYPE_IDENTIFIER:" & *p->value.id )

		case OBJECT_TYPE_STRING
			_PRINT( "OBJECT_TYPE_STRING:" & *p->value.str )

		case OBJECT_TYPE_CONS

			_PRINT( !"OBJECT_TYPE_CONS:\n" )
			_PRINT( space(indent * 2 ) & !"(\n" )
			_PRINT( "*" )
			p1 = p
			do
				dump_helper( ctx, p1->value.cell.car, indent+1 )
				p1 = p1->value.cell.cdr
				if( p1 <> _NIL_ ) then
					'' _PRINT( " " )
					if( p1->dtype <> OBJECT_TYPE_CONS ) then
						_PRINT( space(indent * 2 ) & !".\n" )
						dump_helper( ctx, p1, indent+1 )
						exit do
					else
						_PRINT( space(indent * 2 ) & !"->\n" )
					end if
				elseif( p1->dtype <> OBJECT_TYPE_CONS ) then
					exit do
				else
					exit do
				end if
			loop
			_PRINT( space(indent * 2 ) & ")" )

		case else

			_PRINT( "UNKNOWN OBJECT TYPE = " & p->dtype )

		end select	

	end if

	_PRINT( !"\n" )

	function = _NIL_

end function

'' ---------------------------------------------------------------------------
'' lisp-syntax: (dump ...)
''
define_lisp_function( dump, args )

	function = dump_helper( ctx, args, 0 )

end_lisp_function()

end namespace
