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

#include once "lisp_runtime.bi"

'' ---------------------------------------------------------------------------
'' (princ-string <string>)
''
define_lisp_function( princ_string, args )

	'' FIXME: create unescape function

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

	print res;

	function = p

end_lisp_function


'' ---------------------------------------------------------------------------
'' (princ-object <item>)
''
define_lisp_function( princ_object, args )

	_OBJ(p) = args
	_OBJ(p1) = any

	select case p->dtype
	case OBJECT_TYPE_NIL
		print "nil";

	case OBJECT_TYPE_T
		print "t";

	case OBJECT_TYPE_IDENTIFIER
		print *p->value.id;

	case OBJECT_TYPE_STRING
		_CALL( princ_string, p )

	case OBJECT_TYPE_INTEGER
		print str(p->value.int);

	case OBJECT_TYPE_REAL
		print str(p->value.flt);

	case OBJECT_TYPE_CONS
		print "(";
		p1 = p
		do
			_CALL( princ_object, p1->value.cell.car )
			p1 = p1->value.cell.cdr
			if( p1 <> _NIL_ ) then
				print " ";
				if( p1->dtype <> OBJECT_TYPE_CONS ) then
					print ". ";
					_CALL( princ_object, p1 )
				end if
			elseif( p1->dtype <> OBJECT_TYPE_CONS ) then
				exit do
			else
				exit do
			end if
		loop
		print ")";
	end select

	function = p

end_lisp_function

'' ---------------------------------------------------------------------------
'' (princ <expr>...)
''
define_lisp_function( princ, args )

	_OBJ(p) = args
	_OBJ(p1) = any

	do
		p1 = _EVAL(_CAR(p))
		if( _IS_STRING(p1) ) then
			print *p1->value.str;
		else
			_CALL( princ_object, p1 )
		end if
		p = _CDR(p)
	loop while ( p <> _NIL_ )

	function = p1

end_lisp_function

'' ---------------------------------------------------------------------------
'' (gc)
'' (garbage-collect)
''
define_lisp_function( gc, args )

	function = ctx->objects->garbage_collect()

end_lisp_function

'' ---------------------------------------------------------------------------
''
sub bind_intrinsic_funcs2( byval functions as LISP_FUNCTIONS ptr )

	BIND_FUNC( functions, "princ", princ )
	BIND_FUNC( functions, "princ-object", princ_object )	'' INTERNAL
	BIND_FUNC( functions, "princ-string", princ_string )	'' INTERNAL

	BIND_FUNC( functions, "gc", gc )
	BIND_FUNC( functions, "garbage-collect", gc )

end sub