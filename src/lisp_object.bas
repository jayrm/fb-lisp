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

namespace LISP

'' ---------------------------------------------------------------------------
'' OBJECT
'' ---------------------------------------------------------------------------

''
constructor LISP_OBJECT( )

	dtype = OBJECT_TYPE_INVALID
	usage_id = 0

end constructor

''
destructor LISP_OBJECT( )

	select case dtype
	case OBJECT_TYPE_STRING
		lisp.strdel( value.str )

	case OBJECT_TYPE_IDENTIFIER
		lisp.strdel( value.id )

	end select

end destructor

''
operator LISP_OBJECT.+= ( byref rhs as LISP_OBJECT )
	
	if( dtype = OBJECT_TYPE_INTEGER ) then
		if( rhs.dtype = OBJECT_TYPE_INTEGER ) then
			value.int += rhs.value.int

		elseif( rhs.dtype = OBJECT_TYPE_REAL ) then
			value.flt = cast( LISP_REAL, value.int ) + rhs.value.flt
			dtype = OBJECT_TYPE_REAL

		end if

	elseif( dtype = OBJECT_TYPE_REAL ) then
		if( rhs.dtype = OBJECT_TYPE_INTEGER ) then
			value.flt += cast( LISP_REAL, rhs.value.int )

		elseif( rhs.dtype = OBJECT_TYPE_REAL ) then
			value.flt += rhs.value.flt

		end if

	end if
	
end operator

''
operator LISP_OBJECT.-= ( byref rhs as LISP_OBJECT )

	if( dtype = OBJECT_TYPE_INTEGER ) then
		if( rhs.dtype = OBJECT_TYPE_INTEGER ) then
			value.int -= rhs.value.int

		elseif( rhs.dtype = OBJECT_TYPE_REAL ) then
			value.flt = cast( LISP_REAL, value.int ) - rhs.value.flt
			dtype = OBJECT_TYPE_REAL

		end if

	elseif( dtype = OBJECT_TYPE_REAL ) then
		if( rhs.dtype = OBJECT_TYPE_INTEGER ) then
			value.flt -= cast( LISP_REAL, rhs.value.int )

		elseif( rhs.dtype = OBJECT_TYPE_REAL ) then
			value.flt -= rhs.value.flt

		end if

	end if

end operator

''
operator LISP_OBJECT.*= ( byref rhs as LISP_OBJECT )

	if( dtype = OBJECT_TYPE_INTEGER ) then
		if( rhs.dtype = OBJECT_TYPE_INTEGER ) then
			value.int *= rhs.value.int

		elseif( rhs.dtype = OBJECT_TYPE_REAL ) then
			value.flt = cast( LISP_REAL, value.int ) * rhs.value.flt
			dtype = OBJECT_TYPE_REAL

		end if

	elseif( dtype = OBJECT_TYPE_REAL ) then
		if( rhs.dtype = OBJECT_TYPE_INTEGER ) then
			value.flt *= cast( LISP_REAL, rhs.value.int )

		elseif( rhs.dtype = OBJECT_TYPE_REAL ) then
			value.flt *= rhs.value.flt

		end if

	end if

end operator

''
operator LISP_OBJECT./= ( byref rhs as LISP_OBJECT )

	'' WARNING: this won't catch division by zero.  That needs to be
	'' done by the caller

	if( dtype = OBJECT_TYPE_INTEGER ) then
		value.flt = cast( LISP_REAL, value.int )
		dtype = OBJECT_TYPE_REAL
	end if

	if( rhs.dtype = OBJECT_TYPE_INTEGER ) then
		value.flt /= cast( LISP_REAL, rhs.value.int )

	elseif( rhs.dtype = OBJECT_TYPE_REAL ) then
		value.flt /= rhs.value.flt

	end if

end operator

''
function LISP_OBJECT.IsZero() as integer
	
	function = FALSE
	
	if( dtype = OBJECT_TYPE_INTEGER ) then
		if( value.int = 0 ) then
			function = TRUE
		end if
	elseif( dtype = OBJECT_TYPE_REAL ) then
		if( value.flt = 0 ) then
			function = TRUE
		end if
	end if

end function

''
#macro IMPL_COMPARE( n, p, a, b )
	function LISP_OBJECT.compare_##n( p ) as integer
		if( a < b ) then
			function = -1
		elseif( a > b ) then
			function = 1
		else
			function = 0
		end if
	end function
#endmacro

IMPL_COMPARE( integer, byref rhs as LISP_OBJECT, value.int, rhs.value.int )
IMPL_COMPARE( integer, byval rhs as LISP_INTEGER, value.int, rhs )
IMPL_COMPARE( real, byref rhs as LISP_OBJECT, value.flt, rhs.value.flt )
IMPL_COMPARE( real, byval rhs as LISP_REAL, value.flt, rhs )
IMPL_COMPARE( string, byref rhs as LISP_OBJECT, *value.str, *rhs.value.str )
IMPL_COMPARE( string, byval rhs as zstring ptr, *value.str, *rhs )


''
#macro IMPL_REL_OP( op )
operator LISP_OBJECT.op ( byref lhs as LISP_OBJECT, byref rhs as LISP_OBJECT ) as integer

	operator = FALSE

	if( lhs.dtype = OBJECT_TYPE_INTEGER ) then
		if( rhs.dtype = OBJECT_TYPE_INTEGER ) then
			operator = ( compare_integer( rhs ) op 0 )
		elseif( rhs.dtype = OBJECT_TYPE_REAL ) then
			operator = ( -rhs.compare_real( cast( LISP_REAL, lhs.value.int ) ) op 0 )
		end if

	elseif( lhs.dtype = OBJECT_TYPE_REAL ) then
		if( rhs.dtype = OBJECT_TYPE_INTEGER ) then
			operator = ( compare_real( cast( LISP_INTEGER, rhs.value.int ) ) op 0 )
		elseif( rhs.dtype = OBJECT_TYPE_REAL ) then
			operator = ( compare_real( rhs) op 0 )
		end if
		
	elseif( lhs.dtype = OBJECT_TYPE_STRING ) then
		if( rhs.dtype = OBJECT_TYPE_STRING ) then
			operator = ( compare_string( rhs ) op 0 )
		end if
	end if

end operator
#endmacro

IMPL_REL_OP( = )
IMPL_REL_OP( < )
IMPL_REL_OP( > )
IMPL_REL_OP( <> )
IMPL_REL_OP( >= )
IMPL_REL_OP( <= )

''
operator LISP_OBJECT.cast() as double

	if( dtype = OBJECT_TYPE_INTEGER ) then
		operator = cast(double, value.int)
	elseif( dtype = OBJECT_TYPE_REAL ) then
		operator = cast(double, value.flt)
	else
		operator = 0
	end if

end operator

''
operator LISP_OBJECT.cast() as single

	if( dtype = OBJECT_TYPE_INTEGER ) then
		operator = cast(single, value.int)
	elseif( dtype = OBJECT_TYPE_REAL ) then
		operator = cast(single, value.flt)
	else
		operator = 0
	end if

end operator

''
operator LISP_OBJECT.cast() as integer

	if( dtype = OBJECT_TYPE_INTEGER ) then
		operator = cast(integer, value.int)
	elseif( dtype = OBJECT_TYPE_REAL ) then
		operator = cast(integer, value.flt)
	else
		operator = 0
	end if

end operator

end namespace
