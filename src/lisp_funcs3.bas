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
'' (append <expr>...)
''
define_lisp_function( append, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(first) = NULL
	_OBJ(prev) = NULL

	if( p = _NIL_ ) then
		'' no arguments so return nil
		function = _NIL_
		exit function
	end if

	'' iterate through the arguments
	while( p <> _NIL_ )

		'' current argument
		p1 = _EVAL(_CAR(p))

		'' iterate through the elements of the current list
		_OBJ(e) = p1
		_OBJ(e1) = any

		while( e <> _NIL_ )
			'' add elements to the list
			e1 = _NEW( OBJECT_TYPE_CONS )
			if( _IS_CONS(e) ) then
				e1->value.cell.car = _COPY(_CAR(e))
			else
				e1->value.cell.car = _COPY(e)
			end if

			if( first = NULL ) then
				first = e1
			end if
			if( prev <> NULL ) then
				prev->value.cell.cdr = e1
			end if
			prev = e1

			'' next element
			if( _IS_CONS(e) ) then
				e = _CDR(e)
			else
				e = p1
				exit while
			end if
		wend

		'' next argument
		p = _CDR(p)
	wend

	if( first = NULL ) then
		first = _NIL_
	else
		function = first
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (apply function [list])
''
define_lisp_function( apply, args )

	_OBJ(f) = _EVAL(_CAR(args))
	_OBJ(a) = _CAR(_CDR(args))

	'' function name?
	if( f = _NIL_ ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
		exit function
	end if

	'' is identifier?
	if( _IS_IDENTIFIER(f) = FALSE ) then
		_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
		function = _NIL_
		exit function
	end if

	'' execute
	function = ctx->evaluator->execute( f->value.id, _EVAL(a) )

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (length <list>)
''
define_lisp_function( length, args )

	_OBJ(p) = args
	_OBJ(p1) = any

	dim count as integer = 0

	'' no arguments?
	if( p = _NIL_ ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
		exit function
	end if

	'' more than one argument?
	p1 = _CDR(args)
	if( p1 <> _NIL_ ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
		exit function
	end if

	'' first argument
	p = _EVAL(_CAR(p))

	'' not nil?
	if( p <> _NIL_ ) then

		'' not a list?
		if( _IS_CONS( p ) = FALSE ) then
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			function = _NIL_
			exit function
		end if

		'' count the elements
		while( _IS_CONS( p ) )
			count += 1
			p = _CDR(p)
		wend
	
	end if

	function = _NEW_INTEGER( count )

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (nth <index> <list>)
''
define_lisp_function( nth, args)

	if( _LENGTH(args) <> 2 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else

		_OBJ(p1) = _EVAL(_CAR(args))
		_OBJ(p2) = _EVAL(_CAR(_CDR(args)))

		if( _IS_INTEGER(p1) ) then
			dim i as integer = p1->value.int
			if( i < 0 ) then
				_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			else
				if( p2 = _NIL_ ) then
					function = _NIL_
				elseif( _IS_CONS(p2) ) then
					while( p2 <> _NIL_ and i > 0 )
						p2 = _CDR(p2)
						i -= 1
					wend
					if( p2 <> _NIL_ ) then
						function = _CAR(p2)
					else
						function = _NIL_
					end if
				else
					_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
					function = _NIL_
				end if
			end if
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			function = _NIL_
		end if
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (elt <list> <index>)
''
define_lisp_function( elt, args)

	if( _LENGTH(args) <> 2 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else

		_OBJ(p2) = _EVAL(_CAR(args))
		_OBJ(p1) = _EVAL(_CAR(_CDR(args)))

		if( _IS_INTEGER(p1) ) then
			dim i as integer = p1->value.int
			if( i < 0 ) then
				_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			else
				if( p2 = _NIL_ ) then
					function = _NIL_
				elseif( _IS_CONS(p2) ) then
					while( p2 <> _NIL_ and i > 0 )
						p2 = _CDR(p2)
						i -= 1
					wend
					if( p2 <> _NIL_ ) then
						function = _CAR(p2)
					else
						function = _NIL_
					end if
				else
					_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
					function = _NIL_
				end if
			end if
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			function = _NIL_
		end if
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (last <list>)
''
define_lisp_function( last, args)

	if( _LENGTH(args) <> 1 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else
		_OBJ(p) = _EVAL(_CAR(args))
		if( p = _NIL_ ) then
			function = _NIL_
		else
			dim i as integer = _LENGTH(p) - 1
			if( i >= 0 ) then
				if( _IS_CONS(p) ) then
					while( p <> _NIL_ and i > 0 )
						p = _CDR(p)
						i -= 1
					wend
					if( p <> _NIL_ ) then
						function = _CAR(p)
					else
						function = _NIL_
					end if
				else
					_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
					function = _NIL_
				end if
			else
				function = _NIL_
			end if
		end if
	end if

end_lisp_function()

'' ---------------------------------------------------------------------------
'' (mapcar <function> <list1...listn>)
''
define_lisp_function( mapcar, args)

	dim n as integer = _LENGTH(args) - 1
	dim l as integer = 0

	if( n <= 0 ) then
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )
		function = _NIL_
		exit function
	end if

	_OBJ(f) = _EVAL(_CAR(args))
	_OBJ(a) = _CDR(args)
	
	'' function name?
	if( f = _NIL_ ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
		exit function
	end if

	'' is identifier?
	if( _IS_IDENTIFIER(f) = FALSE ) then
		_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
		function = _NIL_
		exit function
	end if

	'' Create an object array to store each list argument
	'' and determine the length of the shortest list
	_OBJ(p)(0 to n-1)

	for i as integer = 0 to n-1
		p(i) = _EVAL(_CAR(a))
		a = _CDR(a)
		if( i = 0 ) then
			l = _LENGTH(p(i))
		else
			dim tmplen as integer = _LENGTH(p(i))
			if( tmplen < l ) then
				l = tmplen
			end if
		end if
	next

	'' Shortest list has no elements?
	if( l <= 0 ) then
		function = _NIL_
		exit function
	end if

	'' at this point we know
	'' - we have n lists (from args)
	'' - we have to use l elements from each list

	'' the results
	_OBJ(res_first) = NULL
	_OBJ(res_prev) = NULL
	_OBJ(res_p1)

	'' get the j'th element from the i'th list

	'' iterate through to the length of the lists
	for j as integer = 0 to l-1

		_OBJ(first) = NULL
		_OBJ(prev) = NULL
		_OBJ(p1)

		'' iterate through each list and build a (temporary) argument
		'' list to pass to the function
		for i as integer = 0 to n-1
			p1 = _NEW( OBJECT_TYPE_CONS )
			p1->value.cell.car = _EVAL(_CAR( p(i) ))
			if( first = NULL ) then
				first = p1
			end if
			if( prev <> NULL ) then
				prev->value.cell.cdr = p1
			end if
			prev = p1
		next

		'' call the function
		_OBJ(res1) = ctx->evaluator->execute( f->value.id, first ) 
		
		'' append results to res
		res_p1 = _NEW( OBJECT_TYPE_CONS )
		res_p1->value.cell.car = res1
		if( res_first = NULL ) then
			res_first = res_p1
		end if
		if( res_prev <> NULL ) then
			res_prev->value.cell.cdr = res_p1
		end if
		res_prev = res_p1

		'' next element
		for i as integer = 0 to n-1
			p(i) = _CDR(p(i))
		next
	next

	if( res_first = NULL ) then
		function = _NIL_
	else
		function = res_first
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


'' ---------------------------------------------------------------------------
''
sub bind_intrinsic_funcs3( byval functions as LISP_FUNCTIONS ptr )

	BIND_FUNC( functions, "append", append )
	BIND_FUNC( functions, "apply", apply )
	BIND_FUNC( functions, "length", length )
	BIND_FUNC( functions, "nth", nth )
	BIND_FUNC( functions, "elt", elt )
	BIND_FUNC( functions, "last", last )
	BIND_FUNC( functions, "mapcar", mapcar )

	BIND_FUNC( functions, "listp", listp )
	BIND_FUNC( functions, "consp", consp )
	BIND_FUNC( functions, "integerp", integerp )
	BIND_FUNC( functions, "numberp", numberp )
	BIND_FUNC( functions, "zerop", zerop )
	BIND_FUNC( functions, "stringp", stringp )


	BIND_FUNC( functions, "=", eq_atom )
	BIND_FUNC( functions, "<", lt_atom )
	BIND_FUNC( functions, ">", gt_atom )
	BIND_FUNC( functions, "/=", ne_atom )
	BIND_FUNC( functions, ">=", ge_atom )
	BIND_FUNC( functions, "<=", le_atom )

end sub

end namespace
