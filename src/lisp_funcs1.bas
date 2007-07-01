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
 *
 *
 * Copyright (c) 1997-2001 Sandro Sigala.  All rights reserved.
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
'' (quote expr)
''
define_lisp_function( quote, args )
	function = _CAR( args )
end_lisp_function

'' ---------------------------------------------------------------------------
'' (car expr)
''
define_lisp_function( car, args )
	function = _CAR( _EVAL( _CAR( args ) ) )
end_lisp_function

'' ---------------------------------------------------------------------------
'' (cdr expr)
''
define_lisp_function( cdr, args )
	function = _CDR( _EVAL( _CAR( args )))
end_lisp_function

'' ---------------------------------------------------------------------------
'' (eval expr)
''
define_lisp_function( eval, args )
	function = _EVAL(_EVAL(_CAR( args )))
end_lisp_function

'' ---------------------------------------------------------------------------
'' (+ expr... )
''
define_lisp_function( add, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(res) = any

	res = _NEW( OBJECT_TYPE_INTEGER )

	do
		p1 = _EVAL(_CAR(p))
		if( _IS_NUMBER(p1) ) then
			*res += *p1
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			exit do
		end if
		p = _CDR(p)
	loop while( p <> _NIL_ )
	
	function = res

end_lisp_function

'' ---------------------------------------------------------------------------
'' (- expr... )
''
define_lisp_function( sub, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(res) = any

	res = _NEW( OBJECT_TYPE_INTEGER )

	do
		p1 = _EVAL(_CAR(p))
		if( _IS_NUMBER(p1) ) then
			if( (p = args) and (_CDR(p) <> _NIL_)) then
				res->dtype = p1->dtype
				res->value = p1->value
			else
				*res -= *p1
			end if
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			exit do
		end if
		p = _CDR(p)
	loop while( p <> _NIL_ )
	
	function = res

end_lisp_function

'' ---------------------------------------------------------------------------
'' (* expr... )
''
define_lisp_function( mul, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(res) = any

	res = _NEW( OBJECT_TYPE_INTEGER )

	do
		p1 = _EVAL(_CAR(p))
		if( _IS_NUMBER(p1) ) then
			if(p = args) then
				res->dtype = p1->dtype
				res->value = p1->value
			else
				*res *= *p1
			end if
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			exit do
		end if
		p = _CDR(p)
	loop while( p <> _NIL_ )
	
	function = res

end_lisp_function

'' ---------------------------------------------------------------------------
'' (/ expr... )
''
define_lisp_function( div, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(res) = any

	res = _NEW( OBJECT_TYPE_INTEGER )

	do
		p1 = _EVAL(_CAR(p))
		if( _IS_NUMBER(p1) ) then
			if(p = args) then
				res->dtype = p1->dtype
				res->value = p1->value
			else
				if( p1->IsZero() ) then
					_RAISEERROR( LISP_ERR_DIVISION_BY_ZERO )
				else
					*res /= *p1
				end if
			end if
		else
			_RAISEERROR( LISP_ERR_INVALID_ARGUMENT )
			exit do
		end if
		p = _CDR(p)
	loop while( p <> _NIL_ )
	
	function = res

end_lisp_function

'' ---------------------------------------------------------------------------
'' (progn expr... )
''
define_lisp_function( progn, args )

	_OBJ(p) = args
	_OBJ(p1) = any

	do
		p1 = _EVAL(_CAR(p))
		p = _CDR(p)
	loop while ( p <> _NIL_ )

	function = p1

end_lisp_function

'' ---------------------------------------------------------------------------
'' (set name value)
''
define_lisp_function( set, args )

	_OBJ(p1) = _EVAL(_CAR(args))
	_OBJ(p2) = _EVAL(_CAR(_CDR(args)))

	if( p1 = _NIL_ ) then
		_RAISEERROR( LISP_ERR_SETTING_VALUE_OF_NIL_OBJECT )
	else
		_SET( p1, p2 )
	end if

	function = p2

end_lisp_function

'' ---------------------------------------------------------------------------
'' (setq name value...)
'' (setf name value...)
''
define_lisp_function( setq, args )

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any

	do
		p1 = _CAR(p)
		p2 = _EVAL(_CAR(_CDR(p)))
		_SET( p1, p2 )
		p = _CDR(_CDR(p))
	loop while ( p <> _NIL_ )

	function = p2

end_lisp_function

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

end_lisp_function


'' ---------------------------------------------------------------------------
'' (and expr...)
''
define_lisp_function( and, args)

	_OBJ(p) = args
	_OBJ(p1) = any

	do
		p1 = _EVAL(_CAR(p))
		if( p1 = _NIL_ ) then
			function = _NIL_
			exit function
		end if
		p = _CDR(p)
	loop while ( p <> _NIL_ )

	function = p1

end_lisp_function

'' ---------------------------------------------------------------------------
'' (or expr...)
''
define_lisp_function( or, args)

	_OBJ(p) = args
	_OBJ(p1) = any

	do
		p1 = _EVAL(_CAR(p))
		if( p1 = _NIL_ ) then
			function = p1
			exit function
		end if
		p = _CDR(p)
	loop while ( p <> _NIL_ )

	function = _NIL_

end_lisp_function

'' ---------------------------------------------------------------------------
'' (not expr)
'' (null expr)
''
define_lisp_function( not, args)

	_OBJ(p) = _EVAL(_CAR(args))

	if( p <> _NIL_ ) then
		function = _NIL_
	else
		function = _T_
	end if

end_lisp_function

'' ---------------------------------------------------------------------------
'' (cond (expr1 [expr2])...)
''
define_lisp_function( cond, args)

	_OBJ(p) = args
	_OBJ(p1) = any
	_OBJ(p2) = any
	_OBJ(p3) = any

	do
		p1 = _CAR(p)
		p2 = _EVAL(_CAR(p1))
		if( p2 <> _NIL_ ) then
			p3 = _CDR(p1)
			if( p3 <> _NIL_ ) then
				function = _CALL( progn, p3 )
			else
				function = p2
			end if
			exit function
		end if
		p = _CDR(p)
	loop while (p <> _NIL_)

	function = _NIL_

end_lisp_function

'' ---------------------------------------------------------------------------
'' (if expr then-expr else-expr...)
''
define_lisp_function( if, args)

	_OBJ(p1) = _CAR(args)
	_OBJ(p2) = _CAR(_CDR(args))
	_OBJ(p3) = _CDR(_CDR(args))

	if( _EVAL(p1) <> _NIL_ ) then
		function = _EVAL(p2)
	else
		function = _CALL( progn, p3 )
	end if

end_lisp_function

'' ---------------------------------------------------------------------------
'' (when expr then-expr...)
''
define_lisp_function( when, args )

	_OBJ(p1) = _CAR(args)
	_OBJ(p2) = _CDR(args)

	if( _EVAL(p1) <> _NIL_ ) then
		function = _CALL( progn, p2 )
	else
		function = _NIL_
	end if

end_lisp_function

'' ---------------------------------------------------------------------------
'' (unless expr else-expr...)
''
define_lisp_function( unless, args)

	_OBJ(p1) = _CAR(args)
	_OBJ(p2) = _CDR(args)

	if( _EVAL(p1) = _NIL_ ) then
		function = _CALL(progn, p2 )
	else
		function = _NIL_
	end if

end_lisp_function

'' ---------------------------------------------------------------------------
'' (while expr exprs...)
''
define_lisp_function( while, args )

	_OBJ(p1) = _CAR(args)
	_OBJ(p2) = _CDR(args)

	while (_EVAL(p1) <> _NIL_)
		_CALL( progn, p2)
	wend

	function = _NIL_

end_lisp_function

'' ---------------------------------------------------------------------------
'' (cons expr1 expr2)
''
define_lisp_function( cons, args)

	_OBJ(p) = any

	p = _NEW( OBJECT_TYPE_CONS )

	p->value.cell.car = _EVAL(_CAR(args))
	p->value.cell.cdr = _EVAL(_CAR(_CDR(args)))

	function = p

end_lisp_function

'' ---------------------------------------------------------------------------
'' (list expr1...)
''
define_lisp_function( list, args)

	_OBJ(p) = args
	_OBJ(first) = NULL
	_OBJ(prev) = NULL
	_OBJ(p1)

	if( p = _NIL_ ) then
		function = _NIL_
		exit function
	end if

	do
		p1 = _NEW( OBJECT_TYPE_CONS )
		p1->value.cell.car = _EVAL(_CAR(p))
		if( first = NULL ) then
			first = p1
		end if
		if( prev <> NULL ) then
			prev->value.cell.cdr = p1
		end if
		prev = p1
		p = _CDR(p)
	loop while (p <> _NIL_ )

	function = first

end_lisp_function

'' ---------------------------------------------------------------------------
'' (defun name (arglist) expr...)
''
'' returns the name of the function defined
''
define_lisp_function( defun, args )

	_OBJ(p1) = _CAR(args)			'' name
	_OBJ(p2) = _CAR(_CDR(args))		'' argument list
	_OBJ(p3) = _CDR(_CDR(args))		'' function body
	_OBJ(lexpr) = any				'' the defined lambda expressin

	lexpr = _NEW(OBJECT_TYPE_CONS)
	lexpr->value.cell.car = _NEW(OBJECT_TYPE_IDENTIFIER)
	lexpr->value.cell.car->value.id = lisp.strdup("lambda")
	lexpr->value.cell.cdr = _NEW(OBJECT_TYPE_CONS)
	lexpr->value.cell.cdr->value.cell.car = p2
	lexpr->value.cell.cdr->value.cell.cdr = p3

	_SET(p1, lexpr)

	'' FIXME: allow overriding built-ins

	function = p1

end_lisp_function

'' ---------------------------------------------------------------------------
'' (eq <expr1> <expr2>)
''
define_lisp_function( eq, args )


	_OBJ(p1) = _EVAL(_CAR(args))
	_OBJ(p2) = _EVAL(_CAR(_CDR(args)))

	function = _NIL_

	if (p1 = p2) then
		function = _T_

	elseif( _IS_CONS(p1) or _IS_CONS(p2) ) then
		''

	elseif( p1->dtype = p2->dtype ) then

		select case p1->dtype
		case OBJECT_TYPE_IDENTIFIER
			if( *p1->value.id = *p2->value.id ) then
				function = _T_
			end if

		case OBJECT_TYPE_INTEGER
			if( p1->value.int = p2->value.int ) then
				function = _T_
			end if

		case OBJECT_TYPE_REAL
			if( p1->value.flt = p2->value.flt ) then
				function = _T_
			end if

		case OBJECT_TYPE_STRING
			if( *p1->value.str = *p2->value.str ) then
				function = _T_
			end if

		end select

	end if

end_lisp_function


'' ---------------------------------------------------------------------------
''
sub bind_intrinsic_funcs1( byval functions as LISP_FUNCTIONS ptr )

	BIND_FUNC( functions, "quote", quote )
	BIND_FUNC( functions, "car", car )
	BIND_FUNC( functions, "cdr", cdr)
	BIND_FUNC( functions, "+", add )
	BIND_FUNC( functions, "-", sub )
	BIND_FUNC( functions, "*", mul )
	BIND_FUNC( functions, "/", div )
	BIND_FUNC( functions, "progn", progn )
	BIND_FUNC( functions, "set", set )
	BIND_FUNC( functions, "setq", setq )
	BIND_FUNC( functions, "setf", setq )
	BIND_FUNC( functions, "eval", eval )
	BIND_FUNC( functions, "atom", atom )
	BIND_FUNC( functions, "and", and )
	BIND_FUNC( functions, "or", or )
	BIND_FUNC( functions, "not", not )
	BIND_FUNC( functions, "null", not )
	BIND_FUNC( functions, "if", if )
	BIND_FUNC( functions, "cond", cond )
	BIND_FUNC( functions, "unless", unless )
	BIND_FUNC( functions, "when", when )
	BIND_FUNC( functions, "while", while )
  	BIND_FUNC( functions, "cons", cons )
	BIND_FUNC( functions, "list", list )
	BIND_FUNC( functions, "defun", defun )
	BIND_FUNC( functions, "eq", eq )

end sub
