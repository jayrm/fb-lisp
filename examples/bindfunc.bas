'' --------------------------------------------------------
'' BINDFUNC.BAS
'' --------------------------------------------------------
''
'' This shows how to bind a function written in FreeBASIC
'' for use in the LISP evaluator.
''
'' compile with: 
''    fbc bindfunc.bas -i ../src -i ../inc -p ../lib
''
'' This example needs access to the header files found in
'' the source directory.
''
'' --------------------------------------------------------

#include once "lisp_runtime.bi"

'' --------------------------------------------------------
'' (distance x1 y1 x2 y2)
''
define_lisp_function( distance, args )

	_OBJ(p1) = _EVAL(_CAR(args))					'' 1st arg
	_OBJ(p2) = _EVAL(_CAR(_CDR(args)))				'' 2nd arg
	_OBJ(p3) = _EVAL(_CAR(_CDR(_CDR(args))))		'' 3rd arg
	_OBJ(p4) = _EVAL(_CAR(_CDR(_CDR(_CDR(args)))))	'' 4th arg
	_OBJ(res) = any									'' results

	if( _LENGTH(args) < 4 ) then
		_RAISEERROR( LISP_ERR_TOO_FEW_ARGUMENTS )
	end if

	if( _IS_NUMBER( p1 ) _
		and _IS_NUMBER( p2 ) _
		and _IS_NUMBER( p3 ) _
		and _IS_NUMBER( p4 ) ) then

		res = _NEW(OBJECT_TYPE_REAL)
		res->value.flt = sqr( ( *p3 - *p1 ) ^ 2 + ( *p4 - *p2 ) ^ 2 )

		function = res

	else
		function = _NIL_

	end if

end_lisp_function


'' --------------------------------------------------------
'' MAIN
'' --------------------------------------------------------

#include once "lisp.bi"

dim lsp as LispModule

''
'' bind the function named "distance" to the FreeBASIC
'' function we defined
''
BIND_FUNC( lsp.functions, "distance", distance )

with lsp
	.eval $"(setq x1 3)" 
	.eval $"(setq y1 0)" 
	.eval $"(setq x2 0)" 
	.eval $"(setq y2 4)" 
	.eval $"(setq d (distance x1 y1 x2 y2))" 
	.eval $"(princ ""The distance from ("" x1 "","" y1 "")"" )"
	.eval $"(princ "" to ("" x2 "","" y2 "")"" )"
	.eval $"(princ "" is "" d ""\n"" )"
end with
