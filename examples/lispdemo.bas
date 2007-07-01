'' --------------------------------------------------------
'' LISPDEMO.BAS
'' --------------------------------------------------------
''
'' This is an interactive demo that allows typing LISP
'' expression at the prompt
''
'' This example needs access to the header files found in
'' the source directory.
''
'' --------------------------------------------------------

dim shared gQuit as integer = 0

''
'' Load a text file from disk as a string
''
function LoadFileAsString( byref filename as string ) as string

	dim x as string

	if( open( filename for input access read as #1 ) = 0 ) then
		close #1
		if( open( filename for binary access read as #1 ) = 0 ) then
			x = space( lof( 1 ))
			get #1,,x
			close #1
		else
			print "Unable to open '" & filename & "'"
		end if 
	else
		print "File not found '" & filename & "'"
	end if

	function = x

end function

'' --------------------------------------------------------

#include once "lisp_runtime.bi"

'' --------------------------------------------------------
'' (load <filename>)
''
define_lisp_function( load, args )

	dim filename as string, text as string
	
	_OBJ(p1) = _EVAL(_CAR(args))	'' filename

	if( _IS_STRING( p1 ) ) then
		
		filename = *p1->value.str

		text = LoadFileAsString( "./lisp/" & filename & ".lsp" )

		if( text > "" ) then
			function = ctx->evaluator.eval( text )
		else
			'' FIXME: raise user defined error
			function = _NIL_
		end if

	else
		_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
		function = _NIL_

	end if

end_lisp_function

'' --------------------------------------------------------
'' (quit)
''
define_lisp_function( quit, args )

	dim filename as string, text as string
	
	_OBJ(p1) = _EVAL(_CAR(args))	'' filename

	if( _IS_STRING( p1 ) ) then
		
		filename = *p1.value.str

		text = 



		res = _NEW(OBJECT_TYPE_REAL)
		res->value.flt = sqr( ( *p3 - *p1 ) ^ 2 + ( *p4 - *p2 ) ^ 2 )

		function = ctx->evaluator( text )

	else
		_RAISEERROR( LISP_ERR_ARGUMENT_TYPE_MISMATCH )
		function = _NIL_

	end if

end_lisp_function


'' --------------------------------------------------------
'' MAIN
'' --------------------------------------------------------

#include once "lisp.bi"

dim lsp as LispModule

BIND_FUNC( lsp.functions, "load", load )
BIND_FUNC( lsp.functions, "quit", quit )

print "LISP DEMO"
print "---------"
print
print "Enter LISP expressions at the prompt."
print
print "To exit type 'quit'."
print

dim expr as string

do				  
	print "lisp > ";
	line input expr

	if( lcase(trim(expr)) = "quit" ) then
		exit do
	
	elseif( trim(expr) > "" ) then

		'' pass the LISP code to the evaluator
		result = lsp.Eval( code )

		'' check for an error
		if( result <> LISP_ERR_SUCCESS ) then
			print "Error " & lsp.ErrorCode & " on line " & lsp.ErrorLine & " of '" & filename & "'"
			print "  " & lsp.ErrorText
			exit while
		end if

	end if

loop while gQuit = 0
