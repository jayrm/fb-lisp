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

dim shared gQuit as boolean = false

'' --------------------------------------------------------

#include once "lisp_runtime.bi"

namespace LISP

'' --------------------------------------------------------
'' (quit)
''
define_lisp_function( quit, args )

	if( _LENGTH(args) <> 0 ) then
		_RAISEERROR( LISP_ERR_WRONG_NUMBER_OF_ARGUMENTS )
		function = _NIL_
	else
		gQuit = true
	end if

	function = _T_

end_lisp_function()

end namespace

'' --------------------------------------------------------
'' MAIN
'' --------------------------------------------------------

#include once "lisp.bi"

using LISP
dim lsp as LispModule
dim result as LISP_ERROR

BIND_FUNC( lsp.functions, "quit", quit )

print "LISP DEMO"
print "---------"
print
print "Enter LISP expressions at the prompt."
print
print "To exit type '(quit)'"
print

dim expr as string

do				  
	print "lisp > ";
	line input expr

	if( lcase(trim(expr)) = "quit" ) then
		exit do
	
	elseif( trim(expr) > "" ) then

		'' pass the LISP code to the evaluator
		result = lsp.Eval( expr )

		'' check for an error
		if( result <> LISP_ERR_SUCCESS ) then
			print "Error " & lsp.ErrorCode
			print "  " & lsp.ErrorText
		end if

	end if

loop while gQuit = false
