'' --------------------------------------------------------
'' RUNLISP.BAS
'' --------------------------------------------------------
''
'' This example provides a simple command line interface
'' for loading and evaluating LISP code stored as text
'' files on disk.  After the files have been evalulated,
'' a lisp prompt is given allowing lisp expressions to
'' entered directly.
''
'' compile with: 
''    fbc runlisp.bas -i ../inc -p ../lib
''
'' usage:
''    runlisp file1.lsp [file2.lsp]...
''
'' example:
''    runlisp lisp/hello.lsp
''
'' --------------------------------------------------------

#include once "lisp.bi"

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
'' MAIN	PROGRAM
'' --------------------------------------------------------
''
sub main
	''
	'' Check for at least one filename on the command line
	'' otherwise print the usage
	''
	if( command(1) = "" ) then
		print "runlisp [file1.lsp] [file2.lsp]..."
		end
	end if

	dim expr as string
	dim i as integer

	''
	'' Create an instance of the LispModule object
	''
	dim lsp as LispModule
	dim result as LISP_ERROR
	dim filename as string

	'' Turn on display so we can see what's happening
	'' lsp.EchoInput = 1
	'' lsp.ShowResults = 1

	''
	'' Loop through each file on the command line
	'' and pass it over to the LISP evaluator
	''
	i = 1
	while command(i) <> ""

		filename = command(i)

		'' load the file
		expr = LoadFileAsString( filename )

		'' pass the LISP code to the evaluator
		result = lsp.Eval( expr )

		'' check for an error
		if( result <> LISP_ERR_SUCCESS ) then
			print "Error " & lsp.ErrorCode & " on line " & lsp.ErrorLine & " of '" & filename & "'"
			print "  " & lsp.ErrorText
			exit while
		end if

		i += 1
	wend

	'' --------------------------------------------------------

	'' Turn on echo
	lsp.EchoInput = 1
	lsp.ShowResults = 1

	print
	print "To exit type 'quit'"
	print

	do
		print "lisp>";
		line input expr

		if( lcase(trim(expr)) = "quit" ) then
			exit do
		
		elseif( trim(expr) > "" ) then

			'' pass the LISP expr to the evaluator
			result = lsp.Eval( expr )

			'' check for an error
			if( result <> LISP_ERR_SUCCESS ) then
				print "Error " & lsp.ErrorCode
				print "  " & lsp.ErrorText
			end if

		end if

	loop

end sub

main
