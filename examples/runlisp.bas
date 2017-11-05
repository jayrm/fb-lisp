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
''    runlisp
''    runlisp lisp/hello.lsp
''
'' --------------------------------------------------------

#include once "lisp.bi"
using LISP

''
'' Load a text file from disk as a string
''
function LoadFileAsString _
	( _
		byref filename as const string, _
		byref expr as string _
	) as boolean

	dim x as string
	expr = ""

	function = false

	if( open( filename for input access read as #1 ) = 0 ) then
		close #1
		if( open( filename for binary access read as #1 ) = 0 ) then
			x = space( lof( 1 ))
			get #1,,x
			close #1
			expr = x
			function = true
		else
			print "Unable to open '" & filename & "'"
		end if 
	else
		print "File not found '" & filename & "'"
	end if

	

end function

'' --------------------------------------------------------
'' MAIN	PROGRAM
'' --------------------------------------------------------
''

dim shared opt_nfiles as integer = 0
dim shared opt_files() as string
dim shared opt_quit as boolean = false
dim shared opt_help as boolean = false
dim shared opt_interactive as boolean = false
dim shared opt_verbose as boolean = false
dim shared opt_trace as boolean = false

''
sub optAddLispFile( byref filename as const string )

	opt_nfiles += 1
	if( opt_nfiles = 1 ) then
		redim opt_files( 1 to opt_nfiles ) as string
	else
		redim preserve opt_files( 1 to opt_nfiles ) as string
	end if
	opt_files( opt_nfiles ) = filename

end sub

''
sub ShowHelp()

	print "runlisp [options] [file1.lsp] [file2.lsp]..."
	print "options:"
	print "   -h, --help         Show help information"
	print "   -i, --interactive  Show interactive lisp prompt"
	print "   -q, --quit         Quit after processing"
	print "   -v, --verbose      Be more verbose"
	print "   -t, --trace        Trace execution when parsing files"
	print

end sub

''
sub GetCommandLineOptions()

	dim i as integer = 1
	while( command(i) > "" )
		if( left( command(i), 1 ) = "-" ) then
			select case command(i)
			case "-h", "--help"
				opt_help = true
			case "-q", "--quit"
				opt_quit = true
			case "-i", "--interactive"
				opt_interactive = true
			case "-v", "--verbose"
				opt_verbose = true
			case "-t", "--trace"
				opt_trace = true
			case else
				print "Unrecogzied option '" & command(i) & "'"
				end 1
			end select
		else
			optAddLispFile( command(i) )
		end if
		i += 1
	wend
	
	'' no command line options, then set the show help option
	if( command(1) = "" ) then
		opt_help = true
	end if

	if( opt_quit andalso opt_interactive ) then	
		print "Warning: --quit overrides --interactive"
	end if

	'' if no files given, then assume interactive
	if( opt_nfiles = 0 ) then
		opt_interactive = true

	'' else if --quit option not given, then assume interactive
	elseif( opt_quit = false )then
		opt_interactive = true
		
	end if

end sub

''
function InteractivePrompt _
	( _
		byref lsp as LispModule _
	) as boolean


	dim expr as string
	dim result as LISP_ERROR

	function = false

	'' Turn on echo
	lsp.EchoInput = 1
	lsp.ShowResults = 1

	print
	print "To exit type 'quit'"
	print

	do
		print "lisp: ";
		line input expr

		if( lcase(trim(expr)) = "quit" ) then
			function = true
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

end function

''
function ExecuteLispFile _
	( _
		byref lsp as LispModule, _
		byref filename as const string _
	) as boolean


	dim expr as string
	dim result as LISP_ERROR

	function = false

	if( opt_verbose ) then
		print "Loading '" & filename & "'"
	end if

	'' load the file
	if( LoadFileAsString( filename, expr ) = false ) then
		exit function
	end if

	'' pass the LISP code to the evaluator
	result = lsp.Eval( expr )

	'' check for an error
	if( result <> LISP_ERR_SUCCESS ) then
		print "Error " & lsp.ErrorCode & " on line " & lsp.ErrorLine & " of '" & filename & "'"
		print "  " & lsp.ErrorText
	else
		function = true
	end if

end function

''
sub main

	GetCommandLineOptions()

	if( opt_help ) then
		ShowHelp()
		end 0
	end if

	'' Create an instance of the LispModule object
	dim lsp as LispModule

	'' Turn on display so we can see what's happening
	if( opt_trace ) then
		lsp.EchoInput = 1
		lsp.ShowResults = 1
	end if

	'' Loop through each file given on the command line
	'' and pass it over to the LISP evaluator
	for i as integer = 1 to opt_nfiles
		if( ExecuteLispFile( lsp, opt_files(i) ) = false ) then
			exit for
		end if
	next

	'' Start the interactive loop if --interactive option was given
	if( opt_interactive = true ) then
		InterActivePrompt( lsp )
	end if

end sub

main()
