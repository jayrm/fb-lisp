'' --------------------------------------------------------
'' HELLO.BAS
'' --------------------------------------------------------
''
'' The smallest possible example that make use of the LISP
'' evaluator.
''
'' compile with: 
''    fbc hello.bas -i ../inc -p ../lib
''
'' --------------------------------------------------------

#include once "lisp.bi

sub main
	dim lsp as LispModule

	lsp.eval $"(princ ""Hello World\n"")" 
end sub

main