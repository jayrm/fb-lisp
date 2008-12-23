'' --------------------------------------------------------
'' PRINTOUT.BAS
'' --------------------------------------------------------
''
'' Shows how to redirect output to a call-back procedure
'' using SetPrintCallBack() instead of using FB's built-in
'' PRINT statement for output.
''
'' compile with: 
''    fbc printout.bas -i ../inc -p ../lib
''
'' --------------------------------------------------------

#include once "lisp.bi"

sub MyPrintOut( byref s as const string )
	print "**"
	print s
	print "**"
end sub

dim lsp as LISP.LispModule

'' Preserve current call back
dim last_cb as LISP.LISP_PRINT_CALLBACK
last_cb = lsp.GetPrintCallBack()

'' Set a new print handler
lsp.SetPrintCallBack( procptr(MyPrintOut) )

'' Try it out
lsp.eval $"(princ ""Hello World\n"")" 

'' Restore the previous print handler
lsp.SetPrintCallBack( last_cb )
lsp.eval $"(princ ""All Done\n"")" 
