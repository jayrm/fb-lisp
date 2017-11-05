FBC := fbc.exe

LIB := ../lib/liblisp.a

ifdef DEBUG
FBCFLAGS += -g -d DEBUG -w pedantic -exx
endif

all : hello.exe bindfunc.exe runlisp.exe printout.exe lispdemo.exe $(LIB)

bindfunc.exe : bindfunc.bas $(LIB)
	$(FBC) $(FBCFLAGS) bindfunc.bas -i ../src -i ../inc -p ../lib 

runlisp.exe : runlisp.bas $(LIB)
	$(FBC) $(FBCFLAGS) runlisp.bas -i ../inc -p ../lib 

hello.exe : hello.bas $(LIB)
	$(FBC) $(FBCFLAGS) hello.bas -i ../inc -p ../lib

printout.exe : printout.bas $(LIB)
	$(FBC) $(FBCFLAGS) printout.bas -i ../src -i ../inc -p ../lib

lispdemo.exe : lispdemo.bas $(LIB)
	$(FBC) $(FBCFLAGS) lispdemo.bas -i ../src -i ../inc -p ../lib

$(LIB) :
	cd .. && make DEBUG=1

.PHONY : clean
clean:
	rm -f hello.exe bindfunc.exe runlisp.exe printout.exe lispdemo.exe
