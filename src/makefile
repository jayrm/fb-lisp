##########################

LIBNAME := lisp

SRCS := lisp.bas 
SRCS += lisp_int.bas 
SRCS += lisp_ctx.bas 
SRCS += lisp_lexer.bas 
SRCS += lisp_object.bas 
SRCS += lisp_objects.bas 
SRCS += lisp_parser.bas 
SRCS += lisp_funcs.bas
SRCS += lisp_eval.bas
SRCS += lisp_funcs1.bas
SRCS += lisp_funcs2.bas
SRCS += lisp_funcs3.bas

HDRS := $(wildcard *.bi) $(wildcard ../inc/lisp_*.bi)

OBJS := $(patsubst %.bas,obj/%.o,$(SRCS))

##########################

FBC := fbc.exe
FBCFLAGS := -i ../inc

ifdef DEBUG
FBCFLAGS += -g -d DEBUG -w pedantic
endif

.SUFFIXES:
.SUFFIXES: .bas .bi

VPATH = .

##########################

all: ../lib/lib$(LIBNAME).a

obj/%.o : %.bas $(HDRS)
	$(FBC) $(FBCFLAGS) -c $< -o $@

../lib/lib$(LIBNAME).a: $(OBJS)
	$(FBC) -lib $(FBCFLAGS) $(OBJS) $(LIBS) $(RCS) -x ../lib/$(LIBNAME)

.PHONY : clean
clean:
	-rm -f $(OBJS) $(APP)
