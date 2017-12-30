# GNU C++/C Compilers
CXXC = g++
CC   = gcc

CWD = $(shell pwd)

SRCLOC = ./src
LNLOC  = ./linenoise
INCLUDES = $(CWD)/$(LNLOC)

SRCS	= \
	$(SRCLOC)/escheme.cxx \
	$(SRCLOC)/rep.cxx \
	$(SRCLOC)/error.cxx \
	$(SRCLOC)/tstack.cxx \
	$(SRCLOC)/argstack.cxx \
	$(SRCLOC)/regstack.cxx \
	$(SRCLOC)/intstack.cxx \
	$(SRCLOC)/reader.cxx \
	$(SRCLOC)/printer.cxx \
	$(SRCLOC)/symtab.cxx \
	$(SRCLOC)/eval.cxx \
	$(SRCLOC)/eceval.cxx \
	$(SRCLOC)/funtab.cxx \
	$(SRCLOC)/func.cxx \
	$(SRCLOC)/math.cxx \
	$(SRCLOC)/memory.cxx \
	$(SRCLOC)/pio.cxx \
	$(SRCLOC)/tio.cxx \
	$(SRCLOC)/ipcsoc.cxx \
	$(SRCLOC)/sexpr.cxx

OBJS	= $(SRCS:.cxx=.o)

#DEBUG_FLAGS = -g
DEBUG_FLAGS = -O

#CFLAGS = $(DEBUG_FLAGS) -fno-operator-names -fpermissive -std=c++11
#CFLAGS = $(DEBUG_FLAGS) -Wall -Wextra -std=c++11
CFLAGS = $(DEBUG_FLAGS) -pedantic -std=c++11
LFLAGS = $(DEBUG_FLAGS) -v -lm

all 	: escheme

escheme	: $(OBJS) $(LNLOC)/linenoise.o
	$(CXXC) -o escheme $(OBJS) $(LNLOC)/linenoise.o $(LFLAGS)
	rm $(SRCLOC)/*.o
	rm $(LNLOC)/*.o

%.o	: %.cxx
	$(CXXC) -I$(INCLUDES) -c $(CFLAGS) $< -o $@

$(LNLOC)/linenoise.o : $(LNLOC)/linenoise.c
	gcc -Wall -W -Os -c $< -o $@

clean 	:
	rm -f $(SRCLOC)/*.o $(LNLOC)/*.o *~ escheme

