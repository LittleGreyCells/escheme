# GNU C++/C Compilers
C++ = g++
CC  = gcc

CWD = $(shell pwd)

SRCLOC = $(CWD)/src
REP    = $(SRCLOC)/rep
EVAL   = $(SRCLOC)/eval
CORE   = $(SRCLOC)/core
NOISE  = $(SRCLOC)/linenoise
INCLUDES = -I$(SRCLOC)

SRCS	= \
	$(REP)/rep.cxx \
	$(EVAL)/eval.cxx \
	$(EVAL)/eceval.cxx \
	$(CORE)/sexpr.cxx \
	$(CORE)/escheme.cxx \
	$(CORE)/error.cxx \
	$(CORE)/tstack.cxx \
	$(CORE)/argstack.cxx \
	$(CORE)/regstack.cxx \
	$(CORE)/intstack.cxx \
	$(CORE)/reader.cxx \
	$(CORE)/printer.cxx \
	$(CORE)/symtab.cxx \
	$(CORE)/funtab.cxx \
	$(CORE)/func.cxx \
	$(CORE)/math.cxx \
	$(CORE)/memory.cxx \
	$(CORE)/pio.cxx \
	$(CORE)/tio.cxx \
	$(CORE)/ipcsoc.cxx \

OBJS	= $(SRCS:.cxx=.o)

#DEBUG_FLAGS = -g
DEBUG_FLAGS = -O
#DEBUG_FLAGS = -O2
#DEBUG_FLAGS = -O3

#CFLAGS = $(DEBUG_FLAGS) -fno-operator-names -fpermissive -std=c++11
#CFLAGS = $(DEBUG_FLAGS) -Wall -Wextra -std=c++11
CFLAGS = $(DEBUG_FLAGS) -pedantic -std=c++11

LFLAGS = $(DEBUG_FLAGS) -v -lm

DEFINES =
#DEFINES = -DDO_ECE_CHECK
#DEFINES = -DDO_ECE_CHECK -DCHECKED_ACCESS 
#DEFINES = -DGC_STATISTICS_DETAILED -DFS_STATISTICS_DETAILED

escheme : $(OBJS) $(NOISE)/linenoise.o
	$(C++) -o escheme $(OBJS) $(NOISE)/linenoise.o $(LFLAGS)

%.o	: %.cxx
	$(C++) $(DEFINES) $(INCLUDES) -c $(CFLAGS) $< -o $@

$(NOISE)/linenoise.o : $(NOISE)/linenoise.c
	$(CC) -Wall -W -Os -c $< -o $@

clean 	:
	find . -name "*.o" -delete
	find . -name "*~" -delete

install : escheme
	mkdir -p /usr/share/escheme
	mkdir -p /usr/share/escheme/boot
	mkdir -p /usr/share/escheme/macros
	cp escheme.scm  /usr/share/escheme
	cp boot/*.scm   /usr/share/escheme/boot
	cp macros/*.scm /usr/share/escheme/macros
	cp escheme /usr/local/bin/escheme_interpreter
	echo 'ESCHEME=/usr/share/escheme escheme_interpreter $$@' > /usr/local/bin/escheme
	chmod +x /usr/local/bin/escheme

uninstall :
	if [ -d /usr/share/escheme ]; \
	then \
	   rm -rf /usr/share/escheme; \
	   rm -f /usr/local/bin/escheme_interpreter; \
	fi

