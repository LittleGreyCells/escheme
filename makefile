C++ = g++
CC  = gcc

CWD = $(shell pwd)

APP    = escheme

SRCLOC = $(CWD)/src
REP    = $(SRCLOC)/rep
EVAL   = $(SRCLOC)/eval
CORE   = $(SRCLOC)/core
NOISE  = $(SRCLOC)/linenoise
INCLUDES = -I$(SRCLOC)

SRCS	= \
	$(REP)/rep_interp.cxx \
	$(EVAL)/eval.cxx \
	$(EVAL)/eceval.cxx \
	$(CORE)/sexpr.cxx \
	$(CORE)/main.cxx \
	$(CORE)/error.cxx \
	$(CORE)/argstack.cxx \
	$(CORE)/regstack.cxx \
	$(CORE)/intstack.cxx \
	$(CORE)/reader.cxx \
	$(CORE)/printer.cxx \
	$(CORE)/equality.cxx \
	$(CORE)/hash.cxx \
	$(CORE)/dict.cxx \
	$(CORE)/symdict.cxx \
	$(CORE)/symtab.cxx \
	$(CORE)/funtab.cxx \
	$(CORE)/func.cxx \
	$(CORE)/math.cxx \
	$(CORE)/memory.cxx \
	$(CORE)/framestore.cxx \
	$(CORE)/pio.cxx \
	$(CORE)/tio.cxx \
	$(CORE)/transcript.cxx \
	$(CORE)/ipcsoc.cxx \

OBJS	= $(SRCS:.cxx=.o)

#DEBUG_FLAGS = -g
#DEBUG_FLAGS = -O
#DEBUG_FLAGS = -O2
DEBUG_FLAGS = -Ofast

#CFLAGS = $(DEBUG_FLAGS) -fno-operator-names -fpermissive -std=c++14
#CFLAGS = $(DEBUG_FLAGS) -Wall -Wextra -std=c++14
CFLAGS = $(DEBUG_FLAGS) -pedantic -std=c++14

LFLAGS = $(DEBUG_FLAGS) -v -lm

DEFINES =
#DEFINES += -DGC_STATISTICS_DETAILED
#DEFINES += -DMAX_DEPTH
DEFINES += -DITERATIVE_MARK

$(APP) : $(OBJS) $(NOISE)/linenoise.o
	$(C++) -o $@ $(OBJS) $(NOISE)/linenoise.o $(LFLAGS)

%.o	: %.cxx
	$(C++) $(DEFINES) $(INCLUDES) -c $(CFLAGS) $< -o $@

$(NOISE)/linenoise.o : $(NOISE)/linenoise.c
	$(CC) -Wall -W -Os -c $< -o $@

clean 	:
	find . -name "*.o" -delete
	find . -name "*~" -delete
	rm -f $(APP)

install : $(APP)
	mkdir -p /usr/share/$(APP)
	mkdir -p /usr/share/$(APP)/boot
	mkdir -p /usr/share/$(APP)/macros
	cp escheme.scm  /usr/share/$(APP)
	cp boot/*.scm   /usr/share/$(APP)/boot
	cp macros/*.scm /usr/share/$(APP)/macros
	cp $(APP) /usr/local/bin/$(APP)_i
	echo 'ESCHEME=/usr/share/$(APP) $(APP)_i $$@' > /usr/local/bin/$(APP)
	chmod +x /usr/local/bin/$(APP)

uninstall :
	if [ -d /usr/share/$(APP) ]; \
	then \
	   rm -rf /usr/share/$(APP); \
	   rm -f /usr/local/bin/$(APP); \
	   rm -f /usr/local/bin/$(APP)_i; \
	fi

