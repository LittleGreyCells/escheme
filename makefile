# GNU C++/C Compilers
C++ = g++
CC  = gcc

CWD = $(shell pwd)

APP    = escheme1

SRCLOC = $(CWD)/src
EVAL   = $(SRCLOC)/eval
CORE   = $(SRCLOC)/core
NOISE  = $(SRCLOC)/linenoise
INCLUDES = -I$(SRCLOC)

SRCS	= \
	$(EVAL)/eval.cxx \
	$(EVAL)/eceval.cxx \
	$(CORE)/sexpr.cxx \
	$(CORE)/escheme.cxx \
	$(CORE)/rep.cxx \
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

$(APP) : $(OBJS) $(NOISE)/linenoise.o
	$(C++) -o $@ $(OBJS) $(NOISE)/linenoise.o $(LFLAGS)
	ln -sf $(APP) escheme

%.o	: %.cxx
	$(C++) $(DEFINES) $(INCLUDES) -c $(CFLAGS) $< -o $@

$(NOISE)/linenoise.o : $(NOISE)/linenoise.c
	$(CC) -Wall -W -Os -c $< -o $@

clean 	:
	find . -name "*.o" -delete
	find . -name "*~" -delete
	rm -f escheme

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
	ln -s /usr/local/bin/$(APP) /usr/local/bin/escheme

uninstall :
	if [ -d /usr/share/$(APP) ]; \
	then \
	   rm -rf /usr/share/$(APP); \
	   rm -f /usr/local/bin/$(APP); \
	   rm -f /usr/local/bin/$(APP)_i; \
	   rm -f /usr/local/bin/escheme; \
	fi

