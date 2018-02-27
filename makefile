# GNU C++/C Compilers
CXXC = g++
CC   = gcc

CWD = $(shell pwd)

TARGET = escheme
SRCLOC = $(CWD)/src
EVAL   = $(SRCLOC)/eval
CORE   = $(SRCLOC)/core
NOISE  = $(CWD)/linenoise
INCLUDES = -I$(NOISE) -I$(CORE) -I$(EVAL)

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
#DEBUG_FLAGS = -O
DEBUG_FLAGS = -O2
#DEBUG_FLAGS = -O3

#CFLAGS = $(DEBUG_FLAGS) -fno-operator-names -fpermissive -std=c++11
#CFLAGS = $(DEBUG_FLAGS) -Wall -Wextra -std=c++11
CFLAGS = $(DEBUG_FLAGS) -pedantic -std=c++11

LFLAGS = $(DEBUG_FLAGS) -v -lm

#DEFINES = -DDO_CHECK
#DEFINES = -DDO_CHECK -DCHECKED_ACCESS 
DEFINES =

all 	: $(TARGET)

$(TARGET) : $(OBJS) $(NOISE)/linenoise.o
	$(CXXC) -o $(TARGET) $(OBJS) $(NOISE)/linenoise.o $(LFLAGS)
	rm $(EVAL)/*.o
	rm $(CORE)/*.o
	rm $(NOISE)/*.o

%.o	: %.cxx
	$(CXXC) $(DEFINES) $(INCLUDES) -c $(CFLAGS) $< -o $@

$(NOISE)/linenoise.o : $(NOISE)/linenoise.c
	gcc -Wall -W -Os -c $< -o $@

clean 	:
	rm -f $(EVAL)/*.o $(CORE)/*.o $(NOISE)/*.o *~ $(TARGET)

