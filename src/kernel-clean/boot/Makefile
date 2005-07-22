CAMLLIB = $(shell ocamlc -where)

CC       = gcc
INCDIRS  = -I../run/i386/include -Ii386/include -I../mk/arch/i386/include
CFLAGS  += -ansi -Wall -Os -fomit-frame-pointer
CPPFLAGS += -DCAML_NAME_SPACE $(INCDIRS) -I$(CAMLLIB)

ifeq ($(findstring debug, $(MAKECMDGOALS)),debug)
	CPPFLAGS   += -DDEBUG
	CFLAGS += -g
endif


RUN_C = $(wildcard i386/*.c *.c)
RUN_O = $(RUN_C:.c=.o)

all: $(RUN_O)
	@echo "Done with boot/"

%.o: %.c
	@echo "Generating $@..."
	@ $(CC) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<

clean:
	@echo $(RUN_C)
	@echo "Cleaning run"
	@ rm -rf *.o
	
