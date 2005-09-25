INCDIRS  = -I../run/i386/include -Ii386/include -I../mk/arch/i386/include
CFLAGS  += -ansi -Wall -Os -fomit-frame-pointer
CPPFLAGS += -DCAML_NAME_SPACE $(INCDIRS) -I$(CAMLLIB)

ifeq ($(findstring debug, $(MAKECMDGOALS)),debug)
	CPPFLAGS   += -DDEBUG
	CFLAGS += -g
endif


RUN_C = $(wildcard i386/*.c *.c)
RUN_S = $(wildcard i386/*.S)
RUN_O = $(RUN_C:.c=.o)
RUN_SO = $(RUN_S:.S=.o)

all: $(RUN_O) $(RUN_SO)
#	@echo "Done with boot/"

clean:
	@echo "Cleaning run"
	@rm -rf *.o i386/*.o

-include ../../Makefile.common
