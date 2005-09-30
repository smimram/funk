INCDIRS  = -I../run/$(ARCH)/include -I$(ARCH)/include -I../mk/arch/$(ARCH)/include
CFLAGS  += -ansi -Wall -Os -fomit-frame-pointer
CPPFLAGS += -DCAML_NAME_SPACE -I$(CAMLLIB)

RUN_C = $(wildcard i386/*.c *.c)
RUN_S = $(wildcard i386/*.S)
RUN_O = $(RUN_C:.c=.o)
RUN_SO = $(RUN_S:.S=.o)

all: $(RUN_O) $(RUN_SO)
#	@echo "Done with boot/"

clean:
	@echo "Cleaning $(RELDIR)"
	rm -rf *.o $(ARCH)/*.o

-include ../../Makefile.common
