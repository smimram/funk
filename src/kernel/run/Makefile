INCDIRS  = -Ii386/include -I../boot/i386/include -I../mk/arch/i386/include
CFLAGS  += -ansi -Wall -Os -fomit-frame-pointer
CPPFLAGS += -DCAML_NAME_SPACE $(INCDIRS) -I$(CAMLLIB)

RUN_C = $(wildcard *.c)
RUN_O = $(RUN_C:.c=.o)

all: $(RUN_O)

clean:
	@echo "Cleaning $(RELDIR)"
	rm -rf *.o

-include ../../Makefile.common
