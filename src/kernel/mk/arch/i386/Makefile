INCDIRS = -I../../../run/i386/include/ -I../../../boot/i386/include
CFLAGS += -ansi -Wall -Os -fomit-frame-pointer $(INCDIRS)

C = $(wildcard *.c)
O = $(C:.c=.o)

all: $(O)

clean:
	@echo "Cleaning arch/i386"
	rm -rf *.o

-include ../../../../Makefile.common
