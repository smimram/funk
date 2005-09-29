OCAMLPACKAGES = "str extlib"

KERNEL_DIR = ../kernel

# defs
INCDIRS  = -I$(KERNEL_DIR)/mk/arch/i386/include -I$(KERNEL_DIR)/boot/i386/include -I$(KERNEL_DIR)/run/i386/include
CFLAGS  += -ansi -Wall -Os -fomit-frame-pointer
CPPFLAGS += -DCAML_NAME_SPACE $(INCDIRS) -I$(CAMLLIB)
INCDIRS  += -Iarch/i386/include
OCAMLFINDPARAMS = -package $(OCAMLPACKAGES)
OCAMLINCDIRS = -I $(KERNEL_DIR)/mk -I $(KERNEL_DIR)/mk/mm
OCAMLFLAGS_console.cmx = -unsafe
LDFLAGS = 

ifeq ($(findstring debug, $(MAKECMDGOALS)),debug)
	CPPFLAGS   += -DDEBUG
	CFLAGS += -g
endif


#SRC_ML  = $(wildcard *.ml)
SRC_ML  = ports.ml console.ml
SRC_MLI = $(SRC_ML:.ml=.mli)
STUBS   = ports_stubs.c
CMX     = $(SRC_ML:.ml=.cmx)
CMI     = $(SRC_MLI:.mli=.cmi)
STUBS_O = $(STUBS:.c=.o)

FUNKDRIVERS = funkdrivers.o


#all: debug $(FUNKDRIVERS)
all: $(CMI) $(CMX) $(STUBS_O)

debug:
	@echo $(CMI)
	@echo $(CMX)
	@echo $(STUBS_O)

#$(FUNKDRIVERS): $(STUBS_O) $(CMI) $(CMX)
#	@echo "Generating $@..."
#	 $(OCAMLOPT) -linkpkg $(CMX) -output-obj -o $@
#	@echo "Done with drivers"


clean:
	@echo "Cleaning $(RELDIR)"
	@rm -rf *.cm* *.o $(FUNKDRIVERS) .depend

-include ../Makefile.common
