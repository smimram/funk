ARCH    = $(shell arch)
VERSION = $(shell cat ../../VERSION)
KERNEL  = funk-$(ARCH)-$(VERSION)
CAMLLIB = $(shell ocamlc -where)
OCAMLPACKAGES	= "str extlib"

KERNEL_DIR = ../kernel

# defs
CC       = gcc
INCDIRS  = -I$(KERNEL_DIR)/mk/arch/i386/include -I$(KERNEL_DIR)/boot/i386/include -I$(KERNEL_DIR)/run/i386/include
CFLAGS  += -ansi -Wall -Os -fomit-frame-pointer
CPPFLAGS += -DCAML_NAME_SPACE $(INCDIRS) -I$(CAMLLIB)
INCDIRS  += -Iarch/i386/include
OCAMLFINDPARAMS = -package $(OCAMLPACKAGES)
OCAMLINCDIRS = -I $(KERNEL_DIR)/mk -I $(KERNEL_DIR)/mk/mm
OCAMLFLAGS_console.cmx = -unsafe
OCAMLOPT = ocamlfind ocamlopt $(OCAMLFINDPARAMS) $(OCAMLINCDIRS)
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLDEP = ocamlfind ocamldep -native $(OCAMLFINDPARAMS) $(OCAMLINCDIRS)
OCAMLDOC = ocamlfind ocamldoc $(OCAMLFINDPARAMS) $(OCAMLINCDIRS)
LD       = ld
LDFLAGS	 = 

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

%.cmx: %.ml
	@echo "Generating $@..."
	 $(OCAMLOPT) -c -o $@ $<

%.cmi: %.mli
	@echo "Generating $@..."
	 $(OCAMLOPT) -c -o $@ $<

%.cmi: %.ml
	@echo "Generating $@..."
	 $(OCAMLOPT) -c -o $@ $<

%.o: %.c
	@echo "Generating $@..."
	 $(CC) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<



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
	@echo "Cleaning drivers..."
	@rm -rf *.cm* *.o $(FUNKDRIVERS) .depend


