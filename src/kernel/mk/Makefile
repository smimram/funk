ARCH    = $(shell arch)
VERSION = $(shell cat ../../VERSION)
KERNEL  = funk-$(ARCH)-$(VERSION)
CAMLLIB = $(shell ocamlc -where)
OCAMLPACKAGES	= "str extlib"

# defs
CC       = gcc
CFLAGS  += -ansi -Wall -Os -fomit-frame-pointer
INCDIRS  = -Iarch/i386/include
OCAMLFINDPARAMS = -package $(OCAMLPACKAGES)
OCAMLINCDIRS = 
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

.PHONY: doc clean debug depend

SUBDIRS = ipc mm proc perms

SRC_ML  = $(wildcard *.ml)
SRC_MLI = $(wildcard *.mli)
EXT_ML  = $(SRC_ML) $(foreach i,$(SUBDIRS),$(wildcard $i/*.ml))
CMX     = $(SRC_ML:.ml=.cmx)
CMI     = $(SRC_ML:.ml=.cmi)

OCAMLKERNEL = ocamlmkkernel.o


%.cmx: %.ml
	@echo "Generating $@..."
	@ $(OCAMLOPT) -c -o $@ $<

%.cmi: %.mli
	@echo "Generating $@..."
	@ $(OCAMLOPT) -c -o $@ $<

%.cmi: %.ml
	@echo "Generating $@..."
	@ $(OCAMLOPT) -c -o $@ $<



all: $(OCAMLKERNEL)

$(OCAMLKERNEL): $(CMI) $(CMX)
	@echo "Making subdirectories cmx..."
	@make -C ipc
	@make -C mm
	@make -C perms
	@make -C proc
	@echo "Generating $@..."
	@ $(OCAMLOPT) -linkpkg $(EXT_CMXA) $(EXT_CMX) -output-obj -o $@
	@echo "Done with microkernel generation"


clean:
	@echo "Cleaning..."
	@rm -rf *.cm* *.o $(OCAMLKERNEL)
	@make -C ipc   clean
	@make -C mm    clean
	@make -C perms clean
	@make -C proc  clean
