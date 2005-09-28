OCAMLPACKAGES = "str extlib"

# defs
CFLAGS  += -ansi -Wall -Os -fomit-frame-pointer
INCDIRS  = -Iarch/i386/include
OCAMLINCDIRS = 
LDFLAGS = 

ifeq ($(findstring debug, $(MAKECMDGOALS)),debug)
	CPPFLAGS   += -DDEBUG
	CFLAGS += -g
endif

.PHONY: doc clean debug depend

SUBDIRS = ipc mm perms proc

OCAMLINCDIRS += $(foreach i,$(SUBDIRS),-I $i) -I . -I ../../drivers

SRC_ML  = $(wildcard *.ml)
SRC_MLI = $(wildcard *.mli)
EXT_ML  = utils.ml misc.ml $(foreach i,$(SUBDIRS),$(wildcard $i/*.ml))
KERN_ML = mlkernel.ml
KERN_CMX = $(KERN_ML:.ml=.cmx)
CMX     = $(SRC_ML:.ml=.cmx)
CMI     = $(SRC_ML:.ml=.cmi)
EXT_CMX = $(EXT_ML:.ml=.cmx)
EXT_CMI = $(EXT_ML:.ml=.cmi) $(KERN_ML:.ml=.cmi)
DRV_CMX = ../../drivers/ports.cmx ../../drivers/console.cmx
TOT_CMX = $(EXT_CMX) $(DRV_CMX) $(KERN_CMX)

OCAMLKERNEL = ocamlmkkernel.o

all: $(OCAMLKERNEL)

drivers:
	@make -C ../../drivers

$(OCAMLKERNEL): $(EXT_CMI) $(EXT_CMX) drivers $(KERN_CMX)
	@make -C ipc
	@make -C mm
	@make -C perms
	@make -C proc
	@echo "[LD]    $@"
	@$(OCAMLOPT) -linkpkg $(EXT_CMXA) $(TOT_CMX) -output-obj -o $@


clean:
	@echo "Cleaning mk"
	@rm -rf *.cm* *.o $(OCAMLKERNEL)
	@make -C ipc   clean
	@make -C mm    clean
	@make -C perms clean
	@make -C proc  clean

-include ../../Makefile.common
