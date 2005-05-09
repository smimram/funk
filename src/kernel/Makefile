 # makefile [part of the funk project]
 #[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 # contents    : makefile for the kernel
 # copyright   : (C) 2005 by nicolas guenot, samuel thibault,
 #               samuel mimram, alexandre buisse
 # email       : nicolas.guenot@ens-lyon.org, samuel.thibault@ens-lyon.org
 #               samuel.mimram@ens-lyon.org, alexandre.buisse@ens-lyon.org

###############################################################################
#                                                                             #
# This program is free software; you can redistribute it and/or               #
# modify it under the terms of the GNU General Public License                 #
# as published by the Free Software Foundation; either version 2              #
# of the License, or (at your option) any later version.                      #
#                                                                             #
# This program is distributed in the hope that it will be useful,             #
# but WITHOUT ANY WARRANTY; without even the implied warranty of              #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the                #
# GNU General Public License for more details.                                #
#                                                                             #
# You should have received a copy of the GNU General Public License           #
# along with this program; if not, write to the Free Software                 #
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. #
#                                                                             #
###############################################################################

.PHONY: doc clean debug depend

# general definitions
#####################

ARCH    = $(shell arch)
VERSION = $(shell cat ../../VERSION)
KERNEL  = funk-$(ARCH)-$(VERSION)
CAMLLIB = $(shell ocamlc -where)
OCAMLPACKAGES	= "str extlib"

# commands definitions
#####################

CC       = gcc
CFLAGS  += -ansi -Wall -Os -fomit-frame-pointer -g
CPPFLAGS += -DCAML_NAME_SPACE -Irun/$(ARCH) -I$(CAMLLIB) -Irun/include
OCAMLINCDIRS = -I mk -I ../servers -I ../userland
OCAMLFINDPARAMS = -package $(OCAMLPACKAGES)
OCAMLOPT = ocamlfind ocamlopt $(OCAMLFINDPARAMS) $(OCAMLINCDIRS)
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLDEP = ocamlfind ocamldep -native $(OCAMLFINDPARAMS) $(OCAMLINCDIRS)
OCAMLDOC = ocamlfind ocamldoc $(OCAMLFINDPARAMS) $(OCAMLINCDIRS)
OCAMLFLAGS_mk/console.cmx = -unsafe
LD       = ld
# LDFLAGS  = -s
LDFLAGS	 = 

ifeq ($(findstring debug, $(MAKECMDGOALS)),debug)
	CPPFLAGS   += -DDEBUG
	CFLAGS += -g
endif

ifeq ($(findstring int, $(MAKECMDGOALS)),int)
	CPPFLAGS += -DINTERRUPTIONS
endif

ifeq ($(findstring malloc, $(MAKECMDGOALS)),malloc)
	CPPFLAGS += -DMALLOC
endif

# files definitions
#####################

MAIN_RUN_C	= $(wildcard run/*.c)
# run/libc-dummy.o run/kernel.o run/output.o run/input.o run/vsnprintf.o
MAIN_RUN_OBJS	= $(MAIN_RUN_C:.c=.o)
ARCH_RUN_OBJS	= run/$(ARCH)/boot.o run/$(ARCH)/setup.o run/$(ARCH)/entry.o
MK_C	 = $(wildcard mk/*.c)
MK_OBJS	 = $(MK_C:.c=.o)
OBJS	 = $(MAIN_RUN_OBJS) $(ARCH_RUN_OBJS) $(MK_OBJS)

EXTERNAL_ML	= ../servers/vfs.ml ../servers/ramfs.ml ../userland/file.ml ../userland/filecmds.ml ../userland/pci_ids.ml ../userland/sound.ml
ML	 = mk/vfs_defs.ml mk/funk.ml mk/memory.ml mk/kThread.ml mk/irq.ml mk/ipc.ml \
	   mk/mouse.ml mk/cpu.ml mk/dma.ml \
	   mk/pci.ml mk/ne2k.ml mk/cirrusfb.ml \
	   mk/ide.ml mk/floppy.ml \
	   mk/inputmap_fr_latin1.ml mk/keyboard.ml mk/serial.ml mk/console.ml \
	   $(EXTERNAL_ML) \
	   mk/shell_parser.ml mk/shell_lexer.ml mk/shell.ml \
	   mk/mlkernel.ml
EXTERNAL_MLI	= $(wildcard ../servers/*.mli ../userland/*.mli)
MLI	 = $(wildcard mk/*.mli) $(EXTERNAL_MLI)
CMX	 = $(ML:.ml=.cmx)
CMXA	 =

LIBOCAML	= run/libocaml.a
OCAMLKERNEL	= mk/ocamlkernel.o
OCAMLWRAPPER	= run/ocamlwrapper.o

LINK_SCRIPT	= link.ld

# targets definitions
#####################

all: $(KERNEL) depend
debug int malloc: all
	echo $(ML)

$(KERNEL): $(LIBOCAML) $(OBJS) $(LINK_SCRIPT)
	$(LD) $(LDFLAGS) -static -o $(KERNEL) -e multiboot_entry \
              $(OBJS) $(LIBOCAML) $(shell $(CC) -print-libgcc-file-name) \
              -T $(LINK_SCRIPT)

$(OCAMLKERNEL): $(CMX)
	$(OCAMLOPT) -linkpkg $(CMXA) $(CMX) -output-obj -o $@

$(LIBOCAML): $(CMX) $(OCAMLWRAPPER) $(OCAMLKERNEL)
	cp $(CAMLLIB)/libasmrun.a $@
	ar r $@ $(OCAMLWRAPPER) $(OCAMLKERNEL)

%.cmi: %.mli
	$(OCAMLOPT) -c $< $(OCAMLFLAGS_$@) -o $@

%.cmx: %.ml
	$(OCAMLOPT) -c $< $(OCAMLFLAGS_$@) -o $@

%.ml: %.mly
	$(OCAMLYACC) $<

%.ml: %.mll
	$(OCAMLLEX) $<

clean:
	rm -f run/*.o run/*.a run/*/*.o mk/*.cm* mk/*.o $(KERNEL) \
	mk/shell_lexer.ml mk/shell_parser.ml shell_parser.mli \
	../servers/*.cm* ../servers/*.o ../userland/*.cm* ../userland/*.o \
	.depend

doc: all
	rm -rf ../../doc/html
	-mkdir -p ../../doc/html
	$(OCAMLDOC) -d ../../doc/html -keep-code -sort -stars -colorize-code -html $(MLI) $(ML)

depend: .depend

.depend: $(MLI) $(ML)
	$(OCAMLDEP) $(MLI) $(ML) > .depend

ifneq ($(findstring clean, $(MAKECMDGOALS)),clean)
-include .depend
endif
