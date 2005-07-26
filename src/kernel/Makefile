.PHONY: all libcaml kernel mk run boot clean

ARCH    = $(shell arch)
VERSION = $(shell cat ../../VERSION)
CAMLLIB = $(shell ocamlc -where)
KERNEL  = funk-$(ARCH)-$(VERSION)
LIBOCAML    = run/libocaml.a
OCAMLKERNEL = mk/ocamlmkkernel.o
FUNKDRIVERS = ../drivers/funkdrivers.o
LINK_SCRIPT	= link.ld
LD = ld

OBJS_C = $(shell find run -name "*.c") $(shell find boot -name "*.c")
OBJS_S = $(shell find boot -name "*.S")
OBJS_O = $(OBJS_C:.c=.o) $(OBJS_S:.S=.o)

all: libocaml kernel

libocaml: mk funkdrivers $(OCAMLKERNEL) $(FUNKDRIVERS)
	@echo "Generating libocaml.a"
	@cp $(CAMLLIB)/libasmrun.a $(LIBOCAML)
	@ar r $(LIBOCAML) $(FUNKDRIVERS) $(OCAMLKERNEL)

funkdrivers:
	@echo "Generating funkdrivers.o"
	@ make -C ../drivers

kernel: $(KERNEL)

$(KERNEL): run boot $(LIBOCAML) $(OBJS_O) $(LINK_SCRIPT)
	$(LD) $(LDFLAGS) -static -o $(KERNEL) -e multiboot_entry \
              $(OBJS_O) $(LIBOCAML) $(shell $(CC) -print-libgcc-file-name) \
              -T $(LINK_SCRIPT)

mk:
	@make -C mk

run:
	@make -C run

boot:
	@make -C boot

clean:
	@make -C mk clean
	@make -C boot clean
	@make -C run clean
	@make -C ../drivers clean
	@ rm -rf $(KERNEL) $(FUNKDRIVERS) $(OCAMLKERNEL) $(LIBOCAML)
