.PHONY: all libcaml kernel mk run boot clean

VERSION = $(shell cat ../../VERSION)
KERNEL  = funk-$(ARCH)-$(VERSION)
LIBOCAML    = run/libocaml.a
OCAMLKERNEL = mk/ocamlmkkernel.o
LINK_SCRIPT = link.ld

OBJS_C = $(shell find run -name "*.c") $(shell find boot -name "*.c")
OBJS_S = $(shell find boot -name "*.S")
OBJS_O = $(OBJS_C:.c=.o) $(OBJS_S:.S=.o)
EXT_C  = $(shell find mk -name "*.c")
EXT_O  = $(EXT_C:.c=.o) ../drivers/ports_stubs.o

all: libocaml kernel

libocaml: mk $(OCAMLKERNEL)
	@echo "[AR]    $(shell basename $(LIBOCAML))"
	@cp $(CAMLLIB)/libasmrun.a $(LIBOCAML)
	@echo "[AR]    $(shell basename $(OCAMLKERNEL))"
	@ar r $(LIBOCAML) $(OCAMLKERNEL)

kernel: $(KERNEL)

$(KERNEL): run boot $(LIBOCAML) $(OBJS_O) $(LINK_SCRIPT)
	@echo "[LD]    $(KERNEL)"
	@$(LD) $(LDFLAGS) -static -o $(KERNEL) -e multiboot_entry \
              $(OBJS_O) $(EXT_O) $(LIBOCAML) $(shell $(CC) -print-libgcc-file-name) \
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
	@ rm -rf $(KERNEL) $(OCAMLKERNEL) $(LIBOCAML)

-include ../Makefile.common
