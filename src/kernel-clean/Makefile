
ARCH    = $(shell arch)
VERSION = $(shell cat ../../VERSION)
CAMLLIB = $(shell ocamlc -where)
KERNEL  = funk-$(ARCH)-$(VERSION)
LIBOCAML    = run/libocaml.a
OCAMLKERNEL = mk/ocamlmkkernel.o
LINK_SCRIPT	= link.ld
LD = ld

OBJS_C = $(shell find run -name "*.c") $(shell find boot -name "*.c")
OBJS_S = $(shell find boot -name "*.S")
OBJS_O = $(OBJS_C:.c=.o) $(OBJS_S:.S=.o)

all:
	@make -C boot
	@make -C run
	@make -C mk
	@make libocaml
	@make kernel


libocaml: $(OCAMLKERNEL)
	@echo "Generating libocaml.a"
	@cp $(CAMLLIB)/libasmrun.a $(LIBOCAML)
	@ar r $(LIBOCAML) $(OCAMLKERNEL)


kernel: $(KERNEL)
	@echo $(OBJS_O)

$(KERNEL): $(LIBOCAML) $(OBJS_O) $(LINK_SCRIPT)
	$(LD) $(LDFLAGS) -static -o $(KERNEL) -e multiboot_entry \
              $(OBJS_O) $(LIBOCAML) $(shell $(CC) -print-libgcc-file-name) \
              -T $(LINK_SCRIPT)



clean:
	@make -C mk clean
	@make -C boot clean
	@make -C run clean
