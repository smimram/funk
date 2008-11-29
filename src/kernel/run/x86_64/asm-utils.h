static __inline void SAVE_REGS()
{
    __asm__ __volatile__ (" \
	    pusha; \
	    pushq %ds; \
	    pushq %es; \
	    pushq %fs; \
	    pushq %gs; \
	    ");
}

static __inline void RESTORE_REGS()
{
    __asm__ __volatile__ (" \
	    popq %gs; \
	    popq %fs; \
	    popq %es; \
	    popq %ds; \
	    popa; \
	    ");
}

static __inline__ void hlt(void)
{
	__asm__ __volatile__ ("hlt");
}

static __inline__ void __attribute__((__noreturn__)) hang(void)
{
	while(1) hlt();
}

extern int interrupt_disabled;

static __inline__ void cli(void)
{
	if (!(interrupt_disabled++))
		__asm__ __volatile__("cli");
}

static __inline__ void sti(void)
{
	if (!(--interrupt_disabled))
		__asm__ __volatile__("sti");
}

#define fastcall

#define mb()	__asm__ __volatile__("mfence" : : : "memory")
#define wmb()	__asm__ __volatile__("sfence" : : : "memory")
#define rmb()	__asm__ __volatile__("lfence" : : : "memory")
