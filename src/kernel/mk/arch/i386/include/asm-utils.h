static __inline void SAVE_REGS()
{
    __asm__ __volatile__ (" \
	    pusha; \
	    pushl %ds; \
	    pushl %es; \
	    pushl %fs; \
	    pushl %gs; \
	    ");
}

static __inline void RESTORE_REGS()
{
    __asm__ __volatile__ (" \
	    popl %gs; \
	    popl %fs; \
	    popl %es; \
	    popl %ds; \
	    popa; \
	    ");
}

static __inline__ void hlt(void)
{
	__asm__ __volatile__ ("hlt");
}

static __inline__ void hang(void)
{
	while(1) hlt();
}

int interrupt_disabled;

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

#define fastcall __attribute__((regparm(3)))

/* nothing particular on x86 */
#define mb()	__asm__ __volatile__("" : : : "memory")
#define wmb()	mb()
#define rmb()	mb()
