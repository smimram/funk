#if defined(__i386__) || defined(__x86_64__)
#define FRAME_SIZE      4096
#ifdef __x86_64__
/* Length of the top directory. */
#define PAGE_TOP_LEN    (FRAME_SIZE/sizeof(long))
/* Length of the medium directory. */
#define PAGE_MED_LEN    (FRAME_SIZE/sizeof(long))
#endif
/* Length of the page directory. */
#define PAGE_DIR_LEN    (FRAME_SIZE/sizeof(long))
/* Length of ONE page table. */
#define PAGE_TABLE_LEN  (FRAME_SIZE/sizeof(long))

#define PAGE_TABLE_SPAN PAGE_TABLE_LEN
#define PAGE_DIR_SPAN   (PAGE_TABLE_SPAN * PAGE_DIR_LEN)
#ifdef __x86_64__
#define PAGE_MED_SPAN   (PAGE_DIR_SPAN * PAGE_MED_LEN)
#define PAGE_TOP_SPAN   (PAGE_MED_SPAN * PAGE_TOP_LEN)
#endif

#define PAGE_DIR_ENTRY_SIZE	(PAGE_TABLE_LEN * FRAME_SIZE)
#endif

#ifndef ASM

unsigned long *page_base;

void setup_pagination();
void map_page_range(unsigned long *pgd, unsigned long phys_addr, unsigned long len, unsigned long virt_addr, int attr);
void set_page_range_attr(unsigned long *pgd, unsigned long virt_addr, unsigned long len, int attr);

#endif
