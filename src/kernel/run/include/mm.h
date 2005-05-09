#define FRAME_SIZE      4096
/* Length of the page directory. */
#define PAGE_DIR_LEN    1024
/* Length of ONE page table. */
#define PAGE_TABLE_LEN  1024
#define PAGE_DIR_ENTRY_SIZE	(PAGE_TABLE_LEN * FRAME_SIZE)

#ifndef ASM

unsigned long *page_directory;

void setup_pagination();
unsigned long* new_page_dir();
void map_page_range(unsigned long *pgd, unsigned long phys_addr, unsigned long len, unsigned long virt_addr, int attr);
void set_page_range_attr(unsigned long *pgd, unsigned long virt_addr, unsigned long len, int attr);

#endif
