#ifdef TEST
#include "test.h"
#include <string.h>
#else
#include "kernel.h"
#include "setup.h"
#include "libc-dummy.h"
#endif
#include "mm.h"

/*
 * Page Table Entries
 * ------------------
 * An entry that tells about where a page points to, and its attributes:
 * 
 *  31..............12 11...9 8...7    6 5 4...3    2   1   0
 *  Page frame address Avail  Reserved D A Reserved U/S R/W Present
 *
 * Page frame address = Physical address of memory (either the physical address of the page, or the physical address of the page table)
 * Avail = Do what you want with this
 * Reserved = Reserved by Intel
 * D = Dirty
 * A = Accessed
 * U/S = User or supervisor level
 * R/W = Read or read and write
 **/

/* unsigned long __attribute__((section(".bss.page_aligned"))) page_directory[PAGE_DIR_LEN]; */

#define PAGE_ADDR(x)		(x & 0xfffff000)
#define PGD_ENTRY_NB(va)	(va / (FRAME_SIZE * PAGE_TABLE_LEN))
#define BGT_ENTYR_NB(va)	((va / FRAME_SIZE) % PAGE_TABLE_LEN)

unsigned long* new_page_dir()
{
  unsigned long *ans = malloc_frame_aligned(PAGE_DIR_LEN * sizeof(long));
  memset(ans, 0, PAGE_DIR_LEN * sizeof(long));
  return ans;
}

unsigned long* new_page_table()
{
  unsigned long *ans = malloc_frame_aligned(PAGE_TABLE_LEN * sizeof(long));
  memset(ans, 0, PAGE_TABLE_LEN * sizeof(long));
  return ans;
}

static void may_alloc_dir_entry(unsigned long *pgd, int n, int attr)
{
  if (PAGE_ADDR(pgd[n]) == 0)
  {
    /* TODO: use attr? */
    pgd[n] = (unsigned long)new_page_table() | 3;
  }
}

/**
 * Remap a page range.
 *
 * virt_addr and len are supposed to be multiples of FRAME_SIZE.
 **/
void map_page_range(unsigned long *pgd, unsigned long phys_addr, unsigned long len, unsigned long virt_addr, int attr)
{
  int i, istart;
  unsigned long addr; /* Current physical address. */
  unsigned long *pgt; /* Current page table. */
  int pgde; /* Number of page dir entry. */

  /* Be sure that addr is a multiple of FRAME_SIZE. */
  if (virt_addr % FRAME_SIZE != 0)
  {
    phys_addr -= virt_addr % FRAME_SIZE;
    virt_addr -= virt_addr % FRAME_SIZE;
  }
  pgde = virt_addr / PAGE_DIR_ENTRY_SIZE;
  /* Same thing for len. */
  if (len % FRAME_SIZE != 0)
  {
    len += FRAME_SIZE - (len % FRAME_SIZE);
  }
  addr = phys_addr;
  
  /* Starting in the middle of a page dir entry? */
  if (virt_addr % PAGE_DIR_ENTRY_SIZE != 0)
  {
    may_alloc_dir_entry(pgd, pgde, attr);
  }
  pgt = (unsigned long*)PAGE_ADDR(pgd[pgde]);
  istart = virt_addr / FRAME_SIZE;
  if (istart % PAGE_TABLE_LEN == 0)
    pgde--;
  for (i = istart; (i - istart) * FRAME_SIZE < len; i++)
  {
    if (i % PAGE_TABLE_LEN == 0)
    {
      pgde++;
      may_alloc_dir_entry(pgd, pgde, attr);
      pgt = (unsigned long*)PAGE_ADDR(pgd[pgde]);
    }
#ifdef TEST
    printf("pgte: %04d   pgt:%p   pgde: %d   addr: %p\n", i % PAGE_TABLE_LEN, pgt, pgde, (void*)addr);
#endif
    pgt[i % PAGE_TABLE_LEN] = addr | attr;
    addr += FRAME_SIZE;
  }
}

void set_page_range_attr(unsigned long *pgd, unsigned long virt_addr, unsigned long len, int attr)
{
  unsigned long va;
  unsigned long *pgt; /* Current page table. */

  for(va = virt_addr / FRAME_SIZE; va < (virt_addr + len + FRAME_SIZE - 1) / FRAME_SIZE; va++)
  {
    pgt = (unsigned long*)PAGE_ADDR(pgd[va / PAGE_TABLE_LEN]);
    pgt[va % PAGE_TABLE_LEN] = (pgt[va % PAGE_TABLE_LEN] & 0xfffffff8) | attr;
  }
}

void setup_pagination()
{
  page_directory = new_page_dir();

  map_page_range(page_directory, 0, MEM_SIZE, 0, 3);
  set_page_range_attr(page_directory, (unsigned long)&_begin, &_ro_end - &_begin, 1);

  /* Fill the page table. */
  /* for(i = 0; i < ((MEM_SIZE + FRAME_SIZE - 1) / FRAME_SIZE); i++)
  {
    page_table[i] = address | 1 | ((address < (unsigned long) &_begin || address + FRAME_SIZE-1 >= (unsigned long) &_ro_end) ? 2 : 0);
    address += FRAME_SIZE;
  }; */

  /* Fill the the Page Directory Entries. */
  /* for(i = 0; i < (((MEM_SIZE + FRAME_SIZE - 1) / FRAME_SIZE) + PAGE_TABLE_LEN - 1) / PAGE_TABLE_LEN; i++)
  {
    page_directory[i] = (unsigned long)(page_table + i * PAGE_TABLE_LEN) | 3;
  };
  for(; i < PAGE_DIR_LEN; i++)
  {
    page_directory[i] = 0;
  } */

  /* Put the page directory address into CR3.
   * and enable paging (set bit 31 of CR0 to 1). */
  __asm__ __volatile__("\
      movl %0, %%cr3; \
      movl %%cr0, %%eax;\
      orl $0x80000000, %%eax;\
      movl %%eax, %%cr0" : :"r"(page_directory) : "%eax", "memory");
}
