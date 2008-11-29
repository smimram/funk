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
 *  63/31...........12 11...9 8...7    6 5 4...3    2   1   0
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

#ifdef __i386__
#define PAGE_ADDR(x)		(x & 0xfffff000)
#endif
#ifdef __x86_64__
#define PAGE_ADDR(x)		(x & 0xfffffffffffff000)
#endif
#define PGD_ENTRY_NB(va)	(va / (FRAME_SIZE * PAGE_TABLE_LEN))
#define BGT_ENTRY_NB(va)	((va / FRAME_SIZE) % PAGE_TABLE_LEN)

#ifdef __x86_64__
static unsigned long* new_page_top()
{
  unsigned long *ans = malloc_frame_aligned(PAGE_TOP_LEN * sizeof(long));
  memset(ans, 0, PAGE_TOP_LEN * sizeof(long));
  return ans;
}

static unsigned long* new_page_med()
{
  unsigned long *ans = malloc_frame_aligned(PAGE_MED_LEN * sizeof(long));
  memset(ans, 0, PAGE_MED_LEN * sizeof(long));
  return ans;
}
#endif

static unsigned long* new_page_dir()
{
  unsigned long *ans = malloc_frame_aligned(PAGE_DIR_LEN * sizeof(long));
  memset(ans, 0, PAGE_DIR_LEN * sizeof(long));
  return ans;
}

static unsigned long* new_page_table()
{
  unsigned long *ans = malloc_frame_aligned(PAGE_TABLE_LEN * sizeof(long));
  memset(ans, 0, PAGE_TABLE_LEN * sizeof(long));
  return ans;
}

/**
 * Remap a page range.
 *
 * virt_addr and len are supposed to be multiples of FRAME_SIZE.
 **/
void map_page_range(unsigned long *base, unsigned long phys_addr, unsigned long len, unsigned long virt_addr, int attr)
{
  int i, istart;
#ifdef __x86_64__
  unsigned long *top = NULL, *med = NULL;
  int i4, i3;
#endif
  unsigned long *dir = NULL, *pgt = NULL;
  int i2, i1;
  unsigned long addr; /* Current physical address. */

  /* Be sure that addr is a multiple of FRAME_SIZE. */
  if (virt_addr % FRAME_SIZE != 0)
  {
    phys_addr -= virt_addr % FRAME_SIZE;
    virt_addr -= virt_addr % FRAME_SIZE;
  }
  /* Same thing for len. */
  if (len % FRAME_SIZE != 0)
  {
    len += FRAME_SIZE - (len % FRAME_SIZE);
  }
  addr = phys_addr;

  istart = virt_addr / FRAME_SIZE;
#ifdef __x86_64__
  top = base;
  i4 = istart/PAGE_TOP_SPAN;
  i3 = (istart/PAGE_MED_SPAN)%PAGE_MED_LEN;
#else
  dir = base;
#endif
  i2 = (istart/PAGE_DIR_SPAN)%PAGE_DIR_LEN;
  i1 = (istart/PAGE_TABLE_SPAN)%PAGE_TABLE_LEN;

  for (i = istart; (i - istart) * FRAME_SIZE < len; i++)
  {
#ifdef __x86_64__
    if (!med)
    {
      if (top[i4] & 1)
        med = (unsigned long*)PAGE_ADDR(top[i4]);
      else
      {
        med = new_page_med();
        top[i4] = (unsigned long) med | 3;
      }
    }
    if (!dir)
    {
      if (med[i3] & 1)
        dir = (unsigned long*)PAGE_ADDR(med[i3]);
      else
      {
        dir = new_page_dir();
        med[i3] = (unsigned long) med | 3;
      }
    }
#endif
    if (!pgt)
    {
      if (dir[i2] & 1)
        pgt = (unsigned long*)PAGE_ADDR(dir[i2]);
      else
      {
        pgt = new_page_table();
        dir[i2] = (unsigned long) pgt | 3;
      }
    }
#ifdef TEST
    printf("i %d ", i);
#ifdef __x86_64__
    printf("top %p:%d med %p:%d ", top, i4, med, i3);
#endif
    printf("dir %p:%d pgt %p:%d addr: %p\n", dir, i2, pgt, i1, (void*)addr);
#endif
    pgt[i1] = addr | attr;
    addr += FRAME_SIZE;
    i1++;
    if (i1 == PAGE_TABLE_LEN)
    {
      i1 = 0;
      pgt = NULL;
      i2++;
      if (i2 == PAGE_DIR_LEN)
      {
        i2 = 0;
#ifdef __x86_64__
        dir = NULL;
        i3++;
        if (i3 ==  PAGE_MED_LEN)
        {
          i3 = 0;
          med = NULL;
          i4++;
          if (i4 ==  PAGE_TOP_LEN)
          {
            i4 = 0;
            printf("uh, i4 looped ?!\n");
          }
        }
#else
        printf("uh, i2 looped ?!\n");
#endif
      }
    }
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
#ifdef __x86_64__
  page_base = new_page_top();
#else
  page_base = new_page_dir();
#endif

  map_page_range(page_base, 0, MEM_SIZE, 0, 3);
  set_page_range_attr(page_base, (unsigned long)&_begin, &_ro_end - &_begin, 1);

  /* Fill the page table. */
  /* for(i = 0; i < ((MEM_SIZE + FRAME_SIZE - 1) / FRAME_SIZE); i++)
  {
    page_table[i] = address | 1 | ((address < (unsigned long) &_begin || address + FRAME_SIZE-1 >= (unsigned long) &_ro_end) ? 2 : 0);
    address += FRAME_SIZE;
  }; */

  /* Fill the the Page Directory Entries. */
  /* for(i = 0; i < (((MEM_SIZE + FRAME_SIZE - 1) / FRAME_SIZE) + PAGE_TABLE_LEN - 1) / PAGE_TABLE_LEN; i++)
  {
    page_base[i] = (unsigned long)(page_table + i * PAGE_TABLE_LEN) | 3;
  };
  for(; i < PAGE_DIR_LEN; i++)
  {
    page_base[i] = 0;
  } */

  /* Put the page directory address into CR3.
   * and enable paging (set bit 31 of CR0 to 1). */
#ifdef __x86_64__
  __asm__ __volatile__("\
      movq %0, %%cr3; \
      movq %%cr0, %%rax;\
      orl $0x80000000, %%eax;\
      movq %%rax, %%cr0" : :"r"(page_base) : "rax", "memory");
#endif
#ifdef __i386__
  __asm__ __volatile__("\
      movl %0, %%cr3; \
      movl %%cr0, %%eax;\
      orl $0x80000000, %%eax;\
      movl %%eax, %%cr0" : :"r"(page_base) : "eax", "memory");
#endif
}
