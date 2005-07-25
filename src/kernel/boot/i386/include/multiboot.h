/* multiboot.h [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : multiboot-compliance definitions
 * copyright   : (C) 2005 by nicolas guenot
 * email       : nicolas.guenot@ens-lyon.org

*******************************************************************************
*                                                                             *
* This program is free software; you can redistribute it and/or               *
* modify it under the terms of the GNU General Public License                 *
* as published by the Free Software Foundation; either version 2              *
* of the License, or (at your option) any later version.                      *
*                                                                             *
* This program is distributed in the hope that it will be useful,             *
* but WITHOUT ANY WARRANTY; without even the implied warranty of              *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the                *
* GNU General Public License for more details.                                *
*                                                                             *
* You should have received a copy of the GNU General Public License           *
* along with this program; if not, write to the Free Software                 *
* Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. *
*                                                                             *
******************************************************************************/

/* COMMENTS:
 * 12/03/2005 - this file makes the multiboot-compliance definitions. that is,
 * it defines the structures and macros needed by the kernel to analyze the
 * situation after is has been loaded by the bootloader. It is adapted from
 * the "multiboot.h" file, found in the multiboot specification (for further
 * details, see http://www.gnu.org/software/grub/manual/multiboot/).
 */

#ifndef MULTIBOOT_HEADER
#define MULTIBOOT_HEADER

/* magic number for the multiboot header */
#define MULTIBOOT_HEADER_MAGIC  0x1BADB002

/* flags for the multiboot header */
# define MULTIBOOT_HEADER_FLAGS  0x00010003

/* magic number passed by a multiboot-compliant boot loader */
#define MULTIBOOT_BOOTLOADER_MAGIC  0x2BADB002

/* size of the boot stack (16 KB) */
#define STACK_SIZE  0x4000

#ifndef ASM  /* we don't want to include this in the assembly code */

/* Total size of the memory. */
extern unsigned long mem_size;

/* multiboot header structure */
typedef struct multiboot_header
{
  unsigned long magic;
  unsigned long flags;
  unsigned long checksum;
  unsigned long header_addr;
  unsigned long load_addr;
  unsigned long load_end_addr;
  unsigned long bss_end_addr;
  unsigned long entry_addr;
} multiboot_header_t;

/* symbol table for a.out (we don't really need this) */
typedef struct aout_symbol_table
{
  unsigned long tabsize;
  unsigned long strsize;
  unsigned long addr;
  unsigned long reserved;
} aout_symbol_table_t;

/* section header table for elf (don't need this either) */
typedef struct elf_section_header_table
{
  unsigned long num;
  unsigned long size;
  unsigned long addr;
  unsigned long shndx;
} elf_section_header_table_t;

/* multiboot information structure */
typedef struct multiboot_info
{
  unsigned long flags;
  unsigned long mem_lower;
  unsigned long mem_upper;
  unsigned long boot_device;
  unsigned long cmdline;
  unsigned long mods_count;
  unsigned long mods_addr;
  union
  {
    aout_symbol_table_t aout_sym;
    elf_section_header_table_t elf_sec;
  } u;
  unsigned long mmap_length;
  unsigned long mmap_addr;
} multiboot_info_t;

/* module structure */
typedef struct module
{
  unsigned long mod_start;
  unsigned long mod_end;
  unsigned long string;
  unsigned long reserved;
} module_t;

/* memory map (be careful: offset 0 is base_addr_low but no size) */
typedef struct memory_map
{
  unsigned long size;
  unsigned long base_addr_low;
  unsigned long base_addr_high;
  unsigned long length_low;
  unsigned long length_high;
  unsigned long type;
} memory_map_t;

#endif /* #ifndef ASM */
#endif /* #ifndef MULTIBOOT_HEADER */

