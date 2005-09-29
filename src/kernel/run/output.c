/* output.c [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : kernel output source file
 * copyright   : (C) 2005 by nicolas guenot, samuel thibault
 * email       : nicolas.guenot@ens-lyon.org, samuel.thibault@ens-lyon.org

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
 * 12/03/2005 - this file defines functions and data needed by the kernel
 * to print out strings to its standard output.
 */

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

#include "output.h"
#include "libc-dummy.h"
#include "asm-utils.h"

/* screen-related definitions */
#define VIDEO_COLS   80
#define VIDEO_LINES  25
#define CHAR_ATTR    7
#define VIDEO_MEM    ((unsigned char *)0xb8000)

/* current screen position variables */
static int x_pos = 0;
static int y_pos = 10; 

/* void clear_screen()
{
  CAMLparam0();
  memset(VIDEO_MEM,0,VIDEO_COLS*VIDEO_LINES*2);
  x_pos = 0;
  y_pos = 0;
  CAMLreturn0;
} */

/*static void newline()
{
  x_pos = 0;
  if(y_pos<VIDEO_LINES-1)
    y_pos++;
  else {
    memmove(VIDEO_MEM,VIDEO_MEM+2*VIDEO_COLS,(VIDEO_LINES-1)*VIDEO_COLS*2);
    memset(VIDEO_MEM+(VIDEO_LINES-1)*VIDEO_COLS*2,0,VIDEO_COLS*2);
  }
}*/

void c_print_char(unsigned char c)
{
  VIDEO_MEM[(x_pos+y_pos*VIDEO_COLS)*2] = c;
  VIDEO_MEM[(x_pos+y_pos*VIDEO_COLS)*2+1] = CHAR_ATTR;
  x_pos++;
}

void c_print_string(const char *s)
{
  const unsigned char *us = (const unsigned char *) s;
  /*int fd = open ("vga.log",01|02000);*/
  /*x_pos = 0;*/
  /*y_pos = 0;*/
  while((*us) != '\0')
  {
    if ((*us) != '\n')
      c_print_char(*us);
    us++;
  }
  /*close (fd);*/

  x_pos = 0;
  if (++y_pos >= VIDEO_LINES)
    y_pos = 10;
  while (x_pos < VIDEO_COLS)
    c_print_char (' ');
  x_pos = 0;
  c_print_char ('>');
  c_print_char (' ');
}

#if 0
/* character printing function */
void print_char(unsigned char c)
{
  value *val = caml_named_value("funk_put_char");
#ifdef DEBUG
  if (!val) {
    c_printf("funk_put_char not found !\n");
    hang();
  }
#endif
  caml_callback(*val, Val_int(c));
}
#endif

/* string printing function */
void print_string(const char *s)
{
  value *val = caml_named_value("funk_print_string");
#ifdef DEBUG
  if (!val) {
    c_printf("funk_put_string not found !\n");
    hang();
  }
#endif
  caml_callback(*val, caml_copy_string(s));
}
