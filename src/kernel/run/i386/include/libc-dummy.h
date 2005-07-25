/* libc-dummy.h [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : dummy libc header file
 * copyright   : (C) 2005 by samuel thibault
 * email       : samuel.thibault@ens-lyon.org

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

#ifndef __LIBC_DUMMY_H
#define __LIBC_DUMMY_H
#include <stddef.h>
#include <sys/types.h>
typedef void FILE;
#define LONG_MIN (-2147483648UL)
#define HUGE_VAL (__extension__ 0x1.0p2047)
#define INT_MAX 2147483647
#define EOF -1

#define ENOSYS 1
#define ERANGE 2
#define EAGAIN 3

#define PageShift 12
#define HEAP_OFFSET 10

#define time_t int
#define suseconds_t int

struct timeval {
  time_t         tv_sec;   /* seconds */
  suseconds_t    tv_usec;  /* microseconds */
};

struct timezone {
  int  tz_minuteswest; /* minutes W of Greenwich */
  int  tz_dsttime;     /* type of dst correction */
};

static __inline unsigned char inb (unsigned short int port)
{
  unsigned char _v;

  __asm__ __volatile__ ("inb %w1,%0":"=a" (_v):"Nd" (port));
  return _v;
}

static __inline void outb (unsigned char value, unsigned short int port)
{
  __asm__ __volatile__ ("outb %b0,%w1": :"a" (value), "Nd" (port));
}

static __inline unsigned char inb_p (unsigned short int port)
{
  unsigned char _v;

  __asm__ __volatile__ ("inb %w1,%0\noutb %%al,$0x80":"=a" (_v):"Nd" (port));
  return _v;
}

static __inline void outb_p (unsigned char value, unsigned short int port)
{
  __asm__ __volatile__ ("outb %b0,%w1\noutb %%al,$0x80": :"a" (value),
                        "Nd" (port));
}

static __inline unsigned int inl (unsigned short int port)
{
  unsigned int _v;

  __asm__ __volatile__ ("inl %w1,%0":"=a" (_v):"Nd" (port));
  return _v;
}

static __inline void outl (unsigned int value, unsigned short int port)
{
  __asm__ __volatile__ ("outl %0,%w1": :"a" (value), "Nd" (port));
}

static __inline unsigned short int inw (unsigned short int port)
{
  unsigned short _v;

  __asm__ __volatile__ ("inw %w1,%0":"=a" (_v):"Nd" (port));
  return _v;
}

static __inline void outw (unsigned short int value, unsigned short int port)
{
  __asm__ __volatile__ ("outw %w0,%w1": :"a" (value), "Nd" (port));

}

extern size_t strlen(const char *s);
extern size_t strnlen(const char *s, size_t maxlen);
extern int printf(const char *fmt, ...);
extern int snprintf(char *buf, size_t size, const char *fmt, ...);
extern int c_printf(const char *fmt, ...);
extern int gettimeofday(struct timeval *tv, struct timezone *tz);
extern void usleep(unsigned long usec);
extern void *malloc_frame_aligned(size_t n);
extern void *memset(void *s, int c, size_t n);
extern void *memmove(void *s, const void *src, size_t n);
extern void *memcpy(void *s, const void *src, size_t n);

extern void *heap, *heaplimit, *last_seen;

#ifndef _ISbit
# if 0 /*__BYTE_ORDER == __BIG_ENDIAN*/
#  define _ISbit(bit)   (1 << (bit))
# else /* __BYTE_ORDER == __LITTLE_ENDIAN */
#  define _ISbit(bit)   ((bit) < 8 ? ((1 << (bit)) << 8) : ((1 << (bit)) >> 8))
# endif
enum
{
  _ISupper = _ISbit (0),        /* UPPERCASE.  */
  _ISlower = _ISbit (1),        /* lowercase.  */
  _ISalpha = _ISbit (2),        /* Alphabetic.  */
  _ISdigit = _ISbit (3),        /* Numeric.  */
  _ISxdigit = _ISbit (4),       /* Hexadecimal numeric.  */
  _ISspace = _ISbit (5),        /* Whitespace.  */
  _ISprint = _ISbit (6),        /* Printing.  */
  _ISgraph = _ISbit (7),        /* Graphical.  */
  _ISblank = _ISbit (8),        /* Blank (usually SPC and TAB).  */
  _IScntrl = _ISbit (9),        /* Control character.  */
  _ISpunct = _ISbit (10),       /* Punctuation.  */
  _ISalnum = _ISbit (11)        /* Alphanumeric.  */
};
#endif /* ! _ISbit  */

extern const unsigned short int **__ctype_b_loc(void);

#define __isctype(c, type) \
	  ((*__ctype_b_loc ())[(int) (c)] & (unsigned short int) type)
#define __isascii(c)    (((c) & ~0x7f) == 0)    /* If C is a 7 bit value.  */
#define __toascii(c)    ((c) & 0x7f)            /* Mask off high bits.  */
#define __exctype(name) extern int name (int)
__exctype (isalnum);
__exctype (isalpha);
__exctype (iscntrl);
__exctype (isdigit);
__exctype (islower);
__exctype (isgraph);
__exctype (isprint);
__exctype (ispunct);
__exctype (isspace);
__exctype (isupper);
__exctype (isxdigit);

# define isalnum(c)     __isctype((c), _ISalnum)
# define isalpha(c)     __isctype((c), _ISalpha)
# define iscntrl(c)     __isctype((c), _IScntrl)
# define isdigit(c)     __isctype((c), _ISdigit)
# define islower(c)     __isctype((c), _ISlower)
# define isgraph(c)     __isctype((c), _ISgraph)
# define isprint(c)     __isctype((c), _ISprint)
# define ispunct(c)     __isctype((c), _ISpunct)
# define isspace(c)     __isctype((c), _ISspace)
# define isupper(c)     __isctype((c), _ISupper)
# define isxdigit(c)    __isctype((c), _ISxdigit)
#  define isblank(c)    __isctype((c), _ISblank)

#endif /* __LIBC_DUMMY_H */
