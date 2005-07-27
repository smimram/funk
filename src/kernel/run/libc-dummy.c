/* libc-dummy.c [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : dummy libc used to make the caml code run
 * copyright   : (C) 2005 by nicolas guenot, samuel thibault,
 *               samuel mimram, alexandre buisse
 * email       : nicolas.guenot@ens-lyon.org, samuel.thibault@ens-lyon.org
 *               samuel.mimram@ens-lyon.org, alexandre.buisse@ens-lyon.org

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

#include <stdarg.h>
#include "libc-dummy.h"
#include "input.h"
#include "output.h"
#include "vsnprintf.h"
#include "ticks.h"
#include "asm-utils.h"
#include "setup.h"

/* General */

int c_printf(const char*, ...);

int errno = 0;

int *__errno_location(void)
{
  return &errno;
}

static int notImpl_int(void)
{
  errno = ENOSYS;
  return -1;
}

static void *notImpl_ptr(void)
{
  errno = ENOSYS;
  return NULL;
}

static void __attribute__((__noreturn__)) notImpl_fp(void)
{
  __asm__ __volatile__("int $6");
#ifdef DEBUG
  c_printf("FP operation\n");
#endif
  hang();
}

long __sysconf(int name)
{
#ifdef DEBUG
  c_printf("__sysconf(%i) called\n",name);
#endif
  return notImpl_int();
}

char *getenv(const char *name)
{
#ifdef DEBUG
  c_printf("getenv(%s) called\n",name);
#endif
  return notImpl_ptr();
}

int setenv(const char *name, const char *value, int overwrite)
{
#ifdef DEBUG
  c_printf("setenv called(%s,%s,%i)\n",name,value,overwrite);
#endif
  return notImpl_int();
}

char *setlocale(int category, const char *locale)
{
#ifdef DEBUG
  c_printf("setlocale called(%i,%s)\n",category,locale);
#endif
  return notImpl_ptr();
}

/* Process */

void exit(int status)
{
#ifdef DEBUG
  c_printf("exit called(%i)\n",status);
#endif
  hang();
}

#define pid_t int

pid_t getpid(void)
{
#ifdef DEBUG
  c_printf("getpid called\n");
#endif
  return 1;
}

pid_t getppid(void)
{
#ifdef DEBUG
  c_printf("getppid called\n");
#endif
  return 1;
}

int system(const char *command)
{
#ifdef DEBUG
  c_printf("system(%s) called\n",command);
#endif
  return notImpl_int();
}

/* Memory */

void *memcpy(void *dest, const void *src, size_t n)
{
#ifdef DEBUG
/*  c_printf("memcpy(%p,%p,%u) called\n",dest,src,n);*/
#endif
  int i;
  char *cdest = dest;
  const char *csrc = src;
  
  for (i = 0; i < n; i++)
    cdest[i] = csrc[i];
  return dest;
}

void *memmove(void *dest, const void *src, size_t n)
{
#ifdef DEBUG
/*  c_printf("memmove(%p,%p,%u) called\n",dest,src,n);*/
#endif
  if((dest > src) && (dest < (src + n)))
  {
    int i;
    char *cdest = dest;
    const char *csrc = src;
    
    for(i = n-1; i >= 0; i--)
      cdest[i] = csrc[i];
    return dest;
  }
  else
    return memcpy(dest, src, n);
}

void *memset(void *s, int c, size_t n)
{
#ifdef DEBUG
  /*c_printf("memset(%p,%i,%u) called\n",s,c,n);*/
#endif
  int i;
  char *cs = s;
  
  for(i = 0; i < n; i++)
    cs[i] = c;

  return s;
}

void *heap, *heaplimit;


/* 
 * Configuration of the memory :
 * ___________________________________ 
 *     |      |      |           |
 * ... | prev | next | size_next | ... 
 * ____|______|______|___________|____
 *                               ^
 *                               |
 *                 (void*) cur ---
 *
 *  This is a doubly linked list. size_prev and size_next represent the size one
 *  can really use. 
 *  We also reserve the first emplacement for a pointer to the first free place.
 *  We use next fit.
 */

void* last_seen;

#ifdef MALLOC
void *malloc(size_t size)
{
  int s = sizeof(void*);
  
 /* Number of used pages */
  int pSize = (size >> PageShift) + 1;
  /* Rounded size */
  int rSize = pSize << PageShift;
#ifdef DEBUG
  c_printf("malloc called (%u - %u)\n",size, rSize);
#endif
  void *cur, *new, *prv, *nxt;
  int b;
  for (cur = last_seen, b = 0; 
       cur != last_seen || !b; 
       b = 1,cur = (*(void**) (cur - 2*s) == NULL) ?
	      *(void**) (cur - 2*s) : 
	      *(void**) (heap + HEAP_OFFSET))
  {
    if (*(int*)(cur-s) >= rSize + s)
    {
      /* We've found the place */
      new = cur + rSize;
      prv = *(void**) (cur - 3*s);
      nxt = *(void**) (cur - 2*s);
      if (prv)
	*(void**) (prv - 2*s) = new;
      if (nxt)
	*(void**) (nxt - 3*s) = new;
      *(void**) (new - 3*s) = prv;
      *(void**) (new - 2*s) = nxt;
      *(int*)   (new - s)   = *(int*) (cur - s) - rSize;
      last_seen = nxt;
      if (cur == *(void**)(heap + HEAP_OFFSET))
	*(void**)(heap + HEAP_OFFSET) = new;
      *(int*)(cur - 3*s) = rSize;
#ifdef DEBUG
    c_printf ("new: %p (prev: %p, next: %p, size: %d)\n", new, *(void**)(new-3*s), *(void**)(new-2*s), *(int*)(new-s));
#endif
      return (cur-2*s);
    }
  }
#ifdef DEBUG
  c_printf ("Same Player, shoot again\n");
#endif
  /* If qemu segfaults, we see nothing */
  while (1);
  return NULL;
}

void free (void* ptr)
{
  int s = sizeof(void*);
  int size = *(int*) (ptr - s);
  void *cur, *prv, *nxt, *sup_nxt;
/*#ifdef DEBUG*/
  c_printf ("calling free %p\n", ptr);
/*#endif*/
  /* find out if we should merge two empty spaces */
  for (cur = *(void**) (heap + HEAP_OFFSET); *(void**)(cur - 2*s) < ptr; cur = *(void**)(cur - 2*s))
    if (cur + *(int*) (cur - s) + s == ptr)
    {
      if (ptr + size + 3*s == *(void**) (cur - 2*s))
      {
	nxt     = *(void**) (cur - 2*s);
	sup_nxt = *(void**) (nxt - 2*s);
	*(int*)   (cur - s)      += size + *(int*) (nxt - s) + size + 4*s;
	*(void**) (cur - 2*s)     = sup_nxt;
	*(void**) (sup_nxt - 3*s) = cur;
	c_printf ("free: merged three of them\n");
      } else
      {
	*(int*) (cur - s) += size + s;
	c_printf ("free: merged with before\n");
      }
      return;
    } else {
      if (ptr + size + 3*s == *(void**) (cur - 2*s))
      {
	nxt     = *(void**) (cur - 2*s);
	sup_nxt = *(void**) (nxt - 2*s); 
	*(void**) (ptr - s)   = cur;
	*(void**)  ptr        = *(void**) (nxt - 2*s);
	*(int*)   (ptr + s)   = size + 4*s + *(int*) (nxt - s);
	*(void**) (cur - 2*s) = ptr + 2*s;
	*(void**) (nxt - 3*s) = ptr + 2*s;
	c_printf ("free: merged with after\n");
	return;
      }
    }
  prv = *(void**) (cur - 3*s);
  *(void**) (ptr - s)   = prv;
  *(void**)  ptr        = cur;
  *(int*)   (ptr + s)   = size;
  *(void**) (prv - 2*s) = ptr + 2*s;
  *(void**) (cur - 3*s) = ptr + 2*s;
  c_printf ("free: no merges\n");
}
#endif

#ifndef MALLOC
void* malloc(size_t size)
{
#ifdef DEBUG
  c_printf ("heap: %p, heaplimit: %p\n", heap, heaplimit);
#endif
  void *ans = heap;
  if (heap+size>heaplimit)
    return NULL;
  heap += size;
  return ans;
}


void free(void *ptr)
{
#ifdef DEBUG
  c_printf("free(%p) called\n",ptr);
#endif
}

#endif
void* malloc_frame_aligned(size_t size)
{
  int offs = (int)heap % FRAME_SIZE;
  
  if (offs)
  {
    heap += (FRAME_SIZE - offs);
  }
  return (malloc(size));
}


void *realloc(void *ptr, size_t size)
{
#ifdef DEBUG
  c_printf("realloc(%p,%u) called\n",ptr,size);
#endif
  void *ans = NULL;
  if (size) {
    ans = malloc(size);
    if (!ans)
      return NULL;
    if (ptr)
      memcpy(ans, ptr, size);
  }
  free(ptr);
  return ans;
}

/* Signals */

#define sigset_t int

int sigemptyset(sigset_t *set)
{
#ifdef DEBUG
  c_printf("sigemptyset(%p) called\n",set);
#endif
  return notImpl_int();
}

int sigaddset(sigset_t *set, int signum)
{
#ifdef DEBUG
  c_printf("sigdaddset(%p,%i) called\n",set,signum);
#endif
  return notImpl_int();
}

int sigdelset(sigset_t *set, int signum)
{
#ifdef DEBUG
  c_printf("sigdelset(%p,%i) called\n",set,signum);
#endif
  return notImpl_int();
}

#define siginfo_t void

struct sigaction
{
  void (*sa_handler)(int);
  void (*sa_sigaction)(int, siginfo_t *, void *);
  sigset_t sa_mask;
  int sa_flags;
  void (*sa_restorer)(void);
};

int sigaction(int signum,const struct sigaction *act,struct sigaction *oldact)
{
#ifdef DEBUG
  c_printf("sigaction(%i,%p,%p) called\n",signum,act,oldact);
#endif
  return notImpl_int();
}

int sigprocmask(int how, const sigset_t *set, sigset_t *oldset)
{
#ifdef DEBUG
  c_printf("sigprocmask(%i,%p,%p) called\n",how,set,oldset);
#endif
  return notImpl_int();
}

typedef struct
{
  void  *ss_sp;
  int    ss_flags;
  size_t ss_size;
} stack_t;

static stack_t sigstack;

int sigaltstack(const stack_t *ss, stack_t *oss)
{
#ifdef DEBUG
  c_printf("sigaltstack(%p,%p) called\n",ss,oss);
#endif
  if (oss)
    memcpy(oss,&sigstack,sizeof(*oss));
  memcpy(&sigstack,ss,sizeof(stack_t));
  return 0;
}

#define sigjmp_buf void*

int __sigsetjmp(sigjmp_buf env, int savesigs)
{
#ifdef DEBUG
  c_printf("__sigsetjmp(%i) called\n",savesigs);
#endif
  memset(&env,0,sizeof(env));
  return 0;
}

/* I/O */

FILE *stdin=(FILE *)(&stdin);
FILE *stdout=(FILE *)(&stdin+1);
FILE *stderr=(FILE *)(&stdin+2);

int sscanf(const char *str, const char *format, ...)
{
#ifdef DEBUG
  c_printf("sscanf(%s,%s) called\n",str,format);
#endif
  return notImpl_int();
}

static char buf[1024];

int printf(const char *fmt, ...)
{
  va_list args;
  int i;
  /* !! not reentrant !! */
  va_start(args,fmt);
  i = vsnprintf(buf,sizeof(buf),fmt,args);
  va_end(args);
  print_string(buf);
  return i;
}

int c_printf(const char *fmt, ...)
{
  va_list args;
  int i;
  /* !! not reentrant !! */
  va_start(args,fmt);
  i = vsnprintf(buf,sizeof(buf),fmt,args);
  va_end(args);
  c_print_string(buf);
  return i;
}

int snprintf(char *buf, size_t size, const char *fmt, ...)
{
  va_list args;
  int i;
  va_start(args,fmt);
  i = vsnprintf(buf,size,fmt,args);
  va_end(args);
  return i;
}

int puts(const char *s)
{
  print_string(s);
  print_char('\n');
  return 0;
}

int fprintf(FILE *stream, const char *format, ...)
{
#ifdef DEBUG
  c_printf("fprintf(%p,%s) called\n",stream,format);
#endif
  if (stream != stdin && stream != stdout && stream != stderr)
    return notImpl_int();
  else {
    va_list args;
    int i;
    va_start(args,format);
    i = vsnprintf(buf,sizeof(buf),format,args);
    va_end(args);
    print_string(buf);
    return i;
  }
}

int sprintf(char *str, const char *format, ...)
{
#ifdef DEBUG
  c_printf("sprintf(%p,%s) called\n",str,format);
#endif
  va_list args;
  int i;
  va_start(args,format);
  i = vsnprintf(str,INT_MAX,format,args);
  va_end(args);
  return i;
}

/* Files */

int open(const char *pathname, int flags, ...)
{
#ifdef DEBUG
  c_printf("open(%s,%i) called\n",pathname,flags);
#endif
  return notImpl_int();
}

int close(int fd)
{
#ifdef DEBUG
  c_printf("close(%i) called\n",fd);
#endif
  return notImpl_int();
}

ssize_t read(int fd, void *buf, size_t count)
{
  int c;
#ifdef DEBUG
  /*c_printf("read(%i,%p,%u) called\n",fd,buf,count);*/
#endif
  if (!count) return 0;
  if (fd != 0 && fd != 1 && fd != 2)
    return notImpl_int();
  c = getch();
  if (c == EOF) return 0;
  *(unsigned char *)buf = c;
  return 1;
}

ssize_t write(int fd, const void *buf, size_t count)
{
#ifdef DEBUG
  /*c_printf("write(%i,%p,%u) called\n",fd,buf,count);*/
#endif
  size_t remain = count;
  const unsigned char *cbuf = buf;
  if (fd != 0 && fd == 1 && fd == 2)
    return notImpl_int();
  while (remain--)
    print_char(*cbuf++);
  return count;
}

int fcntl(int fd, int cmd)
{
#ifdef DEBUG
  c_printf("fcntl(%i,%i) called\n",fd,cmd);
#endif
  return notImpl_int();
}

#define off64_t long int

off64_t lseek64(int fd, off64_t offset, int whence)
{
#ifdef DEBUG
  c_printf("lseek64(%i,%li,%i) called\n",fd,offset,whence);
#endif
  return notImpl_int();
}

int open64(const char *__file,int __oflag,...)
{
#ifdef DEBUG
  c_printf("open64(%s,%i) called\n",__file,__oflag);
#endif
  return notImpl_int();
}

int fputc(int c, FILE *stream)
{
#ifdef DEBUG
  c_printf("fputc(%i,%p) called\n",c,stream);
#endif
  return notImpl_int();
}

int fputs(const char *s, FILE *stream)
{
#ifdef DEBUG
  c_printf("fputs(%s,%p) called\n",s,stream);
#endif
  return notImpl_int();
}

size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream)
{
#ifdef DEBUG
  c_printf("fwrite(%p,%u,%u,%p) called\n",ptr,size,nmemb,stream);
#endif
  return notImpl_int();
}

int fflush(FILE *stream)
{
#ifdef DEBUG
  c_printf("fflush(%p) called\n",stream);
#endif
  return notImpl_int();
}

#define __dev_t      unsigned long long int
#define __ino_t      unsigned long int
#define __mode_t     unsigned int
#define __nlink_t    unsigned int
#define __uid_t      unsigned int
#define __gid_t      unsigned int
#define __off64_t    long long int
#define __blksize_t  long int
#define __blkcnt64_t long long int
#define __time_t     long int
#define __ino64_t    unsigned long long int

struct stat64
  {
    __dev_t st_dev;
    unsigned int __pad1;
    __ino_t __st_ino;
    __mode_t st_mode;
    __nlink_t st_nlink;
    __uid_t st_uid;
    __gid_t st_gid;
    __dev_t st_rdev;
    unsigned int __pad2;
    __off64_t st_size;
    __blksize_t st_blksize;
    __blkcnt64_t st_blocks;
    __time_t st_atime;
    unsigned long int st_atimensec;
    __time_t st_mtime;
    unsigned long int st_mtimensec;
    __time_t st_ctime;
    unsigned long int st_ctimensec;
    __ino64_t st_ino;
  };

int __xstat64(int __ver,__const char *__filename,struct stat64 *__stat_buf)
{
#ifdef DEBUG
  c_printf("__xstat64(%i,%s,%p) called\n",__ver,__filename,__stat_buf);
#endif
  return notImpl_int();
}

/* Dirs */

#define DIR void

DIR *opendir(const char *name)
{
#ifdef DEBUG
  c_printf("opendir(%s) called\n",name);
#endif
  return notImpl_ptr();
}

int closedir(DIR *dir)
{
#ifdef DEBUG
  c_printf("closedir(%p) called\n",dir);
#endif
  return notImpl_int();
}

char *getcwd(char *buf, size_t size)
{
#ifdef DEBUG
  c_printf("getcwd(%p,%u) called\n",buf,size);
#endif
  static const char pwd[] = "/";
  if (size<sizeof(pwd)) {
    errno = ERANGE;
    return NULL;
  }
  memcpy(buf,pwd,sizeof(pwd));
  return buf;
}

int chdir(const char *path)
{
#ifdef DEBUG
  c_printf("chdir(%s) called\n",path);
#endif
  return notImpl_int();
}

int readlink(const char *path, char *buf, size_t bufsiz)
{
#ifdef DEBUG
  c_printf("readlink(%s,%p,%u called\n",path,buf,bufsiz);
#endif
  return notImpl_int();
}

int rename(const char *oldpath, const char *newpath)
{
#ifdef DEBUG
  c_printf("rename(%s,%s) called\n",oldpath,newpath);
#endif
  return notImpl_int();
}

int unlink(const char *pathname)
{
#ifdef DEBUG
  c_printf("unlink(%s) called\n",pathname);
#endif
  return notImpl_int();
}

struct dirent64
{
  __ino64_t d_ino;
  __off64_t d_off;
  unsigned short int d_reclen;
  unsigned char d_type;
  char d_name[256];
};

struct dirent64 *readdir64(DIR *__dirp)
{
#ifdef DEBUG
  c_printf("readdir64(%p) called\n",__dirp);
#endif
  return notImpl_ptr();
}

/* Strings */

size_t strlen(const char *s)
{
#ifdef DEBUG
  /*c_printf("strlen(%s) called\n",s);*/
#endif
  int i;
  for (i = 0; s[i] != '\0'; i++);
  return i;
}

size_t strnlen(const char *s, size_t maxlen)
{
#ifdef DEBUG
  /*c_printf("strnlen(%p,%u) called\n",s,maxlen);*/
#endif
  int i;
  for (i = 0; i<maxlen && s[i] != '\0'; i++);
  return i;
}

char *strcpy(char *dest, const char *src)
{
#ifdef DEBUG
  c_printf("strcpy(%p,%s) called\n",dest,src);
#endif
  return memmove(dest, src, strlen(src) + 1);
}

char *strcat(char *dest, const char *src)
{
#ifdef DEBUG
  c_printf("strcat(%p,%s) called\n",dest,src);
#endif
  memmove(dest + strlen(dest), src, strlen(src) + 1);
  return dest;
}

static char *sys_errlist[] = {
  [0]      = "No error",
  [ENOSYS] = "Function not implemented",
  [ERANGE] = "Result too large",
  [EAGAIN] = "Try again",
};
static int sys_nerr = sizeof(sys_errlist)/sizeof(*sys_errlist);

char *strerror(int errnum)
{
#ifdef DEBUG
  c_printf("strerror(%i) called\n",errnum);
#endif
  if (errnum < sys_nerr && sys_errlist[errnum])
    return sys_errlist[errnum];
  return "unknown error";
}

int strcmp(const char *s1, const char *s2)
{
#ifdef DEBUG
  c_printf("strcmp(%s,%s) called\n",s1,s2);
#endif
  while (*s1 && *s2 && *s1==*s2) {
    s1++;
    s2++;
  }
#ifdef DEBUG
  c_printf ("strcmp done\n");
#endif
  if (*s1<*s2) return -1;
  else if (*s2>*s1) return 1;
  else return 0;
}

long int __strtol_internal(const char *__nptr,char **__endptr,
			   int __base, int __group)
{
#ifdef DEBUG
  c_printf("__strtol_internal(%s,%p,%i,%i) called\n",__nptr,__endptr,__base,__group);
#endif
  errno = ERANGE;
  return LONG_MIN;
}

double __strtod_internal(const char *__nptr,char **__endptr,int __group)
{
#ifdef DEBUG
  c_printf("__strtod_internal(%s,%p,%i) called\n",__nptr,__endptr,__group);
#endif
  errno = ERANGE;
  return HUGE_VAL;
}

/* Trigo */

double exp(double x)
{
  notImpl_fp();
}

double frexp(double x, int *exp)
{
  notImpl_fp();
}

double ldexp(double x, int exp)
{
  notImpl_fp();
}

double log(double x)
{
  notImpl_fp();
}

double log10(double x)
{
  notImpl_fp();
}

double modf(double x, double *iptr)
{
  notImpl_fp();
}

double sqrt(double x)
{
  notImpl_fp();
}

double pow(double x, double y)
{
  notImpl_fp();
}

#  define __sincos_code \
  register long double __cosr;                                                \
  register long double __sinr;                                                \
  __asm __volatile__                                                          \
    ("fsincos\n\t"                                                            \
     "fnstsw    %%ax\n\t"                                                     \
     "testl     $0x400, %%eax\n\t"                                            \
     "jz        1f\n\t"                                                       \
     "fldpi\n\t"                                                              \
     "fadd      %%st(0)\n\t"                                                  \
     "fxch      %%st(1)\n\t"                                                  \
     "2: fprem1\n\t"                                                          \
     "fnstsw    %%ax\n\t"                                                     \
     "testl     $0x400, %%eax\n\t"                                            \
     "jnz       2b\n\t"                                                       \
     "fstp      %%st(1)\n\t"                                                  \
     "fsincos\n\t"                                                            \
     "1:"                                                                     \
     : "=t" (__cosr), "=u" (__sinr) : "0" (__x));                             \
  *__sinx = __sinr;                                                           \
  *__cosx = __cosr

void __sincos (double __x, double *__sinx, double *__cosx)
{
  __sincos_code;
}

double sin(double x)
{
  double ans, dummy;
  __sincos(x, &ans, &dummy);
  return (ans);
}

double sinh(double x)
{
  notImpl_fp();
}

double cos(double x)
{
  double ans, dummy;
  __sincos(x, &dummy, &ans);
  return (ans);
}

double cosh(double x)
{
  notImpl_fp();
}

double tan(double x)
{
  return (sin(x)/cos(x));
}

double tanh(double x)
{
  notImpl_fp();
}

double asin(double x)
{
  notImpl_fp();
}

double acos(double x)
{
  notImpl_fp();
}

double atan(double x)
{
  notImpl_fp();
}

double atan2 (double y, double x)
{
  notImpl_fp();
}

double fmod (double x, double y)
{
  notImpl_fp();
}

/* DL */

void *dlopen(const char *filename, int flag)
{
#ifdef DEBUG
  c_printf("dlopen(%s,%i) called\n",filename,flag);
#endif
  return notImpl_ptr();
}

char *dlerror(void)
{
#ifdef DEBUG
  c_printf("dlerror called\n");
#endif
  return notImpl_ptr();
}

void *dlsym(void *handle, const char *symbol)
{
#ifdef DEBUG
  c_printf("dlsym(%p,%s) called\n",handle,symbol);
#endif
  return notImpl_ptr();
}

int dlclose(void *handle)
{
#ifdef DEBUG
  c_printf("dlsym(%p) called\n",handle);
#endif
  return notImpl_int();
}

/* Time */

int gettimeofday(struct timeval *tv, struct timezone *tz)
{
#ifdef DEBUG
  /*c_printf("gettimeofday(%p,%p) called\n",tv,tz);*/
#endif
  /* le temps est grossier: on a des machines à 1GHz en gros, on va dire */
  tick_t tick;

  if (!tv) return notImpl_int();

  ticks(tick);
  tick.tick /= 1000ULL;
  tv->tv_sec = tick.tick / 1000000ULL;
  tv->tv_usec = tick.tick % 1000000ULL;
  return 0;
}

void usleep(unsigned long usec)
{
  struct timeval tv1,tv2;
  gettimeofday(&tv1,NULL);
  do {
    gettimeofday(&tv2,NULL);
  } while (((tv2.tv_sec-tv1.tv_sec)*1000000+(tv2.tv_usec-tv1.tv_usec))<usec);
}

#define clock_t int

struct tms {
  clock_t tms_utime;  /* user time */
  clock_t tms_stime;  /* system time */
  clock_t tms_cutime; /* user time of dead children */
  clock_t tms_cstime; /* system time of dead children */
};

clock_t times(struct tms *buf)
{
#ifdef DEBUG
  c_printf("times(%p) called\n",buf);
#endif
  /* De même, on va dire que les ticks sont à 1KHz, en supposant une horloge à 1GHz */
  tick_t tick;
  ticks(tick);
  buf->tms_stime = 0;
  buf->tms_utime = tick.tick / 1000000ULL;
  buf->tms_cutime = 0;
  buf->tms_cstime = 0;
  return buf->tms_utime;
}

/* Resource */

#define __rlimit_resource_t int
#define rlim_t unsigned long int
struct rlimit
{
  rlim_t rlim_cur;
  rlim_t rlim_max;
};

int getrlimit64(__rlimit_resource_t __resource,struct rlimit *__rlimits)
{
#ifdef DEBUG
  c_printf("getrlimit64(%i,%p) called\n",__resource,__rlimits);
#endif
  return notImpl_int();
}

static const char _nl_C_LC_CTYPE_class[768] /* attribute_hidden */ =
  /* 0x80 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x86 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x8c */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x92 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x98 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x9e */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xa4 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xaa */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xb0 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xb6 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xbc */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xc2 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xc8 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xce */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xd4 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xda */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xe0 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xe6 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xec */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xf2 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xf8 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xfe */ "\000\000" "\000\000" "\002\000" "\002\000" "\002\000" "\002\000"
  /* 0x04 */ "\002\000" "\002\000" "\002\000" "\002\000" "\002\000" "\003\040"
  /* 0x0a */ "\002\040" "\002\040" "\002\040" "\002\040" "\002\000" "\002\000"
  /* 0x10 */ "\002\000" "\002\000" "\002\000" "\002\000" "\002\000" "\002\000"
  /* 0x16 */ "\002\000" "\002\000" "\002\000" "\002\000" "\002\000" "\002\000"
  /* 0x1c */ "\002\000" "\002\000" "\002\000" "\002\000" "\001\140" "\004\300"
  /* 0x22 */ "\004\300" "\004\300" "\004\300" "\004\300" "\004\300" "\004\300"
  /* 0x28 */ "\004\300" "\004\300" "\004\300" "\004\300" "\004\300" "\004\300"
  /* 0x2e */ "\004\300" "\004\300" "\010\330" "\010\330" "\010\330" "\010\330"
  /* 0x34 */ "\010\330" "\010\330" "\010\330" "\010\330" "\010\330" "\010\330"
  /* 0x3a */ "\004\300" "\004\300" "\004\300" "\004\300" "\004\300" "\004\300"
  /* 0x40 */ "\004\300" "\010\325" "\010\325" "\010\325" "\010\325" "\010\325"
  /* 0x46 */ "\010\325" "\010\305" "\010\305" "\010\305" "\010\305" "\010\305"
  /* 0x4c */ "\010\305" "\010\305" "\010\305" "\010\305" "\010\305" "\010\305"
  /* 0x52 */ "\010\305" "\010\305" "\010\305" "\010\305" "\010\305" "\010\305"
  /* 0x58 */ "\010\305" "\010\305" "\010\305" "\004\300" "\004\300" "\004\300"
  /* 0x5e */ "\004\300" "\004\300" "\004\300" "\010\326" "\010\326" "\010\326"
  /* 0x64 */ "\010\326" "\010\326" "\010\326" "\010\306" "\010\306" "\010\306"
  /* 0x6a */ "\010\306" "\010\306" "\010\306" "\010\306" "\010\306" "\010\306"
  /* 0x70 */ "\010\306" "\010\306" "\010\306" "\010\306" "\010\306" "\010\306"
  /* 0x76 */ "\010\306" "\010\306" "\010\306" "\010\306" "\010\306" "\004\300"
  /* 0x7c */ "\004\300" "\004\300" "\004\300" "\002\000" "\000\000" "\000\000"
  /* 0x82 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x88 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x8e */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x94 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x9a */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xa0 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xa6 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xac */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xb2 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xb8 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xbe */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xc4 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xca */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xd0 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xd6 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xdc */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xe2 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xe8 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xee */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xf4 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xfa */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
;

static const unsigned short int *__ctype_b =
  (const unsigned short int *)_nl_C_LC_CTYPE_class + 128;

const unsigned short int **__ctype_b_loc(void)
{
  return &__ctype_b;
}
