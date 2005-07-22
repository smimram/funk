/* vsprintf.h -- Lars Wirzenius & Linus Torvalds. */
/*
 * Wirzenius wrote this portably, Torvalds fucked it up :-)
 */

#ifndef __PRINTF_H
#define __PRINTF_H

#include <stdarg.h>

int vsnprintf(char *buf, size_t size, const char *fmt, va_list args);

#endif

