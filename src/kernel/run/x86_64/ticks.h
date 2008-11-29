/* ticks.h [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : header used by time functions in the dummy libc
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

#ifndef __TICKS_H
#define __TICKS_H

typedef union {
  unsigned long long tick;
  struct {
    unsigned long low;
    unsigned long high;
  } sub;
} tick_t;

#define ticks(tick) __asm__ __volatile__("rdtsc" : "=a" \
                ((tick).sub.low),"=d" ((tick).sub.high));

#endif /* __TICKS_H */

