#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

#include "libc-dummy.h"

/* Hum, where could all this code come from?... */

#define CLOCK_TICK_RATE 1193182 /* Underlying HZ */

#define HZ 100

/* LATCH is used in the interval timer and ftape setup. */
#define LATCH  ((CLOCK_TICK_RATE + HZ/2) / HZ)  /* For divider */

#define CALIBRATE_LATCH (5 * LATCH)

static __inline void mach_prepare_counter(void)
{
  /* Set the Gate high, disable speaker */
  outb((inb(0x61) & ~0x02) | 0x01, 0x61);

  /*
   * Now let's take care of CTC channel 2
   *
   * Set the Gate high, program CTC channel 2 for mode 0,
   * (interrupt on terminal count mode), binary count,
   * load 5 * LATCH count, (LSB and MSB) to begin countdown.
   *
   * Some devices need a delay here.
   */
  outb(0xb0, 0x43);                       /* binary, mode 0, LSB/MSB, Ch 2 */
  outb_p(CALIBRATE_LATCH & 0xff, 0x42);   /* LSB of count */
  outb_p(CALIBRATE_LATCH >> 8, 0x42);       /* MSB of count */
}

#define rdtsc(low,high) \
  __asm__ __volatile__("rdtsc" : "=a" (low), "=d" (high))

static __inline void mach_countup(unsigned long *count_p)
{
  unsigned long count = 0;
  do
  {
    count++;
  } while ((inb_p(0x61) & 0x20) == 0);
  *count_p = count;
}

/* ------ Calibrate the TSC -------
 * Return 2^32 * (1 / (TSC clocks per usec)) for do_fast_gettimeoffset().
 * Too much 64-bit arithmetic here to do this cleanly in C, and for
 * accuracy's sake we want to keep the overhead on the CTC speaker (channel 2)
 * output busy loop as low as possible. We avoid reading the CTC registers
 * directly because of the awkward 8-bit access mechanism of the 82C54
 * device.
 */

#define CALIBRATE_TIME  (5 * 1000020/HZ)

unsigned long calibrate_tsc(void)
{
  mach_prepare_counter();

  {
    unsigned long startlow, starthigh;
    unsigned long endlow, endhigh;
    unsigned long count;

    rdtsc(startlow,starthigh);
    mach_countup(&count);
    rdtsc(endlow,endhigh);


    /* Error: ECTCNEVERSET */
    if (count <= 1)
      goto bad_ctc;

    /* 64-bit subtract - gcc just messes up with long longs */
    __asm__("subl %2,%0\n\t"
	"sbbl %3,%1"
	:"=a" (endlow), "=d" (endhigh)
	:"g" (startlow), "g" (starthigh),
	"0" (endlow), "1" (endhigh));

    /* Error: ECPUTOOFAST */
    if (endhigh)
      goto bad_ctc;

    /* Error: ECPUTOOSLOW */
    if (endlow <= CALIBRATE_TIME)
      goto bad_ctc;

    __asm__("divl %2"
	:"=a" (endlow), "=d" (endhigh)
	:"r" (endlow), "0" (0), "1" (CALIBRATE_TIME));

    return endlow;
  }

  /*
   * The CTC wasn't reliable: we got a hit on the very first read,
   * or the CPU was so fast/slow that the quotient wouldn't fit in
   * 32 bits..
   */
bad_ctc:
  return 0;
}

CAMLprim value caml_funk_cpu_get_freq(value vunit)
{
  CAMLparam1(vunit);
  unsigned long cpu_khz = 0;
  unsigned long tsc_quotient = calibrate_tsc();
  
  if (tsc_quotient)
  {
    /* report CPU clock rate in Hz.
     * The formula is (10^6 * 2^32) / (2^32 * 1 / (clocks/us)) =
     * clock/second. Our precision is about 100 ppm.
     */
    {
      unsigned long eax=0, edx=1000;
      __asm__("divl %2"
	  :"=a" (cpu_khz), "=d" (edx)
	  :"r" (tsc_quotient),
	  "0" (eax), "1" (edx));
      /* printk("Detected %lu.%03lu MHz processor.\n", cpu_khz / 1000, cpu_khz % 1000); */
    }
  }

  CAMLreturn(caml_copy_double(((double)cpu_khz)/1000));
}
