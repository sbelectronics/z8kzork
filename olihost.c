/* host.c
 *
 * This file holds host-specific code, for character IO, file IO, etc.
 */

#include "host.h"

char *hstrdup(s)
char *s;
{
  char *dest = malloc(strlen(s)+1);
  strcpy(dest,s);
  return dest;
}

voidret hmemset(dest, ch, len)
char *dest;
char ch;
unsigned short len;
{
  unsigned short i;

  for (i=0; i<len; i++) {
    dest[i] = ch;
  }
  voidreturn;
}

voidret hmemmove(dest, src, len)
char *dest;
char *src;
unsigned short len;
{
  unsigned short i;
  for (i=0; i<len; i++) {
    *dest = *src;
    dest++;
    src++;
  }
  voidreturn;
}

long seed = 1;

/* random nunmber - may be machine dependent */
unsigned short hrand(amount)
unsigned short amount;
{
    long int a = 16807L, m = 2147483647L, q = 127773L, r = 2836L;
    long int lo, hi, test;

    hi = seed / q;
    lo = seed % q;
    test = a * lo - r * hi;
    if (test > 0) 
		    seed = test; /* test for overflow */
    else 
		    seed = test + m;
    return(seed % amount);
}
