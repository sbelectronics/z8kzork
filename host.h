#include <stdio.h>
#ifdef LINUX
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define HAVE_FILES
#endif
#ifdef OLIVETTI
/* no stdint.h for z8kgcc on Olivetti */
#include <stdlib.h>
#include <string.h>
#define HAVE_FILES
#endif

#ifdef OLIVETTI
/* olivetti needs this, otherwise errors while optimizing */
#define VOLATILE volatile
#else
/* assumption is zcc doesn't like this */
#define VOLATILE
#endif

/* zcc hates the static keyword */
#define static /**/

/* zcc gets wound up if you use const */
#define const /**/

/* zcc seems to really dislike the void keyword; also dislikes funcs that don't declare a return kind */
typedef int voidret;
#define voidarg /**/
#define voidreturn return 0;

#ifdef SUPPORTS_UINT8
/* for Linux... */
typedef uint8_t uint8;
#else
#ifdef OLIVETTI
/* Olivetti char is unsigned by default. Make it signed for
 * parity with the ZCC version, which only has signed chars.
 */
typedef signed char uint8;
#else
/* ZCC only allows signed chars and they're simply called char. */
typedef char uint8;
#endif
#endif
#ifdef OLIVETTI
/* Again with the need to specify signed for z8kgcc/Olivetti */
typedef signed char sint8;
#else
/* ... and with ZCC assuming all chars are signed... */
typedef char sint8;
#endif
typedef unsigned short uint16;
typedef short sint16;
typedef long uint32;
typedef long sint32;
typedef unsigned int uintptr;

#define SEEK_SET    0   /* set file offset to offset */
#define SEEK_CUR    1   /* set file offset to current plus offset */
#define SEEK_END    2   /* set file offset to EOF plus offset */

#ifdef HAVE_FILES
/* Olivetti and Linux */
#define FOPENRB(x) fopen(x,"rb")
#define FOPENWB(x) fopen(x,"wb")
#else
/* ZCC */
#define FOPENRB(x) fopenb(x,"r")
#define FOPENWB(x) fopenb(x,"w")
#endif

/* Define these ourselves because they don't exist on ZCC */
#define STRDUP(s) hstrdup(s)
#define MEMSET(dest, ch, len) hmemset(dest, ch, len)
#define MEMMOVE(dest, src, len) hmemmove(dest, src, len)
#define RAND(range) hrand(range)

/* zcc has no prototypes for fseek and will mess up the arg pass if we don't cast to long */
#define FSEEK(f, offs, whence) fseek(f, (long)(offs), (unsigned short)(whence))
#define FREAD(b, sz, nem, f) fread(b, (uint16)(sz), (uint16)(nem), f)

#ifdef OLIVETTI
/* Z8KGCC for Olivetti does support prototypes, and we will mess up parameter pass
 * if we do not use them. Kinda the opposite situation as zcc...
 */
unsigned short hrand(unsigned short amount);
char *hstrdup(char *s);
voidret hmemset(char *dest, char ch, unsigned short len);
voidret hmemmove(char *dest, char *src, unsigned short len);
#else
/* ZCC - no prototypes. Len is passed as an unsigned short */
unsigned short hrand(amount);
char *hstrdup(s);
voidret hmemset(dest, ch, len);
voidret hmemmove(dest, src, len);
#endif
