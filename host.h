#include <stdio.h>
#ifdef LINUX
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
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
typedef uint8_t uint8;
#else
typedef char uint8;
#endif
typedef char sint8;
typedef unsigned short uint16;
typedef short sint16;
typedef long uint32;
typedef long sint32;
typedef unsigned int uintptr;

#define SEEK_SET    0   /* set file offset to offset */
#define SEEK_CUR    1   /* set file offset to current plus offset */
#define SEEK_END    2   /* set file offset to EOF plus offset */

#ifdef LINUX
#define FOPENRB(x) fopen(x,"rb")
#define FOPENWB(x) fopen(x,"wb")
#else
#define FOPENRB(x) fopenb(x,"r")
#define FOPENWB(x) fopenb(x,"w")
#endif

#define STRDUP(s) hstrdup(s)
#define MEMSET(dest, ch, len) hmemset(dest, ch, len)
#define MEMMOVE(dest, src, len) hmemmove(dest, src, len)
#define RAND(range) hrand(range)

/* zcc has no prototypes for fseek and will mess up the arg pass if we don't cast to long */
#define FSEEK(f, offs, whence) fseek(f, (long)(offs), (unsigned short)(whence))
#define FREAD(b, sz, nem, f) fread(b, (uint16)(sz), (uint16)(nem), f)

unsigned short hrand(amount);
char *hstrdup(s);
voidret hmemset(dest, ch, len);
voidret hmemmove(dest, src, len);
