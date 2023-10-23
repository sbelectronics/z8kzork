/* 
 * MojoZork; a simple, just-for-fun implementation of Infocom's Z-Machine.
 * 
 * Mercilessly butchered by smbaker to make it work on CP/M-8000
 * 
 * Multiple things turned this program into a bit of a mess:
 *   1) zcc can't handle reads/writes of words from non-word-aligned addrs
 *   2) zcc doesn't have an "unsigned char" type, or I don't know what
 *      it's called.
 *   3) zcc doesn't type-convert arguments. If you pass an int to a 
 *      function that takes a long, it'll pass it incorrectly.
 *   4) insufficient memory to fit the whole story in RAM at once
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 * Originally by Ryan C. Gordon.
 * 
 * Build:
 * 
 *  zcc zork.z8k
 *  zcc host.z8k
 * 	a:ld8k -i -w -s -o zork.z8k startup.o z8kzork.o host.o -lcpm
 *
 * The Z-Machine specifications 1.1:
 *     https://inform-fiction.org/zmachine/standards/z1point1/index.html
 */

#include "host.h"

#define READUI8(ptr) *(ptr++)
#define READUI16(ptr) ((((uint16) QSE(ptr[0])) << 8) | ((uint16) QSE(ptr[1]))); ptr += sizeof (uint16)
#define WRITEUI16(dst, src) { *(dst++) = (uint8) ((src >> 8) & 0xFF); *(dst++) = (uint8) (src & 0xFF); }

/* like READUI16 but without the built-in increment */
#define RUI16NI(ptr) ((((uint16) QSE(ptr[0])) << 8) | ((uint16) QSE(ptr[1])));

#define dbg if (Debug) printf

#define ASSERT(x,y) if (!(x)) { printf(y); exit(-1); }

/* some macros to handle fetching bytes from memory */

#define PCPTRO(x) pcptro((uint32) (x))
#define PCPTR pcptr()
#define PCBI pcbi()
#define PCBIW pcbiw()
#define PCWI pcwi()
#define PCOFS GState->virtpc
#define PCSET(x) GState->virtpc=(x)
#define PCADD(x) GState->virtpc+=(x)

/* zcc does not understand "unsigned char" ... or I didn't figure out how to do it. */
/* turn a signed char into a unsigned short */
#define QSE(x) ((uint16)(((x)<0) ? (256+(x)) : (x)))

/* We can't fit zork1 into memory, so we'll load the data portion, and then
 * page the code portion. 20 code pages of 1K each.
 * 
 * Get too close to the limit on memory, and the damn thing will just silently
 * exit when we read the header from the file or do other weird crap. It probably
 * runs out of stack space or temp space. I chose about 17K with some slack.
 * 
 * We use an allocate and reap scheme reminiscent of page cache implementation.
 * 
 * BLKSLACK keeps some slack around the blocks, as I'm not confident some function
 * won't walk a pointer a few extra bytes off the end of a block. Reducing it to 0
 * will break "(providing light)" in inventory, for example.
 */

#define NUM_SLOTS 256
#define BLKSIZE 512
#define BLKSLACK 4

#ifdef OLIVETTI
/* Keep an eye on the map file. You can set this big enough so that it's larger
 * than a segment. I don't know what happens then...
 */
#define HEAP_SIZE (24576 + 1024 + 150)
#else
#define HEAP_SIZE (16384 + 1024 + 150)
#endif

#define MAX_STORYBASE 24800

/* split-I/D on the z8k works better with static data than malloc */
uint8 StBase[MAX_STORYBASE];
uint8 Heap[HEAP_SIZE];
uint8 *Slots[NUM_SLOTS];
uint8 SlotHit[NUM_SLOTS];
uint8 *heapPtr;
uint8 *heapEnd;
uint16 reaper;

typedef voidret (*OpcodeFn)(voidarg);
typedef voidret (*DieFn)();         /* char *s */
typedef voidret (*WriteCharFn)();   /* char ch */

typedef struct
{
    const char *name;
    OpcodeFn fn;
} Opcode;

typedef struct ZHeader
{
    uint8 version;
    uint8 flags1;
    uint16 release;
    uint16 himem_addr;
    uint16 pc_start;  /* in ver6, packed address of main() */
    uint16 dict_addr;
    uint16 objtab_addr;
    uint16 globals_addr;
    uint16 staticmem_addr;  /* offset of static memory, also: size of dynamic mem. */
    uint16 flags2;
    char serial_code[7];  /* six ASCII chars in ver2. In ver3+: ASCII of completion date: YYMMDD */
    uint16 abbrtab_addr;  /* abbreviations table */
    uint16 story_len;
    uint16 story_checksum;
    /* !!! FIXME: more fields here, all of which are ver4+ */
} ZHeader;

typedef struct ZMachineState
{
    FILE *storyFile;
    uint8 *storyBase;
    uint32 storyBaseLen;

    uint32 instructions_run;
    ZHeader header;
    uint32 virtpc;
    uint32 logical_pc;
    uint16 *sp;       /* stack pointer */
    uint16 bp;        /* base pointer */
    int quit;
    int step_completed;  /* possibly time to break out of the Z-Machine simulation loop. */
    uint16 stack[2048];  /* !!! FIXME: make this dynamic? */
    uint16 operands[8];
    uint8 operand_count;
    char alphabet_table[78];
    const char *startup_script;
    char *story_filename;

    /* this is kinda wasteful (we could pack the 89 opcodes in their various forms
     *  into separate arrays and strip off the metadata bits) but it simplifies
     *  some things to just have a big linear array.
     */
    Opcode opcodes[256];

    /* The extended ones, however, only have one form, so we pack that tight. */
    Opcode extended_opcodes[30];

    WriteCharFn writechar;
    DieFn die;
} ZMachineState;

static ZMachineState *GState = NULL;
static uint8 Debug = 0;

/* turn a pointer into something that can be meaningfully printed */
uint16 PPP(v)
uint8* v;
{
    uint16 addr;
    uint16 i;
    if ((v>GState->storyBase) && (v<=(GState->storyBase+GState->storyBaseLen))) {
        addr = v-GState->storyBase;
        return addr;
    }

    for (i=0; i<NUM_SLOTS; i++) {
       if ((Slots[i] != NULL) && (v>=Slots[i]) && (v<Slots[i]+BLKSIZE+BLKSLACK)) {
            addr = v-Slots[i] + i*BLKSIZE;
            return addr;
       }
    }

    return v-((uint8*)GState->stack) + 0xE000;
}

voidret copyword(dest, src)
uint8 *dest;
uint8 *src;
{
    uint8 v;

    /* note a=*b will be assumed to be a=a*b */

    v = *src;
    *dest=v;

    v = *(src+1);
    *(dest+1)=v;
}

uint16 fetchCount = 0;

/* return pointer to the PC plus an offset */
uint8 *pcptro(offset)
uint32 offset;
{
    uint8 *ptr;
    int seekres;
    uint16 bread;
    uint16 slot;
    uint32 blkoffset;

    /* easy, it's in the first ~ 20K */
    if ((offset+BLKSLACK) < GState->storyBaseLen) {
        ptr = GState->storyBase + offset;
        return ptr;
    }

    slot = (offset/BLKSIZE);
    blkoffset = ((uint32) slot)*BLKSIZE;

    if (Slots[slot]!=NULL) {
        SlotHit[slot] = 1;
        ptr = Slots[slot] + offset - blkoffset;
        return ptr;
    }

    /* slot miss. fill da slot. */

    dbg("offset %X", offset);
    dbg(" fetching blkOffset %X", blkoffset);
    dbg(" for slot %d\n", slot);

    if (heapPtr > (heapEnd-BLKSIZE-BLKSLACK)) {
        /* don't fear the reaper */
        dbg("don't fear the reaper: heapPtr=%p, heapEnd=%p\n", heapPtr, heapEnd);
        while (1) {
            reaper++;
            if (reaper>=NUM_SLOTS) {
                reaper=0;
            }
            if (Slots[reaper]!=NULL) {
                if (SlotHit[reaper]) {
                  /* recently hit; reap someone else */
                  /* printf("skip %d\n", reaper); */
                  SlotHit[reaper]=0;
                } else {
                  /* printf("reap %d\n", reaper); */
                  Slots[slot] = Slots[reaper];
                  Slots[reaper] = NULL;
                  break;
                }
            }
        }
    } else {
      Slots[slot] = heapPtr;
      heapPtr += BLKSIZE + BLKSLACK;
    }

    SlotHit[slot]=1;

    dbg("Seeking\n");

    seekres = FSEEK(GState->storyFile, blkoffset, SEEK_SET);
    ASSERT(seekres==0, "pcptro: failed to seek\n");

    dbg("Reading\n");
    
    bread = FREAD(Slots[slot], 1, BLKSIZE+BLKSLACK, GState->storyFile);
    ASSERT(bread>0, "pcptro: failed to read\n");
    dbg("read %d bytes\n", bread);

    ptr = Slots[slot] + offset - blkoffset;
    return ptr;
}

/* return pointer to the pc */
uint8 *pcptr()
{
    return PCPTRO(GState->virtpc);
}

/* return the byte at the offset */
uint8 pcbread(offset)
uint32 offset;
{
    return *PCPTRO(offset);
}

/* return the current byte at the PC and then increment by 1 */
uint8 pcbi()
{
    uint8 ch = pcbread((uint32) GState->virtpc);
    GState->virtpc++;
    return ch;
}

/* return the current byte converted to a word at the PC and then increment by 1 */
uint16 pcbiw()
{
    uint8 ch = pcbread((uint32) GState->virtpc);
    GState->virtpc++;
    return QSE(ch);
}

/* return the current word at the PC and then increment by 2 */
uint16 pcwi()
{
    uint16 v = ((((uint16) QSE(*PCPTR)) << 8) | ((uint16) QSE(*(PCPTR+1)))); 
    PCADD(2);
    return v;
}

/* The Z-Machine can't directly address 32-bits, but this needs to expand past 16 bits when we multiply by 2, 4, or 8, etc. */
static uint32 unpackAddress(addr)
const uint32 addr;
{
    if (GState->header.version <= 3)
        return (addr * 2);
    else if (GState->header.version <= 5)
        return (addr * 4);
    else if (GState->header.version <= 6)
        GState->die("write me");  /*   4P + 8R_O    Versions 6 and 7, for routine calls ... or 4P + 8S_O    Versions 6 and 7, for print_paddr */
    else if (GState->header.version <= 8)
        return (addr * 8);

    GState->die("FIXME Unsupported version for packed addressing");
    return 0;
} /* unpackAddress */

static uint8 *varAddress(ivar, writing)
const uint8 ivar;
const int writing;
{
    uint16 var = QSE(ivar);
    if (var == 0) /* top of stack */
    {
        if (writing)
        {
            if ((GState->sp-GState->stack) >= (sizeof (GState->stack) / sizeof (GState->stack[0])))
                GState->die("Stack overflow");
            dbg("push stack\n");
            return (uint8 *) GState->sp++;
        } /* if */
        else
        {
            const uint16 numlocals;
            if (GState->sp == GState->stack)
                GState->die("Stack underflow");  /* nothing on the stack at all? */

            numlocals = GState->bp ? GState->stack[GState->bp-1] : 0;
            if ((GState->bp + numlocals) >= (GState->sp-GState->stack))
                GState->die("Stack underflow");  /* no stack data left in this frame. */

            dbg("pop stack\n");
            return (uint8 *) --GState->sp;
        } /* else */
    } /* if */

    else if ((var >= 0x1) && (var <= 0xF))  /* local var. */
    {
        if (GState->stack[GState->bp-1] <= (var-1))
            GState->die("referenced unallocated local var"); /* #%u (%u available)", (unsigned int) (var-1), (unsigned int) GState->stack[GState->bp-1]); */
        return (uint8 *) &GState->stack[GState->bp + (var-1)];
    } /* else if */

    /* else, global var */
    return PCPTRO(GState->header.globals_addr + ((var-0x10) * sizeof (uint16))); /* return (GState->story + GState->header.globals_addr) + ((var-0x10) * sizeof (uint16)); */
} /* varAddress */

static voidret op_call(voidarg)
{
    uint8 args = GState->operand_count;
    const uint16 *operands = GState->operands;
    const uint8 storeid = PCBI;
    /* no idea if args==0 should be the same as calling addr 0... */
    if ((args == 0) || (operands[0] == 0))  /* legal no-op; store 0 to return value and bounce. */
    {
        uint8 *store = varAddress(storeid, 1);
        WRITEUI16(store, 0);
    } /* if */
    else
    {
        const uint8 numlocals;
        const uint32 pcoffset;
        const uint16 *src;
        uint8 *dst;
        sint8 i;
        uint32 routine = unpackAddress((uint32) operands[0]);

        dbg("routine is at %X\n", routine);

        GState->logical_pc = routine;
        numlocals = *(PCPTRO(routine));
        routine++;
        if (numlocals > 15)
            GState->die("Routine has too many local variables"); /* (%u)", numlocals); */

        *(GState->sp++) = (uint16) QSE(storeid);  /* save where we should store the call's result. */
        dbg("call stack %x\n", *(GState->sp-1));

        /* next instruction to run upon return. */
        pcoffset = (uint32) PCOFS;
        *(GState->sp++) = (pcoffset & 0xFFFF);
        dbg("call stack %x\n", *(GState->sp-1));
        *(GState->sp++) = ((pcoffset >> 16) & 0xFFFF);
        dbg("call stack %x\n", *(GState->sp-1));

        *(GState->sp++) = GState->bp;  /* current base pointer before the call. */
        dbg("call stack %x\n", *(GState->sp-1));
        *(GState->sp++) = numlocals;  /* number of locals we're allocating. */
        dbg("call stack %x\n", *(GState->sp-1));

        GState->bp = (uint16) (GState->sp-GState->stack);
        dbg("call stack %x\n", *(GState->sp-1));

        if (GState->header.version <= 4)
        {
            for (i = 0; i < numlocals; i++, routine += sizeof (uint16)) {
                copyword((uint8*) GState->sp, PCPTRO(routine)); /* leave it byteswapped when moving to the stack. */
                dbg("call stack %x\n", *(GState->sp));
                GState->sp++;
            }
        } /* if */
        else
        {
            for (i = 0; i < numlocals; i++) {
                *(GState->sp++) = 0;
                dbg("call stack %x\n", *(GState->sp-1));
            }
        } /* else */

        args--;  /* remove the return address from the count. */
        if (args > numlocals)  /* it's legal to have more args than locals, throw away the extras. */
            args = numlocals;

        src = operands + 1;
        dst = (uint8 *) (GState->stack + GState->bp);
        for (i = 0; i < args; i++)
        {
            dbg("write operand at %x: %x\n", PPP(dst), src[i]);
            WRITEUI16(dst, src[i]);
        } /* for */

        PCSET(routine);
        /* next call to runInstruction() will execute new routine. */
    } /* else */
    voidreturn;
} /* op_call */

static voidret doReturn(val)
const uint16 val;
{
    const uint32 pcoffset;
    const uint8 storeid;
    uint8 *store;

    if (GState->bp == 0)
        GState->die("Stack underflow in return operation");

    dbg("popping stack for return\n");
    dbg("returning: initial pc=%x, bp=%u, sp=%u\n", (unsigned int) PCOFS, (unsigned int) GState->bp, (unsigned int) (GState->sp-GState->stack));

    GState->sp = GState->stack + GState->bp;  /* this dumps all the locals and data pushed on the stack during the routine. */
    GState->sp--;  /* dump our copy of numlocals */
    GState->bp = *(--GState->sp);  /* restore previous frame's base pointer, dump it from the stack. */

    GState->sp -= 2;  /* point to start of our saved program counter. */
    pcoffset = ((uint32) GState->sp[0]) | (((uint32) GState->sp[1]) << 16);

    PCSET(pcoffset); /* next instruction is one following our original call. */

    storeid = (uint8) *(--GState->sp);  /* pop the result storage location. */

    dbg("returning: new pc=%x, bp=%u, sp=%u\n", (unsigned int) PCOFS, (unsigned int) GState->bp, (unsigned int) (GState->sp-GState->stack));
    store = varAddress(storeid, 1);  /* and store the routine result. */
    dbg("write return at %x: %x\n", PPP(store), val);
    WRITEUI16(store, val);
    voidreturn;
} /* doReturn */

static voidret op_ret(voidarg)
{
    doReturn(GState->operands[0]);
    voidreturn;
} /* op_ret */

static voidret op_rtrue(voidarg)
{
    doReturn(1);
    voidreturn;
} /* op_rtrue */

static voidret op_rfalse(voidarg)
{
    doReturn(0);
    voidreturn;
} /* op_rfalse */

static voidret op_ret_popped(voidarg)
{
    uint8 *ptr = varAddress(0, 0);   /* top of stack. */
    const uint16 result = READUI16(ptr);
    dbg("op_ret_popped: %x\n", result);
    doReturn(result);
    voidreturn;
} /* op_ret_popped */

static voidret op_push(voidarg)
{
    uint8 *store = varAddress(0, 1);   /* top of stack. */
    dbg("push at %x: %x\n", PPP(store), GState->operands[0]);
    WRITEUI16(store, GState->operands[0]);
    voidreturn;
} /* op_push */

static voidret op_pull(voidarg)
{
    uint8 *store;
    const uint8 *ptr = varAddress(0, 0);   /* top of stack. */
    const uint16 val = READUI16(ptr);
    store = varAddress((uint8) GState->operands[0], 1);
    dbg("pull at %x: %x\n", PPP(store), val);
    WRITEUI16(store, val);
    voidreturn;    
} /* op_pull */

static voidret op_pop(voidarg)
{
    varAddress(0, 0);   /* this causes a pop. */
    voidreturn;    
} /* op_pop */

static voidret op_add(voidarg)
{
    uint8 *store = varAddress(PCBI, 1);
    const sint16 result = ((sint16) GState->operands[0]) + ((sint16) GState->operands[1]);
    dbg("add at %x: %x\n", PPP(store), result);
    WRITEUI16(store, result);
    voidreturn;    
} /* op_add */

static voidret op_sub(voidarg)
{
    uint8 *store = varAddress(PCBI, 1);
    const sint16 result = ((sint16) GState->operands[0]) - ((sint16) GState->operands[1]);
    dbg("sub at %x: %x\n", PPP(store), result);
    WRITEUI16(store, result);
    voidreturn;    
} /* op_sub */

static voidret doBranch(truth)
int truth;
{
    const uint16 branch = PCBIW;
    const int farjump = (branch & (1<<6)) == 0;
    const int onTruth = (branch & (1<<7)) ? 1 : 0;
    const uint16 byte2 = farjump ?  PCBIW : 0;

    dbg("doBranch %x %x %x %x\n", branch, farjump, onTruth, byte2);

    if (truth == onTruth)  /* take the branch? */
    {
        sint16 offset = (sint16) (branch & 0x3F);
        if (farjump)
        {
            if (offset & (1 << 5))
                offset |= 0xC0;   /* extend out sign bit. */
            offset = (offset << 8) | ((sint16) byte2);
            dbg("doBranch Far Jump %x\n", offset);
        } /* else */

        if (offset == 0) { /* return false from current routine. */
            dbg("doBranch return 0 \n");
            doReturn(0);
        } else if (offset == 1) { /* return true from current routine. */
            dbg("doBranch return \n");
            doReturn(1);
        } else {
            dbg("doBranch Near Jump %x\n", offset);
            PCSET(PCOFS + offset -2);
        }
    } /* if */
    voidreturn;    
} /* doBranch */

static voidret op_je(voidarg)
{
    const uint16 a = GState->operands[0];
    sint8 i;
    for (i = 1; i < GState->operand_count; i++)
    {
        if (a == GState->operands[i])
        {
            doBranch(1);
            voidreturn;
        } /* if */
    } /* for */

    doBranch(0);
    voidreturn;    
} /* op_je */

static voidret op_jz(voidarg)
{
    doBranch((GState->operands[0] == 0) ? 1 : 0);
    voidreturn;    
} /* op_jz */

static voidret op_jl(voidarg)
{
    doBranch((((sint16) GState->operands[0]) < ((sint16) GState->operands[1])) ? 1 : 0);
    voidreturn;
} /* op_jl */

static voidret op_jg(voidarg)
{
    doBranch((((sint16) GState->operands[0]) > ((sint16) GState->operands[1])) ? 1 : 0);
    voidreturn;
} /* op_jg */

static voidret op_test(voidarg)
{
    doBranch((GState->operands[0] & GState->operands[1]) == GState->operands[1]);
    voidreturn;    
} /* op_test */

static voidret op_jump(voidarg)
{
    /* this opcode is not a branch instruction, and doesn't follow those rules. */
    PCADD(((sint16) GState->operands[0]) - 2);
    voidreturn;    
} /* op_jump */

static voidret op_div(voidarg)
{
    const uint16 result;
    uint8 *store = varAddress(PCBI, 1);
    if (GState->operands[1] == 0)
        GState->die("Division by zero");
    result = (uint16) (((sint16) GState->operands[0]) / ((sint16) GState->operands[1]));
    dbg("div at %x: %x\n", PPP(store), result);
    WRITEUI16(store, result);
    voidreturn;
} /* op_div */

static voidret op_mod(voidarg)
{
    const uint16 result;
    uint8 *store = varAddress(PCBI, 1);
    if (GState->operands[1] == 0)
        GState->die("Division by zero");
    result = (uint16) (((sint16) GState->operands[0]) % ((sint16) GState->operands[1]));
    dbg("mod at %x: %x\n", PPP(store), result);
    WRITEUI16(store, result);
    voidreturn;
} /* op_div */

static voidret op_mul(voidarg)
{
    uint8 *store = varAddress(PCBI, 1);
    const uint16 result = (uint16) (((sint16) GState->operands[0]) * ((sint16) GState->operands[1]));
    dbg("mul at %x: %x\n", PPP(store), result);
    WRITEUI16(store, result);
    voidreturn;
} /* op_mul */

static voidret op_or(voidarg)
{
    uint8 *store = varAddress(PCBI, 1);
    const uint16 result = (GState->operands[0] | GState->operands[1]);
    dbg("or at %x: %x\n", PPP(store), result);
    WRITEUI16(store, result);
    voidreturn;    
} /* op_or */

static voidret op_and(voidarg)
{
    uint8 *store = varAddress(PCBI, 1);
    const uint16 result = (GState->operands[0] & GState->operands[1]);
    dbg("and at %x: %x\n", PPP(store), result);
    WRITEUI16(store, result);
    voidreturn;
} /* op_and */

static voidret op_not(voidarg)
{
    uint8 *store = varAddress(PCBI, 1);
    const uint16 result = ~GState->operands[0];
    dbg("not at %x: %x\n", PPP(store), result);
    WRITEUI16(store, result);
    voidreturn;    
} /* op_not */

static voidret op_inc_chk(voidarg)
{
    uint8 *store = varAddress((uint8) GState->operands[0], 1);
    sint16 val = READUI16(store);
    store -= sizeof (uint16);
    val++;
    dbg("inc_chk at %x: %x\n", PPP(store), val);
    WRITEUI16(store, (uint16) val);
    doBranch( (((sint16) val) > ((sint16) GState->operands[1])) ? 1 : 0 );
    voidreturn;
} /* op_inc_chk */

static voidret op_inc(voidarg)
{
    uint8 *store = varAddress((uint8) GState->operands[0], 1);
    sint16 val = (sint16) READUI16(store);
    store -= sizeof (uint16);
    val++;
    WRITEUI16(store, (uint16) val);
    voidreturn;    
} /* op_inc */

static voidret op_dec_chk(voidarg)
{
    uint8 *store = varAddress((uint8) GState->operands[0], 1);
    sint16 val = (sint16) READUI16(store);
    store -= sizeof (uint16);
    val--;
    WRITEUI16(store, (uint16) val);
    doBranch( (((sint16) val) < ((sint16) GState->operands[1])) ? 1 : 0 );
    voidreturn;    
} /* op_dec_chk */

static voidret op_dec(voidarg)
{
    uint8 *store = varAddress((uint8) GState->operands[0], 1);
    sint16 val = (sint16) READUI16(store);
    store -= sizeof (uint16);
    val--;
    WRITEUI16(store, (uint16) val);
    voidreturn;    
} /* op_dec */

static voidret op_load(voidarg)
{
    uint8 *store;
    const uint8 *valptr = varAddress((uint8) (GState->operands[0] & 0xFF), 0);
    const uint16 val = READUI16(valptr);
    store = varAddress(PCBI, 1);
    WRITEUI16(store, val);
    dbg("load: %02x%02x to %x", *(store-2)&0xFF, *(store-1)&0xFF, PPP(store)-2);
    voidreturn;    
} /* op_load */

static voidret op_ldw(voidarg)
{
    const uint16 offset;
    const uint16 *src;
    uint16 *store = (uint16 *) varAddress(PCBI, 1);
    offset = (GState->operands[0] + (GState->operands[1] * 2));
    dbg("offset %x\n", offset);
    src = (const uint16 *) PCPTRO(offset);
    copyword(store, src); /* copy from bigendian to bigendian: no byteswap. */
    dbg("ldw: %02x%02x to %x from %x\n", *(((uint8*)store))&0xFF, *(((uint8*)(store))+1)&0xFF, PPP(store), PPP(src));
    voidreturn;    
} /* op_ldw */

static voidret op_ldb(voidarg)
{
    const uint16 offset;
    const uint8 *src;
    const uint16 value;
    uint8 *store = varAddress(PCBI, 1);
    offset = (GState->operands[0] + GState->operands[1]);
    src = PCPTRO(offset);
    value = QSE(*src);  /* expand out to 16-bit before storing. */
    WRITEUI16(store, value);
    dbg("ldb: %02x%02x to %x\n", *(store-2)&0xFF, *(store-1)&0xFF, PPP(store)-2);
    voidreturn;    
} /* op_ldb */

static voidret op_stw(voidarg)
{
    const uint16 offset;
    uint8 *dst;
    const uint16 src;
    offset = (GState->operands[0] + (GState->operands[1] * 2));
    dst = PCPTRO(offset);
    src = GState->operands[2];
    WRITEUI16(dst, src);
    dbg("stw: %02x%02x at %x\n", *(dst-2)&0xFF, *(dst-1)&0xFF, PPP(dst)-2);
    voidreturn;    
} /* op_stw */

static voidret op_stb(voidarg)
{
    const uint16 offset;
    const uint8 src;
    uint8 *dst;
    offset = (GState->operands[0] + GState->operands[1]);
    dst = PCPTRO(offset);
    src = (uint8) GState->operands[2];
    *dst = src;
    dbg("stb: %02x%02x at %x\n", *(dst-2)&0xFF, *(dst-1)&0xFF, PPP(dst)-2);
    voidreturn;    
} /* op_stb */

static voidret op_store(voidarg)
{
    uint8 *store = varAddress((uint8) (GState->operands[0] & 0xFF), 1);
    const uint16 src = GState->operands[1];
    WRITEUI16(store, src);
    dbg("store: %02x%02x at %x\n", *(store-2)&0xFF, *(store-1)&0xFF, PPP(store)-2);
    voidreturn;    
} /* op_store */

static uint8 *getObjectPtr(objid)
const uint16 objid;
{
    uint8 *ptr;

    dbg("GetObjectID %x\n", objid);

    if (objid == 0)
        GState->die("Object id #0 referenced");

    if ((GState->header.version <= 3) && (objid > 255))
        GState->die("Invalid object id referenced");

    /* ptr = GState->story + GState->header.objtab_addr; */
    ptr = PCPTRO(GState->header.objtab_addr);
    ptr += 31 * sizeof (uint16);  /* skip properties defaults table */
    ptr += 9 * (objid-1);  /* find object in object table */

    dbg("getObjectPtr returning %x\n", (uint16) (GState->header.objtab_addr + 31 * sizeof (uint16) +  9 * (objid-1)));

    return ptr;
} /* getObjectPtr */

static voidret op_tattr(voidarg)
{
    const uint16 objid = GState->operands[0];
    const uint16 attrid = GState->operands[1];
    uint8 *ptr = getObjectPtr(objid);

    if (GState->header.version <= 3)
    {
        ptr += (attrid / 8);
        dbg("test attr %x\n", (QSE(*ptr) & (0x80 >> (attrid & 7))));
        doBranch((QSE(*ptr) & (0x80 >> (attrid & 7))) ? 1 : 0);
    } /* if */
    else
    {
        GState->die("write me");
    } /* else */
    voidreturn;    
} /* op_tattr */

static voidret op_set_attr(voidarg)
{
    const uint16 objid = GState->operands[0];
    const uint16 attrid = GState->operands[1];
    uint8 *ptr = getObjectPtr(objid);

    if (GState->header.version <= 3)
    {
        ptr += (attrid / 8);
        *ptr |= 0x80 >> (attrid & 7);
    } /* if */
    else
    {
        GState->die("write me");
    } /* else */
    voidreturn;    
} /* op_set_attr */

static voidret op_clear_attr(voidarg)
{
    const uint16 objid = GState->operands[0];
    const uint16 attrid = GState->operands[1];
    uint8 *ptr = objid ? getObjectPtr(objid) : NULL;

    if (ptr == NULL) {
        voidreturn;  /* Zork 1 will trigger this on "go X" where "x" isn't a direction, so ignore it. */
    }

    if (GState->header.version <= 3)
    {
        ptr += (attrid / 8);
        *ptr &= ~(0x80 >> (attrid & 7));
    } /* if */
    else
    {
        GState->die("write me");
    } /* else */
    voidreturn;    
} /* op_clear_attr */

static uint8 *getOPParent(objptr)
const uint8 *objptr;
{
    if (GState->header.version <= 3)
    {
        const uint16 parent = QSE(objptr[4]);
        dbg("objid from parent %x\n", parent);
        return parent ? getObjectPtr(parent) : NULL;
    }
    else
    {
        GState->die("write me");
        return NULL;
    } /* else */
} /* getgetOPParent */

static voidret unparentObject(_objid)
const uint16 _objid;
{
    const uint16 objid = _objid;
    uint8 *objptr = getObjectPtr(objid);
    uint8 *parentptr = getOPParent(objptr);
    if (parentptr != NULL)  /* if NULL, no need to remove it. */
    {
        uint8 *ptr = parentptr + 6;  /* 4 to skip attrs, 2 to skip to child. */
        while (QSE(*ptr) != objid) /* if not direct child, look through sibling list... */
            ptr = getObjectPtr(QSE(*ptr)) + 5;  /* get sibling field. */
        *ptr = *(objptr + 5);  /* obj sibling takes obj's place. */
    } /* if */
} /* unparentObject */

static voidret op_insert_obj(voidarg)
{
    const uint16 objid = GState->operands[0];
    const uint16 dstid = GState->operands[1];

    uint8 *objptr = getObjectPtr(objid);
    uint8 *dstptr = getObjectPtr(dstid);    
    dbg("insert_obj %x %x\n", objid, dstid);

    if (GState->header.version <= 3)
    {
        unparentObject(objid);  /* take object out of its original tree first. */

        /* now reinsert in the right place. */
        *(objptr + 4) = (uint8) dstid;  /* parent field: new destination */
        *(objptr + 5) = *(dstptr + 6);  /* sibling field: new dest's old child. */
        *(dstptr + 6) = (uint8) objid;  /* dest's child field: object being moved. */
    } /* if */
    else
    {
        GState->die("write me");  /* fields are different in ver4+. */
    } /* else */
    voidreturn;    
} /* op_insert_obj */

static voidret op_remove_obj(voidarg)
{
    const uint16 objid = GState->operands[0];
    uint8 *objptr = getObjectPtr(objid);

    if (GState->header.version > 3)
        GState->die("write me");  /* fields are different in ver4+. */
    else
    {
        unparentObject(objid);  /* take object out of its original tree first. */

        /* now clear out object's relationships... */
        *(objptr + 4) = 0;  /* parent field: zero. */
        *(objptr + 5) = 0;  /* sibling field: zero. */
    } /* else */
    voidreturn;    
} /* op_remove_obj */

static uint8 *getOProp(objid, propid, _size)
const uint16 objid;
const uint32 propid;
uint8 *_size;
{
    uint8 *ptr = getObjectPtr(objid);

    if (GState->header.version <= 3)
    {
        const uint16 addr;
        ptr += 7;  /* skip to properties address field. */
        addr = READUI16(ptr);
        dbg("getOProp: addr=%x\n", addr);
        ptr = PCPTRO(addr);
        ptr += (QSE(*ptr) * 2) + 1;  /* skip object name to start of properties. */
        while (1)
        {
            const uint8 info = *(ptr++);
            const uint16 num = (info & 0x1F);  /* 5 bits for the prop id. */
            const uint8 size = ((info >> 5) & 0x7) + 1; /* 3 bits for prop size. */
            /* these go in descending numeric order, and should fail */
            /*  the interpreter if missing. We use 0xFFFFFFFF internally to mean "first property". */
            if ((num == propid) || (propid == 0xFFFFFFFF))  /* found it? */
            {
                if (_size)
                    *_size = size;
                return ptr;
            } /* if */

            else if (num < propid)  /* we're past it. */
                break;

            ptr += size;  /* try the next property. */
        } /* while */
    } /* if */
    else
    {
        GState->die("write me");
    } /* else */

    return NULL;
} /* getOProp */

static voidret op_put_prop(voidarg)
{
    const uint16 objid = GState->operands[0];
    const uint16 propid = GState->operands[1];
    const uint16 value = GState->operands[2];
    uint8 size = 0;
    uint8 *ptr = getOProp(objid, (uint32) propid, &size);

    dbg("op_put_prop size=%x\n", size);

    if (!ptr)
        GState->die("Lookup on missing object property"); /* (obj=%x, prop=%x)", (unsigned int) objid, (unsigned int) propid); */
    else if (size == 1)
        *ptr = (value & 0xFF);
    else
    {
        dbg("put_prop at %x: %x\n", PPP(ptr), value);
        WRITEUI16(ptr, value);
    } /* else */
    voidreturn;    
} /* op_put_prop */

static uint16 getDefaultObjectProperty(propid)
const uint16 propid;
{
    const uint8 *values;
    const uint16 result;
    if ( ((GState->header.version <= 3) && (propid > 31)) ||
         ((GState->header.version >= 4) && (propid > 63)) )
    {
        return 0;
    } /* if */

    values = PCPTRO(GState->header.objtab_addr);
    values += (propid-1) * sizeof (uint16);
    result = READUI16(values);
    return result;
} /* getDefaultObjectProperty */

static voidret op_get_prop(voidarg)
{
    uint8 *store = varAddress(PCBI, 1);
    const uint16 objid = GState->operands[0];
    const uint16 propid = GState->operands[1];
    uint16 result = 0;
    uint8 size = 0;
    uint8 *ptr = getOProp(objid, (uint32) propid, &size);

    dbg("op_get_prop size=%x\n", size);

    if (!ptr)
        result = getDefaultObjectProperty(propid);
    else if (size == 1)
        result = *ptr;
    else
    {
        result = READUI16(ptr);
    } /* else */

    dbg("get_prop %x: %x\n", PPP(store), result);
    WRITEUI16(store, result);
    voidreturn;    
} /* op_get_prop */

static voidret op_gpa(voidarg)
{
    uint8 *store = varAddress(PCBI, 1);
    const uint16 objid = GState->operands[0];
    const uint16 propid = GState->operands[1];
    uint8 *ptr = getOProp(objid, (uint32) propid, NULL);
    const uint16 result = ptr ? ((uint16) (ptr-GState->storyBase)) : 0; /* XXX smbaker */
    dbg("gpa %x: %x\n", PPP(store), result);
    WRITEUI16(store, result);
    voidreturn;    
} /* op_gpa */

static voidret op_gpl(voidarg)
{
    uint8 *store = varAddress(PCBI, 1);
    uint16 result;

    if (GState->operands[0] == 0)
        result = 0;  /* this must return 0, to avoid a bug in older Infocom games. */
    else if (GState->header.version <= 3)
    {
        const uint16 offset = GState->operands[0];
        const uint8 *ptr = PCPTRO(offset);
        const uint8 info = ptr[-1];  /* the size field. */
        result = ((info >> 5) & 0x7) + 1; /* 3 bits for prop size. */
    } /* if */
    else
    {
        GState->die("write me");
    } /* else */

     dbg("gpl at %x: %x\n", PPP(store), result);
    WRITEUI16(store, result);
    voidreturn;    
} /* op_gpl */

static voidret op_gnp(voidarg)
{
    uint8 *store = varAddress(PCBI, 1);
    const uint16 objid = GState->operands[0];
    const int firstProp = (GState->operands[1] == 0);
    uint16 result = 0;
    uint8 size = 0;
    uint8 *ptr = getOProp(objid, firstProp ? (uint32) 0xFFFFFFFF : (uint32) GState->operands[1], &size);

    if (!ptr)
        GState->die("get_next_prop on missing property obj=%x, prop=%x"); /* , (unsigned int) objid, (unsigned int) GState->operands[1]); */
    else if (GState->header.version <= 3)
        result = ptr[firstProp ? -1 : ((sint8) size)] & 0x1F;  /* 5 bits for the prop id. */
    else
        GState->die("write me");

    dbg("gpn at %x: %x\n", PPP(store), result);
    WRITEUI16(store, result);
    voidreturn;    
} /* op_gnp */

static voidret op_jin(voidarg)
{
    const uint16 objid = GState->operands[0];
    const uint16 parentid = GState->operands[1];
    const uint8 *objptr = objid ? getObjectPtr(objid) : NULL;

    if (objptr == NULL) {
        voidreturn;  /* Zork 1 will trigger this on "go X" where "x" isn't a direction. */
    }

    if (GState->header.version <= 3)
        doBranch((((uint16) QSE(objptr[4])) == parentid) ? 1 : 0);
    else
        GState->die("write me");  /* fields are different in ver4+. */
    voidreturn;        
} /* op_jin */

static uint16 getORelationship(objid, relationship)
const uint16 objid;
const uint8 relationship;
{
    const uint8 *objptr = getObjectPtr(objid);

    if (GState->header.version <= 3)
        return QSE(objptr[relationship]);
    else
        GState->die("write me");  /* fields are different in ver4+. */
    return 0;
} /* getORelationship */

static voidret op_gpar(voidarg)
{
    uint8 *store = varAddress(PCBI, 1);
    const uint16 result = getORelationship(GState->operands[0], 4);
    dbg("gpr at %x: %x\n", PPP(store), result);
    WRITEUI16(store, result);
    voidreturn;    
} /* op_gpar */

static voidret op_gsibling(voidarg)
{
    uint8 *store = varAddress(PCBI, 1);
    const uint16 result = getORelationship(GState->operands[0], 5);
    dbg("gsibling at %x: %x\n", PPP(store), result);
    WRITEUI16(store, result);
    doBranch((result != 0) ? 1: 0);
    voidreturn;    
} /* op_gsibling */

static voidret op_gchild(voidarg)
{
    uint8 *store = varAddress(PCBI, 1);
    const uint16 result = getORelationship(GState->operands[0], 6);
    dbg("gchild at %x: %x\n", PPP(store), result);
    WRITEUI16(store, result);
    doBranch((result != 0) ? 1: 0);
    voidreturn;    
} /* op_gchild */

static voidret op_new_line(voidarg)
{
    GState->writechar('\n');
    voidreturn;    
} /* op_new_line */

static voidret pr_zch(val)
const uint16 val;
{
    char ch = 0;

    /* only a few values are valid ZSCII codes for output. */
    if ((val >= 32) && (val <= 126))
        ch = (char) val;  /* FIXME: we assume you have an ASCII terminal for now. */
    else if (val == 13)  /* newline */
        ch = '\n';
    else if (val == 0)
        /* val==0 is "valid" but produces no output. */ ;
    else if ((val >= 155) && (val <= 251))
        ch = '?';  /* extended characters */
    else
        ch = '?';  /* this is illegal, but we'll be nice. */

    if (ch)
        GState->writechar(ch);
    voidreturn;        
} /* pr_zch */

static uintptr print_zscii(_ofs, abbr)
uint32 _ofs;
const int abbr;
{
    /* ZCSII encoding is so nasty. */
    uint32 ofs = _ofs;
    uint16 code = 0;
    uint8 alphabet = 0;
    uint8 useAbbrTable = 0;
    uint8 zscii_collector = 0;
    uint16 zscii_code = 0;

    do
    {
        sint8 i;
        code = RUI16NI(PCPTRO(ofs));
        ofs+=sizeof(uint16);

        /* characters are 5 bits each, packed three to a 16-bit word. */
        for (i = 10; i >= 0; i -= 5)
        {
            int newshift = 0;
            char printVal = 0;
            const uint8 ch = ((code >> i) & 0x1F);

            if (zscii_collector)
            {
                if (zscii_collector == 2)
                    zscii_code |= ((uint16) ch) << 5;
                else
                    zscii_code |= ((uint16) ch);

                zscii_collector--;
                if (!zscii_collector)
                {
                    pr_zch(zscii_code);
                    alphabet = useAbbrTable = 0;
                    zscii_code = 0;
                } /* if */
                continue;
            } /* if */

            else if (useAbbrTable)
            {
                const uintptr index;
                const uint8 *ptr;
                const uint16 abbraddr;
                if (abbr)
                    GState->die("Abbreviation strings can't use abbreviations");
                /*FIXME("Make sure offset is sane"); */
                index = ((32 * (((uintptr) useAbbrTable) - 1)) + (uintptr) ch);
                ptr = PCPTRO(GState->header.abbrtab_addr + (index * sizeof (uint16)));
                abbraddr = READUI16(ptr);
                print_zscii((uint32) (abbraddr * sizeof (uint16)), 1);
                useAbbrTable = 0;
                alphabet = 0;  /* FIXME: no shift locking in ver3+, but ver1 needs it. */
                continue;
            } /* if */

            switch (ch)
            {
                case 0:
                    printVal = ' ';
                    break;

                case 1:
                    if (GState->header.version == 1)
                        printVal = '\n';
                    else
                        useAbbrTable = 1;
                    break;

                case 2:
                case 3:
                    if (GState->header.version <= 2)
                        GState->die("write me: handle ver1/2 alphabet shifting");
                    else
                        useAbbrTable = ch;
                    break;

                case 4:
                case 5:
                    if (GState->header.version <= 2)
                        GState->die("write me: handle ver1/2 alphabet shift locking");
                    else
                    {
                        newshift = 1;
                        alphabet = ch - 3;
                    } /* else */
                    break;

                default:
                    if ((ch == 6) && (alphabet == 2))
                        zscii_collector = 2;
                    else
                        printVal = GState->alphabet_table[(alphabet*26) + (ch-6)];
                    break;
            } /* switch */

            if (printVal)
                GState->writechar(printVal);

            if (alphabet && !newshift)
                alphabet = 0;
        } /* for */

        /* there is no NULL terminator, you look for a word with the top bit set. */
    } while ((code & (1<<15)) == 0);

    return ofs - _ofs;
} /* print_zscii */

static voidret op_print(voidarg)
{
    uint16  written = print_zscii((uint32) PCOFS, 0);
    PCADD(written);
    voidreturn;    
} /* op_print */

static voidret op_pnum(voidarg)
{
    char buf[32];
    const char *ptr;
    sprintf(buf, "%d", (int) ((sint16) GState->operands[0]));
    for (ptr = buf; *ptr; ptr++)
        GState->writechar(*ptr);
    voidreturn;        
} /* op_pnum */

static voidret op_pchar(voidarg)
{
    pr_zch(GState->operands[0]);
    voidreturn;    
} /* op_pchar */

static voidret op_pret(voidarg)
{
    uint16 written = print_zscii((uint32) PCOFS, 0);
    PCADD(written);
    GState->writechar('\n');
    doReturn(1);
    voidreturn;    
} /* op_pret */

static voidret op_pobj(voidarg)
{
    const uint8 *ptr = getObjectPtr(GState->operands[0]);
    if (GState->header.version <= 3)
    {
        const uint16 addr;
        ptr += 7;  /* skip to properties field. */
        addr = READUI16(ptr);  /* dereference to get to property table. */
        print_zscii((uint32) (addr + 1), 0);
    } /* if */
    else
    {
        GState->die("write me");
    } /* else */
    voidreturn;    
} /* op_pobj */

static voidret op_paddr(voidarg)
{
    print_zscii((uint32) GState->operands[0], 0);
    voidreturn;    
} /* op_paddr */

static voidret op_ppaddr(voidarg)
{
    print_zscii((uint32) unpackAddress((uint32) GState->operands[0]), 0);
    voidreturn;    
} /* op_ppaddr */

static uint16 doRandom(range)
const sint16 range;
{
    /* Note: this must not return 0 */
    return RAND(range)+1;
} /* doRandom */

static voidret op_random(voidarg)
{
    uint8 *store = varAddress(PCBI, 1);
    const sint16 range = (sint16) GState->operands[0];
    const uint16 result = doRandom(range);
    WRITEUI16(store, result);
    voidreturn;    
} /* op_random */

static voidret op_show_status(voidarg)
{
    voidreturn;    
} /* op_show_status */

static voidret tokenizeUserInput(voidarg)
{
    static const char table_a2_v1[] = "0123456789.,!?_#\'\"/\\<-:()";
    static const char table_a2_v2plus[] = "\n0123456789.,!?_#\'\"/\\-:()";
    const char *table_a2 = (GState->header.version <= 1) ? table_a2_v1 : table_a2_v2plus;
    const uint8 *input = PCPTRO(GState->operands[0]);
    uint8 *parse = PCPTRO(GState->operands[1]);
    const uint8 parselen = *(parse++);
    const uint8 *seps = PCPTRO(GState->header.dict_addr);
    const uint8 numseps = *(seps++);
    const uint8 *dict = seps + numseps;
    const uint8 entrylen = *(dict++);
    const uint8 *strstart;
    const uint8 *ptr;
    uint8 numtoks = 0;    
    const uint16 numentries = READUI16(dict);

    input++;  /* skip over inputlen byte; we checked this and capped input elsewhere. */
    parse++;  /* skip over where we will write the final token count. */

    strstart = input;
    ptr = (const uint8 *) input;
    while (1)
    {
        int isSep = 0;
        const uint8 ch = *ptr;
        if ((ch == ' ') || (ch == '\0'))
            isSep = 1;
        else
        {
            uint8 i;
            for (i = 0; i < numseps; i++)
            {
                if (ch == seps[i])
                {
                    isSep = 1;
                    break;
                } /* if */
            } /* for */
        } /* else */

        if (isSep)
        {
            uint16 encoded[3] = { 0, 0, 0 };
            uint8 zchars[12];
            uint16 i;
            uint8 pos;
            int zchidx = 0;
            const uint8 *dictptr;
            const uint16 dictaddr;

            const uint8 toklen = (uint8) (ptr-strstart);
            if (toklen == 0)
                break;  /* ran out of string. */

            for (i = 0; i < toklen; i++)
            {
                const char ch = strstart[i];
                if ((ch >= 'a') && (ch <= 'z'))
                    zchars[zchidx++] = (uint8) ((ch - 'a') + 6);
                else if ((ch >= 'A') && (ch <= 'Z'))
                    zchars[zchidx++] = (uint8) ((ch - 'A') + 6);  /* in a generic encoder, this would be table a1, but we convert to lowercase (table a0) here. */
                else
                {
                    const char *ptr = strchr(table_a2, ch);
                    if (ptr)
                    {
                        zchars[zchidx++] = 3;  /* command char to shift to table A2 for just the next char. */
                        zchars[zchidx++] = (uint8) ((((int) (ptr -  table_a2)) + 1) + 6);  /* +1 because the first table entry is a different piece of magic. */
                    } /* if */
                } /* else */

                if (zchidx >= (sizeof (zchars) / sizeof (zchars[0])))
                    break;
            } /* for */

            pos = 0;
            encoded[0] |= ((pos < zchidx) ? zchars[pos++] : 5) << 10;
            encoded[0] |= ((pos < zchidx) ? zchars[pos++] : 5) << 5;
            encoded[0] |= ((pos < zchidx) ? zchars[pos++] : 5) << 0;
            encoded[1] |= ((pos < zchidx) ? zchars[pos++] : 5) << 10;
            encoded[1] |= ((pos < zchidx) ? zchars[pos++] : 5) << 5;
            encoded[1] |= ((pos < zchidx) ? zchars[pos++] : 5) << 0;

            dictptr = dict;
            if (GState->header.version <= 3)
            {
                encoded[1] |= 0x8000;

                for (i = 0; i < numentries; i++)
                {
                    const uint16 zscii2;
                    const uint16 zscii1 = READUI16(dictptr);
                    zscii2 = READUI16(dictptr);
                    if ((encoded[0] == zscii1) && (encoded[1] == zscii2))
                    {
                        dictptr -= sizeof (uint16) * 2;
                        break;
                    } /* if */
                    dictptr += (entrylen - 4);
                } /* for */
            } /* if */
            else
            {
                encoded[2] |= ((pos < zchidx) ? zchars[pos++] : 5) << 10;
                encoded[2] |= ((pos < zchidx) ? zchars[pos++] : 5) << 5;
                encoded[2] |= ((pos < zchidx) ? zchars[pos++] : 5) << 0;
                encoded[2] |= 0x8000;

                for (i = 0; i < numentries; i++)
                {
                    const uint16 zscii2;
                    const uint16 zscii3;
                    const uint16 zscii1 = READUI16(dictptr);
                    zscii2 = READUI16(dictptr);
                    zscii3 = READUI16(dictptr);
                    if ((encoded[0] == zscii1) && (encoded[1] == zscii2) && (encoded[2] == zscii3))
                    {
                        dictptr -= sizeof (uint16) * 3;
                        break;
                    } /* if */
                    dictptr += (entrylen - 6);
                } /* for */
            } /* else */

            if (i == numentries)
                dictptr = NULL;  /* not found. */
            dictaddr = dictptr ? ((unsigned int) (dictptr - GState->storyBase)) : 0;  /* XXX smbaker */

            /*dbg("Tokenized dictindex=%x, tokenlen=%u, strpos=%u\n", (unsigned int) dictaddr, (unsigned int) toklen, (unsigned int) ((uint8) (strstart-input))); */

            WRITEUI16(parse, dictaddr);
            *(parse++) = (uint8) toklen;
            *(parse++) = (uint8) ((strstart-input) + 1);
            numtoks++;

            if (numtoks >= parselen)
                break;  /* ran out of space. */

            strstart = ptr + 1;
        } /* if */

        if (ch == '\0')  /* end of string */
            break;

        ptr++;
    } /* while */

    dbg("Tokenized %u tokens\n", (unsigned int) numtoks);

    *PCPTRO(GState->operands[1] + 1) = numtoks;
    voidreturn;    
}

static voidret op_read(voidarg)
{
    static char *script = NULL;
    const uint8 inputlen;
    const uint8 *parse;
    const uint8 parselen;
    uint8 *input;

    //printf("XX heapPtr=%p, heapEnd=%p\n", heapPtr, heapEnd);

    dbg("read from input stream: text-buffer=%x parse-buffer=%x\n", (unsigned int) GState->operands[0], (unsigned int) GState->operands[1]);

    input = PCPTRO(GState->operands[0]);
    inputlen = *(input++);
    dbg("max input: %u\n", (unsigned int) inputlen);
    if (inputlen < 3)
        GState->die("text buffer is too small for reading");  /* happens on buffer overflow. */

    parse = PCPTRO(GState->operands[1]);
    parselen = *(parse++);

    dbg("max parse: %u\n", (unsigned int) parselen);
    if (parselen < 4)
        GState->die("parse buffer is too small for reading");  /* happens on buffer overflow. */

    if (GState->startup_script != NULL)
    {
        sprintf((char *) input, "#script %s\n", GState->startup_script);
        input[inputlen-1] = '\0';
        GState->startup_script = NULL;
        printf("%s", (const char *) input);
    } /* if */

    else if (script == NULL)
    {
        op_show_status();
        if (!fgets((char *) input, inputlen, stdin))
            GState->die("EOF or error on stdin during read");
    } /* else if */

    else
    {
        uint16 i;
        char *scriptptr = script;
        for (i = 0; i < inputlen; i++, scriptptr++)
        {
            const char ch = *scriptptr;
            if (ch == '\0')
                break;
            else if (ch == '\n')
            {
                scriptptr++;
                break;
            } /* else if */
            else if (ch == '\r')
            {
                i--;
                continue;
            } /* else if */
            else
            {
                input[i] = (uint8) ch;
            } /* else */
        } /* for */
        input[i] = '\0';

        printf("%s\n", input);

        MEMMOVE(script, scriptptr, strlen(scriptptr) + 1);
        if (script[0] == '\0')
        {
            printf("*** Done running script.\n");
            free(script);
            script = NULL;
        } /* if */
    } /* else */

    dbg("input string from user is '%s'\n", (const char *) input);
    {
        char *ptr;
        for (ptr = (char *) input; *ptr; ptr++)
        {
            if ((*ptr >= 'A') && (*ptr <= 'Z'))
                *ptr -= 'A' - 'a';  /* make it lowercase. */
            else if ((*ptr == '\n') || (*ptr == '\r'))
            {
                *ptr = '\0';
                break;
            } /* if */
        } /* for */
    }

    if (strncmp((const char *) input, "#script ", 8) == 0)
    {
        const char *fname;
        long len;
        FILE *io;
        if (script != NULL)
            GState->die("FIXME: Can't nest scripts at the moment");

        fname = (const char *) (input + 8);
        len = 0;
        io = NULL;
        if ((io = FOPENRB(fname)) == NULL)
            GState->die("Failed to open"); /*  '%s'", fname); */
        else if ((FSEEK(io, 0, SEEK_END) == -1) || ((len = ftell(io)) == -1))
            GState->die("Failed to determine size"); /*  of '%s'", fname); */
        else if ((script = malloc(len)) == NULL)
            GState->die("Out of memory");
        else if ((FSEEK(io, 0, SEEK_SET) == -1) || (FREAD(script, len, 1, io) != 1))
            GState->die("Failed to read"); /* '%s'", fname); */
        fclose(io);
        printf("*** Running script '%s'...\n", fname);
        op_read();  /* start over. */
        voidreturn;
    } /* if */

    else if (strncmp((const char *) input, "#random ", 8) == 0)
    {
        const uint16 val = doRandom((sint16) atoi((const char *) (input+8)));
        printf("*** random replied: %u\n", (unsigned int) val);
        op_read();  /* go again. */
        voidreturn;
    } /* else if */

    tokenizeUserInput();
    voidreturn;    
} /* op_read */

static voidret op_verify(voidarg)
{
    const uint32 total = GState->header.story_len;
    uint32 checksum = 0;
    uint32 i;

    for (i = 0x40; i < total; i++)
        checksum += QSE(GState->storyBase[i]);

    doBranch((((uint16) (checksum % 0x10000)) == GState->header.story_checksum) ? 1 : 0);
    voidreturn;    
} /* op_verify */

#ifdef OLIVETTI
static voidret loadStory(const char *fname);
#else
static voidret loadStory(fname); /* const char *fname); */
#endif

static voidret op_restart(voidarg)
{
    loadStory(GState->story_filename);
    voidreturn;    
} /* op_restart */

static voidret op_save(voidarg)
{
    const uint32 addr;
    const uint32 sp;
    FILE *io;
    int okay;

    addr = PCOFS;
    sp = (uint32) (GState->sp-GState->stack);
    io = FOPENWB("save.dat");
    okay = 1;
    okay &= io != NULL;
    okay &= fwrite(GState->storyBase, GState->header.staticmem_addr, 1, io) == 1;
    okay &= fwrite(&addr, sizeof (addr), 1, io) == 1;
    okay &= fwrite(&sp, sizeof (sp), 1, io) == 1;
    okay &= fwrite(GState->stack, sizeof (GState->stack), 1, io) == 1;
    okay &= fwrite(&GState->bp, sizeof (GState->bp), 1, io) == 1;
    if (io)
        fclose(io);
    doBranch(okay ? 1 : 0);
    voidreturn;    
} /* op_save */

static voidret op_rstr(voidarg)
{
    FILE *io;
    uint16 bread;
    int okay = 1;
    uint32 x = 0;

    io = FOPENRB("save.dat");

    /* Note: If this starts to fall all over itself, be wary of much data is allocated... */

    okay &= (io != NULL);
    bread = FREAD(GState->storyBase, 1, GState->header.staticmem_addr, io);
    okay &= (bread == GState->header.staticmem_addr);
    okay &= FREAD(&x, sizeof (x), 1, io) == 1;
    GState->logical_pc = x;
    PCSET(x);
    okay &= FREAD(&x, sizeof (x), 1, io) == 1;
    GState->sp = GState->stack + x;
    okay &= FREAD(GState->stack, sizeof (GState->stack), 1, io) == 1;
    okay &= FREAD(&GState->bp, sizeof (GState->bp), 1, io) == 1;
    if (io)
        fclose(io);

    if (!okay)
        GState->die("Failed to restore.");

    doBranch(okay ? 1 : 0);
    voidreturn;    
} /* op_rstr */

static voidret op_quit(voidarg)
{
    GState->quit = 1;
    GState->step_completed = 1;  /* possibly time to break out of the Z-Machine simulation loop. */
    voidreturn;    
} /* op_quit */

static voidret op_nop(voidarg)
{
    /* that's all, folks. */
    voidreturn;    
} /* op_nop */


static int parseOperand(optype, operand)
const uint16 optype;
uint16 *operand;
{
    const uint8 *addr;
    switch (optype)
    {
        case 0:
          *operand = (uint16) PCWI;
          dbg("parseOperand large const %x\n", *operand);
          return 1;  /* large constant (uint16) */
        case 1:
          *operand = PCBIW;
          dbg("parseOperand small const %x\n", *operand);
          return 1;  /* small constant (uint8) */
        case 2: { /* variable */
            addr = varAddress(PCBI, 0);
            dbg("parseOperand variable at %x", PPP(addr));
            *operand = READUI16(addr);
            dbg(" %x\n", *operand);
            return 1;
        }
        case 3: break;  /* omitted altogether, we're done. */
    } /* switch */

    return 0;
} /* parseOperand */

static uint8 parseVarOperands(operands)
uint16 *operands;
{
    const uint16 operandTypes = PCBIW;
    uint8 shifter = 6;
    uint8 i;

    for (i = 0; i < 4; i++)
    {
        const uint16 optype = (operandTypes >> shifter) & 0x3;
        shifter -= 2;
        if (!parseOperand(optype, operands + i))
            break;
    } /* for */

    return i;
} /* parseVarOperands */


static voidret runInstruction(voidarg)
{
    const Opcode *op;
    const int extended;
    uint16 opcode;

    GState->logical_pc = PCOFS;
    opcode = PCBIW;

    op = NULL;

    extended = ((opcode == 190) && (GState->header.version >= 5)) ? 1 : 0;
    if (extended)
    {
        opcode = PCBI;
        if (opcode >= (sizeof (GState->extended_opcodes) / sizeof (GState->extended_opcodes[0])))
            GState->die("Unsupported or unknown extended opcode #%u", (unsigned int) opcode);
        GState->operand_count = parseVarOperands(GState->operands);
        op = &GState->extended_opcodes[opcode];
    } /* if */
    else
    {
        if (opcode <= 127)  /* 2OP */
        {
            GState->operand_count = 2;
            parseOperand(((opcode >> 6) & 0x1) ? 2 : 1, GState->operands + 0);
            parseOperand(((opcode >> 5) & 0x1) ? 2 : 1, GState->operands + 1);
        } /* if */

        else if (opcode <= 175)  /* 1OP */
        {
            const uint8 optype;
            GState->operand_count = 1;
            optype = (opcode >> 4) & 0x3;
            parseOperand(optype, GState->operands);  /* 1OP or 0OP */
        } /* else if */

        else if (opcode <= 191)  /* 0OP */
            GState->operand_count = 0;

        else if (opcode > 191)  /* VAR */
        {
            const int takes8 = ((opcode == 236) || (opcode == 250));  /* call_vs2 and call_vn2 take up to EIGHT arguments! */
            if (!takes8)
                GState->operand_count = parseVarOperands(GState->operands);
            else
            {
                GState->operand_count = parseVarOperands(GState->operands);
                if (GState->operand_count == 4)
                    GState->operand_count += parseVarOperands(GState->operands + 4);
                else
                    PCADD(1); /* skip the next byte, since we don't have any more args. */
            } /* else */
        } /* else */

        op = &GState->opcodes[opcode];
    } /* if */

    if (!op->name)
        GState->die("Unsupported or unknown %sopcode"); /*  #%u", extended ? "extended " : "", (unsigned int) opcode); */
    else if (!op->fn)
        GState->die("Unimplemented %sopcode"); /* #%d ('%s')", extended ? "extended " : "", (unsigned int) opcode, op->name); */
    else
    {
        dbg("pc=%x %sopcode=%u ('%s') [", (unsigned int) GState->logical_pc, extended ? "ext " : "", opcode, op->name);
        if (GState->operand_count)
        {
            uint8 i;
            for (i = 0; i < GState->operand_count-1; i++)
                dbg("%x,", (unsigned int) GState->operands[i]);
            dbg("%x", (unsigned int) GState->operands[i]);
        }
        dbg("]\n");

        op->fn();
        GState->instructions_run++;
    } /* else */
    voidreturn;    
} /* runInstruction */

static voidret initAlphabetTable(voidarg)
{
    char *ptr = GState->alphabet_table;
    uint8 i;

    /* alphabet A0 */
    for (i = 0; i < 26; i++)
        *(ptr++) = 'a' + i;

    /* alphabet A1 */
    for (i = 0; i < 26; i++)
        *(ptr++) = 'A' + i;

    /* alphabet A2 */
    *(ptr++) = '\0';

    if (GState->header.version != 1)
        *(ptr++) = '\n';

    for (i = 0; i < 10; i++)
        *(ptr++) = '0' + i;
    *(ptr++) = '.';
    *(ptr++) = ',';
    *(ptr++) = '!';
    *(ptr++) = '?';
    *(ptr++) = '_';
    *(ptr++) = '#';
    *(ptr++) = '\'';
    *(ptr++) = '"';
    *(ptr++) = '/';
    *(ptr++) = '\\';

    if (GState->header.version == 1)
        *(ptr++) = '<';

    *(ptr++) = '-';
    *(ptr++) = ':';
    *(ptr++) = '(';
    *(ptr++) = ')';
    dbg("Alphabet table initialized\n");
    voidreturn;    
} /* initAlphabetTable */

static voidret inititialOpcodeTableSetup(voidarg)
{
    Opcode *opcodes;

    MEMSET((char*) GState->opcodes, '\0', sizeof (GState->opcodes));
    MEMSET((char*) GState->extended_opcodes, '\0', sizeof (GState->extended_opcodes));

    opcodes = GState->opcodes;

    opcodes[1].name = "je"; opcodes[1].fn = op_je;
    opcodes[2].name = "jl"; opcodes[2].fn = op_jl;
    opcodes[3].name = "jg"; opcodes[3].fn = op_jg;
    opcodes[4].name = "dec_chk"; opcodes[4].fn = op_dec_chk;
    opcodes[5].name = "inc_chk"; opcodes[5].fn = op_inc_chk;
    opcodes[6].name = "jin"; opcodes[6].fn = op_jin;
    opcodes[7].name = "test"; opcodes[7].fn = op_test;
    opcodes[8].name = "or"; opcodes[8].fn = op_or;
    opcodes[9].name = "and"; opcodes[9].fn = op_and;
    opcodes[10].name = "test_attr"; opcodes[10].fn = op_tattr;
    opcodes[11].name = "set_attr"; opcodes[11].fn = op_set_attr;
    opcodes[12].name = "clear_attr"; opcodes[12].fn = op_clear_attr;
    opcodes[13].name = "store"; opcodes[13].fn = op_store;
    opcodes[14].name = "insert_obj"; opcodes[14].fn = op_insert_obj;
    opcodes[15].name = "loadw"; opcodes[15].fn = op_ldw;
    opcodes[16].name = "loadb"; opcodes[16].fn = op_ldb;
    opcodes[17].name = "get_prop"; opcodes[17].fn = op_get_prop;
    opcodes[18].name = "get_prop_addr"; opcodes[18].fn = op_gpa;
    opcodes[19].name = "get_next_prop"; opcodes[19].fn = op_gnp;
    opcodes[20].name = "add"; opcodes[20].fn = op_add;
    opcodes[21].name = "sub"; opcodes[21].fn = op_sub;
    opcodes[22].name = "mul"; opcodes[22].fn = op_mul;
    opcodes[23].name = "div"; opcodes[23].fn = op_div;
    opcodes[24].name = "mod"; opcodes[24].fn = op_mod;

    opcodes[128].name = "jz"; opcodes[128].fn = op_jz;
    opcodes[129].name = "get_sibling"; opcodes[129].fn = op_gsibling;
    opcodes[130].name = "get_child"; opcodes[130].fn = op_gchild;
    opcodes[131].name = "get_parent"; opcodes[131].fn = op_gpar;
    opcodes[132].name = "get_prop_len"; opcodes[132].fn = op_gpl;
    opcodes[133].name = "inc"; opcodes[133].fn = op_inc;
    opcodes[134].name = "dec"; opcodes[134].fn = op_dec;
    opcodes[135].name = "print_addr"; opcodes[135].fn = op_paddr;
    opcodes[137].name = "remove_obj"; opcodes[137].fn = op_remove_obj;
    opcodes[138].name = "print_obj"; opcodes[138].fn = op_pobj;
    opcodes[139].name = "ret"; opcodes[139].fn = op_ret;
    opcodes[140].name = "jump"; opcodes[140].fn = op_jump;
    opcodes[141].name = "print_paddr"; opcodes[141].fn = op_ppaddr;
    opcodes[142].name = "load"; opcodes[142].fn = op_load;
    opcodes[143].name = "not"; opcodes[143].fn = op_not;

    opcodes[176].name = "rtrue"; opcodes[176].fn = op_rtrue;
    opcodes[177].name = "rfalse"; opcodes[177].fn = op_rfalse;
    opcodes[178].name = "print"; opcodes[178].fn = op_print;
    opcodes[179].name = "print_ret"; opcodes[179].fn = op_pret;
    opcodes[180].name = "nop"; opcodes[180].fn = op_nop;
    opcodes[181].name = "save"; opcodes[181].fn = op_save;
    opcodes[182].name = "restore"; opcodes[182].fn = op_rstr;
    opcodes[183].name = "restart"; opcodes[183].fn = op_restart;
    opcodes[184].name = "ret_popped"; opcodes[184].fn = op_ret_popped;
    opcodes[185].name = "pop"; opcodes[185].fn = op_pop;
    opcodes[186].name = "quit"; opcodes[186].fn = op_quit;
    opcodes[187].name = "new_line"; opcodes[187].fn = op_new_line;

    opcodes[224].name = "call"; opcodes[224].fn = op_call;
    opcodes[225].name = "storew"; opcodes[225].fn = op_stw;
    opcodes[226].name = "storeb"; opcodes[226].fn = op_stb;
    opcodes[227].name = "put_prop"; opcodes[227].fn = op_put_prop;
    opcodes[228].name = "read"; opcodes[228].fn = op_read;
    opcodes[229].name = "print_char"; opcodes[229].fn = op_pchar;
    opcodes[230].name = "print_num"; opcodes[230].fn = op_pnum;
    opcodes[231].name = "random"; opcodes[231].fn = op_random;
    opcodes[232].name = "push"; opcodes[232].fn = op_push;
    opcodes[233].name = "pull"; opcodes[233].fn = op_pull;

    if (GState->header.version < 3)
        return 0;

    opcodes[188].name = "show_status"; opcodes[188].fn = op_show_status;
    opcodes[189].name = "verify"; opcodes[189].fn = op_verify;
    opcodes[234].name = "split_window";
    opcodes[235].name = "set_window";
    opcodes[243].name = "output_stream";
    opcodes[244].name = "input_stream";
    opcodes[245].name = "sound_effect";

    if (GState->header.version < 4)
        return 0;  

    voidreturn;    
}

static voidret initOpcodeTable(voidarg)
{
    uint16 i;
    Opcode *opcodes;
    inititialOpcodeTableSetup();

    /* finalize the opcode table... */
    opcodes = GState->opcodes;
    for (i = 32; i <= 127; i++)  /* 2OP opcodes repeating with different operand forms. */
        opcodes[i] = opcodes[i % 32];
    for (i = 144; i <= 175; i++)  /* 1OP opcodes repeating with different operand forms. */
        opcodes[i] = opcodes[128 + (i % 16)];
    for (i = 192; i <= 223; i++)  /* 2OP opcodes repeating with VAR operand forms. */
        opcodes[i] = opcodes[i % 32];
    dbg("Opcode table initialized\n");
    voidreturn;        
} /* initOpcodeTable */

/* WE OWN THIS copy of story, which we will free() later. Caller should not free it! */
static voidret initStory(fname, story)
const char *fname;
uint8 *story;
{
    const uint8 *ptr;
    int seekres, bread;

    dbg("initStory0: GState=%p, GState->stack=%p, GState->operands=%p, sstack=%d, sop=%d\n", GState, GState->stack, GState->operands, sizeof(GState->stack), sizeof(GState->operands));

    ASSERT(strlen(fname)<255, "filename is too long");
    GState->story_filename = fname;

    GState->instructions_run = 0;
    PCSET(0);
    GState->logical_pc = 0;
    GState->quit = 0;
    MEMSET((char*) GState->stack, '\0', sizeof (GState->stack));
    MEMSET((char*) GState->operands, '\0', sizeof (GState->operands));
    GState->operand_count = 0;
    GState->sp = NULL;  /* stack pointer */
    GState->bp = 0;  /* base pointer */

    MEMSET((char*) &GState->header, '\0', sizeof (GState->header));
    ptr = GState->storyBase;

    /*GState->story[1] &= ~(1<<3);   this is the infamous "Tandy Bit". Turn it off. */
    GState->storyBase[1] |= (1<<4);  /* report that we don't (currently) support a status bar. */

    GState->header.version = READUI8(ptr);
    GState->header.flags1 = READUI8(ptr);
    GState->header.release = READUI16(ptr);
    GState->header.himem_addr = READUI16(ptr);
    GState->header.pc_start = READUI16(ptr);
    GState->header.dict_addr = READUI16(ptr);
    GState->header.objtab_addr = READUI16(ptr);
    GState->header.globals_addr = READUI16(ptr);
    GState->header.staticmem_addr = READUI16(ptr);
    GState->header.flags2 = READUI16(ptr);
    GState->header.serial_code[0] = READUI8(ptr);
    GState->header.serial_code[1] = READUI8(ptr);
    GState->header.serial_code[2] = READUI8(ptr);
    GState->header.serial_code[3] = READUI8(ptr);
    GState->header.serial_code[4] = READUI8(ptr);
    GState->header.serial_code[5] = READUI8(ptr);
    GState->header.abbrtab_addr = READUI16(ptr);
    GState->header.story_len = READUI16(ptr);
    GState->header.story_checksum = READUI16(ptr);

    dbg("Story '%s' header:\n", fname);
    dbg(" - version %u\n", (unsigned int) GState->header.version);
    dbg(" - flags 0x%x\n", (unsigned int) GState->header.flags1);
    dbg(" - release %u\n", (unsigned int) GState->header.release);
    dbg(" - high memory addr %x\n", (unsigned int) GState->header.himem_addr);
    dbg(" - program counter start %x\n", (unsigned int) GState->header.pc_start);
    dbg(" - dictionary address %x\n", (unsigned int) GState->header.dict_addr);
    dbg(" - object table address %x\n", (unsigned int) GState->header.objtab_addr);
    dbg(" - globals address %x\n", (unsigned int) GState->header.globals_addr);
    dbg(" - static memory address %x\n", (unsigned int) GState->header.staticmem_addr);
    dbg(" - flags2 0x%x\n", (unsigned int) GState->header.flags2);
    dbg(" - serial '%s'\n", GState->header.serial_code);
    dbg(" - abbreviations table address %x\n", (unsigned int) GState->header.abbrtab_addr);
    dbg(" - story length %u\n", (unsigned int) GState->header.story_len);
    dbg(" - story checksum 0x%x\n", (unsigned int) GState->header.story_checksum);

    if (GState->header.version != 3)
        GState->die("FIXME: only version 3 is supported right now, this is");

    ASSERT(GState->header.staticmem_addr < (MAX_STORYBASE-BLKSLACK), "Not enough room for dynamics!\n");

    /* technically, the dictionary doesn't need to be in dynamic memory, but the code will walk pointers
     * through it would calling PCPTRO, and it'll walk off into no-man's land. For now, just require it
     * all to fit.
     */
    ASSERT(GState->header.himem_addr < (MAX_STORYBASE-BLKSLACK), "Not enough room for the dictionary\n");

    GState->storyBaseLen = GState->header.himem_addr;
    dbg("Reserved %d + %d for storyBase\n", (uint16) GState->storyBaseLen, BLKSLACK);
    seekres = FSEEK(GState->storyFile, 0, SEEK_SET);
    ASSERT(seekres==0, "initstory: failed to seek");
    bread = FREAD(GState->storyBase, 1, GState->storyBaseLen+BLKSLACK, GState->storyFile);
    ASSERT(bread==GState->storyBaseLen+BLKSLACK, "initStory: failed to read storyBase\n");
    dbg("Read %d for storyBase\n", bread);

#ifdef OLIVETTI
    dbg("storyBase=%p Slots=%x GState=%p\n", GState->storyBase, (uint16) Slots, GState);
#else
    dbg("storyBase=%x Slots=%x GState=%x\n", (uint16) GState->storyBase, (uint16) Slots, (uint16) GState);
#endif

    initAlphabetTable();
    initOpcodeTable();

    PCSET(GState->header.pc_start); /* GState->pc = GState->story + GState->header.pc_start; */
    GState->logical_pc = (uint32) GState->header.pc_start;
    GState->bp = 0;
    GState->sp = GState->stack;
}

static voidret loadStory(fname)
const char *fname;
{
    uint8 *story;
    FILE *io;
    int bread;
    uint16 i;

    if (!fname)
        GState->die("USAGE: zork <story_file>");
    else if ((io = FOPENRB(fname)) == NULL)
        GState->die("Failed to open"); /*  '%s'", fname); */

    GState->storyFile = io;
    GState->storyBase = StBase;

    ASSERT(GState->storyBase!=NULL, "Failed to malloc storybase\n");
    bread = FREAD(GState->storyBase, 1, 1024, io);
    ASSERT(bread==1024, "Failed to read header\n");

    for (i=0; i<NUM_SLOTS; i++) {
        Slots[i] = NULL;
        SlotHit[i] = 0;
    }
    heapPtr = Heap;
    heapEnd = heapPtr + HEAP_SIZE;
    reaper = 0;

    initStory(fname, story);
    voidreturn;    
} /* loadStory */


static voidret die(s)
const char *s;
{
    printf("%s\n", s);
    exit(1);
    voidreturn;    
} /* die */

static voidret putchar_wrapper(ch)
const int ch;
{
    putchar(ch);
    /* fflush causes zcc to eat the next character */
/*    if (ch == '\n') {
        fflush(stdout);
    }*/
    voidreturn;        
}

ZMachineState zmachine_state;

int main(argc, argv)
int argc;
char **argv;
{
    const char *fname = "zork1.dat";
    uint8 i;

    printf("z8kzork based on mojozork by Ryan C. Gordon.\n");
#ifdef OLIVETTI
    printf("modifed for Olivetti PCOS by Scott M Baker, www.smbaker.com\n\n");
#else
    printf("modifed for cp/m-8000 by Scott M Baker, www.smbaker.com\n\n");
#endif

    Debug = 0;
    for (i=1; i<argc; i++) {
        if (argv[i][0]=='-') {
            if ((argv[i][1] == 'd') || (argv[i][1]=='D')) {
                Debug = 1;
            }
        } else if ((argv[i][0]=='D') && (argv[i][1]=='B') && (argv[i][2]=='G')) {
            /* I think the Olivetti may mess with args that start with -, so let's also
             * let the user type "DBG" for an arg.
             */
            Debug = 1;
        } else {
            fname = argv[i];
        }
    }
    
    GState = &zmachine_state;
    GState->startup_script = NULL; /* (argc >= 3) ? argv[2] : NULL; */
    GState->die = die;
    GState->writechar = putchar_wrapper;

    /* srandom((unsigned long) time(NULL)); */

    loadStory(fname);

    dbg("Story Loaded\n");

    while (!GState->quit)
        runInstruction();

    dbg("ok.\n");

    return 0;
} /* main */
