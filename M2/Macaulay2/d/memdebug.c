/* Copyright 1997 by Daniel R. Grayson */

#include <M2/config.h>
#ifdef MEMDEBUG

/* note: the debugging facilities in this file partially conflict with
   the debugging facilities gc.h provides, since when DEBUG is
   defined, it records the location of the calls to GC_malloc.  In
   fact, we bypass their debugging entirely, by calling the functions
   instead of the macros.
*/

/*

    Here is how you might use this.  (Turn it on by configuring with --enable-memdebug.)

	(gdb) run
	Starting program: /home/dan/local/src/M2/tmp/Macaulay2/bin/M2 -q --no-loaddata
	[New Thread 16384 (LWP 5498)]
	[Switching to Thread 16384 (LWP 5498)]

	Breakpoint 1, trap () at ../../../Macaulay2/d/debug.c:7

    We always set a breakpoint in trap().

    I want to look at the millionth memory allocation:

	(gdb) set trapset=1000000

	(gdb) c
	Continuing.
	Macaulay 2, version 0.9.5
	--package Main installed

	Breakpoint 1, trap () at ../../../Macaulay2/d/debug.c:7
	(gdb) up
	#1  0x0804d7d9 in trapchk (p=0x998cfd0) at ../../../Macaulay2/d/debug.c:11
	(gdb) up
	#2  0x08050b88 in M2_debug_malloc_atomic (size=19)
	    at ../../../Macaulay2/d/memdebug.c:110
	(gdb) up
	#3  0x0804df20 in getmem_atomic (n=19) at ../../../Macaulay2/d/M2mem.c:34
	(gdb) up
	#4  0x0804dd86 in strings_join (x=0x83dde80, y=0x9cf54e0)
	    at ../../../Macaulay2/d/M2types.c:119
	(gdb) up
	#5  0x08118521 in strings_plus_ (s_1=0x83dde80, t=0x9cf54e0) at strings.d:21
	(gdb) up
	#6  0x0809ff29 in presentfun (e_33={type_ = 34, ptr_ = 0x9cf54e0})
	    at actors4.d:820
	(gdb) up
	#7  0x080d0441 in evaluate_apply_4 (f_12={type_ = 5, ptr_ = 0x83e08b0}, e_1=
	      {type_ = 34, ptr_ = 0x9cf54e0}) at evaluate.d:585

    Let's say I'm suddenly interested in this pointer, f_12;

    I have a routine that will tell me the size of the memory area:

	(gdb) p M2_debug_size(f_12.ptr_)
	$2 = 8

    and a variable that tells how bytes are appended in front

	(gdb) p front
	$3 = 16

    and in the rear:

	(gdb) p rear
	$4 = 16

    So now look at memory, including the two fences:

	(gdb) x/10x f_12.ptr_-front
	0x83e08a0:	0x00001193	0x00000008	0xaaaaaaaa	0xaaaaaaaa
	0x83e08b0:	0x0809fed0	0x000f43e5	0xcccccccc	0xcccccccc
	0x83e08c0:	0x00000008	0x00001193

    The words 0xaaaaaaaa are the (intact) fence words in front, and the
    words 0xcccccccc are the fence words behind, while the memory is
    active.  They get changed to 0xa0a0a0a0 and to 0xc0c0c0c0 when the
    memory is freed.

    The data bytes themselves are initialized to 0xbbbbbbbb upon
    allocation, and changed to 0xb0b0b0b0 when the memory is freed.

    The two copies of 0x00001193 are the sequence number, and the two
    copies of 0x00000008 are the size.  They should agree.

*/

#include <M2/config.h>
#include <M2/gc-include.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#define MEMDEBUG_INTERNAL
#include "memdebug.h"
#include "debug.h"
#include "M2mem.h"

#define FREE_DELAY       10
#define FENCE_INTS 	 2
#define FRONT_FENCE      0xaaaaaaaa
#define FRONT_FENCE_GONE 0xa0a0a0a0
#define BODY_PART        0xbbbbbbbb
#define BODY_PART_GONE   0xb0b0b0b0
#define REAR_FENCE       0xcccccccc
#define REAR_FENCE_GONE  0xc0c0c0c0

struct FRONT {
     int trapcount;
     size_t size;
     unsigned int fence[FENCE_INTS];
     };

struct REAR {
     unsigned int fence[FENCE_INTS];
     size_t size;
     int trapcount;
     };




void *delay_chain[FREE_DELAY];
int delay_chain_index;

size_t M2_debug_size(void *p) {
     struct FRONT *f;
     if (p == NULL) return 0;
     f = (struct FRONT *)(p - sizeof(struct FRONT));
     return f->size;
}

void *M2_debug_malloc(size_t size) {
     struct FRONT *f;
     char *p;
     struct REAR *r;
     int i;
     int INTS_BODY = (size + sizeof(int) - 1)/sizeof(int);
     f = (struct FRONT *)GC_MALLOC( sizeof(struct FRONT) + sizeof(int)*INTS_BODY + sizeof(struct REAR) );
     if (f == NULL) outofmem2(size);
     p = (char *)f + sizeof(struct FRONT);
     r = (struct REAR *)(p + sizeof(int)*INTS_BODY);
     f->size = r->size = size;
     for (i=0; i<FENCE_INTS; i++) f->fence[i] = FRONT_FENCE;
     for (i=0; i<INTS_BODY; i++) ((int *)p)[i] = BODY_PART;
     for (i=0; i<FENCE_INTS; i++) r->fence[i] = REAR_FENCE;
     f->trapcount = r->trapcount = trapcount+1;
     trapchk(p);			/* trapchk increments trapcount before possibly calling trap() -- set your breakpoint in trap() */
     return p;
     }

void* M2_debug_malloc_uncollectable(size_t size) {
     struct FRONT *f;
     char *p;
     struct REAR *r;
     int i;
     int INTS_BODY = (size + sizeof(int) - 1)/sizeof(int);
     f = (struct FRONT *)GC_MALLOC_UNCOLLECTABLE(
	  sizeof(struct FRONT) + sizeof(int)*INTS_BODY + sizeof(struct REAR)
	  );
     if (f == NULL) outofmem2(size);
     p = (char *)f + sizeof(struct FRONT);
     r = (struct REAR *)(p + sizeof(int)*INTS_BODY);
     f->size = r->size = size;
     for (i=0; i<FENCE_INTS; i++) f->fence[i] = FRONT_FENCE;
     for (i=0; i<INTS_BODY; i++) ((int *)p)[i] = BODY_PART;
     for (i=0; i<FENCE_INTS; i++) r->fence[i] = REAR_FENCE;
     f->trapcount = r->trapcount = trapcount+1;
     trapchk(p);
     return p;
     }

void* M2_debug_malloc_atomic(size_t size) {
     struct FRONT *f;
     char *p;
     struct REAR *r;
     int i;
     int INTS_BODY = (size + sizeof(int) - 1)/sizeof(int);
     f = (struct FRONT *)GC_MALLOC_ATOMIC( sizeof(struct FRONT) + sizeof(int)*INTS_BODY + sizeof(struct REAR) );
     if (f == NULL) outofmem2(size);
     p = (void *)f + sizeof(struct FRONT);
     r = (struct REAR *)(p + sizeof(int)*INTS_BODY);
     f->size = r->size = size;
     for (i=0; i<FENCE_INTS; i++) f->fence[i] = FRONT_FENCE;
     for (i=0; i<INTS_BODY; i++) ((int *)p)[i] = BODY_PART;
     for (i=0; i<FENCE_INTS; i++) r->fence[i] = REAR_FENCE;
     f->trapcount = r->trapcount = trapcount+1;
     trapchk(p);
     return p;
     }

void* M2_debug_malloc_atomic_uncollectable(size_t size) {
     struct FRONT *f;
     char *p;
     struct REAR *r;
     int i;
     int INTS_BODY = (size + sizeof(int) - 1)/sizeof(int);
     f = (struct FRONT *)GC_MALLOC_ATOMIC_UNCOLLECTABLE( sizeof(struct FRONT) + sizeof(int)*INTS_BODY + sizeof(struct REAR) );
     if (f == NULL) outofmem2(size);
     p = (void *)f + sizeof(struct FRONT);
     r = (struct REAR *)(p + sizeof(int)*INTS_BODY);
     f->size = r->size = size;
     for (i=0; i<FENCE_INTS; i++) f->fence[i] = FRONT_FENCE;
     for (i=0; i<INTS_BODY; i++) ((int *)p)[i] = BODY_PART;
     for (i=0; i<FENCE_INTS; i++) r->fence[i] = REAR_FENCE;
     f->trapcount = r->trapcount = trapcount+1;
     trapchk(p);
     return p;
     }

static void volatile smashed(void *p) {
     if (0 == GC_base(p)) {
	  fprintf(stderr,"-- *** memdebug -- non-heap object encountered, %p, aborting\n",p);
	  }
     else {
	  fprintf(stderr,"-- *** memdebug -- smashed object found, %p, aborting\n",p);
	  }
     trap();
     abort();
     }

void *M2_debug_to_outer(void *p) {
     struct FRONT *f = p - sizeof(struct FRONT);
     if (f->fence[0] != FRONT_FENCE) smashed(p);
     return f;
     }


void *M2_debug_to_inner(void *q) {
     struct FRONT *f = q;
     if (f->fence[0] != FRONT_FENCE) smashed(q);
     return (void *)f + sizeof(struct FRONT);
     }

void M2_debug_info(void *p) {
     struct FRONT *f;
     struct REAR *r;
     int INTS_BODY, i, smashed = 0;
     size_t size;
     if (p == NULL) return;
     f = p - sizeof(struct FRONT);
     size = f->size;
     INTS_BODY = (size + sizeof(int) - 1)/sizeof(int);
     r = (struct REAR *)(p + sizeof(int)*INTS_BODY);
     for (i=0; i<FENCE_INTS; i++) if (f->fence[i] != FRONT_FENCE) smashed = 1;
     for (i=0; i<FENCE_INTS; i++) if (r->fence[i] != REAR_FENCE ) smashed = 1;
     fprintf(stderr,"addr %p, base %p, GC_base %p, trapcount %d, length %lu%s\n",p,f,GC_base(p),f->trapcount,size,smashed ? ", smashed" : "");
     }

void M2_debug_free(void *p) {
     struct FRONT *f;
     struct REAR *r;
     int INTS_BODY, i, _trapcount;
     size_t size;
     if (p == NULL) return;
     f = (struct FRONT *)(p - sizeof(struct FRONT));
     size = f->size;
     INTS_BODY = (size + sizeof(int) - 1)/sizeof(int);
     r = p + sizeof(int)*INTS_BODY;
     _trapcount = f->trapcount;
     if (r->trapcount != _trapcount || r->size != size) smashed(p);
     for (i=0; i<FENCE_INTS; i++) if (f->fence[i] != FRONT_FENCE) smashed(p);
     for (i=0; i<FENCE_INTS; i++) if (r->fence[i] != REAR_FENCE ) smashed(p);
     if (_trapcount == trapset) trap();
     trapchk(p);
     for (i=0; i<FENCE_INTS; i++) f->fence[i] = FRONT_FENCE_GONE;
     for (i=0; i<INTS_BODY; i++) ((int *)p)[i] = BODY_PART_GONE;
     for (i=0; i<FENCE_INTS; i++) r->fence[i] = REAR_FENCE_GONE;
#if FREE_DELAY != 0
     if (delay_chain[delay_chain_index] != NULL) {
	  GC_FREE(delay_chain[delay_chain_index]);
	  }
     delay_chain[delay_chain_index] = (void *)f;
     delay_chain_index ++;
     if (delay_chain_index == FREE_DELAY) delay_chain_index = 0;
#else
     GC_FREE(f);
#endif
     }

void* M2_debug_realloc(void *old, size_t size) {
     void *new = M2_debug_malloc(size);
     size_t oldsize = M2_debug_size(old);
     if (new == NULL) outofmem2(size);
     memcpy(new,old,size < oldsize ? size : oldsize);
     return new;
     }

#endif /* MEMDEBUG */
