/* Copyright 1997 by Daniel R. Grayson */

#include <gc.h>
#include <stdio.h>
#include <unistd.h>
#define MEM_DEBUG_INTERNAL
#include "memdebug.h"

#ifdef MEM_DEBUG

extern void outofmem(void);
extern void trap(void);

int trapset = 0;
void *trapaddr = 0;
int trapcount = 0;
void *delay_chain[FREE_DELAY];
int delay_chain_index;

void* debug_new(size_t size) {
     front *f;
     char *p;
     rear *r;
     int i;
     int INTS_BODY = (size + sizeof(int) - 1)/sizeof(int);
     f = (front *)GC_MALLOC(
	  sizeof(front) + sizeof(int)*INTS_BODY + sizeof(rear)
	  );
     if (f == NULL) outofmem();
     p = (char *)f + sizeof(front);
     r = (rear *)(p + sizeof(int)*INTS_BODY);
     f->size = r->size = size;
     f->trapcount = r->trapcount = ++trapcount;
     for (i=0; i<FENCE_INTS; i++) f->fence[i] = FRONT_FENCE;
     for (i=0; i<INTS_BODY; i++) ((int *)p)[i] = BODY_PART;
     for (i=0; i<FENCE_INTS; i++) r->fence[i] = REAR_FENCE;
     if (trapcount == trapset || p == trapaddr) trap();
     return p;
     }

void* debug_new_uncollectable(size_t size) {
     front *f;
     char *p;
     rear *r;
     int i;
     int INTS_BODY = (size + sizeof(int) - 1)/sizeof(int);
     f = (front *)GC_MALLOC_UNCOLLECTABLE(
	  sizeof(front) + sizeof(int)*INTS_BODY + sizeof(rear)
	  );
     if (f == NULL) outofmem();
     p = (char *)f + sizeof(front);
     r = (rear *)(p + sizeof(int)*INTS_BODY);
     f->size = r->size = size;
     f->trapcount = r->trapcount = ++trapcount;
     for (i=0; i<FENCE_INTS; i++) f->fence[i] = FRONT_FENCE;
     for (i=0; i<INTS_BODY; i++) ((int *)p)[i] = BODY_PART;
     for (i=0; i<FENCE_INTS; i++) r->fence[i] = REAR_FENCE;
     if (trapcount == trapset || p == trapaddr) trap();
     return p;
     }

void* debug_new_atomic(size_t size) {
     front *f;
     char *p;
     rear *r;
     int i;
     int INTS_BODY = (size + sizeof(int) - 1)/sizeof(int);
     f = (front *)GC_MALLOC_ATOMIC(
	  sizeof(front) + sizeof(int)*INTS_BODY + sizeof(rear)
	  );
     if (f == NULL) outofmem();
     p = (void *)f + sizeof(front);
     r = (rear *)(p + sizeof(int)*INTS_BODY);
     f->size = r->size = size;
     f->trapcount = r->trapcount = ++trapcount;
     for (i=0; i<FENCE_INTS; i++) f->fence[i] = FRONT_FENCE;
     for (i=0; i<INTS_BODY; i++) ((int *)p)[i] = BODY_PART;
     for (i=0; i<FENCE_INTS; i++) r->fence[i] = REAR_FENCE;
     if (trapcount == trapset || p == trapaddr) trap();
     return p;
     }

static void volatile smashed() {
     fprintf(stderr,"smashed object found\n");
     trap();
     _exit(1);
     }

void debug_delete(void *p) {
     front *f;
     rear *r;
     int INTS_BODY, i, _trapcount;
     size_t size;
     if (p == NULL) return;
     f = (front *)(p - sizeof(front));
     size = f->size;
     INTS_BODY = (size + sizeof(int) - 1)/sizeof(int);
     r = (rear *)(p + sizeof(int)*INTS_BODY);
     _trapcount = f->trapcount;
     if (r->trapcount != _trapcount || r->size != size) smashed();
     for (i=0; i<FENCE_INTS; i++) if (f->fence[i] != FRONT_FENCE) smashed();
     for (i=0; i<FENCE_INTS; i++) if (r->fence[i] != REAR_FENCE) smashed();
     if (_trapcount == trapset || p == trapaddr) trap();
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

#endif
