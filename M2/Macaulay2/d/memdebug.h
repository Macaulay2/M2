/* Copyright 1997 by Daniel R. Grayson */

#define MEM_DEBUG 0				/* edit this line */

#if MEM_DEBUG

void debug_delete(void *);
void* debug_new(size_t size);
void* debug_new_atomic(size_t size);
void* debug_new_uncollectable(size_t size);

#ifndef MEM_DEBUG_INTERNAL

#undef GC_MALLOC_UNCOLLECTABLE
#define GC_MALLOC_UNCOLLECTABLE(n) debug_new_uncollectable(n)

#undef GC_MALLOC_ATOMIC
#define GC_MALLOC_ATOMIC(n) debug_new_atomic(n)

#undef GC_MALLOC
#define GC_MALLOC(n) debug_new(n)

#undef GC_FREE
#define GC_FREE(n) debug_delete(n)

#endif

#define FREE_DELAY       0
#define FENCE_INTS 	 2
#define FRONT_FENCE      0xaaaaaaaa
#define FRONT_FENCE_GONE 0xa0a0a0a0
#define BODY_PART        0xbbbbbbbb
#define BODY_PART_GONE   0xb0b0b0b0
#define REAR_FENCE       0xcccccccc
#define REAR_FENCE_GONE  0xc0c0c0c0

typedef struct FRONT {
     int trapcount;
     size_t size;
     unsigned int fence[FENCE_INTS];
     } front;

typedef struct REAR {
     unsigned int fence[FENCE_INTS];
     size_t size;
     int trapcount;
     } rear;

#define __addfront__(x) (((void *) x) + sizeof(front))
#define __subfront__(x) (((void *) x) - sizeof(front))
#else
#define __addfront__(x) (x)
#define __subfront__(x) (x)
#endif
