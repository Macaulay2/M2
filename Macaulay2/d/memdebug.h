/* Copyright 1997 by Daniel R. Grayson */

void debug_delete(void *);
void* debug_new(size_t size);
void* debug_new_atomic(size_t size);
void* debug_new_uncollectable(size_t size);

#undef GC_MALLOC_UNCOLLECTABLE
#define GC_MALLOC_UNCOLLECTABLE(n) debug_new_uncollectable(n)

#undef GC_MALLOC_ATOMIC
#define GC_MALLOC_ATOMIC(n) debug_new_atomic(n)

#undef GC_MALLOC
#define GC_MALLOC(n) debug_new(n)

#undef GC_FREE
#define GC_FREE(n) debug_delete(n)
