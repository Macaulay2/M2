/* Copyright 1997,2004 by Daniel R. Grayson */

#ifndef MEMDEBUG_H
#define MEMDEBUG_H
#ifdef MEMDEBUG
#if defined(__cplusplus)
extern "C" {
#endif

    void M2_debug_free(void *);
    void* M2_debug_malloc(size_t size);
    void* M2_debug_malloc_atomic(size_t size);
    void* M2_debug_malloc_atomic_uncollectable(size_t size);
    void* M2_debug_realloc(void *,size_t size);
    void* M2_debug_malloc_uncollectable(size_t size);
    void *M2_debug_to_outer(void *p);
    void *M2_debug_to_inner(void *f);

#if defined(__cplusplus)
}
#endif

#ifndef MEMDEBUG_INTERNAL

#undef GC_MALLOC_UNCOLLECTABLE
#define GC_MALLOC_UNCOLLECTABLE M2_debug_malloc_uncollectable
#define GC_malloc_uncollectable M2_debug_malloc_uncollectable

#define GC_malloc_atomic_uncollectable M2_debug_malloc_atomic_uncollectable

#undef GC_MALLOC_ATOMIC
#define GC_MALLOC_ATOMIC M2_debug_malloc_atomic
#define GC_malloc_atomic M2_debug_malloc_atomic

#undef GC_MALLOC
#define GC_MALLOC M2_debug_malloc
#define GC_malloc M2_debug_malloc
#define GC_debug_malloc M2_debug_malloc

#undef GC_REALLOC
#define GC_REALLOC M2_debug_realloc
#define GC_realloc M2_debug_realloc
#define GC_debug_realloc M2_debug_realloc

#undef GC_FREE
#define GC_FREE M2_debug_free
#define GC_free M2_debug_free
#define GC_debug_free M2_debug_free

#endif
#endif
#endif
