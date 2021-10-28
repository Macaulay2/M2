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

#endif
#endif
