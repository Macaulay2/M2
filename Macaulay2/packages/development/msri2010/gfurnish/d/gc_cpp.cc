/*************************************************************************
Copyright (c) 1994 by Xerox Corporation.  All rights reserved.
 
THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 
    Last modified on Sat Nov 19 19:31:14 PST 1994 by ellis
                  on Sat Jun  8 15:10:00 PST 1994 by boehm

Permission is hereby granted to copy this code for any purpose,
provided the above notices are retained on all copies.

This implementation module for gc_c++.h provides an implementation of
the global operators "new" and "delete" that calls the Boehm
allocator.  All objects allocated by this implementation will be
non-collectable but part of the root set of the collector.

You should ensure (using implementation-dependent techniques) that the
linker finds this module before the library that defines the default
built-in "new" and "delete".

Authors: John R. Ellis and Jesse Hull

**************************************************************************/
/* Boehm, December 20, 1994 7:26 pm PST */

/* modified by Dan Grayson, 1996, 1997, for use with Macaulay 2 */

/* #define GC_DEBUG */

#include <stdlib.h>
#include "gc_cpp.h"
#include <M2/config.h>

extern "C" void outofmem();

#if !defined(__MWERKS__)

    extern "C" {
#   include "memdebug.h"
    }

#   ifndef NDEBUG

	static void set_to_garbage(void *p, size_t size) {
	  char *q = (char *)p;
	  for (; size>0; size--, q++) *q = 0xe5;
	}

	void* operator new( size_t size ) {
	    void *p;
	    p = malloc( size );
	    if (p == NULL) outofmem();
	    set_to_garbage(p,size);
	    return p;
	}

	void operator delete( void* p ) {
	    if (p != NULL) free(p) ;
	     }

	void* operator new[]( size_t size ) {
	    void *p;
	    p = malloc( size );
	    if (p == NULL) outofmem();
	    set_to_garbage(p,size);
	    return p;
	    }

	void operator delete[]( void* p ) {
	    if (p != NULL) free(p) ;
	    }

#   else

	void* operator new( size_t size ) {
	    void *p;
	    p = GC_MALLOC_UNCOLLECTABLE( size );
	    if (p == NULL) outofmem();
	    return p;
	}

	void operator delete( void* p ) {
	    if (p != NULL) GC_FREE(p) ;
	     }

	void* operator new[]( size_t size ) {
	    void *p;
	    p = GC_MALLOC_UNCOLLECTABLE( size );
	    if (p == NULL) outofmem();
	    return p;
	    }

	void operator delete[]( void* p ) {
	    if (p != NULL) GC_FREE(p) ;
	    }


#       if 0

        ///////////////////////////////////////////////////
        // This is the old way

	void* __builtin_new( unsigned int size ) {
	    void *p = GC_MALLOC_UNCOLLECTABLE( size );
	    if (p == NULL) outofmem();
	    return p;
	}

	void __builtin_delete( void* obj ) {
	    if (obj != NULL) GC_FREE( obj );
	    }

	void* __builtin_vec_new( unsigned int size ) {
	    void *p = GC_MALLOC_UNCOLLECTABLE( size );
	    if (p == NULL) outofmem();
	    return p;
	}

	void __builtin_vec_delete( void* obj ) {
	    if (obj != NULL) GC_FREE( obj );
	    }

	//
        ///////////////////////////////////////////////////

#       endif

#    endif

#endif
