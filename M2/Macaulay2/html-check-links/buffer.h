#ifndef BUFFER_H
#define BUFFER_H

#include "getmem.h"

#define BUFFER(type) \
	typedef struct { int size, used; type *array; } type##buf; \
	static inline void rm##type(type##buf *x) { \
		if (x->used > 0) x->used--; \
		} \
	static inline void empty##type(type##buf *x) { \
		x->used = 0; \
		} \
	static inline void add##type(type##buf *x,type i) { \
		if (x->used == x->size) { \
			unsigned newsize = 2 * x->size + 1; \
			type *y = (type *) getmem(newsize * sizeof(type)); \
			if (x->size > 0) memcpy(y, x->array, x->size * sizeof(type)); \
			x->array = y; \
			x->size = newsize; \
			} \
		x->array[x->used++] = i; \
		}			

#define  scan(x,f) do { int __i, __n = x.used; for(__i=0    ; __i< __n; __i++) f(x.array[__i]); } while(0)
#define rscan(x,f) do { int __i, __n = x.used; for(__i=__n-1; __i>=0  ; __i--) f(x.array[__i]); } while(0)

BUFFER(char)
BUFFER(int)
#endif
