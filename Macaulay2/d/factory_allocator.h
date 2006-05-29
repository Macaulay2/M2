#include <stdlib.h>

extern void* getBlock ( size_t );
extern void* getBlockClear ( size_t, size_t );
extern void freeBlockN ( void* );
extern void* reallocBlock ( void* , size_t , size_t );
extern void* reallocBlockN ( void* , size_t );

#ifdef REPLACE_MALLOC
#define malloc getBlock
#define calloc getBlockClear
#define free freeBlockN
#define realloc reallocBlockN
#endif
