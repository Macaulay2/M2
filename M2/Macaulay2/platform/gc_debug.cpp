#include "gc_debug.h"

void onExitGCStatistics()
{
#if 0
  fprintf(stderr,"gc: heap size = %d, free space divisor = %ld, collections = %ld\n", 
	  GC_get_heap_size(), GC_free_space_divisor, GC_gc_no-old_collections);
#endif
}
