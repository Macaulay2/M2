#include <stdio.h>

#include "types.h"
#include "M2mem.h"
#include "M2mem2.h"

sigjmp_buf out_of_memory_jump;
char out_of_memory_jump_set = FALSE;

void outofmem(void) {
     static int count = 0;
     if (!tokens_stopIfError && out_of_memory_jump_set && count++ < 5) {
     	  fprintf(stderr,"out of memory, returning to top level");
     	  fflush(stderr);
     	  siglongjmp(out_of_memory_jump,1);
	  }
     else {
     	  fprintf(stderr,"out of memory, exiting\n");
	  exit(1);
	  }
     }

char *getmem(unsigned int n)
{
  char *p;
  p = GC_MALLOC(n);
  if (p == NULL) outofmem();
  return p;
}

char *getmem_atomic(unsigned int n)
{
  char *p;
  p = GC_MALLOC_ATOMIC(n);
  if (p == NULL) outofmem();
  return p;
}

char *getmem_malloc(unsigned int n)
{
  char *p;
  p = malloc(n);
  if (p == NULL) outofmem();
  return p;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
