#include <stdio.h>

#include "types.h"
#include "M2mem.h"
#include "M2mem2.h"
#include "debug.h"

sigjmp_buf out_of_memory_jump;
char out_of_memory_jump_set = FALSE;

void outofmem(void) {
#if 0
     static int count = 0;
     if (!tokens_stopIfError && out_of_memory_jump_set && count++ < 5) {
     	  fprintf(stderr,"out of memory, returning to top level\n");
     	  fflush(stderr);
     	  siglongjmp(out_of_memory_jump,1);
	  }
     else 
#endif
     {
	  char *msg = "\n\n *** out of memory, exiting ***\n";
	  write(STDERR,msg,strlen(msg));
	  exit(1);
     }
}

char *getmem(unsigned int n)
{
  char *p;
  p = GC_MALLOC(n);
  if (p == NULL) outofmem();
#ifdef DEBUG
  trapchk(p);
#endif
  return p;
}

char *getmem_atomic(unsigned int n)
{
  char *p;
  p = GC_MALLOC_ATOMIC(n);
  if (p == NULL) outofmem();
#ifdef DEBUG
  trapchk(p);
#endif
  return p;
}

char *getmem_malloc(unsigned int n)
{
  char *p;
  p = malloc(n);
  if (p == NULL) outofmem();
#ifdef DEBUG
  trapchk(p);
#endif
  return p;
}

char *getmem_atomic_clear(unsigned int n)
{
  char *p;
  p = GC_MALLOC_ATOMIC(n);
  if (p == NULL) outofmem();
#ifdef DEBUG
  trapchk(p);
#endif
  bzero(p,n);
  return p;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
