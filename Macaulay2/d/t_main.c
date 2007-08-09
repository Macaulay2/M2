#include <stdio.h>
#include <stdlib.h>

#include "config.h"
#include <gc/gc.h>

#include "memdebug.h"
#include <gmp.h>
#include "types.h"

void outofmem(){
  fprintf(stderr,"out of memory, exiting\n");
  exit(1);
}

int main(int argc, char **argv) {
  extern int t__prepare();
  mp_set_memory_functions(GC_malloc1,GC_realloc3,GC_free2);
  t__prepare();
  return 0;
}
