#include <setjmp.h>
#include <stdlib.h>
#include <stddef.h>
#include <unistd.h>
#include <stdio.h>
#include "dumpdata.h"
#include "std.h"

static char *x = "initial value";
static char message[100];
int main(int argc, char **argv) {
  jmp_buf j;
  char *filename = "check-data";
  static char *p[6];
  if (!setjmp(j)) {
    if (argc > 2) strcpy(message,argv[2]);
    fprintf(stderr,"  p[] = {%p,%p,%p,%p,%p,%p}\n", p[0], p[1], p[2], p[3], p[4], p[5]);
    fprintf(stderr,"  x = %s\n", x);
    if (argc > 1 && 0 == strcmp(argv[1],"dump")) {
      x = "new value";
      p[0] = malloc(4);
      p[1] = malloc(4);
      p[2] = malloc(4);
      fprintf(stderr,"  p[] = {%p,%p,%p,%p,%p,%p}\n", p[0], p[1], p[2], p[3], p[4], p[5]);
      fprintf(stderr,"  x = %s\n", x);
      if (ERROR == dumpdata(filename)) { fprintf(stderr, "failed to dump data to file %s\n", filename); return 1; }
      fprintf(stderr,"  data dumped\n");
    }
    else if (argc > 1 && 0 == strcmp(argv[1],"load")) {
      if (ERROR == loaddata(filename)) { fprintf(stderr, "failed to load data from file %s\n", filename); return 1; }
      fprintf(stderr,"  data loaded\n");
      longjmp(j,1);
    }
    else { fprintf(stderr, "usage: %s [dump|load]\n", argv[0]); return 0; }
    return OKAY;
  }
  else {
    int haderror = FALSE;
    fprintf(stderr,"  argv[2] was \"%s\"\n", message);
    fprintf(stderr,"  argv[2] is \"%s\"\n", argv[2]);
    fprintf(stderr,"  x = %s\n", x);
    fprintf(stderr,"  p[] = {%p,%p,%p,%p,%p,%p}\n", p[0], p[1], p[2], p[3], p[4], p[5]);
    fprintf(stderr,"  allocating some more memory\n");
    p[3] = malloc(4);
    p[4] = malloc(4);
    p[5] = malloc(4);
    fprintf(stderr,"  p[] = {%p,%p,%p,%p,%p,%p}\n", p[0], p[1], p[2], p[3], p[4], p[5]);
    if (p[3]-p[2] != p[2]-p[1]) {
      fprintf(stderr,"something's peculiar about that!\n");
      haderror = TRUE;
    }
    return haderror;
  }
}
