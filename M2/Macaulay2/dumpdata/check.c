#include <stddef.h>
#include <unistd.h>
#include <stdio.h>
#include "dumpdata.h"

char *x = "initial value";

int main(int argc, char **argv) {
  char *filename = "check-data";
  if (argc > 1 && 0 == strcmp(argv[1],"dump")) {
    x = "new value";
    if (ERROR == dumpdata(filename)) { fprintf(stderr, "failed to dump data to file %s\n", filename); return 1; }
  }
  else if (argc > 1 && 0 == strcmp(argv[1],"load")) {
    if (ERROR == loaddata(filename)) { fprintf(stderr, "failed to load data from file %s\n", filename); return 1; }
    printf("x = %s\n", x);
  }
  else { fprintf(stderr, "usage: %s [dump|load]\n", argv[0]); return 0; }
  return 0;
}
