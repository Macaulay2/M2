#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "env.h"

int main (int argc, char **argv) {
  if (argc < 3) {
    fprintf(stderr,"%s NAME\n",argv[0]);
    exit(1);
  }
  unsetenv(argv[1]);
  execvp(argv[2],argv+2);
  fprintf(stderr,"%s: failed to exec \"%s\"\n", argv[0], argv[2]);
  return 1;
}
