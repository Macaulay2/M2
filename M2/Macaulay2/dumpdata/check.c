#include <unistd.h>
#include <stdio.h>
#include "dumpdata.h"

int main() {
  map *m = getmaps();
  printmaps(m);
  return 0;
}
