#include <stdio.h>
#include <unistd.h>
#include "printmaps.h"
#include "std.h"
#include "maputil.h"

int printmaps(int fd) {
  int nmaps = nummaps(), i;
  struct MAP dumpmaps[nmaps];
  if (ERROR == getmaps(nmaps,dumpmaps)) return ERROR;
  checkmaps(nmaps,dumpmaps);
  for (i=0; i<nmaps; i++) fdprintmap(fd,&dumpmaps[i]);
  return 0;
}
