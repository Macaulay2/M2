#include "std.h"

int haveDumpdata() {
  return FALSE;
}

int nummaps() {
  return 0;
}

int getmaps(int nmaps, struct MAP maps[nmaps]) {
  return ERROR;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/dumpdata "
 End:
*/
