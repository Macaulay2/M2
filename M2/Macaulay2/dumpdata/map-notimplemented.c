#include "std.h"

int haveDumpdata() {
  return FALSE;
}

int nummaps() {
  return 0;
}

int getmaps(int nmaps, struct MAP *maps) {
  return ERROR;
}

int isNotCheckable() {
     return FALSE;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/dumpdata "
 End:
*/
