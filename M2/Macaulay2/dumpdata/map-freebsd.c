#include <unistd.h>
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include "warning.h"
#include "map.h"
#include "file.h"
#include "std.h"

static char mapfilename[] = "/proc/curproc/map";
static char mapfmt[] = "%p %p %*d %*d %*d %c%c%c %*d %*d 0x%*x %*s\n";

int haveDumpdata() {
  return TRUE;
}

int nummaps() {
  return fnumlines(mapfilename);
}

int getmaps(int nmaps, struct MAP map[nmaps]) {
  int mlen = filelen(mapfilename);
  if (mlen == ERROR) return ERROR;
  else {
    char buf[mlen];
    char *line[nmaps];
    int i;
    if (ERROR == getfile(mapfilename,mlen,buf)) return ERROR;
    lines(mlen,buf,nmaps,line);
    for (i=0; i<nmaps; i++) {
      int ret;
      char r, w, x;
      int len = linelen(line[i]);
      char buf[len + 1];
      memcpy(buf,line[i],len);
      buf[len]=0;
      ret = sscanf(buf, mapfmt, &map[i].from, &map[i].to, &r, &w, &x);
      if (5 != ret) {
	warning("can't parse map '%s' from file '%s'\n", buf, mapfilename);
	return ERROR;
      }
      map[i].r = r == 'r';
      map[i].w = w == 'w';
      map[i].x = x == 'x';
      map[i].checksum = 0;
      map[i].filename = NULL;
    }
    return OKAY;
  }
}

int isNotCheckable() {
     return FALSE;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/dumpdata "
 End:
*/
