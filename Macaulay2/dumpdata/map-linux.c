#include <string.h>
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

static char mapfilename[] = "/proc/self/maps";
static char mapfmt[] = "%p-%p %c%c%c%*c %08*x %02*x:%02*x %8*d %*s\n";

int nummaps() {
  return fnumlines(mapfilename);
}

int getmaps(int nmaps, struct MAP maps[nmaps]) {
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
      char buf2[len + 1];
      memcpy(buf2,line[i],len);
      buf2[len]=0;
      ret = sscanf(buf2, mapfmt, &maps[i].from, &maps[i].to, &r, &w, &x);
      if (5 != ret) {
	warning("can't parse map '%s' from file '%s'\n", buf2, mapfilename);
	return ERROR;
      }
      maps[i].r = r == 'r';
      maps[i].w = w == 'w';
      maps[i].x = x == 'x';
      maps[i].checksum = 0;
    }
    return OKAY;
  }
}
