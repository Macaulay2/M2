/*
  This works on SunOS 5.5.1.
  Read about it in "man -s4 proc".
  But it's obsolete - the new way is to get the information from /proc/$$/map, with SunOS 5.8
*/

#include <stddef.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/fault.h>
#include <sys/syscall.h>
#include <sys/procfs.h>
#include <sys/ioctl.h>
#include "map.h"
#include "std.h"

int haveDumpdata() {
  return TRUE;
}

static int openproc() {
  char buf[30];
  snprintf(buf,sizeof(buf),"/proc/%d",(int)getpid());
  return open(buf,O_RDONLY);
}

extern int nummaps() {
  int nmaps, fd = openproc();
  if (fd == ERROR) return ERROR;
  if (ERROR == ioctl(fd, PIOCNMAP, &nmaps)) {
    close(fd);
    perror("can't get number of memory maps\n");
    exit(1);
  }
  close(fd);
  return nmaps;
}

extern int getmaps(int nmaps, struct MAP maps[nmaps]) {
  int i;
  prmap_t buf[nmaps+1];
  int fd = openproc();
  if (fd == ERROR) return ERROR;
  if (ERROR == ioctl(fd, PIOCMAP, buf)) {
    close(fd);
    return ERROR;
  }
  for (i=0; i<nmaps; i++) {
    maps[i].from = buf[i].pr_vaddr;
    maps[i].to = buf[i].pr_vaddr + buf[i].pr_size;
    maps[i].r = (buf[i].pr_mflags & MA_READ ) != 0;
    maps[i].w = (buf[i].pr_mflags & MA_WRITE) != 0;
    maps[i].x = (buf[i].pr_mflags & MA_EXEC ) != 0;
    maps[i].checksum = 0;
    maps[i].filename = NULL;
  }
  return close(fd);
}

int isNotCheckable() {
     return FALSE;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/dumpdata "
 End:
*/
