/*
  This works on SunOS saturn.math.uiuc.edu 5.5.1 Generic sun4m sparc SUNW,SPARCstation-5
  Read about it in "man -s4 proc".
*/

#include <stddef.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <sys/fault.h>
#include <sys/syscall.h>
#include <sys/procfs.h>
#include "map.h"
#include "std.h"

static int openproc() {
  char buf[30];
  sprintf(buf,"/proc/%d",(int)getpid());
  return open(buf,O_RDONLY);
}

extern int nummaps() {
  int nmaps, fd = openproc();
  if (fd == ERROR) return ERROR;
  if (ERROR == ioctl(fd, PIOCNMAP, &nmaps)) return ERROR;
  close(fd);
  return nmaps;
}

extern int getmaps(int nmaps, struct MAP maps[nmaps]) {
  int i;
  prmap_t buf[nmaps];
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
  }
  return close(fd);
}
