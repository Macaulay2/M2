#include <unistd.h>
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "dumpdata.h"

static int isStack(map m) {
  char dummy;
  void *p = (void *)&dummy;
  return p - m->from >= 0 && m->to - p > 0;
}

static int isDumpable(map m) {
  return m->w == 'w' && m->r == 'r' && m->p == 'p' && !isStack(m);
}

static int checksum(unsigned char *p, unsigned int len) {
  unsigned int c = 0;
  while (0 < len--) c = 23 * c + *p++;
  return c;
}

static void printmap(FILE *f, map m) {
  fprintf(f,"%08x-%08x %c%c%c%c %08x %02x:%02x %8d %10u %s\n",
	 (int)m->from, (int)m->to,
	 m->r, m->w, m->x, m->p,
	 m->offset,
	 m->dev_major, m->dev_minor, m->inode,
	 m->w == 'w' ? 0 : checksum((unsigned char *)m->from, m->to - m->from),
	 m->filename
	 );
}

static void dumpmap(int fd, map m) {
  write(fd, m->from, m->to-m->from);
}

void dumpdata(const char *dumpfilename) {
  struct MAP map;
  char fn[1000], buf[1000];
  int fd = open(dumpfilename,O_WRONLY|O_CREAT|O_TRUNC,0666);
  FILE *f  = fdopen(fd,"w");
  static char *mapfilename = "/proc/self/maps";
  long pos, n;
  FILE *maps = fopen(mapfilename,"r");
  if (maps == NULL) { fprintf(stderr,"can't open %s\n",mapfilename); exit(1); }
  while (TRUE) {
    if (NULL == fgets(buf,sizeof buf,maps)) break;
    fn[0] = 0;
    if (0 == sscanf(buf,"%x-%x %c%c%c%c %x %x:%x %d %1000s",
	   (int *)&map.from, (int *)&map.to, &map.r, &map.w, &map.x, &map.p,
	   &map.offset, &map.dev_major, &map.dev_minor, &map.inode, 
	   fn)) break;
    map.filename = fn;
    if (isStack(&map)) continue;
    printmap(f,&map);
  }
  pos = ftell(f);
  n = ((pos + EXEC_PAGESIZE - 1)/EXEC_PAGESIZE) * EXEC_PAGESIZE - pos;
  while (0 < n--) putc('\n',f);
  fflush(f);
  rewind(maps);
  while (TRUE) {
    if (NULL == fgets(buf,sizeof buf,maps)) break;
    fn[0] = 0;
    if (0 == sscanf(buf,"%x-%x %c%c%c%c %x %x:%x %d %1000s",
	   (int *)&map.from, (int *)&map.to, &map.r, &map.w, &map.x, &map.p,
	   &map.offset, &map.dev_major, &map.dev_minor, &map.inode, 
	   fn)) break;
    map.filename = fn;
    if (!isDumpable(&map)) continue;
    dumpmap(fd,&map);
  }
  fclose(maps);
  close(fd);
}
