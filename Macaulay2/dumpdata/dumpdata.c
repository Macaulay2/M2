#include <unistd.h>
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "dumpdata.h"

typedef struct MAP {
  void *from, *to;
  char r, w, x, p;
  int offset;
  int dev_major, dev_minor, inode;
  unsigned int chksum;
  char *filename;
} * map;

static int isStack(map m) {
  char dummy;
  void *p = (void *)&dummy;
  return p - m->from >= 0 && m->to - p > 0;
}

static int isDumpable(map m) {
  return m->w == 'w' && m->r == 'r' && m->p == 'p' && !isStack(m);
}

static unsigned int checksum(unsigned char *p, unsigned int len) {
  unsigned int c = 0;
  while (0 < len--) c = 23 * c + *p++;
  return c;
}

static char mapfmt[] = "%p-%p %c%c%c%c %08x %02x:%02x %8d      %s\n";
static char mapfmt2[] = "%p-%p %c%c%c%c %08x %02x:%02x %8d %10u %s\n";

static void printmap(FILE *f, map m) {
  fprintf(f,mapfmt2,
	 m->from, m->to,
	 m->r, m->w, m->x, m->p,
	 m->offset,
	 m->dev_major, m->dev_minor, m->inode,
	 m->chksum,
	 m->filename
	 );
}

static int dumpmap(int fd, map m) {
  return write(fd, m->from, m->to-m->from);
}

static char mapfilename[] = "/proc/self/maps";

int dumpdata(const char *dumpfilename) {
  struct MAP map;
  char fn[1000], buf[1000];
  char *p;
  int fd = open(dumpfilename,O_WRONLY|O_CREAT|O_TRUNC,0666);
  FILE *f;
  long pos, n;
  FILE *maps;
  f  = fdopen(fd,"w");
  if (f == NULL) return ERROR;
  maps = fopen(mapfilename,"r");
  if (maps == NULL) return ERROR;
  while (TRUE) {
    if (NULL == fgets(buf,sizeof buf,maps)) break;
    fn[0] = 0;
    if (0 == sscanf(buf,mapfmt,
	   &map.from, &p, &map.r, &map.w, &map.x, &map.p,
	   &map.offset, &map.dev_major, &map.dev_minor, &map.inode, 
	   fn)) break;
    map.to = p;			/* work around a gcc bug */
    map.filename = fn;
    map.chksum = map.r == 'r' && map.w == '-' ? checksum((unsigned char *)map.from, map.to - map.from) : 0;
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
    if (0 == sscanf(buf,mapfmt,
	   &map.from, &p, &map.r, &map.w, &map.x, &map.p,
	   &map.offset, &map.dev_major, &map.dev_minor, &map.inode, 
	   fn)) break;
    map.to = p;			/* work around a gcc bug */
    map.filename = fn;
    if (!isDumpable(&map)) continue;
    if (ERROR == dumpmap(fd,&map)) return fclose(maps), fclose(f), ERROR;
  }
  fclose(maps);
  return close(fd);
}

int loaddata(const char *filename) {
  struct MAP map, fmap;
  int fd = open(filename,O_RDONLY);
  FILE *f;
  char fbuf[1000], fn[1000], buf[1000];
  char *p;
  FILE *maps;
  if (fd == ERROR) return ERROR;
  f = fdopen(fd,"r");
  if (f == NULL) { fprintf(stderr, "failed to open file from fd %d\n", fd); return ERROR; }
  maps = fopen(mapfilename,"r");
  if (maps == NULL) { fprintf(stderr, "failed to open map file %s\n", mapfilename); return ERROR; }
  while (TRUE) {
    int maps_end = NULL == fgets(buf,sizeof buf,maps);
    int f_end = NULL == fgets(fbuf,sizeof fbuf,f) || fbuf[0]=='\n';
    if (maps_end != f_end) { 
      fclose(maps); fclose(f); 
      if (!f_end) fprintf(stderr,"data file has an extra map: %s\n", fbuf);
      else        fprintf(stderr,"program running with an extra map: %s\n", buf);
      return ERROR;
    }
    fn[0] = 0;
    if (11 > sscanf(buf,mapfmt,
		    &map.from, &p, &map.r, &map.w, &map.x, &map.p,
		    &map.offset, &map.dev_major, &map.dev_minor, &map.inode, 
		    fn)
	) return ERROR;
    map.to = p;			/* work around a gcc bug */
    map.filename = fn;
    map.chksum = map.r == 'r' && map.w == '-' ? checksum((unsigned char *)map.from, map.to - map.from) : 0;
    fn[0] = 0;
    if (12 > sscanf(fbuf,mapfmt2,
		    &fmap.from, &p, &fmap.r, &fmap.w, &fmap.x, &fmap.p,
		    &fmap.offset, &fmap.dev_major, &fmap.dev_minor, &fmap.inode, 
		    &fmap.chksum, fn)
	) return ERROR;
    fmap.to = p;			/* work around a gcc bug */
    fmap.filename = fn;
    if (!isStack(&map)) {
      if (fmap.from != map.from) return ERROR;
      if (fmap.to != map.to) return ERROR;
    }
    if (fmap.r != map.r) return ERROR;
    if (fmap.w != map.w) return ERROR;
    if (fmap.x != map.x) return ERROR;
    if (fmap.p != map.p) return ERROR;
    if (fmap.chksum != map.chksum) { 
      fprintf(stderr, "checksum of library %s has changed from %u to %u\n", 
	      map.filename, fmap.chksum, map.chksum);
      return ERROR;
    }
    if (0 != strcmp(fmap.filename,map.filename)) { 
      fprintf(stderr, "filename library %s has changed to %s\n", fmap.filename, map.filename);
      return ERROR;
    }
    printmap(stdout,&map);
    printmap(stdout,&fmap);
  }
  return 0;
}
  
