#include <unistd.h>
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include "dumpdata.h"

typedef struct MAP {
  void *from, *to;
  char r, w, x, p;
  int offset;
  int dev_major, dev_minor, inode;
  unsigned int chksum;
  char *filename;
} * map;

static void trim(char *s) {
  if (s == NULL) return;
  if (s[0] == 0) return;
  while (s[1]) s++;
  if (s[0] == '\n') s[0] = 0;
}

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

static char mapfmt [] = "%p-%p %c%c%c%c %08x %02x:%02x %8d      %s\n";
static char mapfmt2[] = "%p-%p %c%c%c%c %08x %02x:%02x %8d %10u %s\n";

#if 0
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
#endif

static void fdprintmap(int fd, map m) {
  char buf[1000];
  sprintf(buf,mapfmt2,
	 m->from, m->to,
	 m->r, m->w, m->x, m->p,
	 m->offset,
	 m->dev_major, m->dev_minor, m->inode,
	 m->chksum,
	 m->filename
	 );
  write(fd,buf,strlen(buf));
}

static int install(int fd, map m, long *pos) {
  char *start = m->from;
  int len = m->to - m->from;
  int prot = (
		    (m->r == 'r' ? PROT_READ : 0)
		    |
		    (m->w == 'w' ? PROT_WRITE : 0)
		    |
		    (m->x == 'x' ? PROT_EXEC : 0)
		    );
  int offset = *pos;
  int flags = (
	       MAP_FIXED 
	       | 
	       (m->p == 'p' ? MAP_PRIVATE : 0)
	       );
  *pos += len;
#if 0
  printf("start=%p end=%p len=%x prot=%x flags=%x fd=%d offset=%x\n",start,m->to,len,prot,flags,fd,offset);
  return OKAY;
#else
  return MAP_FAILED == mmap(start, len, prot, flags, fd, offset) ? ERROR : OKAY;
#endif
}

static int dumpmap(int fd, map m) {
  return write(fd, m->from, m->to-m->from);
}

static char mapfilename[] = "/proc/self/maps";

#define NDUMPS 20

static int getfile(char *filename, char buf[], int maxlen) {
  int len = 0;
  int fd = open(filename, O_RDONLY);
  if (fd == ERROR) return ERROR;
  maxlen --;			/* for null-termination */
  while (len < maxlen) {
    int r = read(fd,&buf[len],maxlen-len);
    if (r == ERROR) return ERROR;
    if (r == 0) break;
    len += r;
  }
  close(fd);
  buf[len] = 0;
  return len;
}

static int filelen(char *filename) {
  int n;
  for (n = 4096; ; n *= 2) {
    char buf[n];
    int len = getfile(filename,buf,n);
    if (len < n) return len;
  }
}

static int numlines(unsigned char *buf, int buflen) {
  int n = 0, last = '\n';
  for (; *buf && buflen > 0; last=*buf++, buflen--) if (*buf == '\n') n++;
  if (last != '\n') n++;
  return n;
}

static void lines(unsigned char *buf, char *x[]) { /* destructive */
  int n = 0;
  while (*buf) {
    x[n++] = buf;
    for (; *buf; buf++) {
      if (*buf == '\n') { *buf++ = 0; break; }
    }
  }
}

int dumpdata(const char *dumpfilename) {
  struct MAP map, dumpmaps[NDUMPS];
  int ndumps = 0, i;
  char *p;
  int maplen;
  int fd = open(dumpfilename,O_WRONLY|O_CREAT|O_TRUNC,0666);
  long pos, n;
  if (fd == ERROR) {
    fprintf(stderr, "can't open dump data file '%s'\n", dumpfilename);
    return ERROR;
  }
  maplen = filelen(mapfilename);
  if (maplen == ERROR) {
    fprintf(stderr, "dumpdata: can't open %s\n",mapfilename);
    return ERROR;
  }
  {
    char mapbuf[maplen];
    int nlines;
    if (ERROR == getfile(mapfilename,mapbuf,maplen)) {
      fprintf(stderr, "dumpdata: can't read %s\n",mapfilename);
      return ERROR;
    }
    nlines = numlines(mapbuf,maplen);
    {
      int k;
      char *linebuf[nlines];
      char fn[maplen];
      lines(mapbuf,linebuf);
      for (k=0; k<nlines; k++) {
	fn[0] = 0;
	if (0 == sscanf(linebuf[k],mapfmt,
	       &map.from, &p, &map.r, &map.w, &map.x, &map.p,
	       &map.offset, &map.dev_major, &map.dev_minor, &map.inode, 
	       fn)) break;
	map.to = p;			/* work around a gcc bug */
	map.filename = fn;
	map.chksum = map.r == 'r' && map.w == '-' ? checksum((unsigned char *)map.from, map.to - map.from) : 0;
	fdprintmap(fd,&map);
	if (!isDumpable(&map)) continue;
	if (ndumps == NDUMPS) {
	  fprintf(stderr,"can't dump data, too many dumpable memory maps, recompile\n");
	  return ERROR;
	}
	dumpmaps[ndumps++] = map;
      }
      pos = lseek(fd,0,SEEK_END);
      n = ((pos + EXEC_PAGESIZE - 1)/EXEC_PAGESIZE) * EXEC_PAGESIZE - pos;
      {
	char buf[n];
	int i;
	for (i=0; i<n; i++) buf[i] = '\n';
	write(fd,buf,n);
      }
      for (i=0; i<ndumps; i++) {
	if (ERROR == dumpmap(fd,&dumpmaps[i])) {
	  fprintf(stderr,"error dumping data to file '%s', [fd=%d, i=%d]\n", dumpfilename, fd, i);
	  close(fd);
	  return ERROR;
	}
      }
      return close(fd);
    }
  }
}

int loaddata(const char *filename) {
  struct MAP map, fmap, dumpmaps[NDUMPS];
  int i, ndumps=0;
  int fd = open(filename,O_RDONLY);
  int installed_one = FALSE;
  long pos;
  FILE *f;
  char fbuf[1000], fn[1000], ffn[1000], buf[1000];
  char *p;
  FILE *maps;
  if (fd == ERROR) {
    fprintf(stderr, "loaddata: can't open file '%s'\n", filename);
    return ERROR;
  }
  f = fdopen(fd,"r");
  if (f == NULL) { fprintf(stderr, "loaddata: failed to open file from fd %d\n", fd); return ERROR; }
  maps = fopen(mapfilename,"r");
  if (maps == NULL) { fprintf(stderr, "loaddata: failed to open map file %s\n", mapfilename); return ERROR; }
  while (TRUE) {
    int n, maps_end, f_end;
    fbuf[0]=0;
    f_end = NULL == fgets(fbuf,sizeof fbuf,f) || fbuf[0]=='\n';
    if (f_end) break;
    trim(fbuf);

    ffn[0] = 0;
    n = sscanf(fbuf,mapfmt2,
	       &fmap.from, &p, &fmap.r, &fmap.w, &fmap.x, &fmap.p,
	       &fmap.offset, &fmap.dev_major, &fmap.dev_minor, &fmap.inode, &fmap.chksum, ffn);
    if (11 > n) {
      fprintf(stderr,"loaddata: in data file %s: invalid map: %s  [sscanf=%d]\n", filename, buf, n);
      return ERROR;
    }
    fmap.to = p;			/* work around a gcc bug */
    fmap.filename = ffn;

    do {
      buf[0]=0;
      maps_end = NULL == fgets(buf,sizeof buf,maps) || buf[0]=='\n';
      if (maps_end) break;
      trim(buf);
      fn[0] = 0;
      n = sscanf(buf,mapfmt,
		 &map.from, &p, &map.r, &map.w, &map.x, &map.p,
		 &map.offset, &map.dev_major, &map.dev_minor, &map.inode, fn);
      if (10 > n) {
	fprintf(stderr,"loaddata: in data file '%s': invalid map: %s [sscanf=%d]\n",mapfilename, buf, n);
	return ERROR;
      }
      map.to = p;			/* work around a gcc bug */
      map.filename = fn;
      map.chksum = map.r == 'r' && map.w == '-' ? checksum((unsigned char *)map.from, map.to - map.from) : 0;
    } while (fmap.from - map.from > 0);

    if (!f_end && fmap.from == map.from) {
      if (fmap.r != map.r || fmap.w != map.w || fmap.x != map.x || fmap.p != map.p) {
	fprintf(stderr, "loaddata: map protection has changed.\n  from: %s\n    to: %s\n",fbuf,buf);
	return ERROR;
      }
      if (fmap.chksum != map.chksum) { 
	fprintf(stderr, "loaddata: map checksum has changed.\n  from: %s\n    to: %s\n",fbuf,buf);
	return ERROR;
      }
      if (0 != strcmp(fmap.filename,map.filename)) { 
	fprintf(stderr, "loaddata: map filename has changed.\n  from: %s\n    to: %s\n",fbuf,buf);
	return ERROR;
      }
    }
    if (!isDumpable(&fmap)) continue;
    if (ndumps == NDUMPS) {
      fprintf(stderr,"loaddata: too many dumpable memory maps, recompile\n");
      return ERROR;
    }
    dumpmaps[ndumps++] = fmap;
#if 0
    printmap(stdout,&fmap);
#endif
  }
  fclose(maps);
  pos = ftell(f);
  pos = ((pos + EXEC_PAGESIZE - 1)/EXEC_PAGESIZE) * EXEC_PAGESIZE;
  for (i=0; i<ndumps; i++) {
    if (ERROR == install(fd,&dumpmaps[i],&pos)) {
      if (installed_one) {
	char *p = "loaddata: failed to map memory, aborting\n";
	write(STDERR,p,strlen(p));
	abort();
      }
      else {
	fprintf(stderr,"loaddata: failed to map memory\n");
	fclose(maps);
	fclose(f);
	return ERROR;
      }
    }
    installed_one = TRUE;
  }
  close(fd);
  return OKAY;
}
