/*		Copyright 1999 by Daniel R. Grayson		*/

#include <string.h>
#include <stddef.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#ifndef MAP_FAILED
#define MAP_FAILED	((__ptr_t) -1)
#endif
#include "dumpdata.h"
#include "map.h"
#include "warning.h"
#include "std.h"

#define DRYRUN 0

#if !defined(PAGESIZE)
#if defined(EXEC_PAGESIZE)
#define PAGESIZE EXEC_PAGESIZE
#else
#define PAGESIZE 4096
#endif
#endif

static char mapfmt[] = "%p-%p %c%c%c %u\n";

static int isStack(map m) {
  void *p = &p;
  return p - m->from >= 0 && m->to - p > 0;
}

static int isDumpable(map m) {
  return m->w && m->r && !isStack(m);
}
  
static unsigned int checksum(unsigned char *p, unsigned int len) {
  unsigned int c = 0;
  while (0 < len--) c = 23 * c + *p++;
  return c;
}

static int isCheckable(map m) {
  return !m->w && m->r && !isStack(m);
}

static void checkmap(map m) {
  m->checksum = isCheckable(m) ? checksum(m->from, m->to - m->from) : 0;
}

static void checkmaps(int nmaps, struct MAP m[nmaps]) {
  int i;
  for (i=0; i<nmaps; i++) {
    checkmap(&m[i]);
  }
}

static void sprintmap(char *s, map m) {
  sprintf(s,mapfmt, 
	  m->from, m->to,
	  m->r ? 'r' : '-', m->w ? 'w' : '-', m->x ? 'x' : '-',
	  m->checksum);
}

static void fdprintmap(int fd, map m) {
  char buf[200];
  sprintmap(buf,m);
  write(fd,buf,strlen(buf));
}

static void trim(char *s) {
  if (s == NULL) return;
  if (s[0] == 0) return;
  while (s[1]) s++;
  if (s[0] == '\n') s[0] = 0;
}

static int extend_memory(void *newbreak) {
  if (ERROR == brk(newbreak)) {
    warning("loaddata: out of memory (extending break to 0x%p)", newbreak);
    return ERROR;
  }
  else return OKAY;
}

static int install(int fd, map m, long *pos) {
  void *start = m->from, *finish = m->to;
  int len = finish - start;
  void *ret;
  int prot = (m->r ? PROT_READ : 0) | (m->w ? PROT_WRITE : 0) | (m->x ? PROT_EXEC : 0) ;
  int offset = *pos;
  int flags = MAP_FIXED | MAP_PRIVATE;
  *pos += len;
#if DRYRUN
  printf("start=%p end=%p len=%x prot=%x flags=%x fd=%d offset=%x\n",
	 start,start+len,len,prot,flags,fd,offset);
  return OKAY;
#else
  ret = mmap(start, len, prot, flags, fd, offset);
  return (char *)MAP_FAILED == ret ? ERROR : OKAY;
#endif
}

static int dumpmap(int fd, map m) {
  return write(fd, m->from, m->to-m->from);
}

static char brkfmt[] = "sbrk(0)=%p\n";

static int fdprintbrk(int fd) {
  char buf[80];
  sprintf(buf,brkfmt,sbrk(0));
  return write(fd,buf,strlen(buf));
}

int dumpdata(char const *dumpfilename) {
  long pos, n;
  int nmaps = nummaps();
  struct MAP dumpmaps[nmaps];
  int i;
  int fd = open(dumpfilename,O_WRONLY|O_CREAT|O_TRUNC,0666);
  if (fd == ERROR) {
    warning("can't open dump data file '%s'\n", dumpfilename);
    return ERROR;
  }
  if (ERROR == fdprintbrk(fd)) return ERROR;
  if (ERROR == getmaps(nmaps,dumpmaps)) return ERROR;
  checkmaps(nmaps,dumpmaps);
  for (i=0; i<nmaps; i++) fdprintmap(fd,&dumpmaps[i]);
  write(fd,"\n",1);
  pos = lseek(fd,0,SEEK_END);
  n = ((pos + PAGESIZE - 1)/PAGESIZE) * PAGESIZE - pos;
  {
    char buf[n];
    int k;
    for (k=0; k<n; k++) buf[k] = '\n';
    write(fd,buf,n);
  }
  for (i=0; i<nmaps; i++) {
    if (isDumpable(&dumpmaps[i]) && ERROR == dumpmap(fd,&dumpmaps[i])) {
      warning("warning dumping data to file '%s', [fd=%d, i=%d]\n", dumpfilename, fd, i);
      close(fd);
      return ERROR;
    }
  }
  return close(fd);
}

int loaddata(char const *filename) {
  void *newbreak;
  int got_newbreak = 0;
  int nmaps = nummaps();
  struct MAP dumpedmap, currmap[nmaps], dumpmaps[80];
  int i, ndumps=0, j=0;
  int fd = open(filename,O_RDONLY);
  int installed_one = FALSE;
  FILE *f = fdopen(fd,"r");
  if (ERROR == getmaps(nmaps,currmap)) return ERROR;
  checkmaps(nmaps,currmap);
  if (fd == ERROR || f == NULL) { warning("loaddata: can't open file '%s'\n", filename); return ERROR; }
  while (TRUE) {
    char fbuf[200];
    int n=0, f_end, ret;
    char r, w, x;
    fbuf[0]=0;
    f_end = NULL == fgets(fbuf,sizeof fbuf,f) || fbuf[0]=='\n';
    if (!got_newbreak) {
      if (0 == sscanf(fbuf,brkfmt,&newbreak)) {
	warning("loaddata: in data file %s: expected sbrk(0) line: \n", filename, fbuf);
	fclose(f);
	return ERROR;
      }
      got_newbreak = TRUE;
      continue;
    }
    if (f_end) break;
    trim(fbuf);
    n++;
    ret = sscanf(fbuf, mapfmt, &dumpedmap.from, &dumpedmap.to, &r, &w, &x, &dumpedmap.checksum);
    if (6 != ret) {
      warning("loaddata: in data file %s: invalid map: %s\n", filename, fbuf, n);
      fclose(f);
      return ERROR;
    }
    dumpedmap.r = r == 'r';
    dumpedmap.w = w == 'w';
    dumpedmap.x = x == 'x';
    for (; j<nmaps; j++) {
      if ((uintP)dumpedmap.from <= (uintP)currmap[j].from) break;
      if (isCheckable(&currmap[j])) {
	char buf[100];
	sprintmap(buf,&currmap[j]);
	warning("loaddata: map has appeared or changed its location: %s\n",buf);
	fclose(f);
	return ERROR;
      }
    };

    if (!f_end && !dumpedmap.w && (uintP)dumpedmap.from < (uintP)currmap[j].from) {
      warning("loaddata: map has disappeared or changed its location:\n  %s\n", fbuf);
      fclose(f);
      return ERROR;
    }

    if (!f_end && dumpedmap.from == currmap[j].from) {
      if (dumpedmap.r != currmap[j].r || dumpedmap.w != currmap[j].w || dumpedmap.x != currmap[j].x) {
	char buf[100];
	sprintmap(buf,&currmap[j]);
	warning("loaddata: map protection has changed.\n  from: %s\n    to: %s\n",fbuf,buf);
	fclose(f);
	return ERROR;
      }
      if (dumpedmap.checksum != currmap[j].checksum) {
	warning("loaddata: checksum for map has changed from %u to %u\n",
		dumpedmap.checksum, currmap[j].checksum);
	if (getenv("LOADDATA_IGNORE_CHECKSUMS") == NULL) {
	  fclose(f);
	  return ERROR;
	}
      }
      j++;
    }

    if (!isDumpable(&dumpedmap)) continue;
    if (ndumps == numberof(dumpmaps)) {
      warning("too many maps dumped, recompile\n");
      fclose(f);
      return ERROR;
    }
    else dumpmaps[ndumps++] = dumpedmap;
  }
#if 0
  for (i=0; i<ndumps; i++) {
    fdprintmap(STDOUT,&dumpmaps[i]);
  }
#endif
  {
    long pos = ftell(f);
    /* now we must stop using static memory and the heap! */
    pos = ((pos + PAGESIZE - 1)/PAGESIZE) * PAGESIZE;
    if (ERROR == extend_memory(newbreak)) return ERROR;
    for (i=0; i<ndumps; i++) {
      if (ERROR == install(fd,&dumpmaps[i],&pos)) {
        if (installed_one) {
          char x[] = "loaddata: failed to map memory completely\n";
          write(STDERR,x,strlen(x));
          _exit(1);
        }
        else {
          char x[] = "loaddata: failed to map any memory\n";
          write(STDERR,x,strlen(x));
          return ERROR;
        }
      }
      else installed_one = TRUE;
    }
  }
  close(fd);
  return OKAY;
}
