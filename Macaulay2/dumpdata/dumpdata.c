/*		Copyright 1999 by Daniel R. Grayson		*/

#include <string.h>
#include <stddef.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
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
#include "maputil.h"

extern bool notify;

#define DRYRUN 0

#if !defined(PAGESIZE)
#if defined(EXEC_PAGESIZE)
#define PAGESIZE EXEC_PAGESIZE
#else
#define PAGESIZE 4096
#endif
#endif

static void trim(char *s) {
  if (s == NULL) return;
  if (s[0] == 0) return;
  while (s[1]) s++;
  if (s[0] == '\n') s[0] = 0;
}

static int extend_memory(void *newbreak) {
  if (ERROR == (int)brk(newbreak)) {
    warning("--warning: loaddata: out of memory (extending break to %p)\n", newbreak);
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
  snprintf(buf,sizeof(buf),brkfmt,sbrk(0));
  return write(fd,buf,strlen(buf));
}

int dumpdata(char const *dumpfilename) {
  long pos, n;
  int i;
  int fd;
  if (!haveDumpdata()) return ERROR;
  fd = open(dumpfilename,O_WRONLY|O_CREAT|O_TRUNC,0666);
  if (fd == ERROR) {
    warning("--warning: can't open dump data file '%s'\n", dumpfilename);
    return ERROR;
  }
  if (ERROR == fdprintbrk(fd)) return ERROR;
  {
    int nmaps = nummaps();
    struct MAP dumpmaps[nmaps];
    if (ERROR == getmaps(nmaps,dumpmaps)) return ERROR;
    checkmaps(nmaps,dumpmaps);
    for (i=0; i<nmaps; i++) fdprintmap(fd,&dumpmaps[i]);
    if (getenv("LOADDATA_DEBUG")) {
	 for (i=0; i<nmaps; i++) fdprintmap(STDERR,&dumpmaps[i]);
    }
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
	warning("--warning: error occurred while dumping data to file '%s', [fd=%d, i=%d]\n", dumpfilename, fd, i);
	close(fd);
	return ERROR;
      }
    }
    return close(fd);
  }
}

int loaddata(char const *filename) {
  void *newbreak;
  int got_newbreak = 0;
  int nmaps = nummaps();
  int debug = NULL != getenv("LOADDATA_DEBUG");
  int haderror = FALSE;
  struct MAP dumpedmap, currmap[nmaps], dumpmaps[80];
  int i, ndumps=0, j=0;
  int fd = open(filename,O_RDONLY);
  int installed_one = FALSE;
  void *previousTopOfStack, *currentTopOfStack;	/* we assume stacks grow downward, not always correct */
  int fd2 = dup(fd);
  FILE *f = fdopen(fd2,"r");
  if (ERROR == getmaps(nmaps,currmap)) return ERROR;
  checkmaps(nmaps,currmap);
  if (fd == ERROR || f == NULL) { warning("--warning: loaddata: can't open file '%s'\n", filename); return ERROR; }
  while (TRUE) {
    char fbuf[200];
    int n=0, f_end, ret;
    char r, w, x, S;
    fbuf[0]=0;
    f_end = NULL == fgets(fbuf,sizeof fbuf,f) || fbuf[0]=='\n';
    if (!got_newbreak) {
      if (0 == sscanf(fbuf,brkfmt,&newbreak)) {
	warning("--warning: loaddata: in data file %s: expected sbrk(0) line: \n", filename, fbuf);
	fclose(f);
	close(fd);
	return ERROR;
      }
      got_newbreak = TRUE;
      continue;
    }
    if (f_end) break;
    trim(fbuf);
    n++;
    ret = sscanf(fbuf, mapfmt, &dumpedmap.from, &dumpedmap.to, &r, &w, &x, &dumpedmap.checksum, &S);
    if (7 != ret) {
      warning("--warning: loaddata: in data file %s: invalid map %d: %s\n", filename, n, fbuf);
      warning("         : map format \"%s\" matched only %d item(s)\n", mapfmt,ret);
      if (ret >= 1) warning("         : from : %p\n", dumpedmap.from);
      if (ret >= 2) warning("         : to   : %p\n", dumpedmap.to);
      if (ret >= 3) warning("         : r    : %c\n", r);
      if (ret >= 4) warning("         : w    : %c\n", w);
      if (ret >= 5) warning("         : x    : %c\n", x);
      if (ret >= 6) warning("         : chk  : %u\n", dumpedmap.checksum);
      if (ret >= 7) warning("         : stk  : %c\n", S);
      fclose(f);
      close(fd);
      return ERROR;
    }
    dumpedmap.r = r == 'r';
    dumpedmap.w = w == 'w';
    dumpedmap.x = x == 'x';
    dumpedmap.isStack = S == 'S';
    if (dumpedmap.isStack) previousTopOfStack = dumpedmap.to;
    for (; j<nmaps; j++) {
      if ((uintP)dumpedmap.from <= (uintP)currmap[j].from) break;
      if (isCheckable(&currmap[j])) {
	char buf[100];
	sprintmap(buf,sizeof(buf),&currmap[j]);
	trim(buf);
	if (debug) warning("--warning: loaddata: map %d has appeared or changed its location: %s\n",j,buf);
	haderror = TRUE;
      }
      if (getenv("LOADDATA_DEBUG")) fprintf(stderr,"--loaddata: current map: "), fdprintmap(STDERR,&currmap[j]);
      if (currmap[j].isStack) currentTopOfStack = currmap[j].to;
    };

    if (getenv("LOADDATA_DEBUG")) fprintf(stderr,"--loaddata:  dumped map: "), fdprintmap(STDERR,&dumpedmap);

    if ((uintP)dumpedmap.from < (uintP)currmap[j].from) {
      if (getenv("LOADDATA_DEBUG")) fprintf(stderr,"--loaddata: current map: "), fdprintmap(STDERR,&currmap[j]);
      if (currmap[j].isStack) currentTopOfStack = currmap[j].to;
      if (debug) warning("--warning: loaddata: map has disappeared or changed its location: %s\n",fbuf);
      if (dumpedmap.isStack) {
	   if (debug) warning("--       : loaddata: map is part of stack, ignoring change: %s\n",fbuf);
      }
      else haderror = TRUE;
    }

    if (dumpedmap.from == currmap[j].from) {
      if (getenv("LOADDATA_DEBUG")) fprintf(stderr,"--loaddata: current map: "), fdprintmap(STDERR,&currmap[j]), fprintf(stderr,"\n");
      if (currmap[j].isStack) currentTopOfStack = currmap[j].to;

      if (dumpedmap.r != currmap[j].r || dumpedmap.w != currmap[j].w || dumpedmap.x != currmap[j].x) {
	char buf[100];
	sprintmap(buf,sizeof(buf),&currmap[j]);
	trim(buf);
	if (debug) warning("--warning: loaddata: map %d protection has changed.\n--   from: %s\n--     to: %s\n",j,fbuf,buf);
	if (dumpedmap.r == currmap[j].r && dumpedmap.w == currmap[j].w) {
	     if (debug) warning("--       : loaddata: ignoring map protection executability change\n",j,fbuf,buf);
	}
	else haderror = TRUE;
      }

      /* at least one map seems to change its size */
      /* when we reload, we'll just put it to its former size, usually *larger* */
      if (dumpedmap.to != currmap[j].to && !isStatic(&dumpedmap)) {
#       if 0
	char buf[100];
	sprintmap(buf,sizeof(buf),&currmap[j]);
	trim(buf);
	if (debug) warning("--warning: loaddata: map %d has changed its size.\n--   from: %s\n--     to: %s\n",j,fbuf,buf);
	fclose(f);
	close(fd);
	return ERROR;
#       endif
      }

      if (dumpedmap.checksum != currmap[j].checksum) {
	char buf[100];
	sprintmap(buf,sizeof(buf),&currmap[j]);
	trim(buf);
	if (debug) warning("--warning: map %d checksum has changed\n",j);
	if (getenv("LOADDATA_IGNORE_CHECKSUMS") == NULL) {
	  haderror = TRUE;
	}
	else if (debug) warning("--warning: ... ignoring checksum change\n");
      }
      j++;
    }

    if (!isDumpable(&dumpedmap)) continue;
    if (ndumps == numberof(dumpmaps)) {
      warning("--warning: too many maps dumped, recompile\n");
      fclose(f);
      close(fd);
      return ERROR;
    }
    else dumpmaps[ndumps++] = dumpedmap;
  }

  for (;j<nmaps;j++) {
       if (getenv("LOADDATA_DEBUG")) fprintf(stderr,"--loaddata: current map: "), fdprintmap(STDERR,&currmap[j]);
       if (currmap[j].isStack) currentTopOfStack = currmap[j].to;
  }

  if (currentTopOfStack != previousTopOfStack) {
       if (debug) warning("--warning: loaddata: top of stack has changed from %p to %p\n",previousTopOfStack,currentTopOfStack);
#if 0
       haderror = TRUE;
#else
       /* we make do by having gc recompute the stack base after loading data */
       if (debug) warning("--       : loaddata: ignoring stack address change\n",previousTopOfStack,currentTopOfStack);
#endif
  }

  if (haderror) {
       fclose(f);
       close(fd);
       if (notify) fprintf(stderr,"--memory maps have changed, can'n load cached data\n");
       return ERROR;
  }

  {
    long pos = ftell(f);
    fclose(f);
    /* now we must stop using static memory and the heap! */
    pos = ((pos + PAGESIZE - 1)/PAGESIZE) * PAGESIZE;
    if (ERROR == extend_memory(newbreak)) return ERROR;
    for (i=0; i<ndumps; i++) {
      if (ERROR == install(fd,&dumpmaps[i],&pos)) {
        if (installed_one) {
          char x[] = "--error: loaddata: failed to map memory completely\n";
          write(STDERR,x,strlen(x));
          _exit(1);
        }
        else {
	  char buf[100];
	  sprintmap(buf,sizeof(buf),&currmap[i]);
	  trim(buf);
	  fprintf(stderr,"--error: loaddata: failed to map any memory: %s: %s\n",buf,strerror(errno));
          return ERROR;
        }
      }
      else installed_one = TRUE;
    }
  }
  close(fd);
  return OKAY;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/dumpdata "
 End:
*/
