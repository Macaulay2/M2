#include <stdio.h>
#include <gc.h>
#include "dumpdata.h"

static int isStack(map m) {
  char dummy;
  void *p = (void *)&dummy;
  return p - m->from >= 0 && m->to - p > 0;
}

static int isDumpable(map m) {
  return m->w == 'w' && !isStack(m);
}

static void printmap(map m) {
  printf("%08x-%08x %c%c%c%c %08x %02x:%02x %8d %s%s%s\n",
	 (int)m->from, (int)m->to,
	 m->r, m->w, m->x, m->p,
	 m->offset,
	 m->dev_major, m->dev_minor, m->inode,
	 m->filename,
	 isStack(m) ? "(stack)" : "",
	 isDumpable(m) ? "*" : ""
	 );
}

void printmaps(map *m) {
  if (m) for (; *m; m++) printmap(*m);
}

static char *strperm(char *s) {
  unsigned int n = strlen(s);
  char *t = GC_MALLOC_ATOMIC(n+1);
  if (t == NULL) {
    fprintf(stderr,"out of memory");
    exit(1);
  }
  strcpy(t,s);
  return t;
}

map *getmaps() {
  static char *filename = "/proc/self/maps";
  FILE *maps = fopen(filename,"r");
  if (maps == NULL) {
    fprintf(stderr,"can't open %s\n",filename);
    exit(1);
  }
  while (TRUE) {
    struct MAP n;
    char fn[1000], buf[1000];
    if (NULL == fgets(buf,sizeof buf,maps)) break;
    fn[0] = 0;
    if (0 == sscanf(buf,"%x-%x %c%c%c%c %x %x:%x %d %1000s",
	   (int *)&n.from, (int *)&n.to, &n.r, &n.w, &n.x, &n.p,
	   &n.offset, &n.dev_major, &n.dev_minor, &n.inode, 
	   fn)) break;
    n.filename = strperm(fn);
    printmap(&n);
  }
  return 0;
}
