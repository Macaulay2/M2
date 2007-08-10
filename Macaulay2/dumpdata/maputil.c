#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "maputil.h"
  
static unsigned int checksum(unsigned char *p, unsigned int len) {
  unsigned int c = 0;
  while (0 < len--) c = 23 * c + *p++;
  return c;
}

int isStack(map m) {
     return m->isStack;
}

int isCheckable(map m) {
  return !m->w && m->r && !isNotCheckable(m);
}

static void checkmap(map m) {
  m->checksum = isCheckable(m) ? checksum(m->from, m->to - m->from) : 0;
}

void checkmaps(int nmaps, struct MAP m[nmaps]) {
  void *p = &p;
  int i, j;
  for (i=0; i<nmaps; i++) m[i].isStack = 0;
  for (i=0; i<nmaps; i++) checkmap(&m[i]);
  for (i=0; i<nmaps; i++) {
       if (p - m[i].from >= 0 && m[i].to - p > 0) {
	    m[i].isStack = 1;
	    for (j = i-1; j >= 0    && m[j].to   == m[j+1].from && m[j].w; j--) m[j].isStack = 1;
	    for (j = i+1; j < nmaps && m[j].from == m[j-1].to   && m[j].w; j++) m[j].isStack = 1;
	    break;
       }
  }
}

char mapfmt[] = "%p-%p %c%c%c %u %c\n";

int isStatic(map m) {
  static char p0[] = "h";
  static char q0[1];
  void *p = (void *)(p0), *q = (void *)(q0);
  return p - m->from >= 0 && m->to - p > 0 || q - m->from >= 0 && m->to - q > 0;
}

int isDumpable(map m) {
     return m->w && m->r && !isStack(m);
}

void sprintmap(char *s, unsigned size, map m) {
  snprintf(s,size,mapfmt, 
	  m->from, m->to,
	  m->r ? 'r' : '-', m->w ? 'w' : '-', m->x ? 'x' : '-',
	  m->checksum,
	  m->isStack ? 'S' : 'M');
}

void fdprintmap(int fd, map m) {
  char buf[200];
  sprintmap(buf,sizeof(buf),m);
  write(fd,buf,strlen(buf));
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/dumpdata "
 End:
*/
