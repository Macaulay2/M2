/* file utilities that don't use the heap or fstat or static memory */

/* /proc/self/maps does not reveal its length with fstat - you just
   have to read it to find out.
*/

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "std.h"
#include "file.h"

int getfile(const char *filename, int buflen, char buf[buflen]) {
  int len = 0;
  int fd = open(filename, O_RDONLY);
  if (fd == ERROR) return ERROR;
  while (len < buflen) {
    int r = read(fd,&buf[len],buflen-len);
    if (r == ERROR) return ERROR;
    if (r == 0) break;
    len += r;
  }
  close(fd);
  return len;
}

int filelen(char const *filename) {
  int n;
  for (n = 4096; ; n *= 2) {
    char buf[n];
    int len = getfile(filename,n,buf);
    if (len == ERROR) return ERROR;
    if (len < n) return len;
  }
}

int numlines(int len, char buf[len]) {
  int i, nlines = 0;
  for (i=0; i<len; i++) if (buf[i] == '\n') nlines++;
  return nlines;
}

int fnumlines(char const *filename) {
  int len = filelen(filename);
  if (len == ERROR) return ERROR;
  else {
    char buf[len];
    getfile(filename,sizeof buf,buf);
    return numlines(sizeof buf,buf);
  }
}

void lines(int len, char buf[len], int nlines, char *line[nlines]) {
  int i, n;
  for (n=i=0; i<len && n<nlines; ) {
    char *bol = &buf[i];
    for (; i<len; i++) if (buf[i] == '\n') { line[n++] = bol; i++; break; }
  }
}

int linelen(char *p) {
  int n=0;
  while (*p && *p != '\n') n++, p++;
  return n;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/dumpdata "
 End:
*/
