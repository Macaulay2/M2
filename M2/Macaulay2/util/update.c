#if 1
#include <unistd.h>
#endif
#include <errno.h>
#include <sys/types.h>

#ifdef _WIN32
#include <io.h>
#endif

#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "restart.h"

#define ERROR (-1)
#define FALSE 0
#define TRUE 1

static char *getmem(n)
unsigned int n;
{
     char *p = malloc(n);
     if (p == NULL) {
	  fprintf(stderr,"out of memory\n");
	  exit(1);
	  }
     return p;
     }

char *readfile(int fd, int *len)
{
     char *text;
     unsigned int size = 0, bufsize = 4096;
     text = getmem(bufsize);
     while (TRUE) {
	  int n = read(fd,text+size,bufsize-size);
	  if (ERROR == n) {
	       if (errno == EINTR) continue;
	       fprintf(stderr,"can't read file descriptor %d", fd);
	       exit(1);
	       }
	  if (0 == n) break;
	  size += n;
	  if (size == bufsize) {
	       char *p;
	       int newbufsize = 2 * bufsize;
	       p = getmem(newbufsize);
	       memcpy(p,text,size);
	       free(text);
	       bufsize = newbufsize;
	       text = p;
	       }
	  }
     *len = size;
     return text;
     }

int equal(char *indata, int insize, char *outdata, int outsize) {
     int i;
     char *p, *q;
     if (insize != outsize) return FALSE;
     for (p=indata, q=outdata, i=insize; i>0; i--, p++, q++) {
	  if (*p != *q) return FALSE;
	  }
     return TRUE;
     }

int main(int argc, char **argv) {
     int in, out;
     int insize, outsize;
     char *indata, *outdata;
     if (argc != 3) {
	  fprintf(stderr,
	       "usage: %s sourcefile targetfile\n"
	       "       Copies file if different from target and then exits with %d.\n"
	       "       In that case it also creates a file 'restart.tmp'.\n",
	       argv[0],RESTART);
	  exit(1);
	  }
     in = open(argv[1],O_RDONLY);
     if (in == ERROR) {
	  fprintf(stderr,"%s: can't open file %s\n", argv[0], argv[1]);
	  exit(1);
	  }
     out = open(argv[2],O_RDONLY);
     if (out == ERROR) {
		 close(in);
		 if (0 != rename(argv[1],argv[2])) {
	       fprintf(stderr, "%s: can't rename %s to %s\n",
		       argv[0],argv[1],argv[2]);
	       exit(1);
		 }
	  exit(0);
	  }
     indata = readfile(in,&insize);
     close(in);
     outdata = readfile(out,&outsize);
     close(out);
     if (! equal(indata, insize, outdata, outsize)) {
	  if (0 != unlink(argv[2])) {
	       fprintf(stderr, "%s: can't unlink %s\n",argv[0],argv[2]);
	       exit(1);
	  }
	  if (0 != rename(argv[1],argv[2])) {
	       fprintf(stderr, "%s: can't rename %s to %s\n",
		       argv[0],argv[1],argv[2]);
	       exit(1);
	  }
	  fprintf(stderr, "%s: updating %s.  (Restart make.)\n", 
	       argv[0], argv[2]);
	  {
	       FILE *g = fopen("restart.tmp", "w");
	       if (g == NULL) {
		    fprintf(stderr, "%s: can't create 'restart.tmp'\n", argv[0]);
		    exit(1);
		    }
	       }
	  exit(RESTART);
	  }
     unlink(argv[1]);
     exit(0);
	 return 0;
     }
