/*
 nash = "naive shell"
 a replacement for bash designed just to do commands like this:
	/bin/nash -c "exec command arg1 arg2 ..."
 Hopefully we can use it to make gdb more transparent.

 No globbing, for now.

*/

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "env.h"

#define TRUE 1
#define FALSE 0
#define ERROR (-1)
#define STDIN 0
#define STDOUT 1
#define STDERR 2

static int argc, xargc;
static char **argv, **xargv;

static void *getmem(int n) {
  char *p = malloc(n);
  if (p == NULL) {
    fprintf(stderr,"%s: out of memory\n",argv[0]);
    exit(1);
  }
  return p;
}

static void parse(char *args) {
  char *p = args;
  int quote = FALSE, dquote = FALSE, inarg = FALSE, inc = 1;
  xargc = 0;
  for (;*p;p++) {
    switch (*p) {
    case ' ': case '\t': if (inarg && !dquote && !quote) inarg = FALSE; break;
    case '"': if (!quote) dquote = !dquote; goto x;
    case '\\': if (p[1] != 0) p++; goto x;
    case '\'': if (!dquote) quote = !quote; goto x;
    default: 
      x: if (!inarg) {inarg = TRUE; xargc++;} break;
    }
  }
  xargv = (char **)getmem((xargc+1)*sizeof *xargv);
  quote = FALSE, dquote = FALSE, inarg = FALSE;
  p = args;
  xargc = 0;
  for (;*p;p+=inc,inc=1) {
    switch (*p) {
      case ' ': case '\t':
	if (inarg && !dquote && !quote) {
	  inarg = FALSE;
	  *p = 0;
	  }
	break;
      case '"': 
	if (!quote) {
	  dquote = !dquote;
	  goto z;
	}
	goto y;
      case '\\': 
	if (p[1] != 0) {
	  strcpy(p,p+1);
	  goto y;
	}
	goto y;
      case '\'': 
	if (!dquote) {
	  quote = !quote; 
	  goto z;
	}
	goto y;
      z:
	strcpy(p,p+1);
	inc = 0;
      default: 
      y:
	if (!inarg) {
	  inarg = TRUE;
	  xargv[xargc] = p;
	  xargc++;
	}
    }
  }
  if (dquote || quote) {
    fprintf(stderr,"%s: mismatched quotes\n",argv[0]);
    exit(1);
  }
}  

static void argcpy(char **dst,char **src) {
  do *dst++ = *src; while (*src++);
}
    
static void redirect(){
  char **av = xargv;
  int inc=1, fd;
  for (;*av;av+=inc,inc=1) {
    char  *fn, *arg = *av;
    switch (arg[0]) {
    case '<':
      fn = arg+1;
      fd = open(fn,O_RDONLY);
      if (fd == ERROR) {
	fprintf(stderr,"%s: failed to open \"%s\" for reading\n",argv[0],fn);
	exit(1);
      }
      if (fd != STDIN) {
	dup2(fd,STDIN);
	close(fd);
      }
      argcpy(av,av+1);
      inc = 0;
      break;
    case '>':
      fn = arg+1;
      fd = open(fn,O_CREAT|O_WRONLY,0666);
      if (fd == ERROR) {
	fprintf(stderr,"%s: failed to open \"%s\" for writing\n",argv[0],fn);
	exit(1);
      }
      if (fd != STDOUT) {
	dup2(fd,STDOUT);
	close(fd);
      }
      argcpy(av,av+1);
      inc = 0;
      break;
    }
  }
}

static void usage() {
  fprintf(stderr,"usage: %s -c \"exec command arg1 arg2 ...\"\n",argv[0]);
  fprintf(stderr,"environment variables:\n");
  fprintf(stderr,"    OLDSHELL=foo   set SHELL to foo and erase OLDSHELL\n");
  fprintf(stderr,"    OLDARG0=foo    set argv[0] to foo and erase OLDARG0\n");
  exit(1);
}

int main (int _argc, char **_argv) {
  argc = _argc;
  argv = _argv;
  if (argc != 3 || 0 != strcmp("-c",argv[1]) || 0 != strncmp("exec ",argv[2],5)) usage();
  parse(argv[2] + 5);
  redirect();
  if (NULL != getenv("OLDSHELL")) {
    setenv("SHELL",getenv("OLDSHELL"),TRUE);
    unsetenv("OLDSHELL");
  }
  if (NULL != getenv("OLDARG0")) {
    xargv[0] = getenv("OLDARG0");
    unsetenv("OLDARG0");
  }
  /* fprintf(stderr,"%s: executing \"%s\"\n", argv[0], xargv[0]); */
  execvp(xargv[0],xargv);
  fprintf(stderr,"%s: failed to exec \"%s\"\n", argv[0], xargv[0]);
  return 1;
}
