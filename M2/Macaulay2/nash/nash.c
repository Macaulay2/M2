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
#define TRUE 1
#define FALSE 0

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
    
static void usage() {
  fprintf(stderr,"usage: %s -c \"exec command arg1 arg2 ...\"\n",argv[0]);
  exit(1);
}

int main (int _argc, char **_argv) {
  argc = _argc;
  argv = _argv;
  if (argc != 3 || 0 != strcmp("-c",argv[1]) || 0 != strncmp("exec ",argv[2],5)) usage();
  parse(argv[2] + 5);
  if (NULL != getenv("OLDSHELL")) {
    setenv("SHELL",getenv("OLDSHELL"),TRUE);
    unsetenv("OLDSHELL");
  }
  execvp(xargv[0],xargv);
  fprintf(stderr,"%s: failed to exec \"%s\"\n", argv[0], xargv[0]);
  return 1;
}
