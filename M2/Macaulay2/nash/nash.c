/*
 nash = "naive shell"
 a replacement for bash designed just to do commands like this:
	/bin/nash -c "exec command arg1 arg2 ..."
 Hopefully we can use it to make gdb more transparent.
*/

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#define TRUE 1
#define FALSE 0

int argc, xargc;
char **argv, **xargv;

void parse(char *args) {
  xargc = 1;
  xargv = malloc((xargc + 1) * sizeof (*xargv));
  if (xargv == NULL) {
    fprintf(stderr,"%s: out of memory\n",argv[0]);
    exit(1);
  }
  xargv[0] = args;
  xargv[xargc] = NULL;
}

void usage() {
  fprintf(stderr,"usage: %s -c \"exec command arg1 arg2 ...\"\n",argv[0]);
  exit(1);
}

int main (int _argc, char **_argv) {
  argc = _argc;
  argv = _argv;
  if (argc != 3 || 0 != strcmp("-c",argv[1]) || 0 != strncmp("exec ",argv[2],5)) usage();
  parse(argv[2] + 5);
  execvp(xargv[0],xargv);
  fprintf(stderr,"%s: failed to exec \"%s\"\n", argv[0], xargv[0]);
  return 1;
}
