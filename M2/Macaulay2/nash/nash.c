/*
 nash = "naive shell"
 a replacement for bash designed just to do commands like this:
	/bin/nash -c "exec command arg1 arg2 ..."
 Hopefully we can use it to make gdb more transparent.
*/

#include <unistd.h>
#include <stdio.h>
#include <string.h>

int argc;
char **argv, *args;

usage() {
  fprintf(stderr,"usage: %s -c \"exec command arg1 arg2 ...\"\n",argv[0]);
  exit(1);
}

int main (int _argc, char **_argv) {
  argc = _argc;
  argv = _argv;
  if (argc != 2 || 0 != strcmp("-c",argv[1]) || 0 != strncmp("exec ",argv[2],5)) usage();
  args = &argv[2] + 5;
}
