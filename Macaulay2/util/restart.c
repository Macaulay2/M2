#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <sys/stat.h>
#include <unistd.h>
#include "restart.h"

char *argv0;

void usage() {
     fprintf(stderr,"usage: %s filename command\n",argv0);
     fprintf(stderr,"       Executes command repeatedly as long as its return\n"
	            "       code is %d, or a file with the given name has been\n"
		    "       created.\n", RESTART);
     exit(1);
     }

main(int argc,char **argv) {
     int i,n;
     char *filename, *command;
     struct stat buf;
     argv0 = argv[0], argc--, argv++;
     if (argc == 0) usage();
     filename = argv[0], argc--, argv++;
     if (argc == 0) usage();
     for (i=0, n=0; i<argc; i++) n += strlen(argv[i]) + 1;
     command = (char *)malloc(n+1);
     *command = 0;
     for (i=0;;) {
	  strcat(command,argv[i]);
	  if (++i >= argc) break;
	  strcat(command," ");
	  }
     while (1) {
	  int ret;
	  puts(command);
	  ret = system(command);
	  if (ret >= 256) ret >>= 8;
	  if (0 == stat(filename,&buf)) {
	       unlink(filename);
	       continue;
	       }
	  if (ret == RESTART) continue;
	  exit(ret);
	  }
     }
