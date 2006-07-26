#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include "restart.h"

char *progname;

void usage() {
     fprintf(stderr,"usage: %s filename command\n",progname);
     fprintf(stderr,"       Executes command repeatedly as long as its return\n"
	            "       code is %d, or a file with the given name has been\n"
		    "       created.\n", RESTART);
     exit(1);
     }

int main(int argc,char **argv) {
     int i;
     unsigned n;
     char *filename, *command;
     struct stat buf;
     progname = argv[0], argc--, argv++;
     if (argc == 0) usage();
     filename = argv[0], argc--, argv++;
     // fprintf(stderr,"%s: trigger file name : %s\n",progname,filename);
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
	  fprintf(stderr,"%s: running: %s\n",progname,command);
	  fflush(stderr);
	  ret = system(command);
	  if (ret >= 256) ret >>= 8;
	  if (0 == stat(filename,&buf)) {
	    fprintf(stderr,"%s: file '%s' found, removing it and restarting\n",progname,filename);
	    if (-1 == unlink(filename)) {
	      fprintf(stderr,"%s: error: failed to remove file '%s'\n",progname,filename);
	      exit(1);
	    }
	    continue;
	  }
	  if (ret == RESTART) {
	    fprintf(stderr,"%s: returning with code %d to signal a restart\n",progname, RESTART);
	  }
	  fprintf(stderr,"restart: done\n");
	  return ret;
	  }
     }
