#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define true 1
#define false 0

char *argv0;

void usage(){
     fprintf(stderr,"usage: %s \">file\" args...\n",argv0);
     fprintf(stderr,"   or: %s \">>file\" args...\n",argv0);
     fprintf(stderr,"     : -r     reverse order\n");
     fprintf(stderr,"     : -r2    reverse order in pairs\n");
     exit(1);
     }

main(int argc, char **argv) {
     int i;
     FILE *f;
     char *fn = NULL;
     int reverse = false, append = false, reversepairs = false;
     argv0 = argv++[0], argc--;
     if (argc >= 1 && 0 == strcmp(*argv,"-r")) {
	  reverse = true;
	  argc --;
	  argv ++;
	  }
     else if (argc >= 1 && 0 == strcmp(*argv,"-r2")) {
	  reversepairs = true;
	  argc --;
	  argv ++;
	  }
     if (argc >= 1 && argv[0][0] == '>') {
	  if (argv[0][1] == '>') {
	       fn = argv[0]+2;
	       append = true;
	       }
	  else {
	       fn = argv[0]+1;
	       }
	  argv++, argc--;
	  if (strlen(fn) == 0) usage();
	  }
     if (fn == NULL) f = stdout;
     else {
#         ifdef __DJGPP__
          f = fopen(fn,append ? "at" : "wt");
#         else
          f = fopen(fn,append ? "a" : "w");
#         endif
          }
     if (f == NULL) {
	  fprintf(stderr,"%s: can't open file '%s'\n",argv0,fn);
	  exit(2);
     	  }
     if (reverse) {
     	  for (i=argc-1; i>=0; i--) {
	       fputs(argv[i],f);
	       putc('\n',f);
     	       }
	  }
     else if (reversepairs) {
     	  for (i=argc-1; i>=0; i-=2) {
	       if (i-1 >= 0) fputs(argv[i-1],f), putc('\n',f);
	       fputs(argv[i],f), putc('\n',f);
     	       }
	  }
     else {
     	  for (i=0; i<argc; i++) {
	       fputs(argv[i],f);
	       putc('\n',f);
     	       }
	  }
     return 0;
     }

