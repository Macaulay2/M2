#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#define true 1
#define false 0

typedef int bool;

static char delim1[] = "<<<";
static char delim2[] = ">>>";

static char *progname;

static void error(char *s) {
     fprintf(stderr, "%s: %s\n",progname,s);
     exit(1);
     }

static void usage() {
     fprintf(stderr,"usage: %s filename.tex > filename.m2\n",progname);
     exit(1);
     }

int pass(char *s, FILE *f, FILE *o) {
     char *p;
     int c;
     for (p=s; *p;) {
	  c = getc(f);
	  if (c == EOF) return EOF;
	  if (*p == c) {
	       p++;
	       }
	  else {
	       if (o != NULL) {
	       	    char *q;
	       	    for (q=s; q<p; q++) putc(*q,o);
	       	    putc(c,o);
		    }
	       p = s;
	       }
	  }
     return true;
     }

int main(int argc, char **argv) {
     FILE *infile = NULL;
     progname = argv[0];
     if (argc != 2) usage();
     infile = fopen(argv[1],"r");
     if (infile == NULL) {
	  char buf[100];
	  sprintf(buf, "%s: couldn't open file %s for reading",progname,argv[1]);
	  perror(buf);
	  exit(1);
	  }
     while (true) {
     	  int c = pass(delim1,infile,NULL);
	  if (c == EOF) exit(0);
	  c = pass(delim2,infile,stdout);
	  if (c == EOF) error("encountered end of file within delimiters");
	  putc('\n',stdout);
	  }
     }
