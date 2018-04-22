/* Copyright 1998 by Daniel R. Grayson */

#include <stdlib.h>
#include <M2/gc-include.h>
#include "html-check-links.h"
#include "grammar.h"
#include "getmem.h"
int ERRLIMIT = 64;
int abs_links = TRUE;
int verbose = FALSE;
extern FILE *yyin, *yyout;
extern int yyparse(void);
#ifndef NDEBUG
extern int yydebug;
#endif

static char *tab[][2] = {
   {" " , "sp"},
   {"*" , "st"},
   {"|" , "vb"},
   {"(" , "lp"},
   {")" , "rp"},
   {"<" , "lt"},
   {">" , "gt"},
   {"&" , "am"},
   {"@" , "at"},
   {"=" , "eq"},
   {"," , "cm"},
   {"#" , "sh"},
   {"+" , "pl"},
   {"$" , "do"},
   {"%" , "pc"},
   {"'" , "sq"},
   {"/" , "sl"},
   {":" , "co"},
   {";" , "se"},
   {"?" , "qu"},
   {"\"", "dq"},
   {"\\", "bs"},
   {"_" , "us"}
};

static char *lookup(char b, char c) {
  int i;
  for (i=0; i < sizeof(tab)/sizeof(*tab); i++) {
    if (tab[i][1][0] == b &&tab[i][1][1] == c) return tab[i][0];
  }
  return NULL;
}

static char buf[1000];
static int bufpos;
static void addchar(char c) {
  if (bufpos < sizeof(buf)-1) buf[bufpos++]=c;
}

char *demangle (char *s) {
  char *p;
  int c=0, b=0, a=0;
  bufpos = 0;
  while (1) {
    if (a != 0) addchar(a);
    a=b;
    b=c;
    c=*s++;
    if (c == 0) break;
    if (b == '/' && c == '_') c=0;
    else if (a == '_' && b == '_') a=b=0;
    else if (a == '_') {
      p = lookup(b,c);
      if (p != NULL) a=b=c=0, addchar(*p);
    }
  }
  if (a != 0) addchar(a);
  if (b != 0) addchar(b);
  if (c != 0) addchar(c);
  addchar(0);
  return buf;
}

static char *dir(char *s) {
  char *t = strrchr(s,'/');
  if (t == NULL) return "";
  t++;
  return strnperm(s,t-s);
}

static void process(char *f) {
  if (verbose) fprintf(stderr,"--file: %s\n",f);
  yyin = fopen(f,"r");
  if (yyin == NULL) {
    error("can't open %s\n",f);
    return;
  }
  filename = f;
  Dirname = dir(f);
  yyparse();
  fclose(yyin);
}

void usage() {
     fprintf(stderr,
	     "Usage: html-check-links [OPTION]... [FILE]...\n"
	     "Check html links in html files.\n"
	     "\n"
	     "Options:\n"
	     "  --help                display this help and exit\n"
	     "  --root PREFIX         prepend the given root prefix to each absolute path\n"
	     "  --no-limit            change error message limit from 64 to infinity\n"
	     "  --no-absolute-links   flag links with absolute paths as errors\n"
	     "  --verbose             print the names of the files as they are opened\n");
     }

int main(int argc, char **argv) {
  int i = 1;
# ifndef NDEBUG
  /*    yydebug = 1; */
# endif
  GC_init();
  while (TRUE) {
    if (argc > i+1 && 0 == strcmp("--root",argv[i])) {
      rootname = argv[i+1];
      i += 2;
    }
    else if (argc > i && 0 == strcmp("--no-limit",argv[i])) {
      ERRLIMIT = 0;
      i++;
    }
    else if (argc > i && 0 == strcmp("--no-absolute-links",argv[i])) {
      abs_links = FALSE;
      i++;
    }
    else if (argc > i && 0 == strcmp("--verbose",argv[i])) {
      verbose = TRUE;
      i++;
    }
    else if (argc > i && 0 == strcmp("--help",argv[i])) {
      usage();
      exit(0);
    }
    else break;
  }
  for (; i<argc; i++) process(argv[i]);
  return errors > 0 ? 1 : 0;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/html-check-links html-check-links"
 End:
 */
