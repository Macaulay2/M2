/* Copyright 1998 by Daniel R. Grayson */

#include "html-check-links.h"
#include "grammar.h"
#include "getmem.h"
extern FILE *yyin, *yyout;
extern int yyparse(void);
#ifdef DEBUG
extern int yydebug;
#endif

static char *dir(char *s) {
  char *t = strrchr(s,'/');
  if (t == NULL) return "";
  t++;
  return strnperm(s,t-s);
}

static void process(char *f) {
  yyin = fopen(f,"r");
  if (yyin == NULL) {
    error("can't open %s\n",f);
    return;
  }
  filename = f;
  dirname = dir(f);
  yyparse();
  fclose(yyin);
}

int main(int argc, char **argv) {
  int i = 1;
# ifdef DEBUG
    yydebug = 1;
# endif
  if (argc > i+1 && 0 == strcmp("--root",argv[i])) rootname = argv[i+1], i += 2;
  for (; i<argc; i++) process(argv[i]);
  return errors > 0 ? 1 : 0;
}
