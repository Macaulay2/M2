/*		Copyright 1993 by Daniel R. Grayson		*/

   /* declarations */

%{
#include "html-check-links.h"
#include "buffer.h"
#include "grammar.h"
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
extern unsigned lastcount;

typedef struct { int lineno, column; char *opener; } location;
BUFFER(location)
locationbuf openings;
static void opening(char *opener) {
  location t;
  t.lineno = lineno;
  t.column = column-lastcount;
  t.opener = opener;
  addlocation(&openings,t);
}
#define open(x,y) opening("'" #x "' unmatched by '" #y "'")
#define rhs(x) opening("expression to right of '" x "' incomplete")
static void closing() { rmlocation(&openings); }

static void yyerror(char *);
#define end_of_input 0
extern int yylex (void);
#include <stdarg.h>
static void checkURL(char *);
%}

%token '<' '>' '=' WORD SRC HREF BACKGROUND

%start html

%% /* rules */

empty : ;
html : empty | html tag ;
tag : '<'                                  { opening("unmatched '<'"); }
      values '>'                           { closing(); }
    | error '>' ;
values : empty | values value ;
key : BACKGROUND | HREF | SRC ;
value
    : WORD
    | WORD '=' WORD 
    | key '=' WORD { checkURL($3); }
    ;

%% /* programs */

#ifdef MPWC
char errfmt  [] = "    File \"%s\"; Line %d # Column %d: %s\n";
char errfmtnc[] = "    File \"%s\"; Line %d # %s\n";
#elif defined(_WIN32) && !defined(__CYGWIN32__)
char errfmt  [] = "%s(%d) : column %d : %s\n";
char errfmtnc[] = "%s(%d) : %s\n";
#else
char errfmt  [] = "%s:%d:%d: %s\n";
char errfmtnc[] = "%s:%d: %s\n";
#endif

#define VA_START_HAS_TWO_ARGS

#define ERRLIMIT 64
int errors = 0;

void fatal(char *s,...)
{
     char buf[200];
     va_list ap;
#ifdef VA_START_HAS_TWO_ARGS
     va_start(ap,s);
#else
     va_start(ap);
#endif
     vsnprintf(buf,sizeof buf,s,ap);
     fprintf(stderr,errfmt,filename,lineno,column-lastcount,buf);
     fflush(stderr);
     va_end(ap);
     exit(1);
     }

void error(char *s,...)
{
     char buf[200];
     char *err = "error: ";
     char new_s[strlen(s) + strlen(err) + 1];
     va_list ap;
#ifdef VA_START_HAS_TWO_ARGS
     va_start(ap,s);
#else
     va_start(ap);
#endif
     strcpy(new_s,err);
     strcat(new_s,s);
     vsnprintf(buf,sizeof buf,new_s,ap);
     fprintf(stderr,errfmt,filename,lineno,column-lastcount,buf);
     fflush(stderr);
     va_end(ap);
     if (++errors > ERRLIMIT) fatal("too many errors");
     }

void warning(char *s,...)
{
     char buf[200];
     char *err = "warning: ";
     char new_s[strlen(s) + strlen(err) + 1];
     va_list ap;
#ifdef VA_START_HAS_TWO_ARGS
     va_start(ap,s);
#else
     va_start(ap);
#endif
     strcpy(new_s,err);
     strcat(new_s,s);
     vsnprintf(buf,sizeof buf,new_s,ap);
     fprintf(stderr,errfmt,filename,lineno,column-lastcount,buf);
     fflush(stderr);
     va_end(ap);
     }

static void printopening(location x) {
  char buf[100];
  snprintf(buf,sizeof buf,"  (%s)", x.opener);
  fprintf(stderr,errfmt,filename,x.lineno,x.column,buf);
}

static void yyerror(char *s) {
     if (yychar == 0) s = concat(s," at end of file");
     error(s);
#if 1
     rscan(openings,printopening);
#else
     if (openings.used > 0) printopening(openings.array[openings.used-1]);
#endif
     emptylocation(&openings);
     }

static int strseg(char *s, char *t) {
  while (*t) if (*s++ != *t++) return FALSE;
  return TRUE;
}

static void checkURL(char *s) {
  char *t, *u;
  if (s[0] == '"' && s[strlen(s)-1] == '"') s++, s[strlen(s)-1]=0;
  else if (s[0] == '\'' && s[strlen(s)-1] == '\'') s++, s[strlen(s)-1]=0;
  if (strseg(s,"mailto:") || strseg(s,"http://") || strseg(s,"ftp://") || strseg(s,"file://")) {
    /* warning("unchecked external link: %s",s); */
    return;
  }
  t = strrchr(s,'#'); if (t != NULL) *t = 0;
  if (*s == 0) return;
  s = concat(s[0] == '/' ? rootname : Dirname,s);
  if (-1 == access(s, R_OK)) {
    char *p = demangle(s);
    if (0 == strcmp(p,s)) error("can't reach %s",s);
    else error("can't reach %s ==> %s",s,p);
    u = concat(s,"");
#if 0
    {
      char buf[1000];
      t = rindex(u,'/'); 
      if (t != NULL) {
	*t = 0;
	snprintf(buf,sizeof buf,
		 "ls '%s'/* 2>/dev/null | egrep -i '^%s$' | sed 's/^/    but look at this: /'",u,s);
      }
      else {
	snprintf(buf,sizeof buf,"ls * 2>/dev/null | egrep -i '^%s$' | sed 's/^/    but look at this: /'",s);
      }
      system(buf);
    }
#endif
  }
}
