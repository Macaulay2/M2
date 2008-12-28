/*		Copyright 1993 by Daniel R. Grayson		*/

#include "scc.h"

void fatal(char *s,...)
{
     va_list ap;
     va_start(ap,s);
     if (cur.filename != NULL) {
     	  fprintf(stderr,errfmt,cur.filename,cur.lineno,cur.column+1,"");
     	  }
     vfprintf(stderr,s,ap);
     fprintf(stderr,"\n");
     fflush(stderr);
     va_end(ap);
     exit(1);
     }

int n_errors = 0;
#define ERRLIMIT 24
int warnings = 0;
#define WARNLIMIT 24

void error(char *s,...)
{
     va_list ap;
     va_start(ap,s);
     fprintf(stderr,errfmt,
	  cur.filename!=NULL?cur.filename:"",cur.lineno,cur.column+1,"");
     vfprintf(stderr,s,ap);
     fprintf(stderr,"\n");
     fflush(stderr);
     va_end(ap);
     n_errors++;
     if (n_errors > ERRLIMIT) fatal("too many errors");
     }

void warning(char *s,...)
{
     va_list ap;
     va_start(ap,s);
     vfprintf(stderr,s,ap);
     fprintf(stderr,"\n");
     fflush(stderr);
     va_end(ap);
     warnings++;
     if (warnings > WARNLIMIT) fatal("too many errors");
     }

void fatalpos(node p, char *s,...)
{
     va_list ap;
     downpos(p);
     va_start(ap,s);
     vfprintf(stderr,s,ap);
     fprintf(stderr,"\n");
     fflush(stderr);
     va_end(ap);
     exit(1);
     }

void errorpos(node p, char *s,...)
{
     va_list ap;
     downpos(p);
     va_start(ap,s);
     vfprintf(stderr,s,ap);
     fprintf(stderr,"\n");
     fflush(stderr);
     va_end(ap);
     n_errors++;
     if (n_errors > ERRLIMIT) fatal("too many errors");
     }

void warningpos(node p, char *s,...)
{
     va_list ap;
     downpos(p);
     fprintf(stderr,"warning: ");
     va_start(ap,s);
     vfprintf(stderr,s,ap);
     fprintf(stderr,"\n");
     fflush(stderr);
     va_end(ap);
     warnings++;
     if (warnings > WARNLIMIT) fatal("too many errors");
     }

node typemismatch(node e){
     errorpos(e,"type mismatch");
     return bad_K;
     }

node badnumargs(node e,int n){
     errorpos(e,"should have %d argument%s",n,n==1?"":"s");
     return bad_K;
     }

node notimpl(node e){
     errorpos(e,"not implemented yet");
     return bad_K;
     }

void quit(){
     myexit(n_errors != 0);
     }

void fail(char *filename, int lineno) {
     fprintf(stderr,"%s:%d: assertion failed\n", filename,lineno);
     if (cur.filename != NULL) {
     	  fprintf(stderr,"%s:%d: <- here\n",
	       cur.filename, cur.lineno);
	  }
     myexit(1);
     }

void downpos(node n){
     struct POS *p = pos(n);
     if (p != NULL && p->filename != NULL) {
	  fprintf(stderr,errfmt,p->filename,p->lineno,p->column+1,"");
	  }
     else if (cur.filename != NULL) {
	  fprintf(stderr,errfmt,cur.filename,cur.lineno,cur.column+1,"");
	  }
     else fprintf(stderr,errfmt,"",0,0,"");
     }


void failpos(char *filename, int lineno, node p) {
     downpos(p);
     fprintf(stderr,"(%s:%d): assertion failed\n", filename,lineno);
     if (cur.filename != NULL) {
     	  fprintf(stderr,"%s:%d: <- here\n",
	       cur.filename, cur.lineno);
	  }
     myexit(1);
     }

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
