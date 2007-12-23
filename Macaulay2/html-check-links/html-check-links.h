#ifndef C2_H
#define C2_H
#include <stdio.h>
#define TRUE 1
#define FALSE 0
#define put(s) fputs(s,stdout)
#define EQUAL 0
#define numberof(x) (sizeof(x)/sizeof(x[0]))
#define forarray(i,x) for(i=0; i<numberof(x); i++)
#define forlist(p) for(;p;p=p->next)
#define ckarray(x,i) (assert(i >= 0),assert(i < numberof(x)))
#ifndef roundup
#define roundup(n,d) ((((n)+(d)-1)/(d))*(d))
#endif
#endif
extern char *filename, *Dirname, *rootname;
extern int lineno, column;
extern char *demangle (char *);
