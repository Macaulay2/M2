/*		Copyright 1993 by Daniel R. Grayson		*/

#if 1
#define assertpos(e,f)     ((e) ? 0 : failpos(__FILE__, __LINE__,f))
#define assert(e)          ((e) ? 0 : fail(__FILE__, __LINE__))
#else
#define assertpos(e,f)     0
#define assert(e)          0
#endif

extern void fail(char *, int);
extern void failpos(char *, int, node);
extern int n_errors;
void fatal(const char *s,...);
void error(char *s,...);
void warning(char *s,...);
void fatalpos(node p, char *s,...);
void errorpos(node p, char *s,...);
void warningpos(node p, char *s,...);
node typemismatch(node e);
node badnumargs(node e,int n);
node notimpl(node e);
void quit(void);
void downpos(node n);

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
