/*		Copyright 1993 by Daniel R. Grayson		*/

#if 1
#define assertpos(e,f)     ((e) ? 0 : (failpos(__FILE__, __LINE__,f),0))
#define assert(e)          ((e) ? 0 : (fail(__FILE__, __LINE__),0))
#else
#define assertpos(e,f)     1
#define assert(e)          1
#endif

extern void fail(const char *, int);
extern void failpos(const char *, int, node);
extern int n_errors;
void fatal(const char *s,...);
void error(const char *s,...);
void warning(const char *s,...);
void fatalpos(node p, const char *s,...);
void errorpos(node p, const char *s,...);
void warningpos(node p, const char *s,...);
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
