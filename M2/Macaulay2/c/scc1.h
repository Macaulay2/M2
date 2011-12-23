/*		Copyright 1993 by Daniel R. Grayson		*/

extern FILE *dependfile;
extern const char *targetname;
extern const char *outfilename;
extern bool do_this_cxx;
extern bool noline;
extern bool arraychks;
extern bool casechks;
extern bool nomacros;
extern bool compilerThreadLocal;
extern bool pthreadThreadLocal;
extern bool gdbm_ronly;
extern char *getmem(unsigned);
extern char *strnperm(const char *, unsigned);
extern char *strperm(const char *);
extern node newnode1(unsigned int, enum TAG);
extern void fail (const char *, int);
extern struct POS empty_pos;
extern const char *intToString(int);
extern int substr(const char *, const char *);
extern int strequaln(const char *, const char *, unsigned int);    
extern const char *tail(const char *);
extern const char *BaseName(const char *) ;
extern const char *newsuffix(const char *, const char *);
extern const char *newsuffixbase(const char *, const char *);

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
