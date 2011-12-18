/*		Copyright 1993 by Daniel R. Grayson		*/

extern FILE *dependfile;
extern char *targetname;
extern char *outfilename;
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
extern void fail (char *, int);
extern const struct POS empty_pos;
extern char *intToString(int);
extern int substr(char *, char *);
extern int strequaln(char *, char *, unsigned int);    
extern char *tail(char *);
extern char *BaseName(char *) ;
extern char *newsuffix(char *, char *);
extern char *newsuffixbase(char *, char *);

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
