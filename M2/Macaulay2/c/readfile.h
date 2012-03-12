/*		Copyright 1993 by Daniel R. Grayson		*/

struct CURRENT {
     char *filename, *text, *eot;
     int lineno, column;
     bool wrapit;
     } ;
extern struct CURRENT cur;
extern bool iswhite(int);
extern void read_setup();
extern node gettoken(void);
extern bool validtoken(char *);
extern void advance(void);
extern node readfile(char *filename);
extern int tabwidth;
extern bool sigreadfile(char *, char **);
extern char *sigpath;

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
