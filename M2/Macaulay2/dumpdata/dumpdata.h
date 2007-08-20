
/* We would like to eliminate the use of brk(), since it isn't posix standard, but some systems out there still use it.  For example,
   malloc() may call it (indirectly), and if any of our libraries uses it, then we have to. */

#define USE_BRK 1

int dumpdata(const char *);
int loaddata(int, const char *);


/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/dumpdata "
 End:
*/
