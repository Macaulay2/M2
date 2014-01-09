/*		Copyright 1993 by Daniel R. Grayson		*/

#ifdef MPWC
char posfmt[] = "    File \"%s\"; Line %d # Column %d: Loaddepth %d: ";
char errfmt[] = "    File \"%s\"; Line %d # Column %d: %s";
char errfmtnc[] = "    File \"%s\"; Line %d # %s";
#elif defined(_WIN32) && !defined(__CYGWIN32__)
char posfmt[] = "%s(%d) : column %d : loaddepth %d : ";
char errfmt[] = "%s(%d) : column %d : %s";
char errfmtnc[] = "%s(%d) : %s";
#else
char posfmt[] = "%s:%d:%d:(%d):";
char errfmt[] = "%s:%d:%d: error: %s";
char errfmtnc[] = "%s:%d: error: %s";
#endif


/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
