/*		Copyright 1993 by Daniel R. Grayson		*/

#ifdef MPWC
char errfmt[] = "    File \"%s\"; Line %d # Column %d: %s";
char errfmtnc[] = "    File \"%s\"; Line %d # %s";
#else
char errfmt[] = "%s:%d:%d: %s";
char errfmtnc[] = "%s:%d: %s";
#endif

