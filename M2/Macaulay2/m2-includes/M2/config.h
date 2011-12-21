#ifndef _CONFIG_H_
#define _CONFIG_H_


#define M2_CONFIG_H 1
/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "Macaulay2@math.uiuc.edu"

/* Define to the full name of this package. */
#define PACKAGE_NAME "Macaulay 2"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "Macaulay 2 1.4.0.1"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "Macaulay2"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.4.0.1"
/* suffix the compiler appends to executable filenames */
#define EXEEXT ""

/* whether we are linking with the frobby library */
#define HAVE_FROBBY 1
/* whether we are linking with the pari library */
#define HAVE_PARI 1

/* whether getaddrinfo can handle numeric service (port) numbers */
#define GETADDRINFO_WORKS 1
/* whether to link with lapack */
#define LAPACK 1
/* whether we are linking with the xml library */
#define HAVE_XML 1

/* whether we are linking with the mysql library */
#define USE_MYSQL 0


#endif
