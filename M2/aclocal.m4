define(M2_UPPER,[translit($1, `a-z', `A-Z')])

AC_DEFUN(M2_IS_THERE,[
  AC_CHECK_FUNC($1,,AC_DEFINE(M2_UPPER(NO_$1),1,whether $1 is missing))
])

AC_DEFUN(M2_IS_DECLARED,[
  AC_CACHE_CHECK(whether $2 is declared, m2_cv_$2_is_declared,
    AC_TRY_COMPILE(
      [
      #include <stdio.h>
      #include <errno.h>
      ],
      $1 x = $2,
      m2_cv_$2_is_declared=yes,
      m2_cv_$2_is_declared=no))
  if test "$m2_cv_$2_is_declared" = yes
  then AC_DEFINE(M2_UPPER($2_IS_DECLARED),,whether $2 is declared in errno.h or stdio.h)
  fi
])

AC_DEFUN(M2_SHOW_CONFDEFS,[
  echo contents of confdefs.h: >&6
  sed -e '/^$/d' -e 's/^/   /' confdefs.h >&6
  ])

AC_DEFUN(M2_ENABLE_DUMPDATA,[
DUMPDATA=yes
AC_SUBST(DUMPDATA)
AC_ARG_ENABLE(dumpdata,[\
  --disable-dumpdata      do not cache data with dumpdata
  --enable-dumpdata=old   cache data with the old version of dumpdata\
],DUMPDATA="$enableval")
case "$DUMPDATA" in
   no) ;;
   old) ;;
   yes) AC_DEFINE(NEWDUMPDATA,,whether to use the new version of dumpdata) ;;
   *) echo configure: error: unrecognized value --enable-dumpdata="$enableval" >&2; exit 1 ;;
esac
case "$DUMPDATA" in
   yes|old) AC_DEFINE(DUMPDATA,,whether to use dumpdata) ;;
esac
])

AC_DEFUN(M2_WITH_SOCKS,[
SOCKS=no
AC_SUBST(SOCKS)
AC_ARG_WITH(socks,[\
  --with-socks		  compile and link with socks5\
],SOCKS="$withval")
case "$SOCKS" in
   no) ;;
   yes) ;;
   *) echo configure: error: unrecognized value --with-socks="$withval" >&2; exit 1 ;;
esac
case "$SOCKS" in
   yes) AC_DEFINE(SOCKS,,whether to compile and link with socks5) ;;
esac
])

AC_DEFUN(M2_ENABLE_SHARED,[
SHAREDLIBS=no
AC_SUBST(SHAREDLIBS)
AC_ARG_ENABLE(shared,[\
  --enable-shared         create shared libraries\
],SHAREDLIBS="$enableval")
case "$SHAREDLIBS" in
   no) ;;
   yes) ;;
   *) echo configure: error: unrecognized value --enable-shared="$enableval" >&2; exit 1 ;;
esac
case "$SHAREDLIBS" in
   yes) AC_DEFINE(SHAREDLIBS,,whether to use shared libraries) ;;
esac
])

AC_DEFUN(M2_WITH_FACTORY,[
FACTORY=yes
AC_SUBST(FACTORY)
AC_ARG_WITH(factory,[\
  --without-factory       do not link with factory\
],FACTORY="$withval")
case "$FACTORY" in
   no) ;;
   yes) FACTORYVERSION=`egrep '^factory_version=' $srcdir/factory/configure.in | sed s/.*=//`
	AC_SUBST(FACTORYVERSION)
	AC_DEFINE_UNQUOTED(FACTORYVERSION,$FACTORYVERSION,factory version number)
	;;
   *) echo configure: error: unrecognized value --with-factory="$withval" >&2; exit 1 ;;
esac
case "$FACTORY" in
   yes) AC_DEFINE(FACTORY,,whether to link with factory) ;;
esac
])


AC_DEFUN(M2_ENABLE_PROFILE,[
PROFILE=no
AC_SUBST(PROFILE)
AC_ARG_ENABLE(profile,[\
  --enable-profile        enable profiling\
],PROFILE="$enableval")
])

AC_DEFUN(M2_ENABLE_DEBUG,[
DEBUG=no
AC_SUBST(DEBUG)
AC_ARG_ENABLE(debug,[\
  --enable-debug          enable debugging\
],DEBUG="$enableval")
])

AC_DEFUN(M2_ENABLE_VERBOSE,[
VERBOSE=no
AC_SUBST(VERBOSE)
AC_ARG_ENABLE(verbose,[\
  --enable-verbose        enable verbose memory allocation\
],VERBOSE="$enableval")
])

AC_DEFUN(M2_ENABLE_STATIC,[
STATIC=no
AC_SUBST(STATIC)
AC_ARG_ENABLE(static,[\
  --enable-static         enable static linking\
],STATIC="$enableval")
])

AC_DEFUN(M2_ENABLE_MEMDEBUG,[
MEMDEBUG=no
AC_SUBST(MEMDEBUG)
AC_ARG_ENABLE(memdebug,[\
  --enable-memdebug       enable memory allocation debugging\
],MEMDEBUG="$enableval")
])

AC_DEFUN(M2_ENABLE_STRIP,[
STRIP=yes
AC_SUBST(STRIP)
AC_ARG_ENABLE(strip,[\
  --disable-strip         don't strip the Macaulay 2 binary\
],STRIP="$enableval")
])

AC_DEFUN(M2_ENABLE_OPTIMIZE,[
OPTIMIZE=yes
AC_SUBST(OPTIMIZE)
AC_ARG_ENABLE(profile,[\
  --disable-optimize      disable optimization\
],OPTIMIZE="$enableval")
])


dnl this list is the same as the one in Makefile-configure
AC_DEFUN(M2_CONFIGURED_FILES,
[config.Makefile\
 Makefile\
 Macaulay2/Makefile\
 Macaulay2/m2/Makefile\
 Macaulay2/Mathematica/Makefile\
 Macaulay2/Vasconcelos-appendix/Makefile\
 Macaulay2/basictests/Makefile\
 Macaulay2/benchmarks/Makefile\
 Macaulay2/benchmarks2/Makefile\
 Macaulay2/book/Makefile\
 Macaulay2/c/Makefile\
 Macaulay2/c2/Makefile\
 Macaulay2/d/Makefile\
 Macaulay2/dbm/Makefile\
 Macaulay2/dumpdata/Makefile\
 Macaulay2/e/Makefile\
 Macaulay2/emacs/Makefile\
 Macaulay2/experiments/Makefile\
 Macaulay2/gmp-macros/Makefile\
 Macaulay2/html/Makefile\
 Macaulay2/mike-slides/Makefile\
 Macaulay2/mike-tut/Makefile\
 Macaulay2/packages/Makefile\
 Macaulay2/schubert/Makefile\
 Macaulay2/screen/Makefile\
 Macaulay2/slides/Makefile\
 Macaulay2/socket/Makefile\
 Macaulay2/test/Makefile\
 Macaulay2/tex/Makefile\
 Macaulay2/texmacs/Makefile\
 Macaulay2/thread/Makefile\
 Macaulay2/tutorial/Makefile\
 Macaulay2/setup\
 Macaulay2/util/Makefile])
