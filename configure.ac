AC_INIT(Macaulay 2, 0.9.5, Macaulay2@math.uiuc.edu, Macaulay2)
dnl -- check Makefile-configuration is present in srcdir?

define(M2_UPPER,[translit($1, [a-z], [A-Z])])

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

AC_DEFUN(M2_CONFIGURED_FILES,patsubst(m4_include(config/files),[
],[ ]))

echo "any environment variables set:"
test "$CC"         && echo "   CC=$CC"
test "$CPPFLAGS"   && echo "   CPPFLAGS=$CPPFLAGS"
test "$CFLAGS"     && echo "   CFLAGS=$CFLAGS"
test "$CXX"        && echo "   CXX=$CXX"
test "$CXXFLAGS"   && echo "   CXXFLAGS=$CXXFLAGS"
test "$LDFLAGS"    && echo "   LDFLAGS=$LDFLAGS"
test "$LIBS"       && echo "   LIBS=$LIBS"
test "$LD_LIBRARY_PATH"   && echo "   LD_LIBRARY_PATH=$LD_LIBRARY_PATH"

# 5 compiler messages saved in config.log
# 6 checking for... messages and results

AC_CHECK_PROGS(MAKE,gmake make)

echo $ECHO_N "checking whether $MAKE is GNU make... " >&6
if "$MAKE" -n --version nothing | grep GNU >/dev/null 2>&1
then echo "yes" >&6
else echo "`basename $0`: $MAKE: GNU make is required" >&2
     exit 1
fi

NODENAME="`uname -n`"
AC_SUBST(NODENAME)
AC_DEFINE_UNQUOTED(NODENAME,"$NODENAME",hostname used for compilation)

REL="`  uname -r | sed -e s=/=-=g -e 's= =+=' -e 's=[()]==g'  `"
AC_SUBST(REL)
AC_DEFINE_UNQUOTED(REL,"$REL",[operating system release, obtained with uname -r])

ARCH="`  uname -m | sed -e s=/=-=g -e 's= ==g' `"
AC_SUBST(ARCH)
AC_DEFINE_UNQUOTED(ARCH,"$ARCH",[machine hardware type, obtained with uname -m])

OS="`  uname -s | sed s=/=-=g  `"
AC_SUBST(OS)
AC_DEFINE_UNQUOTED(OS,"$OS",[operating system name, obtained with uname -s])

AC_CONFIG_AUX_DIR(config)
AC_CANONICAL_HOST()
AC_VALIDATE_CACHED_SYSTEM_TUPLE()

# ARCH=$host_cpu
# AC_SUBST(ARCH)
# AC_DEFINE_UNQUOTED(ARCH,"$ARCH",[machine hardware type])

AC_ARG_VAR(CC,C compiler to use)
AC_ARG_VAR(CXX,C++ compiler to use)

AC_PROG_RANLIB()
AC_PROG_INSTALL()
AC_PROG_AWK()
AC_PROG_CC()
AC_SUBST(GCC)
AC_PROG_CXX()
AC_PROG_YACC()

AC_FUNC_ALLOCA()

DUMPDATA=yes
AC_SUBST(DUMPDATA)
AC_ARG_ENABLE(dumpdata,
[  --disable-dumpdata      do not cache data with dumpdata
  --enable-dumpdata=old   cache data with the old version of dumpdata],
DUMPDATA="$enableval")
case "$DUMPDATA" in
   no) ;;
   old) ;;
   yes) AC_DEFINE(NEWDUMPDATA,,whether to use the new version of dumpdata) ;;
   *) echo configure: error: unrecognized value --enable-dumpdata="$enableval" >&2; exit 1 ;;
esac
case "$DUMPDATA" in
   yes|old) AC_DEFINE(DUMPDATA,,whether to use dumpdata) ;;
esac

AC_SUBST(PROFILE)
AC_ARG_ENABLE(profile,
    AC_HELP_STRING(--enable-profile,enable profiling (and disable stripping)),
    PROFILE="$enableval",
    PROFILE=no)

AC_SUBST(DEBUG)
AC_ARG_ENABLE(debug,
    AC_HELP_STRING(--enable-debug,enable debugging),
    DEBUG="$enableval",
    DEBUG=no)

AC_SUBST(VERBOSE)
AC_ARG_ENABLE(verbose,
    AC_HELP_STRING(--enable-verbose,enable verbose memory allocation),
    VERBOSE="$enableval",
    VERBOSE=no)

AC_SUBST(STATIC)
AC_ARG_ENABLE(static,
    AC_HELP_STRING(--enable-static,enable static linking),
    STATIC="$enableval",
    STATIC=no)

AC_SUBST(MEMDEBUG)
AC_ARG_ENABLE(memdebug,
    AC_HELP_STRING(--enable-memdebug,enable memory allocation debugging),
    MEMDEBUG="$enableval",
    MEMDEBUG=no)

AC_SUBST(OPTIMIZE)
AC_ARG_ENABLE(optimize,
    AC_HELP_STRING(--disable-optimize,disable optimization),
    OPTIMIZE="$enableval",
    OPTIMIZE=yes)

AC_SUBST(STRIP)
AC_ARG_ENABLE(strip,
    AC_HELP_STRING(--disable-strip,do not strip the Macaulay 2 binary),
    STRIP="$enableval",
    STRIP=yes)

AC_SUBST(encap)
AC_ARG_ENABLE(encap,
    AC_HELP_STRING(--enable-encap,create an encap package),
    encap="$enableval",
    encap=no)

AC_SUBST(LAPACK)
AC_ARG_ENABLE(lapack,
    AC_HELP_STRING(--enable-lapack,use experimental LAPACK interface),
    [ LAPACK="$enableval" ; AC_DEFINE(LAPACK,,whether to use LAPACK) ],
    LAPACK=yes)

AC_SUBST(encapsrc)
AC_ARG_WITH(encapsrc,
    AC_HELP_STRING(--with-encapsrc=...,encap directory [PREFIX/encap]),
    encapsrc="$withval",
    )

AC_SUBST(GC_FOR_NEW)
AC_ARG_ENABLE(gc_for_new,
    AC_HELP_STRING(--enable-gc-for-new,use gc instead of malloc for c++ 'new' and 'delete' operators),
    GC_FOR_NEW="$enableval",
    GC_FOR_NEW=no)

AC_CHECK_DECL(__finite,,AC_DEFINE(__finite,finite,whether __finite is missing),[#include <math.h>])
AC_CHECK_DECL(__isnan,,AC_DEFINE(__isnan,isnan,whether __isnan is missing),[#include <math.h>])
AC_CHECK_DECL(__isinf,,AC_DEFINE(__isinf,isinf,whether __isinf is missing),[#include <math.h>])
AC_CHECK_DECLS(sys_nerr)

SAVELIBS="$LIBS"

AC_MSG_CHECKING([whether gc library is installed])
LIBS="-lgc $SAVELIBS"
AC_TRY_LINK(
    [#include <gc.h>],
    [GC_malloc(1);],
    ,AC_MSG_RESULT([no]);AC_MSG_ERROR([missing library: libgc]))
AC_MSG_RESULT([yes])

AC_MSG_CHECKING([whether gmp library is installed])
LIBS="-lgmp $SAVELIBS"
AC_TRY_LINK(
    [#include <gmp.h>],
    [__gmp_version;],
    ,AC_MSG_RESULT([no]);AC_MSG_ERROR([missing library: gmp]))
AC_MSG_RESULT([yes])

AC_MSG_CHECKING([whether gdbm library is installed])
LIBS="-lgdbm $SAVELIBS"
AC_TRY_LINK([#include <gdbm.h>],
    [gdbm_close(0);],
    ,AC_MSG_RESULT([no]);AC_MSG_ERROR([missing library: gdbm]))
AC_MSG_RESULT([yes])

AC_MSG_CHECKING([whether factory library is installed])
LIBS="-lcf $SAVELIBS"
dnl -- we do not try linking, because we do not know whether libcfmem.a is installed or needed by libcf.a
dnl -- we do not need libcfmem later, in either case, because we include replacement routines in d/scclib.c
AC_TRY_COMPILE(
    [#include <factoryconf.h>],
    [FACTORYVERSION;],
    ,AC_MSG_RESULT([no]);AC_MSG_ERROR([missing library: factory]))
AC_MSG_RESULT([yes])

AC_MSG_CHECKING([whether libfac library is installed])
LIBS="-lfac -lcf $SAVELIBS"
AC_TRY_LINK(
    [extern const char * libfac_version;],
    [libfac_version;],
    ,AC_MSG_RESULT([no]);AC_MSG_ERROR([missing library: libfac]))
AC_MSG_RESULT([yes])

LIBS="$SAVELIBS"

AC_CHECK_HEADERS(wordexp.h)
AC_CHECK_FUNCS(wordexp)
M2_SHOW_CONFDEFS()

# the next three are obsolete:
M2_IS_THERE(sys_errlist)
M2_IS_THERE(herror)
M2_IS_THERE(gnu_get_libc_version)
AC_CHECK_DECLS([sys_errlist])
AC_CHECK_DECLS([herror])
AC_CHECK_DECLS([gnu_get_libc_version])

M2_IS_DECLARED(char **,sys_errlist)
M2_IS_DECLARED(char **,h_errlist)
AC_CONFIG_HEADER(include/config.h)
CONFIGURED_FILES="M2_CONFIGURED_FILES()"
AC_SUBST(CONFIGURED_FILES)
AC_OUTPUT(M2_CONFIGURED_FILES())

dnl -- Local Variables:
dnl -- mode: sh
dnl -- compile-command: "make -f Makefile-configuration"
dnl -- End:
