AC_INIT(Makefile-configuration)

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

AC_DEFUN(M2_CONFIGURED_FILES,m4_patsubst(m4_include(config/files),[
],[ ]))

echo "any environment variables set:"
test "$CC"         && echo "   CC=$CC"
test "$CFLAGS"     && echo "   CFLAGS=$CFLAGS"
test "$LDFLAGS"    && echo "   LDFLAGS=$LDFLAGS"
test "$CPPFLAGS"   && echo "   CPPFLAGS=$CPPFLAGS"
test "$CXX"        && echo "   CXX=$CXX"
test "$CXXFLAGS"   && echo "   CXXFLAGS=$CXXFLAGS"

# 5 compiler messages saved in config.log
# 6 checking for... messages and results

AC_CHECK_PROGS(MAKE,gmake make)

echo $ECHO_N "checking whether $MAKE is GNU make... " >&6
if "$MAKE" -n --version nothing | grep GNU >/dev/null 2>&1
then echo "yes" >&6
else echo "`basename $0`: $MAKE: GNU make is required" >&2
     exit 1
fi

PACKAGE_NAME=Macaulay2
PACKAGE_VERSION="`cat $srcdir/version`"
AC_DEFINE_UNQUOTED(PACKAGE_VERSION,"$PACKAGE_VERSION",version number)

NODENAME="`uname -n`"
AC_SUBST(NODENAME)
AC_DEFINE_UNQUOTED(NODENAME,"$NODENAME",hostname used for compilation)

REL="`  uname -r | sed -e s=/=-=g -e 's= =+=' -e 's=[()]==g'  `"
AC_SUBST(REL)
AC_DEFINE_UNQUOTED(REL,"$REL",[operating system release, obtained with uname -r])

ARCH="`  uname -m | sed s=/=-=g  `"
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

AC_SUBST(encapdir)
AC_ARG_WITH(encapdir,
    AC_HELP_STRING(--with-encapdir=...,encap source directory [/usr/local/encap]),
    encapdir="$withval",
    encapdir='$(prefix)/encap')

AC_SUBST(GC_FOR_NEW)
AC_ARG_ENABLE(gc_for_new,
    AC_HELP_STRING(--enable-gc-for-new,use gc instead of builtin_new),
    GC_FOR_NEW="$enableval",
    GC_FOR_NEW=no)

AC_CHECK_FUNC(__finite,,AC_DEFINE(__finite,finite,whether __finite is missing))
AC_CHECK_FUNC(__isnan,,AC_DEFINE(__isnan,isnan,whether __isnan is missing))
AC_CHECK_FUNC(__isinf,,AC_DEFINE(__isinf,isinf,whether __isinf is missing))

OLIBS="$LIBS"
LIBS="$LIBS -lgmp -lgc -lgdbm -lcf -lfac"
AC_TRY_LINK(
    [#include <gc.h>],
    [GC_malloc(1);],
    ,AC_MSG_WARN(missing library: libgc))
AC_TRY_LINK(
    [#include <gmp.h>],
    [__gmp_version;],
    ,AC_MSG_WARN(missing library: libgmp))
AC_TRY_LINK([#include <gdbm.h>],
    [gdbm_close(0);],
    ,AC_MSG_WARN(missing library: libgdbm))
AC_TRY_LINK(
    [#include <factoryconf.h>],
    [FACTORYVERSION;],
    ,AC_MSG_WARN(missing library: libcf))
AC_TRY_LINK(
    [extern const char * libfac_version;],
    [libfac_version;],
    ,AC_MSG_WARN(missing library: libfac))
LIBS="$OLIBS"

# M2_SHOW_CONFDEFS()
M2_IS_THERE(sys_errlist)
M2_IS_THERE(herror)
M2_IS_THERE(gnu_get_libc_version)
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
