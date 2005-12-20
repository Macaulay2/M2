AC_INIT(Macaulay 2, 0.9.7, Macaulay2@math.uiuc.edu, Macaulay2)
AC_REVISION($Revision$)

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
test "$CXX"        && echo "   CXX=$CXX"
test "$CPPFLAGS"   && echo "   CPPFLAGS=$CPPFLAGS"
dnl -- autoconf default is: -g -O2
test "$CFLAGS"     && echo "   CFLAGS=$CFLAGS"
dnl -- autoconf default is: -g -O2
test "$CXXFLAGS"   && echo "   CXXFLAGS=$CXXFLAGS"
test "$LDFLAGS"    && echo "   LDFLAGS=$LDFLAGS"

if test "$CXX" = gcc
then echo oops, CXX=gcc, should use CXX=g++ >&2
     exit 1
fi

AC_ARG_VAR(CFLAGS,[C compiler flags, default: -g -O2])

AC_ARG_VAR(LOADLIBES,[first path for linker to search for libraries])
test "$LOADLIBES"  && echo "   LOADLIBES=$LOADLIBES"
AC_ARG_VAR(LDLIBS,[second path for linker to search for libraries])
test "$LDLIBS"     && echo "   LDLIBS=$LDLIBS"
AC_ARG_VAR(LIBS,[path for linker to search for libraries during configuration])
test "$LIBS"     && echo "   LIBS=$LIBS"

# 5 compiler messages saved in config.log
# 6 checking for... messages and results

AC_CHECK_PROGS(MAKE,gmake make)

echo $ECHO_N "checking whether $MAKE is GNU make... " >&6
if "$MAKE" -n --version nothing | grep GNU >/dev/null 2>&1
then echo "yes" >&6
else echo "`basename $0`: $MAKE: GNU make is required" >&2
     exit 1
fi

AC_CHECK_PROG(INSTALL_INFO,install-info,yes)
if ! test "$INSTALL_INFO"
then echo "configure: error: program 'install-info' (from the 'texinfo' package) not found" >&2
     exit 1
fi
install-info --version |
  egrep '^install-info' |
[  sed -e 's/^.* \([1-9][0-9]*\)\.\([1-9][0-9]*\)/\1 \2/' -e 's/^i.*//' |]
  if read major minor
  then min=4
       if test "$major" -lt "$min"
       then echo "Macaulay2 requires 'install-info' (from the 'texinfo' package) at least version $min" >&2
	    false
       fi
  else echo "configure: error: program 'install-info' version number not recognized" >&2
       false
  fi || exit 1

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

CONFIG_ARGS="$ac_configure_args"
AC_SUBST(CONFIG_ARGS)
AC_DEFINE_UNQUOTED(CONFIG_ARGS,"$CONFIG_ARGS",arguments used for configure)

CONFIG_ARG0="$0"
AC_SUBST(CONFIG_ARG0)

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

AC_SUBST(FACTORY)
AC_ARG_ENABLE(factory,
    AC_HELP_STRING(--disable-factory,disable the use of the 'factory' library),
    FACTORY="$enableval",
    FACTORY=yes)
if test "$FACTORY" = yes
then AC_DEFINE(FACTORY,,whether to use the 'factory' library)
fi

AC_SUBST(STRIP)
AC_ARG_ENABLE(strip,
    AC_HELP_STRING(--disable-strip,do not strip the Macaulay 2 binary),
    STRIP="$enableval",
    STRIP=yes)

AC_SUBST(DEBUG)
AC_ARG_ENABLE(debug,
    AC_HELP_STRING(--enable-debug,enable debugging (and disable stripping)),
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

AC_SUBST(ENCAP)
AC_ARG_ENABLE(encap,
    AC_HELP_STRING(--enable-encap,create an encap package),
    ENCAP="$enableval",
    ENCAP=no)

AC_SUBST(XCODE)
AC_ARG_ENABLE(xcode,
    AC_HELP_STRING(--enable-xcode,create Macaulay2/d/interpret.a for use with xcode),
    XCODE="$enableval",
    XCODE="no"
    )

AC_SUBST(ALTIVEC)
AC_ARG_ENABLE(altivec,
    AC_HELP_STRING(--enable-altivec,compile with "-faltivec" option),
    ALTIVEC="$enableval",
    ALTIVEC="no"
    )

LAPACK=yes
AC_SUBST(LAPACK)
AC_DEFINE(LAPACK,,whether to use LAPACK)

AC_SUBST(M2SUFFIX)
AC_ARG_WITH(M2suffix,
    AC_HELP_STRING(--with-M2suffix=...,[suffix to append to executable name M2]),
    M2SUFFIX="$withval")
AC_DEFINE_UNQUOTED(M2SUFFIX,"$M2SUFFIX",[suffix to append to executable name M2])

AC_DEFINE_UNQUOTED(EXEEXT,"$EXEEXT",[suffix the compiler appends to executable filenames])

AC_SUBST(LAPACKLIBS)
AC_ARG_WITH(lapacklibs,
    AC_HELP_STRING(--with-lapacklibs=...,lapack libraries (-llapack -lcblas -lf77blas -lg2c)),
    LAPACKLIBS="$withval",
    LAPACKLIBS="-llapack -lcblas -lf77blas -lg2c"
    )
AC_ARG_WITH(veclib,
    AC_HELP_STRING(--with-veclib,replace lapack libraries by "-framework vecLib"),
    LAPACKLIBS="-framework vecLib"
    )

AC_CHECK_DECL(__finite,,AC_DEFINE(__finite,finite,whether __finite is missing),[#include <math.h>])
AC_CHECK_DECL(__isnan,,AC_DEFINE(__isnan,isnan,whether __isnan is missing),[#include <math.h>])
AC_CHECK_DECL(__isinf,,AC_DEFINE(__isinf,isinf,whether __isinf is missing),[#include <math.h>])
AC_CHECK_DECLS(sys_nerr)

SAVELIBS="$LIBS"

AC_MSG_CHECKING([whether gc library is installed])
LIBS="-lgc -ldl $SAVELIBS"
AC_TRY_LINK(
    [#include <gc.h>],
    [GC_malloc(1);],
    ,AC_MSG_RESULT([no]);AC_MSG_ERROR([missing library: libgc]))
AC_MSG_RESULT([yes])

AC_MSG_CHECKING([whether gc library, profiling version, is installed (-lgc-pg)])
AC_SUBST(HAVE_LIBGC_PG)
LIBS="-lgc-pg -ldl $SAVELIBS"
AC_TRY_LINK(
    [#include <gc.h>],
    [GC_malloc(1);],
    HAVE_LIBGC_PG=yes;AC_MSG_RESULT([yes]),
    HAVE_LIBGC_PG=no;AC_MSG_RESULT([no]))

AC_MSG_CHECKING([whether gmp library is installed])
LIBS="-lgmp $SAVELIBS"
AC_TRY_LINK(
    [#include <gmp.h>],
    [return *__gmp_version;],
    ,AC_MSG_RESULT([no]);AC_MSG_ERROR([missing library: gmp]))
AC_MSG_RESULT([yes])

AC_MSG_CHECKING([whether gmp library, profiling version, is installed (-lgmp-pg)])
AC_SUBST(HAVE_LIBGMP_PG)
LIBS="-lgmp-pg $SAVELIBS"
AC_TRY_LINK(
    [#include <gmp.h>],
    [return *__gmp_version;],
    HAVE_LIBGMP_PG=yes;AC_MSG_RESULT([yes]),
    HAVE_LIBGMP_PG=no;AC_MSG_RESULT([no]))

AC_MSG_CHECKING([whether gdbm library is installed])
LIBS="-lgdbm $SAVELIBS"
AC_TRY_LINK([#include <gdbm.h>],
    [gdbm_close(0);],
    ,AC_MSG_RESULT([no]);AC_MSG_ERROR([missing library: gdbm]))
AC_MSG_RESULT([yes])

AC_MSG_CHECKING([whether ntl library is installed])
LIBS="-lntl $SAVELIBS"
AC_LANG(C++)
AC_TRY_LINK(
    [#include <NTL/tools.h>],
    [_ntl_GetTime();],
    AC_MSG_RESULT([yes]),
    AC_MSG_RESULT([no]);AC_MSG_ERROR([missing library: ntl (-lntl)]))

AC_MSG_CHECKING([whether ntl library, profiling version, is installed (-lntl-pg)])
AC_SUBST(HAVE_LIBNTL_PG)
LIBS="-lntl-pg $SAVELIBS"
AC_LANG(C++)
AC_TRY_LINK(
    [#include <NTL/tools.h>],
    [_ntl_GetTime();],
    HAVE_LIBNTL_PG=yes;AC_MSG_RESULT([yes]),
    HAVE_LIBNTL_PG=no;AC_MSG_RESULT([no]))

AC_MSG_CHECKING([whether factory library is installed])
LIBS="-lcf $SAVELIBS"
AC_LANG(C++)
AC_TRY_LINK(
    [#include <factoryconf.h>],
    [extern const char factoryVersion[]; return *factoryVersion;],
    AC_MSG_RESULT([yes]),
    AC_MSG_RESULT([no]);AC_MSG_ERROR([missing library: factory (-lcf)]))

AC_MSG_CHECKING([whether factory library, profiling version, is installed (-lcf-pg)])
AC_SUBST(HAVE_LIBCF_PG)
LIBS="-lcf-pg $SAVELIBS"
AC_LANG(C++)
AC_TRY_LINK(
    [#include <factoryconf.h>],
    [extern const char factoryVersion[]; return *factoryVersion;],
    HAVE_LIBCF_PG=yes;AC_MSG_RESULT([yes]),
    HAVE_LIBCF_PG=no;AC_MSG_RESULT([no]))

AC_MSG_CHECKING([whether libfac library is installed])
LIBS="-lfac -lcf $SAVELIBS"
AC_LANG(C++)
AC_TRY_LINK(
    [extern const char * libfac_version;],
    [return *libfac_version;],
    AC_MSG_RESULT([yes]),
    AC_MSG_RESULT([no]);AC_MSG_ERROR([missing library: libfac (-lfac)]))

AC_MSG_CHECKING([whether libfac library, profiling version, is installed (-lfac-pg)])
AC_SUBST(HAVE_LIBFAC_PG)
LIBS="-lfac-pg -lcf $SAVELIBS"
AC_LANG(C++)
AC_TRY_LINK(
    [extern const char * libfac_version;],
    [return *libfac_version;],
    HAVE_LIBFAC_PG=yes;AC_MSG_RESULT([yes]),
    HAVE_LIBFAC_PG=no;AC_MSG_RESULT([no]))

AC_MSG_CHECKING([whether readline library is installed])
LIBS="-lreadline -lhistory -lncurses $SAVELIBS"
AC_LANG(C)
AC_TRY_LINK(
    [extern char *readline(const char *);extern void add_history (const char *);],
    [readline;add_history;],
    ,AC_MSG_RESULT([no]);AC_MSG_ERROR([missing library: readline]))
AC_MSG_RESULT([yes])

LIBS="$SAVELIBS"

AC_CHECK_HEADERS(wordexp.h)
AC_CHECK_FUNCS(wordexp)

dnl -- M2_SHOW_CONFDEFS()

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
