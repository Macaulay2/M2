AC_INIT(Macaulay 2, 0.9.8, Macaulay2@math.uiuc.edu, Macaulay2)

case "$0" in
  *" "*) CONFIG_ARG0="\"$0\"" ;;
  *) CONFIG_ARG0="$0" ;;
esac
for i in "$@"
do case "$i" in
     "--no-create"|"--no-recursion") ;;
     *" "*) 
	  CONFIG_ARGS="$CONFIG_ARGS \"$i\"" 
	  CONFIG_ARGS_C="$CONFIG_ARGS_C \"$i\"," 
	  ;;
     *) 
	  CONFIG_ARGS="$CONFIG_ARGS $i" 
	  CONFIG_ARGS_C="$CONFIG_ARGS_C \"$i\"," 
	  ;;
   esac
done
CONFIG_CMD="$CONFIG_ARG0 $CONFIG_ARGS"
# test "$LOADLIBES" && CONFIG_ARGS="LOADLIBES=\"$LOADLIBES\" $CONFIG_ARGS"
# test "$LDLIBS" && CONFIG_ARGS="LDLIBS=\"$LDLIBS\" $CONFIG_ARGS"
# test "$LDFLAGS" && CONFIG_ARGS="LDFLAGS=\"$LDFLAGS\" $CONFIG_ARGS"
# test "$CXXFLAGS" && CONFIG_ARGS="CXXFLAGS=\"$CXXFLAGS\" $CONFIG_ARGS"
# test "$CXX" && CONFIG_ARGS="CXX=\"$CXX\" $CONFIG_ARGS"
# test "$CPPFLAGS" && CONFIG_ARGS="CPPFLAGS=\"$CPPFLAGS\" $CONFIG_ARGS"
# test "$CFLAGS" && CONFIG_ARGS="CFLAGS=\"$CFLAGS\" $CONFIG_ARGS"
# test "$CC" && CONFIG_ARGS="CC=\"$CC\" $CONFIG_ARGS"
echo "# invocation: $CONFIG_CMD" >&5
AC_SUBST(CONFIG_ARG0)
AC_SUBST(CONFIG_ARGS)
AC_SUBST(CONFIG_CMD)
AC_DEFINE_UNQUOTED(CONFIG_ARGS,$CONFIG_ARGS_C,arguments used for configure)

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
if test ! "$INSTALL_INFO"
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

AC_SUBST(PROFILING)
AC_ARG_ENABLE(profile,
    AC_HELP_STRING(--enable-profile,enable profiling (and disable stripping)),
    PROFILING="$enableval",
    PROFILING=no)
if test "$PROFILING" = yes
then AC_DEFINE(PROFILING,,[whether profiling has been enabled])
fi

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

AC_SUBST(DEBUG)
AC_ARG_ENABLE(debug,
    AC_HELP_STRING(--enable-debug,enable debugging (and disable stripping)),
    DEBUG="$enableval",
    DEBUG=no)

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

AC_SUBST(M2SUFFIX)
AC_ARG_WITH(M2suffix,
    AC_HELP_STRING(--with-M2suffix=...,[suffix to append to executable name M2]),
    M2SUFFIX="$withval")
AC_DEFINE_UNQUOTED(M2SUFFIX,"$M2SUFFIX",[suffix to append to executable name M2])

AC_DEFINE_UNQUOTED(EXEEXT,"$EXEEXT",[suffix the compiler appends to executable filenames])

AC_SUBST(LAPACKLIBS)
# LAPACKLIBS="-llapack -lcblas -lf77blas -lg2c" was our former default
# LAPACKLIBS="-llapack -lblas -lg2c" is our new default, because redhat provides lapack this way now
AC_ARG_WITH(lapacklibs,
    AC_HELP_STRING([--with-lapacklibs=...],[lapack libraries (-llapack -lblas -lg2c)]),
    LAPACKLIBS="$withval",
    LAPACKLIBS="-llapack -lblas -lg2c")
AC_ARG_WITH(veclib,
    AC_HELP_STRING(--with-veclib,replace lapack libraries by "-framework vecLib"),
    LAPACKLIBS="-framework vecLib")

AC_SUBST(LAPACK)
AC_ARG_ENABLE(lapack,
    AC_HELP_STRING(--disable-lapack,[disable linking with lapack]),
    LAPACK="$enableval";LAPACKLIBS="",
    LAPACK=yes)
if test "$LAPACK" = yes
then AC_DEFINE(LAPACK,,whether to link with lapack)
fi

AC_SUBST(GDBMLIB)
AC_ARG_WITH(gdbmlib,
    AC_HELP_STRING(--with-gdbmlib=...,gdbm library (-lgdbm)),
    GDBMLIB="$withval",
    GDBMLIB="-lgdbm"
    )

AC_SUBST(READLINELIB)
AC_ARG_WITH(readlinelib,
    AC_HELP_STRING(--with-readlinelib=...,readline library (-lreadline)),
    READLINELIB="$withval",
    READLINELIB="-lreadline"
    )

AC_SUBST(HISTORYLIB)
AC_ARG_WITH(historylib,
    AC_HELP_STRING(--with-historylib=...,history library (-lhistory)),
    HISTORYLIB="$withval",
    HISTORYLIB="-lhistory"
    )

AC_CHECK_DECL(__finite,,AC_DEFINE(__finite,finite,whether __finite is missing),[#include <math.h>])
AC_CHECK_DECL(__isnan,,AC_DEFINE(__isnan,isnan,whether __isnan is missing),[#include <math.h>])
AC_CHECK_DECL(__isinf,,AC_DEFINE(__isinf,isinf,whether __isinf is missing),[#include <math.h>])
AC_CHECK_DECLS(sys_nerr)

SAVELIBS="$LIBS"

GCLIB=-lgc
AC_SUBST(GCLIB)
AC_ARG_WITH(gclib, AC_HELP_STRING(--with-gclib=...,gc library (-lgc)), GCLIB="$withval")
AC_MSG_CHECKING([whether gc library is installed ($GCLIB)])
LIBS="$GCLIB -lpthread -ldl $SAVELIBS"
AC_TRY_LINK( [#include <gc.h>], [GC_malloc(1);], HAVE_LIBGC=yes;AC_MSG_RESULT([yes]), HAVE_LIBGC=no;AC_MSG_RESULT([no]))

HAVE_LIBGC_PG=no
if test "$PROFILING" = yes -a "$GCLIB" = "-lgc"
then AC_MSG_CHECKING([whether gc library, profiling version, is installed (-lgc-pg)])
     LIBS="-lgc-pg -lpthread -ldl $SAVELIBS"
     AC_TRY_LINK( [#include <gc.h>], [GC_malloc(1);], HAVE_LIBGC_PG=yes;AC_MSG_RESULT([yes]), AC_MSG_RESULT([no]))
fi

if   test "$PROFILING" = yes
then test "$HAVE_LIBGC_PG" = no -a "$HAVE_LIBGC" = no && AC_MSG_ERROR([missing libraries: libgc (and libgc-pg)])
     test "$HAVE_LIBGC_PG" = yes && GCLIB=-lgc-pg
else test "$HAVE_LIBGC" = no && AC_MSG_ERROR([missing library: libgc])
fi

GMPLIB="-lgmp"
AC_SUBST(GMPLIB)
AC_ARG_WITH(gmplib, AC_HELP_STRING(--with-gmplib=...,gmp library (-lgmp)), GMPLIB="$withval")
AC_MSG_CHECKING([whether gmp library is installed ($GMPLIB)])
LIBS="$GMPLIB $SAVELIBS"
AC_TRY_LINK( [#include <gmp.h>], [return *__gmp_version;], AC_MSG_RESULT([yes]), AC_MSG_RESULT([no]))

HAVE_LIBGMP_PG=no
if test "$PROFILING" = yes -a "$GMPLIB" = "-lgmp"
then AC_MSG_CHECKING([whether gmp library, profiling version, is installed (-lgmp-pg)])
     LIBS="-lgmp-pg $SAVELIBS"
     AC_TRY_LINK( [#include <gmp.h>], [return *__gmp_version;], HAVE_LIBGMP_PG=yes;AC_MSG_RESULT([yes]), AC_MSG_RESULT([no]))
fi

if   test "$PROFILING" = yes
then test "$HAVE_LIBGMP_PG" = no -a "$HAVE_LIBGMP" = no && AC_MSG_ERROR([missing libraries: libgmp (and libgmp-pg)])
     test "$HAVE_LIBGMP_PG" = yes && GMPLIB=-lgmp-pg
else test "$HAVE_LIBGMP" = no && AC_MSG_ERROR([missing library: libgmp])
fi

AC_MSG_CHECKING([whether gdbm library is installed])
LIBS="-lgdbm $SAVELIBS"
AC_TRY_LINK([#include <gdbm.h>], [gdbm_close(0);], AC_MSG_RESULT([yes]), AC_MSG_RESULT([no]);AC_MSG_ERROR([missing library: gdbm]))

NTLLIB=-lntl
AC_SUBST(NTLLIB)
AC_ARG_WITH(ntllib, AC_HELP_STRING(--with-ntllib=...,ntl library (-lntl)), NTLLIB="$withval")
AC_MSG_CHECKING([whether ntl library is installed])
LIBS="$NTLLIB -lgmp $SAVELIBS"
AC_LANG(C++)
AC_TRY_LINK( [#include <NTL/tools.h>], [_ntl_GetTime();], HAVE_LIBNTL=yes;AC_MSG_RESULT([yes]), HAVE_LIBNTL=no;AC_MSG_RESULT([no]))

HAVE_LIBNTL_PG=no
if test "$PROFILING" = yes -a "$NTLLIB" = "-lntl"
then AC_MSG_CHECKING([whether ntl library, profiling version, is installed (-lntl-pg)])
     LIBS="-lntl-pg -lgmp $SAVELIBS"
     AC_LANG(C++)
     AC_TRY_LINK( [#include <NTL/tools.h>], [_ntl_GetTime();], HAVE_LIBNTL_PG=yes;AC_MSG_RESULT([yes]), AC_MSG_RESULT([no]))
fi

if   test "$PROFILING" = yes
then test "$HAVE_LIBNTL_PG" = no -a "$HAVE_LIBNTL" = no && AC_MSG_ERROR([missing libraries: libntl (and libntl-pg)])
     test "$HAVE_LIBNTL_PG" = yes && NTLLIB=-lntl-pg
else test "$HAVE_LIBNTL" = no && AC_MSG_ERROR([missing library: libntl])
fi

LIBCF=-lcf
AC_SUBST(LIBCF)
AC_MSG_CHECKING([whether factory library is installed])
LIBS="$LIBCF $SAVELIBS"
AC_LANG(C++)
AC_TRY_LINK( [#include <factoryconf.h>], [extern const char factoryVersion[]; return *factoryVersion;], HAVE_LIBCF=yes;AC_MSG_RESULT([yes]), HAVE_LIBCF=no;AC_MSG_RESULT([no]))

AC_MSG_CHECKING([whether factory library, profiling version, is installed (-lcf-pg)])
AC_SUBST(HAVE_LIBCF_PG)
LIBS="-lcf-pg $SAVELIBS"
AC_LANG(C++)
AC_TRY_LINK( [#include <factoryconf.h>], [extern const char factoryVersion[]; return *factoryVersion;], HAVE_LIBCF_PG=yes;AC_MSG_RESULT([yes]), HAVE_LIBCF_PG=no;AC_MSG_RESULT([no]))

if   test "$PROFILING" = yes
then test "$HAVE_LIBCF_PG" = no -a "$HAVE_LIBCF" = no && AC_MSG_ERROR([missing libraries: libcf (and libcf-pg)])
     test "$HAVE_LIBCF_PG" = yes && LIBCF=-lcf-pg
else test "$HAVE_LIBCF" = no && AC_MSG_ERROR([missing library: libcf])
fi

LIBFAC=-lfac
AC_SUBST(LIBFAC)
AC_MSG_CHECKING([whether libfac library is installed ($LIBFAC)])
LIBS="$LIBCF $LIBFAC $SAVELIBS"
AC_LANG(C++)
AC_TRY_LINK( [extern const char * libfac_version;], [return *libfac_version;], HAVE_LIBFAC=yes;AC_MSG_RESULT([yes]), HAVE_LIBFAC=no;AC_MSG_RESULT([no]))

AC_MSG_CHECKING([whether libfac library, profiling version, is installed (-lfac-pg)])

LIBS="-lfac-pg $LIBCF $SAVELIBS"
AC_LANG(C++)
AC_TRY_LINK( [extern const char * libfac_version;], [return *libfac_version;], HAVE_LIBFAC_PG=yes;AC_MSG_RESULT([yes]), HAVE_LIBFAC_PG=no;AC_MSG_RESULT([no]))

if   test "$PROFILING" = yes
then test "$HAVE_LIBFAC_PG" = no -a "$HAVE_LIBFAC" = no && AC_MSG_ERROR([missing libraries: libfac (and libfac-pg)])
     test "$HAVE_LIBFAC_PG" = yes && LIBFAC=-lfac-pg
else test "$HAVE_LIBFAC" = no && AC_MSG_ERROR([missing library: libfac])
fi

AC_MSG_CHECKING([whether readline library is installed])
LIBS="-lreadline -lhistory -lncurses $SAVELIBS"
AC_LANG(C)
AC_TRY_LINK(
    [extern char *readline(const char *);extern void add_history (const char *);],
    [readline;add_history;],
    AC_MSG_RESULT([yes]),
    AC_MSG_RESULT([no]);AC_MSG_ERROR([missing library: readline]))

if test "$LAPACK" = "yes"
then AC_MSG_CHECKING([whether lapack library is installed])
     LIBS="$LAPACKLIBS $SAVELIBS"
     AC_LANG(C)
     AC_TRY_LINK(
	 [],
	 [dgetrf_();],
	 AC_MSG_RESULT([yes]),
	 AC_MSG_RESULT([no]);AC_MSG_ERROR([missing library: lapack ($LAPACKLIBS)]))
fi

LIBS="$SAVELIBS"

AC_CHECK_HEADERS(wordexp.h)
AC_CHECK_FUNCS(wordexp)

AC_CHECK_HEADERS(linux/personality.h)
AC_CHECK_FUNCS(personality)
AC_CHECK_DECLS(PER_LINUX,,,[#include <linux/personality.h>])

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
