AC_INIT(Macaulay 2, 0.9.8, Macaulay2@math.uiuc.edu, Macaulay2)

case "$0" in
  *" "*) CONFIG_ARG0="\"$0\"" ;;
  *) CONFIG_ARG0="$0" ;;
esac
for i in "$@"
do case "$i" in
     "--no-create"|"--no-recursion") ;;
     *" "*) CONFIG_ARGS="$CONFIG_ARGS \"$i\"" CONFIG_ARGS_C="$CONFIG_ARGS_C \"$i\"," ;;
     *) CONFIG_ARGS="$CONFIG_ARGS $i" CONFIG_ARGS_C="$CONFIG_ARGS_C \"$i\"," ;;
   esac
done
CONFIG_CMD="$CONFIG_ARG0 $CONFIG_ARGS"
echo "# invocation: $CONFIG_CMD" >&5

AC_CONFIG_HEADERS(include/config.h)

AC_SUBST(CONFIG_ARG0)
AC_SUBST(CONFIG_ARGS)
AC_SUBST(CONFIG_CMD)
AC_DEFINE_UNQUOTED(CONFIG_ARGS,$CONFIG_ARGS_C,arguments used for configure)

define(TO_UPPER,[translit($1, [a-z], [A-Z])])

AC_DEFUN(IS_DECLARED,[
  AC_CACHE_CHECK(whether $2 is declared, m2_cv_$2_is_declared,
    AC_TRY_COMPILE($3, [$1 x = $2], [m2_cv_$2_is_declared=yes], [m2_cv_$2_is_declared=no]))
  test $m2_cv_$2_is_declared = no; val=$?
  AC_DEFINE_UNQUOTED(TO_UPPER($2_IS_DECLARED),$val,[whether $2 is declared in errno.h or stdio.h])
])

AC_DEFUN(SHOW_CONFDEFS,[
  echo contents of confdefs.h: >&6
  sed -e '/^$/d' -e 's/^/   /' confdefs.h >&6
  ])

AC_CONFIG_FILES(m4_include(config/files))

echo "any environment variables set:"
test "$CC"         && echo "   CC=$CC"
test "$CXX"        && echo "   CXX=$CXX"
test "$CPPFLAGS"   && echo "   CPPFLAGS=$CPPFLAGS"                 # autoconf default is: -g -O2
test "$CFLAGS"     && echo "   CFLAGS=$CFLAGS"                     # autoconf default is: -g -O2
test "$CXXFLAGS"   && echo "   CXXFLAGS=$CXXFLAGS"
test "$LDFLAGS"    && echo "   LDFLAGS=$LDFLAGS"
test "$LOADLIBES"  && echo "   LOADLIBES=$LOADLIBES"
test "$LDLIBS"     && echo "   LDLIBS=$LDLIBS"

for i in $CFLAGS
do case $i in
   -I*) echo "error: preprocessor flag $i found in CFLAGS instead of in CPPFLAGS" >&2 ; exit 1 ; break ;;
   -L*) echo "error: link flag $i found in CFLAGS instead of in LDFLAGS" >&2 ; exit 1 ; break ;;
   esac
done

for i in $CXXFLAGS
do case $i in
   -I*) echo "error: preprocessor flag $i found in CXXFLAGS instead of in CPPFLAGS" >&2 ; exit 1 ; break ;;
   -L*) echo "error: link flag $i found in CXXFLAGS instead of in LDFLAGS" >&2 ; exit 1 ; break ;;
   esac
done

for i in $CPPFLAGS
do case $i in
   -L*) echo "error: link flag $i found in CPPFLAGS instead of in LDFLAGS" >&2 ; exit 1 ; break ;;
   esac
done

for i in $LDFLAGS
do case $i in
   -I*) echo "error: preprocessor flag $i found in LDFLAGS instead of in CPPFLAGS" >&2 ; exit 1 ; break ;;
   esac
done

if test "$CXX" = gcc
then echo oops, CXX=gcc, should use CXX=g++ >&2
     exit 1
fi

AC_ARG_VAR(CFLAGS,[C compiler flags, default: -g -O2])
AC_ARG_VAR(LOADLIBES,[first list of libraries to link against])
AC_ARG_VAR(LDLIBS,[second list of libraries to link against])
LIBS="$LOADLIBES $LDLIBS"

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
AC_ARG_VAR(CC,C compiler to use)
AC_ARG_VAR(CXX,C++ compiler to use)
AC_PROG_RANLIB()
AC_PROG_INSTALL()
AC_PROG_AWK()
AC_PROG_CC()
AC_SUBST(GCC)
AC_PROG_CXX()
AC_PROG_YACC()

AC_MSG_CHECKING([whether linking C++ programs with gcc works])
cat >unwindtest-main.cpp <<EOF
#include "unwindtest-monomials.hpp"
int main(int argc, char **argv) { Monomials *M = new Monomials(); return 1; }
EOF
cat >unwindtest-monomials.hpp <<EOF
class Monomials { public: Monomials(); };
EOF
cat >unwindtest-monomials.cpp <<EOF
#include "unwindtest-monomials.hpp"
Monomials::Monomials() {}
EOF
if   (  set -x
	$CXX $CPPFLAGS $CXXFLAGS -O3 -c -o unwindtest-main.o unwindtest-main.cpp &&
	$CXX $CPPFLAGS $CXXFLAGS -O3 -c -o unwindtest-monomials.o unwindtest-monomials.cpp &&
	$CC $LDFLAGS unwindtest-main.o unwindtest-monomials.o $LOADLIBES $LDLIBS -lstdc++ -o unwindtest-main
     ) >unwindtest-log 2>&1
then AC_MSG_RESULT(yes); rm -f unwindtest-*
else AC_MSG_RESULT(no);  AC_MSG_ERROR([failure: linking C++ program with $CC, see file unwindtest-log.])
fi

AC_HEADER_TIME() dnl # time.h sys/time.h
AC_CHECK_HEADERS(sys/ioctl.h termios.h sys/mman.h sys/socket.h netdb.h netinet/in.h arpa/inet.h sys/time.h sys/wait.h sys/resource.h io.h linux/personality.h)
AC_CHECK_FUNCS([personality sys_errlist herror error socket gnu_get_libc_version __environ _environ environ _setmode getaddrinfo])
AC_CHECK_DECLS([ADDR_NO_RANDOMIZE],,,[#include <linux/personality.h>])
AC_CHECK_DECLS([sys_errlist,sys_nerr,herror,gnu_get_libc_version])
AC_FUNC_ALLOCA()

AC_SUBST(STRIP)      AC_ARG_ENABLE(strip, AS_HELP_STRING(--disable-strip,do not strip the symbol table from the Macaulay 2 binary), STRIP="$enableval", STRIP=yes)
AC_SUBST(VERBOSE)    AC_ARG_ENABLE(verbose, AS_HELP_STRING(--enable-verbose,enable verbose memory allocation), VERBOSE="$enableval", VERBOSE=no)
AC_SUBST(STATIC)     AC_ARG_ENABLE(static, AS_HELP_STRING(--enable-static,enable static linking), STATIC="$enableval", STATIC=no)
AC_SUBST(MEMDEBUG)   AC_ARG_ENABLE(memdebug, AS_HELP_STRING(--enable-memdebug,enable memory allocation debugging), MEMDEBUG="$enableval", MEMDEBUG=no)
AC_SUBST(OPTIMIZE)   AC_ARG_ENABLE(optimize, AS_HELP_STRING(--disable-optimize,disable optimization), OPTIMIZE="$enableval", OPTIMIZE=yes)
AC_SUBST(DEBUG)      AC_ARG_ENABLE(debug, AS_HELP_STRING(--enable-debug,enable debugging (and disable stripping)), DEBUG="$enableval", DEBUG=no)
AC_SUBST(ENCAP)      AC_ARG_ENABLE(encap, AS_HELP_STRING(--disable-encap,do not encapsulate file in a subdirectory), ENCAP="$enableval", ENCAP=yes)
AC_SUBST(XCODE)      AC_ARG_ENABLE(xcode, AS_HELP_STRING(--enable-xcode,create Macaulay2/d/interpret.a for use with xcode), XCODE="$enableval", XCODE="no" )
AC_SUBST(ALTIVEC)    AC_ARG_ENABLE(altivec, AS_HELP_STRING(--enable-altivec,compile with "-faltivec" option), ALTIVEC="$enableval", ALTIVEC="no" )
AC_SUBST(DUMPDATA)   AC_ARG_ENABLE(dumpdata, AS_HELP_STRING(--disable-dumpdata,do not cache data with dumpdata), DUMPDATA="$enableval", DUMPDATA=yes)
AC_SUBST(PROFILING)  AC_ARG_ENABLE(profile, AS_HELP_STRING(--enable-profile,enable profiling (and disable stripping)), PROFILING="$enableval", PROFILING=no)
AC_SUBST(COMPRESS)   AC_ARG_ENABLE(compress, AS_HELP_STRING([--enable-compress=[gz|bz2]],compression method for tarball), COMPRESS="$enableval", COMPRESS=gz)
AC_SUBST(EXPERIMENT) AC_ARG_ENABLE(experimental-code, AS_HELP_STRING(--enable-experimental-code,enable experimental code), EXPERIMENT="$enableval", EXPERIMENT=no)

test "$COMPRESS" != "gz" -a "$COMPRESS" != "bz2" && AC_MSG_ERROR(unrecognized option for enable-compress)

AC_SUBST(CYGWIN)    AC_ARG_ENABLE(cygwin,  AS_HELP_STRING(--enable-cygwin,prepare a binary package for cygwin (disables encap and enables strip)), CYGWIN="$enableval", CYGWIN=no)
if test "$CYGWIN" = yes ; then ENCAP=no ; STRIP=yes ; fi

AC_SUBST(M2SUFFIX)
test "$program_suffix" != NONE && M2SUFFIX="$program_suffix"
AC_ARG_ENABLE(dummy, AS_HELP_STRING(--program-suffix=...,suffix to append to executable name M2))

test "$DUMPDATA" = no;   val=$?; AC_DEFINE_UNQUOTED(DUMPDATA,  $val,whether to use dumpdata)
test "$PROFILING" = no;  val=$?; AC_DEFINE_UNQUOTED(PROFILING, $val,whether profiling has been enabled)
test "$FACTORY" = no;    val=$?; AC_DEFINE_UNQUOTED(FACTORY,   $val,whether to use the factory library)
test "$EXPERIMENT" = no; val=$?; AC_DEFINE_UNQUOTED(EXPERIMENT,$val,whether experimental code has been enabled)

AC_SUBST(GDBMLIB)     AC_ARG_WITH(gdbmlib, AS_HELP_STRING(--with-gdbmlib=...,gdbm library (-lgdbm)), GDBMLIB="$withval", GDBMLIB="-lgdbm" )
AC_SUBST(READLINELIB) AC_ARG_WITH(readlinelib, AS_HELP_STRING(--with-readlinelib=...,readline library (-lreadline)), READLINELIB="$withval", READLINELIB="-lreadline" )
AC_SUBST(HISTORYLIB)  AC_ARG_WITH(historylib, AS_HELP_STRING(--with-historylib=...,history library (-lhistory)), HISTORYLIB="$withval", HISTORYLIB="-lhistory")
AC_SUBST(FACTORY)     AC_ARG_WITH(factory, AS_HELP_STRING(--without-factory,disable the use of the 'factory' library), FACTORY="$enableval",FACTORY=yes)

AC_DEFINE(M2_CONFIG_H,1,a macro definition to ensure our config.h was the one loaded)

AC_DEFINE_UNQUOTED(M2SUFFIX,"$M2SUFFIX",[suffix to append to executable name M2])

WITH_NEWLINE_CR=0
WITH_NEWLINE_CRLF=0
AC_ARG_WITH(newline, AS_HELP_STRING([--with-newline=...], [crlf, cr, or lf (the default)]),
    [ case "$withval" in
	  crlf) WITH_NEWLINE_CRLF=1 WITH_NEWLINE_CR=0 ;;
	  cr)   WITH_NEWLINE_CR=1 WITH_NEWLINE_CRLF=0 ;;
	  lf)   WITH_NEWLINE_CR=0 WITH_NEWLINE_CRLF=0 ;;
	  *)    AC_MSG_ERROR([--with-newline expected crlf, cr, or lf]) ;;
      esac ])
AC_DEFINE_UNQUOTED(WITH_NEWLINE_CRLF,$WITH_NEWLINE_CRLF,[whether newline is cr lf])
AC_DEFINE_UNQUOTED(WITH_NEWLINE_CR, $WITH_NEWLINE_CR,   [whether newline is cr])

AC_DEFINE_UNQUOTED(EXEEXT,"$EXEEXT",[suffix the compiler appends to executable filenames])

AC_SUBST(LAPACKLIBS)
dnl # LAPACKLIBS="-llapack -lcblas -lf77blas -lg2c" was our former default
dnl # LAPACKLIBS="-llapack -lblas -lg2c" is our new default, because redhat provides lapack this way now
AC_ARG_WITH(lapacklibs, AS_HELP_STRING([--with-lapacklibs=...],[lapack libraries (-llapack -lblas -lg2c)]), LAPACKLIBS="$withval", LAPACKLIBS="-llapack -lblas -lg2c")
AC_ARG_WITH(veclib, AS_HELP_STRING(--with-veclib,replace lapack libraries by "-framework vecLib"), LAPACKLIBS="-framework vecLib")
AC_SUBST(LAPACK)
AC_ARG_WITH(lapack, AS_HELP_STRING(--without-lapack,[compile and link without lapack]), LAPACK="$enableval";LAPACKLIBS="", LAPACK=yes)
test "$LAPACK" = no; val=$?
AC_DEFINE_UNQUOTED(LAPACK,$val,whether to link with lapack)

SAVELIBS="$LIBS"

LIBS=-ldl
AC_MSG_CHECKING(whether libdl exists (-ldl))
AC_TRY_LINK([],[],LIBDL=-ldl;AC_MSG_RESULT([yes]), LIBDL=; AC_MSG_RESULT([no]))

LIBS=-lpthread
AC_MSG_CHECKING(whether libpthread exists (-lpthread))
AC_TRY_LINK([],[],LIBPTHREAD=-lpthread;AC_MSG_RESULT([yes]), LIBPTHREAD=; AC_MSG_RESULT([no]))

GCLIB="-lgc $LIBDL $LIBPTHREAD"
AC_SUBST(GCLIB)
AC_ARG_WITH(gclib, AS_HELP_STRING(--with-gclib=...,gc library (-lgc)), GCLIB="$withval")
LIBS="$GCLIB $SAVELIBS"
AC_MSG_CHECKING([whether gc library (-lgc) is installed (using $LIBS)])
AC_TRY_LINK( [#include <gc.h>], [GC_malloc(1);], HAVE_LIBGC=yes;AC_MSG_RESULT([yes]), HAVE_LIBGC=no;AC_MSG_RESULT([no]))

HAVE_LIBGC_PG=no
if test "$PROFILING" = yes -a "$GCLIB" = "-lgc"
then AC_MSG_CHECKING([whether gc library, profiling version, is installed (-lgc-pg)])
     LIBS="-lgc-pg $LIBPTHREAD $LIBDL $SAVELIBS"
     AC_TRY_LINK( [#include <gc.h>], [GC_malloc(1);], HAVE_LIBGC_PG=yes;AC_MSG_RESULT([yes]), AC_MSG_RESULT([no]))
fi

if   test "$PROFILING" = yes
then test "$HAVE_LIBGC_PG" = no -a "$HAVE_LIBGC" = no && AC_MSG_ERROR([missing libraries: libgc (and libgc-pg)])
     test "$HAVE_LIBGC_PG" = yes && GCLIB=-lgc-pg
else test "$HAVE_LIBGC" = no && AC_MSG_ERROR([missing library: libgc])
fi

GMPLIB="-lgmp"
AC_SUBST(GMPLIB)
AC_ARG_WITH(gmplib, AS_HELP_STRING(--with-gmplib=...,gmp library (-lgmp)), GMPLIB="$withval")
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
AC_ARG_WITH(ntllib, AS_HELP_STRING(--with-ntllib=...,ntl library (-lntl)), NTLLIB="$withval")
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

AC_MSG_CHECKING([whether (and where) the libfac library is installed (-lfac or -lsingfac)])
AC_SUBST(LIBFAC)
AC_LANG(C++)
LIBFAC=-lfac
LIBS="$LIBCF $LIBFAC $SAVELIBS"
AC_TRY_LINK(
 [extern const char * libfac_version;],
 [return *libfac_version;],
 HAVE_LIBFAC=yes;AC_MSG_RESULT([yes ($LIBFAC)]),
 LIBFAC=-lsingfac
 LIBS="$LIBCF $LIBFAC $SAVELIBS"
 AC_TRY_LINK(
  [extern const char * libfac_version;],
  [return *libfac_version;],
  HAVE_LIBFAC=yes;AC_MSG_RESULT([yes ($LIBFAC)]),
  HAVE_LIBFAC=no;AC_MSG_RESULT([no])
 )
)

AC_MSG_CHECKING([whether libfac library, profiling version, is installed ($LIBFAC-pg)])

LIBS="$LIBFAC-pg $LIBCF $SAVELIBS"
AC_LANG(C++)
AC_TRY_LINK( [extern const char * libfac_version;], [return *libfac_version;], HAVE_LIBFAC_PG=yes;AC_MSG_RESULT([yes]), HAVE_LIBFAC_PG=no;AC_MSG_RESULT([no]))

if   test "$PROFILING" = yes
then test "$HAVE_LIBFAC_PG" = no -a "$HAVE_LIBFAC" = no && AC_MSG_ERROR([missing libraries: libfac (and libfac-pg)])
     test "$HAVE_LIBFAC_PG" = yes && LIBFAC=$LIBFAC-pg
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

dnl SHOW_CONFDEFS()

IS_DECLARED(char **,sys_errlist, [
      #include <stdio.h>
      #include <errno.h>
      ])
IS_DECLARED(char **,h_errlist, [
      #include <stdio.h>
      #include <errno.h>
      ])

CONFIGURED_FILES="$ac_config_files"
AC_SUBST(CONFIGURED_FILES)
AC_OUTPUT()

dnl # Local Variables:
dnl # mode: sh
dnl # compile-command: "make -f Makefile"
dnl # End:
