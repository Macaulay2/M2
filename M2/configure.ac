AC_INIT(Macaulay 2, 0.9.97, Macaulay2@math.uiuc.edu, Macaulay2) dnl -*- autoconf -*-
AC_CONFIG_HEADERS(include/config.h)

case "$0" in
  *" "*) CONFIG_ARG0="\"$0\"" ;;
  *) CONFIG_ARG0="$0" ;;
esac

for i in $ac_configure_args
do case "$i" in
     "--no-create"|"--no-recursion") ;;
     *"$"*" "*|*" "*"$"*)
	    j=`echo $i |sed 's/\\$/\\\\$$/g'`
	    CONFIG_ARGS="$CONFIG_ARGS \"$j\"" CONFIG_ARGS_C="$CONFIG_ARGS_C \"$i\"," ;;
     *"$"*) j=`echo $i |sed 's/\\$/\\\\$$/g'`
	    CONFIG_ARGS="$CONFIG_ARGS $j" CONFIG_ARGS_C="$CONFIG_ARGS_C \"$i\"," ;;
     *" "*) CONFIG_ARGS="$CONFIG_ARGS \"$i\"" CONFIG_ARGS_C="$CONFIG_ARGS_C \"$i\"," ;;
     *) CONFIG_ARGS="$CONFIG_ARGS $i" CONFIG_ARGS_C="$CONFIG_ARGS_C \"$i\"," ;;
   esac
done
CONFIG_CMD="$CONFIG_ARG0 $CONFIG_ARGS"
echo "# invocation: $CONFIG_CMD" >&5
AC_SUBST(CONFIG_ARG0)
AC_SUBST(CONFIG_ARGS)
AC_SUBST(CONFIG_CMD)
AC_DEFINE_UNQUOTED(CONFIG_ARGS,"$CONFIG_ARGS",arguments used for configure)

define(TO_UPPER,[translit($1, [a-z], [A-Z])])

AC_DEFUN(IS_DECLARED,[
  AC_CACHE_CHECK(whether $2 is declared, m2_cv_$2_is_declared,
        AC_LANG(C)
	AC_COMPILE_IFELSE([
	    $3
	    int main () { $1 x = $2 ; } ],
	m2_cv_$2_is_declared=yes,
	m2_cv_$2_is_declared=no))
  test $m2_cv_$2_is_declared = no; val=$?
  AC_DEFINE_UNQUOTED(TO_UPPER($2_IS_DECLARED),$val,[whether $2 is declared in errno.h or stdio.h])
])

AC_CONFIG_FILES(m4_include(config/files))

echo "any environment variables set:"
test "$CC"         && echo "   CC=$CC"
test "$FC"         && echo "   FC=$FC"
test "$CXX"        && echo "   CXX=$CXX"
test "$CPPFLAGS"   && echo "   CPPFLAGS=$CPPFLAGS"                 # autoconf default is: -g -O2
test "$CFLAGS"     && echo "   CFLAGS=$CFLAGS"                     # autoconf default is: -g -O2
test "$FCFLAGS"    && echo "   FCFLAGS=$FCFLAGS"                   # autoconf default is: -g -O2, goes to @FFLAGS@!
test "$CXXFLAGS"   && echo "   CXXFLAGS=$CXXFLAGS"
test "$LDFLAGS"    && echo "   LDFLAGS=$LDFLAGS"
test "$LIBS"       && echo "   LIBS=$LIBS"
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

AC_SUBST(LOADLIBES)
AC_SUBST(LDLIBS)
LIBS="$LIBS $LOADLIBES $LDLIBS"

# 5 compiler messages saved in config.log
# 6 checking for... messages and results

AC_CHECK_PROGS(MAKE,gmake make)
AC_CHECK_PROGS(WGET,wget)
AC_CHECK_PROGS(CURL,curl)
AC_CHECK_PROGS(OBJDUMP,objdump)
AC_CHECK_PROGS(ETAGS,etags)
AC_CHECK_PROGS(LDD,ldd)
AC_CHECK_PROGS(STRIPCMD,strip)

AC_SUBST(TOPDIR,`pwd`)

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

AC_SUBST(DEBIANARCH)
case "$ARCH" in
  i686) DEBIANARCH=i386 ;;
  x86_64) DEBIANARCH=amd64 ;;
  *) DEBIANARCH="$ARCH" ;;
esac

OS="`  uname -s | sed s=/=-=g  `"
AC_SUBST(OS)
AC_DEFINE_UNQUOTED(OS,"$OS",[operating system name, obtained with uname -s])

AC_CONFIG_AUX_DIR(config)
AC_CANONICAL_HOST()
AC_VALIDATE_CACHED_SYSTEM_TUPLE()
dnl AC_ARG_VAR(CC,C compiler to use)
dnl AC_ARG_VAR(CXX,C++ compiler to use)
AC_PROG_RANLIB()
AC_PROG_INSTALL()
AC_PROG_AWK()
AC_PROG_CC()
AC_SUBST(GCC)

AC_PROG_FC(,Fortran 77)
if test "$FC"
then AC_F77_LIBRARY_LDFLAGS()
     AC_F77_DUMMY_MAIN()
     AC_F77_WRAPPERS()
fi

AC_PROG_CXX()
AC_PROG_YACC()
AC_PROG_MKDIR_P()

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
AC_CHECK_HEADERS(stddef.h stdint.h inttypes.h bits/wordsize.h endian.h elf.h)
AC_CHECK_HEADERS(sys/types.h factoryconf.h)
AC_CHECK_LIB(socket, socket)
AC_CHECK_LIB(resolv, hstrerror)
AC_CHECK_FUNCS([personality somesillyfunction sys_errlist herror error socket gnu_get_libc_version __environ _environ environ _setmode getaddrinfo hstrerror])
AC_CHECK_DECLS([ADDR_NO_RANDOMIZE],,,[#include <linux/personality.h>])
AC_CHECK_DECLS([sys_errlist,sys_nerr,herror,gnu_get_libc_version])
AC_FUNC_ALLOCA()

AC_LANG(C++)
AC_CHECK_HEADERS(NTL/tools.h)
AC_LANG(C)

AC_SUBST(HAVE_PERSONALITY)  HAVE_PERSONALITY=$ac_cv_func_personality


AC_SUBST(STRIP,yes)    AC_ARG_ENABLE(strip, AS_HELP_STRING(--disable-strip,do not strip the symbol table from the Macaulay 2 binary), STRIP="$enableval")
AC_SUBST(STATIC,no)    AC_ARG_ENABLE(static, AS_HELP_STRING(--enable-static,enable static linking), STATIC="$enableval")
AC_SUBST(MEMDEBUG,no)  AC_ARG_ENABLE(memdebug, AS_HELP_STRING(--enable-memdebug,enable memory allocation debugging), MEMDEBUG="$enableval")
AC_SUBST(OPTIMIZE,yes) AC_ARG_ENABLE(optimize, AS_HELP_STRING(--disable-optimize,disable optimization), OPTIMIZE="$enableval")
AC_SUBST(DEBUG,no)     AC_ARG_ENABLE(debug, AS_HELP_STRING(--enable-debug,enable debugging (and disable stripping)), DEBUG="$enableval")
AC_SUBST(ENCAP,yes)    AC_ARG_ENABLE(encap, AS_HELP_STRING(--disable-encap,do not encapsulate file in a subdirectory), ENCAP="$enableval")
AC_SUBST(XCODE,no)     AC_ARG_ENABLE(xcode, AS_HELP_STRING(--enable-xcode,create Macaulay2/d/interpret.a for use with xcode), XCODE="$enableval")
AC_ARG_ENABLE(altivec, AS_HELP_STRING(--enable-altivec,compile with "-faltivec" option), 
    ALTIVEC="$enableval"
    LDFLAGS="$LDFLAGS -faltivec"
    CFLAGS="CFLAGS -faltivec"
    CXXFLAGS="$CXXFLAGS -faltivec"
)
AC_SUBST(PROFILING,no) AC_ARG_ENABLE(profile, AS_HELP_STRING(--enable-profile,enable profiling (and disable stripping)), PROFILING="$enableval")
AC_SUBST(COMPRESS,gz)  AC_ARG_ENABLE(compress, AS_HELP_STRING([--enable-compress=[gz|bz2]],compression method for tarball), COMPRESS="$enableval")
AC_SUBST(EXPERIMENT,no)AC_ARG_ENABLE(experimental-code, AS_HELP_STRING(--enable-experimental-code,enable experimental code), EXPERIMENT="$enableval")
AC_SUBST(M2TARFILE,no) AC_ARG_ENABLE(tarfile, AS_HELP_STRING(--enable-tarfile,prepare a binary package as a compressed tar file), M2TARFILE="$enableval")
AC_SUBST(TARLIBS,no)   AC_ARG_ENABLE(tarlibs, AS_HELP_STRING(--enable-tarlibs,include symbolic links to needed shared libraries for tar), TARLIBS="$enableval")
AC_SUBST(SHARED,no)    AC_ARG_ENABLE(shared, AS_HELP_STRING(--enable-shared,enable building of shared libraries), SHARED="$enableval")

test "$MEMDEBUG" = "yes" && DEBUG=yes
test "$DEBUG" = "yes" && STRIP=no
test "$PROFILING" = "yes" && CFLAGS="$CFLAGS -pg" CXXFLAGS="$CXXFLAGS -pg" LDFLAGS="$LDFLAGS -pg" STRIP=no

AC_SUBST(PACKAGES)
PACKAGES="Macaulay2
Style
Classic
PrimaryDecomposition
FourierMotzkin
Dmodules
Depth
Elimination
FirstPackage
GenericInitialIdeal
IntegralClosure
HyperplaneArrangements
LexIdeals
Markov
NoetherNormalization
Points
PackageTemplate
ReesAlgebra
SchurRings
SimplicialComplexes
LLLBases
Parsing
TangentCone
ChainComplexExtras
Benchmark
Schubert2"

# convert the newlines in the list above to spaces
PACKAGES=`echo $PACKAGES`

AC_DEFINE_UNQUOTED(PACKAGES,"$PACKAGES",the list packages included with the release of Macaulay 2)

AC_DEFINE_UNQUOTED(buildsystemtype,"$build",the type of system on which the package was built)
AC_DEFINE_UNQUOTED(hostsystemtype,"$host",the type of system on which the package runs)

case "$host" in
   i486-*|i586-*|i686-*)
	newhost=`echo $host | sed s/i.86-/i386-/`
	AC_MSG_NOTICE([warning: building for host $host might be compatible enough, consider using option "--build=$newhost"]) ;;
esac

AC_ARG_ENABLE(
    auto-instantiation, 
    AS_HELP_STRING([--enable-auto-instantiation,enable automatic instantiation of C++ templates (it uses -frepo option to g++)]),
    AUTOINST="$enableval",
    AUTOINST=no)
AC_SUBST(AUTOINST)
test "$AUTOINST" = no; val=$?; AC_DEFINE_UNQUOTED(AUTOINST,$val,whether to instantiate templates automatically)

AC_ARG_ENABLE(
    implicit-templates,
    AS_HELP_STRING([--disable-implicit-templates,disable implicit instantiation of C++ templates (it uses -fno-implicit-templates option to g++)]),
    IMPLINST="$enableval",
    IMPLINST=yes)
AC_SUBST(IMPLINST)
test "$IMPLINST" = no; val=$?; AC_DEFINE_UNQUOTED(IMPLINST,$val,whether to instantiate templates implicitly)

AC_SUBST(DEVELOPMENT) AC_ARG_ENABLE(development, AS_HELP_STRING(--enable-development,build a development version),
    DEVELOPMENT="$enableval";AC_DEFINE(DEVELOPMENT,1,whether to build a development version), DEVELOPMENT==no)
AC_SUBST(DEVELOPER)   AC_ARG_WITH(developer,   AS_HELP_STRING(--with-developer=...,specify the name of the developer ($USER)),DEVELOPER="$withval")

AC_SUBST(DUMPDATA)   AC_ARG_ENABLE(dumpdata, AS_HELP_STRING(--disable-dumpdata,do not cache data with dumpdata), DUMPDATA="$enableval")
if ! test "$DUMPDATA"
then case "$OS" in
	 Linux|SunOS) DUMPDATA=yes;;
	 *) DUMPDATA=no;;
     esac
     AC_MSG_NOTICE([default dumpdata option for $OS operating system used, value: $DUMPDATA])
else case "$DUMPDATA" in
         yes|no) ;;
	 *) AC_MSG_ERROR([dumpdata option: invalid value: $DUMPDATA])
     esac
fi

AC_SUBST(TAR_COMPRESS_OPTION)
case "$COMPRESS" in
   gz) TAR_COMPRESS_OPTION=--gzip ;;
  bz2) TAR_COMPRESS_OPTION=--bzip ;;
    *) AC_MSG_ERROR(unrecognized option for enable-compress) ;;
esac

AC_SUBST(SRCTARFILE)  AC_ARG_WITH(srctarfile, AS_HELP_STRING(--with-srctarfile=...,source tar file to use for inclusion with distributions (cygwin)),
    dirname=`dirname  "$withval"`
    SRCTARFILE=`cd "$dirname" && pwd`/`basename "$withval"`
    ,
    SRCTARFILE=)

AC_SUBST(CYGWIN)
AC_ARG_ENABLE(cygwin,  AS_HELP_STRING(--enable-cygwin,prepare a binary package for cygwin (disables encap and enables strip)), CYGWIN="$enableval", CYGWIN=no)
if test "$CYGWIN" = yes 
then ENCAP=no STRIP=yes M2TARFILE=yes
     if ! test "$SRCTARFILE" -o -d "$srcdir/.svn"
     then AC_MSG_ERROR([source tar file needed for cygwin distribution, specify it with --with-srctarfile=...])
     fi
fi

AC_SUBST(DEBIAN)
AC_ARG_ENABLE(debian,  AS_HELP_STRING(--enable-debian,prepare a binary package for debian (enables encap)), DEBIAN="$enableval", DEBIAN=no)
if test "$DEBIAN" = yes ; then ENCAP=yes M2TARFILE=yes ; fi

AC_SUBST(RPM)
AC_ARG_ENABLE(rpm,  AS_HELP_STRING(--enable-rpm,prepare a binary rpm package (enables encap)), RPM="$enableval", RPM=no)
if test "$RPM" = yes ; then ENCAP=yes M2TARFILE=yes ; fi

AC_SUBST(M2SUFFIX)
test "$program_suffix" != NONE && M2SUFFIX="$program_suffix"
AC_ARG_ENABLE(dummy, AS_HELP_STRING(--program-suffix=...,suffix to append to executable name M2))

test "$DUMPDATA" = no;   val=$?; AC_DEFINE_UNQUOTED(DUMPDATA,  $val,whether to use dumpdata)
test "$PROFILING" = no;  val=$?; AC_DEFINE_UNQUOTED(PROFILING, $val,whether profiling has been enabled)
test "$EXPERIMENT" = no; val=$?; AC_DEFINE_UNQUOTED(EXPERIMENT,$val,whether experimental code has been enabled)

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

AC_SUBST(ISYSTEM)
AC_MSG_CHECKING(whether g++ gets confused about C linkage in system library files)
AC_LANG(C++)
AC_COMPILE_IFELSE([# 1 "foo" 1 3 4
	template <class T> struct A { };],AC_MSG_RESULT(no);ISYSTEM=yes,AC_MSG_RESULT(yes);ISYSTEM=no)

AC_SUBST(BUILTLIBS)

AC_LANG(C)
AC_SEARCH_LIBS(tgoto,ncurses curses)
AC_SUBST(BUILDREADLINE,no)
AC_ARG_ENABLE(build-readline, AS_HELP_STRING(--enable-build-readline,build library readline from downloaded source code), BUILDREADLINE="$enableval")
if test "$BUILDREADLINE" = no
then AC_SEARCH_LIBS(add_history,history,,BUILDREADLINE=yes)
     AC_SEARCH_LIBS(readline,readline,,BUILDREADLINE=yes)
     AC_CHECK_HEADER(readline/readline.h,,BUILDREADLINE=yes)
     if test "$BUILDREADLINE" = yes
     then AC_MSG_NOTICE(readline library will be compiled)
	  BUILTLIBS="-lreadline -lhistory $BUILTLIBS"
     fi
fi

AC_LANG(C)
AC_SUBST(BUILDGMP,no)
AC_ARG_ENABLE(build-gmp, AS_HELP_STRING(--enable-build-gmp,build library gmp from downloaded source code), BUILDGMP="$enableval")
AC_ARG_WITH(gmplib, AS_HELP_STRING(--with-gmplib=...,gmp library (-lgmp)),
    LIBS="$withval $LIBS" BUILDGMP=no,
    [
    if test "$BUILDGMP" = no
    then AC_SEARCH_LIBS(__gmpz_init,gmp,,BUILDGMP=yes)
	 AC_CHECK_HEADER(gmp.h,,BUILDGMP=yes)
	 test "$BUILDGMP" = yes && BUILTLIBS="-lgmp $BUILTLIBS"
    fi
    ])

AC_LANG(C)
AC_SUBST(BUILDGC,no)
AC_ARG_ENABLE(build-gc, AS_HELP_STRING(--enable-build-gc,build library gc from downloaded source code), BUILDGC="$enableval")
AC_ARG_WITH(gclib, AS_HELP_STRING(--with-gclib=...,gc library (-lgc)),
    LIBS="$withval $LIBS" BUILDGC=no,
    [
    if test "$BUILDGC" = no
    then AC_SEARCH_LIBS(GC_free,gc,,BUILDGC=yes)
	 AC_CHECK_HEADER(gc/gc.h,,BUILDGC=yes)
	 test "$BUILDGC" = yes && BUILTLIBS="-lgc $BUILTLIBS"
    fi
    ])

AC_LANG(C)
AC_SUBST(BUILDGDBM,no)
AC_ARG_ENABLE(build-gdbm, AS_HELP_STRING(--enable-build-gdbm,build library gdbm from downloaded source code), BUILDGDBM="$enableval")
AC_ARG_WITH(gdbmlib, AS_HELP_STRING(--with-gdbmlib=...,gdbm library (-lgdbm)),
    LIBS="$withval $LIBS" BUILDGDBM=no,
    [
    if test "$BUILDGDBM" = no
    then AC_SEARCH_LIBS(gdbm_close,gdbm,,BUILDGDBM=yes)
	 AC_CHECK_HEADER(gdbm.h,,BUILDGDBM=yes)
	 test "$BUILDGDBM" = yes && BUILTLIBS="-lgdbm $BUILTLIBS"
    fi
    ])

AC_SUBST(BUILDNTL,no)
AC_ARG_ENABLE(build-ntl, AS_HELP_STRING(--enable-build-ntl,build library ntl from downloaded source code), BUILDNTL="$enableval")
if test "$BUILDNTL" = no
then AC_LANG(C++)
    SAVELIBS=$LIBS
    AC_MSG_CHECKING(for nlt library)
    FOUND=
    for lib in "" "-lntl"
    do  test "$lib" && LIBS="$lib $LIBS"
	AC_LINK_IFELSE( [
	        #include <NTL/tools.h>
		int main () { _ntl_GetTime(); }
		],
	    FOUND=yes; break;)
    done
    if test "$FOUND"
    then if test "$lib"
	then AC_MSG_RESULT(none needed)
	else AC_MSG_RESULT(found $lib)
	fi
    else AC_MSG_RESULT(none found)
	LIBS=$SAVELIBS BUILDNTL=yes BUILTLIBS="-lntl $BUILTLIBS"
    fi 
fi

AC_SUBST(BUILDBLAS,no)
AC_ARG_ENABLE(build-blas, AS_HELP_STRING(--enable-build-blas,build library blas from downloaded source code), BUILDBLAS="$enableval")

AC_SUBST(BUILDLAPACK,no)
AC_ARG_ENABLE(build-lapack, AS_HELP_STRING(--enable-build-lapack,build library lapack from downloaded source code), BUILDLAPACK="$enableval")

AC_ARG_WITH(veclib, AS_HELP_STRING(--with-veclib,[use "-framework vecLib" for lapack]), LIBS="-framework vecLib $LIBS" )

AC_SUBST(LAPACK,yes)
AC_ARG_WITH(lapack, AS_HELP_STRING(--without-lapack,[compile and link without lapack and blas]), LAPACK="$enableval")
test "$LAPACK" = no; val=$?
AC_DEFINE_UNQUOTED(LAPACK,$val,whether to link with lapack)

if test "$LAPACK" = yes
then AC_MSG_CHECKING([whether the vecLib framework is available])
     SAVELIBS="$LIBS"
     LIBS="-framework vecLib $LIBS"
     AC_LANG(C)
     AC_LINK_IFELSE(
	 AC_LANG_PROGRAM(,[sgemv_();dgetrf_();]),
	 [
	     AC_MSG_RESULT(yes)
	     ],
	 [
	     AC_MSG_RESULT(no)
	     LIBS="$SAVELIBS"
	     if test "$BUILDBLAS" = no
	     then AC_LANG(Fortran 77)
		  AC_SEARCH_LIBS(sgemv,blas f77blas,,BUILDBLAS=yes BUILTLIBS="-lblas $BUILTLIBS")
	     fi
	     if test "$BUILDLAPACK" = no
	     then AC_LANG(Fortran 77)
		  AC_SEARCH_LIBS(dgetrf,lapack,,BUILDLAPACK=yes BUILTLIBS="-llapack $BUILTLIBS")
	     fi
	 ])
fi

# after this point we add no more libraries to $LIBS, e.g., with AC_SEARCH_LIBS()
SAVELIBS=$LIBS

# we need to control the position of -lfac -lcf in the link line, so we don't add them to $LIBS nor to $BUILTLIBS, putting them in $SINGULARLIBS instead
AC_SUBST(SINGULARLIBS,"-lcf -lfac")
AC_SUBST(BUILDFACTORY,no)
AC_ARG_ENABLE(build-factory, AS_HELP_STRING(--enable-build-factory,build libraries libfac and factory from downloaded source code), BUILDFACTORY="$enableval")
if test "$BUILDFACTORY" = no
then AC_LANG(C++)
     AC_MSG_CHECKING([whether factory library is installed (-lcf)])
     LIBS="$SINGULARLIBS $LIBS"
     AC_LINK_IFELSE( [[
	     #include <factoryconf.h>
	     extern const char * libfac_version;
	     int main () { extern const char factoryVersion[]; return *factoryVersion; return *libfac_version; }
	     ]],
	  AC_MSG_RESULT(yes),
	  AC_MSG_RESULT(no); BUILDFACTORY=yes)
     LIBS="$SAVELIBS"
fi

AC_SUBST(FINALPREFIX)
AC_ARG_WITH(final-prefix,
    AS_HELP_STRING(--with-final-prefix=...,final installation prefix (/usr) (used by libtool and debian installation scripts)),
    FINALPREFIX="$withval", 
    FINALPREFIX="/usr")

AC_MSG_CHECKING(whether nm accepts the demangle option)
if nm --help 2>&1 | grep demangle >/dev/null
   then NM_DEMANGLES=yes; AC_MSG_RESULT(yes)
   else NM_DEMANGLES=no ; AC_MSG_RESULT(no)
fi
AC_SUBST(NM_DEMANGLES)

LIBS="$SAVELIBS"
AC_LANG(C)
AC_MSG_CHECKING([whether getaddrinfo can handle numeric service (port) numbers])
AC_RUN_IFELSE([
    #include <sys/types.h>
    #include <sys/socket.h>
    #include <netdb.h>
    main() { struct addrinfo *addr; return 0 != getaddrinfo("1.2.3.4", "80", 0, &addr); }
    ],
    GETADDRINFO_WORKS=1;AC_MSG_RESULT(yes),
    GETADDRINFO_WORKS=0;AC_MSG_RESULT(no),
    AC_MSG_RESULT([yes (cross-compiling, not tested)]))
AC_DEFINE_UNQUOTED(GETADDRINFO_WORKS,$GETADDRINFO_WORKS,[whether getaddrinfo can handle numeric service (port) numbers])

AC_SUBST(LIBRARIES,"")
# this sequence reflects dependencies
test "$BUILDGDBM" = yes && LIBRARIES="$LIBRARIES gdbm"
test "$BUILDGMP" = yes && LIBRARIES="$LIBRARIES gmp"
test "$BUILDREADLINE" = yes && LIBRARIES="$LIBRARIES readline"
test "$BUILDGC" = yes && LIBRARIES="$LIBRARIES gc"
test "$BUILDNTL" = yes && LIBRARIES="$LIBRARIES ntl"
# factory needs ntl and gmp, libfac needs factory
test "$BUILDFACTORY" = yes && LIBRARIES="$LIBRARIES factory libfac"
test "$BUILDBLAS" = yes && LIBRARIES="$LIBRARIES blas"
# lapack needs blas
test "$BUILDLAPACK" = yes && LIBRARIES="$LIBRARIES lapack"

TESTFLAG=-Wno-strict-aliasing
AC_MSG_CHECKING(whether $CC accepts $TESTFLAG)
AC_SUBST(HAVE_NO_STRICT_ALIASING)
SAVE="$CFLAGS"
CFLAGS="$TESTFLAG $CFLAGS"
AC_LANG(C)
AC_COMPILE_IFELSE([int main() {}], HAVE_NO_STRICT_ALIASING=yes, HAVE_NO_STRICT_ALIASING=no)
CFLAGS="$SAVE"
AC_MSG_RESULT($HAVE_NO_STRICT_ALIASING)

AC_MSG_CHECKING(whether $CC accepts -Wfatal-errors)
AC_LANG(C)
SAVE="$CFLAGS"
CFLAGS="$CFLAGS -Wfatal-errors"
AC_COMPILE_IFELSE([int main() {}], 
    CPPFLAGS="$CPPFLAGS -Wfatal-errors"
    CXXFLAGS="$CXXFLAGS -Wfatal-errors"
    AC_MSG_RESULT(yes, adding -Wfatal-errors to flags),
    CFLAGS="$SAVE"
    AC_MSG_RESULT(no))

IS_DECLARED(char **,sys_errlist, [
      #include <stdio.h>
      #include <errno.h>
      ])
IS_DECLARED(char **,h_errlist, [
      #include <stdio.h>
      #include <errno.h>
      ])

dnl # delete this eventually:
AC_ARG_WITH(blaslib, AS_HELP_STRING([--with-blaslib=...],obsolete option), AC_MSG_ERROR(--with-blaslib : obsolete option))
AC_ARG_WITH(lapacklibs, AS_HELP_STRING([--with-lapacklibs=...],obsolete option), AC_MSG_ERROR(--with-lapacklibs : obsolete option))
AC_ARG_WITH(readlinelib, AS_HELP_STRING(--with-readlinelib=...,obsolete option), AC_MSG_ERROR(--with-readlinelib : obsolete option))
AC_ARG_WITH(historylib, AS_HELP_STRING(--with-historylib=...,obsolete option), AC_MSG_ERROR(--with-historylib : obsolete option))

AC_MSG_NOTICE([using LIBRARIES    = $LIBRARIES (libraries to be built from downloaded sources)])
AC_MSG_NOTICE([using BUILTLIBS    = $BUILTLIBS (linker options for most of them)])
AC_MSG_NOTICE([using SINGULARLIBS = $SINGULARLIBS (linker options for some of them)])
AC_MSG_NOTICE([using LIBS         = $LIBS (M2 linker options)])

AC_SUBST(DUMPDATAFILE,Macaulay2-$ARCH-data)

if test "$DEBUG" = yes
then CPPFLAGS="$CPPFLAGS -DDEBUG"
else CPPFLAGS="$CPPFLAGS -DNDEBUG"
fi

if test "$OPTIMIZE" = yes
then CFLAGS="$CFLAGS -O3"
     CXXFLAGS="$CXXFLAGS -O3"
else CFLAGS="$CFLAGS -O0"
     CXXFLAGS="$CXXFLAGS -O0"
fi

test "$MEMDEBUG" = yes && CPPFLAGS="$CPPFLAGS -DMEMDEBUG"
test "$DEBUG" = yes && CPPFLAGS="$CPPFLAGS -DGC_DEBUG"

if test "$OS" = Darwin 
then 
    # we don't know what this does, but some apple documentation writers seem to like it:
    LDFLAGS="$LDFLAGS -bind_at_load"

    # this one makes it find and use our readline.a first, even if there is a file readline.dylib in /usr/lib
    # the point is that the system's readline.dylib might be much older and conflict with our newer one
    LDFLAGS="$LDFLAGS -Wl,-search_paths_first"
fi

AC_SUBST(package,Macaulay2-$PACKAGE_VERSION)
AC_SUBST(encapdir)
if test "$ENCAP" = yes
then encapdir="\${prefix}/$package"
else encapdir="\${prefix}"
fi
AC_SUBST(SRC_TGZ,$package.tar.gz)
AC_SUBST(SRC_TBZ,$package.tar.bz2)
AC_SUBST(SRC_TZ,$package.tar.$COMPRESS)
AC_SUBST(PKG_TGZ,$package-$ARCH-$OS.tar.gz)
AC_SUBST(PKG_TBZ,$package-$ARCH-$OS.tar.bz2)
AC_SUBST(PKG_DEB,$package-$DEBIANARCH-$OS.deb)
AC_SUBST(PKG_RPM,$package-$ARCH-$OS.rpm)
AC_SUBST(PKG_TZ,$package-$ARCH-$OS.tar.$COMPRESS)
AC_SUBST(EXE,$M2SUFFIX$EXEEXT)

AC_SUBST(DESC,$PACKAGE_VERSION-$host_cpu-$OS-$REL)
# we used to use @ ARCH @ instead of @ host_cpu @ here, but for DEC alphas,
# and perhaps other machines, it gives more information.  The script
# config.guess assembles host_cpu from /proc/cpuinfo, and can be something
# like "alphaev6" or "alphaev56", but ARCH, which is determined from 'uname'
# by the 'configure' script, will be simply "alpha".

CONFIGURED_FILES="$ac_config_files"
AC_SUBST(CONFIGURED_FILES)
AC_OUTPUT()

dnl # Local Variables:
dnl # mode: sh
dnl # compile-command: "make -f Makefile"
dnl # End:
