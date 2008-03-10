AC_INIT(Macaulay 2, 1.1.1, Macaulay2@math.uiuc.edu, Macaulay2) dnl -*- autoconf -*-
AC_CONFIG_HEADERS(include/config.h)
AC_CONFIG_FILES(m4_include(config/files))
AC_SUBST(CONFIGURED_FILES,"$ac_config_files")
AC_SUBST(CONFIG_ARGS,"$ac_configure_args")
AC_DEFINE_UNQUOTED(CONFIG_ARGS,"$CONFIG_ARGS",arguments used for configure)
AC_SUBST(CONFIG_CMD,"'$0' $ac_configure_args")

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

echo "relevant environment variable values, if any:"
for x in CC FC CXX CPPFLAGS CFLAGS FCFLAGS CXXFLAGS LDFLAGS LIBS SINGULARLIBS ISSUE DISTRIBUTION
do eval v=\$$x
   test "$v" && echo "   $x=$v"
done

AC_SUBST(DISTRIBUTION) dnl use this starting number to sequentially number the downstream distributions
test "$DISTRIBUTION" || DISTRIBUTION=1

for i in $CFLAGS
do case $i in
   -I*) AC_MSG_ERROR("preprocessor flag $i found in CFLAGS instead of in CPPFLAGS") ;;
   -L*) AC_MSG_ERROR("link flag $i found in CFLAGS instead of in LDFLAGS");;
   esac
done

for i in $CXXFLAGS
do case $i in
   -I*) AC_MSG_ERROR("preprocessor flag $i found in CXXFLAGS instead of in CPPFLAGS");;
   -L*) AC_MSG_ERROR("link flag $i found in CXXFLAGS instead of in LDFLAGS");;
   esac
done

for i in $CPPFLAGS
do case $i in
   -L*) AC_MSG_ERROR("link flag $i found in CPPFLAGS instead of in LDFLAGS");;
   esac
done

for i in $LDFLAGS
do case $i in
   -I*) AC_MSG_ERROR("preprocessor flag $i found in LDFLAGS instead of in CPPFLAGS");;
   esac
done

test "$CXX" = gcc && AC_MSG_ERROR("oops, CXX=gcc, should use CXX=g++")

AC_CHECK_PROGS(MAKE,gmake make)
AC_CHECK_PROGS(WGET,wget)
AC_CHECK_PROGS(CURL,curl)
AC_CHECK_PROGS(OBJDUMP,objdump)
AC_CHECK_PROGS(ETAGS,etags :)
AC_CHECK_PROGS(LINTIAN,lintian)
AC_CHECK_PROGS(LDD,ldd)
AC_CHECK_PROGS(FAKEROOT,fakeroot)
AC_CHECK_PROGS(STRIPCMD,strip)
if test "$STRIPCMD" != ""
then AC_MSG_CHECKING(whether $STRIPCMD accepts the remove-section option)
     if strip --help 2>&1 | grep remove-section >/dev/null
     then val=yes
     else val=no
     fi
     AC_MSG_RESULT($val)
     AC_SUBST(STRIP_REMOVE_SECTION,$val)
fi

AC_SUBST(TOPDIR,`pwd`)

AC_MSG_CHECKING("checking whether $MAKE is GNU make")
if "$MAKE" -n --version nothing | grep GNU >/dev/null 2>&1
then AC_MSG_RESULT(yes)
else AC_MSG_ERROR("GNU make is required: $MAKE")
fi

AC_SUBST(NODENAME,"`uname -n`")
AC_DEFINE_UNQUOTED(NODENAME,"$NODENAME",hostname used for compilation)

AC_SUBST(ISSUE) dnl no initial value
AC_ARG_WITH(issue, AS_HELP_STRING([--with-issue=...,specify the OS issue (e.g., Ubuntu-7.10)]),ISSUE=$withval)
if test -f /etc/issue -a ! "$ISSUE"
then ISSUE_FLAVOR=[`</etc/issue head -1 | sed 's/^\([A-Za-z ]*\).*/\1/' | sed 's/ //g' `]
     ISSUE_RELEASE=[`</etc/issue head -1 | sed 's/[^0-9]*\([0-9.]*\).*/\1/'`]
     case $ISSUE_FLAVOR in
	 "FedoraCore"*) ISSUE_FLAVOR=FedoraCore ;;
	 "Fedora"*) ISSUE_FLAVOR=Fedora ;;
	 "RedHatEnterprise"*) ISSUE_FLAVOR=RedHatEnterprise ;;
	 "RedHat"*) ISSUE_FLAVOR=RedHat ;;
	 "Debian"*) ISSUE_FLAVOR=Debian ;;  dnl Debian GNU/Linux 4.0 \n \l
	 "") ISSUE_FLAVOR=unknown ;;
     esac
     case $ISSUE_RELEASE in
	 "") ISSUE_RELEASE=unknown ;;
     esac
     ISSUE=$ISSUE_FLAVOR-$ISSUE_RELEASE
fi
AC_MSG_NOTICE([issue to be used in distribution file names: $ISSUE])

AC_SUBST(OS,"`  uname -s | sed s=/=-=g  `")
AC_SUBST(REL,"`  uname -r | sed -e s=/=-=g -e 's= =+=' -e 's=[()]==g'  `")

case $OS-$REL in
  Darwin-7.*) REL=10.3 ; OS=MacOS ;;
  Darwin-8.*) REL=10.4 ; OS=MacOS ;;
  Darwin-9.*) REL=10.5 ; OS=MacOS ;;
  Darwin-*) echo "Darwin release number translation needed in configure.ac" >&2 ; exit 1
esac

case $OS in
  dnl Here we normalize the name of the OS for windows for startup.m2, which
  dnl needs to know that C:/FOO/BAR is an absolute path.
  CYGWIN*) OS=MicrosoftWindows ; ISSUE=Cygwin ;;
esac

case $OS in
  MacOS) 
	AC_MSG_NOTICE([[*** This is MacOS, but we are not adding -lgcc_s.10.5 to LIBS for you, 
because there might be a better way.  See the discussion in emacs info about -static-libgcc
and -shared-libgcc, try both of those in LDFLAGS, and also try -lgcc and -lgcc_eh in LIBS.
If all else fails, add "LIBS=-lgcc_s.10.5" to the configure command line.  Check whether the 
binary ends up requiring a sharable library with a similar name. ***]])
	dnl # LIBS="$LIBS -lgcc_s.10.5" # what kind of crock is this??
	;;
esac

AC_DEFINE_UNQUOTED(OS,"$OS",[operating system name, obtained with uname -s, perhaps modified])
AC_DEFINE_UNQUOTED(REL,"$REL",[operating system release, obtained with uname -r])

AC_CONFIG_AUX_DIR(config)

AC_CANONICAL_HOST()
AC_SUBST(ARCH,$build_cpu)
AC_DEFINE_UNQUOTED(ARCH,"$ARCH",[machine hardware type])

AC_SUBST(DEBARCH,)
case $ARCH in
  x86_64) DEBARCH=amd64 ;;
  *) DEBARCH=$ARCH ;;
esac

AC_SUBST(PKG_RPM,$PACKAGE_TARNAME-$PACKAGE_VERSION-$DISTRIBUTION.$ARCH-$OS-$ISSUE.rpm)
AC_SUBST(PKG_COM_RPM,$PACKAGE_TARNAME-$PACKAGE_VERSION-$DISTRIBUTION.common.rpm)
AC_SUBST(PKG_DEB,$PACKAGE_TARNAME-$PACKAGE_VERSION-$DEBARCH-$OS-$ISSUE.deb)
AC_SUBST(PKG_COM_DEB,$PACKAGE_TARNAME-$PACKAGE_VERSION-common.deb)

AC_VALIDATE_CACHED_SYSTEM_TUPLE()
dnl AC_ARG_VAR(CC,C compiler to use)
dnl AC_ARG_VAR(CXX,C++ compiler to use)
AC_PROG_RANLIB()
AC_PROG_INSTALL()
AC_PROG_AWK()
AC_PROG_CC()
AC_SUBST(GCC) dnl gets set to yes or no somehow

AC_PROG_CXX()
AC_PROG_YACC()
AC_PROG_MKDIR_P()

AC_MSG_CHECKING([whether linking C++ programs with C compiler $CC works])
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
	$CXX $CPPFLAGS $CXXFLAGS -O3 -c -o unwindtest-main.$OBJEXT unwindtest-main.cpp &&
	$CXX $CPPFLAGS $CXXFLAGS -O3 -c -o unwindtest-monomials.$OBJEXT unwindtest-monomials.cpp &&
	$CC $LDFLAGS unwindtest-main.$OBJEXT unwindtest-monomials.$OBJEXT $LIBS -lstdc++ -o unwindtest-main &&
	test -f unwindtest-main$EXEEXT
     ) >&AS_MESSAGE_LOG_FD 2>&1
then AC_MSG_RESULT(yes); rm -f unwindtest-*
else AC_MSG_RESULT(no);  AC_MSG_ERROR([failure: linking C++ program with $CC, see file config.log])
fi

AC_MSG_CHECKING(order of construction implemented by the linker)
    cat >conf_main.cc <<EOF
    #include <stdio.h>
    extern void o1(), a2(), o3(), a4();
    int main() { 
      o1();
      a2();
      o3();
      a4();
      putchar('\n');
      return 0;
    }
EOF
    cat >conf_test1.cc <<EOF
    #include <stdio.h>
    static struct s1 { s1() { fputs("o1",stdout); } } x;
    void o1(){}
EOF
    cat >conf_test2.cc <<EOF
    #include <stdio.h>
    static struct s2 { s2() { fputs("a2",stdout); } } x;
    void a2(){}
EOF
    cat >conf_test3.cc <<EOF
    #include <stdio.h>
    static struct s3 { s3() { fputs("o3",stdout); } } x;
    void o3(){}
EOF
    cat >conf_test4.cc <<EOF
    #include <stdio.h>
    static struct s4 { s4() { fputs("a4",stdout); } } x;
    void a4(){}
EOF
    AC_SUBST(CONSTRUCTOR_ORDER,)
    AC_SUBST(CONSTRUCTOR_ORDER_OBJ,)
    AC_SUBST(CONSTRUCTOR_ORDER_LIB,)
    if (set -xe
	$CXX -c conf_test1.cc
	$CXX -c conf_test2.cc
	$CXX -c conf_test3.cc
	$CXX -c conf_test4.cc
	if test $CXX = cl
	then lib conf_test2.$OBJEXT
	     lib conf_test4.$OBJEXT
	     $CXX conf_main.cc conf_test1.$OBJEXT conf_test2.lib conf_test3.$OBJEXT conf_test4.lib -o conf_main
	else ar ru libconf_test2.a conf_test2.$OBJEXT
	     ar ru libconf_test4.a conf_test4.$OBJEXT
	     $CXX conf_main.cc -L. conf_test1.$OBJEXT -lconf_test2 conf_test3.$OBJEXT -lconf_test4 -o conf_main
	fi
	./conf_main
	) 2>&AS_MESSAGE_LOG_FD 1>&AS_MESSAGE_LOG_FD
    then CONSTRUCTOR_ORDER=`./conf_main`
	 case $CONSTRUCTOR_ORDER in
	     *o1*o3*) CONSTRUCTOR_ORDER_OBJ=LR ;;
	     *o3*o1*) CONSTRUCTOR_ORDER_OBJ=RL ;;
	 esac
	 case $CONSTRUCTOR_ORDER in
	     *a2*a4*) CONSTRUCTOR_ORDER_LIB=LR ;;
	     *a4*a2*) CONSTRUCTOR_ORDER_LIB=RL ;;
	 esac
	 rm -f libconf_test*.a conf_test*.cc conf_test*.$OBJEXT conf_main*.cc conf_main
	 AC_MSG_RESULT([link order o1a2o3a4, construction order $CONSTRUCTOR_ORDER, obj order $CONSTRUCTOR_ORDER_OBJ, lib order $CONSTRUCTOR_ORDER_LIB])
    else AC_MSG_ERROR([test failed, see config.log])
    fi

AC_HEADER_TIME() dnl # time.h sys/time.h
AC_CHECK_HEADERS(sys/ioctl.h termios.h sys/mman.h sys/socket.h netdb.h netinet/in.h arpa/inet.h sys/time.h sys/wait.h sys/resource.h io.h linux/personality.h)
AC_CHECK_HEADERS(stddef.h stdint.h inttypes.h bits/wordsize.h endian.h elf.h execinfo.h)
AC_CHECK_HEADERS(sys/types.h)
AC_SEARCH_LIBS(socket,socket)
AC_SEARCH_LIBS(hstrerror,resolv)
AC_CHECK_FUNCS([somesillyfunction sys_errlist herror error socket gnu_get_libc_version __environ _environ environ _setmode getaddrinfo hstrerror])
AC_CHECK_DECLS([ADDR_NO_RANDOMIZE],,,[#include <linux/personality.h>])
AC_CHECK_DECLS([sys_errlist,sys_nerr,herror,gnu_get_libc_version])
AC_TYPE_INT64_T()
AC_FUNC_ALLOCA()

AC_LANG(C++)
AC_CHECK_HEADERS(NTL/tools.h)
AC_LANG(C)

AC_SUBST(HAVE_PERSONALITY,)
AC_CHECK_FUNC(personality, HAVE_PERSONALITY=1, HAVE_PERSONALITY=0)

AC_SUBST(PTHREADS,no)  AC_ARG_ENABLE(pthreads, AS_HELP_STRING(--enable-pthreads,enable pthreads in gc), PTHREADS=$enableval)
AC_SUBST(STRIP,yes)    AC_ARG_ENABLE(strip, AS_HELP_STRING(--disable-strip,do not strip the symbol table from the Macaulay 2 binary), STRIP=$enableval)
AC_SUBST(STATIC,no)    AC_ARG_ENABLE(static, AS_HELP_STRING(--enable-static,enable static linking), STATIC=$enableval)
AC_SUBST(MEMDEBUG,no)  AC_ARG_ENABLE(memdebug, AS_HELP_STRING(--enable-memdebug,enable memory allocation debugging), MEMDEBUG=$enableval)
AC_SUBST(OPTIMIZE,yes) AC_ARG_ENABLE(optimize, AS_HELP_STRING(--disable-optimize,disable optimization), OPTIMIZE=$enableval)
AC_SUBST(DEBUG,no)     AC_ARG_ENABLE(debug, AS_HELP_STRING(--enable-debug,enable debugging (and disable stripping)), DEBUG=$enableval)
AC_SUBST(ENCAP,yes)    AC_ARG_ENABLE(encap, AS_HELP_STRING(--disable-encap,do not encapsulate file in a subdirectory), ENCAP=$enableval)
AC_SUBST(XCODE,no)     AC_ARG_ENABLE(xcode, AS_HELP_STRING(--enable-xcode,create Macaulay2/d/interpret.a for use with xcode), XCODE=$enableval)
AC_ARG_ENABLE(altivec, AS_HELP_STRING(--enable-altivec,compile with "-faltivec" option), 
    ALTIVEC=$enableval
    LDFLAGS="$LDFLAGS -faltivec"
    CFLAGS="CFLAGS -faltivec"
    CXXFLAGS="$CXXFLAGS -faltivec"
)


AC_PROG_FC()
AC_SUBST(FCLIBS_DYNAMIC,)
AC_SUBST(FCLIBS_STATIC,)
AC_SUBST(FCLIBS_FLAGS,)
if test "$FC"
then AC_FC_DUMMY_MAIN()
     AC_FC_WRAPPERS()
     AC_FC_LIBRARY_LDFLAGS()
     for i in $FCLIBS
     do case $i in
	     -L*) FCLIBS_FLAGS="$FCLIBS_FLAGS $i" ;;
	     -lgcc_s) FCLIBS_DYNAMIC="$FCLIBS_DYNAMIC $i" ;; dnl # see discussion in emacs info about -static-libgcc and -shared-libgcc
	     -lm) FCLIBS_DYNAMIC="$FCLIBS_DYNAMIC $i" ;;
	     *) FCLIBS_STATIC="$FCLIBS_STATIC $i" ;;
	esac
     done
fi


if test "$PTHREADS" = yes
then AC_SEARCH_LIBS(pthread_mutex_trylock,pthread)
fi

AC_SUBST(PROFILING,no)
AC_ARG_ENABLE(profile, AS_HELP_STRING(--enable-profile,enable profiling (and disable stripping)), PROFILING=$enableval)
test "$PROFILING" = no;  val=$?; AC_DEFINE_UNQUOTED(PROFILING, $val,whether profiling has been enabled)

AC_SUBST(COMPRESS,gz)  AC_ARG_ENABLE(compress, AS_HELP_STRING([--enable-compress=[gz|bz2]],compression method for tarball), COMPRESS=$enableval)

AC_SUBST(EXPERIMENT,no)
AC_ARG_ENABLE(experimental-code, AS_HELP_STRING(--enable-experimental-code,enable experimental code), EXPERIMENT=$enableval)
test "$EXPERIMENT" = no; val=$?; AC_DEFINE_UNQUOTED(EXPERIMENT,$val,whether experimental code has been enabled)

AC_SUBST(M2TARFILE,no) AC_ARG_ENABLE(tarfile, AS_HELP_STRING(--enable-tarfile,prepare a binary package as compressed tar files), M2TARFILE=$enableval)
AC_SUBST(TARLIBS,no)   AC_ARG_ENABLE(tarlibs, AS_HELP_STRING(--enable-tarlibs,include symbolic links to needed shared libraries for tar), TARLIBS=$enableval)
AC_SUBST(SHARED,no)    AC_ARG_ENABLE(shared, AS_HELP_STRING(--enable-shared,enable building of shared libraries), SHARED=$enableval)

test "$MEMDEBUG" = "yes" && DEBUG=yes
test "$DEBUG" = "yes" && STRIP=no
test "$PROFILING" = "yes" && CFLAGS="$CFLAGS -pg" CXXFLAGS="$CXXFLAGS -pg" LDFLAGS="$LDFLAGS -pg" STRIP=no

AC_SUBST(PACKAGES,"
  Macaulay2Doc
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
  Regularity
  SchurRings
  SimplicialComplexes
  LLLBases
  Parsing
  TangentCone
  ChainComplexExtras
  Benchmark
  Schubert2
  Browse
  InvolutiveBases
")

# convert the newlines in the list above to spaces
PACKAGES=`echo $PACKAGES`

AC_DEFINE_UNQUOTED(PACKAGES,"$PACKAGES",the list packages included with the release of Macaulay 2)

AC_DEFINE_UNQUOTED(buildsystemtype,"$build",the type of system on which the package was built)
AC_DEFINE_UNQUOTED(hostsystemtype,"$host",the type of system on which the package runs)

case $host in
   i486-*|i586-*|i686-*)
	newhost=`echo $host | sed s/i.86-/i386-/`
	AC_MSG_NOTICE([warning: building for host $host might not be compatible enough, consider using option "--build=$newhost"]) ;;
esac

AC_SUBST(AUTOINST,no)
AC_ARG_ENABLE(
    auto-instantiation, 
    AS_HELP_STRING([--enable-auto-instantiation,enable automatic instantiation of C++ templates (it uses -frepo option to g++)]),
    AUTOINST=$enableval)
test "$AUTOINST" = no; val=$?; AC_DEFINE_UNQUOTED(AUTOINST,$val,whether to instantiate templates automatically)

AC_SUBST(IMPLINST,yes)
AC_ARG_ENABLE(
    implicit-templates,
    AS_HELP_STRING([--disable-implicit-templates,disable implicit instantiation of C++ templates (it uses -fno-implicit-templates option to g++)]),
    IMPLINST=$enableval)
test "$IMPLINST" = no; val=$?; AC_DEFINE_UNQUOTED(IMPLINST,$val,whether to instantiate templates implicitly)

AC_SUBST(DEVELOPMENT,no)
AC_ARG_ENABLE(development, AS_HELP_STRING(--enable-development,build a development version), DEVELOPMENT=$enableval)
if test "$DEVELOPMENT" = yes
then AC_DEFINE_UNQUOTED(DEVELOPMENT,1,whether to build a development version)
fi

AC_SUBST(DEVELOPER,)
AC_ARG_WITH(developer, AS_HELP_STRING(--with-developer=...,specify the name of the developer ($USER)),DEVELOPER=$withval)

AC_SUBST(DUMPDATA,)
AC_ARG_ENABLE(dumpdata, AS_HELP_STRING(--disable-dumpdata,do not cache data with dumpdata), DUMPDATA=$enableval)
if ! test "$DUMPDATA"
then case $OS in
	 Linux|SunOS) DUMPDATA=yes;;
	 *) DUMPDATA=no;;
     esac
     AC_MSG_NOTICE([default dumpdata option for $OS operating system used, value: $DUMPDATA])
else case $DUMPDATA in
         yes|no) ;;
	 *) AC_MSG_ERROR([dumpdata option: invalid value: $DUMPDATA])
     esac
fi
test "$DUMPDATA" = no;   val=$?; AC_DEFINE_UNQUOTED(DUMPDATA,  $val,whether to use dumpdata)

AC_SUBST(TAR_COMPRESS_OPTION,)
case $COMPRESS in
   gz) TAR_COMPRESS_OPTION=--gzip ;;
  bz2) TAR_COMPRESS_OPTION=--bzip ;;
    *) AC_MSG_ERROR(unrecognized option for enable-compress) ;;
esac

AC_SUBST(SRCTARFILE,)
AC_ARG_WITH(srctarfile, AS_HELP_STRING(--with-srctarfile=...,source tar file to use for inclusion with distributions (cygwin)),
    dirname=`dirname  "$withval"`
    SRCTARFILE=`cd "$dirname" && pwd`/`basename "$withval"`)

AC_SUBST(CYGWIN,no)
AC_ARG_ENABLE(cygwin,  AS_HELP_STRING(--enable-cygwin,prepare a binary package for cygwin (disables encap and enables strip)), CYGWIN=$enableval)
if test "$CYGWIN" = yes 
then ENCAP=yes STRIP=yes M2TARFILE=yes
     if ! test "$SRCTARFILE" -o -d "$srcdir/.svn"
     then AC_MSG_ERROR([source tar file needed for cygwin distribution, specify it with --with-srctarfile=...])
     fi
fi

AC_SUBST(DEB,no)
AC_ARG_ENABLE(deb,  AS_HELP_STRING(--enable-deb,prepare a *.deb package (for debian, ubuntu, ...) (enables encap)), DEB=$enableval)
test "$DEB" = yes && ENCAP=yes M2TARFILE=yes

AC_SUBST(RPM,no)
AC_ARG_ENABLE(rpm,  AS_HELP_STRING(--enable-rpm,prepare a *.rpm package (for red hat based systems) (enables encap)), RPM=$enableval)
test "$RPM" = yes && ENCAP=yes M2TARFILE=yes

AC_SUBST(DMG,no)
AC_ARG_ENABLE(dmg,  AS_HELP_STRING(--enable-dmg,prepare a *.dmg package (for Mac OS) (enables encap)), DMG=$enableval)
test "$DMG" = yes && ENCAP=yes M2TARFILE=yes

AC_SUBST(M2SUFFIX,)
test "$program_suffix" != NONE && M2SUFFIX=$program_suffix
AC_ARG_ENABLE(dummy, AS_HELP_STRING(--program-suffix=...,suffix to append to executable name M2))

AC_DEFINE(M2_CONFIG_H,1,a macro definition to ensure our config.h was the one loaded)

AC_DEFINE_UNQUOTED(M2SUFFIX,"$M2SUFFIX",[suffix to append to executable name M2])

WITH_NEWLINE_CR=0
WITH_NEWLINE_CRLF=0
AC_ARG_WITH(newline, AS_HELP_STRING([--with-newline=...], [crlf, cr, or lf (the default)]),
    [ case $withval in
	  crlf) WITH_NEWLINE_CRLF=1 WITH_NEWLINE_CR=0 ;;
	  cr)   WITH_NEWLINE_CR=1 WITH_NEWLINE_CRLF=0 ;;
	  lf)   WITH_NEWLINE_CR=0 WITH_NEWLINE_CRLF=0 ;;
	  *)    AC_MSG_ERROR([--with-newline expected crlf, cr, or lf]) ;;
      esac ])
AC_DEFINE_UNQUOTED(WITH_NEWLINE_CRLF,$WITH_NEWLINE_CRLF,[whether newline is cr lf])
AC_DEFINE_UNQUOTED(WITH_NEWLINE_CR, $WITH_NEWLINE_CR,   [whether newline is cr])

AC_DEFINE_UNQUOTED(EXEEXT,"$EXEEXT",[suffix the compiler appends to executable filenames])

AC_SUBST(ISYSTEM,no)
AC_MSG_CHECKING(whether g++ gets confused about C linkage in system library files)
AC_LANG(C++)
AC_COMPILE_IFELSE([# 1 "foo" 1 3 4
	template <class T> struct A { };],AC_MSG_RESULT(no);ISYSTEM=yes,AC_MSG_RESULT(yes))

AC_SUBST(BUILTLIBS,)

# this list reflects dependencies
#  factory needs ntl and gmp, libfac needs factory
#  lapack includes blas, makes both libblas and liblapack
#  mpfr needs gmp
AC_SUBST(LIBLIST, "gc    gdbm    gmp    mpfr    readline    ntl    factory        libfac lapack")
AC_SUBST(LIBFILES,"libgc libgdbm libgmp libmpfr libreadline libntl libcf libcfmem libfac libblas liblapack")
for i in $LIBLIST
do eval BUILD_$i=no
done

LIBLIST=" $LIBLIST "
AC_ARG_ENABLE(build-libraries, AS_HELP_STRING(--enable-build-libraries=...,list of libraries to build from downloaded source code (e.g., gc gdbm gmp mpfr readline ntl factory libfac lapack)),
    [for i in $enableval
    do case $LIBLIST in
	    *" $i "*) eval BUILD_$i=yes ;;
	    *) AC_MSG_ERROR(unrecognized library name: $i) ;;
       esac
    done])

AC_LANG(C)
AC_SEARCH_LIBS(tgoto,ncurses curses)
if test $BUILD_readline = no
then AC_SEARCH_LIBS(add_history,history,,BUILD_readline=yes)
     AC_SEARCH_LIBS(readline,readline,,BUILD_readline=yes)
     AC_CHECK_HEADER(readline/readline.h,,BUILD_readline=yes)
     if test $BUILD_readline = yes
     then AC_MSG_NOTICE(readline library will be compiled)
     fi
fi
test $BUILD_readline = yes && BUILTLIBS="-lreadline -lhistory $BUILTLIBS"

AC_LANG(C)
if test $BUILD_gmp = no
then AC_SEARCH_LIBS(__gmpz_init,gmp,,BUILD_gmp=yes)
     AC_CHECK_HEADER(gmp.h,,BUILD_gmp=yes)
fi
test $BUILD_gmp = yes && BUILTLIBS="-lgmp $BUILTLIBS"

AC_SUBST(ULIMIT_T,yes)
AC_SUBST(ULIMIT_M,yes)
AC_SUBST(ULIMIT_V,yes)
( ulimit -t 2000 2>/dev/null) || ULIMIT_T=no
( ulimit -m 2000 2>/dev/null) || ULIMIT_M=no
( ulimit -v 2000 2>/dev/null) || ULIMIT_V=no

AC_LANG(C)
if test $BUILD_gmp = yes
then BUILD_mpfr=yes
elif test $BUILD_mpfr = no
then AC_SEARCH_LIBS(mpfr_init,mpfr,,BUILD_mpfr=yes)
     # check for lgamma -- it's in 2.3.0 but not in 2.2.1
     AC_SEARCH_LIBS(mpfr_lgamma,mpfr,,BUILD_mpfr=yes)
     AC_CHECK_HEADER(mpfr.h,,BUILD_mpfr=yes,[#include <gmp.h>])
     if test $BUILD_mpfr = no
     then AC_MSG_CHECKING([whether mpfr library is recent enough, using mpfr.h])
	  AC_RUN_IFELSE([ 
		  #include <stdio.h>
		  #include <mpfr.h>
		  main () {
		      FILE *msg = fdopen(AS_MESSAGE_FD,"w");
		      unsigned major, minor, patch_level;
		      major = MPFR_VERSION_MAJOR;
		      minor = MPFR_VERSION_MINOR;
		      patch_level = MPFR_VERSION_PATCHLEVEL;
		      fprintf(msg,"(version %d.%d.%d found) ", major, minor, patch_level);
		      return !( major > 2 || major == 2 && minor >= 3 ); }],
	     [ AC_MSG_RESULT(yes) ],
	     [ AC_MSG_RESULT([no, version at least 2.3.0 is required (mpfr will be built from downloaded sources)]); BUILD_mpfr=yes ],
	     [ AC_MSG_RESULT([cross-compiling, test not possible]) ])
     fi
fi
test $BUILD_mpfr = yes && BUILTLIBS="-lmpfr $BUILTLIBS"

AC_LANG(C)
if test $BUILD_gc = no
then AC_SEARCH_LIBS(GC_free,gc,,BUILD_gc=yes)
     AC_CHECK_HEADER(gc/gc.h,,BUILD_gc=yes)
     if test $BUILD_gc = no
     then AC_MSG_CHECKING(whether libgc is recent enough)
	  AC_RUN_IFELSE([ 
		  #include <stdio.h>
		  main () {
		      extern unsigned GC_version;
		      FILE *msg = fdopen(AS_MESSAGE_FD,"w");
		      unsigned major, minor, alpha;
		      major = GC_version >> 16;
		      minor = (GC_version >> 8) & 0xff;
		      alpha = GC_version & 0xff;
		      if (alpha == 0xff) fprintf(msg,"(version %d.%d found) ", major, minor);
		      else fprintf(msg,"(version %d.%d alpha %d found) ", major, minor, alpha);
		      return !( major >= 7 ); }],
	     [ AC_MSG_RESULT(yes) ],
	     [ AC_MSG_RESULT([no, version at least 7.0 is required (gc will be built from downloaded sources)]); BUILD_gc=yes ],
	     [ AC_MSG_RESULT([cross-compiling, test not possible]) ])
     fi
fi
test $BUILD_gc = yes && BUILTLIBS="-lgc $BUILTLIBS"

AC_LANG(C)
if test $BUILD_gdbm = no
then AC_SEARCH_LIBS(gdbm_close,gdbm,,BUILD_gdbm=yes)
     AC_CHECK_HEADER(gdbm.h,,BUILD_gdbm=yes)
fi
test $BUILD_gdbm = yes && BUILTLIBS="-lgdbm $BUILTLIBS"

if test $BUILD_ntl = no
then
    AC_LANG(C++)
    AC_CHECK_HEADER(NTL/version.h,,BUILD_ntl=yes)

    AC_MSG_CHECKING(for ntl library)
    FOUND=
    SAVELIBS=$LIBS
    for lib in "" "-lntl"
    do  test "$lib" && LIBS="$lib $LIBS"
	AC_LINK_IFELSE( [
	        #include <NTL/tools.h>
		int main () { _ntl_GetTime(); }
		],
	    FOUND=yes; break;,
	    LIBS=$SAVELIBS)
    done
    if test "$FOUND"
    then if test "$lib"
	then AC_MSG_RESULT([$lib])
	else AC_MSG_RESULT([none needed])
	fi
    else AC_MSG_RESULT(none found)
	BUILD_ntl=yes
    fi 

    if test $BUILD_ntl = no
    then AC_MSG_CHECKING([whether ntl library is recent enough, using NTL/version.h])
	 AC_RUN_IFELSE([[
		 #include <stdio.h>
		 #include <NTL/version.h>
		 main () {
		     FILE *msg = fdopen(]]AS_MESSAGE_FD[[,"w");
		     unsigned major, minor, revision;
		     major = NTL_MAJOR_VERSION;
		     minor = NTL_MINOR_VERSION;
		     revision = NTL_REVISION;
		     fprintf(msg,"(version %d.%d.%d found) ", major, minor, revision);
		     return !( major > 5 || major == 5 && minor > 4 || major == 5 && minor == 4 && revision >= 1 ); }]],
	    [ AC_MSG_RESULT(yes) ],
	    [ AC_MSG_RESULT([no, version at least 5.4.1 is required (ntl will be built from downloaded sources)]); BUILD_ntl=yes ])
    fi

    if test $BUILD_ntl = no
    then AC_MSG_CHECKING([whether ntl library is compiled with gmp, using NTL/config.h])
	 AC_RUN_IFELSE([[
		 #include <NTL/config.h>
		 main () {
		     return !( 
			 #ifdef NTL_GMP_LIP
			 1
			 #else
			 0
			 #endif
			 ); }]],
	    [ AC_MSG_RESULT(yes) ],
	    [ AC_MSG_RESULT([no, ntl compiled with gmp is required (ntl will be built from downloaded sources)]); BUILD_ntl=yes ])
    fi

fi
test $BUILD_ntl = yes && BUILTLIBS="-lntl $BUILTLIBS"

AC_ARG_WITH(veclib, AS_HELP_STRING(--with-veclib,[use "-framework vecLib" for lapack]), LIBS="-framework vecLib $LIBS" )

AC_SUBST(LAPACK,yes)
AC_ARG_WITH(lapack, AS_HELP_STRING(--without-lapack,[compile and link without lapack and blas]), LAPACK=$withval)
test "$LAPACK" = no; val=$?
AC_DEFINE_UNQUOTED(LAPACK,$val,whether to link with lapack)

# we need to control the position of -lfac -lcf in the link line, so we don't add them to $LIBS nor to $BUILTLIBS, putting them in $SINGULARLIBS instead
SAVELIBS=$LIBS
if ! test "$SINGULARLIBS"
then SINGULARLIBS="-lfac -lcf"
fi
AC_SUBST(SINGULARLIBS) dnl no initial value
while test $BUILD_factory = no -o $BUILD_libfac = no
do AC_LANG(C++)
   AC_CHECK_HEADER(factory.h,,BUILD_factory=yes BUILD_libfac=yes ; break)
   AC_CHECK_HEADER(factor.h,,BUILD_factory=yes BUILD_libfac=yes ; break)
   SLIBS="$SINGULARLIBS -lcfmem"
   LIBS="$SLIBS $LIBS"
   AC_MSG_CHECKING([whether factory library is installed ($SLIBS)])
   AC_LINK_IFELSE( [[ #include <factory.h>
		      #include <templates/ftmpl_inst.cc>
		      int main () { return 0 ; }
	       ]],
       AC_MSG_RESULT(yes),
       AC_MSG_RESULT(no)
       SLIBS="$SLIBS -lntl"
       LIBS="$SLIBS $SAVELIBS"
       AC_MSG_CHECKING([whether factory library is installed ($SLIBS)])
       AC_LINK_IFELSE( [[ #include <factory.h>
			  #include <templates/ftmpl_inst.cc>
			  int main () { return 0 ; }
		   ]],
	   AC_MSG_RESULT(yes),
	   AC_MSG_RESULT(no); BUILD_factory=yes BUILD_libfac=yes; break
	   ))
   AC_MSG_CHECKING([whether (recent) libfac and factory libraries are installed ($LIBS)])
   AC_RUN_IFELSE( [[ /* this is line 1 */
	   #include <stdio.h>
	   #include <string.h>
	   #include <factory.h>
	   #include <factor.h>
	   #include <templates/ftmpl_inst.cc>
	   extern const char * libfac_version;
	   int main () {
		FILE *msg = fdopen(]]AS_MESSAGE_FD[[,"w");
		static int factoryV[3], fV[3] = {3,0,3}, fOK;
		static int  libfacV[3], lV[3] = {3,0,1}, lOK;
		sscanf(FACTORYVERSION,"%d.%d.%d", &factoryV[0], &factoryV[1], &factoryV[2]);
		sscanf(libfac_version,"%d.%d.%d", &libfacV[0], &libfacV[1], &libfacV[2]);
		fOK = factoryV[0] > fV[0] ||
		      factoryV[0] == fV[0] && factoryV[1] > fV[1] ||
		      factoryV[0] == fV[0] && factoryV[1] == fV[1] && factoryV[2] >= fV[2];
		lOK = libfacV[0] > lV[0] ||
		      libfacV[0] == lV[0] && libfacV[1] > lV[1] ||
		      libfacV[0] == lV[0] && libfacV[1] == lV[1] && libfacV[2] >= lV[2];
		fprintf(msg,"(factory-%s %s %d.%d.%d, libfac-%s %s %d.%d.%d) ",
		    FACTORYVERSION, fOK ? ">=" : "<", fV[0], fV[1], fV[2],
		    libfac_version, lOK ? ">=" : "<", lV[0], lV[1], lV[2]
		    );
		return !( lOK && fOK );
		}
	   ]],
	AC_MSG_RESULT(yes),
	AC_MSG_RESULT(no); BUILD_factory=yes BUILD_libfac=yes)
   break
done
LIBS=$SAVELIBS

# we need to do the fortran library testing last, in case AC_SEARCH_LIBS adds
# one of them to $LIBS, making it impossible to check for the presence of C or
# C++ libraries.  (I'm not sure why putting -llapack on the gcc command line
# needlessly causes the library to be linked against.)
if test "$LAPACK" = yes
then AC_MSG_CHECKING([whether the vecLib framework is available])
     SAVELIBS=$LIBS LIBS="-framework vecLib $LIBS"
     AC_LANG(C)
     AC_LINK_IFELSE(
	 AC_LANG_PROGRAM(,[sgemv_();dgetrf_();]),
	 [
	     AC_MSG_RESULT(yes)
	     ],
	 [
	     AC_MSG_RESULT(no)
	     LIBS=$SAVELIBS
	     if test $BUILD_lapack = no
	     then AC_LANG(Fortran)
		  AC_SEARCH_LIBS(sgemv,blas f77blas,,BUILD_lapack=yes)
		  AC_SEARCH_LIBS(dgetrf,lapack,,BUILD_lapack=yes)
	     fi
	 ])
fi
test $BUILD_lapack = yes && BUILTLIBS="-llapack -lblas $BUILTLIBS"

# after this point we add no more libraries to $LIBS, e.g., with AC_SEARCH_LIBS()
SAVELIBS=$LIBS

AC_SUBST(FINALPREFIX,/usr)
AC_ARG_WITH(final-prefix,
    AS_HELP_STRING(--with-final-prefix=...,final installation prefix (/usr) (used by libtool and our installation scripts)),
    FINALPREFIX=$withval)
case $FINALPREFIX in
    /*) ;;
    *) AC_MSG_ERROR(final prefix should begin with /) ;;
esac

AC_SUBST(NM_DEMANGLES,)
AC_MSG_CHECKING(whether nm accepts the demangle option)
if nm --help 2>&1 | grep demangle >/dev/null
   then NM_DEMANGLES=yes; AC_MSG_RESULT(yes)
   else NM_DEMANGLES=no ; AC_MSG_RESULT(no)
fi

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

AC_SUBST(LIBRARIES,)
for i in $LIBLIST
do eval t=\$BUILD_$i
   test "$t" = yes && LIBRARIES="$LIBRARIES $i"
done

TESTFLAG=-Wno-strict-aliasing
AC_MSG_CHECKING(whether $CC accepts $TESTFLAG)
AC_SUBST(HAVE_NO_STRICT_ALIASING,)
SAVE=$CFLAGS
CFLAGS="$TESTFLAG $CFLAGS"
AC_LANG(C)
AC_COMPILE_IFELSE([int main() {}], HAVE_NO_STRICT_ALIASING=yes, HAVE_NO_STRICT_ALIASING=no)
M2_CPPFLAGS="$M2_CPPFLAGS $TESTFLAG"
CFLAGS=$SAVE
AC_MSG_RESULT($HAVE_NO_STRICT_ALIASING)

AC_SUBST(M2_CPPFLAGS,)

AC_MSG_CHECKING(whether $CC accepts -Wfatal-errors)
AC_LANG(C)
SAVE=$CFLAGS
CFLAGS="$CFLAGS -Wfatal-errors"
AC_COMPILE_IFELSE([int main() {}], 
    M2_CPPFLAGS="$M2_CPPFLAGS -Wfatal-errors"
    AC_MSG_RESULT(yes, adding -Wfatal-errors to flags),
    AC_MSG_RESULT(no))
CFLAGS=$SAVE

IS_DECLARED(char **,sys_errlist, [
      #include <stdio.h>
      #include <errno.h>
      ])
IS_DECLARED(char **,h_errlist, [
      #include <stdio.h>
      #include <errno.h>
      ])

AC_SUBST(DUMPDATAFILE,Macaulay2-$ARCH-data)

if test "$DEBUG" = yes
then M2_CPPFLAGS="$M2_CPPFLAGS -DDEBUG"
else M2_CPPFLAGS="$M2_CPPFLAGS -DNDEBUG"
fi

if test "$OPTIMIZE" = yes
then CFLAGS="$CFLAGS -O3"
     CXXFLAGS="$CXXFLAGS -O3"
else CFLAGS="$CFLAGS -O0 -fno-inline"
     CXXFLAGS="$CXXFLAGS -O0 -fno-inline"
fi

test "$MEMDEBUG" = yes && M2_CPPFLAGS="$M2_CPPFLAGS -DMEMDEBUG"
test "$DEBUG" = yes && M2_CPPFLAGS="$M2_CPPFLAGS -DGC_DEBUG"

if test "$OS" = MacOS 
then 
    # we don't know what this does, but some apple documentation writers seem to like it:
    LDFLAGS="$LDFLAGS -bind_at_load"

    # this one makes it find and use our readline.a first, even if there is a file readline.dylib in /usr/lib
    # the point is that the system's readline.dylib might be much older and conflict with our newer one
    LDFLAGS="$LDFLAGS -Wl,-search_paths_first"
fi

AC_SUBST(package,Macaulay2-$PACKAGE_VERSION)
AC_SUBST(encapdir,)
if test "$ENCAP" = yes
then encapdir="\${prefix}/$package"
else encapdir="\${prefix}"
fi
AC_SUBST(EXE,$M2SUFFIX$EXEEXT)

AC_SUBST(DESC,$PACKAGE_VERSION-$host_cpu-$OS-$REL)
# we used to use @ ARCH @ instead of @ host_cpu @ here, but for DEC alphas,
# and perhaps other machines, it gives more information.  The script
# config.guess assembles host_cpu from /proc/cpuinfo, and can be something
# like "alphaev6" or "alphaev56", but ARCH, which is determined from 'uname'
# by the 'configure' script, will be simply "alpha".

if ! test "$FAKEROOT"
then if test "$DEB" = yes -o "$RPM" = yes
     then AC_MSG_ERROR([program "fakeroot" not installed, required for creating *.deb or *.rpm distributions])
     elif test "$M2TARFILE" = yes
     then AC_MSG_NOTICE([warning: program "fakeroot" not installed, tar file will appear to contain files not owned by root])
     fi
fi

# Here we split up LIBS into two parts, LIBS_STATIC and LIBS_DYNAMIC.
# M2 will get linked against LIBS_STATIC and then against LIBS_DYNAMIC.
# Under --enable-static the libraries in LIBS_STATIC will be linked statically.
AC_SUBST(LIBS_STATIC,)
AC_SUBST(LIBS_DYNAMIC,)
for i in $LIBS
do case $i in 
	# we must link dynamically with some libraries
	-lncurses | dnl ncurses has a database which we aren't providing and whose location we don't know
	-lsocket  | dnl dynamic loading of libraries is used to determine the current way to look up an IP number
	-lgcc_s*  ) dnl libgcc_s comes only in dynamic form
	   LIBS_DYNAMIC="$LIBS_DYNAMIC $i" ;;
	*) 
	   LIBS_STATIC="$LIBS_STATIC $i" ;;
   esac
done

# Here we compute the relative positions of various subdirectories with respect to ${prefix}, so we can record the layout
# of the directory tree, and check that the subdirectories specified are within the tree.
AC_SUBST(bindirtail,) AC_SUBST(datadirtail,) AC_SUBST(datarootdirtail,) AC_SUBST(docdirtail,) AC_SUBST(dvidirtail,)
AC_SUBST(htmldirtail,) AC_SUBST(includedirtail,) AC_SUBST(infodirtail,) AC_SUBST(libdirtail,) AC_SUBST(libexecdirtail,)
AC_SUBST(localedirtail,) AC_SUBST(localstatedirtail,) AC_SUBST(mandirtail,) AC_SUBST(pdfdirtail,)
AC_SUBST(psdirtail,) AC_SUBST(sbindirtail,) AC_SUBST(sharedstatedirtail,) AC_SUBST(sysconfdirtail,)

AC_MSG_NOTICE([using LIBRARIES    = $LIBRARIES (libraries to be built from downloaded sources)])
AC_MSG_NOTICE([using BUILTLIBS    = $BUILTLIBS])
AC_MSG_NOTICE([using SINGULARLIBS = $SINGULARLIBS])
AC_MSG_NOTICE([using LIBS_STATIC  = $LIBS_STATIC])
AC_MSG_NOTICE([using LIBS_DYNAMIC = $LIBS_DYNAMIC])

BUILDING=yes
AC_ARG_ENABLE(building, AS_HELP_STRING(--disable-building,disable automatic building of libraries), BUILDING=$enableval)
if test "$BUILDING" = no -a "$LIBRARIES"
then AC_MSG_ERROR([automatic building of libraries disabled, but some must be built])
fi

# Here we insert autoconf's default values so we can compute the relative locations in the directory tree
test "$prefix" || prefix=/usr/local
test "$exec_prefix" = NONE && exec_prefix='${prefix}'
for i in bindir datadir datarootdir docdir dvidir htmldir includedir infodir libdir libexecdir localedir localstatedir mandir pdfdir psdir sbindir sharedstatedir sysconfdir
do eval w=\$$i ; eval v="$w" ; eval v="$v" ; eval v="$v" ; eval v="$v" ; eval v="$v"
   case $v in
     "$prefix" | "$prefix"/) eval ${i}tail="$prefix" ;;
     "$prefix"/*) eval ${i}tail=`echo $v | sed "s,^$prefix/,,"` ;; # let's just hope $prefix contains no comma
     *) AC_MSG_ERROR(expected "$i" value "$w" to start with "prefix" value "$prefix") ;;
   esac
done

AC_SUBST(docdirtail,$datadirtail/doc)
AC_SUBST(packagesdirtail,$datadirtail/Macaulay2)
AC_SUBST(docpackagesdirtail,$docdirtail/Macaulay2)
AC_SUBST(libm2dirtail,$libdirtail/Macaulay2)
AC_SUBST(man1dirtail,$mandirtail/man1)
AC_SUBST(man3dirtail,$mandirtail/man3)
AC_SUBST(emacsdirtail,$datadirtail/emacs/site-lisp)
AC_SUBST(librariesdirtail,$libm2dirtail/Core)
AC_SUBST(coredatadirtail,$packagesdirtail/Core)
AC_SUBST(corem2srcdirtail,$coredatadirtail/m2)
AC_SUBST(corecachedirtail,$libm2dirtail/Core/cache)

AC_OUTPUT()


dnl # Local Variables:
dnl # mode: sh
dnl # compile-command: "make -f Makefile"
dnl # End:
