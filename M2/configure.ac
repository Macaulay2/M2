dnl This is the only place where the Macaulay2 version number appears:

    AC_INIT(Macaulay 2, 1.4.0.1, Macaulay2@math.uiuc.edu, Macaulay2)

dnl When incrementing the version number, add an entry to distributions/deb/changelog, too.

dnl The convention for version numbers of Macaulay2 is this:
dnl   1.3     : a relatively major release, such as occurs every six months
dnl   1.3.1   : a minor update, such as might occur a week after a major release
dnl   1.3.1.1 : a development version, with no binary distributions
dnl   1.3.9   : almost ready for a major release
dnl   2.0     : this just means we've gotten bored seeing "1" at the beginning of the number

AC_CONFIG_HEADERS(include/M2/config.h)
AC_CONFIG_FILES(m4_include(config/files))
AC_SUBST(CONFIGURED_FILES,"$ac_config_files")
AC_SUBST(CONFIG_ARGS,"$ac_configure_args")
echo "'$0' $ac_configure_args" > config.args
C_CONFIG_ARGS=` echo "$ac_configure_args" | sed -e 's=\\\\=\\\\\\\\=g' -e 's=\\"=\\\\"=g' `
AC_DEFINE_UNQUOTED(CONFIG_ARGS,"$C_CONFIG_ARGS",arguments used for configure)
AC_SUBST(CONFIG_CMD,"'$0' $ac_configure_args")

define(TO_UPPER,[translit($1, [a-z], [A-Z])])
dnl we should be able to replace IS_DECLARED with AC_CHECK_DECLS:
AC_DEFUN(IS_DECLARED,[
  AC_CACHE_CHECK([whether $2 is declared],[m2_cv_$2_is_declared],
  	[AC_LANG(C)]
	[AC_COMPILE_IFELSE([AC_LANG_SOURCE([$3
	    int main () { $1 x = $2 ; } ])],
	 m2_cv_$2_is_declared=yes,
	 m2_cv_$2_is_declared=no)
	])
  test $m2_cv_$2_is_declared = no; val=$?
  AC_DEFINE_UNQUOTED(TO_UPPER($2_IS_DECLARED),$val,[whether $2 is declared in errno.h or stdio.h])
])

echo "relevant environment variable values, if any:"
for x in CC FC CXX CPPFLAGS CFLAGS FCFLAGS CXXFLAGS LDFLAGS LIBS ISSUE DISTRIBUTION
do eval v=\$$x
   test "$v" && echo "   $x=$v"
done

AC_SUBST(DISTRIBUTION) # use this starting number to sequentially number the downstream distributions
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
AC_CHECK_PROGS(OBJCOPY,objcopy)
AC_CHECK_PROGS(ETAGS,etags :)
AC_CHECK_PROGS(LINTIAN,lintian)
AC_CHECK_PROGS(LDD,ldd)
AC_CHECK_PROGS(FAKEROOT,fakeroot)
AC_CHECK_PROGS(RPMLINT,rpmlint)
AC_CHECK_PROGS(TAR,gtar gnutar tar)
AC_CHECK_PROGS(FIND,gfind find)
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

AC_MSG_CHECKING(whether $MAKE is GNU make)
if "$MAKE" -n --version nothing | head -1 | grep GNU >/dev/null 2>&1
then AC_MSG_RESULT(yes)
else AC_MSG_ERROR("$MAKE: GNU make is required")
fi

AC_MSG_CHECKING(whether $TAR is GNU tar)
if "$TAR" --version | head -1 | grep "GNU tar" >/dev/null 2>&1
then AC_MSG_RESULT(yes)
else AC_MSG_ERROR("$TAR: GNU tar is required")
fi

dnl AC_MSG_CHECKING(whether $FIND is GNU find)
dnl if "$FIND" --version | head -1 | grep "GNU find" >/dev/null 2>&1
dnl then AC_MSG_RESULT(yes)
dnl else AC_MSG_ERROR("$FIND: GNU find is required")
dnl fi

AC_SUBST(NODENAME,"`uname -n`")
AC_DEFINE_UNQUOTED(NODENAME,"$NODENAME",hostname used for compilation)

AC_SUBST(ISSUE) # no initial value
AC_ARG_WITH(issue, AS_HELP_STRING([--with-issue=...,specify the OS issue (e.g., Ubuntu-7.10)]),ISSUE=$withval)
if test -f /etc/issue -a ! "$ISSUE"
then ISSUE_FLAVOR=[`</etc/issue head -1 | sed 's/^\([A-Za-z ]*\).*/\1/' | sed 's/ //g' `]
     ISSUE_RELEASE=[`</etc/issue head -1 | sed 's/[^0-9]*\([0-9.]*\).*/\1/'`]
     case $ISSUE_FLAVOR in
	 "FedoraCore"*) ISSUE_FLAVOR=FedoraCore ;;
	 "Fedora"*) ISSUE_FLAVOR=Fedora ;;
	 "RedHatEnterprise"*) ISSUE_FLAVOR=RedHatEnterprise ;;
	 "RedHat"*) ISSUE_FLAVOR=RedHat ;;
	 "Scientific"*) ISSUE_FLAVOR="ScientificLinux" ;;
	 "Debian"*) ISSUE_FLAVOR=Debian ;;  # Debian GNU/Linux 4.0 \n \l
	 *"openSUSE") ISSUE_FLAVOR=openSUSE ;; # Welcome to openSUSE 11.2 "Emerald" - Kernel \r (\l).
	 "") ISSUE_FLAVOR=unknown ;;
     esac
     case $ISSUE_RELEASE in
	 "") ISSUE_RELEASE=unknown ;;
     esac
     ISSUE=$ISSUE_FLAVOR-$ISSUE_RELEASE
fi

AC_SUBST(OS,"`  uname -s | sed s=/=-=g  `")
AC_SUBST(REL,"`  uname -r | sed -e s=/=-=g -e 's= =+=' -e 's=[()]==g'  `")
AC_SUBST(UNAME_INFO_COMMAND," uname -pmso 2>/dev/null || uname -pms ")
AC_SUBST(UNAME_INFO,"` eval $UNAME_INFO_COMMAND `")
dnl interesting behavior: $A expansion happens after statement separation!
dnl       $ A="echo hi 2>/dev/null "
dnl       $ echo "`$A`"
dnl       hi 2>/dev/null
dnl       $ $A
dnl       hi 2>/dev/null
dnl       $ eval $A
dnl       hi

case $OS-$REL in
  Darwin-7.*) REL=10.3 ; OS=MacOS ;;
  Darwin-8.*) REL=10.4 ; OS=MacOS ;;
  Darwin-9.*) REL=10.5 ; OS=MacOS ;;
  Darwin-10.*) REL=10.6 ; OS=MacOS ;;
  Darwin-11.*) REL=10.7 ; OS=MacOS ;;
  Darwin-12.*) REL=10.8 ; OS=MacOS ;;
  Darwin-*) echo "Darwin release number translation needed in configure.ac" >&2 ; exit 1
esac

case $OS in
  # Here we normalize the name of the OS for windows for startup.m2, which
  # needs to know that C:/FOO/BAR is an absolute path.
  CYGWIN*) OS=MicrosoftWindows ; ISSUE=Cygwin ;;
esac

test "$ISSUE" || ISSUE=$REL

AC_DEFINE_UNQUOTED(ISSUE,"$ISSUE",[issue (flavor) of operating system, if any])
AC_MSG_NOTICE([operating system to be used in distribution file names: $OS])
AC_MSG_NOTICE([issue to be used in distribution file names: $ISSUE])

case $OS in
  MacOS) 
dnl 	AC_MSG_NOTICE([[*** This is MacOS, but we are not adding -lgcc_s.10.5 to LIBS for you, 
dnl because there might be a better way.  See the discussion in emacs info about -static-libgcc
dnl and -shared-libgcc, try both of those in LDFLAGS, and also try -lgcc and -lgcc_eh in LIBS.
dnl If all else fails, add "LIBS=-lgcc_s.10.5" to the configure command line.  Check whether the 
dnl binary ends up requiring a sharable library with a similar name. ***]])
	# LIBS="$LIBS -lgcc_s.10.5" # what kind of crock is this??
	;;
esac

AC_DEFINE_UNQUOTED(OS,"$OS",[operating system name, obtained with uname -s, perhaps modified])
AC_DEFINE_UNQUOTED(REL,"$REL",[operating system release, obtained with uname -r])

AC_CONFIG_AUX_DIR(config)

AC_CANONICAL_HOST()
AC_SUBST(ARCH,$build_cpu)
AC_DEFINE_UNQUOTED(ARCH,"$ARCH",[machine hardware type])

AC_VALIDATE_CACHED_SYSTEM_TUPLE()
dnl AC_ARG_VAR(CC,C compiler to use)
dnl AC_ARG_VAR(CXX,C++ compiler to use)
AC_PROG_RANLIB()
AC_PROG_INSTALL()
AC_PROG_AWK()

AC_SUBST(OPTIMIZE,yes) AC_ARG_ENABLE(optimize, AS_HELP_STRING(--disable-optimize,disable optimization), OPTIMIZE=$enableval)
AC_SUBST(DEBUG,no)     AC_ARG_ENABLE(debug, AS_HELP_STRING(--enable-debug,enable debugging (and disable stripping)), DEBUG=$enableval)
AC_SUBST(M2_CPPFLAGS,)

AC_PROG_CC() 			# set CFLAGS before this
AC_SUBST(GCC)			# gets set to yes or no by AC_PROG_CC
AC_PROG_CXX()			# set CXXFLAGS before this
AC_SUBST(GXX)			# gets set to yes or no by AC_PROG_CXX
AC_PROG_YACC()

if test "$DEBUG" = yes
then CPPFLAGS="$CPPFLAGS -DDEBUG"
     M2_CPPFLAGS="$M2_CPPFLAGS -DDEBUG"
     if test "$GCC" = yes
     then CFLAGS="$CFLAGS -g3"
	  CXXFLAGS="$CXXFLAGS -g3"
	  LDFLAGS="$LDFLAGS -g3"
     else CFLAGS="$CFLAGS -g"
	  CXXFLAGS="$CXXFLAGS -g"
	  LDFLAGS="$LDFLAGS -g"
     fi
else M2_CPPFLAGS="$M2_CPPFLAGS -DNDEBUG"
     CPPFLAGS="$CPPFLAGS -DNDEBUG"
fi

[
 CFLAGS=` echo $CFLAGS | sed -e 's/-O[0-9]//g' -e 's/   */ /g' -e 's/^  *//' -e 's/ * $//' `
 CXXFLAGS=` echo $CXXFLAGS | sed -e 's/-O[0-9]//g' -e 's/   */ /g' -e 's/^  *//' -e 's/ * $//' `
 FFLAGS=` echo $FFLAGS | sed -e 's/-O[0-9]//g' -e 's/   */ /g' -e 's/^  *//' -e 's/ * $//' `
]

if test "$OPTIMIZE" = yes
then CFLAGS="$CFLAGS -O2"
     CXXFLAGS="$CXXFLAGS -O2"
     FFLAGS="$FFLAGS -O2"
else CFLAGS="$CFLAGS -O0"
     CXXFLAGS="$CXXFLAGS -O0"
     FFLAGS="$FFLAGS -O0"
fi

AC_SUBST(GCCVERSION,)
if test $GCC = yes
then AC_MSG_CHECKING(for $CC version)
     GCCVERSION=`$CC -dumpversion 2>&1 | sed 's/.*-\(.*\):.*/\1/'`
     AC_MSG_RESULT($GCCVERSION)
fi

dnl this macro is available only in autoconf 2.60, but most systems have 2.59 as the default:
dnl AC_PROG_MKDIR_P()

AC_CHECK_SIZEOF([int *])
AC_SUBST(SIZEOF_INT_P,$ac_cv_sizeof_int_p)

dnl Now we make the 32 bit / 64 bit choice explicit, because config.guess may guess it's a 32 bit system even though gcc compiles
dnl 64 bit object code by default, and then mpir's configure script (versions 1.2.1 and 1.3.0-rc1) gets confused.
dnl if test "$GCC" = yes
dnl then if test "$SIZEOF_INT_P" = 4
dnl      then case " $CC $CFLAGS " in
dnl      	    *" -m32 "*) ;;
dnl 	    *) CC="$CC -m32" ;;
dnl 	  esac
dnl      fi
dnl      if test "$SIZEOF_INT_P" = 8
dnl      then case " $CC $CFLAGS " in
dnl      	    *" -m64 "*) ;;
dnl 	    *) CC="$CC -m64" ;;
dnl 	  esac
dnl      fi
dnl fi
dnl if test "$GXX" = yes
dnl then if test "$SIZEOF_INT_P" = 4
dnl      then case " $CXX $CFLAGS " in
dnl      	    *" -m32 "*) ;;
dnl 	    *) CXX="$CXX -m32" ;;
dnl 	  esac
dnl      fi
dnl      if test "$SIZEOF_INT_P" = 8
dnl      then case " $CXX $CFLAGS " in
dnl      	    *" -m64 "*) ;;
dnl 	    *) CXX="$CXX -m64" ;;
dnl 	  esac
dnl      fi
dnl fi

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
	$CXX $CPPFLAGS $CXXFLAGS -O2 -c -o unwindtest-main.$OBJEXT unwindtest-main.cpp &&
	$CXX $CPPFLAGS $CXXFLAGS -O2 -c -o unwindtest-monomials.$OBJEXT unwindtest-monomials.cpp &&
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
	if test "$CXX" = cl
	then lib conf_test2.$OBJEXT
	     lib conf_test4.$OBJEXT
	     $CXX conf_main.cc conf_test1.$OBJEXT conf_test2.lib conf_test3.$OBJEXT conf_test4.lib -o conf_main
	else ar ru libconf_test2.a conf_test2.$OBJEXT
	     $RANLIB libconf_test2.a
	     ar ru libconf_test4.a conf_test4.$OBJEXT
	     $RANLIB libconf_test4.a
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
	 rm -f libconf_test* conf_test* conf_main*
	 AC_MSG_RESULT([link order o1a2o3a4, construction order $CONSTRUCTOR_ORDER, obj order $CONSTRUCTOR_ORDER_OBJ, lib order $CONSTRUCTOR_ORDER_LIB])
    else AC_MSG_ERROR([test failed, see config.log])
    fi

AC_HEADER_TIME() # time.h sys/time.h
AC_CHECK_HEADERS(sys/ioctl.h termios.h sys/mman.h sys/socket.h netdb.h netinet/in.h arpa/inet.h sys/time.h sys/wait.h sys/resource.h io.h linux/personality.h)
AC_CHECK_HEADERS(stddef.h stdint.h inttypes.h bits/wordsize.h endian.h elf.h execinfo.h stdlib.h syscall.h)
AC_CHECK_HEADERS(sys/types.h execinfo.h)
AC_SEARCH_LIBS(clock_gettime,rt)
AC_SEARCH_LIBS(socket,socket)
AC_SEARCH_LIBS(hstrerror,resolv)
AC_SEARCH_LIBS(dlopen,dl)
AC_SEARCH_LIBS(gethostbyname,nsl)
AC_CHECK_FUNCS([somesillyfunction sys_errlist herror error socket backtrace clock_gettime gnu_get_libc_version __environ _environ environ _setmode getaddrinfo hstrerror])
AC_CHECK_FUNCS([gnu_get_libc_version __environ _environ environ _setmode getaddrinfo hstrerror])
AC_CHECK_DECLS([ADDR_NO_RANDOMIZE],,,[#include <linux/personality.h>])
AC_CHECK_DECLS([gnu_get_libc_version])
AC_CHECK_DECLS([sys_errlist,sys_nerr,herror],,,[
	#include <stdio.h>
	#include <errno.h>
	])
AC_CHECK_DECLS([__environ,_environ,environ],,,unistd.h)

AC_SUBST(__INTEL_COMPILER,no)
AC_CHECK_DECL(__INTEL_COMPILER,__INTEL_COMPILER=yes)

dnl this macro is available only in autoconf 2.60, but most systems have 2.59 as the default:
dnl AC_TYPE_INT64_T()

AC_FUNC_ALLOCA()

AC_LANG(C++)
AC_CHECK_HEADERS(NTL/tools.h)
AC_LANG(C)

AC_SUBST(HAVE_PERSONALITY,)
AC_CHECK_FUNC(personality, HAVE_PERSONALITY=1, HAVE_PERSONALITY=0)
AC_DEFINE_UNQUOTED(HAVE_PERSONALITY,$HAVE_PERSONALITY,[whether personality() is found])

dnl AC_SUBST(PTHREADS,yes) AC_ARG_ENABLE(pthreads, AS_HELP_STRING(--disable-pthreads,[disable pthreads (for gc)]), PTHREADS=$enableval)

AC_SUBST(STRIP,no)     AC_ARG_ENABLE(strip, AS_HELP_STRING(--enable-strip,strip the symbol table from the Macaulay 2 binary), STRIP=$enableval)
AC_SUBST(STATIC,no)    AC_ARG_ENABLE(static, AS_HELP_STRING(--enable-static,enable static linking), STATIC=$enableval)
AC_SUBST(MEMDEBUG,no)  AC_ARG_ENABLE(memdebug, AS_HELP_STRING(--enable-memdebug,enable memory allocation debugging), MEMDEBUG=$enableval)
AC_SUBST(ENCAP,no)     AC_ARG_ENABLE(encap, AS_HELP_STRING(--enable-encap,encapsulate all files in a subdirectory at installation time), ENCAP=$enableval)
AC_SUBST(XCODE,no)     AC_ARG_ENABLE(xcode, AS_HELP_STRING(--enable-xcode,create Macaulay2/d/interpret.a for use with xcode), XCODE=$enableval)

dnl AC_SUBST(USE_THREADS,yes) AC_ARG_ENABLE(threads, AS_HELP_STRING(--disable-threads,[disable thread local storage and thread creation]), USE_THREADS=$enableval)
AC_SUBST(CC_THREAD_SUPPORT)
AC_MSG_CHECKING(whether the C compiler accepts the __thread storage attribute)
AC_LANG(C)
AC_COMPILE_IFELSE(
	[AC_LANG_SOURCE([__thread int x;])],
     	CC_THREAD_SUPPORT=yes; [AC_MSG_RESULT(yes)],
	CC_THREAD_SUPPORT=no ; [AC_MSG_RESULT(no)])
dnl if [[ "$USE_THREADS" = yes ]]
dnl then AC_DEFINE(USE_THREADS,1,[whether to use thread local storage])
dnl fi

AC_SUBST(FROBBY,yes)
AC_ARG_ENABLE(frobby, AS_HELP_STRING(--disable-frobby,[do not download, compile, and link with frobby library]), FROBBY=$enableval)
if test "$FROBBY" = yes
then AC_DEFINE(HAVE_FROBBY,1,[whether we are linking with the frobby library])
fi

AC_SUBST(PARI,yes)
AC_ARG_ENABLE(pari, AS_HELP_STRING(--disable-pari,[do not download, compile, and link with pari library]), PARI=$enableval)
if test "$PARI" = yes
then AC_DEFINE(HAVE_PARI,1,[whether we are linking with the pari library])
fi

AC_SUBST(XML,yes)
AC_ARG_ENABLE(xml, AS_HELP_STRING(--disable-xml,[do not link with xml library]), XML=$enableval)
if test "$XML" = yes
then AC_DEFINE(HAVE_XML,1,[whether we are linking with the xml library])
fi

AC_SUBST(PYTHON,no)
AC_ARG_WITH(python, AS_HELP_STRING(--with-python,[link with libpython]), PYTHON=$withval)
if test "$PYTHON" = yes
then AC_DEFINE(HAVE_PYTHON,1,[whether we are linking with the python library])
fi

AC_SUBST(MYSQL,no)
AC_ARG_WITH(mysql, AS_HELP_STRING(--with-mysql,[link with mysql]), MYSQL=$withval)
test "$MYSQL" = no; USE_MYSQL=$?
AC_DEFINE_UNQUOTED(USE_MYSQL,$USE_MYSQL,[whether we are linking with the mysql library])

LIBPYTHONORIG=-lpython2.5
AC_SUBST(LIBPYTHON,$LIBPYTHONORIG)
AC_ARG_WITH(libpython, AS_HELP_STRING(--with-libpython=...,specify the python library ($LIBPYTHON)),LIBPYTHON=$withval)

AC_ARG_ENABLE(altivec, AS_HELP_STRING(--enable-altivec,compile with "-faltivec" option))
if test "$enable_altivec" = yes
then LDFLAGS="$LDFLAGS -faltivec"
     CFLAGS="CFLAGS -faltivec"
     CXXFLAGS="$CXXFLAGS -faltivec"
fi

AC_PROG_FC()
USE_FCLIBS=unspecified
AC_ARG_ENABLE(fc-lib-ldflags,
    AS_HELP_STRING(--disable-fc-lib-ldflags,[do not use extra libraries for linking with Fortran (the default under Cygwin)]),
    USE_FCLIBS=$enableval)

if test "$USE_FCLIBS" = unspecified
then if test "$ISSUE" = Cygwin
     then USE_FCLIBS=no
          AC_MSG_NOTICE([by default under Cygwin, not adding linker flags to link with Fortran libraries])
     else USE_FCLIBS=yes
          AC_MSG_NOTICE([by default, adding linker flags to link with Fortran libraries])
     fi
fi

if test "$FC"
then AC_FC_DUMMY_MAIN()
     AC_FC_WRAPPERS()
     AC_FC_LIBRARY_LDFLAGS()
fi

dnl if test "$PTHREADS" = yes
dnl then
    AC_SEARCH_LIBS(pthread_mutex_trylock,pthread)
dnl fi

AC_SUBST(PROFILING,no)
AC_ARG_ENABLE(profile, AS_HELP_STRING(--enable-profile,enable profiling (and disable stripping)), PROFILING=$enableval)
test "$PROFILING" = no;  val=$?; AC_DEFINE_UNQUOTED(PROFILING, $val,whether profiling has been enabled)

AC_SUBST(COMPRESS,gz)  AC_ARG_ENABLE(compress, AS_HELP_STRING([--enable-compress=[gz|bz2]],compression method for tarball), COMPRESS=$enableval)

AC_SUBST(EXPERIMENT,no)
AC_ARG_ENABLE(experimental-code, AS_HELP_STRING(--enable-experimental-code,enable experimental code), EXPERIMENT=$enableval)
test "$EXPERIMENT" = no; val=$?; AC_DEFINE_UNQUOTED(EXPERIMENT,$val,whether experimental code has been enabled)

AC_SUBST(M2TARFILE,no) AC_ARG_ENABLE(tarfile, AS_HELP_STRING(--enable-tarfile,prepare binary and source packages as compressed tar files), M2TARFILE=$enableval)
AC_SUBST(TARLIBS,no)   AC_ARG_ENABLE(tarlibs, AS_HELP_STRING(--enable-tarlibs,include symbolic links to needed shared libraries for tar), TARLIBS=$enableval)
AC_SUBST(SHARED,no)    AC_ARG_ENABLE(shared, AS_HELP_STRING(--enable-shared,enable building of shared libraries), SHARED=$enableval)

test "$MEMDEBUG" = "yes" && DEBUG=yes
test "$DEBUG" = "yes" && STRIP=no
test "$PROFILING" = "yes" && CFLAGS="$CFLAGS -pg" CXXFLAGS="$CXXFLAGS -pg" LDFLAGS="$LDFLAGS -pg" STRIP=no

AC_SUBST(PACKAGES, dnl first Style, then Macaulay2Doc, then the other internal ones, then other ones, starting with the oldest
"
  Style
  Macaulay2Doc
  FirstPackage
  Parsing
  Classic
  Browse
  Benchmark
  Text
  SimpleDoc
  PackageTemplate
  PrimaryDecomposition
  FourierMotzkin
  Dmodules
  Depth
  Elimination
  GenericInitialIdeal
  IntegralClosure
  HyperplaneArrangements
  LexIdeals
  Markov
  NoetherNormalization
  Points
  ReesAlgebra
  Regularity
  SchurRings
  SymmetricPolynomials
  SchurFunctors
  SimplicialComplexes
  LLLBases
  TangentCone
  ChainComplexExtras
  Schubert2
  LocalRings
  BoijSoederberg
  BGG
  Bruns
  InvolutiveBases
  ConwayPolynomials
  EdgeIdeals
  FourTiTwo
  StatePolytope
  Polyhedra
  Polymake
  gfanInterface
  PieriMaps
  Normaliz
  Posets
  XML
  OpenMath
  SCSCP
  RationalPoints
  MapleInterface
  ConvexInterface
  SRdeformations
  NumericalAlgebraicGeometry
  BeginningMacaulay2
  FormalGroupLaws
  Graphics
  WeylGroups
  HodgeIntegrals
  Cyclotomic
  Binomials
  Kronecker
  Nauty
  ToricVectorBundles
  ModuleDeformations
  PHCpack
  SimplicialDecomposability
  BooleanGB
  AdjointIdeal
  Parametrization
  Serialization
  NAGtypes
  NormalToricVarieties
  DGAlgebras
  Graphs
  GraphicalModels
  BIBasis
  KustinMiller
  Units
  MonomialMultiplierIdeals
  NautyGraphs
  VersalDeformations
  CharacteristicClasses
  RandomObjects
  RandomPlaneCurves
  RandomSpaceCurves
  RandomGenus14Curves
  RandomCanonicalCurves
  RandomCurves
  TensorComplexes
  MonomialAlgebras
  QthPower
  EliminationMatrices
  EllipticIntegrals
")

# convert the newlines in the list above to spaces
PACKAGES=`echo $PACKAGES`

AC_DEFINE_UNQUOTED(PACKAGES,"$PACKAGES",the list of packages included with the release of Macaulay 2)

AC_DEFINE_UNQUOTED(buildsystemtype,"$build",the type of system on which the package was built)
AC_DEFINE_UNQUOTED(hostsystemtype,"$host",the type of system on which the package runs)

case $host in
   i586-*|i686-*)
	newhost=`echo $host | sed s/i.86-/i486-/`
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
	 Linux|SunOS) DUMPDATA=no;; # sigh, dumpdata no longer works, even under linux
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

AC_SUBST(CYGWIN,no)
AC_ARG_ENABLE(cygwin,  AS_HELP_STRING(--enable-cygwin,prepare a binary package for cygwin), CYGWIN=$enableval)
if test "$CYGWIN" = yes 
then if test "$OPTIMIZE" = no
     then AC_MSG_ERROR([--disable-optimize and --enable-cygwin both specified])
     fi
     if test "$DEBUG$" = yes
     then AC_MSG_ERROR([--enable-cygwin and --enable-debug both specified])
     fi
fi

AC_SUBST(DEB,no)
AC_ARG_ENABLE(deb,  AS_HELP_STRING(--enable-deb,prepare a *.deb package (for debian, ubuntu, ...)), DEB=$enableval)
if test "$DEB" = yes 
then if test "$OPTIMIZE" = no
     then AC_MSG_ERROR([--disable-optimize and --enable-deb both specified])
     fi
     if test "$DEBUG" = yes
     then AC_MSG_ERROR([--enable-debug and --enable-deb both specified])
     fi
fi

AC_SUBST(FREEBSD,no)
AC_ARG_ENABLE(freebsd,  AS_HELP_STRING(--enable-freebsd,prepare a package file for freebsd), FREEBSD=$enableval)
if test "$FREEBSD" = yes 
then if test "$OPTIMIZE" = no
     then AC_MSG_ERROR([--disable-optimize and --enable-freebsd both specified])
     fi
     if test "$DEBUG" = yes
     then AC_MSG_ERROR([--enable-debug and --enable-freebsd both specified])
     fi
fi

AC_SUBST(RPM,no)
AC_ARG_ENABLE(rpm,  AS_HELP_STRING(--enable-rpm,prepare a *.rpm package (for red hat based systems)), RPM=$enableval)
if test "$RPM" = yes 
then if test "$OPTIMIZE" = no
     then AC_MSG_ERROR([--disable-optimize and --enable-rpm both specified])
     fi
     if test "$DEBUG" = yes
     then AC_MSG_ERROR([--enable-debug and --enable-rpm both specified])
     fi
fi

AC_SUBST(DMG,no)
AC_ARG_ENABLE(dmg,  AS_HELP_STRING(--enable-dmg,prepare a *.dmg package (for Mac OS)), DMG=$enableval)
if test "$DPM" = yes 
then if test "$OPTIMIZE" = no
     then AC_MSG_ERROR([--disable-optimize and --enable-dpm both specified])
     fi
     if test "$DEBUG" = yes
     then AC_MSG_ERROR([--enable-debug and --enable-dpm both specified])
     fi
fi

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
AC_COMPILE_IFELSE([AC_LANG_SOURCE([
	# 1 "foo" 1 3 4
	template <class T> struct A { };])],AC_MSG_RESULT(no);ISYSTEM=yes,AC_MSG_RESULT(yes))

dnl multi line sed commands may not work, reported by Anton:
dnl AC_MSG_CHECKING(for the presence of synchronization builtins)
dnl AC_SUBST(SYNC_MACROS,)
dnl AC_LINK_IFELSE([
dnl     #include <stdint.h>
dnl     main() { uint64_t x; __sync_lock_test_and_set(&x,1); __sync_lock_release(&x); }
dnl     ],
dnl     SYNC_MACROS="
dnl     #define SYNC_T uint64_t
dnl     #define ACQUIRE(x) __sync_lock_test_and_set(&x,1)
dnl     #define RELEASE(x) __sync_lock_release(&x)
dnl     ")
dnl AC_LINK_IFELSE([
dnl     #include <stdint.h>
dnl     main() { uint32_t x; __sync_lock_test_and_set(&x,1); __sync_lock_release(&x); }
dnl     ],
dnl     SYNC_MACROS="
dnl     #define SYNC_T uint32_t
dnl     #define ACQUIRE(x) __sync_lock_test_and_set(&x,1)
dnl     #define RELEASE(x) __sync_lock_release(&x)
dnl     ")
dnl AC_LINK_IFELSE([
dnl     #include <stdint.h>
dnl     main() { uint16_t x; __sync_lock_test_and_set(&x,1); __sync_lock_release(&x); }
dnl     ],
dnl     SYNC_MACROS="
dnl     #define SYNC_T uint16_t
dnl     #define ACQUIRE(x) __sync_lock_test_and_set(&x,1)
dnl     #define RELEASE(x) __sync_lock_release(&x)
dnl     ")
dnl AC_LINK_IFELSE([
dnl     #include <stdint.h>
dnl     main() { uint8_t x; __sync_lock_test_and_set(&x,1); __sync_lock_release(&x); }
dnl     ],
dnl     SYNC_MACROS="
dnl     #define SYNC_T uint8_t
dnl     #define ACQUIRE(x) __sync_lock_test_and_set(&x,1)
dnl     #define RELEASE(x) __sync_lock_release(&x)
dnl     ")
dnl if test "$SYNC_MACROS"
dnl then AC_MSG_RESULT(found)
dnl else AC_MSG_RESULT(none found)
dnl fi

AC_SUBST(BUILTLIBS,)
# the list LIBLIST is the list of libraries that might be used and linked into M2
# the list PROGLIST is the list of programs and libraries for them that are distributed with M2
#     initially, we offer no option for not compiling some of them
# it reflects dependencies, with prerequisites listed first
# in particular, there are the following dependencies:
#    factory needs gftables, ntl and gmp
#    libfac needs factory
#    ntl needs gmp
#    lapack includes blas, makes both libblas and liblapack
#    mpfr needs gmp
#    mpir is a plug-in replacement for gmp
#    4ti2 needs glpk
#    gfan needs cddlib
#    polymake cannot be included in Macaulay2 because its compile/build/install procedure is flawed:
#	it uses a dynamic readline library that can only be provided by fink and probably will not be copied into the install location
#       it offers no way to provide a directory tree containing a readline library we've compiled
#	it writes a file into the user's home directory called ".polymake"
#	it asks questions
#    normaliz needs mpir providing libgmp and libgmpxx, but we provide that, anyway
#    normaliz needs boost
#    Greg Smith requested cddplus and lrslib for future use
#    nauty is used by the package Nauty
#    normaliz is used by the package Normaliz
#    gfan is used by the packages gfanInterface and StatePolytope
#    4ti2 is used by the package FourTiTwo
#    linbox is provided as an option for experimentation
#    linbox needs fflas-ffpack and givaro
#    fflas-ffpack needs givaro and lapack
#    mpir is used by givaro
#    fflas-ffpack is provided as an option for experimentation
#	 warning: it is licensed only under GPL 2
#        it needs lapack
#    Henry Duong is experimenting with mpack, a multi-precision version of blas+lapack based on mpfr
#    mpack depends on gmp (which mpir provides) mpfr mpc qd
#    flint is experimental, depends on mpir and mpfr
AC_SUBST(PROGLIST, "4ti2 gfan normaliz nauty cddplus lrslib gftables")
AC_SUBST(LIBLIST, " gc gdbm mpir mpfr pari readline ntl factory libfac lapack frobby glpk cddlib givaro fflas_ffpack linbox boost mpc qd mpack flint memtailor gtest ")
BUILD_ALWAYS=" gtest "
for i in $LIBLIST
do eval BUILD_$i=no
done
for i in $PROGLIST
do eval BUILD_$i=yes
done

LIBLIST=" $LIBLIST "
AC_ARG_ENABLE(build-libraries, AS_HELP_STRING(--enable-build-libraries=...,[list of libraries and programs to build from downloaded source code (e.g., gc gdbm mpir mpfr pari readline ntl gftables factory libfac lapack frobby glpk cddlib givaro fflas_ffpack linbox boost mpc qd mpack 4ti2 gfan normaliz nauty cddplus lrslib)]),
    [for i in $enableval
    do case $LIBLIST in
	    *" $i "*) 
	        eval BUILD_$i=yes 
		BUILD_ALWAYS="$BUILD_ALWAYS $i"
	    	;;
	    *) AC_MSG_ERROR(unrecognized library name: $i) ;;
       esac
    done])

AC_SUBST(ENABLE_FFLAS_FFPACK,no)
AC_ARG_ENABLE(fflas-ffpack,
    [AS_HELP_STRING(--enable-fflas-ffpack,enable building the fflas-ffpack library and linking with it)],
    AC_DEFINE(HAVE_FFLAS_FFPACK,1,[whether we are linking with the fflas-ffpack library])
    BUILD_fflas_ffpack=yes
    ENABLE_FFLAS_FFPACK=yes
    BUILD_givaro=yes
    ENABLE_GIVARO=yes
    )

AC_SUBST(ENABLE_MEMTAILOR,no)
AC_ARG_ENABLE(memtailor,
    [AS_HELP_STRING(--enable-memtailor,enable building the memtailor library and linking with it)],
    AC_DEFINE(HAVE_MEMTAILOR,1,[whether we are linking with the memtailor library])
    BUILD_memtailor=yes
    ENABLE_MEMTAILOR=yes
    )

BUILD_gtest=yes

PROGLIST=" $PROGLIST "
AC_ARG_WITH(unbuilt-programs, 
    [AS_HELP_STRING(--with-unbuilt-programs=...,list of programs not to build from downloaded source code (e.g., $PROGLIST))],
    [for i in $withval
     do case $PROGLIST in
	     *" $i "*) eval BUILD_$i=no ;;
	     *) AC_MSG_ERROR(unrecognized program name: $i) ;;
	esac
     done])

AC_SUBST(DOWNLOAD,no)
AC_ARG_ENABLE(download, AS_HELP_STRING(--enable-download,enable automatic downloading of needed third-party libraries), DOWNLOAD=$enableval)

# the order of these segments also reflects dependencies
AC_LANG(C)
AC_SEARCH_LIBS(tgoto,ncurses curses,,AC_MSG_ERROR(libcurses and libncurses not found))
if test $BUILD_readline = no
then AC_CHECK_HEADER(readline/readline.h,,BUILD_readline=yes)
fi
if test $BUILD_readline = no
then AC_MSG_CHECKING([for library containing common symbol _rl_comment_begin and text symbol rl_set_prompt])
     FOUND=
     SAVELIBS=$LIBS
     for lib in "" "-lreadline" "-lreadline -lhistory"
     do  test "$lib" && LIBS="$lib $LIBS"
	 AC_LINK_IFELSE(
	     [AC_LANG_SOURCE([
	      [ extern int rl_set_prompt(); extern int _rl_comment_begin; int main () { return rl_set_prompt() + _rl_comment_begin; } ]
	     ])],
	     FOUND=yes; break;,
	     LIBS=$SAVELIBS)
     done
     if test "$FOUND"
     then if test "$lib"
	 then AC_MSG_RESULT([yes, $lib])
	 else AC_MSG_RESULT([none required])
	 fi
     else AC_MSG_RESULT([no, will build readline])
	 BUILD_readline=yes
     fi 
fi
if test $BUILD_readline = no
then AC_SEARCH_LIBS(rl_completion_matches,readline,,BUILD_readline=yes)
fi
if test $BUILD_readline = no
then AC_SEARCH_LIBS(readline,readline,,BUILD_readline=yes)
fi
if test $BUILD_readline = no
then AC_SEARCH_LIBS(add_history,history readline,,BUILD_readline=yes)
fi
if test $BUILD_readline = yes
then AC_MSG_NOTICE(readline library will be compiled)
     BUILTLIBS="-lreadline -lhistory $BUILTLIBS"
fi

dnl We no longer link with gmp.
dnl if test $BUILD_gmp = no
dnl then :
dnl      AC_LANG(C)
dnl      AC_SEARCH_LIBS(__gmpz_init,gmp,,BUILD_gmp=yes)
dnl      AC_CHECK_HEADER(gmp.h,,BUILD_gmp=yes)
dnl      if test $BUILD_gmp = no
dnl      then AC_MSG_CHECKING([whether gmp library is recent enough])
dnl           # version 4.1.4 of gmp has a random number generator different from 4.2.1's
dnl      	  AC_RUN_IFELSE( [[
dnl      	  #include <gmp.h>
dnl      	  #include <stdio.h>
dnl      	  int main() {
dnl      	    int A=4,B=2,a=0,b=0,c=0,r=sscanf(__gmp_version,"%d.%d.%d",&a,&b,&c), t;
dnl      	    if (r < 2) {
dnl      	      fprintf(stderr,"failed to parse gmp version number: %s\n", __gmp_version);
dnl      	      return 1;
dnl      	    }
dnl      	    t = a>A || a==A && b>=B;
dnl      	    printf("(gmp version %s %s %d.%d) ",__gmp_version, t ? ">=" : "<", A, B);
dnl      	    return !t;
dnl      	  }
dnl      	  ]],
dnl      	  [ AC_MSG_RESULT(yes) ],
dnl      	  [ AC_MSG_RESULT(no); BUILD_gmp=yes ],
dnl      	  [ AC_MSG_RESULT([cross-compiling, test not possible]) ])
dnl      fi
dnl fi
dnl dnl frobby uses gmpxx
dnl if test $BUILD_gmp = yes
dnl then BUILTLIBS="-lgmpxx -lgmp $BUILTLIBS"
dnl else LIBS="-lgmpxx $LIBS"
dnl fi

if test $BUILD_mpir = no
then AC_LANG(C)
     AC_SEARCH_LIBS(__mpir_version,mpir,,BUILD_mpir=yes)
     AC_CHECK_HEADER(mpir.h,,BUILD_mpir=yes)
fi
dnl frobby uses mpirxx
if test $BUILD_mpir = yes
then BUILTLIBS="-lmpirxx -lmpir $BUILTLIBS"
else LIBS="-lmpirxx $LIBS"
fi

if test "$PARI" = yes
then if test $BUILD_pari = no
     then AC_LANG(C)
	  AC_SEARCH_LIBS(pari_init,pari,,BUILD_pari=yes,-lm)
	  AC_CHECK_HEADER(pari/pari.h,,BUILD_pari=yes)
     fi
     test $BUILD_pari = yes && BUILTLIBS="-lpari $BUILTLIBS"
fi

if test "$PYTHON" = yes
then AC_LANG(C)
     if test "$LIBPYTHON" = "$LIBPYTHONORIG"
     then AC_SEARCH_LIBS(Py_Initialize,python2.5,,AC_MSG_ERROR(libpython2.5 not found))
     else LIBS="$LIBPYTHON $LIBS"
     fi
     AC_CHECK_HEADER(python2.5/Python.h,,AC_MSG_ERROR(include file python2.5/Python.h not found))
fi

if test "$FROBBY" = yes
then if test $BUILD_frobby = no
     then AC_MSG_CHECKING(for frobby library)
	  AC_LANG(C++)
          SAVELIBS=$LIBS
	  LIBS="-lfrobby $LIBS"
	  AC_LINK_IFELSE([AC_LANG_SOURCE([
	  	#include <frobby.h>
		int main () {
			Frobby::Ideal(0);
			return 0;
			}
		])],
		[AC_MSG_RESULT(yes)],
		[LIBS=$SAVELIBS
		 BUILD_frobby=yes
		 AC_MSG_RESULT([no, will build it])])
     fi
     test $BUILD_frobby = yes && BUILTLIBS="-lfrobby $BUILTLIBS"
fi

if test $BUILD_glpk = no
then AC_LANG(C)
     AC_CHECK_HEADER(glpk.h,,[
     	CPPFLAGS="-I/usr/include/glpk $CPPFLAGS"
	unset ac_cv_header_glpk_h
     	AC_CHECK_HEADER(glpk.h,,BUILD_glpk=yes)])
fi

if test $BUILD_boost = no
then AC_LANG(C++)
     AC_CHECK_HEADER(boost/version.hpp,,BUILD_boost=yes)
fi

if test $BUILD_cddlib = no
then AC_LANG(C)
     AC_CHECK_HEADER(cdd.h,,
     	[ 
	  CPPFLAGS="-I/usr/include/cddlib $CPPFLAGS"
	  unset ac_cv_header_cdd_h
	  AC_CHECK_HEADER(cdd.h,,BUILD_cddlib=yes,[#include <setoper.h>])
	],
	[#include <setoper.h>])
fi

AC_SUBST(ULIMIT_T,yes)
AC_SUBST(ULIMIT_M,yes)
AC_SUBST(ULIMIT_V,yes)
AC_SUBST(ULIMIT_S,yes)
( ulimit -t 2000 2>/dev/null) || ULIMIT_T=no
( ulimit -m 2000 2>/dev/null) || ULIMIT_M=no
( ulimit -v 2000 2>/dev/null) || ULIMIT_V=no
( ulimit -s 2000 2>/dev/null) || ULIMIT_S=no

AC_SUBST(MPIR_CONFIG_OPTIONS,)
AC_ARG_WITH(mpir_config_options,
	AS_HELP_STRING(--with-mpir-config-options=...,specify additional options for the configure script used with mpir),
	MPIR_CONFIG_OPTIONS=$withval)

if test $BUILD_mpir = yes
then BUILD_mpfr=yes
elif test $BUILD_mpfr = no
then AC_LANG(C)
     AC_SEARCH_LIBS(mpfr_init,mpfr,,BUILD_mpfr=yes)
     # check for lgamma -- it's in 2.3.0 but not in 2.2.1
     AC_SEARCH_LIBS(mpfr_lgamma,mpfr,,BUILD_mpfr=yes)
     AC_CHECK_HEADER(mpfr.h,,BUILD_mpfr=yes,[#include <gmp.h>])
     if test $BUILD_mpfr = no
     then AC_MSG_CHECKING([whether mpfr library is recent enough, using mpfr.h])
	  AC_RUN_IFELSE([AC_LANG_SOURCE([
		  #include <stdio.h>
		  #include <mpfr.h>
		  main () {
		      FILE *msg = fdopen(AS_MESSAGE_FD,"w");
		      unsigned major, minor, patch_level;
		      major = MPFR_VERSION_MAJOR;
		      minor = MPFR_VERSION_MINOR;
		      patch_level = MPFR_VERSION_PATCHLEVEL;
		      fprintf(msg,"(version %d.%d.%d found) ", major, minor, patch_level);
		      return !( major > 2 || major == 2 && minor >= 3 ); }])],
	     [ AC_MSG_RESULT(yes) ],
	     [ AC_MSG_RESULT([no, version at least 2.3.0 is required (mpfr will be built from downloaded sources)]); BUILD_mpfr=yes ],
	     [ AC_MSG_RESULT([cross-compiling, test not possible]) ])
     fi
fi
test $BUILD_mpfr = yes && BUILTLIBS="-lmpfr $BUILTLIBS"

if test $BUILD_gc = no
then AC_LANG(C)
     AC_SEARCH_LIBS(GC_free,gc,
     	[AC_CHECK_HEADER(gc/gc.h,
		[AC_CHECK_HEADER(atomic_ops.h,
			[
			  AC_MSG_CHECKING(whether libgc is recent enough)
			  AC_RUN_IFELSE([AC_LANG_SOURCE([ 
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
				      return !( major > 7 || major == 7 && minor >= 2); }])],
			     [ AC_MSG_RESULT(yes) ],
			     [ AC_MSG_RESULT([no, version at least 7.2 is required (gc will be built from downloaded sources)]); BUILD_gc=yes ],
			     [ AC_MSG_RESULT([cross-compiling, test not possible]) ])
			],
			[
			AC_MSG_RESULT([atomic_ops.h not found, gc will be built from downloaded sources])
			BUILD_gc=yes
			]
			)],
		BUILD_gc=yes)],
	BUILD_gc=yes)
fi
AC_SUBST(GC_LIBS,-lgc)
if test $BUILD_gc = yes
then 
     dnl if test $PTHREADS = yes
     dnl then # libpthread isn't really built by us, but gc will need to link against it
	  BUILTLIBS="-lpthread $BUILTLIBS"
	  GC_LIBS="$GC_LIBS -lpthread"
     dnl fi
     BUILTLIBS="-lgc $BUILTLIBS"
fi

if test $BUILD_gdbm = no
then AC_LANG(C)
     AC_SEARCH_LIBS(gdbm_close,gdbm,,BUILD_gdbm=yes)
     AC_CHECK_HEADER(gdbm.h,,BUILD_gdbm=yes)
fi
test $BUILD_gdbm = yes && BUILTLIBS="-lgdbm $BUILTLIBS"

# We are using mpir.  If ntl is present, and sharable, then it depends
# explicitly on gmp instead of mpir, which can cause a conflict.

if test $BUILD_ntl = no
then AC_LANG(C++)
     AC_CHECK_HEADER(NTL/version.h,,BUILD_ntl=yes)
     AC_MSG_CHECKING(for ntl library)
     FOUND=
     SAVELIBS=$LIBS
     for lib in "" "-lntl" "-lntl -lgf2x"
     do  test "$lib" && LIBS="$lib $LIBS"
	 AC_LINK_IFELSE([AC_LANG_SOURCE([
		 #include <NTL/tools.h>
		 int main () { _ntl_GetTime(); }
		 ])],
	     FOUND=yes; break;,
	     LIBS=$SAVELIBS)
     done
     if test "$FOUND"
     then if test "$lib"
	  then AC_MSG_RESULT([$lib])
	  else AC_MSG_RESULT([none needed])
	  fi
     else AC_MSG_RESULT([no, will build ntl])
	  BUILD_ntl=yes
     fi 

     if test $BUILD_ntl = no
     then AC_MSG_CHECKING([whether ntl library is recent enough, using NTL/version.h])
	  AC_RUN_IFELSE([AC_LANG_SOURCE([[
		  #include <stdio.h>
		  #include <NTL/version.h>
		  main () {
		      FILE *msg = fdopen(]]AS_MESSAGE_FD[[,"w");
		      unsigned major, minor, revision;
		      major = NTL_MAJOR_VERSION;
		      minor = NTL_MINOR_VERSION;
		      revision = NTL_REVISION;
		      fprintf(msg,"(version %d.%d.%d found) ", major, minor, revision);
		      return !( major > 5 || major == 5 && minor > 4 || major == 5 && minor == 4 && revision >= 1 ); }]])],
	     [ AC_MSG_RESULT(yes) ],
	     [ AC_MSG_RESULT([no, version at least 5.4.1 is required (ntl will be built from downloaded sources)]); BUILD_ntl=yes ])
     fi

     if test $BUILD_ntl = no
     then AC_MSG_CHECKING([whether ntl library is compiled with gmp (or mpir), using NTL/config.h])
	  AC_RUN_IFELSE([AC_LANG_SOURCE([[
		  #include <NTL/config.h>
		  main () {
		      return !( 
			  #ifdef NTL_GMP_LIP
			  1
			  #else
			  0
			  #endif
			  ); }]])],
	     [ AC_MSG_RESULT(yes) ],
	     [ AC_MSG_RESULT([no, ntl compiled with gmp (or mpir) is required (ntl will be built from downloaded sources)]); BUILD_ntl=yes ])
     fi
fi
test $BUILD_ntl = yes && BUILTLIBS="-lntl $BUILTLIBS"

AC_SUBST(ENABLE_FLINT,no)
AC_ARG_ENABLE(flint,
    [AS_HELP_STRING(--enable-flint,[enable building the flint library, linking with it, and compiling the experimental code that uses it])],
    AC_DEFINE(HAVE_FLINT,1,[whether we are linking with the flint library])
    BUILD_flint=yes
    BUILTLIBS="-lflint $BUILTLIBS"
    ENABLE_FLINT=yes
    )

AC_SUBST(ENABLE_GIVARO,no)
AC_ARG_ENABLE(givaro,
    [AS_HELP_STRING(--enable-givaro,[enable building the givaro library and linking with it])],
    AC_DEFINE(HAVE_GIVARO,1,[whether we are linking with the givaro library])
    BUILD_givaro=yes
    ENABLE_GIVARO=yes
    )

AC_SUBST(ENABLE_LINBOX,no)
AC_ARG_ENABLE(linbox,
    [AS_HELP_STRING(--enable-linbox,enable building the linbox library and linking with it)],
    AC_DEFINE(HAVE_LINBOX,1,[whether we are linking with the linbox library])
    BUILD_linbox=yes
    ENABLE_LINBOX=yes
    HAVE_GIVARO=1
    BUILD_givaro=yes
    ENABLE_GIVARO=yes
    BUILD_fflas_ffpack=yes
    ENABLE_FFLAS_FFPACK=yes
    )

test $BUILD_givaro = yes && BUILTLIBS="-lgivaro $BUILTLIBS"
test $BUILD_linbox = yes && BUILTLIBS="-llinbox $BUILTLIBS"

AC_ARG_WITH(veclib, AS_HELP_STRING(--with-veclib,[use "-framework vecLib" for lapack]), LIBS="-framework vecLib $LIBS" )

AC_SUBST(LAPACK,yes)
AC_ARG_WITH(lapack, AS_HELP_STRING(--without-lapack,[compile and link without lapack and blas]), LAPACK=$withval)
test "$LAPACK" = no; val=$?
AC_DEFINE_UNQUOTED(LAPACK,$val,whether to link with lapack)

BUILD_gftables=yes

# the instantiations needed by an initialization in libcf are in libfac,
# and libfac depends on libcf, so we mention libfac twice:
if test "$DEBUG" = yes
then SINGULARLIBS="-lfac -lcf -lfac -lcfmem"
else SINGULARLIBS="-lfac -lcf -lfac -lcfmem"
fi
while test $BUILD_factory = no -o $BUILD_libfac = no
do AC_LANG(C++)
   AC_CHECK_HEADER(factory/factory.h,,BUILD_factory=yes BUILD_libfac=yes ; break)
   AC_CHECK_HEADER(factor.h,,BUILD_factory=yes BUILD_libfac=yes ; break)
   SAVELIBS=$LIBS
   LIBS="$SINGULARLIBS $LIBS"
   AC_MSG_CHECKING([whether factory library is installed ($SINGULARLIBS)])
   dnl if -lntl is needed, it's been added to $LIBS above
   AC_LINK_IFELSE([AC_LANG_SOURCE([[ #undef NOSTREAMIO
	              #include <factory/factory.h>
		      int main () { return 0 ; } ]])],
       [AC_MSG_RESULT(yes)],
       [AC_MSG_RESULT([no, will build factory and libfac]) ; BUILD_factory=yes BUILD_libfac=yes LIBS=$SAVELIBS ; break] )
   AC_MSG_CHECKING([whether up-to-date libfac and factory libraries are installed])
   AC_RUN_IFELSE([AC_LANG_SOURCE([[
	   #include <stdio.h>
	   #include <string.h>
           #undef NOSTREAMIO
	   #include <factory/factory.h>
	   #include <factor.h>
	   extern const char * libfac_version;
	   int main () {
		FILE *msg = fdopen(]]AS_MESSAGE_FD[[,"w");
		static int factoryV[3], fV[3] = {3,1,0}, fOK;
		static int  libfacV[3], lV[3] = {3,1,0}, lOK;
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
	   ]])],
	[AC_MSG_RESULT(yes)],
	[AC_MSG_RESULT([no, will build factory and libfac]); BUILD_factory=yes BUILD_libfac=yes LIBS=$SAVELIBS])
   break
done
test $BUILD_factory = yes && BUILTLIBS="$SINGULARLIBS $BUILTLIBS"

# we need to do the fortran library testing last, in case AC_SEARCH_LIBS adds
# one of them to $LIBS, making it impossible to check for the presence of C or
# C++ libraries.  (I'm not sure why putting -llapack on the gcc command line
# needlessly causes the library to be linked against.)
FORTRANUSED=no
if test "$LAPACK" = yes
then AC_MSG_CHECKING([whether the vecLib framework is available])
     SAVELIBS=$LIBS LIBS="-framework vecLib $LIBS"
     AC_LANG(C)
     AC_LINK_IFELSE(
	 [AC_LANG_PROGRAM(,[sgemv_();dgetrf_();])],
	 [
	     AC_MSG_RESULT(yes)
	     ],
	 [
	     AC_MSG_RESULT(no)
	     LIBS=$SAVELIBS
	     FORTRANUSED=yes
	     if test $BUILD_lapack = no
	     then AC_LANG(Fortran)
	     	  if test "$FC" = ""
		  then AC_MSG_ERROR(no fortran compiler found)
		  else AC_MSG_NOTICE(using fortran compiler $FC)
		  fi
		  AC_SEARCH_LIBS(sgemv,blas f77blas,,BUILD_lapack=yes)
		  AC_SEARCH_LIBS(dgetrf,lapack,,BUILD_lapack=yes)
	     fi
	 ])
fi
test $BUILD_lapack = yes && BUILTLIBS="-llapack -lblas $BUILTLIBS"

if test $BUILD_lapack = yes
then # test whether the fortran compiler can handle lapack, which, starting with
     # version 3.2, requires fortran 90, not fortran 77
    AC_LANG(Fortran)
    AC_MSG_CHECKING([whether the fortran compiler is modern enough for lapack])
    AC_COMPILE_IFELSE([instrinsic maxloc],
	AC_MSG_ERROR(the fortran compiler ($FC) does not support the Fortran 90 language required for compiling lapack),
	AC_MSG_RESULT(yes))
fi

if test $BUILD_memtailor = yes
then BUILTLIBS="-lmemtailor $BUILTLIBS"
fi

test "$USE_FCLIBS" = no && FCLIBS=
test "$FORTRANUSED" = no && FCLIBS=
AC_SUBST(FCLIBS_DYNAMIC,)
AC_SUBST(FCLIBS_STATIC,)
AC_SUBST(FCLIBS_FLAGS,)
    # Under Ubuntu 8.10 -lgfortranbegin & -lgfortran come from libgfortran3, but that package isn't in Ubuntu 8.4
    # so we try to link it statically, because there are many Ubuntu users of 8.4.
    # Linking statically with -llapack -lblas results in conflicts between symbols in the two packages under Ubuntu:
    #     /usr/lib/gcc/x86_64-linux-gnu/4.3.2/../../../../lib/libblas.a(xerbla.o): In function `xerbla_':
    #     (.text+0x0): multiple definition of `xerbla_'
    #     /usr/lib/gcc/x86_64-linux-gnu/4.3.2/../../../../lib/liblapack.a(xerbla.o):(.text+0x0): first defined here
    #     collect2: ld returned 1 exit status
    # Note: static version is here:
    #  /usr/lib/gcc/x86_64-linux-gnu/4.3.2/libgfortran.a
    #  /usr/lib/gcc/x86_64-linux-gnu/4.3.2/libgfortranbegin.a
    # Note: see discussion in emacs info about -static-libgcc and -shared-libgcc; -lgcc_s should be dynamically linked
for i in $FCLIBS
do case $i in
	-L*) FCLIBS_FLAGS="$FCLIBS_FLAGS $i" ;;
	-lgfortranbegin | -lgfortran) FCLIBS_STATIC="$FCLIBS_STATIC $i" ;;
	*) FCLIBS_DYNAMIC="$FCLIBS_DYNAMIC $i" ;;
   esac
done

AC_LANG(C)
AC_CHECK_HEADER(libxml/parser.h,,
   [
      CPPFLAGS="-I/usr/include/libxml2 $CPPFLAGS"
      unset ac_cv_header_libxml_parser_h
      AC_CHECK_HEADER(libxml/parser.h,,
          [AC_MSG_ERROR([include file libxml/parser.h not found, set CPPFLAGS or install libxml2 development files])]
 	  )
   ])
AC_SEARCH_LIBS(pow,m,,AC_MSG_ERROR(libm not found or not linkable))
AC_SEARCH_LIBS(gzopen,z,,AC_MSG_ERROR(libz not found or not linkable))
AC_SEARCH_LIBS(xmlNewNode,xml2,,AC_MSG_ERROR(libxml2 not found or not linkable))

AC_SUBST(ENABLE_MPACK,no)
AC_ARG_ENABLE(mpack,
    [AS_HELP_STRING(--enable-mpack,[enable building the mpack library, linking with it, and compiling the experimental code that uses it])],
    AC_DEFINE(HAVE_MPACK,1,[whether we are linking with the mpack library])
    BUILD_mpack=yes
    BUILD_qd=yes
    BUILD_mpc=yes
    ENABLE_MPACK=yes
    BUILTLIBS="-lmlapack_mpfr -lmblas_mpfr -lmpfrcxx $BUILTLIBS"
    )

# after this point we add no more libraries to $LIBS, e.g., with AC_SEARCH_LIBS()
SAVELIBS=$LIBS

AC_SUBST(NM_DEMANGLES,)
AC_MSG_CHECKING(whether nm accepts the demangle option)
if nm --help 2>&1 | grep demangle >/dev/null
   then NM_DEMANGLES=yes; AC_MSG_RESULT(yes)
   else NM_DEMANGLES=no ; AC_MSG_RESULT(no)
fi

AC_LANG(C)
AC_MSG_CHECKING([whether getaddrinfo can handle numeric service (port) numbers])
AC_RUN_IFELSE([AC_LANG_SOURCE([
    #include <sys/types.h>
    #include <sys/socket.h>
    #include <netdb.h>
    main() { struct addrinfo *addr; return 0 != getaddrinfo("1.2.3.4", "80", 0, &addr); }
    ])],
    GETADDRINFO_WORKS=1;AC_MSG_RESULT(yes),
    GETADDRINFO_WORKS=0;AC_MSG_RESULT(no),
    AC_MSG_RESULT([yes (cross-compiling, not tested)]))
AC_DEFINE_UNQUOTED(GETADDRINFO_WORKS,$GETADDRINFO_WORKS,[whether getaddrinfo can handle numeric service (port) numbers])

AC_SUBST(BUILDLIST,)
for i in $LIBLIST $PROGLIST
do eval t=\$BUILD_$i
   test "$t" = yes && BUILDLIST="$BUILDLIST $i"
done

AC_SUBST(BUILDLIBLIST,)
for i in $LIBLIST
do eval t=\$BUILD_$i
   test "$t" = yes && BUILDLIBLIST="$BUILDLIBLIST $i"
done

AC_SUBST(BUILDPROGLIST,)
for i in $PROGLIST
do eval t=\$BUILD_$i
   test "$t" = yes && BUILDPROGLIST="$BUILDPROGLIST $i"
done

TESTFLAG=-Wno-strict-aliasing
AC_MSG_CHECKING(whether $CC accepts $TESTFLAG)
AC_SUBST(HAVE_NO_STRICT_ALIASING,)
SAVE=$CFLAGS
CFLAGS="$TESTFLAG $CFLAGS"
AC_LANG(C)
AC_COMPILE_IFELSE([AC_LANG_SOURCE([int main() {}])], HAVE_NO_STRICT_ALIASING=yes, HAVE_NO_STRICT_ALIASING=no)
M2_CPPFLAGS="$M2_CPPFLAGS $TESTFLAG"
CFLAGS=$SAVE
AC_MSG_RESULT($HAVE_NO_STRICT_ALIASING)


AC_MSG_CHECKING(whether $CC accepts -Wfatal-errors)
AC_LANG(C)
SAVE=$CFLAGS
CFLAGS="$CFLAGS -Wfatal-errors"
AC_COMPILE_IFELSE([AC_LANG_SOURCE([int main() {}])], 
    M2_CPPFLAGS="$M2_CPPFLAGS -Wfatal-errors"
    AC_MSG_RESULT(yes, adding -Wfatal-errors to flags),
    AC_MSG_RESULT(no))
CFLAGS=$SAVE

IS_DECLARED(char **,sys_errlist, [
      #include <stdio.h>
      #include <errno.h>])
IS_DECLARED(char **,h_errlist, [
      #include <stdio.h>
      #include <errno.h>])

AC_SUBST(DUMPDATAFILE,Macaulay2-$ARCH-data)

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
AC_SUBST(EXE,$M2SUFFIX$EXEEXT)

AC_SUBST(DESC,$PACKAGE_VERSION-$host_cpu-$OS-$REL)
# we used to use @ ARCH @ instead of @ host_cpu @ here, but for DEC alphas,
# and perhaps other machines, it gives more information.  The script
# config.guess assembles host_cpu from /proc/cpuinfo, and can be something
# like "alphaev6" or "alphaev56", but ARCH, which is determined from 'uname'
# by the 'configure' script, will be simply "alpha".

if ! test "$FAKEROOT"
then if test "$RPM" = yes
     then AC_MSG_ERROR([program "fakeroot" not installed, required for creating *.deb or *.rpm distributions])
     fi
fi

# Here we split up LIBS into three parts, LIBS_STATIC, LIBS_EITHER, and LIBS_DYNAMIC.
# M2 will get linked against LIBS_STATIC, LIBS_EITHER, and LIBS_DYNAMIC, in that order
# Under --enable-static the libraries in LIBS_EITHER will be linked statically.
AC_SUBST(LIBS_STATIC,)
AC_SUBST(LIBS_EITHER,)
AC_SUBST(LIBS_DYNAMIC,)
for i in $LIBS
do case $i in 
	# we must link dynamically with some libraries:
	#   ncurses: has a database which we aren't providing and whose location we don't know
	#    socket: dynamic loading of libraries is used to determine the current way to look up an IP number
	#  libgcc_s: comes only in dynamic form
	-lncurses | -lsocket  | -lgcc_s* ) LIBS_DYNAMIC="$LIBS_DYNAMIC $i" ;;
	# but on some systems, libpari is provided only dynamically (fedora), so we link dynamically
	# and later we'll figure out a run-time test for whether pari is based on gmp
	# # we must link statically with some libraries:
	# #   libpari: provides no version number at run time, and we must ensure it is configured based on gmp
	# -lpari ) LIBS_STATIC="$LIBS_STATIC $i" ;;
	*) LIBS_EITHER="$LIBS_EITHER $i" ;;
   esac
done

AC_MSG_NOTICE([using BUILDLIBLIST  = $BUILDLIBLIST (libraries to be built from downloaded sources)])
AC_MSG_NOTICE([using BUILDPROGLIST = $BUILDPROGLIST (programs to be built from downloaded sources)])
AC_MSG_NOTICE([using BUILDLIST     = $BUILDLIST (libraries and programs to be built from downloaded sources)])
AC_MSG_NOTICE([using BUILD_ALWAYS  = $BUILD_ALWAYS])
AC_MSG_NOTICE([link using:])
dnl the order of this list should agree with that in Macaulay2/bin/Makefile.in
AC_MSG_NOTICE([     BUILTLIBS      = $BUILTLIBS])
AC_MSG_NOTICE([     LIBS_STATIC    = $LIBS_STATIC])
AC_MSG_NOTICE([     LIBS_EITHER    = $LIBS_EITHER])
AC_MSG_NOTICE([     LIBS_DYNAMIC   = $LIBS_DYNAMIC])
AC_MSG_NOTICE([     FCLIBS_STATIC  = $FCLIBS_STATIC])
AC_MSG_NOTICE([     FCLIBS_DYNAMIC = $FCLIBS_DYNAMIC])
AC_MSG_NOTICE([using CC            = $CC])
AC_MSG_NOTICE([using CPPFLAGS      = $CPPFLAGS])
AC_MSG_NOTICE([using CFLAGS        = $CFLAGS])
AC_MSG_NOTICE([using CXX           = $CXX])
AC_MSG_NOTICE([using CXXFLAGS      = $CXXFLAGS])
AC_MSG_NOTICE([using LDFLAGS       = $LDFLAGS])

function subset {
    # whether the words of $1 are among the words of $2
    y=" $2 "
    for i in $1
    do 	case "$y" in
	    *" $i "*) ;;
	    *) return 1 ;;
	esac
    done
    return 0
}

BUILDING=yes
AC_ARG_ENABLE(building, AS_HELP_STRING(--disable-building,disable automatic building of libraries), BUILDING=$enableval)
subset "$BUILDLIBLIST" "$BUILD_ALWAYS"
sub=$?
if test "$BUILDING" = no -a $sub = 1
then AC_MSG_ERROR([automatic building of libraries disabled, but some must be built])
fi

# Here we insert autoconf's default values so we can compute the relative locations in the directory tree
test "$prefix" || prefix=/usr/local

# for some distributions, prefix should be /usr
if test "$prefix" != /usr
then test "$CYGWIN"  = yes && AC_MSG_ERROR(--enable-cygwin specified and --prefix not set to /usr)
     test "$DEB"     = yes && AC_MSG_ERROR(--enable-deb specified and --prefix not set to /usr)
     test "$RPM"     = yes && AC_MSG_ERROR(--enable-rpm specified and --prefix not set to /usr)
fi
if test "$prefix" != /usr/local
then test "$FREEBSD" = yes && AC_MSG_ERROR(--enable-freebsd specified and --prefix not set to /usr/local)
fi

# these are the variables that make their way into autoconf after version 2.59
test "$datarootdir" || AC_SUBST(datarootdir,'${prefix}/share')
test "$docdir"      || AC_SUBST(docdir,'${datarootdir}/doc/${PACKAGE_TARNAME}')
test "$dvidir"      || AC_SUBST(dvidir,'${docdir}')
test "$htmldir"     || AC_SUBST(htmldir,'${docdir}')
test "$pdfdir"      || AC_SUBST(pdfdir,'${docdir}')
test "$psdir"       || AC_SUBST(psdir,'${docdir}')
test "$localedir"   || AC_SUBST(localedir,'${datarootdir}/locale')

# Here we normalize all the configure variables so each one begins either with ${prefix} or with ${exec_prefix},
# so they can be handled more simply in Macaulay2, by a single text replacement.
save_prefix=$prefix
save_exec_prefix=$exec_prefix
prefix=NONE
exec_prefix=NOWHERE
# here we normalize *everything*
# put docdir and datarootdir at end, because we are changing the values of the variables, and
# other variables depend on them
for i in bindir datadir includedir infodir libdir libexecdir localstatedir mandir sbindir \
         sharedstatedir sysconfdir psdir pdfdir dvidir htmldir localedir docdir datarootdir
do eval w=\$$i ; eval v="$w" ; eval v="$v" ; eval v="$v" ; eval v="$v" ; eval v="$v"
   case $v in
     "$exec_prefix"|"$exec_prefix"/*)
	   eval tail_${i}=`echo $v | sed s,"^$exec_prefix/","",` 
	   eval  pre_${i}=`echo $v | sed s,"^$exec_prefix","'\\${pre_exec_prefix}'",` 
	   eval      ${i}=`echo $v | sed s,"^$exec_prefix","'\\${exec_prefix}'",` 
	   ;;
     "$prefix"|"$prefix"/*)
	   eval tail_${i}=`echo $v | sed s,"^$prefix/","",` 
	   eval  pre_${i}=`echo $v | sed s,"^$prefix","'\\${pre_prefix}'",`
	   eval      ${i}=`echo $v | sed s,"^$prefix","'\\${prefix}'",`
	   ;;
     *) AC_MSG_ERROR([expected "\${$i}" => "$w" to start with "\${prefix}" or with "\${exec_prefix}"]) ;;
   esac
done
prefix=$save_prefix
exec_prefix=$save_exec_prefix

AC_SUBST(pre_bindir)
AC_SUBST(pre_datadir)
AC_SUBST(pre_includedir)
AC_SUBST(pre_infodir)
AC_SUBST(pre_libdir)
AC_SUBST(pre_libexecdir)
AC_SUBST(pre_localstatedir)
AC_SUBST(pre_mandir)
AC_SUBST(pre_sbindir)
AC_SUBST(pre_sharedstatedir)
AC_SUBST(pre_sysconfdir)
AC_SUBST(pre_psdir)
AC_SUBST(pre_pdfdir)
AC_SUBST(pre_dvidir)
AC_SUBST(pre_htmldir)
AC_SUBST(pre_localedir)
AC_SUBST(pre_docdir)
AC_SUBST(pre_datarootdir)

AC_SUBST(tail_bindir)
AC_SUBST(tail_datadir)
AC_SUBST(tail_includedir)
AC_SUBST(tail_infodir)
AC_SUBST(tail_libdir)
AC_SUBST(tail_libexecdir)
AC_SUBST(tail_localstatedir)
AC_SUBST(tail_mandir)
AC_SUBST(tail_sbindir)
AC_SUBST(tail_sharedstatedir)
AC_SUBST(tail_sysconfdir)
AC_SUBST(tail_psdir)
AC_SUBST(tail_pdfdir)
AC_SUBST(tail_dvidir)
AC_SUBST(tail_htmldir)
AC_SUBST(tail_localedir)
AC_SUBST(tail_docdir)
AC_SUBST(tail_datarootdir)

AC_SUBST(MACHINE,"$ARCH-$OS-$ISSUE")
AC_DEFINE_UNQUOTED(MACHINE,"$MACHINE",[complete machine description (to appear in name of tar file)])

AC_SUBST(COMMONSTAGINGAREA,StagingArea) # staging area for common files
AC_SUBST(LOCALSTAGINGAREA,StagingArea) # staging area for arch. dep. files
AC_ARG_WITH(staging-area,
    AS_HELP_STRING(--with-staging-area=...,directory for pre-installation of architecture-independent files (StagingArea)),
    COMMONSTAGINGAREA=$withval)
AC_ARG_ENABLE(common-staging-area,
    AS_HELP_STRING(--enable-common-staging-area,use the common staging area),
    if test "$enableval" = yes
    then if test "$COMMONSTAGINGAREA" != StagingArea
	 then AC_MSG_ERROR(--with-staging-area and --enable-common-staging-area options both provided)
	 fi
	 COMMONSTAGINGAREA=\${abs_top_srcdir}/BUILD/CommonStagingArea # this depends on config.Makefile.in setting abs_top_srcdir
    fi
    )
[
case `pwd` in
  .) ;;
  *)
    case "$COMMONSTAGINGAREA" in
      [\\/]* | ?:[\\/]* | '${abs'* );;
      .) COMMONSTAGINGAREA=`pwd`;;
      *) COMMONSTAGINGAREA=`pwd`/"$COMMONSTAGINGAREA";;
    esac
    case "$LOCALSTAGINGAREA" in
      [\\/]* | ?:[\\/]* | '${abs'* );;
      .) LOCALSTAGINGAREA=`pwd`;;
      *) LOCALSTAGINGAREA=`pwd`/"$LOCALSTAGINGAREA";;
    esac;;
esac
]
AC_SUBST(pre_prefix,$COMMONSTAGINGAREA/common) # as in layout.m2.in
AC_SUBST(pre_exec_prefix,$LOCALSTAGINGAREA/$MACHINE) # as in layout.m2.in
AC_MSG_NOTICE([staging area for common files: $pre_prefix])
AC_MSG_NOTICE([staging area for architecture dependent files: $pre_exec_prefix])

AC_SUBST(packagesdir,$datadir/Macaulay2)
AC_SUBST(libm2dir,$libdir/Macaulay2/$MACHINE)
AC_SUBST(emacsdir,$datadir/emacs/site-lisp)
AC_SUBST(librariesdir,$libm2dir/lib)
AC_SUBST(programsdir,$libexecdir/Macaulay2/$MACHINE)
AC_SUBST(licensesdir,$libexecdir/Macaulay2/program-licenses)
AC_SUBST(packagecachecoredir,$libm2dir/cache)

AC_SUBST(tail_packagesdir,$tail_datadir/Macaulay2)
AC_SUBST(tail_libm2dir,$tail_libdir/Macaulay2/$MACHINE)
AC_SUBST(tail_emacsdir,$tail_datadir/emacs/site-lisp)
AC_SUBST(tail_librariesdir,$tail_libm2dir/lib)
AC_SUBST(tail_programsdir,$tail_libexecdir/Macaulay2/$MACHINE)
AC_SUBST(tail_licensesdir,$tail_libexecdir/Macaulay2/program-licenses)
AC_SUBST(tail_packagecachecoredir,$tail_libm2dir/cache)

AC_SUBST(pre_packagesdir,$pre_datadir/Macaulay2)
AC_SUBST(pre_libm2dir,$pre_libdir/Macaulay2/$MACHINE)
AC_SUBST(pre_emacsdir,$pre_datadir/emacs/site-lisp)
AC_SUBST(pre_librariesdir,$pre_libm2dir/lib)
AC_SUBST(pre_programsdir,$pre_libexecdir/Macaulay2/$MACHINE)
AC_SUBST(pre_licensesdir,$pre_libexecdir/Macaulay2/program-licenses)
AC_SUBST(pre_packagecachecoredir,$pre_libm2dir/cache)

AC_OUTPUT()

dnl Local Variables:
dnl mode: autoconf
dnl compile-command: "make -f Makefile"
dnl End:
