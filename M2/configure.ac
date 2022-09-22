AC_INIT([Macaulay2],[m4_esyscmd_s([cat VERSION])],[Macaulay2@math.uiuc.edu],[Macaulay2],[http://macaulay2.com/])
AC_MSG_NOTICE([configuring Macaulay2 version $PACKAGE_VERSION])
AC_CONFIG_SRCDIR(INSTALL)
AC_CONFIG_HEADERS(include/M2/config.h)
AC_CONFIG_FILES(m4_include(config/files))
AC_SUBST(CONFIGURED_FILES,"$ac_config_files")
AC_SUBST(CONFIG_ARGS,"$ac_configure_args")
echo "'$0' $ac_configure_args" > config.args
C_CONFIG_ARGS=` echo "$ac_configure_args" | sed -e 's=\\\\=\\\\\\\\=g' -e 's=\\"=\\\\"=g' `
AC_DEFINE_UNQUOTED(CONFIG_ARGS,"$C_CONFIG_ARGS",arguments used for configure)
AC_SUBST(CONFIG_CMD,"'$0' $ac_configure_args")

AC_CONFIG_MACRO_DIR([config])	dnl can't get this to work
m4_include(config/ax_compare_version.m4)
m4_include(config/ax_prog_cc_for_build.m4)
m4_include(config/ax_func_accept_argtypes.m4)
m4_include(config/relpaths.m4)
m4_include(config/gtest.m4)
m4_include(config/openmp.m4)
m4_include(config/search-libraries.m4)
m4_include(config/ax_boost_base.m4)
m4_include(config/ax_boost_regex.m4)

dnl define(TO_UPPER,[translit($1, [a-z], [A-Z])])

echo "relevant environment variable values, if any:"
for x in CC FC CXX AR CPPFLAGS CFLAGS FCFLAGS CXXFLAGS LDFLAGS LIBS ISSUE DISTRIBUTION PKG_CONFIG_PATH GFTABLESDIR
do eval v=\$$x
   test "$v" && echo "   $x = $v"
done

AC_SUBST(DISTRIBUTION) # use this starting number to sequentially number the downstream distributions
test "$DISTRIBUTION" || DISTRIBUTION=1

for i in $CFLAGS
do case $i in
   -I*) AC_MSG_ERROR(preprocessor flag $i found in CFLAGS instead of in CPPFLAGS) ;;
   -L*) AC_MSG_ERROR(link flag $i found in CFLAGS instead of in LDFLAGS);;
   esac
done

for i in $CXXFLAGS
do case $i in
   -I*) AC_MSG_ERROR(preprocessor flag $i found in CXXFLAGS instead of in CPPFLAGS);;
   -L*) AC_MSG_ERROR(link flag $i found in CXXFLAGS instead of in LDFLAGS);;
   esac
done

for i in $CPPFLAGS
do case $i in
   -L*) AC_MSG_ERROR(link flag $i found in CPPFLAGS instead of in LDFLAGS);;
   esac
done

for i in $LDFLAGS
do case $i in
   -I*) AC_MSG_ERROR(preprocessor flag $i found in LDFLAGS instead of in CPPFLAGS);;
   esac
done

dnl AC_MSG_CHECKING(whether $FIND is GNU find)
dnl if "$FIND" --version | head -1 | grep "GNU find" >/dev/null 2>&1
dnl then AC_MSG_RESULT(yes)
dnl else AC_MSG_ERROR($FIND: GNU find is required")
dnl fi

dnl interesting behavior: $A expansion happens after statement separation!
dnl       $ A="echo hi 2>/dev/null "
dnl       $ echo "`$A`"
dnl       hi 2>/dev/null
dnl       $ $A
dnl       hi 2>/dev/null
dnl       $ eval $A
dnl       hi

AC_SUBST(NODENAME,"`uname -n`")
AC_DEFINE_UNQUOTED(NODENAME,"$NODENAME",hostname used for compilation)

AC_SUBST(ISSUE) # no initial value
AC_ARG_WITH(issue, AS_HELP_STRING([--with-issue=...,specify the OS issue (e.g., Ubuntu-7.10)]),ISSUE=$withval)
AC_SUBST(OS,"`  uname -s | sed s=/=-=g  `")
AC_SUBST(REL,"`  uname -r | sed -e s=/=-=g -e 's= =+=' -e 's=[()]==g'  `")
AC_SUBST(UNAME_INFO_COMMAND," uname -pmso 2>/dev/null || uname -pms ")
AC_SUBST(UNAME_INFO,"` eval $UNAME_INFO_COMMAND `")

case $OS in
  # Here we normalize the name of the OS for windows for startup.m2, which
  # needs to know that C:/FOO/BAR is an absolute path.
  CYGWIN*) OS=MicrosoftWindows ; ISSUE=${ISSUE:-Cygwin} ;;
esac

if test ! "$ISSUE"
then if test -f /usr/bin/sw_vers
     then ISSUE_FLAVOR=`/usr/bin/sw_vers -productName`
          ISSUE_RELEASE=`/usr/bin/sw_vers -productVersion`
     elif test -f /usr/bin/lsb_release
     then ISSUE_FLAVOR=`lsb_release -s --id`
          ISSUE_RELEASE=`lsb_release -s --release`
     elif test -f /etc/os-release
     then ISSUE_FLAVOR=`. /etc/os-release ; echo $ID`
          ISSUE_RELEASE=`. /etc/os-release ; echo $VERSION_ID`
     elif test -f /usr/lib/os-release
     then ISSUE_FLAVOR=`. /usr/lib/os-release ; echo $ID`
          ISSUE_RELEASE=`. /usr/lib/os-release ; echo $VERSION_ID`
     elif test -f /bin/freebsd-version
     then ISSUE_FLAVOR=FreeBSD
          ISSUE_RELEASE=`freebsd-version`
     elif test -f /etc/system-release
     then ISSUE_FLAVOR=[`</etc/system-release head -1 | sed 's/^\([A-Za-z ]*\).*/\1/' | sed 's/ //g' `]
          ISSUE_RELEASE=[`</etc/system-release head -1 | sed 's/[^0-9]*\([0-9.]*\).*/\1/'`]
     elif test -f /etc/issue
     then ISSUE_FLAVOR=[`</etc/issue head -1 | sed 's/^\([A-Za-z ]*\).*/\1/' | sed 's/ //g' `]
          ISSUE_RELEASE=[`</etc/issue head -1 | sed 's/[^0-9]*\([0-9.]*\).*/\1/'`]
     fi
     # translate to something standard (for us), and verify :
     case $ISSUE_FLAVOR in
         "Mac OS X") ISSUE_FLAVOR=MacOS ;;
	 "debian"|"Debian"*) ISSUE_FLAVOR=Debian ;;
         "ubuntu"|"Ubuntu") ISSUE_FLAVOR=Ubuntu ;;
	 "FedoraCore"*) ISSUE_FLAVOR=FedoraCore ;;
	 "Fedora"*) ISSUE_FLAVOR=Fedora ;;
	 "RedHatEnterprise"*) ISSUE_FLAVOR=RedHatEnterprise ;;
	 "RedHat"*) ISSUE_FLAVOR=RedHat ;;
	 "Scientific"*) ISSUE_FLAVOR="ScientificLinux" ;;
	 "Raspbian"*) ISSUE_FLAVOR=Raspbian ;;
	 *"openSUSE") ISSUE_FLAVOR=openSUSE ;;
	 "SUSE LINUX") ISSUE_FLAVOR=SuseLinux ;;
	 "arch") ISSUE_FLAVOR=ArchLinux ; ISSUE_RELEASE=none ;;
	 "") AC_MSG_ERROR([issue not found]) ;;
	 *)  AC_MSG_NOTICE([unrecognized issue: $ISSUE_FLAVOR]) ;;
     esac
     ISSUE_FLAVOR=`echo $ISSUE_FLAVOR | sed 's/ /-/g'`
     case $ISSUE_RELEASE in
         none) ISSUE=$ISSUE_FLAVOR ;;
	 "")   AC_MSG_NOTICE([release number not found])
	       ISSUE_RELEASE=unknown
	       ISSUE=$ISSUE_FLAVOR-unknown ;;
	 *)    ISSUE=$ISSUE_FLAVOR-$ISSUE_RELEASE
     esac
     
fi

# some operating systems have no ISSUE_FLAVOR, e.g., MacOS
test "$ISSUE" || ISSUE=$REL

AC_DEFINE_UNQUOTED(ISSUE,"$ISSUE",[issue (flavor) of operating system, if any])
AC_DEFINE_UNQUOTED(OS,"$OS",[operating system name, obtained with uname -s, perhaps modified])
AC_DEFINE_UNQUOTED(REL,"$REL",[operating system release, obtained with uname -r])

AC_MSG_NOTICE([operating system information:
   OS    = $OS
   ISSUE = $ISSUE])

AC_CONFIG_AUX_DIR(config)

AC_CHECK_PROGS(MAKE,gmake make,false)
AC_CHECK_PROGS(PKG_CONFIG,pkg-config,false)
if test $PKG_CONFIG = false; then AC_MSG_ERROR(pkg-config is required); fi
AC_CHECK_PROGS(ETAGS,etags ctags,false)
if test "$ETAGS" = false; then AC_MSG_WARN(without etags no TAGS files will be made); fi
AC_CHECK_PROGS(LINTIAN,lintian,false)
AC_CHECK_PROGS(OTOOL,otool,false)
AC_CHECK_PROGS(RPMLINT,rpmlint,false)
AC_CHECK_PROGS(TAR,gtar gnutar tar,false)
AC_CHECK_PROGS(FIND,gfind find,false)
AC_CHECK_PROGS(OBJDUMP,objdump,false)
AC_CHECK_PROGS(OBJCOPY,objcopy,false)
AC_CHECK_PROGS(LDD,ldd,false)
AC_CHECK_PROGS(GIT,git,false)

AC_CHECK_TOOL(AR,ar,false)
AC_CHECK_TOOL(AS,as,false)
AC_CHECK_TOOL(DLLTOOL,dlltool,false)
AC_CHECK_TOOL(LD,ld,false)
AC_CHECK_TOOL(STRIP,strip,false)

if test "$STRIP" != "false"
then AC_MSG_CHECKING(whether $STRIP accepts the remove-section option)
     if "$STRIP" --help 2>&1 | grep remove-section >/dev/null
     then val=yes
     else val=no
     fi
     AC_MSG_RESULT($val)
     AC_SUBST(STRIP_REMOVE_SECTION,$val)
fi

AC_MSG_CHECKING(whether $MAKE is GNU make)
if "$MAKE" --version | head -1 | grep GNU >/dev/null 2>&1
then AC_MSG_RESULT(yes)
else AC_MSG_RESULT(no)
     AC_MSG_ERROR($MAKE: GNU make is required)
fi

AC_MSG_CHECKING(whether GNU make is recent enough)
dnl Note: run our makefiles with GNU make, version 4 or larger.
dnl       Version 3.81, provided on Mac OS X, has a bug, but you can
dnl	  use "homebrew" to install a modern version under the name "gmake"
if "$MAKE" --version | head -1 | grep "GNU Make 3.81" >/dev/null 2>&1
then AC_MSG_RESULT(no)
     AC_MSG_ERROR([$MAKE: GNU make 3.81 found, too old; install a more recent version])
else AC_MSG_RESULT(yes)
fi

AC_MSG_CHECKING(whether $TAR is GNU tar)
if "$TAR" --version | head -1 | grep "GNU tar" >/dev/null 2>&1
then AC_MSG_RESULT(yes)
else AC_MSG_ERROR($TAR: GNU tar is required)
fi

AC_ARG_ENABLE([documentation],
    [AS_HELP_STRING([--enable-documentation=...],
        [space-delimited list of types of documentation to build; available
         options are "html", "info", and "pdf"; default value is "html info"])],
    [DOCUMENTATION=$enableval],
    [DOCUMENTATION="html info"])

if test "$DOCUMENTATION" = no
then DOCUMENTATION=""
elif test "$DOCUMENTATION" = yes
then DOCUMENTATION="html info"
else for DOCTYPE in $DOCUMENTATION
    do case $DOCTYPE in
            html|info|pdf)
                ;;
            *)
                AC_MSG_ERROR([unknown documentation type ($DOCTYPE); expected html, info, or pdf])
                ;;
        esac
    done
fi

AC_SUBST(DOCUMENTATION)

AC_ARG_ENABLE([syntax-highlighting],
    [AS_HELP_STRING([--enable-syntax-highlighting],
	[whether to enable syntax highlighting using highlightjs in the html
	 documentation])],
    [HIGHLIGHTJS=$enableval],
    [HIGHLIGHTJS=no])

if test "x$HIGHLIGHTJS" = xyes
then AC_CHECK_PROGS(NPM,npm,false)
     if test "x$NPM" = xfalse
     then AC_MSG_ERROR([npm is required for syntax highlighting])
     fi
else AC_SUBST(NPM,false)
fi

AC_SUBST(HIGHLIGHTJS)

AC_CANONICAL_HOST()
AC_SUBST(ARCH,$build_cpu)
AC_DEFINE_UNQUOTED(ARCH,"$ARCH",[machine hardware type])


dnl AC_ARG_VAR(CC,C compiler to use)
dnl AC_ARG_VAR(CXX,C++ compiler to use)

AC_SUBST(OPTIMIZE,yes) AC_ARG_ENABLE(optimize, AS_HELP_STRING(--disable-optimize,disable optimization), OPTIMIZE=$enableval)

AC_SUBST(NTL_WIZARD,no) AC_ARG_ENABLE(ntl-wizard, AS_HELP_STRING(--enable-ntl-wizard,enable running the NTL wizard), NTL_WIZARD=$enableval)

AC_SUBST(M2_CPPFLAGS)
AC_SUBST(M2_CFLAGS)
AC_SUBST(M2_CXXFLAGS)

dnl Fix standards, apply everywhere
CXXFLAGS="-std=gnu++14 $CXXFLAGS"
CFLAGS="-std=gnu11 $CFLAGS"

AC_PROG_CC() 			# set CFLAGS before this
AC_SUBST(GCC)			# gets set to yes or no by AC_PROG_CC

AC_PROG_CXX()			# set CXXFLAGS before this
AC_SUBST(GXX)			# gets set to yes or no by AC_PROG_CXX

AC_PROG_YACC()
if test "$YACC" = byacc
then AC_MSG_ERROR([byacc found, but leads to a crash in scc1: install bison instead])
fi

AC_PROG_RANLIB()
AC_PROG_INSTALL()
AC_PROG_AWK()

dnl always compile with "-g", so we can debug even optimized versions
if test "$GCC" = yes
then CFLAGS="$CFLAGS -g3"
     CXXFLAGS="$CXXFLAGS -g3"
     LDFLAGS="$LDFLAGS -g3"
else CFLAGS="$CFLAGS -g"
     CXXFLAGS="$CXXFLAGS -g"
     LDFLAGS="$LDFLAGS -g"
fi

AC_SUBST(DEBUG,no)     AC_ARG_ENABLE(debug, AS_HELP_STRING(--enable-debug,enable debugging (and disable stripping)), DEBUG=$enableval)
if test "$DEBUG" = yes
then # It is a mistake to add "-DDEBUG" to CPPFLAGS, because it is nonstandard and may confusing libraries, such as "flint".
     # Instead, NDEBUG being *not* defined as a C macro is what indicates debug mode.  This is standard practice.
     # gc.h obeys the GC_DEBUG flag:
     M2_CPPFLAGS="$M2_CPPFLAGS -DGC_DEBUG"
     CPPFLAGS="$CPPFLAGS -DGC_DEBUG"
else M2_CPPFLAGS="$M2_CPPFLAGS -DNDEBUG"
     CPPFLAGS="$CPPFLAGS -DNDEBUG"
fi

[
 CFLAGS=` echo $CFLAGS | sed -e 's/-O[0-9]//g' -e 's/   */ /g' -e 's/^  *//' -e 's/ * $//' `
 CXXFLAGS=` echo $CXXFLAGS | sed -e 's/-O[0-9]//g' -e 's/   */ /g' -e 's/^  *//' -e 's/ * $//' `
 FCFLAGS=` echo $FCFLAGS | sed -e 's/-O[0-9]//g' -e 's/   */ /g' -e 's/^  *//' -e 's/ * $//' `
]

if test "$OPTIMIZE" = yes
then CFLAGS="$CFLAGS -O2"
     CXXFLAGS="$CXXFLAGS -O2"
     FCFLAGS="$FCFLAGS -O2"
else CFLAGS="$CFLAGS -O0"
     CXXFLAGS="$CXXFLAGS -O0"
     FCFLAGS="$FCFLAGS -O0"
fi

AC_SUBST(GCCVERSION)
if test $GCC = yes
then AC_MSG_CHECKING(for $CC version)
     GCCVERSION=`$CC -dumpversion 2>&1 | sed 's/.*-\(.*\):.*/\1/'`
     AC_MSG_RESULT($GCCVERSION)
fi

dnl this macro is available only in autoconf 2.60, but most systems have 2.59 as the default:
dnl AC_PROG_MKDIR_P()

AC_MSG_CHECKING(whether exceptions are caught)
  AC_LANG(C++)
  AC_RUN_IFELSE([AC_LANG_SOURCE([
    #include <stdexcept>
    #include <string>
    int main () {
      const std::string s("caught");
      try { throw(std::runtime_error(s)); } 
      catch (std::runtime_error e) { }
      return 0;
    }])],
    [AC_MSG_RESULT(yes)],
    [AC_MSG_RESULT([no, use a different C++ compiler])]; exit 1
    )

AC_CHECK_SIZEOF([int *])
AC_SUBST(SIZEOF_INT_P,$ac_cv_sizeof_int_p)
AC_CHECK_SIZEOF([long])
AC_SUBST(SIZEOF_LONG,$ac_cv_sizeof_long)

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

AC_CHECK_HEADERS_ONCE([sys/time.h])
AC_CHECK_HEADERS(sys/ioctl.h termios.h sys/mman.h sys/socket.h netdb.h netinet/in.h arpa/inet.h sys/time.h time.h sys/wait.h sys/resource.h io.h linux/personality.h stddef.h stdint.h inttypes.h elf.h execinfo.h stdlib.h syscall.h sys/types.h sys/stat.h unistd.h math.h pthread.h assert.h alloca.h malloc.h dlfcn.h)
AC_CHECK_HEADERS(winsock2.h) dnl used with ws2_32.dll under mingw64
dnl winsock2.h should be included before including windows.h
dnl  pthread.h includes windows.h
dnl   therefore winsock2.h should be included before pthread
AC_SEARCH_LIBS(clock_gettime,rt)
dnl winsock2.h should be used with ws2_32.lib; it defines:
dnl 	accept bind closesocket connect freeaddrinfo getaddrinfo gethostbyaddr
dnl 	gethostbyname gethostname getnameinfo getpeername getprotobyname
dnl 	getprotobynumber getservbyname getservbyport getsockname getsockopt htonl htons
dnl 	inet_addr inet_ntoa inet_ntop inet_pton ioctlsocket listen ntohl ntohs recv
dnl 	recvfrom select send sendto setsockopt shutdown socket
AC_SEARCH_LIBS(socket,socket ws2_32) dnl ws2_32 is used under mingw64
AC_SEARCH_LIBS(hstrerror,resolv)
AC_SEARCH_LIBS(dlopen,dl)
AC_SEARCH_LIBS(gethostbyname,nsl)

AC_SUBST(ENABLE_TBB,yes)  AC_ARG_ENABLE(tbb, AS_HELP_STRING(--disable-tbb,disable use of the tbb (threaded building blocks) library), ENABLE_TBB=$enableval)
if test $ENABLE_TBB = yes
then AC_DEFINE(WITH_TBB,1,[whether the tbb library has been enabled by the builder])
     AC_SUBST(LIBTBB)		dnl LIBTBB is usually set to -ltbb
     AC_LANG(C++)
     AC_CHECK_HEADER(tbb/tbb.h,
	[AC_SEARCH_LIBS(TBB_runtime_interface_version,"tbb",
	   LIBTBB=$ac_cv_search_TBB_runtime_interface_version,
	   [AC_MSG_ERROR([tbb library not found])])],
	 [AC_MSG_ERROR([tbb include file (tbb.h) not found])])
     AC_RUN_IFELSE([AC_LANG_SOURCE([[
		#include <stdio.h>
		#define TBB_SUPPRESS_DEPRECATED_MESSAGES 1
		#include <tbb/tbb.h>
		int main () {
		  printf("tbb version %d.%d found\n", TBB_VERSION_MAJOR, TBB_VERSION_MINOR);
		  return 0;
		}
 	     ]])])
fi

AC_CHECK_FUNCS([herror error clock_gettime getaddrinfo hstrerror sync getpgrp setpgid fchmod pipe waitpid setrlimit alarm fork sigprocmask kill longjmp siglongjmp sigaction wait4 readlink lstat realpath mkdir link symlink socket accept fcntl personality ioctl])

AC_SUBST(HAVE_PERSONALITY,$ac_cv_func_personality)

AC_DEFINE_UNQUOTED(SOCKLEN_T,[`echo "$ac_cv_func_accept_arg3" | sed 's/ \*$//'`],[socket length type used by accept()])

if test $host_os = mingw32
then CPPFLAGS="$CPPFLAGS -D_POSIX" # arrange for SIGPIPE to get defined
     CPPFLAGS="$CPPFLAGS -D__USE_MINGW_ALARM" # for SIGALRM to get defined
fi

AC_SUBST(__INTEL_COMPILER,no)
AC_CHECK_DECL(__INTEL_COMPILER,__INTEL_COMPILER=yes)

dnl this macro is available only in autoconf 2.60, but most systems have 2.59 as the default:
dnl AC_TYPE_INT64_T()

AC_FUNC_ALLOCA()

dnl AC_SUBST(PTHREADS,yes) AC_ARG_ENABLE(pthreads, AS_HELP_STRING(--disable-pthreads,[disable pthreads (for gc)]), PTHREADS=$enableval)

AC_SUBST(ENABLE_STRIP,no) AC_ARG_ENABLE(strip, AS_HELP_STRING(--enable-strip,strip the symbol table from the Macaulay2 binary), ENABLE_STRIP=$enableval)
test "$DEBUG" = "yes" && ENABLE_STRIP=no
# note: we build many programs for inclusion in the distribution -- stripping them saves 9MB of 70

AC_SUBST(MEMDEBUG,no)  AC_ARG_ENABLE(memdebug, AS_HELP_STRING(--enable-memdebug,enable memory allocation debugging), MEMDEBUG=$enableval)
AC_SUBST(ENCAP,no)     AC_ARG_ENABLE(encap, AS_HELP_STRING(--enable-encap,encapsulate all files in a subdirectory at installation time), ENCAP=$enableval)
AC_SUBST(XCODE,no)     AC_ARG_ENABLE(xcode, AS_HELP_STRING(--enable-xcode,create Macaulay2/d/interpret.a for use with xcode), XCODE=$enableval)

AC_SUBST(CC_THREAD_SUPPORT)
AC_MSG_CHECKING(whether the C compiler accepts the __thread storage attribute)
AC_LANG(C)
AC_COMPILE_IFELSE(
	[AC_LANG_SOURCE([__thread int x;])],
     	CC_THREAD_SUPPORT=yes; [AC_MSG_RESULT(yes)],
	CC_THREAD_SUPPORT=no ; [AC_MSG_RESULT(no)])

AC_SUBST(XML,yes)
AC_ARG_ENABLE(xml, AS_HELP_STRING(--disable-xml,[do not link with xml library]), XML=$enableval)
if test "$XML" = yes
then AC_DEFINE(WITH_XML,1,[whether we are linking with the xml library])
fi

AC_SUBST(MYSQL,no)
AC_ARG_WITH(mysql, AS_HELP_STRING(--with-mysql,[link with mysql]), MYSQL=$withval)
if test "$MYSQL" = yes
then AC_DEFINE(WITH_MYSQL,1,[whether we are linking with the mysql library])
fi

AC_ARG_ENABLE(altivec, AS_HELP_STRING(--enable-altivec,compile with "-faltivec" option))
if test "$enable_altivec" = yes
then LDFLAGS="$LDFLAGS -faltivec"
     CFLAGS="CFLAGS -faltivec"
     CXXFLAGS="$CXXFLAGS -faltivec"
fi

AC_PROG_FC()			dnl check for Fortran compiler presence
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
AC_SUBST(SHARED,yes)    AC_ARG_ENABLE(shared, AS_HELP_STRING(--disable-shared,disable building of shared libraries), SHARED=$enableval)

dnl Under Mac OS X 10.15 the Gatekeeper makes it impossible for us to distribute shared libraries with our program to end users in disk image (*.dmg) format:
dnl ... but we've given up on trying to satisfy the gatekeeper.  There is a strange error message when we try to link with libgmp.a:
dnl     ld: illegal text-relocation to '___gmp_binvert_limb_table' in /Users/runner/runners/2.263.0/work/M2/M2/M2/BUILD/cicd/usr-host/lib/libgmp.a(mp_minv_tab.o) from '___gmpn_divexact_1' in /Users/runner/runners/2.263.0/work/M2/M2/M2/BUILD/cicd/usr-host/lib/libgmp.a(dive_1.o) for architecture x86_64
dnl if test "$ISSUE_FLAVOR" = "MacOS"
dnl then SHARED=no
dnl      AC_MSG_NOTICE([disabling the building of shared libraries under Mac OS X])
dnl fi

test "$MEMDEBUG" = "yes" && DEBUG=yes
test "$PROFILING" = "yes" && CFLAGS="$CFLAGS -pg" CXXFLAGS="$CXXFLAGS -pg" LDFLAGS="$LDFLAGS -pg" ENABLE_STRIP=no

if <$srcdir/Macaulay2/packages/=distributed-packages sort|uniq -c|grep -v -q ' 1 '
then echo "error: some packages are listed more than once in Macaulay2/packages/=distributed-packages:"
     <$srcdir/Macaulay2/packages/=distributed-packages sort|uniq -c|grep -v ' 1 '
     exit 1
fi
AC_SUBST(PACKAGES,`cat $srcdir/Macaulay2/packages/=distributed-packages`)
[if [ $? != 0 ] ; then exit 1 ; fi]
# convert the newlines to spaces
PACKAGES=`echo $PACKAGES`
AC_DEFINE_UNQUOTED(PACKAGES,"$PACKAGES",the list of packages included with the release of Macaulay2)

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

AC_SUBST(DEVELOPER)
AC_ARG_WITH(developer, AS_HELP_STRING(--with-developer=...,specify the name of the developer ($USER)),DEVELOPER=$withval)

AC_SUBST(TAR_COMPRESS_OPTION,)
case $COMPRESS in
   gz) TAR_COMPRESS_OPTION=--gzip ;;
  bz2) TAR_COMPRESS_OPTION=--bzip ;;
    *) AC_MSG_ERROR(unrecognized option for enable-compress) ;;
esac

AC_SUBST(GIT_DESCRIPTION)
# TODO: currently finds the last commit that changed the VERSION file
# but ideally it should get the last release commit instead
NEW_COMMITS=`$GIT rev-list $($GIT rev-list -1 HEAD $srcdir/VERSION)..HEAD --count`
GIT_COMMIT=`$GIT describe --dirty --always --match HEAD`
if test "$NEW_COMMITS-$GIT_COMMIT" = "-"
then GIT_DESCRIPTION=version-$PACKAGE_VERSION-unknown
else GIT_DESCRIPTION=version-$PACKAGE_VERSION-$NEW_COMMITS-$GIT_COMMIT
fi
AC_DEFINE_UNQUOTED(GIT_DESCRIPTION,"$GIT_DESCRIPTION",[summary of git status])

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
if test "$DMG" = yes 
then if test "$OPTIMIZE" = no
     then AC_MSG_ERROR([--disable-optimize and --enable-dmg both specified])
     fi
     if test "$DEBUG" = yes
     then AC_MSG_ERROR([--enable-debug and --enable-dmg both specified])
     fi
     if test $SHARED = yes
     then SHARED=no
          AC_MSG_WARN([--enable-dmg specified, so disabling the building of shared libraries])
     fi
fi

AC_SUBST(M2SUFFIX)
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

dnl Check for openmp and fail if it cannot be found.
dnl This is required for building the library csdp and good for building the library normaliz
AC_LANG(C++)
AC_OPENMP
if test "$ac_cv_prog_cxx_openmp" = unsupported
   then AC_MSG_ERROR([OpenMP does not work, use a compiler that supports OpenMP])
fi
TMP=$OPENMP_CXXFLAGS
OPENMP_CXXFLAGS=
for i in $TMP
do case $i in
   -l*) OPENMP_LIBS="$OPENMP_LIBS $i" ;;
     *) OPENMP_CXXFLAGS="$OPENMP_CXXFLAGS $i" ;;
   esac
done
AC_SUBST(OPENMP_CXXFLAGS,"$OPENMP_CXXFLAGS")
AC_SUBST(OPENMP_LIBS,"$OPENMP_LIBS")

#############################################################################
AC_SUBST(BUILTLIBS)
# The list BUILTLIBS is the list of link options for the libraries we have decided to build,
#    of the form "-lFOO", which will be used to link Macaulay2 later on.  These libraries are not
#    available at the time this configure script is run, which is why we can't add "-lFOO" to LIBS.
#    (Programs linked by this configure script are linked with the options in LIBS.  This allows libraries
#    dependent on previously detected libraries to be detected by tests that involve linking.

AC_SUBST(LIBLIST, " atomic_ops gc gdbm gmp mpir mpfr mpfi readline ntl flint factory lapack mpsolve frobby glpk cddlib fplll linbox gtest ")
# The list LIBLIST is the list of libraries that might be used and linked into M2.

AC_SUBST(PROGLIST, "4ti2 gfan normaliz csdp nauty cddplus lrslib gftables topcom cohomcalg")
# The list PROGLIST is the list of programs and libraries for them that are distributed with M2.
#     Initially, we offer no option for not compiling some of them.

AC_SUBST(SUBLIST, " memtailor mathic mathicgb fflas_ffpack givaro ")

# The list SUBLIST is the list of submodules that might be used and linked into M2.

# These three lists reflect dependencies, with prerequisites listed first, including the following dependencies:
#    mathicgb needs mathic and memtailor
#    mathic needs memtailor
#    gc needs atomic_ops (and sometimes includes it)
#    factory needs flint, ntl and gmp; it includes and installs gftables, so doesn't need it separately
#    libfac has been replaced by code in factory
#    lapack includes blas, makes both libblas and liblapack
#    mpir is a plug-in replacement for gmp and can provide libgmp and libgmpxx, too.
#    mpfr needs gmp (or mpir
#    	  mpfr puts pointers to gmp numbers in thread local variables, unless
# 	  specially configured, so we shouldn't tell gmp to use libgc (we used to do that)
#    ntl needs gmp (or mpir)
#    4ti2 needs glpk
#    glpk needs gmp (or mpir)
#    topcom depends on cddlib
#    gfan needs cddlib
#    cddlib uses gmp
#    polymake cannot be included in Macaulay2 because its compile/build/install procedure is flawed:
#	it uses a dynamic readline library that can only be provided by fink and probably will not be copied into the install location
#       it offers no way to provide a directory tree containing a readline library we've compiled
#	it writes a file into the user's home directory called ".polymake"
#	it asks questions
#    normaliz needs libgmp, libgmpxx, boost
#    Greg Smith requested cddplus and lrslib for future use
#    nauty is used by the package Nauty
#    normaliz is used by the package Normaliz
#    gfan is used by the packages gfanInterface and StatePolytope
#    4ti2 is used by the package FourTiTwo
#    linbox is provided as an option for experimentation
#    linbox needs fflas_ffpack and givaro
#    givaro uses gmp
#    fflas_ffpack needs givaro and lapack, but fflas_ffpack is just source code, so we don't *have* to build it
#    mpir is used by givaro
#    mpir and mpfr are used by fplll
#    flint depends on gmp (or mpir) and mpfr
#    frobby depends on gmp
#    mpfi needs mpfr

AC_SUBST(FILE_PREREQS)
# This is the list of files found in the system that we need to be there for the user who
# installs a binary distribution.

# a library is listed in BUILD_ALWAYS if and only if we *always* build it, 
# even if the option "--disable-building" is specified:
BUILD_ALWAYS=""

for i in $LIBLIST $SUBLIST $PROGLIST
do eval BUILD_$i=no
done

# Convert package identifiers used here to package names as known by the package.
# The point is that BUILD_fflas_ffpack is a valid shell identifier, but
# the name of the package is fflas-ffpack and BUILD_fflas-ffpack is not valid as
# a shell identifier.  We may need the name of the package, for example, as an
# argument to pkg-config.
for i in $LIBLIST
do eval PKGNAME_$i=$i
done
PKGNAME_fflas_ffpack=fflas-ffpack

#############################################################################

LIBLIST=" $LIBLIST "
AC_ARG_ENABLE(build-libraries, AS_HELP_STRING(--enable-build-libraries=...,[list of libraries, submodules, and programs to build from downloaded source code (e.g., gc gdbm mpir mpfr mpfi readline ntl gftables factory lapack mpsolve frobby glpk cddlib givaro fflas_ffpack linbox 4ti2 gfan normaliz csdp nauty cddplus lrslib)]),
    [for i in $enableval
     do case " $LIBLIST $SUBLIST $PROGLIST " in
	    *" $i "*) 
	        eval BUILD_$i=yes 
		BUILD_ALWAYS="$BUILD_ALWAYS $i"
	    	;;
	    *) AC_MSG_ERROR(--enable-build-libraries option: unrecognized library name: $i) ;;
        esac
     done])

#############################################################################

dnl We disable something temporarily that didn't work out, because if we put a static library, such
dnl as libflint.a, into LIBS, then mathicgb, mathic, and memtailor get confused and include a copy
dnl of the file in their *.a file.
TRY_STATIC=no

AC_SUBST(LIBS_GDBM)
AC_LANG(C)
AC_CHECK_HEADER(gdbm.h,
	test $BUILD_gdbm = yes && AC_MSG_WARN([[library gdbm exists, but we are asked to build it]]),
	BUILD_gdbm=yes)
if test $BUILD_gdbm = yes
then LIBS_GDBM="-lgdbm"
     BUILTLIBS="$LIBS_GDBM $BUILTLIBS"
else if test $SHARED = no -a $TRY_STATIC = yes
     then SEARCH_LIBRARIES(gdbm_close,"/usr/lib/libgdbm.a" "/usr/local/lib/libgdbm.a",,[AC_MSG_ERROR([libgdbm.a not found])])
     else AC_SEARCH_LIBS(gdbm_close,"gdbm",,[AC_MSG_ERROR([libgdbm not found])])
     fi
     if test "none required" != "$ac_cv_search_gdbm_close"
     then LIBS_GDBM=$ac_cv_search_gdbm_close
     fi
fi

## we build gc by default so we can configure it for large memory spaces
## see issue #2445
AC_ARG_WITH([system-gc],
    [AS_HELP_STRING([--with-system-gc],
        [use system gc instead of building it])],
    [],
    [with_system_gc=no])

AS_IF([test "x$with_system_gc" = xyes],
    [AC_MSG_CHECKING(whether package bdw-gc is provided)
dnl Note: we don't include -lgccpp below, because linking with that library is what replaces ::new and friends with GC_MALLOC_UNCOLLECTABLE and friends:
     if [ pkg-config --exists bdw-gc ]
     then AC_MSG_RESULT(yes)
          CPPFLAGS="`pkg-config --cflags-only-I bdw-gc | sed -e 's=^-I/=-isystem /=g' -e 's= -I/= -isystem /=g'` $CPPFLAGS"
          dnl should probably use `pkg-config --libs bdw-gc` here instead
          FOUND_gc=yes
     else AC_MSG_RESULT(no)
          BUILD_gc=yes
     fi],
    [BUILD_gc=yes])

if test $BUILD_gc = yes
then BUILTLIBS="-lgc -lpthread $BUILTLIBS"
     BUILD_ALWAYS="$BUILD_ALWAYS gc"
else if test $SHARED = no -a $TRY_STATIC = yes
     then SEARCH_LIBRARIES(GC_malloc,"/usr/lib/libgc.a" "/usr/local/lib/libgc.a",,
          [AC_MSG_ERROR([libgc.a not found])])
     else AC_SEARCH_LIBS(GC_malloc,"gc",,[AC_MSG_ERROR([libgc not found])])
     fi
fi

# mpir and gmp serve the same purpose
AC_SUBST(LIBS_GMP)
AC_LANG(C)
AC_CHECK_HEADER(gmp.h, FOUND_gmp=yes ,FOUND_gmp=no )
AC_CHECK_HEADER(mpir.h,FOUND_mpir=yes,FOUND_mpir=no)
dnl Remark: both gmp.h and mpir.h are surrounded by 
dnl 	#ifndef __GMP_H__
dnl 	#endif
dnl , so only one can be loaded.  Similarly for gmpxx.h and mpirxx.h.  However, the contents of
dnl the files differ.  For example, mpf_cmp_z is defined only in gmp.h.
AC_SUBST(PREFERRED_INTEGER_PACKAGE,gmp)
AC_ARG_WITH(integer-package,
		AS_HELP_STRING(--with-integer-package=[mpir|gmp],specify the big integer package to use (mpir or gmp)),
		if ! test $withval = mpir -o $withval = gmp
		then AC_MSG_ERROR([big integer package option: invalid value: $withval])
		fi
		PREFERRED_INTEGER_PACKAGE=$withval)
if test $BUILD_gmp = yes -a $BUILD_mpir = yes
then AC_MSG_ERROR([refusing to build both mpir and gmp, since the are interchangeable])
fi
AC_MSG_CHECKING([whether to use gmp or mpir])
if test "$PREFERRED_INTEGER_PACKAGE" = mpir
then USING_MPIR=1
     AC_MSG_RESULT([using mpir])
     AC_DEFINE(USING_MPIR,1,[Whether we use MPIR (instead of GMP)])
     if test "$FOUND_mpir" = no
     then BUILD_mpir=yes
          LIBS_GMP="-lmpirxx -lmpir"
          BUILTLIBS="$LIBS_GMP $BUILTLIBS"
     else if test $SHARED = no -a $TRY_STATIC = yes
          then SEARCH_LIBRARIES(__gmpz_init,"/usr/lib/libmpirxx.a /usr/lib/libmpir.a" "/usr/local/lib/libmpirxx.a /usr/local/lib/libmpir.a",,
					    [AC_MSG_ERROR([libmpir.a not found])])
	  else SEARCH_LIBRARIES(__gmpz_init,"-lmpirxx -lmpir",,[AC_MSG_ERROR([libmpir not found])])
	  fi
	  if test "none required" != "$ac_cv_search___gmpz_init"
	  then LIBS_GMP=$ac_cv_search___gmpz_init
	  fi
     fi
else USING_MPIR=0		# i.e., using gmp
     AC_MSG_RESULT([using gmp])
     AC_DEFINE(USING_MPIR,0,[Whether we use MPIR (instead of GMP)])
     if test "$FOUND_gmp" = no
     then BUILD_gmp=yes
     	  LIBS_GMP="-lgmpxx -lgmp"
          BUILTLIBS="$LIBS_GMP $BUILTLIBS"
     else AC_LANG(C)
	  if test $SHARED = no -a $TRY_STATIC = yes
          then SEARCH_LIBRARIES(__gmpz_init,"/usr/lib/libgmpxx.a /usr/lib/libgmp.a" "/usr/local/lib/libgmpxx.a /usr/local/lib/libgmp.a",,
					    [AC_MSG_ERROR([libgmp.a not found])])
	  else SEARCH_LIBRARIES(__gmpz_init,"-lgmpxx -lgmp",,[AC_MSG_ERROR([libgmp not found])])
	  fi
	  if test "none required" != "$ac_cv_search___gmpz_init"
	  then LIBS_GMP=$ac_cv_search___gmpz_init
	  fi
	  dnl By the way: with Hombebrew on the Mac gmp comes with package configuration:
	  dnl $ pkg-config gmp --cflags
	  dnl -I/usr/local/Cellar/gmp/6.2.0/include
	  dnl $ pkg-config gmp --libs
	  dnl -L/usr/local/Cellar/gmp/6.2.0/lib -lgmp
     fi
fi
AC_SUBST(USING_MPIR)

AC_LANG(C)
AC_CHECK_HEADER(mpfr.h,,BUILD_mpfr=yes)
if test $BUILD_mpfr = yes
then BUILTLIBS="-lmpfr $BUILTLIBS"
else if test $SHARED = no -a $TRY_STATIC = yes
     then SEARCH_LIBRARIES(mpfr_digamma,"/usr/lib/libmpfr.a" "/usr/local/lib/libmpfr.a",,
				        [AC_MSG_ERROR([libmpfr.a not found])])
     else AC_SEARCH_LIBS(mpfr_digamma,"mpfr",,[AC_MSG_ERROR([libmpfr not found])])
     fi
fi

AC_LANG(C)
AC_CHECK_HEADER(mpfi.h,,BUILD_mpfi=yes)
if test $BUILD_mpfi = yes
then BUILTLIBS="-lmpfi $BUILTLIBS"
else if test $SHARED = no -a $TRY_STATIC = yes
     then SEARCH_LIBRARIES(mpfi_set_prec,"/usr/lib/libmpfi.a" "/usr/local/lib/libmpfi.a",,
				        [AC_MSG_ERROR([libmpfi.a not found])])
     else AC_SEARCH_LIBS(mpfi_set_prec,"mpfi",,[AC_MSG_ERROR([libmpfi not found])])
     fi
fi

# including mpsolve (requires gmp or mpir) 
AC_LANG(C)
AC_CHECK_HEADER(mps/mps.h,LIBS="-lmps $LIBS",BUILD_mpsolve=yes)
if test $BUILD_mpsolve = yes
then BUILTLIBS="-lmps $BUILTLIBS"
fi

AC_SUBST(LIBS_GLPK)
AC_LANG(C)
AC_CHECK_HEADER(glpk.h,,BUILD_glpk=yes)
if test $BUILD_glpk = yes
then BUILTLIBS="-lglpk $BUILTLIBS"
else if test $SHARED = no -a $TRY_STATIC = yes
     then SEARCH_LIBRARIES(glp_free,"/usr/lib/libglpk.a" "/usr/local/lib/libglpk.a",,[AC_MSG_ERROR([libglpk.a not found])])
     else AC_SEARCH_LIBS(glp_free,"glpk",,[AC_MSG_ERROR([libglpk not found])])
     fi
     if test "none required" != "$ac_cv_search_glp_free"
     then LIBS_GLPK=$ac_cv_search_glp_free
     fi
fi

AC_MSG_CHECKING([whether givaro library is installed])
AC_SUBST(BUILD_givaro)
if $PKG_CONFIG --exists givaro
then AC_MSG_RESULT(yes)
     LIBS="`$PKG_CONFIG --libs givaro` $LIBS"
     CPPFLAGS="$CPPFLAGS `$PKG_CONFIG --cflags-only-I givaro`"
     AC_MSG_CHECKING([whether givaro has isUnit or isunit])
     AC_LANG([C++])
     AC_COMPILE_IFELSE(
	 [AC_LANG_PROGRAM( [ #include <givaro/gfq.h>
	 		     ],
	 		   [ class Givaro::GFqDom<long int> foo; foo.isunit(0) ])],
	 [AC_MSG_RESULT([isunit]); AC_DEFINE([HAVE_GIVARO_isunit], [1],[whether givaro has isunit])],
	 [AC_MSG_RESULT([isUnit]); AC_DEFINE([HAVE_GIVARO_isunit], [0],[whether givaro has isunit])])
else AC_MSG_RESULT(no)
     AC_DEFINE([HAVE_GIVARO_isunit], [0],[whether givaro has isunit])
     BUILD_givaro=yes
fi
if test $BUILD_givaro = yes
then BUILTLIBS="-lgivaro $BUILTLIBS"
fi

# We add these directories to the end, so the user can override this by setting CPPFLAGS.
# /cddlib/ is more modern than /cdd/
if test -d /usr/local/include/cddlib 
then CPPFLAGS="$CPPFLAGS -I/usr/local/include/cddlib"
fi
if test -d /usr/local/include/cdd 
then CPPFLAGS="$CPPFLAGS -I/usr/local/include/cdd"
fi
if test -d /usr/include/cddlib 
then CPPFLAGS="$CPPFLAGS -I/usr/include/cddlib"
fi
if test -d /usr/include/cdd 
then CPPFLAGS="$CPPFLAGS -I/usr/include/cdd"
fi
AC_LANG(C)
AC_CHECK_HEADER(cdd.h,,[ AC_MSG_RESULT([[cddlib not found, will build]])
			     BUILD_cddlib=yes],
		       [
		          #include <setoper.h>
		       ])
if test $BUILD_cddlib = yes
then BUILTLIBS="-lcdd $BUILTLIBS"
else if test $SHARED = no -a $TRY_STATIC = yes
     then SEARCH_LIBRARIES(dd_Equal,"/usr/lib/libcdd.a" "/usr/local/lib/libcdd.a",,
				    [AC_MSG_ERROR([cdd.h found but libcdd.a not found])])
     else AC_SEARCH_LIBS(dd_Equal,"cdd",,[AC_MSG_ERROR([cdd.h found but libcdd not found])])
     fi
fi

dnl factory needs to know:
AC_SUBST(BUILD_flint)
if test $BUILD_flint = no
then AC_LANG(C)
     AC_MSG_CHECKING([for flint >= 2.6.3])
     AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM(
	  [#include <flint/flint.h>
	  ],
	  [
	  /* version == __FLINT_VERSION . __FLINT_VERSION_MINOR . __FLINT_VERSION_PATCHLEVEL */
	  #if    (__FLINT_VERSION < 2) \
	      || (__FLINT_VERSION == 2 && __FLINT_VERSION_MINOR < 6) \
	      || (__FLINT_VERSION == 2 && __FLINT_VERSION_MINOR == 6 && __FLINT_VERSION_PATCHLEVEL < 3)
	  #error old flint
	  #endif
	  ])],
	  AC_MSG_RESULT([yes])
	  if test $SHARED = no -a $TRY_STATIC = yes
	  then SEARCH_LIBRARIES(_fmpz_mod_mul1,/usr/lib/libflint.a /usr/local/lib/libflint.a,,
					       [AC_MSG_ERROR([libflint.a not found])])
	  else AC_SEARCH_LIBS(_fmpz_mod_mul1,"flint",,[AC_MSG_ERROR([libflint not found])])
	  fi,
	  AC_MSG_RESULT([no])
	  BUILD_flint=yes)
fi
if test $BUILD_flint = yes
then BUILTLIBS="-lflint $BUILTLIBS -lm"
fi

dnl debian: 4ti2-
dnl suse: 4ti2_
dnl fedora: /usr/lib/4ti2/bin or /usr/lib64/4ti2/bin
AC_MSG_CHECKING(whether the package 4ti2 is installed)
SAVE=$PATH
PATH="/usr/lib/4ti2/bin:/usr/lib64/4ti2/bin:$PATH"
FOUND_4ti2=no
for fourti2_prefix in "" "4ti2-" "4ti2_"
do
    if test "`type -t ${fourti2_prefix}circuits`" = file
    then AC_MSG_RESULT([yes, using prefix "$fourti2_prefix"])
         FILE_PREREQS="$FILE_PREREQS `type -p ${fourti2_prefix}circuits`"
         FOUND_4ti2=yes
         break
    fi
done
if test $FOUND_4ti2 = no
then AC_MSG_RESULT([no, will build])
     BUILD_4ti2=yes
fi
PATH=$SAVE

AC_MSG_CHECKING(whether the package cohomcalg is installed)
if test "`type -t cohomcalg`" = file
then if test "$OS" = Darwin -a "$DMG" = yes
     then # On the Mac, we have no way (yet) to install prerequisite homebrew packages:
          # the user just copies a directory tree to /Applications.  Change this when
	  # porting to homebrew.
     	  # Also, the package CohomCalg looks only in the directory where Macaulay2's binaries are
	  AC_MSG_RESULT([yes, will build anyway])
          BUILD_cohomcalg=yes
     else AC_MSG_RESULT([yes])
          FILE_PREREQS="$FILE_PREREQS `type -p cohomcalg`"
     fi
else AC_MSG_RESULT([no, will build])
     BUILD_cohomcalg=yes
fi

AC_MSG_CHECKING(whether the package gfan is installed)
if test "`type -t gfan`" = file
then gfan_version=`gfan _version | head -2 | tail -1 | sed 's/gfan//'`
     AX_COMPARE_VERSION([$gfan_version], [ge], [0.6],
          [AC_MSG_RESULT([yes])
           FILE_PREREQS="$FILE_PREREQS `type -p gfan`"],
          [AC_MSG_RESULT([yes, but < 0.6, will build])
           BUILD_gfan=yes])
else AC_MSG_RESULT([no, will build])
     BUILD_gfan=yes
     dnl we'll also build cddlib until gfan can find its files in /usr/include instead of in /usr/include/cdd
     BUILD_cddlib=yes
fi

AC_MSG_CHECKING(whether the package lrs is installed)
if test "`type -t lrs`" = file
then if test "$OS" = Darwin -a "$DMG" = yes
     then # On the Mac, we have no way (yet) to install prerequisite homebrew packages:
          # the user just copies a directory tree to /Applications.  Change this when
	  # porting to homebrew.
	  AC_MSG_RESULT([yes, will build anyway])
          BUILD_lrslib=yes
     else AC_MSG_RESULT(yes)
          FILE_PREREQS="$FILE_PREREQS `type -p lrs`"
     fi
else AC_MSG_RESULT([no, will build])
     BUILD_lrslib=yes
fi

AC_MSG_CHECKING(whether the package csdp is installed)
if test "`type -t csdp`" = file
then AC_MSG_RESULT([yes])
     FILE_PREREQS="$FILE_PREREQS `type -p csdp`"
else AC_MSG_RESULT([no, will build])
     BUILD_csdp=yes
fi

AC_MSG_CHECKING(whether the package normaliz is installed)
if test "`type -t normaliz`" = file
then normaliz_version=`normaliz --version | head -1 | cut -d " " -f 2`
     AX_COMPARE_VERSION([$normaliz_version], [ge], [2.11],
          [AC_MSG_RESULT([yes])
           FILE_PREREQS="$FILE_PREREQS `type -p normaliz`"],
          [AC_MSG_RESULT([yes, but < 2.11, will build])
           BUILD_normaliz=yes])
else AC_MSG_RESULT([no, will build])
     BUILD_normaliz=yes
fi

if test "$BUILD_nauty" = no
then dnl debian/fedora: nauty-
     AC_MSG_CHECKING(whether the package nauty is installed)
     FOUND_nauty=no
     for nauty_prefix in "" "nauty-"
     do
	 if test "`type -t ${nauty_prefix}complg`" = file
	 then
     dnl check if #1408 is a problem
	     if ${nauty_prefix}biplabg < /dev/null 2>/dev/null
	     then
		 AC_MSG_RESULT([yes, using prefix "$nauty_prefix"])
		 FILE_PREREQS="$FILE_PREREQS `type -p ${nauty_prefix}complg`"
		 FOUND_nauty=yes
		 break
	     else
		 FOUND_nauty=bad
	     fi
	 fi
     done
     if test $FOUND_nauty != yes
     then
	 if test $FOUND_nauty = bad
	 then AC_MSG_RESULT([yes, but ${nauty_prefix}biplabg raises an error, will build])
	 else AC_MSG_RESULT([no, will build])
	 fi
	 BUILD_nauty=yes
     fi
fi

dnl debian: topcom-
dnl fedora: TOPCOM- (only for cube)
dnl gentoo: topcom_ (only for cube, 4 others)
dnl we save ourselves some trouble and check one of the programs that
dnl only debian gives a prefix
AC_MSG_CHECKING(whether the package topcom is installed)
FOUND_topcom=no
for topcom_prefix in "" "topcom-"
do
    if test "`type -t ${topcom_prefix}points2finetriangs`" = file
    then AC_MSG_RESULT([yes, using prefix "$topcom_prefix"])
         FILE_PREREQS="$FILE_PREREQS `type -p ${topcom_prefix}points2finetriangs`"
         FOUND_topcom=yes
         break
    fi
done
if test $FOUND_topcom = no
then AC_MSG_RESULT([no, will build])
     BUILD_topcom=yes
fi

AC_MSG_CHECKING([whether eigen3 library is installed])
if $PKG_CONFIG --exists eigen3
then CFO=`$PKG_CONFIG --cflags-only-other eigen3`
     CFI=`$PKG_CONFIG --cflags-only-I eigen3`
     CXXFLAGS="$CXXFLAGS $CFO"
     CPPFLAGS="$CPPFLAGS $CFI"
     AC_MSG_RESULT([yes, with flags $CFI $CFO])
else AC_MSG_ERROR(eigen library not found)
fi

AC_LANG(C++)
AC_CHECK_HEADER(NTL/version.h,,BUILD_ntl=yes)
if test $BUILD_ntl = yes
then BUILTLIBS="-lntl $BUILTLIBS"
else AC_MSG_CHECKING([whether libntl is present and usable])
     if test $SHARED = no -a $TRY_STATIC = yes
     then dnl It would be better to use SEARCH_LIBRARIES here, but all the symbols in libntl.a are mangled by the C++ compiler,
          dnl so I don't know how to do it.
	  if test -f /usr/lib/libntl.a
     	  then LIBS="/usr/lib/libntl.a $LIBS"
	  elif test -f /usr/local/lib/libntl.a
	  then LIBS="/usr/local/lib/libntl.a $LIBS"
	  else AC_MSG_ERROR([libntl.a not found])
	  fi
     else LIBS="-lntl $LIBS"
     fi
     AC_LINK_IFELSE(
	[AC_LANG_PROGRAM(
	    [
	     #include <NTL/tools.h>
	     ],
	    [_ntl_GetTime()])],
	[AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no, even though include files are present])
	 AC_MSG_ERROR(quitting)
	 ])
fi
dnl factory needs to know:
AC_SUBST(BUILD_ntl)

AC_LANG(C++)
AC_CHECK_HEADER(frobby.h,LIBS="-lfrobby $LIBS",BUILD_frobby=yes)
if test $BUILD_frobby = yes
then BUILTLIBS="-lfrobby $BUILTLIBS"
    dnl uncomment this to [1] when frobby is built from a submodule (?)
    dnl AC_DEFINE([HAVE_FROBBY_VERSION], [0], [whether frobby has frobby_version])
else
    AC_MSG_CHECKING([whether frobby has frobby_version])
    AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([#include <frobby.h>], [frobby_version])],
        [AC_MSG_RESULT([yes]) && AC_DEFINE([HAVE_FROBBY_VERSION], [1], [whether frobby has frobby_version])],
        [AC_MSG_RESULT([no])])
fi

#############################################################################

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
AC_CHECK_PROGS(WGET,wget,false)
AC_CHECK_PROGS(CURL,curl,false)
if test $DOWNLOAD = yes
then if test "$WGET" = false -a "$CURL" = false; then AC_MSG_ERROR(wget or curl is required); fi
fi

# the order of these segments also reflects dependencies
AC_LANG(C)
AC_SEARCH_LIBS(tgoto,curses tinfo ncurses,,AC_MSG_ERROR([[not found: library containing symbol tgoto; tried libcurses, libncurses, and libtinfo)]]))
if test $BUILD_readline = no
then AC_CHECK_HEADER(readline/readline.h,,BUILD_readline=yes)
fi
if test $BUILD_readline = no
then AC_SEARCH_LIBS(rl_set_prompt,readline,,BUILD_readline=yes)
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
if test $BUILD_readline = no
then # readline on Mac OS X is stuck at version 4.2, which has this bug:
     #   CTRL-A doesn't go all the way to the beginning of the
     #   line after typing r e s o TAB C-a
     # So we build it ourselves.
     AC_LANG(C)
     AC_MSG_CHECKING([whether readline library is new enough (version at least 6)])
     AC_RUN_IFELSE([AC_LANG_SOURCE([[
	     #include <stdio.h>
	     #include <readline/readline.h>
	     int main () { return ! ( RL_READLINE_VERSION >= 6 * 0x100 ) ; }]])],
	[ AC_MSG_RESULT([yes]) ],
	[ AC_MSG_RESULT([no, will build it]) ; BUILD_readline=yes ],
	[ AC_MSG_RESULT([cross-compiling, test not possible]) ])
fi
if test $BUILD_readline = no
then AC_CHECK_DECL(rl_catch_signals,,BUILD_readline=yes,[
	#include <stdio.h>
	#include <readline/readline.h>
	])
fi
if test $BUILD_readline = yes
then AC_MSG_NOTICE(readline library will be compiled)
     BUILTLIBS="-lreadline -lhistory $BUILTLIBS"
fi

AC_ARG_WITH([system-memtailor],
    [AS_HELP_STRING([--with-system-memtailor],
	[use system memtailor instead of building git submodule])],,
    [with_system_memtailor=no])
if test $with_system_memtailor = no
then
    BUILD_memtailor=yes
    BUILTLIBS="-lmemtailor $BUILTLIBS"
else AC_LANG(C++)
    AC_SEARCH_LIBS(MEMTAILOR_VERSION_STRING,memtailor,,BUILD_memtailor=yes)
    AC_CHECK_HEADER(memtailor.h,,BUILD_memtailor=yes)
fi

AC_ARG_WITH([system-mathic],
    [AS_HELP_STRING([--with-system-mathic],
	[use system mathic instead of building git submodule])],,
    [with_system_mathic=no])
if test $with_system_mathic = no
then
    BUILD_mathic=yes
    BUILTLIBS="-lmathic $BUILTLIBS"
else AC_LANG(C++)
    AC_SEARCH_LIBS(MATHIC_VERSION_STRING,mathic,,BUILD_mathic=yes)
    AC_CHECK_HEADER(mathic.h,,BUILD_mathic=yes)
fi

AC_ARG_WITH([system-mathicgb],
    [AS_HELP_STRING([--with-system-mathicgb],
	[use system mathicgb instead of building git submodule])],,
    [with_system_mathicgb=no])
if test $with_system_mathicgb = no
then
    BUILD_mathicgb=yes
    BUILTLIBS="-lmathicgb $BUILTLIBS"
else AC_LANG(C++)
    AC_SEARCH_LIBS(MATHICGB_VERSION_STRING,mathicgb,,BUILD_mathicgb=yes)
    AC_CHECK_HEADER(mathicgb.h,,BUILD_mathicgb=yes)
fi

if test $BUILD_gtest = no
then
    CHECK_GTEST
    if test $have_gtest = yes
    then
        CPPFLAGS="$GTEST_CPPFLAGS $CPPFLAGS"
        GTEST_PATH=$GTEST_SOURCE
	AC_MSG_NOTICE(we have found gtest)
    else
        BUILD_gtest=yes
        GTEST_PATH="\$(BUILTLIBPATH)/include/gtest"
	AC_MSG_NOTICE(we will build gtest)
    fi
else
    GTEST_PATH="\$(BUILTLIBPATH)/include/gtest"
fi
AC_SUBST(GTEST_PATH)

AC_SUBST(PYTHON,no)
AC_ARG_WITH(python, AS_HELP_STRING(--with-python@<:@=3.x@:>@.,
    [link with libpython. If the version number is not specified, then the
     system's default version will be detected.]), PYTHON=$withval)
if test $PYTHON != no
then AC_DEFINE(WITH_PYTHON,1,[whether we are linking with the python library])
    if test $PYTHON = yes
    then
	AC_CHECK_PROGS(PYTHON_BIN, python3, false)
    else
	AC_CHECK_PROGS(PYTHON_BIN, python$PYTHON, false)
	PYTHON=yes
    fi
    if test $PYTHON_BIN = false
    then
	AC_MSG_ERROR([could not find python])
    else
	PYTHON_VERSION=`$PYTHON_BIN -c \
	    "from sys import version_info; \
	    print('%d.%d' % (version_info.major, version_info.minor))"`
    fi
    PYTHON_INCLUDE_DIR=`$PYTHON_BIN -c \
	"from sysconfig import get_config_var; \
	print(get_config_var('CONFINCLUDEPY'))"`
    CPPFLAGS="$CPPFLAGS -I$PYTHON_INCLUDE_DIR"
    AC_CHECK_HEADER(Python.h,,AC_MSG_ERROR(Python.h not found))
    AC_SEARCH_LIBS(Py_Initialize,
	python${PYTHON_VERSION} python${PYTHON_VERSION}m,,
	AC_MSG_ERROR(libpython not found))
fi

dnl boost 1.65 was the first release containing the stacktrace library
AX_BOOST_BASE([1.65],,
    [AC_MSG_ERROR([boost library >= 1.65 not installed, but required])])
CPPFLAGS="$CPPFLAGS $BOOST_CPPFLAGS"
LDFLAGS="$LDFLAGS $BOOST_LDFLAGS"

AC_MSG_CHECKING([whether the Boost::Stacktrace library is available])
AC_LANG(C++)
SAVECPPFLAGS=$CPPFLAGS
CPPFLAGS="$CPPFLAGS -DBOOST_STACKTRACE_LINK"
SAVELIBS=$LIBS
FOUND_stacktrace_library=no
for stacktrace_library in backtrace addr2line
do
    LIBS="$SAVELIBS -lboost_stacktrace_${stacktrace_library}"
    AC_LINK_IFELSE(
        [AC_LANG_PROGRAM([#include <boost/stacktrace.hpp>
         ], [boost::stacktrace::stacktrace();])],
        [AC_MSG_RESULT([yes, boost_stacktrace_${stacktrace_library}])
         FOUND_stacktrace_library=yes
         break],)
done
if test $FOUND_stacktrace_library = no
then
    AC_MSG_RESULT([no, will use header-only library])
    CPPFLAGS=$SAVECPPFLAGS
    LIBS=$SAVELIBS
fi

AX_BOOST_REGEX
LIBS="$LIBS $BOOST_REGEX_LIB"

AC_CHECK_HEADERS([boost/math/special_functions.hpp], [],
    [AC_MSG_ERROR([Boost Math Toolkit not available])])

AC_SUBST(ULIMIT_T,yes)
AC_SUBST(ULIMIT_M,yes)
AC_SUBST(ULIMIT_V,yes)
AC_SUBST(ULIMIT_S,yes)
( ulimit -t 2000 2>/dev/null) || ULIMIT_T=no
( ulimit -m 2000 2>/dev/null) || ULIMIT_M=no
( ulimit -v 2000 2>/dev/null) || ULIMIT_V=no
( ulimit -s 2000 2>/dev/null) || ULIMIT_S=no

if test $BUILD_atomic_ops = no
then AC_MSG_CHECKING(whether package atomic_ops is provided)
     if [ pkg-config --exists atomic_ops ]
     then AC_MSG_RESULT(yes)
	  CPPFLAGS="`pkg-config --cflags-only-I atomic_ops | sed -e 's=^-I/=-isystem /=g' -e 's= -I/= -isystem /=g'` $CPPFLAGS"
     else AC_MSG_RESULT([no, will be built from downloaded sources])
          BUILD_atomic_ops=yes
     fi
fi

# test for fflas_ffpack
if test $BUILD_fflas_ffpack = no
then AC_MSG_CHECKING([for fflas_ffpack library, version at least 2])
     if test "`type -t fflas-ffpack-config`" = "file" && test "`fflas-ffpack-config --decimal-version`" -gt 20000
     then AC_MSG_RESULT(found)
          FFLAS_FFPACK_CXXFLAGS=$(for x in $(fflas-ffpack-config --cflags 2>&1) ; do case $x in (-I*) /bin/echo -n "$x " ;; esac done)
	  AC_MSG_NOTICE([adding fflas_ffpack flags $FFLAS_FFPACK_CXXFLAGS])
          M2_CXXFLAGS="$M2_CXXFLAGS $FFLAS_FFPACK_CXXFLAGS"
	  LIBS="$LIBS `fflas-ffpack-config --libs`" dnl note: this might add "-Xpreprocessor -fopenmp" to LIBS, and that will confuse libtool
	  LIBS=`echo $LIBS | sed 's/-Xpreprocessor -fopenmp//'`
     else AC_MSG_RESULT(not found, will build)
          BUILD_fflas_ffpack=yes
     fi
fi

AC_SUBST(ENABLE_LINBOX,no)
AC_ARG_ENABLE(linbox,
    [AS_HELP_STRING(--enable-linbox,enable building the linbox library and linking with it)],
    AC_DEFINE(HAVE_LINBOX,1,[whether we are linking with the linbox library])
    BUILD_linbox=yes
    ENABLE_LINBOX=yes
    )

AC_SUBST(ENABLE_FPLLL,no)
AC_ARG_ENABLE(fplll,
    [AS_HELP_STRING(--enable-fplll,[enable building the fplll library and linking with it])],
    AC_DEFINE(HAVE_FPLLL,1,[whether we are linking with the fplll library])
    BUILD_fplll=yes
    ENABLE_FPLLL=yes
    )
    
test $BUILD_linbox = yes && BUILTLIBS="-llinbox $BUILTLIBS"
test $BUILD_fplll = yes && BUILTLIBS="-lfplll $BUILTLIBS"

SINGULARLIBS="-lfactory  "
if test $BUILD_factory = no
then
    AC_MSG_CHECKING([whether factory library is installed])
    FACTORY_MINVERSION=4.2.0
    if $PKG_CONFIG --exists "factory >= $FACTORY_MINVERSION"
    then
	FACTORY_NAME=factory
	AC_MSG_RESULT([yes ($FACTORY_NAME)])
    elif $PKG_CONFIG --exists "singular-factory >= $FACTORY_MINVERSION"
    then
	FACTORY_NAME=singular-factory
	AC_MSG_RESULT([yes ($FACTORY_NAME)])
    elif $PKG_CONFIG --exists factory || $PKG_CONFIG --exists singular-factory
    then
	BUILD_factory=yes
	FACVER=`pkg-config --modversion factory`
	AC_MSG_RESULT([yes, version $FACVER is installed, but version $FACTORY_MINVERSION is required: will build])
    else
	BUILD_factory=yes
	AC_MSG_RESULT([no; will build])
    fi
fi
if test $BUILD_factory = no
then
     LIBS="`$PKG_CONFIG --libs $FACTORY_NAME` $LIBS"
     CPPFLAGS="`$PKG_CONFIG --cflags $FACTORY_NAME` $CPPFLAGS"
else
     BUILTLIBS="$SINGULARLIBS $BUILTLIBS"
fi

AC_ARG_VAR([GFTABLESDIR],
    [path to gftables directory if factory is already installed])
if test $BUILD_factory = yes
then
    BUILD_gftables=yes
    GFTABLESDIR="${datadir}/Macaulay2/Core/factory/"
else
    BUILD_gftables=no
    if test x$GFTABLESDIR = x
    then
	if test $FACTORY_NAME = factory
	then
	    GFTABLESDIR="`$PKG_CONFIG --variable=prefix $FACTORY_NAME`/share/factory/"
	else
	    GFTABLESDIR="`$PKG_CONFIG --variable=prefix $FACTORY_NAME`/share/singular/factory/"
	fi
    fi
    adl_RECURSIVE_EVAL([$GFTABLESDIR], [GFTABLESDIR])
    AC_CHECK_FILE([${GFTABLESDIR}gftables/961],,
	[AC_MSG_ERROR([could not find gftables but we are not building factory; try specifying the directory with GFTABLESDIR])])
    FILE_PREREQS="$FILE_PREREQS ${GFTABLESDIR}gftables/961"
fi

if test $BUILD_factory = no
then
    AC_MSG_CHECKING([whether factory was built with --enable-streamio])
    AC_LANG([C++])
    AC_COMPILE_IFELSE(
	[AC_LANG_PROGRAM(
	    [
	    #include <factory/factory.h>
	    #include <iostream>
	    ],
	    [Variable x; x = Variable(); std::cout << x])],
	[AC_MSG_RESULT([yes])
	 AC_DEFINE([FACTORY_STREAMIO], [1])
	],
	[AC_MSG_RESULT([no])
	 AC_DEFINE([FACTORY_STREAMIO], [0])])
else AC_DEFINE([FACTORY_STREAMIO], [1], [whether factory was built with --enable-streamio])
fi

AC_SUBST(LINALGLIBS)

# we need to do the fortran library testing last, in case AC_SEARCH_LIBS adds
# one of them to $LIBS, making it impossible to check for the presence of C or
# C++ libraries.  (I'm not sure why putting -llapack on the gcc command line
# needlessly causes the library to be linked against.)
FORTRANUSED=no
if test $BUILD_lapack = yes
then if test $USE_FCLIBS != no
     then LIBS="$FCLIBS $LIBS"
     fi
else lapack_found=no
     blas_found=no
     AC_MSG_CHECKING([whether the Accelerate framework for lapack is available])
     LINALGLIBS="-framework Accelerate"
     SAVELIBS=$LIBS LIBS="$LINALGLIBS $LIBS"
     AC_LANG(C)
     AC_LINK_IFELSE(
	 [AC_LANG_PROGRAM(,[sgemv_();dgetrf_();])],
	 [USE_FCLIBS=no; blas_found=yes; lapack_found=yes; AC_MSG_RESULT(yes)],
	 [LINALGLIBS= LIBS=$SAVELIBS; AC_MSG_RESULT(no)])
     if test $blas_found = no
     then AC_MSG_CHECKING([whether package blas is provided (by pkg-config)])
	  if pkg-config --exists blas
	  then AC_MSG_RESULT(yes)
	       blas_found=yes
	       BLASLIBS=`pkg-config --libs blas`
	       LIBS="$BLASLIBS $LIBS"
	       CPPFLAGS="`pkg-config --cflags-only-I blas | sed -e 's=^-I/=-isystem /=g' -e 's= -I/= -isystem /=g'` $CPPFLAGS"
	       AC_SEARCH_LIBS(sgemv_,,,AC_MSG_ERROR(blas function sgemv_ not found with $BLASLIBS))
	  else AC_MSG_RESULT(no)
	  fi
     fi
     if test $lapack_found = no
     then AC_MSG_CHECKING([whether package lapack is provided (by pkg-config)])
	  if pkg-config --exists lapack
	  then AC_MSG_RESULT(yes)
	       lapack_found=yes
	       LAPACKLIBS="`pkg-config --libs lapack`"
	       LIBS="$LAPACKLIBS $LIBS"
	       CPPFLAGS="`pkg-config --cflags-only-I lapack | sed -e 's=^-I/=-isystem /=g' -e 's= -I/= -isystem /=g'` $CPPFLAGS"
	       AC_SEARCH_LIBS(dgetrf_,,,AC_MSG_ERROR(lapack function dgetrf_ not found with $LAPACKLIBS))
	  else AC_MSG_RESULT(no)
	  fi
     fi
     if test $USE_FCLIBS != no
     then LIBS="$FCLIBS $LIBS"; added_fclibs=yes
     fi
     if test $blas_found = no
     then AC_SEARCH_LIBS(sgemv_,refblas blas f77blas atlcblas,
                         blas_found=yes
			 if test "none required" != "$ac_cv_search_sgemv_"
			 then BLASLIBS=$ac_cv_search_sgemv_
			 fi
			 )
          # we've been told that both f77blas and atlcblas are needed.  If so, fix this somehow.
     fi
     if test $lapack_found = no
     then AC_SEARCH_LIBS(dgetrf_,lapack atllapack,
	  		 lapack_found=yes
			 if test "none required" != "$ac_cv_search_dgetrf_"
			 then LAPACKLIBS=$ac_cv_search_dgetrf_
			 fi
			 )
     fi
     if test $blas_found = no -o $lapack_found = no
     then BUILD_lapack=yes
          LINALGLIBS="$LINALGLIBS -llapack -lrefblas"
     else LINALGLIBS="$LINALGLIBS $LAPACKLIBS $BLASLIBS"
     fi
fi

if test $BUILD_lapack = yes
then if test $added_fclibs != yes
     then LIBS="$FCLIBS $LIBS"
     fi
     LINALGLIBS="$LINALGLIBS -llapack -lrefblas"
     dnl we'll need the fortran compiler to be present to compile lapack
     FORTRANUSED=yes
     if test "$FC" = ""
     then AC_MSG_ERROR([no fortran compiler found (FC not set)])
     else AC_MSG_NOTICE(using fortran compiler $FC)
     fi
fi

dnl This code doesn't seem to be needed, and it wasn't ever working, anyway:
dnl if test $BUILD_lapack = yes
dnl then # test whether the fortran compiler can handle lapack, which, starting with
dnl      # version 3.2, requires fortran 90, not fortran 77
dnl     AC_LANG(Fortran)
dnl     AC_MSG_CHECKING([whether the fortran compiler is modern enough for lapack])
dnl     AC_COMPILE_IFELSE([intrinsic maxloc], dnl this is said to be a Fortran 90 feature
dnl 	AC_MSG_RESULT(yes),
dnl 	AC_MSG_RESULT(no))
dnl fi

test "$USE_FCLIBS" = no && FCLIBS=
test "$FORTRANUSED" = no && FCLIBS=

AC_LANG(C)

dnl        The installation of libxml2 under Mac OS X is problematic:
dnl            (1) Starting with Mac OS X 10.15, /usr/include is not present.  The compilers look in
dnl                /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include instead.  It's a good idea
dnl                not to depend on that.
dnl            (2) The libraries own include files have lines like this:
dnl                   #include <libxml/xmlversion.h>
dnl                but the files is found at this path
dnl                   /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/libxml2/libxml/xmlexports.h
dnl        Combine (1) and (2) and we conclude that somehow we have to get this option on the compiler command line:
dnl                   -I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/libxml2
dnl        Ubuntu doesn't have this problem, because the configuration is done correctly:
dnl                   $ pkg-config --cflags libxml-2.0
dnl                   -I/usr/include/libxml2
dnl        , and we can just use that.  Homebrew tries to emulate that by providing pkg-config files that provide the
dnl        same value, but as we said above, /usr/include is not present, so that doesn't work!
dnl
dnl        The solution seems to be to use the C preprocessor to obtain the path to the file libxml include files, and
dnl        to use that to automatically add the correct option to CPPFLAGS.  This assumes the system allows
dnl	              #include <libxml2/libxml/xmlversion.h>
dnl        to work, but not
dnl	              #include <libxml/xmlversion.h>
dnl        , so we test those two lines early.
dnl
if pkg-config --exists libxml-2.0
then XML_INCLUDE=`pkg-config --cflags-only-I libxml-2.0 | sed -e 's=^-I/=-isystem /=g' -e 's= -I/= -isystem /=g'`
     if test "$XML_INCLUDE"
     then AC_MSG_NOTICE([pkg-config: will search for include files also in $XML_INCLUDE])
          CPPFLAGS="$XML_INCLUDE $CPPFLAGS"
     fi
fi

AC_CHECK_HEADER(libxml/xmlexports.h,,
     dnl if it's not found, it might be here:
     AC_CHECK_HEADERS(libxml2/libxml/xmlexports.h)
     AC_LANG(C)
     AC_PREPROC_IFELSE(
	[AC_LANG_SOURCE([[
	     #include <libxml2/libxml/xmlexports.h>
	     ]])],
	[ LIBXML_LOCATION=` grep libxml2/libxml/xmlexports.h conftest.i | sed 's=.* "\(.*\)/libxml/xmlexports.h.*=\1=' `
	  if test -z "$LIBXML_LOCATION"
	  then AC_MSG_WARN([failed to extract path to libxml from preprocessor output:])
	       dnl perhaps this happens when libxml2 is a symbolic link and the preprocessor unpacks it
	       sed 's/^/| /' <conftest.i
	  else AC_MSG_NOTICE(libxml found in libxml2: will search for include files also in $LIBXML_LOCATION)
	       CPPFLAGS="-isystem '$LIBXML_LOCATION' $CPPFLAGS"
	  fi
	  ]
	))
AC_CHECK_HEADERS(libxml/xmlexports.h libxml/parser.h) dnl these might be found only after the improvement above

AC_SEARCH_LIBS(pow,m,,AC_MSG_ERROR(libm not found or not linkable))
AC_SEARCH_LIBS(gzopen,z,,AC_MSG_ERROR(libz not found or not linkable))
AC_SEARCH_LIBS(lzma_end,lzma,,AC_MSG_ERROR(library lzma not found or not linkable))
AC_SEARCH_LIBS(xmlNewNode,xml2,,AC_MSG_ERROR(library xml2 not found or not linkable))

AC_MSG_CHECKING([whether package libffi is provided])
AS_IF([pkg-config --exists libffi],
      [AC_MSG_RESULT([yes])
       CPPFLAGS="$(pkg-config --cflags-only-I libffi) $CPPFLAGS"
       AC_CHECK_HEADER([ffi.h], [], [AC_MSG_ERROR([ffi.h not found])])
       AC_SEARCH_LIBS([ffi_call], [ffi], [],
	   [AC_MSG_ERROR([libffi not found or not linkable])])],
      [AC_MSG_RESULT([no])
       AC_MSG_ERROR([libffi is required])])

# after this point we add no more libraries to $LIBS, e.g., with AC_SEARCH_LIBS()
SAVELIBS=$LIBS

AC_SUBST(NM_DEMANGLES)
AC_MSG_CHECKING(whether nm accepts the demangle option)
if nm --help 2>&1 | grep demangle >/dev/null
   then NM_DEMANGLES=yes; AC_MSG_RESULT(yes)
   else NM_DEMANGLES=no ; AC_MSG_RESULT(no)
fi

AC_LANG(C)
AC_MSG_CHECKING([whether getaddrinfo can handle numeric service (port) numbers])
AC_RUN_IFELSE([AC_LANG_SOURCE([
    #include <sys/types.h>
    #ifdef HAVE_SYS_SOCKET_H
     #include <sys/socket.h>
    #endif
    #ifdef HAVE_WINSOCK2_H
     #include <winsock2.h>
    #endif
    #ifdef HAVE_NETDB_H
     #include <netdb.h>
    #endif
    main() { 
       struct addrinfo *addr; 
       return 0 != getaddrinfo("1.2.3.4", "80", 0, &addr) ? 99 : 0 ;
       }
    ])],
    [AC_DEFINE_UNQUOTED(GETADDRINFO_WORKS,1,[whether getaddrinfo can handle numeric service (port) numbers])] [AC_MSG_RESULT(yes)],
    [if test $? = 99 ; then AC_MSG_RESULT(no) ; else AC_MSG_ERROR([test file failed to compile]) ; fi],
    [AC_DEFINE_UNQUOTED(GETADDRINFO_WORKS,1,[whether getaddrinfo can handle numeric service (port) numbers])] [AC_MSG_RESULT([probably (cross-compiling, not tested)])])

AC_SUBST(BUILDLIST)
for i in $LIBLIST $PROGLIST
do eval t=\$BUILD_$i
   test "$t" = yes && BUILDLIST="$BUILDLIST $i"
done

AC_SUBST(BUILDLIBLIST)
for i in $LIBLIST
do eval t=\$BUILD_$i
   test "$t" = yes && BUILDLIBLIST="$BUILDLIBLIST $i"
done

AC_SUBST(BUILDSUBLIST)
for i in $SUBLIST
do eval t=\$BUILD_$i
   test "$t" = yes && BUILDSUBLIST="$BUILDSUBLIST $i"
done

AC_SUBST(BUILDPROGLIST)
for i in $PROGLIST
do eval t=\$BUILD_$i
   test "$t" = yes && BUILDPROGLIST="$BUILDPROGLIST $i"
done

TESTFLAG=-Wframe-address
AC_MSG_CHECKING(whether $CC accepts $TESTFLAG)
AC_SUBST(HAVE_WFRAME_ADDRESS)
SAVE=$CFLAGS
CFLAGS="-Werror $TESTFLAG $CFLAGS"
AC_LANG(C)
AC_COMPILE_IFELSE([AC_LANG_SOURCE([int main() {}])], M2_CFLAGS="$M2_CFLAGS $TESTFLAG" HAVE_WFRAME_ADDRESS=yes, HAVE_WFRAME_ADDRESS=no)
CFLAGS=$SAVE
AC_MSG_RESULT($HAVE_WFRAME_ADDRESS)

TESTFLAG=-Wstrict-aliasing
AC_MSG_CHECKING(whether $CC accepts $TESTFLAG)
AC_SUBST(HAVE_WSTRICT_ALIASING)
SAVE=$CFLAGS
CFLAGS="$TESTFLAG -Werror $CFLAGS"
AC_LANG(C)
AC_COMPILE_IFELSE([AC_LANG_SOURCE([int main() {}])], M2_CFLAGS="$M2_CFLAGS $TESTFLAG" HAVE_WSTRICT_ALIASING=yes, HAVE_WSTRICT_ALIASING=no)
CFLAGS=$SAVE
AC_MSG_RESULT($HAVE_WSTRICT_ALIASING)

TESTFLAG=-Wincompatible-pointer-types-discards-qualifiers
AC_MSG_CHECKING(whether $CC accepts $TESTFLAG)
AC_SUBST(HAVE_WDISCARDS_QUALIFIERS)
SAVE=$CFLAGS
CFLAGS="$TESTFLAG -Werror $CFLAGS"
AC_LANG(C)
AC_COMPILE_IFELSE([AC_LANG_SOURCE([int main() {}])], M2_CFLAGS="$M2_CFLAGS $TESTFLAG" HAVE_WDISCARDS_QUALIFIERS=yes, HAVE_WDISCARDS_QUALIFIERS=no)
CFLAGS=$SAVE
AC_MSG_RESULT($HAVE_WDISCARDS_QUALIFIERS)

AC_MSG_CHECKING(whether $CC accepts -Wfatal-errors)
AC_LANG(C)
SAVE=$CFLAGS
CFLAGS="$CFLAGS -Wfatal-errors -Werror"
AC_COMPILE_IFELSE([AC_LANG_SOURCE([int main() {}])], 
    M2_CFLAGS="$M2_CFLAGS -Wfatal-errors"
    AC_MSG_RESULT(yes, adding -Wfatal-errors to flags),
    AC_MSG_RESULT(no))
CFLAGS=$SAVE

dnl Always check for the option that turns the warning on, because gcc doesn't complain
dnl about invalid options that turn off warnings, if there are no warnings to be issued anyway.
dnl It's a feature, not a bug!
dnl On the other hand, clang does complain!

AC_MSG_CHECKING(whether $CXX accepts -Wmismatched-tags)
AC_LANG(C++)
SAVE=$CXXFLAGS
CXXFLAGS="$CXXFLAGS -Wmismatched-tags -Werror"
AC_COMPILE_IFELSE([AC_LANG_SOURCE([int main() {}])], 
    CXXFLAGS=$SAVE
    CXXFLAGS="$CXXFLAGS -Wno-mismatched-tags"
    dnl We don't like seeing this warning, especially from the "clang++" compiler, because
    dnl we like using "struct" and "class" interchangeably.
    AC_MSG_RESULT([[yes, adding -Wno-mismatched-tags to flags]]),
    CXXFLAGS=$SAVE
    AC_MSG_RESULT(no))

dnl we should make a macro for this test
AC_MSG_CHECKING(whether $CC accepts -Wunused-but-set-variable)
AC_SUBST(HAVE_WUNUSED_BUT_SET_VARIABLE)
AC_LANG(C)
SAVE=$CFLAGS
CFLAGS="$CFLAGS -Wunused-but-set-variable -Werror"
AC_COMPILE_IFELSE([AC_LANG_SOURCE([int main() {}])],
    HAVE_WUNUSED_BUT_SET_VARIABLE=yes; AC_MSG_RESULT(yes),
    HAVE_WUNUSED_BUT_SET_VARIABLE=no ; AC_MSG_RESULT(no))
CFLAGS=$SAVE

AC_MSG_CHECKING(whether $CC accepts -Wsometimes-uninitialized)
AC_SUBST(HAVE_WSOMETIMES_UNINITIALIZED)
AC_LANG(C)
SAVE=$CFLAGS
CFLAGS="$CFLAGS -Wsometimes-uninitialized -Werror"
AC_COMPILE_IFELSE([AC_LANG_SOURCE([int main() {}])],
    HAVE_WSOMETIMES_UNINITIALIZED=yes; AC_MSG_RESULT(yes),
    HAVE_WSOMETIMES_UNINITIALIZED=no ; AC_MSG_RESULT(no))
CFLAGS=$SAVE

AC_MSG_CHECKING(whether $CC accepts -Wmaybe-uninitialized)
AC_SUBST(HAVE_WMAYBE_UNINITIALIZED)
AC_LANG(C)
SAVE=$CFLAGS
CFLAGS="$CFLAGS -Wmaybe-uninitialized -Werror"
AC_COMPILE_IFELSE([AC_LANG_SOURCE([int main() {}])],
    HAVE_WMAYBE_UNINITIALIZED=yes; AC_MSG_RESULT(yes),
    HAVE_WMAYBE_UNINITIALIZED=no ; AC_MSG_RESULT(no))
CFLAGS=$SAVE

AC_MSG_CHECKING(whether $CC accepts -Wparentheses-equality)
AC_SUBST(HAVE_WPARENTHESES_EQUALITY)
AC_LANG(C)
SAVE=$CFLAGS
CFLAGS="$CFLAGS -Wparentheses-equality -Werror"
AC_COMPILE_IFELSE([AC_LANG_SOURCE([int main() {}])], 
    HAVE_WPARENTHESES_EQUALITY=yes;  AC_MSG_RESULT(yes),
    HAVE_WPARENTHESES_EQUALITY=no ;  AC_MSG_RESULT(no))
CFLAGS=$SAVE

AC_MSG_CHECKING([whether $CC accepts -Wl,-z,noexecstack])
AC_SUBST(HAVE_WL_X_NOEXECSTACK)
AC_LANG(C)
SAVE=$LDFLAGS
LDFLAGS="$LDFLAGS -Wl,-z,noexecstack -Werror"
AC_LINK_IFELSE([AC_LANG_SOURCE([int main() {}])], 
    HAVE_WL_X_NOEXECSTACK=yes;  AC_MSG_RESULT(yes),
    HAVE_WL_X_NOEXECSTACK=no ;  AC_MSG_RESULT(no))
LDFLAGS=$SAVE

AC_MSG_CHECKING([whether $CC accepts -Wl,--enable-auto-import])
AC_SUBST(HAVE_WL_ENABLE_AUTO_IMPORT)
AC_LANG(C)
SAVE=$LDFLAGS
LDFLAGS="$LDFLAGS -Wl,--enable-auto-import -Werror"
AC_LINK_IFELSE([AC_LANG_SOURCE([int main() {}])], 
    HAVE_WL_ENABLE_AUTO_IMPORT=yes;  AC_MSG_RESULT(yes),
    HAVE_WL_ENABLE_AUTO_IMPORT=no ;  AC_MSG_RESULT(no))
LDFLAGS=$SAVE

AC_MSG_CHECKING([whether $CC accepts -Wl,-Map,conftest.mapfile])
AC_SUBST(HAVE_WL_MAP)
AC_LANG(C)
SAVE=$LDFLAGS
LDFLAGS="$LDFLAGS -Wl,-Map,conftest.mapfile -Werror"
AC_LINK_IFELSE([AC_LANG_SOURCE([int main() {}])], 
    HAVE_WL_MAP=yes;  AC_MSG_RESULT(yes),
    HAVE_WL_MAP=no ;  AC_MSG_RESULT(no))
rm -f conftest.mapfile
LDFLAGS=$SAVE

AC_SUBST(DUMPDATAFILE,Macaulay2-$ARCH-data)

test "$MEMDEBUG" = yes && M2_CPPFLAGS="$M2_CPPFLAGS -DMEMDEBUG"

if test "$OS" = MacOS 
then 
    # we don't know what this does, but some apple documentation writers seem to like it:
    LDFLAGS="$LDFLAGS -bind_at_load"

    # this one makes it find and use our readline.a first, even if there is a file readline.dylib in /usr/lib
    # the point is that the system's readline.dylib might be much older and conflict with our newer one
    LDFLAGS="$LDFLAGS -Wl,-search_paths_first"
fi

AC_SUBST(package,Macaulay2-$PACKAGE_VERSION)

# The suffix "-binary" distinguishes the binary program M2-binary from the shell script M2.
# The purpose of the shell script M2 is to set LD_LIBRARY_PATH appropriately.
AC_SUBST(EXE,-binary$M2SUFFIX$EXEEXT)

AC_SUBST(DESC,$PACKAGE_VERSION-$host_cpu-$OS-$REL)
# we used to use @ ARCH @ instead of @ host_cpu @ here, but for DEC alphas,
# and perhaps other machines, it gives more information.  The script
# config.guess assembles host_cpu from /proc/cpuinfo, and can be something
# like "alphaev6" or "alphaev56", but ARCH, which is determined from 'uname'
# by the 'configure' script, will be simply "alpha".

AC_MSG_NOTICE([using BUILDLIBLIST  = $BUILDLIBLIST])
AC_MSG_NOTICE([using BUILDSUBLIST  = $BUILDSUBLIST])
AC_MSG_NOTICE([using BUILDPROGLIST = $BUILDPROGLIST])
AC_MSG_NOTICE([using BUILDLIST     = $BUILDLIST])
AC_MSG_NOTICE([using BUILD_ALWAYS  = $BUILD_ALWAYS])
AC_MSG_NOTICE([using FILE_PREREQS  = $FILE_PREREQS])
AC_MSG_NOTICE([link using:])
dnl the order of this list should agree with that in Macaulay2/bin/Makefile.in
AC_MSG_NOTICE([     BUILTLIBS          = $BUILTLIBS])
AC_MSG_NOTICE([     LINALGLIBS         = $LINALGLIBS])
AC_MSG_NOTICE([     LIBS               = $LIBS])
AC_MSG_NOTICE([     FCLIBS             = $FCLIBS])
AC_MSG_NOTICE([using CC                = $CC])
AC_MSG_NOTICE([using CPP               = $CPP])
AC_MSG_NOTICE([using CPPFLAGS          = $CPPFLAGS])
AC_MSG_NOTICE([using CFLAGS            = $CFLAGS])
AC_MSG_NOTICE([using CXX               = $CXX])
AC_MSG_NOTICE([using CXXFLAGS          = $CXXFLAGS])
AC_MSG_NOTICE([using FC                = $FC])
AC_MSG_NOTICE([using AR                = $AR])
AC_MSG_NOTICE([using RANLIB            = $RANLIB])
AC_MSG_NOTICE([using LDFLAGS           = $LDFLAGS])
AC_MSG_NOTICE([using BLASLIBS          = $BLASLIBS])
AC_MSG_NOTICE([using LAPACKLIBS        = $LAPACKLIBS])
AC_MSG_NOTICE([using LIBS_GDBM         = $LIBS_GDBM])
AC_MSG_NOTICE([using LIBS_GLPK         = $LIBS_GLPK])
AC_MSG_NOTICE([using LIBS_GMP          = $LIBS_GMP])
AC_MSG_NOTICE([using CC_FOR_BUILD      = $CC_FOR_BUILD])
AC_MSG_NOTICE([with  ISSUE             = $ISSUE])
AC_MSG_NOTICE([with  NODENAME          = $NODENAME])
AC_MSG_NOTICE([with  OS REL            = $OS $REL])
AC_MSG_NOTICE([with  ARCH              = $ARCH])
AC_MSG_NOTICE([with  OPTIMIZE          = $OPTIMIZE])
AC_MSG_NOTICE([with  DEBUG             = $DEBUG])
AC_MSG_NOTICE([with  GIT_DESCRIPTION   = $GIT_DESCRIPTION])
AC_MSG_NOTICE([with  USING_MPIR        = $USING_MPIR])
AC_MSG_NOTICE([with  DOCUMENTATION     = $DOCUMENTATION])

subset() {
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

complement() {
    # the words of $1 not among the words of $2
    y=" $2 "
    for i in $1
    do 	case "$y" in
	    *" $i "*) ;;
	    *) /bin/echo -n "$i " ;;
	esac
    done
    echo
}


BUILDING=yes
AC_ARG_ENABLE(building, AS_HELP_STRING(--disable-building,disable automatic building of libraries), BUILDING=$enableval)
# This option looks useless, but it allows us to test the configure script.  After we build the libraries, we
# run the configure script again, and it should detect the ones we've built.
subset "$BUILDLIBLIST" "$BUILD_ALWAYS"
sub=$?
if test "$BUILDING" = no -a $sub = 1
then c=`complement "$BUILDLIBLIST" "$BUILD_ALWAYS"`
     AC_MSG_ERROR([ *** - automatic building of libraries disabled, but some must be built: $c])
fi

# Here we insert autoconf's default values so we can compute the relative locations in the directory tree
test "$prefix" || prefix=/usr/local

# for some distributions, we know what the prefix should be:
test "$DEB"     = yes && test "$prefix" != /usr && 
     AC_MSG_ERROR(--enable-deb specified and --prefix not set to /usr)
test "$RPM"     = yes && test "$prefix" != /usr && 
     AC_MSG_ERROR(--enable-rpm specified and --prefix not set to /usr)

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

AC_ARG_WITH([emacsdir],
    [AS_HELP_STRING([--with-emacsdir=...],
        [set installation directory for installing Emacs Lisp files;
         must begin with ${prefix} or ${exec_prefix}])],
    [emacsdir=$withval],
    [emacsdir=${datarootdir}/emacs/site-lisp/macaulay2])
AC_SUBST(emacsdir)

# Here we normalize all the configure variables so each one begins either with ${prefix} or with ${exec_prefix},
# so they can be handled more simply in Macaulay2, by a single text replacement.
save_prefix=$prefix
save_exec_prefix=$exec_prefix
prefix=NONE
exec_prefix=NOWHERE
# Here we normalize almost everything.
# We put docdir and datarootdir at end, because we are changing the values of the variables, and
# other variables depend on them.
# We don't normalize sysconfdir, sharedstatedir, and localstatedir, because Fedora
# insists in /usr/share/config.site that they be /etc, /var, and /var, respectively.
# We don't use those directories, anyway.  To ensure that, we set them to /nowhere.
# (We could avoid config.site with the CONFIG_SITE environment variable.)
sysconfdir=/nowhere
sharedstatedir=/nowhere
localstatedir=/nowhere
for i in bindir datadir includedir infodir libdir libexecdir mandir sbindir \
         psdir pdfdir dvidir htmldir localedir GFTABLESDIR docdir datarootdir \
         emacsdir
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
     *) if test $i != GFTABLESDIR #only needs normalized if we build factory
	then AC_MSG_ERROR([expected "\${$i}" => "$w" to start with "\${prefix}" or with "\${exec_prefix}"])
	fi ;;
   esac
done
prefix=$save_prefix
exec_prefix=$save_exec_prefix

if test $BUILD_gftables = no
then PRE_GFTABLESDIR="$GFTABLESDIR"
else
PRE_GFTABLESDIR="$pre_datadir/Macaulay2/Core/factory/"
fi
AC_SUBST(PRE_GFTABLESDIR)

AC_SUBST(pre_bindir) dnl In Macaulay2/bin/M2.in we assume that $pre_bindir/.. is $pre_exec_prefix, which follows from pre_bindir=${pre_exec_prefix}/bin.
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
AC_SUBST(pre_emacsdir)

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
AC_SUBST(tail_emacsdir)

AC_SUBST(MACHINE,"$ARCH-$OS-$ISSUE")
AC_DEFINE_UNQUOTED(MACHINE,"$MACHINE",[complete machine description (to appear in name of tar file)])

AC_SUBST(COMMONSTAGINGAREA,usr-dist) # staging area for common files
AC_SUBST(LOCALSTAGINGAREA,usr-dist) # staging area for arch. dep. files
AC_ARG_WITH(staging-area,
    AS_HELP_STRING(--with-staging-area=...,directory for pre-installation of architecture-independent files (usr-dist)),
    COMMONSTAGINGAREA=$withval)
AC_ARG_ENABLE(common-staging-area,
    AS_HELP_STRING(--enable-common-staging-area,use the common staging area),
    if test "$enableval" = yes
    then if test "$COMMONSTAGINGAREA" != usr-dist
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

# Here we define the Macaulay2 layout, once and for all.
# There is a hidden dependency: see M2/distributions/dmg/Makefile.in and adjust the number "../"s if the basic layout here is changed.

AC_SUBST(packagesdir,$datadir/Macaulay2)
AC_SUBST(libm2dir,$libdir/Macaulay2)
AC_SUBST(librariesdir,$libm2dir/lib)
AC_SUBST(programsdir,$libexecdir/Macaulay2/bin)
AC_SUBST(licensesdir,$libexecdir/Macaulay2/program-licenses)
AC_SUBST(packagecachecoredir,$libm2dir/cache)

AC_SUBST(tail_packagesdir,$tail_datadir/Macaulay2)
AC_SUBST(tail_libm2dir,$tail_libdir/Macaulay2)
AC_SUBST(tail_librariesdir,$tail_libm2dir/lib)
AC_SUBST(tail_programsdir,$tail_libexecdir/Macaulay2/bin)
AC_SUBST(tail_licensesdir,$tail_libexecdir/Macaulay2/program-licenses)
AC_SUBST(tail_packagecachecoredir,$tail_libm2dir/cache)

AC_SUBST(pre_packagesdir,$pre_datadir/Macaulay2)
AC_SUBST(pre_libm2dir,$pre_libdir/Macaulay2)
AC_SUBST(pre_librariesdir,$pre_libm2dir/lib)
AC_SUBST(pre_programsdir,$pre_libexecdir/Macaulay2/bin)
AC_SUBST(pre_licensesdir,$pre_libexecdir/Macaulay2/program-licenses)
AC_SUBST(pre_packagecachecoredir,$pre_libm2dir/cache)

AC_OUTPUT

dnl Local Variables:
dnl mode: autoconf
dnl compile-command: "make -f Makefile "
dnl End:
