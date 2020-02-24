set(CMAKE_VERBOSE_MAKEFILE OFF)

## Summary of git status
# old output: git describe --dirty --long --always --abbrev=40 --tags --match "version-*"
execute_process(
  COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE   GIT_DESCRIPTION
  ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE
  )

message("## Configuring Macaulay2 version ${PROJECT_VERSION} from commit ${GIT_DESCRIPTION}")

################################################################

include(flavor)

## Complete machine description (to appear in name of tar file)
set(OS      ${CMAKE_SYSTEM_NAME})             # e.g. `uname -s`, Linux, Darwin
set(REL     ${CMAKE_SYSTEM_VERSION})          # e.g. `uname -r`
set(ARCH    ${CMAKE_SYSTEM_PROCESSOR})        # e.g. `uname -p`, x86_64, arm
# FIXME: see issue.cmake
set(ISSUE   ${ISSUE_FLAVOR}-${ISSUE_RELEASE}) # e.g. Fedora-31, Ubuntu-7.10
set(MACHINE ${ARCH}-${OS}-${ISSUE})           # e.g. x86_64-Linux-Fedora-31
SITE_NAME(NODENAME)                           # e.g. `uname -n`

# TODO: Is this still necessary?
# The suffix "-binary" distinguishes the binary program M2-binary from the shell script M2.
# The purpose of the shell script M2 is to set LD_LIBRARY_PATH appropriately.
set(M2SUFFIX "") # used to be set by --progam-suffix
set(EXEEXT   "")
set(EXE      "-binary${M2SUFFIX}${EXEEXT}")

################################################################

## Configure options
option(SHARED       "build shared libraries"              ON)
option(OPTIMIZE     "enable optimization"                 ON)
option(PROFILING    "enable profiling"                   OFF)
option(DEBUG        "enable debugging"                   OFF)
option(MEMDEBUG     "enable memory allocation debugging" OFF)
option(ENABLE_STRIP "discard symbols from object files"  OFF)
#option(PTHREADS     "link with pthreads (for gc)"         ON)
#option(XML          "link with xml2"                      ON)
#option(MYSQL        "link with mysql"                    OFF)
#option(PYTHON       "link with libpython"                OFF)
#option(NTL_WIZARD   "enable running the NTL wizard"      OFF)
#option(ALTIVEC      "compile with '-faltivec' option"    OFF)
#option(XCODE        "build Macaulay2/d/interpret.a"      OFF)

################################################################

## Setting compiler flags

# TODO: unnecessary?
if("${OS}" STREQUAL "Darwin")
  # we don't know what this does, but some apple documentation writers seem to like it:
#  LDFLAGS="$LDFLAGS -bind_at_load"
  # this one makes it find and use our readline.a first, even if there is a file readline.dylib in /usr/lib
  # the point is that the system's readline.dylib might be much older and conflict with our newer one
#  LDFLAGS="$LDFLAGS -Wl,-search_paths_first"
endif()

# TODO: where use SHARED setting? In e?

if(${OPTIMIZE})
#  CFLAGS="$CFLAGS -O2"
#  CXXFLAGS="$CXXFLAGS -O2"
#  FCFLAGS="$FCFLAGS -O2"
else()
#  CFLAGS="$CFLAGS -O0"
#  CXXFLAGS="$CXXFLAGS -O0"
#  FCFLAGS="$FCFLAGS -O0"
endif()

# TODO we can do better
if(${PROFILING})
  set(ENABLE_STRIP ON)
#  CFLAGS="$CFLAGS -pg"
#  CXXFLAGS="$CXXFLAGS -pg"
#  LDFLAGS="$LDFLAGS -pg"
endif()

if(${MEMDEBUG})
  set(DEBUG ON)
#  M2_CPPFLAGS="$M2_CPPFLAGS -DMEMDEBUG"
endif()

# It is a mistake to add "-DDEBUG" to CPPFLAGS, because it is nonstandard and may confusing libraries, such as "flint".
# Instead, NDEBUG being *not* defined as a C macro is what indicates debug mode.  This is standard practice.
# gc.h obeys the GC_DEBUG flag:
if(${DEBUG})
  set(ENABLE_STRIP ON)
#  M2_CPPFLAGS="$M2_CPPFLAGS -DGC_DEBUG"
#  CPPFLAGS="$CPPFLAGS -DGC_DEBUG"
else()
#  M2_CPPFLAGS="$M2_CPPFLAGS -DNDEBUG"
#  CPPFLAGS="$CPPFLAGS -DNDEBUG"
endif()

if(${ENABLE_STRIP})
  set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -s")
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -s")
endif()

# always compile with "-g", so we can debug even optimized versions
if(${CMAKE_C_COMPILER_ID} STREQUAL GNU)
#  CFLAGS="$CFLAGS -g3"
#  CXXFLAGS="$CXXFLAGS -g3"
#  LDFLAGS="$LDFLAGS -g3"
#  LDFLAGS_FOR_BUILD="$LDFLAGS_FOR_BUILD -g3"
else()
#  CFLAGS="$CFLAGS -g"
#  CXXFLAGS="$CXXFLAGS -g"
#  LDFLAGS="$LDFLAGS -g"
#  LDFLAGS_FOR_BUILD="$LDFLAGS_FOR_BUILD -g"
endif()

#if(${ALTIVEC})
#  CFLAGS="CFLAGS -faltivec"
#  CXXFLAGS="$CXXFLAGS -faltivec"
#  LDFLAGS="$LDFLAGS -faltivec"
#endif()

################################################################

# TODO: Are environment variables still relevant?
## Relevant environment variable values, if any:
foreach(X CC FC CXX AR CPPFLAGS CFLAGS FCFLAGS CXXFLAGS LDFLAGS LIBS ISSUE DISTRIBUTION PKG_CONFIG_PATH GFTABLESDIR)
  if(NOT ("$ENV{${X}}" STREQUAL ""))
    # TODO: Save in cache?
    set(${X} $ENV{${X}})
    message("## Set via environment:   ${X} = ${${X}}")
  endif()
endforeach()

message("## Operating system information:
     ISSUE             = ${ISSUE}
     NODENAME          = ${NODENAME}
     OS REL            = ${OS} ${REL}
     ARCH              = ${ARCH}
     OPTIMIZE          = ${OPTIMIZE}
     DEBUG             = ${DEBUG}
     GIT_DESCRIPTION   = ${GIT_DESCRIPTION}
     USING_MPIR        = ${USING_MPIR}")

################################################################

# FIXME
#AC_FUNC_ACCEPT_ARGTYPES()

#WITH_NEWLINE_CR=0
#WITH_NEWLINE_CRLF=0
#AC_ARG_WITH(newline, AS_HELP_STRING([--with-newline=...], [crlf, cr, or lf (the default)]),
#    [ case $withval in
#	  crlf) WITH_NEWLINE_CRLF=1 WITH_NEWLINE_CR=0 ;;
#	  cr)   WITH_NEWLINE_CR=1 WITH_NEWLINE_CRLF=0 ;;
#	  lf)   WITH_NEWLINE_CR=0 WITH_NEWLINE_CRLF=0 ;;
#	  *)    AC_MSG_ERROR([--with-newline expected crlf, cr, or lf]) ;;
#      esac ])

#AC_LANG(C)
#AC_MSG_CHECKING([whether getaddrinfo can handle numeric service (port) numbers])
#AC_RUN_IFELSE([AC_LANG_SOURCE([
#    #include <sys/types.h>
#    #ifdef HAVE_SYS_SOCKET_H
#     #include <sys/socket.h>
#    #endif
#    #ifdef HAVE_WINSOCK2_H
#     #include <winsock2.h>
#    #endif
#    #ifdef HAVE_NETDB_H
#     #include <netdb.h>
#    #endif
#    main() {
#       struct addrinfo *addr;
#       return 0 != getaddrinfo("1.2.3.4", "80", 0, &addr) ? 99 : 0 ;
#       }
#    ])],
#    [AC_DEFINE_UNQUOTED(GETADDRINFO_WORKS,1,[whether getaddrinfo can handle numeric service (port) numbers])] [AC_MSG_RESULT(yes)],
#    [if test $? = 99 ; then AC_MSG_RESULT(no) ; else AC_MSG_ERROR([test file failed to compile]) ; fi],
#    [AC_DEFINE_UNQUOTED(GETADDRINFO_WORKS,1,[whether getaddrinfo can handle numeric service (port) numbers])] [AC_MSG_RESULT([probably (cross-compiling, not tested)])])

################################################################

## Definitions in M2/config.h

#AC_DEFINE(HAVE_XML,1,[whether we are linking with the xml library])
#AC_DEFINE_UNQUOTED(USE_MYSQL,$USE_MYSQL,[whether we are linking with the mysql library])
#AC_DEFINE(HAVE_PYTHON,1,[whether we are linking with the python library])

#AC_DEFINE(USING_MPIR,1,[Whether we use MPIR (instead of GMP)])
#AC_DEFINE([HAVE_GIVARO_isunit],,[whether givaro has isunit]) # otherwise
#AC_DEFINE(HAVE_LINBOX,1,[whether we are linking with the linbox library])
#AC_DEFINE(HAVE_FPLLL,1,[whether we are linking with the fplll library])
#AC_DEFINE([HAVE_FACTORY_PREM],[1],[whether Prem() from factory is public])
#AC_DEFINE([FACTORY_STREAMIO],[1],[whether factory was built with --enable-streamio])
#AC_DEFINE(HAVE_MPACK,1,[whether we are linking with the mpack library])

#AC_DEFINE(M2_CONFIG_H,1,a macro definition to ensure our config.h was the one loaded)
#AC_DEFINE_UNQUOTED(CONFIG_ARGS,"$C_CONFIG_ARGS",arguments used for configure)

#AC_DEFINE_UNQUOTED(SOCKLEN_T,[`echo "$ac_cv_func_accept_arg3" | sed 's/ \*$//'`],[socket length type used by accept()])
#AC_DEFINE_UNQUOTED(GETADDRINFO_WORKS,1,[whether getaddrinfo can handle numeric service (port) numbers])
#AC_DEFINE(BUILTIN_RETURN_ADDRESS_ACCEPTS_NONZERO_ARGUMENT,1,[Define if __builtin_return_address accepts a non-zero argument])

#AC_DEFINE_UNQUOTED(OS,"$OS",[operating system name, obtained with uname -s, perhaps modified])
#AC_DEFINE_UNQUOTED(REL,"$REL",[operating system release, obtained with uname -r])
#AC_DEFINE_UNQUOTED(ARCH,"$ARCH",[machine hardware type])
#AC_DEFINE_UNQUOTED(ISSUE,"$ISSUE",[issue (flavor) of operating system, if any])
#AC_DEFINE_UNQUOTED(MACHINE,"$MACHINE",[complete machine description (to appear in name of tar file)])
#AC_DEFINE_UNQUOTED(NODENAME,"$NODENAME",hostname used for compilation)
#AC_DEFINE_UNQUOTED(GIT_DESCRIPTION,"$GIT_DESCRIPTION",[summary of git status])
#AC_DEFINE_UNQUOTED(M2SUFFIX,"$M2SUFFIX",[suffix to append to executable name M2])
#AC_DEFINE_UNQUOTED(EXEEXT,"$EXEEXT",[suffix the compiler appends to executable filenames])
#AC_DEFINE_UNQUOTED(PACKAGES,"$PACKAGES",the list of packages included with the release of Macaulay2)

#AC_DEFINE_UNQUOTED(PROFILING, $val,whether profiling has been enabled)
#AC_DEFINE_UNQUOTED(EXPERIMENT,$val,whether experimental code has been enabled)
#AC_DEFINE_UNQUOTED(DEVELOPMENT,1,whether to build a development version)

#AC_DEFINE_UNQUOTED(AUTOINST,$val,whether to instantiate templates automatically)
#AC_DEFINE_UNQUOTED(IMPLINST,$val,whether to instantiate templates implicitly)

#AC_DEFINE_UNQUOTED(WITH_NEWLINE_CRLF,$WITH_NEWLINE_CRLF,[whether newline is cr lf])
#AC_DEFINE_UNQUOTED(WITH_NEWLINE_CR, $WITH_NEWLINE_CR,   [whether newline is cr])

#AC_DEFINE_UNQUOTED(buildsystemtype,"$build",the type of system on which the package was built)
#AC_DEFINE_UNQUOTED(hostsystemtype,"$host",the type of system on which the package runs)

################################################################

#content of version:
#memtailor version = 1.0
#readline version = 8.0
#factory version = 4.1.1
#givaro version = 4.0.2
#gmp version = not present
#pointer size = 8
#mpir version = 3.0.0
#M2 name = M2
#machine = x86_64-Linux-Fedora-31
#scscp version = not present
#M2 suffix =
#fflas_ffpack version = 2.3.2
#linbox version = not present
#architecture = x86_64
#gc version = 8.0.4
#python version = not present
#operating system release = 5.4.17-200.fc31.x86_64
#atomic_ops version = 7.6.10
#issue = Fedora-31
#mathic version = 1.0
#compile time = Feb  8 2020, 16:21:30
#mysql version = not present
#configure arguments =  '--enable-rpm' '--prefix=/usr' '--enable-download' 'CPPFLAGS=-I/usr/include -I/usr/include/eigen3 -I/usr/include/eigen3/unsupported' 'LDFLAGS=-L/usr/lib64'
#dumpdata = false
#ntl version = 10.5.0
#compiler = gcc 9.2.1
#gdbm version = GDBM version 1.18.1. 27/10/2018 (built Sep 23 2019 00:00:00)
#pari version = 2.11.2
#flint version = 2.5.2
#mpfr version = 4.0.2
#host = x86_64-pc-linux-gnu
#executable extension =
#packages = Style Macaulay2Doc ...
#git description = version-1.14.0.1-403-g698a43717742958165ea6909edc971f2c1435d4a
#build = x86_64-pc-linux-gnu
#operating system = Linux
#frobby version = 0.9.0
#endianness = dcba
#mathicgb version = 1.0
#VERSION = 1.15.0.1
#compile node name = noether



################################################################

#### STILL LEFT TO PROCESS ####

#AC_CHECK_PROGS(ETAGS,etags ctags,false)
#if test "$ETAGS" = false; then AC_MSG_WARN(without etags no TAGS files will be made); fi

#if test "$STRIP" != "false"
#then AC_MSG_CHECKING(whether $STRIP accepts the remove-section option)
#     if "$STRIP" --help 2>&1 | grep remove-section >/dev/null
#     then val=yes
#     else val=no
#     fi
#     AC_MSG_RESULT($val)
#     AC_SUBST(STRIP_REMOVE_SECTION,$val)
#fi

#AC_MSG_CHECKING(whether $MAKE is GNU make)
#if "$MAKE" --version | head -1 | grep GNU >/dev/null 2>&1
#then AC_MSG_RESULT(yes)
#else AC_MSG_RESULT(no)
#     AC_MSG_ERROR($MAKE: GNU make is required)
#fi

#AC_VALIDATE_CACHED_SYSTEM_TUPLE()
#dnl AC_ARG_VAR(CC,C compiler to use)
#dnl AC_ARG_VAR(CXX,C++ compiler to use)

#AC_SUBST(M2_CPPFLAGS,)
#AC_SUBST(M2_CFLAGS,)
#AC_SUBST(M2_CXXFLAGS,)

#AC_CHECK_SIZEOF([int *])
#AC_SUBST(SIZEOF_INT_P,$ac_cv_sizeof_int_p)
#AC_CHECK_SIZEOF([long])
#AC_SUBST(SIZEOF_LONG,$ac_cv_sizeof_long)

#AC_HEADER_TIME()
#AC_CHECK_HEADERS(sys/ioctl.h termios.h sys/mman.h sys/socket.h netdb.h netinet/in.h arpa/inet.h sys/time.h time.h sys/wait.h sys/resource.h io.h linux/personality.h stddef.h stdint.h inttypes.h elf.h execinfo.h stdlib.h syscall.h sys/types.h sys/stat.h unistd.h math.h pthread.h assert.h alloca.h malloc.h dlfcn.h)
#AC_CHECK_HEADERS(winsock2.h) dnl used with ws2_32.dll under mingw64
#dnl winsock2.h should be included before including windows.h
#dnl  pthread.h includes windows.h
#dnl   therefore winsock2.h should be included before pthread
#AC_SEARCH_LIBS(clock_gettime,rt)
#dnl winsock2.h should be used with ws2_32.lib; it defines:
#dnl 	accept bind closesocket connect freeaddrinfo getaddrinfo gethostbyaddr
#dnl 	gethostbyname gethostname getnameinfo getpeername getprotobyname
#dnl 	getprotobynumber getservbyname getservbyport getsockname getsockopt htonl htons
#dnl 	inet_addr inet_ntoa inet_ntop inet_pton ioctlsocket listen ntohl ntohs recv
#dnl 	recvfrom select send sendto setsockopt shutdown socket
#AC_SEARCH_LIBS(socket,socket ws2_32) dnl ws2_32 is used under mingw64
#AC_SEARCH_LIBS(hstrerror,resolv)
#AC_SEARCH_LIBS(dlopen,dl)
#AC_SEARCH_LIBS(gethostbyname,nsl)
#AC_SUBST(HAVE_LIBTBB,no)
#AC_SUBST(LIBTBB,)
#   AC_LANG(C++)
#   AC_CHECK_HEADER(tbb/tbb.h,
#      [AC_SEARCH_LIBS(TBB_runtime_interface_version,tbb,
#	 LIBTBB=$ac_cv_search_TBB_runtime_interface_version
#	 HAVE_LIBTBB=yes)])
#   AC_LANG(C)
#AC_CHECK_FUNCS([herror error backtrace clock_gettime __environ _environ environ _setmode getaddrinfo hstrerror sync getpgrp setpgid fchmod pipe waitpid setrlimit alarm fork sigprocmask kill longjmp siglongjmp sigaction wait4 readlink lstat realpath mkdir link symlink socket accept fcntl personality ioctl])

#AC_SUBST(HAVE_PERSONALITY,$ac_cv_func_personality)

#AC_CHECK_DECLS([ADDR_NO_RANDOMIZE],,,[#include <linux/personality.h>])
#AC_CHECK_DECLS([herror],,,[
#	#ifdef HAVE_STDLIB_H
#	 #include <stdlib.h>
#	#endif
#	#include <stdio.h>
#	#include <errno.h>
#	])
#AC_CHECK_DECLS([__environ,_environ,environ],,,[#include <unistd.h>])
