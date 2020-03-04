# FIXME: currently various pieces and libraries are not built by cmake
# BUILD/cmake-bootstrap should be a symlink to an existing build directory
set(BOOTSTRAP ${CMAKE_SOURCE_DIR}/BUILD/cmake-bootstrap)
message("## Bootstrapping from previous build in ${BOOTSTRAP}")

################################################################

## Summary of git status
# old output: git describe --dirty --long --always --abbrev=40 --tags --match "version-*"
find_package(Git QUIET)
execute_process(
  COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE   GIT_DESCRIPTION
  ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE
  )

# TODO: when this is stable, use STATUS instead of ##
message("## Configuring Macaulay2 version ${PROJECT_VERSION} from commit ${GIT_DESCRIPTION} for ${CMAKE_SYSTEM_NAME}")

################################################################

# TODO: Which environment variables still relevant?
## Relevant environment variable values, if any:
foreach(X CC CXX AR CPPFLAGS CFLAGS FCFLAGS CXXFLAGS LDFLAGS LIBS ISSUE DISTRIBUTION PKG_CONFIG_PATH GFTABLESDIR)
  if(NOT ("$ENV{${X}}" STREQUAL ""))
    set(${X} "$ENV{${X}}" CACHE STRING "set via environment variable at configure time")
    message("## Set via environment:   ${X} = ${${X}}")
  endif()
endforeach()

## Sets ISSUE, ISSUE_FLAVOR, and ISSUE_RELEASE
include(flavor)

## Complete machine description (to appear in name of tar file)
set(OS      ${CMAKE_SYSTEM_NAME})             # e.g. `uname -s`, Linux, Darwin
set(REL     ${CMAKE_SYSTEM_VERSION})          # e.g. `uname -r`
set(ARCH    ${CMAKE_SYSTEM_PROCESSOR})        # e.g. `uname -p`, x86_64, arm
set(MACHINE ${ARCH}-${OS}-${ISSUE})           # e.g. x86_64-Linux-Fedora-31
SITE_NAME(NODENAME)                           # e.g. `uname -n`

# TODO: Is this still necessary?
# The suffix "-binary" distinguishes the binary program M2-binary from the shell script M2.
# The purpose of the shell script M2 is to set LD_LIBRARY_PATH appropriately.
set(M2SUFFIX "") # used to be set by --progam-suffix
set(EXEEXT   "${CMAKE_EXECUTABLE_SUFFIX}") # DEPRECATE: used in config.h.cmake, version.dd, and M2-init.el.in
set(EXE      "-binary${M2SUFFIX}${CMAKE_EXECUTABLE_SUFFIX}")

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

# TODO: what is the default? mpir currently fails
set(MP_LIBRARY "gmp" CACHE STRING "specify the big integer package to use (mpir or gmp)")

################################################################
## Setting compiler flags

# TODO: where do we use the SHARED setting? In e?


if(${MP_LIBRARY} STREQUAL "mpir")
  set(USING_MPIR 1)
  # TODO: use target_include_directories instead? Maybe this is easier
  include_directories(${CMAKE_SOURCE_DIR}/include/M2/gmp-to-mpir)
  # LIBS="-lmpirxx -lmpir $LIBS"
else()
  set(USING_MPIR 0)
  # LIBS="-lgmpxx  -lgmp  $LIBS"
endif()

if(${PROFILING})
  # TODO: easier way to do this? generator expressions don't work at configure time
  set(PROFILING_STRING 1)
  set(ENABLE_STRIP OFF)
  # TODO: we can do better
  add_compile_options(-pg)
  add_link_options(-pg)
else()
  set(PROFILING_STRING 0)
endif()

# TODO: necessary?
if("${OS}" STREQUAL "Darwin")
  set(SHARED OFF) # TODO: Gatekeeper issue on macOS
  # we don't know what this does, but some apple documentation writers seem to like it:
  set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -bind_at_load")
  # this one makes it find and use our readline.a first, even if there is a file readline.dylib in /usr/lib
  # the point is that the system's readline.dylib might be much older and conflict with our newer one
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,-search_paths_first")
endif()

# TODO: maybe just set this per build configuration?
if(${OPTIMIZE})
  add_compile_options(-O2)
else()
  add_compile_options(-O0)
endif()

if(${MEMDEBUG})
  set(DEBUG ON)
  add_compile_options(-DMEMDEBUG)
endif()

# It is a mistake to pass "-DDEBUG" to preprocessor because it is nonstandard and may confusing libraries,
# such as "flint". Instead, NDEBUG being *not* defined as a C macro is what indicates debug mode.
# This is standard practice. gc.h obeys the GC_DEBUG flag.
if(${DEBUG})
  set(ENABLE_STRIP OFF)
  add_compile_options(-DGC_DEBUG)
else()
  add_compile_options(-DNDEBUG)
endif()

if(${ENABLE_STRIP})
  add_compile_options(-s)
endif()

# always compile with "-g", so we can debug even optimized versions
if(${CMAKE_C_COMPILER_ID} STREQUAL GNU)
  add_compile_options(-g3)
  add_link_options(-g3)
else()
  add_compile_options(-g)
  add_link_options(-g)
endif()

if(${ALTIVEC})
  add_compile_options(-faltivec)
  add_link_options(-faltivec)
endif()

################################################################
## Check for certain header files, functions

# TODO: use CMAKE_REQUIRED_QUIET?

include(CheckTypeSize)
check_type_size("int *" SIZEOF_INT_P)
check_type_size("long" SIZEOF_LONG)

include(CheckSymbolExists)
CHECK_SYMBOL_EXISTS(ADDR_NO_RANDOMIZE "linux/personality.h" HAVE_DECL_ADDR_NO_RANDOMIZE)
CHECK_SYMBOL_EXISTS(herror       "stdlib.h;stdio.h;errno.h" HAVE_DECL_HERROR) # TODO: can stdlib.h always be included?
CHECK_SYMBOL_EXISTS(environ                      "unistd.h" HAVE_DECL_ENVIRON)
CHECK_SYMBOL_EXISTS(_environ                     "unistd.h" HAVE_DECL__ENVIRON)
CHECK_SYMBOL_EXISTS(__environ                    "unistd.h" HAVE_DECL___ENVIRON)

include(CheckLibraryExists)
check_library_exists(rt clock_gettime "" HAVE_CLOCK_GETTIME)
check_library_exists(resolv hstrerror "" HAVE_HSTRERROR)

include(CheckCSourceCompiles)
check_c_source_compiles("int main(){__builtin_return_address(1);return 0;}" BUILTIN_RETURN_ADDRESS_ACCEPTS_NONZERO_ARGUMENT)

include(CheckCXXSourceCompiles)
# whether givaro has isUnit or isunit
# TODO: still necessary?
check_cxx_source_compiles([[#include <givaro/gfq.h>
  int main(){class Givaro::GFqDom<long int> foo; foo.isunit(0);return 0;}]] HAVE_GIVARO_isunit)

# TODO: is there better way to do this?
if(HAVE_GIVARO_isunit)
  set(GIVARO_isunit_STRING 1)
else()
  set(GIVARO_isunit_STRING 0)
ENDIF()

#AC_DEFINE(HAVE_LINBOX,1,[whether we are linking with the linbox library])
#AC_DEFINE(HAVE_FPLLL,1,[whether we are linking with the fplll library])
#AC_DEFINE([HAVE_FACTORY_PREM],[1],[whether Prem() from factory is public])
#AC_DEFINE([FACTORY_STREAMIO],[1],[whether factory was built with --enable-streamio])

#AC_DEFINE_UNQUOTED(GETADDRINFO_WORKS,1,[whether getaddrinfo can handle numeric service (port) numbers])
#AC_DEFINE_UNQUOTED(AUTOINST,$val,whether to instantiate templates automatically)
#AC_DEFINE_UNQUOTED(IMPLINST,$val,whether to instantiate templates implicitly)
#AC_DEFINE_UNQUOTED(WITH_NEWLINE_CRLF,$WITH_NEWLINE_CRLF,[whether newline is cr lf])
#AC_DEFINE_UNQUOTED(WITH_NEWLINE_CR, $WITH_NEWLINE_CR,   [whether newline is cr])

include(CheckIncludeFiles)
# TODO: are all still relevant?
check_include_files(arpa/inet.h	HAVE_ARPA_INET_H)
check_include_files(assert.h	HAVE_ASSERT_H)
check_include_files(dlfcn.h	HAVE_DLFCN_H)
check_include_files(elf.h	HAVE_ELF_H)
check_include_files(execinfo.h	HAVE_EXECINFO_H)
check_include_files(inttypes.h	HAVE_INTTYPES_H)
check_include_files(io.h	HAVE_IO_H)
check_include_files(linux/personality.h	HAVE_LINUX_PERSONALITY_H)
check_include_files(malloc.h	HAVE_MALLOC_H)
check_include_files(math.h	HAVE_MATH_H)
check_include_files(memory.h	HAVE_MEMORY_H)
check_include_files(netdb.h	HAVE_NETDB_H)
check_include_files(netinet/in.h	HAVE_NETINET_IN_H)
check_include_files(pthread.h	HAVE_PTHREAD_H)
check_include_files(stddef.h	HAVE_STDDEF_H)
check_include_files(stdint.h	HAVE_STDINT_H)
check_include_files(stdlib.h	HAVE_STDLIB_H)
check_include_files(strings.h	HAVE_STRINGS_H)
check_include_files(string.h	HAVE_STRING_H)
check_include_files(syscall.h	HAVE_SYSCALL_H)
check_include_files(sys/ioctl.h	HAVE_SYS_IOCTL_H)
check_include_files(sys/mman.h	HAVE_SYS_MMAN_H)
check_include_files(sys/resource.h	HAVE_SYS_RESOURCE_H)
check_include_files(sys/socket.h	HAVE_SYS_SOCKET_H)
check_include_files(sys/stat.h	HAVE_SYS_STAT_H)
check_include_files(sys/time.h	HAVE_SYS_TIME_H)
check_include_files(sys/types.h	HAVE_SYS_TYPES_H)
check_include_files(sys/wait.h	HAVE_SYS_WAIT_H)
check_include_files(termios.h	HAVE_TERMIOS_H)
check_include_files(time.h	HAVE_TIME_H)
check_include_files(unistd.h	HAVE_UNISTD_H)

include(CheckFunctionExists)
# TODO: can't getaddrinfo
check_function_exists(herror	HAVE_HERROR)
check_function_exists(error	HAVE_ERROR)
check_function_exists(backtrace	HAVE_BACKTRACE)
check_function_exists(clock_gettime	HAVE_CLOCK_GETTIME)
check_function_exists(__environ	HAVE___ENVIRON)
check_function_exists(_environ	HAVE__ENVIRON)
check_function_exists(environ	HAVE_ENVIRON)
check_function_exists(_setmode	HAVE__SETMODE)
check_function_exists(getaddrinfo	HAVE_GETADDRINFO)
check_function_exists(hstrerror	HAVE_HSTRERROR)
check_function_exists(sync	HAVE_SYNC)
check_function_exists(getpgrp	HAVE_GETPGRP)
check_function_exists(setpgid	HAVE_SETPGID)
check_function_exists(fchmod	HAVE_FCHMOD)
check_function_exists(pipe	HAVE_PIPE)
check_function_exists(waitpid	HAVE_WAITPID)
check_function_exists(setrlimit	HAVE_SETRLIMIT)
check_function_exists(alarm	HAVE_ALARM)
check_function_exists(fork	HAVE_FORK)
check_function_exists(sigprocmask	HAVE_SIGPROCMASK)
check_function_exists(kill	HAVE_KILL)
check_function_exists(longjmp	HAVE_LONGJMP)
check_function_exists(siglongjmp	HAVE_SIGLONGJMP)
check_function_exists(sigaction	HAVE_SIGACTION)
check_function_exists(wait4	HAVE_WAIT4)
check_function_exists(readlink	HAVE_READLINK)
check_function_exists(lstat	HAVE_LSTAT)
check_function_exists(realpath	HAVE_REALPATH)
check_function_exists(mkdir	HAVE_MKDIR)
check_function_exists(link	HAVE_LINK)
check_function_exists(symlink	HAVE_SYMLINK)
check_function_exists(socket	HAVE_SOCKET)
check_function_exists(accept	HAVE_ACCEPT)
check_function_exists(fcntl	HAVE_FCNTL)
check_function_exists(personality	HAVE_PERSONALITY)
check_function_exists(ioctl	HAVE_IOCTL)

# this is an alternative for AC_FUNC_ALLOCA()
check_function_exists(alloca HAVE_ALLOCA)
check_include_files(alloca.h HAVE_ALLOCA_H)

## mpir and gmp serve the same purpose
# Remark: both gmp.h and mpir.h are surrounded by 
# 	#ifndef __GMP_H__
# 	#endif
# so only one can be loaded.  Similarly for gmpxx.h and mpirxx.h.  However, the contents of
# the files differ.  For example, mpf_cmp_z is defined only in gmp.h.
check_include_files(gmp.h	GMP_FOUND)
check_include_files(mpir.h	MPIR_FOUND)

if(USING_MPIR AND NOT MPIR_FOUND)
  # TODO: build
elseif(NOT USING_MPIR AND NOT GMP_FOUND)
  message(ERROR "gmp integer package specified, but not found")
endif()

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
#executable extension =
#packages = Style Macaulay2Doc ...
#git description = version-1.14.0.1-403-g698a43717742958165ea6909edc971f2c1435d4a
#operating system = Linux
#frobby version = 0.9.0
#endianness = dcba
#mathicgb version = 1.0
#VERSION = 1.15.0.1
#compile node name = noether

################################################################

#### STILL LEFT TO PROCESS ####


#AC_DEFINE([HAVE_FACTORY_PREM], [1],
#    [whether Prem() from factory is public])
#if test $BUILD_factory = no
#then
#    AC_MSG_CHECKING([whether Prem() from factory is public])
#    AC_LANG([C++])
#    AC_COMPILE_IFELSE(
#	[AC_LANG_PROGRAM(
#	    [#include <factory/factory.h>],
#	    [CanonicalForm p,q; Prem(p,q)])],
#	[AC_MSG_RESULT([yes])],
#	[AC_MSG_RESULT([no])
#	 AC_DEFINE([HAVE_FACTORY_PREM], [0])])
#fi

#AC_DEFINE([FACTORY_STREAMIO], [1],
#    [whether factory was built with --enable-streamio])
#if test $BUILD_factory = no
#then
#    AC_MSG_CHECKING([whether factory was built with --enable-streamio])
#    AC_LANG([C++])
#    AC_COMPILE_IFELSE(
#	[AC_LANG_PROGRAM(
#	    [#include <factory/factory.h>],
#	    [Variable x; x = Variable(); std::cout << x])],
#	[AC_MSG_RESULT([yes])],
#	[AC_MSG_RESULT([no])
#	 AC_DEFINE([FACTORY_STREAMIO], [0])])
#fi

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

#AC_SUBST(M2_CPPFLAGS,)
#AC_SUBST(M2_CFLAGS,)
#AC_SUBST(M2_CXXFLAGS,)


#WITH_NEWLINE_CR=0
#WITH_NEWLINE_CRLF=0
#AC_ARG_WITH(newline, AS_HELP_STRING([--with-newline=...], [crlf, cr, or lf (the default)]),
#    [ case $withval in
#	  crlf) WITH_NEWLINE_CRLF=1 WITH_NEWLINE_CR=0 ;;
#	  cr)   WITH_NEWLINE_CR=1 WITH_NEWLINE_CRLF=0 ;;
#	  lf)   WITH_NEWLINE_CR=0 WITH_NEWLINE_CRLF=0 ;;
#	  *)    AC_MSG_ERROR([--with-newline expected crlf, cr, or lf]) ;;
#      esac ])
