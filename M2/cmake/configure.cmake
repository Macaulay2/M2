set(CMAKE_VERBOSE_MAKEFILE OFF)

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

# TODO: how do we not include gmp? Factory always includes it!
if(${MP_LIBRARY} STREQUAL "mpir")
  set(USING_MPIR 1)
else()
  set(USING_MPIR 0)
endif()

# TODO: easier way to do this? generator expressions don't work at configure time
if(${PROFILING})
  set(PROFILING_STRING 1)
else()
  set(PROFILING_STRING 0)
endif()

# TODO: unnecessary?
if("${OS}" STREQUAL "Darwin")
  set(SHARED OFF) # TODO: Gatekeeper issue
  # we don't know what this does, but some apple documentation writers seem to like it:
#  LDFLAGS="$LDFLAGS -bind_at_load"
  # this one makes it find and use our readline.a first, even if there is a file readline.dylib in /usr/lib
  # the point is that the system's readline.dylib might be much older and conflict with our newer one
#  LDFLAGS="$LDFLAGS -Wl,-search_paths_first"
endif()

# TODO: where do we use the SHARED setting? In e?

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
## Look for packages and libraries using CMake or pkg-config

# FIXME: should these stay here or go to bin/CMakeLists.txt?
# TODO: which ones need to be patched and built?
## Find libraries available as CMake modules
find_package(BLAS    3.8 REQUIRED QUIET)
find_package(LAPACK  3.8 REQUIRED QUIET) # TODO: both?
find_package(Eigen3  3.3 REQUIRED QUIET NO_MODULE)
find_package(LibXml2 2.9 REQUIRED QUIET) # need xmlNewNode
find_package(Threads 2.1 REQUIRED QUIET) # pthread
find_package(LibLZMA 5.2 REQUIRED QUIET) # need lzma_end
find_package(MPIR    3.0          QUIET)          # cmake/FindMPIR.cmake
# OpenMP is required for building the library csdp and good for building the library normaliz
find_package(OpenMP      REQUIRED QUIET) # TODO: use OPENMP_LIBS/CXXFLAGS for csdb
find_package(PkgConfig   REQUIRED QUIET)

if(NOT ${MPIR_FOUND})
  set(USING_MPIR 0)
endif()

## Set paths for pkg-config
set(ENV{PKG_CONFIG_PATH} "${BOOTSTRAP}/usr-host/lib/pkgconfig:${BOOTSTRAP}/submodules/mathicgb/build/autotools:${BOOTSTRAP}/submodules/mathic/build/autotools:${BOOTSTRAP}/submodules/memtailor/build/autotools")

## Find libraries available via pkg-config
## tip: use cmake -LA to list resolved variables
# TODO: use foo>=VERSION to specify version
pkg_search_module(GIVARO    REQUIRED givaro                   IMPORTED_TARGET)
pkg_search_module(MATHICGB  REQUIRED mathicgb                 IMPORTED_TARGET)
# TODO: investigate error when factory-devel package is installed:
# sample Factory finite field addition table file missing, needed for factorization:
# /home/mahrud/Projects/M2/M2/M2/BUILD/mahrud/build/usr-dist//usr/share/factory/
pkg_search_module(FACTORY   REQUIRED factory singular-factory IMPORTED_TARGET)
# To fix the error, change the givaro requirement in ${BOOTSTRAP}/usr-host/lib/pkgconfig/fflas-ffpack.pc to 4.0.2
pkg_search_module(FFLAS_FFPACK       fflas-ffpack             IMPORTED_TARGET)
pkg_search_module(READLINE           readline                 IMPORTED_TARGET)
# TODO: should we export GC_LARGE_ALLOC_WARN_INTERVAL=1?
pkg_search_module(GC        REQUIRED bdw-gc                   IMPORTED_TARGET)

## Find all other libraries
find_library(LIBHISTORY history)
find_library(LIBFROBBY frobby)
find_library(LIBGDBM gdbm)
find_library(LIBM m) # need pow from math.h
find_library(LIBC c)

# TODO:
#test:
#	: "PKG_CONFIG_PATH    = $(PKG_CONFIG_PATH)"
#	: "M2_PKG_CONFIG_PATH = $(M2_PKG_CONFIG_PATH)"
#	:
#	: "GIVARO_CXXFLAGS       = $(GIVARO_CXXFLAGS)"
#	: "FFLAS_FFPACK_CXXFLAGS = $(FFLAS_FFPACK_CXXFLAGS)"
#	:
#	: "GIVARO_LIBS           = $(GIVARO_LIBS)"
#	: "FFLAS_FFPACK_LIBS     = $(FFLAS_FFPACK_LIBS)"
#	: "M2_LIBRARIES          = $(M2_LIBRARIES)"

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

#AC_DEFINE([HAVE_GIVARO_isunit],,[whether givaro has isunit]) # otherwise
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

################################################################

# TODO
# message("## Host operating system information:")

message("## Target operating system information:
     ISSUE             = ${ISSUE}
     NODENAME          = ${NODENAME}
     OS REL            = ${OS} ${REL}
     ARCH              = ${ARCH}
     OPTIMIZE          = ${OPTIMIZE}
     DEBUG             = ${DEBUG}
     GIT_DESCRIPTION   = ${GIT_DESCRIPTION}
     USING_MPIR        = ${USING_MPIR}")

################################################################
## Setting variables for installation directories and Macaulay2 Layout
# TODO: where should these be set? most are only used in startup.m2, so could move there
# if they weren't prefixed with common and exec they would be more useful

## Only overwrite the default prefix, not one provided via command line
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/usr-dist CACHE PATH "..." FORCE)
endif()

set(M2_INSTALL_PREFIX	${CMAKE_INSTALL_PREFIX})
set(M2_COMMON_PREFIX	${M2_INSTALL_PREFIX}/common)     # staging area for common files as in layout.m2.in
set(M2_EXEC_PREFIX	${M2_INSTALL_PREFIX}/${MACHINE}) # staging area for arch. dep. files as in layout.m2.in
set(M2_PACKAGE_DIR	${M2_COMMON_PREFIX}/share/Macaulay2)
set(M2_CORE_DIR		${M2_PACKAGE_DIR}/Core)

## Rewrite this so that GNUInstallDirs module can use it to generate architecture independent directories
set(CMAKE_INSTALL_PREFIX ${M2_EXEC_PREFIX})
# Possible alternative:
#set(CMAKE_INSTALL_DATAROOTDIR common/share) # ${MACHINE}/common will be a symlink to ../common

## This is using https://cmake.org/cmake/help/latest/module/GNUInstallDirs.html#module:GNUInstallDirs
## Which follows https://www.gnu.org/prep/standards/html_node/Directory-Variables.html
## Also see FHS  https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html
include(GNUInstallDirs)

message("## Staging area directory:
     common:	${M2_COMMON_PREFIX}
     exec:	${M2_EXEC_PREFIX}")

# TODO: install in /opt instead?

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
