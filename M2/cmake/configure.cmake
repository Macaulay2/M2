###############################################################################
## This file has multiple sections:
#    1. Define configure options and cached variables
#    2. Detect and print information about build system
#    3. Define variables for installation directories and Macaulay2 Layout
#    4. Define compiler and linker flags and feature
#    5. Detect type sizes and existence of symbols, headers, and functions

###############################################################################
## Define configure options and cached variables
# use CMAKE_BUILD_TYPE=Debug                 instead of DEBUG
# use CMAKE_BUILD_TYPE=Release               instead of OPTIMIZE
# use CMAKE_BUILD_TYPE=RelWithDebInfo        instead of PROFILING
# use CMAKE_BUILD_TYPE=RelMinSize            instead of ENABLE_STRIP
# use BUILD_SHARED_LIBS=ON                   instead of SHARED

option(MEMDEBUG      "enable memory allocation debugging" OFF)
option(GIT_SUBMODULE "update submodules during build"     OFF)
# TODO:
#option(MYSQL        "link with mysql"                    OFF)
#option(PYTHON       "link with libpython"                OFF)
#option(NTL_WIZARD   "enable running the NTL wizard"      OFF)
#option(ALTIVEC      "compile with '-faltivec' option"    OFF)
#option(XCODE        "build Macaulay2/d/interpret.a"      OFF)

set(MP_LIBRARY    MPIR CACHE STRING "specify the multiple precision library to use (MPIR or GMP)")
set(PARALLEL_JOBS 4    CACHE STRING "specify the number of parallel jobs for libraries and programs")

# TODO: deprecate these variables
set(M2SUFFIX "")
set(EXEEXT   "${CMAKE_EXECUTABLE_SUFFIX}")
set(EXE      "-binary${M2SUFFIX}${CMAKE_EXECUTABLE_SUFFIX}")
set(PACKAGE_VERSION ${Macaulay2_VERSION})

###############################################################################
## Detect and print information about build system

## Summary of git status
find_package(Git QUIET)
if(GIT_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/../.git")
  # previous describe code: git describe --dirty --long --always --abbrev=40 --tags --match "version-*"
  execute_process(
    COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
    ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE   GIT_COMMIT)
endif()

message("## Configure Macaulay2
     M2 Version        = ${PROJECT_VERSION}
     Git Commit        = ${GIT_COMMIT}
     CMake Build Type  = ${CMAKE_BUILD_TYPE}
     BUILD_TESTING     = ${BUILD_TESTING}")

## Relevant environment variable values, if any:
foreach(X AR CPPFLAGS CFLAGS CXXFLAGS LDFLAGS LIBS ISSUE DISTRIBUTION)
  if(NOT "$ENV{${X}}" STREQUAL "${${X}}")
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

message("## Host operating system information:
     ISSUE             = ${ISSUE}
     NODENAME          = ${NODENAME}
     OS REL            = ${OS} ${REL}
     ARCH              = ${ARCH}")

# TODO
# message("## Target operating system information:")

################################################################
## Define variables for installation directories and Macaulay2 Layout
# TODO: install the unstripped library with debug_info in the appropriate place.
# On Fedora: /usr/lib/debug/usr/lib64/

# staging area for building libraries needed to compile M2
set(M2_INSTALL_PREFIX	${CMAKE_BINARY_DIR}/usr-dist CACHE PATH "target build prefix")
set(M2_HOST_PREFIX	${CMAKE_BINARY_DIR}/usr-host CACHE PATH "host build prefix")
set(M2_EXEC_INFIX	${MACHINE}	CACHE INTERNAL "infix for architecture dependent files")
set(M2_DATA_INFIX	common		CACHE INTERNAL "infix for architecture independent files")

set(CMAKE_INSTALL_DATADIR share/Macaulay2)
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/usr-dist CACHE PATH "installation prefix" FORCE)
endif()

## This is using https://cmake.org/cmake/help/latest/module/GNUInstallDirs.html#module:GNUInstallDirs
## Which follows https://www.gnu.org/prep/standards/html_node/Directory-Variables.html
include(GNUInstallDirs)

# setting architecture dependent paths as in layout.m2.in
foreach(DIR IN ITEMS BINDIR LIBDIR LIBEXECDIR)
  set(M2_INSTALL_${DIR} ${M2_EXEC_INFIX}/${CMAKE_INSTALL_${DIR}})
  GNUInstallDirs_get_absolute_install_dir(M2_INSTALL_FULL_${DIR} M2_INSTALL_${DIR})
endforeach()

# setting architecture independent paths as in layout.m2.in
foreach(DIR IN ITEMS SYSCONFDIR DATAROOTDIR DATADIR INFODIR LOCALEDIR MANDIR DOCDIR INCLUDEDIR)
  set(M2_INSTALL_${DIR} ${M2_DATA_INFIX}/${CMAKE_INSTALL_${DIR}})
  GNUInstallDirs_get_absolute_install_dir(M2_INSTALL_FULL_${DIR} M2_INSTALL_${DIR})
endforeach()

set(M2_INSTALL_PROGRAMSDIR ${M2_INSTALL_PREFIX}/${M2_EXEC_INFIX}/${CMAKE_INSTALL_LIBEXECDIR}/Macaulay2)

message("## Staging area directories:
     common:	${M2_INSTALL_PREFIX}/${M2_DATA_INFIX}
     exec:	${M2_INSTALL_PREFIX}/${M2_EXEC_INFIX}")
message("## Installation prefix: ${CMAKE_INSTALL_PREFIX}")

###############################################################################
## Define compiler and linker flags and features
# look into compiler features:
# https://cmake.org/cmake/help/latest/prop_gbl/CMAKE_CXX_KNOWN_FEATURES.html

# Common flags
add_compile_options(
  -I${CMAKE_SOURCE_DIR}/include
  -I${CMAKE_BINARY_DIR}/include

  # TODO: where should these be set?
  -DSING_NDEBUG -DOM_NDEBUG # factory wants these
  )

# Flags based on options
if(MEMDEBUG)
  add_compile_options(-DMEMDEBUG)
endif()
# TODO: deal with all SIMDs together
if(ALTIVEC)
  add_compile_options(-faltivec)
  add_link_options(-faltivec)
endif()

# Flags based on build type
# NOTE: gc.h obeys the GC_DEBUG flag.
if(CMAKE_BUILD_TYPE MATCHES "Deb") # Debugging
  add_compile_options(-O0 -DGC_DEBUG)
endif()
if(CMAKE_BUILD_TYPE MATCHES "RelWithDebInfo") # Profiling
  add_compile_options(-pg) #DNDEBUG
  add_link_options(-pg)
endif()
if(CMAKE_BUILD_TYPE MATCHES "Release|MinSizeRel")
  add_compile_options(-O2 -DNDEBUG -Wuninitialized)
  # TODO: what are the right strip options?
  if(CMAKE_C_COMPILER_ID STREQUAL GNU)
    add_compile_options(-s)
  endif()
endif()

# Flags based on compiler
# always compile with "-g", so we can debug even optimized versions
if(CMAKE_C_COMPILER_ID STREQUAL GNU)
  add_compile_options(-g3

    # TODO: is this not necessary with clang?
    # -fopenmp # fflas_ffpack wants this, but currently normaliz fails on macOS with it
    -fabi-version=6 # givaro and fflas_ffpack want this
    )
  add_link_options(-g3)
else()
  add_compile_options(-g)
  add_link_options(-g)
endif()

###############################################################################
## Detect type sizes and existence of symbols, headers, and functions
# TIP: set CMAKE_REQUIRED_QUIET=ON to turn off output in this section

include(CheckTypeSize)
check_type_size("int *" SIZEOF_INT_P)
check_type_size("long" SIZEOF_LONG)

include(CheckSymbolExists)
CHECK_SYMBOL_EXISTS(ADDR_NO_RANDOMIZE "linux/personality.h" HAVE_DECL_ADDR_NO_RANDOMIZE)
CHECK_SYMBOL_EXISTS(herror       "stdlib.h;stdio.h;errno.h" HAVE_DECL_HERROR)
CHECK_SYMBOL_EXISTS(environ                      "unistd.h" HAVE_DECL_ENVIRON)
CHECK_SYMBOL_EXISTS(_environ                     "unistd.h" HAVE_DECL__ENVIRON)
CHECK_SYMBOL_EXISTS(__environ                    "unistd.h" HAVE_DECL___ENVIRON)

include(CheckLibraryExists)
check_library_exists(rt clock_gettime "" HAVE_CLOCK_GETTIME)
check_library_exists(resolv hstrerror "" HAVE_HSTRERROR)

include(CheckIncludeFiles)
# TODO: are all still relevant?
check_include_files("stdlib.h;stdarg.h;string.h;float.h" STDC_HEADERS)
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
# TODO: clear out d/types.h

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

include(CheckCSourceCompiles)
include(CheckCXXSourceCompiles)

# TODO: what is this for?
check_c_source_compiles("int main(){__builtin_return_address(1);return 0;}" BUILTIN_RETURN_ADDRESS_ACCEPTS_NONZERO_ARGUMENT)

# TODO: is this necessary?
# whether getaddrinfo can handle numeric service (port) numbers
check_c_source_compiles([[
  #include <sys/types.h>
  #ifdef HAVE_SYS_SOCKET_H
   #include <sys/socket.h>
  #endif
  #ifdef HAVE_NETDB_H
   #include <netdb.h>
  #endif
  int main(){struct addrinfo *addr;return 0 != getaddrinfo("1.2.3.4", "80", 0, &addr) ? 99 : 0;}]] GETADDRINFO_WORKS)

################################################################

# this is an alternative for AC_FUNC_ALLOCA()
# TODO: is it still relevant?
check_function_exists(alloca HAVE_ALLOCA)
check_include_files(alloca.h HAVE_ALLOCA_H)
