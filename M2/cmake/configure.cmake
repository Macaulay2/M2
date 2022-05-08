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
# use CMAKE_BUILD_TYPE=Release               for releases
# use CMAKE_BUILD_TYPE=RelWithDebInfo        instead of PROFILING
# use CMAKE_BUILD_TYPE=RelMinSize            for minimized release
# use BUILD_TESTING=ON                       to build the testing tree

option(USING_MPIR	"Use MPIR instead of GMP"		OFF)
option(DEVELOPMENT	"Set the DEVELOPMENT macro in config.h"	OFF)
option(EXPERIMENT	"Set the EXPERIMENT macro in config.h"	OFF)
option(LINTING		"Enable linting source files"		OFF)
option(MEMDEBUG		"Enable memory allocation debugging"	OFF)
option(PROFILING	"Enable profiling build flags"		OFF)
option(COVERAGE		"Enable Clang code coverage test"	OFF)
option(GIT_SUBMODULE	"Update submodules during build"	ON)
option(BUILD_NATIVE	"Use native SIMD instructions"		ON)
option(BUILD_SHARED_LIBS "Build shared libraries"		OFF)
option(BUILD_DOCS	"Build internal documentation"		OFF)
option(AUTOTUNE		"Autotune library parameters"		OFF)
option(WITH_OMP		"Link with the OpenMP library"		ON)
option(WITH_TBB		"Link with the TBB library"		ON)
# TODO: parse.d expr.d tokens.d actors4.d actors5.d still need xml
option(WITH_XML		"Link with the libxml2 library"		ON)
# TODO: still not operational
option(WITH_PYTHON	"Link with the Python library"		OFF)
option(WITH_MYSQL	"Link with the MySQL library"		OFF)

set(BUILD_PROGRAMS  "" CACHE STRING "Build programs, even if found")
set(BUILD_LIBRARIES "" CACHE STRING "Build libraries, even if found")
set(PARALLEL_JOBS 4
  CACHE STRING "Number of parallel jobs for libraries and programs")
set(SKIP_TESTS "mpsolve;googletest" CACHE STRING "Tests to skip")
set(SLOW_TESTS "eigen;ntl;flint"    CACHE STRING "Slow tests to skip")

# TODO: hopefully make these automatic
if(USING_MPIR)
  list(APPEND BUILD_LIBRARIES "MPIR;MPFR;NTL;Flint;Factory;Frobby;Givaro")
endif()

# TODO: deprecate these variables
set(M2SUFFIX "")
set(EXEEXT   "${CMAKE_EXECUTABLE_SUFFIX}")
set(EXE      "-binary${M2SUFFIX}${CMAKE_EXECUTABLE_SUFFIX}")
set(PACKAGE_NAME    ${PROJECT_NAME})
set(PACKAGE_TARNAME ${PROJECT_NAME})
set(PACKAGE_VERSION ${Macaulay2_VERSION})

###############################################################################
## Detect and print information about build system

## Summary of git status
find_package(Git QUIET)
if(GIT_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/../.git")
  execute_process(
    COMMAND ${GIT_EXECUTABLE} describe --dirty --always --match HEAD
    ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE   GIT_COMMIT)
  # TODO: currently finds the last commit that changed the VERSION file
  # but ideally it should get the last release commit instead
  execute_process(
    COMMAND ${GIT_EXECUTABLE} rev-list -1 HEAD VERSION
    ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE   _release_commit)
  execute_process(
    COMMAND ${GIT_EXECUTABLE} rev-list ${_release_commit}..HEAD --count
    ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE   COMMIT_COUNT)
  set(GIT_DESCRIPTION version-${PROJECT_VERSION}-${COMMIT_COUNT}-${GIT_COMMIT})
else()
  message(NOTICE "## Not building from a git repository; submodules may need to be manually populated")
  set(GIT_DESCRIPTION version-${PROJECT_VERSION} CACHE INTERNAL "state of the repository")
  file(GLOB _submodules LIST_DIRECTORIES true ${CMAKE_SOURCE_DIR}/submodules/*)
  foreach(_submodule IN LISTS _submodules)
    if(IS_DIRECTORY ${_submodule})
      # CMake doesn't like empty source directories for ExternalProject_Add
      file(TOUCH ${_submodule}/.nogit)
    endif()
  endforeach()
endif()

## Detect brew prefix
find_program(BREW NAMES brew)
if(EXISTS ${BREW})
  execute_process(
    COMMAND ${BREW} --prefix
    ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE
    OUTPUT_VARIABLE HOMEBREW_PREFIX)
endif()

message("## Configure Macaulay2
     M2 version        = ${PROJECT_VERSION}
     Git description   = ${GIT_DESCRIPTION}
     Install prefix    = ${CMAKE_INSTALL_PREFIX}\n
     CMAKE_BUILD_TYPE  = ${CMAKE_BUILD_TYPE}
     BUILD_NATIVE      = ${BUILD_NATIVE}
     BUILD_SHARED_LIBS = ${BUILD_SHARED_LIBS}
     BUILD_TESTING     = ${BUILD_TESTING}
     BUILD_DOCS        = ${BUILD_DOCS}\n
     COVERAGE          = ${COVERAGE}
     MEMDEBUG          = ${MEMDEBUG}
     PROFILING         = ${PROFILING}\n
     DEVELOPMENT       = ${DEVELOPMENT}
     EXPERIMENT        = ${EXPERIMENT}")

## Set machine description variables used in version.dd
include(flavor) ## Set ISSUE, ISSUE_FLAVOR, and ISSUE_RELEASE
set(OS      ${CMAKE_SYSTEM_NAME})             # e.g. `uname -s`, Linux, Darwin
set(REL     ${CMAKE_SYSTEM_VERSION})          # e.g. `uname -r`
set(ARCH    ${CMAKE_SYSTEM_PROCESSOR})        # e.g. `uname -p`, x86_64, arm
set(MACHINE ${ARCH}-${OS}-${ISSUE})           # e.g. x86_64-Linux-Fedora-31
SITE_NAME(NODENAME)                           # e.g. `uname -n`

message("\n## Host OS information
     ISSUE             = ${ISSUE}
     NODENAME          = ${NODENAME}
     OS REL            = ${OS} ${REL}
     ARCH              = ${ARCH}")

# TODO
# message("## Target OS information")

###############################################################################
## Define variables for installation directories and Macaulay2 Layout
# TODO: install the unstripped library with debug_info in the appropriate place.
# On Fedora: /usr/lib/debug/usr/lib64/

## Setting the staging area for building libraries needed to compile M2
set(M2_DIST_PREFIX	${CMAKE_BINARY_DIR}/usr-dist	CACHE PATH	"target build prefix")
set(M2_HOST_PREFIX	${CMAKE_BINARY_DIR}/usr-host	CACHE PATH	"host build prefix")
set(M2_EXEC_INFIX	${MACHINE}			CACHE INTERNAL	"infix for architecture dependent files")
set(M2_DATA_INFIX	common				CACHE INTERNAL	"infix for architecture independent files")

## Setting the prefix so pkg-config can find libraries we've built
set(ENV{PKG_CONFIG_PATH}	${M2_HOST_PREFIX}/lib/pkgconfig:$ENV{PKG_CONFIG_PATH})

## Setting the prefixes where CMake will look for headers, libraries, and programs
set(CMAKE_SYSTEM_PREFIX_PATH	${M2_HOST_PREFIX} ${CMAKE_SYSTEM_PREFIX_PATH})
set(CMAKE_PREFIX_PATH		${CMAKE_PREFIX_PATH} ${M2_HOST_PREFIX})

## Setting the folder for Macaulay2 Core and packages
set(CMAKE_INSTALL_DATADIR	share/Macaulay2)

## This is using https://cmake.org/cmake/help/latest/module/GNUInstallDirs.html#module:GNUInstallDirs
## Which follows https://www.gnu.org/prep/standards/html_node/Directory-Variables.html
include(GNUInstallDirs)

# setting architecture dependent paths as in layout.m2.in
foreach(DIR IN ITEMS BINDIR LIBDIR LIBEXECDIR)
  set(M2_INSTALL_${DIR} ${M2_EXEC_INFIX}/${CMAKE_INSTALL_${DIR}})
  GNUInstallDirs_get_absolute_install_dir(M2_INSTALL_FULL_${DIR} M2_INSTALL_${DIR} ${DIR})
endforeach()

# setting architecture independent paths as in layout.m2.in
foreach(DIR IN ITEMS SYSCONFDIR DATAROOTDIR DATADIR INFODIR LOCALEDIR MANDIR DOCDIR INCLUDEDIR)
  set(M2_INSTALL_${DIR} ${M2_DATA_INFIX}/${CMAKE_INSTALL_${DIR}})
  GNUInstallDirs_get_absolute_install_dir(M2_INSTALL_FULL_${DIR} M2_INSTALL_${DIR} ${DIR})
endforeach()

set(M2_INSTALL_LICENSESDIR ${M2_DIST_PREFIX}/${M2_EXEC_INFIX}/${CMAKE_INSTALL_LIBEXECDIR}/Macaulay2/program-licenses)
set(M2_INSTALL_PROGRAMSDIR ${M2_DIST_PREFIX}/${M2_EXEC_INFIX}/${CMAKE_INSTALL_LIBEXECDIR}/Macaulay2/bin)
set(CMAKE_PROGRAM_PATH     ${M2_INSTALL_PROGRAMSDIR})

message("\n## Staging area prefixes
     usr-host          = ${M2_HOST_PREFIX}
     common            = ${M2_DIST_PREFIX}/${M2_DATA_INFIX}
     exec              = ${M2_DIST_PREFIX}/${M2_EXEC_INFIX}")

###############################################################################
## Define compiler and linker flags and features

# Flags based on architecture
# Clang:
#  - https://clang.llvm.org/docs/CrossCompilation.html
#  - clang -march=native -E - -###
# GCC:
#  - gcc -march=native -Q --help=target
#  - gcc -march=native -E - -###
if(BUILD_NATIVE)
  add_compile_options(-march=native)
  add_link_options(-march=native)
else()
  # TODO
endif()

# Flags based on options
if(MEMDEBUG)
  add_compile_definitions(MEMDEBUG)
endif()
if(PROFILING)
  add_compile_definitions(PROFILING)
  add_compile_options(-pg)
  add_link_options(-pg)
endif()

# Flags based on build type
# Note: certain flags are initialized by CMake based on the compiler and build type.
if(CMAKE_BUILD_TYPE MATCHES "Debug") # Debugging
  # INIT: -g
  add_compile_definitions(GC_DEBUG)
  add_compile_options(-O0)
else()
  add_compile_definitions(NDEBUG OM_NDEBUG SING_NDEBUG)
  add_compile_options(-Wuninitialized)
endif()
if(CMAKE_BUILD_TYPE MATCHES "MinSizeRel")
  # INIT: -Os
elseif(CMAKE_BUILD_TYPE MATCHES "Release")
  # INIT: -O2
elseif(CMAKE_BUILD_TYPE MATCHES "RelWithDebInfo")
  # INIT: -O2 -g
endif()

# Flags based on compiler
if(CMAKE_C_COMPILER_ID STREQUAL GNU)
  add_compile_options(-g3)
  add_link_options(-g3)
elseif(CMAKE_C_COMPILER_ID STREQUAL AppleClang)
  add_compile_options(-g --sysroot=${CMAKE_OSX_SYSROOT})
  add_link_options(-g --sysroot=${CMAKE_OSX_SYSROOT})
else()
  add_compile_options(-g)
  add_link_options(-g)
endif()

# TODO: look into compiler features:
# https://cmake.org/cmake/help/latest/prop_gbl/CMAKE_CXX_KNOWN_FEATURES.html

# Common flags
# TODO: reduce these if possible
add_link_options(-L${M2_HOST_PREFIX}/lib)
add_compile_options(
  -I${M2_HOST_PREFIX}/include
  -I${CMAKE_SOURCE_DIR}/include
  -I${CMAKE_BINARY_DIR}/include
  )

# Querying the options so we can print them
get_property(COMPILE_DEFINITIONS DIRECTORY PROPERTY COMPILE_DEFINITIONS)
get_property(COMPILE_OPTIONS DIRECTORY PROPERTY COMPILE_OPTIONS)
get_property(LINK_OPTIONS    DIRECTORY PROPERTY LINK_OPTIONS)

message("\n## Compiler information
     C                 = ${CMAKE_C_COMPILER_ID} ${CMAKE_C_COMPILER_VERSION} (${CMAKE_C_COMPILER})
     C++               = ${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION} (${CMAKE_CXX_COMPILER})
     Ccache            = ${CMAKE_C_COMPILER_LAUNCHER}\n")

if(VERBOSE)
  message("## Build flags (excluding standard ${CMAKE_BUILD_TYPE} flags)
     Compiler preprocessor options = ${COMPILE_DEFINITIONS}
     Compiler flags    = ${COMPILE_OPTIONS}
     Linker flags      = ${LINK_OPTIONS}\n")
  message("## CMake path variables
     CMAKE_SYSTEM_PREFIX_PATH
       ${CMAKE_SYSTEM_PREFIX_PATH}
     CMAKE_C_IMPLICIT_LINK_DIRECTORIES
       ${CMAKE_C_IMPLICIT_LINK_DIRECTORIES}\n")
endif()

###############################################################################
## Detect type sizes and existence of symbols, headers, and functions

if(NOT VERBOSE)
  set(CMAKE_REQUIRED_QUIET ON)
endif()

include(CheckTypeSize)
check_type_size("int *" SIZEOF_INT_P)
check_type_size("long" SIZEOF_LONG)

include(CheckLibraryExists)
check_library_exists(rt clock_gettime "" HAVE_CLOCK_GETTIME)
check_library_exists(resolv hstrerror "" HAVE_HSTRERROR)

include(CheckIncludeFiles)
# TODO: are all still relevant?
# HAVE_SCSCP is used in version.dd: https://www.openmath.org/standard/scscp/
check_include_files("stdlib.h;stdarg.h;string.h;float.h" STDC_HEADERS)
check_include_files(arpa/inet.h	HAVE_ARPA_INET_H)
check_include_files(assert.h	HAVE_ASSERT_H)
check_include_files(dlfcn.h	HAVE_DLFCN_H)
check_include_files(execinfo.h	HAVE_EXECINFO_H)
check_include_files(inttypes.h	HAVE_INTTYPES_H)
check_include_files(io.h	HAVE_IO_H)
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
check_function_exists(clock_gettime	HAVE_CLOCK_GETTIME)
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

###############################################################################

# this is an alternative for AC_FUNC_ALLOCA()
# TODO: is it still relevant?
check_function_exists(alloca HAVE_ALLOCA)
check_include_files(alloca.h HAVE_ALLOCA_H)
