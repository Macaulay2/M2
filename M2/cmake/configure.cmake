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
message("## Configure Macaulay2
     M2 Version        = ${PROJECT_VERSION}
     Git Commit        = ${GIT_DESCRIPTION}
     CMake Build Type  = ${CMAKE_BUILD_TYPE}")

################################################################

# TODO: Which environment variables still relevant?
## Relevant environment variable values, if any:
foreach(X CC CXX AR CPPFLAGS CFLAGS CXXFLAGS LDFLAGS LIBS ISSUE DISTRIBUTION PKG_CONFIG_PATH GFTABLESDIR)
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

# TODO: is it okay to do this globally?
add_compile_options(-I${CMAKE_SOURCE_DIR}/include -I${CMAKE_BINARY_DIR}/include)

if(PROFILING)
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
if(APPLE)
  set(SHARED OFF) # TODO: Gatekeeper issue on macOS
  # we don't know what this does, but some apple documentation writers seem to like it:
  set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -bind_at_load")
  # this one makes it find and use our readline.a first, even if there is a file readline.dylib in /usr/lib
  # the point is that the system's readline.dylib might be much older and conflict with our newer one
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,-search_paths_first")
endif()

# TODO: maybe just set this per build configuration?
if(OPTIMIZE)
  add_compile_options(-O2 -Wuninitialized)
else()
  add_compile_options(-O0)
endif()

if(MEMDEBUG)
  set(DEBUG ON)
  add_compile_options(-DMEMDEBUG)
endif()

# It is a mistake to pass "-DDEBUG" to preprocessor because it is nonstandard and may confusing libraries,
# such as "flint". Instead, NDEBUG being *not* defined as a C macro is what indicates debug mode.
# This is standard practice. gc.h obeys the GC_DEBUG flag.
if(DEBUG)
  set(ENABLE_STRIP OFF)
  add_compile_options(-DGC_DEBUG)
else()
  add_compile_options(-DNDEBUG)
endif()

if(ENABLE_STRIP)
  add_compile_options(-s)
endif()

# always compile with "-g", so we can debug even optimized versions
if(CMAKE_C_COMPILER_ID STREQUAL GNU)
  add_compile_options(-g3)
  add_link_options(-g3)
else()
  add_compile_options(-g)
  add_link_options(-g)
endif()

if(ALTIVEC)
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

#AC_DEFINE(HAVE_LINBOX,1,[whether we are linking with the linbox library])
#AC_DEFINE(HAVE_FPLLL,1,[whether we are linking with the fplll library])

#AC_DEFINE_UNQUOTED(AUTOINST,$val,whether to instantiate templates automatically)
#AC_DEFINE_UNQUOTED(IMPLINST,$val,whether to instantiate templates implicitly)
#AC_DEFINE_UNQUOTED(WITH_NEWLINE_CRLF,$WITH_NEWLINE_CRLF,[whether newline is cr lf])
#AC_DEFINE_UNQUOTED(WITH_NEWLINE_CR, $WITH_NEWLINE_CR,   [whether newline is cr])

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
check_c_source_compiles("int main(){__builtin_return_address(1);return 0;}" BUILTIN_RETURN_ADDRESS_ACCEPTS_NONZERO_ARGUMENT)

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

include(CheckCXXSourceCompiles)
# whether givaro has isUnit (4.0.3) or isunit (4.0.2)
check_cxx_source_compiles([[#include <givaro/gfq.h>
  int main(){class Givaro::GFqDom<long int> foo; foo.isunit(0);return 0;}]] HAVE_GIVARO_isunit)

# whether factory was built with --enable-streamio
check_cxx_source_compiles([[#include <factory/factory.h>
  int main(){Variable x; x = Variable(); std::cout << x;return 0;}]] FACTORY_STREAMIO)
# whether Prem() from factory is public
check_cxx_source_compiles([[#include <factory/factory.h>
  int main(){CanonicalForm p,q; Prem(p,q);return 0;}]] HAVE_FACTORY_PREM)

################################################################

# this is an alternative for AC_FUNC_ALLOCA()
check_function_exists(alloca HAVE_ALLOCA)
check_include_files(alloca.h HAVE_ALLOCA_H)

# TODO: remove dependence on atomic_ops.h

# TODO: do we use these?
## topcom depends on cddlib, but includes setoper.h, rather than cdd/setoper.h, so we do, too
check_include_files(setoper.h	HAVE_CDDLIB)
check_include_files(NTL/version.h	HAVE_NTL)
