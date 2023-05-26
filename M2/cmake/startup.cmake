###############################################################################
## This script is called from Macaulay2/d/CMakeLists.txt and is responsible for
## setting the Layout strings and strings for startup.c. Note that the system's
## layout structure is detected in cmake/configure.cmake and startup.c will be
## configured in Macaulay2/d/CMakeLists.txt

###############################################################################
## regex macro for C-style character escaping
## escapes / and "
## converts each line to "CONTENT" if with_newline is NO or "CONTENT\n" if it is YES
MACRO (_STARTUP_REGEX input retval with_newline)
  STRING (STRIP "${input}" _output)
  string(      REPLACE "\\" "\\\\"    _output "${_output}") # sed -e 's/\\/\\\\/g'
  string(REGEX REPLACE "\"" "\\\\\""  _output "${_output}") # set -e 's/"/\\"/g'
  # Note: we could use s/(.*)\n/.../g but for now string(REGEX REPLACE) behave differently than `sed -e`
  # See https://gitlab.kitware.com/cmake/cmake/issues/16899
  string(PREPEND _output "\"")
  if("${with_newline}")
    string(REGEX REPLACE "\n" [[\\n"\n   "]] _output "${_output}") # sed -e 's/\(.*\)/"\1\\n"/'
    string(APPEND  _output "\\n\"")
  else()
    string(REGEX REPLACE "\n"    [["\n   "]] _output "${_output}") # sed -e 's/\(.*\)/"\1"/'
    string(APPEND  _output "\"")
  endif()
  STRING (STRIP "${_output}" ${retval})
ENDMACRO (_STARTUP_REGEX)

## test for regex macro
set(_STARTUP_REGEX_TEST_YES "\nA-B\n\nA\\;\"B\n")
set(_STARTUP_REGEX_TEST_NO "A-BA\\;\"B.m2")
_STARTUP_REGEX([[${_STARTUP_REGEX_TEST_YES}]] _STARTUP_REGEX_TEST_YES YES)
if(NOT "${_STARTUP_REGEX_TEST_YES}" STREQUAL "\"A-B\\n\"\n   \"\\n\"\n   \"A\\\\;\\\"B\\n\"")
  message(ERROR "_STARTUP_MACTO_TEST_YES failed: ${_STARTUP_REGEX_TEST_YES}")
endif()
_STARTUP_REGEX([[${_STARTUP_REGEX_TEST_NO}]] _STARTUP_REGEX_TEST_NO  NO)
if(NOT "${_STARTUP_REGEX_TEST_NO}"  STREQUAL "\"A-BA\\\\;\\\"B.m2\"")
  message(ERROR "_STARTUP_MACTO_TEST_NO failed: ${_STARTUP_REGEX_TEST_NO}")
endif()

###############################################################################
## Setting the Layout strings for startupString

# These are literal strings which startup.m2 replaces at runtime
set(prefix	[[${prefix}]])
set(exec_prefix	[[${exec_prefix}]])
# cmake/configure.cmake sets these locations for each platform
set(bindir	${exec_prefix}/${CMAKE_INSTALL_BINDIR})
set(libdir	${exec_prefix}/${CMAKE_INSTALL_LIBDIR})
set(libexecdir	${exec_prefix}/${CMAKE_INSTALL_LIBEXECDIR})
set(datadir	${prefix}/${CMAKE_INSTALL_DATAROOTDIR})
set(datarootdir	${prefix}/${CMAKE_INSTALL_DATAROOTDIR})
set(infodir	${prefix}/${CMAKE_INSTALL_INFODIR})
set(mandir	${prefix}/${CMAKE_INSTALL_MANDIR})
set(docdir	${prefix}/${CMAKE_INSTALL_DOCDIR})
# These locations are Macaulay2 conventions
set(libm2dir	${libdir}/Macaulay2)
set(emacsdir	${datarootdir}/emacs/site-lisp/macaulay2)
set(GFTABLESDIR	${datarootdir}/Macaulay2/Core/factory/)
set(packagesdir	${datarootdir}/Macaulay2)
set(programsdir	${libexecdir}/Macaulay2/bin)
set(licensesdir	${libexecdir}/Macaulay2/program-licenses)
set(librariesdir ${libm2dir}/lib)

# TODO: any need for these?
# SHAREDSTATEDIR: modifiable architecture-independent data (com)
# LOCALSTATEDIR:  modifiable single-machine data (var)
# RUNSTATEDIR:    run-time variable data (LOCALSTATEDIR/run)

###############################################################################
## Set variables for configuring Macaulay2/d/startup.c

## read startup.m2.in and apply regex to address and content
file(GLOB STARTUP_M2_ADDR "Macaulay2/m2/startup.m2.in")
file(READ "Macaulay2/m2/startup.m2.in"   STARTUP_M2_CONTENT)
_STARTUP_REGEX([[${STARTUP_M2_ADDR}]]    STARTUP_M2_ADDR    NO)
_STARTUP_REGEX([[${STARTUP_M2_CONTENT}]] STARTUP_M2_CONTENT YES)
string(CONFIGURE "${STARTUP_M2_CONTENT}" STARTUP_M2_CONTENT @ONLY)

## read tests from basictests folder and apply regex to address and content
set(TEST_STRINGS "")
set(TEST_STRINGS_TEMPLATE "   {\n   @BASICTEST_M2_ADDR@,\n   @BASICTEST_M2_CONTENT@\n   },\n")

file(GLOB BASICTEST_FILES "Macaulay2/m2/basictests/*.m2")
foreach(BASICTEST_FILE ${BASICTEST_FILES})
  file(TO_NATIVE_PATH "${BASICTEST_FILE}"    BASICTEST_M2_ADDR)
  file(READ           "${BASICTEST_FILE}"    BASICTEST_M2_CONTENT)
  _STARTUP_REGEX([[${BASICTEST_M2_ADDR}]]    BASICTEST_M2_ADDR    NO)
  _STARTUP_REGEX([[${BASICTEST_M2_CONTENT}]] BASICTEST_M2_CONTENT YES)
  string(CONFIGURE "${TEST_STRINGS_TEMPLATE}" BASICTEST @ONLY)
  string(APPEND TEST_STRINGS "${BASICTEST}")
endforeach()

## ensure that cmake reruns when any of the sources change
set_property(DIRECTORY APPEND PROPERTY
  CMAKE_CONFIGURE_DEPENDS "${STARTUP_M2_ADDR};${BASICTEST_FILES}")
