################################################################
#### Configuring Macaulay2/d/startup.c

## regex macro for C-style character escaping
## escapes / and "
## converts each line to "CONTENT" if with_newline is NO or "CONTENT\n" if it is YES
MACRO (_STARTUP_REGEX input retval with_newline)
  STRING (STRIP "${input}" _output)
  string(      REPLACE "\\" "\\\\"    _output "${_output}") # sed -e 's/\\/\\\\/g'
  string(REGEX REPLACE "\"" "\\\\\""  _output "${_output}") # set -e 's/"/\\"/g'
  string(PREPEND _output "\"")
  string(APPEND  _output "\\n\"")
  # Note: we could use s/(.*)\n/.../g but for now string(REGEX REPLACE) behave differently than `sed -e`
  # See https://gitlab.kitware.com/cmake/cmake/issues/16899
  if("${with_newline}")
    string(REGEX REPLACE "\n" [[\\n"\n"]] _output "${_output}") # sed -e 's/\(.*\)/"\1\\n"/'
  else()
    string(REGEX REPLACE "\n"    [["\n"]] _output "${_output}") # sed -e 's/\(.*\)/"\1"/'
  endif()
  STRING (STRIP "${_output}" ${retval})
ENDMACRO (_STARTUP_REGEX)

## test for regex macro
set(_STARTUP_REGEX_TEST "\nA-B\n\nA\\;\"B\n")
_STARTUP_REGEX([[${_STARTUP_REGEX_TEST}]] _STARTUP_REGEX_TEST YES)
if(NOT "${_STARTUP_REGEX_TEST}" STREQUAL "\"A-B\\n\"\n\"\\n\"\n\"A\\\\;\\\"B\\n\"")
#  message(ERROR "_STARTUP_MACTO_TEST failed: ${_STARTUP_REGEX_TEST}")
endif()

## read startup.m2.in and apply regex to address and content
file(TO_NATIVE_PATH "startup.m2.in"      STARTUP_M2_ADDR)
file(READ           "startup.m2.in"      STARTUP_M2_CONTENT)
_STARTUP_REGEX([[${STARTUP_M2_ADDR}]]    STARTUP_M2_ADDR    NO)
_STARTUP_REGEX([[${STARTUP_M2_CONTENT}]] STARTUP_M2_CONTENT YES)
string(CONFIGURE "${STARTUP_M2_CONTENT}" STARTUP_M2_CONTENT @ONLY)

## read tests from basictests folder and apply regex to address and content
set(TEST_STRINGS "")
set(TEST_STRINGS_TEMPLATE "   {\n   @BASICTEST_M2_ADDR@,\n   @BASICTEST_M2_CONTENT@\n   },\n")

file(GLOB BASICTEST_FILES "basictests/*.m2")
foreach(BASICTEST_FILE ${BASICTEST_FILES})
  file(TO_NATIVE_PATH "${BASICTEST_FILE}"    BASICTEST_M2_ADDR)
  file(READ           "${BASICTEST_FILE}"    BASICTEST_M2_CONTENT)
  _STARTUP_REGEX([[${BASICTEST_M2_ADDR}]]    BASICTEST_M2_ADDR    NO)
  _STARTUP_REGEX([[${BASICTEST_M2_CONTENT}]] BASICTEST_M2_CONTENT YES)
  string(CONFIGURE "${TEST_STRINGS_TEMPLATE}" BASICTEST @ONLY)
  string(APPEND TEST_STRINGS "${BASICTEST}")
endforeach()
