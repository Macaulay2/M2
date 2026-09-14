###############################################################################
## This script is called from Macaulay2/d/CMakeLists.txt and contains the macro
## for translating .d and .dd files to .c and .cc files, respectively.

# Tip: -noline remove the line macros to make reading the output easier
set(SCCFLAGS "-O" CACHE STRING "Flags for the Safe C Compiler")

# Generate a C or C++ source (.c or .cc) from the D source (.d or .dd, resp.)
#  _source:  D source filename; e.g. interp.dd
#  _prev:    see the notes below
#
# NOTE: also sets two variables:
#  ${_prev}_name.sig: the signature file of the generated source
#  ${_prev}_source:   filename of the generated source
#
# For sequential generation, this macro uses _prev to add dependency on ${_prev}_source
MACRO (_SCC_TRANSLATE _source _prev)
  # Get the name and extension
  string(REGEX MATCH "^([a-zA-Z0-9_]+)\.(d|dd)$" NULL ${_source})
  set(_name ${CMAKE_MATCH_1})
  string(REPLACE "d" "c" _ext ${CMAKE_MATCH_2})

  # TODO: disabled until scc1 supports multiple threads
  # Only Ninja supports reading .dep files
  if(FALSE AND CMAKE_GENERATOR STREQUAL "Ninja" AND
      EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${_name}.dep)
    set(_dependency DEPFILE ${_name}.dep)
  else()
    set(_dependency ${${_prev}_source})
  endif()

  # the compiler runs from the top of the build tree, so the paths that scc1
  # records in its "# line" directives have to be relative to that
  file(RELATIVE_PATH _rel_source
    ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/${_source})
  file(RELATIVE_PATH _rel_bindir ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR})

  # scc1 names its output after the basename of its input, so it lands in the
  # working directory and has to be moved back here
  add_custom_command(OUTPUT ${_name}-tmp.${_ext} ${_name}-exports.h ${_name}.sig ${_name}.dep
    COMMENT "Generating ${_name}-tmp.${_ext}"
    COMMAND
      scc1 ${SCCFLAGS} -I${_rel_bindir} ${_rel_source}
    COMMAND
      mv ${_name}-tmp.${_ext} ${CMAKE_CURRENT_BINARY_DIR}/${_name}-tmp.${_ext} &&
      mv ${_name}.sig.tmp ${CMAKE_CURRENT_BINARY_DIR}/${_name}.sig &&
      mv ${_name}.dep.tmp ${CMAKE_CURRENT_BINARY_DIR}/${_name}.dep &&
      mv ${_name}-exports.h.tmp ${CMAKE_CURRENT_BINARY_DIR}/${_name}-exports.h
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${_source}
    DEPENDS scc1 ${_dependency})

  set(${_prev}_name.sig ${_name}.sig)
  set(${_prev}_source   ${_name}-tmp.${_ext})
ENDMACRO (_SCC_TRANSLATE)
