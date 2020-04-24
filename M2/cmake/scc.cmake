###############################################################################
## This script is called from Macaulay2/d/CMakeLists.txt and contains the macro
## for compiling .d and .dd files to .c and .cc files, respectively

if(CMAKE_BUILD_TYPE MATCHES "Rel")
  set(SCCFLAGS -O)
else()
  # TODO: set(SCCFLAGS -debug)
endif()
# set(SCCFLAGS -noline)

# Generate a C or C++ source (.c or .cc) from the D source (.d or .dd, resp.)
#  _source:  D source filename; e.g. interp.dd
#  _prev:    t
#
# NOTE: also sets two variables:
#  ${_prev}_name.sig: the signature file of the generated source
#  ${_prev}_source:   filename of the generated source
#
# For sequential generation, this macro uses _prev to add dependency on ${_prev}_source
MACRO (_SCC_GENERATE _source _prev)
  # Get the name and extension
  string(REGEX MATCH "^([a-zA-Z0-9_]+)\.(d|dd)$" NULL ${_source})
  set(_name ${CMAKE_MATCH_1})
  string(REPLACE "d" "c" _ext ${CMAKE_MATCH_2})

  # TODO: use the {_name}.dep file to check for dependencies
  # TODO: use DEPFILE option, but only works with ninja
  add_custom_command(OUTPUT ${_name}-tmp.${_ext} ${_name}-exports.h ${_name}.sig ${_name}.dep
    COMMENT "Generating ${_name}-tmp.${_ext}"
    COMMAND
      scc1 ${SCCFLAGS} -dep ${CMAKE_CURRENT_SOURCE_DIR}/${_source}
    COMMAND
      mv ${_name}.sig.tmp ${_name}.sig && mv ${_name}.dep.tmp ${_name}.dep
    COMMAND
      scc1 ${SCCFLAGS} ${CMAKE_CURRENT_SOURCE_DIR}/${_source}
    COMMAND
      mv ${_name}.sig.tmp ${_name}.sig && mv ${_name}.dep.tmp ${_name}.dep
    COMMAND
      mv ${_name}-exports.h.tmp ${_name}-exports.h
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${_source}
    DEPENDS ${${_prev}_source}
    DEPFILE ${_name}.dep
    )

  set(${_prev}_name.sig ${_name}.sig)
  set(${_prev}_source   ${_name}-tmp.${_ext})
ENDMACRO (_SCC_GENERATE)
