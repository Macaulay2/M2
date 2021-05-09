# Guide: https://devblogs.microsoft.com/cppblog/clear-functional-c-documentation-with-sphinx-breathe-doxygen-cmake/
# Requirement:
#   pip install breathe

# Sphinx and Doxygen options
set(SPHINX_OPTIONS	"" CACHE STRING "...")
set(DOXYGEN_OPTIONS	"" CACHE STRING "...")
set(CALL_GRAPH_SETTING	NO CACHE BOOL "...")
set(HAVE_DOT_SETTING	NO CACHE BOOL "...")

set(DOXYFILE_IN ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in)
set(DOXYFILE_OUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)
set(DOXYGEN_INPUT_DIR ${CMAKE_SOURCE_DIR}/Macaulay2/e)
set(DOXYGEN_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/doxygen)
set(DOXYGEN_INDEX_FILE ${DOXYGEN_OUTPUT_DIR}/xml/index.xml)

set(SPHINX_INPUT_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(SPHINX_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR})
set(SPHINX_INDEX_FILE ${SPHINX_OUTPUT_DIR}/index.html)

file(GLOB_RECURSE DOXYGEN_SOURCES ${DOXYGEN_INPUT_DIR}/*.h ${DOXYGEN_INPUT_DIR}/*.hpp)
file(GLOB_RECURSE SPHINX_SOURCES  ${SPHINX_INPUT_DIR}/*.rst)

file(MAKE_DIRECTORY ${DOXYGEN_OUTPUT_DIR})
configure_file(Doxyfile.in Doxyfile @ONLY)

###############################################################################

# TODO: use doxygen_add_docs instead?
# Only regenerate Doxygen when the Doxyfile or public headers change
add_custom_command(OUTPUT ${DOXYGEN_INDEX_FILE}
  COMMENT "Generating Macaulay2 engine documentation"
  COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYFILE_OUT} ${DOXYGEN_OPTIONS}
  MAIN_DEPENDENCY ${DOXYFILE_OUT}
  DEPENDS ${DOXYGEN_SOURCES}
  USES_TERMINAL
  )

add_custom_target(M2-docs-doxygen DEPENDS ${DOXYGEN_INDEX_FILE})

###############################################################################

# alternative typs: singlehtml, pickle?, latexpdf, man, info, changes?, linkcheck, doctest?, coverage?, etc.
add_custom_command(OUTPUT ${SPHINX_INDEX_FILE}
  COMMENT "Generating Macaulay2 internal documentation"
  COMMAND ${SPHINX_EXECUTABLE} -b html -Dbreathe_projects.Macaulay2=${DOXYGEN_OUTPUT_DIR}/xml
    ${SPHINX_INPUT_DIR} ${SPHINX_OUTPUT_DIR} ${SPHINX_OPTIONS}
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  MAIN_DEPENDENCY ${SPHINX_INPUT_DIR}/conf.py
  DEPENDS ${SPHINX_SOURCES} ${DOXYGEN_INDEX_FILE}
  USES_TERMINAL
  )

add_custom_target(M2-docs-sphinx DEPENDS ${SPHINX_INDEX_FILE})
