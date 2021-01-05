###############################################################################
## This directory contains the rules for installing and checking the packages.
## - set options via the cmake command line:
##     RerunExamples=true cmake .
## - targets:
##     cmake --build . --target {install,check,all,uninstall}-{packages,PACKAGE}
## - running tests in parallel:
##     ctest -R check-PACKAGE -j4

include(latex) ## CMake script for creating PDF documentation

## Macro for configuring M2 commands so they can be run as M2 -e ${STR}
# _template_var:  variable holding the string template
# _string_var:    return variable (optional, default: _template_var)
# _one_line:      whether to remove newlines and extra space
MACRO (_M2_STRING_CONFIGURE _template_var) # optional: _string_var _one_line
  if(NOT "${ARGV1}" STREQUAL "")
    set(_string_var ${ARGV1})
  else()
    set(_string_var ${_template_var})
  endif()
  string(CONFIGURE "${${_template_var}}" ${_string_var})
  if("${ARGV2}")
    string(REGEX REPLACE "(\n *| +)" " " ${_string_var} "${${_string_var}}")
  endif()
ENDMACRO ()

#################################################################################
## Package options
## The default is to install all distributed packages
set(PACKAGES "${DISTRIBUTED_PACKAGES}" CACHE INTERNAL "the list of packages to install")
set(PACKAGES_DEVEL       "EngineTests" CACHE INTERNAL "the list of packages to only check")
## Arguments to M2
set(errorDepth                  "3" CACHE STRING "set the error printing depth")
set(debugLevel                  "0" CACHE STRING "set the debugging level")
set(gbTrace                     "0" CACHE STRING "set the Groebner basis trace level")
## Options to installPackage
set(CheckDocumentation       "true" CACHE STRING "check documentation for completeness")
set(IgnoreExampleErrors     "false" CACHE STRING "ignore errors in example code")
set(RemakeAllDocumentation  "false" CACHE STRING "remake all documentation")
set(RerunExamples           "false" CACHE STRING "rerun example outpuat files")
## Arguments to the M2 subprocess that runs individual tests and exampels; see runFile in m2/run.m2
set(ArgumentMode "InvertArgs|SetOutputFile|SetCaptureErr|NoCapture" # |ArgNoThreads for --no-threads
  CACHE INTERNAL "bitmask for ctest check arguments")

# Allow arguments above to be set via environment variables
foreach(_var in ITEMS PACKAGES PACKAGES_DEVEL errorDepth debugLevel gbTrace
    CheckDocumentation IgnoreExampleErrors RemakeAllDocumentation RerunExamples ArgumentMode)
  if(DEFINED ENV{${_var}})
    message("## Set via environment:   ${_var}\t= $ENV{${_var}} (default: ${${_var}})")
    set(${_var} "$ENV{${_var}}")
  endif()
endforeach()

# Make sure PACKAGES is a list
string(REPLACE " " ";" PACKAGES "${PACKAGES}")

#################################################################################
## M2 arguments and commands that will be passed to M2 -e ${STR}
set(M2 ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR}/M2)
set(M2_ARGS -q --stop --silent --no-preload --no-threads -e errorDepth=${errorDepth} -e debugLevel=${debugLevel})
set(M2_INSTALL_TEMPLATE [[installPackage("@package@",
  Verbose                => $<IF:$<BOOL:${VERBOSE}>,true,false>,
  RerunExamples          => ${RerunExamples},
  CheckDocumentation     => ${CheckDocumentation},
  IgnoreExampleErrors    => ${IgnoreExampleErrors},
  RemakeAllDocumentation => ${RemakeAllDocumentation},
  InstallPrefix          => "${M2_DIST_PREFIX}/",
  UserMode               => false,
  SeparateExec           => true,
  DebuggingMode          => true)]])
set(M2_NEED_TEMPLATE    [[needsPackage("@package@", LoadDocumentation=>true, DebuggingMode=>true)]])
set(M2_TEST_TEMPLATE    [[debug Core \; argumentMode = @ArgumentMode@ \; check(@_i@, "@package@")]])
set(M2_INFO_TEMPLATE    [["info-"|"@package@" << @package@#"test number" << close]]) # ignore the color
set(M2_CHECK_TEMPLATE   [[check("@package@", UserMode=>false, Verbose=>$<IF:$<BOOL:${VERBOSE}>,true,false>)]])

# TODO: simplify
set(M2_DOC_TEMPLATE [[installPackage("@package@",
  Verbose                => $<IF:$<BOOL:${VERBOSE}>,true,false>,
  RerunExamples          => ${RerunExamples},
  CheckDocumentation     => ${CheckDocumentation},
  IgnoreExampleErrors    => ${IgnoreExampleErrors},
  RemakeAllDocumentation => ${RemakeAllDocumentation},
  InstallPrefix          => "${M2_DIST_PREFIX}/",
  UserMode               => false,
  SeparateExec           => true,
  DebuggingMode          => true,
  MakeHTML               => false,
  MakeInfo               => false,
  MakePDF                => true)]])

#################################################################################
## Declare all targets: {install,check,all,uninstall}-{packages,PACKAGE}

add_custom_target(list-packages
  COMMENT "Listing available packages"
  COMMAND ${CMAKE_COMMAND} -E echo ${DISTRIBUTED_PACKAGES})

# Prefixes for package targets
set(_target_prefixes "install;info;doc;check;all;uninstall")
# Package targets that should be included in ALL
set(_all_target_list "")

if(BUILD_DOCS)
  list(APPEND _all_target_list "install-packages")
endif()

if(BUILD_TESTING)
  list(APPEND _all_target_list "info-packages")
endif()

foreach(_target IN LISTS _target_prefixes)
  string(TOUPPER "${_target}_TARGETS" _dependencies_list)
  ## eg: ${_target}-Style ${_target}-Macaulay2Doc ...
  list(TRANSFORM PACKAGES PREPEND "${_target}-" OUTPUT_VARIABLE ${_dependencies_list})
  ## Add individual dependencies to the main target
  add_custom_target(${_target}-packages DEPENDS ${${_dependencies_list}})
endforeach()

set_target_properties(${_all_target_list} PROPERTIES EXCLUDE_FROM_ALL OFF)

#################################################################################
## Handling special dependencies

# 1. all packages depend on Macaulay2Doc and Macaulay2Doc depends on Style
# TODO: reduce dependencies, or move Macaulay2Doc and Style to Core
# See https://github.com/Macaulay2/M2/issues/1085
set(Macaulay2Doc_dependencies
  ${M2_DIST_PREFIX}/${M2_INSTALL_LIBDIR}/Macaulay2/FirstPackage/.installed
  ${M2_DIST_PREFIX}/${M2_INSTALL_LIBDIR}/Macaulay2/Style/.installed)
set(_default_dependencies
  ${M2_DIST_PREFIX}/${M2_INSTALL_LIBDIR}/Macaulay2/Macaulay2Doc/.installed)
set(EngineTests_dependencies ${_default_dependencies})

# TODO: automate this process
# 2. FourTiTwo needs 4ti2
# 3. CohomCalg needs cohomcalg
# 4. gfanInterface and StatePolytope need gfan
# 5. Polyhedra needs lrslib
# 6. SemidefiniteProgramming needs csdp
# 7. Normaliz needs normaliz
# 8. Nauty and NautyGraphs need nauty
# 9. Topcom needs topcom
# 10. Polymake and PolymakeInterface need polymake

#################################################################################

foreach(package IN LISTS PACKAGES)
  ## Make a list of package sources to detect changes
  file(GLOB_RECURSE ${package}_sources RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${package}.m2 ${package}/*)

  ## Copy package and auxiliary folder to usr-dist
  file(COPY ${package}.m2 DESTINATION ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR})
  if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${package})
    file(COPY ${package}  DESTINATION ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR})
  endif()

  ## Configuring the command strings
  _M2_STRING_CONFIGURE(M2_INSTALL_TEMPLATE M2_INSTALL_STRING TRUE)
  _M2_STRING_CONFIGURE(M2_CHECK_TEMPLATE   M2_CHECK_STRING)
  _M2_STRING_CONFIGURE(M2_NEED_TEMPLATE    M2_NEED_STRING)
  _M2_STRING_CONFIGURE(M2_INFO_TEMPLATE    M2_INFO_STRING)
  _M2_STRING_CONFIGURE(M2_DOC_TEMPLATE     M2_DOC_STRING TRUE)

  ###############################################################################
  ## Enable USES_TERMINAL only for Macaulay2Doc and EngineTests
  ## Also make all other packages depend on Macaulay2Doc and Style
  if(package MATCHES "(Style|FirstPackage|Macaulay2Doc)" OR package IN_LIST PACKAGES_DEVEL)
    set(USES_TERMINAL USES_TERMINAL)
  else()
    unset(USES_TERMINAL)
    foreach(_dep IN LISTS _default_dependencies)
      if(TARGET ${_dep})
	list(APPEND ${package}_dependencies ${_dep})
      elseif(NOT EXISTS ${_dep})
	list(APPEND ${package}_dependencies ${_dep})
      endif()
    endforeach()
  endif()

  ## TODO: remove this; see https://github.com/Macaulay2/M2/pull/1483
  if(package MATCHES "(ThreadedGB)")
    set(USES_TERMINAL USES_TERMINAL)
  endif()

  ## Custom command for getting package information
  # TODO: currently only number of tests is stored, but we can do more
  add_custom_command(OUTPUT info-${package}
    COMMENT "Getting package information for ${package}"
    COMMAND ${M2} ${M2_ARGS} -e ${M2_NEED_STRING} -e ${M2_INFO_STRING} -e "exit 0"
    VERBATIM
    DEPENDS M2-core ${${package}_sources}
    )

  ## Custom target for installing the package
  add_custom_target(install-${package}
    COMMENT "Installing package ${package}"
    COMMAND ${M2} ${M2_ARGS} -e ${M2_INSTALL_STRING} -e ${M2_INFO_STRING} -e "exit 0"
    COMMAND gzip -nf9 ${M2_DIST_PREFIX}/${M2_INSTALL_INFODIR}/${package}.info
    VERBATIM
    ${USES_TERMINAL}
    DEPENDS M2-core ${${package}_dependencies}
    BYPRODUCTS ${M2_DIST_PREFIX}/${M2_INSTALL_LIBDIR}/Macaulay2/${package}/.installed
    )

  ## Custom target for generating PDF documentation for the package
  # TODO: put the individual package's version in the name
  # NOTE: has to remain in sync with installPDF in m2/book.m2
  set(_bookname manual)
  set(_bookdir  ${M2_DIST_PREFIX}/${M2_INSTALL_DOCDIR}/${package})
  _ADD_LATEX_TARGET(${package} ${_bookdir} ${_bookname})
  add_custom_target(doc-${package} DEPENDS ${_bookdir}/${_bookname}.pdf)

  ## Custom target for uninstalling the package
  add_custom_target(uninstall-${package}
    COMMENT "Uninstalling package ${package}"
    COMMAND
      rm -rf info-${package}
        ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR}/${package}*
        ${M2_DIST_PREFIX}/${M2_INSTALL_LIBDIR}/Macaulay2/${package}*
        ${M2_DIST_PREFIX}/${M2_INSTALL_DOCDIR}/${package}*
        ${M2_DIST_PREFIX}/${M2_INSTALL_INFODIR}/${package}*
    )

  ###############################################################################

  # FIXME: what is if ! grep "CacheExampleOutput => true" @srcdir@/$i.m2 ?
  ## Custom target for checking the package
  add_custom_target(check-${package}
    COMMENT "Checking package ${package}"
    COMMAND ${M2} ${M2_ARGS} -e ${M2_CHECK_STRING} -e ${M2_INFO_STRING} -e "exit 0"
    VERBATIM
    ${USES_TERMINAL}
    DEPENDS M2-core
    )

  # TODO: call a script to this this in build step of info-${package}?
  if(BUILD_TESTING)
    ## Read the number of tests
    if(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/info-${package})
      file(READ ${CMAKE_CURRENT_BINARY_DIR}/info-${package} ${package}-testcount)
    else()
      set(${package}-testcount 0)
    endif()
    ## Add individual tests for package
    foreach(_i RANGE ${${package}-testcount})
      if(${_i} LESS ${${package}-testcount})
	_M2_STRING_CONFIGURE(M2_TEST_TEMPLATE M2_TEST_STRING)
        add_test(NAME "check-${package}-${_i}"
          COMMAND ${M2} ${M2_ARGS} -e ${M2_TEST_STRING} -e "exit 0"
          )
      endif()
    endforeach()
  endif()

  ###############################################################################

  ## Everything bagel
  add_custom_target(all-${package}
    COMMENT "Installing and checking package ${package}"
    COMMAND ${M2} ${M2_ARGS} -e ${M2_INSTALL_STRING} -e ${M2_INFO_STRING} -e ${M2_CHECK_STRING} -e "exit 0"
    VERBATIM
    ${USES_TERMINAL}
    DEPENDS M2-core
    )
endforeach()
