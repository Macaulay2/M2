###############################################################################
## This directory contains the rules for installing and checking the packages.
## - set options via the cmake command line:
##     cmake -DM2_RerunExamples=true .
## - targets:
##     cmake --build . --target {install,check,all,uninstall}-{packages,PACKAGE}
## - running tests in parallel:
##     ctest -R check-PACKAGE -j4

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
# TODO: use if(DEFINED ENV{var}) for these variables
## The default is to install all distributed packages
set(PACKAGES "${DISTRIBUTED_PACKAGES}" CACHE INTERNAL "the list of packages to install")
set(PACKAGES_DEVEL       "EngineTests" CACHE INTERNAL "the list of packages to only check")
set(M2_ReinstallPackages       "false" CACHE STRING   "reinstall the packages")
# TODO: use ReinstallPackages or unmark-packages target?
## Arguments to M2
set(M2_errorDepth                  "3" CACHE STRING "set the error printing depth")
set(M2_debugLevel                  "0" CACHE STRING "set the debugging level")
set(M2_gbTrace                     "0" CACHE STRING "set the Groebner basis trace level")
## Options to installPackage
set(M2_CheckDocumentation       "true" CACHE STRING "check documentation for completeness")
set(M2_IgnoreExampleErrors     "false" CACHE STRING "ignore errors in example code")
set(M2_RemakeAllDocumentation  "false" CACHE STRING "remake all documentation")
set(M2_RerunExamples           "false" CACHE STRING "rerun example outpuat files")
set(M2_UserMode "(1<<31)+(1<<32)-(1<<64)" # -(1<<8)" # for --no-threads
  CACHE INTERNAL "bitmask for ctest check arguments")

# Make sure PACKAGES is a list
string(REPLACE " " ";" PACKAGES "${PACKAGES}")

#################################################################################
## M2 arguments and commands that will be passed to M2 -e ${STR}
set(M2 ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR}/M2)
set(M2_ARGS -q --stop --silent --no-threads -e errorDepth=${M2_errorDepth} -e debugLevel=${M2_debugLevel})
set(M2_INSTALL_TEMPLATE [[installPackage("@package@",
  Verbose                => $<IF:$<BOOL:${VERBOSE}>,true,false>,
  RerunExamples          => ${M2_RerunExamples},
  CheckDocumentation     => ${M2_CheckDocumentation},
  IgnoreExampleErrors    => ${M2_IgnoreExampleErrors},
  RemakeAllDocumentation => ${M2_RemakeAllDocumentation},
  InstallPrefix          => "${M2_DIST_PREFIX}/",
  UserMode               => false,
  SeparateExec           => true,
  DebuggingMode          => true)]])
set(M2_NEED_TEMPLATE    [[needsPackage("@package@", LoadDocumentation=>true, DebuggingMode=>true)]])
set(M2_TEST_TEMPLATE    [[check(@_i@, "@package@", UserMode=>(@M2_UserMode@))]]) # see runFile in m2/html.m2
set(M2_INFO_TEMPLATE    [["info-"|"@package@" << @package@#"test number" << close]]) # ignore the color
set(M2_CHECK_TEMPLATE   [[check("@package@", UserMode=>false)]])

#################################################################################
## Declare all targets: {install,check,all,uninstall}-{packages,PACKAGE}

add_custom_target(list-packages
  COMMENT "Listing available packages"
  COMMAND ${CMAKE_COMMAND} -E echo ${PACKAGES})

# Prefixes for package targets
set(_target_prefixes "install;check;all;uninstall")
# Package targets that should be included in ALL
# TODO: add install-packages once it is fast enough
set(_all_target_list "")

if(BUILD_TESTING)
  list(APPEND _target_prefixes "info")
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

  ###############################################################################
  ## Enable USES_TERMINAL only for Macaulay2Doc and EngineTests
  ## Also make all other packages depend on Macaulay2Doc and Style
  # TODO: remove dependencies, or move Macaulay2Doc and Style to Core
  if(package MATCHES "Macaulay2Doc" OR package IN_LIST PACKAGES_DEVEL)
    set(USES_TERMINAL USES_TERMINAL)
    unset(PACKAGE_DEPENDENCIES)
  else()
    unset(USES_TERMINAL)
    set(PACKAGE_DEPENDENCIES install-Macaulay2Doc install-Style)
  endif()

  # TODO: Special dependencies:
  # 1. all packages seem to depend on Macaulay2Doc: https://github.com/Macaulay2/M2/issues/1085
  # 2. FourTiTwo needs 4ti2 which needs glpk
  # 3. cohomcalg
  # 4. gfanInterface and StatePolytope need gfan which needs cddlib
  # 5. lrslib
  # 6. csdp
  # 7. Normaliz needs normaliz which needs libgmp, libgmpxx, boost
  # 8. nauty
  # 9. topcom
  # 10. polymake

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
    VERBATIM
    ${USES_TERMINAL}
    DEPENDS M2-core ${PACKAGE_DEPENDENCIES}
    )

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
    COMMAND ${M2} ${M2_ARGS} -e ${M2_INSTALL_STRING} -e ${M2_COUNT_STRING} -e ${M2_CHECK_STRING} -e "exit 0"
    VERBATIM
    ${USES_TERMINAL}
    DEPENDS M2-core
    )
endforeach()
