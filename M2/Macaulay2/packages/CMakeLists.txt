###############################################################################
## This directory contains the rules for installing and checking the packages.
## - set options via the cmake command line:
##     cmake -DM2_RerunExamples=true .
## - targets:
##     cmake --build . --target {install,check,all,uninstall}-{packages,PACKAGE}
## - running tests in parallel:
##     ctest -R check-PACKAGE -j4

#################################################################################
## Package options
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
set(M2_IgnoreExampleErrors      "true" CACHE STRING "ignore errors in example code")
set(M2_RemakeAllDocumentation  "false" CACHE STRING "remake all documentation")
set(M2_RerunExamples           "false" CACHE STRING "rerun example outpuat files")

## Arguments for M2 and installPackage
set(M2 ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR}/M2)
set(M2_ARGS -q --stop --silent -e errorDepth=${M2_errorDepth} -e debugLevel=${M2_debugLevel})
set(M2_INSTALL_ARGS
  Verbose                => $<IF:$<BOOL:${VERBOSE}>,true,false>,
  RerunExamples          => ${M2_RerunExamples},
  CheckDocumentation     => ${M2_CheckDocumentation},
  IgnoreExampleErrors    => ${M2_IgnoreExampleErrors},
  RemakeAllDocumentation => ${M2_RemakeAllDocumentation},
  InstallPrefix          => \\"${M2_DIST_PREFIX}/\\",
  UserMode               => false,
  SeparateExec           => true,
  DebuggingMode          => true
  )

#################################################################################
## Make a list of package files and copy them to usr-dist

## eg: Style.m2 Macaulay2Doc.m2 ...
string(REPLACE " " ";" PACKAGES "${PACKAGES}")
list(TRANSFORM PACKAGES APPEND ".m2" OUTPUT_VARIABLE PACKAGE_OUTPUTS)

## Copy packages and auxiliary files to the destination
foreach(PACKAGE IN LISTS PACKAGES)
  file(COPY ${PACKAGE}.m2 DESTINATION ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR})
  if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${PACKAGE})
    file(COPY ${PACKAGE}  DESTINATION ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR})
  endif()
endforeach()

#################################################################################
## Declare all targets: {install,check,all,uninstall}-{packages,PACKAGE}

add_custom_target(list-packages
  COMMENT "Listing available packages"
  COMMAND ${CMAKE_COMMAND} -E echo ${PACKAGES})


## eg: install-Style install-Macaulay2Doc ...
list(TRANSFORM PACKAGES PREPEND "install-" OUTPUT_VARIABLE INSTALL_TARGETS)
## Remove install-EngineTests from install-packages
list(TRANSFORM PACKAGES_DEVEL PREPEND "install-" OUTPUT_VARIABLE PHONY_TARGETS)
list(REMOVE_ITEM INSTALL_TARGETS ${PHONY_TARGETS})
## Install packages and make documentation
add_custom_target(install-packages DEPENDS ${INSTALL_TARGETS})


## eg: check-Style check-Macaulay2Doc ...
list(TRANSFORM PACKAGES       PREPEND "check-" OUTPUT_VARIABLE CHECK_TARGETS)
## Run tests and check packages
add_custom_target(check-packages DEPENDS ${CHECK_TARGETS})


## eg: all-Style all-Macaulay2Doc ...
list(TRANSFORM PACKAGES PREPEND "all-" OUTPUT_VARIABLE ALL_TARGETS)
## Remove all-EngineTests from all-packages
list(TRANSFORM PACKAGES_DEVEL PREPEND "all-" OUTPUT_VARIABLE PHONY_TARGETS)
list(REMOVE_ITEM ALL_TARGETS ${PHONY_TARGETS})
## Install and check packages
add_custom_target(all-packages DEPENDS ${ALL_TARGETS})


## eg: uninstall-Style uninstall-Macaulay2Doc ...
list(TRANSFORM PACKAGES PREPEND "uninstall-" OUTPUT_VARIABLE UNINSTALL_TARGETS)
## Uninstall packages
# TODO: have this clear out the folders completely and uninstall-[package] only run uninstallPackage
add_custom_target(uninstall-packages DEPENDS ${UNINSTALL_TARGETS})

#################################################################################

foreach(package IN LISTS PACKAGES)
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

  ###############################################################################
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

  # Various command snippets that can be passed to M2 -e "..."
  set(M2_INSTALL_STRING installPackage(\\"${package}\\", ${M2_INSTALL_ARGS}) $<SEMICOLON>)
  set(M2_COUNT_STRING   \\\"${package}-testcount\\\"<<${package}\#\\\"test number\\\"<<close $<SEMICOLON>)
  set(M2_CHECK_STRING   check(${package}, UserMode=>false) $<SEMICOLON>)
  set(M2_NEED_STRING    needsPackage(\\"${package}\\", LoadDocumentation=>true, DebuggingMode=>true) $<SEMICOLON>)
  # M2_TEST_STRING is defined further down, as it includes an index

  ## Custom target for installing the package
  # also stores the number of tests in a file
  add_custom_target(install-${package}
    COMMENT "Installing package ${package}"
    COMMAND ${M2} ${M2_ARGS} -e \"${M2_INSTALL_STRING} ${M2_COUNT_STRING} exit 0\"
    ${USES_TERMINAL}
    BYPRODUCTS ${package}-testcount
    DEPENDS M2-core ${PACKAGE_DEPENDENCIES}
    )

  ## Custom target for uninstalling the package
  add_custom_target(uninstall-${package}
    COMMENT "Uninstalling package ${package}"
    COMMAND
      rm -rf ${package}-testcount
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
    COMMAND ${M2} ${M2_ARGS} -e \"${M2_NEED_STRING} ${M2_CHECK_STRING} exit 0\"
    ${USES_TERMINAL}
    DEPENDS M2-core
    )

  if(BUILD_TESTING)
    ## Read the number of tests
    if(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${package}-testcount)
      file(READ ${CMAKE_CURRENT_BINARY_DIR}/${package}-testcount ${package}-testcount)
    else()
      set(${package}-testcount 0)
    endif()
    ## Add individual tests for package
    foreach(_i RANGE ${${package}-testcount})
      if(${_i} LESS ${${package}-testcount})
	set(M2_TEST_STRING "check_${_i}(\"${package}\",UserMode=>false)$<SEMICOLON>exit(0)")
        add_test(NAME "check-${package}-${_i}"
          COMMAND ${M2} ${M2_ARGS} -e ${M2_TEST_STRING}
          )
      endif()
    endforeach()
  endif()

  ###############################################################################

  ## Everything bagel
  add_custom_target(all-${package}
    COMMENT "Installing and checking package ${package}"
    COMMAND ${M2} ${M2_ARGS} -e \"${M2_INSTALL_STRING} ${M2_COUNT_STRING} ${M2_CHECK_STRING} exit 0\"
    ${USES_TERMINAL}
    DEPENDS M2-core
    )
endforeach()
