#set(CMAKE_VERBOSE_MAKEFILE on)

## Set options via the cmake command line:
## $ cmake -DM2_RerunExamples=true -DPACKAGES=LocalRings .
## Note that options prefixed with M2_* are sticky. Use -U to revert to default:
## $ cmake -UM2_RerunExamples .

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
set(M2_IgnoreExampleErrors     "false" CACHE STRING "ignore errors in example code")
set(M2_RemakeAllDocumentation  "false" CACHE STRING "remake all documentation")
set(M2_RerunExamples           "false" CACHE STRING "rerun example outpuat files")

#################################################################################

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

## TODO: turn these (at least EngineTests) into CTest targets
## eg: check-Style check-Macaulay2Doc ...
list(TRANSFORM PACKAGES       PREPEND "check-" OUTPUT_VARIABLE CHECK_TARGETS)
## Run tests and check packages
add_custom_target(check-packages DEPENDS ${CHECK_TARGETS})

## eg: uninstall-Style uninstall-Macaulay2Doc ...
list(TRANSFORM PACKAGES PREPEND "uninstall-" OUTPUT_VARIABLE UNINSTALL_TARGETS)
## Uninstall packages
# TODO: have this clear out the folders completely and uninstall-[package] only run uninstallPackage
add_custom_target(uninstall-packages DEPENDS ${UNINSTALL_TARGETS})

## Arguments for M2-binary and installPackage
set(M2_ARGS -q --stop --silent -e errorDepth=${M2_errorDepth} -e debugLevel=${M2_debugLevel})
set(M2_INSTALL_ARGS
  Verbose                => $<IF:$<BOOL:${CMAKE_VERBOSE_MAKEFILE}>,true,false>,
  RerunExamples          => ${M2_RerunExamples},
  CheckDocumentation     => ${M2_CheckDocumentation},
  IgnoreExampleErrors    => ${M2_IgnoreExampleErrors},
  RemakeAllDocumentation => ${M2_RemakeAllDocumentation},
  InstallPrefix          => \\"${M2_DIST_PREFIX}/\\",
  UserMode               => false,
  SeparateExec           => true,
  DebuggingMode          => true
  )

foreach(PACKAGE IN LISTS PACKAGES)
  ## Custom target for installing the package
  # TODO: enable USES_TERMINAL only for Macaulay2Docs?
  add_custom_target(install-${PACKAGE}
    COMMENT
      "Installing package ${PACKAGE}"
    COMMAND
      M2-binary ${M2_ARGS} -e \"installPackage(\\"${PACKAGE}\\", ${M2_INSTALL_ARGS}) $<SEMICOLON> exit 0\"
    DEPENDS M2-core install-Macaulay2Doc
    )

  ## Custom target for uninstalling the package
  add_custom_target(uninstall-${PACKAGE}
    COMMENT
      "Uninstalling package ${PACKAGE}"
    COMMAND
      rm -rf
        ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR}/${PACKAGE}*
        ${M2_DIST_PREFIX}/${M2_INSTALL_LIBDIR}/Macaulay2/${PACKAGE}*
        ${M2_DIST_PREFIX}/${M2_INSTALL_DOCDIR}/${PACKAGE}*
        ${M2_DIST_PREFIX}/${M2_INSTALL_INFODIR}/${PACKAGE}*
    )

  # FIXME: what is if ! grep "CacheExampleOutput => true" @srcdir@/$i.m2 ?
  ## Custom target for checking the package
  add_custom_target(check-${PACKAGE}
    COMMENT
      "Checking package ${PACKAGE}"
    COMMAND
      M2-binary ${M2_ARGS} -e \"needsPackage(\\"${PACKAGE}\\", LoadDocumentation=>true, DebuggingMode=>true) $<SEMICOLON>
      check(${PACKAGE}, UserMode=>false) $<SEMICOLON> exit 0\"
    DEPENDS M2-core
    )

  ## Everything bagel
  add_custom_target(all-${PACKAGE} DEPENDS install-${PACKAGE} check-${PACKAGE})
endforeach()

#################################################################################
# Special dependencies:

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

