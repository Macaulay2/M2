#set(CMAKE_VERBOSE_MAKEFILE on)

## Set options via the cmake command line:
## $ cmake -DM2_RerunExamples=true -DPACKAGES=LocalRings .
## Note that options prefixed with M2_* are sticky. Use -U to revert to default:
## $ cmake -UM2_RerunExamples .

set(PACKAGE_DIR ${CMAKE_BINARY_DIR}/Macaulay2/share/Macaulay2)

## eg: Style Macaulay2Doc ...
file(STRINGS "\=distributed-packages" DISTRIBUTED_PACKAGES)

## Package options
## The default is to install all distributed packages
set(PACKAGES "${DISTRIBUTED_PACKAGES}" CACHE INTERNAL "the list of packages to install")
set(M2_ReinstallPackages       "false" CACHE STRING   "reinstall the packages")
# TODO: use ReinstallPackages or unmark-packages target?
## Arguments to M2
set(M2_errorDepth                  "3" CACHE STRING "set the error printing depth")
set(M2_debugLevel                  "0" CACHE STRING "set the debugging level")
## Options to installPackage
set(M2_CheckDocumentation       "true" CACHE STRING "check documentation for completeness")
set(M2_IgnoreExampleErrors     "false" CACHE STRING "ignore errors in example code")
set(M2_RemakeAllDocumentation  "false" CACHE STRING "remake all documentation")
set(M2_RerunExamples           "false" CACHE STRING "rerun example outpuat files")

# errors
# set(PACKAGES
#  Macaulay2Doc CoincidentRootLoci Bertini QuillenSuslin MonodromySolver
#  StatePolytope PHCpack MultiplierIdeals NumericalAlgebraicGeometry )

#################################################################################

## eg: Style.m2 Macaulay2Doc.m2 ...
list(TRANSFORM PACKAGES APPEND ".m2" OUTPUT_VARIABLE PACKAGE_OUTPUTS)

## Copy packages and auxiliary files to the destination
add_custom_target(copy-packages ALL DEPENDS ${PACKAGE_OUTPUTS})

foreach(PACKAGE ${PACKAGES})
  add_custom_command(OUTPUT ${PACKAGE_DIR}/${PACKAGE}.m2
    COMMENT
      "Copying package ${PACKAGE}"
    COMMAND
      ${CMAKE_COMMAND} -E copy_if_different ${PACKAGE}.m2 ${PACKAGE_DIR}
    COMMAND
      if [ -d "${PACKAGE}" ] \; then
        ${CMAKE_COMMAND} -E copy_directory  ${PACKAGE}    ${PACKAGE_DIR}/${PACKAGE} \;
      fi
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS m2
    )
endforeach()

#################################################################################

# TODO: how to handle EngineTests and other packages under development that
# should be checked but not installed

## eg: install-Style install-Macaulay2Doc ...
list(TRANSFORM PACKAGES PREPEND "install-" OUTPUT_VARIABLE INSTALL_TARGETS)
## Install packages and make documentation
add_custom_target(install-packages DEPENDS ${INSTALL_TARGETS})

## eg: check-Style check-Macaulay2Doc ...
list(TRANSFORM PACKAGES PREPEND "check-" OUTPUT_VARIABLE CHECK_TARGETS)
## Run tests and check packages
add_custom_target(check-packages DEPENDS ${CHECK_TARGETS})

## eg: uninstall-Style uninstall-Macaulay2Doc ...
list(TRANSFORM PACKAGES PREPEND "uninstall-" OUTPUT_VARIABLE UNINSTALL_TARGETS)
## Uninstall packages
add_custom_target(uninstall-packages DEPENDS ${UNINSTALL_TARGETS})


## Arguments for M2-binary and installPackage
set(M2_ARGS -q --stop --silent -e errorDepth=${M2_errorDepth} -e debugLevel=${M2_debugLevel})
set(M2_INSTALL_ARGS
  Verbose                => $<IF:$<BOOL:${CMAKE_VERBOSE_MAKEFILE}>,true,false>,
  RerunExamples          => ${M2_RerunExamples},
  CheckDocumentation     => ${M2_CheckDocumentation},
  IgnoreExampleErrors    => ${M2_IgnoreExampleErrors},
  RemakeAllDocumentation => ${M2_RemakeAllDocumentation},
  InstallPrefix          => \\"${DIST_DIR}/\\",
  UserMode               => false,
  SeparateExec           => true,
  DebuggingMode          => true
  )

foreach(PACKAGE ${PACKAGES})
  ## Custom target for installing the package
  add_custom_target(install-${PACKAGE}
    COMMENT
      "Installing package ${PACKAGE}"
    COMMAND
      M2-binary ${M2_ARGS} -e \"installPackage(\\"${PACKAGE}\\", ${M2_INSTALL_ARGS}) $<SEMICOLON> exit 0\"
    DEPENDS copy-packages
    )

  # TODO: use cmake -E remove?
  ## Custom target for uninstalling the package
  add_custom_target(uninstall-${PACKAGE}
    COMMENT
      "Uninstalling package ${PACKAGE}"
#    COMMAND
#      rm -rf
#        @pre_libm2dir@/$1
#        @pre_infodir@/$1.info
#        @pre_infodir@/$1.info.errors
#        @pre_packagesdir@/$1
#        @pre_docdir@/$1
    )

  # FIXME: what is if ! grep "CacheExampleOutput => true" @srcdir@/$i.m2 ?
  ## Custom target for checking the package
  add_custom_target(check-${PACKAGE}
    COMMENT
      "Checking package ${PACKAGE}"
    COMMAND
      M2-binary ${M2_ARGS} -e \"needsPackage(\\"${PACKAGE}\\", LoadDocumentation=>true, DebuggingMode=>true) $<SEMICOLON>
      check(${PACKAGE}, UserMode=>false) $<SEMICOLON> exit 0\"
    DEPENDS install-${PACKAGE}
    )

  ## Everything bagel
  add_custom_target(all-${PACKAGE} DEPENDS install-${PACKAGE} check-${PACKAGE})
endforeach()

#################################################################################

# TODO: test? big-check?
