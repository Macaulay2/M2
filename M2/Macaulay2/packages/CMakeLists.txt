#set(CMAKE_VERBOSE_MAKEFILE on)

## Example:
## $ cmake -DRerunExamples=ON [source directory]
## $ VERBOSE=1 make LocalRings
## Note: options are sticky!

set(PACKAGE_DIR ${CMAKE_BINARY_DIR}/Macaulay2/share/Macaulay2)

## eg: Style Macaulay2Doc ...
file(STRINGS "\=distributed-packages" DISTRIBUTED_PACKAGES)
## the default is to install all distributed packages
# TODO: add option to modify the list of packages to install
set(PACKAGES "${DISTRIBUTED_PACKAGES}")

# TODO: use ReinstallPackages or unmark-packages target?
option(ReinstallPackages      "reinstall the packages"               OFF)
option(CheckDocumentation     "check documentation for completeness" ON)
option(IgnoreExampleErrors    "ignore errors in example code"        OFF)
option(RemakeAllDocumentation "remake all documentation"             OFF)
option(RerunExamples          "rerun example output files"           OFF)
# TODO: Options only get ON or OFF, what's a nice way to get optional values from env?
set(errorDepth 3) # TODO: add option to set the error printing depth
set(debugLevel 0) # TODO: add option to set the M2 debugging level

#################################################################################

## eg: Style.m2 Macaulay2Doc.m2 ...
list(TRANSFORM PACKAGES APPEND ".m2" OUTPUT_VARIABLE PACKAGE_OUTPUTS)

## Copy packages and auxiliary files to the destination
add_custom_target(copy-packages DEPENDS ${PACKAGE_OUTPUTS})

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

## eg: install-Style install-Macaulay2Doc ...
list(TRANSFORM PACKAGES PREPEND "install-" OUTPUT_VARIABLE PACKAGE_TARGETS)

## Install packages and make documentation
add_custom_target(install-packages ALL DEPENDS ${PACKAGE_TARGETS})

## Arguments for M2-binary and installPackage
set(ARGS -q --stop --silent -e errorDepth=${errorDepth} -e debugLevel=${debugLevel})
set(INSTALL_ARGS
  Verbose                => $<IF:$<BOOL:${CMAKE_VERBOSE_MAKEFILE}>,true,false>,
  RerunExamples          => $<IF:$<BOOL:${RerunExamples}>,true,false>,
  CheckDocumentation     => $<IF:$<BOOL:${CheckDocumentation}>,true,false>,
  IgnoreExampleErrors    => $<IF:$<BOOL:${IgnoreExampleErrors}>,true,false>,
  RemakeAllDocumentation => $<IF:$<BOOL:${RemakeAllDocumentation}>,true,false>,
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
      M2-binary ${ARGS} -e \"installPackage(\\"${PACKAGE}\\", ${INSTALL_ARGS}) $<SEMICOLON> exit 0\"
    DEPENDS copy-packages
    )
endforeach()

#################################################################################

# TODO: check-<PACKAGE> install-<PACKAGE> uninstall-package-<PACKAGE> all-<PACKAGE>
# TODO: test check big-check
# TODO: EngineTests and other packages under development that should be checked but not installed
