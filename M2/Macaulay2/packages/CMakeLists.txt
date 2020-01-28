# TODO: add all packages; these are the bare minimum

set(PACKAGE_DIR ${CMAKE_BINARY_DIR}/Macaulay2/share/Macaulay2)

file(STRINGS "\=distributed-packages" DISTRIBUTED_PACKAGES)

list(TRANSFORM DISTRIBUTED_PACKAGES APPEND ".m2" OUTPUT_VARIABLE PACKAGES)

add_custom_target(packages ALL DEPENDS ${PACKAGES})

foreach(PACKAGE ${DISTRIBUTED_PACKAGES})
  add_custom_command(OUTPUT ${PACKAGE_DIR}/${PACKAGE}.m2
    COMMENT
      "Generating package ${PACKAGE}"
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

# TODO
# Macaulay2/packages/Style/Makefile

# TODO
# cd ${PACKAGE}-temporary && M2-binary  -q --stop --silent -e errorDepth=3 -e debugLevel=0  -e "installPackage(\"${PACKAGE}\", RemakeAllDocumentation => false, IgnoreExampleErrors => false, RerunExamples => false, CheckDocumentation => true, UserMode => false, Verbose => false, InstallPrefix => \"${CMAKE_BINARY_DIR}/Macaulay2/share/\", SeparateExec => true, DebuggingMode => true); exit 0"

# TODO: unmark-packages?
