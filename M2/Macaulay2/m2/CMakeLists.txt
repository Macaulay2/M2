###############################################################################
## This directory contains the rules for building M2-core

set(CORE_DIR ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR}/Core)
set(CORE_TESTS ${M2_DIST_PREFIX}/${M2_INSTALL_DOCDIR}/Core/tests)
set(M2_ARGS -q --silent --stop -e errorDepth=0 --no-preload --no-tvalues)

# List of Core sources
file(STRINGS loadsequence M2LIST)

###############################################################################
## Generate startup.m2, version.m2, and tvalues.m2 and copy the core to usr-dist

## Configure startup.m2
configure_file(startup.m2.in startup.m2 @ONLY)

add_custom_target(M2-core ALL DEPENDS ${CORE_DIR}/tvalues.m2)

add_custom_command(OUTPUT ${CORE_DIR}/tvalues.m2
  COMMENT "Generating Macaulay2/Core/tvalues.m2"
  COMMAND
    M2-binary ${M2_ARGS} ${CMAKE_CURRENT_SOURCE_DIR}/tvalues-make.m2 -e "make \"${CMAKE_SOURCE_DIR}/Macaulay2/d/\"; exit 0"
  WORKING_DIRECTORY ${CORE_DIR}
  VERBATIM)

configure_file(version.m2.in ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR}/Core/version.m2 @ONLY)

foreach(filename IN LISTS M2LIST ITEMS
    ${CMAKE_CURRENT_BINARY_DIR}/startup.m2 setup.m2 loadsequence installedpackages)
  file(COPY ${filename} DESTINATION ${CORE_DIR})
endforeach()

## Copy Core tests from Macaulay2/tests/normal to share/doc/Macaulay2/Core/tests
file(COPY ${CMAKE_SOURCE_DIR}/Macaulay2/tests/normal/ DESTINATION ${CORE_TESTS}
  FILES_MATCHING PATTERN "*.m2")

# TODO: handle Style here.
