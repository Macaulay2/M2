# Give each install command a single owner. File dependencies on commands in
# other directories do not recursively copy their rules into every consumer.
# Explicit target dependencies in _install_targets provide cross-directory order.
add_custom_target(install-${package} DEPENDS ${_install_stamp} ${_install_marker} ${_install_info})
add_custom_command(OUTPUT ${_install_stamp} ${_install_marker} ${_install_info}
  DEPENDS_EXPLICIT_ONLY
  COMMENT "Installing package ${package}"
  COMMAND ${CMAKE_COMMAND} -E rm -f ${_install_stamp}
  COMMAND ${M2} ${M2_ARGS} -e ${M2_INSTALL_STRING} -e ${M2_INFO_STRING} -e "exit 0"
  COMMAND gzip -nf9 ${M2_DIST_PREFIX}/${M2_INSTALL_INFODIR}/${package}.info
  COMMAND ${CMAKE_COMMAND} -E touch ${_install_stamp}
  WORKING_DIRECTORY ${_install_working_directory}
  VERBATIM
  ${USES_TERMINAL}
  DEPENDS M2-core M2-binary ${_install_targets} ${_install_sources} ${${package}_dependencies}
    ${_package_core_sources} ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR}/Core/tvalues.m2
    ${_install_inputs} ${_install_rules_file} ${CMAKE_CURRENT_LIST_FILE}
  )
