# - This module looks for Sphinx
# Find the Sphinx documentation generator
#
# This modules defines
# SPHINX_EXECUTABLE
# SPHINX_FOUND
find_program(SPHINX_EXECUTABLE
  NAMES sphinx-build
  PATHS
    /usr/bin
    /usr/local/bin
    /opt/local/bin
  DOC "Sphinx documentation generator"
)

if( NOT SPHINX_EXECUTABLE )
  set(_Python_VERSIONS
    3.8 3.7 3.6 3 2.7 2.6 2
  )
  foreach( _version ${_Python_VERSIONS} )
    set( _sphinx_NAMES sphinx-build-${_version} )
    find_program( SPHINX_EXECUTABLE
      NAMES ${_sphinx_NAMES}
      PATHS
        /usr/bin
        /usr/local/bin
        /opt/loca/bin
      DOC "Sphinx documentation generator"
    )
  endforeach()
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Sphinx DEFAULT_MSG SPHINX_EXECUTABLE)
mark_as_advanced(SPHINX_EXECUTABLE)

function(Sphinx_add_target target_name builder conf source destination)

  add_custom_target(${target_name} ALL
    COMMAND ${SPHINX_EXECUTABLE} -b ${builder}
    -c ${conf}
    ${source}
    ${destination}
    COMMENT "Generating sphinx documentation: ${builder}"
  )

  set_property(DIRECTORY
    APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${destination}
  )

endfunction()
