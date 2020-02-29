set(CMAKE_VERBOSE_MAKEFILE OFF)

add_library(kernel OBJECT
  bibasis/bibasis.cpp
  bibasis/allocator.cpp
  bibasis/launcher.cpp
  bibasis/monom.cpp
  bibasis/monomDL.cpp
  bibasis/monomDRL.cpp
  bibasis/monomLex.cpp
  bibasis/settings-manager.cpp
  )

target_include_directories(kernel
  PUBLIC
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_BINARY_DIR}/include
    ${CMAKE_BINARY_DIR}/Macaulay2/e-includes
    # TODO: why does putting c before e cause errors?
    ${CMAKE_SOURCE_DIR}/Macaulay2/e
    ${CMAKE_SOURCE_DIR}/Macaulay2/c # needs scc-core.h
    ${CMAKE_BINARY_DIR}/Macaulay2/d
  )

add_dependencies(kernel interpreter) # needs pthread-exports.h
