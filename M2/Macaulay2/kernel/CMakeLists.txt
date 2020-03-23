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

# common warning flags
target_compile_options(kernel PRIVATE
  -Wall
  -Wcast-qual
  -Wextra
  -Wfatal-errors
  -Wno-parentheses
  -Wno-shadow
  -Wno-sign-compare
  -Wno-sign-conversion
  -Wno-uninitialized
  -Wno-unused
  -Wno-unused-parameter
  # temporary flags
  #-Wno-deprecated-copy
  #-Wno-unknown-pragmas
  )
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  # using Clang or AppleClang
  target_compile_options(kernel PRIVATE
    -Wno-mismatched-tags # needed currently, but if we clean up includes, should not be needed?
    -Wno-unknown-pragmas
    )
endif()
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  # using GCC
  target_compile_options(kernel PRIVATE
    #-pedantic-errors
    #-Wno-extra-semi
    -Wno-error=pragmas
    #-Wno-unused-local-typedefs # one warning, in mathic.h from x-aring.cpp
    )
endif()
# add other compilers (eg: Intel, MSVC) here
