set(CMAKE_VERBOSE_MAKEFILE OFF)

add_library(M2-kernel OBJECT
  bibasis/bibasis.cpp
  bibasis/allocator.cpp
  bibasis/launcher.cpp
  bibasis/monom.cpp
  bibasis/monomDL.cpp
  bibasis/monomDRL.cpp
  bibasis/monomLex.cpp
  bibasis/settings-manager.cpp
  )

add_dependencies(M2-kernel M2-interpreter) # needs pthread-exports.h

target_include_directories(M2-kernel
  PUBLIC
    .
    ${CMAKE_BINARY_DIR}/Macaulay2/d # needs engine-exports.h
    ${CMAKE_SOURCE_DIR}/Macaulay2/e # needs matrix.hpp
    ${CMAKE_SOURCE_DIR}/Macaulay2/c # needs scc-core.h
    ${CMAKE_BINARY_DIR}/Macaulay2/e-includes
  )

# common warning flags
target_compile_options(M2-kernel PRIVATE
  -Wall
  -Wcast-qual
  -Wextra
  -Wfatal-errors
  -Wno-unused
  -Wno-unused-parameter
  # temporary flags
  -Wno-ignored-qualifiers
  )
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  # using Clang or AppleClang
  target_compile_options(M2-kernel PRIVATE
    -Wno-mismatched-tags # needed currently, but if we clean up includes, should not be needed?
    -Wno-unknown-pragmas
    )
endif()
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  # using GCC
  target_compile_options(M2-kernel PRIVATE
    #-pedantic-errors
    #-Wno-extra-semi
    -Wno-error=pragmas
    #-Wno-unused-local-typedefs # one warning, in mathic.h from x-aring.cpp
    )
endif()
# add other compilers (eg: Intel, MSVC) here
