#set(CMAKE_VERBOSE_MAKEFILE on)

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
    ${BOOTSTRAP}/usr-host/include
    ${BOOTSTRAP}/Macaulay2/kernel
    ${BOOTSTRAP}/Macaulay2/d
    ${CMAKE_BINARY_DIR}/Macaulay2/e-includes
    ${CMAKE_SOURCE_DIR}/Macaulay2/e
    ${CMAKE_SOURCE_DIR}/include
    ${BOOTSTRAP}/include
    ${BOOTSTRAP}/usr-host/include/cdd
    ${CMAKE_SOURCE_DIR}/submodules/memtailor/src
    ${CMAKE_SOURCE_DIR}/submodules/mathic/src
    ${CMAKE_SOURCE_DIR}/submodules/mathicgb/src
    ${CMAKE_SOURCE_DIR}/submodules/fflas_ffpack/src
    /usr/include/libxml2
    /usr/include/eigen3
    ${CMAKE_SOURCE_DIR}/Macaulay2/d
    ${CMAKE_SOURCE_DIR}/Macaulay2/c
  )
