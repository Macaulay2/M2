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
    /usr/local/include
    ${BOOTSTRAP}/include
    ${BOOTSTRAP}/usr-host/include
    ${CMAKE_BINARY_DIR}/Macaulay2/d
    ${CMAKE_BINARY_DIR}/Macaulay2/e-includes
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_SOURCE_DIR}/Macaulay2/e
    ${CMAKE_SOURCE_DIR}/Macaulay2/c
  )
