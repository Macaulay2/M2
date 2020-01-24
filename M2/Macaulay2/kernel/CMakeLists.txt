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
    ${BOOTSTRAP}/Macaulay2/e-includes
    ${SOURCE}/Macaulay2/e
    ${SOURCE}/include
    ${BOOTSTRAP}/include
    ${BOOTSTRAP}/usr-host/include/cdd 
    ${SOURCE}/submodules/memtailor/src 
    ${SOURCE}/submodules/mathic/src 
    ${SOURCE}/submodules/mathicgb/src 
    ${SOURCE}/submodules/fflas_ffpack/src
    /usr/include/libxml2
    /usr/include/eigen3
    ${SOURCE}/Macaulay2/d
    ${SOURCE}/Macaulay2/c
  )

