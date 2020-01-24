#set(CMAKE_VERBOSE_MAKEFILE on)

add_library(system OBJECT
  supervisor.cpp
  m2file.cpp
  tests.cpp
  )

target_include_directories(system
  PUBLIC
    ${BOOTSTRAP}/Macaulay2/d
    ${SOURCE}/Macaulay2/system
    ${SOURCE}/Macaulay2/e
    ${SOURCE}/include
    ${BOOTSTRAP}/include
    ${BOOTSTRAP}/usr-host/include
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
