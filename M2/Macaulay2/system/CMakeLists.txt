#set(CMAKE_VERBOSE_MAKEFILE on)

add_library(system OBJECT
  supervisor.cpp
  m2file.cpp
  tests.cpp
  )

target_include_directories(system
  PUBLIC
    ${BOOTSTRAP}/Macaulay2/d
    ${CMAKE_SOURCE_DIR}/Macaulay2/system
    ${CMAKE_SOURCE_DIR}/Macaulay2/e
    ${CMAKE_SOURCE_DIR}/include
    ${BOOTSTRAP}/include
    ${BOOTSTRAP}/usr-host/include
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
