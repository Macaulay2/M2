#set(CMAKE_VERBOSE_MAKEFILE on)

add_library(system OBJECT
  supervisor.cpp
  m2file.cpp
  tests.cpp
  )

target_include_directories(system
  PUBLIC
    .
    ${BOOTSTRAP}/include
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_SOURCE_DIR}/Macaulay2/c
    ${CMAKE_SOURCE_DIR}/Macaulay2/e
    ${CMAKE_BINARY_DIR}/Macaulay2/d
    ${LIBXML2_INCLUDE_DIR}
  )
