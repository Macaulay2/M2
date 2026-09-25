# Keep CI's library test targets when using installed libraries. Copy just the
# tests so their test/ includes work without placing bundled library headers
# ahead of the installed target's headers in the include search path.
if(NOT BUILD_TESTING)
  return()
endif()
# Keep these source lists in sync with the pinned submodule test targets.
set(_memtailor_tests ArenaTest BufferPoolTest MemoryBlocksTest testMain)
set(_mathic_tests BitTriangle DivFinder HashTable PairQueue testMain)
set(_mathicgb_tests F4MatrixBuilder F4MatrixReducer MathicIO MonoMonoid PrimeField
  QuadMatrixBuilder Range Scanner SparseMatrix gb-test ideals mathicgb poly-test testMain)
foreach(lib IN ITEMS memtailor mathic mathicgb)
  if(NOT _${lib}_external)
    continue()
  endif()
  set(test_root "${CMAKE_CURRENT_BINARY_DIR}/external-${lib}-tests")
  file(GLOB_RECURSE test_files CONFIGURE_DEPENDS
    RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}/${lib}"
    "${lib}/test/*.cpp" "${lib}/test/*.h" "${lib}/test/*.hpp")
  if(lib STREQUAL "mathic")
    file(GLOB helpers CONFIGURE_DEPENDS RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}/${lib}"
      "${lib}/divsim/*.h")
    list(APPEND test_files ${helpers})
  endif()
  foreach(file IN LISTS test_files)
    configure_file("${lib}/${file}" "${test_root}/${file}" COPYONLY)
  endforeach()
  set(sources)
  foreach(test IN LISTS _${lib}_tests)
    list(APPEND sources "${test_root}/test/${test}.cpp")
  endforeach()
  add_executable(${lib}-unit-tests ${sources})
  target_include_directories(${lib}-unit-tests PRIVATE "${test_root}")
  if(NOT GTEST_FOUND)
    find_package(GTest QUIET)
  endif()
  if(GTEST_FOUND)
    target_link_libraries(${lib}-unit-tests ${lib} GTest::GTest GTest::Main)
  else()
    if(NOT TARGET gtest)
      include(FetchContent)
      FetchContent_Declare(googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG v1.16.0)
      FetchContent_MakeAvailable(googletest)
    endif()
    target_link_libraries(${lib}-unit-tests ${lib} gtest)
  endif()
  include(GoogleTest)
  gtest_discover_tests(${lib}-unit-tests TEST_PREFIX unit-tests:)
endforeach()
