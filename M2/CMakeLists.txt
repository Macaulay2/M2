##### WARNING!! ##################################
# This is not yet functional
# it is designed for me (MES) to be able to use the CLion IDE
# and its refactorization tools.  If anyone would like to
# help get this functional, please contact us!
#
# In particular: one currently needs the built include files
# from our build tree (see CMakeLists.txt in Macaulay2/e)
##################################################

cmake_minimum_required(VERSION 3.6)
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

project(macaulay2 VERSION 1.9 LANGUAGES C CXX)

add_subdirectory(Macaulay2/e)

# target_include_directories(m2 PUBLIC
#     "BUILD/mike/builds.tmp/darwin64-gcc4.9/include"
#     "BUILD/mike/builds.tmp/darwin64-gcc4.9/Macaulay2/d"
#     "BUILD/mike/builds.tmp/darwin64-gcc4.9/Macaulay2/e-includes"
#     include
#     Macaulay2/e
#     Macaulay2/c
#     "${PROJECT_BINARY_DIR}"
#     )                            

# set(E-FILES
#     Macaulay2/e/comp.cpp
# )
# set(SOURCE_FILES
#     Macaulay2/e/comp.cpp
#     Macaulay2/e/unit-tests/testMain.cpp
#     )

# add_library(m2 STATIC ${E-FILES})
# add_library(m2 STATIC class.cpp)
# target_link_libraries(m2 STATIC )



# enable_testing()
# add_executable(macaulay2-test Macaulay2/e/unit-tests/testMain.cpp)

# find_package(GTest REQUIRED)
# include_directories(${GTEST_INCLUDE_DIRS})
# target_link_libraries(macaulay2-test ${GTEST_BOTH_LIBRARIES})
# add_test(AllTestsInFoo macaulay2-test)


# add_executable(proj-test
#   ${source_files}
#   ${test_files}
#   )

# set(GTEST_INCLUDE_DIRS
#   gtest-1.7.0/include
#   gtest-1.7.0
#   )

# target_include_directories(proj-test PUBLIC
#   ${GTEST_INCLUDE_DIRS}
#   include
#   )


# include_directories("${PROJECT_BINARY_DIR}")
# include_directories(
#     "BUILD/mike/builds.tmp/darwin64-gcc4.9/include"
#     "BUILD/mike/builds.tmp/darwin64-gcc4.9/Macaulay2/d"
#     "BUILD/mike/builds.tmp/darwin64-gcc4.9/Macaulay2/e-includes"
#     include
#     Macaulay2/e
#     Macaulay2/c
# )

