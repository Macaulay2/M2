##### WARNING!! ##################################
# This is not yet functional
# it is designed for me (MES) to be able to use the CLion IDE
# and its refactorization tools.  If anyone would like to
# help get this functional, please contact us!
#
# In particular: one currently needs the built include files
# from our build tree (see CMakeLists.txt in Macaulay2/e)
##################################################

#set(CMAKE_VERBOSE_MAKEFILE on)
set(CMAKE_BUILD_DEBUG on)

cmake_minimum_required(VERSION 3.14)

# specify the C++ standard
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# set the project name, version, and languages
project(macaulay2 VERSION 1.15 LANGUAGES C CXX)

set(SHARE_DIR ${CMAKE_BINARY_DIR}/Macaulay2/share) # FIXME: usr-dist/common/share gives error?!
set(CORE_DIR  ${SHARE_DIR}/Macaulay2/Core)

# set(BOOTSTRAP ${CMAKE_SOURCE_DIR}/BUILD/mahrud/build)
set(BOOTSTRAP ${CMAKE_SOURCE_DIR}/BUILD/mike/builds.tmp/darwin64-latestclang) # Mike's
message("## Bootstrapping from previous build in: ${BOOTSTRAP}")

add_subdirectory(Macaulay2)

# TODO: Where to move this?
set(GFTABLES 10201 1024 10609 11449 11881 121 12167 125 12769 128 1331 1369 14641 15625 16 16129 16384 16807 1681 169 17161 1849 18769 19321 19683 2048 2187 2197 2209 22201 22801 2401 243 24389 24649 25 256 26569 27 27889 2809 28561 289 29791 29929 3125 32 32041 32761 32768 343 3481 361 36481 3721 37249 38809 39601 4 4096 44521 4489 49 4913 49729 5041 50653 512 51529 52441 529 5329 54289 57121 58081 59049 6241 625 63001 64 6561 6859 6889 729 7921 8 81 8192 841 9 9409 961)

add_custom_target(factory
  COMMAND
    ${CMAKE_COMMAND} -E make_directory ${CORE_DIR}/factory/gftables
  COMMAND
    ${CMAKE_COMMAND} -E copy_if_different ${GFTABLES} ${CORE_DIR}/factory/gftables
  WORKING_DIRECTORY ${BOOTSTRAP}/libraries/gftables/build/gftables
  )
