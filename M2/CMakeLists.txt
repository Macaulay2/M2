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

cmake_minimum_required(VERSION 3.10)

# specify the C++ standard
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# set the project name, version, and languages
project(macaulay2 VERSION 1.15 LANGUAGES C CXX)

set(SOURCE /home/mahrud/Projects/M2/M2/M2)
set(BOOTSTRAP ${SOURCE}/BUILD/mahrud/build)
# set(BOOTSTRAP ${SOURCE}/BUILD/mike/builds.tmp/darwin64-clang # Mike's

add_subdirectory(Macaulay2/c)
add_subdirectory(Macaulay2/d)
add_subdirectory(Macaulay2/e)
add_subdirectory(Macaulay2/bin)
add_subdirectory(Macaulay2/regex)
add_subdirectory(Macaulay2/system)
add_subdirectory(Macaulay2/kernel)

# TODO
#add_subdirectory(Macaulay2/book)
#add_subdirectory(Macaulay2/compat)
#add_subdirectory(Macaulay2/e-includes)
#add_subdirectory(Macaulay2/emacs)
#add_subdirectory(Macaulay2/html-check-links)
#add_subdirectory(Macaulay2/m2)
#add_subdirectory(Macaulay2/man)
#add_subdirectory(Macaulay2/packages)
#add_subdirectory(Macaulay2/subdirs)
#add_subdirectory(Macaulay2/tests)
#add_subdirectory(Macaulay2/util)

#add_subdirectory(BUILD/mahrud/build-gcc/libraries/gc/build/gc-8.0.4)
