##### WARNING!! ##################################
# This is not yet functional
# it is designed for me (MES) to be able to use the CLion IDE
# and its refactorization tools.  If anyone would like to
# help get this functional, please contact us!
#
# In particular: one currently needs the built include files
# from our build tree (see CMakeLists.txt in Macaulay2/e)
##################################################

cmake_minimum_required(VERSION 3.10)

# specify the C++ standard
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# set the project name, version, and languages
project(macaulay2 VERSION 1.15 LANGUAGES C CXX)

set(SOURCE /home/mahrud/Projects/M2/M2/M2)
set(BOOTSTRAP ${SOURCE}/BUILD/mahrud/build) # ${SOURCE}/BUILD/mike/builds.tmp/darwin64-clang
#set(GCCBOOTSTRAP ${SOURCE}/BUILD/mahrud/build-gcc)

add_subdirectory(Macaulay2/c)
add_subdirectory(Macaulay2/d)
add_subdirectory(Macaulay2/e)
#add_subdirectory(BUILD/mahrud/build-gcc/libraries/gc/build/gc-8.0.4)

#xinclude_directories(Macaulay2/e)

set(CMAKE_VERBOSE_MAKEFILE on)



if(FALSE)

add_executable(M2-binary
#  ${SOURCE}/Macaulay2/e/comp.cpp
  ${SOURCE}/Macaulay2/d/main.c
#  ${SOURCE}/Macaulay2/d/M2lib.c
#  ${SOURCE}/Macaulay2/e/engine.cpp
  )

target_link_libraries(M2-binary
  ${BOOTSTRAP}/Macaulay2/d/actors2.o
  ${BOOTSTRAP}/Macaulay2/d/actors3.o
  ${BOOTSTRAP}/Macaulay2/d/actors4.o
  ${BOOTSTRAP}/Macaulay2/d/actors5.o
  ${BOOTSTRAP}/Macaulay2/d/actors6.o
  ${BOOTSTRAP}/Macaulay2/d/actors.o
  ${BOOTSTRAP}/Macaulay2/d/arithmetic.o
  ${BOOTSTRAP}/Macaulay2/d/atomic.o
  ${BOOTSTRAP}/Macaulay2/d/basic.o
  ${BOOTSTRAP}/Macaulay2/d/binding.o
  ${BOOTSTRAP}/Macaulay2/d/buckets.o
  ${BOOTSTRAP}/Macaulay2/d/classes.o
  ${BOOTSTRAP}/Macaulay2/d/common.o
  ${BOOTSTRAP}/Macaulay2/d/convertr.o
  ${BOOTSTRAP}/Macaulay2/d/ctype.o
  ${BOOTSTRAP}/Macaulay2/d/engine.o
  ${BOOTSTRAP}/Macaulay2/d/equality.o
  ${BOOTSTRAP}/Macaulay2/d/errio.o
  ${BOOTSTRAP}/Macaulay2/d/err.o
  ${BOOTSTRAP}/Macaulay2/d/evaluate.o
  ${BOOTSTRAP}/Macaulay2/d/expr.o
  ${BOOTSTRAP}/Macaulay2/d/flint.o
  ${BOOTSTRAP}/Macaulay2/d/gdbm_interface.o
  ${BOOTSTRAP}/Macaulay2/d/getline.o
  ${BOOTSTRAP}/Macaulay2/d/gmp1.o
  ${BOOTSTRAP}/Macaulay2/d/gmp_aux.o
  ${BOOTSTRAP}/Macaulay2/d/gmp.o
  ${BOOTSTRAP}/Macaulay2/d/hashtables.o
  ${BOOTSTRAP}/Macaulay2/d/interface2.o
  ${BOOTSTRAP}/Macaulay2/d/interface.o
  ${BOOTSTRAP}/Macaulay2/d/interp.o
  ${BOOTSTRAP}/Macaulay2/d/interrupts.o
  ${BOOTSTRAP}/Macaulay2/d/lex.o
  ${BOOTSTRAP}/Macaulay2/d/M2lib.o
  ${BOOTSTRAP}/Macaulay2/d/M2mem.o
  ${BOOTSTRAP}/Macaulay2/d/M2.o
  ${BOOTSTRAP}/Macaulay2/d/M2types.o
#  ${BOOTSTRAP}/Macaulay2/d/main.o
  ${BOOTSTRAP}/Macaulay2/d/mysqldummy.o
  ${BOOTSTRAP}/Macaulay2/d/nets.o
  ${BOOTSTRAP}/Macaulay2/d/parse.o
  ${BOOTSTRAP}/Macaulay2/d/parser.o
  ${BOOTSTRAP}/Macaulay2/d/pthread0.o
  ${BOOTSTRAP}/Macaulay2/d/pthread.o
  ${BOOTSTRAP}/Macaulay2/d/scclib.o
  ${BOOTSTRAP}/Macaulay2/d/sets.o
  ${BOOTSTRAP}/Macaulay2/d/startup.o
  ${BOOTSTRAP}/Macaulay2/d/stdio0.o
  ${BOOTSTRAP}/Macaulay2/d/stdio.o
  ${BOOTSTRAP}/Macaulay2/d/stdiop0.o
  ${BOOTSTRAP}/Macaulay2/d/stdiop.o
  ${BOOTSTRAP}/Macaulay2/d/strings1.o
  ${BOOTSTRAP}/Macaulay2/d/strings.o
  ${BOOTSTRAP}/Macaulay2/d/struct.o
  ${BOOTSTRAP}/Macaulay2/d/system.o
  ${BOOTSTRAP}/Macaulay2/d/texmacs.o
  ${BOOTSTRAP}/Macaulay2/d/threads.o
  ${BOOTSTRAP}/Macaulay2/d/tokens.o
  ${BOOTSTRAP}/Macaulay2/d/util.o
  ${BOOTSTRAP}/Macaulay2/d/vararray.o
  ${BOOTSTRAP}/Macaulay2/d/varnets.o
  ${BOOTSTRAP}/Macaulay2/d/varstrin.o
  ${BOOTSTRAP}/Macaulay2/d/version.o
  ${BOOTSTRAP}/Macaulay2/d/xmlactors.o
  ${BOOTSTRAP}/Macaulay2/d/xml-c.o
  ${BOOTSTRAP}/Macaulay2/d/xml.o
  # PRIVATE
  ${BOOTSTRAP}/Macaulay2/regex/regex.o
  ${BOOTSTRAP}/Macaulay2/bin/timestamp.o
  ${BOOTSTRAP}/Macaulay2/kernel/bibasis/allocator.o
  ${BOOTSTRAP}/Macaulay2/kernel/bibasis/bibasis.o
  ${BOOTSTRAP}/Macaulay2/kernel/bibasis/launcher.o
  ${BOOTSTRAP}/Macaulay2/kernel/bibasis/monomDL.o
  ${BOOTSTRAP}/Macaulay2/kernel/bibasis/monomDRL.o
  ${BOOTSTRAP}/Macaulay2/kernel/bibasis/monomLex.o
  ${BOOTSTRAP}/Macaulay2/kernel/bibasis/monom.o
  ${BOOTSTRAP}/Macaulay2/kernel/bibasis/settings-manager.o
  ${BOOTSTRAP}/Macaulay2/system/m2file.o
  ${BOOTSTRAP}/Macaulay2/system/supervisor.o
  ${BOOTSTRAP}/Macaulay2/system/tests.o
  )

target_include_directories(M2-binary
  PUBLIC
    ${SOURCE}/include
    ${SOURCE}/Macaulay2/c
#    ${SOURCE}/Macaulay2/d
    ${SOURCE}/Macaulay2/e
    ${BOOTSTRAP}/include
    ${BOOTSTRAP}/Macaulay2/d
    ${BOOTSTRAP}/Macaulay2/e-includes
    ${BOOTSTRAP}/usr-host/include
  PRIVATE
    /usr/include/libxml2
#    /usr/include/eigen3
#    ../../submodules/memtailor/src
#    ../../submodules/mathic/src
#    ../../submodules/mathicgb/src
#    .
#    "${PROJECT_BINARY_DIR}"
  )

target_link_libraries(M2-binary
  engine
  )

endif()


