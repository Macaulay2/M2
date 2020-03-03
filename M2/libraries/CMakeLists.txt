# TODO:
# for ubuntu: mpir readline
# for mac:    mpir readline history

set(ENV{PKG_CONFIG_PATH} "${CMAKE_BINARY_DIR}/usr-host/lib/pkgconfig:${CMAKE_BINARY_DIR}/usr-host/lib64/pkgconfig")

set(TARFILES
  # My machine download these:
  4ti2      3053e7467b5585ad852f6a56e78e28352653943e7249ad5e5174d4744d174966 4ti2-1.6.9.tar.gz
  cddlib    fe6d04d494683cd451be5f6fe785e147f24e8ce3ef7387f048e739ceb4565ab5 cddlib-094h.tar.gz
  cohomcalg 367c52b99c0b0a4794b215181439bf54abe4998872d3ef25d793bc13c4d40e42 cohomCalg-0.32.tar.gz
  csdp      7f202a15f33483ee205dcfbd0573fdbd74911604bb739a04f8baa35f8a055c5b Csdp-6.2.0.tgz
  factory   9dd84d11204e1457dac0a0d462a78d4cd4103c14cbf792b83d488aa529ad5724 factory-4.1.1.tar.gz 
  flint     cbf1fe0034533c53c5c41761017065f85207a1b770483e98b2392315f6575e87 flint-2.5.2.tar.gz
#  frobby    af092383e6dc849c86f4e79747ae0e5cd309a690747230e10aa38d60640062df frobby_v0.9.0.tar.gz
#  gc        436a0ddc67b1ac0b0405b61a9675bca9e075c8156f4debd1d06f3a56c7cd289d gc-8.0.4.tar.gz
  gfan      a674d5e5dc43634397de0d55dd5da3c32bd358d05f72b73a50e62c1a1686f10a gfan0.6.2.tar.gz
#  gftables  9cd158ceb1c2b1c47bdca2c0b004bba92cb0e0aaa0ea6a43ca784ebdce10eebd factory.4.0.1-gftables.tar.gz
#  givaro    19101e41161db46a925a0d055cf530c6d731b0dcc79e69f4358e483778306d16 givaro-4.0.3.tar.gz
  glpk      e398be2e7cb8a98584325268704729872558a4a88555bc8a54139d017eb9ebae glpk-4.59.tar.gz
#  gtest     58a6f4277ca2bc8565222b3bbd58a177609e9c488e8a72649359ba51450db7d8 gtest-1.8.0.tar.gz
  lrslib    adf92f9c7e70c001340b9c28f414208d49c581df46b550f56ab9a360348e4f09 lrslib-062.tar.gz
  mpc       6985c538143c1208dcb1ac42cedad6ff52e267b47e5f970183a3e75125b43c2e mpc-1.1.0.tar.gz
  mpfr      1d3be708604eae0e42d578ba93b390c2a145f17743a744d8f3f8c2ad5855a38a mpfr-4.0.2.tar.xz
  mpir      52f63459cf3f9478859de29e00357f004050ead70b45913f2c2269d9708675bb mpir-3.0.0.tar.bz2
  nauty     5d52211cec767d8d8e43483d96202be235f85696d1373c307291273463c812fa nauty27b11.tar.gz
  normaliz  436a870a1ab9a5e0c2330f5900d904dc460938c17428db1c729318dbd9bf27aa normaliz-3.7.2.tar.gz
  ntl       b90b36c9dd8954c9bc54410b1d57c00be956ae1db5a062945822bbd7a86ab4d2 ntl-10.5.0.tar.gz
  pari      4a6532b3c77350363fa618ead5cd794a172d7b7e5757a28f7788e658b5469339 pari-2.11.2.tar.gz
  topcom    3f83b98f51ee859ec321bacabf7b172c25884f14848ab6c628326b987bd8aaab TOPCOM-0.17.8.tar.gz
  # Everything possible:
  # LIBLIST gc gdbm mpir mpfr readline ntl flint factory lapack frobby glpk cddlib fplll givaro linbox boost mpc qd mpack gtest
  # PROGLIST 4ti2 gfan normaliz csdp nauty cddplus lrslib gftables topcom cohomcalg
  # SUBLIST memtailor mathic mathicgb fflas_ffpack
  # FIXME: gmp libtool pari are Missing below
  )

# whether the package 4ti2 is installed
#4ti2-circuits 4ti2
#cohomcalg cohomcalg
#gfan gfan
# whether the package lrs is installed
#lrs lrslib
#csdp csdp
#normaliz normaliz
# whether the package nauty is installed
#nauty-complg nauty

#############################################################################

# The list BUILTLIBS is the list of link options for the libraries we have decided to build,
#    of the form "-lFOO", which will be used to link Macaulay2 later on.  These libraries are not
#    available at the time this configure script is run, which is why we can't add "-lFOO" to LIBS.
#    (Programs linked by this configure script are linked with the options in LIBS.  This allows libraries
#    dependent on previously detected libraries to be detected by tests that involve linking.
set(BUILTLIBS "")

# The list LIBLIST is the list of libraries that might be used and linked into M2.
set(LIBLIST
  gc gdbm mpir mpfr readline ntl flint factory lapack
  frobby glpk cddlib fplll givaro linbox boost mpc qd mpack gtest
  )

# The list PROGLIST is the list of programs and libraries for them that are distributed with M2.
#     Initially, we offer no option for not compiling some of them.
set(PROGLIST 4ti2 gfan normaliz csdp nauty cddplus lrslib gftables topcom cohomcalg)

# The list SUBLIST is the list of submodules that might be used and linked into M2.
set(SUBLIST  memtailor mathic mathicgb fflas_ffpack)

# These three lists reflect dependencies, with prerequisites listed first, including the following dependencies:
#    mathicgb needs mathic and memtailor
#    mathic needs memtailor
#    factory needs flint, ntl and gmp; it includes and installs gftables, so doesn't need it separately
#    libfac has been replaced by code in factory
#    lapack includes blas, makes both libblas and liblapack
#    mpir is a plug-in replacement for gmp and can provide libgmp and libgmpxx, too.
#    mpfr needs gmp (or mpir
#    	  mpfr puts pointers to gmp numbers in thread local variables, unless
# 	  specially configured, so we shouldn't tell gmp to use libgc (we used to do that)
#    mpc needs mpfr
#    ntl needs gmp (or mpir)
#    4ti2 needs glpk
#    glpk needs gmp (or mpir)
#    topcom depends on cddlib
#    gfan needs cddlib
#    cddlib uses gmp
#    polymake cannot be included in Macaulay2 because its compile/build/install procedure is flawed:
#	it uses a dynamic readline library that can only be provided by fink and probably will not be copied into the install location
#       it offers no way to provide a directory tree containing a readline library we've compiled
#	it writes a file into the user's home directory called ".polymake"
#	it asks questions
#    normaliz needs libgmp, libgmpxx, boost
#    Greg Smith requested cddplus and lrslib for future use
#    nauty is used by the package Nauty
#    normaliz is used by the package Normaliz
#    gfan is used by the packages gfanInterface and StatePolytope
#    4ti2 is used by the package FourTiTwo
#    linbox is provided as an option for experimentation
#    linbox needs fflas_ffpack and givaro
#    givaro uses gmp
#    fflas_ffpack needs givaro and lapack, but fflas_ffpack is just source code, so we don't *have* to build it
#    mpir is used by givaro
#    mpir and mpfr are used by fplll
#    Henry Duong is experimenting with mpack, a multi-precision version of blas+lapack based on mpfr
#    mpack depends on gmp (or mpir) mpfr mpc qd
#    flint depends on gmp (or mpir) and mpfr
#    frobby depends on gmp

#LIBLIST=" $LIBLIST "
#AC_ARG_ENABLE(build-libraries, AS_HELP_STRING(--enable-build-libraries=...,[list of libraries, submodules, and programs to build from downloaded source code (e.g., gc gdbm mpir mpfr readline ntl gftables factory lapack frobby glpk cddlib givaro fflas_ffpack linbox boost mpc qd mpack 4ti2 gfan normaliz csdp nauty cddplus lrslib)]),
#    [for i in $enableval
#    do case "$LIBLIST $SUBLIST" in
#	    *" $i "*) 
#	        eval BUILD_$i=yes 
#		BUILD_ALWAYS="$BUILD_ALWAYS $i"
#	    	;;
#	    *) AC_MSG_ERROR(--enable-build-libraries option: unrecognized library name: $i) ;;
#       esac
#    done])

# TOOD
message("## External library information:
     BUILDLIBLIST      = ${BUILDLIBLIST}
     BUILDSUBLIST      = ${BUILDSUBLIST}
     BUILDPROGLIST     = ${BUILDPROGLIST}
     BUILDLIST         = ${BUILDLIST}
     BUILD_ALWAYS      = ${BUILD_ALWAYS}")

# TODO
message("## Linker information:
     BUILTLIBS         = ${BUILTLIBS}
     LINALGLIBS        = ${LINALGLIBS}
     LIBS              = ${LIBS}")

#############################################################################

## mpir and gmp serve the same purpose
#AC_LANG(C)
#AC_CHECK_HEADER(gmp.h, FOUND_gmp=yes ,FOUND_gmp=no )
#AC_CHECK_HEADER(mpir.h,FOUND_mpir=yes,FOUND_mpir=no)
#dnl Remark: both gmp.h and mpir.h are surrounded by 
#dnl 	#ifndef __GMP_H__
#dnl 	#endif
#dnl , so only one can be loaded.  Similarly for gmpxx.h and mpirxx.h.  However, the contents of
#dnl the files differ.  For example, mpf_cmp_z is defined only in gmp.h.
#AC_SUBST(PREFERRED_INTEGER_PACKAGE)
#AC_ARG_WITH(integer-package,
#		AS_HELP_STRING(--with-integer-package=[mpir|gmp],specify the big integer package to use (mpir or gmp)),
#		if ! test $withval = mpir -o $withval = gmp
#		then AC_MSG_ERROR([big integer package option: invalid value: $withval])
#		fi
#		PREFERRED_INTEGER_PACKAGE=$withval)
#if test "$FOUND_mpir" = yes
#then if test "$FOUND_gmp" = yes
#     then if test "$PREFERRED_INTEGER_PACKAGE" = gmp
#          then USING_MPIR=0
#	  else USING_MPIR=1
#	  fi
#     else USING_MPIR=1
#     fi
#else if test "$FOUND_gmp" = yes
#     then if test "$PREFERRED_INTEGER_PACKAGE" = mpir
#          then BUILD_mpir=yes
#	       USING_MPIR=1
#	  else USING_MPIR=0
#	  fi
#     else if test "$PREFERRED_INTEGER_PACKAGE" = gmp
#          then AC_MSG_ERROR([gmp integer package specified, but not found])
#	  else BUILD_mpir=yes
#	       USING_MPIR=1
#	  fi
#     fi
#fi
#if test "$USING_MPIR" = 0
#then AC_DEFINE(USING_MPIR,0,[Whether we use MPIR (instead of GMP)])
#else AC_DEFINE(USING_MPIR,1,[Whether we use MPIR (instead of GMP)])
#fi
#if test $BUILD_mpir = yes
#then BUILTLIBS="-lmpirxx -lmpir $BUILTLIBS"
#else if test "$USING_MPIR" = 1
#     then LIBS="-lmpirxx -lmpir $LIBS"
#     else LIBS="-lgmpxx  -lgmp  $LIBS"
#     fi
#fi
#AC_SUBST(USING_MPIR)



#AC_LANG(C)
#dnl topcom includes setoper.h, rather than cdd/setoper.h, so we do, too
#AC_CHECK_HEADER(setoper.h,,BUILD_cddlib=yes)
#if test $BUILD_cddlib = yes
#then BUILTLIBS="-lcdd $BUILTLIBS"
#else LIBS="-lcdd $LIBS"
#fi

#AC_LANG(C)
#AC_CHECK_HEADER(flint/flint.h,LIBS="-lflint -lgmp $LIBS",BUILD_flint=yes)
#if test $BUILD_flint = yes
#then BUILTLIBS="-lflint $BUILTLIBS"
#fi
#AC_SUBST(BUILD_flint)		dnl factory needs to know

#AC_LANG(C)
#AC_CHECK_HEADER(NTL/version.h,,BUILD_ntl=yes)
#if test $BUILD_ntl = yes
#then BUILTLIBS="-lntl $BUILTLIBS"
#else LIBS="-lntl $LIBS"
#fi
#AC_SUBST(BUILD_ntl)		dnl factory needs to know

##############################################################################

#PROGLIST=" $PROGLIST "
#AC_ARG_WITH(unbuilt-programs, 
#    [AS_HELP_STRING(--with-unbuilt-programs=...,list of programs not to build from downloaded source code (e.g., $PROGLIST))],
#    [for i in $withval
#     do case $PROGLIST in
#	     *" $i "*) eval BUILD_$i=no ;;
#	     *) AC_MSG_ERROR(unrecognized program name: $i) ;;
#	esac
#     done])

#
## the order of these segments also reflects dependencies
#AC_LANG(C)
#AC_SEARCH_LIBS(tgoto,tinfo ncurses curses,,AC_MSG_ERROR([[not found: library containing symbol tgoto; tried libcurses, libncurses, and libtinfo)]]))
#if test $BUILD_readline = no
#then AC_CHECK_HEADER(readline/readline.h,,BUILD_readline=yes)
#fi
#if test $BUILD_readline = no
#then AC_SEARCH_LIBS(rl_set_prompt,readline,,BUILD_readline=yes)
#fi
#if test $BUILD_readline = no
#then AC_SEARCH_LIBS(rl_completion_matches,readline,,BUILD_readline=yes)
#fi
#if test $BUILD_readline = no
#then AC_SEARCH_LIBS(readline,readline,,BUILD_readline=yes)
#fi
#if test $BUILD_readline = no
#then AC_SEARCH_LIBS(add_history,history readline,,BUILD_readline=yes)
#fi
#if test $BUILD_readline = no
#then # readline on Mac OS X is stuck at version 4.2, which has this bug:
#     #   CTRL-A doesn't go all the way to the beginning of the
#     #   line after typing r e s o TAB C-a
#     # So we build it ourselves.
#     AC_LANG(C)
#     AC_MSG_CHECKING([whether readline library is new enough (version at least 6)])
#     AC_RUN_IFELSE([AC_LANG_SOURCE([[
#	     #include <stdio.h>
#	     #include <readline/readline.h>
#	     int main () { return ! ( RL_READLINE_VERSION >= 6 * 0x100 ) ; }]])],
#	[ AC_MSG_RESULT([yes]) ],
#	[ AC_MSG_RESULT([no, will build it]) ; BUILD_readline=yes ],
#	[ AC_MSG_RESULT([cross-compiling, test not possible]) ])
#fi
#if test $BUILD_readline = no
#then AC_CHECK_DECL(rl_catch_signals,,BUILD_readline=yes,[
#	#include <stdio.h>
#	#include <readline/readline.h>
#	])
#fi
#if test $BUILD_readline = yes
#then AC_MSG_NOTICE(readline library will be compiled)
#     BUILTLIBS="-lreadline -lhistory $BUILTLIBS"
#fi

#if test $BUILD_memtailor = no
#then AC_LANG(C++)
#     SAVE_CXXFLAGS="$CXXFLAGS"
#     CXXFLAGS="$CXXFLAGS"
#     AC_SEARCH_LIBS(MEMTAILOR_VERSION_STRING,memtailor,,BUILD_memtailor=yes)
#     AC_CHECK_HEADER(memtailor.h,,BUILD_memtailor=yes)
#     CXXFLAGS="$SAVE_CXXFLAGS"
#fi
#test $BUILD_memtailor = yes && BUILTLIBS="-lmemtailor $BUILTLIBS"

#if test $BUILD_mathic = no
#then AC_LANG(C++)
#     SAVE_CXXFLAGS="$CXXFLAGS"
#     CXXFLAGS="$CXXFLAGS"
#     AC_SEARCH_LIBS(MATHIC_VERSION_STRING,mathic,,BUILD_mathic=yes)
#     AC_CHECK_HEADER(mathic.h,,BUILD_mathic=yes)
#     CXXFLAGS="$SAVE_CXXFLAGS"
#fi
#test $BUILD_mathic = yes && BUILTLIBS="-lmathic $BUILTLIBS"

#if test $BUILD_mathicgb = no
#then AC_LANG(C++)
#     SAVE_CXXFLAGS="$CXXFLAGS"
#     CXXFLAGS="$CXXFLAGS"
#     AC_SEARCH_LIBS(MATHICGB_VERSION_STRING,mathicgb,,BUILD_mathicgb=yes)
#     AC_CHECK_HEADER(mathicgb.h,,BUILD_mathicgb=yes)
#     CXXFLAGS="$SAVE_CXXFLAGS"
#fi
#test $BUILD_mathicgb = yes && BUILTLIBS="-lmathicgb $BUILTLIBS"

#
#if test "$PYTHON" = yes
#then AC_LANG(C)
#     if test "$LIBPYTHON" = "$LIBPYTHONORIG"
#     then AC_SEARCH_LIBS(Py_Initialize,python2.7,,AC_MSG_ERROR(libpython2.7 not found))
#     else LIBS="$LIBPYTHON $LIBS"
#     fi
#     AC_CHECK_HEADER(python2.7/Python.h,,AC_MSG_ERROR(include file python2.7/Python.h not found))
#fi

#
#if test $BUILD_boost = no
#then AC_LANG(C++)
#     AC_CHECK_HEADER(boost/version.hpp,,BUILD_boost=yes)
#fi

#SINGULARLIBS="-lfactory  "
#if test $BUILD_factory = no
#then
#    AC_MSG_CHECKING([whether factory library is installed])
#    FACTORY_MINVERSION=4.0.0
#    if $PKG_CONFIG --exists "factory >= $FACTORY_MINVERSION"
#    then
#	FACTORY_NAME=factory
#	AC_MSG_RESULT([yes ($FACTORY_NAME)])
#    elif $PKG_CONFIG --exists "singular-factory >= $FACTORY_MINVERSION"
#    then
#	FACTORY_NAME=singular-factory
#	AC_MSG_RESULT([yes ($FACTORY_NAME)])
#    elif $PKG_CONFIG --exists factory || $PKG_CONFIG --exists singular-factory
#    then
#	BUILD_factory=yes
#	AC_MSG_RESULT([yes, but version $FACTORY_MINVERSION required; will build])
#    else
#	BUILD_factory=yes
#	AC_MSG_RESULT([no; will build])
#    fi
#fi
#if test $BUILD_factory = no
#then
#     LIBS="`$PKG_CONFIG --libs $FACTORY_NAME` $LIBS"
#     CPPFLAGS="`$PKG_CONFIG --cflags $FACTORY_NAME` $CPPFLAGS"
#else
#     BUILTLIBS="$SINGULARLIBS $BUILTLIBS"
#fi

#################################################################################

# Testing ..
set(M2_SOURCE_URL https://faculty.math.illinois.edu/Macaulay2/Downloads/OtherSourceCode)

include(ExternalProject) # populate at build time; FetchContent populates at configure time
#find_program(MAKE_EXE NAMES gmake nmake make)
set(MAKE_EXE make)

# TODO: build in destination or install there?
ExternalProject_Add(gftables
  URL               ${M2_SOURCE_URL}/factory.4.0.1-gftables.tar.gz
  URL_HASH          SHA256=9cd158ceb1c2b1c47bdca2c0b004bba92cb0e0aaa0ea6a43ca784ebdce10eebd
  PREFIX            gftables
  SOURCE_DIR        ${M2_CORE_DIR}/factory/gftables
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ""
  INSTALL_COMMAND   ""
  )

#AC_DEFINE([HAVE_GIVARO_isunit],,[whether givaro has isunit])
#AC_MSG_CHECKING([whether givaro library is installed])
#if $PKG_CONFIG --exists givaro
#then AC_MSG_RESULT(yes)
#     LIBS="`$PKG_CONFIG --libs givaro` $LIBS"
#     CPPFLAGS="$CPPFLAGS `$PKG_CONFIG --cflags-only-I givaro`"
#     AC_MSG_CHECKING([whether givaro has isUnit or isunit])
#     AC_LANG([C++])
#     AC_COMPILE_IFELSE(
#	 [AC_LANG_PROGRAM( [ #include <givaro/gfq.h>
#	 		     ],
#	 		   [ class Givaro::GFqDom<long int> foo; foo.isunit(0) ])],
#	 [AC_MSG_RESULT([isunit]); AC_DEFINE([HAVE_GIVARO_isunit], [1])],
#	 [AC_MSG_RESULT([isUnit]); AC_DEFINE([HAVE_GIVARO_isunit], [0])])
#else AC_MSG_RESULT(no)
#     AC_DEFINE([HAVE_GIVARO_isunit], [0])
#     BUILD_givaro=yes
#fi
#if test $BUILD_givaro = yes
#then BUILTLIBS="-lgivaro $BUILTLIBS"
#fi

ExternalProject_Add(givaro
  URL               ${M2_SOURCE_URL}/givaro-4.0.3.tar.gz
  URL_HASH          SHA256=19101e41161db46a925a0d055cf530c6d731b0dcc79e69f4358e483778306d16
  PREFIX            givaro
  SOURCE_DIR        givaro/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ./configure --prefix=${CMAKE_BINARY_DIR}/usr-host
  BUILD_COMMAND     ${MAKE_EXE}
        COMMAND     ${MAKE_EXE} install
  INSTALL_COMMAND   ""
  )

# FIXME: permissions on the installed files are wrong
# TODO: which flags are important??
# TODO: AC_CHECK_HEADER(frobby.h,LIBS="-lfrobby -lgmpxx -lgmp $LIBS",BUILD_frobby=yes)
ExternalProject_Add(frobby
  URL               ${M2_SOURCE_URL}/frobby_v0.9.0.tar.gz
  URL_HASH          SHA256=af092383e6dc849c86f4e79747ae0e5cd309a690747230e10aa38d60640062df 
  PREFIX            frobby
  SOURCE_DIR        frobby/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/frobby/patch-0.9.0
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ${MAKE_EXE} -j4
    prefix=${CMAKE_BINARY_DIR}/usr-host
    GMP_INC_DIR=${CMAKE_BINARY_DIR}/usr-host/include
    RANLIB=${CMAKE_RANLIB} library
#    'CXXFLAGS=-std=gnu++11 -g3 -O2  -Wno-mismatched-tags -w' CXX=g++
#    'LDFLAGS=-L${CMAKE_BINARY_DIR}/usr-host/lib 
#    -l${CMAKE_BINARY_DIR}/submodules/memtailor/.libs 
#    -l${CMAKE_BINARY_DIR}/submodules/mathic/.libs 
#    -l${CMAKE_BINARY_DIR}/submodules/mathicgb/.libs -g3'
#    'CPPFLAGS=-I${CMAKE_BINARY_DIR}/../../../include
#    -I${CMAKE_BINARY_DIR}/include
#    -I${CMAKE_BINARY_DIR}/usr-host/include
#    -I${CMAKE_BINARY_DIR}/usr-host/include/cdd 
#    -I${CMAKE_BINARY_DIR}/../../../submodules/memtailor/src 
#    -I${CMAKE_BINARY_DIR}/../../../submodules/mathic/src 
#    -I${CMAKE_BINARY_DIR}/../../../submodules/mathicgb/src
#    -isystem /usr/include/libxml2
#    -DNDEBUG
#    -I/usr/include/eigen3'
        COMMAND
          /usr/bin/install -c -d ${CMAKE_BINARY_DIR}/usr-host/lib &&
          /usr/bin/install -c -d ${CMAKE_BINARY_DIR}/usr-host/include &&
          cp bin/libfrobby.a ${CMAKE_BINARY_DIR}/usr-host/lib/libfrobby.a &&
          cp src/frobby.h ${CMAKE_BINARY_DIR}/usr-host/include/frobby.h &&
          cp src/stdinc.h ${CMAKE_BINARY_DIR}/usr-host/include/stdinc.h
  INSTALL_COMMAND   ""
  )

#################################################################################
## git submodules section

# TODO: do we actually need googletest installed, or just gtest-all.cc?
ExternalProject_Add(googletest
  GIT_REPOSITORY    https://github.com/google/googletest.git 
  GIT_TAG           release-1.10.0 # 42bc671f
  PREFIX            googletest
  SOURCE_DIR        googletest/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
#  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/usr-host -DBUILD_GMOCK=OFF # -DINSTALL_GTEST=OFF
  STEP_TARGETS      download
  )
set(GTEST_PATH  ${CMAKE_BINARY_DIR}/libraries/googletest/build/googletest) # ${CMAKE_BINARY_DIR}/usr-host/include/gtest

## bdwgc
# Note: Starting with 8.0, libatomic_ops is not necessary for C11 or C++14.
# Currently cloning master for significant cmake support. Hopefully soon there will be a stable release
ExternalProject_Add(bdwgc
  GIT_REPOSITORY    https://github.com/ivmai/bdwgc.git
  GIT_TAG           master
  PREFIX            bdwgc
  SOURCE_DIR        bdwgc/build
  INSTALL_DIR       usr-host
  BUILD_IN_SOURCE   ON
# TODO: what is GC_LARGE_ALLOC_WARN_INTERVAL=1?
  CMAKE_ARGS        -Denable_cplusplus=ON -Denable_threads=ON -Denable_large_config=ON -Dbuild_cord=OFF
                    -Denable_throw_bad_alloc_library=OFF -Denable_gcj_support=OFF -Denable_java_finalization=OFF
		    # -Denable_gc_debug=ON -Denable_parallel_mark=OFF -Denable_gc_assertions=ON -Dbuild_tests=ON
		    # -DGC_ABORT_ON_LEAK
                    -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/usr-host
  INSTALL_COMMAND   ""
  )

# TODO: PATCHFILE = libraries/fflas_ffpack/patch-2.2.2
ExternalProject_Add(fflas_ffpack
  GIT_REPOSITORY    https://github.com/Macaulay2/fflas-ffpack.git
  GIT_TAG           712cef0e
  PREFIX            fflas_ffpack
  SOURCE_DIR        fflas_ffpack/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND PKG_CONFIG_PATH=$ENV{PKG_CONFIG_PATH} ./configure --prefix=${CMAKE_BINARY_DIR}/usr-host
  BUILD_COMMAND     ${MAKE_EXE}
        COMMAND     ${MAKE_EXE} install
  INSTALL_COMMAND   ""
  DEPENDS           givaro
  )

# flint2 https://github.com/Macaulay2/flint2.git ??

# TODO: would it be better to use FetchContent_Declare instead?
ExternalProject_Add(memtailor
  GIT_REPOSITORY    https://github.com/mahrud/memtailor.git
  GIT_TAG           4fb227410ca7e37baf4451a1540e349536dd3cde # original: e85453b
  PREFIX            memtailor
  SOURCE_DIR        memtailor/build
  BUILD_IN_SOURCE   ON
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/usr-host -DPACKAGE_TESTS=OFF
  # TODO: use this
  DEPENDS           googletest
  )

ExternalProject_Add(mathic
  GIT_REPOSITORY    https://github.com/mahrud/mathic.git
  GIT_TAG           master # 5ac885917c89a2356c410d84e5f518e3cd0b67ed # original: 023afcf
  PREFIX            mathic
  SOURCE_DIR        mathic/build
  BUILD_IN_SOURCE   ON
  CMAKE_ARGS        -DCMAKE_PREFIX_PATH=${CMAKE_BINARY_DIR}/usr-host -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/usr-host
                    -DCMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/cmake -DPACKAGE_TESTS=OFF
  DEPENDS           memtailor
  )

ExternalProject_Add(mathicgb
  GIT_REPOSITORY    https://github.com/mahrud/mathicgb.git
  GIT_TAG           2dad1309e3afb659b29d9b76033263543e711f9f # original: bd634c8
  PREFIX            mathicgb
  SOURCE_DIR        mathicgb/build
  BUILD_IN_SOURCE   ON
  CMAKE_ARGS        -DCMAKE_PREFIX_PATH=${CMAKE_BINARY_DIR}/usr-host -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/usr-host
                    -DCMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/cmake -DPACKAGE_TESTS=OFF
  DEPENDS           mathic
  )

# mpir https://github.com/Macaulay2/mpir.git 82816d99

# TODO: replace readline with https://github.com/AmokHuginnsson/replxx


# TODO: add option to disable building shared libraries
# ifeq (@SHARED@,no)
# CONFIGOPTIONS += --disable-shared --enable-static
# endif

# TODO: add option to strip
