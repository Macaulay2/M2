# TODO:
# for ubuntu: mpir frobby readline
# for mac:    mpir fflas readline history

set(TARFILES
  # My machine download these:
  4ti2      3053e7467b5585ad852f6a56e78e28352653943e7249ad5e5174d4744d174966 4ti2-1.6.9.tar.gz
  cddlib    fe6d04d494683cd451be5f6fe785e147f24e8ce3ef7387f048e739ceb4565ab5 cddlib-094h.tar.gz
  cohomcalg 367c52b99c0b0a4794b215181439bf54abe4998872d3ef25d793bc13c4d40e42 cohomCalg-0.32.tar.gz
  csdp      7f202a15f33483ee205dcfbd0573fdbd74911604bb739a04f8baa35f8a055c5b Csdp-6.2.0.tgz
  factory   9dd84d11204e1457dac0a0d462a78d4cd4103c14cbf792b83d488aa529ad5724 factory-4.1.1.tar.gz 
  flint     cbf1fe0034533c53c5c41761017065f85207a1b770483e98b2392315f6575e87 flint-2.5.2.tar.gz
  frobby    af092383e6dc849c86f4e79747ae0e5cd309a690747230e10aa38d60640062df frobby_v0.9.0.tar.gz
  gc        436a0ddc67b1ac0b0405b61a9675bca9e075c8156f4debd1d06f3a56c7cd289d gc-8.0.4.tar.gz
  gfan      a674d5e5dc43634397de0d55dd5da3c32bd358d05f72b73a50e62c1a1686f10a gfan0.6.2.tar.gz
  gftables  9cd158ceb1c2b1c47bdca2c0b004bba92cb0e0aaa0ea6a43ca784ebdce10eebd factory.4.0.1-gftables.tar.gz
  givaro    19101e41161db46a925a0d055cf530c6d731b0dcc79e69f4358e483778306d16 givaro-4.0.3.tar.gz
  glpk      e398be2e7cb8a98584325268704729872558a4a88555bc8a54139d017eb9ebae glpk-4.59.tar.gz
  gtest     58a6f4277ca2bc8565222b3bbd58a177609e9c488e8a72649359ba51450db7d8 gtest-1.8.0.tar.gz
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
  # LIBLIST atomic_ops gc gdbm mpir mpfr readline ntl flint factory lapack frobby glpk cddlib fplll givaro linbox boost mpc qd mpack gtest
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
  atomic_ops gc gdbm mpir mpfr readline ntl flint factory lapack
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
#    gc needs atomic_ops (and sometimes includes it)
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

#AC_LANG(C)
#AC_CHECK_HEADER(gdbm.h,
#	test $BUILD_gdbm = yes && AC_MSG_WARN([[library gdbm exists, but we are asked to build it]])
#	LIBS="-lgdbm $LIBS"
#	,
#	BUILD_gdbm=yes)
#if test $BUILD_gdbm = yes
#then BUILTLIBS="-lgdbm $BUILTLIBS"
#fi

#AC_MSG_CHECKING(whether package bdw-gc is provided)
#dnl Note: we don't include -lgccpp below, because linking with that library is what replaces ::new and friends with GC_MALLOC_UNCOLLECTABLE and friends:
#if [ pkg-config --exists bdw-gc ]
#then AC_MSG_RESULT(yes)
#     CPPFLAGS="`pkg-config --cflags-only-I bdw-gc | sed -e 's=^-I/=-isystem /=g' -e 's= -I/= -isystem /=g'` $CPPFLAGS"
#     LIBS="-lgc $LIBS"
#     FOUND_gc=yes
#else AC_MSG_RESULT(no)
#     BUILD_gc=yes
#fi
#if test $BUILD_gc = yes
#then BUILTLIBS="-lgc -lpthread $BUILTLIBS"
#fi

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
#AC_CHECK_HEADER(mpfr.h,LIBS="-lmpfr $LIBS",BUILD_mpfr=yes)
#if test $BUILD_mpfr = yes
#then BUILTLIBS="-lmpfr $BUILTLIBS"
#fi

#AC_LANG(C)
#AC_CHECK_HEADER(mpc.h,LIBS="-lmpc $LIBS",BUILD_mpc=yes)
#if test $BUILD_mpc = yes
#then BUILTLIBS="-lmpc $BUILTLIBS"
#fi

#AC_LANG(C)
#AC_CHECK_HEADER(glpk.h,LIBS="-lglpk $LIBS",BUILD_glpk=yes)
#if test $BUILD_glpk = yes
#then BUILTLIBS="-lglpk $BUILTLIBS"
#fi

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

#AC_LANG(C++)
#AC_CHECK_HEADER(frobby.h,LIBS="-lfrobby -lgmpxx -lgmp $LIBS",BUILD_frobby=yes)
#if test $BUILD_frobby = yes
#then BUILTLIBS="-lfrobby $BUILTLIBS"
#fi

#if test $BUILD_gtest = no
#then
#    CHECK_GTEST
#    if test $have_gtest = yes
#    then
#        CPPFLAGS="$GTEST_CPPFLAGS $CPPFLAGS"
#        GTEST_PATH=$GTEST_SOURCE
#    else
#        BUILD_gtest=yes
#        GTEST_PATH="\$(BUILTLIBPATH)/include/gtest"
#    fi
#else
#    GTEST_PATH="\$(BUILTLIBPATH)/include/gtest"
#fi
#AC_SUBST(GTEST_PATH)

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

#if test $BUILD_atomic_ops = no
#then AC_MSG_CHECKING(whether package atomic_ops is provided)
#     if [ pkg-config --exists atomic_ops ]
#     then AC_MSG_RESULT(yes)
#	  CPPFLAGS="`pkg-config --cflags-only-I atomic_ops | sed -e 's=^-I/=-isystem /=g' -e 's= -I/= -isystem /=g'` $CPPFLAGS"
#     else AC_MSG_RESULT([no, will be built from downloaded sources])
#          BUILD_atomic_ops=yes
#     fi
#fi

## test for fflas_ffpack
#if test $BUILD_fflas_ffpack = no
#then AC_MSG_CHECKING(for fflas_ffpack library)
#     FFLAS_FFPACK_CXXFLAGS=`fflas-ffpack-config --cflags 2>&1`
#     if test $? = 0 && test "`fflas-ffpack-config --decimal-version`" -gt 20000
#     then AC_MSG_RESULT(found)
#          M2_CXXFLAGS="$M2_CXXFLAGS $FFLAS_FFPACK_CXXFLAGS"
#     else AC_MSG_RESULT(not found, will build)
#          BUILD_fflas_ffpack=yes
#     fi
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

#AC_DEFINE([HAVE_FACTORY_PREM], [1],
#    [whether Prem() from factory is public])
#if test $BUILD_factory = no
#then
#    AC_MSG_CHECKING([whether Prem() from factory is public])
#    AC_LANG([C++])
#    AC_COMPILE_IFELSE(
#	[AC_LANG_PROGRAM(
#	    [#include <factory/factory.h>],
#	    [CanonicalForm p,q; Prem(p,q)])],
#	[AC_MSG_RESULT([yes])],
#	[AC_MSG_RESULT([no])
#	 AC_DEFINE([HAVE_FACTORY_PREM], [0])])
#fi

#AC_ARG_VAR([GFTABLESDIR],
#    [path to gftables directory if factory is already installed])
#if test $BUILD_factory = yes
#then
#    BUILD_gftables=yes
#    AC_SUBST([gftablesdir], [${datadir}/Macaulay2/Core/factory/])
#else
#    BUILD_gftables=no
#    if test x$GFTABLESDIR = x
#    then
#	if test $FACTORY_NAME = factory
#	then
#	    GFTABLESDIR="${datadir}/factory/"
#	else
#	    GFTABLESDIR="${datadir}/singular/factory/"
#	fi
#    fi
#    adl_RECURSIVE_EVAL([$GFTABLESDIR], [GFTABLESDIR])
#    AC_CHECK_FILE([${GFTABLESDIR}gftables/961],
#	[AC_SUBST([gftablesdir], [$GFTABLESDIR])],
#	[AC_MSG_ERROR([could not find gftables but we are not building factory; try specifying the directory with GFTABLESDIR])])
#fi

#AC_DEFINE([FACTORY_STREAMIO], [1],
#    [whether factory was built with --enable-streamio])
#if test $BUILD_factory = no
#then
#    AC_MSG_CHECKING([whether factory was built with --enable-streamio])
#    AC_LANG([C++])
#    AC_COMPILE_IFELSE(
#	[AC_LANG_PROGRAM(
#	    [#include <factory/factory.h>],
#	    [Variable x; x = Variable(); std::cout << x])],
#	[AC_MSG_RESULT([yes])],
#	[AC_MSG_RESULT([no])
#	 AC_DEFINE([FACTORY_STREAMIO], [0])])
#fi

#AC_SUBST(LINALGLIBS)

## we need to do the fortran library testing last, in case AC_SEARCH_LIBS adds
## one of them to $LIBS, making it impossible to check for the presence of C or
## C++ libraries.  (I'm not sure why putting -llapack on the gcc command line
## needlessly causes the library to be linked against.)
#FORTRANUSED=no
#if test $BUILD_lapack = yes
#then if test $USE_FCLIBS != no
#     then LIBS="$FCLIBS $LIBS"
#     fi
#else lapack_found=no
#     blas_found=no
#     AC_MSG_CHECKING([whether the Accelerate framework for lapack is available])
#     LINALGLIBS="-framework Accelerate"
#     SAVELIBS=$LIBS LIBS="$LINALGLIBS $LIBS"
#     AC_LANG(C)
#     AC_LINK_IFELSE(
#	 [AC_LANG_PROGRAM(,[sgemv_();dgetrf_();])],
#	 [USE_FCLIBS=no; blas_found=yes; lapack_found=yes; AC_MSG_RESULT(yes)],
#	 [LINALGLIBS= LIBS=$SAVELIBS; AC_MSG_RESULT(no)])
#     if test $blas_found = no
#     then AC_MSG_CHECKING([whether package blas is provided (by pkg-config)])
#	  if pkg-config --exists blas
#	  then AC_MSG_RESULT(yes)
#	       blas_found=yes
#	       BLASLIBS=`pkg-config --libs blas`
#	       LIBS="$BLASLIBS $LIBS"
#	       CPPFLAGS="`pkg-config --cflags-only-I blas | sed -e 's=^-I/=-isystem /=g' -e 's= -I/= -isystem /=g'` $CPPFLAGS"
#	       AC_SEARCH_LIBS(sgemv_,,,AC_MSG_ERROR(blas function sgemv_ not found with $BLASLIBS))
#	  else AC_MSG_RESULT(no)
#	  fi
#     fi
#     if test $lapack_found = no
#     then AC_MSG_CHECKING([whether package lapack is provided (by pkg-config)])
#	  if pkg-config --exists lapack
#	  then AC_MSG_RESULT(yes)
#	       lapack_found=yes
#	       LAPACKLIBS="`pkg-config --libs lapack`"
#	       LIBS="$LAPACKLIBS $LIBS"
#	       CPPFLAGS="`pkg-config --cflags-only-I lapack | sed -e 's=^-I/=-isystem /=g' -e 's= -I/= -isystem /=g'` $CPPFLAGS"
#	       AC_SEARCH_LIBS(dgetrf_,,,AC_MSG_ERROR(lapack function dgetrf_ not found with $LAPACKLIBS))
#	  else AC_MSG_RESULT(no)
#	  fi
#     fi
#     if test $USE_FCLIBS != no
#     then LIBS="$FCLIBS $LIBS"; added_fclibs=yes
#     fi
#     if test $blas_found = no
#     then AC_SEARCH_LIBS(sgemv_,refblas blas f77blas atlcblas,
#                         blas_found=yes
#			 BLASLIBS=$ac_cv_search_sgemv_
#			 )
#          # we've been told that both f77blas and atlcblas are needed.  If so, fix this somehow.
#     fi
#     if test $lapack_found = no
#     then AC_SEARCH_LIBS(dgetrf_,lapack atllapack,
#	  		 lapack_found=yes
#			 LAPACKLIBS=$ac_cv_search_dgetrf_
#			 )
#     fi
#     if test $blas_found = no -o $lapack_found = no
#     then BUILD_lapack=yes
#          LINALGLIBS="$LINALGLIBS -llapack -lrefblas"
#     else LINALGLIBS="$LINALGLIBS $LAPACKLIBS $BLASLIBS"
#     fi
#fi

#if test $BUILD_lapack = yes
#then if test $added_fclibs != yes
#     then LIBS="$FCLIBS $LIBS"
#     fi
#     LINALGLIBS="$LINALGLIBS -llapack -lrefblas"
#     dnl we'll need the fortran compiler to be present to compile lapack
#     FORTRANUSED=yes
#     if test "$FC" = ""
#     then AC_MSG_ERROR([no fortran compiler found (FC not set)])
#     else AC_MSG_NOTICE(using fortran compiler $FC)
#     fi
#fi

#
#AC_SUBST(BUILDLIST,)
#for i in $LIBLIST $PROGLIST
#do eval t=\$BUILD_$i
#   test "$t" = yes && BUILDLIST="$BUILDLIST $i"
#done

#AC_SUBST(BUILDLIBLIST,)
#for i in $LIBLIST
#do eval t=\$BUILD_$i
#   test "$t" = yes && BUILDLIBLIST="$BUILDLIBLIST $i"
#done

#AC_SUBST(BUILDSUBLIST,)
#for i in $SUBLIST
#do eval t=\$BUILD_$i
#   test "$t" = yes && BUILDSUBLIST="$BUILDSUBLIST $i"
#done

#AC_SUBST(BUILDPROGLIST,)
#for i in $PROGLIST
#do eval t=\$BUILD_$i
#   test "$t" = yes && BUILDPROGLIST="$BUILDPROGLIST $i"
#done

#################################################################################

# Testing ..
set(M2_SOURCE_URL https://faculty.math.illinois.edu/Macaulay2/Downloads/OtherSourceCode)

include(ExternalProject) # populate at build time; FetchContent populates at configure time

ExternalProject_Add(gftables
  URL               ${M2_SOURCE_URL}/factory.4.0.1-gftables.tar.gz
  URL_HASH          SHA256=9cd158ceb1c2b1c47bdca2c0b004bba92cb0e0aaa0ea6a43ca784ebdce10eebd
  PREFIX            gftables
  SOURCE_DIR        ${M2_CORE_DIR}/factory/gftables # gftables/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ""
  INSTALL_COMMAND   ""
)

#################################################################################
## git submodules section

# bdwgc https://github.com/Macaulay2/bdwgc.git origin/master

# TODO: need to change givaro requirement to 4.0.2
ExternalProject_Add(fflas_ffpack
  GIT_REPOSITORY    https://github.com/Macaulay2/fflas-ffpack.git
  GIT_TAG           712cef0e
  PREFIX            fflas_ffpack
  SOURCE_DIR        fflas_ffpack/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND "" # ./autogen.sh ; ./configure ?
  BUILD_COMMAND     "" # ${MAKE_EXE}
  INSTALL_COMMAND   ""
)

# flint2 https://github.com/Macaulay2/flint2.git ??

# gtest https://github.com/google/googletest.git 42bc671f

# libatomic_ops https://github.com/Macaulay2/libatomic_ops.git ba6c317

ExternalProject_Add(mathic
  GIT_REPOSITORY    https://github.com/Macaulay2/mathic.git
  GIT_TAG           023afcf
  PREFIX            mathic
  SOURCE_DIR        mathic/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND "" # ./autogen.sh ; ./configure ?
  BUILD_COMMAND     "" # ${MAKE_EXE}
  INSTALL_COMMAND   ""
)

ExternalProject_Add(mathicgb
  GIT_REPOSITORY    https://github.com/Macaulay2/mathicgb.git
  GIT_TAG           bd634c8
  PREFIX            mathicgb
  SOURCE_DIR        mathicgb/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND "" # ./autogen.sh ; ./configure ?
  BUILD_COMMAND     "" # ${MAKE_EXE}
  INSTALL_COMMAND   ""
)

ExternalProject_Add(memtailor
  GIT_REPOSITORY    https://github.com/Macaulay2/memtailor.git
  GIT_TAG           e85453b
  PREFIX            memtailor
  SOURCE_DIR        memtailor/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND "" # ./autogen.sh ; ./configure ?
  BUILD_COMMAND     "" # ${MAKE_EXE}
  INSTALL_COMMAND   ""
)

# mpir https://github.com/Macaulay2/mpir.git 82816d99
