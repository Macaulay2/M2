define(M2_UPPER,[translit($1, `a-z', `A-Z')])

AC_DEFUN(M2_IS_THERE,[
  AC_CHECK_FUNC($1,,AC_DEFINE(M2_UPPER(NO_$1),1,whether $1 is missing))
])

AC_DEFUN(M2_IS_DECLARED,[
  AC_CACHE_CHECK(whether $2 is declared, m2_cv_$2_is_declared,
    AC_TRY_COMPILE(
      [
      #include <stdio.h>
      #include <errno.h>
      ],
      $1 x = $2,
      m2_cv_$2_is_declared=yes,
      m2_cv_$2_is_declared=no))
  if test "$m2_cv_$2_is_declared" = yes
  then AC_DEFINE(M2_UPPER($2_IS_DECLARED),,whether $2 is declared in errno.h or stdio.h)
  fi
])

AC_DEFUN(M2_SHOW_CONFDEFS,[
  echo contents of confdefs.h: >&6
  sed -e '/^$/d' -e 's/^/   /' confdefs.h >&6
  ])

dnl this list is the same as the one in Makefile-configure
AC_DEFUN(M2_CONFIGURED_FILES,
[config.Makefile\
 Makefile\
 Macaulay2/Makefile\
 Macaulay2/m2/Makefile\
 Macaulay2/Mathematica/Makefile\
 Macaulay2/Vasconcelos-appendix/Makefile\
 Macaulay2/basictests/Makefile\
 Macaulay2/benchmarks/Makefile\
 Macaulay2/benchmarks2/Makefile\
 Macaulay2/book/Makefile\
 Macaulay2/c/Makefile\
 Macaulay2/c2/Makefile\
 Macaulay2/d/Makefile\
 Macaulay2/dbm/Makefile\
 Macaulay2/dumpdata/Makefile\
 Macaulay2/e/Makefile\
 Macaulay2/emacs/Makefile\
 Macaulay2/experiments/Makefile\
 Macaulay2/gmp-macros/Makefile\
 Macaulay2/html/Makefile\
 Macaulay2/mike-slides/Makefile\
 Macaulay2/mike-tut/Makefile\
 Macaulay2/packages/Makefile\
 Macaulay2/schubert/Makefile\
 Macaulay2/screen/Makefile\
 Macaulay2/slides/Makefile\
 Macaulay2/socket/Makefile\
 Macaulay2/test/Makefile\
 Macaulay2/tex/Makefile\
 Macaulay2/texmacs/Makefile\
 Macaulay2/thread/Makefile\
 Macaulay2/tutorial/Makefile\
 Macaulay2/setup\
 Macaulay2/util/Makefile])
