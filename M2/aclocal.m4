
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
  then AC_DEFINE($2_IS_DECLARED,,whether $2 is declared in errno.h or stdio.h)
  fi
])
