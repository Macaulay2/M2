// (c) 1997 Michael E. Stillman

#ifndef _error_h_
#define _error_h_

#if defined(__cplusplus)
extern "C" {
#endif

  // under mingw32 an include file defines ERROR as 0
  #undef ERROR

  void ERROR(const char *s,...);
  void INTERNAL_ERROR(const char *s, ...); /* Exits the program with an error code */
  int error(); /* returns 0 for false, 1 for true */
  const char *error_message();

#if defined(__cplusplus)
}
#endif

#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
