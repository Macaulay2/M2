// (c) 1997 Michael E. Stillman

#ifndef _error_h_
#define _error_h_

#if defined(__cplusplus)
extern "C" {
#endif

  void ERROR(const char *s,...);
  void INTERNAL_ERROR(const char *s, ...); /* Exits the program with an error code */
  int error(); /* returns 0 for false, 1 for true */
  char *error_message();

#if defined(__cplusplus)
}
#endif

#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
*/
