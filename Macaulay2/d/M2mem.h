#if defined(__cplusplus)
extern "C" {
#endif
  extern void outofmem(void);
  extern char *getmem(unsigned int);
  extern char *getmem_clear(unsigned int);
  extern char *getmem_atomic(unsigned int);
  extern char *getmem_malloc(unsigned int);
  extern char *getmem_atomic_clear(unsigned int);
#if defined(__cplusplus)
}
#endif

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
