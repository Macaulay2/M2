#if defined(__APPLE__) && defined(__MACH__)
     {
     extern char *get_etext(), *get_end();
     GC_add_roots(get_etext(),get_end());
     }
#endif
