#include <stdio.h>
#include <dlfcn.h>

void setup_readline() {
  char libname[] = "libreadline.so.4";
  char funname[] = "readline";
  char *(*readline) (char *prompt);
  static void *handle;
  if (handle == NULL) {
    handle = dlopen(libname, RTLD_LAZY);
    if (handle == NULL) {
      fprintf(stderr,"can't load library %s (%s)\n", libname, dlerror());
      return;
    }
    readline = dlsym(handle, funname);
    if (readline == NULL) {
      fprintf(stderr,"can't link function %s from library %s (%s)\n",
	      funname, libname, dlerror());
      return;
    }
  }
  else {
    fprintf(stderr,"library %s already loaded\n", libname);
  }
  /* printf ("%s() = %s\n", funname, readline(">>> ")); */
  /* dlclose(handle); */
}
