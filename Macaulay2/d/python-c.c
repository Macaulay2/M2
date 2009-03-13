#include <python2.5/Python.h>
#include "M2types.h"
#include <gc/gc.h>

void python_RunSimpleString(M2_string s) {
  char *t = tocharstar(s);
  PyRun_SimpleString(t);
  GC_FREE(t);
}

int python_Main() {
  static char pn[3] = "M2";
  static char *argv[2] = {pn,NULL};
  static int argc = 1;
  return Py_Main(argc,argv);
}
