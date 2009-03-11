#include <python2.5/Python.h>
#include "M2types.h"
#include <gc/gc.h>

void python_RunSimpleString(M2_string s) {
  char *t = tocharstar(s);
  PyRun_SimpleString(t);
  GC_FREE(t);
}
