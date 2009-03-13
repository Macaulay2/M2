#include <python2.5/Python.h>
#include "M2types.h"
#include <gc/gc.h>

int python_RunSimpleString(M2_string s) {
  char *t = tocharstar(s);
  int ret = PyRun_SimpleString(t);
  GC_FREE(t);
  return ret;
}

PyObject *globals, *locals;

static void init() {
  if (!globals) {
#if 0    
    globals = PyEval_GetGlobals(); /* this returns null because no frame is currently executing */
#elif 1
    globals = PyDict_New();
    PyDict_SetItemString(globals, "__builtins__", PyEval_GetBuiltins());
#else
    globals = PyDict_New();
    PyRun_String("import __builtin__ as __builtins__",Py_eval_input, globals, locals);
#endif
  }
}

PyObject *python_RunString(M2_string s) {
  char *t = tocharstar(s);
  init();
  PyObject *ret = PyRun_String(t,Py_eval_input,globals,locals);
  GC_FREE(t);
  if (PyErr_Occurred()) {
    fprintf(stderr,"PyErr_Occurred()\n");
#if 1
    PyErr_Print();
    return NULL;
#else
    PyObject *type, *value, *traceback;
    PyErr_Fetch(&type,&value,&traceback);
    return value;
#endif
  }
  return ret;
}

int python_Main() {
  static char pn[3] = "M2";
  static char *argv[2] = {pn,NULL};
  static int argc = 1;
  return Py_Main(argc,argv);
}
