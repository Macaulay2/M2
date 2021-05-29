#include <Python.h>
#include "python-exports.h"

int python_RunSimpleString(M2_string s) {
  char *t = M2_tocharstar(s);
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
  char *t = M2_tocharstar(s);
  init();
  PyObject *ret = PyRun_String(t,Py_eval_input,globals,locals);
  GC_FREE(t);
  if (PyErr_Occurred()) {
#if 1
    PyErr_Print();
    return NULL;
#else
    PyObject *type, *value, *traceback;
    PyErr_Fetch(&type,&value,&traceback);
    return value;		/* this is not such a great thing to do */
#endif
  }
#if 0
  if (ret) {
    PyObject *str = PyObject_Str(ret);
    fprintf(stderr, "runString: %s\n", PyString_AS_STRING(str));
  }
#endif
  return ret;
}

int python_Main() {
  static wchar_t pn[3] = L"M2";
  static wchar_t *argv[2] = {pn,NULL};
  static int argc = 1;
  return Py_Main(argc,argv);
}

PyObject *python_SysGetObject(M2_string s) {
  char *t = M2_tocharstar(s);
  PyObject *ret = PySys_GetObject(t);
  GC_FREE(t);
  return ret;
}

PyObject *python_ObjectType(PyObject *o) {
  return PyObject_Type(o);
}

/* see http://docs.python.org/extending/extending.html for this example */

static PyObject * spam_system(PyObject *self, PyObject *args) {
  const char *command;
  int sts;
  if (!PyArg_ParseTuple(args, "s", &command)) return NULL;
  sts = system(command);
  return Py_BuildValue("i", sts);
}
static PyObject *SpamError;
static PyMethodDef SpamMethods[] = {
  {"system",  spam_system, METH_VARARGS, "Execute a shell command."},
  {NULL, NULL, 0, NULL}
};
static struct PyModuleDef moduledef = {
  PyModuleDef_HEAD_INIT, "spam", NULL, -1, SpamMethods, NULL, NULL, NULL, NULL}
;
void python_initspam() {
  static char name[] = "spam.error";
  PyObject *m;
  m = PyModule_Create(&moduledef);
  if (m == NULL) return;
  SpamError = PyErr_NewException(name, NULL, NULL);
  Py_INCREF(SpamError);
  PyModule_AddObject(m, "error", SpamError);
}

PyObject *python_NumberAdd(PyObject *o1, PyObject *o2) {
	return PyNumber_Add(o1, o2);
}

PyObject *python_NumberSubtract(PyObject *o1, PyObject *o2) {
	return PyNumber_Subtract(o1, o2);
}

PyObject *python_NumberMultiply(PyObject *o1, PyObject *o2) {
	return PyNumber_Multiply(o1, o2);
}

PyObject *python_NumberTrueDivide(PyObject *o1, PyObject *o2) {
	return PyNumber_TrueDivide(o1, o2);
}

#if 0
Local Variables:
compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d python-c.o "
End:
#endif
