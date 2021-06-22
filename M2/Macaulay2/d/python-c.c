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

/**************
 * exceptions *
 **************/

/* simplified to return 1 or 0 rather than a PyObject or NULL */
int python_ErrOccurred(void) {
	if (PyErr_ExceptionMatches(PyExc_StopIteration)) {
		PyErr_Clear();
		return 0;
	} else
		return (PyErr_Occurred() != NULL);
}

void python_ErrPrint(void) {
	PyErr_Print();
}

PyObject *python_RunString(M2_string s) {
  char *t = M2_tocharstar(s);
  init();
  PyObject *ret = PyRun_String(t,Py_eval_input,globals,locals);
  GC_FREE(t);
  return ret;
}

int python_Main() {
  static wchar_t pn[3] = L"M2";
  static wchar_t *argv[2] = {pn,NULL};
  static int argc = 1;
  return Py_Main(argc,argv);
}

/***********
 * objects *
 ***********/

PyObject *python_ObjectType(PyObject *o) {
  return PyObject_Type(o);
}

int python_ObjectRichCompareBool(PyObject *o1, PyObject *o2, int opid) {
	return PyObject_RichCompareBool(o1, o2, opid);
}

int python_ObjectHasAttrString(PyObject *o, char *attr) {
	return PyObject_HasAttrString(o, attr);
}

PyObject *python_ObjectGetAttrString(PyObject *o, char *attr) {
	return PyObject_GetAttrString(o, attr);
}

int python_ObjectSetAttrString(PyObject *o, char *attr_name, PyObject *v) {
	return PyObject_SetAttrString(o, attr_name, v);
}

PyObject *python_ObjectStr(PyObject *o) {
	return PyObject_Str(o);
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

/*********
 * bools *
 *********/

PyObject *python_True = Py_True;
PyObject *python_False = Py_False;

/********
 * ints *
 ********/

long python_LongAsLong(PyObject *o) {
	return PyLong_AsLong(o);
}

PyObject *python_LongFromLong(long v) {
	return PyLong_FromLong(v);
}

/**********
 * floats *
 **********/

double python_FloatAsDouble(PyObject *o) {
	return PyFloat_AsDouble(o);
}

PyObject *python_FloatFromDouble(double v) {
	return PyFloat_FromDouble(v);
}

/*************
 * complexes *
 *************/

PyObject* python_ComplexFromDoubles(double real, double imag) {
	return PyComplex_FromDoubles(real, imag);
}

/***********
 * strings *
 ***********/

const char *python_UnicodeAsUTF8(PyObject *o) {
	return PyUnicode_AsUTF8(o);
}

PyObject *python_UnicodeFromString(char *u) {
	return PyUnicode_FromString(u);
}

PyObject *python_UnicodeConcat(PyObject *o1, PyObject *o2) {
	return PyUnicode_Concat(o1, o2);
}

/**********
 * tuples *
 **********/

PyObject *python_TupleNew(int n) {
	return PyTuple_New(n);
}

int python_TupleSetItem(PyObject *L, int i, PyObject *item) {
	return PyTuple_SetItem(L, i, item);
}

/*********
 * lists *
 *********/

PyObject *python_ListNew(int n) {
	return PyList_New(n);
}

int python_ListSetItem(PyObject *L, int i, PyObject *item) {
	return PyList_SetItem(L, i, item);
}

/****************
 * dictionaries *
 ****************/

PyObject *python_DictNew(void) {
	return PyDict_New();
}

int python_DictSetItem(PyObject *p, PyObject *key, PyObject *val) {
	return PyDict_SetItem(p, key, val);
}

/********
 * sets *
 ********/

PyObject *python_SetNew(PyObject *o) {
	return PySet_New(o);
}

/*************
 * callables *
 *************/

PyObject *python_ObjectCall(PyObject *o, PyObject *args, PyObject *kwargs) {
	return PyObject_Call(o, args, kwargs);
}

/********
 * none *
 ********/

PyObject *python_None = Py_None;

/*************
 * importing *
 *************/

PyObject *python_ImportImportModule(char *name) {
	return PyImport_ImportModule(name);
}

#if 0
Local Variables:
compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d python-c.o "
End:
#endif
