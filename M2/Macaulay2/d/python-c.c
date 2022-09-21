#include <Python.h>
#include "python-exports.h"

#include <gmp.h>

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

/* simplified to return -1, 0, or 1 rather than a PyObject or NULL */
int python_ErrOccurred(void) {
	if (PyErr_ExceptionMatches(PyExc_StopIteration)) {
		PyErr_Clear();
		return -1;
	} else
		return (PyErr_Occurred() != NULL);
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

/********
 * ints *
 ********/

/* GMP <-> Python integer conversion routines from gmpy2
 * https://github.com/aleaxit/gmpy
 * Copyright 2000-2009 Alex Martelli
 * Copyright 2008-2022 Case Van Horsen
 * LGPL-3.0+ */

mpz_ptr python_LongAsZZ(mpz_ptr z, PyObject *obj)
{
  int negative;
  Py_ssize_t len;
  PyLongObject *templong;

  templong = (PyLongObject *)obj;

  switch (Py_SIZE(templong)) {
  case -1:
    mpz_set_si(z, -(sdigit)templong->ob_digit[0]);
    break;
  case 0:
    mpz_set_si(z, 0);
    break;
  case 1:
    mpz_set_si(z, templong->ob_digit[0]);
    break;
  default:
    mpz_set_si(z, 0);

    if (Py_SIZE(templong) < 0) {
      len = -Py_SIZE(templong);
      negative = 1;
    } else {
      len = Py_SIZE(templong);
      negative = 0;
    }

    mpz_import(z, len, -1, sizeof(templong->ob_digit[0]), 0,
	       sizeof(templong->ob_digit[0])*8 - PyLong_SHIFT,
	       templong->ob_digit);

    if (negative)
      mpz_neg(z, z);
  }

  return z;
}

PyObject *python_LongFromZZ(mpz_srcptr z)
{
  int negative;
  size_t count, size;
  PyLongObject *result;

  if (mpz_sgn(z) < 0)
    negative = 1;
  else
    negative = 0;

  size = (mpz_sizeinbase(z, 2) + PyLong_SHIFT + 1) / PyLong_SHIFT;
  result = _PyLong_New(size);
  if (!result)
    return NULL;

  mpz_export(result->ob_digit, &count, -1, sizeof(result->ob_digit[0]), 0,
	     sizeof(result->ob_digit[0]) * 8 - PyLong_SHIFT, z);

  if (count == 0)
    result->ob_digit[0] = 0;

  while ((size > 0) && (result->ob_digit[size - 1] == 0))
    size--;

#if PY_VERSION_HEX >= 0x030900A4
  Py_SET_SIZE(result, size);
#else
  Py_SIZE(result) = size;
#endif

  if (negative) {
#if PY_VERSION_HEX >= 0x030900A4
    Py_SET_SIZE(result, - Py_SIZE(result));
#else
    Py_SIZE(result) = - Py_SIZE(result);
#endif
  }

  return (PyObject*)result;
}

#if 0
Local Variables:
compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d python-c.o "
End:
#endif
