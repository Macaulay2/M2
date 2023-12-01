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
 * Copyright 2008-2023 Case Van Horsen
 * LGPL-3.0+ */

/* #define's from src/gmpy2_convert.h */
#if PY_VERSION_HEX >= 0x030C0000
#  define TAG_FROM_SIGN_AND_SIZE(is_neg, size) ((is_neg?2:(size==0)) | (((size_t)size) << 3))
#  define _PyLong_SetSignAndDigitCount(obj, is_neg, size) (obj->long_value.lv_tag = TAG_FROM_SIGN_AND_SIZE(is_neg, size))
#elif PY_VERSION_HEX >= 0x030900A4
#  define _PyLong_SetSignAndDigitCount(obj, is_neg, size) (Py_SET_SIZE(obj, (is_neg?-1:1)*Py_SIZE(result)))
#else
#  define _PyLong_SetSignAndDigitCount(obj, is_neg, size) (Py_SIZE(obj) = (is_neg?-1:1)*Py_SIZE(result))
#endif

#if PY_VERSION_HEX >= 0x030C0000
#  define GET_OB_DIGIT(obj) obj->long_value.ob_digit
#  define _PyLong_IsNegative(obj) ((obj->long_value.lv_tag & 3) == 2)
#  define _PyLong_DigitCount(obj) (obj->long_value.lv_tag >> 3)
#else
#  define GET_OB_DIGIT(obj) obj->ob_digit
#  define _PyLong_IsNegative(obj) (Py_SIZE(obj) < 0)
#  define _PyLong_DigitCount(obj) (_PyLong_IsNegative(obj)? -Py_SIZE(obj):Py_SIZE(obj))
#endif

/* mpz_set_PyLong from src/gmpy2_convert_gmp.c */
void python_LongAsZZ(mpz_ptr z, PyObject *obj)
{
  int negative;
  Py_ssize_t len;
  PyLongObject *templong = (PyLongObject*)obj;

  len = _PyLong_DigitCount(templong);
  negative = _PyLong_IsNegative(templong);

  switch (len) {
  case 1:
    mpz_set_si(z, (sdigit)GET_OB_DIGIT(templong)[0]);
    break;
  case 0:
    mpz_set_si(z, 0);
    break;
  default:
    mpz_import(z, len, -1, sizeof(GET_OB_DIGIT(templong)[0]), 0,
	       sizeof(GET_OB_DIGIT(templong)[0])*8 - PyLong_SHIFT,
	       GET_OB_DIGIT(templong));
  }

  if (negative) {
    mpz_neg(z, z);
  }
  return;
}

/* GMPy_PyLong_From_MPZ from src/gmpy2_convert_gmp.c */
/* replace obj->z with z when updating */
PyObject *python_LongFromZZ(mpz_srcptr z)
{
  int negative;
  size_t count, size;
  PyLongObject *result;

  /* Assume gmp uses limbs as least as large as the builtin longs do */

  negative = mpz_sgn(z) < 0;
  size = (mpz_sizeinbase(z, 2) + PyLong_SHIFT - 1) / PyLong_SHIFT;

  if (!(result = _PyLong_New(size))) {
    /* LCOV_EXCL_START */
    return NULL;
    /* LCOV_EXCL_STOP */
  }

  mpz_export(GET_OB_DIGIT(result), &count, -1, sizeof(GET_OB_DIGIT(result)[0]), 0,
	     sizeof(GET_OB_DIGIT(result)[0])*8 - PyLong_SHIFT, z);

  if (count == 0) {
    GET_OB_DIGIT(result)[0] = 0;
  }

  /* long_normalize() is file-static so we must reimplement it */
  /* longobjp = long_normalize(longobjp); */
  while ((size>0) && (GET_OB_DIGIT(result)[size-1] == 0)) {
    size--;
  }

  _PyLong_SetSignAndDigitCount(result, negative, size);
  return (PyObject*)result;
}

#if 0
Local Variables:
compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d python-c.o "
End:
#endif
