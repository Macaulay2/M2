#include <Python.h>
#include "python-exports.h"

/* vendored from https://github.com/python/pythoncapi-compat
 * suppress warnings we can't fix */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include "pythoncapi_compat.h"
#pragma GCC diagnostic pop

#include <dlfcn.h>
#include <gmp.h>
#include <libgen.h>
#include <limits.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

const char * python_Initialize(char *executable)
{
#if PY_MAJOR_VERSION >= 3 && PY_MINOR_VERSION >= 8
  PyConfig config;
  PyStatus status;

  PyConfig_InitIsolatedConfig(&config);
  status = PyConfig_SetBytesString(&config, &config.executable, executable);
  if (PyStatus_Exception(status))
    goto exception;

  status = Py_InitializeFromConfig(&config);

exception:
  PyConfig_Clear(&config);

  if (PyStatus_Exception(status) != 0)
    return status.err_msg;
  else
    return NULL;
#else
  (void)executable;

  Py_Initialize();
  return NULL;
#endif
}

/* Look for the Python executable belonging to the same installation as the
 * library we're linked against, relative to the directory containing the
 * library.  We check, in order:
 *   bin       (macOS frameworks, e.g., .../Versions/3.x/Python)
 *   ../bin    (most installations, e.g., PREFIX/lib/libpython3.x.so)
 *   ../../bin (Debian multiarch, e.g., /usr/lib/<triplet>/libpython3.x.so)
 * Returns NULL if we can't find it, e.g., if libpython is linked statically. */
const char *python_DefaultExecutable(void)
{
  static const char *subdirs[] = {"bin", "../bin", "../../bin"};
  static char executable[PATH_MAX];
  char libpath[PATH_MAX];
  const char *libdir;
  Dl_info info;
  size_t i;

  if (dladdr((void *)Py_Initialize, &info) == 0 || info.dli_fname == NULL)
    return NULL;

  /* dirname may modify its argument */
  snprintf(libpath, sizeof(libpath), "%s", info.dli_fname);
  libdir = dirname(libpath);

  for (i = 0; i < sizeof(subdirs) / sizeof(subdirs[0]); i++) {
    snprintf(executable, sizeof(executable), "%s/%s/python%d.%d", libdir,
             subdirs[i], PY_MAJOR_VERSION, PY_MINOR_VERSION);
    if (access(executable, X_OK) == 0)
      return executable;
  }

  return NULL;
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
  (void)self;
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
  Py_DECREF(m);
}

/**********************
 * reference counting *
 **********************/

/* Python objects wrapped by Macaulay2 are released by a garbage collector
 * finalizer, which may run in any thread.  If the current thread doesn't
 * hold the GIL, then we queue the object up and release it later. */

static pthread_mutex_t pending_decrefs_mutex = PTHREAD_MUTEX_INITIALIZER;
static PyObject **pending_decrefs = NULL;
static size_t pending_decrefs_len = 0, pending_decrefs_cap = 0;

void python_ProcessPendingDecRefs(void)
{
  PyObject **objs;
  size_t len, i;

  pthread_mutex_lock(&pending_decrefs_mutex);
  objs = pending_decrefs;
  len = pending_decrefs_len;
  pending_decrefs = NULL;
  pending_decrefs_len = pending_decrefs_cap = 0;
  pthread_mutex_unlock(&pending_decrefs_mutex);

  /* Py_DECREF may run arbitrary Python code (and trigger more finalizers),
   * so we do it with the lock released */
  for (i = 0; i < len; i++)
    Py_DECREF(objs[i]);
  free(objs);
}

void python_DecRef(PyObject *obj)
{
  if (!Py_IsInitialized())
    return;

  if (PyGILState_Check()) {
    Py_DECREF(obj);
    python_ProcessPendingDecRefs();
    return;
  }

  pthread_mutex_lock(&pending_decrefs_mutex);
  if (pending_decrefs_len == pending_decrefs_cap) {
    size_t cap = pending_decrefs_cap ? 2 * pending_decrefs_cap : 16;
    PyObject **objs = realloc(pending_decrefs, cap * sizeof(PyObject *));
    if (objs == NULL) { /* leak rather than crash */
      pthread_mutex_unlock(&pending_decrefs_mutex);
      return;
    }
    pending_decrefs = objs;
    pending_decrefs_cap = cap;
  }
  pending_decrefs[pending_decrefs_len++] = obj;
  pthread_mutex_unlock(&pending_decrefs_mutex);
}

/*******************************
 * wrapped Macaulay2 functions *
 *******************************/

/* Python's memory isn't scanned by the garbage collector, so we keep the
 * method definition (which points to the name) and the function closure in
 * uncollectable memory, which is scanned but never freed by the collector.
 * We free it ourselves when Python releases the capsule. */

struct wrapped_m2_function {
  PyMethodDef def;
  parse_FunctionClosure f;
};

static void wrapped_m2_function_destructor(PyObject *capsule)
{
  GC_FREE(PyCapsule_GetPointer(capsule, NULL));
}

PyObject *python_WrapM2Function(char *name, parse_FunctionClosure f)
{
  struct wrapped_m2_function *w;
  PyObject *capsule, *r;

  w = GC_MALLOC_UNCOLLECTABLE(sizeof(struct wrapped_m2_function));
  if (w == NULL)
    return PyErr_NoMemory();
  w->def.ml_name = name;
  w->def.ml_meth = (PyCFunction)python_wrappedM2Function;
  w->def.ml_flags = METH_VARARGS;
  w->def.ml_doc = "wrapped Macaulay2 function";
  w->f = f;

  capsule = PyCapsule_New(w, NULL, wrapped_m2_function_destructor);
  if (capsule == NULL) {
    GC_FREE(w);
    return NULL;
  }

  /* PyCFunction_New takes its own reference to the capsule, so we release
   * ours (which also frees w if PyCFunction_New failed) */
  r = PyCFunction_New(&w->def, capsule);
  Py_DECREF(capsule);
  return r;
}

void *python_WrappedM2FunctionClosure(PyObject *self)
{
  struct wrapped_m2_function *w = PyCapsule_GetPointer(self, NULL);
  return w == NULL ? NULL : w->f;
}

PyObject *python_GetBuiltins(void)
{
  return PyEval_GetFrameBuiltins();
}

/********
 * ints *
 ********/

/* GMP <-> Python integer conversion routines from gmpy2
 * https://github.com/aleaxit/gmpy
 * Copyright 2000-2009 Alex Martelli
 * Copyright 2008-2025 Case Van Horsen
 * LGPL-3.0+ */

/* mpz_set_PyLong from src/gmpy2_convert_gmp.c */
int python_LongAsZZ(mpz_ptr z, PyObject *obj)
{
#ifndef PYPY_VERSION
    const PyLongLayout *layout = PyLong_GetNativeLayout();
    PyLongExport long_export = {0, 0, 0, 0, 0};

    if (PyLong_Export(obj, &long_export) < 0) {
        /* LCOV_EXCL_START */
        return -1;
        /* LCOV_EXCL_STOP */
    }
    if (long_export.digits) {
        mpz_import(z, long_export.ndigits, layout->digits_order,
                   layout->digit_size, layout->digit_endianness,
                   layout->digit_size*8 - layout->bits_per_digit,
                   long_export.digits);
        if (long_export.negative) {
            mpz_neg(z, z);
        }
        PyLong_FreeExport(&long_export);
    }
    else {
        const int64_t value = long_export.value;

        if (LONG_MIN <= value && value <= LONG_MAX) {
            mpz_set_si(z, value);
        }
        else {
            mpz_import(z, 1, -1, sizeof(int64_t), 0, 0, &value);
            if (value < 0) {
                mpz_t tmp;
                mpz_init(tmp);
                mpz_ui_pow_ui(tmp, 2, 64);
                mpz_sub(z, z, tmp);
                mpz_clear(tmp);
            }
        }
    }
    return 0;
#else
    int overflow;
    long value = PyLong_AsLongAndOverflow(obj, &overflow);
    if (!overflow) {
        mpz_set_si(z, value);
        return 0;
    }

    PyObject *s = PyNumber_ToBase(obj, 16);

    if (!s) {
        /* LCOV_EXCL_START */
        return -1;
        /* LCOV_EXCL_STOP */
    }

    const char *str = PyUnicode_AsUTF8(s), *p = str;

    if (!str) {
        /* LCOV_EXCL_START */
        Py_DECREF(s);
        return -1;
        /* LCOV_EXCL_STOP */
    }

    int negative = (str[0] == '-');

    p += 2;
    if (negative) {
        p++;
    }
    mpz_init_set_str(z, p, 16);
    Py_DECREF(s);
    if (negative) {
        mpz_neg(z, z);
    }
    return 0;
#endif
}

/* GMPy_PyLong_From_MPZ from src/gmpy2_convert_gmp.c */
/* replace obj->z with z when updating */
PyObject *python_LongFromZZ(mpz_srcptr z)
{
    if (mpz_fits_slong_p(z)) {
        return PyLong_FromLong(mpz_get_si(z));
    }

#ifndef PYPY_VERSION
    const PyLongLayout *layout = PyLong_GetNativeLayout();
    size_t size = (mpz_sizeinbase(z, 2) +
                   layout->bits_per_digit - 1)/layout->bits_per_digit;
    void *digits;
    PyLongWriter *writer = PyLongWriter_Create(mpz_sgn(z) < 0, size,
                                               &digits);
    if (writer == NULL) {
        /* LCOV_EXCL_START */
        return NULL;
        /* LCOV_EXCL_STOP */
    }

    mpz_export(digits, NULL, layout->digits_order, layout->digit_size,
               layout->digit_endianness,
               layout->digit_size*8 - layout->bits_per_digit, z);
    return PyLongWriter_Finish(writer);
#else
    PyObject *str = GMPy_PyStr_From_MPZ(obj, 16, 0, NULL);

    if (!str) {
        /* LCOV_EXCL_START */
        return NULL;
        /* LCOV_EXCL_STOP */
    }

    PyObject *res = PyLong_FromUnicodeObject(str, 16);

    Py_DECREF(str);
    return res;
#endif
}

#if 0
Local Variables:
compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d python-c.o "
End:
#endif
