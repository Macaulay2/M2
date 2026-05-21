# `python.d` — CPython embedding

`python.d` provides M2's **CPython bindings** — letting M2 call
into Python code, manipulate Python objects, and embed a Python
interpreter inside M2.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
-- Copyright 2009,2010 by Daniel R. Grayson
use common;
use util;
use evaluate;

declarations "#include <Python.h>";

WrongArgPythonObject():Expr := WrongArg("a python object");
```

The single line `declarations "#include <Python.h>"` pulls in
CPython's full public API.

## What's exposed

- **`PythonObject`** — the M2 type wrapping a `PyObject *`.
- **Conversion** — `toPython(M2value)`, `value PythonObject`.
- **Attribute access** — `obj.attr`, `obj.method(args)`.
- **Module import** — `import` (Python's, exposed to M2).
- **Evaluation** — `runPythonString(...)`.

A typical M2 use of Python:

```m2
needsPackage "PythonInterpreter";
np = import "numpy"
arr = np.array {1, 2, 3, 4}
print(np.linalg.norm arr)
```

## Companion files

- **`python-c.c`** — C-side helpers that interact directly with
  `<Python.h>`.
- **`pythoncapi_compat.h`** — compatibility shims across CPython
  versions.

## Used by

- The `PythonInterpreter` user package wraps `python.d` for the
  end-user API.
- M2 users embedding Python (typically for numpy / scipy access).

## Related

- [`README.md`](README.md) — d/ overview.
- `python-c.c` — C-side helpers.
- CPython — external linked library.
- M2's `--with-python` configure option enables this code path.
- Sister FFI files: [`file-ffi.md`](file-ffi.md),
  [`file-mysql.md`](file-mysql.md), [`file-xml.md`](file-xml.md).
