# `system.d` — POSIX / OS bindings

`system.d` is the **OS-bindings layer** — wraps POSIX calls
(`stat`, `chdir`, `time`, `fork`, `mkdir`, signal handling, ...)
into `.d`-callable functions.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
--		Copyright 1994 by Daniel R. Grayson

use M2;
header "
#ifdef HAVE_SYS_TYPES_H
 #include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
 #include <sys/stat.h>
#endif
#ifdef HAVE_TIME_H
 #include <time.h>
#endif
#ifdef HAVE_ASSERT_H
 #include <assert.h>
#endif
#ifdef HAVE_PTHREAD_H
 #include <pthread.h>
#endif
#ifdef HAVE_SYS_WAIT_H
 #include <sys/wait.h>
#endif
";
```

The `#ifdef HAVE_*` blocks make `system.d` build cleanly on systems
missing any of these headers — the autoconf / cmake machinery in
[`../../include/`](../../include/README.md) sets the `HAVE_*` macros.

## What `system.d` wraps

- **File-system** — `stat`, `chdir`, `mkdir`, `unlink`, `rmdir`,
  `link`, `symlink`, `readlink`, `chmod`.
- **Process** — `fork`, `exec*`, `wait*`, `kill`, `getpid`,
  `getppid`.
- **Time** — `time`, `gettimeofday`, `clock_gettime`, `nanosleep`.
- **Environment** — `getenv`, `setenv`, `unsetenv`.
- **Signals** — `signal`, `sigaction`, `sigprocmask`.
- **POSIX threads** — basic locking (sister file
  [`pthread0.d`](README.md) goes deeper).

Each binding follows the same shape: declare a wrapper, call
`Ccode(...)` to drop into C, return an M2-shaped result.

## Why "system" instead of separate `os.d`, `fs.d`, ...

The pattern in `.d` is to keep one module per syscall *header* —
splitting further would create a fan-out of tiny files. Big OS
modules like `system.d` are easier to maintain in practice.

## Used by

- Most `actors*.d` files that expose OS-level operations to M2 —
  `cd`, `mkdir`, `time`, `currentTime`, etc.
- [`file-evaluate.md`](file-evaluate.md) — uses `time` for the
  `--check` timeouts.
- Engine boundary in some paths via
  [`file-engine-dd.md`](file-engine-dd.md).
- The supervisor in [`../system/README.md`](../system/README.md)
  exchanges POSIX state with the interpreter through here.

## Related

- [`README.md`](README.md) — d/ overview.
- [`../../include/README.md`](../../include/README.md) —
  `HAVE_*` macros set up here.
- [`../system/README.md`](../system/README.md) — supervisor's own
  POSIX use.
- `pthread.d` / `pthread0.d` — sister threading bindings.
- [`../m2/file-system.md`](../m2/file-system.md) — M2-side wrapper
  for these primitives.
