# `M2/include/valgrind/` — bundled Valgrind client headers

Copies of Valgrind's public client-request headers. These let M2 (when built
with Valgrind support) embed Valgrind macros — `VALGRIND_DO_LEAK_CHECK`,
`VALGRIND_PRINTF`, memory-tracking annotations, etc. — without requiring
Valgrind's development headers to be installed on the build machine.

| File | Role |
|---|---|
| `valgrind.h` | The umbrella client header |
| `memcheck.h` | Memcheck-specific client requests (memory tracking, defined-bits, leak checks) |

These are upstream Valgrind files vendored unchanged. They have no effect at
runtime unless the program is run under Valgrind.

## Related

- [`../../files/M2-suppressions.supp`](../../files/README.md) — M2-specific
  Valgrind suppressions consumed at run time.
- [`../../BUILD/docker/valgrind/`](../../BUILD/README.md) — a Docker build
  flavour for running M2 under Valgrind.
