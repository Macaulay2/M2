# M2 startup sequence

This document walks **end-to-end through M2's boot path** — from
the kernel calling `main()` to the first `i1 :` prompt appearing.
Synthesises information scattered across the per-file docs into
one linear narrative.

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) · [Tour](TOUR.md)

## The eight phases

```
1. kernel → M2 wrapper script → M2-binary
2. C runtime → main()
3. GC initialisation
4. Library version assertions
5. Engine initialisation (IM2_initialize)
6. Supervisor initialisation
7. Interpreter initialisation (interp_init)
8. Run embedded startup.m2 → load Core via loadsequence
9. REPL ready, first prompt shown
```

Each phase has its own failure mode and its own doc(s). The
sections below trace each step with pointers into the deep-dive
layer.

## Phase 1: Wrapper script → binary

`M2` (the command users type) is **not the main executable** on
Linux/macOS — it's a tiny wrapper that sets library paths and
execs the real binary.

**Source**:
[`M2/Macaulay2/bin/file-M2-in.md`](M2/Macaulay2/bin/file-M2-in.md).

```sh
#! /bin/sh
case "$host_os" in
   darwin*) export DYLD_LIBRARY_PATH=`dirname "$0"`/../lib/...:$DYLD_LIBRARY_PATH ;;
        *) export LD_LIBRARY_PATH=`dirname "$0"`/../lib/...:$LD_LIBRARY_PATH ;;
esac
exec `dirname "$0"`/M2-binary "$@"
```

**Why it exists**: M2 ships bundled copies of GMP / MPFR / FLINT
/ etc. The wrapper makes them findable without the user
configuring `LD_LIBRARY_PATH` themselves.

**Failure mode**: if you run `M2` and immediately get "library
not found" errors, the wrapper isn't running or `LD_LIBRARY_PATH`
isn't propagating. Check `$(which M2)` and `head -5 $(which M2)`.

## Phase 2: main()

The actual executable's entry point lives in `bin/main.cpp`.

**Source**:
[`M2/Macaulay2/bin/file-main.md`](M2/Macaulay2/bin/file-main.md).

Order of work in `main()`:

```cpp
int main(int argc, char **argv) {
    // 1. GC_INIT() — must come before any allocation
    GC_INIT();

    // 2. Library version assertions
    assert_mpfr_version();
    assert_gmp_version();
    ...

    // 3. Engine bootstrap
    IM2_initialize();

    // 4. Boost stacktrace handler for crashes
    install_stacktrace_handler();

    // 5. Interpreter init
    interp_init(argc, argv);

    // 6. Top-level REPL
    return interp_topLevel(argc, argv);
}
```

The order matters: GC must initialise before any allocating code
runs; engine globals must initialise before the interpreter tries
to use them; the stacktrace handler must register before
errors can occur in the interpreter.

**Failure mode**: SIGSEGV at start usually means a library was
linked with a different version than M2 was built against —
e.g., FLINT or MPFR ABI mismatch.

## Phase 3: GC initialisation

`GC_INIT()` from `<gc.h>` (Boehm-Demers-Weiser).

**Sources**: [`GLOSSARY.md`](GLOSSARY.md#boehm-gc--bdwgc),
[`M2/submodules/file-submodules.md`](M2/submodules/file-submodules.md)
(bdwgc submodule).

Once initialised, every subsequent `getmem(n)` /
`GC_malloc(n)` call routes through the collector. M2's engine
classes that inherit from `our_new_delete` (in
[`M2/Macaulay2/e/file-newdelete.md`](M2/Macaulay2/e/file-newdelete.md))
automatically use the GC.

**Why it must come first**: any `new`/`malloc` before
`GC_INIT()` creates "untracked" allocations the GC won't scan.
Lost roots → premature collection → crashes much later.

## Phase 4: Library version assertions

```cpp
if (strcmp(mpfr_get_version(), MPFR_VERSION_STRING) != 0) {
    fprintf(stderr, "MPFR version mismatch\n");
    exit(1);
}
```

(Approximate — actual code in `bin/main.cpp`.)

M2 asserts that the linked **MPFR / GMP / FLINT / NTL** versions
match the headers it was built against. Strict version checks
because:

- Mathematical libraries often have **ABI-breaking releases**.
- A subtle ABI mismatch (different struct layout for `mpz_t`)
  produces silent data corruption.
- Better to refuse to start than produce wrong answers.

**Failure mode**: "GMP version mismatch" or similar at startup
means the system library is at a version different from what M2
was built against. Reinstall M2 against the current libs.

## Phase 5: Engine initialisation — `IM2_initialize()`

**Source**:
[`M2/Macaulay2/e/file-engine-h.md`](M2/Macaulay2/e/file-engine-h.md),
[`M2/Macaulay2/e/architecture.md`](M2/Macaulay2/e/architecture.md).

`IM2_initialize()` sets up the engine's global state:

- **Pre-built rings**: `ZZ`, `QQ`, `RR`, `CC` get singleton
  instances.
- **Default monoid prefabs**: empty monoid, degree-1 monoid.
- **Hash-counter state**: `MutableEngineObject::mNextMutableHashValue`.
- **Geobucket size table**: `heap_size[GEOHEAP_SIZE]` (see
  [`M2/Macaulay2/e/file-geobucket.md`](M2/Macaulay2/e/file-geobucket.md)).
- **Random number generator seed**.

After this, every engine-side call (`rawMatrix`, `rawGB`, etc.)
has the globals it needs. The function is also called from the
gtest binary
([`M2/Macaulay2/e/unit-tests/file-test-harness.md`](M2/Macaulay2/e/unit-tests/file-test-harness.md))
before any test runs.

**Failure mode**: engine assert-fail or crash here usually means
a broken build (one engine source file was rebuilt with stale
headers).

## Phase 6: Supervisor initialisation

**Source**:
[`M2/Macaulay2/system/architecture.md`](M2/Macaulay2/system/architecture.md).

The supervisor:

- Detects core count.
- Spawns the worker thread pool (`clamp(4, 16, numCores) + 1`
  threads).
- Registers each worker with Boehm GC (so the GC's stop-the-world
  pause works).
- Initialises mutex / condvar state.

After this, M2's `Task` / `schedule` / `taskResult` API is
functional (though no user-level tasks have run yet).

**Failure mode**: if `numCores` reports zero (very rare,
container quirks), the supervisor falls back to a single-thread
pool. Otherwise this phase rarely fails.

## Phase 7: Interpreter initialisation — `interp_init()`

**Sources**:
[`M2/Macaulay2/d/file-M2lib.md`](M2/Macaulay2/d/file-M2lib.md),
[`M2/Macaulay2/d/file-interp.md`](M2/Macaulay2/d/file-interp.md),
[`M2/Macaulay2/d/architecture.md`](M2/Macaulay2/d/architecture.md).

The interpreter setup:

- **Read command-line arguments** — `--script`, `--no-prompt`,
  `--check`, etc.
- **Open stdin/stdout/stderr** as `M2File` objects (per-thread
  state — see
  [`M2/Macaulay2/system/file-m2file.md`](M2/Macaulay2/system/file-m2file.md)).
- **Initialise lexer state** — character classification table from
  [`M2/Macaulay2/d/file-ctype.md`](M2/Macaulay2/d/file-ctype.md).
- **Initialise symbol tables** — empty global dictionary.
- **Bind built-in operators** — every `+`, `*`, `==`, ... from
  `actors*.d` ([`M2/Macaulay2/d/file-actors.md`](M2/Macaulay2/d/file-actors.md)).
- **Bind FFI entries** — Python, libxml2, libffi, JSON, MySQL,
  ... if compiled in (see
  [`M2/Macaulay2/d/architecture.md`](M2/Macaulay2/d/architecture.md)).

After this, the interpreter can lex, parse, and evaluate M2
expressions — but only the **builtin** operators work. No user-
level types (`Module`, `Matrix`, etc.) yet; those come from
Core M2.

**Failure mode**: error like "unrecognised operator" or "unknown
class" at this stage means an `actors*.d` file failed to wire up
correctly.

## Phase 8: Run embedded startup.m2 → load Core

**Sources**:
[`M2/Macaulay2/bin/file-startup.md`](M2/Macaulay2/bin/file-startup.md),
[`M2/Macaulay2/m2/file-loadsequence.md`](M2/Macaulay2/m2/file-loadsequence.md),
[`M2/Macaulay2/m2/architecture.md`](M2/Macaulay2/m2/architecture.md).

The M2 binary has **`startup.m2` embedded as a C string**
(see [`bin/startup.c.cmake`](M2/Macaulay2/bin/file-startup.md)).
The interpreter starts executing this embedded source.

`startup.m2` runs the `loadsequence` script
([`M2/Macaulay2/m2/file-loadsequence.md`](M2/Macaulay2/m2/file-loadsequence.md)),
which `load`s each `.m2` file in
[`M2/Macaulay2/m2/`](M2/Macaulay2/m2/architecture.md) in
canonical order.

Approximate load order (~100 files):

```
1. classes.m2           — type system
2. methods.m2           — method dispatch
3. expressions.m2       — Expression AST + format
4. option.m2            — keyword args
5. packages.m2          — package machinery
6. ring.m2              — Ring constructors
7. matrix.m2            — Matrix constructors
8. module.m2            — Module constructors
9. gb.m2                — Gröbner basis API
10. res.m2              — resolution API
11. hilbert.m2          — Hilbert function
12. ...                 — ~90 more files
N.  Core.m2             — finalise the Core package
```

Each file defines types, methods, and constants. By the end, the
**Core package** has thousands of symbols defined — every
user-visible type, every built-in function.

**Failure mode**: an error here is usually one of:

- A typo in a recently-edited `.m2` file (M2 prints "syntax error
  in startup.m2 at line ...").
- A type-system invariant violated (M2 prints "...assertion
  failed in classes.m2 at line ...").
- A package's documentation block didn't load (the
  `beginDocumentation()` marker is missing or misplaced).

Recovery: pass `--no-debug` to skip the assert + print the error
verbosely.

## Phase 9: REPL ready

**Source**:
[`M2/Macaulay2/d/file-interp.md`](M2/Macaulay2/d/file-interp.md).

The interpreter prints its banner:

```
Macaulay2, version 1.26.05
with packages: ConwayPolynomials, Elimination, IntegralClosure,
               InverseSystems, Isomorphism, LLLBases, MinimalPrimes,
               OnlineLookup, PrimaryDecomposition, ReesAlgebra,
               Saturation, TangentCone, Truncations, Varieties

i1 : 
```

The version comes from
[`M2/file-VERSION.md`](M2/file-VERSION.md). The
auto-loaded-packages list comes from `=distributed-packages`
([`M2/Macaulay2/packages/README.md`](M2/Macaulay2/packages/README.md)).

The REPL loop:

1. Show prompt `iN : `.
2. Read a line (via [`M2/Macaulay2/d/file-getline.md`](M2/Macaulay2/d/file-getline.md)).
3. Lex + parse + bind + evaluate via the
   [interpreter pipeline](M2/Macaulay2/d/architecture.md).
4. Format the result and show `oN = ...`.
5. Increment `N`, repeat.

The interpreter is now **fully alive**.

## Special startup modes

M2 supports several non-REPL startup modes:

| Flag | Behaviour |
|---|---|
| `--script FILE` | Run `FILE`, exit. No prompt. |
| `--no-prompt` | Suppress `iN : ` prompts (for shell-pipe use). |
| `--check N` | Run the level-N test suite, exit. |
| `--texmacs` | Output in TeXmacs's data-message protocol (see [`M2/Macaulay2/d/file-texmacs.md`](M2/Macaulay2/d/file-texmacs.md)). |
| `--version` | Print version string and exit. |
| `--help` | Print usage info and exit. |
| `-q` | Skip loading user init file (`~/.Macaulay2/init.m2`). |

Each takes the same boot path through phases 1-8, then diverges
at phase 9.

## When startup is slow

A cold start (no caches) takes ~500ms-1s on a modern machine.
Where the time goes:

| Phase | Approximate time |
|---|---|
| Phases 1-4 (startup + GC + version checks) | < 10ms |
| Phase 5 (engine init) | ~50ms |
| Phase 6 (supervisor) | ~10ms |
| Phase 7 (interpreter init) | ~100ms |
| Phase 8 (Core load, ~100 files) | ~300-500ms |
| Phase 9 (banner + prompt) | < 10ms |

The Core load dominates because it executes ~100 `.m2` files
each defining methods. The `-q` flag doesn't speed this up; Core
must always load.

**Speed optimisations**:

- Pre-built `info` / HTML databases (built via
  [`m2/file-installPackage.md`](M2/Macaulay2/m2/file-installPackage.md))
  let `help foo` skip parsing on demand.
- The doc database is loaded **lazily** — only when the user
  invokes help.

## Debugging startup issues

If M2 crashes or hangs at startup:

1. **Get a backtrace** — the Boost stacktrace handler installed
   in phase 2 prints one on SIGSEGV.
2. **Identify the phase** — what line was last printed before the
   crash? Match it against the eight phases.
3. **Reproduce minimally** — try `M2 -q --no-prompt --version`
   first (skips phase 8's Core load); does it work?
4. **Read the relevant phase's deep-dives** — every phase's
   source files are documented.

## Used by

- Anyone debugging an M2 startup crash.
- Engine developers wanting to know "when does my class get
  instantiated?"
- Package authors wondering "what's loaded before my package?"

## Related

- [`README.md`](README.md) — repository TOC.
- [`TOUR.md`](TOUR.md) — Path B (engine debugger) covers
  post-startup crash diagnosis.
- [`GLOSSARY.md`](GLOSSARY.md) — terminology.
- [`M2/Macaulay2/bin/file-main.md`](M2/Macaulay2/bin/file-main.md)
  — phase 2 source.
- [`M2/Macaulay2/d/architecture.md`](M2/Macaulay2/d/architecture.md)
  — phase 7's full architecture.
- [`M2/Macaulay2/m2/architecture.md`](M2/Macaulay2/m2/architecture.md)
  — phase 8's full architecture.
- [`M2/Macaulay2/system/architecture.md`](M2/Macaulay2/system/architecture.md)
  — phase 6's full architecture.
