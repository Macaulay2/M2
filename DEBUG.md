# Debugging M2

This document is the **practical debugging reference** for M2 —
tools, techniques, and pattern recipes for diagnosing problems at
every layer.

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) · [Tour](TOUR.md) · [Startup](STARTUP.md) · [Memory](MEMORY.md) · [Threading](THREADING.md) · [Testing](TESTING.md)

## Pick your symptom

| Symptom | Jump to |
|---|---|
| M2 crashes at startup | [Startup crashes](#startup-crashes) |
| M2 crashes mid-computation | [Runtime crashes](#runtime-crashes) |
| Wrong mathematical answer | [Wrong answers](#wrong-answers) |
| Hangs / infinite loop | [Hangs](#hangs) |
| Memory bloat / OOM | [Memory issues](#memory-issues) |
| Race condition under threading | [Threading issues](#threading-issues) |
| Failing test | [Test debugging](#test-debugging) |
| Slow performance | [Performance](#performance) |
| Build failure | [Build issues](#build-issues) |
| M2-level error message | [M2-level errors](#m2-level-errors) |

## Debugging at each layer

M2 spans four code layers + multiple subsystems. Each has its own
debugging tools.

| Layer | Tool / mechanism | Source doc |
|---|---|---|
| User M2 code | `debugError = true`, `error`, `stack`, `code` | M2-level docs |
| Core M2 (`m2/`) | M2 debugger, `error`, `debug` flag | [`m2/architecture.md`](M2/Macaulay2/m2/architecture.md) |
| Interpreter (`d/`) | `debugging.dd`, error/interrupt flags, gdb on `M2-binary` | [`d/architecture.md`](M2/Macaulay2/d/architecture.md) |
| Engine (`e/`) | `debug.{cpp,hpp}`, gdb, gtest, valgrind | [`e/architecture.md`](M2/Macaulay2/e/architecture.md) |
| Supervisor (`system/`) | `tests.cpp`, helgrind, TSAN | [`system/architecture.md`](M2/Macaulay2/system/architecture.md) |
| Translator (`c/`) | `debugging.c` flags, `assert` macros | [`c/architecture.md`](M2/Macaulay2/c/architecture.md) |

## Startup crashes

M2 crashes before printing its prompt.

**First step**: identify the phase from
[`STARTUP.md`](STARTUP.md). The eight phases each have distinct
failure modes:

| Phase | Crash pattern | Diagnosis |
|---|---|---|
| 1 (wrapper) | "library not found" | Library path issue; check `LD_LIBRARY_PATH` |
| 2 (main, GC init) | Immediate SIGSEGV | `GC_INIT()` not called; rebuild |
| 3 (version asserts) | "GMP/MPFR/FLINT version mismatch" | System libs differ from build; reinstall or rebuild |
| 4 (engine init) | Crash before banner | `IM2_initialize()` bug; check for stale `.o` files |
| 5 (supervisor) | Hang at start | Worker thread creation failed; check pthread limits |
| 6 (interpreter) | "unrecognised operator" | An `actors*.d` failed to load; check translator output |
| 7-8 (startup.m2 / Core) | Specific .m2 line errors | Edit error in m2/ file; see line number |

For SIGSEGV-class crashes:

```sh
# Get a backtrace via Boost.Stacktrace (auto-installed in phase 2)
M2 2>&1 | head -50

# Or run under gdb
gdb --args M2 --no-prompts
(gdb) run
(gdb) bt
(gdb) thread apply all bt   # for multi-thread crashes
```

The Boost stacktrace handler is installed in
[`bin/file-main.md`](M2/Macaulay2/bin/file-main.md) and produces
source-file-line numbers via `addr2line`.

## Runtime crashes

M2 starts, but a specific computation crashes.

### 1. Minimal reproduction

```m2
-- in M2:
restart
-- the minimal sequence that crashes:
R = QQ[x, y, z]
I = ideal(x^3 - y, y^3 - z)
G = gb I    -- crashes here
```

The smaller the reproducer, the faster the diagnosis. Bisect by
deleting half the input.

### 2. Find the responsible code

Follow the call chain (see [`TOUR.md`](TOUR.md) Path B —
"Engine debugger"):

```
M2 user input        gb I
   ↓
m2/gb.m2             gb method on Ideal
   ↓
m2/raw.m2 or similar rawGB(...)
   ↓
d/interface.dd       interpreter binding
   ↓
e/interface/groebner.cpp   public C API: IM2_GB_make()
   ↓
e/comp-gb*.cpp       internal C++ implementation
   ↓
algorithm           gb-default.cpp / f4.cpp / gb-f4/...
```

The `raw…` call is the boundary between M2-side and engine-side.
M2-level traces stop there; engine traces start there.

### 3. Run under gdb / lldb

```sh
# Linux:
gdb --args M2-binary --no-prompts
# macOS:
lldb -- M2-binary --no-prompts

(gdb) run
< type the failing M2 code >
< wait for crash >
(gdb) bt                 # backtrace
(gdb) frame N            # navigate to interesting frame
(gdb) info locals        # examine state
(gdb) call dntl_ZZ(p)    # call a debug printer
```

The wrapper script `M2` is fine for normal runs, but for gdb you
typically want `M2-binary` directly (the wrapper is a shell
script).

### 4. Engine-side debug printers

The engine has gdb-callable printers in
[`e/file-debug.md`](M2/Macaulay2/e/file-debug.md):

```cpp
void dring(const Ring *R);
void dmatrix(const Matrix *M);
void dideal(const Matrix *I);
void dgb(const GBComputation *G);
void dpoly(ring_elem r, const Ring *R);
```

(Approximate names — see `debug.cpp` for the actual API.)

From gdb:

```
(gdb) call dring(R)
(gdb) call dmatrix(M)
```

Prints to stdout. Useful for "I'm in this frame; what does this
pointer actually contain?"

### 5. Engine assertion failures

Engine code uses `assert(...)` for internal invariants. A crash
with "assertion failed in foo.cpp:NNN" means an invariant was
violated. The line tells you exactly where.

Often the **bug isn't at that line** — the bug is earlier (where
the invariant got broken). The assert just caught it. Walk the
backtrace to find the earlier site.

### 6. Translator-side issues

If you suspect a bug in `scc1`'s output, build with
`-DSCCFLAGS="-O -noline"` to get unmangled C output, then
inspect `M2/BUILD/build/Macaulay2/d/<filename>.c`. See
[`c/file-scc1.md`](M2/Macaulay2/c/file-scc1.md).

## Wrong answers

M2 produces output, but it's mathematically wrong.

### 1. Confirm it's wrong

Easy mistakes: ordering matters in many operations. `gb I` over a
different monomial order can produce a different basis with the
same span. Make sure you're comparing the right thing.

### 2. Isolate the layer

The bug is in one of these places:

| Suspected layer | Test |
|---|---|
| M2-level wrapper | Try the same operation via `raw…()` directly |
| Engine boundary | Try the same operation via the gtest API |
| Engine algorithm | Cross-check against an independent implementation (msolve, Singular) |
| Coefficient ring | Cross-validate against an alternative backend (GMP vs FLINT, native GF vs Givaro) |

See [`TESTING.md`](TESTING.md) for which test suite tests which
layer.

### 3. Modular reduction trick

For computations over `QQ`, run the same computation over `Z/p`
for various primes. If the modular results disagree with the
rational result modulo p, the bug is in the rational-arithmetic
path (likely a CRT or rational-reconstruction issue — see
[`e/file-cra.md`](M2/Macaulay2/e/file-cra.md)).

### 4. Monomial overflow

For computations with large degrees, **silent overflow** in
monomial arithmetic produces wrong answers without crashing. See
[`MEMORY.md`](MEMORY.md) Layer 5. If you see degrees you don't
expect, suspect `safe::*` overflow detection bypass.

### 5. Check against the book

If the operation is documented in *Computations in Algebraic
Geometry with Macaulay 2*, the
[`ComputationsBook` test suite](M2/Macaulay2/tests/ComputationsBook/file-computations-book-catalogue.md)
captures expected outputs. Running that chapter's test reveals
if outputs have drifted.

## Hangs

M2 starts a computation that never finishes.

### 1. Distinguish hang from slow

A "hang" might be a genuine infinite loop, a deadlock, or
"the computation needs 30 more minutes." Try:

```sh
# Send SIGINT (Ctrl-C). M2's polling loops should bail out
# within ~1 second of seeing the interrupt flag.
```

If Ctrl-C works, it was just slow. If Ctrl-C doesn't work, the
loop is **not polling `system_interrupted()`** — that's a bug
worth fixing.

### 2. Attach gdb to a running process

```sh
gdb -p $(pidof M2-binary)
(gdb) thread apply all bt
```

Look for:

- A worker thread stuck in a loop without checking the interrupt
  flag.
- Two threads waiting on each other's mutexes (deadlock).
- A thread waiting on a condvar that should have been signalled.

See [`THREADING.md`](THREADING.md) for the threading mechanisms.

### 3. Use TSAN for race conditions

If a hang is intermittent under parallel workloads:

```sh
# Rebuild with ThreadSanitizer
cmake -DCMAKE_CXX_FLAGS="-fsanitize=thread -fno-omit-frame-pointer" ...
cmake --build M2/BUILD/build --target M2-binary

# Run the reproducer:
./Macaulay2/bin/M2-binary --script repro.m2
```

TSAN flags data races precisely.

## Memory issues

OOM, slow growth, valgrind reports memory errors.

Full coverage: [`MEMORY.md`](MEMORY.md). Quick recipes:

### 1. Check GC heap usage

From M2:

```m2
collectGarbage()
GC_print_heap_usage()    -- prints heap stats
```

(Function names approximate — actual API is in
[`e/file-newdelete.md`](M2/Macaulay2/e/file-newdelete.md).)

### 2. Run under valgrind

```sh
valgrind \
    --suppressions=$M2/files/M2-suppressions.supp \
    --tool=memcheck \
    --leak-check=full \
    M2-binary --script repro.m2
```

The suppression file
([`files/file-files-content.md`](M2/files/file-files-content.md))
silences known false positives (Boehm GC, GMP, MPFR).

### 3. Check pool-allocator scope

If memory grows monotonically inside a computation but releases
afterward, that's expected — pool allocators
([`MEMORY.md`](MEMORY.md) Layer 3) release en masse. Grows
across computations → real leak.

### 4. Missing finaliser

External-library state without a finaliser silently leaks. See
[`MEMORY.md`](MEMORY.md) Layer 2. Common offenders: a new
external-library binding that wraps the lib's handle but forgets
to register a finaliser.

## Threading issues

Race conditions, deadlocks, missing GC registration.

Full coverage: [`THREADING.md`](THREADING.md). Quick recipes:

### 1. TSAN

See above under [Hangs](#hangs).

### 2. helgrind

```sh
valgrind --tool=helgrind M2-binary --script repro.m2
```

helgrind is the lock-aware sister of memcheck. It detects:

- Mutex held across an interpreter call.
- Reentrant lock acquisitions (deadlock-prone).
- Lock acquisition order inconsistencies.

### 3. Check GC thread registration

If a crash correlates with parallel workloads, suspect a
worker thread not registered with Boehm GC. See
[`THREADING.md`](THREADING.md) "GC + threads interaction." The
supervisor handles its own pool; TBB workers are
auto-registered. Custom threads need manual `GC_register_my_thread`.

## Test debugging

A test fails. How to investigate.

Full coverage: [`TESTING.md`](TESTING.md). Quick recipes:

### M2-level test (`tests/normal/*.m2`)

```sh
# Run interactively to see all output:
M2 --script M2/Macaulay2/tests/normal/the-test.m2
```

If the test's `assert(...)` fails, the line + condition is
printed.

### gtest failure (`e/unit-tests/*`)

```sh
# Filter to the failing test:
./Macaulay2/e/unit-tests/M2-unit-tests \
    --gtest_filter='SuiteName.TestName'

# With more output:
./Macaulay2/e/unit-tests/M2-unit-tests \
    --gtest_filter='SuiteName.*' \
    --gtest_break_on_failure
```

The `--gtest_break_on_failure` flag triggers a debug breakpoint
on first failure — useful with gdb attached.

### ComputationsBook diff

```sh
# A chapter's output differs from expected:
diff tests/ComputationsBook/<chapter>/chapter.out \
     tests/ComputationsBook/<chapter>/chapter.out.expected
```

The diff shows exactly what changed. The `patterns` file in each
chapter dir defines what should be normalised before diffing.
See [`tests/ComputationsBook/file-computations-book-catalogue.md`](M2/Macaulay2/tests/ComputationsBook/file-computations-book-catalogue.md).

### Package test (`check "Foo"`)

```m2
-- from inside M2:
check "Foo"   -- runs all TEST blocks; reports failures
```

Each failure prints the test source line. The `TEST ///...///`
block can have a `--` comment with a date or issue reference for
context.

## Performance

M2 runs but is slow.

### 1. Profile with perf

```sh
# Linux: perf record
perf record -g M2 --script slow-repro.m2
perf report
# View hot functions

# Or as a flame graph (using BUILD/cmake/stackcollapse-m2.sh):
perf script | stackcollapse-perf | flamegraph.pl > out.svg
```

See [`cmake/file-misc-cmakes.md`](M2/cmake/file-misc-cmakes.md)
for the helper script.

### 2. Profile with valgrind / callgrind

```sh
valgrind --tool=callgrind M2-binary --script slow-repro.m2
kcachegrind callgrind.out.NNNN
```

Slower than perf but produces detailed function-level breakdowns.

### 3. ccache for build speed

```sh
# Auto-detected at configure time; verify:
which ccache
ccache --show-stats
```

If `ccache` is installed at configure time it gets wired into the
build automatically. See
[`BUILD.md`](BUILD.md) phase 4.

### 4. Engine-level timing

```cpp
#include "timing.hpp"
double start = timing_start();
// ...
double elapsed = timing_elapsed(start);
```

See [`e/file-timing.md`](M2/Macaulay2/e/file-timing.md).

### 5. The `time` operator

```m2
time computeGroebnerBasis I    -- prints elapsed wall time
elapsedTime computeGroebnerBasis I    -- variant
```

Built-in M2 timing. See [`d/file-chrono.md`](M2/Macaulay2/d/file-chrono.md).

### 6. Compare against alternative engines

If an algorithm is slow, check whether an alternative engine
exists:

| Algorithm | Alternative engines |
|---|---|
| GB over Z/p | F4 ([`f4/`](M2/Macaulay2/e/f4/architecture.md)), gb-f4 ([`gb-f4/`](M2/Macaulay2/e/gb-f4/architecture.md)), mathicgb (submodule) |
| Resolution | Older ([`file-res-old.md`](M2/Macaulay2/e/file-res-old.md)), Schreyer ([`schreyer-resolution/`](M2/Macaulay2/e/schreyer-resolution/architecture.md)) |
| Dense linalg over Z/p | FFPACK vs FLINT ([`file-dmat-lu-variants.md`](M2/Macaulay2/e/file-dmat-lu-variants.md)) |

M2's `Strategy =>` option to many functions selects between
these.

## Build issues

A `cmake --build` step fails.

Full coverage: [`BUILD.md`](BUILD.md). Quick recipes:

### 1. Increase verbosity

```sh
cmake --build M2/BUILD/build --target X --verbose
```

Shows actual compiler invocations.

### 2. Check the CMake cache

```sh
cat M2/BUILD/build/CMakeCache.txt | grep -i "MISSING\|NOT_FOUND"
```

Variables that say "NOTFOUND" indicate libraries that configure
expected but couldn't locate.

### 3. Clean rebuild

```sh
rm -rf M2/BUILD/build
cmake -GNinja -S M2 -B M2/BUILD/build
cmake --build M2/BUILD/build
```

Mixed-state caches are the #1 cause of weird build errors.

### 4. Force library build-from-source

```sh
cmake -GNinja -S M2 -B M2/BUILD/build \
    -DBUILD_LIBRARIES="GMP MPFR FLINT NTL"
```

Bypasses system libraries; useful when a system library has wrong
version / ABI.

## M2-level errors

M2 prints an error message; you want to understand it.

### 1. Enter the debugger

```m2
debugError = true   -- enable
< the failing command >
-- M2 drops into a debugger session at the error site
```

The debugger lets you inspect locals, walk the stack, evaluate
sub-expressions. Built into the interpreter — see
[`d/file-debugging.md`](M2/Macaulay2/d/file-debugging.md).

Commands inside the debugger:

```
back                 -- one frame up
forward              -- one frame down
return               -- continue (treat as if returned)
break                -- show breakpoint info
listLocalSymbols     -- locals in current frame
```

### 2. Use `stack` and `code`

```m2
< failing command >
stack        -- prints the call stack
code(symbol) -- prints the source of a function
```

Useful even without `debugError = true`.

### 3. Look up the error in docs

Most error messages have a corresponding doc node (`help
"the error message"`). The doc explains what condition triggers
the error and how to fix it.

## General techniques

### Bisection by feature

```sh
# When did this break?
git log --oneline | head -30
git bisect start HEAD <known-good-commit>
git bisect run ./test-repro.sh
```

### Bisection by package

```m2
-- which package introduces the bug?
< minimal repro without any needsPackage >
needsPackage "Suspect"
< re-run >
```

### Print everything

When in doubt:

```m2
print T               -- type tag
print describe T      -- detailed
print precedingValues -- recent computations
print errorDepth      -- error nesting level
```

### Use the C++ interactive debugger

```sh
# Some IDEs let you set breakpoints in engine C++ code
# directly. Use gdb's "tui" mode for terminal-based:
gdb --tui --args M2-binary
```

### Compare with known-good binary

If you have an old M2 binary that works:

```sh
diff <(./old-M2 --script repro.m2) <(./new-M2 --script repro.m2)
```

The diff identifies exactly when output changed.

## Engine internal debug flags

Several engine files have **compile-time debug flags** that
print extra diagnostics:

```cpp
// In e/file-foo.cpp:
#define M2_DEBUG_FOO 1
```

When set to 1, prints diagnostics on every call. Search the
engine source for `M2_DEBUG_` to find what's available.

Not the right tool for normal debugging — typically only used
when debugging a specific engine bug requiring deep introspection.

## When all else fails

1. **Report a bug**: https://github.com/Macaulay2/M2/issues —
   include the minimal reproducer, M2 version, OS, and any error
   output.
2. **Ask on the mailing list**:
   https://groups.google.com/group/macaulay2.
3. **Check the Wiki**: https://github.com/Macaulay2/M2/wiki for
   FAQ and troubleshooting.

## Used by

- Anyone debugging an M2 issue.
- New contributors learning the debugger ecosystem.
- CI failures requiring root-cause analysis.

## Related

- [`README.md`](README.md) — repository TOC.
- [`STARTUP.md`](STARTUP.md) — for startup-phase crashes.
- [`MEMORY.md`](MEMORY.md) — for memory-related crashes.
- [`THREADING.md`](THREADING.md) — for race conditions /
  deadlocks.
- [`TESTING.md`](TESTING.md) — for test-failure debugging.
- [`BUILD.md`](BUILD.md) — for build-failure debugging.
- [`TOUR.md`](TOUR.md) — Path B (engine debugger) covers
  the engine call chain.
- [`M2/Macaulay2/d/file-debugging.md`](M2/Macaulay2/d/file-debugging.md)
  — interpreter-side debugger.
- [`M2/Macaulay2/e/file-debug.md`](M2/Macaulay2/e/file-debug.md)
  — engine debug printers.
- [`M2/Macaulay2/c/file-debugging.md`](M2/Macaulay2/c/file-debugging.md)
  — translator-side debug flags.
- [`M2/files/file-files-content.md`](M2/files/file-files-content.md)
  — valgrind suppression file.
