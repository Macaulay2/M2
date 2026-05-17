# Code style guide

This document captures **coding conventions** across all four
M2 language layers. Complements [`CONTRIBUTING-DOCS.md`](CONTRIBUTING-DOCS.md)
(documentation conventions) and [`MEMORY.md`](MEMORY.md) /
[`THREADING.md`](THREADING.md) (which encode safety conventions).

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) · [Contributing docs](CONTRIBUTING-DOCS.md)

## By layer

| Layer | Primary source | Tool | Notes |
|---|---|---|---|
| Translator (`c/`) | C, plain | manual indent, 4 spaces | Hanson-style; minimal modernisation |
| Interpreter (`d/`) | `.d` / `.dd` | `scc1`-enforced | Follows `.d` language syntax (see [`c/architecture.md`](M2/Macaulay2/c/architecture.md)) |
| Engine (`e/`) | C++17 | `clang-format` from project `.clang-format` | See engine [`STYLE.txt`](M2/Macaulay2/e/file-style.md) |
| Core M2 (`m2/`) | `.m2` | manual (no formatter) | M2 syntax; 2-space indent typical |
| Supervisor (`system/`) | C++17 | `clang-format` | Same as engine |
| Build (`cmake/`) | CMake | manual; 2-space indent | Follows CMake idioms |

The engine's per-area [`STYLE.txt`](M2/Macaulay2/e/file-style.md)
is the authoritative source for the engine; this doc summarises
plus covers other layers.

## Engine C++ (`e/`, `system/`)

### Format with clang-format

```sh
clang-format -i -style=file Macaulay2/e/foo.{hpp,cpp}
```

The `.clang-format` config lives at the M2 root. Run before
committing — CI flags formatting drift.

### Header ordering

Per the [Google C++ Style Guide](https://google.github.io/styleguide/cppguide.html#Names_and_Order_of_Includes):

```cpp
// 1. Matching header for a .cpp file (if any)
#include "foo.hpp"

// 2. C system headers
#include <stdio.h>

// 3. C++ standard library
#include <vector>
#include <memory>

// 4. Other library headers
#include <gmp.h>
#include <flint/flint.h>

// 5. M2 internal headers
#include "engine-includes.hpp"
#include "ring.hpp"
```

Each block alphabetised within itself.

### Naming

| Entity | Convention | Example |
|---|---|---|
| Class | `PascalCase` | `MutableMatrix`, `ARingZZpFlint` |
| Member function | `lowercase_with_underscores` | `set_from_long`, `is_zero` |
| Static method | same as member | `Ring::create()` |
| Member variable | `mCamelCase` or `lowercase_with_underscores` (legacy varies) | `mRing`, `coefficient_ring` |
| Local variable | `lowercase_with_underscores` | `monomial_count` |
| Constant | `UPPER_SNAKE_CASE` | `GEOHEAP_SIZE`, `EQ`, `GT` |
| Template parameter | `PascalCase` | `template<typename RingType>` |
| File | `lowercase-hyphenated.cpp` | `aring-zz-flint.cpp` |

The `m` prefix for member variables is **newer convention**; older
code uses underscored names. New code prefers `mFoo`.

### Memory

Per [`MEMORY.md`](MEMORY.md):

```cpp
// GC-managed class:
class Foo : public our_new_delete { ... };

// Mutable + identity-tracked:
class Bar : public MutableEngineObject { ... };

// Atomic-leaf data:
void *p = getmem_atomic(n);
```

**Never** call `new` for a class without `: public our_new_delete`
(or equivalent). The GC won't track it.

### Comments

```cpp
// Single-line comments for terse notes
//
// Multi-line comments for paragraphs.

/// Doxygen-style for public API (note the triple slash).
/// @brief Compute the foo of the bar.
class Foo {
  /// What this method does.
  void compute();
};
```

Engine code uses Doxygen comments for the API exposed to the
generated Sphinx docs. See
[`Macaulay2/e/file-defgroups.md`](M2/Macaulay2/e/file-defgroups.md)
and [`Macaulay2/docs/`](M2/Macaulay2/docs/README.md).

### Error handling

Prefer the C ABI flag mechanism over C++ exceptions at the
boundary; C++ exceptions are fine inside engine internals. See
[`e/file-error.md`](M2/Macaulay2/e/file-error.md) and
[`e/file-exceptions.md`](M2/Macaulay2/e/file-exceptions.md).

```cpp
// At the boundary (interface/foo.cpp):
const Matrix* IM2_Matrix_op(const Matrix* M) {
  try {
    return M->op();
  } catch (const exc::engine_error& e) {
    ERROR(e.what());
    return nullptr;
  }
}
```

### Threading

Per [`THREADING.md`](THREADING.md):

```cpp
// Thread-safe shared counter:
std::atomic<long> counter{0};

// Larger critical section:
pthreadMutex mutex;
{
    std::lock_guard<pthreadMutex> g(mutex);
    // ... critical section ...
}

// STL container holding GC-tracked pointers:
gc_map<int, Matrix*> myMap;   // not std::map!
```

## Interpreter `.d` / `.dd` (`d/`)

The `.d` language has its own syntax (see
[`c/architecture.md`](M2/Macaulay2/c/architecture.md)). Key
conventions:

### Naming

| Entity | Convention | Example |
|---|---|---|
| Type | `lowercase` | `int`, `string`, `Expr` |
| Sum-type tag | `lowercase` | `is i:ZZcell` |
| Function | `lowercaseCamel` | `evaluateCode`, `toExpr` |
| Constant | `lowercaseCamel` | `errorOccurred` |

### Common patterns

```d
-- Pattern matching:
when expr is i:ZZcell do
    ...
is s:stringCell do
    ...
else
    error("unexpected type");

-- Error handling:
if invalidCondition then return WrongArg("foo");

-- C escape:
ans := Ccode(string, "((", x, ").contents)");
```

### Files

| Suffix | Compiles to |
|---|---|
| `.d` | C (`*.c`) |
| `.dd` | C++ (`*.cpp`) |

Prefer `.d` unless you specifically need C++. Most interpreter
files are `.d`; a few (like `interp.dd`) need C++.

### Per-file size

The d/ directory has 70+ files because each "module" is one file
(`lex.d`, `tokens.d`, `parser.d`, etc.). Split modules when they
grow past ~1000 lines.

## Core M2 `.m2` (`m2/`)

### Indentation

```m2
foo = method(Options => {
    KeyA => 1,
    KeyB => "x"
})
foo Ring := opts -> R -> (
    if opts.KeyA == 0 then return null;
    M := mutableMatrix(R, 3, 3);
    -- ...
    return matrix M;
)
```

Two-space indent. No tabs.

### Naming

| Entity | Convention | Example |
|---|---|---|
| Type | `PascalCase` | `Module`, `Matrix`, `FreeModule` |
| Method | `lowercaseCamel` | `gb`, `resolution`, `numgens` |
| Local variable | `lowercaseCamel` | `currentRing`, `gbBasis` |
| Option key | `PascalCase` | `Strategy`, `DegreeLimit` |

Type names match standard mathematical notation where possible
(`ZZ`, `QQ`, `RR`, `CC`).

### Documentation

Always inside `beginDocumentation()`. Use the typed DSL — see
[`DOCUMENTATION-SYSTEM.md`](DOCUMENTATION-SYSTEM.md).

```m2
doc ///
Key
   myFunction
Headline
   compute the foo of a bar
Description
  Text
     ...
  Example
     R = QQ[x, y];
     myFunction R
SeeAlso
   relatedFunction
///
```

### Methods vs functions

```m2
-- Prefer method (typed dispatch):
foo = method(...)
foo Ring := R -> ...

-- Avoid functions where types matter:
-- foo = R -> ...     -- WORSE: no type-based dispatch
```

Methods get typed dispatch, work well with multiple-inheritance,
and integrate with M2's documentation system. Plain functions
should be used only for truly type-generic operations.

### Tests

```m2
TEST ///
R = QQ[x, y, z];
I = ideal(x^2 - y, x*y - z);
assert(numgens gb I == 3)
assert(degree (R/I) == ...);
///
```

`assert(...)` everywhere — no `print` statements.

## Translator C (`c/`)

The `scc1` translator follows **1993-era plain C** conventions:

- 4-space indent.
- K&R brace style.
- `typedef`s for opaque types.
- Macros for portability (see
  [`c/file-compat.md`](M2/Macaulay2/c/file-compat.md)).

```c
node chk(node e, scope v) {
  node f;
  if (iscons(e)) {
    f = car(e);
    if (issymbol(f)) {
      /* ... */
    }
  }
  return e;
}
```

Don't modernise unless there's a concrete reason. The translator
is stable; gratuitous churn risks regressions.

## CMake (`cmake/`)

```cmake
# Variables: UPPER_SNAKE_CASE
set(M2_LIBRARIES gmp mpfr flint)

# Functions / commands: lowercase
add_executable(M2-unit-tests
    testMain.cpp
    ARingZZTest.cpp
)

# Target properties:
target_link_libraries(M2-unit-tests
    PRIVATE M2-engine gtest
)
```

Two-space indent. Function names follow CMake's
`lowercase_with_underscores` convention.

Per-module conventions:
[`cmake/architecture`](M2/cmake/README.md).

## Cross-layer naming

Some entities span multiple layers. Conventions:

| M2 user level | Core M2 (`m2/`) | Interpreter (`d/`) | Engine (`e/`) |
|---|---|---|---|
| `Ring` | `Ring` (class) | `RawRing` (opaque) | `Ring` / `aring::*` |
| `Matrix` | `Matrix` | `RawMatrix` (opaque) | `Matrix` |
| `gb` | `gb` (method) | `rawGB` | `IM2_GB_make` |

Pattern: M2-level name → `raw…` for boundary → `IM2_…` for engine
C ABI → internal C++ class.

## Languages M2 vendors / interacts with

| Language | Where | Style |
|---|---|---|
| `.d`/`.dd` | interpreter | M2-specific; see [`c/architecture.md`](M2/Macaulay2/c/architecture.md) |
| C | `c/`, some `d/` glue, `bin/timestamp.c` | K&R, 4-space indent |
| C++ | `e/`, `system/`, `bin/main.cpp` | clang-format with project config |
| `.m2` | `m2/`, `packages/` | 2-space indent, M2 conventions |
| CMake | `cmake/`, per-dir `CMakeLists.txt` | 2-space indent |
| Bison/yacc | `c/grammar.y`, `html-check-links/grammar.y` | Bison conventions |
| Flex | `c/lex` (embedded), `html-check-links/lex.l` | Flex conventions |
| Doxygen | `e/`, `system/` headers | `///` triple-slash |
| Sphinx RST | `docs/` | reStructuredText |
| Markdown | every `.md` doc | GitHub-flavored |
| HTML / CSS | `editors/prism/`, `Style/` | per-tool conventions |

## Linting and quality tools

```sh
# Spell-check (CI gates this):
codespell --ignore-words=.codespell_ignore M2/Macaulay2/packages

# C++ static analysis:
cmake --build M2/BUILD/build --target clang-tidy-all
cmake --build M2/BUILD/build --target cppcheck-all
cmake --build M2/BUILD/build --target iwyu-all

# C++ formatting:
cmake --build M2/BUILD/build --target clang-format-all
```

The clang-tidy checks are configured in
[`cmake/file-misc-cmakes.md`](M2/cmake/file-misc-cmakes.md):

```
CLANG_TIDY_CHECKS = -*,clang-analyzer-*,cppcoreguidelines-*,
                    performance-*,modernize-*
```

## C++ standard

M2 currently targets **C++17** (set in
[`M2/file-CMakeLists-txt.md`](M2/file-CMakeLists-txt.md)):

```cmake
set(CMAKE_CXX_STANDARD 17)
```

Migration to C++20 is planned but not yet done. Until then,
**don't use C++20-only features**:

- `std::span` → use [`Range<T>`](M2/Macaulay2/e/NCAlgebras/file-Range.md)
  instead.
- `std::format` → use the engine's `buffer` API.
- Designated initialisers in some contexts.
- `consteval`, `constinit`.

Some C++20 features are partially usable if conditional via
`#if __cplusplus >= 202002L`. Use sparingly.

## Modernisation hot-takes

Things the codebase has agreed to do:

- **Use `auto`** for verbose iterator types.
- **Use `nullptr`** instead of `NULL` / `0` for pointers.
- **Use `enum class`** instead of plain `enum`.
- **Use range-for loops** where appropriate.
- **Use `std::unique_ptr`** for owned non-GC resources.

Things the codebase has agreed NOT to do (yet):

- **No C++20 features** — see above.
- **No exceptions across the C ABI** — see
  [`THREADING.md`](THREADING.md).
- **No raw `new`/`delete`** for engine objects — use the GC
  hooks.

## Pre-commit checklist

Before committing:

1. **`clang-format`** ran on edited C++ files.
2. **Tests pass**: `ctest --output-on-failure -R "unit-tests"`.
3. **Smoke-test M2**: `./Macaulay2/bin/M2 --check 1`.
4. **Doc links resolve**: run the audit from
   [`CONTRIBUTING-DOCS.md`](CONTRIBUTING-DOCS.md).
5. **No `printf` debug statements** left in.
6. **Commit message** describes *why*, not just *what*.

## Common style mistakes

### `int` for indices into large arrays

```cpp
// BAD: int may overflow on large inputs:
int i;
for (i = 0; i < matrix_rank; i++) ...

// GOOD: use size_t (or specific typed integers):
for (size_t i = 0; i < matrix_rank; i++) ...
```

The engine's typed-integer family in
[`gb-f4/file-MonomialTypes.md`](M2/Macaulay2/e/gb-f4/file-MonomialTypes.md)
(`Index`, `MonomialIndex`, `HashInt`) is the preferred pattern
for new code.

### Raw `std::map<K, T*>` for GC pointers

```cpp
// BAD: std::map's internals use std::allocator → GC won't scan:
std::map<int, Matrix*> bad;

// GOOD: GC-aware allocator:
gc_map<int, Matrix*> good;
```

See [`MEMORY.md`](MEMORY.md) Layer 4.

### Silent integer overflow in monomial arithmetic

```cpp
// BAD: silent overflow on large degrees:
int product_degree = a_deg * b_deg;

// GOOD: overflow-checked:
int product_degree = safe::mul(a_deg, b_deg);
```

See [`MEMORY.md`](MEMORY.md) Layer 5.

### Missing `our_new_delete`

```cpp
// BAD: Foo will leak (GC doesn't know about it):
class Foo { /* ... */ };
Foo *f = new Foo;

// GOOD:
class Foo : public our_new_delete { /* ... */ };
Foo *f = new Foo;
```

### Forgetting to register a finaliser

```cpp
// BAD: Wraps mpz_t but doesn't free it on GC reclaim:
struct ZZcell : public our_new_delete {
    mpz_t value;
    ~ZZcell() { mpz_clear(value); }   // destructor doesn't run on GC reclaim!
};

// GOOD: explicit finaliser registration:
struct ZZcell : public our_new_delete {
    mpz_t value;
};
ZZcell *p = new ZZcell;
mpz_init(p->value);
register_finalizer(p, [](ZZcell *q) { mpz_clear(q->value); });
```

See [`MEMORY.md`](MEMORY.md) Layer 2.

## Used by

- Anyone writing M2 code.
- CI's lint checks (`clang-tidy`, `clang-format`, `codespell`).
- Code reviewers.

## Related

- [`README.md`](README.md) — repository TOC.
- [`CONTRIBUTING-DOCS.md`](CONTRIBUTING-DOCS.md) — doc
  conventions (this is the code-conventions sister).
- [`MEMORY.md`](MEMORY.md) — memory conventions (`our_new_delete`,
  finalisers, `safe::*`).
- [`THREADING.md`](THREADING.md) — concurrency conventions
  (`gc_map`, atomic primitives).
- [`DEBUG.md`](DEBUG.md) — debugging conventions (when style
  rules break for diagnostic purposes).
- [`DOCUMENTATION-SYSTEM.md`](DOCUMENTATION-SYSTEM.md) — doc DSL
  conventions for `.m2` files.
- [`M2/Macaulay2/e/file-style.md`](M2/Macaulay2/e/file-style.md)
  — engine's own `STYLE.txt`.
- [`M2/cmake/file-misc-cmakes.md`](M2/cmake/file-misc-cmakes.md)
  — lint configuration.
- Project [Wiki](https://github.com/Macaulay2/M2/wiki) for
  end-user-facing style.
