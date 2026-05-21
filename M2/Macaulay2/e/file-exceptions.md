# `exceptions.hpp` — engine exception types

`exceptions.hpp` declares the **C++ exception types** the engine uses
internally before they cross the C boundary back to the interpreter.
The boundary itself uses the thread-local error string in
[`file-error.md`](file-error.md); these exceptions are an intra-engine
mechanism for unwinding cleanly through templated code.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## Hierarchy

```cpp
#include <stdexcept>
#include <string>

namespace exc {

struct engine_error : public std::runtime_error {
    explicit engine_error(const std::string &msg) : std::runtime_error(msg) {}
};

struct overflow_exception : public engine_error {
    explicit overflow_exception(const std::string &msg) : engine_error(msg) {}
};

struct division_by_zero_error : public engine_error {
    explicit division_by_zero_error(const std::string &msg) : engine_error(msg) {}
    explicit division_by_zero_error()
        : engine_error(std::string{"division by zero"}) {}
};

struct internal_error : public engine_error {
    explicit internal_error(const std::string &msg) : engine_error(msg) {}
};

}
```

A tiny hierarchy rooted at `std::runtime_error`:

| Type | Meaning |
|---|---|
| `engine_error` | General engine-side error |
| `overflow_exception` | Arithmetic overflow (e.g. monomial exponent overflow caught by [`file-overflow.md`](file-overflow.md)) |
| `division_by_zero_error` | The eponymous error; a default constructor is provided since the message is fixed |
| `internal_error` | Bug or invariant violation — should not normally be reachable |

## How exceptions cross the engine ↔ interpreter boundary

C++ exceptions **must not** propagate across the boundary to the
interpreter (which is built from C-generated `.d` code that doesn't
unwind C++ stacks). The engine catches them at the boundary:

```cpp
try {
    // engine work
} catch (const exc::engine_error &e) {
    ERROR("%s", e.what());           // sets thread-local error string
    return error_return_value;
} catch (...) {
    ERROR("unknown engine error");
    return error_return_value;
}
```

The `ERROR(...)` macro is defined in
[`file-error.md`](file-error.md). The exception's `what()` string
flows into the thread-local message; the interpreter then renders it
to the user.

## Where exceptions get thrown

- **`file-overflow.md`** — throws `overflow_exception` when a monomial
  arithmetic op would overflow.
- Concrete `aring`s — throw `division_by_zero_error` when given a
  zero divisor.
- Engine invariant checks — throw `internal_error` for bugs.

## Why exceptions plus error string

Using both mechanisms (C++ exceptions inside the engine; thread-local
string at the boundary) lets the engine's templated arithmetic code
unwind cleanly through arbitrary call depth without needing an
explicit early-return-on-error check at every step.

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`file-error.md`](file-error.md) — the boundary translator.
- [`file-overflow.md`](file-overflow.md) — primary `overflow_exception`
  source.
