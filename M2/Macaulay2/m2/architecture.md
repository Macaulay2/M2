# Core M2 layer architecture

This document is the **architectural reference** for
`M2/Macaulay2/m2/` — the ~100 `.m2` files that the interpreter
loads at startup to define the **Core package** (everything you
get without a `needsPackage` call).

[← m2/ overview](README.md) · [← top-level TOC](../../../README.md#repository-architecture-table-of-contents)

## Position in the four-language stack

```
c/        scc1 translates...
d/        interpreter source...
e/        engine, linked into M2 binary
m2/       ← Core M2 source — loaded at startup
          to define Core's content
[Macaulay2/m2/]
```

Unlike c/d/e which are *compiled into the M2 binary*, files in
m2/ are **loaded at runtime**. The M2 binary starts with no Core
loaded; the `loadsequence` script (see [`file-loadsequence.md`](file-loadsequence.md))
brings everything in.

## The startup load sequence

```
M2-binary starts
   │
   ▼ bin/main.cpp
IM2_initialize() initialises engine globals
   │
   ▼ d/M2lib.c
interpreter starts
   │
   ▼ embedded startup.c
runs startup.m2 (compiled into the binary)
   │
   ▼ startup.m2 sources
Core.m2 + loadsequence + every file listed in loadsequence
   │
   ▼
Core fully loaded; prompt appears
```

The **`loadsequence` script** (which is a literal M2 file) lists
the ~100 m2 files in their canonical load order. Order matters —
type declarations must precede method definitions, packages must
follow the package machinery, etc.

## The four conceptual layers

```
                ┌──────────────────────────────────────────────┐
                │   Bootstrap: Core.m2, loadsequence,           │
                │              exports.m2, packages.m2          │
                ├──────────────────────────────────────────────┤
                │   Type system + dispatch:                     │
                │   classes.m2, methods.m2, expressions.m2,     │
                │   option.m2                                   │
                ├──────────────────────────────────────────────┤
                │   Core mathematical types and operations:     │
                │   rings, matrices, modules, GB, resolutions,  │
                │   homological algebra, ...                    │
                ├──────────────────────────────────────────────┤
                │   Documentation DSL + system integration:     │
                │   document.m2, installPackage.m2, html.m2,    │
                │   examples.m2, ...                            │
                └──────────────────────────────────────────────┘
```

## The Core package

Everything `m2/` defines lives in one package: **`Core`**. This is
the package every other package inherits from automatically (i.e.,
`needsPackage` is unnecessary for Core symbols).

`Core` exports thousands of symbols:

- **Types** — `Ring`, `Module`, `Matrix`, `Ideal`, `Variety`,
  `ChainComplex`, ...
- **Operators** — `+`, `*`, `^`, `==`, `||`, `|`, ...
- **Functions** — `gb`, `resolution`, `degree`, `kernel`,
  `Hom`, `Ext`, `Tor`, ...
- **Variables** — `QQ`, `ZZ`, `RR`, `CC`, `Boolean`, ...

See [`file-Core.md`](file-Core.md) for the package declaration
and [`file-exports.md`](file-exports.md) for the public-symbol
manifest.

## Type system: classes, methods, dispatch

M2's type system is **value-oriented and method-dispatched**.
Every value has a class; every class has methods.

```
Type (a HashTable of method-name → function)
   │
   ▼ inheritance chain (via parent)
Thing → Nothing → Boolean → ZZ → QQ → ...
        ↑          ↑          ↑
        │          │          │
     classes.m2 sets these up via interpreter's
     primitive classes.dd ([`../d/file-classes-dd.md`](../d/file-classes-dd.md))
```

Method lookup walks the inheritance chain. See:

- [`file-classes.md`](file-classes.md) — class machinery.
- [`file-methods.md`](file-methods.md) — method declaration.
- [`file-expressions.md`](file-expressions.md) — `Expression`
  AST used by `format`.
- [`file-option.md`](file-option.md) — `OptionTable` keyword args.

## The documentation DSL

A defining feature of M2: documentation is **typed M2 values**
that get installed alongside packages. See
[`file-document.md`](file-document.md) for the DSL.

```m2
doc ///
Key
   Module
Headline
   the class of finitely-presented modules
Description
  Text
    A Module is ...
  Example
    M = R^3
SeeAlso
   FreeModule
///
```

The documentation system processes these into:

- **HTML pages** ([`file-html.md`](file-html.md)).
- **Info-database entries** for `help` / `viewHelp`.
- **Texinfo source** for the manual.

The example code in `Example` blocks is **executed during
`installPackage`** — its outputs become part of the captured
docs, ensuring docs stay in sync with reality.

## Calling into the engine

```
M2 user:        I = ideal(x, y); gb I
   ↓
m2/gb.m2        wraps user call, validates args, options
   ↓
m2/freemod.m2   converts M2 Matrix to engine RawMatrix
   ↓
m2/raw.m2       raw…() calls — Core's C-ABI veneer
   ↓
d/interface.dd  interpreter binding
   ↓
e/interface/groebner.cpp   engine entry point
   ↓
e/comp-gb.cpp              algorithm
```

The "raw" interface (`rawGB`, `rawMatrix`, ...) is what most
Core m2 code calls. Higher-level Core functions wrap raw calls
with type-checking and conversion. See
[`../e/architecture.md#the-engine-boundary`](../e/architecture.md#the-engine-boundary).

## The package system

Beyond Core, M2 has ~400 distributed packages
([`../packages/`](../packages/README.md)). The package machinery
lives in:

| File | Role |
|---|---|
| [`file-packages.md`](file-packages.md) | `newPackage`, `loadPackage`, package state |
| [`file-installPackage.md`](file-installPackage.md) | Build HTML / info / examples |
| [`file-document.md`](file-document.md) | Documentation DSL |
| [`file-examples.md`](file-examples.md) | Example-running infrastructure |
| [`file-help.md`](file-help.md) | `help` / `viewHelp` / `?` |

A package author writes `Foo.m2` using `newPackage(...)`,
`export {...}`, `beginDocumentation()`, `doc ///...///`, and
`TEST ///...///`. The infrastructure here handles the rest.

## I/O and formatting

The pretty-printer and net (2D character grid) system live here:

| File | Role |
|---|---|
| [`file-nets.md`](file-nets.md) | `Net` operations |
| [`file-format.md`](file-format.md) | `format` / `toString` |
| [`file-printing.md`](file-printing.md) | Output dispatching |
| [`file-html.md`](file-html.md), [`file-texmacs.md`](file-texmacs.md) | HTML / TeXmacs export |

The interpreter has its own `Net` primitive ([`../d/file-nets.md`](../d/file-nets.md)),
but the M2-level operations and dispatch live here.

## Mathematical entry points

The bulk of m2/ is the **mathematical Core**: dozens of files
implementing the user-facing API for rings, modules, matrices,
ideals, etc. Examples:

- `ring.m2` — ring construction (`QQ[x,y]`, `R/I`, `frac R`, ...).
- `matrix.m2` — matrix construction and operations.
- `module.m2` — module construction.
- `gb.m2` — Gröbner basis user API.
- `res.m2` — resolution user API.
- `hilbert.m2` — Hilbert function / Betti.
- `homology.m2` — Ext, Tor, Hom.
- `varieties.m2` — Variety, ProjectiveVariety, AffineVariety.
- `schemes.m2` — Scheme (older API).

Each wraps the engine's raw computational primitives in
M2-idiomatic syntax.

## How to extend Core

Adding a new function:

1. **Pick the right file** — by topic or load order.
2. **Use `method(...)`** to declare it:
   ```m2
   foo = method(Options => {...})
   foo Ring := opts -> R -> ...
   ```
3. **Add to `exports.m2`** if user-visible.
4. **Document** in
   [`../packages/file-Macaulay2Doc.md`](../packages/file-Macaulay2Doc.md).
5. **Test** with `TEST` blocks or in
   [`../tests/normal/`](../tests/normal/file-normal-tests-catalogue.md).

Adding a new type:

1. **Declare** with `NewType := new Type of OldType`.
2. **Define `==`, `?`, `hash`** if needed.
3. **Define class methods** as for any operation.
4. **Document and test** as above.

## Cross-cutting concerns

| Concern | Where |
|---|---|
| Error handling | [`file-debugging.md`](file-debugging.md) |
| Memory (GC) | Inherited from the interpreter |
| Threading | [`file-threads.md`](file-threads.md) |
| Random numbers | `random.m2` |
| Time / profiling | `time.m2` (see [`README.md`](README.md)), [`file-profile.md`](file-profile.md) |
| File I/O | Various — see [`file-system.md`](file-system.md) |

## Related

- [`README.md`](README.md) — m2/ navigation hub.
- [`../c/architecture.md`](../c/architecture.md) — `scc1`
  translator architecture.
- [`../d/architecture.md`](../d/architecture.md) — interpreter
  architecture this layer runs on.
- [`../e/architecture.md`](../e/architecture.md) — engine
  architecture this layer calls into.
- [`../packages/`](../packages/README.md) — packages that extend
  Core.
- [`../../../README.md#the-four-language-stack`](../../../README.md#the-four-language-stack)
  — overview of all four layers.
