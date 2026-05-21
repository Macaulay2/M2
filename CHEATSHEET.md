# M2 development cheatsheet

One-page quick reference. Each row links to the in-depth doc.

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) ·
[Index](INDEX.md) · [Tour](TOUR.md)

## Build

| Want | Command | Doc |
|---|---|---|
| First CMake build | `cmake -GNinja -S M2 -B M2/BUILD/build && cmake --build M2/BUILD/build --target build-libraries build-programs M2-core M2-emacs` | [BUILD.md](BUILD.md) |
| Release build (optimised) | add `-DCMAKE_BUILD_TYPE=Release` to `cmake -S M2 -B …` | [BUILD.md](BUILD.md) |
| First autotools build | `cd M2/BUILD/build && ../../autogen.sh && ../../configure --with-system-gc --with-fplll && make -j$(nproc) -C libraries && make -j$(nproc)` | [BUILD.md](BUILD.md) |
| Rebuild after `.cpp` change | `cmake --build M2/BUILD/build --target M2-core` | [BUILD.md](BUILD.md) |
| Rebuild after `.m2` Core change | `cmake --build M2/BUILD/build --target M2-core` (Core M2 baked into binary) | [m2/architecture.md](M2/Macaulay2/m2/architecture.md) |
| Install packages | `cmake --build M2/BUILD/build --target install-packages` | [PACKAGES.md](PACKAGES.md) |
| Single package install | inside M2: `installPackage "Foo"` | [PACKAGES.md](PACKAGES.md) |
| Iterate on one package (no docs) | inside M2: `loadPackage("Foo", Reload => true)` | [PACKAGES.md](PACKAGES.md) |
| Full clean | `rm -rf M2/BUILD/build` (CMake) — never `make clean` mid-build | [BUILD.md](BUILD.md) |

## Test

| Want | Command | Doc |
|---|---|---|
| Quick smoke | `M2/BUILD/build/M2 -q --check 1` | [TESTING.md](TESTING.md) |
| Medium | `… --check 2` | [TESTING.md](TESTING.md) |
| Slow / nightly | `… --check 3` | [TESTING.md](TESTING.md) |
| Engine gtest | `cd M2/BUILD/build && cmake --build . --target M2-unit-tests && ctest -R unit-tests` | [TESTING.md](TESTING.md) |
| Single package check | `cd M2/BUILD/build && ctest -R "check-Foo"` | [TESTING.md](TESTING.md) |
| Doc link check | `make -C M2/BUILD/build/Macaulay2/html-check-links check` | [DOCUMENTATION-SYSTEM.md](DOCUMENTATION-SYSTEM.md) |
| Spell-check packages | `codespell --ignore-words=.codespell_ignore M2/Macaulay2/packages` | [STYLE.md](STYLE.md) |
| ComputationsBook (CTest) | `cd M2/BUILD/build && ctest -j4 -R ComputationsBook` | [TESTING.md](TESTING.md) |

## Debug

| Symptom | Try | Doc |
|---|---|---|
| Startup crash | `gdb --args M2/BUILD/build/M2 -q` then `bt` | [DEBUG.md](DEBUG.md), [STARTUP.md](STARTUP.md) |
| GB / resolution wrong answer | `gbTrace = 3` in M2 session | [DEBUG.md](DEBUG.md), [COMPUTATIONS.md](COMPUTATIONS.md) |
| Memory corruption | rebuild with `-DMEMDEBUG` (see [memdebug](M2/Macaulay2/d/file-c-glue.md)) | [MEMORY.md](MEMORY.md), [DEBUG.md](DEBUG.md) |
| Threading race | rebuild with TSAN: `CXXFLAGS="-fsanitize=thread"` | [THREADING.md](THREADING.md), [DEBUG.md](DEBUG.md) |
| Slow computation | profile with `perf record M2 -q script.m2` | [DEBUG.md](DEBUG.md) |
| `installPackage` fails | check the `.m2` file for syntax errors first; doc DSL errors are usually clear | [DOCUMENTATION-SYSTEM.md](DOCUMENTATION-SYSTEM.md) |
| "library not found" at link | check the `Find*.cmake` for the lib; might need `apt install lib<x>-dev` | [DEPENDENCIES.md](DEPENDENCIES.md) |

## Find things

| Want | Where |
|---|---|
| A specific source file's deep dive | look for `file-<basename>.md` in the same directory |
| Engine class `Foo` | grep `class Foo` in `M2/Macaulay2/e/*.hpp`; doc usually `file-foo.md` in same dir |
| M2-level function `bar` | grep `setupfun("bar"` in `M2/Macaulay2/d/*.d` (binding side) and `bar = method(...)` in `m2/*.m2` (M2 side) |
| Ring type `R`'s engine class | [RING-ZOO.md](RING-ZOO.md) has a mapping table |
| Computation strategy | [COMPUTATIONS.md](COMPUTATIONS.md) |
| A doc by concept name | [GLOSSARY.md](GLOSSARY.md) — 96 entries with deep-dive links |
| A doc when you don't remember its location | [INDEX.md](INDEX.md) — flat alphabetical |
| Reading order for your role | [TOUR.md](TOUR.md) — newcomer / debugger / package author / build maintainer / engine extender / algorithm-curious |

## Add things

| Want | Steps | Doc |
|---|---|---|
| New M2 package | place `Foo.m2` in `M2/Macaulay2/packages/`, then add `Foo` to `=distributed-packages` | [PACKAGES.md](PACKAGES.md) |
| New engine function | implement in `e/`, expose via `e/interface/<area>.{h,cpp}`, bind in `d/<area>.dd`, wrap in `m2/<area>.m2`, gtest in `e/unit-tests/` | [e/architecture.md](M2/Macaulay2/e/architecture.md) |
| New coefficient ring | new `aring-<name>.{cpp,hpp}` in `e/`, register in `aring.hpp`, wrap with `ConcreteRing<>`; M2-level constructor in `m2/` | [RING-ZOO.md](RING-ZOO.md), [e/coefficient-rings.md](M2/Macaulay2/e/coefficient-rings.md) |
| New computation engine | subclass `Computation` in `e/`, register strategy in [`comp.cpp`](M2/Macaulay2/e/file-comp.md) dispatcher | [COMPUTATIONS.md](COMPUTATIONS.md) |
| New `.d` operator | add to `tokens.d`, parse rule to `parser.d`, evaluator rule to `actors4.d`, bind via `setupop` | [d/architecture.md](M2/Macaulay2/d/architecture.md) |
| New external dependency | `Find<Foo>.cmake` in `M2/cmake/`, list in `check-libraries.cmake`, optional fallback in `build-libraries.cmake` | [DEPENDENCIES.md](DEPENDENCIES.md) |

## Common slowdowns and fixes

| Why builds are slow | Fix |
|---|---|
| Touching `engine-includes.hpp` rebuilds ~30 files | scope new includes elsewhere if possible — see [file-engine-includes-hpp.md](M2/Macaulay2/e/file-engine-includes-hpp.md) |
| Touching `Core.m2` rebuilds the whole `M2-core` target | use `loadPackage` for in-session iteration |
| `installPackage` is very slow | only run when docs or examples changed; use `loadPackage` otherwise |
| Repeated `cmake` configure | `cmake --build` only re-configures when `CMakeLists.txt` changes; CMake caching usually works |
| Cold ccache | enable `ccache`; the first build still takes ~5 min but rebuilds become near-instant |

## File-naming cheat

| Source file | Deep-dive file |
|---|---|
| `foo.cpp` (only) | `file-foo.md` |
| `foo.hpp` (only) | `file-foo.md` |
| `foo.cpp` + `foo.hpp` (different docs) | `file-foo-cpp.md` + `file-foo-hpp.md` |
| `foo.cpp` + `foo.hpp` (one doc) | `file-foo.md` |
| Family of related files | consolidated `file-<family>.md` |
| Directory architectural overview | `architecture.md` (no `file-` prefix) |
| Directory file index | `README.md` |
| Engine "area" concept doc | `<area>.md` (no `file-` prefix) e.g. `coefficient-rings.md` |

## See also

- [CONTRIBUTING-DOCS.md](CONTRIBUTING-DOCS.md) — full doc conventions
- [TOUR.md](TOUR.md) — guided reading orders
- [INDEX.md](INDEX.md) — flat alphabetical index
- [GLOSSARY.md](GLOSSARY.md) — terminology
