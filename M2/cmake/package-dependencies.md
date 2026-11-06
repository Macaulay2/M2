# Package dependencies and incremental CMake installation

This guide is for package authors and contributors to the CMake build. Commands
start at the repository root (the directory containing `M2/`). Manifest
generation uses an **externally installed M2** to read this checkout's sources;
it does not require a fresh M2 build or a configured CMake build directory.

## Declaring dependencies

Declare packages needed when loading your package in its `newPackage` header:

```m2
newPackage("ExamplePackage", PackageImports => {"Graphs"})
```

CMake uses `PackageImports` and `PackageExports` as installation prerequisites.
In this example it installs Graphs before ExamplePackage. `Core` and `User` are
provided by the runtime and need no separate package installations. Other
declared dependencies must be in `M2/Macaulay2/packages/=distributed-packages`.

Installation prerequisites must be acyclic. The generator checks declared
imports together with CMake's documentation bootstrap order: Style, then
FirstPackage, then Macaulay2Doc, then the remaining packages. A cycle can arise
from combining these rules even if the declared imports alone are acyclic.
The error reports a cycle path, for example:

```text
Cyclic installation dependencies: Alpha -> Beta -> Alpha
```

Literal `needsPackage`, `loadPackage`, and `importFrom` calls in package bodies,
auxiliary `.m2` files, documentation and tests contribute **source dependencies**.
Each installation tracks the transitive source files, but these calls do not
require completed installations. Lazy imports and examples may legitimately
form cycles, such as Polyhedra/FourTiTwo. Editing a file in such a cycle
invalidates the dependent installations without creating a build-order cycle.

The source scan is conservative: unused examples or code after `end` may cause
extra reinstalls. It recognizes literal names in the distributed package set,
but cannot resolve computed names or arbitrary dynamically generated code.
External executables and their versions are not tracked.

## Updating and checking the manifest

After adding, removing or changing imports, or changing `=distributed-packages`,
regenerate and check the manifest:

```sh
python3 M2/cmake/package-dependencies.py --m2 M2
python3 M2/cmake/package-dependencies.py --m2 M2 --check
git diff -- M2/cmake/package-dependencies.cmake
```

Replace `--m2 M2` with `--m2 /absolute/path/to/M2` to select another executable.
The generator uses `readPackage` through `M2/cmake/package-headers.m2` to read the
checkout's headers without loading package bodies, then validates the combined
installation graph. Missing or invalid headers, unknown dependencies and cycles
cause a nonzero exit status and leave the previous manifest intact. With
`--check`, stale manifest contents also cause failure.

Commit `M2/cmake/package-dependencies.cmake` alongside the source changes; do not
edit it by hand. Ordinary source edits that leave dependencies unchanged need
no manifest update. CMake includes the committed file during configuration,
so configuring a fresh build requires neither an existing M2 nor the generator.
Continue to run `installPackage` and `check` for changed packages: manifest
validation does not execute their bodies, examples or tests.

## Incremental installation

`install-packages` and `install-<package>` use file-based rules. Successful
installation and Info compression create `.cmake-installed` beside the package's
`.installed` file. Failed or interrupted installations are retried; unchanged
completed packages are skipped. Existing installations without the new stamp
are installed once. Selecting a package through `PACKAGES` also selects its
declared imports and the documentation bootstrap packages.

With `IgnoreExampleErrors=true`, an installation can finish without producing
`.installed`. Its completion is still recorded by `.cmake-installed`, so ignored
example errors do not cause repeated installations of unchanged packages.

All installations depend on `M2-core` and track the M2 binary and Core sources,
including source additions and removals. The generated Core directory is not
watched; `tvalues.m2` is an explicit dependency, so its creation during the first
build does not change the recorded source list.

`all-<package>` first installs the package, then runs its checks. `all-packages`
does this for every selected package. Installations are reused, but checks run
on every invocation. A failed installation prevents that package's checks;
retrying after a check failure does not repeat a successful installation:

```sh
cmake --build M2/BUILD/build --target all-Graphs
cmake --build M2/BUILD/build --target all-packages
```

Replace `M2/BUILD/build` with your build directory. The standalone
`check-<package>` and `check-packages` targets run checks without first building
the corresponding installations.

## Reinstalling after untracked changes

After changing an external dependency, force a package to reinstall by removing
its completion stamp and building its installation target. For example:

```sh
find M2/BUILD/build/usr-dist -type f -path '*/Graphs/.cmake-installed' -delete
cmake --build M2/BUILD/build --target install-Graphs
```

To force all selected packages to reinstall:

```sh
find M2/BUILD/build/usr-dist -type f -name .cmake-installed -delete
cmake --build M2/BUILD/build --target install-packages
```

Adjust the build path in both commands. Do not remove stamps while a build is
running. These commands work with Ninja and Makefile builds.

## CI and tests

The [Package dependencies workflow](../../.github/workflows/package-dependencies.yml)
installs an external M2, checks manifest freshness and runs dependency-generation
and incremental-installation fixtures. It requires no full M2 source build.

To run the tests locally, install M2, Python 3, CMake 3.24 or newer, Ninja, Make
and a C compiler, then run:

```sh
python3 M2/cmake/tests/test_package_dependencies.py
python3 M2/cmake/tests/test_dependency_manifest.py
python3 M2/cmake/tests/test_incremental_packages.py
python3 M2/cmake/tests/test_package_build_graph.py
```

The generator tests cover dependency extraction, stale manifests, header errors
and installation cycles. The incremental tests use a small fake installer with
Ninja and Unix Makefiles to check restart, freshness, failures, missing outputs,
package selection, ignored example errors and cyclic source dependencies. They
also check that combined `all-*` targets reuse installations and repeat checks.

The full-package fixture configures every distributed package with a stub
runtime under both generators and checks that the Ninja graph is acyclic.
Each installation rule has its own CMake subdirectory to avoid recursively
duplicating shared prerequisite rules in every consumer.
