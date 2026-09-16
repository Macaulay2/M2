# Package dependencies and incremental CMake installation

This guide is for package authors changing imports and for contributors working
on the CMake build. All shell commands below start at the repository root (the
directory containing `M2/`). The cycle checker and manifest generator can use an
**externally installed M2**: they inspect the package sources in this checkout,
not the installed copies of those packages. A freshly compiled M2 is not needed.

## Declaring dependencies

Declare packages needed when loading your package in its `newPackage` header,
for example:

```m2
newPackage("ExamplePackage", PackageImports => {"Graphs"})
```

Both `PackageImports` and `PackageExports` declare dependencies for the audit and
CMake installation order. Here `ExamplePackage -> Graphs` means that Graphs is a
prerequisite of ExamplePackage: CMake installs Graphs first. The checker follows
these dependencies transitively. `Core` and `User` are provided by the runtime
and do not need separate package installations.

Keep declared dependencies acyclic, including avoiding self-imports. If A
imports B and B imports A, consider moving their shared functionality into a
third package that both can import. Do not move a required import into a package
body merely to evade the header check: the build still needs an accurate
installation order.

Calls such as `needsPackage "Graphs"`, `loadPackage "Graphs"`, and
`importFrom("Graphs", {...})` in package bodies, documentation, or tests are
handled differently. They may run only when a method or example is evaluated;
they are not automatically installation-order requirements. The manifest scans
literal package names in these calls to track source changes. This source-only
graph may contain cycles; see the details below.

## Checking for cycles before submitting a change

Run the header audit after changing `PackageImports` or `PackageExports`:

```sh
M2 --script M2/Macaulay2/m2/check-package-dependencies.m2
```

By default it starts with every package in
`M2/Macaulay2/packages/=distributed-packages` and follows their declared imports.
It uses `readPackage` to read headers without loading package bodies. To inspect
a single package and its prerequisites, including a new package not yet in the
distribution list, give the package directory and root package names explicitly:

```sh
M2 --script M2/Macaulay2/m2/check-package-dependencies.m2 --edges M2/Macaulay2/packages FirstPackage
```

Replace `FirstPackage` with your package name; additional names may follow it.
`--edges` prints each dependency and whether it came from `PackageImports`,
`PackageExports`, or both. Use an absolute path to an M2 executable in place of
`M2` if needed. To check another source tree, pass that tree's package directory.

A successful audit prints `Cyclic components: 0` and exits with status 0. A cycle
exits with status 1 and lists each mutually dependent group and its internal
edges. For example, headers where AuditA imports AuditB and AuditB exports AuditA
produce:

```text
Cyclic components: 1
  {AuditA, AuditB}
    AuditA -> AuditB [PackageImports]
    AuditB -> AuditA [PackageExports]
```

Missing, unreadable, or invalid headers cause exit status 2: the audit is
incomplete, even if no cycles were found in the headers it could read.

The header audit does **not** inspect imports in package bodies, documentation,
or tests, and does not include CMake's implicit documentation bootstrap order.
Also run the manifest check to validate that combined installation graph and
check that the committed dependency data matches the sources:

```sh
python3 M2/cmake/package-dependencies.py --m2 M2 --check
```

This command exits nonzero for cycles, dependencies outside the distributed
package set, header errors, or a stale manifest. A passing header audit alone is
therefore not the complete CMake dependency check. Neither check proves that
runtime imports or package examples will succeed; still run `installPackage`
and `check` for your package.

## How CMake uses the dependencies

`install-packages` and `install-<package>` use file-based rules. Successful
installation and Info compression create `.cmake-installed` beside the package's
`.installed` file. An interrupted or failed installation leaves no completion
stamp, so the next invocation retries it and skips unchanged completed packages.
Existing installations without the new stamp are installed once.

All installs depend on `M2-core` and track the M2 binary and Core source files.
Style initializes the shared documentation search index; FirstPackage follows
Style, and Macaulay2Doc follows both. Every other package follows Macaulay2Doc.
A package selected through `PACKAGES` also brings in its declared imports and
these bootstrap packages.

## Updating the dependency manifest

The checked-in `package-dependencies.cmake` avoids requiring an existing M2 when
configuring a fresh source build. CMake includes this file; it does not run the
M2 audit, Python generator, or dependency test scripts during configuration.
Regenerate it after adding, removing, or changing declared imports or literal
body/example/test imports, or changing `=distributed-packages`:

```sh
python3 M2/cmake/package-dependencies.py --m2 /path/to/M2
```

Commit the manifest alongside the source changes. CI checks it using the same
command with `--check` and rejects stale data. The generator validates the graph
before writing the file, so a cycle or header error leaves the previous manifest
intact. Do not hand-edit the generated file. Ordinary source edits that do not
change dependencies need no manifest update; CMake tracks the source files.

The generator first runs `check-package-dependencies.m2`, which uses `readPackage`
to read `PackageImports` and `PackageExports` from the checkout. These imports
become dependencies on completed installations. The generator also checks for
cycles introduced by combining imports with the documentation bootstrap order.

Literal `needsPackage`, `loadPackage`, and `importFrom` calls in package bodies,
auxiliary `.m2` files, documentation, and tests form a separate source-dependency
graph. Each install tracks the transitive source files in that graph. Those
edges do not require completed installations: lazy imports can legitimately
form cycles, such as Polyhedra/FourTiTwo. Files in these cycles invalidate one
another's installed results when edited, without introducing a build-order cycle.

The bootstrap order can add cycles even when the declared-import graph is
acyclic. For example, if Style declares a dependency on an ordinary package, that
package already depends on Macaulay2Doc, which depends on Style. The generator
rejects this with `Cyclic installation dependencies` and a cycle path.

The literal scan recognizes names in the distributed package set. It is
conservative and includes unused examples or code after `end`; changes there
can cause extra reinstalls. It cannot resolve computed
package names or arbitrary dynamically constructed code. Dependencies on
external executables and their versions are not tracked by this manifest.

## Reinstalling after untracked changes

To force a package to reinstall after changing an external dependency, remove
its `.cmake-installed` file under the build's `usr-dist` directory and rebuild
`install-<package>`. To force all installs, remove all such stamps. Do not delete
stamps while another build is running.

## CI and tests

The [Package dependencies workflow](../../.github/workflows/package-dependencies.yml)
runs on pushes and pull requests. It installs an external M2, runs the header
audit and its self-tests, verifies that synthetic cycles and missing headers
fail, checks manifest freshness, and exercises dependency generation and CMake
installation with test fixtures. These checks require no full M2 source build.

To run the checker self-tests and the build-dependency tests locally, install
M2, Python 3, CMake 3.30 or newer, Ninja, Make, and a C compiler, then run:

```sh
M2 --script M2/Macaulay2/m2/check-package-dependencies.m2 --self-test
python3 M2/cmake/tests/test_package_dependencies.py
python3 M2/cmake/tests/test_dependency_manifest.py
python3 M2/cmake/tests/test_incremental_packages.py
python3 M2/cmake/tests/test_package_build_graph.py
```

`test_incremental_packages.py` exercises the real package CMake rules with a small fake installer
under both Ninja and Unix Makefiles. It covers restart, freshness, failed
compression, missing outputs, selected-package import closure, and source cycles.

`test_package_build_graph.py` configures all distributed packages with a stub
runtime and checks that the generated Ninja graph is acyclic. This also exercises
configuration of the full graph with both generators. Each install command has
its own CMake subdirectory to avoid recursively duplicating shared prerequisite
rules in every consumer.
