# Incremental package installation

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
configuring a fresh source build. After changing package imports or adding a
package to `=distributed-packages`, regenerate it using an installed M2:

```sh
python3 M2/cmake/package-dependencies.py --m2 /path/to/M2
```

Commit the manifest alongside the source changes. CI checks it using the same
command with `--check` and rejects stale data. Ordinary source edits that do not
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

The literal scan is conservative and includes unused examples or code after
`end`; changes there can cause extra reinstalls. It cannot resolve computed
package names or arbitrary dynamically constructed code. Dependencies on
external executables and their versions are not tracked by this manifest.

To force a package to reinstall after changing an external dependency, remove
its `.cmake-installed` file under the build's `usr-dist` directory and rebuild
`install-<package>`. To force all installs, remove all such stamps. Do not delete
stamps while another build is running.

## Tests

```sh
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
