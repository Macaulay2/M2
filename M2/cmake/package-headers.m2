-- Emit tab-separated package/prerequisite pairs for package-dependencies.py.
-- readPackage reads headers without loading package bodies.
(
packageDirectory := currentFileDirectory | "../Macaulay2/packages/";
path = prepend(packageDirectory, path);
packageNames := select(lines get(packageDirectory | "=distributed-packages"),
    pkg -> match("^[a-zA-Z0-9]+$", pkg));
scan(packageNames, pkg -> (
    src := packageDirectory | pkg | ".m2";
    if not fileExists src then error("missing package source: ", src);
    opts := readPackage(pkg, FileName => src);
    deps := select(opts.PackageImports | opts.PackageExports, dep -> dep =!= null);
    scan(unique deps, dep -> (
        if not instance(dep, String) then
            error(pkg, ": expected a package name in PackageImports/PackageExports");
        stdio << pkg << "\t" << dep << endl;
        ));
    ));
)
exit 0
