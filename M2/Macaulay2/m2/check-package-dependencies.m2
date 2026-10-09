-- Audit declared package dependencies without loading package bodies.
-- Usage:
--   M2 --script check-package-dependencies.m2 [--edges] [package-directory [package ...]]
--   M2 --script check-package-dependencies.m2 --self-test
-- By default use ../packages/=distributed-packages relative to this script.
-- Edges point from a package to its prerequisite. Both PackageImports and
-- PackageExports are dependencies. Core and User are runtime-provided leaves.
-- This is a header audit: needsPackage/loadPackage/importFrom calls in bodies,
-- documentation, and tests are not discovered by readPackage.
-- Exit status: 0 = acyclic, 1 = cycles, 2 = unreadable/invalid/missing headers.

-- Load the checkout's Graphs package so the audit uses the SCC method under test.
loadPackage("Graphs", FileName => currentFileDirectory | "../packages/Graphs.m2", Reload => true);

(
packageDependencyCycles := adj -> (
    V := sort keys adj;
    E := flatten apply(V, v -> apply(adj#v, w -> {v, w}));
    D := digraph(V, E, EntryMode => "edges");
    sort apply(select(stronglyConnectedComponents D,
        C -> #C > 1 or member(first C, children(D, first C))), sort)
    );

if isMember("--self-test", commandLine) then (
    assert(packageDependencyCycles(hashTable {"A" => {"B", "C"},
        "B" => {"D"}, "C" => {"D"}, "D" => {}}) == {});
    assert(packageDependencyCycles(hashTable {"A" => {"A"}}) == {{"A"}});
    assert(packageDependencyCycles(hashTable {"A" => {"B"}, "B" => {"A"},
        "C" => {"D"}, "D" => {"E"}, "E" => {"C"}, "F" => {"A"}})
        == {{"A", "B"}, {"C", "D", "E"}});
    assert(packageDependencyCycles(hashTable {"A" => {"B"}, "B" => {"C"},
        "C" => {"A", "D"}, "D" => {"B"}}) == {{"A", "B", "C", "D"}});
    assert(packageDependencyCycles(hashTable {}) == {});
    print "PASS: dependency graph self-tests";
    exit 0;
    );

-- --script passes its remaining arguments through to commandLine.
scriptIndex := position(commandLine, arg -> arg == "--script");
if scriptIndex === null then error "run this file with M2 --script";
arguments := drop(commandLine, scriptIndex + 2);
showEdges := isMember("--edges", arguments);
arguments = select(arguments, arg -> arg != "--edges");
packageDirectory := realpath(if #arguments == 0
    then currentFileDirectory | "../packages/" else first arguments);
if last packageDirectory != "/" then packageDirectory = packageDirectory | "/";
packageNames := if #arguments > 1 then drop(arguments, 1) else
    select(lines get(packageDirectory | "=distributed-packages"), pkg -> match("^[a-zA-Z0-9]+$", pkg));
packageNames = sort unique packageNames;
-- Nested readPackage calls in headers must also use this source tree.
path = prepend(packageDirectory, path);

adjacency := new MutableHashTable from {"Core" => {}, "User" => {}};
edgeKinds := new MutableHashTable;
problems := new MutableHashTable;
headerCount := 0;
local readHeader;
readHeader = pkg -> (
    if adjacency#?pkg then return;
    adjacency#pkg = {}; -- mark before descending, including in the cyclic case
    src := packageDirectory | pkg | ".m2";
    if not fileExists src then (
        problems#(#problems) = "missing package source: " | src;
        return;
        );
    opts := try readPackage(pkg, FileName => src) else null;
    if opts === null then (
        problems#(#problems) = "could not read package header: " | src;
        return;
        );
    headerCount = headerCount + 1;
    deps := flatten apply({PackageImports, PackageExports}, k ->
        apply(select(opts#k, dep -> dep =!= null), dep -> (
            if not instance(dep, String) then (
                problems#(#problems) = pkg | ": non-string " | toString k | " entry";
                null
                )
            else (
                e := (pkg, dep);
                if not edgeKinds#?e then edgeKinds#e = set {};
                edgeKinds#e = edgeKinds#e + set {toString k};
                dep
                )
            )));
    adjacency#pkg = sort unique select(deps, dep -> dep =!= null);
    scan(adjacency#pkg, readHeader);
    );
scan(packageNames, readHeader);

print("Read " | toString headerCount | " package headers from " | packageDirectory);
print("Roots: " | toString (#packageNames) | "; dependency edges: " | toString (#(keys edgeKinds)));
if showEdges then scan(sort keys edgeKinds, e ->
    print(e#0 | " -> " | e#1 | " [" | demark(", ", rsort toList edgeKinds#e) | "]"));
cycles := packageDependencyCycles adjacency;
print("Cyclic components: " | toString (#cycles));
scan(cycles, C -> (
    print("  {" | demark(", ", C) | "}");
    scan(C, v -> scan(select(adjacency#v, w -> isMember(w, C)),
        w -> print("    " | v | " -> " | w | " [" |
            demark(", ", rsort toList edgeKinds#(v, w)) | "]")));
    ));
scan(#problems, i -> stderr << problems#i << endl);
if #problems > 0 then (
    stderr << "Dependency audit incomplete: " << (#problems) << " header error(s)" << endl;
    exit 2;
    );
exit if #cycles > 0 then 1 else 0
)
