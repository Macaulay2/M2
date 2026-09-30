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

packageDependencyCycles = adjacency -> (
    vertices := sort keys adjacency;
    arcs := flatten apply(vertices, name -> apply(adjacency#name,
        dependency -> {name, dependency}));
    D := digraph(vertices, arcs, EntryMode => "edges");
    sort apply(select(stronglyConnectedComponents D,
        component -> #component > 1 or member(first component,
            children(D, first component))), sort)
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
scriptIndex = position(commandLine, arg -> arg == "--script");
if scriptIndex === null then error "run this file with M2 --script";
arguments = drop(commandLine, scriptIndex + 2);
showEdges = isMember("--edges", arguments);
arguments = select(arguments, arg -> arg != "--edges");
packageDirectory = realpath(if #arguments == 0
    then currentFileDirectory | "../packages/" else first arguments);
if last packageDirectory != "/" then packageDirectory = packageDirectory | "/";
packageNames = if #arguments > 1 then drop(arguments, 1) else
    select(lines get(packageDirectory | "=distributed-packages"), name -> match("^[a-zA-Z0-9]+$", name));
packageNames = sort unique packageNames;
-- Nested readPackage calls in headers must also use this source tree.
path = prepend(packageDirectory, path);

adjacency = new MutableHashTable from {"Core" => {}, "User" => {}};
edgeKinds = new MutableHashTable;
problems = {};
headerCount = 0;
local readHeader;
readHeader = name -> (
    if adjacency#?name then return;
    adjacency#name = {}; -- mark before descending, including in the cyclic case
    filename := packageDirectory | name | ".m2";
    if not fileExists filename then (
        problems = append(problems, "missing package source: " | filename);
        return;
        );
    opts := try readPackage(name, FileName => filename) else null;
    if opts === null then (
        problems = append(problems, "could not read package header: " | filename);
        return;
        );
    headerCount = headerCount + 1;
    scan({PackageImports, PackageExports}, kind -> (
        scan(select(opts#kind, dependency -> dependency =!= null), dependency -> (
            if not instance(dependency, String) then
                problems = append(problems, name | ": non-string " | toString kind | " entry")
            else (
                adjacency#name = append(adjacency#name, dependency);
                edge := (name, dependency);
                if not edgeKinds#?edge then edgeKinds#edge = {};
                edgeKinds#edge = append(edgeKinds#edge, toString kind);
                );
            ));
        ));
    adjacency#name = sort unique adjacency#name;
    scan(adjacency#name, readHeader);
    );
scan(packageNames, readHeader);

print("Read " | toString headerCount | " package headers from " | packageDirectory);
print("Roots: " | toString (#packageNames) | "; dependency edges: " | toString (#(keys edgeKinds)));
if showEdges then scan(sort keys edgeKinds, edge ->
    print(edge#0 | " -> " | edge#1 | " [" | demark(", ", edgeKinds#edge) | "]"));
cycles = packageDependencyCycles adjacency;
print("Cyclic components: " | toString (#cycles));
scan(cycles, component -> (
    print("  {" | demark(", ", component) | "}");
    scan(component, name -> scan(select(adjacency#name, dependency -> isMember(dependency, component)),
        dependency -> print("    " | name | " -> " | dependency | " [" |
            demark(", ", edgeKinds#(name, dependency)) | "]")));
    ));
scan(problems, problem -> stderr << problem << endl);
if #problems > 0 then (
    stderr << "Dependency audit incomplete: " << (#problems) << " header error(s)" << endl;
    exit 2;
    );
exit if #cycles > 0 then 1 else 0
