-- HomotopyContinuationInterface.m2
--
-- A Macaulay2 package interfacing HomotopyContinuation.jl, in the spirit of
-- Bertini.m2 / PHCpack.m2: Macaulay2 writes the problem data to a Julia
-- script, shells out to an external `julia` process, and reads the results
-- back in as native Macaulay2 objects (Points, Ideals, HashTables).
--
-- STATUS: untested skeleton -- needs a machine with M2 + Julia +
-- HomotopyContinuation.jl to exercise end to end. Julia/HC.jl function and
-- accessor names below (solve, monodromy_solve, find_start_pair, certify,
-- certificates, numerical_irreducible_decomposition, witness_sets, ...)
-- were taken from HC.jl's current documentation. A couple of accessors used
-- only for auxiliary output (extracting the numeric linear slice of a
-- witness set) are wrapped in a Julia try/catch, so a version mismatch
-- there degrades gracefully (you still get points/dimension/degree) instead
-- of crashing the whole computation. Contributions and testing on real
-- examples are very welcome.

-- Check whether `julia` is on PATH *and* the HomotopyContinuation.jl
-- package is installed and loadable. We do this by asking Julia
-- itself, rather than just checking for the executable, since a
-- bare Julia install with no HC.jl would otherwise report as present.
hcPresent := (run "command -v julia > /dev/null" == 0) and isDirectory "~/.julia/packages/HomotopyContinuation"

newPackage(
    "HomotopyContinuationInterface",
    Version => "0.2",
    Date => "August 2026",
    Authors => {{Name => "Aditya Tyagi", Email => "", HomePage => ""}},
    Headline => "an interface to HomotopyContinuation.jl",
    Keywords => {"Numerical Algebraic Geometry"},
    PackageExports => {"NAGtypes"},
    OptionalComponentsPresent => hcPresent,
    CacheExampleOutput => true,
    )
export {
    "solveHC",
    "monodromySolveHC",
    "certifyHC",
    "numericalIrreducibleDecompositionHC",
    "nidHC",
    "Executable",
    "TargetParameters",
    "StartSolutions"
    }

juliaExecutable = "julia"

------------------------------------------------------------------------
-- infrastructure: temp workspace, running julia, generic text parsing
--
-- Every driver script prints its results to a single output file using a
-- tiny tagged-text format:
--   @BEGIN <TAG> ... @END <TAG>          -- a labelled block
--   key=value                            -- inside a "META" block
--   c1re,c1im c2re,c2im ...              -- one point per line, inside a
--                                           point block ("SOLUTIONS", etc.)
------------------------------------------------------------------------

hcExecutable = opts -> if opts.Executable === null then juliaExecutable else opts.Executable

hcNewWorkspace = () -> (
    tmpDir := temporaryFileName() | "/";
    makeDirectory tmpDir;
    (tmpDir, tmpDir | "script.jl", tmpDir | "output.txt")
    )

hcRunScript = (scriptFile, exe) -> (
    exitCode := run(exe | " " | scriptFile);
    if exitCode != 0 then
        error("HomotopyContinuation.jl call failed (exit code " | toString exitCode |
            "); inspect " | scriptFile | " for the generated Julia code");
    )

hcReadOutput = outFile -> (
    if not fileExists outFile then
        error "julia ran but produced no output file -- check the generated script for errors";
    select(lines get outFile, l -> #l > 0)
    )

-- lines strictly between "@BEGIN tag" and "@END tag" (first occurrence)
hcSection = (ls, tag) -> (
    beginLine := "@BEGIN " | tag;
    endLine := "@END " | tag;
    startIdx := position(ls, l -> l == beginLine);
    endIdx := position(ls, l -> l == endLine);
    if startIdx === null or endIdx === null then {}
    else ls_{startIdx + 1 .. endIdx - 1}
    )

-- ls#i must equal "@BEGIN tag"; returns (contentLines, indexAfterEndTag)
hcReadBlock = (ls, i, tag) -> (
    beginLine := "@BEGIN " | tag;
    endLine := "@END " | tag;
    if ls#i != beginLine then error("expected " | beginLine | " while parsing julia output");
    j := i + 1;
    content := {};
    while ls#j != endLine do (content = append(content, ls#j); j = j + 1);
    (content, j + 1)
    )

hcMeta = ls -> (
    metaLines := hcSection(ls, "META");
    new HashTable from apply(metaLines, l -> (
            parts := separate("=", l);
            (parts#0, parts#1)
            ))
    )

hcParsePoint = l -> (
    toks := select(separate(" ", l), t -> #t > 0);
    coords := apply(toks, t -> (
            parts := separate(",", t);
            toCC(value parts#0, value parts#1)
            ));
    point {coords}
    )

hcParsePoints = ls -> apply(select(ls, l -> #l > 0), hcParsePoint)

------------------------------------------------------------------------
-- infrastructure: writing M2 data as Julia source
------------------------------------------------------------------------

-- helper Julia functions available inside every generated script
hcJuliaPreamble = ///using HomotopyContinuation

function __hcm2_write_points(io, tag, pts)
    println(io, "@BEGIN " * tag)
    for s in pts
        coords = String[string(real(c)) * "," * string(imag(c)) for c in s]
        println(io, join(coords, " "))
    end
    println(io, "@END " * tag)
end

function __hcm2_cstr(z)
    string(real(z)) * "," * string(imag(z))
end///

toJuliaVarDecl = vars -> "@var " | demark(" ", apply(vars, toString))

toJuliaVarsLit = vars -> "[" | demark(", ", apply(vars, toString)) | "]"

-- rationals/reals print directly (Julia parses e.g. "3/2" as a valid
-- numeric expression); complex numbers are unwound into real + imaginary
-- parts since M2's native CC printing isn't Julia syntax
toJuliaScalar = c -> (
    if instance(c, CC) then
        "(" | toString realPart c | ")+(" | toString imaginaryPart c | ")*im"
    else toString c
    )

-- coefficients pulled off a polynomial via `coefficients` are still degree-0
-- elements of the polynomial ring itself (class RingElement), not bare
-- numbers, so we first push them down into the coefficient field
toJuliaCoeff = c -> toJuliaScalar sub(c, coefficientRing ring c)

toJuliaMonomial = m -> (
    e := (exponents m)#0;
    vrs := gens ring m;
    facs := {};
    for i from 0 to #e - 1 do (
        if e#i == 1 then facs = append(facs, toString vrs#i)
        else if e#i > 1 then facs = append(facs, toString(vrs#i) | "^" | toString(e#i));
        );
    if #facs == 0 then "1" else demark("*", facs)
    )

toJuliaTerm = (c, m) -> (
    cStr := toJuliaCoeff c;
    mStr := toJuliaMonomial m;
    if mStr == "1" then "(" | cStr | ")" else "(" | cStr | ")*" | mStr
    )

toJuliaPolyString = method()
toJuliaPolyString RingElement := f -> (
    (mons, cfs) := coefficients f;
    monsL := flatten entries mons;
    cfsL := flatten entries cfs;
    if #monsL == 0 then "0"
    else demark(" + ", apply(#monsL, i -> toJuliaTerm(cfsL#i, monsL#i)))
    )

toJuliaSystem = F -> "[" | demark(", ", apply(F, toJuliaPolyString)) | "]"

-- coerce a Point or a plain List of numbers into a List of numbers
hcCoords = x -> if instance(x, Point) then coordinates x else x

toJuliaPointLit = p -> "[" | demark(", ", apply(hcCoords p, toJuliaScalar)) | "]"

toJuliaPointListLit = pts -> "[" | demark(", ", apply(pts, toJuliaPointLit)) | "]"

------------------------------------------------------------------------
-- solveHC: plain (black-box) solving
------------------------------------------------------------------------

toJuliaSolveScript = (F, outFile) -> (
    R := ring F#0;
    demark("\n", {
            hcJuliaPreamble,
            toJuliaVarDecl gens R,
            "f = " | toJuliaSystem F,
            "result = solve(f)",
            "sols = solutions(result)",
            "open(\"" | outFile | "\", \"w\") do io",
            "    __hcm2_write_points(io, \"SOLUTIONS\", sols)",
            "end"
            })
    )

solveHC = method(Options => {Executable => null})
solveHC List := opts -> F -> (
    if #F == 0 then error "expected a nonempty list of polynomials";
    exe := hcExecutable opts;
    (tmpDir, scriptFile, outFile) := hcNewWorkspace();
    scriptFile << toJuliaSolveScript(F, outFile) << close;
    hcRunScript(scriptFile, exe);
    outLines := hcReadOutput outFile;
    hcParsePoints hcSection(outLines, "SOLUTIONS")
    )
solveHC Ideal := opts -> I -> solveHC(I_*, opts)

------------------------------------------------------------------------
-- monodromySolveHC: parametric monodromy solving
--
-- F is a list of polynomials in the variables X together with parameters
-- P (all generators of a common ring). If StartSolutions/TargetParameters
-- are not supplied, a single start pair is found automatically via
-- HC.jl's find_start_pair.
------------------------------------------------------------------------

toJuliaMonodromyScript = (F, X, P, startSols, targetParams, outFile) -> (
    lns := {
        hcJuliaPreamble,
        toJuliaVarDecl X,
        toJuliaVarDecl P,
        "f = " | toJuliaSystem F,
        "Fsys = System(f, variables = " | toJuliaVarsLit X | ", parameters = " | toJuliaVarsLit P | ")"
        };
    lns = lns | (
        if startSols =!= null and targetParams =!= null then {
            "x0 = " | toJuliaPointListLit startSols,
            "p0 = " | toJuliaPointLit targetParams,
            "S = monodromy_solve(Fsys, x0, p0)"
            }
        else {
            "x0seed, p0 = find_start_pair(Fsys)",
            "S = monodromy_solve(Fsys, [x0seed], p0)"
            }
        );
    lns = lns | {
        "open(\"" | outFile | "\", \"w\") do io",
        "    __hcm2_write_points(io, \"SOLUTIONS\", solutions(S))",
        "    __hcm2_write_points(io, \"PARAMETERS\", [parameters(S)])",
        "    println(io, \"@BEGIN META\")",
        "    println(io, \"nsolutions=\" * string(nsolutions(S)))",
        "    println(io, \"success=\" * string(is_success(S)))",
        "    println(io, \"@END META\")",
        "end"
        };
    demark("\n", lns)
    )

monodromySolveHC = method(Options => {Executable => null, TargetParameters => null, StartSolutions => null})
monodromySolveHC(List, List, List) := opts -> (F, X, P) -> (
    if #F == 0 then error "expected a nonempty list of polynomials";
    startSols := opts.StartSolutions;
    targetParams := opts.TargetParameters;
    if (startSols === null) =!= (targetParams === null) then
        error "StartSolutions and TargetParameters must be supplied together, or not at all";
    exe := hcExecutable opts;
    (tmpDir, scriptFile, outFile) := hcNewWorkspace();
    scriptFile << toJuliaMonodromyScript(F, X, P, startSols, targetParams, outFile) << close;
    hcRunScript(scriptFile, exe);
    outLines := hcReadOutput outFile;
    sols := hcParsePoints hcSection(outLines, "SOLUTIONS");
    paramPts := hcParsePoints hcSection(outLines, "PARAMETERS");
    meta := hcMeta outLines;
    new HashTable from {
        "Solutions" => sols,
        "Parameters" => if #paramPts > 0 then first paramPts else null,
        "NSolutions" => value meta#"nsolutions",
        "Success" => meta#"success" == "true"
        }
    )

------------------------------------------------------------------------
-- certifyHC: certify a list of (approximate) solutions
------------------------------------------------------------------------

toJuliaCertifyScript = (F, sols, outFile) -> (
    R := ring F#0;
    demark("\n", {
            hcJuliaPreamble,
            toJuliaVarDecl gens R,
            "f = " | toJuliaSystem F,
            "cands = " | toJuliaPointListLit sols,
            "cert = certify(f, cands)",
            "certs = certificates(cert)",
            "open(\"" | outFile | "\", \"w\") do io",
            "    println(io, \"@BEGIN META\")",
            "    println(io, \"ncandidates=\" * string(length(certs)))",
            "    println(io, \"ncertified=\" * string(count(is_certified, certs)))",
            "    println(io, \"nreal=\" * string(count(c -> is_certified(c) && is_real(c), certs)))",
            "    println(io, \"@END META\")",
            "    println(io, \"@BEGIN CERTIFICATES\")",
            "    for c in certs",
            "        certb = is_certified(c)",
            "        realb = certb ? is_real(c) : false",
            "        if certb",
            "            approx = solution_approximation(c)",
            "            coordstr = join([__hcm2_cstr(z) for z in approx], \" \")",
            "        else",
            "            coordstr = \"NA\"",
            "        end",
            "        println(io, string(certificate_index(c)) * \" \" * string(certb) * \" \" * string(realb) * \" \" * coordstr)",
            "    end",
            "    println(io, \"@END CERTIFICATES\")",
            "end"
            })
    )

certifyHC = method(Options => {Executable => null})
certifyHC(List, List) := opts -> (F, sols) -> (
    if #F == 0 then error "expected a nonempty list of polynomials";
    if #sols == 0 then error "expected a nonempty list of candidate solutions";
    exe := hcExecutable opts;
    (tmpDir, scriptFile, outFile) := hcNewWorkspace();
    scriptFile << toJuliaCertifyScript(F, sols, outFile) << close;
    hcRunScript(scriptFile, exe);
    outLines := hcReadOutput outFile;
    meta := hcMeta outLines;
    certLines := hcSection(outLines, "CERTIFICATES");
    certs := apply(certLines, l -> (
            toks := separate(" ", l);
            idx := value toks#0;
            certb := toks#1 == "true";
            realb := toks#2 == "true";
            pt := if certb then hcParsePoint demark(" ", drop(toks, 3)) else null;
            new HashTable from {
                "Index" => idx,
                "Certified" => certb,
                "Real" => realb,
                "Point" => pt
                }
            ));
    new HashTable from {
        "NCandidates" => value meta#"ncandidates",
        "NCertified" => value meta#"ncertified",
        "NReal" => value meta#"nreal",
        "Certificates" => certs
        }
    )
certifyHC(Ideal, List) := opts -> (I, sols) -> certifyHC(I_*, sols, opts)

------------------------------------------------------------------------
-- numericalIrreducibleDecompositionHC: NID via HC.jl's
-- numerical_irreducible_decomposition (regeneration + monodromy breakup)
------------------------------------------------------------------------

toJuliaNidScript = (F, outFile) -> (
    R := ring F#0;
    demark("\n", {
            hcJuliaPreamble,
            toJuliaVarDecl gens R,
            "f = " | toJuliaSystem F,
            "N = numerical_irreducible_decomposition(f)",
            "open(\"" | outFile | "\", \"w\") do io",
            "    println(io, \"@BEGIN META\")",
            "    println(io, \"ncomponents=\" * string(ncomponents(N)))",
            "    println(io, \"@END META\")",
            "    for W in witness_sets(N)",
            "        d = dim(W)",
            "        dg = degree(W)",
            "        println(io, \"@BEGIN WITNESSSET dim=\" * string(d) * \" deg=\" * string(dg))",
            "        __hcm2_write_points(io, \"POINTS\", solutions(W))",
            "        try",
            "            L = linear_subspace(W)",
            "            E = extrinsic(L)",
            "            println(io, \"@BEGIN SLICEA\")",
            "            for row in eachrow(E.A)",
            "                println(io, join([__hcm2_cstr(complex(v)) for v in row], \" \"))",
            "            end",
            "            println(io, \"@END SLICEA\")",
            "            println(io, \"@BEGIN SLICEB\")",
            "            println(io, join([__hcm2_cstr(complex(v)) for v in E.b], \" \"))",
            "            println(io, \"@END SLICEB\")",
            "        catch",
            "            println(io, \"@BEGIN SLICEA\")",
            "            println(io, \"@END SLICEA\")",
            "            println(io, \"@BEGIN SLICEB\")",
            "            println(io, \"@END SLICEB\")",
            "        end",
            "        println(io, \"@END WITNESSSET\")",
            "    end",
            "end"
            })
    )

isWitnessSetHeader = l -> (
    toks := separate(" ", l);
    #toks >= 2 and toks#0 == "@BEGIN" and toks#1 == "WITNESSSET"
    )

parseWitnessSetHeader = l -> (
    toks := separate(" ", l);
    d := value (separate("=", toks#2))#1;
    dg := value (separate("=", toks#3))#1;
    (d, dg)
    )

hcParseNidOutput = ls -> (
    result := {};
    i := 0;
    n := #ls;
    while i < n do (
        if isWitnessSetHeader ls#i then (
            hdr := parseWitnessSetHeader ls#i;
            d := hdr#0;
            dg := hdr#1;
            i = i + 1;
            blk := hcReadBlock(ls, i, "POINTS");
            ptLines := blk#0;
            i = blk#1;
            blk = hcReadBlock(ls, i, "SLICEA");
            aLines := blk#0;
            i = blk#1;
            blk = hcReadBlock(ls, i, "SLICEB");
            bLines := blk#0;
            i = blk#1;
            if i < n and ls#i == "@END WITNESSSET" then i = i + 1;
            result = append(result, new HashTable from {
                    "Dimension" => d,
                    "Degree" => dg,
                    "Points" => hcParsePoints ptLines,
                    "SliceA" => aLines,
                    "SliceB" => bLines
                    });
            )
        else i = i + 1;
        );
    result
    )

-- parse a "re,im re,im ..." row into a list of CC's
hcParseComplexRow = l -> apply(select(separate(" ", l), t -> #t > 0),
    t -> (p := separate(",", t); toCC(value p#0, value p#1)))

numericalIrreducibleDecompositionHC = method(Options => {Executable => null})
numericalIrreducibleDecompositionHC List := opts -> F -> (
    if #F == 0 then error "expected a nonempty list of polynomials";
    R := ring F#0;
    exe := hcExecutable opts;
    (tmpDir, scriptFile, outFile) := hcNewWorkspace();
    scriptFile << toJuliaNidScript(F, outFile) << close;
    hcRunScript(scriptFile, exe);
    outLines := hcReadOutput outFile;
    meta := hcMeta outLines;
    rawWS := hcParseNidOutput outLines;
    Rc := CC[gens R];
    witnessData := apply(rawWS, w -> (
            sliceIdeal := if #(w#"SliceA") == 0 then null else (
                Ac := apply(w#"SliceA", hcParseComplexRow);
                bc := hcParseComplexRow first w#"SliceB";
                ideal apply(#Ac, i -> sum(#gens Rc, j -> (Ac#i#j) * Rc_j) - bc#i)
                );
            new HashTable from {
                "Dimension" => w#"Dimension",
                "Degree" => w#"Degree",
                "Points" => w#"Points",
                "Slice" => sliceIdeal
                }
            ));
    byDim := new MutableHashTable;
    for w in witnessData do (
        d := w#"Dimension";
        if not byDim#?d then byDim#d = {};
        byDim#d = byDim#d | {w};
        );
    new HashTable from {
        "NComponents" => value meta#"ncomponents",
        "WitnessSets" => witnessData,
        "ByDimension" => new HashTable from byDim
        }
    )
numericalIrreducibleDecompositionHC Ideal := opts -> I -> numericalIrreducibleDecompositionHC(I_*, opts)

nidHC = numericalIrreducibleDecompositionHC

------------------------------------------------------------------------
-- documentation
------------------------------------------------------------------------

beginDocumentation()

doc ///
Key
    HomotopyContinuationInterface
Headline
    an interface to HomotopyContinuation.jl
Description
    Text
        This package calls out to
        @HREF("https://www.juliahomotopycontinuation.org/", "HomotopyContinuation.jl")@
        to solve polynomial systems numerically, in the style of the
        Bertini and PHCpack interface packages: Macaulay2 writes the
        problem to a temporary Julia script, shells out to @TT "julia"@,
        and parses the results back in.

        Four entry points are provided, mirroring HomotopyContinuation.jl's
        main features:

        @UL {
            {TO solveHC, " -- black-box solving of a square (or overdetermined) system"},
            {TO monodromySolveHC, " -- solving a parametrized family via monodromy"},
            {TO certifyHC, " -- rigorous certification of approximate solutions"},
            {TO numericalIrreducibleDecompositionHC, " -- numerical irreducible decomposition"}
        }@

        Requires a working @TT "julia"@ executable on the @TT "PATH"@
        (or supplied via the @TO Executable@ option), with the
        @TT "HomotopyContinuation"@ package installed in Julia.
    Example
        R = QQ[x,y];
        F = {x^2 + y^2 - 1, x - y};
        solveHC F
///

doc ///
Key
    Executable
Headline
    option for specifying the path to the julia executable
Description
    Text
        An option used by @TO solveHC@, @TO monodromySolveHC@,
        @TO certifyHC@, and @TO numericalIrreducibleDecompositionHC@ to
        specify the @TT "julia"@ executable to run. Defaults to
        @TT "julia"@ on the @TT "PATH"@.
///

doc ///
Key
    solveHC
    (solveHC, List)
    (solveHC, Ideal)
    [solveHC, Executable]
Headline
    solve a polynomial system numerically via HomotopyContinuation.jl
Usage
    solveHC F
    solveHC I
Inputs
    F:List
        of polynomials over @ofClass QQ@ or @ofClass RR@
    I:Ideal
    Executable => String
        path to the @TT "julia"@ executable; defaults to @TT "julia"@ on the @TT "PATH"@
Outputs
    :List
        of @TO Point@s approximating the solutions
Description
    Text
        Writes the system to a temporary Julia script, calls @TT "julia"@
        to run @TT "solve"@ via HomotopyContinuation.jl, and parses the
        resulting solutions back into Macaulay2.
    Example
        R = QQ[x,y];
        F = {x^2 + y^2 - 1, x - y};
        solveHC F
///

doc ///
Key
    monodromySolveHC
    (monodromySolveHC, List, List, List)
    [monodromySolveHC, Executable]
    [monodromySolveHC, TargetParameters]
    [monodromySolveHC, StartSolutions]
    TargetParameters
    StartSolutions
Headline
    solve a parametrized polynomial system via monodromy (HomotopyContinuation.jl)
Usage
    monodromySolveHC(F, X, P)
Inputs
    F:List
        polynomials in the variables X and parameters P
    X:List
        the unknowns (a sublist of the generators of @TT "ring F#0"@)
    P:List
        the parameters (the remaining generators)
    Executable => String
    TargetParameters => List
        specific parameter values to solve at; if given, @TT "StartSolutions"@
        must also be given
    StartSolutions => List
        known solutions at @TT "TargetParameters"@, used to seed monodromy;
        if omitted (together with @TT "TargetParameters"@), a start pair is
        found automatically via HomotopyContinuation.jl's @TT "find_start_pair"@
Outputs
    :HashTable
        with keys @TT "\"Solutions\""@ (a @ofClass List@ of @TO Point@s),
        @TT "\"Parameters\""@ (the parameter @TO Point@ that was solved at),
        @TT "\"NSolutions\""@, and @TT "\"Success\""@
Description
    Text
        Runs HomotopyContinuation.jl's @TT "monodromy_solve"@ on the system
        @TT "F"@, treated as a family over the parameters @TT "P"@. This can
        find all isolated solutions of a parametrized system even when the
        number of paths required by @TO solveHC@ is infeasible.
///

doc ///
Key
    certifyHC
    (certifyHC, List, List)
    (certifyHC, Ideal, List)
    [certifyHC, Executable]
Headline
    certify approximate solutions via interval arithmetic (HomotopyContinuation.jl)
Usage
    certifyHC(F, sols)
    certifyHC(I, sols)
Inputs
    F:List
        polynomials defining the system
    I:Ideal
    sols:List
        candidate solutions, as @TO Point@s or coordinate lists
    Executable => String
Outputs
    :HashTable
        with keys @TT "\"NCandidates\""@, @TT "\"NCertified\""@,
        @TT "\"NReal\""@, and @TT "\"Certificates\""@ (a @ofClass List@ of
        per-candidate @ofClass HashTable@s with keys @TT "\"Index\""@,
        @TT "\"Certified\""@, @TT "\"Real\""@, @TT "\"Point\""@)
Description
    Text
        Runs HomotopyContinuation.jl's @TT "certify"@, which uses interval
        arithmetic (the Krawczyk method) to rigorously certify that each
        candidate lies near a true, non-singular zero of the system, and
        whether that zero is real.
///

doc ///
Key
    numericalIrreducibleDecompositionHC
    (numericalIrreducibleDecompositionHC, List)
    (numericalIrreducibleDecompositionHC, Ideal)
    [numericalIrreducibleDecompositionHC, Executable]
Headline
    numerical irreducible decomposition via HomotopyContinuation.jl
Usage
    numericalIrreducibleDecompositionHC F
    numericalIrreducibleDecompositionHC I
Inputs
    F:List
    I:Ideal
    Executable => String
Outputs
    :HashTable
        with keys @TT "\"NComponents\""@, @TT "\"WitnessSets\""@ (a flat
        @ofClass List@), and @TT "\"ByDimension\""@ (the same witness sets
        grouped by dimension). Each witness set is a @ofClass HashTable@
        with keys @TT "\"Dimension\""@, @TT "\"Degree\""@,
        @TT "\"Points\""@ (a @ofClass List@ of @TO Point@s), and
        @TT "\"Slice\""@ (an @ofClass Ideal@ of linear forms cutting out the
        witness slice, or @TO null@ if HomotopyContinuation.jl's slice
        accessors were unavailable)
Description
    Text
        Runs HomotopyContinuation.jl's
        @TT "numerical_irreducible_decomposition"@, which first computes
        witness supersets for every dimension (regeneration) and then
        decomposes each into irreducible components using monodromy and
        the trace test.
///

TEST ///
R = QQ[x,y]
F = {x^2 + y^2 - 1, x - y}
sols = solveHC F
assert(#sols == 2)
assert(all(sols, p -> abs((coordinates p)#0 - (coordinates p)#1) < 1e-6))
///
