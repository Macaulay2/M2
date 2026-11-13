newPackage(
    "PALPInterface",
    Version => "0.2",
    Date => "20 September 2026",
    Headline => "an interface to the PALP package for lattice polytopes",
    Authors => {
        {Name => "Mike Stillman",
            Email => "mike@math.cornell.edu",
            HomePage => "https://mikestillman.github.io"}
        },
    Keywords => {"Interfaces", "Toric Geometry"},
    HomePage => "https://github.com/mikestillman/stringtorics",
    AuxiliaryFiles => false,
    DebuggingMode => true,
    -- Polyhedra is exported, so that vertices (of a NefPartition, for instance)
    -- and the Polyhedron returned by weightSystemPolytope can be used at once
    PackageExports => {"Polyhedra"},
    PackageImports => {"NormalToricVarieties", "LLLBases"}
    )

export {
    -- locating and running the PALP executables
    "palpProgram",
    "palpRun",
    "toPalp",
    "fromPalpMatrix",

    -- combined weight systems
    "WeightSystem",
    "weightSystem",

    -- poly.x
    "palpNormalForm",
    "palpMVertices",
    "palpMPoints",
    "palpNVertices",
    "palpNPoints",
    "palpInfo",
    "palpIsReflexive",
    "palpIsIP",
    "palpIsCanonical",

    -- weight systems
    "weightSystems",
    "Reflexive",
    "weightSystemVertices",
    "weightSystemPolytope",

    -- nef partitions
    "palpNefPartitions",
    "NefPartition",
    "Codim",                     -- the keys of a NefPartition
    "Parts",
    "Vertices",
    "Properties",
    "Euler",                     -- with HodgeNumbers, keys of its cache
    "HodgeZero",
    "Projection",                -- the possible entries of its Properties
    "DirectProduct",
    "DualProjection",
    "hodgeNumbers",
    "hodgeZero",
    "properties",
    "HodgeNumbers",
    "Products",
    "Projections",
    "KeepSymmetric",
    "Lattice",
    "isDirectProduct",
    "isProjection",
    "isDualProjection",
    "cayleyPolytopeVertices",
    "palpHodgeNumbers",
    "naiveNefPartitions",
    "nefPartitionDivisors"
    }

exportMutable {
    "palpVerbosity",
    "palpDmax"
    }

palpVerbosity = 0

WeightSystem = new Type of BasicList
NefPartition = new Type of HashTable
  -- The keys are Codim, Parts, Vertices, Properties and cache; see
  -- palpNefPartitions below.  The Properties, as PALP's source (palp-2.21)
  -- defines them:
  --   projection: some part is a single vertex v of Delta^*, so nabla_j = conv(0, v)
  --     has just 2 lattice points (Remove_Proj in E_Poly.c).  nef.x prints these
  --     only with -P, and gives them no label.
  --   direct product: the ranks of the sets {0} \cup E_j add up to dim Delta^*, so
  --     Delta^* is a free sum (Dir_Product in Nefpart.c).  Printed only with -D,
  --     marked " D".
  --   nef.x also marks a partition "DP" when its dual nef partition is a projection.
  --   The "P:k" field in nef.x output is just a counter, not the type.

-------------------------------------------------
-- Locating the PALP executables -----------------
-------------------------------------------------

-- PALP is compiled with a fixed upper bound POLY_Dmax on the dimensions it can
-- handle, and `make all-dims` builds one executable per bound.  A program named
-- e.g. "poly" is therefore available as poly-4d.x, poly-5d.x, poly-6d.x,
-- poly-11d.x, and poly.x (which is the same build as poly-6d.x).
--
-- The requirement is  POLY_Dmax >= dim N + codim - 1,  so codim 1 computations
-- (poly.x, cws.x) are fine with the default build up to dimension 6, while nef
-- partitions run out much sooner: dim 5 with codim 3, or dim 6 with codim 2,
-- already need the 11d build.  We therefore prefer the largest build.
palpDmax = 11

palpDmaxSuffixes = new HashTable from {4 => "-4d", 5 => "-5d", 6 => "", 11 => "-11d"}

palpProgramCache = new MutableHashTable

palpProgram = method(TypicalValue => Program)
palpProgram String := name -> (
    if not palpDmaxSuffixes#?palpDmax then error(
        "palpDmax should be one of " | demark(", ", toString \ sort keys palpDmaxSuffixes));
    if palpProgramCache#?(name, palpDmax) then return palpProgramCache#(name, palpDmax);
    exe := name | palpDmaxSuffixes#palpDmax | ".x";
    P := findProgram(exe, exe | " -h", RaiseError => false);
    if P === null and palpDmaxSuffixes#palpDmax =!= "" then (
        -- the requested variant is not installed; the default build is always 6d
        exe = name | ".x";
        P = findProgram(exe, exe | " -h", RaiseError => false);
        );
    if P === null then error(
        "cannot find the PALP program " | name | ".x" | newline |
        "PALP is at https://hep.itp.tuwien.ac.at/~kreuzer/CY/CYpalp.html, or use" | newline |
        "  brew install macaulay2/tap/palp");
    palpProgramCache#(name, palpDmax) = P
    )

-------------------------------------------------
-- Running a PALP executable ---------------------
-------------------------------------------------

-- true if the string contains no non-whitespace character
isBlankString = s -> not match("[^[:space:]]", s)

-- The compile-time bounds PALP complains about, as they appear in its messages
-- ("Please increase POLY_Dmax to at least 8 ...", "Need SYM_Nmax > 46083 !!").
-- None of these strings occurs in ordinary PALP output.
palpLimits = {"POLY_Dmax", "POINT_Nmax", "VERT_Nmax", "AMBI_Dmax", "SYM_Nmax", "WDIM"}

-- PALP takes its input from a file named on the command line.  We write that
-- file under M2's session temporary directory, which M2 removes on exit, rather
-- than into the current directory.  Set palpVerbosity > 0 to see where it is,
-- or use KeepFiles => true to hang on to it.
runPALP = method(Options => {KeepFiles => false})
runPALP(Program, String, String) := String => opts -> (P, args, input) -> (
    f := temporaryFileName() | ".palp";
    f << input << close;
    if palpVerbosity > 0 then printerr(
        "palp: " | P#"name" | " " | args | " " | f | newline | input);
    result := runProgram(P, args | " " | f, RaiseError => false);
    if opts.KeepFiles
    then printerr("palp: input left in " | f)
    else removeFile f;
    out := result#"output";
    if result#"return value" =!= 0 then error(
        "PALP command failed: " | result#"command" | newline | result#"error");
    if not isBlankString result#"error" then error(
        "PALP wrote to stderr: " | result#"command" | newline | result#"error");
    -- PALP is compiled with fixed bounds on the size of everything it handles,
    -- and reports running out of one of them on stdout while still exiting with
    -- status 0, so this can only be caught by inspecting the output.  POLY_Dmax
    -- bounds the dimension and is the one palpDmax selects; the others are fixed
    -- when PALP is built.  SYM_Nmax, for instance, stops poly.x -N at dimension
    -- eight on a simplex.
    lim := select(palpLimits, v -> match(v, out));
    if #lim > 0 then error(
        P#"name" | " ran out of " | demark(", ", lim) | " on this input:" | newline |
        out |
        (if member("POLY_Dmax", lim)
         then "Set palpDmax to a larger value, or install that variant of PALP."
         else "This is a compile-time bound; PALP must be rebuilt with a larger value."));
    if palpVerbosity > 1 then printerr out;
    out
    )

palpRun = method(Options => {KeepFiles => false})
palpRun(String, String, Matrix) :=
palpRun(String, String, List) :=
palpRun(String, String, WeightSystem) := String => opts -> (name, args, input) ->
    runPALP(palpProgram name, args, palpInput input, KeepFiles => opts.KeepFiles)

-------------------------------------------------
-- Translating to and from PALP's format ---------
-------------------------------------------------

-- A matrix becomes "#rows #cols" followed by the rows (each row on one line,
-- integer entries separated by spaces).  A list of integers is
-- passed through as a (combined) weight system, which PALP distinguishes from a
-- matrix by the shape of its first line.
toPalp = method()
toPalp Matrix := String => M -> (
    if ring M =!= ZZ then error "expected a matrix over ZZ";
    if numRows M === 0 or numColumns M === 0 then error "expected a non-empty matrix";
    concatenate(
        toString numRows M, " ", toString numColumns M, newline,
        between(newline, for row in entries M list demark(" ", toString \ row)),
        newline)
    )
toPalp List := String => wlist -> (
    wlist = flatten wlist;   -- a list of rows, or already flat
    if #wlist === 0 then error "expected a non-empty weight system";
    if not all(wlist, a -> instance(a, ZZ)) then error "expected a list of integers";
    demark(" ", toString \ wlist) | newline
    )
toPalp WeightSystem := String => cws -> toPalp flatten toList cws

-- PALP writes a block of numbers preceded by a header line
--     <#lines> <#columns>  <comment>
-- Returns (#lines, #columns, rows), or null if there is no block at all.
palpReadBlock = str -> (
    L := select(lines str, ell -> not isBlankString ell);
    if #L === 0 then return null;
    hdr := select(separate(" +", L#0), x -> x =!= "");
    if #hdr < 2 or not all(take(hdr, 2), x -> match("^[0-9]+$", x))
    then error("expected a PALP matrix header, got: " | L#0);
    nlines := value hdr#0;
    ncols := value hdr#1;
    if #L < nlines + 1 then error(
        "PALP announced " | toString nlines | " rows but printed " | toString(#L - 1));
    rows := for ell in take(L, {1, nlines}) list
        for x in select(separate(" +", ell), y -> y =!= "") list (
            if not match("^-?[0-9]+$", x) then error(
                "expected an integer in PALP output, got: " | x);
            value x);
    (nlines, ncols, rows)
    )

-- The matrix exactly as PALP printed it.  No reorientation: this is the raw
-- reader, and is the inverse of toPalp.
fromPalpMatrix = method()
fromPalpMatrix String := Matrix => str -> (
    b := palpReadBlock str;
    if b === null then error "expected non-empty PALP output";
    (nlines, ncols, rows) := b;
    if not all(rows, r -> #r === ncols) then error(
        "PALP announced " | toString ncols | " columns, but its rows do not agree:"
        | newline | str);
    matrix rows
    )

-- PALP chooses the orientation of its output block per example rather than per
-- option -- poly.x -p prints 23 x 3 for one weight system and 2 x 10 for another
-- -- so we orient every point set the same way, one point per COLUMN.  The
-- dimension is the smaller of the two, since a full dimensional polytope in
-- dimension d has more than d points.
--
-- Returns null when PALP printed nothing (for instance poly.x -d applied to a
-- polytope that is not reflexive), and also when the rows carry an extra
-- inhomogeneous entry, which is how poly.x -e reports the inequalities of a
-- polytope that is not reflexive: those are not dual vertices.
palpPointMatrix = str -> (
    b := palpReadBlock str;
    if b === null then return null;
    (nlines, ncols, rows) := b;
    if not all(rows, r -> #r === ncols) then return null;
    M := matrix rows;
    if nlines <= ncols then M else transpose M
    )

-- PALP reads a matrix as a set of points and takes the smaller of its two
-- dimensions to be the dimension of the lattice, assuming throughout that the
-- points affinely span it.  It does not check.  Given a polytope of lower
-- dimension it will quietly return a wrong answer -- poly.x -v reports three of
-- the four vertices of a square lying in a plane in ZZ^3 -- or die on a signal:
-- poly.x -g on that same square exits with a bus error.  Given fewer points
-- than the dimension it prints "Identical points in Vec_Greater_Than !!".  None
-- of that is worth passing on to the caller, so check first.
palpFullDimensional = M -> (
    n := numColumns M;
    if n <= numRows M then false
    else rank((M_(toList(1 ..< n))) - M_{0} * matrix {toList((n-1) : 1)}) === numRows M
    )

checkPalpInput = method()
checkPalpInput Matrix := M -> (
    if numColumns M <= numRows M then error(
        "PALP needs more points than the dimension, but was given a "
        | toString numRows M | " by " | toString numColumns M | " matrix");
    if not palpFullDimensional M then error(
        "PALP needs the points to span the lattice affinely, but the columns of "
        | "this " | toString numRows M | " by " | toString numColumns M
        | " matrix span only dimension "
        | toString rank((M_(toList(1 ..< numColumns M))) - M_{0} * matrix {toList((numColumns M - 1) : 1)})
        | newline
        | "Change coordinates so that the polytope is full dimensional.");
    )

weightSystem = method(Options => {Verify => true})
weightSystem List := WeightSystem => opts -> q -> (
    -- q: a (possibly combined) weight system, either flat, as PALP writes it,
    --    {d_1, q_10, ..., q_1n, d_2, q_20, ..., q_2n, ...}, or as a list of rows
    --    {{d_1, q_10, ..., q_1n}, {d_2, q_20, ..., q_2n}, ...}
    -- returns a WeightSystem, the list of rows.  If q is not a weight system,
    --    this is an error, or with Verify => false, null.
    fail := msg -> (
        if opts.Verify then error msg;
        if debugLevel > 0 then << msg << endl;
        null);
    cws := if #q > 0 and all(q, r -> instance(r, List)) then new WeightSystem from q
    else (
        if #q === 0 or not all(q, a -> instance(a, ZZ)) then return fail(
            "expected a weight system, a non-empty list of integers or a list of rows, got "
            | toString q);
        -- split q into rows the way PALP does (ReadCwsPp in Coord.c): a row is a
        -- degree followed by weights adding up to it; zeros that come next still
        -- belong to the row, and the next positive number starts a new row
        blocks := {};
        cur := {q#0};
        rem := q#0;
        for a in drop(q, 1) do (
            if a > rem then (
                if rem > 0 then return fail "weights sum to more than expected sum";
                blocks = append(blocks, cur);
                cur = {a};
                rem = a;
                )
            else (
                cur = append(cur, a);
                rem = rem - a;
                ));
        if rem > 0 then return fail "weights sum to less than expected sum";
        new WeightSystem from append(blocks, cur));
    msg := weightSystemProblem cws;
    if msg =!= null then fail msg else cws
    )

-- two WeightSystems are equal when they have the same rows
WeightSystem == WeightSystem := Boolean => (a, b) -> toList a == toList b

-- weightSystemProblem cws
--   cws: a WeightSystem
--   returns null if cws is well-formed, i.e. it is a non-empty list of rows {d, q_0, ..., q_n} where
--     (1) every entry is an integer, and every q_i >= 0,
--     (2) d = q_0 + ... + q_n, and d > 0,
--     (3) every row has the same length,
--     (4) for each i, some row has q_i > 0,
--   and otherwise returns a String saying what is wrong.
weightSystemProblem = method()
weightSystemProblem WeightSystem := cws -> (
    if #cws === 0 then return "expected at least one weight system";
    for r in cws do (
        if not instance(r, List) then return "expected a list, got " | toString r;
        if #r < 2 then return "expected a row {d, q_0, ..., q_n}, got " | toString r;
        if not all(r, a -> instance(a, ZZ)) then
            return "expected integer entries, got " | toString r;
        if any(drop(r, 1), a -> a < 0) then
            return "expected nonnegative weights, got " | toString r;
        if r#0 <= 0 then return "expected a positive degree, got " | toString r;
        if r#0 =!= sum drop(r, 1) then
            return "expected the degree to be the sum of the weights, got " | toString r;
        );
    if #unique apply(toList cws, r -> #r) > 1 then
        return "expected every row to have the same number of weights, got " | toString toList cws;
    for i from 1 to #(cws#0) - 1 do
        if all(cws, r -> r#i === 0) then
            return "expected every weight to be positive in some row, but q_" | toString(i-1) | " is 0 in every row";
    null
    )

-- With debugLevel > 0, the reason for returning false is printed.
isWellDefined WeightSystem := Boolean => cws -> (
    msg := weightSystemProblem cws;
    if msg =!= null and debugLevel > 0 then << "-- isWellDefined WeightSystem: " << msg << endl;
    msg === null
    )

checkPalpInput WeightSystem := cws -> (
    msg := weightSystemProblem cws;
    if msg =!= null then error msg;
    -- poly.x fails an assertion in ReadCwsPp and aborts on a single weight system of dimension one
    if #cws === 1 and #(cws#0) === 3 then error(
        "PALP aborts on a weight system of dimension one; give the polytope as a "
        | "matrix instead, for instance matrix{{1,-1}}");
    )
-- weightSystem raises an error if q is not a weight system
checkPalpInput List := q -> checkPalpInput weightSystem q

-- The first copy of each column.  PALP mishandles repeated points: a repeated
-- vertex makes poly.x print "Identical points in Vec_Greater_Than !!" and stop,
-- and a repeated point that is not a vertex is kept, so that poly.x -p lists it
-- twice and poly.x -g counts it twice.  Neither changes the polytope, so drop them.
palpDistinctColumns = M -> (
    seen := new MutableHashTable;
    cols := entries transpose M;
    M_(select(#cols, j -> if seen#?(cols#j) then false else seen#(cols#j) = true))
    )

-- everything that hands a polytope to PALP goes through this.  A matrix over QQ
-- with integer entries, such as vertices of a Polyhedra polytope, is lifted to
-- ZZ; any other matrix not over ZZ is rejected by toPalp.
palpInput = A -> (
    if instance(A, Matrix) and ring A === QQ then A = try lift(A, ZZ) else A;
    if instance(A, Matrix) then A = palpDistinctColumns A;
    checkPalpInput A;
    toPalp A
    )

palpPoly = (args, A) -> palpPointMatrix runPALP(palpProgram "poly", args, palpInput A)

-------------------------------------------------
-- The basic poly.x computations -----------------
-------------------------------------------------

-- Each of these accepts either a matrix whose columns span the polytope or a
-- (combined) weight system, and returns a matrix whose columns are the points
-- asked for, or null if PALP produced no answer.

palpNormalForm = method()
palpNormalForm Matrix :=
palpNormalForm List :=
palpNormalForm WeightSystem := Matrix => A -> palpPoly("-N", A)

palpMVertices = method()
palpMVertices Matrix :=
palpMVertices List :=
palpMVertices WeightSystem := Matrix => A -> palpPoly("-v", A)

palpMPoints = method()
palpMPoints Matrix :=
palpMPoints List :=
palpMPoints WeightSystem := Matrix => A -> palpPoly("-p", A)

palpNVertices = method()
palpNVertices Matrix :=
palpNVertices List :=
palpNVertices WeightSystem := Matrix => A -> palpPoly("-e", A)

palpNPoints = method()
palpNPoints Matrix :=
palpNPoints List :=
palpNPoints WeightSystem := Matrix => A -> palpPoly("-d", A)

-- poly.x -g prints one line of "Key:value ..." fields, for example
--     M:7 6 N:27 8 Pic:17 Cor:0
--     10 1 1 2 2 2 2 M:378 6 N:8 6 H:2,0,350 [2160]
-- where M and N give (#points, #vertices), H the Hodge numbers, and the value in
-- brackets the Euler characteristic.
palpParseInfo = ell -> (
    H := new MutableHashTable;
    key := null;
    for t in select(separate(" +", ell), x -> x =!= "") do (
        if match("^\\[-?[0-9]+\\]$", t) then (
            H#"Euler" = value substring(t, 1, #t - 2);
            key = null;
            )
        else if match(":", t) then (
            i := first first regex(":", t);
            key = substring(t, 0, i);
            rest := substring(t, i + 1);
            H#key = if rest === "" then {} else for x in separate(",", rest) list value x;
            )
        else if key =!= null and match("^-?[0-9]+$", t)
        then H#key = append(H#key, value t)
        else key = null;
        );
    new HashTable from H
    )

palpInfo = method()
palpInfo Matrix :=
palpInfo List :=
palpInfo WeightSystem := HashTable => A -> (
    out := runPALP(palpProgram "poly", "-g", palpInput A);
    -- Every summary line poly.x -g prints has an "M:" field, even for input
    -- that is not reflexive or not IP ({9,1,1,2,5} gives "M:39 7 F:6").  So a
    -- line without one is something else, such as a PALP error message on
    -- stdout: before repeated columns were dropped, a repeated vertex produced
    -- "Identical points in Vec_Greater_Than !!" here.  No input is known to
    -- reach this now; it is a guard against returning an empty table.
    H := if isBlankString out then null else palpParseInfo first lines out;
    if H === null or not H#?"M" then error(
        "unexpected output from poly.x -g:" | newline | out);
    H
    )

-- Two PALP options answer this.  The summary line of poly.x -g carries an "N:"
-- field for a reflexive polytope and an "F:" field for one that is not, but -g,
-- like the other options that find lattice points (-p, -d), becomes expensive
-- as the dimension grows: on the simplex in dimension eight it takes three
-- seconds, where -e takes a few milliseconds.  So we use -e, which returns the
-- vertices of the dual when there are any and inequalities, which
-- palpPointMatrix reports as null, when there are not.  The two agree wherever
-- both are affordable.
palpIsReflexive = method()
palpIsReflexive Matrix :=
palpIsReflexive List :=
palpIsReflexive WeightSystem := Boolean => A -> palpNVertices A =!= null

-- poly.x -I prints "No IP" when the origin is not an interior point of the
-- polytope, and nothing when it is.  PALP calls a polytope IP when every facet
-- inequality <a,x> + c >= 0 has c > 0, so the origin must be strictly inside.
palpIsIP = method()
palpIsIP Matrix :=
palpIsIP List :=
palpIsIP WeightSystem := Boolean => A -> (
    out := runPALP(palpProgram "poly", "-I", palpInput A);
    if match("No IP", out) then false
    else if isBlankString out then true
    else error("unexpected output from poly.x -I:" | newline | out))

-- Canonical: IP, and the origin is the only interior lattice point.  A reflexive
-- polytope is canonical, and poly.x -e says whether it is one, so the lattice
-- points are only needed when it is not.  Then -e prints one inequality
-- <a,x> + c >= 0 per line, as a_1 ... a_n c, in the coordinates -p uses, and a
-- point is interior when all of these are strictly positive.
palpIsCanonical = method()
palpIsCanonical Matrix :=
palpIsCanonical List :=
palpIsCanonical WeightSystem := Boolean => A -> (
    if not palpIsIP A then return false;
    b := palpReadBlock runPALP(palpProgram "poly", "-e", palpInput A);
    if b === null then error "expected output from poly.x -e";
    (ne, n, rows) := b;
    if all(rows, r -> #r === n) then return true;     -- vertices of the dual: reflexive
    if not all(rows, r -> #r === n + 1) then error(
        "unexpected output from poly.x -e: rows of length " | toString unique apply(rows, length));
    E := matrix rows;
    pts := palpMPoints A;
    if numRows pts =!= n then error "poly.x -p and -e disagree about the dimension";
    vals := E_{0 ..< n} * pts + E_{n} * matrix {toList(numColumns pts : 1)};
    #select(numColumns pts, j -> all(flatten entries vals_{j}, x -> x > 0)) === 1)

-------------------------------------------------
-- Weight systems --------------------------------
-------------------------------------------------

-- A weight system is written {d, q_0, ..., q_n} with d the degree.  The ones of
-- interest here are those with d = sum of the q_i, which are exactly the ones
-- whose degree d hypersurface in the weighted projective space P(q) has trivial
-- canonical class.  cws.x enumerates them.
-- cws.x -w has two modes.  Given a range of degrees "L H" it runs a general
-- search whose cost grows steeply with the degree (in dimension 4, degrees 5..200
-- take 31 seconds, and degree 3402 alone more than half an hour).  Given no range,
-- in dimension at most 4 it runs the enumeration of Kreuzer and Skarke instead,
-- which lists all 184026 weight systems of dimension 4 in about three minutes;
-- in dimension 5 and higher it fails an assertion.  So an unbounded range of
-- degrees is answered by the second mode, keeping the degrees >= lo afterwards.
weightSystems = method(Options => {Degrees => {0, infinity}, Reflexive => false})
weightSystems ZZ := List => opts -> n -> (
    if n < 1 then error "expected a positive dimension";
    drange := opts.Degrees;
    if instance(drange, ZZ) then drange = {drange, drange};
    if not instance(drange, BasicList) or #drange =!= 2
    or not instance(drange#0, ZZ)
    or not (instance(drange#1, ZZ) or drange#1 === infinity)
    then error "expected Degrees => {lo, hi}, Degrees => {lo, infinity}, or Degrees => deg";
    (lo, hi) := toSequence drange;
    if hi =!= infinity and lo > hi then error(
        "expected Degrees => {lo, hi} with lo <= hi, got " | toString drange);
    if hi === infinity and n > 4 then error(
        "PALP lists the weight systems of dimension " | toString n | " only for a "
        | "finite range of degrees.  Give a range, for instance" | newline
        | "    weightSystems(" | toString n | ", Degrees => {0, 20})");
    args := "-w" | toString n;
    if hi =!= infinity then args = args | " " | toString lo | " " | toString hi;
    -- cws.x -w takes no input file, so we call it directly rather than via runPALP
    result := runProgram(palpProgram "cws", args, RaiseError => false);
    if result#"return value" =!= 0 then error(
        "cws.x failed: " | result#"command" | newline | result#"error");
    if match("POLY_Dmax", result#"output") then error(
        "cws.x was built with too small a POLY_Dmax for dimension " | toString n |
        ":" | newline | result#"output");
    -- Each line is "<degree> <q_0> ... <q_n>", n+2 integers in dimension n,
    -- followed by flags such as "rt"; in the second mode the last line also
    -- carries a summary "#=184026 #cand=...".
    -- With a range, cws.x adds a summary line of its own, beginning with #.
    -- Keep the leading run of integers of each line, and read it with one value.
    -- The flags contain "r" exactly when the polytope is reflexive (cws.x prints
    -- this in every mode; in dimension at most 4 every line has it).
    L := for ell in lines result#"output" list (
        if match("^ *(#|$)", ell) then continue;
        m := regex("^ *([0-9]+( +[0-9]+)*)", ell);
        if m === null then error("unexpected cws.x output line: " | ell);
        if opts.Reflexive then (
            -- the first token after the integers is the flags, such as "r" or "rt"
            rest := select(separate(" +", substring(ell, m#1#0 + m#1#1)), x -> x =!= "");
            if #rest === 0 or not match("^[a-z]*r[a-z]*$", first rest) then continue);
        ws := value("{" | replace(" +", ",", substring(m#1, ell)) | "}");
        if #ws =!= n + 2 then error("unexpected cws.x output line: " | ell);
        ws);
    if hi === infinity and lo > 0 then L = select(L, ws -> ws#0 >= lo);
    -- cws.x output is already a valid weight system, so skip weightSystem's check
    apply(L, ws -> new WeightSystem from {ws})
    )

-- The vertices of Delta(q).  This is palpMVertices on a weight system; it is
-- named separately because that is how one looks for it, and because a combined
-- weight system is also accepted here.
weightSystemVertices = method()
weightSystemVertices List :=
weightSystemVertices WeightSystem := Matrix => q -> palpMVertices q

-- Delta(q) = {x : q.x = 0, x_i >= -1}, computed from the definition rather than
-- by calling PALP, and returned in a basis of the lattice ker(q) \cap ZZ^n so
-- that it is full dimensional.
--
-- Note that Delta(q) itself is only a rational polytope: the vertex obtained by
-- setting every coordinate but the j-th to -1 is (d - q_j)/q_j, which is an
-- integer only when q_j divides d.  The lattice polytope that PALP reports, and
-- that this function returns, is the convex hull of its lattice points.
weightSystemPolytope = method()
weightSystemPolytope List := Polyhedron => wts -> (
    if #wts < 3 then error "expected a weight system {d, q_0, ..., q_n}";
    q := drop(wts, 1);
    if not all(q, a -> instance(a, ZZ) and a > 0)
    then error "expected the weights to be positive integers";
    if wts#0 =!= sum q then error(
        "expected the degree " | toString wts#0 | " to be the sum of the weights " |
        toString sum q | newline |
        "(a combined weight system is not handled here; use weightSystemVertices)");
    n := #q;
    B := transpose LLL syz matrix {q};    -- rows: a ZZ-basis of ker(q) \cap ZZ^n
    Delta := polyhedronFromHData(- transpose B, matrix toList(n : {1}));
    convexHull matrix {latticePoints Delta}
    )
weightSystemPolytope WeightSystem := Polyhedron => cws -> (
    if #cws =!= 1 then error(
        "expected a single weight system; for a combined weight system use weightSystemVertices");
    weightSystemPolytope cws#0
    )

-------------------------------------------------
-- Nef partitions --------------------------------
-------------------------------------------------

-- nef.x reports one nef partition per line, for example
--     H:2 86 [-168] P:0 V:1 5  6   (2 8) (1 4)     0sec  0cpu
--     H:20 [24] P:0 V0:1 5  6  V1:3 4   (2 4 4) (1 2 2)    1sec  0cpu
-- The V fields list the parts of the partition, as point indices.  In
-- codimension c there are c-1 of them; the points not mentioned form the
-- remaining part.  Returns the list of V fields, each a list of indices.
palpParsePartitionLine = ell -> (
    groups := {};
    collecting := false;
    for t in select(separate(" +", ell), x -> x =!= "") do (
        if match("^V[0-9]*:", t) then (
            rest := substring(t, 1 + first first regex(":", t));
            groups = append(groups, if rest === "" then {} else {value rest});
            collecting = true;
            )
        else if collecting and match("^[0-9]+$", t)
        then groups = append(drop(groups, -1), append(last groups, value t))
        else collecting = false;
        );
    groups
    )

-- the block of vertices of Delta^* that nef.x -V prints, one vertex per column,
-- numbered as in the V fields of its partition lines
palpNefVertices = out -> (
    L := lines out;
    i := position(L, ell -> match("Vertices of P:", ell));
    if i === null then null
    else palpPointMatrix concatenate between(newline, drop(L, i))
    )

-- The Hodge numbers in a line of nef.x output, given as its list of tokens.
-- Returns (h1, chi, h0), or null if there is no H: field, where
--   h1   {h^{1,1}, ..., h^{1,d-1}}, d being the dimension of the complete
--        intersection, so d = #h1 + 1
--   chi  the Euler characteristic, printed in brackets
--   h0   {h^{0,0}, ..., h^{0,d}}.  After the brackets PALP prints "h<i>=<v>" for
--        each 1 <= i <= d/2 with h^{0,i} nonzero, and "h0=<v>" when h^{0,0} is not
--        1, which happens only for direct products.  The rest are 1 and 0, and
--        h^{0,i} = h^{0,d-i}, since the canonical class is trivial (Serre duality).
-- For example "H:2 86 [-168]" gives ({2, 86}, -168, {1, 0, 0, 1}),
-- "H:4 [0] h1=2" gives ({4}, 0, {1, 2, 1}), and for a curve, where PALP prints
-- just "H:[0]", the result is ({}, 0, {1, 1}).
palpParseHodge = toks -> (
    i := position(toks, t -> match("^H:", t));
    if i === null then return null;
    hs := drop(toks, i + 1);
    if toks#i =!= "H:" then hs = prepend(substring(toks#i, 2), hs);
    j := position(hs, t -> match("^\\[-?[0-9]+\\]$", t));
    if j === null then error("unexpected Hodge numbers in nef.x output: " | demark(" ", toks));
    h1 := apply(take(hs, j), value);
    d := #h1 + 1;
    h0 := new MutableList from prepend(1, toList(d : 0));
    for t in drop(hs, j + 1) do (
        m := regex("^h([0-9]+)=(-?[0-9]+)$", t);
        if m =!= null then h0#(value substring(m#1, t)) = value substring(m#2, t));
    for k from 0 to d do if 2 * k > d then h0#k = h0#(d - k);
    (h1, value substring(hs#j, 1, #hs#j - 2), toList h0)
    )

-- One line of nef.x output describing a nef partition, for example
--     H:2 86 [-168] P:0 V:1 5  6   (2 8) (1 4)     0sec  0cpu
--     H:4 [0] h1=2 P:0 V:2 3 5   D     0sec  0cpu
--      P:0 V0:1 5  6  V1:3 4   (2 4 4) (1 2 2)     1sec  0cpu      (-p, codim 3)
-- PALP numbers the vertices of Delta^* first, 0 .. nv-1, then the other points,
-- with the origin last.  Each V field lists one part: its vertices, then (after
-- a double space) its other points, which are dropped here.  PALP leaves out the
-- part with the most vertices; it is put back here, as the last part.
--   H:  the Hodge numbers, read by palpParseHodge
--   D   PALP found a direct product;  DP  the dual nef partition is a projection
-- PALP does not label projections; one is a partition with a part consisting of
-- a single vertex (Remove_Proj in E_Poly.c).
-- nv is the number of vertices, c the codimension, V the matrix of vertices,
-- and relabel#i the index in V of PALP's vertex number i.
palpNefPartitionFromLine = (ell, nv, c, V, relabel) -> (
    toks := select(separate(" +", ell), x -> x =!= "");
    parts := apply(palpParsePartitionLine ell, p -> select(p, i -> i < nv));
    if #parts =!= c - 1 then error("unexpected nef.x output line: " | ell);
    parts = append(parts, toList(0 ..< nv) - set flatten parts);
    parts = apply(parts, p -> sort apply(p, i -> relabel#i));
    props := {};
    if any(parts, p -> #p === 1) then props = append(props, Projection);
    if member("D", toks) then props = append(props, DirectProduct);
    if member("DP", toks) then props = append(props, DualProjection);
    n := new NefPartition from {
        Codim => c,
        Parts => parts,
        Vertices => V,
        Properties => props,
        symbol cache => new CacheTable
        };
    h := palpParseHodge toks;
    if h =!= null then (
        n.cache.HodgeNumbers = h#0;
        n.cache.Euler = h#1;
        n.cache.HodgeZero = h#2;
        );
    n
    )

-- The input describes the reflexive polytope Delta^* on the N side, whose
-- vertices are the rays of the fan:
--   a Matrix             whose columns are the vertices of Delta^*, or some or
--                        all of its nonzero lattice points, including the vertices
--   a NormalToricVariety its rays are used, as such a matrix
--   a weight system      (a List or WeightSystem) this describes Delta, on the M
--                        side, as everywhere else in the package
-- Returns a list of NefPartitions, each a hash table with
--   Codim            c
--   Parts            a list of c lists of indices of columns of Vertices, one
--                    list per part, partitioning all the vertices
--   Vertices         the vertices of Delta^* in the N lattice, one per column.
--                    For a matrix (or variety) they are in the order they appear
--                    among its columns (rays), so if those are exactly the
--                    vertices, the indices are its column indices; for a weight
--                    system, in the order PALP uses.
--   Properties       a sublist of {Projection, DirectProduct, DualProjection}
--   cache            a CacheTable, holding HodgeNumbers {h^{1,1}, ..., h^{1,dim-1}},
--                    Euler, the Euler characteristic, and HodgeZero
--                    {h^{0,0}, ..., h^{0,dim}}, unless HodgeNumbers => false;
--                    hodgeNumbers, euler and hodgeZero compute them when they
--                    are not there
-- or null if nef.x prints nothing, which it does for a polytope that is not
-- reflexive.
palpNefPartitions = method(Options => {
        HodgeNumbers => true,     -- false: nef.x -p, which skips the Hodge numbers
        Products => false,        -- nef.x -D: include direct products
        Projections => false,     -- nef.x -P: include projections
        KeepSymmetric => false    -- nef.x -s: keep partitions equivalent under symmetries
        })
palpNefPartitions(NormalToricVariety, ZZ) := List => opts -> (X, c) ->
    palpNefPartitionsWith(transpose matrix rays X, c, opts)
palpNefPartitions(Matrix, ZZ) :=
palpNefPartitions(List, ZZ) :=
palpNefPartitions(WeightSystem, ZZ) := List => opts -> (A, c) -> palpNefPartitionsWith(A, c, opts)

-- The body of palpNefPartitions; opts is its option table.  A matrix is given
-- to nef.x with -N, so that it is read as Delta^*.  We ask for -V, the vertices
-- of Delta^*, rather than -Lp, all its lattice points: the vertices are all we
-- use, and -Lp also computes the relations among the points (the "L vector"),
-- which fails on some inputs -- an assertion in TriMat_to_Weight on the weight
-- system {36,7,8,9,12}, and in Make_L when Delta^* has more than VERT_Nmax points.
palpNefPartitionsWith = (A, c, opts) -> (
    if c < 1 then error "expected a positive codimension";
    nside := instance(A, Matrix);
    if nside and ring A === QQ then A = try lift(A, ZZ) else A;
    args := (if nside then "-N " else "") | "-V -c" | toString c
        | (if opts.HodgeNumbers then "" else " -p")
        | (if opts.Products then " -D" else "")
        | (if opts.Projections then " -P" else "")
        | (if opts.KeepSymmetric then " -s" else "");
    out := runPALP(palpProgram "nef", args, palpInput A);
    if isBlankString out then return null;
    L := lines out;
    -- the first line is a summary such as "... M:378 6 N:8 6  codim=2 #part=4",
    -- where N gives (#points, #vertices) of Delta^*
    info := palpParseInfo first L;
    if not info#?"N" or #info#"N" =!= 2 then error("unexpected nef.x output: " | first L);
    nv := info#"N"#1;
    V := palpNefVertices out;
    if V === null or numColumns V =!= nv then error("unexpected nef.x output:" | newline | out);
    relabel := toList(0 ..< nv);
    if nside then (
        -- renumber PALP's vertices in the order they first appear among the columns of A
        pv := entries transpose V;
        cols := unique select(entries transpose A, a -> member(a, pv));
        if #cols =!= nv then error "nef.x reported vertices that are not columns of the input";
        relabel = apply(pv, v -> position(cols, a -> a == v));
        V = transpose matrix cols;
        );
    result := for ell in L list (
        if not match("(^| )P:[0-9]+", ell) then continue;
        palpNefPartitionFromLine(ell, nv, c, V, relabel)
        );
    -- With -s, nef.x also prints each nef partition once for every order of the
    -- c-1 parts it names (twice in codimension 3), so keep only the first.
    if opts.KeepSymmetric then (
        seen := new MutableHashTable;
        result = select(result, n -> (
                k := set apply(n.Parts, set);
                if seen#?k then false else seen#k = true)));
    result
    )

-- The kinds of nef partition that nef.x reports, computed directly, as a check
-- on what PALP says.  All three take the same input:
--   V      an integer matrix whose columns are the vertices of the reflexive
--          polytope Delta^* in the N lattice (the N side: the rays of the fan of
--          the toric variety), one vertex per column.  Other nonzero lattice
--          points of the boundary of Delta^* may be included as extra columns,
--          as long as each is put in the part it belongs to.  Leave out the
--          origin: it is not a ray and belongs to no part (if it is included
--          anyway, in any part, none of the three answers changes).
--   parts  a nef partition E_1, ..., E_c of those columns: a list of c disjoint
--          lists of column indices, together containing every column.
-- Write E_S for the union of the parts E_j with j in S.

-- An error unless parts is a list of nonempty, disjoint lists of column indices
-- of V that together contain every column.
checkNefParts = (parts, V) -> (
    if not instance(parts, List) or #parts === 0
    or not all(parts, p -> instance(p, List) and #p > 0 and all(p, i -> instance(i, ZZ)))
    then error "expected the parts to be a nonempty list of nonempty lists of column indices";
    all0 := flatten parts;
    if #unique all0 =!= #all0 then error "expected the parts to be disjoint";
    if sort all0 =!= toList(0 ..< numColumns V) then error(
        "expected the parts to contain each column index 0 .. "
        | toString(numColumns V - 1) | " exactly once");
    )

-- isDirectProduct(parts, V) is true when the parts split into two nonempty
-- groups S and T (each part in exactly one) with
--     dim span(E_S) + dim span(E_T) = dim N,
-- the spans being the linear spans in N_R.  Since all of the columns together
-- span N_R, this says N_R = span(E_S) (+) span(E_T), so Delta^* is the free sum
-- of conv(0, E_S) and conv(0, E_T), the toric variety is a product, and the
-- complete intersection is a product of one in each factor.  The code tries each
-- S containing part 0 (so that S and T are not both tried).
isDirectProduct = method()
isDirectProduct(List, Matrix) := Boolean => (parts, V) -> (
    checkNefParts(parts, V);
    n := numRows V; c := #parts;
    others := toList(1 ..< c);
    any(drop(subsets others, -1), S -> (
        S = prepend(0, S);
        rank V_(flatten parts_S) + rank V_(flatten parts_(others - set S)) == n)))

-- isProjection(parts, V) is true when for some j the polytope
--     nabla_j = conv({0} \cup E_j)    (in N_R)
-- has exactly 2 lattice points, 0 and one other.  Since every vertex of Delta^*
-- is a primitive lattice vector, this happens exactly when the part E_j is a
-- single vertex v, so that the j-th divisor of the nef partition is the single
-- toric divisor D_v.
isProjection = method()
isProjection(List, Matrix) := Boolean => (parts, V) -> (
    checkNefParts(parts, V);
    any(parts, p -> #latticePoints convexHull(V_p | map(ZZ^(numRows V), ZZ^1, 0)) == 2))

-- isDualProjection(parts, V) is true when for some j the polytope
--     Delta_j = {m in M_R : <m,v> >= -1 for each column v in E_j,
--                           <m,v> >=  0 for each column v not in E_j}
-- has exactly 2 lattice points, 0 and one other.  Delta_j, on the M side, is the
-- polytope of the j-th divisor of the nef partition, sum_{v in E_j} D_v, and
-- Delta = Delta_1 + ... + Delta_c.  The Delta_j are the parts of the dual nef
-- partition, so this says that the dual nef partition is a projection.  Every
-- column of V is used as a constraint, so V must contain all the vertices.
isDualProjection = method()
isDualProjection(List, Matrix) := Boolean => (parts, V) -> (
    checkNefParts(parts, V);
    any(parts, p -> (
        a := transpose matrix {apply(numColumns V, i -> if member(i, p) then 1 else 0)};
        #latticePoints polyhedronFromHData(- transpose V, a) == 2)))

isDirectProduct NefPartition := Boolean => n -> isDirectProduct(n.Parts, n.Vertices)
isProjection NefPartition := Boolean => n -> isProjection(n.Parts, n.Vertices)
isDualProjection NefPartition := Boolean => n -> isDualProjection(n.Parts, n.Vertices)

-- The polytopes Delta_i = {m : <m,v> >= -1 for v in E_i, <m,v> >= 0 for the
-- other columns v} on the M side, one for each part, as matrices of their
-- vertices.  The parts form a nef partition exactly when each Delta_i is a
-- lattice polytope and Delta_1 + ... + Delta_c = Delta, the polar of conv(V)
-- (Batyrev-Borisov).  The first condition alone is not enough: it holds for
-- every partition on a smooth toric variety, where every divisor is Cartier.
-- Anything else is an error here.
nefPartitionDeltas = (parts, V) -> (
    Ds := for i from 0 to #parts - 1 list
        polyhedronFromHData(- transpose V,
            transpose matrix {apply(numColumns V, k -> if member(k, parts#i) then 1 else 0)});
    verts := for i from 0 to #Ds - 1 list (
        try lift(vertices Ds#i, ZZ) else error(
            "expected a nef partition, but Delta_" | toString i | " is not a lattice polytope"));
    if fold(minkowskiSum, Ds) != polar convexHull V then error(
        "expected a nef partition, but Delta_0 + ... + Delta_" | toString(#Ds - 1)
        | " is not the dual polytope Delta (some part is not nef)");
    verts
    )

-- The vertices of the Cayley polytope of a nef partition, one per column, in
-- Z^(c-1) x M (Lattice => "M") or Z^(c-1) x N (Lattice => "N").  The first c-1
-- coordinates are 0 for part 0 and e_i for part i, as in PALP's Make_Gore_Poly
-- (E_Poly.c); the rest run over the vertices of
--   Delta_i (see nefPartitionDeltas)    (M side)
--   nabla_i = conv(0, E_i)              (N side)
-- These are the Gorenstein polytopes of the nef partition and of its dual.  On
-- the M side this is an error if the parts do not form a nef partition.
cayleyPolytopeVertices = method(Options => {Lattice => "M"})
cayleyPolytopeVertices(List, Matrix) := Matrix => opts -> (parts, V) -> (
    if not member(opts.Lattice, {"M", "N"}) then error "expected Lattice => \"M\" or \"N\"";
    checkNefParts(parts, V);
    c := #parts;
    n := numRows V;
    pieces := if opts.Lattice === "N"
        then for p in parts list lift(vertices convexHull(map(ZZ^n, ZZ^1, 0) | V_p), ZZ)
        else nefPartitionDeltas(parts, V);
    transpose matrix flatten for i from 0 to c - 1 list
        for m in entries transpose pieces#i list apply(c - 1, d -> if d + 1 == i then 1 else 0) | m
    )
cayleyPolytopeVertices NefPartition := Matrix => opts -> n ->
    cayleyPolytopeVertices(n.Parts, n.Vertices, Lattice => opts.Lattice)

-- The Hodge numbers of one nef partition, from nef.x -G, which reads its input
-- as the support polytope of a reflexive Gorenstein cone of index c and computes
-- its stringy Hodge numbers (AnalyseGorensteinCone in E_Poly.c).  We give it the
-- N side Cayley polytope with -N.  The M side one, without -N, gives the same
-- answer, but more slowly: on {12,1,1,2,2,2,2,2} in codimension 3, 42 seconds
-- rather than 26 for one partition, which is also what nef.x -c3 itself takes.
-- The nef condition is checked first (nefPartitionDeltas); otherwise only PALP's
-- check of the index of the cone would catch a partition that is not nef.
-- Returns a hash table with HodgeNumbers {h^{1,1}, ..., h^{1,d-1}}, Euler, and
-- HodgeZero {h^{0,0}, ..., h^{0,d}} (see palpParseHodge).
-- Given a NefPartition, it also stores them in its cache.
palpHodgeNumbers = method()
palpHodgeNumbers(List, Matrix) := HashTable => (parts, V) -> (
    checkNefParts(parts, V);
    nefPartitionDeltas(parts, V);
    out := runPALP(palpProgram "nef", "-G -N -c" | toString(#parts),
        palpInput cayleyPolytopeVertices(parts, V, Lattice => "N"));
    L := select(lines out, ell -> match("H:", ell));
    if #L =!= 1 then error("unexpected nef.x -G output:" | newline | out);
    h := palpParseHodge select(separate(" +", first L), x -> x =!= "");
    new HashTable from {HodgeNumbers => h#0, Euler => h#1, HodgeZero => h#2}
    )
palpHodgeNumbers NefPartition := HashTable => n -> (
    h := palpHodgeNumbers(n.Parts, n.Vertices);
    n.cache.HodgeNumbers = h.HodgeNumbers;
    n.cache.Euler = h.Euler;
    n.cache.HodgeZero = h.HodgeZero;
    h)

-- Access to the parts of a NefPartition.  The Hodge numbers and Euler
-- characteristic are computed (by palpHodgeNumbers) if they are not already in
-- the cache.
parts NefPartition := List => n -> n.Parts
vertices NefPartition := Matrix => n -> n.Vertices
-- codim is method(Options => true) in Core, so its methods take options this way
codim NefPartition := {} >> opts -> n -> n.Codim
properties = method()
properties NefPartition := List => n -> n.Properties
hodgeNumbers = method()
hodgeNumbers NefPartition := List => n -> (
    if not n.cache#?HodgeNumbers then palpHodgeNumbers n;
    n.cache.HodgeNumbers)
euler NefPartition := ZZ => n -> (
    if not n.cache#?Euler then palpHodgeNumbers n;
    n.cache.Euler)
hodgeZero = method()
hodgeZero NefPartition := List => n -> (
    if not n.cache#?HodgeZero then palpHodgeNumbers n;
    n.cache.HodgeZero)

-------------------------------------------------
-- Nef partitions without PALP --------------------
-------------------------------------------------

-- A nef partition of codimension c is a partition of the rays of V into c parts,
-- each of whose sum of divisors is nef.  This searches for them directly, which
-- is a useful check on what PALP reports, but it looks at every set partition of
-- the rays into c parts and so is only usable for small numbers of rays.
naiveNefPartitions = method()
naiveNefPartitions(NormalToricVariety, ZZ) := List => (V, cod) -> (
    if cod < 1 then error "expected a positive number of parts";
    n := #rays V;
    if cod > n then return {};
    -- Batyrev-Borisov need each divisor Cartier as well as nef, so that its
    -- polytope Delta_j is a lattice polytope; isNef alone only checks nefness
    -- over QQ, which on a singular V admits partitions that are not nef partitions.
    isNefPart := memoize(b -> (D := sum(b, i -> V_i); isCartier D and isNef D));
    result := new MutableList;
    -- restricted growth strings visit each set partition of {0, ..., n-1} once
    recurse := null;
    recurse = (i, blocks) -> (
        if #blocks + (n - i) < cod then return;     -- too few elements left
        if i === n then (
            if #blocks === cod and all(blocks, isNefPart)
            then result#(#result) = blocks;
            return;
            );
        for b from 0 to min(#blocks, cod - 1) do recurse(i + 1,
            if b < #blocks then replace(b, append(blocks#b, i), blocks)
            else append(blocks, {i}));
        );
    recurse(0, {});
    toList result
    )

-- The divisors of a nef partition.  The parts may be given as a list of lists of
-- ray indices, or, for codimension two, as the single list of indices making up
-- one part.  Any rays left over form one more part.  The parts of a nef
-- partition found by palpNefPartitions(X, c) are ray indices when the rays of X
-- are exactly the vertices of Delta^*, and can then be given here directly.
nefPartitionDivisors = method()
nefPartitionDivisors(NormalToricVariety, List) := List => (V, part) -> (
    if #part === 0 then error "expected a nonempty partition";
    n := #rays V;
    parts := if all(part, x -> instance(x, ZZ)) then {toList part}
        else for p in part list (
            if not instance(p, VisibleList)
            then error "expected a list of ray indices, or a list of lists of them";
            toList p);
    if any(parts, p -> #p === 0) then error "expected the parts to be nonempty";
    all0n := flatten parts;
    if not all(all0n, i -> instance(i, ZZ) and 0 <= i and i < n)
    then error("expected ray indices in the range 0 .. " | toString(n - 1));
    if #unique all0n =!= #all0n then error "expected the parts to be disjoint";
    rest := sort toList(set toList(0 .. n - 1) - set all0n);
    if #rest > 0 then parts = append(parts, rest);
    for p in parts list sum(p, i -> V_i)
    )

beginDocumentation()

doc ///
Key
  PALPInterface
Headline
  an interface to the PALP package for lattice polytopes
Description
  Text
    PALP, a Package for Analyzing Lattice Polytopes, was written by Maximilian
    Kreuzer and Harald Skarke in order to classify reflexive polytopes, and in
    particular to produce the list of the 473800776 reflexive polytopes of
    dimension four.  Along the way it acquired a good deal of functionality that
    is useful on its own: normal forms of lattice polytopes, the lattice points
    of a polytope and of its dual, the enumeration of weight systems, and nef
    partitions.  This package makes some of that functionality available inside
    Macaulay2.  If you wish more functionality added, please contact the author of
    this package.
  Text
    The program, its source and its documentation are at
    @HREF "https://hep.itp.tuwien.ac.at/~kreuzer/CY/CYpalp.html"@; the reference
    documentation is reached from there, under "Former PALP wiki" and "Last
    version of the PALP online documentation".  PALP is described in
  Pre
    M. Kreuzer and H. Skarke, PALP: A Package for Analyzing Lattice Polytopes
    with Applications to Toric Geometry, Comput. Phys. Commun. 157 (2004)
    87-106, arXiv:math/0204356.
  -- Text
  --   PALP is a separate program, and must be installed before any of these
  --   functions will run.  On a machine already set up for Macaulay2, use
  -- Pre
  --   brew install macaulay2/tap/palp
  -- Text
  --   The package itself loads whether or not PALP is present; the executables are
  --   looked for only when a function needs one.  If they are installed somewhere
  --   that is not on your path, add that directory to @TO "programPaths"@.
  Text
    @SUBSECTION "Polytope functions"@
  Text
        @UL {
        TO palpMVertices,
        TO palpInfo,
        TO palpIsReflexive,
        TO palpIsIP,
        TO palpIsCanonical
        }@
  Text
    @SUBSECTION "Weight Systems"@
  Text
    Polytopes in PALP are given either by a matrix whose columns are its vertices, or by a weight system,
    (or combined weight system).  Reflexive polytopes of dimension at most 4 can be obtained
    using @TO "ReflexivePolytopesDB::ReflexivePolytopesDB"@.  There are too many reflexive polytopes of dimension at least five
    to enumerate.  Instead, weight systems are a good way to generate reflexive polytopes
    (and they can be used to in principle generate all reflexive polytopes in higher dimensions).
  Text
    @UL {
        TO WeightSystem,
        TO (weightSystems, ZZ),
        TO (weightSystem, List),
        TO (isWellDefined, WeightSystem),
        TO palpMVertices
        }@
  Text
    @SUBSECTION "Nef partitions"@
  Text
    @UL {
        TO NefPartition,
        TO palpNefPartitions,
        TO isProjection,
        TO isDirectProduct,
        TO cayleyPolytopeVertices,
        TO naiveNefPartitions,
        TO nefPartitionDivisors,
        TO palpHodgeNumbers
        }@
  Text
    @SUBSECTION "Normal forms of polytopes"@
  Text
    @UL {
        TO palpNormalForm
        }@
  Text
    A first computation: the polytope of a weight system, its vertices, and its
    normal form.
  Example
    ws = {10, 1, 2, 3, 4}
    V = weightSystemVertices ws
    palpNormalForm V
    P = convexHull V
    isReflexive P
  Text
    The columns of the matrix are the vertices.  PALP itself prints a point set
    sometimes as rows and sometimes as columns, depending on the example, but
    every function here returns one point per column.
  Text
    PALP is compiled with a fixed bound on the dimensions it can handle, and
    ships as several executables with different bounds.  See @TO "palpDmax"@,
    which matters mainly for @TO palpNefPartitions@.
  Text
    PALP assumes its input is a full dimensional polytope and does not check, so
    the functions here check before calling it.  A set of points spanning
    something of lower dimension makes PALP give a wrong answer under some
    options and crash under others, and a weight system of dimension one makes
    it abort; rather than pass any of that on, these are refused with an error.
    The file @TT "PALP-upstream-notes.md"@ in this package's repository has the
    details.
  Example
    M = matrix{{1,-1,-1,1},{1,1,-1,-1},{0,0,0,0}};   -- a square in a plane in ZZ^3
    assert(try (palpMVertices M; false) else true)
SeeAlso
  palpNormalForm
  weightSystems
  palpNefPartitions
  NefPartition
  "palpDmax"
  "ReflexivePolytopesDB::ReflexivePolytopesDB"
///

doc ///
Key
  "palpDmax"
Headline
  which build of the PALP executables to use
Description
  Text
    PALP is compiled with a fixed upper bound @TT "POLY_Dmax"@ on the dimensions
    it can handle, and its @TT "make all-dims"@ target builds one executable per
    bound: @TT "poly-4d.x"@, @TT "poly-5d.x"@, @TT "poly-6d.x"@,
    @TT "poly-11d.x"@, and @TT "poly.x"@, which is the same build as the 6d one.
    This variable says which of them to prefer, and may be set to 4, 5, 6 or 11.
  Example
    palpDmax
  Text
    The requirement PALP imposes is
  Pre
    POLY_Dmax >= dim N + codim - 1
  Text
    so the default build is enough for @TO palpMVertices@ and friends up to
    dimension six, while nef partitions run out much sooner: a five dimensional
    polytope in codimension three, or a six dimensional one in codimension two,
    already needs the 11d build.  That is the main reason to have it.
  Example
    ws = {10, 1, 1, 2, 2, 2, 2};
    #palpNefPartitions(ws, 3, HodgeNumbers => false)
  Text
    Asking for a build that is not installed falls back on the default one.  If
    the result is a PALP that cannot handle the input, that is reported as an
    error rather than being silently mistaken for an empty answer, since PALP
    says so on its standard output and still exits successfully.
Caveat
  Changing this value selects a different executable, so any program already
  located is looked up again.
SeeAlso
  palpProgram
  palpNefPartitions
  "palpVerbosity"
///

doc ///
Key
  "palpVerbosity"
Headline
  how much to report about the PALP commands being run
Description
  Text
    At 0, the default, nothing is printed.  At 1, each PALP command is printed
    together with the temporary file holding its input.  At 2, PALP's output is
    printed as well.
  Example
    palpVerbosity = 1;
    palpNormalForm {3, 1, 1, 1}
    palpVerbosity = 0;
SeeAlso
  palpRun
  "palpDmax"
///

doc ///
Key
  palpProgram
  (palpProgram, String)
Headline
  locate one of the PALP executables
Usage
  P = palpProgram name
Inputs
  name:String
    the base name of a PALP program, such as "poly", "cws" or "nef", with no
    dimension suffix and no ".x"
Outputs
  P:Program
    as returned by @TO findProgram@
Description
  Text
    The build selected by @TO "palpDmax"@ is preferred, falling back on the
    default one if it is not installed.  The result is remembered, so the search
    happens once per program and per value of @TO "palpDmax"@.
  Example
    P = palpProgram "poly"
    P#"name"
  Text
    An error is raised, naming where to get PALP, if the program cannot be found
    at all.  Nothing is looked up when this package is loaded, so the package is
    usable without PALP right up to the point where a computation needs it.
SeeAlso
  palpRun
  "palpDmax"
  "programPaths"
///

doc ///
Key
  palpRun
  (palpRun, String, String, Matrix)
  (palpRun, String, String, List)
  (palpRun, String, String, WeightSystem)
  [palpRun, KeepFiles]
Headline
  run a PALP program and return its output
Usage
  s = palpRun(name, args, A)
Inputs
  name:String
    the base name of a PALP program, as for @TO palpProgram@
  args:String
    the command line options, for instance "-v" or "-Lp -c2"
  A:Matrix
    over the integers, whose columns span the polytope, or a (combined) weight
    system, as a @TO List@ or a @TO WeightSystem@
  KeepFiles => Boolean
    whether to keep the temporary file holding PALP's input, and print its name
Outputs
  s:String
    everything PALP wrote to its standard output
Description
  Text
    This is the general way in to PALP, for the options that have no wrapper of
    their own.
  Example
    M = transpose matrix {{1,0,0},{-1,0,1},{0,1,0},{0,-1,1},{0,0,1},{0,0,-1}};
    print palpRun("poly", "-g", M)
    print palpRun("poly", "-v", {3, 1, 1, 1})
  Text
    The input is written to a file under Macaulay2's temporary directory, which
    is removed when Macaulay2 exits, and nothing is written to the current
    directory.  Set @TO "palpVerbosity"@ to 1 to see the command and the name of
    that file.
  Text
    PALP reports some failures on its standard output while still exiting
    successfully; those are turned into errors here rather than passed along as
    if they were results.
SeeAlso
  palpProgram
  "palpVerbosity"
  fromPalpMatrix
///

doc ///
Key
  toPalp
  (toPalp, Matrix)
  (toPalp, List)
  (toPalp, WeightSystem)
Headline
  the PALP input format for a matrix or a weight system
Usage
  s = toPalp A
Inputs
  A:Matrix
    over the integers, or a @TO List@ of integers holding a (combined) weight
    system
Outputs
  s:String
    in the format PALP reads
Description
  Text
    A matrix becomes its number of rows and columns followed by its entries.
    PALP reads that as a set of points, taking the smaller of the two numbers to
    be the dimension.
  Example
    M = matrix{{1,1,1,1},{0,1,2,3}}
    print toPalp M
  Text
    A list of integers is passed through as a single line, which is how PALP
    recognises a weight system.  The first entry is the degree and the rest are
    the weights; several such blocks in a row make a combined weight system.
  Example
    print toPalp {10, 1, 2, 3, 4}
    print toPalp {3,1,1,1,0,0,0, 3,0,0,0,1,1,1}
SeeAlso
  fromPalpMatrix
  palpRun
///

doc ///
Key
  fromPalpMatrix
  (fromPalpMatrix, String)
Headline
  read a matrix printed by PALP
Usage
  M = fromPalpMatrix s
Inputs
  s:String
    output of a PALP program beginning with a header line
Outputs
  M:Matrix
    over the integers, exactly as PALP printed it
Description
  Text
    This is the inverse of @TO toPalp@ on matrices, and does not change the
    orientation of what it reads.
  Example
    M = matrix{{1,1,1,1},{0,1,2,3}}
    assert(M == fromPalpMatrix toPalp M)
  Example
    print palpRun("poly", "-v", {3, 1, 1, 1})
    fromPalpMatrix palpRun("poly", "-v", {3, 1, 1, 1})
  Text
    Since PALP does not always print a point set the same way round, the
    functions that return point sets, such as @TO palpMVertices@, reorient what
    they read so that each point is a column.  This function does not: it is the
    raw reader.
Caveat
  An error is raised if PALP printed nothing, or if the rows do not have the
  length that PALP's header announced.  The latter happens for the inequalities
  that @TT "poly.x -e"@ prints for a polytope that is not reflexive, which carry
  an extra inhomogeneous entry.
SeeAlso
  toPalp
  palpMVertices
///

doc ///
Key
  palpNormalForm
  (palpNormalForm, Matrix)
  (palpNormalForm, List)
  (palpNormalForm, WeightSystem)
Headline
  the PALP normal form of a lattice polytope
Usage
  N = palpNormalForm A
Inputs
  A:Matrix
    over the integers, whose columns span the polytope, or a (combined) weight
    system, as a @TO List@ or a @TO WeightSystem@
Outputs
  N:Matrix
    whose columns are the vertices in normal form
Description
  Text
    Two lattice polytopes differ by a change of basis of the lattice exactly when
    they have the same normal form, so this is what to compare in order to decide
    whether two descriptions give the same polytope.
  Example
    M = palpMVertices {10, 1, 2, 3, 4}
    palpNormalForm M
    A = matrix {{1, -2, 1}, {2, 1, -2}, {-1, 1, 0}}
    det A
    assert(palpNormalForm (A * M) == palpNormalForm M)
  Text
    A translation is not a change of basis, and a polytope and its translates
    usually have different normal forms.  Here are a reflexive triangle and the
    same triangle moved by $(1,0)$.  PALP also has an affine normal form,
    {\tt poly.x -A}, which is not wrapped here.
  Example
    P = matrix{{1,0,-1}, {0,1,-1}}
    Q = matrix{{2,1,0}, {0,1,-1}}
    palpNormalForm P
    palpNormalForm Q
  Text
    Two weight systems that give the same polytope:
  Example
    palpNormalForm {3402, 40, 41, 486, 1134, 1701}
    palpNormalForm {3486, 41, 42, 498, 1162, 1743}
    assert(oo == ooo)
Caveat
  The result is @TO null@ if PALP printed nothing, which happens for input it
  declines to treat as a polytope, such as a single point.
SeeAlso
  palpMVertices
  weightSystemPolytope
///

doc ///
Key
  palpMVertices
  (palpMVertices, Matrix)
  (palpMVertices, List)
  (palpMVertices, WeightSystem)
  palpMPoints
  (palpMPoints, Matrix)
  (palpMPoints, List)
  (palpMPoints, WeightSystem)
  palpNVertices
  (palpNVertices, Matrix)
  (palpNVertices, List)
  (palpNVertices, WeightSystem)
  palpNPoints
  (palpNPoints, Matrix)
  (palpNPoints, List)
  (palpNPoints, WeightSystem)
Headline
  vertices and lattice points of a polytope and of its dual
Usage
  palpMVertices A
  palpMPoints A
  palpNVertices A
  palpNPoints A
Inputs
  A:Matrix
    over the integers, whose columns span the polytope, or a (combined) weight
    system, as a @TO List@ or a @TO WeightSystem@
Outputs
  :Matrix
    over the integers, one point per column
Description
  Text
    For a polytope $P$ in the lattice $M$, with dual $P^*$ in $N$, these give
    the vertices of $P$, the lattice points of $P$, the vertices of $P^*$, and
    the lattice points of $P^*$ respectively.

    Given a matrix $A$ over the integers, $P$ is the convex hull of its columns;
    a matrix over @TO QQ@ with integer entries, such as the vertices of a
    polyhedron from @TO "Polyhedra::Polyhedra"@, is accepted as well.
    It must be full dimensional, so $A$ has more columns than rows.  Otherwise
    the columns are arbitrary: they need not be vertices, and they may include
    interior points and repeat.  @TO palpMVertices@ and @TO palpMPoints@ need
    no interior lattice point.  The dual
    $P^* = \{y : \langle x, y \rangle \ge -1 \text{ for all } x \in P\}$
    is taken with respect to the origin, and is a lattice polytope exactly when
    $P$ is reflexive; @TO palpNVertices@ and @TO palpNPoints@ give an answer
    only then.  A reflexive polytope has the origin as its only interior lattice
    point, so a polytope with other interior lattice points, or a translate of a
    reflexive polytope, gets @TO null@.  For the dual as a rational polytope, use
    @TO "Polyhedra::polar"@.
  Example
    ws = {10, 1, 2, 3, 4}
    palpMVertices ws
    palpNVertices ws
    numColumns palpMPoints ws
    numColumns palpNPoints ws
  Text
    These agree with @TO "Polyhedra::Polyhedra"@.
  Example
      P = convexHull palpMVertices ws;
    assert(numColumns palpMPoints ws == #latticePoints P)
    assert(set entries transpose palpNVertices ws
        === set entries transpose lift(vertices polar P, ZZ))
  Text
    Each point is a column, whichever way round PALP chose to print it.  PALP
    varies that from example to example rather than from option to option: for
    the weight system above it prints the lattice points as 23 rows of 3, and
    for the next one as 2 rows of 10.
  Example
    numRows palpMPoints {3, 1, 1, 1}
    numColumns palpMPoints {3, 1, 1, 1}
  Text
    When the polytope is not reflexive, its dual is not a lattice polytope, and
    the two functions about the dual return @TO null@.  The smallest example in
    dimension 3 is this simplex: its only interior lattice point is the origin,
    but it is not reflexive.
  Example
    M = matrix{{1,0,0,-1}, {0,1,0,-1}, {0,0,1,-2}}
    interiorLatticePoints convexHull M
    palpIsReflexive M
    palpNVertices M === null
    palpNPoints M === null
Caveat
  These functions run the PALP program {\tt poly.x}, one option each:
  {\tt -v} (the vertices of $P$) for @TO palpMVertices@, {\tt -p} (the lattice
  points of $P$) for @TO palpMPoints@, {\tt -e} (the equations of the facets of
  $P$, which for reflexive $P$ are the vertices of $P^*$) for
  @TO palpNVertices@, and {\tt -d} (the lattice points of $P^*$, only when $P$
  is reflexive) for @TO palpNPoints@.  The option {\tt -g} (general
  information) is the one @TO palpInfo@ uses.  The notes below refer to these.

  The result is @TO null@ when PALP has no answer to give.  This is the usual
  outcome of asking about the dual of a polytope that is not reflexive: there
  {\tt poly.x -d} prints nothing at all, and {\tt poly.x -e} prints
  inequalities rather than vertices.

  PALP requires the polytope to be full dimensional, and so requires more points
  than the dimension.  It assumes both and checks neither: given a polytope of
  lower dimension, {\tt poly.x -v} quietly returns too few vertices while
  {\tt poly.x -g} exits on a bus error, and given too few points it prints
  @TT "Identical points in Vec_Greater_Than !!"@.  These functions test the
  input first and raise an error rather than pass any of that on.

  The options that find lattice points, {\tt -p}, {\tt -d} and {\tt -g}, slow
  down quickly with the dimension, even when there are few points: on the
  simplex they take about twenty milliseconds in dimension six, three seconds in
  dimension eight, and more than a minute in dimension nine, while {\tt -v} and
  {\tt -e} stay fast.  PALP is chiefly used in low dimensions and is not much
  exercised above seven.
SeeAlso
  palpNormalForm
  palpInfo
  weightSystemVertices
///

doc ///
Key
  palpInfo
  (palpInfo, Matrix)
  (palpInfo, List)
  (palpInfo, WeightSystem)
Headline
  the summary line PALP prints for a polytope
Usage
  H = palpInfo A
Inputs
  A:Matrix
    over the integers, whose columns span the polytope, or a (combined) weight
    system, as a @TO List@ or a @TO WeightSystem@
Outputs
  H:HashTable
Description
  Text
    This is @TT "poly.x -g"@, whose one line of output collects the number of
    lattice points and vertices of the polytope and of its dual, and, when PALP
    knows them, the Hodge numbers and the Euler characteristic.
  Example
    palpInfo {10, 1, 1, 2, 2, 2, 2}
  Text
    Each field printed by PALP as @TT "Key:"@ becomes the list of numbers
    following it, and the value in square brackets, the Euler characteristic,
    becomes an integer under "Euler".  The fields are
  Text
    @UL {
      {TT "M", " -- the number of lattice points and the number of vertices of the polytope"},
      {TT "N", " -- the same two numbers for its dual, present only when the polytope is reflexive"},
      {TT "F", " -- the numbers of faces, printed in place of ", TT "N", " when the polytope is not reflexive"},
      {TT "H", " -- Hodge numbers"},
      {TT "Euler", " -- the Euler characteristic, which PALP prints in square brackets"},
      {TT "Pic", " -- for a three dimensional polytope, the Picard number of the generic K3 hypersurface"},
      {TT "Cor", " -- the correction term in the formula for ", TT "Pic", "; see below"}
      }@
  Text
    Which of them appear depends on the dimension and on whether the polytope is
    reflexive, because PALP prints whatever it computes for that case.  For the
    reflexive simplices in dimensions 1 through 8 one gets
  Pre
    dim 1, 2      M N
    dim 3         M N Pic Cor
    dim 4, 5      M N H Euler
    dim 6, 7, 8   M N H
  Text
    and a polytope that is not reflexive gives @TT "M"@ and @TT "F"@ in any
    dimension.  So do not assume a field is there; test with @TT "H#?\"H\""@
    before reading it.  PALP's own summary of what @TT "-g"@ prints is
  Pre
    P reflexive:     numbers of (dual) points/vertices, Hodge numbers
    P not reflexive: numbers of points, vertices, equations
  Text
    so in the second case @TT "F"@ is the number of equations, standing where
    @TT "N"@ would be.
  Text
    In dimension three the anticanonical hypersurface is a K3 surface, and PALP
    reports the Picard number of the generic member together with the term that
    corrects the naive count.  Writing $\ell$ for the number of lattice points
    of a face and $\ell^*$ for the number in its relative interior, and
    $\theta$ for the face of $\Delta$ dual to a face $\theta^*$ of
    $\Delta^*$, Batyrev's formula reads
  Pre
    Pic = l(Delta*) - 4 - sum over facets theta* of l*(theta*)
                        + sum over edges  theta* of l*(theta*) l*(theta)
  Text
    and @TT "Cor"@ is that second sum.  It is nonzero exactly when some edge of
    $\Delta^*$ and the edge of $\Delta$ dual to it both have interior lattice
    points, which is the part of the answer the point counts @TT "M"@ and
    @TT "N"@ cannot show.
  Example
    H = palpInfo transpose matrix {{1,0,0},{-1,0,1},{0,1,0},{0,-1,1},{0,0,1},{0,0,-1}}
    H#"M"
    H#"Pic"
  Text
    A polytope that is canonical but not reflexive, so that PALP reports
    @TT "F"@ and no @TT "N"@:
  Example
    palpInfo matrix{{1,0,0,-1},{0,1,0,-1},{0,0,1,-2}}
SeeAlso
  palpIsReflexive
  palpMVertices
  palpRun
///

doc ///
Key
  palpIsReflexive
  (palpIsReflexive, Matrix)
  (palpIsReflexive, List)
  (palpIsReflexive, WeightSystem)
Headline
  whether PALP considers a polytope reflexive
Usage
  palpIsReflexive A
Inputs
  A:Matrix
    over the integers, whose columns span the polytope, or a (combined) weight
    system, as a @TO List@ or a @TO WeightSystem@
Outputs
  :Boolean
Description
  Text
    This asks PALP for the vertices of the dual, with @TT "poly.x -e"@: a
    reflexive polytope has them, and a polytope that is not reflexive has PALP
    return inequalities instead, which @TO palpNVertices@ reports as
    @TO null@.
  Text
    The summary line of @TT "poly.x -g"@ answers the same question, carrying an
    @TT "N:"@ field for a reflexive polytope and an @TT "F:"@ field for one that
    is not, and @TO palpInfo@ will show you that.  It is the slower route,
    because it counts the lattice points of the dual: in dimension eight it
    takes some seconds against some milliseconds for @TT "-e"@.
  Example
    palpIsReflexive {10, 1, 2, 3, 4}
    palpIsReflexive matrix{{1,0,0,-1},{0,1,0,-1},{0,0,1,-1}}
  Text
    The second polytope below is canonical -- its only interior lattice point is
    the origin -- but it is not reflexive.  It is the fan polytope of the
    weighted projective space $P(1,1,1,2)$, and a simplex of this shape is
    reflexive exactly when every weight divides the sum of the weights.
  Example
    M = matrix{{1,0,0,-1},{0,1,0,-1},{0,0,1,-2}}
    palpIsReflexive M
    isReflexive convexHull M
    interiorLatticePoints convexHull M
Caveat
  This agrees with @TO "Polyhedra::isReflexive"@, but goes through PALP rather
  than computing the dual in Macaulay2.
SeeAlso
  palpIsIP
  palpIsCanonical
  palpInfo
  palpNPoints
///

doc ///
Key
  palpIsIP
  (palpIsIP, Matrix)
  (palpIsIP, List)
  (palpIsIP, WeightSystem)
Headline
  whether the origin is an interior point of a polytope
Usage
  palpIsIP A
Inputs
  A:Matrix
    over the integers, whose columns span the polytope, or a (combined) weight
    system, as a @TO List@ or a @TO WeightSystem@
Outputs
  :Boolean
    true when the origin lies in the interior of the polytope
Description
  Text
    PALP calls a polytope IP when the origin is an interior point, that is,
    when every facet inequality $\langle a, x \rangle + c \ge 0$ has $c > 0$.
    This asks @TT "poly.x -I"@, which prints @TT "No IP"@ exactly when the
    polytope is not IP.  A reflexive polytope is IP, but an IP polytope need not
    be reflexive, and the summary line of @TO palpInfo@ does not tell the two
    kinds of non-reflexive polytope apart.
  Example
    palpIsIP matrix{{1,0,0,-1},{0,1,0,-1},{0,0,1,-1}}
    palpIsIP matrix{{2,0,0,-1},{0,1,0,-1},{0,0,1,-1}}
    palpIsReflexive matrix{{2,0,0,-1},{0,1,0,-1},{0,0,1,-1}}
  Text
    The origin must be strictly inside: a vertex, or a point outside, does not
    count.
  Example
    palpIsIP matrix{{0,1,0,0},{0,0,1,0},{0,0,0,1}}
    palpIsIP matrix{{1,2,1,1},{0,0,1,0},{0,0,0,1}}
  Text
    A weight system is IP when its polytope $\Delta(q)$ is.  PALP computes the
    polytope even when it is not, so @TO weightSystemVertices@ alone does not
    tell you.
  Example
    palpIsIP {5, 1, 1, 1, 1, 1}
    palpIsIP {9, 1, 1, 2, 5}
    weightSystemVertices {9, 1, 1, 2, 5}
Caveat
  The same question can be answered without PALP by
  @TT "inInterior(0, convexHull A)"@, using @TO "Polyhedra::inInterior"@.
SeeAlso
  palpIsCanonical
  palpIsReflexive
  palpInfo
///

doc ///
Key
  palpIsCanonical
  (palpIsCanonical, Matrix)
  (palpIsCanonical, List)
  (palpIsCanonical, WeightSystem)
Headline
  whether the origin is the only interior lattice point of a polytope
Usage
  palpIsCanonical A
Inputs
  A:Matrix
    over the integers, whose columns span the polytope, or a (combined) weight
    system, as a @TO List@ or a @TO WeightSystem@
Outputs
  :Boolean
    true when the polytope is IP and the origin is its only interior lattice
    point
Description
  Text
    A reflexive polytope is canonical, and a canonical polytope is IP, but
    neither converse holds.  The fan polytope of the weighted projective space
    $P(1,1,1,2)$ is canonical but not reflexive, while the simplex below it is
    IP but has a second interior lattice point.
  Example
    M = matrix{{1,0,0,-1},{0,1,0,-1},{0,0,1,-2}}
    palpIsCanonical M, palpIsReflexive M
    S = matrix{{2,0,0,-1},{0,1,0,-1},{0,0,1,-1}}
    palpIsIP S, palpIsCanonical S
    interiorLatticePoints convexHull S
  Text
    This first asks @TO palpIsIP@, then @TT "poly.x -e"@: a reflexive polytope
    is canonical, and otherwise PALP prints the facet inequalities, which are
    checked against the lattice points from @TO palpMPoints@.
  Text
    For a single weight system, IP and canonical are the same: an interior
    lattice point $m$ of $\Delta(q)$ has every $m_i \ge 0$ and
    $\sum q_i m_i = 0$, so it is the origin.  In dimension five many of these are
    not reflexive.
  Example
    L = weightSystems(5, Degrees => 10);
    #L, #select(L, palpIsReflexive), #select(L, palpIsCanonical)
Caveat
  Some authors also require the vertices of a canonical Fano polytope to be
  primitive lattice vectors; this does not check that.  Counting the lattice
  points needs @TT "poly.x -p"@, which can be slow in high dimension when the
  polytope is not reflexive.  @TT "interiorLatticePoints convexHull A"@, from
  @TO "Polyhedra::interiorLatticePoints"@, answers the same question without PALP.
SeeAlso
  palpIsIP
  palpIsReflexive
  palpMPoints
///

doc ///
Key
  weightSystems
  (weightSystems, ZZ)
  [weightSystems, Degrees]
  [weightSystems, Reflexive]
Headline
  the weight systems of a given dimension
Usage
  L = weightSystems n
Inputs
  n:ZZ
    the dimension of the resulting polytopes
  Degrees => {List, ZZ}
    a range {lo, hi} of degrees, where hi may be @TO infinity@, or a single
    degree
  Reflexive => Boolean
    whether to return only the weight systems whose polytope is reflexive
Outputs
  L:List
    of the IP weight systems $\{d, q_0, \ldots, q_n\}$ of dimension $n$, each a
    @TO WeightSystem@ with one row, in the order PALP produces them
Description
  Text
    A weight system is a list $\{d, q_0, \ldots, q_n\}$ of positive integers with
    $d = q_0 + \cdots + q_n$.  The weights $q_i$ are the degrees of the variables
    $x_0, \ldots, x_n$ of the weighted projective space
    $\mathbb{P}(q_0, \ldots, q_n)$, and $d$ is the degree of a hypersurface in
    it.  Since the anticanonical class of $\mathbb{P}(q)$ has degree
    $q_0 + \cdots + q_n$, the condition $d = \sum q_i$ says exactly that such a
    hypersurface has trivial canonical class.

    The polytope of the weight system is the convex hull of the lattice points of
    $$\Delta(q) = \{x \in \mathbb{R}^{n+1} : q_0 x_0 + \cdots + q_n x_n = 0,\ x_i \ge -1\},$$
    which has dimension $n$.  Its lattice points correspond one to one to the
    monomials $x^m$ of degree $d$, by $m = x + (1, \ldots, 1)$.  So
    $\Delta(q) + (1, \ldots, 1)$ is the Newton polytope of a general polynomial
    of degree $d$, and the origin corresponds to the monomial
    $x_0 x_1 \cdots x_n$.  The weight system is IP if the origin is an interior
    point of $\Delta(q)$, and those are the ones returned here.

    For example, there are 95 IP weight systems of dimension 3, that is, with
    four weights.
  Example
    L = weightSystems 3;
    #L
    L#0
    L#90
  Text
    For the weight system $\{10, 1, 2, 3, 4\}$, the monomials of degree 10 and
    the lattice points of the polytope match up.
  Example
    R = QQ[x_0..x_3, Degrees => {1, 2, 3, 4}];
    numColumns basis(10, R)
    numColumns palpMPoints {10, 1, 2, 3, 4}
  Text
    In dimension at most 4 every IP weight system gives a reflexive polytope, but
    in dimension 5 that fails.
  Example
    all(L, palpIsReflexive)
    member(weightSystem {7, 1, 1, 1, 1, 1, 2}, weightSystems(5, Degrees => 7))
    palpIsReflexive {7, 1, 1, 1, 1, 1, 2}
  Text
    In dimension 5 and higher many IP weight systems are not reflexive, and
    functions that need a reflexive polytope, such as
    @TO palpNefPartitions@, return @TO null@ for them.  With
    {\tt Reflexive => true} only the reflexive ones are returned.  This uses
    the mark {\tt cws.x} puts on each of them, so it costs nothing extra, and
    gives the same answer as selecting them with @TO palpIsReflexive@.
  Example
    W = weightSystems(6, Degrees => 20);
    R = weightSystems(6, Degrees => 20, Reflexive => true);
    (#W, #R)
    #select(weightSystems(5, Degrees => 10), palpIsReflexive)
    weightSystems(5, Degrees => 10, Reflexive => true)
  Text
    Restricting the degree:
  Example
    weightSystems(5, Degrees => 10)
    #weightSystems(5, Degrees => {20, 20})
  Text
    An unbounded range is allowed in dimension at most 4.
  Example
    weightSystems(3, Degrees => {40, infinity})
  Text
    {\bf Combined weight systems.}  A combined weight system is several weight
    systems on the same variables, one per row.  PALP writes them one after
    another in a single list,
    $\{d_1, q_{10}, \ldots, q_{1n}, \ d_2, q_{20}, \ldots, q_{2n}, \ \ldots\}$,
    and they may also be given as a list of rows,
    $\{\{d_1, q_{10}, \ldots, q_{1n}\}, \{d_2, q_{20}, \ldots, q_{2n}\}, \ldots\}$.
    The weights are nonnegative integers, each degree is the sum of the weights
    in its row, and every variable has a positive weight in some row.  The weight
    of $x_i$ is then the vector $(q_{1i}, \ldots, q_{ki})$, the degree is
    $(d_1, \ldots, d_k)$, and the polytope, cut out by all $k$ equations, has
    dimension $n + 1 - k$.  For example, the hypersurfaces of bidegree $(3,3)$
    in $\mathbb{P}^2 \times \mathbb{P}^2$:
  Example
    w = {3,1,1,1,0,0,0, 3,0,0,0,1,1,1};
    S = QQ[x_0..x_5, Degrees => {{1,0},{1,0},{1,0},{0,1},{0,1},{0,1}}];
    numColumns basis({3,3}, S)
    numColumns palpMPoints w
    palpMPoints {{3,1,1,1,0,0,0}, {3,0,0,0,1,1,1}} == palpMPoints w
  Text
    The rows need not involve disjoint sets of variables.  Only the span of the
    rows matters, so the same polytope can be written with overlapping rows:
  Example
    palpNormalForm w == palpNormalForm {6,1,1,1,1,1,1, 3,0,0,0,1,1,1}
    palpMVertices {4,1,1,1,1,0, 2,0,0,1,0,1}
  Text
    The functions of this package that take a weight system, such as
    @TO palpMVertices@ and @TO palpNormalForm@, accept combined weight systems
    too; this function only produces single ones.

    This function calls the PALP program {\tt cws.x} with the option {\tt -w}.
Caveat
  PALP's time depends very much on the range.  With no upper bound, in dimension
  at most 4, it lists all weight systems using the algorithm of Kreuzer and
  Skarke, and discards the degrees below {\tt lo} afterwards: for dimension 4,
  that is all 184026 of them, of degrees 5 to 3486, in about three minutes,
  whatever {\tt lo} is.  With a finite range it searches degree by degree, which
  is fast for small degrees and slow for large ones: in dimension 4, degrees 5
  to 100 take a second or two, degrees 5 to 200 half a minute, and degree 3402
  alone more than half an hour.  In dimension 5 and higher, PALP requires a
  finite range.
SeeAlso
  weightSystem
  weightSystemVertices
  weightSystemPolytope
///

doc ///
Key
  weightSystem
  (weightSystem, List)
  [weightSystem, Verify]
  WeightSystem
  (symbol ==, WeightSystem, WeightSystem)
Headline
  make a (combined) weight system
Usage
  cws = weightSystem q
Inputs
  q:List
    a weight system $\{d, q_0, \ldots, q_n\}$, or a combined weight system,
    written either flat, as PALP writes it,
    $\{d_1, q_{10}, \ldots, q_{1n}, \ d_2, q_{20}, \ldots, q_{2n}, \ \ldots\}$,
    or as a list of rows
  Verify => Boolean
    if true, an input that is not a weight system is an error; if false, the
    result is then @TO null@
Outputs
  cws:WeightSystem
    the list of rows $\{\{d_1, q_{10}, \ldots, q_{1n}\}, \{d_2, q_{20}, \ldots, q_{2n}\}, \ldots\}$
Description
  Text
    A {\bf weight system} is a list $\{d, q_0, \ldots, q_n\}$ of positive
    integers with $d = q_0 + \cdots + q_n$: the degree $d$ followed by the
    weights $q_i$.  A {\bf combined weight system} is a list of $r \ge 1$ rows
    $\{d_j, q_{j0}, \ldots, q_{jn}\}$, all of the same length, such that
  Code
    UL {
        "every entry is an integer, and every weight q_ji is at least 0;",
        "each degree is the sum of its row, d_j = q_j0 + ... + q_jn, and is positive;",
        "every column has a positive weight in some row."
        }
  Text
    So a weight system is a combined weight system with one row, and zero
    weights occur only when there are several rows.  The weights need not be
    coprime: $\{6, 2, 2, 2\}$ is a weight system, and gives the same polytope as
    $\{3, 1, 1, 1\}$, since the polytope below depends only on the ratios of
    the weights in each row.  These are the inputs PALP
    accepts, as described in the PALP paper of Kreuzer and Skarke
    (Comput. Phys. Commun. 157 (2004), arXiv:math/0204356, section 2).
  Text
    The polytope of a combined weight system is
    $\Delta(q) = \operatorname{conv}\{ x \in \mathbb{Z}^{n+1} : \sum_i q_{ji} x_i = 0
    \text{ for all } j, \ x_i \ge -1 \text{ for all } i \}$,
    a lattice polytope in the lattice
    $\{ x \in \mathbb{Z}^{n+1} : \sum_i q_{ji} x_i = 0 \text{ for all } j \}$,
    of dimension $n + 1 - r$ when the rows are linearly independent.
    Its lattice points $x$ correspond to the monomials $\prod_i y_i^{x_i + 1}$
    in variables $y_0, \ldots, y_n$ of multidegree $(d_1, \ldots, d_r)$, where
    $y_i$ has multidegree $(q_{1i}, \ldots, q_{ri})$.  The weight system is called IP when the
    origin is an interior point of $\Delta(q)$, and reflexive when $\Delta(q)$ is;
    see @TO palpIsIP@ and @TO palpIsReflexive@.  PALP computes $\Delta(q)$ for
    every combined weight system, IP or not.
  Text
    Kreuzer and Skarke's classification papers use a narrower notion, which
    this package does not impose.  There the weights are positive reals summing
    to 1, the barycentric coordinates of the origin in a simplex of lattice
    points containing it in its interior, and a combined weight system is the
    collection of these for several such simplices, with weight 0 on the
    vertices outside each simplex (Kreuzer-Skarke, Rev. Math. Phys. 14 (2002),
    arXiv:math/0001106, section 2.2; Skarke, Mod. Phys. Lett. A 11 (1996),
    arXiv:alg-geom/9603007).  Scaled to integers, each such row is a weight
    system in the sense above.
  Text
    An object of type {\tt WeightSystem} is a combined weight system, stored as
    its list of rows.  Two of them are equal, under {\tt ==}, when they have the
    same rows.
  Example
    weightSystem {10, 1, 2, 3, 4}
  Text
    A flat list is split into rows the way PALP splits it: a row is a degree
    followed by weights adding up to it, zeros that come next still belong to the
    row, and the next positive number starts a new row.  So a later degree may
    be smaller than the first.
  Example
    cws = weightSystem {3,1,1,1,0,0,0, 3,0,0,0,1,1,1}
    cws == weightSystem {{3,1,1,1,0,0,0}, {3,0,0,0,1,1,1}}
    weightSystem {2,1,1,0, 1,0,0,1}
    toPalp cws
  Text
    The result is checked: each degree is the sum of the weights in its row, the
    rows have the same length, and every variable has a positive weight in some
    row (see @TO (isWellDefined, WeightSystem)@).  If not, this is an error, or,
    with {\tt Verify => false}, the result is @TO null@.
  Example
    weightSystem({10,1,2,3,3}, Verify => false) === null
    weightSystem({3,1,1,1,0,0, 3,0,0,0,1,1,1}, Verify => false) === null
    weightSystem({3,1,1,1,0}, Verify => false) === null
  Text
    The functions of this package that take a weight system, such as
    @TO palpMVertices@, accept a {\tt WeightSystem} as well as a @TO List@.
  Example
    palpMVertices cws == palpMVertices {3,1,1,1,0,0,0, 3,0,0,0,1,1,1}
SeeAlso
  (isWellDefined, WeightSystem)
  weightSystems
  toPalp
///

doc ///
Key
  (isWellDefined, WeightSystem)
Headline
  whether a WeightSystem is a valid combined weight system
Usage
  isWellDefined cws
Inputs
  cws:WeightSystem
Outputs
  :Boolean
    true if {\tt cws} is a non-empty list of rows $\{d, q_0, \ldots, q_n\}$ such
    that every entry is an integer, every $q_i \ge 0$, $d = q_0 + \cdots + q_n$
    and $d > 0$, all rows have the same length, and for each $i$ some row has
    $q_i > 0$
Description
  Text
    The last condition says that no variable has weight zero in every row.

    A result of @TO weightSystem@ always satisfies these conditions.  This
    function is for checking one made some other way.
  Example
    isWellDefined weightSystem {10, 1, 2, 3, 4}
    isWellDefined new WeightSystem from {{10, 1, 2, 3, 4}}
    isWellDefined new WeightSystem from {{3, 1, 1, 1, 0}}
    isWellDefined new WeightSystem from {{4, 1, 1, 1}}
  Text
    When @TO "debugLevel"@ is positive, the reason for returning false is printed.
  Example
    debugLevel = 1
    isWellDefined new WeightSystem from {{2, 1, 1}, {3, 1, 1, 1}}
    debugLevel = 0
SeeAlso
  weightSystem
  weightSystems
///

doc ///
Key
  weightSystemVertices
  (weightSystemVertices, List)
  (weightSystemVertices, WeightSystem)
Headline
  the vertices of the polytope of a weight system
Usage
  V = weightSystemVertices q
Inputs
  q:List
    of integers {d, q_0, ..., q_n}, the degree followed by the weights, or a
    combined weight system: several such blocks one after another, or a list
    of them; or a @TO WeightSystem@
Outputs
  V:Matrix
    over the integers, the vertices of the polytope as its columns
Description
  Text
    This is @TO palpMVertices@ applied to a weight system, under the name one
    would look for it by.
  Example
    ws = {3402, 40, 41, 486, 1134, 1701}
    V = weightSystemVertices ws
      P = convexHull V
    assert isReflexive P
    assert(dim P == 4)
  Text
    Combined weight systems are accepted as well, which is why this goes through
    PALP rather than through @TO weightSystemPolytope@.
  Example
    weightSystemVertices {3,1,1,1,0,0,0, 3,0,0,0,1,1,1}
Caveat
  The result is @TO null@ when PALP declines the weight system, for instance
  because the corresponding polytope has no interior lattice point.

  A weight system with just two weights describes a polytope of dimension one,
  on which PALP fails an assertion in @TT "ReadCwsPp"@ and aborts, so that case
  is refused here.  Give the polytope as a matrix instead: the one dimensional
  reflexive polytope is @TT "matrix{{1,-1}}"@.
SeeAlso
  weightSystems
  weightSystemPolytope
  palpMVertices
///

doc ///
Key
  weightSystemPolytope
  (weightSystemPolytope, List)
  (weightSystemPolytope, WeightSystem)
Headline
  the polytope of a weight system, computed from the definition
Usage
  P = weightSystemPolytope q
Inputs
  q:List
    of integers {d, q_0, ..., q_n}, where d is the sum of the weights, or a
    @TO WeightSystem@ with a single row
Outputs
  P:Polyhedron
Description
  Text
    For a weight system $q$ of degree $d = \sum q_i$, this is
    $\Delta(q) = \{x : \sum q_i x_i = 0, \ x_i \ge -1\}$, returned in a basis of
    the lattice $\ker(q) \cap \mathbb{Z}^n$ so that it is full dimensional.
  Example
    P = weightSystemPolytope {10, 1, 2, 3, 4}
    dim P
      isReflexive P
    vertices P
  Text
    Nothing here calls PALP, so this is a way of checking PALP's answer against
    the definition.  The two agree:
  Example
    ws = {30, 4, 4, 6, 7, 9};
    assert(palpNormalForm lift(vertices weightSystemPolytope ws, ZZ)
        == palpNormalForm weightSystemVertices ws)
  Text
    Note that $\Delta(q)$ is in general only a rational polytope: setting every
    coordinate but the $j$-th to $-1$ gives the vertex $(d - q_j)/q_j$, which is
    an integer only when $q_j$ divides $d$.  What is returned, and what PALP
    reports, is the convex hull of its lattice points.
  Example
    ws = {10, 1, 2, 3, 4};
    numColumns vertices weightSystemPolytope ws
Caveat
  Only a single weight system whose degree is the sum of its weights is
  accepted; for a combined weight system use @TO weightSystemVertices@.
SeeAlso
  weightSystemVertices
  weightSystems
///

doc ///
Key
  palpNefPartitions
  (palpNefPartitions, Matrix, ZZ)
  (palpNefPartitions, NormalToricVariety, ZZ)
  (palpNefPartitions, List, ZZ)
  (palpNefPartitions, WeightSystem, ZZ)
  [palpNefPartitions, HodgeNumbers]
  [palpNefPartitions, Products]
  [palpNefPartitions, Projections]
  [palpNefPartitions, KeepSymmetric]
Headline
  the nef partitions of a reflexive polytope, with their Hodge numbers
Usage
  L = palpNefPartitions(A, c)
Inputs
  A:{Matrix,NormalToricVariety,List,WeightSystem}
    a matrix over the integers whose columns are the vertices of the reflexive
    polytope $\Delta^*$ in the N lattice, or some or all of its nonzero lattice
    points (including the vertices); or a normal toric variety, whose rays are
    used in the same way; or a (combined) weight system, as a @TO List@ or a
    @TO WeightSystem@, which describes the polytope $\Delta$ in the M lattice
  c:ZZ
    the codimension, that is, the number of parts
  HodgeNumbers => Boolean
    whether to compute the Hodge numbers (if false, PALP's option {\tt -p})
  Products => Boolean
    whether to include the nef partitions that are direct products ({\tt -D})
  Projections => Boolean
    whether to include the nef partitions that are projections ({\tt -P})
  KeepSymmetric => Boolean
    whether to keep nef partitions that are equivalent to another one under a
    symmetry of the polytope ({\tt -s})
Outputs
  L:List
    of @TO NefPartition@s, or @TO null@ if the polytope is not reflexive
Description
  Text
    Let $\Delta^* \subset N_{\mathbb{R}}$ be a reflexive polytope, so that the cones
    over its faces form the fan of a Gorenstein toric Fano variety, whose rays
    are the vertices of $\Delta^*$.  A nef partition of codimension $c$ is a
    partition $E_1, \ldots, E_c$ of the vertices such that each divisor
    $D_j = \sum_{v \in E_j} D_v$ is Cartier and nef.  Equivalently, writing
    $\Delta_j$ for the polytope of $D_j$ in $M_{\mathbb{R}}$,
    $\Delta = \Delta_1 + \cdots + \Delta_c$.  The general complete intersection
    of sections of $D_1, \ldots, D_c$ is then a Calabi-Yau variety of dimension
    $\dim \Delta - c$, and its (stringy) Hodge numbers depend only on the
    polytopes.

    This function calls the PALP program {\tt nef.x}.  Each element of the
    result is a @TO NefPartition@, which records the vertices of $\Delta^*$,
    one per column, and the $c$ parts, each a list of column indices; see
    @TO NefPartition@ for how to get at these, and at the Hodge numbers.  For
    example, the weight system $\{10, 1, 1, 2, 2, 2, 2\}$ gives three
    Calabi-Yau threefolds in codimension two.
  Example
    L = palpNefPartitions({10, 1, 1, 2, 2, 2, 2}, 2);
    #L
    n = first L;
    parts n
    vertices n
    (hodgeNumbers n, euler n)
    apply(L, hodgeNumbers)
  Text
    All $c$ parts are listed, and together they contain every vertex.  (PALP
    itself prints only $c-1$ of them; the one it leaves out is put last.)

    A matrix is read as the polytope $\Delta^*$ in the N lattice, and the
    indices in the parts follow the order of its columns.  For the cross
    polytope, the toric variety is $(\mathbb{P}^1)^4$, and in codimension three the
    complete intersections are curves, which have no $h^{1,i}$.
  Example
    A = id_(ZZ^4) | -id_(ZZ^4)
    L = palpNefPartitions(A, 3);
    #L
    vertices first L == A
    apply(L, parts)
    apply(L, n -> (hodgeNumbers n, euler n))
  Text
    The columns may include lattice points of $\Delta^*$ other than its
    vertices.  The vertices are then those among them, in the order they
    appear, and the other points do not appear in the parts.

    For a normal toric variety, its rays are used.  When the rays are exactly
    the vertices of $\Delta^*$, as for a smooth Fano variety, the parts are lists
    of ray indices, and can be given directly to @TO nefPartitionDivisors@.
  Example
    needsPackage "NormalToricVarieties"
    X = smoothFanoToricVariety(3, 5);
    L = palpNefPartitions(X, 2);
    vertices first L == transpose matrix rays X
    apply(L, parts)
    all(L, n -> all(nefPartitionDivisors(X, parts n), isNef))
  Text
    By default PALP leaves out two kinds of nef partition.  A projection is one
    in which some part is a single vertex $v$, so that $D_j$ is the toric divisor
    $D_v$; the complete intersection is then one of codimension $c-1$ in the
    toric variety $D_v$.  A direct product is one whose parts split into two
    groups spanning complementary subspaces of $N_{\mathbb{R}}$, so that the toric
    variety and the complete intersection are products.  These, and the dual
    projections, are listed by @TO (properties, NefPartition)@.  For $\mathbb{P}^5$ in
    codimension two, there are the complete intersections of degrees $(3,3)$ and
    $(2,4)$, and, as a projection, the quintic threefold in a hyperplane.
  Example
    L = palpNefPartitions({6, 1, 1, 1, 1, 1, 1}, 2, Projections => true);
    apply(L, n -> (parts n, hodgeNumbers n, properties n))
  Text
    PALP also keeps only one nef partition from each class of those equivalent
    under symmetries of the polytope.  Which one it keeps depends on how it
    numbers the vertices, so the same polytope given in different ways can give
    different, but equivalent, representatives.  With {\tt KeepSymmetric => true}
    all of them are listed, each once.  (PALP itself, with {\tt -s}, prints each
    one again for every order of the parts it names, which in codimension three
    and higher lists each nef partition more than once; only the first is kept
    here.)
  Example
    #palpNefPartitions({10, 1, 1, 2, 2, 2, 2}, 2, KeepSymmetric => true)
  Text
    Computing the Hodge numbers can take much longer than finding the nef
    partitions, especially in codimension three and higher.  For the
    complete intersection of degrees $(2,2,3)$ in $\mathbb{P}^6$, finding the nef
    partition takes a fraction of a second, but its Hodge numbers take about 20
    seconds.  With {\tt HodgeNumbers => false} they are not computed, and
    @TO hodgeNumbers@ computes them later, when they are asked for.
  Example
    L = palpNefPartitions({7, 1, 1, 1, 1, 1, 1, 1}, 3, HodgeNumbers => false)
    parts first L
  Text
    A polytope that is not reflexive gives @TO null@, and a reflexive polytope
    with no nef partition of the given codimension gives the empty list.
  Example
    palpNefPartitions({9, 1, 1, 2, 5}, 2) === null
    palpNefPartitions({36, 7, 8, 9, 12}, 2)
Caveat
  Codimension $c$ needs a PALP built for dimension at least $\dim N + c - 1$;
  see @TO "palpDmax"@.
SeeAlso
  NefPartition
  palpHodgeNumbers
  cayleyPolytopeVertices
  isProjection
  nefPartitionDivisors
  naiveNefPartitions
  "palpDmax"
///

doc ///
Key
  NefPartition
  Codim
  Parts
  Vertices
  Properties
  Euler
  HodgeZero
  Projection
  DirectProduct
  DualProjection
  (parts, NefPartition)
  (vertices, NefPartition)
  (codim, NefPartition)
  (euler, NefPartition)
  hodgeNumbers
  (hodgeNumbers, NefPartition)
  hodgeZero
  (hodgeZero, NefPartition)
  properties
  (properties, NefPartition)
Headline
  the class of nef partitions of reflexive polytopes
Description
  Text
    A nef partition, as returned by @TO palpNefPartitions@, is a hash table
    with these keys:

    {\tt Vertices}: the vertices of the reflexive polytope $\Delta^*$ in the N
    lattice, one per column of a matrix;

    {\tt Parts}: the $c$ parts $E_1, \ldots, E_c$, each a list of column
    indices into {\tt Vertices}, which together contain every vertex;

    {\tt Codim}: the number $c$ of parts;

    {\tt Properties}: a list of those of the symbols {\tt Projection},
    {\tt DirectProduct} and {\tt DualProjection} that apply.  A projection has
    a part consisting of a single vertex; a direct product has its parts falling
    into two groups that span complementary subspaces of $N_{\mathbb{R}}$; and a
    dual projection is one whose dual nef partition is a projection.

    These are given by the functions @TO (vertices, NefPartition)@,
    @TO (parts, NefPartition)@, @TO (codim, NefPartition)@ and
    @TO (properties, NefPartition)@, or as {\tt n.Vertices}, {\tt n.Parts}, and
    so on.
  Example
    n = first palpNefPartitions({10, 1, 1, 2, 2, 2, 2}, 2, HodgeNumbers => false);
    vertices n
    parts n
    codim n
    properties n
    n.Parts
  Text
    The Hodge numbers $h^{1,1}, h^{1,2}, \ldots, h^{1,d-1}$ of the general
    complete intersection $X$, of dimension $d$, and its Euler characteristic,
    are kept in the cache of the nef partition, under the keys
    {\tt HodgeNumbers} and {\tt Euler}, along with the list
    $h^{0,0}, h^{0,1}, \ldots, h^{0,d}$ under {\tt HodgeZero}.  That list is
    $1, 0, \ldots, 0, 1$ unless the nef partition is a direct product, when
    $X$ may have several components ($h^{0,0} > 1$), or none
    ($h^{0,0} = 0$), or be a product with a factor having $h^{0,i} \ne 0$.
    @TO palpNefPartitions@ puts them there, unless it is given
    {\tt HodgeNumbers => false}.  The functions @TO hodgeNumbers@,
    @TO (euler, NefPartition)@ and @TO hodgeZero@ return them, first computing
    them, with @TO palpHodgeNumbers@, if they are not there.
  Example
    n.cache#?HodgeNumbers
    hodgeNumbers n
    euler n
    hodgeZero n
  Text
    For $\mathbb{P}^2 \times \mathbb{P}^2$, one of the nef partitions is a direct
    product, and its complete intersection is a product of two elliptic curves.
  Example
    L = palpNefPartitions({3,1,1,1,0,0,0, 3,0,0,0,1,1,1}, 2, Products => true);
    apply(L, n -> (properties n, hodgeNumbers n, hodgeZero n))
  Text
    For $\mathbb{P}^5$ in codimension two, the quintic threefold in a
    hyperplane appears as a projection.
  Example
    L = palpNefPartitions({6, 1, 1, 1, 1, 1, 1}, 2, Projections => true);
    apply(L, n -> (parts n, properties n))
SeeAlso
  palpNefPartitions
  palpHodgeNumbers
  cayleyPolytopeVertices
  isProjection
  nefPartitionDivisors
///


doc ///
Key
  palpHodgeNumbers
  (palpHodgeNumbers, NefPartition)
  (palpHodgeNumbers, List, Matrix)
Headline
  the Hodge numbers of the complete intersection of one nef partition
Usage
  H = palpHodgeNumbers n
  H = palpHodgeNumbers(parts, V)
Inputs
  n:NefPartition
  parts:List
    of lists of column indices of {\tt V}, the parts $E_1, \ldots, E_c$, which
    together contain each column exactly once
  V:Matrix
    over the integers, whose columns are the vertices of the reflexive polytope
    $\Delta^*$ in the N lattice; other nonzero lattice points of $\Delta^*$ may
    be included, each in the part it belongs to
Outputs
  H:HashTable
    with keys {\tt HodgeNumbers}, the list $h^{1,1}, \ldots, h^{1,d-1}$,
    {\tt Euler}, the Euler characteristic, and {\tt HodgeZero}, the list
    $h^{0,0}, \ldots, h^{0,d}$, of the general complete intersection of
    dimension $d$
Description
  Text
    This computes the stringy Hodge numbers of a single nef partition, by giving
    PALP its Gorenstein (Cayley) polytope, with {\tt nef.x -G}.  It is what
    @TO hodgeNumbers@ calls for a nef partition found with
    {\tt HodgeNumbers => false}, and it lets one compute the Hodge numbers of
    just the nef partitions one is interested in.  Given a @TO NefPartition@,
    the results are also stored in its cache.
  Example
    L = palpNefPartitions({10, 1, 1, 2, 2, 2, 2}, 2, HodgeNumbers => false);
    n = first L;
    n.cache#?HodgeNumbers
    palpHodgeNumbers n
    n.cache#?HodgeNumbers
  Text
    A nef partition can also be given directly, by its parts and the vertices
    of $\Delta^*$.  Here is $(\mathbb{P}^1)^3$, with the anticanonical divisor
    split into two divisors of degrees $(1,1,1)$ each: the complete
    intersection is an elliptic curve.
  Example
    V = id_(ZZ^3) | -id_(ZZ^3)
    palpHodgeNumbers({{0, 1, 2}, {3, 4, 5}}, V)
  Text
    The parts are checked to be a nef partition first: each polytope
    $\Delta_j$ of the divisor $D_j = \sum_{v \in E_j} D_v$ must be a lattice
    polytope, and $\Delta_1 + \cdots + \Delta_c$ must be $\Delta$.  On the
    smooth Fano threefold below, the first part is not nef.
  Example
    W = transpose matrix {{1,0,0},{-1,0,1},{0,1,0},{0,-1,1},{0,0,1},{0,0,-1}}
    try palpHodgeNumbers({{0, 1}, {2, 3, 4, 5}}, W) else "not a nef partition"
    palpHodgeNumbers({{3, 5}, {0, 1, 2, 4}}, W)
Caveat
  This can take much longer than finding the nef partitions; see
  @TO palpNefPartitions@.  Codimension $c$ needs a PALP built for dimension at
  least $\dim N + c - 1$; see @TO "palpDmax"@.
SeeAlso
  palpNefPartitions
  NefPartition
  hodgeNumbers
  cayleyPolytopeVertices
///

doc ///
Key
  cayleyPolytopeVertices
  (cayleyPolytopeVertices, NefPartition)
  (cayleyPolytopeVertices, List, Matrix)
  [cayleyPolytopeVertices, Lattice]
  Lattice
Headline
  the vertices of the Cayley polytope of a nef partition
Usage
  C = cayleyPolytopeVertices n
  C = cayleyPolytopeVertices(parts, V)
Inputs
  n:NefPartition
  parts:List
    of lists of column indices of {\tt V}, the parts $E_1, \ldots, E_c$, which
    together contain each column exactly once
  V:Matrix
    over the integers, whose columns are the vertices of the reflexive polytope
    $\Delta^*$ in the N lattice
  Lattice => String
    either {\tt "M"}, the default, or {\tt "N"}
Outputs
  C:Matrix
    over the integers, whose columns are the vertices of the Cayley polytope
Description
  Text
    For a nef partition $E_1, \ldots, E_c$, write $\Delta_j \subset M_{\mathbb{R}}$
    for the polytope of the divisor $D_j = \sum_{v \in E_j} D_v$, and
    $\nabla_j = \mathrm{conv}(\{0\} \cup E_j) \subset N_{\mathbb{R}}$.  The Cayley
    polytope of $\Delta_1, \ldots, \Delta_c$ is
    $$\mathrm{conv}\left(\bigcup_j \{e_j\} \times \Delta_j\right) \subset \mathbb{R}^{c-1} \times M_{\mathbb{R}},$$
    where $e_1 = 0$ and $e_2, \ldots, e_c$ are the standard basis vectors of
    $\mathbb{R}^{c-1}$, and similarly for the $\nabla_j$ in
    $\mathbb{R}^{c-1} \times N_{\mathbb{R}}$, with {\tt Lattice => "N"}.  These
    are the reflexive Gorenstein polytopes of index $c$ attached to the nef
    partition and to its dual (Batyrev and Borisov), and are what PALP works
    with to compute Hodge numbers; see @TO palpHodgeNumbers@.  The coordinates
    follow PALP's {\tt Make_Gore_Poly}.
  Example
    n = first palpNefPartitions({10, 1, 1, 2, 2, 2, 2}, 2, HodgeNumbers => false);
    C = cayleyPolytopeVertices n
    numColumns C
    CN = cayleyPolytopeVertices(n, Lattice => "N")
  Text
    The N side Cayley polytope belongs to the dual nef partition, whose complete
    intersection is the mirror: PALP finds the Hodge numbers in the other order.
  Example
    hodgeNumbers n
    print palpRun("nef", "-G -c2", CN)
  Text
    On the M side the parts must form a nef partition, since otherwise some
    $\Delta_j$ is not a lattice polytope; that is an error.
Caveat
  The value of {\tt Lattice} is a string, {\tt "M"} or {\tt "N"}.
SeeAlso
  palpHodgeNumbers
  NefPartition
  palpNefPartitions
///

doc ///
Key
  isProjection
  (isProjection, NefPartition)
  (isProjection, List, Matrix)
  isDirectProduct
  (isDirectProduct, NefPartition)
  (isDirectProduct, List, Matrix)
  isDualProjection
  (isDualProjection, NefPartition)
  (isDualProjection, List, Matrix)
Headline
  whether a nef partition is a projection, a direct product, or a dual projection
Usage
  isProjection n
  isDirectProduct n
  isDualProjection n
  isProjection(parts, V)
Inputs
  n:NefPartition
  parts:List
    of lists of column indices of {\tt V}, the parts $E_1, \ldots, E_c$, which
    together contain each column exactly once
  V:Matrix
    over the integers, whose columns are the vertices of the reflexive polytope
    $\Delta^*$ in the N lattice
Outputs
  :Boolean
Description
  Text
    These compute directly, without PALP, the properties that @TO palpNefPartitions@
    reads from PALP's output and lists under @TO (properties, NefPartition)@:
  Text
    @UL {
      {"a nef partition is a ", TT "Projection", " if some part is a single vertex $v$, so
        that the complete intersection lies in the toric divisor $D_v$;"},
      {"it is a ", TT "DirectProduct", " if its parts fall into two groups whose linear spans
        are complementary subspaces of $N_{\\mathbb{R}}$, so that the toric variety and the
        complete intersection are products;"},
      {"it is a ", TT "DualProjection", " if its dual nef partition is a projection: some
        $\\Delta_j$ has just two lattice points."}
      }@
  Text
    On $(\mathbb{P}^1)^3$, pairing each $e_i$ with $-e_i$ gives a direct product
    of three copies of $\mathbb{P}^1$ with two points each, and putting one
    vertex in a part of its own gives a projection.
  Example
    V = id_(ZZ^3) | -id_(ZZ^3)
    isDirectProduct({{0, 3}, {1, 4}, {2, 5}}, V)
    isProjection({{0, 3}, {1, 4}, {2, 5}}, V)
    isProjection({{0}, {1, 2, 3, 4, 5}}, V)
  Text
    They agree with what PALP reports.
  Example
    L = palpNefPartitions({10, 1, 1, 2, 2, 2, 2}, 2, HodgeNumbers => false,
        Projections => true, Products => true);
    apply(L, n -> (properties n, isProjection n, isDirectProduct n, isDualProjection n))
SeeAlso
  NefPartition
  (properties, NefPartition)
  palpNefPartitions
///

doc ///
Key
  naiveNefPartitions
  (naiveNefPartitions, NormalToricVariety, ZZ)
Headline
  search directly for nef partitions of a toric variety
Usage
  L = naiveNefPartitions(V, c)
Inputs
  V:NormalToricVariety
  c:ZZ
    the number of parts
Outputs
  L:List
    of partitions of the rays of V into c parts, each part having a nef
    Cartier sum
Description
  Text
    A nef partition of codimension $c$ is a partition of the rays into $c$ parts
    such that in each part the sum of the divisors is Cartier and nef.  This looks for them directly,
    with no help from PALP, which makes it a useful check on
    @TO palpNefPartitions@.
  Example
    needsPackage "NormalToricVarieties"
    V = smoothFanoToricVariety(3, 5);
    #rays V
    L = naiveNefPartitions(V, 2)
    assert all(L, p -> all(p, b -> isNef sum(b, i -> V_i)))
  Text
    In codimension one the only candidate is the anticanonical divisor.
  Example
    naiveNefPartitions(V, 1)
  Text
    PALP leaves out projections and direct products unless asked for them, and
    keeps only one nef partition from each class of those equivalent under a
    symmetry of the polytope.  With all of them asked for, the two agree.  Since
    the rays of a smooth Fano variety are the vertices of $\Delta^*$, the parts
    are ray indices in both.
  Example
    L = palpNefPartitions(V, 2, HodgeNumbers => false,
        KeepSymmetric => true, Projections => true, Products => true);
    set apply(L, n -> set apply(parts n, set)) === set apply(naiveNefPartitions(V, 2), p -> set apply(p, set))
  Text
    Both conditions matter when $V$ is singular.  Here $V$ is simplicial, so every
    torus-invariant divisor is $\QQ$-Cartier, and 31 of the 63 splits of its 7
    rays into two parts are nef over $\QQ$.  Only 4 of them have both parts
    Cartier, and these are the nef partitions PALP finds.
  Example
    W = normalToricVariety convexHull weightSystemVertices {20, 1, 1, 1, 2, 6, 9};
    isSimplicial W, isSmooth W
    #select(drop(drop(subsets toList(0 ..< #rays W), 1), -1),
        a -> a#0 === 0 and isNef sum(a, i -> W_i) and isNef sum(toList(0 ..< #rays W) - set a, i -> W_i))
    L = naiveNefPartitions(W, 2)
    #palpNefPartitions(W, 2, HodgeNumbers => false,
        KeepSymmetric => true, Projections => true, Products => true)
Caveat
  Every set partition of the rays into c parts is examined, so this is only
  practical when there are few rays.
SeeAlso
  palpNefPartitions
  nefPartitionDivisors
///

doc ///
Key
  nefPartitionDivisors
  (nefPartitionDivisors, NormalToricVariety, List)
Headline
  the divisors belonging to a nef partition
Usage
  Ds = nefPartitionDivisors(V, part)
Inputs
  V:NormalToricVariety
  part:List
    of ray indices, or a list of lists of them
Outputs
  Ds:List
    of @TO ToricDivisor@s, one for each part
Description
  Text
    Any rays not named make up one further part, so a codimension two partition
    can be given by naming one of its two parts.
  Example
    needsPackage "NormalToricVarieties"
    V = smoothFanoToricVariety(3, 5);
    Ds = nefPartitionDivisors(V, {3, 5})
    assert all(Ds, isNef)
    assert(sum Ds == - toricDivisor V)
  Text
    In higher codimension the parts are given as a list of lists, and again the
    rays left over make up the last part.
  Example
    Es = nefPartitionDivisors(V, {{0, 2}, {1, 3, 4}})
    assert all(Es, isNef)
    assert(sum Es == - toricDivisor V)
  Text
    The divisors always add up to the anticanonical divisor, since the parts
    partition the rays.

    When the rays of $V$ are exactly the vertices of $\Delta^*$, as for a smooth
    Fano variety, the parts of a nef partition found by
    @TO palpNefPartitions@ applied to $V$ are lists of ray indices, and can be
    given here directly.
  Example
    L = palpNefPartitions(V, 2, HodgeNumbers => false);
    apply(L, n -> nefPartitionDivisors(V, parts n))
    all(L, n -> all(nefPartitionDivisors(V, parts n), isNef))
SeeAlso
  naiveNefPartitions
  palpNefPartitions
///

TEST ///
  -- toPalp on matrices, and the round trip through fromPalpMatrix
  M = matrix{{1,1,1,1},{0,1,2,3}}
  assert(toPalp M == "2 4\n1 1 1 1\n0 1 2 3\n")
  assert(M == fromPalpMatrix toPalp M)
  assert(transpose M == fromPalpMatrix toPalp transpose M)

  -- from smoothFanoToricVariety(3, 5)
  N = transpose matrix {{1, 0, 0}, {-1, 0, 1}, {0, 1, 0}, {0, -1, 1}, {0, 0, 1}, {0, 0, -1}}
  assert(N == fromPalpMatrix toPalp N)

  -- weight systems are passed through as a single line
  assert(toPalp {10,1,2,3,4} == "10 1 2 3 4\n")

  assert(try (toPalp matrix(QQ, {{1,2}}); false) else true)      -- not over ZZ
  assert(try (toPalp {1, 1/2}; false) else true)                 -- not integers
  assert(try (fromPalpMatrix "   \n  \n"; false) else true)      -- blank output
///

TEST ///
  -- palpProgram picks a variant and caches it
  assert(instance(palpProgram "poly", Program))
  assert(palpProgram "poly" === palpProgram "poly")          -- cached
  assert(palpDmax == 11)
  assert((palpProgram "poly")#"name" == "poly-11d.x")

  palpDmax = 6
  assert((palpProgram "poly")#"name" == "poly.x")
  palpDmax = 11

  assert(try (palpDmax = 7; palpProgram "poly"; false) else true)
  palpDmax = 11
///

TEST ///
  -- runPALP leaves nothing behind in the current directory
  debug PALPInterface
  before = set readDirectory ".";
  out = palpRun("poly", "-v", {10,1,2,3,4})
  assert(match("Vertices of P", out))
  assert(set readDirectory "." === before)
///

TEST ///
  -- PALP announces a too-small POLY_Dmax on stdout with exit status 0;
  -- runPALP has to turn that into an actual error.
  ws = {10, 1, 1, 2, 2, 2, 2}    -- dim N = 5, so codim 3 needs POLY_Dmax >= 7
  palpDmax = 11
  assert(match("#part", palpRun("nef", "-Lp -c3", ws)))
  palpDmax = 6
  assert(try (palpRun("nef", "-Lp -c3", ws); false) else true)
  palpDmax = 11
///

TEST ///
  -- PALP orients its output block per example, not per option: poly.x -p prints
  -- 23 x 3 for {10,1,2,3,4} but 2 x 10 for {3,1,1,1}.  Every wrapper must hand
  -- back one point per column regardless.
  for ws in {{10,1,2,3,4}, {30,4,4,6,7,9}, {3,1,1,1}, {5,1,1,1,1,1}} do (
      mv := palpMVertices ws;
      d := numRows mv;
      assert(d === #ws - 2);                       -- dim of Delta(q)
      assert(numColumns mv > d);
      for M in {palpMPoints ws, palpNVertices ws, palpNPoints ws, palpNormalForm ws} do (
          assert(M =!= null);
          assert(numRows M === d);
          assert(numColumns M >= d);
          );
      -- the M-lattice points really are the lattice points of the polytope
      P := convexHull mv;
      assert(set entries transpose palpMPoints ws
          === set entries transpose matrix {latticePoints P});
      -- and the N-lattice vertices are the vertices of the dual
      assert(set entries transpose palpNVertices ws
          === set entries transpose lift(vertices polar P, ZZ));
      );

  -- the specific shapes that used to come back transposed
  assert((numRows palpMPoints {10,1,2,3,4}, numColumns palpMPoints {10,1,2,3,4}) === (3, 23))
  assert((numRows palpMPoints {3,1,1,1}, numColumns palpMPoints {3,1,1,1}) === (2, 10))
  assert((numRows palpNPoints {30,4,4,6,7,9}, numColumns palpNPoints {30,4,4,6,7,9}) === (4, 21))
///

TEST ///
  -- palpNormalForm is an invariant of the lattice polytope
  A = matrix {{0, 0, -3, 2}, {-5, 3, 0, 0}, {-3, -6, 1, 0}, {-3, -2, -1, 1}}
  assert(det A == 1 or det A == -1)
  M = fromPalpMatrix "4 12
   1   0   0   1   1  -1   0  -2   4  -2   0   2
   0   1   0   0  -1   0   0   3  -4   1  -1  -3
   0   0   1  -1   0   0   0   1  -4   3  -1  -3
   0   0   0   0   0   0   1  -1   1  -1   1   1
   "
  assert(numRows M == 4 and numColumns M == 12)
  assert(palpNormalForm M == M)
  assert(palpNormalForm (A*M) == M)

  -- a matrix and the weight system it comes from give the same normal form
  ws = {10,1,2,3,4}
  assert(palpNormalForm ws == palpNormalForm palpMVertices ws)

  -- two weight systems describing the same polytope
  assert(palpNormalForm {3402, 40, 41, 486, 1134, 1701}
      == palpNormalForm {3486, 41, 42, 498, 1162, 1743})
///

TEST ///
  -- a polytope that is IP but not reflexive: PALP has M-lattice answers for it,
  -- but its dual has no lattice description, and poly.x reports that by printing
  -- nothing (-d) or inequalities with an extra inhomogeneous entry (-e).
  M = transpose matrix{{1,-1,-1}, {1,-1,1}, {1,1,-1}, {1,1,2}, {-1,0,0}}
  assert not isReflexive convexHull M
  assert(palpMVertices M == M)
  assert(numColumns palpMPoints M == #latticePoints convexHull M)
  assert(palpNVertices M === null)
  assert(palpNPoints M === null)

  -- but fromPalpMatrix, the raw reader, still refuses to guess
  assert(try (fromPalpMatrix palpRun("poly", "-e", M); false) else true)
///

TEST ///
  -- palpInfo collects the summary line of poly.x -g
  H = palpInfo {10,1,1,2,2,2,2}
  assert(H#"M" === {378, 6})
  assert(H#"N" === {8, 6})
  assert(H#"H" === {2, 0, 350})
  assert(H#"Euler" === 2160)

  V = transpose matrix {{1,0,0},{-1,0,1},{0,1,0},{0,-1,1},{0,0,1},{0,0,-1}}
  H2 = palpInfo V
  assert(H2#"M" === {7, 6})
  assert(H2#"Cor" === {0})
  assert(not H2#?"Euler")
///

TEST ///
  -- cws.x enumerates the weight systems of a given dimension
  wss = weightSystems 3
  assert(#wss == 95)
  assert all(wss, ws -> instance(ws, WeightSystem) and #ws == 1 and isWellDefined ws)
  assert(wss#0 == weightSystem {4, 1, 1, 1, 1})
  assert(wss#90 == weightSystem {44, 4, 5, 13, 22})
  assert all(wss, ws -> #(ws#0) == 5)

  -- a degree range; cws.x then appends a summary line, which must not be parsed
  assert(#weightSystems(5, Degrees => (20,20)) == 61)
  assert(#weightSystems(5, Degrees => (10,10)) == 5)
  assert(weightSystems(5, Degrees => 10) == weightSystems(5, Degrees => {10,10}))
  assert(member(weightSystem {10, 1, 1, 2, 2, 2, 2}, weightSystems(5, Degrees => 10)))

  assert(try (weightSystems(3, Degrees => {1,2,3}); false) else true)

  -- the default is Degrees => {0, infinity}; a lower bound alone filters the
  -- full list, and agrees with a finite range reaching the largest degree, 66
  assert(weightSystems(3, Degrees => {0, infinity}) == wss)
  assert(weightSystems(3, Degrees => {40, infinity}) == select(wss, ws -> ws#0#0 >= 40))
  assert(set weightSystems(3, Degrees => {40, infinity}) === set weightSystems(3, Degrees => {40, 66}))
  assert(weightSystems 1 == {weightSystem {2, 1, 1}})
  assert(#weightSystems 2 == 3)
  -- PALP aborts on lo > hi, and needs a finite range in dimension 5 and up
  ans = trap weightSystems(3, Degrees => {8, 6})
  assert(ans#0 === null and match("lo <= hi", toString ans#1))
  ans = trap weightSystems 5
  assert(ans#0 === null and match("weightSystems\\(5, Degrees => \\{0, 20\\}\\)", toString ans#1))
  assert(try (weightSystems(3, Degrees => {1, 2.5}); false) else true)
  -- Reflexive => true keeps the weight systems whose polytope is reflexive,
  -- read from the flags cws.x prints; in dimension at most 4 that is all of them
  R = weightSystems(5, Degrees => 10, Reflexive => true)
  assert(#R == 4 and all(R, palpIsReflexive))
  assert not any(toList(set weightSystems(5, Degrees => 10) - set R), palpIsReflexive)
  assert(weightSystems(3, Reflexive => true) == wss)
  assert(#weightSystems(6, Degrees => 20, Reflexive => true) == 36)
///

TEST ///
  -- isWellDefined WeightSystem
  assert isWellDefined weightSystem {10,1,2,3,4}
  assert isWellDefined weightSystem {3,1,1,1,0,0,0, 3,0,0,0,1,1,1}
  assert not isWellDefined new WeightSystem from {}
  assert not isWellDefined new WeightSystem from {{3,1,1,1,0}}        -- q_3 is 0 in every row
  assert not isWellDefined new WeightSystem from {{4,1,1,1}}          -- degree is not the sum
  assert not isWellDefined new WeightSystem from {{0,0,0,0}}          -- degree is 0
  assert not isWellDefined new WeightSystem from {{2,3,-1}}           -- negative weight
  assert not isWellDefined new WeightSystem from {{2,1,1},{3,1,1,1}}  -- rows of different lengths
  assert not isWellDefined new WeightSystem from {{2,1/1,1}}          -- entry not in ZZ
  assert not isWellDefined new WeightSystem from {5}                  -- not a list of rows
///

TEST ///
  -- the functions that take a weight system accept a WeightSystem as well as a List
  for w in {{10,1,2,3,4}, {3,1,1,1,0,0,0, 3,0,0,0,1,1,1}} do (
      cws := weightSystem w;
      assert(palpNormalForm cws == palpNormalForm w);
      assert(palpMVertices cws == palpMVertices w);
      assert(palpMPoints cws == palpMPoints w);
      assert(palpNVertices cws == palpNVertices w);
      assert(palpNPoints cws == palpNPoints w);
      assert(palpInfo cws === palpInfo w);
      assert(palpIsReflexive cws === palpIsReflexive w);
      assert(weightSystemVertices cws == weightSystemVertices w);
      assert(palpRun("poly", "-v", cws) == palpRun("poly", "-v", w));
      assert(apply(palpNefPartitions(cws, 2, HodgeNumbers => false), parts) == apply(palpNefPartitions(w, 2, HodgeNumbers => false), parts));
      );
  assert(vertices weightSystemPolytope weightSystem {10,1,2,3,4} == vertices weightSystemPolytope {10,1,2,3,4})
  assert(try (weightSystemPolytope weightSystem {3,1,1,1,0,0,0, 3,0,0,0,1,1,1}; false) else true)
  -- a malformed WeightSystem is refused before PALP sees it
  assert(try (palpMVertices new WeightSystem from {{3,1,1,1,0}}; false) else true)
  assert(try (palpMVertices weightSystem {2,1,1}; false) else true)      -- dimension one
///

TEST ///
  -- palpNefPartitions
  ws = {10,1,1,2,2,2,2}
  L = palpNefPartitions(ws, 2)
  assert(#L == 3 and all(L, N -> instance(N, NefPartition)))
  V = (L#0).Vertices
  assert(set entries transpose V === set entries transpose palpNVertices ws)
  -- each nef partition has c parts, which partition the vertices
  assert all(L, N -> #(N.Parts) == 2 and sort flatten N.Parts == toList(0 ..< numColumns V))
  assert((L#0).cache.HodgeNumbers == {2, 86} and (L#0).cache.Euler == -168)
  assert not any(L, N -> member(Projection, N.Properties) or member(DirectProduct, N.Properties))
  -- projections, products, Hodge numbers, symmetric copies are optional
  LP = palpNefPartitions(ws, 2, Projections => true, HodgeNumbers => false)
  assert(#LP == 4 and #select(LP, N -> member(Projection, N.Properties)) == 1)
  assert not any(palpNefPartitions(ws, 2, HodgeNumbers => false), N -> N.cache#?HodgeNumbers)
  assert(#palpNefPartitions(ws, 2, Products => true, Projections => true, KeepSymmetric => true, HodgeNumbers => false) == 15)
  LD = palpNefPartitions({3,1,1,1,0,0,0, 3,0,0,0,1,1,1}, 2, Products => true)
  assert(#select(LD, N -> member(DirectProduct, N.Properties)) == 1)
  -- h^{0,0}, ..., h^{0,d}: for the product, E x E; otherwise a K3 surface
  assert(apply(LD, N -> N.cache.HodgeZero) == {{1,2,1}, {1,0,1}, {1,0,1}, {1,0,1}})
  assert((L#0).cache.HodgeZero == {1,0,0,1})
  -- codimension 3: three parts
  L3 = palpNefPartitions(ws, 3, HodgeNumbers => false)
  assert(#L3 == 1 and #((L3#0).Parts) == 3)
  -- inputs on which nef.x -Lp fails (which is why we use -V): the weight system
  -- {36,7,8,9,12}, which has no nef partitions, and a Delta^* with 1386 points
  assert(palpNefPartitions({36,7,8,9,12}, 2) == {})
  assert(palpNefPartitions(palpMVertices {12,1,1,2,2,2,2,2}, 2, HodgeNumbers => false) == {})
  -- nothing for a polytope that is not reflexive
  assert(palpNefPartitions({9,1,1,2,5}, 2) === null)
  -- N side input: the parts index the columns of the given vertices
  NV = palpNVertices ws
  LN = palpNefPartitions(NV, 2, KeepSymmetric => true, HodgeNumbers => false)
  assert((LN#0).Vertices == NV)
  asCoords = L -> set apply(L, N -> set apply(N.Parts, p -> set apply(p, i -> (entries transpose N.Vertices)#i)))
  assert(asCoords LN === asCoords palpNefPartitions(ws, 2, KeepSymmetric => true, HodgeNumbers => false))
  -- a NormalToricVariety: its rays are the N side.  Here it is the variety of the
  -- normal fan of Delta, whose rays are the vertices of Delta^*
  needsPackage "NormalToricVarieties"
  X = normalToricVariety convexHull palpMVertices ws
  LX = palpNefPartitions(X, 2, KeepSymmetric => true, HodgeNumbers => false)
  assert((LX#0).Vertices == transpose matrix rays X)
  assert(asCoords LX === asCoords LN)
  -- a polytope not from a weight system: the cross polytope, so the toric variety is (P^1)^4;
  -- in codimension 3 the complete intersections are curves, with no h^{1,i}
  L4 = palpNefPartitions(id_(ZZ^4) | -id_(ZZ^4), 3)
  assert(#L4 == 5 and all(L4, N -> N.cache.HodgeNumbers == {} and N.cache.Euler == 0))
  -- elliptic curves; among the products, several curves, or none at all
  assert all(L4, N -> N.cache.HodgeZero == {1,1})
  L4P = palpNefPartitions(id_(ZZ^4) | -id_(ZZ^4), 3, Products => true)
  assert(set apply(L4P, N -> N.cache.HodgeZero) === set {{1,1}, {2,2}, {4,4}})
  -- the accessor functions, and Hodge numbers computed when asked for, then cached
  n = first palpNefPartitions(ws, 2, HodgeNumbers => false)
  assert(parts n === n.Parts and vertices n === n.Vertices and codim n === 2 and properties n === {})
  assert(n.Parts == (L#0).Parts and not n.cache#?HodgeNumbers)
  assert(hodgeNumbers n == {2, 86} and euler n == -168 and n.cache#?HodgeNumbers)
  -- hodgeZero also computes when needed
  m = first palpNefPartitions(ws, 2, HodgeNumbers => false)
  assert(hodgeZero m == {1, 0, 0, 1} and m.cache#?HodgeNumbers)
  assert(properties last LP === {Projection})
///

TEST ///
  -- isDirectProduct, isProjection, isDualProjection agree with what nef.x reports
  agree = L -> all(L, N -> isProjection N === member(Projection, N.Properties)
      and isDirectProduct N === member(DirectProduct, N.Properties) and isDualProjection N === member(DualProjection, N.Properties))
  L = palpNefPartitions(id_(ZZ^3) | -id_(ZZ^3), 2, KeepSymmetric => true,
      Projections => true, Products => true, HodgeNumbers => false)
  assert(agree L and any(L, isDualProjection) and any(L, isDirectProduct) and any(L, isProjection))
  L = palpNefPartitions(id_(ZZ^4) | -id_(ZZ^4), 3,
      Projections => true, Products => true, HodgeNumbers => false)
  assert(agree L and #L == 19)
  L = palpNefPartitions({10,1,1,2,2,2,2}, 2, Projections => true, Products => true, HodgeNumbers => false)
  assert(agree L and #select(L, isProjection) == 1)
  -- directly from parts and vertices: in (P^1)^3, pairing e_i with -e_i is a product
  V = id_(ZZ^3) | -id_(ZZ^3)
  assert isDirectProduct({{0,3}, {1,4}, {2,5}}, V)
  assert not isProjection({{0,3}, {1,4}, {2,5}}, V)
  assert isProjection({{0}, {1,2,3,4,5}}, V)
///

TEST ///
  -- palpHodgeNumbers, one nef partition at a time, agrees with nef.x's own list
  ws = {10,1,1,2,2,2,2}
  L = palpNefPartitions(ws, 2)
  assert all(L, N -> (old := (N.cache.HodgeNumbers, N.cache.Euler, N.cache.HodgeZero);
      h := palpHodgeNumbers N; (h.HodgeNumbers, h.Euler, h.HodgeZero) == old))
  -- the Cayley polytope lives in Z^(c-1) x M, with one vertex for each vertex of each Delta_i
  C = cayleyPolytopeVertices L#0
  assert(numRows C == 5 + 1)
  assert(set entries transpose C === set entries transpose cayleyPolytopeVertices((L#0).Parts, (L#0).Vertices))
  -- the N side Cayley polytope is that of the dual nef partition: mirror Hodge numbers
  assert match("H:86 2 \\[168\\]", palpRun("nef", "-G -c2", cayleyPolytopeVertices(L#0, Lattice => "N")))
  -- a curve, in codimension 3 in (P^1)^4
  N4 = first palpNefPartitions(id_(ZZ^4) | -id_(ZZ^4), 3, HodgeNumbers => false)
  assert(palpHodgeNumbers N4 === new HashTable from {HodgeNumbers => {}, Euler => 0, HodgeZero => {1, 1}})

  -- codimension 3 on {12,1,1,2,2,2,2,2}: two nef partitions.  Their Hodge
  -- numbers, (2,68) and (2,58), take about 40 s, so are not computed here.
  -- Delta^* may be given by its vertices or by all its lattice points (N side),
  -- and gives the same partitions as the weight system, up to coordinates.
  -- Without KeepSymmetric, PALP keeps one partition per symmetry class, and
  -- which one depends on the numbering, so compare with all of them.
  ws = {12,1,1,2,2,2,2,2}
  L = palpNefPartitions(ws, 3, HodgeNumbers => false)
  assert(#L == 2 and all(L, N -> #N.Parts == 3))
  asCoords = L -> set apply(L, N -> set apply(N.Parts, p -> set apply(p, i -> (entries transpose N.Vertices)#i)))
  allOf = A -> asCoords palpNefPartitions(A, 3, HodgeNumbers => false, KeepSymmetric => true)
  LS = allOf ws
  -- nef.x -s prints each of these 25 twice, with the two parts it names in
  -- either order; palpNefPartitions keeps one of each
  assert(#LS == 25 and #palpNefPartitions(ws, 3, HodgeNumbers => false, KeepSymmetric => true) == 25)
  assert(allOf palpNVertices ws === LS)
  assert(allOf palpNPoints ws === LS)
  assert(#palpNefPartitions(palpNPoints ws, 3, HodgeNumbers => false) == 2)
  -- a partition whose parts are Cartier but not nef is refused: on a smooth
  -- variety every Delta_j is a lattice polytope, so the check must also
  -- compare Delta_1 + ... + Delta_c with Delta
  W = transpose matrix {{1,0,0},{-1,0,1},{0,1,0},{0,-1,1},{0,0,1},{0,0,-1}}  -- smoothFanoToricVariety(3, 5)
  assert(try (cayleyPolytopeVertices({{0,1}, {2,3,4,5}}, W); false) else true)
  assert(try (palpHodgeNumbers({{0,1}, {2,3,4,5}}, W); false) else true)
  assert((palpHodgeNumbers({{3,5}, {0,1,2,4}}, W)).HodgeZero == {1, 1})
  -- parts that are not a partition of the columns are refused
  V = id_(ZZ^3) | -id_(ZZ^3)
  for P in {{{}, {0,1,2,3,4,5}}, {{0,1}, {1,2,3,4,5}}, {{0,1}, {2,3,4}}, {{0,1}, {2,3,4,5,6}}} do
      for f in {isProjection, isDirectProduct, isDualProjection, cayleyPolytopeVertices, palpHodgeNumbers} do
          assert(try (f(P, V); false) else true)
///

TEST ///
  -- weightSystemPolytope is computed from the definition, with no call to PALP;
  -- it must agree with what poly.x -v reports.
  for ws in {{10,1,2,3,4}, {30,4,4,6,7,9}, {3,1,1,1}, {44,4,5,13,22},
             {3402,40,41,486,1134,1701}, {5,1,1,1,1,1}} do (
      P := weightSystemPolytope ws;
      assert(dim P == #ws - 2);
      assert isReflexive P;
      assert(palpNormalForm lift(vertices P, ZZ) == palpNormalForm weightSystemVertices ws);
      );

  -- weightSystemVertices is poly.x on the weight system
  assert(weightSystemVertices {10,1,2,3,4} == palpMVertices {10,1,2,3,4})

  -- combined weight systems go through PALP, but not through the definition
  cws = {3,1,1,1,0,0,0, 3,0,0,0,1,1,1}
  assert(numRows weightSystemVertices cws == 4)
  assert(try (weightSystemPolytope cws; false) else true)
  assert(try (weightSystemPolytope {10,1,2,3}; false) else true)   -- degree is not the sum
///

TEST ///
  -- ws = {10,1,1,2,2,2,2} has dim N = 5, so codim 3 needs POLY_Dmax >= 7 and
  -- only runs under the 11d build.  This is the main reason for wanting it.
  ws = {10, 1, 1, 2, 2, 2, 2}
  assert(palpDmax == 11)
  L3 = palpNefPartitions(ws, 3, HodgeNumbers => false)
  assert(apply(L3, parts) == {{{1, 5}, {3, 4}, {0, 2}}})
  -- codimension 1: the hypersurface itself, with all the vertices in one part
  L1 = palpNefPartitions(ws, 1, HodgeNumbers => false)
  assert(apply(L1, parts) == {{toList(0 ..< numColumns (L1#0).Vertices)}})
  assert(apply(palpNefPartitions({5,1,1,1,1,1}, 1), hodgeNumbers) == {{1, 101}})   -- the quintic
  assert(try (palpNefPartitions(ws, 0); false) else true)
///

TEST ///
  -- a reflexive polytope with no nef partitions gives {}, one that is not
  -- reflexive gives null
  assert(palpNefPartitions({36,7,8,9,12}, 2) == {})
  assert(not palpIsReflexive {14, 1, 2, 2, 2, 2, 2, 3})
  assert(palpNefPartitions({14, 1, 2, 2, 2, 2, 2, 3}, 3) === null)

  -- the parser of nef.x's V fields, on its own
  debug PALPInterface
  assert(palpParsePartitionLine "" === {})
  assert(palpParsePartitionLine "H:2 86 [-168] P:0 V:1 5  6   (2 8) (1 4)     0sec  0cpu"
      === {{1, 5, 6}})
  assert(palpParsePartitionLine "H:20 [24] P:0 V0:1 5  6  V1:3 4   (2 4 4) (1 2 2)  1sec"
      === {{1, 5, 6}, {3, 4}})
///

TEST ///
  -- nef partitions found directly, without PALP
  needsPackage "NormalToricVarieties"
  V = smoothFanoToricVariety(3, 5)
  n = #rays V
  assert(n == 6)

  -- codimension 1: the anticanonical divisor, which is nef since V is Fano
  assert(naiveNefPartitions(V, 1) == {{toList(0..n-1)}})

  P2 = naiveNefPartitions(V, 2)
  assert(#P2 > 0)
  elapsedTime for p in P2 do (
      assert(#p == 2);
      assert(sort flatten p == toList(0..n-1));          -- a partition of the rays
      assert all(p, b -> isNef sum(b, i -> V_i));        -- each part is nef
      );
  assert(#unique P2 == #P2)

  -- more parts than rays is empty, not an error
  assert(naiveNefPartitions(V, n + 1) === {})

  -- PALP, keeping everything, finds the same nef partitions, on every smooth
  -- Fano threefold (whose rays are the vertices of Delta^*)
  asSets = L -> set apply(L, p -> set apply(p, set))
  -- the following is currently too long.  Instead we choose 3 values of i in this range.
  --elapsedTime for i from 0 to 17 do elapsedTime (
  elapsedTime for i in {1,4,6} do elapsedTime (
      X := smoothFanoToricVariety(3, i);
      assert(asSets naiveNefPartitions(X, 2) === asSets apply(palpNefPartitions(X, 2,
                  HodgeNumbers => false, KeepSymmetric => true, Projections => true, Products => true), parts)))
  assert(try (naiveNefPartitions(V, 0); false) else true)

  -- On a singular variety each part must be Cartier as well as nef.  This W is
  -- simplicial, and 31 of the 63 splits of its 7 rays into two parts are nef
  -- over QQ, but only 4 have both parts Cartier.  PALP finds the same 4.
  W = normalToricVariety convexHull weightSystemVertices {20, 1, 1, 1, 2, 6, 9}
  assert(isSimplicial W and not isSmooth W and #rays W == 7)
  splits = select(drop(drop(subsets toList(0 ..< 7), 1), -1), a -> a#0 === 0)
  splitDivisors = for a in splits list (d := sum(a, i -> W_i); {d, -toricDivisor W - d})
  splitDivisorsCartier = select(splitDivisors, x -> isCartier x#0 and isCartier x#1)
  nefParts = select(splitDivisorsCartier, x -> isNef x#0 and isNef x#1)

  assert(#splits === 63)
  assert(#splitDivisors === 63)
  assert(#splitDivisorsCartier === 7)
  assert(#nefParts === 4)

  elapsedTime NW = naiveNefPartitions(W, 2)
  assert(#NW == 4)
  assert all(NW, p -> all(p, b -> isCartier sum(b, i -> W_i)))
  assert(asSets NW === asSets apply(palpNefPartitions(W, 2, HodgeNumbers => false,
              KeepSymmetric => true, Projections => true, Products => true), parts))
  -- the default leaves out the projection and the symmetric copies
  assert(#palpNefPartitions({20, 1, 1, 1, 2, 6, 9}, 2, HodgeNumbers => false) == 1)
///

TEST ///
  -- nefPartitionDivisors completes a partition and adds up to the anticanonical
  needsPackage "NormalToricVarieties"
  V = smoothFanoToricVariety(3, 5)
  n = #rays V

  for p in naiveNefPartitions(V, 2) do (
      Ds := nefPartitionDivisors(V, p);
      assert(#Ds == 2);
      assert all(Ds, isNef);
      assert(sum Ds == - toricDivisor V);
      );

  -- codimension two may be given as the one part, with the rest implied
  D = nefPartitionDivisors(V, {2, 3})
  assert(#D == 2)
  assert(D#0 == V_2 + V_3)
  assert(sum D == - toricDivisor V)
  assert(nefPartitionDivisors(V, {{2, 3}}) == D)

  -- naming all but the last part works too
  E = nefPartitionDivisors(V, {{0, 1}, {2, 3}})
  assert(#E == 3)
  assert(sum E == - toricDivisor V)

  assert(try (nefPartitionDivisors(V, {0, 0}); false) else true)      -- not disjoint
  assert(try (nefPartitionDivisors(V, {n}); false) else true)         -- out of range
  assert(try (nefPartitionDivisors(V, {}); false) else true)          -- empty
///

TEST ///
-- MES test
  -- here are small examples of canonical polytopes which are not reflexive, in dimensions 3,4,5.
  M3 = matrix{{1,0,0,-1},{0,1,0,-1},{0,0,1,-2}}
  P3 = convexHull M3
  assert not isReflexive P3
  assert(1 === # interiorLatticePoints P3)
  palpInfo M3
  
  M4 = fromPalpMatrix"4 5
    1 0 0 0 -1
    0 1 0 0 -1
    0 0 1 0 -1
    0 0 0 1 -3"
  P4 = convexHull M4
  assert not isReflexive P4
  assert(1 === # interiorLatticePoints P4)
  
  M5 = fromPalpMatrix"5 6
    1 0 0 0 0 -1
    0 1 0 0 0 -1
    0 0 1 0 0 -1
    0 0 0 1 0 -1
    0 0 0 0 1 -2"
  P5 = convexHull M5
  assert not isReflexive P5
  assert(1 === # interiorLatticePoints P5)


 P =  weightSystemPolytope{7,1,1,1,2,2}
 isReflexive P
///

TEST ///
  -- MES test.  d=1 on all functions (not too many such polytopes in d=1!).
  -- Up to a change of basis [-1,1] is the only canonical polytope in dimension
  -- one: any longer interval containing the origin has a second interior point.
  V = matrix{{1,-1}}
  assert(1 === #interiorLatticePoints convexHull V)
  assert(2 === #interiorLatticePoints convexHull matrix{{-1,2}})

  assert(palpMVertices V == matrix{{1,-1}})
  assert(palpNVertices V == matrix{{1,-1}})
  assert(palpNormalForm V == matrix{{1,-1}})
  assert(set entries transpose palpMPoints V === set {{1},{-1},{0}})
  assert(set entries transpose palpNPoints V === set {{1},{-1},{0}})
  assert palpIsReflexive V
  assert(palpInfo V === new HashTable from {"M" => {3,2}, "N" => {3,2}})

  -- everything keeps one point per column here too
  for f in {palpMVertices, palpMPoints, palpNVertices, palpNPoints, palpNormalForm}
  do assert(numRows f V === 1)

  -- there is exactly one weight system in dimension one
  assert(weightSystems 1 == {weightSystem {2,1,1}})
  assert(lift(vertices weightSystemPolytope {2,1,1}, ZZ) == matrix{{-1,1}})

  -- PALP itself aborts on a dimension one weight system, failing an assertion
  -- in ReadCwsPp, so that case is refused before PALP is called at all
  ans = trap weightSystemVertices {2,1,1}
  assert(ans#0 === null and instance(ans#1, Error))
  assert match("dimension one", toString ans#1)
///

TEST ///
  -- weight systems are checked before PALP sees them; PALP's own messages come
  -- out as a failure to parse its output, or not at all
  err = q -> (ans := trap palpMVertices q; assert(ans#0 === null); toString ans#1)
  assert match("more than expected sum", err {10,2,3,6,0,1})
  assert match("less than expected sum", err {10,1,2,3,3})
  assert match("q_3 is 0", err {3,1,1,1,0})               -- PALP answers this, wrongly
  assert match("nonnegative", err {10,-1,2,3,6})
  assert match("expected a weight system", err {10,1,2,3,4/1})
  assert match("positive", err {0,0,0,1,1})
  assert match("same number", err {3,1,1,1,0,0, 3,0,0,0,1,1,1})
  assert match("q_6 is 0 in every row", err {3,1,1,1,0,0,0,0, 3,0,0,0,1,1,1,0})
  assert match("more than expected sum", err {1,1,1,3})    -- PALP would read this as degree 3, degree last
  -- the order of the weights does not matter, and combined systems still work -- TODO: what does this mean? (where does the order not matter)
  assert(palpNormalForm {10,1,2,3,4} == palpNormalForm {10,4,3,2,1})
  assert(numRows palpMVertices {3,1,1,1,0,0,0, 3,0,0,0,1,1,1} == 4)
///

TEST ///
  -- a combined weight system may also be given as a list of rows
  w = {3,1,1,1,0,0,0, 3,0,0,0,1,1,1}
  wn = {{3,1,1,1,0,0,0}, {3,0,0,0,1,1,1}}
  assert(toPalp wn == toPalp w)
  assert(palpMVertices wn == palpMVertices w)
  assert(palpMPoints wn == palpMPoints w)
  assert(palpNVertices wn == palpNVertices w)
  assert(palpNormalForm wn == palpNormalForm w)
  assert(palpInfo wn === palpInfo w)
  assert palpIsReflexive wn
  assert(weightSystemVertices wn == weightSystemVertices w)
  assert(apply(palpNefPartitions(wn, 2, HodgeNumbers => false), parts) == apply(palpNefPartitions(w, 2, HodgeNumbers => false), parts))
  -- rows sharing variables: x_2 has weight (1,1).  The lattice points are the
  -- monomials of bidegree (4,2), shifted, so there are as many of each
  v = {4,1,1,1,1,0, 2,0,0,1,0,1}
  vn = {{4,1,1,1,1,0}, {2,0,0,1,0,1}}
  assert(palpMPoints vn == palpMPoints v)
  assert(palpNormalForm vn == palpNormalForm v)
  S = QQ[x_0..x_4, Degrees => {{1,0},{1,0},{1,1},{1,0},{0,1}}]
  assert(numColumns palpMPoints vn == numColumns basis({4,2}, S))
  -- only the span of the rows matters: split rows and overlapping ones agree
  assert(palpNormalForm {{6,1,1,1,1,1,1}, {3,0,0,0,1,1,1}} == palpNormalForm wn)
  -- a single row
  assert(palpNormalForm {{10,1,2,3,4}} == palpNormalForm {10,1,2,3,4})
  -- rows are checked one at a time
  err = q -> (ans := trap palpMVertices q; assert(ans#0 === null); toString ans#1)
  assert match("same number", err {{3,1,1,1,0,0,0}, {3,0,0,1,1,1}})
  assert match("sum of the weights", err {{3,1,1,1,0,0,0}, {4,0,0,0,1,1,1}})
  assert match("nonnegative", err {{3,1,1,1,0,0,0}, {3,0,0,0,1,-1,3}})
  assert match("positive", err {{3,1,1,1,0,0}, {0,0,0,0,0,0}})
  assert match("q_6 is 0 in every row", err {{3,1,1,1,0,0,0,0}, {3,0,0,0,1,1,1,0}})
  assert match("q_3 is 0", err {{3,1,1,1,0}})
///

TEST ///
  -- XXX
  -- test: weightSystem, `toPalp WeightSystem`, `isWellDefined WeightSystem`
  
  -- weightSystem splits a combined weight system the way PALP's ReadCwsPp
  -- does: once the weights reach the degree, zeros are still weights, and the
  -- next positive number starts a new block
  -- a product, P(1,1,2)[4] x P(1,2,2)[5]
  w = {4,1,1,2,0,0,0, 5,0,0,0,1,2,2}
  cws = weightSystem w
  toPalp cws
  assert(numRows palpMVertices w == 4)
  -- not a product: x_2 has weight (1,1)
  w = {4,1,1,1,1,0, 2,0,0,1,0,1}
  cws = weightSystem w
  toPalp cws === toPalp w
  assert(numRows palpMVertices w == 3)
  -- a single weight system is one block
  assert(toList weightSystem {10,1,2,3,4} === {{10, 1, 2, 3, 4}})
  -- a later degree may be smaller than the first
  assert(toList weightSystem {2,1,1,0, 1,0,0,1} === {{2,1,1,0}, {1,0,0,1}})

  -- blocks of different lengths are rejected
  w = {3,1,1,1,0,0, 3,0,0,0,1,1,1}
  (ans, err) = trap weightSystem w
  assert(ans === null and match("same number of weights", toString err))
  assert(weightSystem(w, Verify => false) === null)
  -- a weight that is 0 in every row is rejected
  assert(try (weightSystem {3,1,1,1,0}; false) else true)
  assert(weightSystem({3,1,1,1,0}, Verify => false) === null)
  -- == compares the rows
  assert(weightSystem {3,1,1,1,0,0,0, 3,0,0,0,1,1,1} == weightSystem {{3,1,1,1,0,0,0}, {3,0,0,0,1,1,1}})
  assert(weightSystem {10,1,2,3,4} != weightSystem {10,4,3,2,1})

  -- weights that add to greater than the sum (first entry of each block) are not allowed.
  (ans, err) = trap weightSystem {10,2,3,6,0,1}
  assert(ans === null)
  assert(toString err === "weights sum to more than expected sum");

  -- weights that do not add up to the sum
  (ans, err) = trap weightSystem {10,1,2,3,3}
  assert(ans === null)
  assert(toString err === "weights sum to less than expected sum");
///

TEST ///
  -- the columns need only span the polytope: points that are not vertices, and
  -- repeated columns, give the same answers.  PALP itself stops on a repeated
  -- vertex and counts a repeated interior point twice, so repeats are dropped.
  R = matrix{{1,0,0,-1},{0,1,0,-1},{0,0,1,-1}}
  O = matrix{{0},{0},{0}}
  for A in {R | O, R | R_{0}, R | O | O, R | R | O} do (
      assert(palpMVertices A == palpMVertices R);
      assert(palpMPoints A == palpMPoints R);
      assert(palpNVertices A == palpNVertices R);
      assert(palpNormalForm A == palpNormalForm R);
      assert(palpInfo A === palpInfo R));
  assert((palpInfo(R | O | O))#"M" == {5, 4})
  -- no interior lattice point is needed for the functions about P itself; those
  -- about the dual are taken with respect to the origin
  S = matrix{{0,1,0,0},{0,0,1,0},{0,0,0,1}}                -- the unit simplex
  assert(palpMVertices S == S)
  assert(numColumns palpMPoints S == 4)
  assert(palpNVertices S === null and not palpIsReflexive S)
  T = R + matrix{{5},{5},{5}} * matrix{{1,1,1,1}}         -- R translated
  assert(numColumns palpMPoints T == 5)
  assert(not palpIsReflexive T)
  assert(palpNormalForm T != palpNormalForm R)           -- not invariant under translation
  -- a matrix over QQ with integer entries is lifted to ZZ; others are refused
  assert(palpMVertices sub(R, QQ) == palpMVertices R)
  -- vertices from Polyhedra are over QQ, and in another order
  V = vertices convexHull R
  assert(ring V === QQ)
  assert(set entries transpose palpMVertices V === set entries transpose R)
  assert palpIsReflexive V
  assert match("over ZZ", toString last trap palpMVertices(1/2 * sub(R, QQ)))
  assert match("over ZZ", toString last trap palpMVertices sub(R, RR))
///

TEST ///
  -- palpIsReflexive asks poly.x -e for the vertices of the dual: a reflexive
  -- polytope has them, one that is not gets inequalities instead, which come
  -- back as null.  poly.x -g would answer too, via its N: and F: fields, but it
  -- counts the dual's lattice points and is far slower in higher dimensions.
  assert palpIsReflexive {10, 1, 2, 3, 4}
  assert palpIsReflexive matrix{{1,0,0,-1},{0,1,0,-1},{0,0,1,-1}}

  -- canonical but not reflexive, in dimensions 3, 4 and 5
  M3 = matrix{{1,0,0,-1},{0,1,0,-1},{0,0,1,-2}}
  M4 = id_(ZZ^4) | transpose matrix {{-1,-1,-1,-3}}
  M5 = id_(ZZ^5) | transpose matrix {{-1,-1,-1,-1,-2}}
  for M in {M3, M4, M5} do (
      assert not palpIsReflexive M;
      assert(palpIsReflexive M === isReflexive convexHull M);
      assert(1 === #interiorLatticePoints convexHull M);
      assert((palpInfo M)#?"F" and not (palpInfo M)#?"N");
      );

  -- the fan polytope of P(1,a_1,..,a_d) is reflexive exactly when every weight
  -- divides the degree 1 + sum a_i
  for a1 from 1 to 4 do for a2 from a1 to 4 do for a3 from a2 to 4 do (
      a := {a1, a2, a3};
      M := id_(ZZ^3) | transpose matrix {-a};
      if dim convexHull M =!= 3 then continue;
      d := 1 + sum a;
      assert(palpIsReflexive M === all(prepend(1, a), q -> d % q === 0));
      );
///

TEST ///
  -- PALP has several compile-time bounds besides POLY_Dmax, and reports running
  -- out of any of them on stdout while exiting with status 0.  poly.x -N hits
  -- SYM_Nmax on the simplex in dimension eight.  This is well above the
  -- dimensions PALP is ordinarily used in; the point of the test is that the
  -- message becomes an error rather than a confusing parse failure.
  V8 = id_(ZZ^8) | transpose matrix {toList(8 : -1)}
  assert(numRows palpMVertices V8 == 8)            -- the cheap calls are fine
  assert(numRows palpNVertices V8 == 8)
  ans = trap palpNormalForm V8 -- TODO: the error is missing the input!
  assert(ans#0 === null and instance(ans#1, Error))
  assert match("SYM_Nmax", toString ans#1)

  -- and POLY_Dmax is still reported, with advice to change palpDmax
  palpDmax = 6
  ans2 = trap palpNefPartitions({10,1,1,2,2,2,2}, 3)
  palpDmax = 11
  assert(ans2#0 === null and instance(ans2#1, Error))
  assert match("POLY_Dmax", toString ans2#1)
  assert match("palpDmax", toString ans2#1)
///

TEST ///
  -- Every wrapper keeps one point per column, in each dimension.  PALP is
  -- mainly used in low dimensions and is not much exercised above seven, so
  -- this stops at six; poly.x -p also slows sharply beyond that, taking three
  -- seconds in dimension eight against twenty milliseconds in dimension six.
  for d from 1 to 6 do (
      V := id_(ZZ^d) | transpose matrix {toList(d : -1)};
      assert(numRows palpMVertices V === d);
      assert(numRows palpMPoints V === d);
      assert(numRows palpNVertices V === d);
      assert palpIsReflexive V;
      assert(numRows palpNormalForm V === d);
      );
///

TEST ///
  -- Input PALP cannot handle is refused here rather than passed on.  See
  -- PALP-upstream-notes.md for what PALP does with each of these.
  debug PALPInterface

  -- not full dimensional: a square in the plane z = 0 of ZZ^3.  poly.x -v
  -- returns three of its four vertices, and poly.x -g exits on a bus error.
  sq = matrix{{1,-1,-1,1},{1,1,-1,-1},{0,0,0,0}}
  assert(4 == numColumns vertices convexHull sq)
  assert not palpFullDimensional sq
  for f in {palpMVertices, palpMPoints, palpNVertices, palpNPoints, palpInfo} do (
      r := trap f sq;
      assert(r#0 === null and instance(r#1, Error));
      assert match("span the lattice affinely", toString r#1);
      );
  assert(try (palpRun("poly", "-v", sq); false) else true)

  -- fewer points than the dimension cannot span it
  for M in {matrix{{1,-1},{0,0},{0,0}}, matrix{{0},{0},{0}}} do (
      r := trap palpMVertices M;
      assert(r#0 === null and instance(r#1, Error));
      assert match("more points than the dimension", toString r#1);
      );

  -- a weight system of dimension one makes PALP abort
  r = trap weightSystemVertices {2,1,1}
  assert(r#0 === null and instance(r#1, Error))
  assert match("dimension one", toString r#1)

  -- and the ordinary cases still go through
  assert palpFullDimensional matrix{{1,0,-1},{0,1,-1}}
  assert(numColumns palpMVertices {10,1,2,3,4} == 7)
///

"TEST"
/// -- checking nef partitions, codim = 3 or higher...
  -- Start with a weight system in dim 6.
  needsPackage "NormalToricVarieties"
  wsorig = (weightSystems(6, Degrees => {20,20}))_5; -- the _5 was chosen at random
  ws = {20, 1, 1, 2, 2, 2, 2, 10}
  ws = {20, 1, 1, 1, 1, 1, 6, 9}
  assert(ws === wsorig)
  verts = weightSystemVertices ws
  P = convexHull verts
  isReflexive P
  P' = polar P
  isReflexive P'
  V = normalToricVariety P
  V' = normalToricVariety P'
  assert isSimplicial V
  assert not isSimplicial V'
  rays V'
  Ds = for i from 0 to # rays V' - 1 list V'_i
  ds = select(drop(drop(subsets(#rays V), 1), -1), s -> s#0 == 0)
  Dlist = for d in ds list sum(d, i -> V_i)
  select(Dlist, d -> not isNef d)
  Ds/isNef


  Ds = for i from 0 to # rays V' - 1 list V'_i
  ds = select(drop(drop(subsets(#rays V'), 1), -1), s -> s#0 == 0);
  Dlist = for d in ds list sum(d, i -> V'_i);
  elapsedTime select(Dlist, d -> isNef d) -- slow?
  -- IN PROGRESS?? select(drop(subsets Ds, -1), d ->

  elapsedTime palpNefPartitions(ws, 2) -- very slow...!
  weightSystemVertices ws
  P'' = weightSystemPolytope ws -- very slow...!
  vertices P'' -- ouch!

  wsorig = weightSystems(6, Degrees => {20,20}); -- the _5 was chosen at random
  for i from 0 to 33 list wsorig#i => palpNefPartitions(wsorig#i, 3)
  for ws in wsorig list ws => palpNefPartitions(ws, 3)
  ws = last wsorig  
  verts = weightSystemVertices ws
  P = convexHull verts
  isReflexive P
  P' = polar P
  isReflexive P'
  vertices P'
  weightSystemPolytope ws
  V = normalToricVariety P
  V' = normalToricVariety P'
  assert isSimplicial V
  assert not isSimplicial V'

  Ds = for i from 0 to # rays V - 1 list V_i
  ds = select(drop(drop(subsets(#rays V), 1), -1), s -> s#0 == 0)
  ds = drop(drop(subsets(#rays V), 1), -1)
  Dlist = for d in ds list sum(d, i -> V_i)
  select(Dlist, d -> isNef d)
  select(oo, d -> isNef(-toricDivisor V - d))
  Ds/isNef
  palpNefPartitions(ws, 3)
  palpNefPartitions(ws, 2)

  palpNefPartitions(verts, 2)
  
///


"TEST"
/// -- checking nef partitions, codim = 3 in dim 5 (gives a surface, not 3-fold).
-- XXX
  needsPackage "NormalToricVarieties"
  wss = weightSystems(5, Degrees => {20,20}) -- 61 different ws's
  wssr = for ws in wss list (
      verts := weightSystemVertices ws;
      P := convexHull verts;
      if not isReflexive P then continue else ws) -- 41 of them give reflexive
  for ws in wssr list elapsedTime palpNefPartitions(ws, 2)  
  
  ws = {20, 1, 2, 2, 2, 3, 10}
  assert(ws == wsorig)

  verts = weightSystemVertices ws
  P = convexHull verts
  isReflexive P
  P' = polar P
  isReflexive P'
  V = normalToricVariety P
  V' = normalToricVariety P'
  assert isSimplicial V
  assert not isSimplicial V'
  rays V'
  Ds = for i from 0 to # rays V' - 1 list V'_i
  ds = select(drop(drop(subsets(#rays V), 1), -1), s -> s#0 == 0)
  Dlist = for d in ds list sum(d, i -> V_i)
  select(Dlist, d -> not isNef d)
  Ds/isNef


  Ds = for i from 0 to # rays V' - 1 list V'_i
  ds = select(drop(drop(subsets(#rays V'), 1), -1), s -> s#0 == 0);
  Dlist = for d in ds list sum(d, i -> V'_i);
  elapsedTime select(Dlist, d -> isNef d) -- slow?
  -- IN PROGRESS?? select(drop(subsets Ds, -1), d ->

  elapsedTime palpNefPartitions(ws, 2) -- very slow...!
  weightSystemVertices ws
  P'' = weightSystemPolytope ws -- very slow...!
  vertices P'' -- ouch!
///


TEST ///
  -- palpIsIP and palpIsCanonical, against Polyhedra
  needsPackage "Polyhedra"
  refl  = matrix{{1,0,0,-1},{0,1,0,-1},{0,0,1,-1}}      -- reflexive
  ipnr  = matrix{{2,0,0,-1},{0,1,0,-1},{0,0,1,-1}}      -- IP, (1,0,0) also interior
  canon = matrix{{1,0,0,-1},{0,1,0,-1},{0,0,1,-2}}      -- canonical, not reflexive
  vert0 = matrix{{0,1,0,0},{0,0,1,0},{0,0,0,1}}         -- origin is a vertex
  outside = matrix{{1,2,1,1},{0,0,1,0},{0,0,0,1}}       -- origin outside
  onfacet = matrix{{1,0,-1,0,0},{0,1,0,-1,0},{0,0,0,0,1}} -- origin on a facet
  origin = map(ZZ^3, ZZ^1, 0)
  for M in {refl, ipnr, canon, vert0, outside, onfacet} do (
      P := convexHull M;
      assert(palpIsIP M === inInterior(origin, P));
      assert(palpIsCanonical M === (inInterior(origin, P) and #interiorLatticePoints P == 1)));
  assert(palpIsIP ipnr and not palpIsCanonical ipnr and not palpIsReflexive ipnr)
  assert(palpIsCanonical canon and not palpIsReflexive canon)
  assert(not palpIsIP vert0 and not palpIsIP outside and not palpIsIP onfacet)
  -- QQ matrices with integer entries are accepted, as elsewhere
  assert palpIsCanonical vertices convexHull canon

  -- weight systems: PALP gives vertices even when q is not IP
  assert(palpIsIP {5,1,1,1,1,1} and palpIsCanonical {5,1,1,1,1,1})
  for q in {{9,1,1,2,5}, {8,1,1,3,3}, {7,1,2,4}} do (
      assert(weightSystemVertices q =!= null);
      assert(not palpIsIP q and not palpIsCanonical q);
      assert(not inInterior(map(ZZ^(#q - 2), ZZ^1, 0), convexHull weightSystemVertices q)));
  -- for a single weight system, IP means canonical; in dimension 5 not all are reflexive
  L = weightSystems(5, Degrees => 10)
  assert(all(L, palpIsIP) and all(L, palpIsCanonical))
  assert(#select(L, palpIsReflexive) < #L)
  -- a combined weight system
  assert palpIsCanonical {3,1,1,1,0,0,0, 3,0,0,0,1,1,1}
///

end--

-* Development section *-

restart
needsPackage "PALPInterface"
check "PALPInterface"

restart
uninstallPackage "PALPInterface"
restart
elapsedTime installPackage "PALPInterface"
elapsedTime check "PALPInterface"
viewHelp PALPInterface

-- Questions to check:
--  weight systems: do the integers need to be in monotone increasing order?
--    {q, n_0, ..., n_r}, sum(n_i) = q, and n_0 <= n_1 <= ... <= n_r.
--    ANSWER: the n_i do NOT need to be numbered.
--    TODO: check that the integers are >= 1, sum to 0th element?  Otherwise we get a strange error...
--    We could also insist that they are in order, but not sure we care?
ws1 = weightSystemVertices {10,1,2,3,4}
ws2 = weightSystemVertices {10,1,2,4,3}
assert(palpNormalForm ws1 == palpNormalForm ws2)
ws3 = weightSystemVertices {10,4,3,2,1}
assert(palpNormalForm ws1 == palpNormalForm ws3)
ws4 = weightSystemVertices {10,2,3,6,0,1}
weightSystemVertices {5,2,2,1}
weightSystemVertices {3,1,1,1}

-- TODO:
-- doc for weightSystems
--   The description is weak.  Define a weight system.  What is the degree vs weight of a weight system?
--     which partitions are removed?
--   The user doesn't know what cws.x -w is.  It is nice to keep it here, but should not be the lead sentence, and should
--     mention that this is a palp command.
--   For dim >= 4, I think we need to limit the degree range automatically.  I don't think it finishes otherwise (I cancelled it
--     after 100 seconds elapsed.
--     and it is annoyingly large in any case.
help weightSystems
elapsedTime weightSystems 3;
# elapsedTime weightSystems(4, Degrees => 30) == 137
# elapsedTime weightSystems(4, Degrees => 100) == 781

-- TODO: some doc in M2 for palp.
--   mention the executables used.
--   I believe ubuntu and debian include palp too?  (Not everyone is using a mac or homebrew).
--   mention how sometimes one needs to compile it yourself with larger bounds.
--     Johanna, Emmanuel had heuristics about when that is needed (I will find those notes).
--     in any case, if you need to recompile it, how to add it to programPaths? (OK, this should probably be in doc for "programPaths").
--     How to check what executable it found and is using?

-- TODO: add in interfaces to palp routines for finding fibrations.  Doc should say exactly what is it finding too.
--   First, tell me the interface functions, and we can brainstorm (plan) for what the names of the interface functions should be.

-- TODO: I'm not sure I understand the symmetry support in palp.  But we should,
--         and perhaps we want to wrap that functionality too.


needsPackage "PALPInterface"
palpProgram "cws"                        -- which executable was found: /opt/homebrew/bin/cws-11d.x
r = runProgram(palpProgram "cws", "-w3");
r#"command"                              -- "/opt/homebrew/bin/cws-11d.x -w3"
L = lines r#"output"                     -- {"4  1 1 1 1  rt", "5  1 1 1 2  rt", ...}
palpRun("poly", "-v", {10,1,2,3,4})      -- raw poly.x output for any polytope or weight system
palpVerbosity = 2                        -- then every call prints its command, input and output

elapsedTime r = runProgram(palpProgram "cws", "-w4");

echo '20 2 3 3 4 4 4' | nef-11d.x -f -D -P  -Lp -c2
echo '20 2 3 3 4 4 4' | nef-11d.x -f -D -P  -Lv -c2

toPalp palpNVertices {20, 2, 3, 3, 4, 4, 4}
toPalp palpNPoints {20, 2, 3, 3, 4, 4, 4}

-- Let's take the ws, and compute the N lattice polytope.  Use that as input to the nef code, with -N

echo '5 7
      2 0 0 0 0 -3 -2
      1 0 0 1 0 -3 -2
      0 0 1 0 0 -2 -1
      0 1 0 0 0 -2 -1
      0 0 0 0 1 -2 -1' | nef-11d.x -N -f -D -P -c2

-- now in M2 directly.      
nefx = palpProgram("nef-11d")
palpRun("nef", "-f -D -P -Lv -c2",  {20, 2, 3, 3, 4, 4, 4}, KeepFiles => true)
debug PALPInterface
methods runPALP
runPALP(nefx, "-D -P -Lv -c2",  toPalp {20, 2, 3, 3, 4, 4, 4}, KeepFiles => true)
runPALP(nefx, "-D -P -Lv -c2 -N", toPalp palpNVertices {20, 2, 3, 3, 4, 4, 4})
palpNVertices ws
palpNPoints ws

-- This is P^2 x P^1 x P^2
ws = {3, 1, 1, 1, 0, 0, 0, 0, 0,
    2, 0, 0, 0, 1, 1, 0, 0, 0,
    3, 0, 0, 0, 0, 0, 1, 1, 1}
runPALP(nefx, "-D -P -Lv -c2 -N", toPalp palpNVertices ws)
palpVerbosity = 1

needsPackage "PALPInterface"
--fromPalpMatrix "6 3 2 1 0 0 0 0 0 0 0 0  13 6 4 0 1 0 0 0 0 0 1 1  7 3 2 0 0 1 0 0 0 0 1 0  8 3 2 0 0 0 1 0 0 1 1 0  15 6 4 0 0 0 0 1 1 1 1 1"
cws = {6, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0,  13, 6, 4, 0, 1, 0, 0, 0, 0, 0, 1, 1,  7, 3, 2, 0, 0, 1, 0, 0, 0, 0, 1, 0,  8, 3, 2, 0, 0, 0, 1, 0, 0, 1, 1, 0,  15, 6, 4, 0, 0, 0, 0, 1, 1, 1, 1, 1}
ws = weightSystem cws
matrix toList ws
palpNVertices cws
palpMVertices cws
P = convexHull oo
isReflexive P

needsPackage "NormalToricVarieties"
V = normalToricVariety P
assert isFano V
assert not isSimplicial V
assert (isAmple (-toricDivisor V))
transpose matrix rays V

elapsedTime palpNefPartitions(cws, 2)
D1 = V_2 + V_6 + V_10
D2 = -toricDivisor V - D1
assert isNef D1
assert isNef D2
for i from 0 to 10 list isNef V_i

palpVerbosity = 4
elapsedTime palpNefPartitions(cws, 3)

-- Moved from the palpHodgeNumbers TEST (too slow for check):
-- MES tests
ws = {12,1,1,2,2,2,2,2}
L = palpNefPartitions(ws, 3, HodgeNumbers => false)
-- elapsedTime palpHodgeNumbers L#0 -- 41 sec, MES MBP M4
-- elapsedTime palpHodgeNumbers L#1 -- 9 sec, MES MBP M4
-- elapsedTime L1 = palpNefPartitions(ws, 3) -- computes Hodge numbers too, does it in 33.4 sec total.

-- MES: Claude, please check.
-- palpNefPartitions(palpNVertices ws, 3, HodgeNumbers => false)
-- palpNefPartitions(palpNPoints ws, 3, HodgeNumbers => false)

Ws = weightSystems(6, Degrees => 20, Reflexive => true);
Ns = for ws in Ws list (
    n := palpNefPartitions(ws, 3, HodgeNumbers => false);
    if n === null or #n === 0 then continue;
    n)
for i from 0 to #Ws-1 list (
    n := palpNefPartitions(Ws#i, 3, HodgeNumbers => false);
    if n === null or #n === 0 then continue;
    i => n)
-- for n in flatten Ns list elapsedTime palpHodgeNumbers n  -- very slow, it appears....!
-- #0: 239 sec
-- #1: 157 sec
-- #2, #3, #4: 7.7, 9.7, 12.5 sec
-- #5: 93 sec

