
-- HookPieri.m2
-- A Macaulay2 package for Pieri maps, straightening, and finite hook algebras

newPackage(
    "HookPieri",
    Version => "1.0",
    Date => "2026-10-08",
    Authors => {
        { Name => "Keller VandeBogert",
          Email => "keller.v@uky.edu",
          HomePage => "https://sites.google.com/view/kellervandebogert/home" }
    },
    HomePage => "https://sites.google.com/view/kellervandebogert/home",
    Headline => "Pieri maps, straightening, and finite hook algebras",
    Keywords => {"Schur functors","Pieri maps","Young tableaux","Hooks","Representation theory","Groebner bases","Segre products"},
    PackageImports => {"PieriMaps", "SchurRings", "SchurFunctors", "Complexes"},
    AuxiliaryFiles => true,
    DebuggingMode => false
);
export {
    "Content",
    "Progress",
    "Colors",
    "Truncation",
    "KeepInternalDegree",
    "VerificationReport",
    "HookTabloid",
    "tabloid",
    "youngTex",
    "hookDecomposition",
    "frobeniusUnion",
    "frobeniusUnionMatrix",
    "verifyGarnirLift",
    "PoincareSeries",
    "EquivariantBettiTally",
    "genericDetPoincare",
    "symDetPoincare",
    "pfaffianPoincare",
    "symDetCoverPoincare",
    "expandPoincare",
    "dimensionSpecialization",
    "ordinaryPoincareSeries",
    "strandBettiTable",
    "equivariantStrandBettiTable",
    "totalBettiNumbers",
    "poincareSeriesTruncation",
    "determinantalRing",
    "isGolodPoincare",
    "koszulPoincareCheck",
    "verifyPoincareSeries",
    "verifyStrandBettiTable",
    "hookBasis",
    "pieriRowRemove",
    "juxtaposeSymToRow",
    "pieriColumnAdd",
    "pieriMatrix",
    "pieriSourceBasis",
    "pieriTargetBasis",
    "pieriImage",
    "pieriPreimage",
    "hookSymbolicForm",
    "hookStraighten",
    "straighteningStep",
    "straighteningObstruction",
    "isStepTriangular",
    "verifyTriangularity",
    "verifyStraighteningAgainstCokernel",
    "isHookSemistandard",
    "frobeniusRank",
    "hookTabloids",
    "runningVectors",
    "runningIndices",
    "runningCompare",
    "Obstruction",
    "EngineStep",
    "hookRing",
    "straighteningIdeal",
    "pieriIdeal",
    "nonstandardMonomialIdeal",
    "weightTruncationIdeal",
    "hasQuadraticGroebnerBasis",
    "paperOrder",
    "verifyPaperOrder",
    "MaxArm",
    "MaxLeg",
    "Weight",
    "VariableLimit",
    "Force",
    "RelationLimit",
    "Validate",
    "ComparisonLimit",
    "fineSegreRing",
    "fineSegreAlgebra",
    "fineSegreEstimate",
    "fineSegreMap",
    "factors",
    "TypePairs",
    "LeftWeight",
    "RightWeight",
    "hookAlgebra",
    "hookTypes",
    "alphabetSize",
    "weightBound",
    "hookVariable",
    "hookMonomial",
    "tabloidTerms",
    "hookRelation",
    "relationKind",
};


-- Option names of internal (unexported) elimination-matrix builders.
-- Macaulay2 requires option symbols to be declared; protecting them keeps
-- them out of the user-facing API.
protect BaseAlpha; protect Pivot; protect BadSet;
protect BaseSymMon; protect FixedRow; protect KeyBase;

------------------------------------------------------------------------
-- Coefficient field.
--
-- Every straightening, Pieri, and presentation computation is carried out
-- over the current coefficient field hpField(), which is QQ by default.
-- A producer takes the option CoefficientRing => K, with K equal to QQ or
-- a field of positive characteristic (for instance ZZ/7), validates it
-- with hpValidateCoefficientRing and runs its body inside
-- hpWithField(K, () -> ...), which makes K the current field for the
-- duration of the call and restores the previous field afterwards;
-- functions on a ring, ideal or matrix read the field from the object.
-- The paper's formulas divide by the integers a+b-m, a+b, a+b+m and solve
-- the row and column elimination systems of Section 5; in characteristic
-- p these steps can fail, and when they do the package raises an error
-- that names the offending integer or system instead of returning a wrong
-- answer.  The paper proves that no failure occurs when char k = 0 or
-- char k > |lambda| for the shape lambda being straightened.
--
-- The nested-hook composition (PieriMaps) and the Poincare series
-- (SchurRings) remain characteristic-zero computations.
------------------------------------------------------------------------
-- The current field lives in a slot of a mutable hash table rather than
-- in a global variable: assigning a ring to a global variable makes
-- Macaulay2 display an anonymous ring (such as ZZ/7 passed directly) under
-- that variable's name.
hpState = new MutableHashTable from {"field" => QQ, "progress" => 0};
hpField = () -> hpState#"field";
hpLastError = null;
hpFail = msg -> (hpLastError = msg; error msg);
hpIsAllowedField = K -> K === QQ or
    (instance(K, Ring) and isField K and char K > 0);
-- the size guard of the presentations: count items must not exceed the
-- RelationLimit option unless Force is set
hpRelationLimitCheck = (caller, count, noun, limit, force) ->
    if count > limit and not force then hpFail(caller | ": " | toString count | " " |
        noun | " exceed RelationLimit=" | toString limit |
        "; rerun with RelationLimit=>N or Force=>true");
-- the inverse of an integer in the current field, or a clear failure that
-- names the integer and, when given, the map or system that needs it
hpUnitAt = (d, context) -> (
    kk := hpField();
    x := d_kk;
    if x == 0 then hpFail("HookPieri: " | context | " divides by " | toString d |
        ", which is not invertible in characteristic " | toString char kk |
        "; the paper guarantees success when the characteristic is 0 or exceeds" |
        " the number of boxes of the shape being straightened, so choose a" |
        " larger prime with CoefficientRing => ZZ/p");
    1/x
);
-- the canonical-relabel caches are kept separately for each field
hpFieldCache = (cache, kk) -> (
    if not cache#?kk then cache#kk = new MutableHashTable;
    cache#kk
);
-- the setter of the current field: private, used by hpWithField; the
-- public interface has no global setter, only the CoefficientRing option
hpSetCoefficientRing = K -> (
    if not hpIsAllowedField K then
        error "HookPieri: expected QQ or a field of positive characteristic";
    hpState#"field" = K;
    K
);
-- the value of a CoefficientRing option, validated on behalf of caller
hpValidateCoefficientRing = (caller, K) -> (
    if not hpIsAllowedField K then
        error(caller | ": CoefficientRing must be QQ or a field of positive" |
            " characteristic (for instance ZZ/7), not " | toString K);
    K
);
-- run f() with the field temporarily set to K, restoring it afterwards
-- (also on failure, re-raising the package's own message)
hpWithField = (K, f) -> (
    if not hpIsAllowedField K then
        error "HookPieri: expected QQ or a field of positive characteristic";
    old := hpField();
    hpState#"field" = K;
    hpLastError = null;
    result := try f() else (
        hpState#"field" = old;
        error(if hpLastError =!= null then hpLastError
            else "HookPieri: the computation over " | toString K |
                " failed with an error that the package did not record;" |
                " after debug HookPieri, set the field with hpSetCoefficientRing K" |
                " and rerun to see the original message");
    );
    hpState#"field" = old;
    result
);

------------------------------------------------------------
-- General utilities: lists, partitions, sequences, signs, hash tables, and
-- the paging of tables
------------------------------------------------------------

-- a list from a list, sequence, or mutable list (null stays null)
hpAsList = L -> (
    if L === null then null
    else if instance(L, List) then L
    else if instance(L, MutableList) then toList L
    else toList L
);

hpConjugatePartition = lam -> (
    lam = hpAsList lam;
    if #lam == 0 then return {};
    m := max lam;
    for j from 1 to m list #select(lam, r -> r >= j)
);

hpCartesianProductLists = Ls -> (
    Ls = hpAsList Ls;
    if #Ls == 0 then return {{}};
    firstList := Ls#0;
    restProd := hpCartesianProductLists(drop(Ls,1));
    out := {};
    for x in firstList do (
        for rest in restProd do (
            out = out | {{x} | rest};
        );
    );
    out
);

-- Frobenius rank (number of diagonal hooks) of a partition, unchecked
hpFrobeniusRank = lambda -> (
    lambda = toList lambda;
    r := 0;
    for i from 0 to #lambda-1 do (if lambda#i > i then r = r + 1 else break);
    r
);

-- remove ONE occurrence of x from sorted list L
hpRemoveOne = (L,x) -> (
    p := position(L, y -> y==x);
    if p === null then error("hpRemoveOne: " | toString x | " not found in " | toString L);
    take(L,p) | drop(L,p+1)
);

-- counts in list L
hpCountHT = L -> (
    H := new MutableHashTable;
    for x in L do H#x = (if H#?x then H#x else 0) + 1;
    H
);

hpSetMinusList = (L,S) -> (
    HS := new MutableHashTable;
    for x in S do HS#x = true;
    out := {};
    for x in L do if not HS#?x then out = out | {x};
    out
);

------------------------------------------------------------
-- Multiset difference for lists (with multiplicity)
-- returns sorted(A \ B) assuming B is a submultiset of A
------------------------------------------------------------
hpMultisetDiff = (A,B) -> (
    ct := new MutableHashTable;
    for x in A do ct#x = (if ct#?x then ct#x else 0) + 1;
    for y in B do (
        if not ct#?y then error "hpMultisetDiff: B not submultiset of A (missing key).";
        ct#y = ct#y - 1;
        if ct#y < 0 then error "hpMultisetDiff: B not submultiset of A (negative count).";
    );
    out := {};
    for k in keys ct do (
        for t from 1 to ct#k do out = out | {k};
    );
    sort out
);

hpStrictSeqs = (k, lo, hi) -> (
    if k == 0 then return {{}};
    flatten for x from lo to hi-k+1 list (
        for rest in hpStrictSeqs(k-1, x+1, hi) list prepend(x, rest)
    )
);

hpWeakSeqs = (k, lo, hi) -> (
    if k == 0 then return {{}};
    flatten for x from lo to hi list (
        for rest in hpWeakSeqs(k-1, x, hi) list prepend(x, rest)
    )
);

hpInvSign = L -> (
    n := #L;
    parity := 0;
    for p from 0 to n-2 do (
        for q from p+1 to n-1 do (
            if L#p > L#q then parity = 1 - parity
        )
    );
    if parity == 0 then 1 else -1
);

hpStdBasisVec = (N, idx) -> (
    v := mutableMatrix((hpField()), N, 1);
    v_(idx,0) = 1;
    matrix v
);

hpLexCmp = (A,B) -> (
    L := if #A < #B then #A else #B;
    for k from 0 to L-1 do (
        if A#k < B#k then return -1;
        if A#k > B#k then return  1;
    );
    if #A < #B then -1 else if #A > #B then 1 else 0
);

hpLexGE = (A,B) -> (hpLexCmp(A,B) >= 0);

hpLexGT = (A,B) -> (hpLexCmp(A,B) == 1);

hpBetaMin = (rowTail,m) -> (
    rt := sort rowTail;
    reverse take(reverse rt, m)
);

-- numeric lex key for a list (preserves lex order if B > max entry)
hpLexKey = (L,B) -> (
    key := 0;
    for x in L do key = key*B + x;
    key
);

-- add a coefficient to a hash table of terms, dropping the key when the
-- coefficient cancels
hpAddTerm = (H, key, coeff) -> (
    if coeff == 0 then return H;
    if H#?key then H#key = H#key + coeff else H#key = coeff;
    if H#key == 0 then remove(H, key);
    H
);

hpFreezeHT = H -> hashTable apply(keys H, k -> (k => H#k));

hpRemoveAtPos = (L, i) -> (
    take(L, i) | drop(L, i+1)
);

hpShapeOfTabloid = T -> apply(T, row -> #row);

hpMaxEntryTabloid = T -> (
    if T == {} then return 0;
    max flatten T
);

------------------------------------------------------------------------
-- VerificationReport: the value of the verifications that count what
-- they check (verifyTriangularity, verifyStraighteningAgainstCokernel,
-- verifyGarnirLift, verifyPaperOrder).  A report is a HashTable with
-- string keys.  Every report has "kind" (a String naming the
-- verification), "passed" (the verdict, a Boolean) and "keyOrder" (the
-- list of its rows in display order); the constructing function supplies
-- the other rows, in the order it wants them shown, through the private
-- constructor hpVerificationReport(kind, orderedPairs).  The net is the
-- two-column table of the rows in that order with "passed" and, when
-- present, the "conclusion" row last; toString is short.
------------------------------------------------------------------------

VerificationReport = new Type of HashTable;
VerificationReport.synonym = "verification report";

-- orderedPairs is a list of pairs "row" => value; the row "passed" is
-- required and Boolean, the row "conclusion" is optional and a String
hpVerificationReport = (kind, orderedPairs) -> (
    if not instance(kind, String) then
        error "hpVerificationReport: expected a String naming the kind of verification";
    if not instance(orderedPairs, List) or
       any(orderedPairs, p -> not instance(p, Option) or not instance(p#0, String)) then
        error "hpVerificationReport: expected a list of pairs \"row\" => value";
    rowNames := apply(orderedPairs, p -> p#0);
    if #rowNames != #unique rowNames then
        error "hpVerificationReport: a row name is repeated";
    if member("kind", rowNames) or member("keyOrder", rowNames) then
        error "hpVerificationReport: the row names kind and keyOrder are reserved";
    if not member("passed", rowNames) then
        error "hpVerificationReport: a row \"passed\" => Boolean is required";
    H := hashTable orderedPairs;
    if not instance(H#"passed", Boolean) then
        error "hpVerificationReport: the row passed must be true or false";
    if H#?"conclusion" and not instance(H#"conclusion", String) then
        error "hpVerificationReport: the row conclusion must be a String";
    order := select(rowNames, k -> k =!= "passed" and k =!= "conclusion") | {"passed"} |
        (if H#?"conclusion" then {"conclusion"} else {});
    new VerificationReport from (orderedPairs | {"kind" => kind, "keyOrder" => order})
);

-- the cell of one row of the table: a flat list (numbers, strings,
-- Booleans, symbols, ring elements) prints as a list, a short list of
-- compound entries prints its entries side by side, and a long list or
-- hash table prints as a count; the value itself stays under its key
hpReportFlatList = v -> all(v, x -> x === null or instance(x, ZZ) or instance(x, QQ) or
    instance(x, RingElement) or instance(x, String) or instance(x, Boolean) or
    instance(x, Symbol));
hpReportCell = v -> (
    if v === null then net "none"
    else if instance(v, VerificationReport) then net v
    else if instance(v, List) or instance(v, Sequence) then (
        if #v == 0 then net "none"
        else if hpReportFlatList v then net v
        else if #v <= 4 then horizontalJoin between("  ", toList apply(v, hpReportCell))
        else net(toString(#v) | " entries"))
    else if instance(v, HashTable) then (
        if #v <= 4 then net v else net(toString(#v) | " entries"))
    else if instance(v, String) and #v > 72 then wrap(72, v)
    else net v
);
net VerificationReport := R -> netList apply(R#"keyOrder", k -> {k, hpReportCell R#k});
toString VerificationReport := R -> "VerificationReport{kind => " | R#"kind" |
    ", passed => " | toString R#"passed" | "}";

------------------------------------------------------------------------
-- HookTabloid: a hook tabloid, the filling of a partition shape read
-- through its diagonal hooks (Section 3 of the paper).  The entries of
-- the BasicList are the rows, top row first, so toList T recovers them;
-- the type carries no cache.
--   tabloid rows                 the tabloid with these rows
--   tabloid T, tabloid F         a HookTabloid (unchanged) or a Filling of
--                                SchurFunctors (stored by columns)
--   tabloid(rowTail, colFull)    the hook shaped tabloid with the given arm
--                                and full column (the native pair of the
--                                engines: the row without its corner, then
--                                the column including the corner)
--   tabloid(outer, inner)        the tabloid of Frobenius rank two with the
--                                hook shaped tabloids outer and inner on
--                                its two diagonal hooks
--   hookDecomposition T          the diagonal hooks of T, top left first,
--                                as hook shaped HookTabloids
--   youngTex(T, Colors => fills) the KaTeX source of the boxed tableau,
--                                optionally with a background color per
--                                cell (the paper colors the diagonal hooks)
-- Display: net draws the boxes, toString and toExternalString give the
-- re-readable form tabloid {rows}, expression is that function
-- application, tex is youngTex.  Two tabloids are == when their rows
-- agree; ? is the total order by shape, then by rows; new Filling from T
-- converts to a Filling.  Section 09 installs tabloid RingElement.
------------------------------------------------------------------------

HookTabloid = new Type of BasicList;
HookTabloid.synonym = "hook tabloid";

-- the rows of a tabloid: nonempty lists whose lengths form a partition
-- (the entries are not inspected; the Garnir tester uses the alphabet
-- 0..n-1)
hpValidateTabloidRows = T -> (
    if not instance(T,List) or any(T,row -> not instance(row,List)) then
        error "tabloid: expected a list of rows";
    if any(T,row -> #row == 0) then
        error "tabloid: rows must be nonempty";
    lengths := apply(T,row -> #row);
    if #lengths > 1 and
       any(1..#lengths-1,i -> lengths#(i-1) < lengths#i) then
        error "tabloid: row lengths must form a partition";
    T
);

-- the rows of the hook shaped tabloid with arm rowTail and full column
-- colFull
hpNativeToRows = (caller, rowTail, colFull) -> (
    if not instance(rowTail, List) or not instance(colFull, List) then
        error(caller | ": expected two lists, the row tail and the full column");
    if #colFull == 0 then
        error(caller | ": the full column must contain at least the corner");
    if any(rowTail | colFull, x -> instance(x, BasicList)) then
        error(caller | ": the row tail and the full column are lists of entries," |
            " not lists of rows");
    {{colFull#0} | rowTail} | apply(drop(colFull, 1), x -> {x})
);

-- the native pair {rowTail, colFull} of a hook shaped tabloid
hpHookToNative = (caller, T) -> (
    rows := toList T;
    if #rows == 0 or any(drop(rows, 1), r -> #r != 1) then
        error(caller | ": expected a hook shaped tabloid (one row and one column)");
    {drop(rows#0, 1), apply(rows, r -> r#0)}
);

-- the diagonal hooks of a tabloid given by rows, top left first, as native
-- pairs (the generators of a hook ring are indexed by these pairs)
hpDiagonalHooks = rows -> (
    lambda := apply(rows, r -> #r);
    apply(hpFrobeniusRank lambda, k -> toList hpHookOfTabloid(rows, lambda, k))
);

tabloid = method(TypicalValue => HookTabloid);
tabloid List := T -> new HookTabloid from hpValidateTabloidRows T;
tabloid HookTabloid := T -> T;
tabloid Filling := F -> tabloid hpFillingToRows("tabloid", F);
tabloid(List, List) := (rowTail, colFull) ->
    tabloid hpNativeToRows("tabloid", rowTail, colFull);
-- the nesting of an outer hook of type (a,b) around an inner hook of type
-- (c,d) with a > c and b > d: the outer hook fills the first row and
-- column, the inner hook the second row and column
tabloid(HookTabloid, HookTabloid) := (outer, inner) -> (
    outerPair := hpHookToNative("tabloid", outer);
    innerPair := hpHookToNative("tabloid", inner);
    arms := {#(outerPair#0), #(innerPair#0)};
    legs := {#(outerPair#1) - 1, #(innerPair#1) - 1};
    if arms#0 <= arms#1 or legs#0 <= legs#1 then
        error("tabloid: the outer hook needs a longer arm and a longer leg than" |
            " the inner hook (arms " | toString arms | ", legs " | toString legs | ")");
    tabloid hpAssembleHookTabloid(nhpFrobToPart(arms, legs), {outerPair, innerPair})
);

-- the display stack: expression, net (the compact grid of a Filling), toString and
-- toExternalString (re-readable), tex (below, after youngTex)
expression HookTabloid := T -> (expression tabloid) (expression toList T);
hpTabPad = (s,w) -> concatenate((w-#s):" ") | s;
net HookTabloid := T -> (
    rows := toList T;
    if #rows == 0 then return net "()";
    -- the grid of SchurFunctors' Filling: the columns, each stacked, in one
    -- boxed row, so that a tabloid prints like a Filling
    ncols := max apply(rows, r -> #r);
    cols := apply(ncols, j -> for r in rows list (if j < #r then net r#j else continue));
    netList {apply(cols, c -> stack c)}
);
toString HookTabloid := T -> toString expression T;
toExternalString HookTabloid := T -> toString expression T;

-- comparison: equality by rows; a total order by shape (as lists), then
-- by rows, so sort works on lists of tabloids
HookTabloid == HookTabloid := (T, U) -> toList T == toList U;
HookTabloid ? HookTabloid := (T, U) -> (
    shapes := apply(toList T, r -> #r) ? apply(toList U, r -> #r);
    if shapes =!= symbol == then shapes else toList T ? toList U
);

-- conversion to the column stored Filling of SchurFunctors
new Filling from HookTabloid := (FillingType, T) -> hpRowsToFilling("tabloid", toList T);

-- the diagonal hooks as hook shaped tabloids
hookDecomposition = method(TypicalValue => List);
hookDecomposition HookTabloid := T ->
    apply(hpDiagonalHooks toList T, h -> tabloid(h#0, h#1));
hookDecomposition List := T -> hookDecomposition tabloid T;
hookDecomposition Filling := F -> hookDecomposition tabloid F;

-- the KaTeX source of the boxed tableau; Colors gives one background color
-- string per cell, as a list of rows, and defaults to white
hpTexCell = (x, fill) -> "\\fcolorbox{black}{" | fill | "}{\\phantom{|}" | toString x | "\\phantom{|}}";
youngTex = method(Options => {Colors => null}, TypicalValue => String);
youngTex List := opts -> T -> youngTex(tabloid T, opts);
youngTex HookTabloid := opts -> T -> (
    rows := toList T;
    fills := if opts#Colors === null then apply(rows, r -> apply(r, x -> "white")) else (
        if not instance(opts#Colors,List) or #opts#Colors != #rows or
           any(#rows,i -> not instance(opts#Colors#i,List) or
            #(opts#Colors#i) != #(rows#i) or
            any(opts#Colors#i,c -> not instance(c,String))) then
            error "youngTex: Colors must give one color string for each tableau cell";
        opts#Colors
    );
    "\\begin{array}{l}" |
      concatenate between("\\\\[-2pt]",
        apply(#rows, i -> concatenate between("\\!",
        apply(#(rows#i), j -> hpTexCell((rows#i)#j, (fills#i)#j))))) |
      "\\end{array}");
youngTex Filling := opts -> T -> youngTex(tabloid T, opts);
tex HookTabloid := T -> youngTex T;

------------------------------------------------------------------------
-- The Frobenius union projection and the Garnir lift (characteristic 0)
--
--   frobeniusUnion(T1, T2, n)          the image of z_T1 (x) z_T2 under the
--                                      projection S_(a,1^b) (x) S_(c,1^d)
--                                      -> S_nest onto the Schur functor of
--                                      the Frobenius union of the two
--                                      hooks: HashTable HookTabloid => QQ
--   frobeniusUnionMatrix(outer, inner, n)  its matrix in the semistandard
--                                      bases, with "sourceBasis" and
--                                      "targetBasis" in the cache
--   verifyGarnirLift(outer, inner, n)  the test of the naive Garnir lift
--                                      against the projection on every
--                                      tabloid of the nest shape, as a
--                                      VerificationReport of kind
--                                      "GarnirLift"
-- The projection is the composite of three maps of the package PieriMaps,
--   (1) pieri (column strip):        S_(a,1^b) --> Wedge^(b+1) (x) Sym^(a-1)
--   (2) dualPieri (row Pieri):       Sym^(a-1) (x) S_(c,1^d) --> S_(a-1, c, 1^d)
--   (3) dualPieriColumn:             Wedge^(b+1) (x) S_(a-1, c, 1^d) --> S_nest
-- and inherits their normalization.  The engines work on the Filling
-- convention of PieriMaps: tableaux are lists of columns with entries in
-- 0..n-1, a hook of shape (a,1^b) being the full first column followed by
-- a-1 columns of one entry.  The public layer converts through the
-- HookTabloid rows.  Hook types are the paper's {arm, leg}; the first
-- row lengths a = arm + 1 and c = arm + 1 index the engines.
------------------------------------------------------------------------

-- the hook shape (a, 1^b)
nhCompHookShape = (a, b) -> if b == 0 then {a} else prepend(a, toList(b:1));

-- the intermediate shape (a-1, c, 1^d)
nhCompInterShape = (a, c, d) ->
    if d == 0 then {a-1, c} else flatten {{a-1, c}, toList(d:1)};

-- the nest shape (a, c+1, 2^d, 1^{b-d-1}), the Frobenius union of the hooks
nhCompNestShape = (a, b, c, d) -> select(
    flatten {{a, c+1}, toList(d:2), toList(b-d-1:1)},
    r -> r > 0
);

-- the Filling basis of S_lambda(V): standardTableaux uses the conjugate
-- shape convention
nhStdTabsFilling = (n, lambda) -> standardTableaux(n, hpConjugatePartition lambda);

-- the wedge monomial e_{i_1} ^ ... ^ e_{i_k} of a column tableau tLam
-- (shape (1^k); standardTableaux stores it as k rows of length 1)
nhWedgeMono = (tLam, W) -> (
    entries := flatten toList tLam;
    product for e in entries list W_e
);

-- the polynomial ring and the exterior algebra of the PieriMaps
-- evaluations depend only on n; they are built once per alphabet size
nhCompRingCache = new MutableHashTable;
nhCompRings = n -> (
    if not nhCompRingCache#?n then nhCompRingCache#n = (
        QQ[apply(n, i -> getSymbol("nhx" | toString(i+1)))],
        QQ[apply(n, i -> getSymbol("nhw" | toString(i+1))), SkewCommutative => true]);
    nhCompRingCache#n
);

nhValidateNestedHookParameters = (a,b,c,d,n,caller) -> (
    if not all({a,b,c,d,n}, x -> instance(x,ZZ)) then
        error(caller | ": a, b, c, d, and n must be integers");
    if n < 0 then error(caller | ": n must be nonnegative");
    if not (a >= 2 and b >= 1 and c >= 1 and d >= 0) then
        error(caller | ": need a >= 2, b >= 1, c >= 1, d >= 0");
    if not (a > c and b > d) then
        error(caller | ": not nestable; need a > c and b > d");
);

------------------------------------------------------------------------
-- The engine: the image of z_T1 (x) z_T2 as a list of (coefficient,
-- nest tableau) pairs, the tableaux in the column list form
------------------------------------------------------------------------

nhApplyNestedHookComp = (T1, T2, a, b, c, d, n) -> (
    nhValidateNestedHookParameters(a,b,c,d,n,"frobeniusUnion");

    validHookFilling := (T,u,v) -> (
        instance(T,List) and #T == u and
        all(#T, j -> instance(T#j,List) and
            #(T#j) == (if j == 0 then v+1 else 1)) and
        all(flatten T, x -> instance(x,ZZ))
    );
    if not validHookFilling(T1,a,b) then
        error "frobeniusUnion: T1 must have hook shape (a,1^b) in column list form";
    if not validHookFilling(T2,c,d) then
        error "frobeniusUnion: T2 must have hook shape (c,1^d) in column list form";
    if any(flatten(T1|T2), x -> x < 0 or x >= n) then return {};

    conv := "Filling";
    (P, W) := nhCompRings n;

    muO := nhCompHookShape(a, b);
    interSh := nhCompInterShape(a, c, d);
    nest := nhCompNestShape(a, b, c, d);

    -- step 1: applyPieri returns a list of {poly, T_lambda} pairs
    step1Result := applyPieri((muO, toList(a-1:1)), T1, P, Convention => conv);

    boxes2 := flatten {toList(a-1-c:1), toList(c-1:2), {d+2}};
    cols3  := flatten {{a, c+1}, toList(d:2), toList(b-d-1:1)};

    nestAcc := new MutableHashTable;

    for term1 in step1Result do (
        poly := term1#0;
        tLam := term1#1;
        wedgeMono := nhWedgeMono(tLam, W);

        step2Result := applyDualPieri(
            (interSh, boxes2), poly, T2, n,
            Convention => conv);

        for s in step2Result do (
            cInter := s#0;
            tInter := s#1;

            step3Result := applyDualPieriColumn(
                (nest, cols3), wedgeMono, tInter, n,
                Convention => conv);

            for s3 in step3Result do (
                cNest := s3#0;
                tNest := s3#1;
                totalCoeff := cInter * cNest;
                keyNest := toList tNest;
                if nestAcc#?keyNest then
                    nestAcc#keyNest = nestAcc#keyNest + totalCoeff
                else
                    nestAcc#keyNest = totalCoeff;
            );
        );
    );

    for k in keys nestAcc list if nestAcc#k != 0 then (nestAcc#k, k) else continue
);

------------------------------------------------------------------------
-- Conversions between the column list form of the engines and
-- HookTabloids
------------------------------------------------------------------------

-- the column list of a hook argument (a HookTabloid, a list of rows or a
-- Filling of hook shape with integer entries) with its first row length
-- and leg length
nhHookColumns = (caller, T) -> (
    U := if instance(T, HookTabloid) then T
        else if instance(T, List) or instance(T, Filling) then tabloid T
        else error(caller | ": expected a hook shaped tabloid (a HookTabloid," |
            " a list of rows or a Filling)");
    p := hpHookToNative(caller, U);
    if any(p#0 | p#1, x -> not instance(x, ZZ)) then
        error(caller | ": the entries of the hook must be integers");
    ({p#1} | apply(p#0, x -> {x}), #(p#0) + 1, #(p#1) - 1)
);

-- the HookTabloid of a tableau in column list form
nhColumnsToTabloid = cols -> tabloid hpFillingToRows("frobeniusUnion", new Filling from cols);

-- the HashTable HookTabloid => QQ of an engine term list
nhTermsToTable = terms -> hashTable apply(terms, t -> nhColumnsToTabloid t#1 => t#0);

------------------------------------------------------------------------
-- frobeniusUnion(T1, T2, n): the image of z_T1 (x) z_T2, with T1 the
-- outer hook (a,1^b) and T2 the inner hook (c,1^d), a > c and b > d, on
-- the alphabet 0..n-1
------------------------------------------------------------------------

frobeniusUnion = method(TypicalValue => HashTable);
frobeniusUnion(HookTabloid, HookTabloid, ZZ) := (T1, T2, n) -> (
    (cols1, a, b) := nhHookColumns("frobeniusUnion", T1);
    (cols2, c, d) := nhHookColumns("frobeniusUnion", T2);
    nhValidateNestedHookParameters(a, b, c, d, n, "frobeniusUnion");
    nhTermsToTable nhApplyNestedHookComp(cols1, cols2, a, b, c, d, n)
);
frobeniusUnion(List, List, ZZ) := (T1, T2, n) -> frobeniusUnion(tabloid T1, tabloid T2, n);
frobeniusUnion(Filling, Filling, ZZ) := (T1, T2, n) -> frobeniusUnion(tabloid T1, tabloid T2, n);

------------------------------------------------------------------------
-- frobeniusUnionMatrix(outerType, innerType, n): the matrix of the
-- projection over QQ; rows are indexed by the semistandard tableaux of
-- the nest shape, columns by the pairs (outer tableau, inner tableau) of
-- semistandard hook tableaux, the outer index slow, all in the order of
-- standardTableaux of PieriMaps and stored as HookTabloids in the cache
------------------------------------------------------------------------

frobeniusUnionMatrix = method(TypicalValue => Matrix);
frobeniusUnionMatrix(List, List, ZZ) := (outerType, innerType, n) -> (
    hpValidateTypePair("frobeniusUnionMatrix", outerType, innerType);
    (a, b, c, d) := (outerType#0 + 1, outerType#1, innerType#0 + 1, innerType#1);
    nhValidateNestedHookParameters(a, b, c, d, n, "frobeniusUnionMatrix");
    muO := nhCompHookShape(a, b);
    muI := nhCompHookShape(c, d);
    nest := nhCompNestShape(a, b, c, d);
    stdsO := apply(nhStdTabsFilling(n, muO), toList);
    stdsI := apply(nhStdTabsFilling(n, muI), toList);
    stdsN := apply(nhStdTabsFilling(n, nest), toList);
    nestIdx := hashTable for i from 0 to #stdsN-1 list stdsN#i => i;
    dimOut := #stdsN;
    domPairs := flatten for T1 in stdsO list for T2 in stdsI list {T1, T2};
    colVecs := for pair in domPairs list (
        terms := nhApplyNestedHookComp(pair#0, pair#1, a, b, c, d, n);
        colVec := new MutableList from for j from 0 to dimOut - 1 list 0_QQ;
        for term in terms do (
            k := term#1;
            if nestIdx#?k then colVec#(nestIdx#k) = term#0;
        );
        toList colVec
    );
    M := if dimOut == 0 or #domPairs == 0 then map(QQ^dimOut, QQ^(#domPairs), 0)
        else transpose matrix(QQ, colVecs);
    M.cache#"sourceBasis" = apply(domPairs, pair -> apply(pair, nhColumnsToTabloid));
    M.cache#"targetBasis" = apply(stdsN, nhColumnsToTabloid);
    M
);

------------------------------------------------------------------------
-- The naive Garnir lift.  A tabloid T of the nest shape decomposes into
-- its outer hook T1 of shape (a,1^b) and its inner hook T2 of shape
-- (c,1^d), its two diagonal hooks (hookDecomposition); the naive lift
-- sends T to z_T1 (x) z_T2.  verifyGarnirLift tests, for every tabloid T
-- of the nest shape with entries in 0..n-1, whether the projection of the
-- lift is c times the straightening of T in the semistandard basis of
-- S_nest, where the scalar c is read off a reference semistandard
-- tableau.
------------------------------------------------------------------------

-- the outer and inner hook of a nest tabloid in column list form: the
-- nest shape has the column heights b+1 (the outer leg and corner), d+2
-- (the outer leg cap, the inner corner and leg), 2 in the columns 3 to
-- c+1 and 1 thereafter; this is the diagonal hook decomposition of the
-- tabloid
nhDecomposeNestColumns = (T, a, b, c, d) -> (
    Tlist := toList T;
    T1cols := {toList(Tlist#0)} | for j from 1 to a-1 list {(Tlist#j)#0};
    T2col1 := for i from 1 to d+1 list (Tlist#1)#i;
    T2cols := if c == 1 then {T2col1}
            else {T2col1} | for j from 2 to c list {(Tlist#j)#1};
    (T1cols, T2cols)
);

-- all length h tuples with entries in 0..n-1
nhAllCols = (h, n) -> (
    if h == 0 then return {{}};
    rest := nhAllCols(h-1, n);
    flatten for v from 0 to n-1 list for r in rest list prepend(v, r)
);

-- all tabloids of the given column heights with entries 0..n-1, in column
-- list form
nhAllTabloids = (colHeights, nVal) -> (
    if #colHeights == 0 then return {{}};
    h := first colHeights;
    rest := drop(colHeights, 1);
    firstCols := nhAllCols(h, nVal);
    restTabs := nhAllTabloids(rest, nVal);
    flatten for col in firstCols list for r in restTabs list prepend(col, r)
);

-- the straightening of a nest tabloid in column list form (SchurFunctors'
-- straighten on the Filling), as a HashTable HookTabloid => QQ
nhStraightenColumns = T -> (
    S := straighten(new Filling from T);
    hashTable apply(keys S, F -> tabloid F => S#F)
);

-- the record of one counterexample: the tabloid, its two hooks, its
-- straightening, the projection of its lift and the expected value
nhCounterexampleRecord = (T, T1, T2, straightening, actual, expected) -> hashTable {
    "tabloid" => nhColumnsToTabloid T,
    "T1" => nhColumnsToTabloid T1, "T2" => nhColumnsToTabloid T2,
    "straightening" => straightening,
    "actualMu" => actual,
    "expected" => expected
};

-- the counterexamples among the given tabloids (column list form): the
-- records of those T whose projected lift is not cScalar times the
-- straightening of T, in enumeration order, each paired with T
nhGarnirCounterexamples = (a, b, c, d, nVal, cScalar, allTabs) -> (
    ces := {};
    for T in allTabs do (
        (T1, T2) := nhDecomposeNestColumns(T, a, b, c, d);
        actual := nhTermsToTable nhApplyNestedHookComp(T1, T2, a, b, c, d, nVal);
        straightening := nhStraightenColumns T;
        expected := applyValues(straightening, v -> cScalar * v);
        allKeys := unique(keys expected | keys actual);
        if any(allKeys, k -> (if actual#?k then actual#k else 0_QQ) !=
                (if expected#?k then expected#k else 0_QQ)) then
            ces = append(ces, {T, nhCounterexampleRecord(T, T1, T2, straightening, actual, expected)});
    );
    ces
);

-- a clean column two violation: column 2 has a repeated entry, every
-- other column is strictly increasing and every row is weakly increasing
nhIsCleanCol2Violation = T -> (
    col2 := T#1;
    if #unique col2 == #col2 then return false;
    for j from 0 to #T - 1 do (
        if j == 1 then continue;
        colj := T#j;
        if #unique colj != #colj then return false;
        for i from 0 to #colj - 2 do
            if colj#i >= colj#(i+1) then return false;
    );
    nrows := max for col in T list #col;
    for r from 0 to nrows - 1 do (
        row := {};
        for j from 0 to #T - 1 do
            if #(T#j) > r then row = append(row, (T#j)#r);
        for i from 0 to #row - 2 do
            if row#i > row#(i+1) then return false;
    );
    true
);

-- the simplicity of a tabloid, lower first: the number of distinct
-- entries, the largest entry, the sum of the entries
nhSimplicityScore = T -> (
    ent := flatten T;
    {#unique ent, max ent, sum ent}
);

-- the simplest counterexample with a clean column two violation, or null
nhMinimalCleanCounterexample = ces -> (
    clean := select(ces, ce -> nhIsCleanCol2Violation(ce#0));
    if #clean == 0 then return null;
    scored := sort apply(clean, ce -> {nhSimplicityScore(ce#0), ce#1});
    (scored#0)#1
);

------------------------------------------------------------------------
-- verifyGarnirLift(outerType, innerType, n): the VerificationReport of
-- kind "GarnirLift" with the rows
--   parameters            {outerType, innerType, n}
--   status                "consistent", "counterexamples", or one of the
--                         diagnostic statuses "no-ssyt", "reference-zero",
--                         "reference-mismatch" when no verdict is possible
--   universalScalar       the scalar c, or null
--   tabloidsTested        the number of tabloids of the nest shape tested
--   counterexamples       the tabloids on which the lift fails
--   minimalCounterexample the simplest clean column two counterexample
--                         record, or null
--   counterexampleData    the records of all counterexamples
--   message               a short statement of the outcome
--   passed                whether the lift is consistent
--   conclusion            the verdict as a sentence
------------------------------------------------------------------------

nhGarnirReport = (parameters, status, passed, scalar, tested, ces, message, conclusion) ->
    hpVerificationReport("GarnirLift", {
        "parameters" => parameters,
        "status" => status,
        "universalScalar" => scalar,
        "tabloidsTested" => tested,
        "counterexamples" => apply(ces, ce -> (ce#1)#"tabloid"),
        "minimalCounterexample" => nhMinimalCleanCounterexample ces,
        "counterexampleData" => apply(ces, ce -> ce#1),
        "message" => message,
        "passed" => passed,
        "conclusion" => conclusion});

verifyGarnirLift = method(TypicalValue => VerificationReport);
verifyGarnirLift(List, List, ZZ) := (outerType, innerType, nVal) -> (
    hpValidateTypePair("verifyGarnirLift", outerType, innerType);
    (a, b, c, d) := (outerType#0 + 1, outerType#1, innerType#0 + 1, innerType#1);
    nhValidateNestedHookParameters(a, b, c, d, nVal, "verifyGarnirLift");
    parameters := {outerType, innerType, nVal};
    nest := nhCompNestShape(a, b, c, d);
    colHeights := {b+1, d+2} | toList(c-1:2) | toList(a-c-1:1);

    stdsN := nhStdTabsFilling(nVal, nest);
    if #stdsN == 0 then return nhGarnirReport(parameters, "no-ssyt", false, null, 0, {},
        "no semistandard tableaux (the alphabet is too small)",
        "the alphabet " | toString nVal | " is too small for the nest shape " |
            toString nest | "; nothing is tested");

    -- the universal scalar c from a reference semistandard tableau
    refT := toList(stdsN#0);
    (refT1, refT2) := nhDecomposeNestColumns(refT, a, b, c, d);
    refMu := nhApplyNestedHookComp(refT1, refT2, a, b, c, d, nVal);
    if #refMu == 0 then return nhGarnirReport(parameters, "reference-zero", false, null, 0, {},
        "the reference semistandard tableau projects to zero",
        "the lift of the reference semistandard tableau " |
            toString nhColumnsToTabloid refT | " projects to zero; no scalar is defined");
    if #refMu != 1 or toList((refMu#0)#1) != refT then
        return nhGarnirReport(parameters, "reference-mismatch", false, null, 0, {},
            "the reference tableau does not satisfy mu(lift) = c z_T",
            "the projected lift of the reference semistandard tableau " |
                toString nhColumnsToTabloid refT |
                " is not a multiple of the tableau; no scalar is defined");

    cScalar := (refMu#0)#0;

    allTabs := nhAllTabloids(colHeights, nVal);
    ces := nhGarnirCounterexamples(a, b, c, d, nVal, cScalar, allTabs);

    if #ces == 0 then nhGarnirReport(parameters, "consistent", true, cScalar, #allTabs, {},
        "the naive Garnir lift is consistent",
        "the projected lift equals " | toString cScalar | " times the straightening on all " |
            toString(#allTabs) | " tabloids of the nest shape " | toString nest)
    else nhGarnirReport(parameters, "counterexamples", false, cScalar, #allTabs, ces,
        toString(#ces) | " counterexamples found",
        "the projected lift differs from " | toString cScalar | " times the straightening on " |
            toString(#ces) | " of the " | toString(#allTabs) | " tabloids of the nest shape " |
            toString nest |
            (if nhMinimalCleanCounterexample ces === null
                then "; no counterexample has a single column two violation"
                else "; the simplest clean counterexample is under minimalCounterexample"))
);

------------------------------------------------------------------------
-- Poincare series of the residue field over generic, symmetric and
-- Pfaffian determinantal rings, and over the cover M_r of a symmetric
-- determinantal ring (Corollary 7.1.3 of the paper).
--
-- A PoincareSeries is a HashTable with the keys
--   numerator, denominator  polynomials in s and t with coefficients in
--                           the Schur ring; s is the internal degree (u
--                           in the paper), t the homological degree, and
--                           the series is numerator/denominator
--   ring                    the Schur ring: x over y over QQ[s,t] for
--                           the generic family, x over QQ[s,t] otherwise
--   "family"                "generic", "symmetric", "pfaffian" or
--                           "symmetricCover"
--   "parameters"            the arguments of the constructor, (n, m, r)
--                           or (n, r)
--   "generatorDegree"       the degree of the generators of the ideal
--   "denominatorTerms"      one record per nonconstant denominator term:
--                           the partition "lambda" of the sum, the Schur
--                           functor index "mu", the homological degree
--                           "i", the strand index "d", the internal
--                           degree "j", the "sign", the "character" and
--                           the "term" itself
--   cache                   the CacheTable of the derived data
-- expression is the Divide numerator/denominator, so a series prints as
-- hilbertSeries does; describe is the constructor call.  The four
-- constructors memoize their value per parameters, so equal parameters
-- return the same object.
------------------------------------------------------------------------

-- All series share one base ring QQ[s,t] and one Schur ring for each
-- (m), (m,n) or (n), so that series from different calls can be
-- compared and added.  The variables are created as local symbols: the
-- global symbols s, t, x, y are not rebound (type use R to write them).
nhpRingCache = new MutableHashTable;
nhpPoincareBaseRing = () -> (
    if not nhpRingCache#?"base" then (
        sS := local s; tS := local t;
        nhpRingCache#"base" = QQ[sS, tS];
    );
    nhpRingCache#"base"
);
nhpSchurRingY = m -> (
    key := ("y", m);
    if not nhpRingCache#?key then (
        yS := local y;
        nhpRingCache#key = schurRing(nhpPoincareBaseRing(), yS, m);
    );
    nhpRingCache#key
);
nhpSchurRingXY = (m, n) -> (
    key := ("xy", m, n);
    if not nhpRingCache#?key then (
        xS := local x;
        nhpRingCache#key = schurRing(nhpSchurRingY m, xS, n);
    );
    nhpRingCache#key
);
nhpSchurRingX = n -> (
    key := ("x", n);
    if not nhpRingCache#?key then (
        xS := local x;
        nhpRingCache#key = schurRing(nhpPoincareBaseRing(), xS, n);
    );
    nhpRingCache#key
);

------------------------------------------------------------------------
-- The type, its constructor and its display
------------------------------------------------------------------------

PoincareSeries = new Type of HashTable;
PoincareSeries.synonym = "Poincare series";

-- the value of a constructor, memoized under key = (family, parameters)
nhpPoincareResultCache = new MutableHashTable;
nhpPoincareSeries = (key, num, den, R, family, parameters, generatorDegree, denTerms) -> (
    P := new PoincareSeries from {
        symbol numerator => num, symbol denominator => den, symbol ring => R,
        "family" => family, "parameters" => parameters,
        "generatorDegree" => generatorDegree, "denominatorTerms" => denTerms,
        symbol cache => new CacheTable};
    nhpPoincareResultCache#key = P;
    P
);

-- the constructor of a family, for describe
nhpPoincareConstructor = family -> (
    if family === "generic" then genericDetPoincare
    else if family === "symmetric" then symDetPoincare
    else if family === "pfaffian" then pfaffianPoincare
    else if family === "symmetricCover" then symDetCoverPoincare
    else error "PoincareSeries: unknown family"
);

-- the display stack: expression (the fraction), net, toString and
-- texMath through it, describe and toExternalString (the re-readable
-- constructor call)
expression PoincareSeries := P ->
    new Divide from {expression P.numerator, expression P.denominator};
-- the fraction as hilbertSeries prints it when it fits the page; a wider
-- numerator or denominator is wrapped, each over its own rule, so that
-- neither part is cut off
net PoincareSeries := P -> (
    num := net expression P.numerator;
    den := net expression P.denominator;
    if width num + 2 <= printWidth and width den + 2 <= printWidth then return net expression P;
    w := max(40, printWidth - 4);
    num = wrap(w, num);
    den = wrap(w, den);
    stack(num, concatenate(max(width num, width den) : "-"), den)
);
toString PoincareSeries := P -> toString expression P;
texMath PoincareSeries := P -> texMath expression P;
describe PoincareSeries := P ->
    Describe (expression nhpPoincareConstructor P#"family") (expression P#"parameters");
toExternalString PoincareSeries := P -> toString describe P;

------------------------------------------------------------------------
-- EquivariantBettiTally: a Betti table whose entries are characters
-- (elements of the Schur ring of a series) rather than integers.  It is
-- a BettiTally in every respect (keys (i, {j}, j), #, ==, the sum of a
-- column); only the display differs.  Core's layout separates the
-- columns by one space, which runs the two-line nets of the characters
-- of neighboring columns together, so this one draws a vertical rule
-- between the columns.
------------------------------------------------------------------------

EquivariantBettiTally = new Type of BettiTally;
EquivariantBettiTally.synonym = "equivariant Betti tally";

-- the layout of Core's rawBettiTally: the header of homological degrees,
-- the row of totals, then one row per internal shift j - i; a zero entry
-- prints as a dot
nhpRawBettiTable = B -> (
    H := new MutableHashTable;
    scan(pairs B, (key, v) -> (
        (i, d, h) := key;
        if h === null then h = d#0;
        key = (h - i, i);
        if H#?key then H#key = H#key + v else H#key = v));
    ks := keys H;
    rows := toList(min(first \ ks) .. max(first \ ks));
    cols := toList(min(last \ ks) .. max(last \ ks));
    T := table(rows, cols, (r, c) -> if H#?(r, c) then H#(r, c) else 0);
    T = prepend(apply(transpose T, sum), T);
    T = applyTable(T, v -> if v == 0 then symbol . else v);
    T = prepend(cols, T);
    apply(splice {, "total:", apply(toSequence rows, r -> RowExpression{r, symbol :})}, T, prepend)
);
-- the table, or, when the characters make it wider than the page, the
-- same table with the columns broken into panels that each fit, every
-- panel carrying the row labels, as Macaulay2 breaks a wide matrix into
-- blocks.  A single character wider than a panel is wrapped first, so
-- that no column alone overflows; wrap splits the two lines of a
-- character together, keeping each subscript under its Schur polynomial.
net EquivariantBettiTally := B -> (
    if #B == 0 then return net new BettiTally from {};
    rows := nhpRawBettiTable B;
    labelWidth := max apply(rows, r -> width net r#0);
    budget := max(24, printWidth - labelWidth - 7);
    rows = apply(rows, r -> prepend(r#0, apply(drop(r, 1), c -> (
        cell := net c;
        if width cell > budget then wrap(budget, cell) else cell))));
    panel := (a, b) -> netList(apply(rows, r -> prepend(r#0, take(r, {a+1, b}))),
        Alignment => Left, HorizontalSpace => 1, BaseRow => 1, Boxes => {false, true});
    ncols := #(rows#0) - 1;
    full := panel(0, ncols);
    if width full <= printWidth then return full;
    panels := {};
    j := 0;
    while j < ncols do (
        k := j + 1;
        while k < ncols and width panel(j, k + 1) <= printWidth do k = k + 1;
        panels = append(panels, panel(j, k));
        j = k);
    stack between("", panels)
);
-- a table of characters and a table of integers compare entry by entry
EquivariantBettiTally == BettiTally := (E, B) -> hashTable pairs E === hashTable pairs B;
BettiTally == EquivariantBettiTally := (B, E) -> hashTable pairs E === hashTable pairs B;

-- the accessors on the Core generics
numerator PoincareSeries := P -> P.numerator;
denominator PoincareSeries := P -> P.denominator;
ring PoincareSeries := P -> P.ring;
coefficientRing PoincareSeries := P -> nhpBaseRingOf P.ring;

------------------------------------------------------------------------
-- Partitions of the formulas
------------------------------------------------------------------------

-- lambda^{(r)}: insert r rows of length d below the Durfee square.
nhpLamPowerR = (lam, r) -> (
    lam = toList lam;
    if r <= 0 then return lam;
    d := hpFrobeniusRank lam;
    if d == 0 then return lam;
    take(lam, d) | toList(r : d) | drop(lam, d)
);

-- Convert Frobenius coordinates (aList, bList) to partition.
nhpFrobToPart = (aList, bList) -> (
    d := #aList;
    if d == 0 then return {};
    rows := for i from 1 to d list i + aList#(i-1);
    colLens := for j from 1 to d list j + bList#(j-1);
    maxRow := max colLens;
    extras := for i from d+1 to maxRow list #select(colLens, c -> c >= i);
    rows | extras
);

-- All partitions in n x m box (length <= n, parts <= m).
nhpAllInBox = (n, m) -> (
    flatten for k from 0 to n*m list (
        for p in select(partitions(k, m), q -> #q <= n) list toList p
    )
);

-- All Q_delta partitions with l(lambda) <= maxLen.
nhpQDeltaParts = (delta, maxLen) -> (
    minA := max(0, delta);
    maxA1 := maxLen + delta - 1;
    if maxA1 < minA then return {{}};
    result := {{}};
    for d from 1 to maxLen do (
        sets := subsets(toList(minA..maxA1), d);
        for s in sets do (
            aList := reverse sort toList s;
            bList := for v in aList list v - delta;
            result = append(result, nhpFrobToPart(aList, bList));
        );
    );
    result
);

------------------------------------------------------------------------
-- genericDetPoincare(n, m, r):
-- Poincare series of k over S/I_r where S = Sym(V (x) W), dim V = n,
-- dim W = m, and I_r is the ideal of r x r minors of the generic
-- n x m matrix.
------------------------------------------------------------------------

genericDetPoincare = method(TypicalValue => PoincareSeries);
genericDetPoincare(ZZ, ZZ, ZZ) := (n, m, r) -> (
    if not (r >= 2 and n >= 1 and m >= 1) then
        error "genericDetPoincare: need integers with r >= 2, n >= 1, m >= 1";
    key := ("generic", n, m, r);
    if nhpPoincareResultCache#?key then return nhpPoincareResultCache#key;
    Rxy := nhpSchurRingXY(m, n);
    Ry := coefficientRing Rxy;
    P := nhpPoincareBaseRing();
    sV := promote(P_0, Rxy);
    tV := promote(P_1, Rxy);

    num := 0_Rxy;
    for lam in nhpAllInBox(n, m) do (
        sz := sum lam;
        piece := Rxy_lam * promote(Ry_(hpConjugatePartition lam), Rxy);
        num = num + (sV * tV)^sz * piece;
    );

    den := 0_Rxy;
    denTerms := {};
    if n >= r and m >= r then (
        for lam in nhpAllInBox(n-r+1, m-r+1) do (
            rk := hpFrobeniusRank lam;
            sz := sum lam;
            muX := nhpLamPowerR(lam, r-1);
            muY := nhpLamPowerR(hpConjugatePartition lam, r-1);
            if #muX <= n and #muY <= m then (
                sign := (-1)^rk;
                piece := Rxy_muX * promote(Ry_muY, Rxy);
                term := sign * (sV * tV)^(sz + rk) * sV^((r-2)*rk) * piece;
                den = den + term;
                if lam != {} then denTerms = append(denTerms, new HashTable from {
                        "lambda" => lam, "mu" => {muX, muY}, "i" => sz, "d" => rk,
                        "j" => sz + (r-1)*rk, "sign" => sign, "character" => piece,
                        "term" => term});
            );
        );
    ) else (
        den = 1_Rxy;
    );
    nhpPoincareSeries(key, num, den, Rxy, "generic", (n, m, r), r, denTerms)
);

------------------------------------------------------------------------
-- symDetPoincare(n, r):
-- Poincare series of k over S/I_r where S = Sym(S^2 V), dim V = n,
-- and I_r is the ideal of r x r minors of the generic symmetric matrix.
------------------------------------------------------------------------

symDetPoincare = method(TypicalValue => PoincareSeries);
symDetPoincare(ZZ, ZZ) := (n, r) -> (
    if not (r >= 2 and n >= 1) then
        error "symDetPoincare: need integers with r >= 2, n >= 1";
    key := ("symmetric", n, r);
    if nhpPoincareResultCache#?key then return nhpPoincareResultCache#key;
    Rx := nhpSchurRingX n;
    P := nhpPoincareBaseRing();
    sV := promote(P_0, Rx);
    tV := promote(P_1, Rx);

    num := 0_Rx;
    for lam in nhpQDeltaParts(1, n) do (
        sz := sum lam;
        num = num + (sV * tV)^(sz // 2) * Rx_lam;
    );

    den := 0_Rx;
    denTerms := {};
    for lam in nhpQDeltaParts(0, n) do (
        rk := hpFrobeniusRank lam;
        if not even rk then continue;
        mu := nhpLamPowerR(lam, r-2);
        if #mu > n then continue;
        sz := sum lam;
        sign := (-1)^(rk // 2);
        term := sign * (sV * tV)^(sz // 2) * sV^((r-2)*rk // 2) * Rx_mu;
        den = den + term;
        if lam != {} then denTerms = append(denTerms, new HashTable from {
              "lambda" => lam, "mu" => mu, "i" => sz // 2 - rk // 2, "d" => rk // 2,
              "j" => sz // 2 + (r-2)*rk // 2, "sign" => sign, "character" => Rx_mu,
              "term" => term});
    );
    nhpPoincareSeries(key, num, den, Rx, "symmetric", (n, r), r, denTerms)
);

------------------------------------------------------------------------
-- symDetCoverPoincare(n, r):
-- Poincare series of k over the algebra M_r = Sym(k^{r-1} (x) V)^{SO_{r-1}},
-- the finite cover of S/I_r of Corollary 7.1.3(ii) (second formula), with
-- its standard grading (so the internal degree is doubled relative to
-- symDetPoincare).  For r = 2 the algebra is a polynomial ring.
------------------------------------------------------------------------

symDetCoverPoincare = method(TypicalValue => PoincareSeries);
symDetCoverPoincare(ZZ, ZZ) := (n, r) -> (
    if not (r >= 2 and n >= 1) then
        error "symDetCoverPoincare: need integers with r >= 2, n >= 1";
    key := ("symmetricCover", n, r);
    if nhpPoincareResultCache#?key then return nhpPoincareResultCache#key;
    Rx := nhpSchurRingX n;
    P := nhpPoincareBaseRing();
    sV := promote(P_0, Rx);
    tV := promote(P_1, Rx);
    num := 0_Rx;
    den := 0_Rx;
    denTerms := {};
    if r == 2 then (
        for k from 0 to n list num = num + (sV * tV)^k * Rx_(toList(k:1));
        den = 1_Rx;
    ) else (
        for lam in nhpQDeltaParts(1, n) do
            num = num + (sV^2 * tV)^((sum lam) // 2) * Rx_lam;
        for lam in nhpQDeltaParts(0, n) do (
            rk := hpFrobeniusRank lam;
            mu := nhpLamPowerR(lam, r-2);
            if #mu > n then continue;
            sz := sum lam;
            sign := (-1)^rk;
            term := sign * (sV^2 * tV)^((sz + rk) // 2) * sV^((r-3)*rk) * Rx_mu;
            den = den + term;
            if lam != {} then denTerms = append(denTerms, new HashTable from {
                   "lambda" => lam, "mu" => mu, "i" => (sz - rk) // 2, "d" => rk,
                   "j" => sz + (r-2)*rk, "sign" => sign, "character" => Rx_mu,
                   "term" => term});
        );
    );
    nhpPoincareSeries(key, num, den, Rx, "symmetricCover", (n, r), 2, denTerms)
);

------------------------------------------------------------------------
-- pfaffianPoincare(n, r):
-- Poincare series of k over S/I_{2r} where S = Sym(Wedge^2 V), dim V = n,
-- and I_{2r} is the ideal of 2r x 2r Pfaffians of the generic skew-
-- symmetric matrix.
------------------------------------------------------------------------

pfaffianPoincare = method(TypicalValue => PoincareSeries);
pfaffianPoincare(ZZ, ZZ) := (n, r) -> (
    if not (r >= 2 and n >= 1) then
        error "pfaffianPoincare: need integers with r >= 2, n >= 1";
    key := ("pfaffian", n, r);
    if nhpPoincareResultCache#?key then return nhpPoincareResultCache#key;
    Rx := nhpSchurRingX n;
    P := nhpPoincareBaseRing();
    sV := promote(P_0, Rx);
    tV := promote(P_1, Rx);

    num := 0_Rx;
    for lam in nhpQDeltaParts(-1, n) do (
        sz := sum lam;
        num = num + (sV * tV)^(sz // 2) * Rx_lam;
    );

    den := 0_Rx;
    denTerms := {};
    for lam in nhpQDeltaParts(-1, n) do (
        rk := hpFrobeniusRank lam;
        mu := nhpLamPowerR(lam, 2*r-2);
        if #mu > n then continue;
        sz := sum lam;
        sign := (-1)^rk;
        term := sign * (sV * tV)^(sz // 2 + rk) * sV^((r-2)*rk) * Rx_mu;
        den = den + term;
        if lam != {} then denTerms = append(denTerms, new HashTable from {
              "lambda" => lam, "mu" => mu, "i" => sz // 2, "d" => rk,
              "j" => sz // 2 + (r-1)*rk, "sign" => sign, "character" => Rx_mu,
              "term" => term});
    );
    nhpPoincareSeries(key, num, den, Rx, "pfaffian", (n, r), r, denTerms)
);

------------------------------------------------------------------------
-- Truncate a Schur-ring polynomial in (s, t) by the exponent predicate
-- keep(exponents, N): nhpTruncT keeps t-degree <= N and nhpTrunc keeps
-- total degree <= N.  Every nonconstant denominator term has t-degree at
-- least 1, so the geometric series for 1/den is finite modulo t^(N+1).
------------------------------------------------------------------------
nhpTruncBy = (e, N, R, keep) -> (
    if e == 0 then return 0_R;
    tms := listForm e;
    if schurLevel R == 0 then (
        sum for term in tms list (
            if keep(term#0, N) then term#1 * R_0^(term#0#0) * R_1^(term#0#1) else 0_R
        )
    ) else (
        sum for term in tms list (
            partAtLev := term#0;
            cf := term#1;
            truncCf := nhpTruncBy(cf, N, coefficientRing R, keep);
            if truncCf == 0 then 0_R else (promote(truncCf, R)) * R_(toList partAtLev)
        )
    )
);
-- truncation to homological (t) degree N, and to total degree N
nhpTruncT = (e, N, R) -> nhpTruncBy(e, N, R, (ex, N) -> ex#1 <= N);
nhpTrunc = (e, N, R) -> nhpTruncBy(e, N, R, (ex, N) -> sum ex <= N);

------------------------------------------------------------------------
-- All bigraded components at once: HashTable (sDeg, tDeg) => Schur-ring
-- element, containing exactly the nonzero components of e.
------------------------------------------------------------------------
nhpBigradeAll = (e, R) -> (
    out := new MutableHashTable;
    addTo := (k, v) -> hpAddTerm(out, k, v);
    if e == 0 then return new HashTable from out;
    if schurLevel R == 0 then (
        for term in listForm e do addTo(toSequence term#0, term#1 * 1_R);
    ) else (
        for term in listForm e do (
            partAtLev := term#0;
            inner := nhpBigradeAll(term#1, coefficientRing R);
            for k in keys inner do addTo(k, (promote(inner#k, R)) * R_(toList partAtLev));
        );
    );
    new HashTable from out
);

------------------------------------------------------------------------
-- dimensionSpecialization: replace every Schur functor in a (nested)
-- Schur ring element by its dimension, landing in the base ring
-- QQ[s,t] of the series.  Applied to a PoincareSeries this yields the
-- non-equivariant Poincare series of Theorem 1.1.1 of the paper as the
-- pair {num, den} at s = 1, that is the numerator (1+t)^{dim S_1} and
-- the denominator sum (-1)^d beta_{i,i+(r-1)d} t^{i+d} as polynomials
-- in t; KeepInternalDegree => true keeps s.
------------------------------------------------------------------------

-- the base ring QQ[s,t] under the (possibly nested) Schur ring R
nhpBaseRingOf = R -> (P := R; while schurLevel P > 0 do P = coefficientRing P; P);

dimensionSpecialization = method(Options => {KeepInternalDegree => false});
dimensionSpecialization RingElement := opts -> e -> (
    Rr := ring e;
    if schurLevel Rr == 0 then return e;
    if e == 0 then return 0_(nhpBaseRingOf Rr);
    sum for term in listForm e list (
        dim(Rr_(toList term#0)) * dimensionSpecialization term#1));
dimensionSpecialization PoincareSeries := opts -> P -> (
    key := ("dimensionSpecialization", opts#KeepInternalDegree);
    if P.cache#?key then return P.cache#key;
    numD := dimensionSpecialization P.numerator;
    denD := dimensionSpecialization P.denominator;
    P.cache#key = if opts#KeepInternalDegree then {numD, denD} else (
        s := (coefficientRing P)_0;
        {sub(numD, {s => 1}), sub(denD, {s => 1})}));

-- The ordinary, one-variable series as an actual rational function (in
-- lowest terms; dimensionSpecialization P gives the paper's numerator
-- and denominator unreduced).
ordinaryPoincareSeries = method(TypicalValue => RingElement);
ordinaryPoincareSeries PoincareSeries := P -> (
    specialized := dimensionSpecialization P;
    F := frac ring specialized#0;
    promote(specialized#0, F) / promote(specialized#1, F)
);

------------------------------------------------------------------------
-- The expansion of num/den as a power series in (s, t): the characters
-- of Tor^R(k,k).  nhpTorCharacters returns the HashTable (tDeg, sDeg)
-- => Schur ring element (the character of Tor_{tDeg}^R(k,k) in internal
-- degree sDeg) with exactly the nonzero bidegrees; with truncation
-- "Homological" every bidegree of homological degree tDeg <= N is
-- returned, complete in the internal degree, with "TotalDegree" the
-- bidegrees with tDeg + sDeg <= N.  The value is cached in P.cache.
------------------------------------------------------------------------
nhpTorCharacters = (caller, P, N, truncation) -> (
    if not instance(N, ZZ) or N < 0 then
        error(caller | ": N must be a nonnegative integer");
    if not member(truncation, {"Homological", "TotalDegree"}) then
        error(caller | ": Truncation must be \"Homological\" or \"TotalDegree\"");
    key := ("torCharacters", N, truncation);
    if P.cache#?key then return P.cache#key;
    homological := truncation === "Homological";
    truncate := if homological then nhpTruncT else nhpTrunc;
    num := P.numerator;
    den := P.denominator;
    R := P.ring;
    one := 1_R;
    rem := one - den;
    if homological and rem != 0 and any(listForm dimensionSpecialization rem, term -> term#0#1 < 1) then
        error(caller | ": the denominator has a term of homological degree 0, so the series cannot be truncated in homological degree; use Truncation => \"TotalDegree\"");
    inv := one;
    pwr := one;
    for k from 1 to N do (
        pwr = truncate(pwr * rem, N, R);
        if pwr == 0 then break;
        inv = inv + pwr;
    );
    result := truncate(num * inv, N, R);
    -- one pass over the expansion, instead of one full traversal per bidegree
    HT := new MutableHashTable;
    graded := nhpBigradeAll(result, R);
    for k in keys graded do (
        (sDeg, tDeg) := k;
        if (homological and tDeg <= N) or
             (not homological and sDeg + tDeg <= N) then HT#(tDeg, sDeg) = graded#k;
    );
    P.cache#key = new HashTable from HT
);

-- an (i, j) => value hash table as a BettiTally (keys (i, {j}, j)) of
-- integers, or as an EquivariantBettiTally of characters
nhpToBettiTally = B -> new BettiTally from apply(pairs B, kv -> (kv#0#0, {kv#0#1}, kv#0#1) => kv#1);
nhpToEquivariantBettiTally = B -> new EquivariantBettiTally from apply(pairs B, kv -> (kv#0#0, {kv#0#1}, kv#0#1) => kv#1);

-- the characters of Tor^R(k,k) as an EquivariantBettiTally
expandPoincare = method(Options => {Truncation => "Homological"}, TypicalValue => EquivariantBettiTally);
expandPoincare(PoincareSeries, ZZ) := opts -> (P, N) ->
    nhpToEquivariantBettiTally nhpTorCharacters("expandPoincare", P, N, opts#Truncation);

------------------------------------------------------------------------
-- Non-equivariant expansions of the Poincare series of k over R.
------------------------------------------------------------------------

-- the coefficients of the series: HashTable (i, j) => dim Tor_i^R(k,k)_j,
-- complete for every i <= N
nhpBettiNumbers = (caller, P, N) -> (
    E := nhpTorCharacters(caller, P, N, "Homological");
    new HashTable from for k in keys E list (
        v := lift(lift(dimensionSpecialization E#k, QQ), ZZ);
        if v == 0 then continue else (k#0, k#1) => v)
);

-- the Betti table of k over R to homological degree N (a BettiTally, as
-- printed by betti res); the options Weights and Minimize of betti are
-- accepted and ignored
betti(PoincareSeries, ZZ) := opts -> (P, N) -> (
    key := ("betti", N);
    if not P.cache#?key then
        P.cache#key = nhpToBettiTally nhpBettiNumbers("betti", P, N);
    P.cache#key
);

-- the total Betti numbers dim Tor_i^R(k,k), i = 0..N
totalBettiNumbers = method(TypicalValue => List);
totalBettiNumbers(PoincareSeries, ZZ) := (P, N) -> (
    B := nhpBettiNumbers("totalBettiNumbers", P, N);
    apply(N+1, i -> sum(select(keys B, k -> k#0 == i), k -> B#k))
);

-- the series truncated at t^N, in the base ring QQ[s,t] with s = 1 (or,
-- with KeepInternalDegree => true, as a polynomial in s and t)
poincareSeriesTruncation = method(Options => {KeepInternalDegree => false}, TypicalValue => RingElement);
poincareSeriesTruncation(PoincareSeries, ZZ) := opts -> (P, N) -> (
    B := nhpBettiNumbers("poincareSeriesTruncation", P, N);
    S := coefficientRing P;
    s := S_0; t := S_1;
    sum(pairs B, kv -> kv#1 * t^(kv#0#0) * (if opts#KeepInternalDegree then s^(kv#0#1) else 1_S))
);

------------------------------------------------------------------------
-- strandBettiTable: the graded Betti numbers beta_{i,j}(R) of R over S,
-- read off from the Poincare denominator (each term is
-- (-1)^d beta_{i,i+(r-1)d} t^{i+d} s^{i+(r-1)d}), as a BettiTally.  The
-- records of P#"denominatorTerms" carry the (i, d, j) of every term, so
-- this works for every r >= 2, including the Koszul case r = 2 where
-- the exponents alone would not separate the strands.
-- equivariantStrandBettiTable keeps the characters: the
-- EquivariantBettiTally with entries the characters of Tor_i^S(R,k)_j.
------------------------------------------------------------------------

strandBettiTable = method(TypicalValue => BettiTally);
strandBettiTable PoincareSeries := P -> (
    if P.cache#?"strandBettiTable" then return P.cache#"strandBettiTable";
    H := new MutableHashTable;
    H#(0,0) = 1;
    for rec in P#"denominatorTerms" do (
        key := (rec#"i", rec#"j");
        v := lift(dimensionSpecialization rec#"character", ZZ);
        if H#?key then H#key = H#key + v else H#key = v);
    P.cache#"strandBettiTable" = nhpToBettiTally new HashTable from H);

equivariantStrandBettiTable = method(TypicalValue => EquivariantBettiTally);
equivariantStrandBettiTable PoincareSeries := P -> (
    if P.cache#?"equivariantStrandBettiTable" then return P.cache#"equivariantStrandBettiTable";
    R := P.ring;
    H := new MutableHashTable;
    H#(0,0) = 1_R;
    for rec in P#"denominatorTerms" do (
        key := (rec#"i", rec#"j");
        v := rec#"character";
        if H#?key then H#key = H#key + v else H#key = v);
    P.cache#"equivariantStrandBettiTable" = nhpToEquivariantBettiTally new HashTable from H);

------------------------------------------------------------------------
-- The ring the series is about, and the checks of the series against
-- Macaulay2 itself.  Every check returns a Boolean and prints what it
-- compared when debugLevel > 0.
------------------------------------------------------------------------

-- the polynomial ring S and the ideal I of the family over QQ, as the
-- pair {S, I} (the variables are the default p_0, p_1, ... of Macaulay2;
-- the ring is not assigned to any global symbol)
nhpPoincareRing = P -> (
    family := P#"family";
    params := toList P#"parameters";
    n := params#0;
    r := last params;
    S := null; I := null;
    if family == "generic" then (
        m := params#1;
        S = QQ[Variables => n*m];
        I = minors(r, genericMatrix(S, n, m)))
    else if family == "symmetric" then (
        S = QQ[Variables => n*(n+1)//2];
        I = minors(r, genericSymmetricMatrix(S, n)))
    else if family == "pfaffian" then (
        S = QQ[Variables => n*(n-1)//2];
        I = pfaffians(2*r, genericSkewMatrix(S, n)))
    else if family == "symmetricCover" then (
        -- M_r = Sym(k^{r-1} (x) V)^{SO_{r-1}}; for r = 3 the group SO_2 is a
        -- torus and the invariant ring is the Segre ring k[x_i y_j] of an
        -- n x n matrix, for r = 2 the polynomial ring on V
        -- with the standard grading of M_r the generators have degree 2
        if r == 2 then (S = QQ[Variables => n]; I = ideal(0_S))
        else if r == 3 then (
            S = QQ[Variables => n*n, Degrees => toList(n*n : 2)];
            I = minors(2, genericMatrix(S, n, n)))
        else error "determinantalRing: the cover M_r is only constructed for r = 2, 3")
    else error "determinantalRing: unknown family";
    {S, I}
);

-- the quotient ring S/I (or the cover M_r) the series is about, cached
determinantalRing = method(TypicalValue => QuotientRing);
determinantalRing PoincareSeries := P -> (
    if not P.cache#?"determinantalRing" then (
        (S, I) := toSequence nhpPoincareRing P;
        P.cache#"determinantalRing" = S/I);
    P.cache#"determinantalRing"
);

-- true when every denominator term has strand index d = 1
isGolodPoincare = method(TypicalValue => Boolean);
isGolodPoincare PoincareSeries := P -> all(P#"denominatorTerms", rec -> rec#"d" == 1);

-- r = 2: the series must equal 1/H_R(-t) (Koszulness of S/I_2)
koszulPoincareCheck = method(TypicalValue => Boolean);
koszulPoincareCheck PoincareSeries := P -> (
    if P#"generatorDegree" != 2 then
        error "koszulPoincareCheck: only meaningful for r = 2 (a quadratic ideal)";
    A := determinantalRing P;
    S := ambient A;
    I := ideal A;
    if any(degrees S, d -> d != {1}) then (
        -- the identity P(t) = 1/H(-t) needs the generators in degree one
        S1 := newRing(S, Degrees => toList(numgens S : 1));
        I = sub(I, S1);
        A = S1/I;
    );
    h := reduceHilbert hilbertSeries A;
    T := QQ[Variables => 1];
    hNum := sub(value numerator h, {(ring value numerator h)_0 => -T_0});
    hDen := sub(value denominator h, {(ring value denominator h)_0 => -T_0});
    specialized := dimensionSpecialization P;
    B := ring specialized#0;
    pNum := sub(specialized#0, {B_0 => 1_T, B_1 => T_0});
    pDen := sub(specialized#1, {B_0 => 1_T, B_1 => T_0});
    -- P(t) = num/den must equal 1/h(-t) = hDen(-t)/hNum(-t)
    agree := pNum * hNum == pDen * hDen;
    if debugLevel > 0 then (
        << "koszulPoincareCheck " << toString describe P << ": the Hilbert series of the ring is" << endl
            << net h << endl
            << "the predicted Poincare series is" << endl
            << net ordinaryPoincareSeries P << endl
            << "P(t) = 1/H(-t): " << agree << endl);
    agree
);

-- resolve k over S/I to length N and compare with the formula
verifyPoincareSeries = method(TypicalValue => Boolean);
verifyPoincareSeries(PoincareSeries, ZZ) := (P, N) -> (
    if N < 0 then error "verifyPoincareSeries: N must be a nonnegative integer";
    A := determinantalRing P;
    C := freeResolution(coker vars A, LengthLimit => N);
    actual := new BettiTally from select(pairs betti C, kv -> kv#0#0 <= N);
    predicted := betti(P, N);
    agree := actual == predicted;
    if debugLevel > 0 then (
        << "verifyPoincareSeries " << toString describe P << ": the Betti table of the residue field" <<
            " resolved to length " << N << " is" << endl
            << net actual << endl
            << "the formula predicts" << endl
            << net predicted << endl
            << "total Betti numbers " << apply(N+1, i -> if i <= length C then rank C_i else 0) <<
            " (resolution) and " << totalBettiNumbers(P, N) << " (formula)" << endl
            << "agree: " << agree << endl);
    agree
);

-- resolve S/I over S and compare its Betti table with the strand table
verifyStrandBettiTable = method(TypicalValue => Boolean);
verifyStrandBettiTable PoincareSeries := P -> (
    A := determinantalRing P;
    -- The strand table of the cover M_r records Tor over the ring M_r is
    -- finite over, namely Sym(S^2 V) (Corollary 7.1.3(ii)), while
    -- determinantalRing presents M_r in its Segre form over an ambient ring
    -- on n^2 variables.  Resolving over that ambient would compare the table
    -- against the wrong base ring and report a correct table as wrong, so the
    -- cover is pushed forward to Sym(S^2 V) first.  For r = 2 the cover is a
    -- polynomial ring and the two bases agree.
    actual := if P#"family" === "symmetricCover" and last P#"parameters" >= 3 then (
        n := (P#"parameters")#0;
        m := n*(n+1)//2;
        B := (coefficientRing ambient A)[Variables => m, Degrees => toList(m : 2)];
        g := genericMatrix(A, A_0, n, n);
        f := map(A, B, flatten for i from 0 to n-1 list for j from i to n-1 list g_(i,j) + g_(j,i));
        betti freeResolution pushForward(f, A^1))
    else betti freeResolution((ambient A)^1/ideal A);
    predicted := strandBettiTable P;
    agree := actual == predicted;
    if debugLevel > 0 then (
        << "verifyStrandBettiTable " << toString describe P << ": the Betti table of the ring over" <<
            " the polynomial ring it is finite over is" << endl
            << net actual << endl
            << "the denominator predicts" << endl
            << net predicted << endl
            << "agree: " << agree << endl);
    agree
);

------------------------------------------------------------------------
-- Pieri maps on hook Schur functors (Section 4 of the paper)
--
-- Public interface; every producer takes CoefficientRing => K (default QQ)
-- and returns coefficients in K:
--   hookBasis(type, n)             the semistandard hook tabloids of type
--                                  {arm, leg} on the alphabet 1..n, as
--                                  HookTabloids in generator order
--   pieriRowRemove(T, m)           the row Pieri map P_m on a hook
--                                  tabloid: HashTable {beta, U} => c
--   juxtaposeSymToRow(T, alpha)    the multiplication Q_m of a symmetric
--                                  monomial into the first row:
--                                  HashTable HookTabloid => c
--   pieriColumnAdd(T, K)           the column Pieri map Q'_m:
--                                  HashTable HookTabloid => c
--   pieriMatrix(outer, inner, n)   the matrix of the hook Pieri injection
--                                  in the semistandard bases, one content
--                                  with Content => w; the bases are the
--                                  lists of {T1, T2} HookTabloid pairs at
--                                  M.cache#"sourceBasis", M.cache#"targetBasis"
--   pieriSourceBasis M, pieriTargetBasis M      those lists
--   pieriImage(M, i), pieriImage(M, pair)       one column of M as a
--                                  HashTable {U1, U2} => c
--   pieriImage(T1, T2)             the image of one nested pair without
--                                  the matrix
--   pieriPreimage(M, pair)         the Pieri preimage of a target pair:
--                                  HashTable {S1, S2} => c with
--                                  v = M x + w, w in the span of the nested
--                                  semistandard pairs
-- The engines below work on the native pair {rowTail, colFull} of a hook
-- (the arm, then the corner followed by the leg) and on the first row
-- lengths a = arm + 1; the public layer converts through tabloid and
-- hpHookToNative.  Every engine computes over the current field hpField().
------------------------------------------------------------------------

------------------------------------------------------------------------
-- The semistandard hook basis on the alphabet 1..n (native pairs, first
-- row length a, leg length b): columns in strict sequence order, then
-- row tails weakly increasing from the corner entry.  The name is kept
-- unprefixed for the external scripts (decisions.txt item 8).
------------------------------------------------------------------------

hookSSYTBasis = (a,b,n) -> (
    if a < 1 then error "a must be >= 1";
    if b < 0 then error "b must be >= 0";
    flatten for col in hpStrictSeqs(b+1, 1, n) list
        for rowTail in hpWeakSeqs(a-1, col#0, n) list {rowTail, col}
);

-- the record classes of the engines' values (section 05 builds and reads
-- them too; they carry no methods of their own)
hpHookCombinationData = new Type of HashTable;
hpRowPieriData = new Type of HashTable;

------------------------------------------------------------------------
-- Straightening of a single hook: the expansion of {rowTail, colFull} in
-- the semistandard hook basis, keyed by native pairs
------------------------------------------------------------------------

hpStraightenHookHT = (rowTail0, colFull0) -> (
    if not instance(rowTail0,List) or not instance(colFull0,List) then
        error "hpStraightenHookHT: expected a row tail and full column as lists";
    if #colFull0 == 0 then
        error "hpStraightenHookHT: the full column must contain the corner";
    if any(rowTail0 | colFull0,
        x -> not instance(x,ZZ) or x <= 0) then
        error "hpStraightenHookHT: tableau entries must be positive integers";
    H := new MutableHashTable;

    canonizeOne := (rowTail, colFull, coeff) -> (
        -- a repeated column letter is a zero wedge
        if #set(colFull) =!= #colFull then return;

        coeff1 := coeff * hpInvSign(colFull);
        colS := sort colFull;
        rowS := sort rowTail;

        if #rowS == 0 then (
            key0 := {rowS, colS};
            if H#?key0 then H#key0 = H#key0 + coeff1 else H#key0 = coeff1;
            return;
        );

        i1 := rowS#0;
        j1 := colS#0;

        if i1 >= j1 then (
            key1 := {rowS, colS};
            if H#?key1 then H#key1 = H#key1 + coeff1 else H#key1 = coeff1;
            return;
        );

        rowRest := drop(rowS,1);
        L := #colS;

        for ell from 1 to L do (
            jell := colS#(ell-1);
            row2 := sort ({jell} | rowRest);
            col2raw := {i1} | (take(colS, ell-1) | drop(colS, ell));

            if #set(col2raw) =!= #col2raw then continue;

            signLap := if (ell % 2 == 1) then 1 else -1;
            coeff2 := coeff1 * signLap;

            canonizeOne(row2, col2raw, coeff2);
        );
    );

    canonizeOne(rowTail0, colFull0, 1_(hpField()));
    new hpHookCombinationData from H
);

------------------------------------------------------------------------
-- The row Pieri map P_m on a hook tableau.
--
-- This is the closed formula of Section 4 of the paper (horizontal case)
-- in the d-model, with the tableau T = {rowTail, colFull} read as
-- T^alpha_I, alpha the row tail multiset and I the first column in wedge
-- order:
--
--   P_m(T^alpha_I) = sum_{beta <= alpha, |beta| = m} C(alpha,beta)
--                        e^beta (x) T^{alpha-beta}_I
--     + 1/(a+b-m) sum_{i in I} sgn(i,I) sum_{beta} sum_{s in supp(beta)}
--          beta_s C(alpha,beta) e^{beta - e_s + e_i} (x) T^{alpha-beta}_{s ^ (I \ i)},
--
-- where C(alpha,beta) = prod_s binomial(alpha_s, beta_s), sgn(i,I) is the
-- sign of e_i ^ e_{I \ i} = sgn(i,I) e_I, and every output hook is
-- straightened to the semistandard basis by hpStraightenHookHT.
------------------------------------------------------------------------

-- distinct sub-multisets of size m of the sorted multiset alpha
hpSubMultisets = (alpha, m) -> (
    vals := unique alpha;
    counts := hashTable apply(vals, v -> v => #select(alpha, x -> x == v));
    rec := (idx, remaining) -> (
        if idx == #vals then return (if remaining == 0 then {{}} else {});
        v := vals#idx;
        flatten for k from 0 to min(counts#v, remaining) list
            for rest in rec(idx+1, remaining-k) list (toList(k:v) | rest)
    );
    apply(rec(0, m), sort)
);

hpPieriRowRemoveHT = (rowTail, colFull, m) -> (
    if not instance(rowTail,List) or not instance(colFull,List) then
        error "hpPieriRowRemoveHT: expected a row tail and a full column as lists.";
    if #colFull == 0 then error "colFull must include corner.";
    if not instance(m,ZZ) or m < 1 then error "m must be >= 1.";
    if m > #rowTail then error("Need m <= #rowTail = " | toString(#rowTail) | ".");
    if any(rowTail | colFull, x -> not instance(x,ZZ) or x <= 0) then
        error "hpPieriRowRemoveHT: tableau entries must be positive integers.";

    H := new MutableHashTable;
    -- a repeated column letter is a zero wedge
    if #(set colFull) =!= #colFull then return new hpRowPieriData from H;
    sgnCol := hpInvSign colFull;
    I := sort colFull;
    alpha := sort rowTail;
    denom := #alpha + #I - m;          -- a + b - m
    invDenom := hpUnitAt(denom, "the row Pieri map P_" | toString m |
        " on the hook (" | toString(#alpha + 1) | ",1^" | toString(#I - 1) | ")");

    addTerm := (beta, rt, cf, c) -> (
        ht := hpStraightenHookHT(rt, cf);
        for k in keys ht do hpAddTerm(H, {beta, k}, c * ht#k);
    );

    for beta in hpSubMultisets(alpha, m) do (
        cab := product for v in unique beta list
            binomial(#select(alpha, x -> x == v), #select(beta, x -> x == v));
        gamma := hpMultisetDiff(alpha, beta);
        -- principal part
        addTerm(beta, gamma, I, sgnCol * cab);
        -- off-diagonal part: exchange a row letter s with a column letter i
        for p from 0 to #I-1 do (
            iEl := I#p;
            Irest := drop(I, {p,p});
            for s in unique beta do (
                betaS := #select(beta, x -> x == s);
                newBeta := sort append(hpRemoveOne(beta, s), iEl);
                addTerm(newBeta, gamma, prepend(s, Irest),
                    (sgnCol * (-1)^p * betaS * cab) * invDenom);
            );
        );
    );

    new hpRowPieriData from H
);

------------------------------------------------------------------------
-- The multiplication Q_m: the letters of a symmetric monomial are
-- appended to the row tail and the hook is straightened
------------------------------------------------------------------------

hpJuxtaposeSymToRowHT = (rowTail, colFull, alpha) -> (
    rowApp := sort (rowTail | alpha);
    hpStraightenHookHT(rowApp, colFull)
);

------------------------------------------------------------------------
-- The injection matrix in first row length coordinates (a,b,c,d,n): the
-- map S_(c,1^b) (x) S_(a,1^d) -> S_(a,1^b) (x) S_(c,1^d) as the composite
-- of id (x) P_(a-c) and Q_(a-c) (x) id, in the semistandard bases
------------------------------------------------------------------------

-- argument check shared by the injection matrix builders
hpValidateBatchArgs = (caller,a,b,c,d,n,wt) -> (
    if not all({a,b,c,d,n}, x -> instance(x,ZZ)) then
        error(caller | ": expected integers (a,b,c,d,n)");
    if c < 1 then error(caller | ": c must be >= 1 (a and c are first row" |
        " lengths, arm length plus one; pieriMatrix takes {arm,leg} types)");
    if d < 0 then error(caller | ": d must be >= 0");
    if a <= c then error(caller | ": need a > c (the outer hook (a,1^b) has" |
        " the longer first row)");
    if b <= d then error(caller | ": need b > d (the outer hook has the longer leg)");
    if n < 1 then error(caller | ": need n >= 1");
    if wt =!= null then (
        if not instance(wt,List) or #wt != n or
           any(wt, x -> not instance(x,ZZ) or x < 0) then
            error(caller | ": Content must be a list of " | toString n |
                " nonnegative integers");
        if sum wt != a+b+c+d then
            error(caller | ": Content must sum to a+b+c+d = " | toString(a+b+c+d) |
                " (the number of boxes of the two hooks)");
    );
);

-- Fill the columns of the sparse injection matrix Mmut: column colIndex is
-- the image of domPairs#colIndex, computed as the row Pieri map P_m on the
-- second factor followed by the juxtaposition of each symmetric monomial
-- onto the first factor; rowIndex(hook1, hook2) locates the codomain pair.
-- P_m is computed once per second factor and each juxtaposition once per
-- (first factor, symmetric monomial).
hpFillInjectionColumns = (Mmut, domPairs, rowIndex, m) -> (
    byT2 := new MutableHashTable;
    for colIndex from 0 to #domPairs-1 do (
        T2 := domPairs#colIndex#1;
        byT2#T2 = if byT2#?T2 then append(byT2#T2, colIndex) else {colIndex});
    juxCache := new MutableHashTable;
    for T2 in keys byT2 do (
        HT2 := hpPieriRowRemoveHT(T2#0, T2#1, m);
        for key2 in keys HT2 do (
            coeff2 := HT2#key2;
            symMon := key2#0;
            tab2 := key2#1;
            for colIndex in byT2#T2 do (
                T1 := domPairs#colIndex#0;
                ckey := (T1, symMon);
                ht1 := if juxCache#?ckey then juxCache#ckey
                    else juxCache#ckey = hpJuxtaposeSymToRowHT(T1#0, T1#1, symMon);
                for key1 in keys ht1 do (
                    rIndex := rowIndex(key1, tab2);
                    Mmut_(rIndex, colIndex) = Mmut_(rIndex, colIndex) + ht1#key1 * coeff2))));
);

-- the full injection matrix: {M, domPairs, codPairs} in native pairs
hpHookInjectionMatrixFull = (a,b,c,d,n) -> (
    B1 := hookSSYTBasis(c, b, n);
    B2 := hookSSYTBasis(a, d, n);
    C1 := hookSSYTBasis(a, b, n);
    C2 := hookSSYTBasis(c, d, n);
    dimCod2 := #C2;
    idxC1 := hashTable apply(#C1, i -> C1#i => i);
    idxC2 := hashTable apply(#C2, i -> C2#i => i);
    domPairs := flatten for T1 in B1 list for T2 in B2 list {T1, T2};
    codPairs := flatten for T1 in C1 list for T2 in C2 list {T1, T2};
    -- the injection matrix is extremely sparse; a dense QQ matrix of this
    -- size costs seconds to allocate and to garbage collect
    Mmut := mutableMatrix((hpField()), #codPairs, #domPairs, Dense => false);
    hpFillInjectionColumns(Mmut, domPairs,
        (key1, tab2) -> (idxC1#key1)*dimCod2 + idxC2#tab2, a - c);
    {matrix Mmut, domPairs, codPairs}
);

------------------------------------------------------------------------
-- The nested semistandard pairs: the codomain pairs (Touter, Tinner) whose
-- nested tabloid is semistandard; they complement the image of the Pieri
-- injection (Section 4 of the paper)
------------------------------------------------------------------------

hpIsNestedHookSSYTPair = (Touter, Tinner, a,b,c,d) -> (
    row1 := Touter#0;  col1 := Touter#1;
    row2 := Tinner#0;  col2 := Tinner#1;

    if #row1 < c then return false;
    if #col1 < (d+2) then return false;
    if #col2 < (d+1) then return false;

    if row1#0 >= col2#0 then return false;

    for k from 1 to c-1 do (
        if row1#k >= row2#(k-1) then return false;
    );

    if col1#1 > col2#0 then return false;

    for t from 2 to d+1 do (
        if col1#t > col2#(t-1) then return false;
    );

    true
);

------------------------------------------------------------------------
-- Rewriting a codomain pair: the injection matrix M (columns: domPairs)
-- is completed by the unit vectors of the nested semistandard codomain
-- pairs to a square matrix B, inverted once; hpRewriteCodPair then reads
-- the coordinates of a codomain pair off the corresponding column of Binv
-- (the image part on the domain pairs, the cokernel part on the nested
-- semistandard pairs)
------------------------------------------------------------------------

hpPrepareRewrite = (data0, a,b,c,d,n, wt) -> (
    M := data0#0;
    domPairs := data0#1;
    codPairs := data0#2;
    dimCod := numRows M;
    dimDom := numColumns M;
    goodIdx := select(toList(0..dimCod-1), idx ->
        hpIsNestedHookSSYTPair(codPairs#idx#0, codPairs#idx#1, a,b,c,d));
    if dimDom + #goodIdx =!= dimCod then hpFail(
        "HookPieri: the nested semistandard pairs do not complement the Pieri image: " |
        toString(#goodIdx) | " pairs where " | toString(dimCod - dimDom) | " are needed" |
        (if wt === null then "" else " on the content " | toString wt));
    G := mutableMatrix((hpField()), dimCod, #goodIdx, Dense => false);
    for kGood from 0 to #goodIdx-1 do G_(goodIdx#kGood, kGood) = 1;
    B := M | matrix G;
    Binv := (id_((hpField())^dimCod)) // B;
    if B * Binv != id_((hpField())^dimCod) then hpFail(
        "HookPieri: the Pieri complement matrix is singular in characteristic " |
        toString char (hpField()) |
        (if wt === null then "" else " on the weight " | toString wt));
    codIndexHT := new MutableHashTable;
    for idx from 0 to dimCod-1 do codIndexHT#(toString(codPairs#idx)) = idx;
    prepHT := new MutableHashTable from {
        "M" => M, "B" => B, "Binv" => Binv,
        "domPairs" => domPairs, "codPairs" => codPairs,
        "goodIdx" => goodIdx, "codIndexHT" => codIndexHT,
        "a" => a, "b" => b, "c" => c, "d" => d, "n" => n};
    if wt =!= null then prepHT#"weight" = wt;
    prepHT
);
hpPrepareNestedHookRewrite = (a,b,c,d,n) ->
    hpPrepareRewrite(hpHookInjectionMatrixFull(a,b,c,d,n), a,b,c,d,n, null);

hpRewriteCodPair = (prepHT, T1, T2) -> (
    M := prepHT#"M";
    Binv := prepHT#"Binv";
    domPairs := prepHT#"domPairs";
    codPairs := prepHT#"codPairs";
    goodIdx := prepHT#"goodIdx";
    codIndexHT := prepHT#"codIndexHT";

    dimCod := numRows M;
    dimDom := numColumns M;

    idx := codIndexHT#(toString({T1,T2}));

    -- Column idx of Binv is the coordinate vector of e_idx in basis B.
    coordCol := flatten entries (Binv_{idx});

    imgTerms := for j from 0 to dimDom-1 list (
        coeff := coordCol#j;
        if coeff != 0 then {coeff, domPairs#j} else continue
    );

    cokTerms := for j from 0 to #goodIdx-1 list (
        coeff := coordCol#(dimDom + j);
        if coeff != 0 then {coeff, codPairs#(goodIdx#j)} else continue
    );

    out := new MutableHashTable;
    out#"image" = imgTerms;
    out#"cokernel" = cokTerms;
    out
);

------------------------------------------------------------------------
-- Batch rewriting, weight by weight (the engine of the cokernel strategy
-- of section 06).
--
-- The crossed hook Pieri injection preserves content, so its cokernel
-- normal forms can be computed one content at a time on the small
-- weight space matrices (hpPrepareNestedHookRewriteWeight) instead of on
-- the full matrix; the results are identical and are returned in the
-- codomain order of the full injection matrix.  Each record is
-- {T1, T2, cokernelTerms, imageTerms} in native pairs.
------------------------------------------------------------------------
hpBatchRewriteRecords = (a,b,c,d,n,wt,strategy) -> (
    one := 1_(hpField());
    -- the terms of every record are listed in the basis order of the full
    -- injection matrix, whichever way they were computed
    C1 := hookSSYTBasis(a,b,n);
    C2 := hookSSYTBasis(c,d,n);
    B1 := hookSSYTBasis(c,b,n);
    B2 := hookSSYTBasis(a,d,n);
    codPairs := flatten for T1 in C1 list for T2 in C2 list {T1, T2};
    domPairs := flatten for T1 in B1 list for T2 in B2 list {T1, T2};
    codIndex := hashTable apply(#codPairs, i -> codPairs#i => i);
    domIndex := hashTable apply(#domPairs, i -> domPairs#i => i);
    fromPrep := (prepHT, rewrite) -> (
        pairs0 := prepHT#"codPairs";
        goodFlag := set prepHT#"goodIdx";
        for idx from 0 to #pairs0-1 list (
            (T1, T2) := toSequence pairs0#idx;
            if goodFlag#?idx then {T1, T2, {{one, {T1, T2}}}, {}}
            else (
                rw := rewrite(prepHT, T1, T2);
                {T1, T2,
                    sort(select(rw#"cokernel", t -> t#0 != 0), t -> codIndex#(t#1)),
                    sort(select(rw#"image", t -> t#0 != 0), t -> domIndex#(t#1))})));
    if wt =!= null then
        return sort(fromPrep(hpPrepareNestedHookRewriteWeight(a,b,c,d,n,wt), hpRewriteCodPair),
            rec -> codIndex#{rec#0, rec#1});
    if strategy === "Automatic" then
        strategy = if #codPairs <= 1500 then "FullMatrix" else "Weight";
    if strategy === "FullMatrix" then
        return fromPrep(hpPrepareNestedHookRewrite(a,b,c,d,n), hpRewriteCodPair);
    if strategy =!= "Weight" then
        error "HookPieri: Strategy must be \"Automatic\", \"Weight\" or \"FullMatrix\"";
    byWeight := new MutableHashTable;
    for pair in codPairs do (
        w := hpPairWeight(pair, n);
        if not byWeight#?w then byWeight#w = true;
    );
    results := new MutableHashTable;
    for w in keys byWeight do
        for rec in fromPrep(hpPrepareNestedHookRewriteWeight(a,b,c,d,n,w), hpRewriteCodPair) do
            results#{rec#0, rec#1} = rec;
    apply(codPairs, pair -> results#pair)
);

------------------------------------------------------------------------
-- Hook types and shapes
------------------------------------------------------------------------

-- the two hook types {arm,leg} of a nested pair, the outer one larger in
-- both coordinates
hpValidateTypePair = (caller,outerType,innerType) -> (
    if not instance(outerType,List) or #outerType != 2 or
       not instance(innerType,List) or #innerType != 2 then
        error(caller | ": hook types must be lists {arm,leg}");
    if any(outerType|innerType,x -> not instance(x,ZZ) or x < 0) then
        error(caller | ": arm and leg lengths must be nonnegative integers");
    if outerType#0 <= innerType#0 or outerType#1 <= innerType#1 then
        error(caller | ": the outer type must be larger in both coordinates");
);

-- one hook type {arm,leg}
hpValidateHookType = (caller, type) -> (
    if not instance(type,List) or #type != 2 or
       any(type, x -> not instance(x,ZZ) or x < 0) then
        error(caller | ": a hook type is a list {arm,leg} of nonnegative integers");
);

-- the first row length and the leg length of a native hook
hpHookShape = T -> (
    if not instance(T, List) or #T =!= 2 then error "Hook tableau must be {rowTail, colFull}.";
    { (#(T#0) + 1), (#(T#1) - 1) }
);

------------------------------------------------------------------------
-- The image of one nested pair (Tleft, Tright) under the Pieri injection
-- with row difference t = a - c: P_t on the right hook, then Q_t on the
-- left hook; a list of {coefficient, {hook1, hook2}} in native pairs
------------------------------------------------------------------------

hpPieriInjectLocalHT = (Tleft, Tright, t) -> (
    shL := hpHookShape(Tleft);
    shR := hpHookShape(Tright);

    c := shL#0;  b := shL#1;
    a := shR#0;  d := shR#1;

    if t < 0 then error "t must be >= 0.";
    if t =!= a - c then error("Inconsistent t: need t = a-c = " | toString(a-c) | ".");
    if t > #(Tright#0) then error("Need t <= #rowTail(Tright) = " | toString(#(Tright#0)) | ".");

    ht2 := hpPieriRowRemoveHT(Tright#0, Tright#1, t);

    coeffHT := new MutableHashTable;
    dataHT := new MutableHashTable;

    for key2 in keys ht2 do (
        coeff2 := ht2#key2;
        if coeff2 == 0 then continue;

        symMon := key2#0;
        T2out := key2#1;

        ht1 := hpJuxtaposeSymToRowHT(Tleft#0, Tleft#1, symMon);

        for key1 in keys ht1 do (
            coeff1 := ht1#key1;
            if coeff1 == 0 then continue;

            pairOut := {key1, T2out};
            kstr := toString(pairOut);

            if coeffHT#?kstr then (
                coeffHT#kstr = coeffHT#kstr + coeff1*coeff2
            ) else (
                coeffHT#kstr = coeff1*coeff2;
                dataHT#kstr = pairOut;
            );
        );
    );

    out := {};
    for kstr in keys coeffHT do (
        if coeffHT#kstr != 0 then out = out | {{coeffHT#kstr, dataHT#kstr}};
    );
    out
);

-- the nested pair (Tleft, Tright) of native hooks of the Pieri injection:
-- the first hook has the longer first column, the second the longer first
-- row; returns the row difference t
hpValidateNestedHooks = (caller, Tleft, Tright) -> (
    leftShape := hpHookShape Tleft;
    rightShape := hpHookShape Tright;
    if rightShape#0 <= leftShape#0 then
        error(caller | ": the second hook must have the longer first row");
    if leftShape#1 <= rightShape#1 then
        error(caller | ": the first hook must have the longer first column");
    rightShape#0 - leftShape#0
);

------------------------------------------------------------------------
-- Weight utilities and the weight restricted injection matrix (the block
-- of one content), cached per (a,b,c,d,n,content,field)
------------------------------------------------------------------------

-- weight of a single native hook on the alphabet 1..n
hpHookWeight = (T, n) -> (
    if not instance(T, List) or #T =!= 2 then error "hpHookWeight: expected hook tableau {rowTail,colFull}.";
    rowTail := T#0; colFull := T#1;

    wtM := new MutableList;
    for i from 0 to n-1 do wtM#i = 0;

    entries := rowTail | colFull;
    for x in entries do (
        if not instance(x, ZZ) then error "hpHookWeight: entries must be integers.";
        if x < 1 or x > n then error("hpHookWeight: entry " | toString x | " out of range 1.." | toString n);
        wtM#(x-1) = wtM#(x-1) + 1;
    );
    toList wtM
);

hpWeightNonneg = u -> all(u, x -> x >= 0);
hpWeightTotal = u -> sum u;
-- total weight of a tensor basis element (pair of native hooks)
hpPairWeight = (P, n) -> (
    if not instance(P, List) or #P =!= 2 then error "hpPairWeight: expected pair {T1,T2}.";
    hpWeightAdd(hpHookWeight(P#0,n), hpHookWeight(P#1,n))
);

-- bucket a basis list by weight: ht#(wt) = list of tableaux of that wt
hpBucketByWeight = (B, n) -> (
    ht := new MutableHashTable;
    for T in B do (
        wt := hpHookWeight(T,n);
        if ht#?wt then ht#wt = ht#wt | {T} else ht#wt = {T};
    );
    ht
);

hpWeightBasisBucketCache = new MutableHashTable;
hpWeightBasisBuckets = (a,b,n) -> (
    key := {a,b,n};
    if hpWeightBasisBucketCache#?key then
        return hpWeightBasisBucketCache#key;
    basis := hookSSYTBasis(a,b,n);
    data := {basis,hpBucketByWeight(basis,n)};
    hpWeightBasisBucketCache#key = data;
    data
);

hpInjectionMatrixWeightCache = new MutableHashTable;

-- the block of one content: {M_w, domPairs_w, codPairs_w} in native pairs;
-- the name is kept unprefixed for the external scripts (decisions.txt
-- item 8); the cokernel strategy of section 06 calls it
hookInjectionMatrixWeight = (a,b,c,d,n,wt) -> (
    hpValidateBatchArgs("hookInjectionMatrixWeight",a,b,c,d,n,wt);
    cacheKey := {a,b,c,d,n,wt,(hpField())};
    if hpInjectionMatrixWeightCache#?cacheKey then
        return hpInjectionMatrixWeightCache#cacheKey;

    m := a - c;

    -- bases in native pairs
    B1data := hpWeightBasisBuckets(c,b,n);
    B2data := hpWeightBasisBuckets(a,d,n);
    C1data := hpWeightBasisBuckets(a,b,n);
    C2data := hpWeightBasisBuckets(c,d,n);
    B1 := B1data#0;
    B2 := B2data#0;
    C1 := C1data#0;
    C2 := C2data#0;

    -- bucket by weights to avoid forming full cartesian products
    B1by := B1data#1;
    B2by := B2data#1;
    C1by := C1data#1;
    C2by := C2data#1;

    -- form the pairs of content wt only
    domPairs := flatten for u in keys B1by list (
        v := hpWeightSub(wt, u);
        if not (hpWeightNonneg v and B2by#?v) then continue;
        flatten for T1 in B1by#u list for T2 in B2by#v list {T1, T2});
    codPairs := flatten for u in keys C1by list (
        v := hpWeightSub(wt, u);
        if not (hpWeightNonneg v and C2by#?v) then continue;
        flatten for T1 in C1by#u list for T2 in C2by#v list {T1, T2});
    rowIndexHT := hashTable apply(#codPairs, i -> codPairs#i => i);
    -- the injection matrix is extremely sparse; a dense QQ matrix of this
    -- size costs seconds to allocate and to garbage collect
    Mmut := mutableMatrix((hpField()), #codPairs, #domPairs, Dense => false);
    hpFillInjectionColumns(Mmut, domPairs, (key1, tab2) -> (
        pair := {key1, tab2};
        if not rowIndexHT#?pair then error(
            "hookInjectionMatrixWeight: output pair not found in codPairs for this weight: " |
            toString pair);
        rowIndexHT#pair), m);
    answer := {matrix Mmut, domPairs, codPairs};
    hpInjectionMatrixWeightCache#cacheKey = answer;
    answer
);

hpPrepareNestedHookRewriteWeight = (a,b,c,d,n,wt) ->
    hpPrepareRewrite(hookInjectionMatrixWeight(a,b,c,d,n,wt), a,b,c,d,n, wt);

-- binomial coefficient for multisets: choose submultiset B from A
hpBinomMultiset = (A,B) -> (
    HA := hpCountHT A;
    HB := hpCountHT B;
    prod := 1;
    for k in keys HB do (
        if not HA#?k then return 0;
        prod = prod * binomial(HA#k, HB#k);
    );
    prod
);

-- wedge peeling sign sgn_I(i)=(-1)^(pos-1), with I sorted
hpSgnInSet = (I,i) -> (
    p := position(I, x -> x==i);
    if p === null then error("hpSgnInSet: " | toString i | " not in " | toString I);
    if (p % 2 == 0) then 1 else -1
);

-- sign to reorder W into increasing order; duplicates give 0 (the wedge
-- is zero)
hpWedgeReorderSign = W -> (
    if #set(W) =!= #W then return 0;
    hpInvSign(W)
);

------------------------------------------------------------------------
-- The public layer: HookTabloids in, HashTables keyed by HookTabloids
-- out.  A hook argument may be a HookTabloid, a list of rows or a
-- Filling; it must be hook shaped with positive integer entries.
------------------------------------------------------------------------

-- the native pair {rowTail, colFull} of a hook argument
hpHookArgument = (caller, T) -> (
    U := if instance(T, HookTabloid) then T
        else if instance(T, List) or instance(T, Filling) then tabloid T
        else error(caller | ": expected a hook shaped tabloid (a HookTabloid," |
            " a list of rows or a Filling)");
    p := hpHookToNative(caller, U);
    if any(p#0 | p#1, x -> not instance(x, ZZ) or x <= 0) then
        error(caller | ": the entries of the hook must be positive integers");
    p
);

-- a list of letters (positive integers)
hpValidateLetters = (caller, what, L) -> (
    if not instance(L, List) or any(L, x -> not instance(x, ZZ) or x <= 0) then
        error(caller | ": " | what | " must be a list of positive integers (letters)");
    L
);

-- the HookTabloid of a native pair, and a HashTable keyed by native pairs
-- re-keyed by HookTabloids
hpNativeToTabloid = p -> tabloid(p#0, p#1);
hpTabloidTerms = H -> hashTable apply(keys H, k -> hpNativeToTabloid k => H#k);

-- a pair label {T1, T2} of hook tabloids, each given as a HookTabloid, a
-- list of rows or a Filling, in the form stored in the matrix caches
hpPairLabel = (caller, pair) -> (
    if not instance(pair, List) or #pair != 2 then
        error(caller | ": expected a pair {T1, T2} of tabloids");
    apply(pair, T -> if instance(T, HookTabloid) then T
        else if instance(T, List) or instance(T, Filling) then tabloid T
        else error(caller | ": expected a pair {T1, T2} of tabloids"))
);

-- hookBasis(type, n): the semistandard hook tabloids of one type; section
-- 07 installs the Ring methods (hookBasis R, hookBasis(R, type)) on this
-- method function
hookBasis = method(TypicalValue => List);
hookBasis(List, ZZ) := (type, n) -> (
    hpValidateHookType("hookBasis", type);
    if n < 0 then error "hookBasis: the alphabet size must be a nonnegative integer";
    apply(hookSSYTBasis(type#0 + 1, type#1, n), hpNativeToTabloid)
);

-- pieriRowRemove(T, m): the row Pieri map P_m, keys {beta, U} with beta
-- the sorted list of the m removed letters and U a semistandard hook
pieriRowRemove = method(Options => {CoefficientRing => QQ}, TypicalValue => HashTable);
pieriRowRemove(HookTabloid, ZZ) := opts -> (T, m) -> (
    K := hpValidateCoefficientRing("pieriRowRemove", opts#CoefficientRing);
    p := hpHookArgument("pieriRowRemove", T);
    if m < 1 or m > #(p#0) then
        error("pieriRowRemove: m must be between 1 and the arm length " | toString(#(p#0)));
    hpWithField(K, () -> (
        H := hpPieriRowRemoveHT(p#0, p#1, m);
        hashTable apply(keys H, k -> {k#0, hpNativeToTabloid k#1} => H#k)))
);
pieriRowRemove(List, ZZ) := opts -> (T, m) -> pieriRowRemove(tabloid T, m, CoefficientRing => opts#CoefficientRing);
pieriRowRemove(Filling, ZZ) := opts -> (T, m) -> pieriRowRemove(tabloid T, m, CoefficientRing => opts#CoefficientRing);

-- juxtaposeSymToRow(T, alpha): the multiplication Q_m of the symmetric
-- monomial with letters alpha into the first row of T
juxtaposeSymToRow = method(Options => {CoefficientRing => QQ}, TypicalValue => HashTable);
juxtaposeSymToRow(HookTabloid, List) := opts -> (T, alpha) -> (
    K := hpValidateCoefficientRing("juxtaposeSymToRow", opts#CoefficientRing);
    p := hpHookArgument("juxtaposeSymToRow", T);
    hpValidateLetters("juxtaposeSymToRow", "alpha", alpha);
    hpWithField(K, () -> hpTabloidTerms hpJuxtaposeSymToRowHT(p#0, p#1, alpha))
);
juxtaposeSymToRow(List, List) := opts -> (T, alpha) -> juxtaposeSymToRow(tabloid T, alpha, CoefficientRing => opts#CoefficientRing);
juxtaposeSymToRow(Filling, List) := opts -> (T, alpha) -> juxtaposeSymToRow(tabloid T, alpha, CoefficientRing => opts#CoefficientRing);

-- pieriColumnAdd(T, K): the column Pieri map Q'_m adjoining the wedge e_K
-- to the first column of T; the engine hpPieriColumnAddHT(rowTail, colFull,
-- K) lives in section 05 and keeps that name
pieriColumnAdd = method(Options => {CoefficientRing => QQ}, TypicalValue => HashTable);
pieriColumnAdd(HookTabloid, List) := opts -> (T, K) -> (
    kk := hpValidateCoefficientRing("pieriColumnAdd", opts#CoefficientRing);
    p := hpHookArgument("pieriColumnAdd", T);
    hpValidateLetters("pieriColumnAdd", "K", K);
    if #K == 0 then error "pieriColumnAdd: K must be a nonempty list of letters";
    hpWithField(kk, () -> hpTabloidTerms hpPieriColumnAddHT(p#0, p#1, K))
);
pieriColumnAdd(List, List) := opts -> (T, K) -> pieriColumnAdd(tabloid T, K, CoefficientRing => opts#CoefficientRing);
pieriColumnAdd(Filling, List) := opts -> (T, K) -> pieriColumnAdd(tabloid T, K, CoefficientRing => opts#CoefficientRing);

-- the labeled Pieri matrix: the source and target bases as lists of
-- {T1, T2} HookTabloid pairs in the matrix cache
hpLabelPieriMatrix = (M, domPairs, codPairs) -> (
    M.cache#"sourceBasis" = apply(domPairs, p -> apply(p, hpNativeToTabloid));
    M.cache#"targetBasis" = apply(codPairs, p -> apply(p, hpNativeToTabloid));
    M
);

-- the block of one content in the order of the full matrix: the engine
-- hookInjectionMatrixWeight lists the pairs of a content bucket by
-- bucket, and the rows and columns are permuted here into the order of
-- the full injection matrix (first factor slow, second factor fast, each
-- in generator order), so that the block is literally the restriction of
-- the full matrix
hpOrderedWeightBlock = (a, b, c, d, n, wt) -> (
    data := hookInjectionMatrixWeight(a, b, c, d, n, wt);
    fullOrder := (pairs0, X1, X2) -> (
        i1 := hashTable apply(#X1, i -> X1#i => i);
        i2 := hashTable apply(#X2, i -> X2#i => i);
        apply(sort apply(#pairs0, k -> {i1#(pairs0#k#0) * #X2 + i2#(pairs0#k#1), k}), x -> x#1));
    colPerm := fullOrder(data#1, (hpWeightBasisBuckets(c, b, n))#0, (hpWeightBasisBuckets(a, d, n))#0);
    rowPerm := fullOrder(data#2, (hpWeightBasisBuckets(a, b, n))#0, (hpWeightBasisBuckets(c, d, n))#0);
    {(data#0)^rowPerm_colPerm, (data#1)_colPerm, (data#2)_rowPerm}
);

-- pieriMatrix(outerType, innerType, n): the injection
--   S_(c,1^b) (x) S_(a,1^d) -> S_(a,1^b) (x) S_(c,1^d)
-- for the nested hook types outerType = {a-1, b} and innerType = {c-1, d}
-- of the codomain, or its block of one content
pieriMatrix = method(Options => {Content => null, CoefficientRing => QQ}, TypicalValue => Matrix);
pieriMatrix(List, List, ZZ) := opts -> (outerType, innerType, n) -> (
    hpValidateTypePair("pieriMatrix", outerType, innerType);
    (a, b, c, d) := (outerType#0 + 1, outerType#1, innerType#0 + 1, innerType#1);
    hpValidateBatchArgs("pieriMatrix", a, b, c, d, n, opts#Content);
    K := hpValidateCoefficientRing("pieriMatrix", opts#CoefficientRing);
    hpWithField(K, () -> (
        data := if opts#Content === null then hpHookInjectionMatrixFull(a, b, c, d, n)
            else hpOrderedWeightBlock(a, b, c, d, n, opts#Content);
        hpLabelPieriMatrix(data#0, data#1, data#2)))
);

-- the labeled bases of a matrix returned by pieriMatrix or
-- frobeniusUnionMatrix
hpLabeledMatrixCache = (caller, M, key) -> (
    if not M.cache#?key then
        error(caller | ": expected a matrix returned by pieriMatrix or frobeniusUnionMatrix");
    M.cache#key
);
pieriSourceBasis = method(TypicalValue => List);
pieriSourceBasis Matrix := M -> hpLabeledMatrixCache("pieriSourceBasis", M, "sourceBasis");
pieriTargetBasis = method(TypicalValue => List);
pieriTargetBasis Matrix := M -> hpLabeledMatrixCache("pieriTargetBasis", M, "targetBasis");

-- pieriImage: one column of a labeled matrix, by index or by source
-- label, as target label => coefficient; or the image of one nested pair
-- of hook tabloids computed without the matrix.  The matrix forms read
-- the coefficient field from the matrix.
pieriImage = method(Options => {CoefficientRing => QQ}, TypicalValue => HashTable);
pieriImage(Matrix, ZZ) := opts -> (M, i) -> (
    src := pieriSourceBasis M;
    tgt := pieriTargetBasis M;
    if i < 0 or i >= #src then
        error("pieriImage: the source basis index must be between 0 and " | toString(#src - 1));
    col := flatten entries M_{i};
    hashTable for j from 0 to #tgt - 1 list (
        if col#j != 0 then tgt#j => col#j else continue)
);
pieriImage(Matrix, List) := opts -> (M, pair) -> (
    src := pieriSourceBasis M;
    key := hpPairLabel("pieriImage", pair);
    i := position(src, p -> p === key);
    if i === null then error "pieriImage: the pair is not in the source basis of the matrix";
    pieriImage(M, i)
);
pieriImage(HookTabloid, HookTabloid) := opts -> (T1, T2) -> (
    K := hpValidateCoefficientRing("pieriImage", opts#CoefficientRing);
    p1 := hpHookArgument("pieriImage", T1);
    p2 := hpHookArgument("pieriImage", T2);
    t := hpValidateNestedHooks("pieriImage", p1, p2);
    hpWithField(K, () -> hashTable apply(hpPieriInjectLocalHT(p1, p2, t),
        term -> apply(term#1, hpNativeToTabloid) => term#0))
);
pieriImage(List, List) := opts -> (T1, T2) -> pieriImage(tabloid T1, tabloid T2, CoefficientRing => opts#CoefficientRing);
pieriImage(Filling, Filling) := opts -> (T1, T2) -> pieriImage(tabloid T1, tabloid T2, CoefficientRing => opts#CoefficientRing);

-- the whole map as a hash table: every source pair to its image table
-- (target pair => coefficient); the symbolic form of a Pieri matrix
hookSymbolicForm = method(TypicalValue => HashTable);
hookSymbolicForm Matrix := M -> (
    src := pieriSourceBasis M;
    hashTable apply(#src, i -> src#i => pieriImage(M, i))
);

-- the complement data of a Pieri matrix: M completed by the unit vectors
-- of the nested semistandard target pairs to a square matrix and
-- inverted once, in the field of M; cached on the matrix
hpPieriComplement = M -> (
    if M.cache#?"pieriComplement" then return M.cache#"pieriComplement";
    src := pieriSourceBasis M;
    tgt := pieriTargetBasis M;
    isPair := p -> instance(p, List) and #p == 2 and all(p, T -> instance(T, HookTabloid));
    if #tgt == 0 or not all(tgt, isPair) or not all(src, isPair) then
        error "pieriPreimage: expected a matrix returned by pieriMatrix";
    cod := apply(tgt, p -> apply(p, T -> hpHookToNative("pieriPreimage", T)));
    a := #(cod#0#0#0) + 1; b := #(cod#0#0#1) - 1;
    c := #(cod#0#1#0) + 1; d := #(cod#0#1#1) - 1;
    kk := ring M;
    dimCod := numRows M;
    dimDom := numColumns M;
    goodIdx := select(toList(0..dimCod-1), idx ->
        hpIsNestedHookSSYTPair(cod#idx#0, cod#idx#1, a, b, c, d));
    if dimDom + #goodIdx != dimCod then
        error("pieriPreimage: the nested semistandard pairs do not complement the Pieri image: " |
            toString(#goodIdx) | " pairs where " | toString(dimCod - dimDom) | " are needed");
    G := mutableMatrix(kk, dimCod, #goodIdx, Dense => false);
    for k from 0 to #goodIdx-1 do G_(goodIdx#k, k) = 1;
    B := M | matrix G;
    Binv := id_(kk^dimCod) // B;
    if B * Binv != id_(kk^dimCod) then
        error("pieriPreimage: the Pieri complement matrix is singular in characteristic " |
            toString char kk);
    M.cache#"pieriComplement" = hashTable {"Binv" => Binv, "goodIdx" => goodIdx}
);

-- pieriPreimage(M, pair): the coefficients x on the source basis with
-- v = M x + w, where v is the unit vector of the target pair and w lies
-- in the span of the nested semistandard target pairs (the complement of
-- the image); equivalently (v - w) // M
pieriPreimage = method(TypicalValue => HashTable);
pieriPreimage(Matrix, List) := (M, pair) -> (
    src := pieriSourceBasis M;
    tgt := pieriTargetBasis M;
    key := hpPairLabel("pieriPreimage", pair);
    idx := position(tgt, p -> p === key);
    if idx === null then error "pieriPreimage: the pair is not in the target basis of the matrix";
    prep := hpPieriComplement M;
    coordCol := flatten entries (prep#"Binv")_{idx};
    hashTable for j from 0 to #src - 1 list (
        if coordCol#j != 0 then src#j => coordCol#j else continue)
);

------------------------------------------------------------
-- Row elimination systems (Section 5.2 of the paper): the lexicographic
-- closure of a row state and its coefficient matrix, its rows with a fixed
-- output row (the elimination submatrix), and the closed-formula version
-- solved by hpPieriRowSolveCached.
------------------------------------------------------------

------------------------------------------------------------
-- Closure process + coefficient matrix
------------------------------------------------------------

hpPieriLexClosureMatrix = method(Options => {BaseSymMon => null, KeyBase => 1000});

hpPieriLexClosureMatrix(List, List, ZZ) := o -> (rowTail0, colFull0, m) -> (
    if m < 1 then error "Need m >= 1.";
    if m > #rowTail0 then error("Need m <= #rowTail0 = " | toString(#rowTail0) | ".");

    base := o.BaseSymMon;
    if base === null then base = hpBetaMin(rowTail0,m);
    base = sort base;
    if #base =!= m then error("BaseSymMon must be a list of length " | toString m | ".");

    keyB := o.KeyBase;

    -- BFS over tableaux created by "move symMon to row of right tableau"
    tabs := new MutableList;
    queue := new MutableList;
    seen := new MutableHashTable;

    T0 := {sort rowTail0, sort colFull0};
    tabs#0 = T0;
    queue#0 = T0;
    seen#(toString T0) = true;

    -- store encountered row-keys {symMon, tabOut} with symMon >= base
    termData := new MutableHashTable;

    symMax := base;

    q := 0;
    while q < #queue do (
        T := queue#q; q = q + 1;

        HT := hpPieriRowRemoveHT(T#0, T#1, m);

        for key in keys HT do (
            coeff := HT#key;
            if coeff == 0 then continue;

            symMon := sort(key#0);
            tabOut := key#1;

            if hpLexGE(symMon, base) then (
                if hpLexGT(symMon, symMax) then symMax = symMon;

                tStr := toString({symMon, tabOut});
                if not termData#?tStr then termData#tStr = {symMon, tabOut};

                if hpLexGT(symMon, base) then (
                    htMove := hpJuxtaposeSymToRowHT(tabOut#0, tabOut#1, symMon);
                    for newTab in keys htMove do (
                        if htMove#newTab == 0 then continue;
                        newTabCan := {sort(newTab#0), sort(newTab#1)};
                        sNew := toString newTabCan;
                        if not seen#?sNew then (
                            seen#sNew = true;
                            tabs#(#tabs) = newTabCan;
                            queue#(#queue) = newTabCan;
                        );
                    );
                );
            );
        );
    );

    -- rows: list of {symMon, tabOut}, sorted by hpLexKey(symMon)
    terms := for k in keys termData list termData#k;
    terms = sort(terms, t -> hpLexKey(t#0, keyB));

    r := #terms;
    c := #tabs;

    rowIndex := new MutableHashTable;
    for i from 0 to r-1 do rowIndex#(toString terms#i) = i;

    Mmut := mutableMatrix((hpField()), r, c);

    -- fill columns by recomputing restricted Pieri images
    for j from 0 to c-1 do (
        T := tabs#j;
        HT := hpPieriRowRemoveHT(T#0, T#1, m);

        for key in keys HT do (
            coeff := HT#key;
            if coeff == 0 then continue;

            symMon := sort(key#0);
            tabOut := key#1;

            if hpLexGE(symMon, base) then (
                tStr := toString({symMon, tabOut});
                if rowIndex#?tStr then (
                    i := rowIndex#tStr;
                    Mmut_(i,j) = Mmut_(i,j) + coeff;
                );
            );
        );
    );

    out := new MutableHashTable;
    out#"base" = base;
    out#"symMax" = symMax;
    out#"tabs" = toList tabs;
    out#"terms" = terms;
    out#"M" = matrix Mmut;
    out
);

------------------------------------------------------------
-- The rows of hpPieriLexClosureMatrix whose output row tail equals FixedRow:
-- the elimination submatrix used by hpRowStepRank2Data.
--
-- Options:
--   BaseSymMon => null   : default hpBetaMin(rowTail0,m)
--   FixedRow   => null   : if null, fixedRow := rowTail0 \ BaseSymMon  (multiset diff)
--   KeyBase    => 1000   : passed through to hpPieriLexClosureMatrix
------------------------------------------------------------

hpPieriLexClosureSameRowSubmatrix = method(Options => {BaseSymMon => null, FixedRow => null, KeyBase => 1000});

hpPieriLexClosureSameRowSubmatrix(List, List, ZZ) := o -> (rowTail0, colFull0, m) -> (
    -- run the full closure
    data := hpPieriLexClosureMatrix(rowTail0, colFull0, m, BaseSymMon => o.BaseSymMon, KeyBase => o.KeyBase);

    base := data#"base";
    terms := data#"terms";
    tabs  := data#"tabs";
    M     := data#"M";

    -- choose the fixed rowTail to filter on
    fixedRow := o.FixedRow;
    if fixedRow === null then (
        fixedRow = hpMultisetDiff(sort rowTail0, base);
    ) else (
        fixedRow = sort fixedRow;
    );

    -- pick row indices whose tabOut has rowTail == fixedRow
    keepRows := {};
    for i from 0 to #terms-1 do (
        tabOut := (terms#i)#1;
        if sort(tabOut#0) == fixedRow then keepRows = keepRows | {i};
    );

    subM := submatrix(M, keepRows, toList(0..numColumns(M)-1));
    subTerms := for i in keepRows list terms#i;

    out := new MutableHashTable;
    out#"base" = base;
    out#"fixedRow" = fixedRow;
    out#"tabs" = tabs;              -- column labels (tableaux reached in closure)
    out#"terms" = terms;            -- all rows
    out#"keepRows" = keepRows;      -- indices kept
    out#"subTerms" = subTerms;      -- row labels kept
    out#"subM" = subM;              -- the extracted submatrix
    out
);

------------------------------------------------------------
hpPieriFixedRowClosureMatrixFormula = method(Options => {BaseSymMon => null, FixedRow => null});

hpPieriFixedRowClosureMatrixFormula(List, List, ZZ) := o -> (rowTail0, colFull0, m) -> (
    if m < 1 then error "Need m >= 1.";
    if m > #rowTail0 then error("Need m <= #rowTail0 = " | toString(#rowTail0) | ".");

    base := o.BaseSymMon;
    if base === null then base = sort hpBetaMin(rowTail0,m) else base = sort base;

    fixedRow := o.FixedRow;
    if fixedRow === null then fixedRow = hpMultisetDiff(sort rowTail0, base) else fixedRow = sort fixedRow;

    if #base =!= m then error("BaseSymMon must have length " | toString m | ".");
    if sort(rowTail0) =!= sort(fixedRow | base) then (
        error("rowTail0 must equal FixedRow union BaseSymMon as multisets.");
    );

    -- D = a+b-m = (#rowTail + #colFull - m)
    D := (#rowTail0 + #colFull0 - m);
    if D == 0 then error "Denominator D=a+b-m is 0.";
    kk := (hpField());
    invD := hpUnitAt(D, "the row elimination system with m = " | toString m |
        " on the hook (" | toString(#rowTail0 + 1) | ",1^" | toString(#colFull0 - 1) | ")");

    -- vertices are stored as {symMon, colFull} (both sorted lists)
    verts := new MutableList;
    queue := new MutableList;
    seen  := new MutableHashTable;

    v0 := {base, sort colFull0};
    verts#0 = v0;
    queue#0 = v0;
    -- use sequence keys (symMon, I) instead of toString for O(1) hashing
    seen#((v0#0, v0#1)) = true;

    addVertex := v -> (
        sk := (v#0, v#1);
        if not seen#?sk then (
            seen#sk = true;
            verts#(#verts) = v;
            queue#(#queue) = v;
        );
    );

    -- BFS closure
    q := 0;
    while q < #queue do (
        v := queue#q; q = q + 1;
        symMon := v#0;
        I := v#1;

        if not hpLexGE(symMon, base) then continue;

        Hsym := hpCountHT symMon;
        for i in I do (
            for s in keys Hsym do (
                if i == s then continue;
                if Hsym#s == 0 then continue;

                symMon2 := sort (hpRemoveOne(symMon,s) | {i});
                if not hpLexGE(symMon2, base) then continue;

                I2 := sort (hpRemoveOne(I,i) | {s});
                if #set(I2) =!= #I2 then continue;

                if hpLexGT(symMon2, base) then addVertex({symMon2, I2});
            );
        );
    );

    keyB := 1000;

    vertsList := toList verts;
    vertsList = sort(vertsList, v -> (hpLexKey(v#0,keyB), hpLexKey(v#1,keyB)));

    N := #vertsList;

    idx := new MutableHashTable;
    for j from 0 to N-1 do (
        vj := vertsList#j;
        idx#((vj#0, vj#1)) = j;
    );

    Mmut := mutableMatrix((hpField()), N, N);

    for j from 0 to N-1 do (
        v := vertsList#j;
        tau := v#0;
        I := v#1;

        rowTail := sort (fixedRow | tau);

        bin := hpBinomMultiset(rowTail, tau);
        if bin == 0 then error "hpPieriFixedRowClosureMatrixFormula: tau is not a submultiset of the row tail";

        sumDiag := 0;
        Htau := hpCountHT tau;
        for i in I do (
            if Htau#?i then sumDiag = sumDiag + Htau#i;
        );
        diagCoeff := bin * (1_kk + sumDiag * invD);

        rDiag := idx#((tau, I));
        Mmut_(rDiag, j) = Mmut_(rDiag, j) + diagCoeff;

        for i in I do (
            sgn := hpSgnInSet(I,i);

            for s in keys Htau do (
                if i == s then continue;
                mult := Htau#s;
                if mult == 0 then continue;

                tau2 := sort (hpRemoveOne(tau,s) | {i});
                if not hpLexGE(tau2, base) then continue;

                Iminus := hpRemoveOne(I,i);
                W := {s} | Iminus;
                insSign := hpWedgeReorderSign(W);
                if insSign == 0 then continue;

                I2 := sort (Iminus | {s});
                k2 := (tau2, I2);
                if not idx#?k2 then continue;

                coeff := bin * mult * invD * (sgn*insSign);

                r2 := idx#k2;
                Mmut_(r2, j) = Mmut_(r2, j) + coeff;
            );
        );
    );

    out := new MutableHashTable;
    out#"base" = base;
    out#"fixedRow" = fixedRow;
    out#"verts" = vertsList;
    out#"M" = matrix Mmut;
    out
);

------------------------------------------------------------
-- Column elimination system (Section 5.3 of the paper): the Theta closure
-- of a column state and its coefficient matrix, solved by hpThetaSolveCached.
------------------------------------------------------------
hpThetaEliminationMatrix = method(Options => {BaseAlpha => null, Pivot => null, BadSet => null});

hpThetaEliminationMatrix(List, List, ZZ) := o -> (K0, T0, t) -> (
    if not instance(K0, List) then error "K0 must be a list.";
    K0 = sort K0;
    if #set(K0) =!= #K0 then error "K0 must have no repeats.";
    if t =!= #K0 then error("Need t = #K0 = " | toString(#K0) | ".");
    if not instance(T0, List) or #T0 =!= 2 then error "T0 must be {rowTail0, colFull(I)}.";

    rowTail0 := sort(T0#0);
    I := sort(T0#1);

    -- pivot: "smallest element being moved onto the column"
    pivot := o#Pivot;
    if pivot === null then pivot = K0#0;

    -- determine badSet unless supplied:
    -- badSet = { s in supp(rowTail0) : s > pivot, s not in I, s not in K0 }
    badSet := o#BadSet;
    if badSet === null then (
        supp := sort toList set rowTail0;
        badSet = select(supp, s -> (s > pivot) and (not member(s,I)) and (not member(s,K0)));
    ) else (
        badSet = sort toList set badSet;
    );

    -- BaseAlpha is the row tail of the target state; here it is only
    -- length-checked and echoed as out#"baseAlpha".  The callers
    -- (hpThetaSolveCached, hpColStepRank2Data) locate the target state from
    -- their own arguments and key their caches with it.
    baseAlpha := o#BaseAlpha;
    if baseAlpha === null then baseAlpha = rowTail0 else baseAlpha = sort baseAlpha;
    if #baseAlpha =!= #rowTail0 then (
        error("BaseAlpha must have same length as rowTail0 = " | toString(#rowTail0) | ".");
    );

    ------------------------------------------------------------
    -- BFS closure on states (alpha,K), fixed I,
    -- using ONLY swaps that move s ∈ badSet from row to column.
    ------------------------------------------------------------
    states := new MutableList;
    queue := new MutableList;
    seen := new MutableHashTable;

    st0 := {rowTail0, K0};
    states#0 = st0;
    queue#0 = st0;
    -- use sequence keys (alpha, K) instead of toString for O(1) hashing
    seen#((st0#0, st0#1)) = true;

    addState := st -> (
        sk := (st#0, st#1);
        if not seen#?sk then (
            seen#sk = true;
            states#(#states) = st;
            queue#(#queue) = st;
        );
    );

    q := 0;
    while q < #queue do (
        st := queue#q; q = q + 1;
        alpha := st#0;
        K := sort st#1;

        alphaCounts := hpCountHT(alpha);

        -- for each u ∈ K and each bad s present in alpha, swap s into K in place of u
        for u in K do (
            Kminus := hpRemoveOne(K,u);
            for s in badSet do (
                if not alphaCounts#?s then continue;
                if alphaCounts#s <= 0 then continue;

                -- wedge nonzero constraints: s not in I and s not already in Kminus
                if member(s, I) then continue;
                if member(s, Kminus) then continue;

                -- new state
                alpha2 := sort (hpRemoveOne(alpha,s) | {u});
                K2 := sort (Kminus | {s});
                if #set(K2) =!= #K2 then continue; -- safety

                addState({alpha2, K2});
            );
        );
    );

    stList := toList states;

    ------------------------------------------------------------
    -- Build square matrix: rows/cols indexed by stList
    -- We project Θ_t to the span of { (alpha,K) in stList }.
    ------------------------------------------------------------
    N := #stList;
    idx := new MutableHashTable;
    for j from 0 to N-1 do (
        sj := stList#j;
        idx#((sj#0, sj#1)) = j;
    );

    Mmut := mutableMatrix((hpField()), N, N);

    for j from 0 to N-1 do (
        st := stList#j;
        alpha := st#0;
        K := st#1;

        Himg := hpPsiThetaHT(K, {alpha, I});
        for Tout in keys Himg do (
            coeff := Himg#Tout;
            if coeff == 0 then continue;

            alphaOut := sort(Tout#0);
            colOut := sort(Tout#1);

            KOut := hpSetMinusList(colOut, I);
            if #KOut =!= t then continue;

            kOut := (alphaOut, sort KOut);

            if idx#?kOut then (
                i := idx#kOut;
                Mmut_(i,j) = Mmut_(i,j) + coeff;
            );
        );
    );

    out := new MutableHashTable;
    out#"I" = I;
    out#"pivot" = pivot;
    out#"badSet" = badSet;
    out#"baseAlpha" = baseAlpha;
    out#"states" = stList;
    out#"M" = matrix Mmut;
    out
);

-- SchurFunctors stores a Filling by columns, whereas the hook-straightening
-- code stores a tableau by rows.  Keep the conversion in one place and
-- validate the ragged shape before transposing: conjugate(F) silently drops
-- cells when the column heights are not weakly decreasing.
hpFillingToRows = (caller,F) -> (
    if not instance(F,Filling) then
        error(caller | ": expected a Filling");
    cols := toList F;
    if #cols == 0 then return {};
    if any(cols,col -> not instance(col,List)) then
        error(caller | ": Filling columns must be lists");
    if any(cols,col -> #col == 0) then
        error(caller | ": Filling columns must be nonempty");
    heights := apply(cols,col -> #col);
    if #heights > 1 and
       any(1..#heights-1,j -> heights#(j-1) < heights#j) then
        error(caller | ": Filling column heights must form a partition");
    apply(heights#0,i ->
        for j from 0 to #cols-1 when i < heights#j list (cols#j)#i)
);

hpRowsToFilling = (caller,T) -> (
    if instance(T,Filling) then (
        hpFillingToRows(caller,T);
        return T;
    );
    if not instance(T,BasicList) then
        error(caller | ": expected a tableau as a list of rows");
    rows := if instance(T,List) then T else toList T;
    if not instance(rows,List) or
       any(rows,row -> not instance(row,List)) then
        error(caller | ": expected a tableau as a list of rows");
    if any(rows,row -> #row == 0) then
        error(caller | ": tableau rows must be nonempty");
    lengths := apply(rows,row -> #row);
    if #lengths > 1 and
       any(1..#lengths-1,i -> lengths#(i-1) < lengths#i) then
        error(caller | ": row lengths must form a partition");
    if rows == {} then return new Filling from {};
    new Filling from apply(lengths#0,j ->
        for i from 0 to #rows-1 when j < lengths#i list (rows#i)#j)
);

hpToRows = T -> (
    if instance(T,Filling) then hpFillingToRows("hpToRows",T)
    else if instance(T,List) then T
    else if instance(T,HashTable) then
        -- a normal form: the same table with row-list keys
        new HashTable from apply(pairs T, kv -> (hpToRows kv#0, kv#1))
    else toList T
);


-- Validate the row-reading format used by every public tabloid
-- straightener.  The empty tableau is allowed; otherwise the row lengths
-- form a partition and every entry is a positive integer.
hpValidateStraighteningRows = (caller,T) -> (
    if not instance(T,BasicList) then
        error(caller | ": expected a hook tabloid as a list of rows");
    T = if instance(T,Filling) then hpFillingToRows(caller,T)
        else toList T;
    if not instance(T,List) or any(T,row -> not instance(row,List)) then
        error(caller | ": expected a hook tabloid as a list of rows");
    if any(T,row -> #row == 0) then
        error(caller | ": tableau rows must be nonempty");
    lengths := apply(T,row -> #row);
    if #lengths > 1 and
       any(1..#lengths-1,i -> lengths#(i-1) < lengths#i) then
        error(caller | ": row lengths must form a partition");
    if any(flatten T,x -> not instance(x,ZZ) or x <= 0) then
        error(caller | ": tableau entries must be positive integers");
    T
);

hpValidateRankTwoRows = (caller,T) -> (
    T = hpValidateStraighteningRows(caller,T);
    if hpFrobeniusRank apply(T, row -> #row) != 2 then
        error(caller | ": expected a hook tabloid of Frobenius rank two");
    T
);

hpIsCanonicalHook = hook -> (
    rowTail := hook#0;
    colFull := hook#1;
    #colFull > 0 and
    colFull === sort colFull and #colFull == #(set colFull) and
    rowTail === sort rowTail and
    (#rowTail == 0 or rowTail#0 >= colFull#0)
);

------------------------------------------------------------
-- Convert a rank-2 nested tableau (row-reading form)
-- into its two internal hooks {rowTail, colFull}.
------------------------------------------------------------

hpNestedToHooksInternal = T -> (
    T = hpToRows T;
    if #T < 2 then error "expected a hook tabloid of Frobenius rank two (need at least 2 rows)";

    outerCorner := (T#0)#0;
    outerRowTail := if #(T#0) <= 1 then {} else drop(T#0,1);
    outerColFull := {outerCorner} | apply(drop(T,1), r -> r#0);

    if #(T#1) < 2 then error "expected a hook tabloid of Frobenius rank two (the second row must have at least 2 entries)";
    innerCorner := (T#1)#1;
    innerRowTail := if #(T#1) <= 2 then {} else drop(T#1,2);

    innerLeg := {};
    for i from 2 to #T-1 do (
        if #(T#i) >= 2 then innerLeg = innerLeg | {(T#i)#1};
    );
    innerColFull := {innerCorner} | innerLeg;

    {
        {outerRowTail, outerColFull},
        {innerRowTail, innerColFull}
    }
);

------------------------------------------------------------
-- First row obstruction:
-- scan rows from top to bottom, starting with row 2,
-- and detect first row where first entry > second entry.
-- Returns 0-based row index, or null.
------------------------------------------------------------

hpFirstRowObstructionRank2 = T -> (
    T = hpToRows T;
    for r from 1 to #T-1 do (
        if #(T#r) >= 2 then (
            if (T#r)#0 > (T#r)#1 then return r;
        );
    );
    null
);

------------------------------------------------------------
-- First column obstruction:
-- scan overlapping columns in rows 1 and 2, starting at column 2,
-- and detect first c with T_(1,c) >= T_(2,c).
-- Returns 0-based column index, or null.
------------------------------------------------------------

hpFirstColObstructionRank2 = T -> (
    T = hpToRows T;
    if #T < 2 then return null;
    maxOverlap := min(#(T#0), #(T#1)) - 1;
    for c from 1 to maxOverlap do (
        if (T#0)#c >= (T#1)#c then return c;
    );
    null
);

------------------------------------------------------------
-- Local obstruction data of a rank-two tabloid (Section 6 of the paper):
-- a row obstruction is resolved by the column triple move, a column
-- obstruction by the row move with the bumped row factor.
------------------------------------------------------------

-- kind is "Automatic" (a row obstruction takes priority, as in the
-- algorithm), "row" or "column" (resolve that obstruction, which must be
-- present).  The keys carry both the descriptive names and the paper's:
-- in the row case L0, I0, R0 and the triple T1, T2, T3 of Definition
-- def:rank2-row-obstruction-data, in the column case tau0, rho, sigma and
-- T1, T2, T3 of def:rank2-col-obstruction-data; obstructionIndex is the
-- paper's k (equal to the zero-based tableau index).
hpRank2LocalObstructionData = (T,kind) -> (
    T = hpToRows T;
    hooks := hpNestedToHooksInternal T;
    Touter := hooks#0;
    Tinner := hooks#1;

    outerRowTail := sort(Touter#0);
    outerColFull := sort(Touter#1);
    innerRowTail := sort(Tinner#0);
    innerColFull := sort(Tinner#1);

    --------------------------------------------------------
    -- Priority 1: row obstruction ==> column triple move
    --------------------------------------------------------
    rObs := hpFirstRowObstructionRank2 T;
    cObs := hpFirstColObstructionRank2 T;
    -- hpFail, not error: these two are user-facing validation messages, and a
    -- straightening runs inside hpWithField, which can only re-raise a message
    -- the package recorded; a plain error here reaches the user as the generic
    -- "an error that the package did not record" fallback instead
    if kind === "row" and rObs === null then
        hpFail "straighteningStep: this tabloid has no row obstruction (Obstruction => \"row\")";
    if kind === "column" and cObs === null then
        hpFail "straighteningStep: this tabloid has no column obstruction (Obstruction => \"column\")";
    if rObs =!= null and kind =!= "column" then (
        L0 := drop(outerColFull, rObs);
        if #L0 == 0 then error "hpRank2LocalObstructionData: empty moved column tail.";
        ell := L0#0;

        I0 := {};
        for x in innerColFull do (
            if (x > ell) or member(x,L0) then I0 = I0 | {x};
        );
        I0 = sort I0;

        Jminus := take(outerColFull, rObs);
        Krest0 := hpSetMinusList(innerColFull, I0);
        Kmid0 := sort(Krest0 | L0);

        return hashTable{
            "obstructionType" => "row",
            "obstructionRowIndex0" => rObs,
            "obstructionRowIndex1" => rObs + 1,
            "obstructionIndex" => rObs,
            "L0" => L0, "I0" => I0, "R0" => sort Krest0,
            "T1" => {outerRowTail, Jminus}, "T2" => {innerRowTail, Kmid0}, "T3" => I0,

            "outerHook" => {outerRowTail, outerColFull},
            "innerHook" => {innerRowTail, innerColFull},

            "movedColumnTail" => L0,
            "pivot" => ell,
            "bumpedColumnSegment" => I0,

            "leftHook" => {outerRowTail, Jminus},
            "middleHook" => {innerRowTail, Kmid0},
            "thirdFactorKind" => "wedge",
            "thirdFactor" => I0
        };
    );

    --------------------------------------------------------
    -- Priority 2: column obstruction ==> row move
    --
    -- If obstruction is at tableau column cObs+1, then:
    --   tau0      = outerRowTail tail starting there
    --   innerKeep = innerRowTail prefix before that spot
    --   bumpRow   = innerRowTail tail from that spot onward
    --------------------------------------------------------
    if cObs =!= null then (
        splitIdx := cObs - 1;
        if splitIdx < 0 then error "hpRank2LocalObstructionData: bad split index in column case.";

        tau0 := drop(outerRowTail, splitIdx);
        if #tau0 == 0 then error "hpRank2LocalObstructionData: empty moved row tail.";

        innerKeep := take(innerRowTail, splitIdx);
        bumpRow := drop(innerRowTail, splitIdx);

        leftHook := {take(outerRowTail, splitIdx), outerColFull};
        middleHook := {sort(innerKeep | tau0), innerColFull};

        return hashTable{
            "obstructionType" => "column",
            "obstructionColIndex0" => cObs,
            "obstructionColIndex1" => cObs + 1,
            "obstructionIndex" => cObs,
            "tau0" => tau0, "rho" => sort innerKeep, "sigma" => bumpRow,
            "T1" => leftHook, "T2" => middleHook, "T3" => bumpRow,

            "outerHook" => {outerRowTail, outerColFull},
            "innerHook" => {innerRowTail, innerColFull},

            "movedRowTail" => tau0,
            "fixedRightRow" => sort innerKeep,
            "bumpedRowTail" => bumpRow,

            "leftHook" => leftHook,
            "middleHook" => middleHook,
            "thirdFactorKind" => "row",
            "thirdFactor" => bumpRow
        };
    );

    hashTable{
        "obstructionType" => "none",
        "tableau" => T,
        "outerHook" => {outerRowTail, outerColFull},
        "innerHook" => {innerRowTail, innerColFull},
        "message" => "No row or column obstruction detected."
    }
);

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

hpSingletonHookHT = T -> (
    H := new MutableHashTable;
    H#T = 1_(hpField());
    H
);

------------------------------------------------------------
-- Re-nest two internal hooks {rowTail,colFull} into rows
------------------------------------------------------------

hpHooksInternalToNestedRows = (Touter, Tinner) -> (
    outerRowTail := Touter#0;
    outerColFull := Touter#1;
    innerRowTail := Tinner#0;
    innerColFull := Tinner#1;

    rows := {};

    rows = rows | {{outerColFull#0} | outerRowTail};

    if #outerColFull >= 2 then (
        rows = rows | {{outerColFull#1, innerColFull#0} | innerRowTail};
    );

    for r from 2 to #outerColFull-1 do (
        rowNow := {outerColFull#r};
        if (r-1) < #innerColFull then rowNow = rowNow | {innerColFull#(r-1)};
        rows = rows | {rowNow};
    );

    rows
);

------------------------------------------------------------
-- Psi_row evaluator: the row-move image of a middle-hook combination
------------------------------------------------------------

hpPsiRowEvaluateHT = (leftHook, midComb, bumpRow, m) -> (
    Hout := new MutableHashTable;

    for termIndex from 0 to #midComb-1 do (
        termNow := midComb#termIndex;
        coeffMidInput := termNow#0;
        Tmid := termNow#1;

        Hmid := hpPieriRowRemoveHT(Tmid#0, Tmid#1, m);

        for keyMid in keys Hmid do (
            coeffMid := coeffMidInput * Hmid#keyMid;
            if coeffMid != 0 then (
                symMon := sort(keyMid#0);
                rightBase := keyMid#1;

                Hleft := hpJuxtaposeSymToRowHT(leftHook#0, leftHook#1, symMon);

                Hright := null;
                if #bumpRow == 0 then (
                    Hright = hpSingletonHookHT(rightBase);
                ) else (
                    Hright = hpJuxtaposeSymToRowHT(rightBase#0, rightBase#1, bumpRow);
                );

                for leftKey in keys Hleft do (
                    coeffLeft := Hleft#leftKey;
                    if coeffLeft != 0 then (
                        for rightKey in keys Hright do (
                            coeffRight := Hright#rightKey;
                            if coeffRight != 0 then (
                                coeffTotal := coeffMid * coeffLeft * coeffRight;
                                if coeffTotal != 0 then (
                                    nestedTab := hpHooksInternalToNestedRows(leftKey, rightKey);
                                    hpAddTerm(Hout, nestedTab, coeffTotal);
                                );
                            );
                        );
                    );
                );
            );
        );
    );

    hpFreezeHT Hout
);

------------------------------------------------------------
-- Normalize a raw image hash table by the coefficient of orig,
-- then return the rewrite orig = - (other terms)
--
-- Input:
--   Himg : ordinary HashTable or MutableHashTable
--   orig : tableau key that should appear in Himg
--
-- Output:
--   ordinary HashTable of the rewrite terms
------------------------------------------------------------

hpNormalizeRewriteHT = (Himg, orig) -> (
    cOrig := if Himg#?orig then Himg#orig else 0;
    if cOrig == 0 then error "hpNormalizeRewriteHT: original tableau not present in image.";

    Hout := new MutableHashTable;

    for key in keys Himg do (
        if key =!= orig then (
            cc := -(Himg#key / cOrig);
            if cc != 0 then Hout#key = cc;
        );
    );

    hpFreezeHT Hout
);

------------------------------------------------------------
-- Canonical-relabel cache for the Pieri/theta linear solves.
--
-- The matrices built by hpPieriFixedRowClosureMatrixFormula and
-- hpThetaEliminationMatrix, as well as the BFS state lists they are
-- indexed by, are invariant under any *order-preserving* relabeling
-- of the literal values that appear in the inputs.  Concretely:
--
--   * The BFS transitions depend only on multiplicities and lex
--     comparisons, both preserved by order-preserving relabel.
--   * hpBinomMultiset, hpSgnInSet, hpWedgeReorderSign all depend on
--     multiplicities or relative position in sorted order.
--   * The implicit target row of the linear system is determined by
--     the canonical inputs alone.
--
-- We canonicalize the inputs to labels 1..n (sorted distinct values)
-- and cache the canonical (verts, coeffVec) result.  Cache hits skip
-- the BFS, the matrix build, and the linear solve entirely.
------------------------------------------------------------

-- Build an order-preserving relabel sending the union of the supplied
-- lists to {1..n}, and return the inverse map (literal-value lookup).
hpCanonRelabelMap = lists -> (
    seen := new MutableHashTable;
    for L in lists do for x in L do seen#x = true;
    sortedVals := sort keys seen;
    fwd := new MutableHashTable;
    for i from 0 to #sortedVals - 1 do fwd#(sortedVals#i) = i + 1;
    -- inverse map is just sortedVals (canonical i+1 -> sortedVals#i)
    (fwd, sortedVals)
);

hpApplyRelabel = (fwd, L) -> apply(L, x -> fwd#x);
hpApplyInvRelabel = (inv, L) -> apply(L, c -> inv#(c-1));

-- Cache, one table per coefficient field (hpFieldCache): canonical inputs
-- -> (canonical states, coeffVec).  coeffVec is label-independent, so the
-- canonical states can be relabeled back.
pieriRowSolveCache = new MutableHashTable;

-- Same idea but for hpThetaEliminationMatrix (used by hpColStepRank2).
thetaSolveCache = new MutableHashTable;

-- Solve M x = e_target for the elimination matrix M built by buildData(),
-- where the target is the unique state satisfying isTarget, and memoize the
-- pair (states, x) per field under cKey.
hpCachedUnitSolve = (cacheTable, cKey, buildData, statesKey, isTarget, label) -> (
    cache := hpFieldCache(cacheTable, hpField());
    if cache#?cKey then return cache#cKey;
    data := buildData();
    Mc := data#"M";
    states := data#statesKey;
    if numRows Mc =!= numColumns Mc then
        error("HookPieri: the " | label | " elimination matrix is not square");
    targetCanon := position(states, isTarget);
    if targetCanon === null then
        error("HookPieri: the " | label | " elimination target was not found");
    eC := hpStdBasisVec(numRows Mc, targetCanon);
    x := eC // Mc;
    if Mc * x != eC then hpFail(
        "HookPieri: a " | label | " elimination system of size " | toString numRows Mc |
        " is singular in characteristic " | toString char (hpField()));
    cache#cKey = (states, x)
);

hpPieriRowSolveCached = (rowTail0, colFull0, m, base, fixedRow) -> (
    (fwd, inv) := hpCanonRelabelMap({rowTail0, colFull0, base, fixedRow});
    cR := hpApplyRelabel(fwd, rowTail0);
    cC := hpApplyRelabel(fwd, colFull0);
    cB := hpApplyRelabel(fwd, base);
    cF := hpApplyRelabel(fwd, fixedRow);
    cBSorted := sort cB;
    cCSorted := sort cC;
    (vertsCanon, coeffVec) := hpCachedUnitSolve(pieriRowSolveCache, (cR, cC, m, cB, cF),
        () -> hpPieriFixedRowClosureMatrixFormula(cR, cC, m, BaseSymMon => cB, FixedRow => cF),
        "verts", v -> sort(v#0) == cBSorted and sort(v#1) == cCSorted, "row");
    -- relabel canonical verts back to literal labels
    vertsLit := apply(vertsCanon, v -> {hpApplyInvRelabel(inv, v#0), hpApplyInvRelabel(inv, v#1)});
    (vertsLit, coeffVec)
);
-- Wrapper around hpThetaEliminationMatrix.  Inputs are canonicalized to
-- 1..n; the cache stores (states, coeffVec) for the implicit target
-- target = position of (sort baseAlpha, sort K0) in states.
hpThetaSolveCached = (K0, T0, t, baseAlpha, pivot) -> (
    rowTail0Lit := sort(T0#0);
    ILit        := sort(T0#1);
    K0Lit       := sort K0;
    baseAlphaLit := sort baseAlpha;
    (fwd, inv) := hpCanonRelabelMap({K0Lit, rowTail0Lit, ILit, baseAlphaLit, {pivot}});
    cK  := hpApplyRelabel(fwd, K0Lit);
    cR  := hpApplyRelabel(fwd, rowTail0Lit);
    cI  := hpApplyRelabel(fwd, ILit);
    cBA := hpApplyRelabel(fwd, baseAlphaLit);
    cPivot := fwd#pivot;
    cBASorted := sort cBA;
    cKSorted  := sort cK;
    (stCanon, coeffVec) := hpCachedUnitSolve(thetaSolveCache, (cK, cR, cI, t, cBA, cPivot),
        () -> hpThetaEliminationMatrix(cK, {cR, cI}, t, BaseAlpha => cBA, Pivot => cPivot),
        "states", st -> sort(st#0) == cBASorted and sort(st#1) == cKSorted, "column");
    stLit := apply(stCanon, st -> {hpApplyInvRelabel(inv, st#0), hpApplyInvRelabel(inv, st#1)});
    (stLit, coeffVec)
);
------------------------------------------------------------
-- Row step: resolves a column obstruction (hpRowStepRank2)
------------------------------------------------------------

hpRowStepRank2 = (T,D) -> (
    T = hpToRows T;
    if D#"obstructionType" =!= "column" then (
        error("hpRowStepRank2: expected column obstruction, got "
            | toString(D#"obstructionType"));
    );

    tau0 := sort(D#"movedRowTail");
    fixedRow := sort(D#"fixedRightRow");
    leftHook := D#"leftHook";
    middleHook := D#"middleHook";
    bumpRow := sort(D#"thirdFactor");

    m := #tau0;
    if m < 1 then error "hpRowStepRank2: m=0.";

    -- canonical-relabel cached solve: (verts, coeffVec) for the
    -- target = (sort tau0, sort middleHook#1)
    (verts, coeffVec) := hpPieriRowSolveCached(middleHook#0, middleHook#1, m, tau0, fixedRow);

    midComb := {};
    for j from 0 to #verts-1 do (
        cj := coeffVec_(j,0);
        if cj != 0 then (
            v := verts#j;
            tau := sort(v#0);
            Icol := sort(v#1);
            Tmid := {sort(fixedRow | tau), Icol};
            midComb = midComb | {{cj, Tmid}};
        );
    );

    Himg := hpPsiRowEvaluateHT(leftHook, midComb, bumpRow, m);
    hpNormalizeRewriteHT(Himg, hpToRows T)
);

------------------------------------------------------------
-- Semistandardness of a rank-two tabloid
------------------------------------------------------------

-- the predicate used inside the engines on tabloids they generated
-- themselves (always valid rank-two row forms)
hpIsSemistandardRank2Unchecked = T -> (
    T = hpToRows T;
    hpNestedToHooksInternal T;
    hpFirstRowObstructionRank2 T === null and hpFirstColObstructionRank2 T === null
);


-- diagonal hook shapes in Frobenius form:
-- returns list of pairs {rowLength, legLength}
hpDiagonalHookShapes = lam -> (
    lam = hpAsList lam;
    conj := hpConjugatePartition lam;
    d := hpFrobeniusRank lam;
    for i from 1 to d list { lam#(i-1) - i + 1, conj#(i-1) - i }
);

------------------------------------------------------------
-- Assemble a tuple of diagonal-hook tableaux into one tableau
-- in row-reading form.
--
-- Input:
--   lam   = partition shape
--   hooks = list of hook tableaux, one for each diagonal hook,
--           in order i=1,...,hpFrobeniusRank(lam)
--
-- Output:
--   list of rows of the full hook tabloid
------------------------------------------------------------

hpAssembleHookTabloid = (lam, hooks) -> (
    lam = hpAsList lam;
    hooks = hpAsList hooks;

    d := hpFrobeniusRank lam;
    if #hooks =!= d then (
        error("hpAssembleHookTabloid: expected " | toString d
            | " hooks, got " | toString(#hooks) | ".");
    );

    rowsM := new MutableList;
    for i from 0 to #lam-1 do (
        row := new MutableList;
        for j from 0 to lam#i-1 do row#j = null;
        rowsM#i = row;
    );

    for i from 1 to d do (
        T := hooks#(i-1);
        rowTail := T#0;
        colFull := T#1;

        a := #rowTail + 1;    -- first row length of hook
        b := #colFull - 1;    -- leg length of hook

        -- place the corner and row part in row i
        (rowsM#(i-1))#(i-1) = colFull#0;
        for j from 1 to a-1 do (
            (rowsM#(i-1))#(i-1+j) = rowTail#(j-1);
        );

        -- place the column part below the corner
        for t from 1 to b do (
            (rowsM#(i-1+t))#(i-1) = colFull#t;
        );
    );

    out := {};
    for i from 0 to #lam-1 do (
        row := toList(rowsM#i);
        if #select(row, x -> x === null) > 0 then (
            error("hpAssembleHookTabloid: failed to fill all boxes in row " | toString(i+1));
        );
        out = out | {row};
    );

    out
);

------------------------------------------------------------
-- Enumerate the semistandard hook tableaux (hookSSYTBasis) on each
-- diagonal hook of a shape
------------------------------------------------------------

hpDiagonalHookBases = (lam, n) -> (
    shs := hpDiagonalHookShapes lam;
    for sh in shs list hookSSYTBasis(sh#0, sh#1, n)
);

------------------------------------------------------------
-- Enumerate ALL hook tabloids of shape lam on alphabet 1..n
--
-- Output variants:
--   hpAllHookTabloidHookData(lam,n)  = list of tuples of hook tableaux
--   hpAllHookTabloids(lam,n)         = list of full tableaux in row-reading form
------------------------------------------------------------

hpAllHookTabloidHookData = (lam, n) -> (
    hookBases := hpDiagonalHookBases(lam, n);
    hpCartesianProductLists hookBases
);

hpAllHookTabloids = (lam, n) -> (
    apply(hpAllHookTabloidHookData(lam,n), hooks -> hpAssembleHookTabloid(lam, hooks))
);

------------------------------------------------------------
-- Enumerate hook tabloids of FIXED content wt
--
-- wt is a list of length n:
--   wt#0 = number of 1's,
--   wt#1 = number of 2's, etc.
------------------------------------------------------------

-- memoized on (position, remaining content): the suffix enumeration for a
-- given remaining content is shared by every prefix that leaves it
hpHookTupleWeightRec = (hookBases, n, idx, remWt) -> (
    memo := new MutableHashTable;
    weights := apply(hookBases, base -> apply(base, T -> hpHookWeight(T, n)));
    rec := (k, rem) -> (
        if k == #hookBases then (
            if hpWeightTotal(rem) == 0 then return {{}};
            return {};
        );
        key := (k, rem);
        if memo#?key then return memo#key;
        out := new MutableList;
        base := hookBases#k;
        for t from 0 to #base-1 do (
            rem2 := hpWeightSub(rem, weights#k#t);
            if hpWeightNonneg(rem2) then (
                T := base#t;
                for rest in rec(k+1, rem2) do out#(#out) = {T} | rest;
            );
        );
        memo#key = toList out;
        memo#key
    );
    rec(idx, remWt)
);

hpAllHookTabloidHookDataWeight = (lam, n, wt) -> (
    lam = hpAsList lam;
    wt = hpAsList wt;

    if #wt =!= n then error("hpAllHookTabloidHookDataWeight: wt must have length n = " | toString n);

    hookBases := hpDiagonalHookBases(lam, n);
    totalBoxes := sum lam;
    if hpWeightTotal(wt) =!= totalBoxes then (
        error("hpAllHookTabloidHookDataWeight: sum(wt)=" | toString(hpWeightTotal wt)
            | " but shape has " | toString totalBoxes | " boxes.");
    );

    hpHookTupleWeightRec(hookBases, n, 0, wt)
);

hpAllHookTabloidsWeight = (lam, n, wt) -> (
    apply(hpAllHookTabloidHookDataWeight(lam,n,wt), hooks -> hpAssembleHookTabloid(lam, hooks))
);

hpSqfreeTabloids = lam -> (
    lam = hpAsList lam;
    n := sum lam;
    wt := for i from 1 to n list 1;
    hpAllHookTabloidsWeight(lam, n, wt)
);

-- The comparison of two tabloids of the same shape in the running
-- partial order of the paper, as the codes -1 (T before U), 1, 0 (equal)
-- and null (incomparable); the exported runningCompare returns symbols
hpRunningPreorderCompare = (T, U) -> (
    T = hpValidateStraighteningRows("hpRunningPreorderCompare", T);
    U = hpValidateStraighteningRows("hpRunningPreorderCompare", U);
    if hpShapeOfTabloid T != hpShapeOfTabloid U then
        error "hpRunningPreorderCompare: tabloids must have the same shape";
    N := max(hpMaxEntryTabloid T, hpMaxEntryTabloid U);
    hpRunningCompareVectors(hpRunningVectorsFast(T, N), hpRunningVectorsFast(U, N))
);

-- For each term U of one rank-two straightening step of T, the comparison
-- hpRunningPreorderCompare(U,T) in the running partial order of the paper:
-- every value is -1 when the step is triangular (null marks an
-- incomparable term).  verifyTriangularity is the exported, general-rank
-- version.
hpCarefulStep = T -> (H := straightenStepRank2(T);
    applyPairs(H, (U,c) -> (U, hpRunningPreorderCompare(U,T)))
    )

------------------------------------------------------------
-- helper: canonicalize a wedge list
-- returns {sign, sortedList}; sign = 0 if repetition occurs
------------------------------------------------------------

hpCanonicalWedgeData = L -> (
    L = toList L;
    S := sort L;
    if #set(S) =!= #S then return {0, {}};

    invCount := 0;
    if #L > 1 then (
        for i from 0 to #L-1 do (
            for j from i+1 to #L-1 do (
                if L#i > L#j then invCount = invCount + 1;
            );
        );
    );

    sgn := if (invCount % 2 == 0) then 1 else -1;
    {sgn, S}
);

------------------------------------------------------------
-- Transported Schur-side Pieri map, already straightened
--
-- Input:
--   Iw = wedge list to add to the column
--   T  = hook tableau {rowTail,colFull} in the Schur model
--
-- Output:
--   HashTable of semistandard Schur hook tableaux
--
-- Formula:
--   hpPsiThetaHT(Iw,T) = (1/N) * d( Iw wedge kappa(T) )
-- where N = #rowTail + #colFull = a+b.
--
-- hpPsiThetaHTRaw computes it; hpPsiThetaHT is the canonical-relabel cache in
-- front of it (see below).
------------------------------------------------------------

-- Canonical-relabel cache for hpPsiThetaHT: same observation as for the
-- elimination-matrix builders -- the function is invariant under any
-- order-preserving relabeling of values, since wedge signs depend on
-- relative order (preserved) and downstream hpStraightenHookHT is itself
-- relabel-invariant.  This is by far the hottest function in the
-- worklist (hpPsiColEvaluateHT calls it twice per (term, posList) pair).
psiThetaHTCache = new MutableHashTable;

hpPsiThetaHTRaw = (Iw, T) -> (
    if not instance(Iw, List) then error "hpPsiThetaHT: Iw must be a list.";
    if not instance(T, List) or #T =!= 2 then error "hpPsiThetaHT: T must be {rowTail,colFull}.";

    rowTail0 := sort toList (T#0);
    colFull0Raw := toList (T#1);
    IwRaw := toList Iw;

    -- canonicalize the input wedges
    dataCol0 := hpCanonicalWedgeData(colFull0Raw);
    sgnCol0 := dataCol0#0;
    colFull0 := dataCol0#1;

    dataIw := hpCanonicalWedgeData(IwRaw);
    sgnIw := dataIw#0;
    Iw0 := dataIw#1;

    if sgnCol0 == 0 or sgnIw == 0 then return new HashTable from {};

    N := #rowTail0 + #colFull0;   -- = a+b
    invN := hpUnitAt(N, "the column Pieri map on the hook (" |
        toString(#rowTail0 + 1) | ",1^" | toString(#colFull0 - 1) | ")");

    Hout := new MutableHashTable;

    addHookTerm := (coeff, rtRaw, cfRaw) -> (
        if coeff == 0 then return;
        ht := hpStraightenHookHT(rtRaw, cfRaw);
        for key in keys ht do hpAddTerm(Hout, key, coeff * ht#key);
    );

    --------------------------------------------------------
    -- First apply kappa: remove one column entry ci and move it to the row
    --------------------------------------------------------
    for i from 0 to #colFull0-1 do (
        ci := colFull0#i;
        colRem := hpRemoveAtPos(colFull0, i);

        -- sign from removing ci from the wedge colFull0
        sgnKappa := if (i % 2 == 0) then 1 else -1;

        ----------------------------------------------------
        -- Weyl-side naive column concatenation: Iw wedge colRem
        ----------------------------------------------------
        dataWeylCol := hpCanonicalWedgeData(Iw0 | colRem);
        sgnWeylCol := dataWeylCol#0;
        colWeyl := dataWeylCol#1;

        if sgnWeylCol == 0 then continue;

        rowWeyl := sort(rowTail0 | {ci});

        ----------------------------------------------------
        -- Then apply d: move one row entry x back to the column
        ----------------------------------------------------
        for j from 0 to #rowWeyl-1 do (
            x := rowWeyl#j;
            rowSchur := hpRemoveAtPos(rowWeyl, j);  -- still sorted

            dataSchurCol := hpCanonicalWedgeData({x} | colWeyl);
            sgnSchurCol := dataSchurCol#0;
            colSchur := dataSchurCol#1;

            if sgnSchurCol == 0 then continue;

            coeff :=
                (sgnCol0 * sgnIw * sgnKappa * sgnWeylCol * sgnSchurCol) * invN;

            addHookTerm(coeff, rowSchur, colSchur);
        );
    );

    new HashTable from Hout
);

-- Cached wrapper: canonical-relabel inputs and store result by canonical key.
hpPsiThetaHT = (Iw, T) -> (
    rt0 := sort toList (T#0);
    cf0 := toList (T#1);
    iw0 := toList Iw;

    (fwd, inv) := hpCanonRelabelMap({rt0, cf0, iw0});
    cRT := hpApplyRelabel(fwd, rt0);
    cCF := hpApplyRelabel(fwd, cf0);
    cIW := hpApplyRelabel(fwd, iw0);
    cKey := (cIW, cRT, cCF);

    cache := hpFieldCache(psiThetaHTCache, (hpField()));
    cached := if cache#?cKey then cache#cKey else (
        result := hpPsiThetaHTRaw(cIW, {cRT, cCF});
        cache#cKey = result;
        result
    );

    -- Relabel canonical-key entries back to literal labels.
    out := new MutableHashTable;
    for k in keys cached do (
        rtLit := hpApplyInvRelabel(inv, k#0);
        cfLit := hpApplyInvRelabel(inv, k#1);
        out#{rtLit, cfLit} = cached#k;
    );
    new HashTable from out
);

------------------------------------------------------------------------
-- hpPieriColumnAddHT: the column Pieri map Q'_m of Section 4 of the paper,
--   Q'_m : S_{(a,1^b)}(V) (x) Wedge^m V -> S_{(a,1^{b+m})}(V),
--   Q'_m(T^alpha_I (x) e_K) = T^alpha_{I wedge K}
--        - 1/(a+b+m) sum_{k in K} sgn(k,K) sum_j T^{alpha - eps_j + eps_k}_{...},
-- on the hook tableau T = {rowTail, colFull} and the wedge K.  The
-- internal evaluator hpPsiThetaHT computes the same map normalized by
-- 1/(a+b) instead of 1/(a+b+m) and with the opposite wedge order; the two
-- are proportional, and this wrapper rescales by (-1)^{bm} (a+b)/(a+b+m) so
-- that the output agrees with the paper's formula (Section 4).  The
-- column elimination matrices of Section 5 use hpPsiThetaHT directly.
-- Output hooks are straightened to the semistandard basis.
------------------------------------------------------------------------

hpPieriColumnAddHT = (rowTail, colFull, K) -> (
    if not instance(K, List) then error "hpPieriColumnAddHT: K must be a list of letters.";
    N := #rowTail + #colFull;       -- = a+b for the hook of shape (a,1^b)
    m := #K;
    if m < 1 then error "hpPieriColumnAddHT: K must be nonempty.";
    -- hpPsiThetaHT wedges K on the left of the (length b) column left by
    -- kappa, whereas the paper's mu_m wedges it on the right; the two differ
    -- by (-1)^{b m}.  Include that sign so that the output is literally the
    -- paper's Q'_m (verified against the closed formula of Section 4).
    scale := (-1)^((#colFull - 1) * m) * N * hpUnitAt(N + m,
        "the column Pieri map Q'_" | toString m | " on the hook (" |
        toString(#rowTail + 1) | ",1^" | toString(#colFull - 1) | ")");   -- (-1)^{bm} (a+b)/(a+b+m)
    H := hpPsiThetaHT(K, {rowTail, colFull});
    out := new MutableHashTable;
    for k in keys H do (
        c := scale * H#k;
        if c != 0 then out#k = c);
    new hpHookCombinationData from out
);

-- position helpers for the column split of hpPsiColEvaluateHT
hpSplitSignFromPositions = posList -> (
    e := 0;
    for j from 0 to #posList-1 do e = e + (posList#j - j);
    if (e % 2 == 0) then 1 else -1
);
hpSubsetByPositions = (L,posList) -> (for p in posList list L#p);
hpComplementByPositions = (L,posList) -> (
    flag := new MutableHashTable;
    for p in posList do flag#p = true;
    out := {};
    for q from 0 to #L-1 do if not flag#?q then out = out | {L#q};
    out
);

------------------------------------------------------------
-- Psi_col evaluator:
-- split off an m-subset L from the middle column,
-- map L onto T1, and map T3 onto the leftover of T2.
------------------------------------------------------------

hpPsiColEvaluateHT = (tripleComb, m) -> (
    Hout := new MutableHashTable;

    for term in tripleComb do (
        coeff0 := term#0;
        T1 := term#1;
        T2 := term#2;
        T3 := sort(term#3);

        rowMid := sort(T2#0);
        colMid := sort(T2#1);

        if m > #colMid then error "hpPsiColEvaluateHT: m larger than middle column length.";

        posLists := hpStrictSeqs(m, 0, #colMid-1);

        for posList in posLists do (
            L := hpSubsetByPositions(colMid, posList);
            restCol := hpComplementByPositions(colMid, posList);
            sgnSplit := hpSplitSignFromPositions(posList);

            Hleft := hpPsiThetaHT(L, T1);
            Hright := hpPsiThetaHT(T3, {rowMid, sort restCol});

            for Lkey in keys Hleft do (
                cL := Hleft#Lkey;
                if cL == 0 then continue;

                for Rkey in keys Hright do (
                    cR := Hright#Rkey;
                    if cR == 0 then continue;

                    coeff := coeff0 * sgnSplit * cL * cR;
                    if coeff == 0 then continue;

                    nested := hpHooksInternalToNestedRows(Lkey, Rkey);
                    hpAddTerm(Hout, nested, coeff);
                );
            );
        );
    );

    new HashTable from Hout
);

------------------------------------------------------------
-- Column step: resolves a row obstruction (hpColStepRank2) by the left and
-- right elimination systems of Section 5 of the paper and the transported
-- column Pieri map hpPsiThetaHT.
------------------------------------------------------------

hpColStepRank2 = (T,D) -> (
    T = hpToRows T;
    if D#"obstructionType" =!= "row" then (
        error("hpColStepRank2: expected row obstruction, got "
            | toString(D#"obstructionType"));
    );

    --------------------------------------------------------
    -- local triple data
    --------------------------------------------------------
    L0 := sort(D#"movedColumnTail");
    leftHook0 := D#"leftHook";
    middleHook0 := D#"middleHook";
    I0 := sort(D#"thirdFactor");

    m := #L0;
    if m < 1 then error "hpColStepRank2: m=0.";

    beta0 := sort(middleHook0#0);
    C0 := sort(middleHook0#1);
    R0 := hpSetMinusList(C0, L0);

    --------------------------------------------------------
    -- LEFT elimination system (canonical-relabel cached)
    --------------------------------------------------------
    (stL, cLvec) := hpThetaSolveCached(L0, leftHook0, m,
        sort(leftHook0#0), L0#0);

    leftTerms := {};
    for j from 0 to #stL-1 do (
        cj := cLvec_(j,0);
        if cj != 0 then leftTerms = leftTerms | {{cj, stL#j}};
    );

    --------------------------------------------------------
    -- RIGHT elimination system
    --
    -- built on the fixed column R0, not on the full middle column
    --------------------------------------------------------
    rightTerms := {};
    if #I0 == 0 then (
        rightTerms = {{1_(hpField()), {beta0, {}}}};
    ) else (
        (stR, cRvec) := hpThetaSolveCached(I0, {beta0, R0}, #I0, beta0, I0#0);

        for j from 0 to #stR-1 do (
            cj := cRvec_(j,0);
            if cj != 0 then rightTerms = rightTerms | {{cj, stR#j}};
        );
    );

    --------------------------------------------------------
    -- Lift to a triple combination
    --
    -- each left state is (alphaL, Kleft)
    -- each right state is (betaR, Iw)
    --
    -- middle column is R0 union Kleft
    --------------------------------------------------------
    tripleComb := {};

    for lt in leftTerms do (
        cL := lt#0;
        alphaL := sort((lt#1)#0);
        Kleft := sort((lt#1)#1);

        for rt in rightTerms do (
            cR := rt#0;
            betaR := sort((rt#1)#0);
            Iw := sort((rt#1)#1);

            Kmid := sort(R0 | Kleft);
            if #set(Kmid) =!= #Kmid then continue;

            T1 := {alphaL, sort(leftHook0#1)};
            T2 := {betaR, Kmid};

            tripleComb = tripleComb | {{cL*cR, T1, T2, Iw}};
        );
    );

    --------------------------------------------------------
    -- Evaluate Psi_col and normalize
    --------------------------------------------------------
    Himg := hpPsiColEvaluateHT(tripleComb, m);
    hpNormalizeRewriteHT(Himg, hpToRows T)
);

------------------------------------------------------------
-- Dispatcher
------------------------------------------------------------

-- straightenStepRank2, straightenStepRank2Data and the caches pieriRowSolveCache,
-- thetaSolveCache and psiThetaHTCache keep their unprefixed names: the paper's
-- audit scripts use them under debug.
straightenStepRank2 = T -> hpStraightenStepRank2(T,"Automatic");
hpStraightenStepRank2 = (T,which) -> (
    T = hpToRows T;
    D := hpRank2LocalObstructionData(T,which);
    kind := D#"obstructionType";

    if kind == "none" then (
        H := new MutableHashTable;
        H#T = 1_(hpField());
        return hpFreezeHT H;
    );

    -- a step is named by the direction it moves, an obstruction by the
    -- direction it violates: a column obstruction is removed by the row step
    if kind == "column" then return hpRowStepRank2(T,D);
    if kind == "row" then return hpColStepRank2(T,D);

    error("straightenStepRank2: unknown obstruction type " | toString kind);
);

------------------------------------------------------------
-- helper: normalize a raw image by the coefficient of orig
-- and also keep the intermediate data
------------------------------------------------------------

hpNormalizeRewriteHTData = (Himg, orig) -> (
    cOrig := if Himg#?orig then Himg#orig else 0;
    if cOrig == 0 then error "hpNormalizeRewriteHTData: original tableau not present in image.";

    Hnorm := new MutableHashTable;
    Hrew := new MutableHashTable;

    for key in keys Himg do (
        cc := Himg#key / cOrig;
        if cc != 0 then Hnorm#key = cc;
    );

    for key in keys Hnorm do (
        if key =!= orig then Hrew#key = -(Hnorm#key);
    );

    out := new MutableHashTable;
    out#"origCoeff" = cOrig;
    out#"normalizedImage" = hpFreezeHT Hnorm;
    out#"rewrite" = hpFreezeHT Hrew;
    new HashTable from out
);

------------------------------------------------------------
-- Row step, data version: the same computation as hpRowStepRank2, but
-- returning every intermediate (the obstruction data, the elimination
-- submatrix of hpPieriLexClosureSameRowSubmatrix, the solved coefficient
-- vector, the Psi_row images and the normalized rewrite) in one hash
-- table, for straighteningObstruction.
------------------------------------------------------------

hpRowStepRank2Data = (T,D) -> (
    T = hpToRows T;
    if D#"obstructionType" =!= "column" then (
        error("hpRowStepRank2Data: expected column obstruction, got "
            | toString(D#"obstructionType"));
    );

    tau0 := sort(D#"movedRowTail");
    fixedRow := sort(D#"fixedRightRow");
    bumpRow := sort(D#"thirdFactor");
    leftHook := D#"leftHook";
    middleHook := D#"middleHook";

    m := #tau0;
    if m < 1 then error "hpRowStepRank2Data: m=0.";

    --------------------------------------------------------
    -- elimination submatrix on the row side
    --------------------------------------------------------
    dataElim := hpPieriLexClosureSameRowSubmatrix(middleHook#0, middleHook#1, m,
        BaseSymMon => tau0,
        FixedRow => fixedRow
    );

    subM := dataElim#"subM";
    subTerms := dataElim#"subTerms";
    tabs := dataElim#"tabs";

    if numRows subM =!= numColumns subM then (
        error("hpRowStepRank2Data: expected square elimination matrix, got "
            | toString(numRows subM) | "x" | toString(numColumns subM));
    );

    --------------------------------------------------------
    -- target row = distinguished term
    --------------------------------------------------------
    targetIndex := null;
    for i from 0 to #subTerms-1 do (
        symMon := sort((subTerms#i)#0);
        tabOut := (subTerms#i)#1;
        if symMon == tau0 and sort(tabOut#0) == fixedRow and sort(tabOut#1) == sort(middleHook#1) then (
            targetIndex = i;
            break;
        );
    );
    if targetIndex === null then error "hpRowStepRank2Data: target row not found.";

    e := hpStdBasisVec(numRows subM, targetIndex);
    coeffVec := e // subM;

    if not (subM * coeffVec == e) then (
        error "hpRowStepRank2Data: elimination system not solved exactly.";
    );

    --------------------------------------------------------
    -- middle-hook linear combination used in the rewrite
    --------------------------------------------------------
    midComb := {};
    for j from 0 to #tabs-1 do (
        cj := coeffVec_(j,0);
        if cj != 0 then midComb = midComb | {{cj, tabs#j}};
    );

    --------------------------------------------------------
    -- evaluate Psi_row and normalize
    --------------------------------------------------------
    Himg := hpPsiRowEvaluateHT(leftHook, midComb, bumpRow, m);
    normData := hpNormalizeRewriteHTData(Himg, T);

    out := new MutableHashTable;
    out#"input" = T;
    out#"localObstructionData" = D;

    out#"m" = m;
    out#"tau0" = tau0;
    out#"fixedRow" = fixedRow;
    out#"bumpRow" = bumpRow;
    out#"leftHook" = leftHook;
    out#"middleHook" = middleHook;

    out#"eliminationData" = dataElim;
    out#"subM" = subM;
    out#"subTerms" = subTerms;
    out#"tabs" = tabs;
    out#"targetIndex" = targetIndex;
    out#"coeffVec" = coeffVec;
    out#"midComb" = midComb;

    out#"rawImage" = Himg;
    out#"origCoeffInRawImage" = normData#"origCoeff";
    out#"normalizedImage" = normData#"normalizedImage";
    out#"rewrite" = normData#"rewrite";

    new HashTable from out
);

------------------------------------------------------------
-- Column step, data version: the same computation as hpColStepRank2, but
-- returning every intermediate (the obstruction data, the left and right
-- elimination matrices of hpThetaEliminationMatrix with their solved
-- coefficient vectors, the hpPsiColEvaluateHT images and the normalized
-- rewrite) in one hash table, for straighteningObstruction.
------------------------------------------------------------

hpColStepRank2Data = (T,D) -> (
    T = hpToRows T;
    if D#"obstructionType" =!= "row" then (
        error("hpColStepRank2Data: expected row obstruction, got "
            | toString(D#"obstructionType"));
    );

    --------------------------------------------------------
    -- local triple data
    --------------------------------------------------------
    L0 := sort(D#"movedColumnTail");
    leftHook0 := D#"leftHook";
    middleHook0 := D#"middleHook";
    I0 := sort(D#"thirdFactor");

    m := #L0;
    if m < 1 then error "hpColStepRank2Data: m=0.";

    beta0 := sort(middleHook0#0);
    C0 := sort(middleHook0#1);
    R0 := hpSetMinusList(C0, L0);

    --------------------------------------------------------
    -- LEFT elimination system
    --------------------------------------------------------
    dataL := hpThetaEliminationMatrix(L0, leftHook0, m,
        BaseAlpha => sort(leftHook0#0),
        Pivot => L0#0
    );

    ML := dataL#"M";
    stL := dataL#"states";

    if numRows ML =!= numColumns ML then (
        error("hpColStepRank2Data: left elimination matrix not square: "
            | toString(numRows ML) | "x" | toString(numColumns ML));
    );

    idxL := null;
    for i from 0 to #stL-1 do (
        if sort((stL#i)#0) == sort(leftHook0#0) and sort((stL#i)#1) == L0 then (
            idxL = i;
            break;
        );
    );
    if idxL === null then error "hpColStepRank2Data: left target state not found.";

    eL := hpStdBasisVec(numRows ML, idxL);
    cLvec := eL // ML;
    if not (ML * cLvec == eL) then error "hpColStepRank2Data: left system not solved.";

    leftTerms := {};
    for j from 0 to #stL-1 do (
        cj := cLvec_(j,0);
        if cj != 0 then leftTerms = leftTerms | {{cj, stL#j}};
    );

    --------------------------------------------------------
    -- RIGHT elimination system
    --------------------------------------------------------
    rightTerms := {};
    dataR := null;
    MR := null;
    stR := null;
    idxR := null;
    cRvec := null;

    if #I0 == 0 then (
        rightTerms = {{1_(hpField()), {beta0, {}}}};
    ) else (
        dataR = hpThetaEliminationMatrix(I0, {beta0, R0}, #I0,
            BaseAlpha => beta0,
            Pivot => I0#0
        );

        MR = dataR#"M";
        stR = dataR#"states";

        if numRows MR =!= numColumns MR then (
            error("hpColStepRank2Data: right elimination matrix not square: "
                | toString(numRows MR) | "x" | toString(numColumns MR));
        );

        idxR = null;
        for i from 0 to #stR-1 do (
            if sort((stR#i)#0) == beta0 and sort((stR#i)#1) == I0 then (
                idxR = i;
                break;
            );
        );
        if idxR === null then error "hpColStepRank2Data: right target state not found.";

        eR := hpStdBasisVec(numRows MR, idxR);
        cRvec = eR // MR;
        if not (MR * cRvec == eR) then error "hpColStepRank2Data: right system not solved.";

        for j from 0 to #stR-1 do (
            cj := cRvec_(j,0);
            if cj != 0 then rightTerms = rightTerms | {{cj, stR#j}};
        );
    );

    --------------------------------------------------------
    -- lift to the triple combination
    --------------------------------------------------------
    tripleComb := {};

    for lt in leftTerms do (
        cL := lt#0;
        alphaL := sort((lt#1)#0);
        Kleft := sort((lt#1)#1);

        for rt in rightTerms do (
            cR := rt#0;
            betaR := sort((rt#1)#0);
            Iw := sort((rt#1)#1);

            Kmid := sort(R0 | Kleft);
            if #set(Kmid) =!= #Kmid then continue;

            T1 := {alphaL, sort(leftHook0#1)};
            T2 := {betaR, Kmid};

            tripleComb = tripleComb | {{cL*cR, T1, T2, Iw}};
        );
    );

    --------------------------------------------------------
    -- evaluate Psi_col and normalize
    --------------------------------------------------------
    Himg := hpPsiColEvaluateHT(tripleComb, m);
    normData := hpNormalizeRewriteHTData(Himg, T);

    out := new MutableHashTable;
    out#"input" = T;
    out#"localObstructionData" = D;

    out#"m" = m;
    out#"L0" = L0;
    out#"I0" = I0;
    out#"R0" = R0;
    out#"leftHook" = leftHook0;
    out#"middleHook" = middleHook0;

    out#"leftEliminationData" = dataL;
    out#"ML" = ML;
    out#"leftStates" = stL;
    out#"leftTargetIndex" = idxL;
    out#"leftCoeffVec" = cLvec;
    out#"leftTerms" = leftTerms;

    out#"rightEliminationData" = dataR;
    out#"MR" = MR;
    out#"rightStates" = stR;
    out#"rightTargetIndex" = idxR;
    out#"rightCoeffVec" = cRvec;
    out#"rightTerms" = rightTerms;

    out#"tripleComb" = tripleComb;

    out#"rawImage" = Himg;
    out#"origCoeffInRawImage" = normData#"origCoeff";
    out#"normalizedImage" = normData#"normalizedImage";
    out#"rewrite" = normData#"rewrite";

    new HashTable from out
);

------------------------------------------------------------
-- Dispatcher returning data for the first straightening step
------------------------------------------------------------

straightenStepRank2Data = T -> hpStraightenStepRank2Data(T,"Automatic");
hpStraightenStepRank2Data = (T,which) -> (
    T = hpToRows T;
    D := hpRank2LocalObstructionData(T,which);
    kind := D#"obstructionType";

    if kind == "none" then (
        H := new MutableHashTable;
        H#T = 1_(hpField());

        out := new MutableHashTable;
        out#"input" = T;
        out#"localObstructionData" = D;
        out#"rawImage" = hpFreezeHT H;
        out#"origCoeffInRawImage" = 1_(hpField());
        out#"normalizedImage" = hpFreezeHT H;
        out#"rewrite" = new HashTable from {};
        return new HashTable from out;
    );

    if kind == "column" then return hpRowStepRank2Data(T,D);
    if kind == "row" then return hpColStepRank2Data(T,D);

    error("hpStraightenStepRank2Data: unknown obstruction type " | toString kind);
);

------------------------------------------------------------
-- Cached semistandardness test
------------------------------------------------------------

hpIsSemistandardRank2Cached = (T, ssCache) -> (
    if ssCache#?T then return ssCache#T;
    ssCache#T = hpIsSemistandardRank2Unchecked T
);
------------------------------------------------------------
-- Cached one-step rewrite
------------------------------------------------------------

hpGetRewriteCached = (T, rewriteCache) -> (
    if rewriteCache#?T then return rewriteCache#T;
    rewriteCache#T = straightenStepRank2 T
);
------------------------------------------------------------
-- Processing order.  Every rewrite replaces a tabloid by tabloids that are
-- strictly smaller in the running order of the paper (Definition 3.2.3), so
-- if the worklist always rewrites its largest pending tabloid first, no
-- tabloid is rewritten twice.  hpTabloidOrderKey is a total order refining
-- the running order: compare the running vectors from the last index
-- backwards, and at the first disagreement compare (sum, entries); a
-- coordinatewise smaller vector (= larger tabloid) has a smaller sum, so the
-- largest tabloid has the lexicographically smallest key.  A binary
-- min-heap on these keys replaces the earlier first-in-first-out queue,
-- which could rewrite the same tabloid thousands of times.
------------------------------------------------------------

-- The order key of a tabloid: one pass over the diagonal hooks producing,
-- last running stage first, the running vectors of the paper (Definition
-- 3.2.3), each prefixed by its sum.  It is the primitive the engines call
-- once per tabloid; runningVectors and hpRunningVectorsFast are derived
-- from it.
hpTabloidOrderKey = (T, N) -> (
    lambda := apply(T, row -> #row);
    r := hpFrobeniusRank lambda;
    cnt := new MutableList from toList(N:0);
    addEntry := x -> for q from x-1 to N-1 do cnt#q = cnt#q + 1;
    snapshot := () -> (v := toList cnt; prepend(sum v, v));
    out := new MutableList;
    for t from 0 to r-1 do (
        for i from t to #T-1 do if lambda#i > t then addEntry T#i#t;
        out#(#out) = snapshot();
        for j from t+1 to lambda#t-1 do (addEntry T#t#j; out#(#out) = snapshot());
    );
    reverse toList out
);

hpHeapNew = () -> new MutableHashTable from {
    "data" => new MutableList from toList(64:null), "size" => 0};
hpHeapLess = (a, b) -> (a ? b) === symbol <;
hpHeapPush = (H, key, item) -> (
    n := H#"size"; data := H#"data";
    if n == #data then (
        bigger := new MutableList from toList(2*#data : null);
        for i from 0 to n-1 do bigger#i = data#i;
        data = bigger; H#"data" = data;
    );
    data#n = (key, item); H#"size" = n+1;
    i := n;
    while i > 0 do (
        p := (i-1)//2;
        if hpHeapLess(data#i#0, data#p#0) then (
            tmp := data#p; data#p = data#i; data#i = tmp; i = p)
        else break;
    );
);
hpHeapPop = H -> (
    n := H#"size"; data := H#"data";
    top := data#0;
    lastEntry := data#(n-1); data#(n-1) = null; n = n-1; H#"size" = n;
    if n > 0 then (
        data#0 = lastEntry;
        i := 0;
        while true do (
            l := 2*i+1; r := l+1; m := i;
            if l < n and hpHeapLess(data#l#0, data#m#0) then m = l;
            if r < n and hpHeapLess(data#r#0, data#m#0) then m = r;
            if m == i then break;
            tmp := data#m; data#m = data#i; data#i = tmp; i = m;
        );
    );
    top
);

------------------------------------------------------------
-- Largest-first memoized engine
--
-- Input:
--   T            = rank-2 tableau in row-reading form
--   maxIters     = safety cutoff
--   verboseFlag  = whether to print progress
--
-- Output:
--   (ordinary HashTable { semistandard tableau => coefficient }, iterations)
--
-- Notes:
--   * `todo` stores the coefficients of the pending non-semistandard tableaux.
--   * the heap orders them by hpTabloidOrderKey; an entry whose tableau is no
--     longer pending is skipped when popped.
--   * the result is independent of the processing order (each rewrite is an
--     identity in the Schur functor); the order only controls the work.
------------------------------------------------------------

-- one-step rewrites and rank-two normal forms of extracted pairs are
-- memoized across calls (separately for each coefficient field): the same
-- intermediate tabloids recur in different inputs of one shape and content
hpRewriteMemoCache = new MutableHashTable;
hpSubPairMemoCache = new MutableHashTable;

hpStraightenRank2EngineFast = (T, maxIters, verboseFlag) -> (
    T = hpToRows T;

    rewriteCache := hpFieldCache(hpRewriteMemoCache, hpField());
    ssCache      := new MutableHashTable;

    todo := new MutableHashTable;
    done := new MutableHashTable;

    -- every tableau reached from T has the same content, so one alphabet
    -- bound serves for all running-vector keys
    N := max flatten T;
    keyCache := new MutableHashTable;
    keyOf := U -> (if not keyCache#?U then keyCache#U = hpTabloidOrderKey(U, N); keyCache#U);
    heap := hpHeapNew();

    if hpIsSemistandardRank2Cached(T, ssCache) then (
        done#T = 1_(hpField());
    ) else (
        todo#T = 1_(hpField());
        hpHeapPush(heap, keyOf T, T);
    );

    iterNo := 0;

    while heap#"size" > 0 do (
        badKey := (hpHeapPop heap)#1;
        if not todo#?badKey then continue;
        coeffBad := todo#badKey;
        remove(todo, badKey);
        if coeffBad == 0 then continue;

        iterNo = iterNo + 1;
        if iterNo > maxIters then (
            hpFail("HookPieri: the rank-two straightening exceeded maxIters = " |
                toString maxIters | " rewrites (each pending tabloid is rewritten" |
                " at most once); raise maxIters, or use hookStraighten");
        );
        if hpState#"progress" > 0 and iterNo % hpState#"progress" == 0 then (
            print("HookPieri: rewrites=" | toString iterNo | " pending=" |
                toString heap#"size" | " done=" | toString(#done));
            stdio << flush;
        );

        if verboseFlag then (
            cachedStr := if rewriteCache#?badKey then "yes" else "no";
            print(
                "Iter " | toString iterNo
                | " | rewriting coeff=" | toString coeffBad
                | " | tableau=" | toString badKey
                | " | cached=" | cachedStr
                | " | todoSize=" | toString(#keys todo)
                | " | queuePending=" | toString(heap#"size")
            );
        );

        rew := hpGetRewriteCached(badKey, rewriteCache);

        for newKey in keys rew do (
            coeffNew := coeffBad * rew#newKey;
            if coeffNew != 0 then (
                if hpIsSemistandardRank2Cached(newKey, ssCache) then (
                    hpAddTerm(done, newKey, coeffNew);
                ) else (
                    wasPresent := todo#?newKey;
                    hpAddTerm(todo, newKey, coeffNew);
                    -- enqueue only when the tableau has just become pending
                    if not wasPresent and todo#?newKey then
                        hpHeapPush(heap, keyOf newKey, newKey);
                );
            );
        );
    );

    (hpFreezeHT(done), iterNo)
);

------------------------------------------------------------
-- Simple wrapper with a fresh context
------------------------------------------------------------

straightenRank2Fast = (T, maxIters, verboseFlag) -> (
    if not instance(maxIters,ZZ) or maxIters < 0 then
        error "straightenRank2Fast: maxIters must be a nonnegative integer";
    if not instance(verboseFlag,Boolean) then
        error "straightenRank2Fast: verboseFlag must be Boolean";
    T = hpValidateRankTwoRows("straightenRank2Fast",T);
    hooks := hpNestedToHooksInternal T;
    if not all(hooks,hpIsCanonicalHook) then
        error "straightenRank2Fast: each constituent hook must already be semistandard";
    hpStraightenRank2EngineFast(T, maxIters, verboseFlag)
);

------------------------------------------------------------
-- hpStraightenRank2FromAnyHooks
--
-- Accepts a rank-2 nested-hook tabloid T whose constituent
-- hooks need NOT themselves be semistandard. Pre-straightens
-- each hook via the hook relations (hpStraightenHookHT), takes
-- the bilinear product to obtain rank-2 tabloids with valid
-- (semistandard) constituent hooks, then runs the rank-2
-- engine on each and sums the weighted contributions.
--
-- Returns (HashTable : semistandard-rank-2-tabloid => coeff,
--          ZZ       : total rank-2 engine iterations).
------------------------------------------------------------

hpStraightenRank2FromAnyHooks = (caller,T) -> (
    T = hpValidateRankTwoRows(caller,T);

    hooks := hpNestedToHooksInternal T;
    Touter := hooks#0;
    Tinner := hooks#1;

    HT1 := hpStraightenHookHT(Touter#0, Touter#1);
    HT2 := hpStraightenHookHT(Tinner#0, Tinner#1);

    result := new MutableHashTable;
    totalIters := 0;

    for kOuter in keys HT1 do (
        cOuter := HT1#kOuter;
        if cOuter == 0 then continue;
        for kInner in keys HT2 do (
            cInner := HT2#kInner;
            coeffPair := cOuter * cInner;
            if coeffPair == 0 then continue;

            Tnew := hpHooksInternalToNestedRows(kOuter, kInner);
            (ht, iters) := hpStraightenRank2EngineFast(Tnew, 100000, false);
            totalIters = totalIters + iters;

            for k in keys ht do (
                c := ht#k * coeffPair;
                if c == 0 then continue;
                if result#?k then result#k = result#k + c
                else result#k = c;
                if result#?k and result#k == 0 then remove(result, k);
            );
        );
    );

    (hpFreezeHT result, totalIters)
);

------------------------------------------------------------------------
-- Straightening of hook tabloids (Section 6 of the paper).
--
-- A hook tabloid of shape lambda is the product of the hook tabloids on
-- the diagonal hooks of lambda, and it is semistandard when every
-- diagonal hook is semistandard and every consecutive pair of diagonal
-- hooks is semistandard as a tabloid of Frobenius rank two.  The
-- straightening algorithm writes a hook tabloid in the basis of the
-- semistandard ones: it first applies the single-hook relation to every
-- diagonal hook, then rewrites the south-east-most nonsemistandard pair
-- of consecutive hooks (rows k..lambda^t_k - 1, columns
-- k..max(lambda_k, lambda_{k+1}) - 1) by the rank two step of Section 6
-- and re-embeds the terms, until no bad pair remains.  A rewrite may
-- break the pairs (k-1, k) and (k+1, k+2), so the engine keeps a
-- worklist ordered by hpTabloidOrderKey, largest tabloid first, and runs
-- it to convergence.  Every term of a step is strictly smaller than its
-- input in the running order of Definition 3.2.3, which is what makes the
-- algorithm terminate; verifyTriangularity checks that inequality on
-- every tabloid of a shape.
--
-- Public interface of this section (every tabloid argument may be a
-- HookTabloid, a list of rows or a Filling; every returned table is keyed
-- by HookTabloids; every producer takes CoefficientRing => K):
--   hookStraighten T             the normal form, HashTable tabloid => c
--                                (Strategy "Automatic", "Rank2" or
--                                "Cokernel"; Progress => n prints)
--   hookStraighten H             the normal form of a linear combination
--   straighteningStep T          one rewrite of the algorithm
--   straighteningObstruction T   the obstruction data of the next rewrite
--   isStepTriangular T           one step decreases the running order
--   verifyTriangularity          the VerificationReport of that check on a
--                                tabloid, a shape and alphabet, or a shape
--                                and content
--   verifyStraighteningAgainstCokernel  the algorithm against the linear
--                                algebra of the Pieri cokernel
--   isHookSemistandard, frobeniusRank, hookTabloids
--   runningVectors, runningIndices, runningCompare  the running order
-- The forms hookStraighten(T, Ring) and hookStraighten RingElement need
-- a hook algebra and are installed in section 09.
------------------------------------------------------------------------

-- the Frobenius rank: the number of diagonal hooks of a partition, or of
-- the shape of a tabloid
frobeniusRank = method(TypicalValue => ZZ);
frobeniusRank List := lambda -> (
    -- a list of lists is a tabloid given by its rows
    if #lambda > 0 and all(lambda, r -> instance(r, List)) then return frobeniusRank tabloid lambda;
    if not instance(lambda,List) or
       any(lambda,x -> not instance(x,ZZ) or x <= 0) or
       any(1..#lambda-1,i -> lambda#(i-1) < lambda#i) then
        error "frobeniusRank: expected a partition (a weakly decreasing list of positive integers)";
    hpFrobeniusRank lambda
);
frobeniusRank HookTabloid := T -> hpFrobeniusRank apply(toList T, row -> #row);
frobeniusRank Filling := F ->
    hpFrobeniusRank apply(hpFillingToRows("frobeniusRank", F), row -> #row);

------------------------------------------------------------
-- Row form helpers: the sub-tabloid of a pair of hooks, contents, and
-- the extraction and embedding of one diagonal hook
------------------------------------------------------------

-- Relative shape of the rank two sub-tabloid spanned by hooks (k, k+1).
-- Rows are indexed k, k+1, ..., (lambda^t_k - 1) in the original tabloid.
-- Relative row 0 = original row k, with cells in columns k..lambda_k-1.
-- Relative row 1 = original row k+1, with cells in columns k..lambda_{k+1}-1.
-- Relative row i (>= 2) = original row k+i, with cells in
--   columns k, k+1 (length 2) if lambda_{k+i} >= k+2,
--   else only column k (length 1) if lambda_{k+i} >= k+1,
--   else excluded.
hpShapeOfHookPair = (lambda, k) -> (
    if k+1 >= #lambda then error "hpShapeOfHookPair: k+1 out of range.";
    nrows := #lambda;
    rel := {lambda#k - k, lambda#(k+1) - k};
    for i from k+2 to nrows - 1 do (
        if lambda#i >= k + 2 then rel = rel | {2}
        else if lambda#i >= k + 1 then rel = rel | {1}
        else break;
    );
    rel
);

hpExtractHookPair = (T, lambda, k) -> (
    rel := hpShapeOfHookPair(lambda, k);
    for i from 0 to #rel - 1 list
        for j from 0 to rel#i - 1 list T#(k + i)#(k + j)
);

hpEmbedHookPair = (T, lambda, k, sub) -> (
    rel := hpShapeOfHookPair(lambda, k);
    Tnew := apply(T, r -> new MutableList from r);
    for i from 0 to #rel - 1 do (
        for j from 0 to rel#i - 1 do (
            Tnew#(k + i)#(k + j) = sub#i#j;
        );
    );
    apply(Tnew, r -> toList r)
);

-- the content vector of a tabloid on the alphabet 1..N
hpTabloidContent = (T, N) -> (
    entries := flatten T;
    apply(N, q -> #select(entries, x -> x == q+1))
);

-- Replace hook k of a row form tabloid by the hook (rt, cf).
hpEmbedHookK = (T0, lambda, k, rt, cf) -> (
    Tnew := apply(T0, r -> new MutableList from r);
    Tnew#k#k = cf#0;
    for j from 0 to #rt - 1 do Tnew#k#(k+1+j) = rt#j;
    idx := 1;
    for i from k+1 to #T0 - 1 do (
        if lambda#i > k then (
            Tnew#i#k = cf#idx;
            idx = idx + 1;
        );
    );
    apply(Tnew, r -> toList r)
);

-- Extract the (rowTail, colFull) of hook k from a row form tabloid.
hpHookOfTabloid = (T, lambda, k) -> (
    rowTail := for j from k+1 to lambda#k - 1 list T#k#j;
    colFull := flatten for i from k to #T - 1 list (
        if lambda#i > k then {T#i#k} else {}
    );
    (rowTail, colFull)
);

-- a table keyed by row lists, re-keyed by HookTabloids (the rows of the
-- engines are validated already, so the constructor is bypassed)
hpTabloidTable = H -> hashTable apply(pairs H, kv -> (new HookTabloid from kv#0) => kv#1);

------------------------------------------------------------
-- Semistandard hook tabloids
------------------------------------------------------------

-- the test on a list of rows; anything that is not a hook tabloid with
-- positive integer entries is not semistandard
hpIsHookSemistandardRows = T -> (
    if not instance(T,List) or any(T,row -> not instance(row,List)) then return false;
    if T == {} then return true;
    lambda := apply(T, r -> #r);
    if any(lambda,x -> x <= 0) or
       (#lambda > 1 and
        any(1..#lambda-1,i -> lambda#(i-1) < lambda#i)) or
       any(flatten T,x -> not instance(x,ZZ) or x <= 0) then return false;
    r := hpFrobeniusRank lambda;
    -- Each individual hook must be canonical: column strictly
    -- increasing, row tail weakly increasing,
    -- and rowTail#0 >= colFull#0 (corner) when rowTail is nonempty.
    for k from 0 to r - 1 do (
        (rt, cf) := hpHookOfTabloid(T, lambda, k);
        if cf =!= sort cf then return false;
        if #cf =!= #(set cf) then return false;
        if rt =!= sort rt then return false;
        if #rt > 0 and rt#0 < cf#0 then return false;
    );
    -- Then each consecutive pair must be rank two semistandard.
    if r <= 1 then return true;
    for k from 0 to r - 2 do (
        sub := hpExtractHookPair(T, lambda, k);
        if not hpIsSemistandardRank2Unchecked sub then return false;
    );
    true
);

isHookSemistandard = method(TypicalValue => Boolean);
isHookSemistandard Thing := T -> false;
isHookSemistandard List := T -> hpIsHookSemistandardRows T;
isHookSemistandard HookTabloid := T -> hpIsHookSemistandardRows toList T;
isHookSemistandard Filling := F -> (
    rows := try hpFillingToRows("isHookSemistandard", F) else null;
    if rows === null then false else hpIsHookSemistandardRows rows
);

-- Pick the SE-most pair (largest k) that is bad, or null if none.
hpFindSEmostBadPair = (T, lambda) -> (
    r := frobeniusRank lambda;
    k := r - 2;
    while k >= 0 do (
        sub := hpExtractHookPair(T, lambda, k);
        if not hpIsSemistandardRank2Unchecked sub then return k;
        k = k - 1;
    );
    null
);

------------------------------------------------------------
-- The general rank engine: a largest-first worklist that resolves bad
-- pairs one at a time.  Returns (HashTable: semistandard rows => coeff,
-- total iterations); the keys are row lists, and the public functions
-- re-key by HookTabloids.
------------------------------------------------------------

hpHookStraightenFromAnyHooks = (caller,T) -> (
    T = hpValidateStraighteningRows(caller,T);
    lambda := apply(T, r -> #r);
    r := frobeniusRank lambda;

    if r == 0 then return (new HashTable from {T => 1_(hpField())}, 0);

    if r == 1 then (
        -- Single hook: row 0 is corner + row tail, column 0 is full column.
        rowTail := drop(T#0, 1);
        colFull := for i from 0 to #T - 1 list T#i#0;
        ht := hpStraightenHookHT(rowTail, colFull);
        out := new MutableHashTable;
        for k in keys ht do (
            rt := k#0;
            cf := k#1;
            rowsNew := for i from 0 to #cf - 1 list (
                if i == 0 then {cf#0} | rt
                else {cf#i}
            );
            out#rowsNew = ht#k;
        );
        return (new HashTable from out, 0);
    );

    if r == 2 then return hpStraightenRank2FromAnyHooks(caller,T);

    -- r >= 3
    -- Step 1: pre-straighten each individual hook of T into its
    -- canonical-hook expansion.  Iteratively expand hook 0, then
    -- hook 1, ..., hook r-1, multiplying coefficients across the
    -- tensor product of the r single-hook expansions.  After this
    -- step every tabloid in `dict` has all hooks in canonical form;
    -- only rank two pair obstructions remain.
    embedHookK := (T0, k, rt, cf) -> hpEmbedHookK(T0, lambda, k, rt, cf);

    dict := new MutableHashTable;
    dict#T = 1_(hpField());
    for k from 0 to r - 1 do (
        nextDict := new MutableHashTable;
        for T0 in keys dict do (
            c0 := dict#T0;
            (rt, cf) := hpHookOfTabloid(T0, lambda, k);
            ht := hpStraightenHookHT(rt, cf);
            for hkey in keys ht do (
                cNew := c0 * (ht#hkey);
                if cNew == 0 then continue;
                Tnew := embedHookK(T0, k, hkey#0, hkey#1);
                if nextDict#?Tnew then nextDict#Tnew = nextDict#Tnew + cNew
                else nextDict#Tnew = cNew;
                if nextDict#Tnew == 0 then remove(nextDict, Tnew);
            );
        );
        dict = nextDict;
    );

    -- Step 2: worklist over rank two pair obstructions, largest tabloid
    -- first (see hpTabloidOrderKey; the sub-pair's running vectors differ
    -- from the full tabloid's by constants, so a rank two rewrite strictly
    -- decreases the full tabloid too).  `todo` holds the pending
    -- coefficients; a popped tabloid that is no longer pending is skipped.
    -- Rank two normal forms of the extracted sub-tabloids are memoized,
    -- since the same pair recurs in many different tabloids.
    todo := new MutableHashTable;
    done := new MutableHashTable;
    N := max flatten T;
    keyCache := new MutableHashTable;
    keyOf := U -> (if not keyCache#?U then keyCache#U = hpTabloidOrderKey(U, N); keyCache#U);
    heap := hpHeapNew();
    for T0 in keys dict do (todo#T0 = dict#T0; hpHeapPush(heap, keyOf T0, T0));
    subMemo := hpFieldCache(hpSubPairMemoCache, hpField());

    iters := 0;

    while heap#"size" > 0 do (
        T0 := (hpHeapPop heap)#1;
        if not todo#?T0 then continue;
        c0 := todo#T0;
        remove(todo, T0);
        if c0 == 0 then continue;

        iters = iters + 1;
        if iters > 1000000 then hpFail "HookPieri: the straightening exceeded 1000000 pair rewrites";
        if hpState#"progress" > 0 and iters % hpState#"progress" == 0 then (
            print("HookPieri: pair rewrites=" | toString iters | " pending=" |
                toString heap#"size" | " done=" | toString(#done));
            stdio << flush;
        );

        kBad := hpFindSEmostBadPair(T0, lambda);
        if kBad === null then (
            hpAddTerm(done, T0, c0);
        ) else (
            sub := hpExtractHookPair(T0, lambda, kBad);
            htSub := if subMemo#?sub then subMemo#sub else (
                subMemo#sub = first hpStraightenRank2FromAnyHooks(caller,sub));
            for newSub in keys htSub do (
                cNew := c0 * (htSub#newSub);
                if cNew == 0 then continue;
                Tnew := hpEmbedHookPair(T0, lambda, kBad, newSub);
                wasPresent := todo#?Tnew;
                if wasPresent then todo#Tnew = todo#Tnew + cNew else todo#Tnew = cNew;
                if todo#Tnew == 0 then remove(todo, Tnew)
                else if not wasPresent then hpHeapPush(heap, keyOf Tnew, Tnew);
            );
        );
    );

    (new HashTable from done, iters)
);

-- run f() printing a progress line every `progress` rewrites
hpWithProgress = (caller, progress, f) -> (
    -- hpFail so that the message survives an enclosing hpWithField
    if not instance(progress,ZZ) or progress < 0 then
        hpFail(caller | ": Progress must be a nonnegative integer");
    if progress == 0 then return f();
    old := hpState#"progress";
    hpState#"progress" = progress;
    hpLastError = null;
    result := try f() else (
        hpState#"progress" = old;
        error(if hpLastError =!= null then hpLastError
            else caller | ": the straightening failed; rerun without Progress to see the message");
    );
    hpState#"progress" = old;
    result
);

------------------------------------------------------------------------
-- hookStraighten: the normal form of a hook tabloid, or of a linear
-- combination of hook tabloids, in the semistandard basis.  Strategy
-- "Automatic" runs the algorithm above, "Rank2" runs the rank two engine
-- alone (the input must have Frobenius rank two), and "Cokernel" computes
-- the normal form by linear algebra on the Pieri relations of the weight
-- space (below), which involves no straightening step at all.
------------------------------------------------------------------------

hpValidateStrategy = (caller, strategy) -> (
    if not member(strategy, {"Automatic", "Rank2", "Cokernel"}) then
        error(caller | ": Strategy must be \"Automatic\", \"Rank2\" or \"Cokernel\"");
    strategy
);

-- the Rank2 strategy needs a rank two input; checked before the field is
-- set so that the message reaches the user unchanged
hpCheckStrategyRank = (caller, T, strategy) -> (
    if strategy === "Rank2" and hpFrobeniusRank apply(T, row -> #row) != 2 then
        error(caller | ": Strategy => \"Rank2\" expects a hook tabloid of Frobenius rank two");
);

-- the normal form of validated rows by the chosen strategy: rows => c
hpStraightenRowsBy = (caller, T, strategy) -> (
    if strategy === "Cokernel" then return hpCokernelNormalFormRows(caller, T);
    if strategy === "Rank2" then return first hpStraightenRank2FromAnyHooks(caller, T);
    first hpHookStraightenFromAnyHooks(caller, T)
);

-- the (HashTable HookTabloid => c, iteration count) form, for the
-- benchmark scripts (the count is the number of pair rewrites of the
-- worklist; it is zero in rank at most one and for the Cokernel strategy)
hpHookStraightenCount = method(Options => {CoefficientRing => QQ, Strategy => "Automatic", Progress => 0});
hpHookStraightenCount BasicList := opts -> T -> (
    T = hpValidateStraighteningRows("hookStraighten", T);
    strategy := hpValidateStrategy("hookStraighten", opts#Strategy);
    hpCheckStrategyRank("hookStraighten", T, strategy);
    K := hpValidateCoefficientRing("hookStraighten", opts#CoefficientRing);
    (H, iters) := hpWithField(K, () -> hpWithProgress("hookStraighten", opts#Progress, () -> (
        if strategy === "Cokernel" then (hpCokernelNormalFormRows("hookStraighten", T), 0)
        else if strategy === "Rank2" then hpStraightenRank2FromAnyHooks("hookStraighten", T)
        else hpHookStraightenFromAnyHooks("hookStraighten", T))));
    (hpTabloidTable H, iters)
);

-- no TypicalValue: the (Thing, Ring) and RingElement forms of the algebra
-- section return ring elements
hookStraighten = method(Options => {CoefficientRing => QQ, Strategy => "Automatic", Progress => 0});

hpHookStraighten = (T, opts) -> (
    T = hpValidateStraighteningRows("hookStraighten", T);
    strategy := hpValidateStrategy("hookStraighten", opts#Strategy);
    hpCheckStrategyRank("hookStraighten", T, strategy);
    K := hpValidateCoefficientRing("hookStraighten", opts#CoefficientRing);
    hpTabloidTable hpWithField(K, () -> hpWithProgress("hookStraighten", opts#Progress,
        () -> hpStraightenRowsBy("hookStraighten", T, strategy)))
);

hookStraighten HookTabloid := opts -> T -> hpHookStraighten(toList T, opts);
hookStraighten List := opts -> T -> hpHookStraighten(T, opts);
hookStraighten Filling := opts -> F -> hpHookStraighten(F, opts);
-- one hook given by its row tail and its full column
hookStraighten(List, List) := opts -> (rowTail, colFull) ->
    hpHookStraighten(hpNativeToRows("hookStraighten", rowTail, colFull), opts);
-- a linear combination: the normal forms of the keys, weighted by the
-- values and summed; the values are numbers or elements of the field
hookStraighten HashTable := opts -> H -> (
    strategy := hpValidateStrategy("hookStraighten", opts#Strategy);
    K := hpValidateCoefficientRing("hookStraighten", opts#CoefficientRing);
    combination := apply(pairs H, kv -> (
        T := hpValidateStraighteningRows("hookStraighten", kv#0);
        hpCheckStrategyRank("hookStraighten", T, strategy);
        if not (instance(kv#1, ZZ) or instance(kv#1, QQ) or instance(kv#1, RingElement)) then
            error "hookStraighten: the values of the table must be coefficients";
        (T, kv#1)));
    hpTabloidTable hpWithField(K, () -> hpWithProgress("hookStraighten", opts#Progress, () -> (
        out := new MutableHashTable;
        for term in combination do (
            nf := hpStraightenRowsBy("hookStraighten", term#0, strategy);
            for U in keys nf do hpAddTerm(out, U, nf#U * term#1);
        );
        new HashTable from out)))
);

------------------------------------------------------------------------
-- One local step of the straightening algorithm, and triangularity.
--
-- straighteningStep T performs exactly one rewrite of the algorithm on a
-- hook tabloid T of any rank: if a diagonal hook is not semistandard, the
-- single-hook relation (6.1a) on the leftmost such hook; otherwise, in rank
-- two, the row or column step of Section 6 attached to the first
-- obstruction, and in rank r >= 3 the same step applied to the
-- south-east-most nonsemistandard consecutive pair of hooks and
-- re-embedded.  It returns the rewrite T = sum_U c_U U as a HashTable
-- keyed by HookTabloids; a semistandard T returns {T => 1}.
--
-- Every term U of a step is strictly smaller than T in the running order
-- of Definition 3.2.3 (the paper's triangularity statements); since all
-- terms have the content of T and only finitely many tabloids of a given
-- shape and content exist, this alone proves that the algorithm terminates
-- on every tabloid of that shape and content.  verifyTriangularity checks
-- the inequality for one step on every nonsemistandard tabloid of a shape
-- and alphabet (or a shape and content), without running the algorithm.
------------------------------------------------------------------------

hpRequireCanonicalHooks = (caller, T, lambda) -> (
    r := frobeniusRank lambda;
    for k from 0 to r-1 do (
        (rt, cf) := hpHookOfTabloid(T, lambda, k);
        if not hpIsCanonicalHook {rt, cf} then error(caller |
            ": diagonal hook " | toString(k+1) | " of the tabloid is not semistandard" |
            " (straighteningStep applies the single-hook relation to it first;" |
            " hookStraighten straightens everything)");
    );
);

hpValidateObstructionOption = (caller, kind) -> (
    if not member(kind, {"Automatic","row","column"}) then
        error(caller | ": Obstruction must be \"Automatic\", \"row\" or \"column\"");
    kind
);

hpValidateEngineStep = (caller, engine) -> (
    if not instance(engine, Boolean) then error(caller | ": EngineStep must be Boolean");
    engine
);

-- the index of the pair the algorithm rewrites next: for rank two, 0 when
-- the tabloid is not semistandard; for rank >= 3 the south-east-most bad
-- consecutive pair; null when the tabloid is semistandard
hpNextBadPair = (T, lambda, r) -> (
    if r == 2 then (if hpIsSemistandardRank2Unchecked T then null else 0)
    else hpFindSEmostBadPair(T, lambda)
);

-- one rewrite on validated rows: HashTable rows => coefficient
hpStraighteningStepRows = (caller, T, kind, engine) -> (
    one := 1_(hpField());
    lambda := apply(T, row -> #row);
    r := frobeniusRank lambda;
    if r == 0 then return new HashTable from {T => one};
    -- (i) a diagonal hook that is not semistandard: the single-hook
    -- relation (6.1a) on the leftmost one
    for k from 0 to r-1 do (
        (rt, cf) := hpHookOfTabloid(T, lambda, k);
        if not hpIsCanonicalHook {rt, cf} then (
            ht := hpStraightenHookHT(rt, cf);
            out := new MutableHashTable;
            for hkey in keys ht do
                hpAddTerm(out, hpEmbedHookK(T, lambda, k, hkey#0, hkey#1), ht#hkey);
            return new HashTable from out;
        );
    );
    if r == 1 then return new HashTable from {T => one};
    -- (ii) the rank two step on the next bad pair
    kBad := hpNextBadPair(T, lambda, r);
    if kBad === null then return new HashTable from {T => one};
    sub := if r == 2 then T else hpExtractHookPair(T, lambda, kBad);
    H := if engine then first hpStraightenRank2FromAnyHooks(caller, sub)
        else hpStraightenStepRank2(sub, kind);
    if r == 2 then return H;
    out := new MutableHashTable;
    for U in keys H do hpAddTerm(out, hpEmbedHookPair(T, lambda, kBad, U), H#U);
    new HashTable from out
);

straighteningStep = method(Options => {Obstruction => "Automatic", EngineStep => false,
    CoefficientRing => QQ}, TypicalValue => HashTable);
hpStraighteningStep = (T, opts) -> (
    T = hpValidateStraighteningRows("straighteningStep", T);
    kind := hpValidateObstructionOption("straighteningStep", opts#Obstruction);
    engine := hpValidateEngineStep("straighteningStep", opts#EngineStep);
    K := hpValidateCoefficientRing("straighteningStep", opts#CoefficientRing);
    hpTabloidTable hpWithField(K, () -> hpStraighteningStepRows("straighteningStep", T, kind, engine))
);
straighteningStep HookTabloid := opts -> T -> hpStraighteningStep(toList T, opts);
straighteningStep List := opts -> T -> hpStraighteningStep(T, opts);
straighteningStep Filling := opts -> F -> hpStraighteningStep(F, opts);

-- the obstruction data of the pair the algorithm rewrites next, together
-- with everything the rank two step computes on it: a string keyed
-- diagnostic record
straighteningObstruction = method(Options => {Obstruction => "Automatic", CoefficientRing => QQ},
    TypicalValue => HashTable);
hpStraighteningObstructionRows = (T, kind) -> (
    lambda := apply(T, row -> #row);
    r := frobeniusRank lambda;
    kBad := hpNextBadPair(T, lambda, r);
    one := 1_(hpField());
    if kBad === null then return new HashTable from {
        "obstructionType" => "none", "input" => new HookTabloid from T,
        "pairIndex" => null, "subTabloid" => null,
        "step" => new HashTable from {(new HookTabloid from T) => one},
        "message" => "the tabloid is semistandard"};
    sub := if r == 2 then T else hpExtractHookPair(T, lambda, kBad);
    D := hpStraightenStepRank2Data(sub, kind);
    embed := H -> if r == 2 then hpTabloidTable H else
        hpTabloidTable hashTable apply(pairs H, kv -> hpEmbedHookPair(T, lambda, kBad, kv#0) => kv#1);
    new HashTable from (pairs D#"localObstructionData" | {
        "input" => new HookTabloid from T,
        "pairIndex" => kBad,
        "subTabloid" => new HookTabloid from sub,
        "localObstructionData" => D#"localObstructionData",
        "rawImage" => hpTabloidTable D#"rawImage",
        "origCoeffInRawImage" => D#"origCoeffInRawImage",
        "normalizedImage" => hpTabloidTable D#"normalizedImage",
        "rewrite" => hpTabloidTable D#"rewrite",
        "step" => embed D#"rewrite",
        "internal" => D})
);
hpStraighteningObstruction = (T, opts) -> (
    T = hpValidateStraighteningRows("straighteningObstruction", T);
    kind := hpValidateObstructionOption("straighteningObstruction", opts#Obstruction);
    K := hpValidateCoefficientRing("straighteningObstruction", opts#CoefficientRing);
    lambda := apply(T, row -> #row);
    if frobeniusRank lambda < 2 then
        error "straighteningObstruction: expected a hook tabloid of Frobenius rank at least two";
    hpRequireCanonicalHooks("straighteningObstruction", T, lambda);
    hpWithField(K, () -> hpStraighteningObstructionRows(T, kind))
);
straighteningObstruction HookTabloid := opts -> T -> hpStraighteningObstruction(toList T, opts);
straighteningObstruction List := opts -> T -> hpStraighteningObstruction(T, opts);
straighteningObstruction Filling := opts -> F -> hpStraighteningObstruction(F, opts);

------------------------------------------------------------
-- Triangularity of one step and of a whole shape
------------------------------------------------------------

-- the running vectors of a valid row form tabloid, first stage first
-- (the public runningVectors with the validation removed)
hpRunningVectorsFast = (T, N) -> apply(reverse hpTabloidOrderKey(T, N), v -> drop(v, 1));

-- the symbol of a comparison code of hpRunningCompareVectors
hpRunningSymbol = code -> (
    if code === -1 then symbol <
    else if code === 1 then symbol >
    else if code === 0 then symbol ==
    else incomparable
);

-- the StepTriangularity report of one step on validated rows
hpStepTriangularityRows = (caller, T, kind, engine) -> (
    N := if #T == 0 then 0 else max flatten T;
    H := hpStraighteningStepRows(caller, T, kind, engine);
    semistandard := hpIsHookSemistandardRows T;
    lambda := apply(T, row -> #row);
    hookRelation := any(frobeniusRank lambda, k -> not hpIsCanonicalHook toList hpHookOfTabloid(T, lambda, k));
    vT := hpRunningVectorsFast(T, N);
    contentT := hpTabloidContent(T, N);
    terms := for U in keys H list (
        if U == T then continue;
        {H#U, new HookTabloid from U, hpRunningSymbol hpRunningCompareVectors(hpRunningVectorsFast(U, N), vT)}
    );
    failures := select(terms, t -> t#2 =!= symbol <);
    contentOK := all(keys H, U -> hpTabloidContent(U, N) == contentT);
    -- the single-hook relation permutes the entries of one hook and
    -- leaves the running vectors unchanged; it is applied at most once
    -- per hook, so it is not a triangularity question
    passed := (hookRelation or #failures == 0) and contentOK;
    hpVerificationReport("StepTriangularity", {
        "input" => new HookTabloid from T,
        "semistandard" => semistandard,
        "hookRelation" => hookRelation,
        "step" => hpTabloidTable H,
        "terms" => terms,
        "failures" => failures,
        "contentPreserved" => contentOK,
        "passed" => passed,
        "conclusion" => (
            if semistandard then "the tabloid is semistandard, so the step is the identity"
            else if not contentOK then "a term of the step has a different content"
            else if hookRelation then "the step is the single-hook relation, which permutes the entries of one hook"
            else if passed then "every term of the step is strictly smaller in the running order"
            else "a term of the step is not strictly smaller in the running order")})
);

hpVerifyStepTriangularity = (T, opts) -> (
    T = hpValidateStraighteningRows("verifyTriangularity", T);
    kind := hpValidateObstructionOption("verifyTriangularity", opts#Obstruction);
    engine := hpValidateEngineStep("verifyTriangularity", opts#EngineStep);
    K := hpValidateCoefficientRing("verifyTriangularity", opts#CoefficientRing);
    hpWithField(K, () -> hpStepTriangularityRows("verifyTriangularity", T, kind, engine))
);

-- the Triangularity report of one step on every tabloid of a list
hpVerifyTriangularity = (lambda, tabloids, restriction, value, N, kind, engine) -> (
    t0 := cpuTime();
    steps := 0; maxTerms := 0; termsChecked := 0; zeroSteps := 0;
    semistandard := 0; incomparableTerms := 0; contentViolations := 0;
    violations := {}; firstViolation := null;
    for T in tabloids do (
        if hpIsHookSemistandardRows T then (semistandard = semistandard + 1; continue);
        H := hpStraighteningStepRows("verifyTriangularity", T, kind, engine);
        steps = steps + 1;
        if #H > maxTerms then maxTerms = #H;
        if #H == 0 then zeroSteps = zeroSteps + 1;
        vT := hpRunningVectorsFast(T, N);
        contentT := hpTabloidContent(T, N);
        for U in keys H do (
            if U == T then continue;
            termsChecked = termsChecked + 1;
            code := hpRunningCompareVectors(hpRunningVectorsFast(U, N), vT);
            contentOK := hpTabloidContent(U, N) == contentT;
            if code =!= -1 or not contentOK then (
                if code === null then incomparableTerms = incomparableTerms + 1;
                if not contentOK then contentViolations = contentViolations + 1;
                violations = append(violations, {new HookTabloid from T, new HookTabloid from U});
                if firstViolation === null then firstViolation = {
                    new HookTabloid from T, new HookTabloid from U,
                    if not contentOK then "the term has a different content"
                    else if code === null then "the running vectors are incomparable at their last disagreement"
                    else if code === 0 then "the running vectors agree"
                    else "the term is larger in the running order"};
            );
        );
    );
    passed := #violations == 0;
    hpVerificationReport("Triangularity", {
        "shape" => lambda,
        "restriction" => restriction,
        "restrictionValue" => value,
        "obstruction" => kind,
        "engine" => engine,
        "tabloids" => #tabloids,
        "semistandard" => semistandard,
        "steps" => steps,
        "terms" => termsChecked,
        "maxTerms" => maxTerms,
        "zeroSteps" => zeroSteps,
        "incomparable" => incomparableTerms,
        "contentViolations" => contentViolations,
        "violations" => violations,
        "firstViolation" => firstViolation,
        "seconds" => cpuTime() - t0,
        "passed" => passed,
        "conclusion" => (if passed then
            "every step strictly decreases the running order: hookStraighten terminates on this set"
            else "a step does not decrease the running order")})
);

hpValidatePartition = (caller, lambda) -> (
    if not instance(lambda, List) or #lambda == 0 or
       any(lambda, x -> not instance(x, ZZ) or x <= 0) or
       any(1..#lambda-1, i -> lambda#(i-1) < lambda#i) then
        error(caller | ": expected a partition as a weakly decreasing list of positive integers");
);

hpValidateContent = (caller, lambda, content) -> (
    if not instance(content, List) or #content == 0 or
       any(content, x -> not instance(x, ZZ) or x < 0) or sum content != sum lambda then
        error(caller | ": the content must be a list of nonnegative integers summing to the size of the shape");
);

hpValidateAlphabet = (caller, n) -> (
    if not instance(n, ZZ) or n < 1 then error(caller | ": the alphabet size must be a positive integer");
);

-- all hook tabloids of a shape: products of semistandard diagonal hooks,
-- with entries in 1..n or of a given content, as HookTabloids
hookTabloids = method(TypicalValue => List);
hookTabloids(List, ZZ) := (lambda, n) -> (
    hpValidatePartition("hookTabloids", lambda);
    hpValidateAlphabet("hookTabloids", n);
    apply(hpAllHookTabloids(lambda, n), T -> new HookTabloid from T)
);
hookTabloids(List, List) := (lambda, content) -> (
    hpValidatePartition("hookTabloids", lambda);
    hpValidateContent("hookTabloids", lambda, content);
    apply(hpAllHookTabloidsWeight(lambda, #content, content), T -> new HookTabloid from T)
);

verifyTriangularity = method(Options => {Obstruction => "Automatic", EngineStep => false,
    CoefficientRing => QQ}, TypicalValue => VerificationReport);
verifyTriangularity(List, ZZ) := opts -> (lambda, n) -> (
    hpValidatePartition("verifyTriangularity", lambda);
    hpValidateAlphabet("verifyTriangularity", n);
    kind := hpValidateObstructionOption("verifyTriangularity", opts#Obstruction);
    engine := hpValidateEngineStep("verifyTriangularity", opts#EngineStep);
    K := hpValidateCoefficientRing("verifyTriangularity", opts#CoefficientRing);
    hpWithField(K, () -> hpVerifyTriangularity(lambda, hpAllHookTabloids(lambda, n),
        "alphabet", n, n, kind, engine))
);
verifyTriangularity(List, List) := opts -> (lambda, content) -> (
    hpValidatePartition("verifyTriangularity", lambda);
    hpValidateContent("verifyTriangularity", lambda, content);
    kind := hpValidateObstructionOption("verifyTriangularity", opts#Obstruction);
    engine := hpValidateEngineStep("verifyTriangularity", opts#EngineStep);
    K := hpValidateCoefficientRing("verifyTriangularity", opts#CoefficientRing);
    hpWithField(K, () -> hpVerifyTriangularity(lambda,
        hpAllHookTabloidsWeight(lambda, #content, content), "content", content, #content, kind, engine))
);
-- one tabloid: the StepTriangularity report of one step
verifyTriangularity HookTabloid := opts -> T -> hpVerifyStepTriangularity(toList T, opts);
verifyTriangularity List := opts -> T -> hpVerifyStepTriangularity(T, opts);
verifyTriangularity Filling := opts -> F -> hpVerifyStepTriangularity(F, opts);

isStepTriangular = method(Options => {Obstruction => "Automatic", EngineStep => false,
    CoefficientRing => QQ}, TypicalValue => Boolean);
isStepTriangular HookTabloid := opts -> T -> (hpVerifyStepTriangularity(toList T, opts))#"passed";
isStepTriangular List := opts -> T -> (hpVerifyStepTriangularity(T, opts))#"passed";
isStepTriangular Filling := opts -> F -> (hpVerifyStepTriangularity(F, opts))#"passed";

------------------------------------------------------------------------
-- The normal form from the cokernel of the Pieri relations.
--
-- For a hook tabloid T of shape lambda and content w, let W be the span
-- of all hook tabloids of shape lambda and content w (all tuples of
-- semistandard hooks of the diagonal hook shapes).  The degree lambda
-- relations of the universal hook algebra restricted to W are the images
-- of the crossed-hook Pieri injections of every pair of hooks (i,j), i<j,
-- with the remaining hooks fixed (Section 3.1: the relation space
-- R_{(a,b),(c,d)} is the image of Psi_{a-c,0}).  The semistandard tabloids
-- form a complement of that relation space, and the coordinates of T in
-- that complement are its normal form.  No straightening algorithm is
-- involved: this is linear algebra on the weight space, and it is the
-- reference against which hookStraighten is checked.  The relation data
-- of a weight space is memoized per (shape, content, field), so that the
-- Cokernel strategy on a weight space costs one elimination.
------------------------------------------------------------------------

hpHookTypesOfShape = lambda -> (
    conj := hpConjugatePartition lambda;
    apply(frobeniusRank lambda, k -> {lambda#k - k - 1, conj#k - k - 1})
);

-- expand a tabloid whose hooks need not be semistandard into canonical-hook
-- tabloids (the single-hook relations only): HashTable rows => coefficient
hpCanonicalHookExpansion = T -> (
    lambda := apply(T, row -> #row);
    r := frobeniusRank lambda;
    dict := new MutableHashTable;
    dict#T = 1_(hpField());
    for k from 0 to r-1 do (
        nextDict := new MutableHashTable;
        for T0 in keys dict do (
            (rt, cf) := hpHookOfTabloid(T0, lambda, k);
            ht := hpStraightenHookHT(rt, cf);
            hooks0 := apply(r, l -> toList hpHookOfTabloid(T0, lambda, l));
            for hkey in keys ht do (
                cNew := dict#T0 * ht#hkey;
                if cNew == 0 then continue;
                Tnew := hpAssembleHookTabloid(lambda, replace(k, {hkey#0, hkey#1}, hooks0));
                hpAddTerm(nextDict, Tnew, cNew);
            );
        );
        dict = nextDict;
    );
    new HashTable from dict
);

hpCokernelRelationData = new Type of HashTable;
hpCokernelDataCache = new MutableHashTable;

-- the weight space of shape lambda and content w with its relations,
-- memoized per field
hpCokernelData = (lambda, w) -> (
    memo := hpFieldCache(hpCokernelDataCache, hpField());
    memoKey := (lambda, w);
    if memo#?memoKey then return memo#memoKey;
    N := #w;
    kk := (hpField());
    class := hpAllHookTabloidsWeight(lambda, N, w);
    if #class == 0 then hpFail "hookStraighten: no hook tabloids of this shape and content";
    idx := hashTable apply(#class, i -> class#i => i);
    r := frobeniusRank lambda;
    types := hpHookTypesOfShape lambda;
    seen := new MutableHashTable;
    relations := {};
    for X in class do (
        hooksX := apply(r, l -> toList hpHookOfTabloid(X, lambda, l));
        for i from 0 to r-2 do for j from i+1 to r-1 do (
            rest := apply(r, l -> if l == i or l == j then null else hooksX#l);
            pairW := hpTabloidContent({hooksX#i#0 | hooksX#i#1 | hooksX#j#0 | hooksX#j#1}, N);
            key := (i, j, rest, pairW);
            if seen#?key then continue;
            seen#key = true;
            data := hookInjectionMatrixWeight(types#i#0+1, types#i#1, types#j#0+1, types#j#1, N, pairW);
            M := data#0; cod := data#2;
            cols := entries transpose M;
            for col in cols do (
                vec := new MutableHashTable;
                for q from 0 to #cod-1 do if col#q != 0 then (
                    hooksU := replace(j, cod#q#1, replace(i, cod#q#0, hooksX));
                    U := hpAssembleHookTabloid(lambda, hooksU);
                    if not idx#?U then hpFail "hookStraighten: a Pieri image left the weight space";
                    vec#(idx#U) = col#q;
                );
                if #vec > 0 then relations = append(relations, new HashTable from vec);
            );
        );
    );
    standard := select(class, hpIsHookSemistandardRows);
    -- an echelon basis of the relation space, computed once; the semistandard
    -- tabloids must be a complement: the relations have rank
    -- #class - #standard, and no pivot lies on a semistandard coordinate,
    -- i.e. every semistandard basis vector is already reduced
    R := mutableMatrix(kk, #class, max(1, #relations), Dense => false);
    for c from 0 to #relations-1 do for rowIndex in keys relations#c do
        R_(rowIndex, c) = relations#c#rowIndex;
    R = matrix R;
    G := gb R;
    relationRank := numColumns gens G;
    if relationRank != #class - #standard then hpFail(
        "hookStraighten: the Pieri relations of shape " | toString lambda | " and content " |
        toString w | " have rank " | toString relationRank |
        " but the semistandard complement needs " | toString(#class - #standard) |
        (if char kk > 0 then " (characteristic " | toString char kk | ")" else ""));
    S := mutableMatrix(kk, #class, max(1, #standard), Dense => false);
    for t from 0 to #standard-1 do S_(idx#(standard#t), t) = 1_kk;
    S = matrix S;
    if #standard > 0 and S % G != S then
        hpFail "hookStraighten: a semistandard tabloid is not reduced modulo the Pieri relations";
    memo#memoKey = new hpCokernelRelationData from {
        "shape" => lambda, "content" => w, "class" => class, "index" => idx,
        "standard" => standard, "standardIndex" => hashTable apply(#standard, t -> idx#(standard#t) => t),
        "groebner" => G, "numberOfRelations" => #relations, "relationRank" => relationRank
    }
);

-- coordinates of the tabloid with index t in the semistandard complement:
-- reduce its basis vector modulo the relation space; rows => coefficient
hpCokernelReduce = (D, t) -> (
    G := D#"groebner"; kk := ring gens G;
    N := #(D#"class");
    e := mutableMatrix(kk, N, 1); e_(t, 0) = 1_kk; e = matrix e;
    r := e % G;
    standardIndex := D#"standardIndex";
    new HashTable from for i from 0 to N-1 list (
        c := r_(i, 0);
        if c == 0 then continue;
        if not standardIndex#?i then hpFail "hookStraighten: the cokernel reduction is not supported on semistandard tabloids";
        D#"standard"#(standardIndex#i) => c)
);

-- the normal form (rows => coefficient) of validated rows via the cokernel
hpCokernelNormalFormRows = (caller, T) -> (
    lambda := apply(T, row -> #row);
    if #T == 0 then return new HashTable from {T => 1_(hpField())};
    N := max flatten T;
    w := hpTabloidContent(T, N);
    expansion := hpCanonicalHookExpansion T;
    -- a repeated column entry in a diagonal hook makes the tabloid zero
    -- before any relation is needed
    if #expansion == 0 then return new HashTable from {};
    D := hpCokernelData(lambda, w);
    out := new MutableHashTable;
    for T0 in keys expansion do (
        nf := hpCokernelReduce(D, D#"index"#T0);
        for U in keys nf do hpAddTerm(out, U, expansion#T0 * nf#U);
    );
    new HashTable from out
);

-- compare the algorithm with the cokernel on whole weight spaces
hpVerifyAgainstCokernel = (lambda, contents, restriction, value) -> (
    t0 := cpuTime();
    tabloids := 0; agreements := 0; disagreements := {}; relations := 0;
    for w in contents do (
        D := hpCokernelData(lambda, w);
        relations = relations + D#"numberOfRelations";
        class := D#"class";
        for t from 0 to #class-1 do (
            tabloids = tabloids + 1;
            nfC := hpCokernelReduce(D, t);
            nfA := first hpHookStraightenFromAnyHooks("verifyStraighteningAgainstCokernel", class#t);
            if #nfC == #nfA and all(keys nfC, U -> nfA#?U and nfA#U == nfC#U) then
                agreements = agreements + 1
            else disagreements = append(disagreements,
                {new HookTabloid from class#t, hpTabloidTable nfA, hpTabloidTable nfC});
        );
    );
    passed := #disagreements == 0;
    hpVerificationReport("Cokernel", {
        "shape" => lambda,
        "restriction" => restriction,
        "restrictionValue" => value,
        "tabloids" => tabloids,
        "relations" => relations,
        "agreements" => agreements,
        "disagreements" => disagreements,
        "seconds" => cpuTime() - t0,
        "passed" => passed,
        "conclusion" => (if passed then
            "hookStraighten equals the cokernel normal form on every tabloid of this set"
            else "hookStraighten differs from the cokernel normal form; see the disagreements")})
);

-- the contents of all hook tabloids of shape lambda with entries in 1..n
hpContentsOfShape = (lambda, n) -> (
    unique apply(hpAllHookTabloids(lambda, n), T -> hpTabloidContent(T, n))
);

verifyStraighteningAgainstCokernel = method(Options => {CoefficientRing => QQ},
    TypicalValue => VerificationReport);
verifyStraighteningAgainstCokernel(List, ZZ) := opts -> (lambda, n) -> (
    hpValidatePartition("verifyStraighteningAgainstCokernel", lambda);
    hpValidateAlphabet("verifyStraighteningAgainstCokernel", n);
    K := hpValidateCoefficientRing("verifyStraighteningAgainstCokernel", opts#CoefficientRing);
    hpWithField(K, () -> hpVerifyAgainstCokernel(lambda, hpContentsOfShape(lambda, n), "alphabet", n))
);
verifyStraighteningAgainstCokernel(List, List) := opts -> (lambda, content) -> (
    hpValidatePartition("verifyStraighteningAgainstCokernel", lambda);
    hpValidateContent("verifyStraighteningAgainstCokernel", lambda, content);
    K := hpValidateCoefficientRing("verifyStraighteningAgainstCokernel", opts#CoefficientRing);
    hpWithField(K, () -> hpVerifyAgainstCokernel(lambda, {content}, "content", content))
);

------------------------------------------------------------
-- Frobenius / diagonal-hook utilities
------------------------------------------------------------

-- unprefixed: used by the paper's audit scripts under debug
armLength = (lambda,t) -> (
    -- t is 1-based
    (lambda#(t-1)) - t
);

------------------------------------------------------------
-- Boxes are encoded as pairs {r,c}, 1-based
------------------------------------------------------------

hpDiagHookBoxes = (lambda,t) -> (
    lamT := hpConjugatePartition lambda;
    rowBoxes := apply((lambda#(t-1)) - t + 1, j -> {t, t+j});
    colBoxes := apply((lamT#(t-1)) - t, i -> {t+1+i, t});
    rowBoxes | colBoxes
);

hpTruncDiagHookBoxes = (lambda,t,s) -> (
    -- 0 <= s <= armLength(lambda,t)
    lamT := hpConjugatePartition lambda;
    rowBoxes := apply(s+1, j -> {t, t+j}); -- includes diagonal box
    colBoxes := apply((lamT#(t-1)) - t, i -> {t+1+i, t});
    rowBoxes | colBoxes
);

hpRunningRegionBoxes = (lambda,t,s) -> (
    prevHooks := flatten apply(t-1, i -> hpDiagHookBoxes(lambda,i+1));
    prevHooks | hpTruncDiagHookBoxes(lambda,t,s)
);

------------------------------------------------------------
-- Read entries from a tableau at specified boxes
------------------------------------------------------------

hpEntriesAtBoxes = (T, boxes) -> apply(boxes, b -> (
    r := b#0;
    c := b#1;
    (T#(r-1))#(c-1)
));

------------------------------------------------------------
-- Cumulative vectors V_{t,s}(T) and the running order
------------------------------------------------------------

hpRunningCountVector = (T,t,s,N) -> (
    lambda := hpShapeOfTabloid T;
    vals := hpEntriesAtBoxes(T, hpRunningRegionBoxes(lambda,t,s));
    apply(N, q -> #(select(vals, x -> x <= q+1)))
);

hpRunningIndexPairs = lambda -> (
    r := hpFrobeniusRank lambda;
    flatten apply(r, i -> (
        t := i+1;
        a := armLength(lambda,t);
        apply(a+1, s -> {t,s})
    ))
);

-- the running partial order on precomputed running vector sequences:
-- decide at the LAST index where they differ; there T <= U when
-- V(U) <= V(T) coordinatewise.  Returns -1 (T < U), 1 (T > U), 0 (equal
-- vectors) or null (incomparable).
hpRunningCompareVectors = (vT,vU) -> (
    if #vT == 0 then return 0;
    i0 := null;
    for i from 0 to #vT-1 do if vT#i =!= vU#i then i0 = i;
    if i0 === null then return 0;
    a := vT#i0; b := vU#i0;
    if all(#a, q -> b#q <= a#q) then return -1;
    if all(#a, q -> a#q <= b#q) then return 1;
    null
);

-- the running vectors of a tabloid on the alphabet 1..N; N defaults to
-- the largest entry
hpRunningVectors = (T, N) -> (
    T = hpValidateStraighteningRows("runningVectors", T);
    if N === null then N = hpMaxEntryTabloid T;
    if not instance(N,ZZ) or N < 0 then
        error "runningVectors: N must be a nonnegative integer";
    if hpMaxEntryTabloid T > N then
        error "runningVectors: N must be at least the largest entry of the tabloid";
    hpRunningVectorsFast(T,N)
);

runningVectors = method(TypicalValue => List);
runningVectors(HookTabloid, ZZ) := (T, N) -> hpRunningVectors(toList T, N);
runningVectors(List, ZZ) := (T, N) -> hpRunningVectors(T, N);
runningVectors(Filling, ZZ) := (F, N) -> hpRunningVectors(F, N);
runningVectors HookTabloid := T -> hpRunningVectors(toList T, null);
runningVectors List := T -> hpRunningVectors(T, null);
runningVectors Filling := F -> hpRunningVectors(F, null);

-- the running indices {t, s} of a shape in the order of the vectors
runningIndices = method(TypicalValue => List);
runningIndices List := lambda -> (
    -- a list of lists is a tabloid given by its rows
    if #lambda > 0 and all(lambda, r -> instance(r, List)) then return runningIndices tabloid lambda;
    hpValidatePartition("runningIndices", lambda);
    hpRunningIndexPairs lambda
);
runningIndices HookTabloid := T -> hpRunningIndexPairs hpShapeOfTabloid toList T;
runningIndices Filling := F ->
    hpRunningIndexPairs hpShapeOfTabloid hpFillingToRows("runningIndices", F);

------------------------------------------------------------------------
-- runningCompare: the running partial order of Definition 3.2.3 of the
-- paper.  Two hook tabloids T, U of the same shape are compared at the
-- LARGEST running index (t0,s0) at which their running vectors differ;
-- there T <= U when V_{t0,s0}(U) <= V_{t0,s0}(T) coordinatewise.  Returns
-- the symbol < if T < U, > if T > U, == if the running vectors all agree,
-- and incomparable if the two vectors at the last disagreement are
-- incomparable (the partial order is not total).  Verbose => true prints
-- the last disagreement and the two vectors.
------------------------------------------------------------------------

runningCompare = method(Options => {Verbose => false}, TypicalValue => Symbol);
runningCompare(BasicList, BasicList) := opts -> (T, U) -> (
    T = hpValidateStraighteningRows("runningCompare", T);
    U = hpValidateStraighteningRows("runningCompare", U);
    if hpShapeOfTabloid T != hpShapeOfTabloid U then
        error "runningCompare: the two tabloids must have the same shape";
    if not instance(opts#Verbose, Boolean) then error "runningCompare: Verbose must be Boolean";
    N := max(hpMaxEntryTabloid T, hpMaxEntryTabloid U);
    vT := hpRunningVectorsFast(T, N);
    vU := hpRunningVectorsFast(U, N);
    code := hpRunningCompareVectors(vT, vU);
    answer := hpRunningSymbol code;
    if opts#Verbose then (
        i0 := null;
        for i from 0 to #vT-1 do if vT#i =!= vU#i then i0 = i;
        if i0 === null then
            stdio << "runningCompare: the running vectors of the two tabloids agree at every running index, so T == U" << endl
        else (
            index := (hpRunningIndexPairs hpShapeOfTabloid T)#i0;
            stdio << "runningCompare: the last disagreement is at the running index (t,s) = "
                << toString index << endl
                << "    V(T) = " << toString vT#i0 << endl
                << "    V(U) = " << toString vU#i0 << endl
                << "    " << (
                    if code === -1 then "V(U) <= V(T) coordinatewise, so T < U"
                    else if code === 1 then "V(T) <= V(U) coordinatewise, so T > U"
                    else "neither vector is coordinatewise below the other, so T and U are incomparable")
                << endl;
        );
    );
    answer
);

------------------------------------------------------------------------
-- Hook rings and the presentations of finite hook algebras
--
-- A hook type {a,b} is the hook partition (a+1,1^b): a boxes to the
-- right of the corner and b below it.  The hook ring of a set of types
-- on the alphabet 1..n is the polynomial ring with one variable z_i for
-- each semistandard hook tableau of those types, in the paper's term
-- order (Section 3.2 of the paper); the finite hook algebra generated
-- by those hook Schur functors inside the universal hook algebra H(V)
-- is its quotient by the straightening ideal.
--   hookRing(n, types), hookRing(n, MaxArm => A, MaxLeg => B)
--                                the polynomial ring, with its context
--                                (the record hpContextData) stored at
--                                R.cache#"hookContext"
--   straighteningIdeal R         the ideal of the straightening relations
--   pieriIdeal R                 the ideal of the Pieri relations
--   nonstandardMonomialIdeal R   the monomial ideal of the nonstandard
--                                products, the predicted initial ideal
--   weightTruncationIdeal R      the monomial ideal of a content bound
--   hasQuadraticGroebnerBasis I  the Groebner certificate of an ideal
--   paperOrder R                 the rows of the compiled term order
--   verifyPaperOrder R           the audit of that order against the
--                                running order of Definition 3.2.3
-- The engines store a hook as the native pair {rowTail, colFull}: the
-- row without its corner, then the full first column including the
-- corner; the context indexes the variables by these pairs.  A
-- presentation ideal carries its context at I.cache#"hookContext" and
-- its record (hpPresentationData) at I.cache#"presentation"; the
-- Groebner certificate is cached lazily at I.cache#"groebnerCertificate".
-- Section 08 builds fine Segre rings on the same records, and section 09
-- installs the accessors and the hook algebra constructor.
------------------------------------------------------------------------

-- the internal records: the context of a ring and the presentation of an
-- ideal; both are inspected with peek
hpContextData = new Type of HashTable;
hpPresentationData = new Type of HashTable;

hpFHRValidType = (n,h) -> (
    instance(h,List) and #h == 2 and
    instance(h#0,ZZ) and instance(h#1,ZZ) and
    h#0 >= 0 and h#1 >= 0 and h#1 < n
);

hpFHRHookType = hook -> {#(hook#0),#(hook#1)-1};
hpFHRTypeOuter = (h,g) -> h#0 > g#0 and h#1 > g#1;
hpFHRTypeOrder = types -> reverse sort(types,h -> (h#0+h#1+1,h#0,h#1));

hpFHRHookBasis = (n,h) -> (
    if not hpFHRValidType(n,h) then error "hpFHRHookBasis: invalid hook type";
    hookSSYTBasis(h#0+1,h#1,n)
);

-- Enumerate only the tableaux that survive a content bound.  This avoids
-- constructing a large Schur basis merely to discard most of it.
hpFHRBoundedWeakSeqs = (k,lo,n,capacities) -> (
    if k == 0 then return {{}};
    out := {};
    for x from lo to n do if capacities#(x-1) > 0 then (
        nextCap := apply(n,q -> capacities#q-
            (if q == x-1 then 1 else 0));
        for tail in hpFHRBoundedWeakSeqs(k-1,x,n,nextCap) do
            out = out|{prepend(x,tail)};
    );
    out
);

hpFHRHookBasisBounded = (n,h,w) -> (
    if not hpFHRValidType(n,h) then
        error "hpFHRHookBasisBounded: invalid hook type";
    if not hpWeightValid(n,w) then
        error "hpFHRHookBasisBounded: invalid weight bound";
    out := {};
    for col in hpStrictSeqs(h#1+1,1,n) do (
        colContent := apply(n,q -> #select(col,x -> x == q+1));
        if hpWeightLeq(colContent,w) then (
            capacities := apply(n,q -> w#q-colContent#q);
            for rowTail in hpFHRBoundedWeakSeqs(
                h#0,col#0,n,capacities) do
                out = out|{{rowTail,col}};
        );
    );
    out
);

-- the hook length formula for S_(a+1,1^b)(k^n)
hpFHRHookDimension = (n,h) -> (
    a := h#0;
    b := h#1;
    binomial(n+a,a+b+1)*binomial(a+b,b)
);

-- The row tail omits the corner, while colFull contains it, so their
-- concatenation records every box of the hook exactly once.
hpFHRHookContent = (n,hook) ->
    apply(n,q -> #select(hook#0|hook#1,x -> x == q+1));

hpWeightValid = (n,w) -> instance(w,List) and #w == n and
    all(w,x -> instance(x,ZZ) and x >= 0);

hpWeightLeq = (u,w) -> #u == #w and all(#w,i -> u#i <= w#i);

hpWeightAdd = (u,v) -> (
    if #u != #v then error "hpWeightAdd: incompatible content vectors";
    apply(#u,i -> u#i+v#i)
);

hpWeightSub = (u,v) -> (
    if #u != #v then error "hpWeightSub: incompatible content vectors";
    apply(#u,i -> u#i-v#i)
);

-- the variable of a native hook in the ring of the context
hpFHRVariable = (H,hook) -> (
    if not H#"indexByHook"#?hook then
        error("hookVariable: the hook " | toString hook | " (row tail, full column)" |
            " is not a generator of this hook ring");
    H#"ring"_(H#"indexByHook"#hook)
);

-- At stage {p,s}, earlier hooks have been scanned completely, the hook in
-- position p contributes its column and first s arm entries, and later
-- hooks contribute nothing.  Counting scanned entries above q is the
-- nonnegative complement of the paper's cumulative running vector.
hpFHRScannedAbove = (hook,hookPosition,stagePosition,s,q) -> (
    if hookPosition < stagePosition then
        return #select(hook#0|hook#1,x -> x > q);
    if hookPosition > stagePosition then return 0;
    labels := hook#1 | take(hook#0,s);
    #select(labels,x -> x > q)
);

-- the rows of the paper order with their meaning: a list of pairs
-- {label, weights}, one per weight row of the monomial order
hpFHRPaperOrderLabeledRows = (n,orderedTypes,basis) -> (
    positions := new HashTable from apply(#orderedTypes,i -> orderedTypes#i => i);

    -- weighted degree lexicographic order on hook type monomials
    rows := {{"boxes", apply(basis,hook -> #(hook#0)+#(hook#1))}};
    for h in orderedTypes do rows = rows | {{"type " | toString h,
        apply(basis,hook -> if hpFHRHookType(hook) == h then 1 else 0)}};

    stages := {};
    for p from 0 to #orderedTypes-1 do (
        h := orderedTypes#p;
        for s from 0 to h#0 do stages = stages | {{p,s}};
    );

    -- the last scan stage of disagreement is the significant one
    for stage in reverse stages do (
        p := stage#0;
        s := stage#1;
        if n > 1 then for q from 1 to n-1 do (
            row := apply(basis,hook -> hpFHRScannedAbove(
                hook,positions#(hpFHRHookType hook),p,s,q));
            if any(row,x -> x != 0) then rows = rows | {{
                "stage (type " | toString orderedTypes#p | ", " | toString s |
                " arm entries), entries > " | toString q, row}};
        );
    );
    rows
);

hpFHRPaperOrderRows = (n,orderedTypes,basis) ->
    apply(hpFHRPaperOrderLabeledRows(n,orderedTypes,basis), last);

hpFHRMonomialOrderParts = rows -> apply(rows,row -> Weights => row) | {Lex};

------------------------------------------------------------------------
-- Reaching the context from a ring, an algebra or an ideal
------------------------------------------------------------------------

-- the hook ring (a polynomial ring carrying "hookContext") under a ring,
-- a quotient of it, or an ideal in it
hpHookRingOf = (caller, X) -> (
    R := if instance(X, Ideal) then ring X else X;
    while not (instance(R, PolynomialRing) and R.?cache and R.cache#?"hookContext") do (
        if not instance(R, QuotientRing) then
            error(caller | ": expected a hook ring, a hook algebra, a fine Segre ring" |
                " or a quotient of one (from hookRing, hookAlgebra, fineSegreRing or" |
                " fineSegreAlgebra)");
        R = ambient R;
    );
    R
);

hpHookContext = (caller, X) -> (hpHookRingOf(caller, X)).cache#"hookContext";

hpIsFineContext = H -> H#"kind" == "FineSegreRetract";

-- the functions that have no meaning on a fine Segre ring
hpRequireHookKind = (caller, H) -> if hpIsFineContext H then
    error(caller | ": expected a hook ring or hook algebra, not a fine Segre" |
        " ring; apply " | caller | " to the factors (see factors)");

-- the lift of an element of a hook ring, a hook algebra or a quotient of
-- one to the hook ring: the pair (R, g)
hpLiftToHookRing = (caller, f) -> (
    if not instance(f, RingElement) then error(caller | ": expected a ring element");
    R := ring f;
    g := f;
    while not (instance(R, PolynomialRing) and R.?cache and R.cache#?"hookContext") do (
        if not instance(R, QuotientRing) then
            error(caller | ": the element does not belong to a hook ring, a hook" |
                " algebra, a fine Segre ring or a quotient of one");
        S := ambient R;
        g = lift(g, S);
        R = S;
    );
    (R, g)
);

------------------------------------------------------------------------
-- hookRing
------------------------------------------------------------------------

-- the rectangle of types {a,b} with 0 <= a <= A and 0 <= b <= B
hpHookRectangleTypes = (caller, n, A, B) -> (
    if A === null or B === null then
        error(caller | ": give the list of hook types or both MaxArm and MaxLeg");
    if not instance(A,ZZ) or not instance(B,ZZ) or A < 0 or B < 0 then
        error(caller | ": MaxArm and MaxLeg must be nonnegative integers");
    if B >= n then
        error(caller | ": MaxLeg must be at most n-1");
    flatten for a from 0 to A list for b from 0 to B list {a,b}
);

-- the hook ring of the types on the alphabet 1..n over K, with its
-- context stored at R.cache#"hookContext"
hpHookRingBuild = (caller, n, types, K, weight, order, variableLimit, force) -> (
    K = hpValidateCoefficientRing(caller, K);
    if not instance(n, ZZ) or n < 1 then error(caller | ": the alphabet size n must be a positive integer");
    if not instance(variableLimit,ZZ) or variableLimit < 0 then
        error(caller | ": VariableLimit must be a nonnegative integer");
    if not instance(force,Boolean) then
        error(caller | ": Force must be Boolean");
    if weight =!= null and not hpWeightValid(n,weight) then
        error(caller | ": Weight must be a list of n nonnegative integers");
    if not instance(types,List) or #types == 0 then
        error(caller | ": the hook types must form a nonempty list");
    if any(types,h -> not hpFHRValidType(n,h)) then
        error(caller | ": each hook type must be {a,b} with a >= 0 and 0 <= b < n");
    if #unique types != #types then
        error(caller | ": the list of hook types contains a duplicate");
    if order != "Paper" and order != "GRevLex" then
        error(caller | ": MonomialOrder must be \"Paper\" or \"GRevLex\"");

    orderedTypes := hpFHRTypeOrder types;
    basisByType := if weight === null then (
        fullBasisByType := new HashTable from apply(orderedTypes,
            h -> h => hpFHRHookBasis(n,h));
        for h in orderedTypes do
            if #(fullBasisByType#h) != hpFHRHookDimension(n,h) then
                error(caller | ": the hook basis enumeration failed its dimension check");
        fullBasisByType
    ) else new HashTable from apply(orderedTypes,h -> h =>
        hpFHRHookBasisBounded(n,h,weight));
    basis := {};
    for h in orderedTypes do basis = basis | basisByType#h;
    N := #basis;
    if N > variableLimit and not force then
        error(caller | ": " | toString N |
            " variables exceed VariableLimit=" | toString variableLimit |
            "; rerun with VariableLimit => N or Force => true after inspecting the size");

    labeledRows := if order == "Paper" then
        hpFHRPaperOrderLabeledRows(n,orderedTypes,basis) else {};
    orderRows := apply(labeledRows, last);

    variableSymbols := apply(N,i -> getSymbol("z" | toString i));
    R := if order == "Paper" then
        K[variableSymbols,MonomialOrder => hpFHRMonomialOrderParts orderRows]
        else K[variableSymbols,MonomialOrder => GRevLex];

    metadata := apply(N,i -> (
        hook := basis#i;
        h := hpFHRHookType hook;
        new HashTable from {
            "index" => i,
            "type" => h,
            "column" => hook#1,
            "arm" => hook#0,
            "content" => hpFHRHookContent(n,hook),
            "hook" => hook,
            "variable" => R_i
        }
    ));
    indexByHook := new HashTable from apply(N,i -> basis#i => i);

    H := new hpContextData from {
        "kind" => "FiniteHookRetract",
        "coefficientRing" => K,
        "ambientDimension" => n,
        "hookTypes" => orderedTypes,
        "weightBound" => weight,
        "isWeightRestricted" => (weight =!= null),
        "unrestrictedDimensionsByType" => apply(orderedTypes,
            h -> {h,hpFHRHookDimension(n,h)}),
        "basisByType" => basisByType,
        "basis" => basis,
        "numberOfVariables" => N,
        "ring" => R,
        "variables" => apply(N,i -> R_i),
        "metadataByIndex" => metadata,
        "indexByHook" => indexByHook,
        "truncationBound" => weight,
        "truncationContents" => apply(metadata,m -> m#"content"),
        "paperOrderRows" => orderRows,
        "paperOrderLabels" => apply(labeledRows, first),
        "monomialOrder" => order
    };
    R.cache#"hookContext" = H;
    R
);

hookRing = method(Options => {
    CoefficientRing => QQ,
    MaxArm => null,
    MaxLeg => null,
    Weight => null,
    MonomialOrder => "Paper",
    VariableLimit => 500,
    Force => false
}, TypicalValue => PolynomialRing);

hookRing(ZZ, List) := opts -> (n, types) -> (
    if opts#MaxArm =!= null or opts#MaxLeg =!= null then
        error "hookRing: a list of hook types cannot be combined with MaxArm or MaxLeg";
    hpHookRingBuild("hookRing", n, types, opts#CoefficientRing, opts#Weight,
        opts#MonomialOrder, opts#VariableLimit, opts#Force)
);

hookRing ZZ := opts -> n -> (
    if not instance(n, ZZ) or n < 1 then error "hookRing: the alphabet size n must be a positive integer";
    hpHookRingBuild("hookRing", n, hpHookRectangleTypes("hookRing", n, opts#MaxArm, opts#MaxLeg),
        opts#CoefficientRing, opts#Weight, opts#MonomialOrder, opts#VariableLimit, opts#Force)
);

------------------------------------------------------------------------
-- The paper order: its rows and the audit against the running order
------------------------------------------------------------------------

paperOrder = method(TypicalValue => List);
paperOrder PolynomialRing := R -> hpPaperOrderRows("paperOrder", R);
paperOrder QuotientRing := A -> hpPaperOrderRows("paperOrder", A);

hpPaperOrderRows = (caller, X) -> (
    H := hpHookContext(caller, X);
    if H#"monomialOrder" != "Paper" then return {};
    apply(#(H#"paperOrderRows"), k -> {H#"paperOrderLabels"#k, H#"paperOrderRows"#k})
);

-- Independent combinatorial comparison used to audit the compiled order.
-- It reads native nested tableaux but implements the paper's rule: use the
-- last running stage of disagreement, then lexicographically compare its
-- cumulative vector with the reversed orientation.
hpFHRRunningVectorsOfPair = (H,pair) ->
    runningVectors(hpHooksInternalToNestedRows(pair#0,pair#1),H#"ambientDimension");

hpFHRCompareRunningVectors = (leftVectors,rightVectors,n) -> (
    stage := #leftVectors-1;
    while stage >= 0 do (
        if leftVectors#stage != rightVectors#stage then (
            for q from 0 to n-1 do
                if leftVectors#stage#q != rightVectors#stage#q then
                    return if leftVectors#stage#q > rightVectors#stage#q then -1 else 1;
        );
        stage = stage-1;
    );
    0
);

verifyPaperOrder = method(Options => {
    ComparisonLimit => 100000,
    Force => false
}, TypicalValue => VerificationReport);

verifyPaperOrder PolynomialRing := opts -> R -> hpVerifyPaperOrder(R, opts);
verifyPaperOrder QuotientRing := opts -> A -> hpVerifyPaperOrder(A, opts);

hpVerifyPaperOrder = (X, opts) -> (
    H := hpHookContext("verifyPaperOrder", X);
    hpRequireHookKind("verifyPaperOrder", H);
    if not instance(opts#ComparisonLimit,ZZ) or opts#ComparisonLimit < 0 then
        error "verifyPaperOrder: ComparisonLimit must be a nonnegative integer";
    if not instance(opts#Force,Boolean) then
        error "verifyPaperOrder: Force must be Boolean";
    if H#"monomialOrder" != "Paper" then
        error "verifyPaperOrder: the ring does not use the paper order (MonomialOrder => \"Paper\")";
    strictComparisons := 0;
    paperTies := 0;
    mismatches := 0;
    firstMismatch := null;
    for outerType in H#"hookTypes" do for innerType in H#"hookTypes" do
        if hpFHRTypeOuter(outerType,innerType) then (
            tabloids := flatten for left in H#"basisByType"#outerType list
                for right in H#"basisByType"#innerType list {left,right};
            -- running vectors and monomials are computed once per tabloid,
            -- not once per comparison
            vectors := apply(tabloids,pair -> hpFHRRunningVectorsOfPair(H,pair));
            monomials := apply(tabloids,pair ->
                hpFHRVariable(H,pair#0)*hpFHRVariable(H,pair#1));
            if #tabloids >= 2 then
                for i from 0 to #tabloids-2 do for j from i+1 to #tabloids-1 do (
                    if strictComparisons+paperTies >= opts#ComparisonLimit and not opts#Force then
                        error("verifyPaperOrder: ComparisonLimit=" | toString opts#ComparisonLimit |
                            " exceeded; rerun with ComparisonLimit => N or Force => true");
                    comparison := hpFHRCompareRunningVectors(vectors#i,vectors#j,
                        H#"ambientDimension");
                    if comparison == 0 then paperTies = paperTies+1 else (
                        strictComparisons = strictComparisons+1;
                        leftMonomial := monomials#i;
                        rightMonomial := monomials#j;
                        m2SaysLeftLarger := leadMonomial(leftMonomial+rightMonomial)
                            == leftMonomial;
                        agrees := (comparison == 1 and m2SaysLeftLarger) or
                            (comparison == -1 and not m2SaysLeftLarger);
                        if not agrees then (
                            mismatches = mismatches+1;
                            if firstMismatch === null then firstMismatch = {
                                tabloid hpHooksInternalToNestedRows(tabloids#i#0,tabloids#i#1),
                                tabloid hpHooksInternalToNestedRows(tabloids#j#0,tabloids#j#1),
                                comparison,m2SaysLeftLarger};
                        );
                    );
                );
        );
    hpVerificationReport("PaperOrder", {
        "strictComparisons" => strictComparisons,
        "paperTies" => paperTies,
        "mismatches" => mismatches,
        "firstMismatch" => firstMismatch,
        "passed" => (mismatches == 0),
        "conclusion" => if mismatches == 0 then
            "the compiled monomial order agrees with the running order on every strict comparison"
            else "the compiled monomial order disagrees with the running order on " |
                toString mismatches | " comparisons; see firstMismatch"
    })
);

------------------------------------------------------------------------
-- Ideals from lists, and the weight truncation
------------------------------------------------------------------------

hpFHRZeroIdeal = R -> ideal(0_R);
hpFHRIdealFromList = (R,L) -> if #L == 0 then hpFHRZeroIdeal R else ideal L;
hpFHRMonomialIdealFromList = (R,L) -> if #L == 0 then monomialIdeal(0_R) else monomialIdeal ideal L;

hpWeightRequireContext = X -> (
    if not instance(X,HashTable) or not X#?"kind" or
       (X#"kind" != "FiniteHookRetract" and X#"kind" != "FineSegreRetract") then
        error "expected a hook ring or fine Segre context";
    if not X#?"truncationBound" or not X#?"truncationContents" then
        error "the context does not contain weight truncation data";
);

-- The forbidden monomial ideal is
--   <x^u : sum_i u_i content(x_i) is not <= weight>.
-- It is the sum, over coordinates q, of the monomial ideals whose
-- q-weight exceeds weight_q.  We enumerate the divisibility-minimal
-- generators of each coordinate ideal and then minimize their union.
hpWeightTruncationRecords = (X,relationLimit,force) -> (
    hpWeightRequireContext X;
    if not instance(relationLimit,ZZ) or relationLimit < 0 then
        hpFail("weight truncation: RelationLimit must be a nonnegative integer");
    if not instance(force,Boolean) then
        hpFail("weight truncation: Force must be Boolean");
    R := X#"ring";
    contents := X#"truncationContents";
    bound := X#"truncationBound";
    if bound === null or #contents == 0 then return {};

    candidates := {};
    candidateSet := new MutableHashTable;
    for q from 0 to #bound-1 do (
        active := select(toList(0..#contents-1),i -> contents#i#q > 0);
        visit := (startPosition,currentWeight,currentMonomial,support) -> (
            if #active > 0 then for p from startPosition to #active-1 do (
                i := active#p;
                newWeight := currentWeight+contents#i#q;
                newMonomial := currentMonomial*R_i;
                newSupport := if member(i,support) then support else support|{i};
                if newWeight > bound#q then (
                    if all(newSupport,j ->
                        newWeight-contents#j#q <= bound#q) then (
                        if not candidateSet#?newMonomial then (
                            candidateSet#newMonomial = true;
                            candidates = candidates|{newMonomial};
                            hpRelationLimitCheck("weight truncation", #candidates, "candidate monomials", relationLimit, force);
                        );
                    );
                ) else visit(p,newWeight,newMonomial,newSupport);
            );
        );
        visit(0,0,1_R,{});
    );

    if #candidates == 0 then return {};
    minimalIdeal := trim hpFHRIdealFromList(R,unique candidates);
    monomials := sort(flatten entries gens minimalIdeal,
        m -> (sum first exponents m,toString m));
    apply(monomials,m -> (
        exponentVector := first exponents m;
        content := apply(#bound,q ->
            sum apply(#contents,i -> exponentVector#i*contents#i#q));
        new HashTable from {
            "monomial" => m,
            "exponents" => exponentVector,
            "content" => content,
            "degree" => sum exponentVector,
            "overflowCoordinates" => select(toList(0..#bound-1),
                q -> content#q > bound#q)
        }
    ))
);

weightTruncationIdeal = method(Options => {
    RelationLimit => 10000,
    Force => false
}, TypicalValue => MonomialIdeal);

weightTruncationIdeal PolynomialRing := opts -> R -> hpWeightTruncationIdeal(R, opts);
weightTruncationIdeal QuotientRing := opts -> A -> hpWeightTruncationIdeal(A, opts);

hpWeightTruncationIdeal = (X, opts) -> (
    R := hpHookRingOf("weightTruncationIdeal", X);
    if not instance(opts#RelationLimit, ZZ) or opts#RelationLimit < 0 then
        error "weightTruncationIdeal: RelationLimit must be a nonnegative integer";
    if not instance(opts#Force, Boolean) then
        error "weightTruncationIdeal: Force must be true or false";
    if R.cache#?"weightTruncationIdeal" then return R.cache#"weightTruncationIdeal";
    H := R.cache#"hookContext";
    J := hpFHRMonomialIdealFromList(R,
        apply(hpWeightTruncationRecords(H,opts#RelationLimit,opts#Force), record -> record#"monomial"));
    R.cache#"weightTruncationIdeal" = J;
    J
);

hpWeightPairAllowed = (X,i,j) -> (
    bound := X#"truncationBound";
    if bound === null then return true;
    hpWeightLeq(hpWeightAdd(X#"truncationContents"#i,
        X#"truncationContents"#j),bound)
);

-- Higher weight boundary relations needed by the straightening Groebner
-- basis are exactly the divisibility-minimal overweight standard chains.
-- Enumerating chains directly is typically much smaller than constructing
-- the full cutoff ideal and reducing it modulo the bad quadratic ideal.
hpWeightStandardBoundaryRecords = (X,standardPair,relationLimit,force) -> (
    hpWeightRequireContext X;
    bound := X#"truncationBound";
    contents := X#"truncationContents";
    R := X#"ring";
    N := #contents;
    if bound === null or N == 0 then return {};
    records := {};
    pairCache := new MutableHashTable;
    pairIsStandard := (i,j) -> (
        key := {i,j};
        if not pairCache#?key then pairCache#key = standardPair(i,j);
        pairCache#key
    );

    addRecord := (indices,content,monomial) -> (
        records = records|{new HashTable from {
            "monomial" => monomial,
            "indices" => indices,
            "exponents" => first exponents monomial,
            "content" => content,
            "degree" => #indices,
            "overflowCoordinates" => select(toList(0..#bound-1),
                q -> content#q > bound#q)
        }};
        hpRelationLimitCheck("weight boundary", #records, "boundary monomials", relationLimit, force);
    );

    visit := (indices,currentContent,currentMonomial) -> (
        start := last indices+1;
        if start < N then for j from start to N-1 do
            if all(indices,i -> pairIsStandard(i,j)) then (
                newIndices := indices|{j};
                newContent := hpWeightAdd(currentContent,contents#j);
                newMonomial := currentMonomial*R_j;
                if hpWeightLeq(newContent,bound) then
                    visit(newIndices,newContent,newMonomial)
                else if #newIndices >= 3 and all(newIndices,i ->
                    hpWeightLeq(hpWeightSub(newContent,contents#i),bound)) then
                    addRecord(newIndices,newContent,newMonomial);
            );
    );

    for i from 0 to N-1 do visit({i},contents#i,R_i);
    sort(records,record -> (record#"degree",toString record#"monomial"))
);

------------------------------------------------------------------------
-- Rank two normal forms of pairs of generators, and standard pairs
------------------------------------------------------------------------

-- the complete rank two normal form of a nested pair of native hooks,
-- memoized; shared by the hook and the fine Segre presentations
hpFHRPairNormalForm = (H,pair,memo) -> (
    if memo#?pair then return memo#pair;
    rows := hpHooksInternalToNestedRows(pair#0,pair#1);
    answer := new MutableHashTable;
    if hpIsSemistandardRank2Unchecked rows then answer#pair = 1_(H#"coefficientRing") else (
        (rowsNF,iters) := hpStraightenRank2FromAnyHooks("straighteningIdeal", rows);
        for rowsOut in keys rowsNF do (
            pairOut := hpNestedToHooksInternal rowsOut;
            if apply(pairOut,hpFHRHookType) != apply(pair,hpFHRHookType) then
                hpFail("straighteningIdeal: the rank two straightening changed hook types");
            if not H#"indexByHook"#?(pairOut#0) or
            not H#"indexByHook"#?(pairOut#1) then
                hpFail("straighteningIdeal: the rank two straightening left the basis of the hook ring");
            hpAddTerm(answer,pairOut,rowsNF#rowsOut);
        );
    );
    frozen := new HashTable from answer;
    memo#pair = frozen;
    frozen
);

-- The same normal form, re-raising a failure (a non-invertible integer or
-- a singular elimination system in small characteristic) with the pair of
-- generators that was being straightened.
hpFHRPairNormalFormAt = (H,pair,memo,i,j) -> (
    if memo#?pair then return memo#pair;
    hpLastError = null;
    result := try hpFHRPairNormalForm(H,pair,memo) else null;
    if result === null then hpFail(
        (if hpLastError =!= null then hpLastError else "HookPieri: straightening failed") |
        " [while straightening the product of generators " | toString i | " and " |
        toString j | ", hook types " | toString hpFHRHookType(pair#0) | " and " |
        toString hpFHRHookType(pair#1) | ", " |
        toString(sum apply(pair, h -> #(h#0) + #(h#1))) | " boxes, over " |
        toString(H#"coefficientRing") | "]");
    result
);

hpFHRNormalFormPolynomial = (H,normalForm) -> (
    answer := 0_(H#"ring");
    for pair in keys normalForm do (
        if #pair != 2 then error "hpFHRNormalFormPolynomial: expected rank two";
        answer = answer + normalForm#pair*
            hpFHRVariable(H,pair#0)*hpFHRVariable(H,pair#1);
    );
    answer
);

-- the predicate on pairs of basis indices of a hook ring: the product is
-- a standard monomial (a semistandard nested pair)
hpFHRStandardPair = basis -> (i,j) -> (
    hi := hpFHRHookType(basis#i);
    hj := hpFHRHookType(basis#j);
    if hpFHRTypeOuter(hi,hj) then
        return hpIsSemistandardRank2Unchecked hpHooksInternalToNestedRows(basis#i,basis#j);
    if hpFHRTypeOuter(hj,hi) then
        return hpIsSemistandardRank2Unchecked hpHooksInternalToNestedRows(basis#j,basis#i);
    false
);

------------------------------------------------------------------------
-- nonstandardMonomialIdeal
------------------------------------------------------------------------

-- the nonstandard monomials of a hook ring: the products of the pairs of
-- generators that are not standard (nonchain, nonsemistandard, or over
-- the weight bound) and the minimal overweight standard chains
hpFHRNonstandardMonomials = H -> (
    R := H#"ring";
    basis := H#"basis";
    N := #basis;
    standardPair := hpFHRStandardPair basis;
    bad := flatten for i from 0 to N-1 list for j from i to N-1 list (
        if not hpWeightPairAllowed(H,i,j) or not standardPair(i,j) then R_i*R_j else continue);
    bad | apply(hpWeightStandardBoundaryRecords(H,standardPair,1000000,true),
        record -> record#"monomial")
);

nonstandardMonomialIdeal = method(TypicalValue => MonomialIdeal);
nonstandardMonomialIdeal PolynomialRing := R -> hpNonstandardMonomialIdeal R;
nonstandardMonomialIdeal QuotientRing := A -> hpNonstandardMonomialIdeal A;

hpNonstandardMonomialIdeal = X -> (
    R := hpHookRingOf("nonstandardMonomialIdeal", X);
    if R.cache#?"nonstandardMonomialIdeal" then return R.cache#"nonstandardMonomialIdeal";
    H := R.cache#"hookContext";
    J := hpFHRMonomialIdealFromList(R,
        if hpIsFineContext H then hpFineNonstandardMonomials H else hpFHRNonstandardMonomials H);
    R.cache#"nonstandardMonomialIdeal" = J;
    J
);

------------------------------------------------------------------------
-- The straightening presentation
------------------------------------------------------------------------

hpValidatePresentationOptions = (caller, opts) -> (
    if not instance(opts#RelationLimit,ZZ) or opts#RelationLimit < 0 then
        error(caller | ": RelationLimit must be a nonnegative integer");
    if not instance(opts#Force,Boolean) then
        error(caller | ": Force must be Boolean");
    if not instance(opts#Validate,Boolean) then
        error(caller | ": Validate must be Boolean");
);

-- the presentation record of the straightening relations of a hook ring:
-- one relation per nonstandard pair of generators (a nonchain product is
-- zero, a nonsemistandard nested pair is replaced by its rank two normal
-- form, an overweight product is zero) and the minimal overweight
-- standard chains
hpHookStraighteningPresentationBody = (opts, H) -> (
    R := H#"ring";
    basis := H#"basis";
    N := #basis;
    relations := {};
    badMonomials := {};
    records := {};
    weightRecords := {};
    memo := new MutableHashTable;

    standardPair := hpFHRStandardPair basis;

    for i from 0 to N-1 do for j from i to N-1 do (
        target := R_i*R_j;
        if not hpWeightPairAllowed(H,i,j) then (
            content := hpWeightAdd(H#"truncationContents"#i,
                H#"truncationContents"#j);
            record := new HashTable from {
                "kind" => "weight-truncation",
                "indices" => {i,j},
                "target" => target,
                "relation" => target,
                "monomial" => target,
                "content" => content,
                "degree" => 2,
                "overflowCoordinates" => select(
                    toList(0..#(H#"weightBound")-1),
                    q -> content#q > H#"weightBound"#q)
            };
            relations = relations|{target};
            badMonomials = badMonomials|{target};
            records = records|{record};
            weightRecords = weightRecords|{record};
        ) else (
            hi := hpFHRHookType(basis#i);
            hj := hpFHRHookType(basis#j);
            if not hpFHRTypeOuter(hi,hj) and not hpFHRTypeOuter(hj,hi) then (
                relations = relations | {target};
                badMonomials = badMonomials | {target};
                records = records | {new HashTable from {
                    "kind" => "nonchain", "indices" => {i,j},
                    "target" => target, "relation" => target
                }};
            ) else (
                pair := if hpFHRTypeOuter(hi,hj) then {basis#i,basis#j}
                    else {basis#j,basis#i};
                rows := hpHooksInternalToNestedRows(pair#0,pair#1);
                if not hpIsSemistandardRank2Unchecked rows then (
                    normalForm := hpFHRPairNormalFormAt(H,pair,memo,i,j);
                    tailPolynomial := hpFHRNormalFormPolynomial(H,normalForm);
                    relation := target-tailPolynomial;
                    relations = relations | {relation};
                    badMonomials = badMonomials | {target};
                    records = records | {new HashTable from {
                        "kind" => "straightening", "indices" => {i,j},
                        "targetPair" => pair, "target" => target,
                        "normalForm" => normalForm, "relation" => relation
                    }};
                );
            );
        );
        hpRelationLimitCheck("straighteningIdeal", #relations, "relations", opts#RelationLimit, opts#Force);
    );

    higherWeightRecords := hpWeightStandardBoundaryRecords(H,standardPair,
        opts#RelationLimit,opts#Force);
    for record in higherWeightRecords do (
        monomial := record#"monomial";
        relations = relations|{monomial};
        badMonomials = badMonomials|{monomial};
        relationRecord := new HashTable from join(pairs record,{
            "kind" => "weight-truncation",
            "target" => monomial,
            "relation" => monomial
        });
        records = records|{relationRecord};
        weightRecords = weightRecords|{relationRecord};
        hpRelationLimitCheck("straighteningIdeal", #relations, "relations", opts#RelationLimit, opts#Force);
    );

    I := hpFHRIdealFromList(R,relations);
    Jbad := hpFHRIdealFromList(R,badMonomials);
    allQuadratic := all(relations,f -> degree f == {2});
    leadingChecks := apply(#relations,k -> leadMonomial(relations#k) == badMonomials#k);
    allLeading := all(leadingChecks,x -> x);
    if opts#Validate and H#"monomialOrder" == "Paper" and not allLeading then
        hpFail("straighteningIdeal: the compiled order failed a leading term check");

    new hpPresentationData from {
        "kind" => "HookStraighteningPresentation",
        "context" => H,
        "ring" => R,
        "relations" => relations,
        "relationRecords" => records,
        "numberOfRelations" => #relations,
        "numberOfWeightRelations" => #weightRecords,
        "weightRelationRecords" => sort(weightRecords,
            r -> {if r#?"degree" then r#"degree" else 2,
                  toString(if r#?"monomial" then r#"monomial" else r#"target")}),
        "badMonomials" => badMonomials,
        "badMonomialIdeal" => Jbad,
        "ideal" => I,
        "quotient" => R/I,
        "allRelationsQuadratic" => allQuadratic,
        "allLeadingTermsCorrect" => allLeading,
        "leadingTermChecks" => leadingChecks
    }
);

straighteningIdeal = method(Options => {
    Validate => true,
    RelationLimit => 10000,
    Force => false
}, TypicalValue => Ideal);

straighteningIdeal PolynomialRing := opts -> R -> (
    H := hpHookContext("straighteningIdeal", R);
    hpValidatePresentationOptions("straighteningIdeal", opts);
    if R.cache#?"straighteningIdeal" then return R.cache#"straighteningIdeal";
    S := hpWithField(H#"coefficientRing", () ->
        if hpIsFineContext H then hpFineStraighteningPresentationBody(opts, H)
        else hpHookStraighteningPresentationBody(opts, H));
    I := S#"ideal";
    I.cache#"hookContext" = H;
    I.cache#"presentation" = S;
    R.cache#"straighteningIdeal" = I;
    I
);

straighteningIdeal QuotientRing := opts -> A -> straighteningIdeal(
    hpHookRingOf("straighteningIdeal", A),
    Validate => opts#Validate, RelationLimit => opts#RelationLimit, Force => opts#Force);

------------------------------------------------------------------------
-- The Pieri presentation
------------------------------------------------------------------------

-- the presentation record of the Pieri relations of a hook ring: the
-- nonchain products vanish and, for each nested pair of types, the
-- columns of the crossed hook Pieri map of Section 4 of the paper are
-- relations; with a weight bound only the relevant weight blocks of
-- those maps are built
hpHookPieriPresentationBody = (opts, H) -> (
    R := H#"ring";
    basis := H#"basis";
    N := #basis;
    relations := {};
    records := {};
    weightRecords := {};

    standardPair := hpFHRStandardPair basis;

    -- every product of types that do not form a strict chain is zero
    for i from 0 to N-1 do for j from i to N-1 do (
        relation := R_i*R_j;
        if not hpWeightPairAllowed(H,i,j) then (
            content := hpWeightAdd(H#"truncationContents"#i,
                H#"truncationContents"#j);
            record := new HashTable from {
                "kind" => "weight-truncation",
                "indices" => {i,j},
                "relation" => relation,
                "monomial" => relation,
                "content" => content,
                "degree" => 2,
                "overflowCoordinates" => select(
                    toList(0..#(H#"weightBound")-1),
                    q -> content#q > H#"weightBound"#q)
            };
            relations = relations|{relation};
            records = records|{record};
            weightRecords = weightRecords|{record};
        ) else (
            hi := hpFHRHookType(basis#i);
            hj := hpFHRHookType(basis#j);
            if not hpFHRTypeOuter(hi,hj) and not hpFHRTypeOuter(hj,hi) then (
                relations = relations | {relation};
                records = records | {new HashTable from {
                    "kind" => "nonchain", "indices" => {i,j},
                    "relation" => relation
                }};
            );
        );
        hpRelationLimitCheck("pieriIdeal", #relations, "relations", opts#RelationLimit, opts#Force);
    );

    -- for h = (a,b) > g = (c,d), the native matrix of
    -- S_(c+1,1^b) tensor S_(a+1,1^d) -> S_(a+1,1^b) tensor S_(c+1,1^d)
    for outerType in H#"hookTypes" do for innerType in H#"hookTypes" do
        if hpFHRTypeOuter(outerType,innerType) then (
            matrixBlocks := if H#"weightBound" === null then {
                hpHookInjectionMatrixFull(
                    outerType#0+1,outerType#1,
                    innerType#0+1,innerType#1,
                    H#"ambientDimension")
            } else (
                outerContents := unique apply(H#"basisByType"#outerType,
                    hook -> hpFHRHookContent(H#"ambientDimension",hook));
                innerContents := unique apply(H#"basisByType"#innerType,
                    hook -> hpFHRHookContent(H#"ambientDimension",hook));
                weights := unique flatten apply(outerContents,u ->
                    apply(innerContents,v -> hpWeightAdd(u,v)));
                weights = select(weights,w -> hpWeightLeq(w,H#"weightBound"));
                apply(weights,w -> hookInjectionMatrixWeight(
                    outerType#0+1,outerType#1,
                    innerType#0+1,innerType#1,
                    H#"ambientDimension",w))
            );
            for matrixData in matrixBlocks do (
                M := matrixData#0;
                sourcePairs := matrixData#1;
                targetPairs := matrixData#2;
                if opts#Validate and rank M != numColumns M then
                    hpFail("pieriIdeal: the native Pieri matrix is not injective");
                -- read the matrix once instead of one engine call per entry
                columns := entries transpose M;
                for columnIndex from 0 to numColumns M-1 do (
                    sourcePair := sourcePairs#columnIndex;
                    column := columns#columnIndex;
                    relation := 0_R;
                    for rowIndex from 0 to numRows M-1 do if column#rowIndex != 0 then (
                        targetPair := targetPairs#rowIndex;
                        relation = relation + column#rowIndex*
                            hpFHRVariable(H,targetPair#0)*hpFHRVariable(H,targetPair#1);
                    );
                    if relation == 0 then
                        hpFail("pieriIdeal: a native Pieri image is zero");
                    relations = relations | {relation};
                    records = records | {new HashTable from {
                        "kind" => "pieri", "outerType" => outerType,
                        "innerType" => innerType,
                        "source" => sourcePair,
                        "relation" => relation
                    }};
                    hpRelationLimitCheck("pieriIdeal", #relations, "relations", opts#RelationLimit, opts#Force);
                );
            );
        );

    higherWeightRecords := hpWeightStandardBoundaryRecords(H,standardPair,
        opts#RelationLimit,opts#Force);
    for record in higherWeightRecords do (
        monomial := record#"monomial";
        relations = relations|{monomial};
        relationRecord := new HashTable from join(pairs record,{
            "kind" => "weight-truncation",
            "relation" => monomial
        });
        records = records|{relationRecord};
        weightRecords = weightRecords|{relationRecord};
        hpRelationLimitCheck("pieriIdeal", #relations, "relations", opts#RelationLimit, opts#Force);
    );

    I := hpFHRIdealFromList(R,relations);
    allQuadratic := all(relations,f -> sum degree f == 2);
    new hpPresentationData from {
        "kind" => "HookPieriPresentation",
        "context" => H,
        "ring" => R,
        "relations" => relations,
        "relationRecords" => records,
        "numberOfRelations" => #relations,
        "numberOfWeightRelations" => #weightRecords,
        "weightRelationRecords" => sort(weightRecords,
            r -> {if r#?"degree" then r#"degree" else 2,
                  toString(if r#?"monomial" then r#"monomial" else r#"target")}),
        "ideal" => I,
        "quotient" => R/I,
        "allRelationsQuadratic" => allQuadratic
    }
);

pieriIdeal = method(Options => {
    Validate => true,
    RelationLimit => 10000,
    Force => false
}, TypicalValue => Ideal);

pieriIdeal PolynomialRing := opts -> R -> (
    H := hpHookContext("pieriIdeal", R);
    if hpIsFineContext H then
        error("pieriIdeal: a fine Segre ring has no Pieri presentation of its own;" |
            " its Pieri presentation is ker fineSegreMap(W, R1/pieriIdeal R1, R2/pieriIdeal R2)" |
            " with (R1, R2) = factors W");
    hpValidatePresentationOptions("pieriIdeal", opts);
    if R.cache#?"pieriIdeal" then return R.cache#"pieriIdeal";
    S := hpWithField(H#"coefficientRing", () -> hpHookPieriPresentationBody(opts, H));
    I := S#"ideal";
    I.cache#"hookContext" = H;
    I.cache#"presentation" = S;
    R.cache#"pieriIdeal" = I;
    I
);

pieriIdeal QuotientRing := opts -> A -> pieriIdeal(
    hpHookRingOf("pieriIdeal", A),
    Validate => opts#Validate, RelationLimit => opts#RelationLimit, Force => opts#Force);

------------------------------------------------------------------------
-- hasQuadraticGroebnerBasis
------------------------------------------------------------------------

-- The certificate of an ideal I of a hook ring or fine Segre ring: its
-- generators are quadratic, they are a Groebner basis for the ring's
-- monomial order, and the initial ideal is the nonstandard monomial
-- ideal of the ring; the record is cached at I.cache#"groebnerCertificate"
-- and the verdict is its "passed" entry.
hpGroebnerCertificate = I -> (
    if I.cache#?"groebnerCertificate" then return I.cache#"groebnerCertificate";
    R := ring I;
    if not (instance(R, PolynomialRing) and R.?cache and R.cache#?"hookContext") then
        error("hasQuadraticGroebnerBasis: expected an ideal of a hook ring or of a fine" |
            " Segre ring (straighteningIdeal, pieriIdeal or ideal A)");
    relations := select(flatten entries gens I, f -> f != 0);
    Jbad := nonstandardMonomialIdeal R;
    allQuadratic := all(relations, f -> sum degree f == 2);
    G := gb I;
    computedInitial := monomialIdeal leadTerm G;
    suppliedInitial := hpFHRMonomialIdealFromList(R, apply(relations, leadTerm));
    computedQuadratic := all(degrees source gens G, d -> sum d == 2);
    suppliedGroebner := (suppliedInitial == computedInitial);
    initialMatches := (computedInitial == Jbad);
    allLeading := if I.cache#?"presentation" and (I.cache#"presentation")#?"allLeadingTermsCorrect"
        then (I.cache#"presentation")#"allLeadingTermsCorrect"
        else (suppliedInitial == Jbad);
    passed := allQuadratic and suppliedGroebner and initialMatches;
    if debugLevel > 0 and not passed then (
        if not allQuadratic then print "hasQuadraticGroebnerBasis: a generator is not quadratic";
        if not suppliedGroebner then print "hasQuadraticGroebnerBasis: the generators are not a Groebner basis for the monomial order of the ring";
        if not initialMatches then print "hasQuadraticGroebnerBasis: the initial ideal differs from nonstandardMonomialIdeal";
    );
    certificate := new HashTable from {
        "kind" => "QuadraticGroebnerCertificate",
        "passed" => passed,
        "allRelationsQuadratic" => allQuadratic,
        "allSuppliedLeadingTermsCorrect" => allLeading,
        "numberOfComputedBasisElements" => numColumns gens G,
        "computedBasis" => G,
        "computedInitialIdeal" => computedInitial,
        "computedBasisIsQuadratic" => computedQuadratic,
        "computedInitialIdealEqualsBadIdeal" => initialMatches,
        "badInitialIdealEqualsComputed" => initialMatches,
        "suppliedRulesAreGroebner" => suppliedGroebner,
        "suppliedRulesAreQuadraticGroebner" => (allQuadratic and suppliedGroebner),
        "suppliedRelationsAreGroebnerBasis" => suppliedGroebner,
        "suppliedRelationsAreQuadraticGroebnerBasis" => (allQuadratic and suppliedGroebner)
    };
    I.cache#"groebnerCertificate" = certificate;
    certificate
);

hasQuadraticGroebnerBasis = method(TypicalValue => Boolean);
hasQuadraticGroebnerBasis Ideal := I -> (hpGroebnerCertificate I)#"passed";
hasQuadraticGroebnerBasis PolynomialRing := R -> hasQuadraticGroebnerBasis straighteningIdeal R;
hasQuadraticGroebnerBasis QuotientRing := A -> (
    hpHookContext("hasQuadraticGroebnerBasis", A);
    hasQuadraticGroebnerBasis ideal A
);

------------------------------------------------------------------------
-- Fine Segre products of hook rings
--
-- A type pair record has the form {baseType, leftType, rightType}.  The
-- base type is the fine multidegree; the two type assignments must be
-- injective order embeddings with the same orientation, which is
-- precisely the hypothesis under which the rank two straightening rule
-- is well defined in both factors simultaneously.  The fine Segre ring
-- of two hook rings R1, R2 has one variable w_i for each pair (T, U) of
-- a left tableau T of type leftType and a right tableau U of type
-- rightType, one type pair at a time; its context, stored at
-- W.cache#"hookContext" with kind "FineSegreRetract", records the two
-- factor contexts, the type pairs and the pairing of the generators.
--   fineSegreEstimate(R1, R2, typePairs)   the generator counts, a Tally
--   fineSegreRing(R1, R2, TypePairs => ...) the ring W
--   fineSegreRing(n, m, r)                 the fine Segre model of the
--                                          generic r minors of an n by m
--                                          matrix
--   fineSegreAlgebra                       W / straighteningIdeal W
--   fineSegreMap(W, A1, A2)                the map W -> A1 ** A2
--   factors W                              the pair (R1, R2)
-- The straightening presentation of W, its nonstandard monomials and the
-- classification of its pairs of generators are computed here and
-- reached through straighteningIdeal, nonstandardMonomialIdeal,
-- relationKind and hookRelation.
------------------------------------------------------------------------

hpFineComparisonSign = (h,g) -> (
    if hpFHRTypeOuter(h,g) then return 1;
    if hpFHRTypeOuter(g,h) then return -1;
    0
);

hpFineValidAbstractType = h -> (
    instance(h,List) and #h == 2 and
    instance(h#0,ZZ) and instance(h#1,ZZ) and
    h#0 >= 0 and h#1 >= 0
);

hpFineValidateTypePairs = (caller,leftContext,rightContext,typePairs) -> (
    hpRequireHookKind(caller, leftContext);
    hpRequireHookKind(caller, rightContext);
    if leftContext#"coefficientRing" =!= rightContext#"coefficientRing" then
        error(caller | ": the two hook rings must have the same coefficient ring");
    if not instance(typePairs,List) or #typePairs == 0 then
        error(caller | ": TypePairs must be a nonempty list");
    if any(typePairs,pair -> not instance(pair,List) or #pair != 3) then
        error(caller | ": each TypePairs record must be {baseType,leftType,rightType}");
    if any(typePairs,pair -> not hpFineValidAbstractType(pair#0) or
        not hpFineValidAbstractType(pair#1) or
        not hpFineValidAbstractType(pair#2)) then
        error(caller | ": every type must be a pair {a,b} of nonnegative integers");
    if #unique apply(typePairs,pair -> pair#0) != #typePairs then
        error(caller | ": base fine degrees must be distinct");
    for pair in typePairs do (
        if not (leftContext#"basisByType")#?(pair#1) then
            error(caller | ": the left hook ring has no hook type " | toString(pair#1));
        if not (rightContext#"basisByType")#?(pair#2) then
            error(caller | ": the right hook ring has no hook type " | toString(pair#2));
    );
    leftTypes := apply(typePairs,pair -> pair#1);
    rightTypes := apply(typePairs,pair -> pair#2);
    if #unique leftTypes != #typePairs then
        error(caller | ": the left type assignment must be injective");
    if #unique rightTypes != #typePairs then
        error(caller | ": the right type assignment must be injective");
    if #typePairs >= 2 then for i from 0 to #typePairs-2 do
        for j from i+1 to #typePairs-1 do (
            baseSign := hpFineComparisonSign(typePairs#i#0,typePairs#j#0);
            leftSign := hpFineComparisonSign(typePairs#i#1,typePairs#j#1);
            rightSign := hpFineComparisonSign(typePairs#i#2,typePairs#j#2);
            if leftSign != baseSign or rightSign != baseSign then
                error(caller | ": the two type assignments must be " |
                    "order embeddings with the same orientation; failure at " |
                    toString(typePairs#i#0) | " and " | toString(typePairs#j#0));
        );
    true
);

hpFineOrderedTypePairs = typePairs -> (
    baseTypes := hpFHRTypeOrder apply(typePairs,pair -> pair#0);
    pairByBase := new HashTable from apply(typePairs,pair -> pair#0 => pair);
    apply(baseTypes,h -> pairByBase#h)
);

-- the number of fine generators of each ordered type pair
hpFineCounts = (leftContext,rightContext,orderedPairs) -> apply(orderedPairs, pair ->
    #((leftContext#"basisByType")#(pair#1)) * #((rightContext#"basisByType")#(pair#2)));

fineSegreEstimate = method(TypicalValue => Tally);
fineSegreEstimate(PolynomialRing,PolynomialRing,List) := (R1,R2,typePairs) -> (
    leftContext := hpHookContext("fineSegreEstimate", R1);
    rightContext := hpHookContext("fineSegreEstimate", R2);
    hpFineValidateTypePairs("fineSegreEstimate",leftContext,rightContext,typePairs);
    orderedPairs := hpFineOrderedTypePairs typePairs;
    counts := hpFineCounts(leftContext,rightContext,orderedPairs);
    new Tally from apply(#orderedPairs, p -> orderedPairs#p => counts#p)
);

-- The paper order on a fine Segre product is a block order: common fine
-- degree first, then the complete paper order from the left factor, then
-- the complete paper order from the right factor.  A final Lex block is an
-- arbitrary total refinement only after all mathematically relevant rows.
-- The rows come with their labels, as pairs {label, weights}.
hpFinePaperOrderLabeledRows = (leftContext,rightContext,typePairs,metadata) -> (
    baseTypes := apply(typePairs,pair -> pair#0);
    rows := {{"boxes", apply(metadata,record ->
        record#"baseType"#0 + record#"baseType"#1 + 1)}};
    for h in baseTypes do rows = rows | {{"fine degree " | toString h,
        apply(metadata,record -> if record#"baseType" == h then 1 else 0)}};
    for k from 0 to #(leftContext#"paperOrderRows")-1 do (
        factorRow := leftContext#"paperOrderRows"#k;
        rows = rows | {{"left " | leftContext#"paperOrderLabels"#k,
            apply(metadata,record -> factorRow#(record#"leftIndex"))}});
    for k from 0 to #(rightContext#"paperOrderRows")-1 do (
        factorRow := rightContext#"paperOrderRows"#k;
        rows = rows | {{"right " | rightContext#"paperOrderLabels"#k,
            apply(metadata,record -> factorRow#(record#"rightIndex"))}});
    rows
);

-- the fine Segre ring of two hook contexts, with its context stored at
-- W.cache#"hookContext"
hpFineSegreBuild = (caller,leftContext,rightContext,typePairs,order,variableLimit,force) -> (
    if not instance(variableLimit,ZZ) or variableLimit < 0 then
        error(caller | ": VariableLimit must be a nonnegative integer");
    if not instance(force,Boolean) then
        error(caller | ": Force must be Boolean");
    if typePairs === null then
        error(caller | ": specify TypePairs => {{base,left,right},...}");
    hpFineValidateTypePairs(caller,leftContext,rightContext,typePairs);
    orderedPairs := hpFineOrderedTypePairs typePairs;
    counts := hpFineCounts(leftContext,rightContext,orderedPairs);
    N := sum counts;
    if N > variableLimit and not force then
        error(caller | ": " | toString N |
            " variables exceed VariableLimit=" | toString variableLimit |
            "; rerun with VariableLimit => N or Force => true after inspecting fineSegreEstimate");
    if order == "Paper" and
       (leftContext#"monomialOrder" != "Paper" or
        rightContext#"monomialOrder" != "Paper") then
        error(caller | ": the fine paper order requires factors built with the paper order");
    if order != "Paper" and order != "GRevLex" then
        error(caller | ": MonomialOrder must be \"Paper\" or \"GRevLex\"");

    metadata := {};
    indexByGeneratorMutable := new MutableHashTable;
    index := 0;
    for pair in orderedPairs do (
        baseType := pair#0;
        leftType := pair#1;
        rightType := pair#2;
        for leftHook in (leftContext#"basisByType")#leftType do
            for rightHook in (rightContext#"basisByType")#rightType do (
                leftIndex := (leftContext#"indexByHook")#leftHook;
                rightIndex := (rightContext#"indexByHook")#rightHook;
                record := new HashTable from {
                    "index" => index,
                    "baseType" => baseType,
                    "leftType" => leftType,
                    "rightType" => rightType,
                    "leftHook" => leftHook,
                    "rightHook" => rightHook,
                    "leftContent" => hpFHRHookContent(
                        leftContext#"ambientDimension",leftHook),
                    "rightContent" => hpFHRHookContent(
                        rightContext#"ambientDimension",rightHook),
                    "leftIndex" => leftIndex,
                    "rightIndex" => rightIndex
                };
                metadata = metadata | {record};
                indexByGeneratorMutable#{baseType,leftHook,rightHook} = index;
                index = index+1;
            );
    );

    labeledRows := if order == "Paper" then
        hpFinePaperOrderLabeledRows(leftContext,rightContext,orderedPairs,metadata)
        else {};
    orderRows := apply(labeledRows, last);
    variableSymbols := apply(N,i -> getSymbol("w" | toString i));
    degrees := apply(metadata,record -> apply(#orderedPairs,p ->
        if orderedPairs#p#0 == record#"baseType" then 1 else 0));
    K := leftContext#"coefficientRing";
    W := if order == "Paper" then
        K[variableSymbols,Degrees => degrees,
            DegreeRank => #orderedPairs,
            MonomialOrder => hpFHRMonomialOrderParts orderRows]
        else K[variableSymbols,Degrees => degrees,
            DegreeRank => #orderedPairs,MonomialOrder => GRevLex];

    metadataWithVariables := apply(N,i -> new HashTable from join(
        pairs(metadata#i),{"variable" => W_i}));
    leftWeight := leftContext#"weightBound";
    rightWeight := rightContext#"weightBound";
    truncationBound := if leftWeight === null and rightWeight === null then null
        else (if leftWeight === null then {} else leftWeight) |
             (if rightWeight === null then {} else rightWeight);
    truncationContents := if truncationBound === null then {}
        else apply(metadataWithVariables,record ->
            (if leftWeight === null then {} else record#"leftContent") |
            (if rightWeight === null then {} else record#"rightContent"));
    F := new hpContextData from {
        "kind" => "FineSegreRetract",
        "coefficientRing" => K,
        "leftContext" => leftContext,
        "rightContext" => rightContext,
        "typePairs" => orderedPairs,
        "estimate" => new Tally from apply(#orderedPairs, p -> orderedPairs#p => counts#p),
        "numberOfVariables" => N,
        "ring" => W,
        "variables" => apply(N,i -> W_i),
        "metadataByIndex" => metadataWithVariables,
        "indexByGenerator" => new HashTable from indexByGeneratorMutable,
        "baseTypeByLeftType" => new HashTable from apply(orderedPairs, pair -> pair#1 => pair#0),
        "leftWeightBound" => leftWeight,
        "rightWeightBound" => rightWeight,
        "isWeightRestricted" =>
            (leftWeight =!= null or rightWeight =!= null),
        "truncationBound" => truncationBound,
        "truncationContents" => truncationContents,
        "paperOrderRows" => orderRows,
        "paperOrderLabels" => apply(labeledRows, first),
        "monomialOrder" => order
    };
    W.cache#"hookContext" = F;
    W
);

-- the type pairs of the fine Segre model of the generic r minors of an
-- n by m matrix: the base hook (a,b), 0 <= a <= m-r and 0 <= b <= n-r,
-- is paired with the left type (a, b+r-1) and the right type (b, a+r-1)
hpFineGenericTypePairs = (caller, n, m, r) -> (
    if not instance(n,ZZ) or not instance(m,ZZ) or not instance(r,ZZ) then
        error(caller | ": expected three integers n, m, r");
    if r < 2 or r > min(n,m) then
        error(caller | ": the minor size r must satisfy 2 <= r <= min(n,m)");
    shift := r-1;
    baseTypes := flatten for a from 0 to m-r list for b from 0 to n-r list {a,b};
    apply(baseTypes,h -> {h,{h#0,h#1+shift},{h#1,h#0+shift}})
);

fineSegreRing = method(Options => {
    TypePairs => null,
    LeftWeight => null,
    RightWeight => null,
    CoefficientRing => QQ,
    MonomialOrder => "Paper",
    VariableLimit => 500,
    Force => false
}, TypicalValue => PolynomialRing);

fineSegreRing(PolynomialRing,PolynomialRing) := opts -> (R1,R2) -> (
    leftContext := hpHookContext("fineSegreRing", R1);
    rightContext := hpHookContext("fineSegreRing", R2);
    if opts#LeftWeight =!= null or opts#RightWeight =!= null then
        error("fineSegreRing: LeftWeight and RightWeight belong to the form" |
            " fineSegreRing(n, m, r); the weight bounds of given hook rings are set" |
            " by hookRing(..., Weight => w)");
    if opts#CoefficientRing =!= QQ and opts#CoefficientRing =!= leftContext#"coefficientRing" then
        error("fineSegreRing: the coefficient ring of given hook rings is theirs;" |
            " CoefficientRing applies to the form fineSegreRing(n, m, r)");
    hpFineSegreBuild("fineSegreRing",leftContext,rightContext,opts#TypePairs,
        opts#MonomialOrder,opts#VariableLimit,opts#Force)
);

fineSegreRing(ZZ,ZZ,ZZ) := opts -> (n,m,r) -> (
    if opts#TypePairs =!= null then
        error("fineSegreRing: the form fineSegreRing(n, m, r) chooses its own type pairs;" |
            " give TypePairs with two hook rings instead");
    typePairs := hpFineGenericTypePairs("fineSegreRing", n, m, r);
    leftTypes := unique apply(typePairs,pair -> pair#1);
    rightTypes := unique apply(typePairs,pair -> pair#2);
    R1 := hpHookRingBuild("fineSegreRing", n, leftTypes, opts#CoefficientRing,
        opts#LeftWeight, opts#MonomialOrder, opts#VariableLimit, opts#Force);
    R2 := hpHookRingBuild("fineSegreRing", m, rightTypes, opts#CoefficientRing,
        opts#RightWeight, opts#MonomialOrder, opts#VariableLimit, opts#Force);
    hpFineSegreBuild("fineSegreRing",R1.cache#"hookContext",R2.cache#"hookContext",
        typePairs,opts#MonomialOrder,opts#VariableLimit,opts#Force)
);

fineSegreAlgebra = method(Options => {
    TypePairs => null,
    LeftWeight => null,
    RightWeight => null,
    CoefficientRing => QQ,
    MonomialOrder => "Paper",
    VariableLimit => 500,
    RelationLimit => 10000,
    Force => false,
    Validate => true
}, TypicalValue => QuotientRing);

fineSegreAlgebra(PolynomialRing,PolynomialRing) := opts -> (R1,R2) ->
    hpHookAlgebraOf("fineSegreAlgebra",
        fineSegreRing(R1,R2,TypePairs => opts#TypePairs,LeftWeight => opts#LeftWeight,
            RightWeight => opts#RightWeight,CoefficientRing => opts#CoefficientRing,
            MonomialOrder => opts#MonomialOrder,VariableLimit => opts#VariableLimit,
            Force => opts#Force),
        opts#Validate, opts#RelationLimit, opts#Force);

fineSegreAlgebra(ZZ,ZZ,ZZ) := opts -> (n,m,r) ->
    hpHookAlgebraOf("fineSegreAlgebra",
        fineSegreRing(n,m,r,TypePairs => opts#TypePairs,LeftWeight => opts#LeftWeight,
            RightWeight => opts#RightWeight,CoefficientRing => opts#CoefficientRing,
            MonomialOrder => opts#MonomialOrder,VariableLimit => opts#VariableLimit,
            Force => opts#Force),
        opts#Validate, opts#RelationLimit, opts#Force);

fineSegreAlgebra PolynomialRing := opts -> W -> (
    F := hpHookContext("fineSegreAlgebra", W);
    if not hpIsFineContext F then
        error "fineSegreAlgebra: expected a fine Segre ring (from fineSegreRing); a hook ring is presented by hookAlgebra";
    hpHookAlgebraOf("fineSegreAlgebra", W, opts#Validate, opts#RelationLimit, opts#Force)
);

-- the two hook rings of a fine Segre ring or algebra
factors = method(TypicalValue => Sequence);
factors PolynomialRing := W -> hpFineFactors W;
factors QuotientRing := B -> hpFineFactors B;

hpFineFactors = X -> (
    F := hpHookContext("factors", X);
    if not hpIsFineContext F then
        error "factors: expected a fine Segre ring or algebra (from fineSegreRing or fineSegreAlgebra)";
    (F#"leftContext"#"ring", F#"rightContext"#"ring")
);

-- The map of a fine Segre ring into the tensor product of two quotients
-- of its factors: each fine variable goes to the product of the two
-- factor generators it pairs.  Its kernel, for the Pieri quotients of the
-- factors, is the naive Pieri presentation of the fine Segre product.
fineSegreMap = method(TypicalValue => RingMap);
fineSegreMap(PolynomialRing,Ring,Ring) := (W,leftQuotient,rightQuotient) -> (
    F := hpHookContext("fineSegreMap", W);
    if not hpIsFineContext F then
        error "fineSegreMap: expected a fine Segre ring (from fineSegreRing)";
    leftRing := F#"leftContext"#"ring";
    rightRing := F#"rightContext"#"ring";
    if hpHookRingOf("fineSegreMap", leftQuotient) =!= leftRing then
        error "fineSegreMap: the second argument must be the left factor or a quotient of it";
    if hpHookRingOf("fineSegreMap", rightQuotient) =!= rightRing then
        error "fineSegreMap: the third argument must be the right factor or a quotient of it";
    leftExpected := F#"leftContext"#"numberOfVariables";
    rightExpected := F#"rightContext"#"numberOfVariables";
    if numgens leftQuotient != leftExpected then
        error "fineSegreMap: the left quotient has the wrong number of generators";
    if numgens rightQuotient != rightExpected then
        error "fineSegreMap: the right quotient has the wrong number of generators";
    tensorProduct := leftQuotient ** rightQuotient;
    -- with no variables on either side the tensor product is the coefficient
    -- field with degree rank two, on which Core's kernel computation fails;
    -- the same ring with degree rank one is used instead
    if numgens tensorProduct == 0 then tensorProduct = (coefficientRing tensorProduct)[Variables => 0];
    tensorVariables := gens tensorProduct;
    images := apply(F#"metadataByIndex",record ->
        tensorVariables#(record#"leftIndex")*
        tensorVariables#(leftExpected+record#"rightIndex"));
    map(tensorProduct,W,images)
);

------------------------------------------------------------------------
-- The classification of the pairs of fine generators
------------------------------------------------------------------------

hpFineHookPairCompatible = (context,outerHook,innerHook) -> (
    if not hpFHRTypeOuter(hpFHRHookType outerHook,hpFHRHookType innerHook) then
        return false;
    hpIsSemistandardRank2Unchecked hpHooksInternalToNestedRows(outerHook,innerHook)
);

-- The status of the product w_i w_j: "zero:weight" when the total content
-- exceeds an inherited factor bound, "zero:base-nonchain" when the base
-- types are not strictly nested, "standard" when both factor pairs are
-- semistandard, and "bad:left", "bad:right" or "bad:both" otherwise; the
-- record also orients the pair by base type.
hpFinePairStatus = (F,firstIndex,secondIndex) -> (
    if not instance(firstIndex,ZZ) or not instance(secondIndex,ZZ) or
       firstIndex < 0 or secondIndex < 0 or
       firstIndex >= F#"numberOfVariables" or
       secondIndex >= F#"numberOfVariables" then
        error "relationKind: generator index out of range";
    firstRecord := F#"metadataByIndex"#firstIndex;
    secondRecord := F#"metadataByIndex"#secondIndex;
    if not hpWeightPairAllowed(F,firstIndex,secondIndex) then
        return new HashTable from {
            "status" => "zero:weight",
            "leftCompatible" => false,
            "rightCompatible" => false,
            "outerIndex" => null,
            "innerIndex" => null
        };
    firstBase := firstRecord#"baseType";
    secondBase := secondRecord#"baseType";
    if not hpFHRTypeOuter(firstBase,secondBase) and
       not hpFHRTypeOuter(secondBase,firstBase) then
        return new HashTable from {
            "status" => "zero:base-nonchain",
            "leftCompatible" => false,
            "rightCompatible" => false,
            "outerIndex" => null,
            "innerIndex" => null
        };
    outerRecord := if hpFHRTypeOuter(firstBase,secondBase) then
        firstRecord else secondRecord;
    innerRecord := if hpFHRTypeOuter(firstBase,secondBase) then
        secondRecord else firstRecord;
    leftNested := hpFHRTypeOuter(outerRecord#"leftType",innerRecord#"leftType");
    rightNested := hpFHRTypeOuter(outerRecord#"rightType",innerRecord#"rightType");
    if not leftNested or not rightNested then
        error "relationKind: validated type assignments lost their common orientation";
    leftCompatible := hpFineHookPairCompatible(F#"leftContext",
        outerRecord#"leftHook",innerRecord#"leftHook");
    rightCompatible := hpFineHookPairCompatible(F#"rightContext",
        outerRecord#"rightHook",innerRecord#"rightHook");
    status := if leftCompatible and rightCompatible then "standard"
        else if not leftCompatible and not rightCompatible then "bad:both"
        else if not leftCompatible then "bad:left"
        else "bad:right";
    new HashTable from {
        "status" => status,
        "leftCompatible" => leftCompatible,
        "rightCompatible" => rightCompatible,
        "outerIndex" => outerRecord#"index",
        "innerIndex" => innerRecord#"index"
    }
);

-- the predicate on pairs of generator indices of a fine Segre ring
hpFineStandardPair = F -> (i,j) -> (hpFinePairStatus(F,i,j))#"status" == "standard";

-- the nonstandard monomials of a fine Segre ring: the products of the
-- pairs that are not standard and the minimal overweight standard chains
hpFineNonstandardMonomials = F -> (
    W := F#"ring";
    N := F#"numberOfVariables";
    bad := flatten for i from 0 to N-1 list for j from i to N-1 list (
        if (hpFinePairStatus(F,i,j))#"status" != "standard" then W_i*W_j else continue);
    bad | apply(hpWeightStandardBoundaryRecords(F,hpFineStandardPair F,1000000,true),
        record -> record#"monomial")
);

------------------------------------------------------------------------
-- The straightening presentation of a fine Segre ring
------------------------------------------------------------------------

-- A fine product is standard only when its base types form a strict
-- chain and the induced pair of hook tableaux is semistandard in both
-- factors.  Every other quadratic product is either zero or is replaced
-- by the tensor product of the two rank two normal forms.
hpFineStraighteningPresentationBody = (opts, F) -> (
    W := F#"ring";
    variables := F#"variables";
    metadata := F#"metadataByIndex";
    indexByGenerator := F#"indexByGenerator";
    leftMemo := new MutableHashTable;
    rightMemo := new MutableHashTable;
    relations := {};
    badMonomials := {};
    records := {};
    weightRecords := {};
    N := F#"numberOfVariables";
    standardPair := hpFineStandardPair F;

    if N > 0 then for i from 0 to N-1 do for j from i to N-1 do (
        classification := hpFinePairStatus(F,i,j);
        status := classification#"status";
        if status != "standard" then (
            target := variables#i*variables#j;
            badMonomials = badMonomials | {target};
            relation := target;
            numberOfTerms := 0;
            if substring(0,4,status) != "zero" then (
                outerRecord := metadata#(classification#"outerIndex");
                innerRecord := metadata#(classification#"innerIndex");
                leftNormalForm := hpFHRPairNormalFormAt(F#"leftContext",
                    {outerRecord#"leftHook",innerRecord#"leftHook"},leftMemo,i,j);
                rightNormalForm := hpFHRPairNormalFormAt(F#"rightContext",
                    {outerRecord#"rightHook",innerRecord#"rightHook"},rightMemo,i,j);
                for leftPair in keys leftNormalForm do
                    for rightPair in keys rightNormalForm do (
                        outerKey := {outerRecord#"baseType",leftPair#0,rightPair#0};
                        innerKey := {innerRecord#"baseType",leftPair#1,rightPair#1};
                        if not indexByGenerator#?outerKey or
                           not indexByGenerator#?innerKey then
                            hpFail("straighteningIdeal: the rank two straightening left the fine generators");
                        coefficient := leftNormalForm#leftPair*rightNormalForm#rightPair;
                        relation = relation-coefficient*
                            variables#(indexByGenerator#outerKey)*
                            variables#(indexByGenerator#innerKey);
                        numberOfTerms = numberOfTerms+1;
                    );
            );
            if relation == 0 then
                hpFail("straighteningIdeal: a proposed fine Segre relation vanished");
            relations = relations | {relation};
            records = records | {new HashTable from {
                "kind" => if status == "zero:weight" then
                    "weight-truncation" else "quadratic",
                "indices" => {i,j},
                "status" => status,
                "target" => target,
                "relation" => relation,
                "numberOfTermsInNormalForm" => numberOfTerms
            }};
            if status == "zero:weight" then
                weightRecords = weightRecords|{last records};
            hpRelationLimitCheck("straighteningIdeal", #relations, "relations", opts#RelationLimit, opts#Force);
        );
    );

    higherWeightRecords := hpWeightStandardBoundaryRecords(F,standardPair,
        opts#RelationLimit,opts#Force);
    for record in higherWeightRecords do (
        monomial := record#"monomial";
        relations = relations|{monomial};
        badMonomials = badMonomials|{monomial};
        relationRecord := new HashTable from join(pairs record,{
            "kind" => "weight-truncation",
            "status" => "zero:weight-boundary",
            "target" => monomial,
            "relation" => monomial,
            "numberOfTermsInNormalForm" => 0
        });
        records = records|{relationRecord};
        weightRecords = weightRecords|{relationRecord};
        hpRelationLimitCheck("straighteningIdeal", #relations, "relations", opts#RelationLimit, opts#Force);
    );

    I := hpFHRIdealFromList(W,relations);
    Jbad := hpFHRIdealFromList(W,badMonomials);
    allQuadratic := all(relations,f -> sum degree f == 2);
    leadingChecks := apply(#relations,k ->
        leadMonomial(relations#k) == badMonomials#k);
    allLeading := all(leadingChecks,value -> value);
    if opts#Validate and F#"monomialOrder" == "Paper" and not allLeading then
        hpFail("straighteningIdeal: the compiled block order failed a leading term check");
    new hpPresentationData from {
        "kind" => "FineSegreStraighteningPresentation",
        "context" => F,
        "ring" => W,
        "relations" => relations,
        "relationRecords" => records,
        "numberOfRelations" => #relations,
        "numberOfWeightRelations" => #weightRecords,
        "weightRelationRecords" => sort(weightRecords,
            r -> {if r#?"degree" then r#"degree" else 2,
                  toString(if r#?"monomial" then r#"monomial" else r#"target")}),
        "numberOfNonzeroRewrites" =>
            #select(records,record -> record#"numberOfTermsInNormalForm" > 0),
        "badMonomials" => badMonomials,
        "badMonomialIdeal" => Jbad,
        "ideal" => I,
        "quotient" => W/I,
        "allRelationsQuadratic" => allQuadratic,
        "leadingTermChecks" => leadingChecks,
        "allLeadingTermsCorrect" => allLeading,
        "leftNormalFormCacheSize" => #(keys leftMemo),
        "rightNormalFormCacheSize" => #(keys rightMemo)
    }
);

------------------------------------------------------------------------
-- Hook algebras and the accessors of hook rings
--
-- The finite hook algebra of a set of hook types is the quotient of the
-- hook ring by its straightening ideal, built with Core's own quotient
-- construction: ideal A, ambient A, hilbertSeries A, dim A and every
-- ring operation are Core's, and the context of the ring is read from
-- ambient A.
--   hookAlgebra(n, types), hookAlgebra(n, MaxArm => A, MaxLeg => B),
--   hookAlgebra R                the algebra R / straighteningIdeal R
--   hookTypes, alphabetSize, weightBound
--                                the choices the ring was built from
--   hookBasis X, hookBasis(X, type)
--                                the generators as hook tabloids, in
--                                the order of the variables
--   hookVariable(X, T), hookMonomial(X, T)
--                                from tabloids to ring elements
--   tabloid z, tabloidTerms f        from ring elements to tabloids
--   hookStraighten(T, A), hookStraighten f
--                                normal forms as elements of the algebra
--   hookRelation(X, i, j), relationKind(X, i, j)
--                                the stored relation of a pair of
--                                generators and its kind
-- Every accessor accepts the hook ring, the hook algebra, a fine Segre
-- ring or algebra, or a quotient of one of them, and dispatches on the
-- context record stored at R.cache#"hookContext" of the polynomial ring
-- underneath.
------------------------------------------------------------------------

-- the quotient of a hook ring or fine Segre ring by its straightening
-- ideal; Core memoizes the quotient on the ideal, so repeated calls
-- return the same ring, and the context is copied to A.cache for
-- convenience (the accessors read it from the ambient ring)
hpHookAlgebraOf = (caller, R, validate, relationLimit, force) -> (
    H := hpHookContext(caller, R);
    if not instance(R, PolynomialRing) then
        error(caller | ": expected the polynomial ring of a hook ring or fine Segre ring");
    I := straighteningIdeal(R, Validate => validate, RelationLimit => relationLimit, Force => force);
    A := R / I;
    A.cache#"hookContext" = H;
    A
);

hookAlgebra = method(Options => {
    CoefficientRing => QQ,
    MaxArm => null,
    MaxLeg => null,
    Weight => null,
    MonomialOrder => "Paper",
    VariableLimit => 500,
    RelationLimit => 10000,
    Force => false,
    Validate => true
}, TypicalValue => QuotientRing);

hookAlgebra(ZZ, List) := opts -> (n, types) -> (
    if opts#MaxArm =!= null or opts#MaxLeg =!= null then
        error "hookAlgebra: a list of hook types cannot be combined with MaxArm or MaxLeg";
    hpHookAlgebraOf("hookAlgebra",
        hpHookRingBuild("hookAlgebra", n, types, opts#CoefficientRing, opts#Weight,
            opts#MonomialOrder, opts#VariableLimit, opts#Force),
        opts#Validate, opts#RelationLimit, opts#Force)
);

hookAlgebra ZZ := opts -> n -> (
    if not instance(n, ZZ) or n < 1 then error "hookAlgebra: the alphabet size n must be a positive integer";
    hpHookAlgebraOf("hookAlgebra",
        hpHookRingBuild("hookAlgebra", n, hpHookRectangleTypes("hookAlgebra", n, opts#MaxArm, opts#MaxLeg),
            opts#CoefficientRing, opts#Weight, opts#MonomialOrder, opts#VariableLimit, opts#Force),
        opts#Validate, opts#RelationLimit, opts#Force)
);

hookAlgebra PolynomialRing := opts -> R ->
    hpHookAlgebraOf("hookAlgebra", R, opts#Validate, opts#RelationLimit, opts#Force);

-- the algebra of X: X itself when it is a quotient ring, else the
-- quotient of the hook ring or fine Segre ring X by its straightening
-- ideal (with the default guards; build it with hookAlgebra first to
-- pass RelationLimit or Force)
hpAlgebraOf = (caller, X) -> (
    if instance(X, QuotientRing) then (hpHookContext(caller, X); X)
    else hpHookAlgebraOf(caller, hpHookRingOf(caller, X), true, 10000, false)
);

------------------------------------------------------------------------
-- The choices a ring was built from
------------------------------------------------------------------------

hookTypes = method(TypicalValue => List);
hookTypes PolynomialRing := R -> hpHookTypes R;
hookTypes QuotientRing := A -> hpHookTypes A;
hpHookTypes = X -> (
    H := hpHookContext("hookTypes", X);
    if hpIsFineContext H then H#"typePairs" else H#"hookTypes"
);

alphabetSize = method(TypicalValue => ZZ);
alphabetSize PolynomialRing := R -> hpAlphabetSize R;
alphabetSize QuotientRing := A -> hpAlphabetSize A;
hpAlphabetSize = X -> (
    H := hpHookContext("alphabetSize", X);
    if hpIsFineContext H then
        error "alphabetSize: a fine Segre ring has two alphabets; apply alphabetSize to the factors (see factors)";
    H#"ambientDimension"
);

weightBound = method(TypicalValue => List);
weightBound PolynomialRing := R -> hpWeightBound R;
weightBound QuotientRing := A -> hpWeightBound A;
hpWeightBound = X -> (
    H := hpHookContext("weightBound", X);
    if hpIsFineContext H then {H#"leftWeightBound", H#"rightWeightBound"} else H#"weightBound"
);

------------------------------------------------------------------------
-- The generators as tabloids
------------------------------------------------------------------------

-- the hook shaped tabloid of a native pair {rowTail, colFull}
hpNativeTabloid = hook -> tabloid(hook#0, hook#1);

-- the index of the generator of a hook ring carrying a native hook, or an
-- error naming the hook as a tabloid
hpHookGeneratorIndexNative = (caller, H, hook) -> (
    if not H#"indexByHook"#?hook then
        error(caller | ": " | toString hpNativeTabloid hook |
            " is not a generator of this hook ring: a generator is a semistandard hook" |
            " tableau on the letters 1.." | toString H#"ambientDimension" |
            " of one of the types " | toString H#"hookTypes" |
            (if H#"weightBound" === null then ""
                else " with content at most " | toString H#"weightBound"));
    H#"indexByHook"#hook
);

-- the index of the generator carrying a hook shaped tabloid T (given as a
-- HookTabloid, a list of rows or a Filling)
hpHookGeneratorIndex = (caller, H, T) -> (
    if hpIsFineContext H then
        error(caller | ": on a fine Segre ring, expected a pair {T, U} of hook shaped tabloids");
    hpHookGeneratorIndexNative(caller, H, hpHookToNative(caller, tabloid T))
);

-- the index of the generator of a fine Segre ring carrying the pair
-- {T, U} of hook shaped tabloids
hpFineGeneratorIndex = (caller, F, P) -> (
    if not instance(P, List) or #P != 2 then
        error(caller | ": on a fine Segre ring, expected a pair {T, U} of hook shaped tabloids");
    leftHook := hpHookToNative(caller, tabloid P#0);
    rightHook := hpHookToNative(caller, tabloid P#1);
    leftType := hpFHRHookType leftHook;
    if not F#"baseTypeByLeftType"#?leftType then
        error(caller | ": no fine degree pairs a left hook of type " | toString leftType |
            "; the type pairs are " | toString F#"typePairs");
    key := {F#"baseTypeByLeftType"#leftType, leftHook, rightHook};
    if not F#"indexByGenerator"#?key then
        error(caller | ": the pair " | toString apply(P, tabloid) |
            " is not a generator of this fine Segre ring");
    F#"indexByGenerator"#key
);

-- hookBasis(List type, ZZ n) is the enumerator of the Pieri section;
-- this section installs the ring forms on the same method
hookBasis PolynomialRing := R -> hpHookBasis(R, null);
hookBasis QuotientRing := A -> hpHookBasis(A, null);
hookBasis(PolynomialRing, List) := (R, h) -> hpHookBasis(R, h);
hookBasis(QuotientRing, List) := (A, h) -> hpHookBasis(A, h);
hpHookBasis = (X, h) -> (
    H := hpHookContext("hookBasis", X);
    if hpIsFineContext H then (
        records := H#"metadataByIndex";
        if h =!= null then (
            if not any(H#"typePairs", pair -> pair#0 == h) then
                error("hookBasis: " | toString h | " is not a fine degree of this fine Segre ring;" |
                    " the type pairs are " | toString H#"typePairs");
            records = select(records, r -> r#"baseType" == h));
        apply(records, r -> {hpNativeTabloid r#"leftHook", hpNativeTabloid r#"rightHook"}))
    else (
        hooks := if h === null then H#"basis" else (
            if not H#"basisByType"#?h then
                error("hookBasis: " | toString h | " is not a hook type of this hook ring;" |
                    " the types are " | toString H#"hookTypes");
            H#"basisByType"#h);
        apply(hooks, hpNativeTabloid))
);

hookVariable = method(TypicalValue => RingElement);
hookVariable(PolynomialRing, HookTabloid) := (R, T) -> hpHookVariable(R, T);
hookVariable(QuotientRing, HookTabloid) := (A, T) -> hpHookVariable(A, T);
hookVariable(PolynomialRing, List) := (R, L) -> hpHookVariable(R, L);
hookVariable(QuotientRing, List) := (A, L) -> hpHookVariable(A, L);
hookVariable(PolynomialRing, Filling) := (R, F) -> hpHookVariable(R, F);
hookVariable(QuotientRing, Filling) := (A, F) -> hpHookVariable(A, F);
hpHookVariable = (X, T) -> (
    H := hpHookContext("hookVariable", X);
    X_(if hpIsFineContext H then hpFineGeneratorIndex("hookVariable", H, T)
        else hpHookGeneratorIndex("hookVariable", H, T))
);

hookMonomial = method(TypicalValue => RingElement);
hookMonomial(PolynomialRing, HookTabloid) := (R, T) -> hpHookMonomial(R, T);
hookMonomial(QuotientRing, HookTabloid) := (A, T) -> hpHookMonomial(A, T);
hookMonomial(PolynomialRing, List) := (R, L) -> hpHookMonomial(R, L);
hookMonomial(QuotientRing, List) := (A, L) -> hpHookMonomial(A, L);
hookMonomial(PolynomialRing, Filling) := (R, F) -> hpHookMonomial(R, F);
hookMonomial(QuotientRing, Filling) := (A, F) -> hpHookMonomial(A, F);
hpHookMonomial = (X, T) -> (
    H := hpHookContext("hookMonomial", X);
    if hpIsFineContext H then (
        if not instance(T, List) or #T != 2 then
            error "hookMonomial: on a fine Segre ring, expected a pair {T, U} of hook tabloids of the same Frobenius rank";
        hooksL := hpDiagonalHooks toList tabloid T#0;
        hooksR := hpDiagonalHooks toList tabloid T#1;
        if #hooksL != #hooksR then
            error "hookMonomial: the two tabloids of a fine Segre pair must have the same Frobenius rank";
        pairProduct := 1_X;
        for k from 0 to #hooksL-1 do pairProduct = pairProduct * X_(hpFineGeneratorIndex("hookMonomial", H,
            {hpNativeTabloid hooksL#k, hpNativeTabloid hooksR#k}));
        pairProduct)
    else (
        hooks := hpDiagonalHooks toList tabloid T;
        hookProduct := 1_X;
        for hook in hooks do hookProduct = hookProduct * X_(hpHookGeneratorIndexNative("hookMonomial", H, hook));
        hookProduct)
);

------------------------------------------------------------------------
-- From ring elements to tabloids
------------------------------------------------------------------------

hpRequireTypeChain = (caller, types, noun) ->
    for k from 1 to #types-1 do
        if types#(k-1)#0 <= types#k#0 or types#(k-1)#1 <= types#k#1 then
            error(caller | ": the " | noun | " " | toString types |
                " do not form a chain, so the monomial is not a single tabloid" |
                " (a nonchain product is zero in the algebra; relationKind names it)");

-- the tabloid whose diagonal hooks are the given native hooks, which must
-- be listed with strictly decreasing types
hpAssembleTabloid = hooks -> (
    types := apply(hooks, hpFHRHookType);
    tabloid hpAssembleHookTabloid(nhpFrobToPart(apply(types, h -> h#0), apply(types, h -> h#1)), hooks)
);

-- the tabloid of a monomial of a hook ring whose hooks form a chain of
-- types, or the pair {left, right} of tabloids of a monomial of a fine
-- Segre ring whose base types form a chain
hpMonomialTabloid = (caller, H, m) -> (
    e := first exponents m;
    positions := flatten apply(#e, i -> toList(e#i : i));
    if #positions == 0 then error(caller | ": the constant monomial has no tabloid");
    if hpIsFineContext H then (
        records := sort(apply(positions, i -> H#"metadataByIndex"#i), r -> -(r#"baseType"));
        hpRequireTypeChain(caller, apply(records, r -> r#"baseType"), "base types");
        {hpAssembleTabloid apply(records, r -> r#"leftHook"),
         hpAssembleTabloid apply(records, r -> r#"rightHook")})
    else (
        hooks := sort(apply(positions, i -> H#"basis"#i), hook -> -(hpFHRHookType hook));
        hpRequireTypeChain(caller, apply(hooks, hpFHRHookType), "hook types");
        hpAssembleTabloid hooks)
);

-- the tabloid of a generator, or of a monomial in the generators whose
-- hook types form a chain; a variable of a quotient is lifted through
-- the ambient rings until the hook ring is found
tabloid RingElement := z -> (
    (R, f) := hpLiftToHookRing("tabloid", z);
    H := R.cache#"hookContext";
    fromQuotient := ring z =!= R;
    if f == 0 or #terms f != 1 or leadCoefficient f != 1 then
        error("tabloid: expected a single generator or a monomial in the generators" |
            " (a sum has no single tabloid; use tabloids)" |
            (if fromQuotient then "; note that in the quotient ring a product is" |
                " already reduced to its normal form, so take the monomial in the hook ring" else ""));
    hpMonomialTabloid("tabloid", H, leadMonomial f)
);
tabloid IndexedVariable := v -> error("tabloid: " | toString v |
    " is an indexed variable, not a generator of a hook ring; the generators are" |
    " named z0, z1, ... (after use R write z5, or use R_5)");
-- the method returns a HookTabloid on every other input, but on a
-- generator of a fine Segre ring it returns the pair {T, U}, a list of
-- two of them; the declared value of this one method is therefore Thing,
-- so that the documentation does not promise a HookTabloid
typicalValues#(tabloid, RingElement) = Thing;

-- a polynomial in the generators, read as tabloids: the hash table from
-- the tabloid of each monomial to its coefficient
tabloidTerms = method(TypicalValue => HashTable);
tabloidTerms RingElement := f -> (
    (R, g) := hpLiftToHookRing("tabloidTerms", f);
    H := R.cache#"hookContext";
    hashTable apply(terms g, t -> hpMonomialTabloid("tabloidTerms", H, leadMonomial t) => leadCoefficient t)
);

------------------------------------------------------------------------
-- Normal forms in the algebra
------------------------------------------------------------------------

-- the normal form of a tabloid as an element of the hook algebra: the
-- straightening engine straightens T, and the result is read into the
-- algebra through hookMonomial (a tabloid whose content exceeds the
-- weight bound is zero in the algebra)
hookStraighten(Thing, Ring) := opts -> (T, X) -> (
    A := hpAlgebraOf("hookStraighten", X);
    R := hpHookRingOf("hookStraighten", A);
    H := R.cache#"hookContext";
    if hpIsFineContext H then
        error("hookStraighten: on a fine Segre algebra, reduce the monomial of the pair:" |
            " hookStraighten hookMonomial(W, {T, U}) with W the fine Segre ring");
    T = tabloid T;
    hooks := hpDiagonalHooks toList T;
    for hook in hooks do (
        h := hpFHRHookType hook;
        if not member(h, H#"hookTypes") then
            error("hookStraighten: the hook " | toString hpNativeTabloid hook | " of the tabloid has type " |
                toString h | ", which is not a hook type of the algebra (the types are " |
                toString H#"hookTypes" | ")"));
    if H#"weightBound" =!= null then (
        content := apply(H#"ambientDimension", q -> #select(flatten toList T, x -> x == q+1));
        if not hpWeightLeq(content, H#"weightBound") then return 0_A);
    progress := if opts#?Progress then opts#Progress else 0;
    NF := first hpWithProgress("hookStraighten", progress, () ->
        hpWithField(H#"coefficientRing", () -> hpHookStraightenFromAnyHooks("hookStraighten", toList T)));
    answer := 0_A;
    for U in keys NF do answer = answer + (NF#U) * hpHookMonomial(A, tabloid U);
    answer
);

-- the normal form of an element of a hook ring or fine Segre ring, as
-- an element of its algebra (Core reduces modulo the Groebner basis of
-- the straightening ideal); an element of a quotient is already reduced
hookStraighten RingElement := opts -> f -> (
    R := ring f;
    hpHookContext("hookStraighten", R);
    if instance(R, QuotientRing) then f
    else promote(f, hpHookAlgebraOf("hookStraighten", R, true, 10000, false))
);

------------------------------------------------------------------------
-- The stored relations of pairs of generators
------------------------------------------------------------------------

-- the relation record of the pair {i,j} of generators in the
-- straightening presentation of the ring, or null for a standard pair;
-- the records are indexed by pair once and cached on the ideal
hpPairRecord = (caller, X, i, j) -> (
    R := hpHookRingOf(caller, X);
    H := R.cache#"hookContext";
    N := H#"numberOfVariables";
    if not instance(i,ZZ) or not instance(j,ZZ) or i < 0 or j < 0 or i >= N or j >= N then
        error(caller | ": generator index out of range (the ring has " | toString N |
            " generators, indexed from 0)");
    if i > j then (tmp := i; i = j; j = tmp;);
    I := straighteningIdeal R;
    if not I.cache#?"pairRecords" then I.cache#"pairRecords" = hashTable(
        for r in (I.cache#"presentation")#"relationRecords" list
            if r#?"indices" and #(r#"indices") == 2 then r#"indices" => r else continue);
    records := I.cache#"pairRecords";
    if records#?{i,j} then records#{i,j} else null
);

hookRelation = method(TypicalValue => RingElement);
hookRelation(PolynomialRing, ZZ, ZZ) := (R, i, j) -> hpHookRelation(R, i, j);
hookRelation(QuotientRing, ZZ, ZZ) := (A, i, j) -> hpHookRelation(A, i, j);
hpHookRelation = (X, i, j) -> (
    R := hpHookRingOf("hookRelation", X);
    record := hpPairRecord("hookRelation", X, i, j);
    if record === null then 0_R else record#"relation"
);

relationKind = method(TypicalValue => String);
relationKind(PolynomialRing, ZZ, ZZ) := (R, i, j) -> hpRelationKind(R, i, j);
relationKind(QuotientRing, ZZ, ZZ) := (A, i, j) -> hpRelationKind(A, i, j);
hpRelationKind = (X, i, j) -> (
    H := hpHookContext("relationKind", X);
    record := hpPairRecord("relationKind", X, i, j);
    if record === null then "standard"
    else if hpIsFineContext H then record#"status"
    else record#"kind"
);

needsPackage \ {"Text", "SimpleDoc"};

-- All executable definitions are now installed.  On an ordinary package load
-- with cached documentation, Macaulay2 stops reading here and skips the two
-- auxiliary files: HookPieri/doc.m2 holds every documentation node and
-- HookPieri/tests.m2 every TEST block.
beginDocumentation()

load "./HookPieri/doc.m2"
load "./HookPieri/tests.m2"

end

------------------------------------------------------------------------
-- A scratch region for developing the package; nothing after the line
-- "end" above is read when the package is loaded.
------------------------------------------------------------------------

restart
uninstallPackage "HookPieri"
restart
installPackage "HookPieri"
check "HookPieri"
viewHelp "HookPieri"
