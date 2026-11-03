------------------------------------------------------------------------
-- HookPieri/tests.m2
-- The tests of the package HookPieri, loaded by HookPieri.m2; run them with
-- check "HookPieri".
------------------------------------------------------------------------

TEST ///
-- hpShapeOfHookPair on a few shapes
debug HookPieri
assert( hpShapeOfHookPair({3,3,3}, 0) == {3,3,2} );
assert( hpShapeOfHookPair({3,3,3}, 1) == {2,2} );
assert( hpShapeOfHookPair({4,3,3,1}, 0) == {4,3,2,1} );
assert( hpShapeOfHookPair({4,3,3,1}, 1) == {2,2} );
assert( hpShapeOfHookPair({5,4,3,2,1}, 0) == {5,4,2,2,1} );
///

TEST ///
-- VerificationReport: construction through the private constructor, the
-- stable row order with passed and conclusion last, the net, the short
-- toString, and the validation of the rows
debug needsPackage "HookPieri";
R = hpVerificationReport("Example", {"passed" => true, "conclusion" => "fine",
    "shape" => {3,2}, "tabloids" => 45, "violations" => {}});
assert(instance(R, VerificationReport) and instance(R, HashTable));
assert(R#"kind" == "Example" and R#"passed" and R#"shape" == {3,2} and R#"tabloids" == 45);
assert(R#"keyOrder" == {"shape", "tabloids", "violations", "passed", "conclusion"});
assert(all(keys R, k -> instance(k, String)));
assert(instance(net R, Net));
assert(toString R == "VerificationReport{kind => Example, passed => true}");
-- without a conclusion the last row is passed
R2 = hpVerificationReport("Example", {"count" => 0, "passed" => false});
assert(R2#"keyOrder" == {"count", "passed"} and not R2#"passed");
assert(instance(net R2, Net));
assert(toString R2 == "VerificationReport{kind => Example, passed => false}");
-- compound rows print: short lists side by side, long ones and hash
-- tables as counts, nested reports as tables; the values are unchanged
T = tabloid {{1,2},{3}};
R3 = hpVerificationReport("Example", {"first" => {T, T, "reason"},
    "all" => toList(6:{T, T, "reason"}), "table" => hashTable {T => 1},
    "big" => hashTable apply(6, i -> i => i), "inner" => R2, "missing" => null,
    "passed" => true});
assert(instance(net R3, Net));
assert(R3#"first"#0 == T and #(R3#"all") == 6 and R3#"inner" === R2 and R3#"missing" === null);
assert(R3#"keyOrder" == {"first", "all", "table", "big", "inner", "missing", "passed"});
-- validation of the constructor arguments
assert(try (hpVerificationReport(1, {"passed" => true}); false) else true);
assert(try (hpVerificationReport("E", {"passed" => 1}); false) else true);
assert(try (hpVerificationReport("E", {"a" => 1}); false) else true);
assert(try (hpVerificationReport("E", {passed => true}); false) else true);
assert(try (hpVerificationReport("E", {"passed" => true, "passed" => false}); false) else true);
assert(try (hpVerificationReport("E", {"kind" => "x", "passed" => true}); false) else true);
assert(try (hpVerificationReport("E", {"keyOrder" => {}, "passed" => true}); false) else true);
assert(try (hpVerificationReport("E", {"passed" => true, "conclusion" => 5}); false) else true);
assert(try (hpVerificationReport("E", "passed" => true); false) else true);
-- the type is exported; the retired global field setter is not
exported = apply(HookPieri#"exported symbols", toString);
assert(member("VerificationReport", exported) and not member("hookCoefficientRing", exported));
///

TEST ///
-- the coefficient field: the private setter validates its argument,
-- hpWithField sets the field for one call and restores it (also after an
-- error), the option validator names its caller, hpUnitAt inverts in the
-- current field, and the relabel caches are kept per field
debug needsPackage "HookPieri";
K = ZZ/7;
assert(hpField() === QQ);
assert(hpSetCoefficientRing K === K and hpField() === K);
assert(hpSetCoefficientRing QQ === QQ and hpField() === QQ);
assert(try (hpSetCoefficientRing ZZ; false) else true);
assert(try (hpSetCoefficientRing RR; false) else true);
assert(try (hpSetCoefficientRing(QQ[x]); false) else true);
assert(hpField() === QQ);
assert(hpValidateCoefficientRing("f", K) === K and hpValidateCoefficientRing("f", QQ) === QQ);
assert(try (hpValidateCoefficientRing("f", ZZ); false) else true);
assert(try (hpValidateCoefficientRing("f", 7); false) else true);
-- inside hpWithField the field is K; afterwards it is QQ again
assert(hpWithField(K, () -> hpField()) === K);
assert(hpField() === QQ);
assert(hpWithField(K, () -> hpUnitAt(3, "test")) == 5_K);
assert(hpWithField(QQ, () -> hpUnitAt(3, "test")) == 1/3);
assert(try (hpWithField(K, () -> hpUnitAt(7, "test")); false) else true);
assert(hpField() === QQ);
assert(try (hpWithField(K, () -> error "plain"); false) else true);
assert(hpField() === QQ);
assert(try (hpWithField(ZZ, () -> 1); false) else true);
assert(hpField() === QQ);
-- caches are separate for each field
C = new MutableHashTable;
(hpFieldCache(C, QQ))#"x" = 1;
assert(not (hpFieldCache(C, K))#?"x" and (hpFieldCache(C, QQ))#"x" == 1);
assert(hpFieldCache(C, K) === hpFieldCache(C, K));
///

TEST ///
-- Error tests that name the message rather than merely the exit.  The idiom
-- used elsewhere in this suite, assert(try (EXPR; false) else true), is
-- satisfied by any exception at all, including Macaulay2's own "no method
-- found" raised when the call never reached the package: f QQ[x] parses as
-- (f QQ)[x], so it tests dispatch on QQ and not the package's guard.  capture
-- runs the code in a subinterpreter and hands back its output, so the message
-- itself can be checked.  The code argument must be a double-quoted string,
-- since a triple-slash string would close this block.
errorNames = (code, fragment) -> (
    (errored, out) := capture code;
    assert errored;
    assert(match(fragment, out)));
-- the guard on a ring that is not a hook ring, reached only when the argument
-- is parenthesized
errorNames("hookBasis(QQ[x])", "expected a hook ring");
errorNames("hookTypes(QQ[x])", "expected a hook ring");
errorNames("hookAlgebra(QQ[x])", "expected a hook ring");
errorNames("straighteningIdeal(QQ[x])", "expected a hook ring");
-- the three validation messages that a plain error() inside hpWithField used
-- to replace with the generic "an error that the package did not record"
errorNames("hookStraighten({{1,3,4},{2,2,5},{3}}, Progress => -1)",
    "Progress must be a nonnegative integer");
errorNames("straighteningStep({{1,3,4},{2,2,5},{3}}, Obstruction => \"row\")",
    "no row obstruction");
errorNames("straighteningStep({{1,1,2,3},{3,2,3,4},{4,3}}, Obstruction => \"column\")",
    "no column obstruction");
///

TEST ///
-- tabloid: the five constructors agree with each other, and malformed
-- rows, native pairs and nestings are refused with an error
needsPackage "SchurFunctors";
T = tabloid {{1,2,3},{2,1},{3}};
assert(instance(T, HookTabloid) and instance(T, BasicList));
assert(toList T == {{1,2,3},{2,1},{3}});
assert(tabloid T === T);
assert(tabloid(new Filling from {{1,2,3},{2,1},{3}}) == T);
assert(tabloid(new Filling from {{1,2},{3}}) == tabloid {{1,3},{2}});
assert(tabloid({2,3}, {1,2,3}) == tabloid {{1,2,3},{2},{3}});
assert(tabloid({}, {2}) == tabloid {{2}});
assert(tabloid({4,5}, {1}) == tabloid {{1,4,5}});
assert(tabloid(tabloid {{1,2,3},{2},{3}}, tabloid {{2}}) == tabloid {{1,2,3},{2,2},{3}});
assert(tabloid(tabloid({4,5,6},{1,2,3,7}), tabloid({8},{9,9})) == tabloid {{1,4,5,6},{2,9,8},{3,9},{7}});
assert(tabloid(tabloid {{1,2},{3}}, tabloid {{4}}) == tabloid {{1,2},{3,4}});
assert(toList tabloid {} == {});
-- errors
assert(try (tabloid {{1,2},{3,4,5}}; false) else true);
assert(try (tabloid {{1,2},{}}; false) else true);
assert(try (tabloid {1,2}; false) else true);
assert(try (tabloid({2,3}, {}); false) else true);
assert(try (tabloid({{2,3},{1,2,3}}, {{},{2}}); false) else true);
assert(try (tabloid(tabloid {{1,2,3},{2},{3}}, tabloid {{2,3,4}}); false) else true);
assert(try (tabloid(tabloid {{1,2}}, tabloid {{2}}); false) else true);
assert(try (tabloid(tabloid {{1,2},{3,4}}, tabloid {{2}}); false) else true);
assert(try (tabloid(tabloid {{1,2},{3}}, tabloid {}); false) else true);
///

TEST ///
needsPackage "SchurFunctors";
-- HookTabloid display: the boxes, the re-readable string, the expression
-- and the TeX form
T = tabloid {{1,2,3},{2,1},{3}};
assert(instance(net T, Net) and net T == net new Filling from T);
assert(toString T == "tabloid {{1, 2, 3}, {2, 1}, {3}}");
assert(toExternalString T == toString T);
assert(value toExternalString T == T);
assert(instance(expression T, Expression) and toString expression T == toString T);
assert(tex T == youngTex T);
assert(instance(net tabloid {{10,2},{3}}, Net) and net tabloid {{10,2},{3}} == net new Filling from tabloid {{10,2},{3}});
assert(toString tabloid {} == "tabloid {}" and value toExternalString tabloid {} == tabloid {});
assert(instance(net tabloid {}, Net));
///

TEST ///
-- == and ?: equality is by rows, the order compares shapes first and then
-- rows, sort uses it, and equal tabloids are the same hash key
assert(tabloid {{1,2},{3}} == tabloid {{1,2},{3}});
assert(not (tabloid {{1,2},{3}} == tabloid {{1,3},{2}}));
assert(not (tabloid {{1,2},{3}} == tabloid {{1,2}}));
assert((tabloid {{1,2},{3}} ? tabloid {{1,3},{2}}) === symbol <);
assert((tabloid {{1,3},{2}} ? tabloid {{1,2},{3}}) === symbol >);
assert((tabloid {{1,2},{3}} ? tabloid {{1,2},{3}}) === symbol ==);
assert((tabloid {{1,2,3}} ? tabloid {{1,2},{3}}) === symbol >);
assert((tabloid {{1,2},{3}} ? tabloid {{1,2}}) === symbol >);
assert((tabloid {{9,9},{9}} ? tabloid {{1,1,1}}) === symbol <);
L = {tabloid {{2,1}}, tabloid {{1,2},{3}}, tabloid {{1,1}}, tabloid {{1,2,3}}};
S = sort L;
E = {tabloid {{1,1}}, tabloid {{2,1}}, tabloid {{1,2},{3}}, tabloid {{1,2,3}}};
assert(#S == 4 and all(4, i -> S#i == E#i));
assert(sort S == S);
H = hashTable {tabloid {{1,2},{3}} => 5};
assert(H#?(tabloid {{1,2},{3}}) and H#(tabloid {{1,2},{3}}) == 5);
assert(not H#?(tabloid {{1,3},{2}}));
assert(hash tabloid {{1,2},{3}} == hash tabloid {{1,2},{3}});
///

TEST ///
-- conversions: new Filling from a tabloid stores the columns, tabloid of
-- a Filling recovers the rows, and the empty tabloid converts both ways
needsPackage "SchurFunctors";
T = tabloid {{1,2,3},{4,5},{6}};
F = new Filling from T;
assert(instance(F, Filling) and toList F == {{1,4,6},{2,5},{3}});
assert(tabloid F == T);
assert(toList(new Filling from tabloid {}) == {});
assert(tabloid(new Filling from {}) == tabloid {});
///

TEST ///
-- youngTex: the KaTeX source with white cells by default, the Colors
-- option, and its validation
s = youngTex {{1,2},{3}};
assert(instance(s, String));
assert(s == youngTex tabloid {{1,2},{3}} and s == tex tabloid {{1,2},{3}});
assert(#select("white", s) == 3);
assert(#select("phantom", s) == 6);
c = youngTex({{1,2},{3}}, Colors => {{"red","blue"},{"green"}});
assert(#select("red", c) == 1 and #select("blue", c) == 1 and #select("green", c) == 1 and #select("white", c) == 0);
assert(try (youngTex({{1,2},{3}}, Colors => {{"red"},{"green"}}); false) else true);
assert(try (youngTex({{1,2},{3}}, Colors => {{"red","blue"},{1}}); false) else true);
assert(try (youngTex({{1,2},{3}}, Colors => "red"); false) else true);
assert(try (youngTex({{1,2},{3}}, Colors => {{"red","blue"}}); false) else true);
needsPackage "SchurFunctors";
assert(youngTex(new Filling from {{1,3},{2}}) == youngTex {{1,2},{3}});
assert(youngTex tabloid {} == "\\begin{array}{l}\\end{array}");
///

TEST ///
-- hookDecomposition: the diagonal hooks, top left first, as hook shaped
-- tabloids, from a HookTabloid, a list of rows or a Filling; a rank two
-- tabloid is recovered by tabloid(outer, inner); the native engine
-- hpDiagonalHooks gives the same hooks as pairs {rowTail, colFull}
debug needsPackage "HookPieri";
needsPackage "SchurFunctors";
T = tabloid {{1,3,3},{2,2},{3}};
D = hookDecomposition T;
assert(#D == 2 and all(D, h -> instance(h, HookTabloid)));
assert(D#0 == tabloid {{1,3,3},{2},{3}} and D#1 == tabloid {{2}});
assert(tabloid(D#0, D#1) == T);
assert(hookDecomposition {{1,3,3},{2,2},{3}} == D);
assert(hookDecomposition(new Filling from {{1,2,3},{3,2},{3}}) == D);
D3 = hookDecomposition {{1,2,3},{4,5,6},{7,8,9}};
assert(#D3 == 3 and D3#0 == tabloid {{1,2,3},{4},{7}} and D3#1 == tabloid {{5,6},{8}} and D3#2 == tabloid {{9}});
assert(hookDecomposition tabloid {} == {});
assert(hookDecomposition tabloid {{5}} == {tabloid {{5}}});
assert(#hookDecomposition {{1,2,3,4},{1,2,3},{1,2},{1}} == frobeniusRank {4,3,2,1});
assert(hpDiagonalHooks {{1,3,3},{2,2},{3}} == {{{3,3},{1,2,3}}, {{},{2}}});
assert(hpDiagonalHooks {{1,2,3},{4,5,6},{7,8,9}} == {{{2,3},{1,4,7}}, {{6},{5,8}}, {{},{9}}});
assert(hpDiagonalHooks {} == {});
///

TEST ///
-- frobeniusUnion: the image of a pair of hook tabloids as a hash table of
-- semistandard nest tabloids with rational coefficients, the zero image,
-- the three input forms, the letters 0..n-1, and the validation of the
-- shapes
needsPackage "SchurFunctors";
H = frobeniusUnion(tabloid {{0,1},{1}}, tabloid {{0}}, 2);
assert(instance(H, HashTable) and not instance(H, MutableHashTable));
assert(#H == 1 and H#?(tabloid {{0,0},{1,1}}) and H#(tabloid {{0,0},{1,1}}) == 1/8);
assert(all(keys H, T -> instance(T, HookTabloid)) and all(values H, c -> instance(c, QQ) and c != 0));
assert(#frobeniusUnion(tabloid {{0,1},{0}}, tabloid {{1}}, 2) == 0);
assert(#frobeniusUnion(tabloid {{0,1},{0}}, tabloid {{0}}, 2) == 0);
H3 = frobeniusUnion(tabloid {{0,0,1},{1}}, tabloid {{2}}, 3);
assert(#H3 == 2 and all(keys H3, T -> apply(toList T, r -> #r) == {3,2}));
assert(H3#(tabloid {{0,0,1},{1,2}}) == 4/9 and H3#(tabloid {{0,0,2},{1,1}}) == -2/9);
-- the keys are semistandard: rows weakly increasing, columns strictly
assert(all(keys H3, T -> (rows := toList T;
    all(rows, r -> r == sort r) and
    all(#rows#1, j -> rows#0#j < rows#1#j))));
-- the three input forms agree
assert(frobeniusUnion({{0,1},{1}}, {{0}}, 2) === H);
assert(frobeniusUnion(new Filling from {{0,1},{1}}, new Filling from {{0}}, 2) === H);
-- an entry outside 0..n-1 gives the zero image
assert(#frobeniusUnion(tabloid {{0,2},{1}}, tabloid {{0}}, 2) == 0);
-- validation: hook shapes, nesting (a > c and b > d), integer entries
assert(try (frobeniusUnion(tabloid {{0,1},{1,1}}, tabloid {{0}}, 2); false) else true);
assert(try (frobeniusUnion(tabloid {{0,1}}, tabloid {{0}}, 2); false) else true);
assert(try (frobeniusUnion(tabloid {{0,1},{1}}, tabloid {{0,1}}, 2); false) else true);
assert(try (frobeniusUnion(tabloid {{0,1},{1}}, tabloid {{0},{1}}, 2); false) else true);
assert(try (frobeniusUnion(tabloid {{0,1/2},{1}}, tabloid {{0}}, 2); false) else true);
assert(try (frobeniusUnion(tabloid {{0,1},{1}}, tabloid {{0}}, -1); false) else true);
///

TEST ///
-- frobeniusUnionMatrix: the 6 x 24 surjection for the types (1,1) and
-- (0,0) on three letters, its labels, the agreement of its columns with
-- frobeniusUnion, the vanishing of its composite with the Pieri
-- injection after matching the bases, degenerate sizes, and validation
N = frobeniusUnionMatrix({1,1},{0,0},3);
assert(instance(N, Matrix) and ring N === QQ);
assert((numRows N, numColumns N, rank N) == (6, 24, 6));
assert(N.cache#?"sourceBasis" and N.cache#?"targetBasis");
src = pieriSourceBasis N; tgt = pieriTargetBasis N;
assert(#src == 24 and #tgt == 6);
assert(all(src, P -> instance(P, List) and #P == 2 and all(P, T -> instance(T, HookTabloid))));
assert(all(tgt, T -> instance(T, HookTabloid) and apply(toList T, r -> #r) == {2,2}));
shape = T -> apply(toList T, r -> #r);
assert(all(src, P -> shape P#0 == {2,1} and shape P#1 == {1}));
-- every column is the image of its pair
for i from 0 to 23 do (
    I := frobeniusUnion(src#i#0, src#i#1, 3);
    col := flatten entries N_{i};
    assert(#I == #positions(col, c -> c != 0));
    assert(all(keys I, T -> I#T == col#(position(tgt, U -> U === T)))));
-- the composite with the Pieri injection is zero once the target pairs
-- of the injection (letters 1..n) are matched with the source pairs of
-- the union (letters 0..n-1)
M = pieriMatrix({1,1},{0,0},3);
shift = T -> tabloid apply(toList T, r -> apply(r, x -> x + 1));
pos = hashTable apply(#pieriTargetBasis M, i -> (pieriTargetBasis M)#i => i);
perm = apply(src, P -> pos#(apply(P, shift)));
assert(sort perm == toList(0..23));
assert(N_perm * M == 0);
assert(rank N + rank M == numRows M);
-- two letters: the 1 x 4 matrix
N2 = frobeniusUnionMatrix({1,1},{0,0},2);
assert((numRows N2, numColumns N2) == (1, 4) and #pieriTargetBasis N2 == 1);
-- one letter: no semistandard tableau of the nest shape, no rows
N1 = frobeniusUnionMatrix({1,1},{0,0},1);
assert(numRows N1 == 0 and pieriTargetBasis N1 == {});
-- validation
assert(try (frobeniusUnionMatrix({1,1},{1,0},3); false) else true);
assert(try (frobeniusUnionMatrix({0,1},{0,0},3); false) else true);
assert(try (frobeniusUnionMatrix({1,1},{0,0},-1); false) else true);
assert(try (frobeniusUnionMatrix(2,1,1,0,3); false) else true);
///

TEST ///
-- verifyGarnirLift, the consistent case: the arm step 1 lift agrees with
-- straightening on all 81 tabloids of the nest shape (2,2) on three
-- letters; the report's kind, rows, verdict and display
R = verifyGarnirLift({1,1},{0,0},3);
assert(instance(R, VerificationReport) and R#"kind" == "GarnirLift");
assert(R#"passed" === true and R#"status" == "consistent");
assert(R#"parameters" == {{1,1},{0,0},3});
assert(R#"tabloidsTested" == 81);
assert(instance(R#"universalScalar", QQ) and R#"universalScalar" != 0);
assert(R#"counterexamples" == {} and R#"counterexampleData" == {});
assert(R#"minimalCounterexample" === null);
assert(instance(R#"message", String) and instance(R#"conclusion", String));
assert(R#"keyOrder" == {"parameters", "status", "universalScalar", "tabloidsTested",
    "counterexamples", "minimalCounterexample", "counterexampleData", "message", "passed", "conclusion"});
assert(all(keys R, k -> instance(k, String)));
assert(instance(net R, Net));
assert(toString R == "VerificationReport{kind => GarnirLift, passed => true}");
///

TEST ///
-- verifyGarnirLift, the failing case: for the arm step 2 the lift fails,
-- the universal scalar is 4/9, the counterexample records carry
-- HookTabloids and hash tables with the stated relation between actual,
-- expected and straightening, the two hooks of each record are the
-- diagonal hooks of the tabloid, and the minimal clean counterexample
-- has its only repeat in the second column
debug needsPackage "HookPieri";
R = verifyGarnirLift({2,1},{0,0},3);
assert(instance(R, VerificationReport) and R#"kind" == "GarnirLift");
assert(R#"passed" === false and R#"status" == "counterexamples");
assert(R#"universalScalar" == 4/9);
assert(R#"tabloidsTested" == 243);
ces = R#"counterexamples"; data = R#"counterexampleData";
assert(#ces > 0 and #ces == #data);
assert(all(ces, T -> instance(T, HookTabloid) and apply(toList T, r -> #r) == {3,2}));
assert(#unique ces == #ces);
for rec in data do (
    assert(instance(rec, HashTable));
    assert(set keys rec === set {"tabloid", "T1", "T2", "straightening", "actualMu", "expected"});
    assert(instance(rec#"T1", HookTabloid) and instance(rec#"T2", HookTabloid));
    assert(apply(toList rec#"T1", r -> #r) == {3,1} and apply(toList rec#"T2", r -> #r) == {1});
    -- the two hooks are the diagonal hooks of the tabloid
    hks := apply(hpDiagonalHooks toList rec#"tabloid", h -> tabloid(h#0, h#1));
    assert(hks == {rec#"T1", rec#"T2"});
    assert(tabloid(rec#"T1", rec#"T2") == rec#"tabloid");
    -- expected is the scalar times the straightening, and differs from actual
    assert(rec#"expected" === applyValues(rec#"straightening", v -> 4/9 * v));
    assert(rec#"actualMu" =!= rec#"expected");
    assert(all(keys rec#"actualMu" | keys rec#"straightening", T -> instance(T, HookTabloid))));
assert(apply(data, rec -> rec#"tabloid") == ces);
-- the record of an entry is reproduced by frobeniusUnion on its hooks
rec0 = data#0;
assert(frobeniusUnion(rec0#"T1", rec0#"T2", 3) === rec0#"actualMu");
-- the minimal clean counterexample
G = R#"minimalCounterexample";
assert(instance(G, HashTable) and member(G, data));
rows = toList G#"tabloid";
col2 = apply(select(rows, r -> #r > 1), r -> r#1);
assert(#unique col2 < #col2);
assert(all(rows, r -> r == sort r));
col1 = apply(rows, r -> r#0);
assert(col1 == sort col1 and #unique col1 == #col1);
assert(#G#"straightening" == 0 and #G#"expected" == 0 and #G#"actualMu" > 0);
assert(instance(net R, Net));
assert(toString R == "VerificationReport{kind => GarnirLift, passed => false}");
///

TEST ///
-- verifyGarnirLift, the diagnostic case and the validation: a too small
-- alphabet gives the status no-ssyt with passed false and nothing
-- tested; the types and the alphabet are validated
R = verifyGarnirLift({1,1},{0,0},1);
assert(instance(R, VerificationReport) and R#"kind" == "GarnirLift");
assert(R#"status" == "no-ssyt" and R#"passed" === false);
assert(R#"universalScalar" === null and R#"tabloidsTested" == 0);
assert(R#"counterexamples" == {} and R#"minimalCounterexample" === null);
assert(instance(net R, Net));
assert(try (verifyGarnirLift({1,1},{0,0},-1); false) else true);
assert(try (verifyGarnirLift({1,1},{0,0},3/2); false) else true);
assert(try (verifyGarnirLift({1,1},{1,0},3); false) else true);
assert(try (verifyGarnirLift({2,1},{0,1},3); false) else true);
assert(try (verifyGarnirLift(2,1,1,0,3); false) else true);
-- the public names are exported and the retired ones are not
exported = apply(HookPieri#"exported symbols", toString);
assert(all({"frobeniusUnion", "frobeniusUnionMatrix", "verifyGarnirLift"}, n -> member(n, exported)));
assert(not any({"applyNestedHookComp", "nestedHookCompMatrix", "decomposeNestTabloid",
    "testNaiveGarnirLift", "allGarnirCounterexamples", "findMinimalGarnirCounterexample"},
    n -> member(n, exported)));
///

TEST ///
-- PoincareSeries: the four constructors return the type, memoize their
-- value, carry the record keys, and refuse r < 2
P = genericDetPoincare(2,3,2);
assert(instance(P, PoincareSeries) and instance(P, HashTable));
assert(genericDetPoincare(2,3,2) === P);
assert(P#"family" == "generic" and P#"parameters" == (2,3,2) and P#"generatorDegree" == 2);
assert(instance(P#"denominatorTerms", List) and #P#"denominatorTerms" == 2);
rec = first P#"denominatorTerms";
assert(sort keys rec == sort {"lambda", "mu", "i", "d", "j", "sign", "character", "term"});
assert(all(P#"denominatorTerms", r -> r#"j" == r#"i" + (P#"generatorDegree" - 1) * r#"d"));
assert(instance(P.cache, CacheTable));
Q = symDetPoincare(3,2);
assert(instance(Q, PoincareSeries) and Q#"family" == "symmetric" and Q#"parameters" == (3,2));
F = pfaffianPoincare(4,2);
assert(instance(F, PoincareSeries) and F#"family" == "pfaffian" and F#"generatorDegree" == 2);
C = symDetCoverPoincare(3,3);
assert(instance(C, PoincareSeries) and C#"family" == "symmetricCover" and C#"generatorDegree" == 2);
assert(symDetPoincare(3,2) === Q and pfaffianPoincare(4,2) === F and symDetCoverPoincare(3,3) === C);
assert(try (genericDetPoincare(2,2,1); false) else true);
assert(try (symDetPoincare(3,1); false) else true);
assert(try (pfaffianPoincare(4,1); false) else true);
assert(try (symDetCoverPoincare(3,1); false) else true);
assert(try (genericDetPoincare(0,2,2); false) else true);
///

TEST ///
-- PoincareSeries accessors: numerator, denominator, ring and
-- coefficientRing on the Core generics; the rings are shared and the
-- global symbols s, t, x, y stay unbound
P = genericDetPoincare(2,3,2);
assert(numerator P === P.numerator and denominator P === P.denominator);
assert(ring P === P.ring and ring numerator P === ring P and ring denominator P === ring P);
S = coefficientRing P;
assert(instance(S, PolynomialRing) and numgens S == 2 and coefficientRing S === QQ);
assert(toString S_0 == "s" and toString S_1 == "t");
assert(coefficientRing genericDetPoincare(4,4,3) === S and coefficientRing pfaffianPoincare(4,2) === S);
assert(ring genericDetPoincare(2,3,2) === ring genericDetPoincare(2,3,3));
assert(ring symDetPoincare(3,2) === ring pfaffianPoincare(3,2));
assert(instance(s, Symbol) and instance(t, Symbol) and instance(x, Symbol) and instance(y, Symbol));
-- r > min(n, m): the ideal is zero and the denominator is 1
Z = genericDetPoincare(2,2,3);
assert(denominator Z == 1_(ring Z) and #Z#"denominatorTerms" == 0);
assert(dimensionSpecialization Z == {(1 + S_1)^4, 1_S});
///

TEST ///
-- PoincareSeries display: expression is the Divide, net and toString go
-- through it, describe is the constructor call, toExternalString reads
-- back as the same object, texMath is the fraction
P = genericDetPoincare(2,2,2);
E = expression P;
assert(instance(E, Divide));
assert(value numerator E == numerator P and value denominator E == denominator P);
assert(instance(net P, Net) and net P == net E);
assert(toString P == toString E);
assert(instance(describe P, Describe) and toString describe P == "genericDetPoincare(2,2,2)");
assert(toExternalString P == "genericDetPoincare(2,2,2)");
assert(value toExternalString P === P);
assert(toString describe symDetPoincare(3,2) == "symDetPoincare(3,2)");
assert(toString describe pfaffianPoincare(4,2) == "pfaffianPoincare(4,2)");
assert(toString describe symDetCoverPoincare(3,3) == "symDetCoverPoincare(3,3)");
assert(value toExternalString symDetCoverPoincare(3,3) === symDetCoverPoincare(3,3));
assert(instance(texMath P, String) and substring(0, 6, texMath P) == "\\frac{");
///

TEST ///
-- expandPoincare: a BettiTally with Schur ring entries, the trivial
-- character at (0,{0},0), the truncation bounds, and the dimensions of
-- the entries against betti(P, N)
P = genericDetPoincare(2,2,2);
B = expandPoincare(P, 5);
assert(instance(B, BettiTally));
assert(B#(0,{0},0) == 1_(ring P));
assert(all(keys B, k -> k#0 <= 5 and k#1 == {k#2}));
assert(all(values B, v -> instance(v, RingElement) and ring v === ring P));
T = expandPoincare(P, 5, Truncation => "TotalDegree");
assert(all(keys T, k -> k#0 + k#2 <= 5));
-- Koszul rings: Tor_i(k,k) is concentrated in internal degree i
for Q in {P, symDetPoincare(3,2), pfaffianPoincare(4,2)} do (
    H := expandPoincare(Q, 5);
    assert(#keys H >= 1);
    assert(all(keys H, k -> k#0 == k#2)));
E = expandPoincare(genericDetPoincare(3,3,3), 4);
assert(max apply(keys E, k -> k#0) == 4 and E#?(4,{6},6));
assert(sort keys expandPoincare(genericDetPoincare(3,3,3), 4, Truncation => "TotalDegree") == {(0,{0},0),(1,{1},1),(2,{2},2)});
D = betti(genericDetPoincare(3,3,3), 4);
for k in keys E do assert(D#k == lift(dimensionSpecialization E#k, ZZ));
assert(sort keys D == sort keys E);
assert(try (expandPoincare(P, -1); false) else true);
assert(try (expandPoincare(P, 2, Truncation => "Total"); false) else true);
///

TEST ///
-- betti(P, N), totalBettiNumbers and poincareSeriesTruncation: the
-- non-equivariant expansions agree with each other and with the paper
P = genericDetPoincare(5,5,3);
B = betti(P, 4);
assert(instance(B, BettiTally) and B#(4,{6},6) == 10000);
assert(betti(P, 4) === B);
assert(betti(P, 4, Weights => {1}) == B);
assert(totalBettiNumbers(P, 5) == {1, 25, 400, 5250, 64728, 779530});
assert(apply(5, i -> sum(select(keys B, k -> k#0 == i), k -> B#k)) == take(totalBettiNumbers(P, 5), 5));
S = coefficientRing P;
p5 = poincareSeriesTruncation(P, 5);
assert(ring p5 === S);
assert(p5 == 1 + 25*S_1 + 400*S_1^2 + 5250*S_1^3 + 64728*S_1^4 + 779530*S_1^5);
Q = genericDetPoincare(2,2,2);
assert(poincareSeriesTruncation(Q, 3, KeepInternalDegree => true) == 1 + 4*S_0*S_1 + 7*S_0^2*S_1^2 + 8*S_0^3*S_1^3);
assert(totalBettiNumbers(Q, 5) == {1, 4, 7, 8, 8, 8});
assert(betti(genericDetPoincare(2,3,2), 4) == new BettiTally from {(0,{0},0) => 1, (1,{1},1) => 6, (2,{2},2) => 18, (3,{3},3) => 40, (4,{4},4) => 81});
assert(try (betti(Q, -1); false) else true);
///

TEST ///
-- dimensionSpecialization and ordinaryPoincareSeries: the paper's
-- numerator and denominator at s = 1, with s kept, on a single
-- character, and the reduced rational function
P = genericDetPoincare(2,2,2);
S = coefficientRing P;
s = S_0; t = S_1;
L = dimensionSpecialization P;
assert(instance(L, List) and #L == 2 and ring L#0 === S);
assert(L == {(1 + t)^4, 1 - t^2});
assert(dimensionSpecialization P === L);
K = dimensionSpecialization(P, KeepInternalDegree => true);
assert(K == {(1 + s*t)^4, 1 - s^2*t^2});
assert(K#1 == sub(L#1, {t => s*t}));
assert(dimensionSpecialization denominator P == K#1);
e = dimensionSpecialization numerator P;
assert(ring e === S and e == K#0);
f = ordinaryPoincareSeries P;
assert(ring f === frac S);
assert(f * sub(L#1, frac S) == sub(L#0, frac S));
assert(ring ordinaryPoincareSeries symDetPoincare(3,2) === frac S);
assert((dimensionSpecialization genericDetPoincare(4,4,3))#1 == 1 - 16*t^2 - 30*t^3 - 16*t^4 + t^6);
assert((dimensionSpecialization genericDetPoincare(2,4,2))#1 == 1 - 6*t^2 - 8*t^3 - 3*t^4);
assert(dimensionSpecialization symDetCoverPoincare(3,2) == {(1 + t)^3, 1_S});
///

TEST ///
-- strandBettiTable and equivariantStrandBettiTable: BettiTallies for
-- every r, the Eagon--Northcott and Gulliksen--Negard numbers, the
-- characters, and the agreement with actual resolutions
ST = strandBettiTable genericDetPoincare(2,4,2);
assert(instance(ST, BettiTally));
assert(ST == new BettiTally from {(0,{0},0) => 1, (1,{2},2) => 6, (2,{3},3) => 8, (3,{4},4) => 3});
assert(strandBettiTable genericDetPoincare(2,4,2) === ST);
GN = strandBettiTable genericDetPoincare(4,4,3);
assert(all(values GN, v -> instance(v, ZZ) and v > 0));
assert(sum values GN == 1 + 16 + 30 + 16 + 1);
B = strandBettiTable genericDetPoincare(5,5,3);
assert(sum apply(select(keys B, k -> k#2 - k#0 == 2), k -> B#k) == 2303);
S = QQ[a_1..a_9];
assert(strandBettiTable genericDetPoincare(3,3,2) == betti freeResolution(S^1/minors(2, genericMatrix(S,3,3))));
EQ = equivariantStrandBettiTable genericDetPoincare(3,3,2);
assert(instance(EQ, BettiTally));
assert(EQ#(0,{0},0) == 1_(ring genericDetPoincare(3,3,2)));
assert(lift(dimensionSpecialization EQ#(1,{2},2), ZZ) == 9);
assert(sort keys EQ == sort keys strandBettiTable genericDetPoincare(3,3,2));
for k in keys EQ do assert(lift(dimensionSpecialization EQ#k, ZZ) == (strandBettiTable genericDetPoincare(3,3,2))#k);
assert(verifyStrandBettiTable genericDetPoincare(2,4,2));
assert(verifyStrandBettiTable genericDetPoincare(4,4,3));
assert(verifyStrandBettiTable symDetPoincare(4,3));
assert(verifyStrandBettiTable pfaffianPoincare(6,3));
assert(verifyStrandBettiTable symDetCoverPoincare(3,2));
///

TEST ///
-- determinantalRing: the quotient ring of each family, cached, with the
-- cover for r = 2 and r = 3 only
A = determinantalRing genericDetPoincare(2,3,2);
assert(instance(A, QuotientRing) and numgens A == 6 and numgens ideal A == 3);
assert(determinantalRing genericDetPoincare(2,3,2) === A);
assert(coefficientRing A === QQ);
B = determinantalRing symDetPoincare(3,2);
assert(numgens B == 6 and ideal B == minors(2, genericSymmetricMatrix(ambient B, 3)));
C = determinantalRing pfaffianPoincare(5,2);
assert(numgens C == 10 and numgens ideal C == 5);
D = determinantalRing symDetCoverPoincare(3,2);
assert(numgens D == 3 and ideal D == 0);
E = determinantalRing symDetCoverPoincare(2,3);
assert(numgens E == 4 and numgens ideal E == 1 and unique degrees ambient E == {{2}});
assert(try (determinantalRing symDetCoverPoincare(3,4); false) else true);
///

TEST ///
-- the checks: isGolodPoincare, koszulPoincareCheck and
-- verifyPoincareSeries return Booleans, agree with Macaulay2's
-- resolutions and Hilbert series, and print only when debugLevel > 0
assert(isGolodPoincare genericDetPoincare(2,4,2) and not isGolodPoincare genericDetPoincare(4,4,3));
assert(isGolodPoincare genericDetPoincare(2,2,3));
assert(koszulPoincareCheck genericDetPoincare(2,3,2) and koszulPoincareCheck symDetPoincare(3,2) and koszulPoincareCheck pfaffianPoincare(5,2));
assert(try (koszulPoincareCheck genericDetPoincare(3,3,3); false) else true);
for P in {genericDetPoincare(2,3,2), genericDetPoincare(3,3,3), symDetPoincare(3,3), pfaffianPoincare(5,2)} do
    assert(verifyPoincareSeries(P, 3) === true);
assert(try (verifyPoincareSeries(genericDetPoincare(2,2,2), -1); false) else true);
-- the cover M_r of the symmetric determinantal ring
C = symDetCoverPoincare(3,3);
assert(totalBettiNumbers(C, 4) == {1, 9, 45, 181, 684});
assert(koszulPoincareCheck C and verifyPoincareSeries(C, 3));
assert(verifyPoincareSeries(symDetCoverPoincare(4,3), 2));
-- diagnostics go to standard output only when debugLevel > 0
debugLevel = 1;
assert(verifyStrandBettiTable genericDetPoincare(2,4,2) === true);
assert(koszulPoincareCheck genericDetPoincare(2,2,2) === true);
assert(verifyPoincareSeries(genericDetPoincare(2,2,2), 2) === true);
debugLevel = 0;
///

TEST ///
-- expandPoincare and equivariantStrandBettiTable return an
-- EquivariantBettiTally: a BettiTally (keys, ==, #) whose entries are
-- characters in the Schur ring of the series, so that the products the
-- expansion forms are multiplied out by the Littlewood-Richardson rule,
-- and whose net separates the columns by vertical rules
needsPackage "SchurRings";
P = genericDetPoincare(3,3,2);
B = expandPoincare(P, 3);
E = equivariantStrandBettiTable P;
assert(instance(B, EquivariantBettiTally) and instance(B, BettiTally));
assert(instance(E, EquivariantBettiTally));
assert(not instance(betti(P, 3), EquivariantBettiTally) and not instance(strandBettiTable P, EquivariantBettiTally));
R = ring P; Ry = coefficientRing R;
assert(instance(R, SchurRing) and instance(Ry, SchurRing));
-- Tor_2^R(k,k) is (V ⊗ W)^{⊗ 2} minus R_2 = S_2 V ⊗ S_2 W, multiplied out
c = B#(1,{1},1);
assert(c == Ry_{1} * R_{1});
assert(B#(2,{2},2) == Ry_{1,1} * R_{2} + (Ry_{2} + Ry_{1,1}) * R_{1,1});
assert(c^2 - B#(2,{2},2) == Ry_{2} * R_{2});
assert(E#(1,{2},2) == Ry_{1,1} * R_{1,1});
-- the same table rebuilt from its pairs compares equal with ==, tables
-- of characters and of integers compare entry by entry, and the net has
-- a vertical rule before every column
assert(B == new EquivariantBettiTally from pairs B);
assert(B == expandPoincare(P, 3));
assert(B != betti(P, 3) and betti(P, 3) != B);
s = toString net B;
assert(#select("\\|", s) >= 10 and match("total:", s));
-- a table too wide for the page is broken into panels that each fit, and
-- the characters are wrapped so that no single column overflows
oldPrintWidth = printWidth;
printWidth = 79;
wide = expandPoincare(genericDetPoincare(3,3,3), 5);
assert(width net wide <= 79);
assert(#unstack net wide > #unstack net expandPoincare(genericDetPoincare(3,3,3), 2));
printWidth = 400;
assert(width net wide == 287 and not match("\n\n", toString net wide));
printWidth = oldPrintWidth;
-- the non-Koszul table has more than one row, and its dimensions are the
-- entries of the integer table
nk = expandPoincare(genericDetPoincare(3,3,3), 3);
assert(#unique apply(keys nk, k -> k#2 - k#0) == 2);
assert(betti(genericDetPoincare(3,3,3), 3) == new BettiTally from apply(pairs nk, kv -> kv#0 => lift(lift(dimensionSpecialization kv#1, QQ), ZZ)));
-- the dimensions of the entries are the integer table
assert(betti(P, 3) == new BettiTally from apply(pairs B, kv -> kv#0 => lift(lift(dimensionSpecialization kv#1, QQ), ZZ)));
-- the empty and the one-entry tables print
assert(instance(net new EquivariantBettiTally from {}, Net));
assert(#expandPoincare(P, 0) == 1);
///

TEST ///
-- verifyStrandBettiTable compares the strand table against a resolution over
-- the polynomial ring the ring is finite over.  For the cover M_r with r = 3
-- that ring is Sym(S^2 V), not the n^2 variable ambient of the Segre
-- presentation determinantalRing builds, so resolving over the ambient used
-- to reject a correct table.  All four families must verify.
assert(verifyStrandBettiTable symDetCoverPoincare(2,3));
assert(verifyStrandBettiTable symDetCoverPoincare(3,3));
assert(verifyStrandBettiTable symDetCoverPoincare(3,2));
assert(verifyStrandBettiTable genericDetPoincare(3,3,2));
assert(verifyStrandBettiTable symDetPoincare(3,2));
assert(verifyStrandBettiTable pfaffianPoincare(5,2));
-- the cover's strand table is the Betti table of M_3 over Sym(S^2 V), whose
-- variables have degree 2: one generator in degree 0 and three in degree 2,
-- then three first syzygies in degree 4 and one in degree 6
B = strandBettiTable symDetCoverPoincare(3,3);
assert(B == new BettiTally from {
    (0,{0},0) => 1, (0,{2},2) => 3, (1,{4},4) => 3, (1,{6},6) => 1});
-- and the equivariant form has the same shape with characters in place of
-- the dimensions
E = equivariantStrandBettiTable symDetCoverPoincare(3,3);
assert(sort keys E == sort keys B);
assert(all(keys B, k -> lift(lift(dimensionSpecialization E#k, QQ), ZZ) == B#k));
///

TEST ///
-- hookBasis: the semistandard hook tabloids of a type as HookTabloids in
-- generator order, the counts of Example 1.2.2, the empty basis when the
-- column does not fit, and the validation of the type
B = hookBasis({1,1}, 3);
assert(#B == 8 and all(B, T -> instance(T, HookTabloid)));
assert(B#0 == tabloid {{1,1},{2}} and last B == tabloid {{2,3},{3}});
assert(all(B, T -> #toList T == 2 and #(toList T)#0 == 2));
assert(apply({{0,0},{1,1},{2,2}}, h -> #hookBasis(h, 3)) == {3,8,6});
assert(hookBasis({2,3}, 3) == {});
assert(hookBasis({0,0}, 0) == {});
assert(hookBasis({0,0}, 2) == {tabloid {{1}}, tabloid {{2}}});
assert(#unique hookBasis({2,1}, 4) == #hookBasis({2,1}, 4));
assert(sort hookBasis({1,1}, 3) == sort unique hookBasis({1,1}, 3));
-- the private native basis and the public one are the same tabloids
debug needsPackage "HookPieri";
assert(apply(hookSSYTBasis(2,1,3), h -> tabloid(h#0, h#1)) == B);
assert(try (hookBasis({1}, 3); false) else true);
assert(try (hookBasis({1,-1}, 3); false) else true);
assert(try (hookBasis({1,1}, -1); false) else true);
assert(try (hookBasis(3, 3); false) else true);
///

TEST ///
-- pieriRowRemove: the values of the closed formula of Section 4 (the
-- coefficients 9 and 2 computed by hand), the keys {beta, U} with U
-- semistandard, sparsity after cancellation, the sign of the column, the
-- zero map on a repeated column entry, the three input forms, and the
-- coefficient field
needsPackage "SchurFunctors";
H = pieriRowRemove(tabloid {{1,1,1,1,1},{2}}, 2);
assert(instance(H, HashTable) and not instance(H, MutableHashTable));
assert(H#{{1,1}, tabloid {{1,1,1},{2}}} == 9);
assert(all(keys H, k -> instance(k, List) and #k == 2 and instance(k#0, List) and
    instance(k#1, HookTabloid) and isHookSemistandard toList k#1));
assert(all(values H, c -> c != 0 and instance(c, QQ)));
H2 = pieriRowRemove(tabloid {{1,1,2,3,4},{2}}, 2);
assert(H2#{{1,2}, tabloid {{1,3,4},{2}}} == 2);
assert(#keys pieriRowRemove(tabloid({1,2,3,4},{2,1}), 2) == #keys H2);
assert((pieriRowRemove(tabloid({1,2,3,4},{2,1}), 2))#{{1,2}, tabloid {{1,3,4},{2}}} == -2);
assert(#keys pieriRowRemove(tabloid({1,2,3,4},{1,1}), 2) == 0);
-- P_1 sends x_1^3 to 3 x_1 (x) x_1^2
P1 = pieriRowRemove(tabloid {{1,1,1}}, 1);
assert(#P1 == 1 and P1#{{1}, tabloid {{1,1}}} == 3);
-- no zero coefficient is kept
S = pieriRowRemove(tabloid {{1,1,2,3},{2}}, 2);
assert(all(values S, c -> c != 0));
assert(not S#?{{1,2}, tabloid {{1,2},{3}}});
-- the three input forms agree
assert(pieriRowRemove({{1,2},{3}}, 1) === pieriRowRemove(tabloid {{1,2},{3}}, 1));
assert(pieriRowRemove(new Filling from {{1,3},{2}}, 1) === pieriRowRemove(tabloid {{1,2},{3}}, 1));
-- the coefficient field
K = ZZ/7;
HK = pieriRowRemove(tabloid {{1,1,1,1,1},{2}}, 2, CoefficientRing => K);
assert(HK#{{1,1}, tabloid {{1,1,1},{2}}} == 9_K and ring HK#{{1,1}, tabloid {{1,1,1},{2}}} === K);
-- the formula divides by a+b-m; characteristic 5 is refused for a+b-m = 5
assert(try (pieriRowRemove(tabloid {{1,1,1,1},{2},{3}}, 1, CoefficientRing => ZZ/5); false) else true);
assert(instance(pieriRowRemove(tabloid {{1,1,1,1},{2},{3}}, 1, CoefficientRing => ZZ/7), HashTable));
assert(try (pieriRowRemove(tabloid {{1,2},{3}}, 1, CoefficientRing => ZZ); false) else true);
-- validation
assert(try (pieriRowRemove(tabloid {{1,2},{3}}, 0); false) else true);
assert(try (pieriRowRemove(tabloid {{1,2},{3}}, 2); false) else true);
assert(try (pieriRowRemove(tabloid {{1,2},{3,4}}, 1); false) else true);
assert(try (pieriRowRemove(tabloid {{0,2},{3}}, 1); false) else true);
assert(try (pieriRowRemove(tabloid {{1,2},{3}}, 1/2); false) else true);
///

TEST ///
-- juxtaposeSymToRow: the multiplication Q_m into the row, straightened to
-- the semistandard basis; a letter at least the corner is appended, a
-- smaller letter triggers the single hook relation; the input forms and
-- the field
needsPackage "SchurFunctors";
J = juxtaposeSymToRow(tabloid {{1,2},{3}}, {2});
assert(#J == 1 and J#(tabloid {{1,2,2},{3}}) == 1);
J2 = juxtaposeSymToRow(tabloid {{2},{3}}, {1});
assert(instance(J2, HashTable) and all(keys J2, T -> instance(T, HookTabloid) and isHookSemistandard toList T));
assert(all(values J2, c -> c != 0));
assert(J2#?(tabloid {{1,2},{3}}) or J2#?(tabloid {{1,3},{2}}));
-- the letters may be given in any order
assert(juxtaposeSymToRow(tabloid {{3},{4}}, {2,1}) === juxtaposeSymToRow(tabloid {{3},{4}}, {1,2}));
-- the empty monomial is the straightening of the hook itself
J0 = juxtaposeSymToRow(tabloid {{2,4},{3}}, {});
assert(#J0 == 1 and J0#(tabloid {{2,4},{3}}) == 1);
J1 = juxtaposeSymToRow(tabloid({4},{3,2,1}), {});
assert(#J1 == 1 and J1#(tabloid {{1,4},{2},{3}}) == -1);
-- the three input forms agree
assert(juxtaposeSymToRow({{2},{3}}, {1}) === J2);
assert(juxtaposeSymToRow(new Filling from {{2,3}}, {1}) === J2);
-- the field
JK = juxtaposeSymToRow(tabloid {{2},{3}}, {1}, CoefficientRing => ZZ/3);
assert(all(values JK, c -> ring c === ZZ/3));
assert(try (juxtaposeSymToRow(tabloid {{2},{3}}, {1}, CoefficientRing => RR); false) else true);
-- validation
assert(try (juxtaposeSymToRow(tabloid {{2},{3}}, {0}); false) else true);
assert(try (juxtaposeSymToRow(tabloid {{2},{3}}, 1); false) else true);
assert(try (juxtaposeSymToRow(tabloid {{2,3},{3,4}}, {1}); false) else true);
///

TEST ///
-- pieriColumnAdd: the column Pieri map Q'_m with the sign and the factor
-- 1/(a+b+m) of the paper, the zero wedge, the input forms and the field
needsPackage "SchurFunctors";
C = pieriColumnAdd(tabloid {{1,1},{2}}, {3});
assert(#C == 1 and C#(tabloid {{1,1},{2},{3}}) == 3/4);
C2 = pieriColumnAdd(tabloid {{1,1,2,3}}, {3,4});
assert(instance(C2, HashTable) and #C2 > 0);
assert(all(keys C2, T -> instance(T, HookTabloid) and isHookSemistandard toList T and #toList T == 3));
assert(all(values C2, c -> c != 0));
-- a repeated letter in the wedge, or a letter already in the column, gives zero
assert(#pieriColumnAdd(tabloid {{1,1},{2}}, {3,3}) == 0);
assert(#pieriColumnAdd(tabloid {{1,1},{2}}, {2}) == 0);
-- reversing the wedge changes the sign
C3 = pieriColumnAdd(tabloid {{2,3,4}}, {3,5});
C4 = pieriColumnAdd(tabloid {{2,3,4}}, {5,3});
assert(set keys C3 === set keys C4 and all(keys C3, T -> C3#T == -C4#T));
-- the three input forms agree
assert(pieriColumnAdd({{1,1},{2}}, {3}) === C);
assert(pieriColumnAdd(new Filling from {{1,2},{1}}, {3}) === C);
-- the field: 3/4 in ZZ/7 is 6; a+b+m = 4 is refused in characteristic 2
CK = pieriColumnAdd(tabloid {{1,1},{2}}, {3}, CoefficientRing => ZZ/7);
assert(CK#(tabloid {{1,1},{2},{3}}) == 6_(ZZ/7));
assert(try (pieriColumnAdd(tabloid {{1,1},{2}}, {3}, CoefficientRing => ZZ/2); false) else true);
-- validation
assert(try (pieriColumnAdd(tabloid {{1,1},{2}}, {}); false) else true);
assert(try (pieriColumnAdd(tabloid {{1,1},{2}}, {0}); false) else true);
assert(try (pieriColumnAdd(tabloid {{1,1},{2,2}}, {3}); false) else true);
///

TEST ///
-- pieriMatrix: the injection for the types (1,1) and (0,0) on three
-- letters is 24 x 18 of rank 18 with a cokernel of dimension 6, the two
-- string cache keys hold the HookTabloid pair labels in the right
-- numbers and types, injectivity over several small type pairs, the
-- field option, and the validation of the arguments
M = pieriMatrix({1,1},{0,0},3);
assert(instance(M, Matrix) and ring M === QQ);
assert((numRows M, numColumns M, rank M) == (24, 18, 18));
assert(rank coker M == 6);
assert(M.cache#?"sourceBasis" and M.cache#?"targetBasis");
src = M.cache#"sourceBasis"; tgt = M.cache#"targetBasis";
assert(#src == 18 and #tgt == 24);
isPair = P -> instance(P, List) and #P == 2 and all(P, T -> instance(T, HookTabloid));
assert(all(src, isPair) and all(tgt, isPair));
-- source pairs have the crossed types (0,1) and (1,0), target pairs the nested types (1,1) and (0,0)
shape = T -> apply(toList T, r -> #r);
assert(all(src, P -> shape P#0 == {1,1} and shape P#1 == {2}));
assert(all(tgt, P -> shape P#0 == {2,1} and shape P#1 == {1}));
assert(pieriSourceBasis M === src and pieriTargetBasis M === tgt);
-- the labels are the hook bases in generator order
assert(src == flatten for T1 in hookBasis({0,1},3) list for T2 in hookBasis({1,0},3) list {T1, T2});
assert(tgt == flatten for T1 in hookBasis({1,1},3) list for T2 in hookBasis({0,0},3) list {T1, T2});
for types in {({1,2},{0,0},3), ({1,2},{0,1},3), ({2,2},{1,1},3), ({1,1},{0,0},3)} do (
    N := pieriMatrix types;
    assert(rank N == numColumns N and #pieriSourceBasis N == numColumns N and #pieriTargetBasis N == numRows N));
-- the field option
M5 = pieriMatrix({1,1},{0,0},3, CoefficientRing => ZZ/5);
assert(ring M5 === ZZ/5 and rank M5 == 18 and #pieriSourceBasis M5 == 18);
assert(try (pieriMatrix({1,1},{0,0},3, CoefficientRing => ZZ); false) else true);
-- validation: the outer type must be larger in both coordinates, types
-- are pairs of nonnegative integers, n is positive
assert(try (pieriMatrix({1,1},{1,0},3); false) else true);
assert(try (pieriMatrix({1,2},{0,1},3); true) else false);
assert(try (pieriMatrix({0,0},{1,1},3); false) else true);
assert(try (pieriMatrix({1},{0,0},3); false) else true);
assert(try (pieriMatrix({1,1},{0,-1},3); false) else true);
assert(try (pieriMatrix({1,1},{0,0},0); false) else true);
assert(try (pieriMatrix(2,1,1,0,3); false) else true);
///

TEST ///
-- pieriMatrix with Content: the block of one content is the restriction
-- of the full matrix to the pairs of that content (the same entries, the
-- same order), it is cached and relabeled the same way, and the content
-- is validated
M = pieriMatrix({2,2},{1,1},4);
w = {3,2,2,1};
Mw = pieriMatrix({2,2},{1,1},4, Content => w);
assert((numRows Mw, numColumns Mw) == (23, 21));
letters = P -> sort flatten apply(P, T -> flatten toList T);
rows = positions(pieriTargetBasis M, P -> letters P == {1,1,1,2,2,3,3,4});
cols = positions(pieriSourceBasis M, P -> letters P == {1,1,1,2,2,3,3,4});
assert(#rows == 23 and #cols == 21);
assert(M^rows_cols == Mw);
assert((pieriTargetBasis M)_rows == pieriTargetBasis Mw);
assert((pieriSourceBasis M)_cols == pieriSourceBasis Mw);
assert(rank Mw == 21);
-- a second call returns the cached block with its labels
Mw2 = pieriMatrix({2,2},{1,1},4, Content => w);
assert(Mw2 == Mw and pieriSourceBasis Mw2 == pieriSourceBasis Mw);
-- the block of a content with no pairs of the outer leg is empty
assert(numColumns pieriMatrix({1,1},{0,0},3, Content => {4,0,0}) == 0);
-- validation of the content
assert(try (pieriMatrix({2,2},{1,1},4, Content => {3,2,2}); false) else true);
assert(try (pieriMatrix({2,2},{1,1},4, Content => {3,2,2,2}); false) else true);
assert(try (pieriMatrix({2,2},{1,1},4, Content => {3,2,3,-1}); false) else true);
///

TEST ///
-- pieriSourceBasis, pieriTargetBasis, pieriImage: the labels of a
-- matrix, one column as a hash table by index and by label, the local
-- image of a nested pair agreeing with the column, the three tabloid
-- input forms, the field, the Frobenius union matrix, and the errors
needsPackage "SchurFunctors";
M = pieriMatrix({1,1},{0,0},3);
src = pieriSourceBasis M; tgt = pieriTargetBasis M;
for i from 0 to numColumns M - 1 do (
    I := pieriImage(M, i);
    assert(instance(I, HashTable) and all(values I, c -> c != 0));
    col := flatten entries M_{i};
    assert(#I == #positions(col, c -> c != 0));
    assert(all(keys I, P -> I#P == col#(position(tgt, Q -> Q === P))));
    assert(pieriImage(M, src#i) === I);
    -- the local computation agrees with the column
    assert(pieriImage(src#i#0, src#i#1) === I));
-- labels may be given as rows
b = src#4;
assert(pieriImage(M, {toList b#0, toList b#1}) === pieriImage(M, 4));
assert(pieriImage(toList b#0, toList b#1) === pieriImage(M, 4));
assert(pieriImage(new Filling from b#0, new Filling from b#1) === pieriImage(M, 4));
-- the keys of a Pieri image are pairs of semistandard hook tabloids
I4 = pieriImage(M, 4);
assert(all(keys I4, P -> #P == 2 and all(P, T -> instance(T, HookTabloid) and isHookSemistandard toList T)));
-- the field of the local form; the matrix form takes the field of the matrix
IK = pieriImage(b#0, b#1, CoefficientRing => ZZ/7);
assert(set keys IK === set keys I4 and all(keys IK, P -> ring IK#P === ZZ/7 and IK#P == (I4#P)_(ZZ/7)));
M7 = pieriMatrix({1,1},{0,0},3, CoefficientRing => ZZ/7);
assert(pieriImage(M7, 4) === IK);
-- a Frobenius union matrix is labeled the same way
N = frobeniusUnionMatrix({1,1},{0,0},3);
assert(#pieriSourceBasis N == 24 and #pieriTargetBasis N == 6);
assert(all(pieriTargetBasis N, T -> instance(T, HookTabloid)));
IN = pieriImage(N, 0);
assert(all(keys IN, T -> instance(T, HookTabloid)) and all(values IN, c -> c != 0));
assert(#IN == #positions(flatten entries N_{0}, c -> c != 0));
assert(pieriImage(N, (pieriSourceBasis N)#0) === IN);
-- errors: an unlabeled matrix, an index out of range, a label not in the
-- basis, a pair in the wrong order, a nonnested pair
assert(try (pieriSourceBasis matrix{{1,0},{2,3}}; false) else true);
assert(try (pieriTargetBasis matrix{{1,0},{2,3}}; false) else true);
assert(try (pieriImage(matrix{{1,0},{2,3}}, 0); false) else true);
assert(try (pieriImage(M, 18); false) else true);
assert(try (pieriImage(M, -1); false) else true);
assert(try (pieriImage(M, {tabloid {{1,2}}, tabloid {{1}}}); false) else true);
assert(try (pieriImage(M, tgt#0); false) else true);
assert(try (pieriImage(b#1, b#0); false) else true);
assert(try (pieriImage(tabloid {{1,2},{3}}, tabloid {{1,2},{3}}); false) else true);
assert(try (pieriImage(tabloid {{1,2},{3,4}}, tabloid {{1,2,3}}); false) else true);
///

TEST ///
-- pieriPreimage: v = M x + w with w supported on the nested semistandard
-- target pairs, for every target pair; a nested semistandard pair has
-- the zero preimage; the complement is cached on the matrix; it works
-- over a finite field and on a content block; the errors
M = pieriMatrix({1,1},{0,0},3);
src = pieriSourceBasis M; tgt = pieriTargetBasis M;
nested = P -> isHookSemistandard toList tabloid(P#0, P#1);
goodCount = #select(tgt, nested);
assert(goodCount == numRows M - numColumns M);
checkPreimage = (M, idx) -> (
    src := pieriSourceBasis M; tgt := pieriTargetBasis M;
    x := pieriPreimage(M, tgt#idx);
    assert(instance(x, HashTable) and all(keys x, P -> member(P, src)) and all(values x, c -> c != 0));
    xvec := matrix apply(src, s -> {if x#?s then x#s else 0_(ring M)});
    w := (id_(target M))_{idx} - M * xvec;
    supp := positions(flatten entries w, c -> c != 0);
    assert(all(supp, j -> nested tgt#j));
    if nested tgt#idx then assert(#x == 0));
for idx from 0 to numRows M - 1 do checkPreimage(M, idx);
assert(M.cache#?"pieriComplement");
-- labels may be given as rows
t = tgt#3;
assert(pieriPreimage(M, {toList t#0, toList t#1}) === pieriPreimage(M, t));
-- over a finite field
M7 = pieriMatrix({1,1},{0,0},3, CoefficientRing => ZZ/7);
for idx from 0 to numRows M7 - 1 do checkPreimage(M7, idx);
assert(all(values pieriPreimage(M7, (pieriTargetBasis M7)#3), c -> ring c === ZZ/7));
-- on a content block
Mw = pieriMatrix({2,2},{1,1},4, Content => {3,2,2,1});
for idx from 0 to numRows Mw - 1 do checkPreimage(Mw, idx);
-- errors: a pair not in the target basis, a Frobenius union matrix, an
-- unlabeled matrix
assert(try (pieriPreimage(M, src#0); false) else true);
assert(try (pieriPreimage(frobeniusUnionMatrix({1,1},{0,0},3), {tabloid {{0,0},{1}}, tabloid {{0}}}); false) else true);
assert(try (pieriPreimage(matrix{{1,0},{2,3}}, {tabloid {{1}}, tabloid {{1}}}); false) else true);
///

TEST ///
-- the public names of the section are exported, the retired ones are not,
-- the private engines keep their names, and the coefficient field is
-- restored after every call, also after a failure
debug needsPackage "HookPieri";
exported = apply(HookPieri#"exported symbols", toString);
for n in {"hookBasis", "pieriRowRemove", "juxtaposeSymToRow", "pieriColumnAdd", "pieriMatrix",
    "pieriSourceBasis", "pieriTargetBasis", "pieriImage", "pieriPreimage", "hookSymbolicForm", "Content"} do
    assert(member(n, exported));
for n in {"hookSSYTBasis", "straightenHookHT", "pieriRowRemoveHT", "juxtaposeSymToRowHT",
    "hookInjectionMatrix", "hookPieriMatrix", "hookPieri", "hookPieriImage",
    "straightenAll", "allRewrites", "straighteningRelations", "sqfreeStraighten",
    "pieriMatrixWeight", "hookInjectionMatrixWeight", "rule", "sourceBasis", "targetBasis",
    "basisLabels"} do assert(not member(n, exported));
assert(instance(hookSSYTBasis, Function) and instance(hookInjectionMatrixWeight, Function));
assert(instance(hpStraightenHookHT, Function) and instance(hpPieriRowRemoveHT, Function) and
    instance(hpJuxtaposeSymToRowHT, Function) and instance(hpBatchRewriteRecords, Function));
assert(hpField() === QQ);
pieriMatrix({1,1},{0,0},3, CoefficientRing => ZZ/5);
pieriRowRemove(tabloid {{1,2},{3}}, 1, CoefficientRing => ZZ/5);
pieriImage(tabloid {{2},{3}}, tabloid {{1,2}}, CoefficientRing => ZZ/5);
assert(hpField() === QQ);
assert(try (pieriRowRemove(tabloid {{1,1,1,1},{2},{3}}, 1, CoefficientRing => ZZ/5); false) else true);
assert(hpField() === QQ);
///

TEST ///
-- hookSymbolicForm: one entry per source pair, each equal to pieriImage of
-- its column, and every coefficient a matrix entry
M = pieriMatrix({1,1},{0,0},3);
S = hookSymbolicForm M;
src = pieriSourceBasis M; tgt = pieriTargetBasis M;
assert(#S == #src and all(#src, i -> S#(src#i) === pieriImage(M, i)));
assert(all(#src, i -> all(pairs S#(src#i), kv -> M_(position(tgt, q -> q === kv#0), i) == kv#1)));
assert(try (hookSymbolicForm matrix{{1,2},{3,4}}; false) else true);
///

TEST ///
-- hookStraighten: the value is a HashTable keyed by semistandard
-- HookTabloids, the identity on semistandard inputs of every rank, the
-- same for the three input forms, with known small answers
needsPackage "SchurFunctors";
for T in {{{1,2,3},{4},{5}}, {{1,3,5},{2,4},{6}}, {{1,2,3},{4,5,6},{7,8,9}},
    {{1,2,3,4},{5,6,7},{8,9,10},{11}}} do (
    H := hookStraighten T;
    assert(instance(H, HashTable) and #H == 1);
    assert(H#?(tabloid T) and H#(tabloid T) == 1 and instance(first keys H, HookTabloid));
);
for T in {{{4,5,6},{7,1,2},{8}}, {{3,5,7},{8,2,4},{6},{9}}, {{1,2,3},{4,5,9},{7,8,6}},
    {{4,5,6},{1,2,3},{7,8,9}}, {{4,2,5,6},{1,7,8},{3,9,10},{11}}} do (
    H := hookStraighten T;
    assert(#H >= 1 and all(keys H, U -> instance(U, HookTabloid) and isHookSemistandard U and H#U != 0));
    assert(hookStraighten tabloid T === H and hookStraighten(new Filling from tabloid T) === H);
);
-- known answers: a signed exchange of two hooks, a single-hook relation in
-- rank three, and the fourteen terms of the paper's rank two example
H = hookStraighten {{1,2,3},{4,5,9},{7,8,6}};
assert(#H == 1 and H#(tabloid {{1,2,3},{4,5,6},{7,8,9}}) == -1);
H = hookStraighten {{4,2,5,6},{1,7,8},{3,9,10},{11}};
assert(#H == 1 and H#(tabloid {{1,2,5,6},{3,7,8},{4,9,10},{11}}) == 1);
H = hookStraighten {{4,5,6},{7,1,2},{8}};
assert(#H == 14 and H#(tabloid {{1,2,4},{5,6,7},{8}}) == 1 and H#(tabloid {{1,2,5},{4,6,7},{8}}) == 1/3);
assert(hookStraighten {{1,2,6},{4,3,5}} === hashTable {tabloid {{1,2,3},{4,5,6}} => -1/2,
    tabloid {{1,2,4},{3,5,6}} => -1/2, tabloid {{1,2,5},{3,4,6}} => -1_QQ,
    tabloid {{1,3,4},{2,5,6}} => 1/2, tabloid {{1,3,5},{2,4,6}} => 1_QQ});
-- zero tabloids and the empty tabloid
assert(#hookStraighten {{1,1,1},{1,1},{1}} == 0 and #hookStraighten {{1,1},{1,1}} == 0);
assert(#hookStraighten {{1,2,3,4},{1},{2},{3}} == 0);
assert(hookStraighten tabloid {} === hashTable {tabloid {} => 1_QQ});
-- idempotence: every term straightens to itself, and a normal form is its
-- own normal form as a linear combination
for U in keys H do assert(hookStraighten U === hashTable {U => 1_QQ});
assert(hookStraighten H === H);
-- determinism
assert(hookStraighten {{6,7,8},{1,4,5},{9,2,3}} === hookStraighten {{6,7,8},{1,4,5},{9,2,3}});
///

TEST ///
-- hookStraighten: a single hook given by its row tail and full column
-- agrees with the rank one straightening of hpStraightenHookHT, a linear
-- combination is straightened termwise, and malformed input is refused
debug needsPackage "HookPieri";
H = hookStraighten({5,6,7}, {2,1,3,4});
ht = hpStraightenHookHT({5,6,7}, {2,1,3,4});
assert(#H == #ht and all(keys ht, k -> H#(tabloid(k#0, k#1)) == ht#k));
assert(hookStraighten({5,6,7}, {2,1,3,4}) === hookStraighten {{2,5,6,7},{1},{3},{4}});
assert(hookStraighten({}, {3,2}) === hashTable {tabloid {{2},{3}} => -1_QQ});
-- linear combinations: keys in any form, values summed with cancellation
T = tabloid {{1,2,3},{2,1},{3}};
H = hookStraighten T;
C = hookStraighten hashTable {T => 2, tabloid {{1,2},{3}} => 1_QQ};
assert(#C == 3 and C#(tabloid {{1,2},{3}}) == 1 and all(keys H, U -> C#U == 2 * H#U));
assert(hookStraighten hashTable {{{1,2,3},{2,1},{3}} => 1, tabloid {{1,1,2},{2,3},{3}} => 1_QQ} ===
    hashTable {tabloid {{1,1,3},{2,2},{3}} => -1_QQ});
assert(#hookStraighten hashTable {} == 0);
-- errors: entries, shapes, values, strategies, fields
assert(try (hookStraighten {{0}}; false) else true);
assert(try (hookStraighten {{1/2}}; false) else true);
assert(try (hookStraighten {{1,2},{3,4,5}}; false) else true);
assert(try (hookStraighten {{1,2},{}}; false) else true);
assert(try (hookStraighten 5; false) else true);
assert(try (hookStraighten({}, {}); false) else true);
assert(try (hookStraighten hashTable {T => "x"}; false) else true);
assert(try (hookStraighten hashTable {5 => 1_QQ}; false) else true);
assert(try (hookStraighten(T, Strategy => "Fast"); false) else true);
assert(try (hookStraighten(T, CoefficientRing => ZZ); false) else true);
assert(try (hookStraighten(T, Progress => -1); false) else true);
///

TEST ///
-- hookStraighten: the Rank2 and Cokernel strategies agree with the
-- algorithm, Rank2 refuses other ranks, Progress prints without changing
-- the answer, and the private count form returns the table with the
-- number of rewrites
debug needsPackage "HookPieri";
for T in {{{4,5,6},{7,1,2},{8}}, {{3,4,7},{5,1,2},{6}}, {{4,6,9},{5,1,3},{7},{8}},
    {{3,5,7},{8,2,4},{6},{9}}, {{1,2,3},{2,1},{3}}} do (
    H := hookStraighten T;
    assert(hookStraighten(T, Strategy => "Rank2") === H);
    if #flatten T <= 8 then assert(hookStraighten(T, Strategy => "Cokernel") === H);
);
-- (the squarefree class of shape (3,3,3) has 6048 tabloids and is left to the
-- exhaustive harness; rank three here is the small class of {{1,1,1},{2,2,2},{3,3,3}})
for T in {{{2,2,3},{1,1,4},{3,3,4}}, {{2,1,3},{1,2},{3}}, {{1,1,2},{2,2,1},{3,3,3}},
    {{1,2,3},{4},{5}}, {{3,1},{2}}} do
    assert(hookStraighten(T, Strategy => "Cokernel") === hookStraighten T);
assert(hookStraighten(tabloid {}, Strategy => "Cokernel") === hashTable {tabloid {} => 1_QQ});
assert(try (hookStraighten({{1,2,3},{4,5,6},{7,8,9}}, Strategy => "Rank2"); false) else true);
assert(try (hookStraighten({{1,2,3},{4},{5}}, Strategy => "Rank2"); false) else true);
assert(hookStraighten({{4,5,6},{7,1,2},{8}}, Progress => 5) === hookStraighten {{4,5,6},{7,1,2},{8}});
assert(hookStraighten({{4,5,6},{7,1,2},{8}}, Progress => 5, Strategy => "Cokernel") ===
    hookStraighten {{4,5,6},{7,1,2},{8}});
-- the count form
(H, n) = hpHookStraightenCount {{4,5,6},{7,1,2},{8}};
assert(H === hookStraighten {{4,5,6},{7,1,2},{8}} and instance(n, ZZ) and n > 0);
(H1, n1) = hpHookStraightenCount {{1,2,3},{4},{5}};
assert(#H1 == 1 and n1 == 0);
(H2, n2) = hpHookStraightenCount({{4,5,6},{7,1,2},{8}}, Strategy => "Cokernel");
assert(H2 === H and n2 == 0);
assert(first hpHookStraightenCount(tabloid {{4,5,6},{7,1,2},{8}}, Strategy => "Rank2") === H);
-- the row keyed engine form is unchanged for the old sections
(raw, m) = hpHookStraightenFromAnyHooks("test", {{4,5,6},{7,1,2},{8}});
assert(#raw == 14 and all(keys raw, k -> instance(k, List) and H#(tabloid k) == raw#k) and m == n);
///

TEST ///
-- CoefficientRing: the straightening runs over ZZ/p and agrees with the
-- rational result mod p when p exceeds the size of the shape; below that
-- it may fail, and then it fails with an error and leaves the field
-- unchanged; the option is validated by every producer
debug needsPackage "HookPieri";
T = {{4,5,6},{7,1,2},{8}};
HQ = hookStraighten T;
H11 = hookStraighten(T, CoefficientRing => ZZ/11);
assert(#H11 == #HQ and all(keys HQ, U -> H11#?U and H11#U == sub(HQ#U, ZZ/11) and ring H11#U === ZZ/11));
assert(hookStraighten(T, CoefficientRing => ZZ/11, Strategy => "Cokernel") === H11);
assert(hookStraighten(T, CoefficientRing => ZZ/11, Strategy => "Rank2") === H11);
assert(hpField() === QQ);
assert(try (hookStraighten(T, CoefficientRing => ZZ/2); false) else true);
assert(hpField() === QQ);
assert(try (hookStraighten(T, CoefficientRing => ZZ/2, Strategy => "Cokernel"); false) else true);
assert(hpField() === QQ);
S11 = straighteningStep(T, CoefficientRing => ZZ/11);
SQ = straighteningStep T;
assert(#S11 == #SQ and all(keys SQ, U -> S11#U == sub(SQ#U, ZZ/11)));
assert((straighteningObstruction(T, CoefficientRing => ZZ/11))#"step" === S11);
assert(isStepTriangular(T, CoefficientRing => ZZ/11));
assert((verifyTriangularity(T, CoefficientRing => ZZ/11))#"passed");
assert((verifyTriangularity({3,2}, 3, CoefficientRing => ZZ/11))#"passed");
assert((verifyStraighteningAgainstCokernel({3,2}, 3, CoefficientRing => ZZ/11))#"passed");
assert(hpField() === QQ);
for f in {hookStraighten, straighteningStep, straighteningObstruction, isStepTriangular, verifyTriangularity} do
    assert(try (f(T, CoefficientRing => ZZ); false) else true);
assert(try (verifyTriangularity({3,2}, 3, CoefficientRing => RR); false) else true);
assert(try (verifyStraighteningAgainstCokernel({3,2}, 3, CoefficientRing => ZZ); false) else true);
assert(hpField() === QQ);
///

TEST ///
-- straighteningStep: one rewrite is a genuine identity (re-straightening
-- every term reproduces the normal form), its keys are HookTabloids, it
-- honors the Obstruction and EngineStep options, and it validates
needsPackage "SchurFunctors";
T = {{4,5,6},{7,1,2},{8}};
H = straighteningStep T;
assert(#H == 10 and all(keys H, U -> instance(U, HookTabloid)) and not any(keys H, isHookSemistandard));
acc = new MutableHashTable;
for U in keys H do (
    nf := hookStraighten U;
    for W in keys nf do (c := H#U * nf#W; if acc#?W then acc#W = acc#W + c else acc#W = c));
acc = new HashTable from select(pairs acc, kv -> kv#1 != 0);
assert(acc === hookStraighten T);
assert(straighteningStep tabloid T === H and straighteningStep(new Filling from tabloid T) === H);
-- semistandard input, zero input, the empty tabloid
assert(straighteningStep {{1,2,3},{2,3},{3}} === hashTable {tabloid {{1,2,3},{2,3},{3}} => 1_QQ});
assert(#straighteningStep {{1,2},{2,2}} == 0);
assert(straighteningStep tabloid {} === hashTable {tabloid {} => 1_QQ});
-- both obstructions present: the column relation has coefficients +-1/2
P = {{1,1,2,3},{2,1,1,1},{3,4}};
Hrow = straighteningStep P;
Hcol = straighteningStep(P, Obstruction => "column");
assert(sort values Hrow == sort {-1/2, -1/2, 1/6, 1/6});
assert(sort values Hcol == sort {-1/2, -1/2, 1/2, 1/2});
assert(set keys Hrow === set keys Hcol);
assert(straighteningStep(P, Obstruction => "row") === Hrow);
assert(try (straighteningStep({{1,3,4},{2,2,5},{3}}, Obstruction => "row"); false) else true);
-- the paper's Example 6.1.5 has both obstructions too; the default step is
-- the row step and is already the normal form
E = {{1,2,3},{2,1},{3}};
assert(straighteningStep(E, Obstruction => "row") === straighteningStep E);
assert(straighteningStep E === hookStraighten E);
assert(set keys straighteningStep(E, Obstruction => "column") === set keys straighteningStep E);
-- the introductory example needs further straightening after its first step
Tintro = tabloid {{1,2,2},{2,1,1},{3,3}};
assert(all(hookDecomposition Tintro, isHookSemistandard));
assert(not isHookSemistandard Tintro);
Hintro = straighteningStep Tintro;
assert(Hintro === hashTable {
    tabloid {{1,1,2},{2,1,2},{3,3}} => -2_QQ,
    tabloid {{1,1,2},{2,1,3},{3,2}} => 1_QQ});
assert(not any(keys Hintro, isHookSemistandard));
NFintro = hashTable {tabloid {{1,1,1},{2,2,2},{3,3}} => 1_QQ};
assert(hookStraighten Tintro === NFintro and hookStraighten Hintro === NFintro);
-- a hook that is not semistandard is rewritten by the single-hook relation
assert(straighteningStep {{2,1,3},{1,2},{3}} === hashTable {tabloid {{1,1,3},{2,2},{3}} => -1_QQ});
-- rank three: one pair is rewritten, and the engine step agrees
T3 = {{4,5,6},{1,2,3},{7,8,9}};
H3 = straighteningStep T3;
assert(#H3 == 1 and first values H3 == -1 and first keys H3 == tabloid {{1,5,6},{4,2,3},{7,8,9}});
assert(straighteningStep(T3, EngineStep => true) === H3);
-- errors
assert(try (straighteningStep {{1,2},{3,4,5}}; false) else true);
assert(try (straighteningStep({{4,5,6},{7,1,2},{8}}, Obstruction => "diagonal"); false) else true);
assert(try (straighteningStep({{4,5,6},{7,1,2},{8}}, EngineStep => 1); false) else true);
///

TEST ///
-- straighteningObstruction: one string keyed record with the paper's
-- names, the engine's obstruction record, the data of the step and the
-- step itself keyed by HookTabloids; rank two, higher rank, semistandard
-- input and the errors
D = straighteningObstruction {{1,1,2,3},{3,2,3,4},{4,3}};
assert(instance(D, HashTable) and all(keys D, k -> instance(k, String)));
assert(D#"obstructionType" == "row" and D#"L0" == {3,4} and D#"I0" == {3} and D#"R0" == {2});
assert(D#"movedColumnTail" == D#"L0" and D#"bumpedColumnSegment" == D#"I0" and D#"thirdFactorKind" == "wedge");
assert(D#"T1" == D#"leftHook" and D#"T2" == D#"middleHook" and D#"T3" == D#"thirdFactor");
assert(D#"pairIndex" == 0 and D#"input" == tabloid {{1,1,2,3},{3,2,3,4},{4,3}} and D#"subTabloid" == D#"input");
assert(D#"localObstructionData"#"obstructionType" == "row" and D#"localObstructionData"#"L0" == {3,4});
assert(D#"step" === straighteningStep {{1,1,2,3},{3,2,3,4},{4,3}} and D#"rewrite" === D#"step");
assert(all(keys D#"rawImage", U -> instance(U, HookTabloid)) and D#"rawImage"#?(D#"input"));
assert(D#"rawImage"#(D#"input") == D#"origCoeffInRawImage" and D#"normalizedImage"#(D#"input") == 1);
assert(instance(D#"internal", HashTable) and D#"internal"#"input" == {{1,1,2,3},{3,2,3,4},{4,3}});
-- the same for the tabloid and Filling forms
needsPackage "SchurFunctors";
assert((straighteningObstruction tabloid {{1,1,2,3},{3,2,3,4},{4,3}})#"step" === D#"step");
assert((straighteningObstruction(new Filling from tabloid {{1,1,2,3},{3,2,3,4},{4,3}}))#"step" === D#"step");
-- a column obstruction
C = straighteningObstruction {{1,3,4},{2,2,5},{3}};
assert(C#"obstructionType" == "column" and C#"tau0" == {3,4} and C#"sigma" == {5} and C#"rho" == {});
assert(C#"thirdFactorKind" == "row" and C#"pairIndex" == 0);
assert(C#"step" === straighteningStep {{1,3,4},{2,2,5},{3}});
-- the option selects the obstruction when both are present
P = {{1,1,2,3},{2,1,1,1},{3,4}};
assert((straighteningObstruction P)#"obstructionType" == "row");
assert((straighteningObstruction(P, Obstruction => "column"))#"obstructionType" == "column");
assert((straighteningObstruction(P, Obstruction => "column"))#"step" === straighteningStep(P, Obstruction => "column"));
-- rank three: the second pair is rewritten and the step is re-embedded
R3 = straighteningObstruction {{1,2,3},{4,5,9},{7,8,6}};
assert(R3#"pairIndex" == 1 and R3#"subTabloid" == tabloid {{5,9},{8,6}});
assert(R3#"step" === straighteningStep {{1,2,3},{4,5,9},{7,8,6}} and #(R3#"rewrite") == #(R3#"step"));
assert(R3#"rewrite"#?(tabloid {{5,6},{8,9}}) and R3#"step"#?(tabloid {{1,2,3},{4,5,6},{7,8,9}}));
assert(R3#"rewrite"#(tabloid {{5,6},{8,9}}) == R3#"step"#(tabloid {{1,2,3},{4,5,6},{7,8,9}}));
-- semistandard input
N = straighteningObstruction {{1,2,3},{2,3},{3}};
assert(N#"obstructionType" == "none" and N#"pairIndex" === null and N#"subTabloid" === null);
assert(N#"step" === hashTable {tabloid {{1,2,3},{2,3},{3}} => 1_QQ} and instance(N#"message", String));
-- errors: rank below two, a hook that is not semistandard, a missing
-- obstruction, bad options
assert(try (straighteningObstruction {{1,2,3},{4},{5}}; false) else true);
assert(try (straighteningObstruction {{2,1,3},{1,2},{3}}; false) else true);
assert(try (straighteningObstruction({{1,3,4},{2,2,5},{3}}, Obstruction => "row"); false) else true);
assert(try (straighteningObstruction(P, Obstruction => "diagonal"); false) else true);
///

TEST ///
-- verifyTriangularity and isStepTriangular: every step on every tabloid
-- of these shapes is strictly smaller in the running order and preserves
-- the content; the reports are VerificationReports with the documented
-- rows; the one tabloid form reports the terms with symbols
for pr in {({3,2},3), ({3,3},3), ({2,2,2},3)} do (
    R := verifyTriangularity(pr#0, pr#1);
    assert(instance(R, VerificationReport) and R#"kind" == "Triangularity");
    assert(R#"passed" and #(R#"violations") == 0 and R#"firstViolation" === null);
    assert(R#"incomparable" == 0 and R#"contentViolations" == 0 and R#"zeroSteps" >= 0);
    assert(R#"tabloids" == #hookTabloids(pr#0, pr#1));
    assert(R#"semistandard" + R#"steps" == R#"tabloids");
    assert(R#"restriction" == "alphabet" and R#"restrictionValue" == pr#1 and R#"shape" == pr#0);
    assert(R#"obstruction" == "Automatic" and not R#"engine" and instance(R#"seconds", RR));
    assert(instance(net R, Net) and instance(R#"conclusion", String));
);
R = verifyTriangularity({3,3,3}, {3,2,2,2});
assert(R#"passed" and R#"restriction" == "content" and R#"restrictionValue" == {3,2,2,2});
assert(R#"tabloids" == #hookTabloids({3,3,3}, {3,2,2,2}) and R#"tabloids" < #hookTabloids({3,3,3}, 4));
assert(R#"keyOrder" == {"shape", "restriction", "restrictionValue", "obstruction", "engine", "tabloids",
    "semistandard", "steps", "terms", "maxTerms", "zeroSteps", "incomparable", "contentViolations",
    "violations", "firstViolation", "seconds", "passed", "conclusion"});
E = verifyTriangularity({3,2}, 3, EngineStep => true);
assert(E#"passed" and E#"engine");
-- one tabloid
needsPackage "SchurFunctors";
S = verifyTriangularity {{1,3},{2,2}};
assert(instance(S, VerificationReport) and S#"kind" == "StepTriangularity" and S#"passed");
assert(S#"input" == tabloid {{1,3},{2,2}} and not S#"semistandard" and not S#"hookRelation");
assert(S#"step" === straighteningStep {{1,3},{2,2}} and S#"contentPreserved" and #(S#"failures") == 0);
assert(#(S#"terms") == 1 and (S#"terms")#0#2 === symbol < and instance((S#"terms")#0#1, HookTabloid));
assert((S#"terms")#0#0 == (S#"step")#((S#"terms")#0#1));
assert(S#"keyOrder" == {"input", "semistandard", "hookRelation", "step", "terms", "failures",
    "contentPreserved", "passed", "conclusion"});
assert(verifyTriangularity tabloid {{1,3},{2,2}} === S and verifyTriangularity(new Filling from tabloid {{1,3},{2,2}}) === S);
-- the single-hook relation leaves the running vectors unchanged and is
-- counted as triangular
T3 = {{4,5,6},{1,2,3},{7,8,9}};
S3 = verifyTriangularity T3;
assert(S3#"hookRelation" and S3#"passed" and (S3#"terms")#0#2 === symbol == and #(S3#"failures") == 1);
assert(isStepTriangular T3 and isStepTriangular tabloid T3);
assert(isStepTriangular {{1,5,6},{4,2,3},{7,8,9}} and not (verifyTriangularity {{1,5,6},{4,2,3},{7,8,9}})#"hookRelation");
-- semistandard input: the identity step
V = verifyTriangularity {{1,2,3},{2,3},{3}};
assert(V#"semistandard" and V#"passed" and #(V#"terms") == 0);
assert(isStepTriangular {{1,2,3},{2,3},{3}});
-- the paper's example with both obstructions, both ways
P = {{1,1,2,3},{2,1,1,1},{3,4}};
assert(isStepTriangular P and isStepTriangular(P, Obstruction => "column") and isStepTriangular(P, EngineStep => true));
-- every tabloid of these shapes: the step and the full normal form are
-- below the input in the running order
for T in hookTabloids({3,3}, 3) do if not isHookSemistandard T then (
    assert(isStepTriangular T);
    assert(all(keys hookStraighten T, U -> runningCompare(U, T) === symbol <));
);
-- errors
assert(try (verifyTriangularity({3,2}, 0); false) else true);
assert(try (verifyTriangularity({2,3}, 3); false) else true);
assert(try (verifyTriangularity({3,2}, {2,2}); false) else true);
assert(try (verifyTriangularity({3,2}, 3, Obstruction => "x"); false) else true);
assert(try (verifyTriangularity({3,2}, 3, EngineStep => 0); false) else true);
assert(try (verifyTriangularity {{1,2},{3,4,5}}; false) else true);
assert(try (isStepTriangular {{0}}; false) else true);
///

TEST ///
-- verifyStraighteningAgainstCokernel: the cokernel normal form (plain
-- linear algebra on the Pieri relations) agrees with the algorithm on
-- whole shapes, in ranks two and three, including a shape with arm length
-- four; the report is a VerificationReport with the documented rows
for pr in {({3,2},3), ({2,2,2},3), ({3,3,2},4), ({5,4},3)} do (
    R := verifyStraighteningAgainstCokernel(pr#0, pr#1);
    assert(instance(R, VerificationReport) and R#"kind" == "Cokernel");
    assert(R#"passed" and #(R#"disagreements") == 0 and R#"tabloids" == #hookTabloids(pr#0, pr#1));
    assert(R#"agreements" == R#"tabloids" and R#"relations" > 0);
    assert(R#"restriction" == "alphabet" and R#"restrictionValue" == pr#1 and R#"shape" == pr#0);
    assert(instance(net R, Net));
);
R = verifyStraighteningAgainstCokernel({3,3,3}, {3,2,2,2});
assert(R#"passed" and R#"restriction" == "content" and R#"tabloids" == #hookTabloids({3,3,3}, {3,2,2,2}));
assert(R#"keyOrder" == {"shape", "restriction", "restrictionValue", "tabloids", "relations",
    "agreements", "disagreements", "seconds", "passed", "conclusion"});
-- the memoized relation data is shared by the Cokernel strategy
debug HookPieri;
assert((hpFieldCache(hpCokernelDataCache, QQ))#?({3,3,3}, {3,2,2,2}));
T = tabloid {{2,2,3},{1,1,4},{3,3,4}};
assert(hookStraighten(T, Strategy => "Cokernel") === hookStraighten T);
-- errors
assert(try (verifyStraighteningAgainstCokernel({3,2}, 0); false) else true);
assert(try (verifyStraighteningAgainstCokernel({3,2}, {2,2}); false) else true);
assert(try (verifyStraighteningAgainstCokernel({}, 3); false) else true);
///

TEST ///
-- isHookSemistandard and frobeniusRank on known tabloids and partitions,
-- in the three input forms; malformed input is not semistandard rather
-- than an error, and frobeniusRank validates its partition
needsPackage "SchurFunctors";
assert(isHookSemistandard {{1,3},{2,4}} and not isHookSemistandard {{1,3},{2,2}});
assert(isHookSemistandard {{1,2,3},{4,5,6},{7,8,9}});
assert(not isHookSemistandard {{4,2,5,6},{1,7,8},{3,9,10},{11}});
assert(isHookSemistandard {{1,2,5,6},{3,7,8},{4,9,10},{11}});
assert(isHookSemistandard {{1,2,3},{4},{5}} and not isHookSemistandard {{3,1},{2}});
assert(isHookSemistandard {{1,1,2},{2,3}} and not isHookSemistandard {{1,2,3},{2,1},{3}});
assert(isHookSemistandard tabloid {{1,3,5},{2,4},{6}} and isHookSemistandard(new Filling from tabloid {{1,3,5},{2,4},{6}}));
assert(not isHookSemistandard tabloid {{4,5,6},{7,1,2},{8}});
assert(isHookSemistandard {} and isHookSemistandard tabloid {});
-- malformed input
assert(not isHookSemistandard {{1,2},{3,4,5}});
assert(not isHookSemistandard {{1,2},{}});
assert(not isHookSemistandard {{0,1}});
assert(not isHookSemistandard {{1/2}});
assert(not isHookSemistandard {1,2});
assert(not isHookSemistandard 5 and not isHookSemistandard "x" and not isHookSemistandard null);
assert(not isHookSemistandard(new Filling from {{1,2},{3,4,5}}));
-- the semistandard tabloids of a shape are exactly the terms of normal forms
L = hookTabloids({3,2}, 3);
assert(#select(L, isHookSemistandard) == 15);
assert(all(L, T -> all(keys hookStraighten T, isHookSemistandard)));
-- frobeniusRank
assert(frobeniusRank {4,3,2} == 2 and frobeniusRank {3,3,3} == 3 and frobeniusRank {4,1,1,1} == 1);
assert(apply({{1},{2,1},{3,1,1},{2,2},{3,2,1},{3,3,2},{3,3,3}}, frobeniusRank) == {1,1,1,2,2,2,3});
assert(frobeniusRank {} == 0);
assert(frobeniusRank tabloid {{1,2,3},{2,1},{3}} == 2 and frobeniusRank tabloid {} == 0);
assert(frobeniusRank(new Filling from tabloid {{1,2,3},{4,5,6},{7,8,9}}) == 3);
assert(try (frobeniusRank {2,3}; false) else true);
assert(try (frobeniusRank {2,0}; false) else true);
assert(try (frobeniusRank {1/2}; false) else true);
///

TEST ///
-- hookTabloids: the enumerators return HookTabloids whose diagonal hooks
-- are semistandard, with the known counts, and validate their arguments
L = hookTabloids({3,2}, 3);
assert(#L == 45 and all(L, T -> instance(T, HookTabloid)));
assert(#unique L == 45);
debug needsPackage "HookPieri";
assert(all(L, T -> (rows := toList T; lambda := apply(rows, r -> #r);
    all(hpFrobeniusRank lambda, k -> hpIsCanonicalHook toList hpHookOfTabloid(rows, lambda, k)))));
assert(all(L, T -> apply(toList T, r -> #r) == {3,2} and max flatten toList T <= 3));
C = hookTabloids({3,2}, {2,2,1});
assert(#C == 5 and all(C, T -> sort flatten toList T == {1,1,2,2,3}));
assert(#hookTabloids({3,2}, {1,1,1,1,1}) == 15);
assert(#hookTabloids({3,3,3}, 4) == 2880);
assert(#hookTabloids({2,2}, {1,1,1,1}) == 8);
assert(hookTabloids({1}, 1) == {tabloid {{1}}});
assert(try (hookTabloids({3,2}, {2,2}); false) else true);
assert(try (hookTabloids({3,2}, 0); false) else true);
assert(try (hookTabloids({2,3}, 3); false) else true);
assert(try (hookTabloids({}, 3); false) else true);
///

TEST ///
-- the running order: runningVectors agrees with the region definition of
-- Definition 3.2.3, runningIndices lists the indices in the order of the
-- vectors, runningCompare returns the four symbols and explains itself
-- with Verbose, and the private code form is kept for the old engines
debug needsPackage "HookPieri";
needsPackage "SchurFunctors";
assert(runningVectors {{1,1,2},{2,3}} == {{1,2,2},{2,3,3},{2,4,4},{2,4,5}});
assert(runningVectors {{1,1,3},{2,2}} == {{1,2,2},{2,3,3},{2,3,4},{2,4,5}});
assert(runningVectors({{1,1,2},{2,3}}, 3) == runningVectors {{1,1,2},{2,3}});
assert(runningVectors({{1,1,2},{2,3}}, 4) == {{1,2,2,2},{2,3,3,3},{2,4,4,4},{2,4,5,5}});
assert(runningVectors tabloid {{1,1,2},{2,3}} == runningVectors {{1,1,2},{2,3}});
assert(runningVectors(new Filling from tabloid {{1,1,2},{2,3}}, 3) == runningVectors {{1,1,2},{2,3}});
assert(runningVectors tabloid {} == {});
for T in {{{1,2,3},{2,1},{3}}, {{4,5,6},{1,2,3},{7,8,9}}, {{1,1,2,3},{2,1,1,1},{3,4}}, {{3,5,7},{8,2,4},{6},{9}}} do (
    N := max flatten T;
    lambda := apply(T, r -> #r);
    idxs := runningIndices lambda;
    assert(idxs == hpRunningIndexPairs lambda);
    assert(#runningVectors T == #idxs);
    assert(runningVectors(T, N) == apply(idxs, q -> hpRunningCountVector(T, q#0, q#1, N)));
);
assert(runningIndices {3,2} == {{1,0},{1,1},{1,2},{2,0}});
assert(runningIndices {4,4,2} == {{1,0},{1,1},{1,2},{1,3},{2,0},{2,1},{2,2}});
assert(runningIndices tabloid {{1,2,3},{2,1},{3}} == runningIndices {3,2});
assert(runningIndices(new Filling from tabloid {{1,2,3},{2,1},{3}}) == runningIndices {3,2});
assert(runningIndices {1} == {{1,0}});
-- the comparison
assert(runningCompare({{1,1,2},{2,3}}, {{1,1,3},{2,2}}) === symbol <);
assert(runningCompare({{1,1,3},{2,2}}, {{1,1,2},{2,3}}) === symbol >);
assert(runningCompare({{1,1,3},{2,2}}, {{1,1,3},{2,2}}) === symbol ==);
assert(runningCompare(tabloid {{1,1,2},{2,3}}, new Filling from tabloid {{1,1,3},{2,2}}) === symbol <);
assert(runningCompare(tabloid {}, tabloid {}) === symbol ==);
assert(runningCompare({{1,1,2},{2,3}}, {{1,1,3},{2,2}}, Verbose => true) === symbol <);
assert(runningCompare({{1,1,2},{2,3}}, {{1,1,2},{2,3}}, Verbose => true) === symbol ==);
L = hookTabloids({3,2}, 3);
P = select(subsets(L, 2), p -> runningCompare(p#0, p#1) === incomparable);
assert(#P > 0 and all(P, p -> runningCompare(p#1, p#0) === incomparable));
assert(runningCompare(P#0#0, P#0#1, Verbose => true) === incomparable);
-- antisymmetry on every pair of the shape
for p in subsets(L, 2) do (
    c := runningCompare(p#0, p#1); d := runningCompare(p#1, p#0);
    assert((c === symbol < and d === symbol >) or (c === symbol > and d === symbol <) or
        (c === symbol == and d === symbol ==) or (c === incomparable and d === incomparable)));
-- the terms of a normal form are below the input
T = tabloid {{4,5,6},{7,1,2},{8}};
assert(all(keys hookStraighten T, U -> runningCompare(U, T) === symbol <));
-- errors
assert(try (runningCompare({{1,2},{3}}, {{1,2,3}}); false) else true);
assert(try (runningCompare({{1,2},{3}}, {{1,2},{3}}, Verbose => 1); false) else true);
assert(try (runningVectors({{1,2},{3}}, 2); false) else true);
assert(try (runningVectors({{1,2},{3}}, -1); false) else true);
assert(try (runningIndices {2,3}; false) else true);
assert(try (runningVectors {{1,2},{3,4,5}}; false) else true);
-- the private code form used by the old engines
assert(hpRunningPreorderCompare({{1,1,2},{2,3}}, {{1,1,3},{2,2}}) == -1);
assert(hpRunningPreorderCompare({{1,1,3},{2,2}}, {{1,1,2},{2,3}}) == 1);
assert(hpRunningPreorderCompare({{1,1,3},{2,2}}, {{1,1,3},{2,2}}) == 0);
assert(hpRunningPreorderCompare(toList P#0#0, toList P#0#1) === null);
assert(all(values hpCarefulStep {{1,1,3},{2,1}}, c -> c == -1));
///

TEST ///
-- the export list: the new names are exported, the retired ones are not,
-- and the option symbols of this section are exported
exported = apply(HookPieri#"exported symbols", toString);
for n in {"hookStraighten", "straighteningStep", "straighteningObstruction", "isStepTriangular",
    "verifyTriangularity", "verifyStraighteningAgainstCokernel", "isHookSemistandard", "frobeniusRank",
    "hookTabloids", "runningVectors", "runningIndices", "runningCompare", "Progress", "Obstruction",
    "EngineStep"} do assert(member(n, exported));
for n in {"hookStraightenFromAnyHooks", "straightenRank2", "straightenRank2Fast",
    "straightenRank2FromAnyHooks", "isSemistandardRank2", "hookCoefficient", "toRows",
    "stepTriangularity", "hookObstructionData", "straighteningStepData", "straighteningStepTable",
    "cokernelNormalForm", "cokernelNormalForms", "squarefreeHookTabloids", "runningData",
    "runningPreorderCompare", "runningTable", "explainRunningComparison", "hookNormalForm",
    "hookNormalFormTable", "hookNormalFormTabloids", "hookTabloidForm", "hookFromTableau"} do
    assert(not member(n, exported));
///

TEST ///
-- hookRing: the ring, its context, its variables and the two constructor
-- forms; the option validation
R = hookRing(2, {{0,0},{1,1}});
assert(instance(R, PolynomialRing) and numgens R == 4);
assert(R.cache#?"hookContext");
H = R.cache#"hookContext";
assert(H#"kind" == "FiniteHookRetract" and H#"numberOfVariables" == 4);
assert(H#"hookTypes" == {{1,1},{0,0}});
assert(coefficientRing R === QQ);
assert(toString R_0 == "z0" and toString R_3 == "z3");
assert(H#"ring" === R);
assert(numgens hookRing(2, MaxArm => 1, MaxLeg => 1) == 2 + 3 + 1 + 2);
assert(numgens hookRing(3, {{0,0},{1,1},{2,2}}) == 17);
assert(numgens hookRing(2, {{1,1}}, MonomialOrder => "GRevLex") == 2);
assert(char coefficientRing hookRing(2, {{0,0},{1,1}}, CoefficientRing => ZZ/7) == 7);
assert(numgens hookRing(3, {{0,0},{1,1}}, Weight => {1,1,1}) == 3 + 2);
-- errors
assert(try (hookRing(2, {{0,0}}, MaxArm => 1, MaxLeg => 1); false) else true);
assert(try (hookRing(2, MaxArm => 1); false) else true);
assert(try (hookRing(2, MaxArm => 1, MaxLeg => 2); false) else true);
assert(try (hookRing(2, {{0,0},{0,0}}); false) else true);
assert(try (hookRing(2, {{0,2}}); false) else true);
assert(try (hookRing(2, {}); false) else true);
assert(try (hookRing(0, {{0,0}}); false) else true);
assert(try (hookRing(2, {{0,0}}, CoefficientRing => RR); false) else true);
assert(try (hookRing(2, {{0,0}}, CoefficientRing => ZZ); false) else true);
assert(try (hookRing(2, {{0,0},{1,1}}, Weight => {1,1,1}); false) else true);
assert(try (hookRing(2, {{0,0},{1,1}}, MonomialOrder => "Lex"); false) else true);
assert(try (hookRing(3, {{0,0},{1,1},{2,2}}, VariableLimit => 10); false) else true);
assert(numgens hookRing(3, {{0,0},{1,1},{2,2}}, VariableLimit => 10, Force => true) == 17);
///

TEST ///
-- straighteningIdeal and pieriIdeal: the smallest nontrivial hook ring
-- has nine relations of each kind, the two ideals agree, the ideals are
-- memoized and carry their context and presentation
debug needsPackage "HookPieri";
R = hookRing(2, {{0,0},{1,1}});
I = straighteningIdeal R;
assert(instance(I, Ideal) and ring I === R and numgens I == 9);
assert(straighteningIdeal R === I);
assert(I.cache#"hookContext" === R.cache#"hookContext");
S = I.cache#"presentation";
assert(S#"kind" == "HookStraighteningPresentation" and S#"numberOfRelations" == 9);
assert(S#"allRelationsQuadratic" and S#"allLeadingTermsCorrect");
assert(all(S#"relationRecords", r -> member(r#"kind", {"nonchain", "straightening"})));
assert(#select(S#"relationRecords", r -> r#"kind" == "straightening") == 3);
assert(#select(S#"relationRecords", r -> r#"kind" == "nonchain") == 6);
P = pieriIdeal R;
assert(instance(P, Ideal) and numgens P == 9 and pieriIdeal R === P);
assert((P.cache#"presentation")#"kind" == "HookPieriPresentation");
assert(P == I);
assert(all(degrees I, d -> d == {2}) and all(degrees P, d -> d == {2}));
-- the quotient ring forms read the ideals of the ambient ring
A = R/I;
assert(straighteningIdeal A === I and pieriIdeal A === P);
-- Example 1.2.2 of the paper: 17 generators, 136 relations, the same ideal
-- from the Pieri maps
R3 = hookRing(3, {{0,0},{1,1},{2,2}});
I3 = straighteningIdeal R3;
assert(numgens I3 == 136);
assert(pieriIdeal R3 == I3);
-- guards and validation
assert(try (straighteningIdeal(hookRing(3,{{0,0},{1,0},{0,1},{1,1}}), RelationLimit => 1); false) else true);
assert(match("RelationLimit", hpLastError));
assert(try (straighteningIdeal(hookRing(2, {{0,0},{1,1}}), RelationLimit => -1); false) else true);
assert(try (straighteningIdeal(QQ[x]); false) else true);
///

TEST ///
-- nonstandardMonomialIdeal is the initial ideal of the straightening
-- ideal, weightTruncationIdeal is zero without a bound, and both are
-- memoized monomial ideals
R = hookRing(2, {{0,0},{1,1}});
J = nonstandardMonomialIdeal R;
assert(instance(J, MonomialIdeal) and nonstandardMonomialIdeal R === J);
assert(numgens J == 9);
assert(J == monomialIdeal leadTerm gb straighteningIdeal R);
assert(nonstandardMonomialIdeal(R/straighteningIdeal R) === J);
K = weightTruncationIdeal R;
assert(instance(K, MonomialIdeal) and K == monomialIdeal(0_R));
-- the squarefree bound on four letters: 12 variables, 66 quadratic
-- truncation monomials in the cutoff, the standard monomials count the
-- Hilbert function 1, 12, 2
Rw = hookRing(4, {{0,0},{1,1}}, Weight => {1,1,1,1});
assert(numgens Rw == 12);
Kw = weightTruncationIdeal Rw;
assert(instance(Kw, MonomialIdeal) and all(flatten entries gens Kw, m -> sum degree m == 2));
Iw = straighteningIdeal Rw;
assert((Iw.cache#"presentation")#"numberOfWeightRelations" == 64);
assert(apply(toList(0..4), d -> hilbertFunction(d, Rw/Iw)) == {1,12,2,0,0});
assert(isSubset(Kw, Iw));
-- the bound (3,3,2) on the three diagonal types has one cubic boundary relation
Rc = hookRing(3, {{0,0},{1,1},{2,2}}, Weight => {3,3,2});
Ic = straighteningIdeal Rc;
recs = (Ic.cache#"presentation")#"weightRelationRecords";
assert(#select(recs, r -> r#"degree" == 3) == 1);
assert(not (Ic.cache#"presentation")#"allRelationsQuadratic");
assert(#select(flatten entries gens nonstandardMonomialIdeal Rc, m -> sum degree m == 3) == 1);
///

TEST ///
-- hasQuadraticGroebnerBasis: true for the paper order, with a cached
-- certificate; the Pieri ideal has the same certificate; a GRevLex ring
-- fails; a weight bound with a cubic boundary relation fails
R = hookRing(2, {{0,0},{1,1}});
I = straighteningIdeal R;
assert(hasQuadraticGroebnerBasis I);
assert(I.cache#?"groebnerCertificate");
C = I.cache#"groebnerCertificate";
assert(C#"passed" and C#"suppliedRelationsAreQuadraticGroebnerBasis" and C#"computedInitialIdealEqualsBadIdeal");
assert(C#"numberOfComputedBasisElements" == 9);
assert(hasQuadraticGroebnerBasis R and hasQuadraticGroebnerBasis(R/I));
assert(hasQuadraticGroebnerBasis pieriIdeal R);
A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
assert(hasQuadraticGroebnerBasis A);
assert(hasQuadraticGroebnerBasis hookAlgebra(4, {{0,0},{1,1}}, Weight => {1,1,1,1}));
Ac = hookAlgebra(3, {{0,0},{1,1},{2,2}}, Weight => {3,3,2});
assert(not hasQuadraticGroebnerBasis Ac);
assert((ideal Ac).cache#"groebnerCertificate"#"suppliedRelationsAreGroebnerBasis");
Rg = hookRing(3, MaxArm => 2, MaxLeg => 2, MonomialOrder => "GRevLex");
assert(not hasQuadraticGroebnerBasis straighteningIdeal(Rg, Validate => false));
-- an ideal of an unrelated ring is refused
assert(try (hasQuadraticGroebnerBasis ideal(0_(QQ[x])); false) else true);
///

TEST ///
-- paperOrder and verifyPaperOrder: the rows of the compiled order are
-- the weight rows of the monomial order, and the order agrees with the
-- running order on every strict comparison (1557 of them in Example 1.2.2)
R = hookRing(2, {{0,0},{1,1}});
rows = paperOrder R;
assert(instance(rows, List) and #rows > 0);
assert(all(rows, r -> instance(r, List) and #r == 2 and instance(r#0, String) and #(r#1) == numgens R));
assert(rows#0#0 == "boxes" and rows#0#1 == {3,3,1,1});
assert(rows#1#0 == "type {1, 1}" and rows#2#0 == "type {0, 0}");
weights = select(toList (options R).MonomialOrder, o -> o#0 === Weights);
assert(apply(weights, o -> o#1) == apply(rows, last));
assert(paperOrder(R/straighteningIdeal R) == rows);
assert(paperOrder hookRing(2, {{1,1}}, MonomialOrder => "GRevLex") == {});
V = verifyPaperOrder R;
assert(instance(V, VerificationReport) and V#"kind" == "PaperOrder" and V#"passed");
assert(V#"mismatches" == 0 and V#"firstMismatch" === null);
assert(V#"strictComparisons" + V#"paperTies" > 0);
assert(instance(net V, Net));
V3 = verifyPaperOrder hookAlgebra(3, {{0,0},{1,1},{2,2}});
assert(V3#"passed" and V3#"strictComparisons" == 1557 and V3#"mismatches" == 0);
assert(try (verifyPaperOrder(R, ComparisonLimit => 1); false) else true);
assert((verifyPaperOrder(R, ComparisonLimit => 1, Force => true))#"passed");
assert(try (verifyPaperOrder hookRing(2, {{1,1}}, MonomialOrder => "GRevLex"); false) else true);
///

TEST ///
-- positive characteristic: the presentation over ZZ/11 agrees with the
-- rational one, the field is read from the ring, and a small
-- characteristic fails naming the pair of generators and leaves the
-- current field unchanged
debug needsPackage "HookPieri";
A11 = hookAlgebra(3, {{0,0},{1,1},{2,2}}, CoefficientRing => ZZ/11);
assert(char coefficientRing A11 == 11);
assert(apply(toList(0..3), d -> hilbertFunction(d, A11)) == {1,17,17,1});
assert(hasQuadraticGroebnerBasis A11);
assert(hpField() === QQ);
assert(try (hookAlgebra(3, {{0,0},{1,1},{2,2}}, CoefficientRing => ZZ/3); false) else true);
assert(match("generators", hpLastError) and match("characteristic 3", hpLastError));
assert(hpField() === QQ);
assert(try (hookAlgebra(3, {{0,0},{1,1},{2,2}}, CoefficientRing => ZZ/2); false) else true);
assert(hpField() === QQ);
///

TEST ///
-- fineSegreEstimate and fineSegreRing: the counts, the ring with its
-- context, the generators as pairs of tabloids, the factors and the
-- validation of the type pairs
R = hookRing(2, {{0,0},{1,1}});
pairs2 = {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}};
E = fineSegreEstimate(R, R, pairs2);
assert(instance(E, Tally) and sum values E == 8 and #E == 2);
assert(E#{{1,1},{1,1},{1,1}} == 4 and E#{{0,0},{0,0},{0,0}} == 4);
W = fineSegreRing(R, R, TypePairs => pairs2);
assert(instance(W, PolynomialRing) and numgens W == 8);
assert(W.cache#?"hookContext" and (W.cache#"hookContext")#"kind" == "FineSegreRetract");
assert(toString W_0 == "w0" and degreeLength W == 2);
assert(hookTypes W == {{{1,1},{1,1},{1,1}},{{0,0},{0,0},{0,0}}});
assert(factors W === (R, R));
assert(weightBound W == {null, null});
B = hookBasis W;
assert(#B == 8 and all(B, p -> instance(p, List) and #p == 2 and all(p, T -> instance(T, HookTabloid))));
assert(#hookBasis(W, {1,1}) == 4 and #hookBasis(W, {0,0}) == 4);
assert(tabloid W_0 == B#0 and tabloid W_7 == B#7);
assert(hookVariable(W, B#3) == W_3 and hookVariable(W, tabloid W_5) == W_5);
assert(hookMonomial(W, B#6) == W_6);
-- the paper order rows of the block order carry the labels of the factors
rows = paperOrder W;
assert(rows#0#0 == "boxes" and rows#1#0 == "fine degree {1, 1}" and rows#2#0 == "fine degree {0, 0}");
assert(#select(rows, r -> match("^left ", r#0)) == #paperOrder R);
assert(#select(rows, r -> match("^right ", r#0)) == #paperOrder R);
-- errors
assert(try (alphabetSize W; false) else true);
assert(try (fineSegreRing(R, R); false) else true);
assert(try (fineSegreRing(R, R, TypePairs => {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{0,0}}}); false) else true);
assert(try (fineSegreRing(R, R, TypePairs => {{{0,0},{1,1},{0,0}},{{1,1},{0,0},{1,1}}}); false) else true);
assert(try (fineSegreRing(R, R, TypePairs => {{{0,0},{2,2},{0,0}}}); false) else true);
assert(try (fineSegreRing(R, R, TypePairs => pairs2, VariableLimit => 4); false) else true);
assert(numgens fineSegreRing(R, R, TypePairs => pairs2, VariableLimit => 4, Force => true) == 8);
assert(try (fineSegreRing(R, hookRing(2, {{0,0},{1,1}}, CoefficientRing => ZZ/7), TypePairs => pairs2); false) else true);
assert(try (fineSegreRing(R, R, TypePairs => pairs2, LeftWeight => {1,1}); false) else true);
assert(try (fineSegreRing(R, R, TypePairs => pairs2, RightWeight => {1,1}); false) else true);
-- the weights of the generic form restrict the factors: LeftWeight the left
-- factor, RightWeight the right one
assert(weightBound fineSegreRing(2, 3, 2, LeftWeight => {2,1}) == {{2,1}, null});
assert(weightBound fineSegreRing(2, 3, 2, RightWeight => {1,1,1}) == {null, {1,1,1}});
assert(try (factors R; false) else true);
assert(try (pieriIdeal W; false) else true);
///

TEST ///
-- the fine Segre presentation: 35 relations, 3 of them with a nonzero
-- rewrite, one standard pair, a quadratic Groebner basis, and equality
-- with the kernel of the map to the tensor product of the Pieri quotients
R = hookRing(2, {{0,0},{1,1}});
pairs2 = {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}};
W = fineSegreRing(R, R, TypePairs => pairs2);
I = straighteningIdeal W;
assert(instance(I, Ideal) and ring I === W and numgens I == 35 and straighteningIdeal W === I);
S = I.cache#"presentation";
assert(S#"kind" == "FineSegreStraighteningPresentation" and S#"numberOfNonzeroRewrites" == 3);
assert(S#"allRelationsQuadratic" and S#"allLeadingTermsCorrect");
kinds = tally flatten for i from 0 to 7 list for j from i to 7 list relationKind(W, i, j);
assert(kinds#"standard" == 1 and sum values kinds == 36);
assert(hasQuadraticGroebnerBasis I and hasQuadraticGroebnerBasis W);
assert(nonstandardMonomialIdeal W == monomialIdeal leadTerm gb I);
g = fineSegreMap(W, R/pieriIdeal R, R/pieriIdeal R);
assert(instance(g, RingMap) and source g === W);
assert(ker g == I);
-- the classical Segre relations from the plain factor rings
assert(numgens ker fineSegreMap(W, R, R) == 2);
assert(try (fineSegreMap(W, QQ[x], R); false) else true);
assert(try (fineSegreMap(R, R, R); false) else true);
-- the algebra and its accessors
B = fineSegreAlgebra W;
assert(instance(B, QuotientRing) and ideal B === I and fineSegreAlgebra W === B);
assert(factors B === (R, R) and hookTypes B == hookTypes W);
assert(straighteningIdeal B === I and hasQuadraticGroebnerBasis B);
B2 = fineSegreAlgebra(R, R, TypePairs => pairs2);
assert(instance(B2, QuotientRing) and numgens B2 == 8);
assert(try (fineSegreAlgebra R; false) else true);
///

TEST ///
-- hookRelation and tabloids on a fine Segre ring: a stored relation, its
-- tabloid form as pairs, and a standard pair
R = hookRing(2, {{0,0},{1,1}});
W = fineSegreRing(R, R, TypePairs => {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}});
I = straighteningIdeal W;
standard = first select(flatten for i from 0 to 7 list for j from i to 7 list {i,j}, p -> relationKind(W, p#0, p#1) == "standard");
assert(hookRelation(W, standard#0, standard#1) == 0);
assert(hookRelation(W, 0, 0) == W_0^2 and relationKind(W, 0, 0) == "zero:base-nonchain");
rewritten = first select((I.cache#"presentation")#"relationRecords", r -> r#"numberOfTermsInNormalForm" > 0);
ij = rewritten#"indices";
f = hookRelation(W, ij#0, ij#1);
assert(f != 0 and f == rewritten#"relation");
assert(member(relationKind(W, ij#0, ij#1), {"bad:left", "bad:right", "bad:both"}));
NF = tabloidTerms(W_(ij#0)*W_(ij#1) - f);
assert(instance(NF, HashTable) and #NF > 0);
assert(all(keys NF, p -> instance(p, List) and #p == 2 and all(p, T -> instance(T, HookTabloid))));
T = tabloid(W_(ij#0)*W_(ij#1));
assert(instance(T, List) and #T == 2);
assert(hookMonomial(W, T) == W_(ij#0)*W_(ij#1));
assert(try (tabloid(W_0*W_0); false) else true);
assert(try (hookRelation(W, 0, 8); false) else true);
///

TEST ///
-- the generic fine Segre model and independent weight bounds on the two
-- factors: 5 variables, 10 weight relations, a quadratic Groebner basis
-- and equality with the Pieri kernel
G = fineSegreRing(2, 3, 2);
assert(numgens G == 5);
assert(hookTypes G == {{{1,0},{1,1},{0,2}},{{0,0},{0,1},{0,1}}});
(L, Rt) = factors G;
assert(alphabetSize L == 2 and alphabetSize Rt == 3);
assert(hookTypes L == {{1,1},{0,1}} and hookTypes Rt == {{0,2},{0,1}});
assert(hasQuadraticGroebnerBasis straighteningIdeal G);
G7 = fineSegreRing(2, 3, 2, CoefficientRing => ZZ/7);
assert(char coefficientRing G7 == 7 and char coefficientRing first factors G7 == 7);
assert(try (fineSegreRing(2, 3, 4); false) else true);
assert(try (fineSegreRing(2, 3, 2, TypePairs => {{{0,0},{0,0},{0,0}}}); false) else true);
Bg = fineSegreAlgebra(2, 3, 2);
assert(instance(Bg, QuotientRing) and numgens Bg == 5);
RL = hookRing(2, {{0,0},{1,1}}, Weight => {2,1});
RRight = hookRing(2, {{0,0},{1,1}}, Weight => {1,2});
Ww = fineSegreRing(RL, RRight, TypePairs => {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}});
assert(numgens Ww == 5 and weightBound Ww == {{2,1},{1,2}});
Iw = straighteningIdeal Ww;
assert((Iw.cache#"presentation")#"numberOfWeightRelations" == 10);
assert(hasQuadraticGroebnerBasis Iw);
assert(ker fineSegreMap(Ww, RL/pieriIdeal RL, RRight/pieriIdeal RRight) == Iw);
assert(weightTruncationIdeal Ww != monomialIdeal(0_Ww));
///

TEST ///
-- hookAlgebra: a plain QuotientRing with Core's accessors, the three
-- constructor forms, the same object on repeated calls, and the context
-- reachable from the ambient ring
A = hookAlgebra(2, {{0,0},{1,1}});
assert(instance(A, QuotientRing) and class A === QuotientRing);
R = ambient A;
assert(instance(R, PolynomialRing) and R.cache#?"hookContext");
assert(ideal A === straighteningIdeal R and numgens A == 4 and numgens ideal A == 9);
assert(hookAlgebra R === A and hookAlgebra(2, MaxArm => 0, MaxLeg => 0) =!= A);
assert(numgens hookAlgebra(2, MaxArm => 1, MaxLeg => 1) == 8);
assert(coefficientRing A === QQ and dim A == 0);
assert(reduceHilbert hilbertSeries A == reduceHilbert hilbertSeries(R/straighteningIdeal R));
-- Example 1.2.2 of the paper: 17 generators, 136 relations, Hilbert
-- series 1 + 17q + 17q^2 + q^3
A3 = hookAlgebra(3, {{0,0},{1,1},{2,2}});
assert(numgens A3 == 17 and numgens ideal A3 == 136);
assert(apply(toList(0..4), d -> hilbertFunction(d, A3)) == {1,17,17,1,0});
-- the accessors
assert(hookTypes A3 == {{2,2},{1,1},{0,0}} and alphabetSize A3 == 3 and weightBound A3 === null);
assert(hookTypes ambient A3 == hookTypes A3);
Aw = hookAlgebra(4, {{0,0},{1,1}}, Weight => {1,1,1,1});
assert(weightBound Aw == {1,1,1,1} and numgens Aw == 12);
assert(apply(toList(0..3), d -> hilbertFunction(d, Aw)) == {1,12,2,0});
-- errors
assert(try (hookTypes(QQ[x]); false) else true);
assert(try (hookAlgebra(QQ[x]); false) else true);
assert(try (hookAlgebra(2, {{0,0}}, MaxArm => 0, MaxLeg => 0); false) else true);
assert(try (hookAlgebra(3, {{0,0},{1,0},{0,1},{1,1}}, RelationLimit => 1); false) else true);
assert(try (hookAlgebra(2, {{0,0},{1,1}}, CoefficientRing => RR); false) else true);
///

TEST ///
-- hookBasis, hookVariable and hookMonomial on a hook ring and its
-- algebra: the generators in variable order, the inverse lookups, and the
-- monomial of a tabloid as the product of the variables of its hooks
A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
R = ambient A;
B = hookBasis R;
assert(#B == 17 and all(B, T -> instance(T, HookTabloid)));
assert(hookBasis A == B);
assert(all(17, i -> tabloid R_i == B#i and tabloid A_i == B#i));
assert(all(17, i -> hookVariable(R, B#i) == R_i and hookVariable(A, B#i) == A_i));
assert(#hookBasis(R, {2,2}) == 6 and #hookBasis(R, {1,1}) == 8 and #hookBasis(A, {0,0}) == 3);
assert(hookBasis(R, {2,2}) == take(B, 6));
assert(all(hookBasis(R, {1,1}), T -> #(toList T) == 2 and #(first toList T) == 2));
T = tabloid {{1,3,3},{2,2},{3}};
m = hookMonomial(R, T);
assert(m == R_5*R_15);
assert(tabloid m == T);
assert(hookMonomial(A, T) == A_5*A_15);
assert(hookMonomial(R, {{1,3,3},{2,2},{3}}) == m);
needsPackage "SchurFunctors";
assert(hookMonomial(R, new Filling from T) == m);
assert(hookVariable(R, {{1,3,3},{2},{3}}) == R_5 and hookVariable(R, new Filling from tabloid {{2}}) == R_15);
assert(hookMonomial(R, tabloid {}) == 1_R and hookMonomial(A, tabloid {}) == 1_A);
-- errors: a tabloid that is not a generator, a wrong type, a bad index
assert(try (hookVariable(R, tabloid {{3,1,1},{2},{3}}); false) else true);
assert(try (hookVariable(R, tabloid {{1,2,3,4},{2}}); false) else true);
assert(try (hookVariable(R, tabloid {{1,2},{3,4}}); false) else true);
assert(try (hookMonomial(R, {{1,3,3},{2,2},{4}}); false) else true);
assert(try (hookBasis(R, {3,3}); false) else true);
assert(try (hookBasis(QQ[x]); false) else true);
///

TEST ///
-- tabloid and tabloids on ring elements: a variable, a chain monomial, a
-- quotient variable, the sums, and the rejections
A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
R = ambient A;
assert(tabloid R_4 == (hookBasis R)#4 and tabloid A_4 == tabloid R_4);
T = tabloid(R_4*R_14);
assert(instance(T, HookTabloid) and hookMonomial(R, T) == R_4*R_14);
assert(hookDecomposition T == {tabloid R_4, tabloid R_14});
T3 = tabloid(R_0*R_6*R_14);
assert(hookDecomposition T3 == {tabloid R_0, tabloid R_6, tabloid R_14} and hookMonomial(R, T3) == R_0*R_6*R_14);
H = tabloidTerms(R_4*R_14 - 3*R_0*R_12*R_16);
assert(instance(H, HashTable) and #H == 2);
assert(H#(tabloid(R_4*R_14)) == 1 and H#(tabloid(R_0*R_12*R_16)) == -3);
assert(#tabloidTerms(0_R) == 0);
assert(tabloidTerms(A_4*A_14) === tabloidTerms lift(A_4*A_14, R));
assert(all(keys tabloidTerms(A_4*A_14), isHookSemistandard));
-- a quotient of the algebra is lifted through the ambient rings
B = A/ideal(A_0);
assert(tabloid B_4 == tabloid R_4 and #tabloidTerms B_4 == 1);
-- errors
assert(try (tabloid(2*R_0); false) else true);
assert(try (tabloid(R_0 + R_1); false) else true);
assert(try (tabloid(R_0*R_1); false) else true);
assert(try (tabloid(A_4*A_14); false) else true);
assert(try (tabloid(1_R); false) else true);
assert(try (tabloid((QQ[x])_0); false) else true);
assert(try (tabloidTerms(R_0*R_1); false) else true);
assert(try (tabloid z_5; false) else true);
///

TEST ///
-- hookStraighten(T, A) and hookStraighten f: the engine's normal form
-- read into the algebra agrees with Core's reduction of the monomial,
-- with the tabloid normal form, and with the paper's Example 1.2.2
debug needsPackage "HookPieri";
A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
R = ambient A;
for T in {tabloid {{1,3,3},{2,1},{3}}, tabloid {{1,2,3},{2,1},{3}}, tabloid {{1,1,3},{2,1},{3}}} do (
    f := hookStraighten(T, A);
    assert(instance(f, RingElement) and ring f === A);
    assert(f == hookMonomial(A, T));
    assert(f == hookStraighten hookMonomial(R, T));
    assert(f == hookStraighten(T, R));
    NF := first hpHookStraightenFromAnyHooks("test", toList T);
    assert(#NF == #tabloidTerms f);
    assert(all(keys NF, U -> (tabloidTerms f)#(tabloid U) == NF#U)));
-- a tabloid whose hooks are not semistandard is straightened too (its hooks
-- are not generators, so hookMonomial refuses it)
T0 = tabloid {{2,1,3},{1,2},{3}};
f0 = hookStraighten(T0, A);
NF0 = first hpHookStraightenFromAnyHooks("test", toList T0);
assert(#NF0 == #tabloidTerms f0 and all(keys NF0, U -> (tabloidTerms f0)#(tabloid U) == NF0#U));
assert(try (hookMonomial(A, T0); false) else true);
T = tabloid {{1,2,3},{2,1},{3}};
assert(hookStraighten(toList T, A) == hookStraighten(T, A));
assert(hookStraighten(T, A) != 0 and all(keys tabloidTerms hookStraighten(T, A), isHookSemistandard));
assert(hookStraighten(tabloid {{1,2,3},{2,3},{3}}, A) == A_4*A_16);
assert(hookStraighten(tabloid {}, A) == 1_A);
assert(hookStraighten A_4 == A_4 and hookStraighten(R_4*R_14) == A_4*A_14);
assert(hookStraighten(R_4*R_14 - hookRelation(R, 4, 14)) == A_4*A_14);
-- an element of a quotient of the algebra is returned as it is
B = A/ideal(A_0);
assert(hookStraighten B_4 == B_4);
-- over ZZ/11 the result is the reduction of the rational one
A11 = hookAlgebra(3, {{0,0},{1,1},{2,2}}, CoefficientRing => ZZ/11);
H11 = tabloidTerms hookStraighten(T, A11);
HQ = tabloidTerms hookStraighten(T, A);
assert(#H11 == #HQ and all(keys HQ, U -> H11#U == sub(HQ#U, ZZ/11)));
assert(hpField() === QQ);
-- a weight bound: an overweight tabloid is zero, an admissible one is not
Aw = hookAlgebra(4, {{0,0},{1,1}}, Weight => {1,1,1,1});
assert(hookStraighten(tabloid {{1,2},{3,4}}, Aw) != 0);
assert(hookStraighten(tabloid {{1,1},{2,3}}, Aw) == 0);
-- errors: a hook type outside the algebra, a ring that is not a hook ring
assert(try (hookStraighten(tabloid {{1,2,3,4},{2},{3}}, A); false) else true);
assert(try (hookStraighten(T, QQ[x]); false) else true);
assert(try (hookStraighten((QQ[x])_0); false) else true);
///

TEST ///
-- hookRelation and relationKind: the stored relations of the pairs of
-- generators of Example 1.2.2, their kinds, and the index checks
A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
R = ambient A;
f = hookRelation(R, 4, 14);
assert(instance(f, RingElement) and ring f === R and f != 0);
assert(hookRelation(A, 4, 14) == f and hookRelation(R, 14, 4) == f);
assert(leadTerm f == R_4*R_14);
assert(relationKind(R, 4, 14) == "straightening" and relationKind(A, 14, 4) == "straightening");
assert(promote(f, A) == 0);
assert(hookStraighten(R_4*R_14) == promote(R_4*R_14 - f, A));
kinds = tally flatten for i from 0 to 16 list for j from i to 16 list relationKind(R, i, j);
assert(kinds#"standard" == 153 - 136 and kinds#"nonchain" + kinds#"straightening" == 136);
assert(all(17, i -> relationKind(R, i, i) == "nonchain" and hookRelation(R, i, i) == R_i^2));
assert(#select(toList(0..16), j -> relationKind(R, 4, j) == "nonchain") == 6);
assert(#select(toList(0..16), j -> relationKind(R, 4, j) == "straightening") == 10);
stdPairs = select(flatten for i from 0 to 16 list for j from i to 16 list {i,j}, p -> relationKind(R, p#0, p#1) == "standard");
assert(all(stdPairs, p -> hookRelation(R, p#0, p#1) == 0));
assert(all(stdPairs, p -> isHookSemistandard tabloid(R_(p#0)*R_(p#1))));
assert(set flatten entries gens ideal A === set apply(select(flatten for i from 0 to 16 list for j from i to 16 list {i,j}, p -> relationKind(R, p#0, p#1) != "standard"), p -> hookRelation(R, p#0, p#1)));
-- the smallest ring: the one straightening relation
R2 = hookRing(2, {{0,0},{1,1}});
assert(hookRelation(R2, 1, 2) == R2_1*R2_2 + R2_0*R2_3 and relationKind(R2, 1, 2) == "straightening");
assert(hookRelation(R2, 0, 3) == 0 and relationKind(R2, 0, 3) == "standard");
-- a weight bound: overweight pairs
Aw = hookAlgebra(4, {{0,0},{1,1}}, Weight => {1,1,1,1});
Rw = ambient Aw;
assert(#select(flatten for i from 0 to 11 list for j from i to 11 list relationKind(Rw, i, j), k -> k == "weight-truncation") == 64);
-- errors
assert(try (hookRelation(R, 0, 17); false) else true);
assert(try (relationKind(R, -1, 0); false) else true);
assert(try (hookRelation(QQ[x], 0, 0); false) else true);
///

TEST ///
-- the value of tabloid: a HookTabloid on a hook ring, the pair {T, U} of
-- two of them on a fine Segre ring, and the declared value of the
-- RingElement method is Thing rather than a promise of a HookTabloid
A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
assert(instance(tabloid A_4, HookTabloid));
assert(typicalValues#(tabloid, RingElement) === Thing);
assert(typicalValues#tabloid === HookTabloid);
B = fineSegreAlgebra(2, 3, 2);
W = ambient B;
p = tabloid W_0;
assert(instance(p, List) and #p == 2 and all(p, T -> instance(T, HookTabloid)));
assert(tabloid B_0 == p);
-- the pair is the index of the generator: it round trips
assert(hookVariable(W, p) == W_0 and hookMonomial(W, p) == W_0);
-- the two components lie in the two factors, so they are not forced equal
(R1, R2) = factors W;
assert(alphabetSize R1 == 2 and alphabetSize R2 == 3);
assert(p#0 =!= p#1);
-- tabloids on a fine Segre ring is keyed by the same pairs
assert(all(keys tabloidTerms(W_0), k -> instance(k, List) and #k == 2));
///
