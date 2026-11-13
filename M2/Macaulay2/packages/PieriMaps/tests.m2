TEST ///
-- symbolicForm output structure.
P = QQ[a,b,c];
M = pieri({2,1}, {1}, P);
sf = symbolicForm M;     -- runs without error and caches rule
assert(M.cache#?"rule");
assert(#(M.cache#"sourceBasis") == numColumns M);
assert(#(M.cache#"targetBasis") == numRows M);
-- Direct rule evaluation matches the column.
T = (M.cache#"sourceBasis")#0;
img = (M.cache#"rule")(T);
col = flatten entries M_{0};
nonzero = positions(col, x -> x != 0);
assert(#img == #nonzero);
///

TEST ///
-- applyLR matches the corresponding column of lrMap.
n = 3;
shapes = ({3,2,1}, {2,1}, {2,1});
Qs = lrTableaux shapes;
M = lrMap(shapes, Qs#0, n);
Sb = standardTableaux(n, shapes#0);
Nmu = standardTableaux(n, shapes#1 | toList(#(shapes#0) - #(shapes#1) : 0));
Nnu = standardTableaux(n, shapes#2);
for j from 0 to #Sb - 1 do (
     img := applyLR(shapes, Qs#0, Sb#j, n);
     -- Reconstruct as hash for comparison.
     h := new MutableHashTable;
     for trip in img do h#(trip#1, trip#2) = trip#0;
     for r from 0 to numRows M - 1 do (
	  v := M_(r, j);
	  i := r // (#Nmu);
	  jj := r % (#Nmu);
	  key := (Nnu#i, Nmu#jj);
	  expected := if h#?key then h#key else 0;
	  assert(expected == v);
	  );
     );
///

TEST ///
t = new BettiTally from {(0,{0},0)=>8, (1,{1},1) =>15, (2,{3},3)=>10, (3,{5},5)=>3};
assert (t == (betti res coker pieri({3,1},{1},QQ[a,b,c])))
t = new BettiTally from {(0,{0},0)=>15, (1,{1},1) =>24, (2,{4},4)=>15, (3,{6},6)=>6};
assert (t == (betti res coker pieri({4,1},{1},QQ[a,b,c])))
t = new BettiTally from {(0,{0},0)=>20, (1,{2},2) =>60, (2,{3},3)=>64, (3,{4},4)=>20};
assert (t == (betti res coker pieri({3,2},{2,2},QQ[a,b,c,d])))
assert(schurRank(5, {4,3}) == 560)
t = new BettiTally from {(0,{0},0) => 3, (1,{1},1)=>8, (2,{2},2) => 6,
(3,{4},4)=>1};
assert(t == (betti res coker pureFree({0,1,2,4}, GF(4)[a,b,c])))
///

-- LR-coefficient counts (number of LR tableaux).
TEST ///
assert(#lrTableaux({3,2,1}, {2,1}, {2,1}) == 2)
assert(#lrTableaux({2,1}, {1}, {1,1}) == 1)
assert(#lrTableaux({2,2}, {1,1}, {1,1}) == 1)
assert(#lrTableaux({3,2,1}, {1,1}, {2,2}) == 1)
assert(#lrTableaux({4,2,1}, {2,1}, {2,1,1}) == 1)
assert(#lrTableaux({4,3,2,1}, {2,2}, {2,2,1,1}) == 1)
assert(#lrTableaux({5,3,2,1}, {3,2,1}, {3,2}) == 3)
assert(#lrTableaux({3,3}, {2,2}, {2}) == 0)          -- skew has both cells in same column
assert(#lrTableaux({3,2,1}, {2,1}, {2,2}) == 0)      -- |lambda| != |mu| + |nu|
assert(#lrTableaux({3,2,1}, {3,2,1}, {}) == 1)       -- trivial: only empty filling
///

-- LR maps: rank checks for several cases.
-- For an LR tableau Q, Psi_Q is a GL-equivariant injection of the irrep
-- S_lambda V into S_nu V (x) S_mu V, hence has rank = dim S_lambda.  Distinct
-- LR tableaux give linearly independent maps; the joint image has rank
-- c^lambda_{mu,nu} * dim S_lambda.
TEST ///
checkLR := (lambda, mu, nu, n) -> (
     Qs := lrTableaux(lambda, mu, nu);
     Ms := for Q in Qs list lrMap((lambda, mu, nu), Q, n);
     dimSL := schurRank(n, lambda);
     for M in Ms do assert(rank M == dimSL);
     if #Ms >= 2 then (
	  Mall := fold((a, b) -> a | b, Ms);
	  assert(rank Mall == #Ms * dimSL);
	  );
     )
checkLR({3,2,1}, {2,1}, {2,1}, 3)
checkLR({3,2,1}, {2,1}, {2,1}, 4)
checkLR({2,1}, {1}, {1,1}, 3)
checkLR({2,2}, {1,1}, {1,1}, 3)
checkLR({4,2,1}, {2,1}, {2,1,1}, 4)
///

-- Targets of lrMap respect the documented row indexing.
TEST ///
n = 4;
lambda = {3,2,1};
mu = {2,1};
nu = {2,1};
Qs = lrTableaux(lambda, mu, nu);
M = lrMap((lambda, mu, nu), Qs#0, n);
muPad = mu | toList(#lambda - #mu : 0);
Nmu = standardTableaux(n, muPad);
Nnu = standardTableaux(n, nu);
Sb = standardTableaux(n, lambda);
assert(numRows M == #Nnu * #Nmu);
assert(numColumns M == #Sb);
-- Both signatures should give identical matrices.
P = QQ[a,b,c,d];
assert(M == lrMap((lambda, mu, nu), Qs#0, P));
///

-- Convention rank checks: pieri/lrMap/pureFree return rank-correct matrices in
-- all three conventions and Row == Weyl numerically.
TEST ///
needsPackage "SchurFunctors";
linearizeSym = (M, P, k, n) -> (
     X := gens P;
     symBasis := flatten entries basis(k, P);
     entriesM := entries M;
     newRows := flatten for r in entriesM list (
	  for mon in symBasis list for v in r list lift(coefficient(mon, v), QQ));
     matrix newRows
);
-- pieri across conventions.
P = QQ[a,b,c,d];
M_row = pieri({3,2,1}, {1}, P);
M_fil = pieri({3,2,1}, {1}, P, Convention => "Filling");
M_wey = pieri({3,2,1}, {1}, P, Convention => "Weyl");
assert(M_row == M_wey);
assert(rank linearizeSym(M_row, P, 1, 4) == 64);
assert(rank linearizeSym(M_fil, P, 1, 4) == 64);

-- lrMap across conventions.
Qs = lrTableaux({3,2,1}, {2,1}, {2,1});
for Q in Qs do (
     for conv in {"Row","Filling","Weyl"} do
	  assert(rank lrMap(({3,2,1},{2,1},{2,1}), Q, 3, Convention => conv) == 8);
     );

-- pureFree across conventions: same Betti table.
Pp = QQ[a,b,c];
b0 = betti res coker pureFree({0,1,2,4}, Pp, Convention => "Row");
b1 = betti res coker pureFree({0,1,2,4}, Pp, Convention => "Filling");
b2 = betti res coker pureFree({0,1,2,4}, Pp, Convention => "Weyl");
assert(b0 == b1 and b0 == b2);
///

-- PM <-> Filling round-trip is c_lambda * id; PM <-> Weyl is a literal
-- bijection on standard tableaux.
TEST ///
-- Cache PieriMaps' standardTableaux BEFORE SchurFunctors gets loaded (it would
-- shadow the symbol in the user dict).
pmStdTab = value (PieriMaps#"private dictionary"#"standardTableaux");
needsPackage "SchurFunctors";
for shape in {{2,1}, {3,2,1}, {3,2}, {2,2,1}} do (
     for T in pmStdTab(3, shape) do (
	  fil := pmToFilling T;
	  back := new MutableHashTable;
	  for k in keys fil do (
	       sub := fillingToPM k;
	       for K in keys sub do (
		    cc := fil#k * sub#K;
		    if back#?K then back#K = back#K + cc else back#K = cc;
		    );
	       );
	  cleaned := for k in keys back list if back#k != 0 then (k => back#k) else continue;
	  -- Round-trip should be a single nonzero coefficient on T itself.
	  assert(#cleaned == 1);
	  assert((cleaned#0)#0 == T);
	  );
     );
-- PM <-> Weyl bijection.
for shape in {{3,2,1}, {2,2,1}, {3,2}} do (
     pm := pmStdTab(3, shape);
     assert(apply(apply(pm, pmToWeyl), weylToPM) == pm);
     );
///

-- Column-Pieri proportional to row-Pieri after PM<->Filling change of basis.
TEST ///
pmStdTab = value (PieriMaps#"private dictionary"#"standardTableaux");
needsPackage "SchurFunctors";
sfStdTab = value (SchurFunctors#"private dictionary"#"standardTableaux");
mu = {2,1}; r = 1; col = 2; n = 3;
Psym = QQ[x_0..x_(n-1)];
Pskew = QQ[e_0..e_(n-1), SkewCommutative => true];
Mrow = pieri(mu, {r}, Psym);
Mcol = pieriColumn(mu, {col}, Pskew);
pmS = pmStdTab(n, mu);
sfS = sfStdTab(n, toList conjugate (new Partition from mu));
lambda = {1,1};
pmL = pmStdTab(n, lambda);
sfL = sfStdTab(n, toList conjugate (new Partition from lambda));
A = pmToFillingMatrix(mu, n);
B = fillingToPMMatrix(lambda, n);
rowFlat = matrix flatten for ti from 0 to #pmL-1 list
     for kk from 0 to n-1 list
	  for sj from 0 to #pmS - 1 list lift(coefficient(Psym_kk, Mrow_(ti, sj)), QQ);
colFlat = matrix flatten for tj from 0 to #sfL-1 list
     for kk from 0 to n-1 list
	  for siF from 0 to #sfS - 1 list lift(coefficient(Pskew_kk, Mcol_(tj, siF)), QQ);
McolPM = (B ** id_(QQ^n)) * colFlat * A;
assert(rank rowFlat == rank McolPM);
assert(rank (rowFlat | McolPM) == rank rowFlat);  -- proportional
///

-- verifyWellDefined: the Convention option respects the appropriate
-- straightening relations.
TEST ///
needsPackage "SchurFunctors";
Qs = lrTableaux({3,2,1}, {2,1}, {2,1});
for Q in Qs do
     for conv in {"Row","Filling","Weyl"} do
	  assert(verifyWellDefined(({3,2,1},{2,1},{2,1}), Q, 3,
	       Convention => conv, Verbose => false));
///

-- Direct GL_n equivariance check for the row-Pieri inclusion S_mu V -->
-- V (x) S_lambda V.
--
-- The Lie algebra gl_n acts via Chevalley generators E_{i,j} (i != j):
--   * on V = K^n by E_{i,j}(x_l) = delta_{l,j} * x_i  (i.e. x_i d/dx_j),
--   * on S_mu V (PM row-SSYT basis) by replacing each entry equal to j
--     with i, summing over positions, then row-Garnir straightening,
--   * on V (x) S_lambda V by the Leibniz rule.
--
-- For the Pieri matrix  M : S_mu V --> V (x) S_lambda V  encoded as a
-- (#stdLambda) x (#stdMu) matrix over P with degree-1-form entries, the
-- equivariance test is, for each column index T of M:
--     LHS = sum c_k * M_{*, T_k},  where E_{i,j}.T = sum c_k * T_k
--     RHS = (V-action on M_{*,T} entries) + (S_lambda-action on the
--                                            target row indices).
-- The matrix is GL_n-equivariant iff LHS = RHS for every T and every
-- (i, j).  Since our Pieri map is rationally constructed and S_mu V is
-- an irreducible GL representation, equivariance is the unique
-- characterization (up to scalar) by Schur's lemma.
TEST ///
-- Action of E_{i,j} on a row-SSYT tableau, returning a HashTable of std
-- tableau coefficients via straightening.
actE = (i, j, T) -> (
     totalH := new MutableHashTable;
     for r from 0 to #T-1 do
	  for c from 0 to #(T#r)-1 do
	       if T#r#c == j then (
		    Tprime := for r2 from 0 to #T-1 list
			 (if r2 == r then
			      for c2 from 0 to #(T#r2)-1 list
				   (if c2 == c then i else (T#r2)#c2)
			  else T#r2);
		    h := straighten Tprime;
		    for k in keys h do
			 if totalH#?k then totalH#k = totalH#k + h#k
			 else totalH#k = h#k;
		    );
     new HashTable from totalH
     );

-- Test M is gl_n-equivariant by checking commutativity with each E_{i,j}.
checkPieriEquivariant = (mu, boxes, n) -> (
     P := QQ[x_0..x_(n-1)];
     X := gens P;
     M := pieri(mu, boxes, P);
     subOne := (m, k) -> for ii from 0 to #m-1 list (if ii == k-1 then m_ii - 1 else m_ii);
     lambda := mu;
     for b in boxes do lambda = subOne(lambda, b);
     stdsMu  := standardTableaux(n, mu);
     stdsLam := standardTableaux(n, lambda);
     for i from 0 to n-1 do for j from 0 to n-1 do (
	  if i == j then continue;
	  for tIdx from 0 to #stdsMu-1 do (
	       T := stdsMu#tIdx;
	       colT := flatten entries M_{tIdx};
	       -- RHS = E.M(T): V-action + S_lambda-action
	       -- V-action on Sym^m V (x) S_lambda part: x_i * d/dx_j applied
	       -- to each entry (entries are degree-m forms for m-box strips).
	       vAction := for f in colT list X#i * diff(X#j, f);
	       sAction := new MutableList from for s in stdsLam list 0_P;
	       for idx from 0 to #stdsLam-1 do (
		    h := actE(i, j, stdsLam#idx);
		    for kk in keys h do (
			 idxK := position(stdsLam, s -> s == kk);
			 if idxK =!= null then
			      sAction#idxK = sAction#idxK + h#kk * colT#idx;
			 );
		    );
	       RHS := for idx from 0 to #stdsLam-1 list (vAction#idx + sAction#idx);
	       -- LHS = M(E.T)
	       LHS := for s in stdsLam list 0_P;
	       hT := actE(i, j, T);
	       for kk in keys hT do (
		    idxK := position(stdsMu, s -> s == kk);
		    if idxK =!= null then (
			 colK := flatten entries M_{idxK};
			 LHS = for idx from 0 to #stdsLam-1 list
			      (LHS#idx + hT#kk * colK#idx);
			 );
		    );
	       assert(LHS == RHS);
	       );
	  );
     );

-- Single-box, every row removable: the cases that catch the "interior k"
-- bug discussed in the documentation tests for path-based pieri.
checkPieriEquivariant({2,1},   {1}, 3);
checkPieriEquivariant({2,1},   {2}, 3);
checkPieriEquivariant({3,2,1}, {1}, 3);
checkPieriEquivariant({3,2,1}, {2}, 3);  -- INTERIOR row removal
checkPieriEquivariant({3,2,1}, {3}, 3);

-- Multi-box horizontal strips, including patterns through interior rows.
checkPieriEquivariant({3,2,1}, {1,3},   3);
checkPieriEquivariant({3,2,1}, {1,2,3}, 3);
checkPieriEquivariant({3,1},   {1,1},   3);
///

-- dualPieri ∘ pieri == Id (the defining property of the GL-equivariant
-- projection by Schur's lemma).  Tested across single- and multi-box
-- strips and across all three Conventions.
TEST ///
encodeKMat = (M, monBasis, K) -> (
     dimLam := numRows M;
     dimMu  := numColumns M;
     matrix(K, flatten for tLam from 0 to dimLam-1 list (
	       for alpha in monBasis list (
		    for tMu from 0 to dimMu-1 list (
			 lift(coefficient(alpha, M_(tLam, tMu)), K)))))
     );

checkDualPieriId = (mu, boxes, P, conv) -> (
     M := pieri(mu, boxes, P, Convention => conv);
     N := dualPieri(mu, boxes, P, Convention => conv);
     monBasis := flatten entries basis(#boxes, P);
     Mk := encodeKMat(M, monBasis, QQ);
     assert(N * Mk == id_(QQ^(numColumns Mk)));
     );

P = QQ[a,b,c];
for conv in {"Row", "Filling", "Weyl"} do (
     checkDualPieriId({2,1},   {1}, P, conv);
     checkDualPieriId({2,1},   {2}, P, conv);
     checkDualPieriId({3,2,1}, {1}, P, conv);
     checkDualPieriId({3,2,1}, {2}, P, conv);
     checkDualPieriId({3,2,1}, {3}, P, conv);
     checkDualPieriId({3,2,1}, {1,3}, P, conv);
     checkDualPieriId({3,1},   {1,1}, P, conv);
     );
///

-- dualLR ∘ lrMap == Id (per Q), and for multiplicity > 1, the dual at Q
-- annihilates the inclusions for OTHER LR tableaux Q': dualLR_Q ∘ lrMap_{Q'} = 0.
TEST ///
checkDualLRId = (shapes, Q, n, conv) -> (
     M := lrMap(shapes, Q, n, Convention => conv);
     N := dualLR(shapes, Q, n, Convention => conv);
     assert(N * M == id_(QQ^(numColumns M)));
     );

checkDualLRCrossZero = (shapes, Q, Qother, n, conv) -> (
     N := dualLR(shapes, Q, n, Convention => conv);
     M := lrMap(shapes, Qother, n, Convention => conv);
     assert(N * M == 0);
     );

for conv in {"Row", "Filling", "Weyl"} do (
     -- multiplicity-1 cases
     checkDualLRId(({2,1}, {1}, {1,1}), (lrTableaux({2,1},{1},{1,1}))#0, 3, conv);
     checkDualLRId(({2,2}, {1,1}, {1,1}), (lrTableaux({2,2},{1,1},{1,1}))#0, 3, conv);
     -- multiplicity 2: c^{(3,2,1)}_{(2,1),(2,1)} = 2 -- both directions
     Qs := lrTableaux({3,2,1}, {2,1}, {2,1});
     for Q in Qs do checkDualLRId(({3,2,1},{2,1},{2,1}), Q, 3, conv);
     -- cross-Q orthogonality
     checkDualLRCrossZero(({3,2,1},{2,1},{2,1}), Qs#0, Qs#1, 3, conv);
     checkDualLRCrossZero(({3,2,1},{2,1},{2,1}), Qs#1, Qs#0, 3, conv);
     );
///

-- applyDualPieri / applyDualLR: point evaluation respects the matrix product
-- and handles non-standard input via convention-aware straightening.  Inputs
-- can be passed as either a List (PM row form) or a Filling (column form),
-- regardless of Convention.
TEST ///
-- Cache PM's standardTableaux BEFORE loading SchurFunctors (which would
-- shadow the symbol with its own Filling-returning version).
pmStdTab = value (PieriMaps#"private dictionary"#"standardTableaux");
needsPackage "SchurFunctors";

-- (1) applyDualPieri agrees with the matrix product on standard input.
P = QQ[a,b,c];
n = 3;
mu = {3,2,1};
boxes = {1};
lambda = {2,2,1};
N = dualPieri(mu, boxes, P);
stdsLam = pmStdTab(n, lambda);
stdsMu  = pmStdTab(n, mu);
mons = flatten entries basis(1, P);
for tLam in stdsLam do for alpha in mons do (
     img := applyDualPieri((mu, boxes), alpha, tLam, n);
     -- compare with N applied to e_(tLam, alpha)
     iLam := position(stdsLam, s -> s == tLam);
     iAlpha := position(mons, m -> m == alpha);
     col := iLam * #mons + iAlpha;
     expected := flatten entries N_{col};
     observed := for s in stdsMu list (
	  c := select(img, p -> p#1 == s);
	  if #c == 0 then 0_QQ else c#0#0
	  );
     assert(observed == expected);
     );

-- (2) applyDualPieri straightens non-standard input.
-- {{1,0},{2}} is non-standard for shape {2,1}: row 1 is decreasing.
P2 = QQ[a,b,c,d];
nonStdRes = applyDualPieri(({3,2,1}, {3}), a, {{1,0},{2}}, 4);
strRes = applyDualPieri(({3,2,1}, {3}), a, {{0,1},{2}}, 4);
-- Straightening of {{1,0},{2}} swaps row entries: should give same as {{0,1},{2}}
-- (row Garnir gives the standard form trivially since {0,1}=sort{1,0}).
assert(set apply(nonStdRes, p -> {p#0, p#1}) === set apply(strRes, p -> {p#0, p#1}));

-- (3) applyDualLR agrees with the matrix on a standard basis pair.
shapes = ({2,1}, {1}, {1,1});
Q = (lrTableaux shapes)#0;
N2 = dualLR(shapes, Q, 3);
stdsLam = pmStdTab(3, shapes#0);
stdsMu  = pmStdTab(3, shapes#1 | toList(#(shapes#0) - #(shapes#1) : 0));
stdsNu  = pmStdTab(3, shapes#2);
for tNu in stdsNu do for tMu in stdsMu do (
     img := applyDualLR(shapes, Q, {tNu, tMu}, 3);
     iNu := position(stdsNu, s -> s == tNu);
     iMu := position(stdsMu, s -> s == tMu);
     col := iNu * #stdsMu + iMu;
     expected := flatten entries N2_{col};
     observed := for s in stdsLam list (
	  c := select(img, p -> p#1 == s);
	  if #c == 0 then 0_QQ else c#0#0
	  );
     assert(observed == expected);
     );

-- (4) Input-type flexibility: applyDualPieri accepts either a List (PM
-- row form) or a Filling (column form) regardless of Convention, and
-- runs without error on each.  Filling inputs go through fillingToPM
-- internally (under Convention "Row" / "Weyl"), so the result is in
-- PM std-tableau basis on output.
P3 = QQ[a,b,c];
listT = (pmStdTab(3, {2,1}))#0;
imgFromList = applyDualPieri(({3,1}, {1}), a, listT, 3);
assert(#imgFromList >= 1);
-- Same call with a Filling input runs (the conversion path is exercised).
expansion = pmToFilling listT;
filT = (keys expansion)#0;
imgFromFilling = applyDualPieri(({3,1}, {1}), a, filT, 3);
assert(#imgFromFilling >= 1);
-- Output T_mu values come from the PM std basis in either case.
assert(all(imgFromList,    p -> class p#1 === List));
assert(all(imgFromFilling, p -> class p#1 === List));
///

-- Transition matrices between conventions are mutually invertible (up to
-- the documented shape-dependent scalar c_lambda for PM <-> Filling).
TEST ///
needsPackage "SchurFunctors";
for shape in {{2,1}, {3,1}, {3,2}, {3,2,1}, {2,2,1}} do
     for n in {3, 4} do (
	  if n < #shape then continue;
	  A := pmToFillingMatrix(shape, n);
	  B := fillingToPMMatrix(shape, n);
	  -- A * B and B * A are scalar multiples of identity (the c_lambda scalar)
	  prodAB := A * B;
	  prodBA := B * A;
	  -- Diagonals are all the same value
	  diagAB := apply(min(numRows prodAB, numColumns prodAB),
	       i -> prodAB_(i, i));
	  assert(all(diagAB, d -> d == diagAB#0));
	  c := diagAB#0;
	  assert(c != 0);
	  assert(prodAB == c * id_(QQ^(numRows prodAB)));
	  assert(prodBA == c * id_(QQ^(numRows prodBA)));
	  );
///

-- verifyEquivariant: all the maps we construct (pieri, lrMap, dualPieri,
-- dualLR) are GL_n-equivariant.  Tested in Row and Weyl conventions
-- (matrix data is identical between the two; the test machinery uses
-- PM row-Garnir straightening for the gl_n action).
TEST ///
P = QQ[a,b,c];

-- pieri inclusion S_mu V -> Sym^d V (x) S_lambda V
for conv in {"Row", "Filling", "Weyl"} do (
     M := pieri({3,2,1}, {1}, P, Convention => conv);
     assert(verifyEquivariant(M, {3,2,1}, {1}, P, Convention => conv));
     M2 := pieri({3,2,1}, {2}, P, Convention => conv);
     assert(verifyEquivariant(M2, {3,2,1}, {2}, P, Convention => conv));
     -- multi-box
     M3 := pieri({3,2,1}, {1,3}, P, Convention => conv);
     assert(verifyEquivariant(M3, {3,2,1}, {1,3}, P, Convention => conv));
     );
///

TEST ///
-- lrMap S_lambda V -> S_nu V (x) S_mu V
for conv in {"Row", "Filling", "Weyl"} do (
     for Q in lrTableaux({3,2,1}, {2,1}, {2,1}) do (
	  M := lrMap(({3,2,1}, {2,1}, {2,1}), Q, 3, Convention => conv);
	  assert(verifyEquivariant(M, ({3,2,1}, {2,1}, {2,1}), 3, Convention => conv));
	  );
     );
///

TEST ///
-- dualPieri Sym^d V (x) S_lambda V -> S_mu V
P = QQ[a,b,c];
for conv in {"Row", "Filling", "Weyl"} do (
     M := dualPieri({3,2,1}, {1}, P, Convention => conv);
     assert(verifyEquivariant(M, {3,2,1}, {1}, P, Convention => conv, Direction => "Dual"));
     M2 := dualPieri({3,2,1}, {2}, P, Convention => conv);
     assert(verifyEquivariant(M2, {3,2,1}, {2}, P, Convention => conv, Direction => "Dual"));
     );
///

TEST ///
-- dualLR S_nu V (x) S_mu V -> S_lambda V (per Q)
for conv in {"Row", "Filling", "Weyl"} do (
     for Q in lrTableaux({3,2,1}, {2,1}, {2,1}) do (
	  N := dualLR(({3,2,1}, {2,1}, {2,1}), Q, 3, Convention => conv);
	  assert(verifyEquivariant(N, ({3,2,1}, {2,1}, {2,1}), 3, Convention => conv, Direction => "Dual"));
	  );
     );
///

-- pieriColumn with Convention support: matches dim and is consistent with Filling.
TEST ///
P = QQ[e_0..e_2, SkewCommutative => true];
M_F = pieriColumn({2,1}, {1}, P, Convention => "Filling");
M_R = pieriColumn({2,1}, {1}, P, Convention => "Row");
M_W = pieriColumn({2,1}, {1}, P, Convention => "Weyl");
assert(numRows M_R == numRows M_F and numColumns M_R == numColumns M_F);
assert(M_R == M_W);
///

-- dualPieriColumn ∘ pieriColumn = Id (column-form analogue of dualPieri).
TEST ///
P = QQ[e_0..e_2, SkewCommutative => true];
encodeKMat = (M, monBasis, K) -> (
     dimLam := numRows M;
     dimMu  := numColumns M;
     matrix(K, flatten for tLam from 0 to dimLam-1 list (
	       for alpha in monBasis list (
		    for tMu from 0 to dimMu-1 list (
			 lift(coefficient(alpha, M_(tLam, tMu)), K)))))
     );
checkColId = (mu, cols, P, conv) -> (
     M := pieriColumn(mu, cols, P, Convention => conv);
     N := dualPieriColumn(mu, cols, P, Convention => conv);
     monBasis := flatten entries basis(#cols, P);
     Mk := encodeKMat(M, monBasis, QQ);
     assert(N * Mk == id_(QQ^(numColumns Mk)));
     );
for conv in {"Row", "Filling", "Weyl"} do (
     checkColId({2,1},   {1}, P, conv);
     checkColId({2,1},   {2}, P, conv);
     checkColId({2,1,1}, {1}, P, conv);
     );
///

-- applyPieri / applyPieriColumn agree with the corresponding column of the
-- matrix on a standard input tableau.
TEST ///
P = QQ[a,b,c];
mu = {3,2,1};
boxes = {1};
M = pieri(mu, boxes, P);
stdsMu = standardTableaux(3, mu);
for tIdx from 0 to #stdsMu - 1 do (
     T := stdsMu#tIdx;
     for r from 0 to #boxes - 1 list r;
     img := applyPieri((mu, boxes), T, P);
     col := flatten entries M_{tIdx};
     -- Reconstruct img as a vector indexed by lambda's std tableaux.
     stdsLam := standardTableaux(3, {2,2,1});
     reconstructed := for s in stdsLam list (
	  matched := select(img, p -> p#1 == s);
	  if #matched == 0 then 0_P else matched#0#0
	  );
     assert(reconstructed == col);
     );
///

-- verifyWellDefined for pieri / pieriColumn / dualPieri / dualPieriColumn
-- across all conventions.  Smoke test that the apply functions factor
-- through Garnir straightening relations.
TEST ///
P = QQ[a,b,c];
Pcol = QQ[e_0..e_2, SkewCommutative => true];
for conv in {"Row", "Filling", "Weyl"} do (
     -- pieri inclusion
     assert(verifyWellDefined({3,2,1}, {1}, P, Convention => conv, Verbose => false));
     -- dualPieri projection
     assert(verifyWellDefined({3,2,1}, {1}, P, Convention => conv, Direction => "Dual", Verbose => false));
     -- pieriColumn inclusion
     assert(verifyWellDefined({2,1}, {1}, Pcol, Convention => conv, Verbose => false));
     -- dualPieriColumn projection
     assert(verifyWellDefined({2,1}, {1}, Pcol, Convention => conv, Direction => "Dual", Verbose => false));
     );
-- dualLR projection
for conv in {"Row", "Filling", "Weyl"} do (
     for Q in lrTableaux({2,1}, {1}, {1,1}) do
	  assert(verifyWellDefined(({2,1}, {1}, {1,1}), Q, 3, Convention => conv,
	       Direction => "Dual", Verbose => false));
     );
///

-- Edge cases: empty boxes, single-cell shapes, n = 1.
TEST ///
P3 = QQ[a,b,c];
-- Empty boxes for pieri: identity-like map (mu == lambda).  pieri requires
-- at least one box currently; skip and check single-box minimal case.
assert(numRows pieri({1}, {1}, P3) == 1);   -- {1} -> empty: 1x3 matrix
M = pieri({1}, {1}, P3);
assert(rank M == 1);
-- Single-cell input via applyPieri.
img = applyPieri(({1}, {1}), {{0}}, P3);
assert(#img == 1);
assert(img#0#0 == a);
-- n = 1 case: only mu = {k} valid.  pieri({2}, {1}, P3) is fine; pieri({2}, {1}, QQ[x]) too.
P1 = QQ[x];
M1 = pieri({2}, {1}, P1);
assert(numRows M1 == 1 and numColumns M1 == 1);   -- dim S_(2) K^1 = 1, dim S_(1) K^1 = 1
///
