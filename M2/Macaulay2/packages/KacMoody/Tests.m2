-- KacMoody tests.  Loaded by KacMoody.m2 in the package dictionary.

------------------------------------------------------------
------------------------------------------------------------
--
--                         TESTS
--
-- NOTE:  The type-A constructor @TT "cartanA n"@ returns the
-- Cartan matrix of $A_{n-1} = \mathfrak{sl}_n$, i.e.\ of rank
-- $n-1$.  Dynkin labels must have length equal to the rank.
--
------------------------------------------------------------
------------------------------------------------------------


-------------------------------------------------------------------------
-- Datum Tests
-------------------------------------------------------------------------

TEST ///

mustError = f -> (try (f(); false) else true)

sameList = (u,v) -> (#u == #v and all(u,v,(x,y) -> x == y))

------------------------------------------------------------------------
-- Canonical immutable data, strong equality, and hash-key behavior
------------------------------------------------------------------------

A3 = cartanA 4;
D1 = cartanDatum A3;
D2 = cartanDatum(A3 ** QQ);
assert(D1 === D2);
assert(hash D1 === hash D2);
assert((new HashTable from {D1 => "found"})#D2 === "found");
assert(datumCartanMatrix D1 == A3);
assert((D1#"rank",D1#"matrixRank",D1#"corank",D1#"type") === (3,3,0,"finite"));

R1 = standardRealization D1;
R2 = standardRealization D2;
assert(R1 === R2);
assert(R1#"dimension" === 3);
assert(R1#"isMinimal");
assert(matrix(QQ,R1#"simpleCorootCoordinates") *
       transpose matrix(QQ,R1#"simpleRootCoordinates") == A3 ** QQ);

RD1 = rootDatum D1;
RD2 = rootDatum(A3 ** QQ);
assert(RD1 === RD2);
assert(hash RD1 === hash RD2);
assert((new HashTable from {RD1 => 17})#RD2 === 17);

w1 = kmWeight(RD1,{1,0,2});
w2 = kmWeight(RD2,{1_QQ,0_QQ,2_QQ});
assert(w1 === w2);
assert(hash w1 === hash w2);
assert((new HashTable from {w1 => 23})#w2 === 23);
assert((tally{w1,w2})#w1 === 2);

------------------------------------------------------------------------
-- Standard realization, Cartan pairings, and arithmetic
------------------------------------------------------------------------

for i from 0 to 2 do for j from 0 to 2 do (
    alphaJ := kmRoot(RD1,apply(3,k -> if k == j then 1 else 0));
    corootI := kmCoroot(RD1,apply(3,k -> if k == i then 1 else 0));
    assert(pairing(alphaJ,corootI) == A3_(i,j));
);

alpha1 = kmRoot(RD1,{1,0,0});
alpha2 = kmRoot(RD1,{0,1,0});
h1 = kmCoroot(RD1,{1,0,0});
h2 = kmCoroot(RD1,{0,1,0});
assert(alpha1 + alpha2 === kmRoot(RD1,{1,1,0}));
assert(2*alpha1-alpha2 === kmRoot(RD1,{2,-1,0}));
assert(h1+h2 === kmCoroot(RD1,{1,1,0}));
assert(toWeight alpha1 === kmWeight(RD1,{2,-1,0}));
assert(toCoweight h1 === kmCoweight(RD1,{2,-1,0}));
assert(toRoot(toWeight(kmRoot(RD1,{2,1,-1}))) === kmRoot(RD1,{2,1,-1}));
assert(toCoroot(toCoweight(kmCoroot(RD1,{2,1,-1}))) === kmCoroot(RD1,{2,1,-1}));

omega1 = kmWeight(RD1,{1,0,0});
omega2 = kmWeight(RD1,{0,1,0});
fundCoweight1 = kmCoweight(RD1,{1,0,0});
assert(pairing(omega1,h1) == 1);
assert(pairing(omega1,h2) == 0);
assert(omega1+omega2 === kmWeight(RD1,{1,1,0}));
assert(3*omega1-omega2 === kmWeight(RD1,{3,-1,0}));
assert(pairing(omega1,fundCoweight1) == 3/4);

assert(sameList(realizationCoordinates alpha1,{2,-1,0}));
assert(sameList(realizationCoordinates h1,{1,0,0}));
assert(sameList(realizationCoordinates omega1,{1,0,0}));
assert(sameList(realizationCoordinates fundCoweight1,{3/4,1/2,1/4}));

------------------------------------------------------------------------
-- Smith invariants of P/Q and global forms
------------------------------------------------------------------------

assert((pModQInvariants cartanA 2)#"torsionInvariants" === {2});
assert((pModQInvariants cartanA 3)#"torsionInvariants" === {3});
assert((pModQInvariants cartanA 4)#"torsionInvariants" === {4});
assert((pModQInvariants cartanD 4)#"torsionInvariants" === {2,2});
assert((pModQInvariants cartanE 6)#"torsionInvariants" === {3});
assert((pModQInvariants cartanE 7)#"torsionInvariants" === {2});
assert((pModQInvariants cartanE 8)#"torsionInvariants" === {});
assert((pModQInvariants cartanF4())#"torsionInvariants" === {});
assert((pModQInvariants cartanG2())#"torsionInvariants" === {});

A1xA2 = (cartanA 2) ++ (cartanA 3);
Qprod = pModQInvariants A1xA2;
assert(Qprod#"torsionInvariants" === {6});
assert(Qprod#"order" === 6);

Qaff = pModQInvariants cartanAffineA 3;
assert(not Qaff#"finite");
assert(Qaff#"freeRank" === 1);

A1 = cartanA 2;
RDsc = rootDatum(A1,GlobalForm=>"SimplyConnected");
RDad = rootDatum(A1,GlobalForm=>"Adjoint");
RDun = rootDatum(A1,GlobalForm=>"Unspecified");
assert(RDsc =!= RDad);
assert(integratesToGlobalForm kmWeight(RDsc,{1}));
assert(not integratesToGlobalForm kmWeight(RDad,{1}));
assert(integratesToGlobalForm kmWeight(RDad,{2}));
assert(integratesToGlobalForm kmWeight(RDsc,{1/2}) === false);
assert(integratesToGlobalForm kmWeight(RDun,{2}) === null);
assert(isRootLatticeWeight kmWeight(RDad,{2}));
assert(not isRootLatticeWeight kmWeight(RDad,{1}));

-- Parent identity includes the global form, so cross-parent arithmetic fails.
assert(mustError(() -> kmWeight(RDsc,{1}) + kmWeight(RDad,{1})));
assert(kmWeight(RDsc,{1}) =!= kmWeight(RDad,{1}));
assert(characterLatticeBasis RDsc == matrix(QQ,{{1}}));
assert(characterLatticeBasis RDad == matrix(QQ,{{2}}));
assert(cocharacterLatticeBasis RDad == matrix(QQ,{{1/2}}));
assert(characterLatticeBasis RDsc * transpose cocharacterLatticeBasis RDsc == id_(QQ^1));

------------------------------------------------------------------------
-- A custom finite realization
------------------------------------------------------------------------

customR1 = realization(cartanDatum A1,matrix(QQ,{{4}}),matrix(QQ,{{1/2}}));
customR2 = realization(cartanDatum(A1 ** QQ),matrix(QQ,{{4}}),matrix(QQ,{{1/2}}));
assert(customR1 === customR2);
customD1 = rootDatum customR1;
customD2 = rootDatum customR2;
assert(customD1 === customD2);
cw = weightFromRealizationCoordinates(customD1,{2});
ccw = coweightFromRealizationCoordinates(customD1,{1/4});
assert(cw === kmWeight(customD1,{1}));
assert(ccw === kmCoweight(customD1,{1}));
assert(realizationCoordinates cw === {2});
assert(realizationCoordinates ccw === {1/4});
assert(pairing(cw,ccw) == 1/2);
assert(characterLatticeBasis customD1 == matrix(QQ,{{2}}));
assert(cocharacterLatticeBasis customD1 == matrix(QQ,{{1/2}}));
assert(integratesToGlobalForm cw);
assert(not integratesCoweightToGlobalForm ccw);
assert(integratesCoweightToGlobalForm toCoweight(kmCoroot(customD1,{1})));
customIntermediate = rootDatum(customR1,CharacterLattice=>matrix(QQ,{{4}}));
assert((customIntermediate#"globalForm")#"name" === "Custom");
assert(integratesToGlobalForm weightFromRealizationCoordinates(customIntermediate,{4}));
assert(not integratesToGlobalForm weightFromRealizationCoordinates(customIntermediate,{2}));
assert(mustError(() -> rootDatum(customR1,CharacterLattice=>matrix(QQ,{{3}}))));
assert(mustError(() -> realization(cartanDatum A1,matrix{{2}},matrix{{2}})));

------------------------------------------------------------------------
-- Singular realizations retain the directions that Dynkin labels omit
------------------------------------------------------------------------

Aaff = cartanAffineA 3;
Daff = cartanDatum Aaff;
Raff = standardRealization Daff;
RDaff = rootDatum Raff;
assert(Daff#"rank" === 4);
assert(Daff#"matrixRank" === 3);
assert(Daff#"corank" === 1);
assert(Raff#"dimension" === 5);
assert(Raff#"isMinimal");
assert(matrix(QQ,Raff#"simpleCorootCoordinates") *
       transpose matrix(QQ,Raff#"simpleRootCoordinates") == Aaff ** QQ);

labelClass = kmWeight(RDaff,{1,2,3,4});
assert(mustError(() -> realizationCoordinates labelClass));
fullWeight1 = weightFromRealizationCoordinates(RDaff,{1,2,3,4,5});
fullWeight2 = weightFromRealizationCoordinates(RDaff,{1,2,3,4,6});
fullWeight1Copy = weightFromRealizationCoordinates(
    RDaff,{1_QQ,2_QQ,3_QQ,4_QQ,5_QQ});
assert(datumCoordinates fullWeight1 === {1,2,3,4});
assert(fullWeight1 === fullWeight1Copy);
assert(hash fullWeight1 === hash fullWeight1Copy);
assert(fullWeight1 =!= fullWeight2);
assert(fullWeight1 =!= labelClass);
assert(labelClass#"realizationCoordinates" === null);
assert(fullWeight1#"realizationCoordinates" =!= null);
assert(realizationCoordinates fullWeight1 === {1,2,3,4,5});
assert(mustError(() -> labelClass + fullWeight1));
assert(realizationCoordinates(fullWeight1+fullWeight2) === {2,4,6,8,11});

fullCoweight = coweightFromRealizationCoordinates(RDaff,{2,1,0,-1,3});
assert(pairing(fullWeight1,fullCoweight) == 15);
assert(mustError(() -> weightToRootCoordinates labelClass));
assert(integratesToGlobalForm labelClass === null);

-- Numerical tuples retain the original coordinate order and every invisible
-- direction, distinguishing quotient classes from complete weights.
assert(toString w1 === "(1, 0, 2)");
assert(toString cw === "(1)");
assert(toString ccw === "(1)");
assert(toString alpha1 === "(1, 0, 0)");
assert(toString(2*alpha1-alpha2) === "(2, -1, 0)");
assert(toString(-h1) === "(-1, 0, 0)");
assert(toString labelClass ===
    "(1, 2, 3, 4) mod ann(h')");
assert(toString fullWeight1 ===
    "(1, 2, 3, 4, 5)");
assert(toString fullWeight2 ===
    "(1, 2, 3, 4, 6)");
assert(toString fullWeight1 =!= toString fullWeight2);
assert(toString net fullWeight1 === toString fullWeight1);

-- The dual phenomenon occurs for coweights.  The affine central coroot lies
-- in the annihilator of every simple root, so adding it preserves all simple
-- root evaluations but changes the complete coweight.
centralCoweight = toCoweight kmCoroot(RDaff,{1,1,1,1});
fullCoweight2 = fullCoweight + centralCoweight;
assert(datumCoordinates centralCoweight === {0,0,0,0});
assert(datumCoordinates fullCoweight2 === datumCoordinates fullCoweight);
assert(fullCoweight2 =!= fullCoweight);
assert(toString fullCoweight ===
    "(2, 1, 0, -1, 3)");
assert(toString fullCoweight2 ===
    "(3, 2, 1, 0, 3)");
assert(toString fullCoweight =!= toString fullCoweight2);
assert(toString net fullCoweight === toString fullCoweight);

-- In singular type both canonical lattices have full rank, contain the
-- required roots/coroots, and are in perfect duality.
Baff = characterLatticeBasis RDaff;
Yaff = cocharacterLatticeBasis RDaff;
assert(Baff * transpose Yaff == id_(QQ^(Raff#"dimension")));
assert(all(flatten entries(matrix(QQ,Raff#"simpleRootCoordinates") * inverse Baff),
    x -> denominator x == 1));
assert(all(flatten entries(matrix(QQ,Raff#"simpleCorootCoordinates") * inverse Yaff),
    x -> denominator x == 1));

for i from 0 to 3 do for j from 0 to 3 do (
    alphaJaff := kmRoot(RDaff,apply(4,k -> if k == j then 1 else 0));
    corootIaff := kmCoroot(RDaff,apply(4,k -> if k == i then 1 else 0));
    assert(pairing(alphaJaff,corootIaff) == Aaff_(i,j));
);

------------------------------------------------------------------------
-- Coordinate constructors and malformed inputs
------------------------------------------------------------------------

assert(cartanDatum A3 === D1);
assert(datumCartanMatrix RD1 == A3);
assert(datumCoordinates kmRoot(A3,{1,2,3}) === {1,2,3});
assert(datumCoordinates kmCoroot(A3,{1,2,3}) === {1,2,3});
assert(datumCoordinates kmWeight(A3,{1,2,3}) === {1,2,3});
assert(datumCoordinates kmCoweight(A3,{1,2,3}) === {1,2,3});
assert(parentDatum kmWeight(A3,{1,2,3}) === RD1);

assert(mustError(() -> cartanDatum matrix{{2,-1},{0,2}}));
assert(mustError(() -> kmRoot(RD1,{1,0})));
assert(mustError(() -> kmRoot(RD1,{1/2,0,0})));
assert(mustError(() -> kmWeight(RD1,{1,0})));
assert(mustError(() -> rootDatum(A3,GlobalForm=>"Intermediate")));
assert(mustError(() -> cartanDatum(A3,NodeLabels=>{"x","x","z"})));
assert(mustError(() -> cartanDatum(A3,NodeLabels=>{10,10_QQ,30})));
assert((cartanDatum(A3,NodeLabels=>{10,20,30}))#"nodeLabels"
    === {10,20,30});
Dnamed = cartanDatum("A3",NodeLabels=>{"left","middle","right"});
assert(Dnamed#"nodeLabels" === {"left","middle","right"});
assert(datumCartanMatrix Dnamed == A3);
assert(toString kmWeight(rootDatum Dnamed,{1,0,2}) ===
    "(1, 0, 2)");
assert(rootDatum "G2" === rootDatum cartanG2());

-- Rank zero is the finite empty root datum, needed for the derived Levi of
-- a Borel parabolic.
Dzero = cartanDatum matrix(ZZ,{});
assert(Dzero#"rank" == 0);
assert(Dzero#"type" === "finite");
assert(Dzero#"components" === {});
assert(Dzero#"nodeLabels" === {});
assert(numrows datumCartanMatrix Dzero == 0);
RDzero = rootDatum Dzero;
assert((realization RDzero)#"dimension" == 0);
assert(numrows characterLatticeBasis RDzero == 0);
assert((pModQInvariants RDzero)#"order" == 1);
assert(toString kmRoot(RDzero,{}) === "()");
assert(toString kmWeight(RDzero,{}) === "()");
assert(weightBasis RDzero === {});
assert(coweightBasis RDzero === {});

------------------------------------------------------------------------
-- The named mathematical bases remain complete and available independently
-- of the default numerical displays
------------------------------------------------------------------------

WB = weightBasis RD1;
CB = coweightBasis RD1;
assert(#WB == 3 and #CB == 3);
assert(apply(WB,toString) === {"(1, 0, 0)","(0, 1, 0)","(0, 0, 1)"});
assert(apply(CB,toString) === {"(1, 0, 0)","(0, 1, 0)","(0, 0, 1)"});
for i from 0 to 2 do for j from 0 to 2 do (
    assert(pairing(WB#i,kmCoroot(RD1,apply(3,k -> if k == j then 1 else 0))) ==
        if i == j then 1 else 0);
    assert(pairing(kmRoot(RD1,apply(3,k -> if k == j then 1 else 0)),CB#i) ==
        if i == j then 1 else 0);
);
assert(toString kmWeight(RD1,{-1/2,0,3/2}) ===
    "(-1/2, 0, 3/2)");
assert(toString kmWeight(RD1,{0,0,0}) === "(0, 0, 0)");

Da1 = rootDatum cartanAffineA 1;
Ba1 = weightBasis Da1;
Bca1 = coweightBasis Da1;
assert(apply(Ba1,toString) === {"(1, 0, 0)","(0, 1, 0)","(0, 0, 1)"});
assert(apply(Bca1,toString) === {"(0, 0, 1)","(0, 1/2, 1)","(1, 1, 0)"});
assert(Ba1#0 === affineWeightFromDynkinLabels(Da1,{1,0},0));
assert(Ba1#1 === affineWeightFromDynkinLabels(Da1,{0,1},0));
assert(Ba1#2 === toWeight affineNullRoot Da1);
assert(Bca1#2 === toCoweight affineCentralElement Da1);
assert(toString weightFromRealizationCoordinates(Da1,{-1,2,-1}) ===
    "(-1, 2, -1)");
assert(toString kmRoot(Da1,{1,0}) === "(1, 0)");
assert(toString kmCoroot(Da1,{0,1}) === "(0, 1)");
assert(toString kmRoot(Da1,{-3,-3}) === "(-3, -3)");
assert(toString kmCoroot(Da1,{2,2}) === "(2, 2)");
assert(toString kmWeight(Da1,{0,0}) === "(0, 0) mod ann(h')");
assert(toString kmCoweight(Da1,{0,0}) === "(0, 0) mod ann(Q)");

-- A rational change of realization coordinates must not lose the null root
-- even though the output uses the given ambient coordinate tuple.
S = matrix(QQ,{{1,1/2,0},{0,2,1},{1,0,1}});
SRa1 = matrix(QQ,(realization Da1)#"simpleRootCoordinates");
SCa1 = matrix(QQ,(realization Da1)#"simpleCorootCoordinates");
Da1custom = rootDatum realization(cartanDatum Da1,SRa1*S,SCa1*transpose inverse S);
Bcustom = weightBasis Da1custom;
assert(Bcustom#2 === toWeight affineNullRoot Da1custom);
assert(all(take(Bcustom,2),w -> affineDegree(w,affineDerivation Da1custom) == 0));
assert((coweightBasis Da1custom)#2 === toCoweight affineCentralElement Da1custom);
assert(toString affineNullRoot Da1custom === "(1, 1)");

-- Corank two: each invisible direction remains separately visible.
Ar2 = (cartanAffineA 1) ++ (cartanAffineA 1);
Dr2 = rootDatum Ar2;
Br2 = weightBasis Dr2;
assert(#Br2 == 6);
assert(apply(drop(Br2,4),realizationCoordinates) ===
    {{0,0,0,0,1,0},{0,0,0,0,0,1}});
assert(all(drop(Br2,4),w -> datumCoordinates w === {0,0,0,0}));
assert(toString(Br2#4 + Br2#5) === "(0, 0, 0, 0, 1, 1)");
assert(Br2#4 =!= Br2#5);
assert(toString(Br2#4) =!= toString(Br2#5));

-- Nonminimal realizations, including a pure torus, use inspectable complete
-- supplementary bases.  No invisible direction is discarded in output.
Ra1nonminimal = realization(cartanDatum Da1,
    SRa1 | matrix mutableMatrix(QQ,2,1),
    SCa1 | matrix mutableMatrix(QQ,2,1));
Da1nonminimal = rootDatum Ra1nonminimal;
assert(apply(weightBasis Da1nonminimal,toString) ===
    {"(1, 0, 0, 0)","(0, 1, 0, 0)","(0, 0, 1, 0)","(0, 0, 0, 1)"});
assert(toString weightFromRealizationCoordinates(Da1nonminimal,{1,0,2,-3}) ===
    "(1, 0, 2, -3)");
Rtorus = realization(Dzero,matrix mutableMatrix(QQ,0,2),matrix mutableMatrix(QQ,0,2));
Dtorus = rootDatum Rtorus;
assert(apply(weightBasis Dtorus,toString) === {"(1, 0)","(0, 1)"});
assert(apply(coweightBasis Dtorus,toString) === {"(1, 0)","(0, 1)"});
assert(toString weightFromRealizationCoordinates(Dtorus,{2,-3}) ===
    "(2, -3)");
assert(toString coweightFromRealizationCoordinates(Dtorus,{1/2,1}) ===
    "(1/2, 1)");
assert(toString kmWeight(Dtorus,{}) === "() mod ann(h')");

-- Explicit affine node labels do not change tuple order.
Da1named = rootDatum cartanDatum(cartanAffineA 1,NodeLabels=>{"affine","finite"});
assert(toString((weightBasis Da1named)#0) === "(1, 0, 0)");
assert(toString kmRoot(Da1named,{1,0}) === "(1, 0)");

-- A simultaneous permutation never silently renumbers coefficient tuples.
Atw = matrix{{2,-1},{-4,2}};
Dtw = rootDatum Atw;
DtwPermuted = rootDatum submatrix(Atw,{1,0},{1,0});
assert(apply(take(weightBasis Dtw,2),datumCoordinates) === {{1,0},{0,1}});
assert(apply(take(weightBasis DtwPermuted,2),datumCoordinates) === {{1,0},{0,1}});
assert(toString kmRoot(DtwPermuted,{0,1}) === "(0, 1)");
assert(toString kmRoot(DtwPermuted,{1,0}) === "(1, 0)");
assert((weightBasis DtwPermuted)#2 === toWeight affineNullRoot DtwPermuted);

-- Arbitrary node names never alter numerical tuples or coordinate order.
Dspecial = rootDatum cartanDatum(cartanA 3,NodeLabels=>{-1,1/2});
assert(toString kmWeight(Dspecial,{1,2}) === "(1, 2)");
labelSymbol = symbol displayLabel;
Dcollision = rootDatum cartanDatum(cartanA 3,NodeLabels=>{"displayLabel",labelSymbol});
assert(apply(weightBasis Dcollision,toString) ===
    {"(1, 0)","(0, 1)"});

-- In E7^(1) the distinguished affine node is not the first canonical node.
DE7 = rootDatum cartanAffineE 7;
TE7 = affineTypeData DE7;
affPos = TE7#"standardAffineNode";
assert(affineDegree((weightBasis DE7)#(affPos-1),affineDerivation DE7) == 0);
assert(datumCoordinates((weightBasis DE7)#(affPos-1)) ===
    apply(8,i -> if i == affPos-1 then 1 else 0));
assert(toString kmRoot(DE7,{1,0,0,0,0,0,0,0}) === "(1, 0, 0, 0, 0, 0, 0, 0)");

-- Building display bases never mutates equality-bearing data.
hDisplayBefore = hash fullWeight1;
displayBasis = weightBasis RDaff;
displayString = toString fullWeight1;
assert(hash fullWeight1 === hDisplayBefore);
assert((new HashTable from {fullWeight1 => 47})#fullWeight1Copy === 47);

-- Accessor caching must not mutate the immutable datum or change its hash.
hBefore = hash Dnamed;
dummyMatrix = datumCartanMatrix Dnamed;
assert(hash Dnamed === hBefore);
assert((new HashTable from {Dnamed => 41})#Dnamed === 41);

///


-------------------------------------------------------------------------
-- Affine Structure Tests
-------------------------------------------------------------------------

-- AffineStructureTests.m2
-- Rigorous package tests for AffineStructure.m2.

TEST ///

mustErrorAffineDraft = f -> try (f(); false) else true;
affineErrorMessageDraft = f ->
    try (f(); null) except err do toString err;

-- Simultaneously permute rows and columns.  p is the list of old 0-based
-- node positions in the desired new order.
permuteAffineDraft = (A,p) -> submatrix(A,p,p);

-- A_1^(1).
A11 := cartanAffineA 1;
assert(affineMarks A11 === {1,1});
assert(affineComarks A11 === {1,1});
assert(A11 * transpose matrix{affineMarks A11} == 0);
assert(transpose(A11) * transpose matrix{affineComarks A11} == 0);
D11 := affineTypeData A11;
assert(D11#"recognized");
assert(D11#"type" === "A_1^(1)");
assert(set(D11#"specialNodes") === set{1,2});
assert(affineLevel(A11,{3,5}) == 8);
assert(numrows((affineFinitePart A11)#"cartanMatrix") == 1);

-- A_2^(1): all marks and comarks are one.  Restriction after deleting
-- node 1 is the A_2 finite part on nodes {2,3}.
A21 := cartanAffineA 2;
D21 := affineTypeData A21;
assert(D21#"recognized" and D21#"type" === "A_2^(1)");
assert(affineMarks A21 === {1,1,1});
assert(affineComarks A21 === {1,1,1});
assert(affineLevel(A21,{5,2,7}) == 14);
C21 := affineClassicalPart(A21,{5,2,7},1);
assert(C21#"nodes" === {2,3});
assert(C21#"dynkinLabels" === {2,7});
assert(cartanType(C21#"cartanMatrix") === "finite");

-- A_2^(2): right and left kernels differ, so this fixture detects a
-- marks/comarks convention reversal.
A22 := affineCartanA2Twisted 1;
assert(A22 === matrix{{2,-1},{-4,2}});
assert(affineMarks A22 === {1,2});
assert(affineComarks A22 === {2,1});
assert(A22 * transpose matrix{{1,2}} == 0);
assert(transpose(A22) * transpose matrix{{2,1}} == 0);
D22 := affineTypeData A22;
assert(D22#"recognized" and D22#"type" === "A_2^(2)");
assert(D22#"twistOrder" == 2 and D22#"kind" === "twisted");
assert(D22#"specialNodes" === {1});
assert(D22#"dualSpecialNodes" === {2});
assert(D22#"dualSpecialNodes" === (affineTypeData transpose A22)#"specialNodes");
assert(affineLevel(A22,{1,0}) == 2);
assert(affineLevel(A22,{0,1}) == 1);

-- D_4^(3), dual to G_2^(1).
D43 := affineCartanD4Twisted();
DD43 := affineTypeData D43;
assert(DD43#"recognized" and DD43#"type" === "D_4^(3)");
assert(DD43#"twistOrder" == 3);
assert(affineMarks D43 === {1,1,2});
assert(affineComarks D43 === {1,3,2});
-- The second node has mark one, but is not special: deleting it gives A_2
-- instead of the associated G_2 classical datum.
assert(DD43#"markOneNodes" === {1,2});
assert(DD43#"specialNodes" === {1});
assert((affineFinitePart(D43,2))#"cartanMatrix" === cartanA 3);
assert(D43 * transpose matrix{affineMarks D43} == 0);
assert(transpose(D43) * transpose matrix{affineComarks D43} == 0);

-- Simultaneous node permutation preserves type and permutes marks/comarks.
p := {2,0,1};
D43p := permuteAffineDraft(D43,p);
Dp := affineTypeData D43p;
assert(Dp#"recognized" and Dp#"type" === "D_4^(3)");
assert(affineMarks D43p === apply(p,i -> (affineMarks D43)#i));
assert(affineComarks D43p === apply(p,i -> (affineComarks D43)#i));
assert(sort(Dp#"canonicalNodeOrder") === {1,2,3});
assert(Dp#"specialNodes" === {2});

-- Langlands duality swaps marks and comarks.  G_2^(1) and D_4^(3)
-- are the order-three dual pair; A_2^(2) is self-dual up to node reversal.
G21 := cartanAffineG2();
DG21 := affineTypeData G21;
assert(DG21#"type" === "G_2^(1)");
assert((affineTypeData transpose G21)#"type" === "D_4^(3)");
assert(affineMarks(transpose G21) === affineComarks G21);
assert(affineComarks(transpose G21) === affineMarks G21);
assert((affineTypeData transpose A22)#"type" === "A_2^(2)");
assert(affineMarks(transpose A22) === affineComarks A22);

-- A higher-rank order-two dual pair.
B31 := cartanAffineB 3;
ABdual := transpose B31;
assert((affineTypeData B31)#"type" === "B_3^(1)");
assert((affineTypeData ABdual)#"type" === "A_5^(2)");
assert(affineMarks ABdual === affineComarks B31);
assert(affineComarks ABdual === affineMarks B31);

-- One fixture from every remaining twisted family.  The literal kernel
-- vectors are table data, so these tests do not derive their expected
-- answers by calling the implementation under test.
twistedFixtures := {
    {affineCartanA2Twisted 2, "A_4^(2)", 2, {1,2,2}, {2,2,1}},
    {affineCartanAOddTwisted 3, "A_5^(2)", 2,
        {1,1,2,1}, {1,1,2,2}},
    {affineCartanDTwisted 3, "D_4^(2)", 2,
        {1,1,1,1}, {1,2,2,1}},
    {affineCartanE6Twisted(), "E_6^(2)", 2,
        {1,2,3,2,1}, {1,2,3,4,2}}
};
for fixture in twistedFixtures do (
    A := fixture#0;
    typeName := fixture#1;
    twist := fixture#2;
    expectedMarks := fixture#3;
    expectedComarks := fixture#4;
    data := affineTypeData A;
    assert(data#"recognized" and data#"type" === typeName);
    assert(data#"twistOrder" == twist and data#"kind" === "twisted");
    assert(affineMarks A === expectedMarks);
    assert(affineComarks A === expectedComarks);
    assert(A * transpose matrix{expectedMarks} == 0);
    assert(transpose(A) * transpose matrix{expectedComarks} == 0);
);

-- In D_4^(2), all four marks are one, but only the two end deletions
-- produce the associated B_3 datum.  This guards the same distinction in
-- a family where there is more than one genuinely special node.
D42data := affineTypeData affineCartanDTwisted 3;
assert(D42data#"markOneNodes" === {1,2,3,4});
assert(set(D42data#"specialNodes") === set{1,4});
assert(set(D42data#"dualSpecialNodes") === set{1,4});
for n from 1 to 4 do (
    AevenTwisted := affineCartanA2Twisted n;
    evenTwistedData := affineTypeData AevenTwisted;
    assert(evenTwistedData#"specialNodes" === {1});
    assert(evenTwistedData#"dualSpecialNodes" === {n+1});
    assert(evenTwistedData#"dualSpecialNodes" ===
        (affineTypeData transpose AevenTwisted)#"specialNodes");
);

-- Representative untwisted exceptional and classical families are also
-- recognized without relying on constructor identity or node order.
untwistedFixtures := {
    {cartanAffineD 4, "D_4^(1)"},
    {cartanAffineE 6, "E_6^(1)"},
    {cartanAffineF4(), "F_4^(1)"},
    {cartanAffineG2(), "G_2^(1)"}
};
for fixture in untwistedFixtures do (
    A := fixture#0;
    data := affineTypeData A;
    assert(data#"recognized" and data#"type" === fixture#1);
    assert(data#"kind" === "untwisted" and data#"twistOrder" == 1);
);

-- The package's E-affine constructors use tripod numbering.  These
-- fixtures make sure the distinguished affine node is not incorrectly
-- assumed to be the first matrix row, especially in E_7^(1) and E_8^(1).
E61data := affineTypeData cartanAffineE 6;
E71data := affineTypeData cartanAffineE 7;
E81data := affineTypeData cartanAffineE 8;
assert(set(E61data#"specialNodes") === set{1,5,7});
assert(set(E71data#"specialNodes") === set{5,8});
assert(E81data#"specialNodes" === {9});
assert((E61data#"finitePart"#"cartanData"#"components"#0#"label") === "E_6");
assert((E71data#"finitePart"#"cartanData"#"components"#0#"label") === "E_7");
assert((E81data#"finitePart"#"cartanData"#"components"#0#"label") === "E_8");

-- Every node deletion of an affine diagram is finite, although it need
-- not be connected.  In affine D_4, deleting the center leaves A_1^4.
D41data := affineTypeData cartanAffineD 4;
assert(D41data#"finiteDeletionNodes" === {1,2,3,4,5});
assert(#(D41data#"connectedFiniteDeletionNodes") == 4);

-- The low-rank coincidences are retained explicitly rather than silently
-- pretending that the abstract GCM distinguishes the two names.
assert(set((affineTypeData cartanAffineC 2)#"typeAliases")
    === set{"B_2^(1)","C_2^(1)"});
assert(set((affineTypeData affineCartanAOddTwisted 2)#"typeAliases")
    === set{"A_3^(2)","D_3^(2)"});
assert((affineTypeData cartanAffineC 2)#"typeIsAmbiguous");
assert((affineTypeData affineCartanAOddTwisted 2)#"typeIsAmbiguous");
assert(not (affineTypeData cartanAffineA 3)#"typeIsAmbiguous");

-- Matrix-form Dynkin labels are accepted by the classical restriction.
C21matrix := affineClassicalPart(A21,matrix{{5,2,7}},1);
assert(C21matrix#"dynkinLabels" === {2,7});
C21column := affineClassicalPart(A21,transpose matrix{{5,2,7}},1);
assert(C21column#"dynkinLabels" === {2,7});
assert(affineLevel(A21,transpose matrix{{5,2,7}}) == 14);

-- The draft must not invent a derivation coordinate from Dynkin labels.
assert(mustErrorAffineDraft(() -> affineDegree(A11,{1,0})));
assert(mustErrorAffineDraft(() -> weightFromRealizationCoordinates(A11,{1,0})));

-- Domain checks: finite and decomposable affine aggregates are rejected.
assert(mustErrorAffineDraft(() -> affineMarks(cartanA 3)));
Amixed := (cartanAffineA 1) ++ (cartanA 2);
assert(mustErrorAffineDraft(() -> affineTypeData Amixed));

-- Systematic family sweep.  This catches rank-boundary mistakes in the
-- constructors and recognition table, while reversal tests verify that no
-- result depends on the package's chosen node order.
classificationFixtures := {};
for n from 1 to 8 do
    classificationFixtures = append(classificationFixtures,
        {cartanAffineA n,"A_" | toString n | "^(1)"});
for n from 2 to 8 do (
    classificationFixtures = append(classificationFixtures,
        {cartanExtendUntwisted(cartanB n),"B_" | toString n | "^(1)"});
    classificationFixtures = append(classificationFixtures,
        {cartanAffineC n,"C_" | toString n | "^(1)"});
);
for n from 4 to 8 do
    classificationFixtures = append(classificationFixtures,
        {cartanAffineD n,"D_" | toString n | "^(1)"});
for n from 1 to 7 do
    classificationFixtures = append(classificationFixtures,
        {affineCartanA2Twisted n,"A_" | toString(2*n) | "^(2)"});
for n from 2 to 7 do (
    classificationFixtures = append(classificationFixtures,
        {affineCartanAOddTwisted n,"A_" | toString(2*n-1) | "^(2)"});
    classificationFixtures = append(classificationFixtures,
        {affineCartanDTwisted n,"D_" | toString(n+1) | "^(2)"});
);
classificationFixtures = join(classificationFixtures,{
    {cartanAffineE 6,"E_6^(1)"},
    {cartanAffineE 7,"E_7^(1)"},
    {cartanAffineE 8,"E_8^(1)"},
    {cartanAffineF4(),"F_4^(1)"},
    {cartanAffineG2(),"G_2^(1)"},
    {affineCartanE6Twisted(),"E_6^(2)"},
    {affineCartanD4Twisted(),"D_4^(3)"}
});
for fixture in classificationFixtures do (
    A := fixture#0;
    expectedType := fixture#1;
    data := affineTypeData A;
    assert(member(expectedType,data#"typeAliases"));
    assert(data#"nodeCount" == numrows A);
    assert(data#"rank" == data#"nodeCount");
    assert(data#"cartanMatrixRank" == numrows(A)-1);
    assert(data#"corank" == 1);
    assert(data#"finiteRank" == numrows(A)-1);
    assert(data#"finiteFamily" =!= null);
    assert(member(data#"standardAffineNode",data#"specialNodes"));
    assert(#(data#"finiteDeletionNodes") == numrows A);
    assert((data#"finitePart")#"type" === "finite");
    assert((data#"finitePart")#"connected");
    a := data#"marks";
    av := data#"comarks";
    assert(all(a,x -> x > 0) and all(av,x -> x > 0));
    assert(A * transpose matrix{a} == 0);
    assert(transpose(A) * transpose matrix{av} == 0);
    g := 0;
    for x in a do g = gcd(g,x);
    assert(abs g == 1);
    g = 0;
    for x in av do g = gcd(g,x);
    assert(abs g == 1);

    reversal := reverse toList(0..numrows(A)-1);
    reversedData := affineTypeData permuteAffineDraft(A,reversal);
    assert(member(expectedType,reversedData#"typeAliases"));
    assert((reversedData#"marks") === apply(reversal,i -> a#i));
    assert((reversedData#"comarks") === apply(reversal,i -> av#i));
);

-- Additional input-shape and node-boundary checks.
assert(mustErrorAffineDraft(() -> affineLevel(A21,{1,2})));
assert(mustErrorAffineDraft(() -> affineLevel(A21,matrix{{1,2},{3,4}})));
assert(mustErrorAffineDraft(() -> affineFinitePart(A21,0)));
assert(mustErrorAffineDraft(() -> affineFinitePart(A21,4)));
assert(affineMarks(A21 ** QQ) === {1,1,1});

------------------------------------------------------------------------
-- Parent-aware affine elements and complete realization coordinates
------------------------------------------------------------------------

RD21 := rootDatum A21;
delta21 := affineNullRoot RD21;
central21 := affineCentralElement RD21;
assert(instance(delta21,KMRoot));
assert(instance(central21,KMCoroot));
assert(parentDatum delta21 === RD21);
assert(parentDatum central21 === RD21);
assert(datumCoordinates delta21 === {1,1,1});
assert(datumCoordinates central21 === {1,1,1});
assert(affineMarks(cartanDatum A21) === {1,1,1});
assert(affineComarks(realization RD21) === {1,1,1});
assert(affineComarks RD21 === {1,1,1});
assert(affineNullRoot A21 === {1,1,1});

-- Delta and c both have zero simple pairings but are nonzero in the full
-- minimal realization.  This is precisely the information a bare list of
-- Dynkin labels loses.
deltaAsWeight := toWeight delta21;
centralAsCoweight := toCoweight central21;
assert(datumCoordinates deltaAsWeight === {0,0,0});
assert(datumCoordinates centralAsCoweight === {0,0,0});
assert(any(realizationCoordinates deltaAsWeight,x -> x != 0));
assert(any(realizationCoordinates centralAsCoweight,x -> x != 0));
assert(pairing(delta21,central21) == 0);

lambdaClass := kmWeight(RD21,{5,2,7});
assert(affineLevel lambdaClass == 14);
assert(affineLevel lambdaClass == pairing(lambdaClass,central21));

-- Build a complete normalized derivation d with alpha_1(d)=1 and all
-- other simple-root evaluations zero.  Here node 1 is the affine node.
makeNormalizedAffineDerivation := (D,node) -> (
    SR := matrix(QQ,(realization D)#"simpleRootCoordinates");
    r := numrows SR;
    target := matrix(QQ,{apply(r,i -> if i == node-1 then 1 else 0)});
    ambient := target * inverse(SR * transpose SR) * SR;
    coweightFromRealizationCoordinates(D,flatten entries ambient)
);
d21 := makeNormalizedAffineDerivation(RD21,1);
assert(instance(d21,KMCoweight));
assert(datumCoordinates d21 === {1,0,0});
assert(pairing(delta21,d21) == 1);

ambientWeightCoordinates := {2,-1,3,5};
lambdaFull := weightFromRealizationCoordinates(RD21,ambientWeightCoordinates);
assert(instance(lambdaFull,KMWeight));
assert(realizationCoordinates lambdaFull === ambientWeightCoordinates);
degreeExpected := sum apply(ambientWeightCoordinates,
    realizationCoordinates d21,(x,y) -> x*y);
assert(affineDegree(lambdaFull,d21) == degreeExpected);
assert(affineDegree(lambdaFull+deltaAsWeight,d21) == degreeExpected+1);
assert(affineLevel(lambdaFull+deltaAsWeight) == affineLevel lambdaFull);

-- Neither half of a singular Dynkin-label class supplies the omitted
-- realization direction.  Supplying an unnormalized or foreign derivation
-- must also fail rather than silently choose a grading.
assert(mustErrorAffineDraft(() -> affineDegree(lambdaClass,d21)));
assert(mustErrorAffineDraft(() ->
    affineDegree(lambdaFull,kmCoweight(RD21,{1,0,0}))));
doubledD21 := coweightFromRealizationCoordinates(
    RD21,apply(realizationCoordinates d21,x -> 2*x));
assert(pairing(delta21,doubledD21) == 2);
assert(mustErrorAffineDraft(() -> affineDegree(lambdaFull,doubledD21)));
RD21adjoint := rootDatum(A21,GlobalForm=>"Adjoint");
d21adjoint := makeNormalizedAffineDerivation(RD21adjoint,1);
assert(mustErrorAffineDraft(() -> affineDegree(lambdaFull,d21adjoint)));

-- Twisted data retain the same marks/comarks convention in typed form.
RD22 := rootDatum A22;
assert(datumCoordinates(affineNullRoot RD22) === {1,2});
assert(datumCoordinates(affineCentralElement RD22) === {2,1});
assert(affineLevel(kmWeight(RD22,{1,0})) == 2);
assert(affineLevel(kmWeight(RD22,{0,1})) == 1);

-- Custom node labels belong to the parent; coordinate order remains the
-- datum's declared node order.
D21named := cartanDatum(A21,NodeLabels=>{"affine","left","right"});
RD21named := rootDatum D21named;
assert((parentDatum(affineNullRoot RD21named))#"cartanDatum" === D21named);
assert(datumCoordinates(affineNullRoot RD21named) === {1,1,1});

-- Every datum flavor supports the same affine structural query while
-- retaining the supplied typed parent.  The RootDatum result additionally
-- contains honest root and coroot elements, not just coefficient lists.
R21named := realization RD21named;
typedTypeCartan := affineTypeData D21named;
typedTypeRealization := affineTypeData R21named;
typedTypeRoot := affineTypeData RD21named;
assert(typedTypeCartan#"cartanDatum" === D21named);
assert(typedTypeCartan#"nodeLabels" === {"affine","left","right"});
assert(typedTypeCartan#"typedNodeFormat" === "Labels");
assert(typedTypeCartan#"standardAffineNodeLabel" === "affine");
assert(typedTypeCartan#"standardAffineNodeTyped" === "affine");
assert(typedTypeCartan#"specialNodeLabels" ===
    {"affine","left","right"});
assert(apply(typedTypeCartan#"specialNodeRecords",R -> R#"position")
    === {1,2,3});
assert(apply(typedTypeCartan#"specialNodeRecords",R -> R#"label")
    === {"affine","left","right"});
assert(apply(typedTypeCartan#"nullRootByNode",R -> R#"coefficient")
    === {1,1,1});
assert(apply(typedTypeCartan#"centralElementByNode",R -> R#"coefficient")
    === {1,1,1});
assert(instance(typedTypeCartan#"finitePart"#"cartanDatum",CartanDatum));
assert((typedTypeCartan#"finitePart")#"ambientCartanDatum" === D21named);
assert(typedTypeRealization#"cartanDatum" === D21named);
assert(typedTypeRealization#"realization" === R21named);
assert(instance(typedTypeRealization#"finitePart"#"realization",Realization));
assert((typedTypeRealization#"finitePart")#"ambientRealization" === R21named);
assert(typedTypeRoot#"rootDatum" === RD21named);
assert(typedTypeRoot#"realization" === R21named);
assert(instance(typedTypeRoot#"finitePart"#"rootDatum",RootDatum));
assert((typedTypeRoot#"finitePart")#"ambientRootDatum" === RD21named);
assert(instance(typedTypeRoot#"nullRootElement",KMRoot));
assert(instance(typedTypeRoot#"centralElement",KMCoroot));
assert(parentDatum(typedTypeRoot#"nullRootElement") === RD21named);
assert(parentDatum(typedTypeRoot#"centralElement") === RD21named);
assert(datumCoordinates(typedTypeRoot#"nullRootElement") === {1,1,1});
assert(datumCoordinates(typedTypeRoot#"centralElement") === {1,1,1});

-- Typed finite-part records inherit the retained node labels and remember
-- both the finite object and its affine parent.  For a RootDatum, the finite
-- global form is explicitly unspecified because the ambient global form
-- does not canonically determine a finite one.
finitePartCartan := affineFinitePart(D21named,1);
assert(instance(finitePartCartan#"cartanDatum",CartanDatum));
assert(finitePartCartan#"cartanDatum" === finitePartCartan#"finiteCartanDatum");
assert(finitePartCartan#"ambientCartanDatum" === D21named);
assert(finitePartCartan#"nodeLabels" === {"left","right"});
assert(finitePartCartan#"deletedNodeLabel" === "affine");
assert(datumCartanMatrix(finitePartCartan#"cartanDatum") == cartanA 3);

finitePartRealization := affineFinitePart(R21named,1);
assert(instance(finitePartRealization#"realization",Realization));
assert(finitePartRealization#"realization" ===
    finitePartRealization#"finiteRealization");
assert(finitePartRealization#"ambientRealization" === R21named);
assert(cartanDatum(finitePartRealization#"realization") ===
    finitePartRealization#"cartanDatum");

finitePartRoot := affineFinitePart(RD21named,1);
assert(instance(finitePartRoot#"rootDatum",RootDatum));
assert(finitePartRoot#"rootDatum" === finitePartRoot#"finiteRootDatum");
assert(finitePartRoot#"ambientRootDatum" === RD21named);
assert(cartanDatum(finitePartRoot#"rootDatum") ===
    finitePartRoot#"cartanDatum");
assert((finitePartRoot#"rootDatum")#"globalForm"#"name" === "Unspecified");
assert((affineFinitePart D21named)#"deletedNode" == 1);
assert((affineFinitePart R21named)#"deletedNode" == 1);
assert((affineFinitePart RD21named)#"deletedNode" == 1);

-- List, row-matrix, and column-matrix Dynkin labels give the same level for
-- CartanDatum, Realization, and RootDatum inputs.
assert(affineLevel(D21named,{5,2,7}) == 14);
assert(affineLevel(D21named,matrix{{5,2,7}}) == 14);
assert(affineLevel(R21named,transpose matrix{{5,2,7}}) == 14);
assert(affineLevel(RD21named,{5,2,7}) == 14);
assert(affineLevel(RD21named,matrix{{5,2,7}}) == 14);

-- Restricting a KMWeight produces an actual finite semisimple KMWeight.
-- The record also retains the original weight, parent, deletion, and labels,
-- so no information about how the restriction was formed is lost.
namedWeight := kmWeight(RD21named,{5,2,7});
typedClassical := affineClassicalPart namedWeight;
assert(instance(typedClassical#"weight",KMWeight));
assert(typedClassical#"weight" === typedClassical#"finiteWeight");
assert(datumCoordinates(typedClassical#"weight") === {2,7});
assert(parentDatum(typedClassical#"weight") ===
    typedClassical#"finiteRootDatum");
assert(cartanType datumCartanMatrix parentDatum(typedClassical#"weight")
    === "finite");
assert(typedClassical#"ambientWeight" === namedWeight);
assert(typedClassical#"ambientRootDatum" === RD21named);
assert(typedClassical#"deletedNode" == 1);
assert(typedClassical#"deletedNodeLabel" === "affine");
assert(typedClassical#"nodes" === {2,3});
assert(typedClassical#"nodeLabels" === {"left","right"});
assert(typedClassical#"dynkinLabels" === {2,7});

-- Restriction depends only on Dynkin labels, but a complete ambient affine
-- weight is retained verbatim in the metadata, including its delta direction.
namedFullWeight := weightFromRealizationCoordinates(RD21named,{2,-1,3,5});
typedFullClassical := affineClassicalPart(namedFullWeight,1);
assert(datumCoordinates(typedFullClassical#"weight") ===
    apply(typedFullClassical#"nodes",
        i -> (datumCoordinates namedFullWeight)#(i-1)));
assert(typedFullClassical#"ambientWeight" === namedFullWeight);
assert(realizationCoordinates(typedFullClassical#"ambientWeight") ===
    {2,-1,3,5});

-- Typed overloads enforce the same affine-domain and node-boundary checks as
-- their matrix counterparts.
assert(mustErrorAffineDraft(() -> affineFinitePart(D21named,0)));
assert(mustErrorAffineDraft(() -> affineFinitePart(R21named,4)));
assert(mustErrorAffineDraft(() -> affineClassicalPart(namedWeight,0)));
assert(mustErrorAffineDraft(() -> affineClassicalPart(namedWeight,4)));
assert(affineErrorMessageDraft(() ->
    affineClassicalPart(A21,{5,2,7},0)) ===
    "affineClassicalPart: node must be between 1 and rank");

------------------------------------------------------------------------
-- Typed node round trips, canonical derivations, and degree-sensitive lifts
------------------------------------------------------------------------

-- Integer labels colliding with positional node numbers must be returned as
-- records.  Feeding the record back deletes the intended node, whereas a
-- bare integer in 1..rank intentionally retains its positional meaning.
D21collision := cartanDatum(A21,NodeLabels=>{2,1,3});
RD21collision := rootDatum D21collision;
T21collision := affineTypeData RD21collision;
assert(T21collision#"typedNodeFormat" === "Records");
collisionAffineRecord := T21collision#"standardAffineNodeTyped";
assert(instance(collisionAffineRecord,HashTable));
assert(collisionAffineRecord#"position" == 1);
assert(collisionAffineRecord#"label" == 2);
assert((affineFinitePart(D21collision,collisionAffineRecord))#"deletedNode"
    == 1);
assert((affineFinitePart(D21collision,2))#"deletedNode" == 2);
assert((affineClassicalPart(
    kmWeight(RD21collision,{5,2,7}),collisionAffineRecord))#"deletedNode"
    == 1);
assert(mustErrorAffineDraft(() -> affineFinitePart(
    D21collision,new HashTable from {
        "position"=>1,"label"=>1
    })));

-- Out-of-range integers can be genuine custom labels and are therefore
-- resolved after the package-wide positional check.
D21wideLabels := cartanDatum(A21,NodeLabels=>{10,20,30});
RD21wideLabels := rootDatum D21wideLabels;
assert((affineFinitePart(D21wideLabels,10))#"deletedNode" == 1);
assert((affineClassicalPart(
    kmWeight(RD21wideLabels,{5,2,7}),10))#"deletedNode" == 1);

-- The default derivation uses the recognized standard affine node.  An
-- explicit position, label, or record selects another node and scales its
-- simple-root evaluation by the reciprocal mark.
d21named := affineDerivation RD21named;
assert(d21named#"realizationCoordinates" =!= null);
assert(datumCoordinates d21named === {1,0,0});
assert(pairing(affineNullRoot RD21named,d21named) == 1);
assert(datumCoordinates(affineDerivation(RD21named,"left"))
    === {0,1,0});
assert(datumCoordinates(affineDerivation(
    RD21collision,collisionAffineRecord)) === {1,0,0});
d22finite := affineDerivation(RD22,2);
assert(datumCoordinates d22finite === {0,1/2_QQ});
assert(pairing(affineNullRoot RD22,d22finite) == 1);

-- A complete lift is uniquely determined by its Dynkin labels and one
-- degree in a minimal corank-one realization.  Lists, rows, columns, and
-- rational data must all produce the same mathematical element.
lifted21 := affineWeightFromDynkinLabels(
    RD21named,{5,2,7},3,d21named);
assert(datumCoordinates lifted21 === {5,2,7});
assert(lifted21#"realizationCoordinates" =!= null);
assert(affineDegree(lifted21,d21named) == 3);
defaultLift21 := affineWeightFromDynkinLabels(RD21named,{5,2,7},3);
assert(defaultLift21 === lifted21);
rowLift21 := affineWeightFromDynkinLabels(
    RD21named,matrix(QQ,{{5,2,7}}),3_QQ/2,d21named);
columnLift21 := affineWeightFromDynkinLabels(
    RD21named,transpose matrix(QQ,{{5,2,7}}),3_QQ/2,d21named);
listQQLift21 := affineWeightFromDynkinLabels(
    RD21named,{5_QQ,2_QQ,7_QQ},3_QQ/2,d21named);
assert(rowLift21 === columnLift21);
assert(rowLift21 === listQQLift21);
assert(affineDegree(rowLift21,d21named) == 3_QQ/2);

-- Adding c changes the chosen normalized derivation but not its
-- normalization.  The explicit-derivation lift respects that choice.
d21shifted := d21named + toCoweight affineCentralElement RD21named;
assert(pairing(affineNullRoot RD21named,d21shifted) == 1);
shiftedLift21 := affineWeightFromDynkinLabels(
    RD21named,{5,2,7},-4,d21shifted);
assert(datumCoordinates shiftedLift21 === {5,2,7});
assert(affineDegree(shiftedLift21,d21shifted) == -4);

-- Roots compose naturally with both level and degree.
assert(affineLevel(affineNullRoot RD21named) == 0);
assert(affineDegree(affineNullRoot RD21named,d21named) == 1);

-- Incompleteness, bad normalization, foreign parents, and nonminimal
-- realizations are rejected before an arbitrary grading choice can leak in.
assert(mustErrorAffineDraft(() ->
    affineDegree(kmWeight(RD21named,{5,2,7}),d21named)));
assert(affineErrorMessageDraft(() ->
    affineDegree(kmWeight(RD21named,{5,2,7}),d21named)) ===
    concatenate(
        "affineDegree: the weight is incomplete; Dynkin labels determine ",
        "only its class modulo the delta direction. Use ",
        "affineWeightFromDynkinLabels or weightFromRealizationCoordinates to construct ",
        "a complete KMWeight"));
assert(mustErrorAffineDraft(() ->
    affineDegree(lifted21,kmCoweight(RD21named,{1,0,0}))));
assert(affineErrorMessageDraft(() ->
    affineDegree(lifted21,kmCoweight(RD21named,{1,0,0}))) ===
    concatenate(
        "affineDegree: the derivation is incomplete; simple-root ",
        "evaluations determine it only modulo the central direction. ",
        "Use affineDerivation or coweightFromRealizationCoordinates"));
assert(mustErrorAffineDraft(() ->
    affineWeightFromDynkinLabels(
        RD21named,{5,2,7},0,kmCoweight(RD21named,{1,0,0}))));
assert(mustErrorAffineDraft(() ->
    affineWeightFromDynkinLabels(RD21named,{5,2,7},0,d21)));
assert(mustErrorAffineDraft(() -> affineDegree(lifted21,{1,0,0})));
assert(affineErrorMessageDraft(() -> affineDegree(lifted21,{1,0,0}))
    === concatenate(
        "affineDegree: expected a complete KMWeight or KMRoot and a ",
        "complete normalized KMCoweight derivation belonging to the same ",
        "affine RootDatum"));
assert(affineErrorMessageDraft(() ->
    affineWeightFromDynkinLabels(
        RD21named,{5,2,7},"not a degree",d21named)) ===
    "affineWeightFromDynkinLabels: degree must be rational");

R21namedData := realization RD21named;
extraZeroColumn := matrix mutableMatrix(
    QQ,(cartanDatum RD21named)#"rank",1);
R21nonminimal := realization(
    cartanDatum RD21named,
    matrix(QQ,R21namedData#"simpleRootCoordinates") | extraZeroColumn,
    matrix(QQ,R21namedData#"simpleCorootCoordinates") | extraZeroColumn);
RD21nonminimal := rootDatum(
    R21nonminimal,GlobalForm=>"Unspecified");
d21nonminimal := coweightFromRealizationCoordinates(
    RD21nonminimal,append(realizationCoordinates d21named,0));
assert(pairing(affineNullRoot RD21nonminimal,d21nonminimal) == 1);
assert(mustErrorAffineDraft(() -> affineDerivation RD21nonminimal));
assert(mustErrorAffineDraft(() ->
    affineWeightFromDynkinLabels(
        RD21nonminimal,{5,2,7},0,d21nonminimal)));

------------------------------------------------------------------------
-- Matrix-invariant cache behavior
------------------------------------------------------------------------

clearAffineStructureCache();
emptyAffineCache := affineStructureCacheInfo();
assert(emptyAffineCache#"cartanMatrices" == 0);
assert(emptyAffineCache#"marks" == 0);
assert(emptyAffineCache#"comarks" == 0);
assert(emptyAffineCache#"typeData" == 0);
affineMarks A21;
marksCache := affineStructureCacheInfo();
assert(marksCache#"cartanMatrices" == 1);
assert(marksCache#"validatedAffineMatrices" == 1);
assert(marksCache#"marks" == 1);
assert(marksCache#"comarks" == 0);
affineLevel(A21,{5,2,7});
assert((affineStructureCacheInfo())#"comarks" == 1);
affineTypeData A21;
assert((affineStructureCacheInfo())#"typeData" == 1);

cacheLabelsOne := cartanDatum(
    matrix entries A21,NodeLabels=>{"zero","one","two"});
cacheLabelsTwo := cartanDatum(
    matrix entries A21,NodeLabels=>{"affine","left","right"});
cacheTypedOne := affineTypeData cacheLabelsOne;
cacheTypedTwo := affineTypeData cacheLabelsTwo;
assert(cacheTypedOne#"nodeLabels" === {"zero","one","two"});
assert(cacheTypedTwo#"nodeLabels" === {"affine","left","right"});
assert((cacheTypedOne#"finitePart")#"nodeLabels" === {"one","two"});
assert((cacheTypedTwo#"finitePart")#"nodeLabels" === {"left","right"});
clearAffineStructureCache();
assert((affineStructureCacheInfo())#"cartanMatrices" == 0);

finiteRD := rootDatum cartanA 3;
assert(mustErrorAffineDraft(() -> affineNullRoot finiteRD));
assert(mustErrorAffineDraft(() -> affineTypeData cartanDatum finiteRD));
assert(mustErrorAffineDraft(() -> affineFinitePart realization finiteRD));
assert(mustErrorAffineDraft(() -> affineFinitePart finiteRD));
assert(mustErrorAffineDraft(() -> affineLevel(kmWeight(finiteRD,{1,0}))));
assert(realizationCoordinates weightFromRealizationCoordinates(finiteRD,{1,0}) === {1,0});
mixedRD := rootDatum Amixed;
mixedZeroCoordinates := apply((realization mixedRD)#"dimension",i -> 0);
assert(realizationCoordinates weightFromRealizationCoordinates(
    mixedRD,mixedZeroCoordinates) === mixedZeroCoordinates);
assert(mustErrorAffineDraft(() -> affineDegree(A21,{1,0,0})));
assert(mustErrorAffineDraft(() -> weightFromRealizationCoordinates(A21,{1,0,0})));
assert(mustErrorAffineDraft(() -> weightFromRealizationCoordinates(A21,matrix{{1,0,0}})));

print "AffineStructureTests: all fixtures passed";
///


-------------------------------------------------------------------------
-- Weyl Parabolic Tests
-------------------------------------------------------------------------

TEST ///

mustErrorWP = f -> (try (f(); false) else true)

D = rootDatum "A3";
W = weylGroup D;
e = identityWeylElement W;
s1 = simpleWeylReflection(W,1);
s2 = simpleWeylReflection(W,2);
s3 = simpleWeylReflection(W,3);
assert(weylLength e == 0 and reducedWord e === {});
assert(toString e === "1_W");
assert(toString s1 === "s_1");
assert(s1*s1 === e);
assert(s1*s1 == e);
assert((s1*s2)*(s1) === (s2*s1)*s2);
assert((s1*s2)*(s1) == (s2*s1)*s2);
assert(s1*s3 === s3*s1);
assert(s1*s3 == s3*s1);
assert(inverseWeylElement(s1*s2) * (s1*s2) === e);
assert(weylElement(W,{1,2,1}) === weylElement(W,{2,1,2}));
assert(weylElement(W,{1,2,1}) == weylElement(W,{2,1,2}));
assert(bruhatLessOrEqual(s1,s1*s2));
assert(not bruhatLessOrEqual(s1*s2,s1));

-- Mathematical equality requires a common typed Weyl-group parent and
-- agrees with the faithful action on the root lattice.
u121eq = weylElement(W,{1,2,1});
u212eq = weylElement(W,{2,1,2});
u12eq = weylElement(W,{1,2});
assert(u121eq == u212eq and u212eq == u121eq);
assert(not (u121eq == u12eq));
assert(u121eq != u12eq);
assert((u121eq == u212eq) ===
    (weylParent u121eq === weylParent u212eq and
        weylElementMatrix u121eq == weylElementMatrix u212eq));
legacyEq = first weylGroupElements(cartanA 4);
assert(mustErrorWP(() -> (legacyEq == legacyEq)));
assert(mustErrorWP(() -> (legacyEq == e)));

alpha = kmRoot(D,{1,0,0});
assert(weylAction(s1,alpha) === kmRoot(D,{-1,0,0}));
assert(weylAction(s1*s2,alpha) === kmRoot(D,{0,1,0}));
assert(weylAction(s1*s2,alpha) === kmRoot(D,
    flatten entries(weylElementMatrix(s1*s2) * transpose matrix{{1,0,0}})));

lambda = kmWeight(D,{2,-1,3});
assert(datumCoordinates weylAction(s1*s2,lambda)
    === {-1,2,2});
assert(weylAction(e,lambda) === lambda);

h = kmCoroot(D,{1,2,3});
assert(datumCoordinates weylAction(s1*s2,h) ===
    flatten entries(weylElementMatrix(transpose cartanA 4,{1,2}) *
        transpose matrix{{1,2,3}}));

-- The full affine realization direction is retained by the Weyl action.
Daff = rootDatum cartanAffineA 2;
Waff = weylGroup Daff;
full = weightFromRealizationCoordinates(Daff,{1,2,3,7});
acted = weylAction(weylElement(Waff,{1,2,1}),full);
assert(acted#"realizationCoordinates" =!= null);
assert(realizationCoordinates(weylAction(
    inverseWeylElement weylElement(Waff,{1,2,1}),acted)) ===
    realizationCoordinates full);

-- Parent mismatches fail even when coordinate lengths happen to agree.
WB3 = weylGroup rootDatum(cartanB 3);
assert(mustErrorWP(() -> weylAction(simpleWeylReflection(WB3,1),lambda)));
assert(mustErrorWP(() -> s1 * simpleWeylReflection(WB3,1)));
eB3 = identityWeylElement WB3;
assert(weylElementMatrix e == weylElementMatrix eB3);
assert(not (e == eB3));

-- User-supplied node labels work in Weyl words, simple reflections, Coxeter
-- queries, and reflection matrices; stored words remain canonical 1-based
-- integer words.
Dnamed = cartanDatum("A3",NodeLabels=>{"left","middle","right"});
Xnamed = rootDatum Dnamed;
Wnamed = weylGroup Xnamed;
assert(simpleWeylReflection(Wnamed,"left") ==
    simpleWeylReflection(Wnamed,1));
assert(weylElement(Wnamed,{"left","middle","left"}) ==
    weylElement(Wnamed,{1,2,1}));
assert(reducedWord weylElement(Wnamed,{"left","middle"}) === {1,2});
assert(toString weylElement(Wnamed,{"left","middle","left"})
    === "s_left s_middle s_left");
assert(coxeterMatrix(Xnamed,"left","middle") == 3);
assert(coxeterMatrix(Wnamed) === coxeterMatrix(cartanA 4));
assert(reflectionMatrix(Wnamed,"left") == reflectionMatrix(cartanA 4,1));
assert(mustErrorWP(() -> simpleWeylReflection(Wnamed,"unknown")));
assert(not (identityWeylElement W == identityWeylElement Wnamed));

-- Any noninteger node label is usable consistently, not only a String.
DgeneralLabels = cartanDatum(
    "A3",NodeLabels=>{true,false,"tip"});
WgeneralLabels = weylGroup DgeneralLabels;
assert(simpleWeylReflection(WgeneralLabels,true) ==
    simpleWeylReflection(WgeneralLabels,1));
assert(coxeterMatrix(WgeneralLabels,true,false) == 3);
assert(coxeterMatrix(WgeneralLabels,1,false) == 3);
assert(reflectionMatrix(WgeneralLabels,true) ==
    reflectionMatrix(cartanA 4,1));

-- A positional integer in 1..rank takes precedence; an integer outside
-- that range can still be a user-supplied label, as elsewhere in the
-- package's typed APIs.
DintegerLabels = cartanDatum(
    "A3",NodeLabels=>{10,20,30});
WintegerLabels = weylGroup DintegerLabels;
assert(simpleWeylReflection(WintegerLabels,10) ==
    simpleWeylReflection(WintegerLabels,1));
assert(coxeterMatrix(WintegerLabels,10,20) == 3);
assert(coxeterMatrix(WintegerLabels,1,20) == 3);
assert(reflectionMatrix(WintegerLabels,30) ==
    reflectionMatrix(cartanA 4,3));
assert(toString simpleWeylReflection(WintegerLabels,20) === "s_20");
assert(removedNodes standardParabolic(
    DintegerLabels,RemovedNodes=>{20}) === {2});

DrationalLabels = cartanDatum(
    "A3",NodeLabels=>{10_QQ,20_QQ,30_QQ});
WrationalLabels = weylGroup DrationalLabels;
assert(simpleWeylReflection(WrationalLabels,10) ==
    simpleWeylReflection(WrationalLabels,10_QQ));
assert(coxeterMatrix(WrationalLabels,10,20) == 3);

P = standardParabolic(D,RemovedNodes=>{2});
PbyLevi = standardParabolic(D,LeviNodes=>{1,3});
assert(P === PbyLevi);
assert(removedNodes P === {2});
assert(leviNodes P === {1,3});
assert((parabolicLevi P)#"nodeLabels" === {1,3});
assert(datumCartanMatrix(parabolicLevi P) == (cartanA 2) ++ (cartanA 2));
assert(parabolicDegree(P,kmRoot(D,{3,4,5})) == 4);
assert(isLeviRoot(P,kmRoot(D,{3,0,5})));
assert(not isLeviRoot(P,kmRoot(D,{3,1,5})));
assert((leviSubalgebra P)#"parabolicObject" === P);
assert((parabolicData P)#"leviCartanDatum" === parabolicLevi P);
assert(standardParabolic(D,{2}) === P);
assert(standardParabolic(D,set{2}) === P);
assert(standardParabolic("A3",set{2}) ===
    standardParabolic("A3",RemovedNodes=>{2}));
assert(mustErrorWP(() -> standardParabolic(D,
    RemovedNodes=>{1},LeviNodes=>{2,3})));
assert(mustErrorWP(() -> standardParabolic(D,{2},LeviNodes=>{1,3})));

-- Every convention-sensitive legacy routine has a StandardParabolic
-- overload.  In particular, quotient routines obtain Levi nodes from P,
-- while grading and cohomology routines obtain removed nodes from P.
A3wp = datumCartanMatrix D;
assert(parabolicDiagram P === parabolicDiagram(A3wp,{2}));
assert(cartanPositiveRootsDegree(P,1) ==
    cartanPositiveRootsDegree(A3wp,1,{2}));
assert(nilradicalRoots P == nilradicalRoots(A3wp,{2}));
assert(parabolicDegrees P === parabolicDegrees(A3wp,{2}));
assert(leviConnectedComponents P === {{1},{3}});
assert(leviHighestWeights(P,1) == leviHighestWeights(A3wp,1,{2}));
TtypedP = leviHighestWeights(P,1);
VtypedP = first keys TtypedP;
assert(irrepDimension(P,VtypedP) == 4);
assert(decompositionDimension(P,TtypedP) == 4);
assert((irrepData(P,VtypedP))#"parentCartan" ==
    datumCartanMatrix parabolicLevi P);
assert(parabolicCosetReps(P,2) ===
    parabolicCosetReps(A3wp,2,{1,3}));
assert(parabolicDotImages(P,1,{0,0,0}) ===
    parabolicDotImages(A3wp,1,{1,3},{0,0,0}));
HtypedP = kostantLieAlgCohomology(P,{0,0,0});
HlegacyP = kostantLieAlgCohomology(A3wp,{2},{0,0,0});
assert(HtypedP === HlegacyP);
assert(cohomologyDimensions(P,HtypedP) ===
    cohomologyDimensions(A3wp,{2},HlegacyP));
assert(mustErrorWP(() -> parabolicCosetReps(P,2,MaxElements=>1)));
assert(mustErrorWP(() -> leviHighestWeights(P,-1)));
assert(mustErrorWP(() ->
    leviHighestWeights(P,1,WeightFormat=>"Unknown")));

Pnamed = standardParabolic(Xnamed,RemovedNodes=>{"middle"});
PnamedByLevi = standardParabolic(
    Xnamed,LeviNodes=>{"left","right"});
assert(Pnamed == PnamedByLevi);
assert(removedNodes Pnamed === {2});
assert(leviNodes Pnamed === {1,3});
assert((parabolicLevi Pnamed)#"nodeLabels" === {"left","right"});
assert(mustErrorWP(() ->
    standardParabolic(Xnamed,RemovedNodes=>{"unknown"})));

-- Boundary parabolics are valid.  The Borel has a rank-zero derived Levi,
-- while removing no nodes recovers the full derived algebra.
PBorel = standardParabolic(D,LeviNodes=>{});
assert(removedNodes PBorel === {1,2,3});
assert(leviNodes PBorel === {});
assert((parabolicLevi PBorel)#"rank" == 0);
assert(numrows datumCartanMatrix parabolicLevi PBorel == 0);
assert((leviSubalgebra PBorel)#"semisimpleRank" == 0);
assert((leviSubalgebra PBorel)#"centerDimension" == 3);
assert(leviConnectedComponents PBorel === {});
assert(nilradicalRoots PBorel == cartanPositiveRoots A3wp);
TBorel = leviHighestWeights(PBorel,1);
assert(#keys TBorel == 1 and sum values TBorel == 3);
assert(toList first keys TBorel === {});
assert(decompositionDimension(PBorel,TBorel) == 3);
for k from 0 to 6 do assert(
    #parabolicCosetReps(PBorel,k) == #weylGroupElements(A3wp,k));

Pwhole = standardParabolic(D,RemovedNodes=>{});
assert(removedNodes Pwhole === {});
assert(leviNodes Pwhole === {1,2,3});
assert(parabolicLevi Pwhole === cartanDatum D);
assert(parabolicDegree(Pwhole,kmRoot(D,{1,2,3})) == 0);

-- The rank-zero datum has the trivial Weyl group, so the typed operations
-- obtained from a Borel Levi remain closed under the public constructors.
Wzero = weylGroup parabolicLevi PBorel;
ezero = identityWeylElement Wzero;
assert(reducedWord ezero === {});
assert(weylLength ezero == 0);
assert(numrows weylElementMatrix ezero == 0);
assert(coxeterMatrix Wzero === {});
assert(inverseWeylElement ezero == ezero);
assert(leftDescentSet ezero === {});
assert(rightDescentSet ezero === {});
assert(weylGroupOrder datumCartanMatrix parabolicLevi PBorel == 1);
assert(#weylGroupElements datumCartanMatrix parabolicLevi PBorel == 1);
PzeroWP = weylLengthGenerating datumCartanMatrix parabolicLevi PBorel;
assert(instance(PzeroWP,RingElement));
assert(ring PzeroWP === ring kazhdanLusztigVariable());
assert(PzeroWP == 1);
assert(toString PzeroWP === "1" and toString net PzeroWP === "1");
assert(sub(PzeroWP,{(ring PzeroWP)_0=>1}) == 1);
assert(weylExponents datumCartanMatrix parabolicLevi PBorel === {});
assert(invariantDegrees datumCartanMatrix parabolicLevi PBorel === {});
assert(mustErrorWP(() -> simpleWeylReflection(Wzero,1)));

-- Bounded strata make sense for infinite Weyl groups.  A_1^(1) has two
-- elements in every positive length and one identity.
A11 = cartanAffineA 1;
assert(#weylGroupElements(A11,0) == 1);
for k from 1 to 8 do assert(#weylGroupElements(A11,k) == 2);

///


-------------------------------------------------------------------------
-- Finite Structure Tests
-------------------------------------------------------------------------

TEST ///

mustError := f -> try (f(); false) else true;

integralList := L -> apply(L, x -> (
    xQ := x + 0_QQ;
    assert(denominator xQ == 1);
    numerator xQ
));

unitWeight := (r, i) -> for j from 1 to r list
    if j == i then 1 else 0;

-- Public tests use the parent-aware Weyl interface.  The raw
-- testWeylActionOnWeight backend is intentionally not exported.
testWeylActionOnWeight := (A,word,lambda) -> (
    raw := if instance(lambda,Matrix) then flatten entries lambda else lambda;
    D := rootDatum cartanDatum A;
    datumCoordinates weylAction(weylElement(D,word),kmWeight(D,raw))
);

finiteExamples := {
    {"A1", cartanA 2},
    {"A3", cartanA 4},
    {"B3", cartanB 3},
    {"C3", cartanC 3},
    {"D4", cartanD 4},
    {"D5", cartanD 5},
    {"E6", cartanE 6},
    {"E7", cartanE 7},
    {"E8", cartanE 8},
    {"F4", cartanF4()},
    {"G2", cartanG2()}
};

------------------------------------------------------------
-- 1. Ordinary Weyl action and word convention
------------------------------------------------------------

A := cartanA 4;
assert(testWeylActionOnWeight(A, {}, {2,-1,3}) === {2,-1,3});
assert(testWeylActionOnWeight(A, {1}, {1,0,0}) === {-1,1,0});
assert(testWeylActionOnWeight(A, {1,2}, {1,2,3}) ===
    testWeylActionOnWeight(A, {1}, testWeylActionOnWeight(A, {2}, {1,2,3})));
assert(testWeylActionOnWeight(A, {1,2,3}, matrix{{1,2,3}}) ===
    testWeylActionOnWeight(A, {1,2,3}, {1,2,3}));

-- Independent check through simple-root coordinates and the already tested
-- matrix representation of W.
for example in finiteExamples do (
    A = example#1;
    r := numrows A;
    lambda := for i from 1 to r list i;
    word := if r == 1 then {1,1,1} else {1,r,1};
    ordinary := testWeylActionOnWeight(A, word, lambda);
    lambdaSimple := toSimple(A, lambda);
    imageColumn := weylElementMatrix(A, word) * transpose matrix{lambdaSimple};
    imageSimple := for i from 0 to r-1 list imageColumn_(i,0);
    independent := integralList toFundamental(A, imageSimple);
    assert(ordinary === independent);

    -- Every simple reflection squares to the identity.
    for i from 1 to r do
        assert(testWeylActionOnWeight(A, {i,i}, lambda) === lambda);

    -- Every finite braid relation acts identically on a generic weight.
    C := coxeterMatrix A;
    for i from 1 to r do for j from i+1 to r do (
        m := C#(i-1)#(j-1);
        left := for k from 0 to m-1 list if k % 2 == 0 then i else j;
        right := for k from 0 to m-1 list if k % 2 == 0 then j else i;
        assert(testWeylActionOnWeight(A, left, lambda) ===
            testWeylActionOnWeight(A, right, lambda));
    );
);

-- Ordinary and dot action differ by the rho shift.
A = cartanB 3;
lambda = {2,-1,1};
word = {1,2,3,2};
rhoLabels := {1,1,1};
shifted := apply(lambda, rhoLabels, (x,y) -> x+y);
ordinaryShifted := testWeylActionOnWeight(A, word, shifted);
assert(dotActionOnWeight(A, word, lambda) ===
    apply(ordinaryShifted, rhoLabels, (x,y) -> x-y));

------------------------------------------------------------
-- 2. Dominantization witnesses and stabilizer generators
------------------------------------------------------------

for example in finiteExamples do (
    A = example#1;
    r := numrows A;
    lambda := for i from 1 to r list i;
    orbitWeight := testWeylActionOnWeight(A, longElement A, lambda);
    data := dominantizeWeight(A, orbitWeight);
    assert(data#"dominantWeight" === lambda);
    assert(testWeylActionOnWeight(A, data#"word", orbitWeight) === lambda);
    assert(testWeylActionOnWeight(A, data#"inverseWord", lambda) === orbitWeight);
    assert(data#"regular");
    assert(data#"wallNodes" === {});
);

A = cartanA 4;
lambda = {-1,0,0};
data = dominantizeWeight(A, lambda);
assert(data#"dominantWeight" === {0,0,1});
assert(testWeylActionOnWeight(A, data#"word", lambda) === data#"dominantWeight");
assert(data#"wallNodes" === {1,2});

-- A singular stabilizer is a conjugate of the wall parabolic.
dominantSingular := {1,0,1};
lambda = testWeylActionOnWeight(A, {1,2}, dominantSingular);
stab := weightStabilizer(A, lambda);
assert(stab#"dominantWeight" === dominantSingular);
assert(stab#"dominantWallNodes" === {2});
assert(stab#"order" == 2);
for generator in stab#"generatingWords" do
    assert(testWeylActionOnWeight(A, generator, lambda) === lambda);

regularStab := weightStabilizer(A, {1,2,3});
assert(regularStab#"order" == 1);
assert(regularStab#"generatingWords" === {});
zeroStab := weightStabilizer(A, {0,0,0});
assert(zeroStab#"order" == weylGroupOrder A);
assert(#(zeroStab#"generatingWords") == numrows A);

-- Exhaustive rank-two oracle: every small integral weight dominantizes with
-- a valid two-sided witness, and the reported stabilizer order agrees with
-- literal finite-Weyl-group enumeration.
rankTwoExamples := {cartanA 3, cartanB 2, cartanC 2, cartanG2()};
for A in rankTwoExamples do (
    elements := weylGroupElements A;
    for a from -4 to 4 do for b from -4 to 4 do (
        lambda = {a,b};
        data = dominantizeWeight(A, lambda);
        dominant := data#"dominantWeight";
        assert(all(dominant, x -> x >= 0));
        assert(testWeylActionOnWeight(A, data#"word", lambda) === dominant);
        assert(testWeylActionOnWeight(A, data#"inverseWord", dominant) === lambda);
        stab = weightStabilizer(A, lambda);
        literalOrder := #(select(elements, w ->
            testWeylActionOnWeight(A, w#"word", lambda) === lambda));
        assert(stab#"order" == literalOrder);
        for generator in stab#"generatingWords" do
            assert(testWeylActionOnWeight(A, generator, lambda) === lambda);
    );
);

------------------------------------------------------------
-- 3. Dual highest weights
------------------------------------------------------------

assert(dualWeight(cartanA 3, {1,0}) === {0,1});
assert(dualWeight(cartanA 4, {1,2,3}) === {3,2,1});
assert(dualWeight(cartanD 4, {0,0,1,0}) === {0,0,1,0});
assert(dualWeight(cartanD 5, {0,0,0,1,0}) === {0,0,0,0,1});
assert(dualWeight(cartanD 5, {0,0,0,0,1}) === {0,0,0,1,0});
assert(dualWeight(cartanD 6, {0,0,0,0,1,0}) === {0,0,0,0,1,0});
assert(dualWeight(cartanE 6, {1,0,0,0,0,0}) === {0,0,0,0,0,1});
assert(dualWeight(cartanE 6, {0,0,1,0,0,0}) === {0,0,0,0,1,0});
assert(isSelfDual(cartanB 3, {2,1,4}));
assert(isSelfDual(cartanD 4, {0,0,1,0}));
assert(not isSelfDual(cartanD 5, {0,0,0,1,0}));
assert(not isSelfDual(cartanA 3, {1,0}));

for example in finiteExamples do (
    A = example#1;
    r := numrows A;
    lambda := for i from 1 to r list i;
    oracleDual := apply(
        testWeylActionOnWeight(A,longElement A,lambda),x -> -x);
    assert(dualWeight(A,lambda) === oracleDual);
    assert(dualWeight(A, dualWeight(A, lambda)) === lambda);
    assert(all(dualWeight(A, lambda), x -> x >= 0));
    assert(weylDimension(A, lambda) == weylDimension(A, dualWeight(A, lambda)));
);

-- Opposition is a lattice involution beyond the dominant chamber, but that
-- is deliberately distinguished from self-duality of a representation.
A = cartanA 6;
nondominant := {2,-1,0,4,-3};
assert(dualWeight(A,dualWeight(A,nondominant)) === nondominant);
assert(isOppositionFixed(cartanA 3,{-2,-2}));
assert(mustError(() -> isSelfDual(cartanA 3,{-2,-2})));

-- A cold duality call does not construct the longest word.  It recovers one
-- opposition permutation from a regular chamber walk, then caches it.
Acold := cartanA 21;
assert(not Acold.cache#?"longElement");
coldDual := dualWeight(Acold,toList(1..20));
assert(not Acold.cache#?"longElement");
assert(Acold.cache#?"KacMoodyFiniteDraft.oppositionPermutation");
assert(coldDual === reverse toList(1..20));
assert(dualWeight(Acold,coldDual) === toList(1..20));

-- The integral representative cached on a QQ matrix retains the opposition
-- cache, so repeated calls do not silently redo the chamber walk.
AQQ := (cartanA 5) ** QQ;
assert(dualWeight(AQQ,{1,2,3,4}) === {4,3,2,1});
assert(AQQ.cache#?"KacMoody.integralCartanMatrix");
AZZ := AQQ.cache#"KacMoody.integralCartanMatrix";
assert(AZZ.cache#?"KacMoodyFiniteDraft.oppositionPermutation");
assert(dualWeight(AQQ,{4,3,2,1}) === {1,2,3,4});
assert(AQQ.cache#"KacMoody.integralCartanMatrix" === AZZ);

-- Matrix-form input preserves its matrix shape.
matrixDual := dualWeight(cartanA 4,matrix(QQ,{{1,2,3}}));
assert(instance(matrixDual,Matrix));
assert(matrixDual == matrix(QQ,{{3,2,1}}));

-- Reducible and interleaved components are handled componentwise.
AproductDual := matrix{{2,-1,0},{-1,2,0},{0,0,2}};
assert(dualWeight(AproductDual,{7,2,-5}) === {2,7,-5});
dualPermutation := {2,0,1};
AproductPermuted := submatrix(
    AproductDual,dualPermutation,dualPermutation);
assert(dualWeight(AproductPermuted,{-5,7,2}) === {-5,2,7});

-- Typed duality preserves the exact RootDatum parent and all complete
-- realization coordinates.  The hidden Weyl-fixed central character is
-- negated, as required for a contragredient representation.
RextraDual := realization(cartanDatum(cartanA 2),
    matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
DextraDual := rootDatum RextraDual;
lambdaCentralDual := weightFromRealizationCoordinates(DextraDual,{1,3});
lambdaCentralDualStar := dualWeight lambdaCentralDual;
assert(instance(lambdaCentralDualStar,KMWeight));
assert(parentDatum lambdaCentralDualStar === DextraDual);
assert(datumCoordinates lambdaCentralDualStar === {1});
assert(realizationCoordinates lambdaCentralDualStar === {1,-3});
assert(not isSelfDual lambdaCentralDual);
lambdaNeutralDual := weightFromRealizationCoordinates(DextraDual,{1,0});
assert(isSelfDual lambdaNeutralDual);
assert(isOppositionFixed kmWeight(DextraDual,{1}));
assert(mustError(() -> isSelfDual kmWeight(DextraDual,{1})));
assert(dualWeight(DextraDual,lambdaCentralDual)
    === lambdaCentralDualStar);
assert(not isSelfDual(DextraDual,lambdaCentralDual));
assert(dualWeight(lambdaCentralDualStar) === lambdaCentralDual);

-- Typed chamber and stabilizer data are immediately composable and carry
-- both canonical positions and public node labels.
DnamedFinite := rootDatum cartanDatum(
    "A3",NodeLabels=>{"left","middle","right"});
lambdaNamedFinite := kmWeight(DnamedFinite,{-1,0,0});
typedDominant := dominantizeWeight lambdaNamedFinite;
assert(datumCoordinates(typedDominant#"dominantWeight") === {0,0,1});
assert(weylAction(typedDominant#"toDominantElement",lambdaNamedFinite)
    === typedDominant#"dominantWeight");
assert(typedDominant#"wallNodeLabels" === {"left","middle"});
typedStabilizer := weightStabilizer(
    kmWeight(DnamedFinite,{1,0,1}));
assert(typedStabilizer#"dominantWallNodeLabels" === {"middle"});
assert(typedStabilizer#"order" == 2);
for g in typedStabilizer#"generatingElements" do
    assert(weylAction(g,typedStabilizer#"inputWeight")
        === typedStabilizer#"inputWeight");

------------------------------------------------------------
-- 4. Fundamental coweights and normalized invariant form
------------------------------------------------------------

for example in finiteExamples do (
    A = example#1;
    r := numrows A;
    F := fundamentalCoweights A;
    assert(F * (A ** QQ) == id_(QQ^r));
    assert(F == fundamentalWeights transpose A);

    B := normalizedInvariantForm A;
    assert(B == transpose B);
    diagonal := for i from 0 to r-1 list B_(i,i);
    assert(max diagonal == 2_QQ);
    assert(min diagonal == 2_QQ / lacingNumber A);
);

-- Integral Cartan matrices over QQ and matrix-form weights are accepted.
A = cartanB 3;
assert(fundamentalCoweights(A ** QQ) == fundamentalCoweights A);
assert(dualWeight(A ** QQ, matrix{{1,0,0}})
    == matrix{dualWeight(A,{1,0,0})});
assert(normalizedInvariantForm(A ** QQ) == normalizedInvariantForm A);

-- On a reducible finite datum, each component is normalized independently.
Adisconnected := matrix{{2,-1,0},{-1,2,0},{0,0,2}};
Bdisconnected := normalizedInvariantForm Adisconnected;
assert(Bdisconnected == Adisconnected ** QQ);

-- Mixed lacing detects accidental use of one global scale.
Bmixed := (cartanB 2) ++ (cartanC 3);
Gmixed := normalizedInvariantForm Bmixed;
assert(submatrix(Gmixed,{0,1},{0,1})
    == normalizedInvariantForm(cartanB 2));
assert(submatrix(Gmixed,{2,3,4},{2,3,4})
    == normalizedInvariantForm(cartanC 3));
assert(submatrix(Gmixed,{0,1},{2,3,4}) == 0);

-- Every structural command accepts parent-aware finite data.
Dstruct := rootDatum "B3";
assert(fundamentalCoweights Dstruct == fundamentalCoweights(cartanB 3));
assert(normalizedInvariantForm Dstruct
    == normalizedInvariantForm(cartanB 3));
assert(marks Dstruct === marks(cartanB 3));
assert(highestCoroot Dstruct === highestCoroot(cartanB 3));
assert(comarks Dstruct === comarks(cartanB 3));
assert(lacingNumber Dstruct == lacingNumber(cartanB 3));
assert(weylExponents Dstruct === weylExponents(cartanB 3));
assert(invariantDegrees Dstruct === invariantDegrees(cartanB 3));
Rstruct := realization Dstruct;
Cstruct := cartanDatum Dstruct;
Wstruct := weylGroup Dstruct;
for structuralParent in {Cstruct,Rstruct,Wstruct} do (
    assert(marks structuralParent === marks(cartanB 3));
    assert(comarks structuralParent === comarks(cartanB 3));
    assert(normalizedInvariantForm structuralParent
        == normalizedInvariantForm(cartanB 3));
);
assert(marks "B3" === marks(cartanB 3));
assert(minusculeNodes Dstruct === {3});
assert(minusculeNodes Wstruct === {3});

------------------------------------------------------------
-- 5. Marks, comarks, lacing, exponents, and invariant degrees
------------------------------------------------------------

expectedData := {
    {cartanA 4, {1,1,1}, {1,1,1}, 1, {1,2,3}},
    {cartanB 3, {1,2,2}, {1,2,1}, 2, {1,3,5}},
    {cartanC 3, {2,2,1}, {1,1,1}, 2, {1,3,5}},
    {cartanD 4, {1,2,1,1}, {1,2,1,1}, 1, {1,3,3,5}},
    {cartanE 6, {1,2,2,3,2,1}, {1,2,2,3,2,1}, 1,
        {1,4,5,7,8,11}},
    {cartanE 7, {2,2,3,4,3,2,1}, {2,2,3,4,3,2,1}, 1,
        {1,5,7,9,11,13,17}},
    {cartanE 8, {2,3,4,6,5,4,3,2}, {2,3,4,6,5,4,3,2}, 1,
        {1,7,11,13,17,19,23,29}},
    {cartanF4(), {2,3,4,2}, {2,3,2,1}, 2, {1,5,7,11}},
    {cartanG2(), {3,2}, {1,2}, 3, {1,5}}
};

for datum in expectedData do (
    A = datum#0;
    expectedMarks := datum#1;
    expectedComarks := datum#2;
    expectedLacing := datum#3;
    expectedExponents := datum#4;
    assert(marks A === expectedMarks);
    assert(comarks A === expectedComarks);
    assert(highestCoroot A === expectedComarks);
    assert(lacingNumber A == expectedLacing);
    assert(weylExponents A === expectedExponents);
    assert(invariantDegrees A === apply(expectedExponents, x -> x+1));
    assert(sum marks A + 1 == coxeterNumber A);
    assert(sum comarks A + 1 == dualCoxeterNumber A);
    assert(product invariantDegrees A == weylGroupOrder A);

    -- With long roots of norm 2, a_i^vee=a_i*(alpha_i,alpha_i)/2.
    B := normalizedInvariantForm A;
    derivedComarks := integralList for i from 0 to numrows A-1 list
        (marks A)#i * B_(i,i) / 2;
    assert(derivedComarks === comarks A);
);

assert(weylExponents Adisconnected === {1,1,2});
assert(invariantDegrees Adisconnected === {2,2,3});

------------------------------------------------------------
-- 6. Minuscule and cominuscule classifications
------------------------------------------------------------

classification := {
    {cartanA 4, {1,2,3}, {1,2,3}},
    {cartanB 3, {3}, {1}},
    {cartanC 3, {1}, {3}},
    {cartanD 5, {1,4,5}, {1,4,5}},
    {cartanE 6, {1,6}, {1,6}},
    {cartanE 7, {7}, {7}},
    {cartanE 8, {}, {}},
    {cartanF4(), {}, {}},
    {cartanG2(), {}, {}}
};

for datum in classification do (
    A = datum#0;
    expectedMinuscule := datum#1;
    expectedCominuscule := datum#2;
    assert(minusculeNodes A === expectedMinuscule);
    assert(cominusculeNodes A === expectedCominuscule);
    for i from 1 to numrows A do (
        omega := unitWeight(numrows A, i);
        assert(isMinusculeNode(A,i) === member(i, expectedMinuscule));
        assert(isCominusculeNode(A,i) === member(i, expectedCominuscule));
        assert(isMinusculeWeight(A,omega) === member(i, expectedMinuscule));
        assert(isCominusculeWeight(A,omega) === member(i, expectedCominuscule));
    );
    zero := for i from 1 to numrows A list 0;
    assert(isMinusculeWeight(A,zero));
    assert(isCominusculeWeight(A,zero));
);

-- Minuscule and cominuscule are genuinely dual notions in nonsimply-laced
-- type; neither may be substituted for multiplicity-freeness.
assert({
    minusculeNodes(cartanB 3),
    cominusculeNodes(cartanB 3),
    minusculeNodes(cartanC 3),
    cominusculeNodes(cartanC 3)
} === {{3},{1},{1},{3}});
assert(minusculeNodes(cartanB 3)
    === cominusculeNodes(transpose cartanB 3));
assert(cominusculeNodes(cartanB 3)
    === minusculeNodes(transpose cartanB 3));
assert(isMinusculeWeight(cartanB 3,{0,0,1}));
assert(not isCominusculeWeight(cartanB 3,{0,0,1}));
assert(not isMinusculeWeight(cartanB 3,{1,0,0}));
assert(isCominusculeWeight(cartanB 3,{1,0,0}));
assert(weylDimension(cartanB 3,{1,0,0}) == 7);
assert(#weightOrbit(cartanB 3,{1,0,0}) == 6);
assert(all(values weightMultiplicityTable(cartanB 3,{1,0,0}),
    multiplicity -> multiplicity == 1));

-- Typed node output defaults to public labels.  Records retain both labels
-- and positions and round-trip without ambiguity through the predicates.
Dspecial := cartanDatum(
    "B3",NodeLabels=>{"vector","middle","spin"});
assert(minusculeNodes Dspecial === {"spin"});
assert(cominusculeNodes Dspecial === {"vector"});
assert(minusculeNodes(Dspecial,NodeFormat=>"Positions") === {3});
minusculeRecords := minusculeNodes(Dspecial,NodeFormat=>"Records");
assert(#minusculeRecords == 1);
assert((first minusculeRecords)#"position" == 3);
assert((first minusculeRecords)#"label" === "spin");
assert(isMinusculeNode(Dspecial,"spin"));
assert(isMinusculeNode(Dspecial,first minusculeRecords));
assert(isCominusculeNode(Dspecial,"vector"));
assert(isMinusculeWeight kmWeight(rootDatum Dspecial,{0,0,1}));
assert(isCominusculeWeight(rootDatum Dspecial,{1,0,0}));

-- Relabeling and simultaneous node permutation preserve the labeled answer.
B3perm := {2,0,1};
DspecialPermuted := cartanDatum(
    submatrix(cartanB 3,B3perm,B3perm),
    NodeLabels=>{"spin","vector","middle"});
assert(minusculeNodes DspecialPermuted === {"spin"});
assert(cominusculeNodes DspecialPermuted === {"vector"});

-- If custom integer labels collide with canonical positions, the typed
-- default automatically uses records rather than returning a label that
-- would be reinterpreted as the wrong position.
DintegerCollision := cartanDatum(
    "B3",NodeLabels=>{3,2,1});
integerMinuscule := first minusculeNodes DintegerCollision;
integerCominuscule := first cominusculeNodes DintegerCollision;
assert(integerMinuscule#"position" == 3);
assert(integerMinuscule#"label" == 1);
assert(integerCominuscule#"position" == 1);
assert(integerCominuscule#"label" == 3);
assert(isMinusculeNode(DintegerCollision,integerMinuscule));
assert(isCominusculeNode(DintegerCollision,integerCominuscule));
assert(removedNodes standardParabolic(
    DintegerCollision,RemovedNodes=>{integerMinuscule}) === {3});
assert(reducedWord simpleWeylReflection(
    weylGroup DintegerCollision,integerMinuscule) === {3});

-- In a product, minuscule factors can be chosen independently.
B2plusA1 := matrix{{2,-1,0},{-2,2,0},{0,0,2}};
assert(minusculeNodes B2plusA1 === {2,3});
assert(cominusculeNodes B2plusA1 === {1,3});
assert(isMinusculeWeight(B2plusA1,{0,1,1}));
assert(isCominusculeWeight(B2plusA1,{1,0,1}));
assert(not isMinusculeWeight(B2plusA1,{1,0,0}));
assert(not isCominusculeWeight(B2plusA1,{0,1,0}));

-- Independent representation-theoretic criterion: a minuscule irreducible
-- has exactly one Weyl orbit of weights and every multiplicity is one.
for datum in {
    {cartanA 4, 2},
    {cartanB 3, 3},
    {cartanC 3, 1},
    {cartanD 4, 4},
    {cartanE 6, 1}
} do (
    A = datum#0;
    omega := unitWeight(numrows A, datum#1);
    assert(set weightsOfRep(A,omega) === set weightOrbit(A,omega));
    character := weightMultiplicityTable(A,omega);
    assert(all(values character, multiplicity -> multiplicity == 1));
);

------------------------------------------------------------
-- 7. Quadratic Casimir normalization and invariance
------------------------------------------------------------

assert(quadraticCasimir(cartanA 2,{0}) == 0_QQ);
assert(quadraticCasimir(cartanA 2,{1}) == 3_QQ/2);
assert(quadraticCasimir(cartanA 3,{1,0}) == 8_QQ/3);
assert(quadraticCasimir(cartanA 4,{1,0,0}) == 15_QQ/4);
assert(quadraticCasimir(cartanB 3,{1,0,0}) == 6);
assert(quadraticCasimir(cartanB 3,{0,0,1}) == 21_QQ/4);
assert(quadraticCasimir(cartanC 3,{1,0,0}) == 7_QQ/2);
assert(quadraticCasimir(cartanG2(),{1,0}) == 4);

-- marks are simple-root coordinates, whereas Casimir input is in Dynkin
-- labels.  This guard catches an easy but mathematically serious basis mixup.
thetaB3 := integralList toFundamental(cartanB 3,marks cartanB 3);
assert(thetaB3 === {0,1,0});
assert(quadraticCasimir(cartanB 3,thetaB3) == 10);
assert(quadraticCasimir(cartanB 3,marks cartanB 3) == 56);

for example in finiteExamples do (
    A = example#1;
    thetaDynkin := integralList toFundamental(A, highestRoot A);
    assert(quadraticCasimir(A, thetaDynkin) == 2 * dualCoxeterNumber A);
    lambda := for i from 1 to numrows A list i % 3;
    assert(quadraticCasimir(A, lambda) == quadraticCasimir(A, dualWeight(A,lambda)));
);

-- Casimir is additive across semisimple direct products.
assert(quadraticCasimir(Adisconnected,{1,0,2}) ==
    quadraticCasimir(cartanA 3,{1,0}) + quadraticCasimir(cartanA 2,{2}));

-- Mixed-lacing products and typed complete weights use the semisimple
-- Casimir, component by component; no unspecified form on a central torus
-- is silently invented.
B2plusC3 := (cartanB 2) ++ (cartanC 3);
mixedLambda := {0,1,1,0,0};
assert(quadraticCasimir(B2plusC3,mixedLambda) ==
    quadraticCasimir(cartanB 2,{0,1})
    + quadraticCasimir(cartanC 3,{1,0,0}));
assert(quadraticCasimir lambdaCentralDual
    == quadraticCasimir(DextraDual,{1}));
assert(quadraticCasimir(DextraDual,lambdaCentralDual)
    == quadraticCasimir lambdaCentralDual);
assert(quadraticCasimir(DextraDual,{1}) == 3_QQ/2);

-- Dual pairs agree on dimension and Casimir.
for pair in {
    {cartanD 5,{0,0,0,1,0}},
    {cartanE 6,{1,0,0,0,0,0}}
} do (
    A = pair#0;
    lambda = pair#1;
    lambdaStar := dualWeight(A,lambda);
    assert(weylDimension(A,lambda) == weylDimension(A,lambdaStar));
    assert(quadraticCasimir(A,lambda)
        == quadraticCasimir(A,lambdaStar));
);
assert(weylDimension(cartanD 5,{0,0,0,1,0}) == 16);
assert(quadraticCasimir(cartanD 5,{0,0,0,1,0}) == 45_QQ/4);
assert(weylDimension(cartanE 6,{1,0,0,0,0,0}) == 27);
assert(quadraticCasimir(cartanE 6,{1,0,0,0,0,0}) == 52_QQ/3);

------------------------------------------------------------
-- 8. Relabeling equivariance
------------------------------------------------------------

A = cartanB 3;
permutation := {2,0,1};
Ap := submatrix(A, permutation, permutation);
lambda := {2,3,5};
lambdap := apply(permutation, i -> lambda#i);
assert(weylExponents Ap === weylExponents A);
assert(invariantDegrees Ap === invariantDegrees A);
assert(lacingNumber Ap == lacingNumber A);
assert(marks Ap === apply(permutation, i -> (marks A)#i));
assert(comarks Ap === apply(permutation, i -> (comarks A)#i));
assert(quadraticCasimir(Ap,lambdap) == quadraticCasimir(A,lambda));
dual := dualWeight(A,lambda);
assert(dualWeight(Ap,lambdap) === apply(permutation,i -> dual#i));
assert(normalizedInvariantForm Ap ==
    submatrix(normalizedInvariantForm A,permutation,permutation));
expectedMinusculePermuted := for j from 0 to #permutation-1 list
    if member(permutation#j+1, minusculeNodes A) then j+1 else continue;
expectedCominusculePermuted := for j from 0 to #permutation-1 list
    if member(permutation#j+1, cominusculeNodes A) then j+1 else continue;
assert(minusculeNodes Ap === expectedMinusculePermuted);
assert(cominusculeNodes Ap === expectedCominusculePermuted);

------------------------------------------------------------
-- 9. Rank-zero boundaries
------------------------------------------------------------

A0 := matrix(ZZ,{});
assert(dualWeight(A0,{}) === {});
assert(isOppositionFixed(A0,{}));
assert(isSelfDual(A0,{}));
assert(quadraticCasimir(A0,{}) == 0);
assert(minusculeNodes A0 === {});
assert(cominusculeNodes A0 === {});
assert(isMinusculeWeight(A0,{}));
assert(isCominusculeWeight(A0,{}));
assert(weylExponents A0 === {});
assert(invariantDegrees A0 === {});
assert(numrows normalizedInvariantForm A0 == 0);
assert(numrows fundamentalCoweights A0 == 0);
assert(mustError(() -> marks A0));
assert(mustError(() -> comarks A0));
assert(mustError(() -> highestCoroot A0));
assert(mustError(() -> lacingNumber A0));
D0finite := rootDatum cartanDatum A0;
lambda0finite := kmWeight(D0finite,{});
assert(dualWeight lambda0finite === lambda0finite);
assert(isSelfDual lambda0finite);
assert(quadraticCasimir lambda0finite == 0);
assert(minusculeNodes(cartanDatum A0) === {});
assert((weightStabilizer lambda0finite)#"order" == 1);

-- A zero-node, semisimple-rank-zero Cartan datum may still have a
-- positive-dimensional toral realization.  Empty 0-by-d simple-root
-- matrices must retain d, and contragredient duality negates the complete
-- torus character.
emptyTwoColumns := matrix mutableMatrix(QQ,0,2);
Rtorus := realization(
    cartanDatum A0,emptyTwoColumns,emptyTwoColumns);
Dtorus := rootDatum Rtorus;
assert((realization Dtorus)#"dimension" == 2);
assert(numrows characterLatticeBasis Dtorus == 2);
assert(numcols characterLatticeBasis Dtorus == 2);
torusWeight := weightFromRealizationCoordinates(Dtorus,{2,-3});
torusDual := dualWeight torusWeight;
assert(parentDatum torusDual === Dtorus);
assert(realizationCoordinates torusDual === {-2,3});
assert(dualWeight torusDual === torusWeight);
assert(not isSelfDual torusWeight);
assert(quadraticCasimir torusWeight == 0);
assert(integratesToGlobalForm torusWeight);
assert(mustError(() -> isSelfDual kmWeight(Dtorus,{})));

------------------------------------------------------------
-- 10. Adversarial input and domain checks
------------------------------------------------------------

A = cartanA 4;
Aaff := cartanAffineA 3;
assert(mustError(() -> testWeylActionOnWeight(A,{0},{1,0,0})));
assert(mustError(() -> testWeylActionOnWeight(A,{4},{1,0,0})));
assert(mustError(() -> testWeylActionOnWeight(A,{1},{1,0})));
assert(mustError(() -> testWeylActionOnWeight(A,{1},matrix{{1,0}})));
assert(mustError(() -> dominantizeWeight(Aaff,{1,0,0,0})));
assert(mustError(() -> weightStabilizer(A,{1,0})));
assert(mustError(() -> dualWeight(Aaff,{1,0,0,0})));
assert(mustError(() -> isOppositionFixed(Aaff,{1,0,0,0})));
assert(mustError(() -> fundamentalCoweights Aaff));
assert(mustError(() -> normalizedInvariantForm Aaff));
assert(mustError(() -> marks Adisconnected));
assert(mustError(() -> comarks Adisconnected));
assert(mustError(() -> highestCoroot Adisconnected));
assert(mustError(() -> lacingNumber Adisconnected));
assert(mustError(() -> weylExponents Aaff));
assert(mustError(() -> isMinusculeNode(A,0)));
assert(mustError(() -> isCominusculeNode(A,4)));
assert(mustError(() -> isMinusculeWeight(A,{-1,0,0})));
assert(mustError(() -> isCominusculeWeight(A,{0,1_QQ/2,0})));
assert(mustError(() -> quadraticCasimir(A,{-1,0,0})));
assert(mustError(() -> quadraticCasimir(A,{1,0})));
assert(mustError(() -> dualWeight(matrix{{2,-1},{0,2}},{1,0})));
assert(mustError(() ->
    dualWeight(rootDatum "B2",kmWeight(rootDatum "A2",{1,0}))));
assert(mustError(() ->
    minusculeNodes(cartanB 3,NodeFormat=>"Unknown")));
badNodeRecord := new HashTable from {
    "position" => 3,
    "label" => "vector"
};
assert(mustError(() -> isMinusculeNode(Dspecial,badNodeRecord)));

///


-------------------------------------------------------------------------
-- Formal Characters Tests
-------------------------------------------------------------------------

TEST ///

mustErrorFC = f -> (try (f(); false) else true)

D = rootDatum "A2";
omega1 = kmWeight(D,{1,0});
omega2 = kmWeight(D,{0,1});
C1 = finiteCharacter(D,omega1);
C2 = finiteCharacter(D,omega2);
assert(isExactCharacter C1);
assert(toString C1 === concatenate(
    "e^(1, 0) + e^(-1, 1) + e^(0, -1) [Dynkin coordinates]"));
assert(toString net C1 ===
    "e^(1, 0) + e^(-1, 1) + e^(0, -1)\n[Dynkin coordinates]");
assert(characterDimension C1 == 3);
assert(characterCoefficient(C1,omega1) == 1);
assert(characterCoefficient(C1,kmWeight(D,{0,0})) == 0);
assert(characterDimension(C1*C2) == 9);
assert(dualCharacter C1 === C2);
assert(weylAction(weylElement(weylGroup D,{1,2}),C1) === C1);

-- Display is deterministic, mathematical, and honest about virtual signs,
-- empty supports, and bounded expansion.
C1reordered = formalCharacter(D,new HashTable from
    reverse pairs characterTerms C1);
assert(toString C1reordered === toString C1);
virtualDisplayFC = 2*C1-C2;
assert(toString virtualDisplayFC === concatenate(
    "2*e^(1, 0) - e^(0, 1) - e^(1, -1) + ",
    "2*e^(-1, 1) + 2*e^(0, -1) - e^(-1, 0) [Dynkin coordinates]"));
assert(toString(0*C1) === "0 [Dynkin coordinates]");
largeDisplayFC = finiteCharacter(D,{4,4});
assert(match("^e\\^[(]4, 4[)]",toString largeDisplayFC));
assert(match("45 further weights not shown; dimension 125; exact character",
    toString largeDisplayFC));
assert(match("e\\^",toString net largeDisplayFC));
assert(toString formalCharacter(D,new HashTable from
    reverse pairs characterTerms largeDisplayFC) === toString largeDisplayFC);

dec = decomposeCharacter(C1*C2);
assert(dec#(kmWeight(D,{1,1})) == 1);
assert(dec#(kmWeight(D,{0,0})) == 1);
assert(#keys dec == 2);
assert(decomposeCharacter(2*C1+C2) == new Tally from {
    omega1=>2,omega2=>1});

truncated = formalCharacter(D,new Tally from {omega1=>1},
    CharacterStatus=>"Truncated",Truncation=>new HashTable from {"depth"=>0});
assert(not isExactCharacter truncated);
assert(toString truncated === "e^(1, 0) + ... [Dynkin coordinates; truncated]");
emptyTruncatedFC = formalCharacter(D,new Tally,
    CharacterStatus=>"Truncated",
    Truncation=>new HashTable from {"completeThroughDepth"=>2});
assert(toString emptyTruncatedFC === "0 + ... [Dynkin coordinates; truncated]");
assert(mustErrorFC(() -> characterDimension truncated));
assert(mustErrorFC(() -> decomposeCharacter truncated));
assert(not isExactCharacter(C1+truncated));

M = integrableHighestWeightModule(D,omega1);
assert(moduleCharacter M === C1);
assert((M#"kind") === "IntegrableHighestWeight");
VM = vermaModule(D,omega1);
assert(mustErrorFC(() -> moduleCharacter VM));
assert(mustErrorFC(() -> integrableHighestWeightModule(D,{-1,0})));

-- Depth zero is the empty positive-root product, so both truncations consist
-- of the highest-weight monomial alone.  This is also the boundary case used
-- by BGG Euler sums when a resolution term begins exactly at the cutoff.
VC0 = moduleCharacter(VM,Depth=>0);
IC0 = moduleCharacter(M,Depth=>0);
assert(characterSupport VC0 === {omega1});
assert(characterSupport IC0 === {omega1});
assert(characterCoefficient(VC0,omega1) == 1);
assert(characterCoefficient(IC0,omega1) == 1);
assert(VC0#"truncation"#"completeThroughDepth" == 0);
assert(IC0#"truncation"#"completeThroughDepth" == 0);

-- Completed characters are certified by root-depth rather than silently
-- pretending to be finite.  The A2 Verma denominator gives two partitions
-- of alpha1+alpha2: the root itself and alpha1+alpha2 separately.
VC2 = moduleCharacter(VM,Depth=>2);
assert(not isExactCharacter VC2);
assert(VC2#"truncation"#"completeThroughDepth" == 2);
assert(characterCoefficient(VC2,omega1-toWeight(kmRoot(D,{1,1}))) == 2);
assert(match("through root depth 2",toString VC2));
assert(match("^e\\^[(]1, 0[)] [+]",toString VC2));

-- Truncated finite Freudenthal agrees term-by-term with the complete finite
-- character in the certified range.
I21 = integrableHighestWeightModule(D,{2,1});
I21depth3 = moduleCharacter(I21,Depth=>3);
I21full = finiteCharacter(D,{2,1});
for p in pairs characterTerms I21depth3 do
    assert(p#1 == characterCoefficient(I21full,p#0));

-- The basic A1^(1) module has the expected highest, alpha_0, and delta
-- coefficients through depth two.  Full coordinates keep lambda-delta
-- distinct even though their Dynkin labels agree.
Daff = rootDatum cartanAffineA 1;
Lambda0 = weightFromRealizationCoordinates(Daff,{1,0,0});
basic = integrableHighestWeightModule(Daff,Lambda0);
basic2 = moduleCharacter(basic,Depth=>2);
lambdaMinusAlpha0 = Lambda0-toWeight(kmRoot(Daff,{1,0}));
lambdaMinusDelta = Lambda0-toWeight(kmRoot(Daff,{1,1}));
assert(characterCoefficient(basic2,Lambda0) == 1);
assert(characterCoefficient(basic2,lambdaMinusAlpha0) == 1);
assert(characterCoefficient(basic2,lambdaMinusDelta) == 1);
assert(lambdaMinusDelta =!= Lambda0);
assert(datumCoordinates lambdaMinusDelta === datumCoordinates Lambda0);
assert(toString basic2 === concatenate(
    "e^(1, 0, 0) + e^(-1, 2, -1) + e^(1, 0, -1) + ... ",
    "[realization coordinates; through root depth 2]"));
assert(toString net basic2 === concatenate(
    "e^(1, 0, 0) + e^(-1, 2, -1) + e^(1, 0, -1) + ...\n",
    "[realization coordinates; through root depth 2]"));
assert(mustErrorFC(() -> moduleCharacter(
    integrableHighestWeightModule(Daff,{1,0}),Depth=>2)));

-- Restrict the standard sl_3 module to the Levi obtained by deleting node 1.
-- The three one-dimensional/full-Levi weight spaces retain their center
-- characters; after grouping by center, each is irreducible for the A1 Levi.
P = standardParabolic(D,RemovedNodes=>{1});
L = reductiveLeviDatum(P,D);
assert(L#"centerDimension" == 1);
assert(numrows leviCenterBasis L == 1);
RC = restrictCharacter(L,C1);
assert(isExactCharacter RC);
assert(toString RC === concatenate(
    "e^(1, -1) + e^(0, 2) + e^(-1, -1) ",
    "[coordinates: 1 Levi Dynkin, then 1 central]"));
truncatedRC = restrictCharacter(L,VC0);
assert(toString truncatedRC === concatenate(
    "e^(0, 2) + ... [coordinates: 1 Levi Dynkin, then 1 central; ",
    "through root depth 0]"));
assert(sum values characterTerms RC == 3);
LD = decomposeLeviCharacter RC;
assert(sum values LD == 2);
assert(#keys LD == 2);

-- Keeping the center is essential: forgetting it turns the restriction into
-- V_A1(1) plus V_A1(0), but the two summands have distinct central weights.
semisimpleLabels = sort apply(keys LD,x -> datumCoordinates semisimplePart x);
assert(semisimpleLabels === {{0},{1}});
assert(#(set apply(keys LD,centralCharacter)) == 2);

-- The Borel boundary has a rank-zero derived Levi and the full Cartan as
-- its center.  Restriction must retain all three central coordinates.
D3BorelFC = rootDatum "A3";
PBorelFC = standardParabolic(D3BorelFC,LeviNodes=>{});
LBorelFC = reductiveLeviDatum(PBorelFC,D3BorelFC);
assert((LBorelFC#"semisimpleRootDatum")#"cartanDatum"#"rank" == 0);
assert(LBorelFC#"centerDimension" == 3);
muBorelFC = kmWeight(D3BorelFC,{1,2,3});
muLeviBorelFC = restrictWeight(PBorelFC,muBorelFC);
assert(datumCoordinates semisimplePart muLeviBorelFC === {});
assert(centralCharacter muLeviBorelFC === {1,2,3});
borelCharacterFC = restrictCharacter(
    LBorelFC,finiteCharacter(D3BorelFC,{1,0,0}));
assert(toString borelCharacterFC === concatenate(
    "e^(1, 0, 0) + e^(0, 0, -1) + e^(0, -1, 1) + ",
    "e^(-1, 1, 0) [coordinates: 0 Levi Dynkin, then 3 central]"));

-- Cross-parent errors and malformed character coefficients are explicit.
DB2 = rootDatum cartanB 2;
assert(mustErrorFC(() -> C1 + finiteCharacter(DB2,{1,0})));
assert(mustErrorFC(() -> formalCharacter(D,
    new Tally from {omega1=>1/2})));
assert(mustErrorFC(() -> formalCharacter(D,
    new Tally from {kmWeight(DB2,{1,0})=>1})));

-- A nonminimal finite realization has Weyl-fixed directions that Dynkin
-- labels do not see.  Label-class input must remain a label class, while a
-- complete highest weight must carry its central coordinate through every
-- weight of a finite, Verma, or truncated integrable character.
RextraFC = realization(cartanDatum(cartanA 2),
    matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
DextraFC = rootDatum RextraFC;
classLambdaFC = kmWeight(DextraFC,{1});
fullLambdaFC = weightFromRealizationCoordinates(DextraFC,{1,7});
alphaFC = kmRoot(DextraFC,{1});
lowerFullFC = fullLambdaFC-toWeight alphaFC;

classCharacterFC = finiteCharacter(DextraFC,classLambdaFC);
assert(all(characterSupport classCharacterFC,mu ->
    mustErrorFC(() -> realizationCoordinates mu)));
assert(toString classCharacterFC === concatenate(
    "e^(1) + e^(-1) [Dynkin-label classes modulo ann(h')]"));
fullCharacterFC = finiteCharacter(DextraFC,fullLambdaFC);
assert(characterSupport fullCharacterFC === {lowerFullFC,fullLambdaFC}
    or characterSupport fullCharacterFC === {fullLambdaFC,lowerFullFC});
assert(all(characterSupport fullCharacterFC,mu ->
    (realizationCoordinates mu)#1 == 7));
assert(toString fullCharacterFC === concatenate(
    "e^(1, 7) + e^(-1, 7) [realization coordinates]"));

vermaFullFC = moduleCharacter(vermaModule(DextraFC,fullLambdaFC),Depth=>1);
integrableFullFC = moduleCharacter(
    integrableHighestWeightModule(DextraFC,fullLambdaFC),Depth=>1);
assert(characterCoefficient(vermaFullFC,lowerFullFC) == 1);
assert(characterTerms integrableFullFC == characterTerms fullCharacterFC);
assert(mustErrorFC(() -> formalCharacter(DextraFC,new Tally from {
    classLambdaFC=>1,fullLambdaFC=>1})));
assert(mustErrorFC(() -> classCharacterFC+fullCharacterFC));

-- In semisimple rank zero, the complete torus coordinate is the entire
-- weight and must remain visible even though the Dynkin-label list is empty.
A0DisplayFC = matrix mutableMatrix(ZZ,0,0);
E0DisplayFC = matrix mutableMatrix(QQ,0,2);
D0DisplayFC = rootDatum realization(
    cartanDatum A0DisplayFC,E0DisplayFC,E0DisplayFC);
torusWeightFC = weightFromRealizationCoordinates(D0DisplayFC,{2,-3});
torusCharacterFC = finiteCharacter(D0DisplayFC,torusWeightFC);
assert(toString torusCharacterFC === concatenate(
    "e^(2, -3) [realization coordinates]"));

-- Coefficients, constants, and complete central directions are visible in
-- the expression itself.  Printing must not change the underlying character.
zeroWeightFC = kmWeight(D,{0,0});
signedConstantFC = formalCharacter(D,new Tally from {zeroWeightFC=>-3,omega1=>2});
assert(toString signedConstantFC === "2*e^(1, 0) - 3 [Dynkin coordinates]");
assert(toString formalCharacter(D,new Tally from {zeroWeightFC=>1}) ===
    "1 [Dynkin coordinates]");
assert(toString formalCharacter(D,new Tally from {zeroWeightFC=>-1}) ===
    "-1 [Dynkin coordinates]");
originalTermsFC = characterTerms basic2;
originalHashFC = hash basic2;
dummyNetFC = net basic2;
dummyStringFC = toString basic2;
assert(characterTerms basic2 === originalTermsFC);
assert(hash basic2 == originalHashFC);
assert(toString vermaModule(D,omega1) === "M(1, 0)");
assert(toString M === "L(1, 0)");

deltaFC = toWeight affineNullRoot Daff;
shiftedBasicFC = moduleCharacter(
    integrableHighestWeightModule(Daff,Lambda0+2*deltaFC),Depth=>2);
assert(toString shiftedBasicFC === concatenate(
    "e^(1, 0, 2) + e^(-1, 2, 1) + e^(1, 0, 1) + ... ",
    "[realization coordinates; through root depth 2]"));
for ADisplayFC in {cartanAffineB 3,cartanAffineC 3,cartanAffineG2(),
    transpose cartanAffineG2(),matrix{{2,-4},{-1,2}}} do (
    DDisplayFC := rootDatum ADisplayFC;
    basisDisplayFC := weightBasis DDisplayFC;
    shiftedLambdaDisplayFC := first basisDisplayFC + 3*last basisDisplayFC;
    CDisplayFC := moduleCharacter(integrableHighestWeightModule(
        DDisplayFC,shiftedLambdaDisplayFC),Depth=>2);
    expectedHighestDisplayFC := "e^(" | concatenate between(", ",
        apply(realizationCoordinates shiftedLambdaDisplayFC,toString)) | ")";
    assert(substring(0,#expectedHighestDisplayFC,toString CDisplayFC) ===
        expectedHighestDisplayFC);
    assert(match("through root depth 2",toString CDisplayFC));
    assert(characterCoefficient(CDisplayFC,shiftedLambdaDisplayFC) == 1);
);
assert(toString finiteCharacter(DextraFC,
    weightFromRealizationCoordinates(DextraFC,{1,8})) =!= toString fullCharacterFC);

-- Numerical exponents come from the weights themselves, never from an
-- inferred highest weight or optional truncation information.
badAnchorFC = formalCharacter(D,new Tally from {omega1=>1},
    CharacterStatus=>"Truncated",Truncation=>new HashTable from {
        "completeThroughDepth"=>2,"highestWeight"=>omega2});
assert(toString badAnchorFC === "e^(1, 0) + ... [Dynkin coordinates; truncated]");
foreignAnchorFC = formalCharacter(D,new Tally from {omega1=>1},
    CharacterStatus=>"Truncated",Truncation=>new HashTable from {
        "highestWeight"=>kmWeight(DB2,{1,0})});
assert(toString foreignAnchorFC === "e^(1, 0) + ... [Dynkin coordinates; truncated]");
assert(toString moduleCharacter(vermaModule(D,{0,0}),Depth=>1) ===
    "1 + e^(1, -2) + e^(-2, 1) + ... [Dynkin coordinates; through root depth 1]");

-- Levi irreducibles retain both their highest weight and central character.
assert(set apply(keys LD,toString) === set {"V(1, -1)","V(0, 2)"});
assert(toString muLeviBorelFC === "(1, 2, 3)");
assert(not match("(dynkin=|realization=|semisimpleDynkin=|center=)",
    toString basic2 | toString RC | toString fullCharacterFC));

DPreviewFC = rootDatum "A1";
signedPreviewFC = formalCharacter(DPreviewFC,new Tally from apply(25,
    i -> kmWeight(DPreviewFC,{i}) => (-1)^i));
assert(match("9 further weights not shown; virtual dimension 1; exact character",
    toString signedPreviewFC));
assert(characterDimension signedPreviewFC == 1);
assert(#characterTerms signedPreviewFC == 25);
assert(isExactCharacter signedPreviewFC);

///


-------------------------------------------------------------------------
-- Crystals Tests
-------------------------------------------------------------------------

-- CrystalsTests.m2
-- Adversarial tests for the finite crystal models.

TEST ///

mustErrorCrystal = f -> try (f(); false) else true;

crystalIntegralLabels = mu -> apply(datumCoordinates mu,x -> (
    q := x+0_QQ;
    assert(denominator q == 1);
    numerator q
));

-- Verify the normal-crystal axioms at every vertex and every node, rather
-- than checking only a few distinguished paths.
checkCrystalAxioms = B -> (
    D := crystalDatum B;
    A := datumCartanMatrix D;
    vertices := crystalVertices B;
    r := numrows A;
    assert(#vertices > 0);
    assert(all(vertices,b -> crystalParent b === B));
    assert(#(set apply(vertices,crystalVertexData)) == #vertices);
    assert(all(vertices,b -> crystalVertex(B,crystalVertexData b) === b));

    highestVertices := select(vertices,b ->
        all(toList(1..r),i -> crystalE(b,i) === null));
    assert(#highestVertices == 1);
    assert(crystalWeight(first highestVertices) === crystalHighestWeight B);

    for b in vertices do (
        mu := crystalIntegralLabels crystalWeight b;
        assert(parentDatum(crystalWeight b) === D);
        for i from 1 to r do (
            e := crystalE(b,i);
            f := crystalF(b,i);
            epsilon := crystalEpsilon(b,i);
            phi := crystalPhi(b,i);
            assert(instance(epsilon,ZZ) and epsilon >= 0);
            assert(instance(phi,ZZ) and phi >= 0);
            assert(phi-epsilon == mu#(i-1));

            if e =!= null then (
                assert(crystalF(e,i) === b);
                upper := crystalIntegralLabels crystalWeight e;
                assert(upper === for j from 0 to r-1 list mu#j+A_(j,i-1));
            );
            if f =!= null then (
                assert(crystalE(f,i) === b);
                lower := crystalIntegralLabels crystalWeight f;
                assert(lower === for j from 0 to r-1 list mu#j-A_(j,i-1));
            );

            -- epsilon_i and phi_i are the literal lengths to the two ends
            -- of the i-string.
            count := 0;
            cursor := b;
            next := crystalE(cursor,i);
            while next =!= null do (
                count = count+1;
                cursor = next;
                next = crystalE(cursor,i);
            );
            assert(count == epsilon);
            count = 0;
            cursor = b;
            next = crystalF(cursor,i);
            while next =!= null do (
                count = count+1;
                cursor = next;
                next = crystalF(cursor,i);
            );
            assert(count == phi);
        );
    );

    -- Every listed edge is exactly one nonzero f_i, and every such f_i is
    -- listed once.
    edges := crystalEdges B;
    expectedEdges := sum for b in vertices list
        #(select(toList(1..r),i -> crystalF(b,i) =!= null));
    assert(#edges == expectedEdges);
    for edge in edges do (
        assert(edge#"target" === crystalF(edge#"source",edge#"node"));
        assert(crystalE(edge#"target",edge#"node") === edge#"source");
        assert(edge#"nodeLabel" === (cartanDatum D)#"nodeLabels"#(edge#"node"-1));
    );

    C := crystalCharacter B;
    assert(instance(C,FormalCharacter));
    assert(isExactCharacter C);
    assert(characterDatum C === D);
    assert(characterDimension C == #vertices);
    assert(characterTerms C === characterTerms finiteCharacter(D,crystalHighestWeight B));
    true
);

checkSemistandardTableaux = B -> (
    assert(B#"model" === "SemistandardTableaux");
    n := B#"rank"+1;
    for b in crystalVertices B do (
        T := crystalTableau b;
        assert(apply(T,row -> #row) === B#"shape");
        for row in T do (
            assert(all(row,x -> instance(x,ZZ) and x >= 1 and x <= n));
            for j from 1 to #row-1 do assert(row#(j-1) <= row#j);
        );
        if #T > 1 then for row from 1 to #T-1 do
            for column from 0 to #T#row-1 do
                assert(T#(row-1)#column < T#row#column);
    );
    true
);

-------------------------------------------------------------------------
-- Exact type-A tableau crystals
-------------------------------------------------------------------------

DA2 := rootDatum "A2";
BA2adj := highestWeightCrystal(DA2,{1,1});
assert(#crystalVertices BA2adj == 8);
assert(BA2adj#"shape" === {2,1});
assert(checkSemistandardTableaux BA2adj);
assert(checkCrystalAxioms BA2adj);

-- A concrete signature calculation fixes the reading-word convention.
highestAdjointTableau := {{1,1},{2}};
bHighest := crystalVertex(BA2adj,highestAdjointTableau);
assert(crystalWeight bHighest === kmWeight(DA2,{1,1}));
assert(crystalTableau crystalF(bHighest,1) === {{1,2},{2}});
assert(crystalTableau crystalF(bHighest,2) === {{1,1},{3}});
assert(crystalE(crystalF(bHighest,1),1) === bHighest);

-- Weight-zero vertices in the adjoint crystal are distinct mathematical
-- objects.  Their tableaux must remain distinguishable in printed output.
zeroAdjointVertices := select(crystalVertices BA2adj,b ->
    datumCoordinates crystalWeight b === {0,0});
assert(#zeroAdjointVertices == 2);
assert(toString(zeroAdjointVertices#0) =!= toString(zeroAdjointVertices#1));
assert(match("T = \\[1 1; 2\\]",toString bHighest));
assert(match("wt\\(T\\)",toString bHighest));
assert(match("B\\(1, 1\\)",toString BA2adj));
assert(match("wt\\(T\\) = \\(1, 1\\)",toString bHighest));
assert(toString net bHighest === toString bHighest);

-- This tableau has Far-Eastern i=1 signature -+-.  Cancellation leaves the
-- first minus, so e_1 returns the highest tableau and f_1 is undefined.
bCancellation := crystalVertex(BA2adj,{{1,2},{2}});
assert(crystalE(bCancellation,1) === bHighest);
assert(crystalF(bCancellation,1) === null);
assert(crystalEpsilon(bCancellation,1) == 1);
assert(crystalPhi(bCancellation,1) == 0);

-- Exhaust all A_1 weights through 5 omega_1 and all A_2 weights in the
-- 4-by-4 box.  This catches long strings, repeated weight multiplicities,
-- empty shapes, and both fundamental directions.
for k from 0 to 5 do (
    D := rootDatum "A1";
    B := highestWeightCrystal(D,{k});
    assert(#crystalVertices B == k+1);
    assert(checkSemistandardTableaux B);
    assert(checkCrystalAxioms B);
);

for a from 0 to 3 do for b from 0 to 3 do (
    B := highestWeightCrystal(DA2,{a,b});
    assert(checkSemistandardTableaux B);
    assert(checkCrystalAxioms B);
);

-- Nontrivial A_3 shapes include an internal fundamental weight and a shape
-- with three nonempty rows.
DA3 := rootDatum "A3";
for lambda in {{1,1,0},{2,0,1},{0,2,1},{1,1,1}} do (
    B := highestWeightCrystal(DA3,lambda);
    assert(checkSemistandardTableaux B);
    assert(checkCrystalAxioms B);
);

-------------------------------------------------------------------------
-- Canonical node order and simultaneous permutation
-------------------------------------------------------------------------

A3 := cartanA 4;
permutation := {2,0,1};
A3p := submatrix(A3,permutation,permutation);
Dp := rootDatum cartanDatum(A3p,NodeLabels=>{"old3","old1","old2"});
lambda := {2,1,0};
lambdap := apply(permutation,i -> lambda#i);
B := highestWeightCrystal(DA3,lambda);
Bp := highestWeightCrystal(Dp,lambdap);
assert(Bp#"canonicalNodeOrder" ===
    (first((cartanDatum Dp)#"components"))#"canonicalNodeOrder");
assert(#crystalVertices Bp == #crystalVertices B);
assert(checkSemistandardTableaux Bp);
assert(checkCrystalAxioms Bp);

-- Weight multiplicities commute with the same coordinate permutation.
C := crystalCharacter B;
Cp := crystalCharacter Bp;
for term in pairs characterTerms C do (
    permutedWeight := apply(permutation,i -> (datumCoordinates(term#0))#i);
    assert(characterCoefficient(Cp,kmWeight(Dp,permutedWeight)) == term#1);
);
assert(#characterSupport C == #characterSupport Cp);

-- Noninteger node labels are accepted as an accessibility convenience,
-- while the returned edge also records that label.
assert(crystalF(first crystalVertices Bp,"old3") ===
    crystalF(first crystalVertices Bp,1));

-- Row/column matrix highest weights use the same public conversion path as
-- lists.  Integer node labels outside 1..rank are also usable, while ordinary
-- 1-based positions retain precedence.
DnumericLabels := rootDatum cartanDatum(cartanA 3,NodeLabels=>{10,20});
BmatrixRow := highestWeightCrystal(DnumericLabels,matrix{{1,1}});
BmatrixColumn := highestWeightCrystal(cartanDatum DnumericLabels,
    transpose matrix{{1,1}});
assert(#crystalVertices BmatrixRow == 8);
assert(#crystalVertices BmatrixColumn == 8);
bNumeric := first crystalVertices BmatrixRow;
assert(crystalF(bNumeric,10) === crystalF(bNumeric,1));
assert(#crystalVertices minusculeCrystal(DnumericLabels,10) == 3);

DwordLabels := rootDatum cartanDatum(cartanA 3,
    NodeLabels=>{"left","right"});
assert(#crystalVertices minusculeCrystal(DwordLabels,"left") == 3);
assert(#crystalVertices highestWeightCrystal(
    datumCartanMatrix DwordLabels,transpose matrix{{1,0}}) == 3);

-------------------------------------------------------------------------
-- Classification-free minuscule orbit crystals
-------------------------------------------------------------------------

DD4 := rootDatum "D4";
BD4 := minusculeCrystal(DD4,1);
assert(BD4#"model" === "MinusculeOrbit");
assert(#crystalVertices BD4 == 8);
assert(checkCrystalAxioms BD4);
assert(match("b\\(1, 0, 0, 0\\)",toString first crystalVertices BD4));

-- The orbit construction itself is equivariant under simultaneous node
-- permutation, independently of the type-A tableau recognition above.
AD4 := cartanD 4;
permutationD4 := {3,1,0,2};
AD4p := submatrix(AD4,permutationD4,permutationD4);
DD4p := rootDatum AD4p;
lambdaD4p := apply(permutationD4,i -> ({1,0,0,0})#i);
BD4p := minusculeCrystal(DD4p,lambdaD4p);
assert(checkCrystalAxioms BD4p);
CD4 := crystalCharacter BD4;
CD4p := crystalCharacter BD4p;
for term in pairs characterTerms CD4 do (
    permutedWeight := apply(permutationD4,i -> (datumCoordinates(term#0))#i);
    assert(characterCoefficient(CD4p,kmWeight(DD4p,permutedWeight)) == term#1);
);

-- A second nonsimply-laced example guards the root/coroot convention.
DC3 := rootDatum "C3";
BC3 := minusculeCrystal(DC3,{1,0,0});
assert(#crystalVertices BC3 == 6);
assert(checkCrystalAxioms BC3);

-- Nonsimply-laced spin and exceptional minuscule representations guard the
-- distinction between minuscule and cominuscule numbering.  Their dimensions
-- are 8, 27, 27, and 56 respectively.
DB3 := rootDatum "B3";
BB3spin := minusculeCrystal(DB3,3);
assert(#crystalVertices BB3spin == 8);
assert(checkCrystalAxioms BB3spin);

DE6 := rootDatum "E6";
for nodeE6 in {1,6} do (
    BE6 := minusculeCrystal(DE6,nodeE6);
    assert(#crystalVertices BE6 == 27);
    assert(checkCrystalAxioms BE6);
);

BE7 := minusculeCrystal(rootDatum "E7",7);
assert(#crystalVertices BE7 == 56);
assert(checkCrystalAxioms BE7);

AB3 := cartanB 3;
permutationB3 := {2,0,1};
AB3p := submatrix(AB3,permutationB3,permutationB3);
DB3p := rootDatum AB3p;
BB3p := minusculeCrystal(DB3p,apply(permutationB3,i -> ({0,0,1})#i));
assert(#crystalVertices BB3p == 8);
assert(checkCrystalAxioms BB3p);

-- The zero highest weight is minuscule and gives the one-vertex trivial
-- crystal in every connected finite type.
BG2zero := minusculeCrystal(rootDatum "G2",{0,0});
assert(#crystalVertices BG2zero == 1);
assert(#crystalEdges BG2zero == 0);
assert(checkCrystalAxioms BG2zero);

-- Complete weights in a nonminimal realization retain their Weyl-fixed
-- coordinate at every vertex.  This tests both construction routes because
-- A1 admits the tableau model and its fundamental weight is minuscule.
RextraCrystal := realization(cartanDatum(cartanA 2),
    matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
DextraCrystal := rootDatum RextraCrystal;
lambdaExtraCrystal := weightFromRealizationCoordinates(DextraCrystal,{1,3});
for BextraCrystal in {
    highestWeightCrystal(DextraCrystal,lambdaExtraCrystal),
    minusculeCrystal(DextraCrystal,lambdaExtraCrystal)
} do (
    assert(checkCrystalAxioms BextraCrystal);
    highestExtraCrystal := first select(crystalVertices BextraCrystal,b ->
        crystalE(b,1) === null);
    assert(crystalWeight highestExtraCrystal === lambdaExtraCrystal);
    assert(all(crystalVertices BextraCrystal,b ->
        (realizationCoordinates crystalWeight b)#1 == 3));
    assert(characterTerms crystalCharacter BextraCrystal ==
        characterTerms finiteCharacter(DextraCrystal,lambdaExtraCrystal));
);

-------------------------------------------------------------------------
-- Guardrails, malformed input, and parent safety
-------------------------------------------------------------------------

assert(mustErrorCrystal(() -> highestWeightCrystal(DA2,{1,1},MaxElements=>7)));
assert(#crystalVertices highestWeightCrystal(DA2,{1,1},MaxElements=>8) == 8);
assert(mustErrorCrystal(() -> highestWeightCrystal(DA2,{1,0},MaxElements=>0)));
assert(mustErrorCrystal(() -> highestWeightCrystal(DA2,{1,0},MaxElements=>false)));
assert(#crystalVertices highestWeightCrystal(DA2,{1,0},MaxElements=>null) == 3);
assert(mustErrorCrystal(() -> minusculeCrystal(DD4,1,MaxElements=>7)));
assert(#crystalVertices minusculeCrystal(DD4,1,MaxElements=>8) == 8);

assert(#crystalVertices highestWeightCrystal(rootDatum "B2",{1,0}) == 5);
assert(mustErrorCrystal(() -> highestWeightCrystal(rootDatum(cartanAffineA 1),{1,0})));
disconnected := matrix{{2,-1,0},{-1,2,0},{0,0,2}};
assert(#crystalVertices highestWeightCrystal(rootDatum disconnected,{1,0,0}) == 3);
assert(mustErrorCrystal(() -> highestWeightCrystal(DA2,{-1,2})));
assert(mustErrorCrystal(() -> highestWeightCrystal(DA2,{1/2,0})));
assert(mustErrorCrystal(() -> highestWeightCrystal(DA2,kmWeight(rootDatum "B2",{1,0}))));

assert(mustErrorCrystal(() -> minusculeCrystal(DD4,{0,1,0,0})));
assert(mustErrorCrystal(() -> minusculeCrystal(DD4,2)));
assert(mustErrorCrystal(() -> minusculeCrystal(DD4,0)));

vA2 := first crystalVertices BA2adj;
BA3 := highestWeightCrystal(DA3,{1,0,0});
assert(not crystalContains(BA3,vA2));
assert(mustErrorCrystal(() -> crystalWeight(BA3,vA2)));
assert(mustErrorCrystal(() -> crystalF(BA3,vA2,1)));
assert(mustErrorCrystal(() -> crystalE(BA3,vA2,1)));
assert(mustErrorCrystal(() -> crystalVertex(BA2adj,{{3,1}})));
assert(mustErrorCrystal(() -> crystalF(vA2,0)));
assert(mustErrorCrystal(() -> crystalE(vA2,3)));
assert(mustErrorCrystal(() -> crystalF(vA2,"not-a-node")));
assert(mustErrorCrystal(() -> crystalTableau(first crystalVertices BD4)));

-- Exporting the vertex type must not make a forged parent tag sufficient for
-- membership: its data must label an actual vertex of the crystal.
forgedA2 := new CrystalVertex from {
    "parent"=>BA2adj,"data"=>{{99}}
};
assert(not crystalContains(BA2adj,forgedA2));
assert(mustErrorCrystal(() -> crystalWeight(BA2adj,forgedA2)));
assert(mustErrorCrystal(() -> crystalF(BA2adj,forgedA2,1)));
assert(mustErrorCrystal(() -> crystalWeight forgedA2));
assert(mustErrorCrystal(() -> crystalTableau forgedA2));
assert(mustErrorCrystal(() -> crystalF(forgedA2,1)));
assert(mustErrorCrystal(() -> crystalE(forgedA2,1)));
assert(mustErrorCrystal(() -> crystalEpsilon(forgedA2,1)));
assert(mustErrorCrystal(() -> crystalPhi(forgedA2,1)));

///


-------------------------------------------------------------------------
-- Kostant Multiplicity Tests
-------------------------------------------------------------------------

TEST ///

A2 = cartanA 3;
B2 = cartanB 2;
G2 = cartanG2();

assert(kostantPartitionFunction(A2,{0,0}) == 1);
assert(kostantPartitionFunction(A2,{1,0}) == 1);
assert(kostantPartitionFunction(A2,{1,1}) == 2);
assert(kostantPartitionFunction(A2,{2,2}) == 3);
assert(kostantPartitionFunction(A2,{-1,2}) == 0);

-- Exhaustive cross-checks use the independent Freudenthal recurrence.
for fixture in {{A2,{3,2}},{B2,{2,1}},{G2,{1,1}}} do (
    A := fixture#0;
    lambda := fixture#1;
    for mu in weightsOfRep(A,lambda) do
        assert(kostantWeightMultiplicity(A,lambda,mu) ==
            weightMultiplicity(A,lambda,mu,Strategy=>"Freudenthal"));
    assert(weightMultiplicityTable(A,lambda,Strategy=>"Kostant") ==
        weightMultiplicityTable(A,lambda,Strategy=>"Freudenthal"));
);

D = rootDatum "A2";
assert(kostantWeightMultiplicity(D,kmWeight(D,{1,1}),kmWeight(D,{0,0})) == 2);
assert(kostantPartitionFunction(D,kmRoot(D,{1,1})) == 2);

-- Weights in a different root-lattice coset or outside the representation
-- have multiplicity zero, not an exception.
assert(kostantWeightMultiplicity(A2,{1,0},{0,0}) == 0);
assert(kostantWeightMultiplicity(A2,{1,0},{5,-2}) == 0);

///

TEST ///

-- Complete weights in a nonminimal finite realization retain their central
-- character.  Equal Dynkin labels do not suffice for a nonzero multiplicity.
mustErrorKM = f -> (try (f();false) else true);
RextraKM = realization(cartanDatum "A1",
    matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
DextraKM = rootDatum RextraKM;
lambdaKM = weightFromRealizationCoordinates(DextraKM,{1,7});
alphaKM = kmRoot(DextraKM,{1});
lowerKM = lambdaKM-toWeight alphaKM;
shiftedHighestKM = weightFromRealizationCoordinates(DextraKM,{1,8});
shiftedLowerKM = weightFromRealizationCoordinates(DextraKM,{-1,8});
characterKM = finiteCharacter(DextraKM,lambdaKM);
for mu in {lambdaKM,lowerKM,shiftedHighestKM,shiftedLowerKM} do
    assert(kostantWeightMultiplicity(DextraKM,lambdaKM,mu) ==
        characterCoefficient(characterKM,mu));
assert(kostantWeightMultiplicity(DextraKM,lambdaKM,shiftedHighestKM) == 0);
assert(kostantWeightMultiplicity(DextraKM,lambdaKM,shiftedLowerKM) == 0);
assert(kostantWeightMultiplicity(DextraKM,kmWeight(DextraKM,{1}),
    kmWeight(DextraKM,{-1})) == 1);
assert(mustErrorKM(() -> kostantWeightMultiplicity(DextraKM,
    lambdaKM,kmWeight(DextraKM,{1}))));
assert(mustErrorKM(() -> kostantWeightMultiplicity(DextraKM,
    kmWeight(DextraKM,{1}),lambdaKM)));

-- An ambient coordinate can change along a root direction.  Comparing
-- the last coordinate alone would incorrectly discard these genuine weights.
RmixedKM = realization(cartanDatum "A1",
    matrix(QQ,{{2,2}}),matrix(QQ,{{1,0}}));
DmixedKM = rootDatum RmixedKM;
highestMixedKM = weightFromRealizationCoordinates(DmixedKM,{3,7});
centralMixedKM = weightFromRealizationCoordinates(DmixedKM,{0,1});
characterMixedKM = finiteCharacter(DmixedKM,highestMixedKM);
for k from 0 to 3 do (
    mu := highestMixedKM-toWeight kmRoot(DmixedKM,{k});
    assert(kostantWeightMultiplicity(DmixedKM,highestMixedKM,mu) == 1);
    assert(kostantWeightMultiplicity(DmixedKM,highestMixedKM,mu) ==
        characterCoefficient(characterMixedKM,mu));
    assert(kostantWeightMultiplicity(DmixedKM,highestMixedKM,
        mu+centralMixedKM) == 0);
);

-- A rational change of ambient basis mixes two invisible directions with
-- the root span.  Test every weight against the independent character,
-- and then shift each weight without changing any Dynkin label.
A2KM = cartanA 3;
G2KM = matrix(QQ,{{1,1,0,0},{0,2,1,0},
    {0,0,1/3,1},{1,0,0,2}});
rootRows2KM = (transpose A2KM ** QQ) | matrix mutableMatrix(QQ,2,2);
corootRows2KM = id_(QQ^2) | matrix mutableMatrix(QQ,2,2);
R2KM = realization(cartanDatum A2KM,
    rootRows2KM*G2KM,corootRows2KM*transpose inverse G2KM);
D2KM = rootDatum R2KM;
lift2KM = v -> weightFromRealizationCoordinates(D2KM,
    flatten entries(matrix(QQ,{v})*G2KM));
lambda2KM = lift2KM {2,1,3/2,-7/3};
central2KM = lift2KM {0,0,1/2,-2/5};
C2KM = finiteCharacter(D2KM,lambda2KM);
assert(datumCoordinates lambda2KM == {2,1});
assert(datumCoordinates central2KM == {0,0});
for mu in characterSupport C2KM do (
    assert(kostantWeightMultiplicity(D2KM,lambda2KM,mu) ==
        characterCoefficient(C2KM,mu));
    assert(kostantWeightMultiplicity(D2KM,lambda2KM,mu+central2KM) == 0);
    assert(kostantWeightMultiplicity(D2KM,lambda2KM+central2KM,
        mu+central2KM) == characterCoefficient(C2KM,mu));
);
assert(sum apply(characterSupport C2KM,
    mu -> kostantWeightMultiplicity(D2KM,lambda2KM,mu)) ==
    characterDimension C2KM);
assert(kostantWeightMultiplicity(D2KM,lift2KM {1,0,3/2,-7/3},
    lift2KM {0,0,3/2,-7/3}) == 0);
assert(kostantWeightMultiplicity(D2KM,lambda2KM,
    lambda2KM+toWeight kmRoot(D2KM,{1,0})) == 0);

-- Nonsymmetric Cartan matrices also use the same full root-span criterion;
-- the transpose convention for simple-root rows must not be reversed.
for fixture in {{cartanB 2,{1,1}},{cartanG2(),{1,0}}} do (
    A := fixture#0;
    labels := fixture#1;
    R := realization(cartanDatum A,
        ((transpose A ** QQ) | matrix mutableMatrix(QQ,2,2))*G2KM,
        corootRows2KM*transpose inverse G2KM);
    D := rootDatum R;
    highest := weightFromRealizationCoordinates(D,
        flatten entries(matrix(QQ,{labels | {1/3,2/7}})*G2KM));
    central := weightFromRealizationCoordinates(D,
        flatten entries(matrix(QQ,{{0,0,1,0}})*G2KM));
    C := finiteCharacter(D,highest);
    for j from 0 to 1 do (
        simpleRoot := kmRoot(D,apply(2,i -> if i == j then 1 else 0));
        firstLowerWeight := highest-toWeight simpleRoot;
        assert(kostantWeightMultiplicity(D,highest,firstLowerWeight) ==
            if labels#j > 0 then 1 else 0);
    );
    for mu in characterSupport C do (
        assert(kostantWeightMultiplicity(D,highest,mu) ==
            characterCoefficient(C,mu));
        assert(kostantWeightMultiplicity(D,highest,mu+central) == 0);
    );
);

-- Product root systems must preserve every central coordinate as well.
-- The nontrivial zero-weight multiplicity tests a product with an adjoint
-- factor rather than only multiplicity-free representations.
AproductKM = (cartanA 2) ++ (cartanA 3);
GproductKM = matrix(QQ,{{1,1,0,0,0},{0,1,1,0,0},
    {0,0,1,1,0},{0,0,0,1,1},{1,0,0,0,2}});
rootRowsProductKM = (transpose AproductKM ** QQ) |
    matrix mutableMatrix(QQ,3,2);
corootRowsProductKM = id_(QQ^3) | matrix mutableMatrix(QQ,3,2);
DproductKM = rootDatum realization(cartanDatum AproductKM,
    rootRowsProductKM*GproductKM,
    corootRowsProductKM*transpose inverse GproductKM);
liftProductKM = v -> weightFromRealizationCoordinates(DproductKM,
    flatten entries(matrix(QQ,{v})*GproductKM));
lambdaProductKM = liftProductKM {2,1,1,5,-2};
centralProductKM = liftProductKM {0,0,0,1,0};
CproductKM = finiteCharacter(DproductKM,lambdaProductKM);
for mu in characterSupport CproductKM do (
    assert(kostantWeightMultiplicity(DproductKM,lambdaProductKM,mu) ==
        characterCoefficient(CproductKM,mu));
    assert(kostantWeightMultiplicity(DproductKM,lambdaProductKM,
        mu+centralProductKM) == 0);
);
assert(kostantWeightMultiplicity(DproductKM,lambdaProductKM,
    liftProductKM {0,0,0,5,-2}) == 2);
assert(characterDimension CproductKM == 24);

-- Minimal realizations canonicalize complete weights to their Dynkin
-- labels, including when the ambient basis is not the standard basis.
DminimalKM = rootDatum realization(cartanDatum "A1",
    matrix(QQ,{{4}}),matrix(QQ,{{1/2}}));
lambdaMinimalKM = weightFromRealizationCoordinates(DminimalKM,{2});
muMinimalKM = weightFromRealizationCoordinates(DminimalKM,{-2});
assert(kostantWeightMultiplicity(DminimalKM,lambdaMinimalKM,muMinimalKM) == 1);
assert(kostantWeightMultiplicity(DminimalKM,lambdaMinimalKM,
    kmWeight(DminimalKM,{-1})) == 1);

-- Invalid labels and wrong parents must still be rejected, even when an
-- independent central mismatch could already force a multiplicity of zero.
assert(mustErrorKM(() -> kostantWeightMultiplicity(DextraKM,
    weightFromRealizationCoordinates(DextraKM,{-1,7}),shiftedHighestKM)));
assert(mustErrorKM(() -> kostantWeightMultiplicity(DextraKM,
    lambdaKM,weightFromRealizationCoordinates(DextraKM,{1/2,8}))));
DwrongKM = rootDatum realization(cartanDatum(cartanA 2,NodeLabels=>{"other"}),
    matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
assert(mustErrorKM(() -> kostantWeightMultiplicity(DextraKM,
    lambdaKM,weightFromRealizationCoordinates(DwrongKM,{1,7}))));
assert(mustErrorKM(() -> kostantWeightMultiplicity(DextraKM,
    weightFromRealizationCoordinates(DwrongKM,{1,7}),lambdaKM)));
assert(mustErrorKM(() -> kostantWeightMultiplicity(rootDatum cartanAffineA 1,
    kmWeight(rootDatum cartanAffineA 1,{1,0}),
    kmWeight(rootDatum cartanAffineA 1,{1,0}))));

-- A pure torus has no root directions: its one-dimensional character is
-- concentrated at its full highest weight, not at every weight of label {}.
A0KM = matrix mutableMatrix(ZZ,0,0);
E0KM = matrix mutableMatrix(QQ,0,2);
D0KM = rootDatum realization(cartanDatum A0KM,E0KM,E0KM);
lambda0KM = weightFromRealizationCoordinates(D0KM,{2,-3});
mu0KM = weightFromRealizationCoordinates(D0KM,{2,-2});
assert(kostantWeightMultiplicity(D0KM,lambda0KM,lambda0KM) == 1);
assert(kostantWeightMultiplicity(D0KM,lambda0KM,mu0KM) == 0);
assert(kostantWeightMultiplicity(D0KM,kmWeight(D0KM,{}),kmWeight(D0KM,{})) == 1);
assert(mustErrorKM(() -> kostantWeightMultiplicity(D0KM,lambda0KM,kmWeight(D0KM,{}))));

-- A KMRoot already gives its complete simple-root coefficients.  The
-- partition number is therefore independent of the ambient basis or center.
assert(kostantPartitionFunction(DextraKM,kmRoot(DextraKM,{2})) == 1);
assert(kostantPartitionFunction(D2KM,kmRoot(D2KM,{1,1})) == 2);
assert(kostantPartitionFunction(DproductKM,kmRoot(DproductKM,{1,1,1})) == 2);
assert(kostantPartitionFunction(D0KM,kmRoot(D0KM,{})) == 1);
assert(mustErrorKM(() -> kostantPartitionFunction(DextraKM,kmRoot(DwrongKM,{1}))));

///


-------------------------------------------------------------------------
-- Geometry Tests
-------------------------------------------------------------------------

TEST ///

mustErrorGeo = f -> (try (f(); false) else true)

D1 = rootDatum "A1";
R = borelWeilBott(D1,{4});
assert(not R#"vanishes" and R#"degree" == 0);
assert(datumCoordinates(R#"dominantWeight") === {4});
assert(toString R === "H^0 = V(4)");
assert(characterDimension borelWeilBottCharacter(D1,{4}) == 5);
assert((borelWeilBott(D1,{-1}))#"vanishes");
Rneg = borelWeilBott(D1,{-3});
assert(not Rneg#"vanishes" and Rneg#"degree" == 1);
assert(datumCoordinates(Rneg#"dominantWeight") === {1});
assert(toString Rneg === "H^1 = V(1)");

D2 = rootDatum "A2";
R2 = borelWeilBott(D2,{-3,0});
assert(R2#"degree" == 2);
assert(datumCoordinates(R2#"dominantWeight") === {0,0});
assert((borelWeilBott(D2,{-2,0}))#"vanishes");

-- The displayed module is the actual cohomology, not the undualized
-- dominant weight.  For the stated line-bundle convention (1,0) gives
-- H^0 = V(0,1).  The display records only the nonzero cohomology.
Rfund = borelWeilBott(D2,{1,0});
assert(datumCoordinates(Rfund#"cohomologyHighestWeight") === {0,1});
assert(toString Rfund === "H^0 = V(0, 1)");
assert(toString R2 === "H^2 = V(0, 0)");
assert(toString borelWeilBott(D2,{-2,0}) === "0");
assert(toString net Rfund === toString Rfund);

-- Full weights in a nonminimal realization carry a central character.
-- Borel--Weil--Bott retains it through the dot action and reverses its sign
-- in the actual cohomology module.  A skew root tests more than zero padding.
RextraGeo = realization(cartanDatum(cartanA 2),
    matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
DextraGeo = rootDatum RextraGeo;
lambdaExtraGeo = weightFromRealizationCoordinates(DextraGeo,{-3,7});
bwbExtraGeo = borelWeilBott(DextraGeo,lambdaExtraGeo);
assert(bwbExtraGeo#"degree" == 1);
assert(realizationCoordinates(bwbExtraGeo#"dominantWeight") === {1,7});
assert(realizationCoordinates(bwbExtraGeo#"cohomologyHighestWeight") === {1,-7});
assert(dualWeight(bwbExtraGeo#"cohomologyHighestWeight") ===
    bwbExtraGeo#"dominantWeight");
assert(toString bwbExtraGeo === "H^1 = V(1, -7)");

RskewGeo = realization(cartanDatum(cartanA 2),
    matrix(QQ,{{2,1}}),matrix(QQ,{{1,0}}));
DskewGeo = rootDatum RskewGeo;
lambdaSkewGeo = weightFromRealizationCoordinates(DskewGeo,{-3,7});
bwbSkewGeo = borelWeilBott(DskewGeo,lambdaSkewGeo);
assert(realizationCoordinates(bwbSkewGeo#"dominantWeight") === {1,9});
assert(realizationCoordinates(bwbSkewGeo#"cohomologyHighestWeight") === {1,-8});
assert(toString bwbSkewGeo === "H^1 = V(1, -8)");
assert(dualWeight(bwbSkewGeo#"cohomologyHighestWeight") ===
    bwbSkewGeo#"dominantWeight");
assert(characterTerms borelWeilBottCharacter(DskewGeo,lambdaSkewGeo) ===
    characterTerms finiteCharacter(DskewGeo,bwbSkewGeo#"cohomologyHighestWeight"));

-- Label classes are still label classes; the new display must not invent
-- an unrequested central character for this equally valid input.
bwbClassGeo = borelWeilBott(DskewGeo,{-3});
assert((bwbClassGeo#"dominantWeight")#"realizationCoordinates" === null);
assert((bwbClassGeo#"cohomologyHighestWeight")#"realizationCoordinates" === null);
assert(toString bwbClassGeo === "H^1 = V(1) mod ann(h')");

demazureStep = demazureCharacter(D2,{1,0},{1});
assert((characterTerms demazureStep) == (new Tally from {
    (kmWeight(D2,{1,0}))=>1,
    (kmWeight(D2,{-1,1}))=>1}));
assert(demazureCharacter(D2,{1,0},longElement datumCartanMatrix D2) ===
    finiteCharacter(D2,{1,0}));
assert(demazureCharacter(D2,{2,1},{1,2,1}) ===
    demazureCharacter(D2,{2,1},{2,1,2}));

-- The piecewise negative-label branch is part of the Demazure operator,
-- even though dominant Demazure characters begin with a dominant monomial.
negativeMonomial = formalCharacter(D2,new Tally from {kmWeight(D2,{-2,1})=>1});
assert((characterTerms demazureOperator(1,negativeMonomial)) ==
    (new Tally from {(kmWeight(D2,{0,0}))=>-1}));

-- Demazure modules remain finite-dimensional in affine type for a finite
-- Weyl word, and full realization coordinates survive every operator.
Daff = rootDatum cartanAffineA 1;
Lambda0 = weightFromRealizationCoordinates(Daff,{1,0,0});
affDem = demazureCharacter(Daff,Lambda0,{1,2,1});
assert(isExactCharacter affDem);
assert(all(characterSupport affDem,mu -> mu#"realizationCoordinates" =!= null));

assert(mustErrorGeo(() -> borelWeilBott(Daff,Lambda0)));
assert(mustErrorGeo(() -> demazureCharacter(D2,{-1,0},{1})));

///


-------------------------------------------------------------------------
-- Category OTests
-------------------------------------------------------------------------

TEST ///

mustErrorO = f -> (try (f(); false) else true)

A2 = cartanA 3;
W2 = weylGroup A2;
e2 = identityWeylElement W2;
w02 = weylElement(W2,longElement A2);
q = kazhdanLusztigVariable();
elementsA2 = apply(weylGroupElements A2,h -> weylElement(W2,h#"word"));
for u in elementsA2 do for v in elementsA2 do (
    P := kazhdanLusztigPolynomial(u,v);
    if bruhatLessOrEqual(u,v) then assert(P == 1) else assert(P == 0);
);

-- The first singular Schubert pattern in type A3 gives 1+q.
A3 = cartanA 4;
W3 = weylGroup A3;
e3 = identityWeylElement W3;
singular = weylElement(W3,{2,1,3,2});
assert(kazhdanLusztigPolynomial(e3,singular) == 1+q);

-- These are ordinary polynomials in ZZ[q], with the conventional displayed
-- variable.  Reading the result does not require unpacking a coefficient
-- list, and the usual arithmetic, coefficient and substitution commands
-- remain available.
Pdisplay = kazhdanLusztigPolynomial(e3,singular);
PzeroDisplay = kazhdanLusztigPolynomial(singular,e3);
PoneDisplay = kazhdanLusztigPolynomial(e3,e3);
assert(instance(Pdisplay,RingElement));
assert(instance(ring Pdisplay,PolynomialRing));
assert(ring Pdisplay === ring q);
assert(toString q === "q");
assert(toString Pdisplay === "q+1");
assert(toString net Pdisplay === "q + 1");
assert(instance(PzeroDisplay,RingElement) and instance(PoneDisplay,RingElement));
assert(toString PzeroDisplay === "0" and toString net PzeroDisplay === "0");
assert(toString PoneDisplay === "1" and toString net PoneDisplay === "1");
assert((Pdisplay-1)^2 == q^2);
assert(Pdisplay*(Pdisplay+1) == q^2+3*q+2);
assert(coefficient(q,Pdisplay) == 1);
assert(coefficient(q^2,Pdisplay) == 0);
assert(sub(Pdisplay,{q=>0}) == 1);
assert(sub(Pdisplay,{q=>2}) == 3);
assert(sub(Pdisplay,{q=>-1}) == 0);
-- This lower endpoint is the standard A3 example in Sage's independent KL
-- implementation.  It exercises the c=1 branch of the right-descent
-- recurrence rather than only a principal interval.
assert(kazhdanLusztigPolynomial(
    weylElement(W3,{2}),singular) == 1+q);
assert(kazhdanLusztigPolynomial(e3,
    weylElement(W3,longElement A3)) == 1);
assert(kazhdanLusztigPolynomial(singular,e3) == 0);

-- Independent published B3 fixtures, including a disconnected lower word.
B3KL = cartanB 3;
WB3KL = weylGroup B3KL;
assert(kazhdanLusztigPolynomial(
    weylElement(WB3KL,{2}),
    weylElement(WB3KL,{3,2,3,1,2})) == 1+q);
assert(kazhdanLusztigPolynomial(
    weylElement(WB3KL,{1,3}),
    weylElement(WB3KL,{1,3,2,1,3})) == 1+q);

-- KL theory depends on the Coxeter matrix, not root lengths or a chosen node
-- order.  Transposing a nonsimply-laced Cartan matrix and simultaneously
-- permuting its nodes leave the coefficients unchanged.
fixtureLowerB3 = {2};
fixtureUpperB3 = {3,2,3,1,2};
fixtureCoefficientsB3 = kazhdanLusztigCoefficients(
    B3KL,fixtureLowerB3,fixtureUpperB3);
assert(fixtureCoefficientsB3 === kazhdanLusztigCoefficients(
    transpose B3KL,fixtureLowerB3,fixtureUpperB3));
permB3 = {2,3,1};
permutedB3 = matrix table(3,3,
    (i,j) -> B3KL_(permB3#i-1,permB3#j-1));
remapB3Word = word -> apply(word,old ->
    first positions(permB3,node -> node == old)+1);
assert(fixtureCoefficientsB3 === kazhdanLusztigCoefficients(
    permutedB3,remapB3Word fixtureLowerB3,remapB3Word fixtureUpperB3));

-- Degree, constant-term, and interval conditions on every principal and
-- terminal A3 interval, together with every Bruhat cover.  This exercises
-- all 24 elements while keeping the official package test responsive.
elementsA3 = apply(weylGroupElements A3,h -> weylElement(W3,h#"word"));
longestA3 = weylElement(W3,longElement A3);
testPairs = join(
    apply(elementsA3,v -> {e3,v}),
    apply(elementsA3,u -> {u,longestA3}),
    flatten apply(elementsA3,u -> apply(elementsA3,v ->
        if bruhatLessOrEqual(u,v) and weylLength v == weylLength u+1
        then {u,v} else continue)));
for pair in unique testPairs do (
    u := pair#0;
    v := pair#1;
    coeffs := kazhdanLusztigCoefficients(u,v);
    if bruhatLessOrEqual(u,v) then (
        assert(first coeffs == 1);
        if u =!= v then assert(2*(#coeffs-1) <= weylLength(v)-weylLength(u)-1);
    ) else assert(coeffs === {0});
);

-- A short query in A8 has a two-element principal Bruhat interval, even
-- though the ambient Weyl group has 9! elements and exceeds the former
-- whole-group default guard.  The guard now measures the interval actually
-- materialized, including when that interval is already cached.
assert(kazhdanLusztigPolynomial(cartanA 9,{}, {1}) == 1);
assert(kazhdanLusztigPolynomial(
    cartanA 9,{}, {1},MaxElements=>2) == 1);
assert(mustErrorO(() -> kazhdanLusztigPolynomial(
    cartanA 9,{}, {1},MaxElements=>1)));

-- Principal Bruhat intervals are finite in every Coxeter group.  These
-- fixed-element computations therefore work without finite-type case splits.
A1affKL = cartanAffineA 1;
assert(kazhdanLusztigPolynomial(
    A1affKL,{}, {1,2,1,2,1}) == 1);
rankTwoIndefiniteKL = matrix{{2,-3},{-3,2}};
assert(kazhdanLusztigPolynomial(
    rankTwoIndefiniteKL,{1},{1,2,1,2}) == 1);
assert(mustErrorO(() -> kazhdanLusztigPolynomial(
    A1affKL,{}, {0,1})));
assert(mustErrorO(() -> kazhdanLusztigPolynomial(
    A1affKL,{}, {3})));

D = rootDatum "A2";
lambda = kmWeight(D,{1,0});
W = weylGroup D;
e = identityWeylElement W;
s1 = simpleWeylReflection(W,1);
assert(datumCoordinates dotWeylAction(s1,lambda) === {-3,2});
assert(vermaCompositionMultiplicity(D,lambda,e,s1) == 1);
factors = vermaCompositionFactors(D,lambda,e);
assert(sum values factors == 6);
assert(#keys factors == 6);

-- Evaluation at q=1 really records nontrivial Jordan--Holder
-- multiplicities, with the dominant-block convention
-- [M(w.lambda):L(x.lambda)] = P_(w,x)(1).
D3block = rootDatum "A3";
lambda3block = kmWeight(D3block,{0,0,0});
W3block = weylGroup D3block;
w3block = weylElement(W3block,{2});
x3block = weylElement(W3block,{2,1,3,2});
assert(vermaCompositionMultiplicity(
    D3block,lambda3block,w3block,x3block) == 2);
factors3block = vermaCompositionFactors(D3block,lambda3block,w3block);
Lx3block = simpleHighestWeightModule(
    D3block,dotWeylAction(x3block,lambda3block));
assert(factors3block#Lx3block == 2);

-- The fixed-factor theorem is valid for symmetrizable Kac--Moody type even
-- though the complete factor Tally is infinite and is therefore refused.
DaffBlock = rootDatum A1affKL;
lambdaAffBlock = kmWeight(DaffBlock,{1,0});
WaffBlock = weylGroup DaffBlock;
assert(vermaCompositionMultiplicity(
    DaffBlock,lambdaAffBlock,identityWeylElement WaffBlock,
    weylElement(WaffBlock,{1,2,1})) == 1);
assert(mustErrorO(() -> vermaCompositionFactors(
    DaffBlock,lambdaAffBlock,identityWeylElement WaffBlock)));

B = bggResolution(D,lambda);
assert(B#"length" == 3);
assert(B#"exact");
assert(B#"computedThroughDegree" == 3);
assert(B#"truncation" === null);
assert(apply(sort keys B#"terms",d -> #(keys(B#"terms"#d))) === {1,2,2,1});
assert(match("BGG terms for L\\(1, 0\\)",toString B));
assert(match("C_0 = M\\(1, 0\\)",toString B));
assert(match("C_1 = M",toString B));
assert(match("\\(\\+\\)",toString B));
assert(match("differential maps not computed",toString B));
assert(toString net B === toString B);

-- A large resolution must summarize its terms instead of expanding all
-- Verma modules, while retaining the last homological degree.
BlargeDisplay = bggResolution(rootDatum "A3",{0,0,0});
assert(match("Verma summands",toString BlargeDisplay));
assert(match("C_6",toString BlargeDisplay));

-- At absolute depth zero only the highest monomial survives.  In particular,
-- a BGG summand whose offset equals the cutoff must be allowed to contribute
-- its depth-zero Verma truncation rather than requesting a height-zero root
-- snapshot.
Euler0 = bggEulerCharacter(B,Depth=>0);
assert(characterTerms Euler0 === new HashTable from {lambda=>1});

-- The alternating sum of truncated Verma characters equals the irreducible
-- character through the same absolute root depth.
H = 5;
Euler = bggEulerCharacter(B,Depth=>H);
full = finiteCharacter(D,lambda);
expectedTerms = new MutableHashTable;
for p in pairs characterTerms full do (
    beta := weightToRootCoordinates(lambda-p#0);
    if all(beta,a -> a >= 0) and sum beta <= H then expectedTerms#(p#0)=p#1;
);
expected = formalCharacter(D,new HashTable from pairs expectedTerms,
    CharacterStatus=>"Truncated",
    Truncation=>new HashTable from {"test"=>H});
assert(characterTerms Euler == characterTerms expected);

-- A finite resolution may also be requested only through a homological
-- degree.  Terms beyond degree k start strictly below absolute root depth k,
-- so the truncated Euler sum remains certified through that depth.
Bfinite1 = bggResolution(D,lambda,MaxDegree=>1);
assert(not Bfinite1#"exact");
assert(Bfinite1#"computedThroughDegree" == 1);
EulerFinite1 = bggEulerCharacter(Bfinite1,Depth=>1);
expectedFinite1Terms = new MutableHashTable;
for p in pairs characterTerms full do (
    beta := weightToRootCoordinates(lambda-p#0);
    if all(beta,a -> a >= 0) and sum beta <= 1 then
        expectedFinite1Terms#(p#0)=p#1;
);
assert(characterTerms EulerFinite1 ==
    new Tally from pairs expectedFinite1Terms);
assert(mustErrorO(() -> bggEulerCharacter(Bfinite1,Depth=>2)));
assert((bggResolution(D,lambda,MaxDegree=>3))#"exact");

assert(mustErrorO(() -> bggResolution(rootDatum cartanAffineA 1,{1,0})));
assert(mustErrorO(() -> kazhdanLusztigPolynomial(
    e,identityWeylElement weylGroup(rootDatum "B2"))));
assert(mustErrorO(() -> bggResolution(D,lambda,MaxDegree=>-1)));

-- The Kac--Moody BGG resolution is infinite, but every homological degree is
-- finite.  For A1^(1) its first three term counts are 1,2,2, and their Euler
-- sum recovers the basic representation through root depth two.
DaffBGG = rootDatum A1affKL;
Lambda0BGG = weightFromRealizationCoordinates(DaffBGG,{1,0,0});
Baff2 = bggResolution(DaffBGG,Lambda0BGG,MaxDegree=>2);
assert(not Baff2#"exact");
assert(Baff2#"computedThroughDegree" == 2);
assert(apply(sort keys Baff2#"terms",d ->
    #(keys(Baff2#"terms"#d))) === {1,2,2});
assert(match("Through homological degree 2",toString Baff2));
assert(match("differential maps not computed",toString Baff2));
assert(match("M\\(-3, 4, -2\\)",toString Baff2));
EulerAff2 = bggEulerCharacter(Baff2,Depth=>2);
integrableAff2 = moduleCharacter(
    integrableHighestWeightModule(DaffBGG,Lambda0BGG),Depth=>2);
assert(characterTerms EulerAff2 == characterTerms integrableAff2);
assert(mustErrorO(() -> bggEulerCharacter(Baff2,Depth=>3)));
assert(mustErrorO(() -> bggEulerCharacter(
    bggResolution(DaffBGG,{1,0},MaxDegree=>2),Depth=>1)));

-- The same construction is not affine-specific: an invertible indefinite
-- realization already has complete Dynkin coordinates, and its degree-two
-- BGG Euler sum agrees with Freudenthal through depth two.
DindBGG = rootDatum rankTwoIndefiniteKL;
lambdaIndBGG = kmWeight(DindBGG,{1,0});
Bind2 = bggResolution(DindBGG,lambdaIndBGG,MaxDegree=>2);
EulerInd2 = bggEulerCharacter(Bind2,Depth=>2);
integrableInd2 = moduleCharacter(
    integrableHighestWeightModule(DindBGG,lambdaIndBGG),Depth=>2);
assert(characterTerms EulerInd2 == characterTerms integrableInd2);

-- The dot action is a genuine action on the full affine realization and is
-- equivariant for translation by delta, which the Weyl group fixes.
uAffDot = weylElement(WaffBlock,{1,2,1});
vAffDot = weylElement(WaffBlock,{2,1});
deltaAffDot = toWeight affineNullRoot DaffBGG;
assert(dotWeylAction(uAffDot*vAffDot,Lambda0BGG) ===
    dotWeylAction(uAffDot,dotWeylAction(vAffDot,Lambda0BGG)));
assert(dotWeylAction(inverseWeylElement uAffDot,
    dotWeylAction(uAffDot,Lambda0BGG)) === Lambda0BGG);
assert(dotWeylAction(uAffDot,Lambda0BGG+deltaAffDot) ===
    dotWeylAction(uAffDot,Lambda0BGG)+deltaAffDot);

-- Category O respects the two honest interpretations of a nonminimal finite
-- realization: a Dynkin-label class modulo the extra central direction, or a
-- complete weight with that central character fixed.  The Weyl group fixes
-- the latter coordinate, and both BGG Euler sums recover the corresponding
-- finite character.
RextraO = realization(cartanDatum(cartanA 2),
    matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
DextraO = rootDatum RextraO;
lambdaClassO = kmWeight(DextraO,{1});
BclassO = bggResolution(DextraO,lambdaClassO);
EulerClassO = bggEulerCharacter(BclassO,Depth=>2);
assert(characterTerms EulerClassO ==
    characterTerms finiteCharacter(DextraO,lambdaClassO));

lambdaFullO = weightFromRealizationCoordinates(DextraO,{1,7});
BfullO = bggResolution(DextraO,lambdaFullO);
EulerFullO = bggEulerCharacter(BfullO,Depth=>2);
assert(characterTerms EulerFullO ==
    characterTerms finiteCharacter(DextraO,lambdaFullO));
assert(all(flatten apply(values(BfullO#"terms"),T -> keys T),M ->
    (realizationCoordinates(M#"highestWeight"))#1 == 7));

-- A skew extra realization direction is more adversarial than zero-padding:
-- subtracting alpha changes the second displayed coordinate even though the
-- coroot-annihilating component is Weyl fixed.
RskewO = realization(cartanDatum(cartanA 2),
    matrix(QQ,{{2,1}}),matrix(QQ,{{1,0}}));
DskewO = rootDatum RskewO;
lambdaSkewO = weightFromRealizationCoordinates(DskewO,{1,7});
EulerSkewO = bggEulerCharacter(
    bggResolution(DskewO,lambdaSkewO),Depth=>2);
finiteSkewO = finiteCharacter(DskewO,lambdaSkewO);
assert(characterTerms EulerSkewO == characterTerms finiteSkewO);
assert(set apply(keys characterTerms finiteSkewO,realizationCoordinates) ===
    set{{1,7},{-1,6}});

-- Nonsimply-laced and reducible systems test the Euler signs independently
-- of the simply-laced irreducible examples above, including cutoffs equal to
-- the initial depth of a nonidentity BGG summand.
verifyFiniteEulerO = (A,labels,H) -> (
    datum := rootDatum A;
    highest := kmWeight(datum,labels);
    euler := bggEulerCharacter(bggResolution(datum,highest),Depth=>H);
    complete := finiteCharacter(datum,highest);
    retained := new MutableHashTable;
    for term in pairs characterTerms complete do (
        beta := weightToRootCoordinates(highest-term#0);
        if all(beta,a -> instance(a,ZZ) and a >= 0) and sum beta <= H then
            retained#(term#0) = term#1;
    );
    assert(characterTerms euler == new Tally from pairs retained);
    true
);
assert verifyFiniteEulerO(cartanB 2,{2,1},3);
assert verifyFiniteEulerO(diagonalMatrix{2,2},{2,3},4);

///


-------------------------------------------------------------------------
-- Character Operations Tests
-------------------------------------------------------------------------

TEST ///

mustErrorCO = f -> (try (f(); false) else true)

D = rootDatum "A2";
V = finiteCharacter(D,{1,0});
zeroWt = kmWeight(D,{0,0});
assert(characterDimension symmetricPowerCharacter(V,0) == 1);
assert(characterDimension exteriorPowerCharacter(V,0) == 1);
assert(characterDimension symmetricPowerCharacter(V,2) == 6);
assert(characterDimension exteriorPowerCharacter(V,2) == 3);
assert(characterDimension exteriorPowerCharacter(V,3) == 1);
assert(characterDimension exteriorPowerCharacter(V,4) == 0);
assert(symmetricPowerDecomposition(V,2) == new Tally from {
    (kmWeight(D,{2,0}))=>1});
assert(exteriorPowerDecomposition(V,2) == new Tally from {
    (kmWeight(D,{0,1}))=>1});
assert(exteriorPowerDecomposition(V,3) == new Tally from {zeroWt=>1});

-- The second Adams identity is psi^2(V)=Sym^2(V)-Lambda^2(V).
assert(adamsOperation(2,V) ===
    symmetricPowerCharacter(V,2)-exteriorPowerCharacter(V,2));

-- Dimension specializations agree with ordinary binomial formulas across
-- several modules and powers.
for labels in {{1,0},{1,1},{2,0}} do (
    C := finiteCharacter(D,labels);
    n := characterDimension C;
    for k from 0 to 4 do (
        assert(characterDimension exteriorPowerCharacter(C,k) == binomial(n,k));
        assert(characterDimension symmetricPowerCharacter(C,k) == binomial(n+k-1,k));
    );
);

assert(frobeniusSchurIndicator(D,{1,0}) == 0);
D1 = rootDatum "A1";
assert(frobeniusSchurIndicator(D1,{1}) == -1);
assert(frobeniusSchurIndicator(D1,{2}) == 1);
assert(representationDynkinIndex(cartanA 2,{1}) == 1);
assert(representationDynkinIndex(cartanA 3,{1,0}) == 1);
assert(representationDynkinIndex(cartanB 3,{1,0,0}) == 2);
assert(representationDynkinIndex(cartanB 3,{0,0,1}) == 2);
assert(representationDynkinIndex(cartanC 3,{1,0,0}) == 1);
assert(representationDynkinIndex(cartanD 5,{0,0,0,1,0}) == 4);
assert(representationDynkinIndex(cartanE 6,{1,0,0,0,0,0}) == 6);
assert(representationDynkinIndex(cartanF4(),{0,0,0,1}) == 6);
assert(representationDynkinIndex(cartanG2(),{1,0}) == 2);
assert(mustErrorCO(() -> representationDynkinIndex(
    (cartanA 2) ++ (cartanA 2),{1,1})));

truncated = moduleCharacter(vermaModule(D,{1,0}),Depth=>1);
assert(mustErrorCO(() -> exteriorPowerCharacter(truncated,2)));

-- Lambda operations respect label classes versus complete weights in a
-- nonminimal finite realization.  A nonzero Weyl-fixed central character
-- prevents self-duality even when the semisimple Dynkin labels are self-dual.
RextraCO = realization(cartanDatum(cartanA 2),
    matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
DextraCO = rootDatum RextraCO;
CclassCO = finiteCharacter(DextraCO,{1});
assert(characterDimension symmetricPowerCharacter(CclassCO,2) == 3);

lambdaCentralCO = weightFromRealizationCoordinates(DextraCO,{1,3});
CcentralCO = finiteCharacter(DextraCO,lambdaCentralCO);
SymCentralCO = symmetricPowerCharacter(CcentralCO,2);
assert(all(characterSupport SymCentralCO,mu ->
    (realizationCoordinates mu)#1 == 6));
assert(frobeniusSchurIndicator(DextraCO,lambdaCentralCO) == 0);

lambdaNeutralCO = weightFromRealizationCoordinates(DextraCO,{1,0});
assert(frobeniusSchurIndicator(DextraCO,lambdaNeutralCO) == -1);

///


-------------------------------------------------------------------------
-- Infrastructure Tests
-------------------------------------------------------------------------

TEST ///

clearInfrastructureCaches();
assert(((infrastructureCacheInfo())#"affineStructure")#"cartanMatrices" == 0);
assert((infrastructureCacheInfo())#"datumMatrices" == 0);
assert((infrastructureCacheInfo())#"kazhdanLusztigData" == 0);
assert((infrastructureCacheInfo())#"crystalData" == 0);
assert((infrastructureCacheInfo())#"vermaPartitions" == 0);
assert(((infrastructureCacheInfo())#"weightNotation")#"latticeBases" == 0);
D = rootDatum "A2";
dummy = datumCartanMatrix D;
dummyAffine = affineTypeData cartanAffineA 2;
W = weylGroup D;
BCache = highestWeightCrystal(D,{1,0});
dummyCrystal = crystalCharacter BCache;
dummyVerma = moduleCharacter(vermaModule(D,{0,0}),Depth=>2);
dummyDisplay = toString dummyVerma;
assert(((infrastructureCacheInfo())#"weightNotation")#"latticeBases" > 0);
dummyKL = kazhdanLusztigPolynomial(
    identityWeylElement W,weylElement(W,longElement datumCartanMatrix D));
assert((infrastructureCacheInfo())#"datumMatrices" >= 1);
assert(((infrastructureCacheInfo())#"affineStructure")#"typeData" >= 1);
assert((infrastructureCacheInfo())#"kazhdanLusztigData" == 1);
assert((infrastructureCacheInfo())#"crystalData" >= 1);
assert((infrastructureCacheInfo())#"vermaPartitions" == 1);
clearInfrastructureCaches();
assert(((infrastructureCacheInfo())#"affineStructure")#"cartanMatrices" == 0);
assert((infrastructureCacheInfo())#"datumMatrices" == 0);
assert((infrastructureCacheInfo())#"kazhdanLusztigData" == 0);
assert((infrastructureCacheInfo())#"crystalData" == 0);
assert((infrastructureCacheInfo())#"vermaPartitions" == 0);
assert(((infrastructureCacheInfo())#"weightNotation")#"latticeBases" == 0);
assert(((infrastructureCacheInfo())#"weightNotation")#"nodeConventions" == 0);
assert(toString dummyVerma === dummyDisplay);

-- Public displays retain numerical coordinates rather than replacing
-- them by named basis elements.  This applies to nested module outputs too.
displayWeightsInfra = {kmRoot(D,{1,-2}),kmCoroot(D,{-1,2}),
    kmWeight(D,{2,1}),kmCoweight(D,{1,2})};
PDisplayInfra = standardParabolic(D,RemovedNodes=>{1});
displayObjectsInfra = join(displayWeightsInfra,{
    finiteCharacter(D,{1,0}),
    vermaModule(D,{1,0}),integrableHighestWeightModule(D,{1,0}),
    restrictWeight(PDisplayInfra,kmWeight(D,{1,0})),
    first keys decomposeLeviCharacter restrictCharacter(PDisplayInfra,
        finiteCharacter(D,{1,0})),
    borelWeilBott(D,{1,0}),bggResolution(D,{1,0}),
    BCache,first crystalVertices BCache});
for object in displayObjectsInfra do (
    assert(not match("(omega_|Lambda_|gamma_|eta_|zeta_|alpha_)",toString object));
    assert(not match("(omega_|Lambda_|gamma_|eta_|zeta_|alpha_)",toString net object));
);

-- Typed bridges reject unsupported nonfinite data through the same public
-- validation path as matrix input.  Finite bridge construction is exercised
-- in the package's existing optional-dependency tests.
assert(try (toRootSystem rootDatum(cartanAffineA 1); false) else true);
assert(try (toLieAlgebra cartanDatum(cartanAffineA 1); false) else true);

///


-- Test 0: basic Cartan matrix recognition
TEST ///
    assert(isCartanMatrix cartanA 4);
    assert(isCartanMatrix cartanB 3);
    assert(isCartanMatrix cartanC 3);
    assert(isCartanMatrix cartanD 4);
    assert(isCartanMatrix cartanE 6);
    assert(isCartanMatrix cartanE 7);
    assert(isCartanMatrix cartanE 8);
    assert(isCartanMatrix cartanF4());
    assert(isCartanMatrix cartanG2());
    assert(isCartanMatrix matrix{{2,-2},{-2,2}});       -- affine sl_2
    assert(isCartanMatrix matrix{{2,-3},{-3,2}});       -- rank-2 hyperbolic
    assert(not isCartanMatrix matrix{{2,-1},{-1,1}});   -- diagonal not 2
    assert(not isCartanMatrix matrix{{2,-1,0},{0,2,-1},{0,-1,2}}); -- zero pattern asymmetric
///

-- Test 0a: typed Dynkin diagrams retain labels and unambiguous positions.
TEST ///
    Apath := cartanA 4;
    Acycle := cartanAffineA 2;
    Abranch := cartanD 5;

    -- Matrix rendering is the historical position-based interface.
    assert(dynkinDiagram Apath === "1 --- 2 --- 3");
    assert(dynkinDiagram Acycle ===
        "1 --- 3 --- 2 --- 1  (cycle)");
    assert(dynkinDiagram Abranch ===
        "1 --- 2 --- 3 --- 5" | newline |
        "            |" | newline |
        "            4");
    rawDiagramData := dynkinDiagramData Apath;
    assert(rawDiagramData#"nodes" === {1,2,3});
    assert(rawDiagramData#"edges" ===
        {{1,2,-1,-1},{2,3,-1,-1}});
    standardDatum := cartanDatum Apath;
    assert(dynkinDiagram standardDatum === dynkinDiagram Apath);
    assert((dynkinDiagramData standardDatum)#"nodeFormat" ===
        "Positions");

    namedDatum := cartanDatum(
        Acycle,NodeLabels=>{"affine","left","right"});
    namedRealization := standardRealization namedDatum;
    namedRootDatum := rootDatum namedRealization;
    namedRendering :=
        "1[affine] --- 3[right] --- 2[left] --- 1[affine]  (cycle)";
    assert(dynkinDiagram namedDatum === namedRendering);
    assert(dynkinDiagram namedRealization === namedRendering);
    assert(dynkinDiagram namedRootDatum === namedRendering);

    namedData := dynkinDiagramData namedRootDatum;
    assert(namedData#"nodeFormat" === "Records");
    assert((namedData#"nodes"#0)#"position" == 1);
    assert((namedData#"nodes"#0)#"label" === "affine");
    assert(namedData#"labeledEdges" === {
        {"affine","left",-1,-1},
        {"affine","right",-1,-1},
        {"left","right",-1,-1}});
    assert(namedData#"positions" === {1,2,3});
    assert(namedData#"positionEdges" === {
        {1,2,-1,-1},{1,3,-1,-1},{2,3,-1,-1}});

    -- A custom integer label in the positional range must not silently
    -- masquerade as a position.  The typed default therefore uses records.
    integerDatum := cartanDatum(Apath,NodeLabels=>{2,1,3});
    assert(dynkinDiagram integerDatum ===
        "1[2] --- 2[1] --- 3[3]");
    integerData := dynkinDiagramData integerDatum;
    assert(integerData#"nodeFormat" === "Records");
    assert((integerData#"nodes"#0)#"position" == 1);
    assert((integerData#"nodes"#0)#"label" == 2);
    assert((integerData#"nodes"#1)#"position" == 2);
    assert((integerData#"nodes"#1)#"label" == 1);
    assert(dynkinDiagram(integerDatum,NodeFormat=>"Positions") ===
        "1 --- 2 --- 3");
    assert(dynkinDiagram(integerDatum,NodeFormat=>"Labels") ===
        "2 --- 1 --- 3");
    assert((dynkinDiagramData(
        integerDatum,NodeFormat=>"Positions"))#"nodes" === {1,2,3});
    assert((dynkinDiagramData(
        integerDatum,NodeFormat=>"Labels"))#"edges" ===
        {{2,1,-1,-1},{1,3,-1,-1}});

    mixedTypeLabels := cartanDatum(
        cartanA 3,NodeLabels=>{"1",1});
    assert(dynkinDiagram mixedTypeLabels ===
        "1[\"1\"] --- 2[1]");

    -- Width-aware branch alignment uses the displayed label widths.
    branchDatum := cartanDatum(Abranch,NodeLabels=>{
        "outerLeft","next","branch","side","outerRight"});
    assert(dynkinDiagram branchDatum ===
        "1[outerLeft] --- 2[next] --- 3[branch] --- 5[outerRight]" |
        newline | concatenate toList(33:" ") | "|" | newline |
        concatenate toList(30:" ") | "4[side]");

    assert(try (
        dynkinDiagram(Apath,NodeFormat=>"Labels");
        false
    ) else true);
    assert(try (
        dynkinDiagramData(Apath,NodeFormat=>"Records");
        false
    ) else true);

    emptyDatum := cartanDatum matrix mutableMatrix(ZZ,0,0);
    assert(dynkinDiagram emptyDatum === "");
    assert((dynkinDiagramData emptyDatum)#"nodes" === {});
    assert((dynkinDiagramData(rootDatum emptyDatum))#"edges" === {});
///

-- Test 1: cartanType classifies finite/affine/indefinite
TEST ///
    assert(cartanType cartanA 4 === "finite");
    assert(cartanType cartanE 8 === "finite");
    assert(cartanType cartanF4() === "finite");
    assert(cartanType cartanG2() === "finite");
    assert(cartanType matrix{{2,-2},{-2,2}} === "affine");
    assert(cartanType matrix{{2,-3},{-3,2}} === "indefinite");
///

-- Test 2: symmetrizers
-- symmetrizer returns the diagonal matrix D with D*A symmetric
TEST ///
    diagOf := A -> for i from 0 to numrows A - 1 list (symmetrizer A)_(i,i);
    -- D*A symmetric is the defining property
    check := A -> (
        D := symmetrizer A;
        assert(D*A == transpose(D*A));
    );
    -- Simply laced: all diagonal entries equal
    for A in {cartanA 5, cartanD 4, cartanE 6, cartanE 7, cartanE 8} do (
        check A;
        d := diagOf A;
        assert(all(d, x -> x == first d));
    );
    -- Non-simply-laced: exact values (the package normalizes long root to 1)
    check cartanB 3;  assert(diagOf cartanB 3 == {1, 1, 1/2});
    check cartanC 3;  assert(diagOf cartanC 3 == {1, 1, 2});
    check cartanG2(); assert(diagOf cartanG2() == {1, 3});
    check cartanF4(); assert(diagOf cartanF4() == {1, 1, 1/2, 1/2});
///

-- Test 3: Langlands duality for non-simply-laced types
TEST ///
    assert(dualCartanMatrix cartanB 3 == cartanC 3);
    assert(dualCartanMatrix cartanC 3 == cartanB 3);
    assert(dualCartanMatrix cartanA 4 == cartanA 4);   -- self-dual
    assert(dualCartanMatrix cartanD 4 == cartanD 4);
    assert(dualCartanMatrix cartanF4() == transpose cartanF4());
    assert(dualCartanMatrix cartanG2() == transpose cartanG2());
///

-- Test 4: root counts for classical and small exceptional types
TEST ///
    -- |Phi^+(A_n)| = n(n+1)/2;  note cartanA k is A_{k-1}
    assert(numrows cartanPositiveRoots cartanA 3 === 3);   -- A_2
    assert(numrows cartanPositiveRoots cartanA 4 === 6);   -- A_3
    -- |Phi^+(B_n)| = |Phi^+(C_n)| = n^2
    assert(numrows cartanPositiveRoots cartanB 3 === 9);
    assert(numrows cartanPositiveRoots cartanC 3 === 9);
    -- |Phi^+(D_n)| = n(n-1)
    assert(numrows cartanPositiveRoots cartanD 4 === 12);
    -- Exceptional
    assert(numrows cartanPositiveRoots cartanG2() === 6);
    assert(numrows cartanPositiveRoots cartanF4() === 24);
///

-- Test 5: allRoots/negativeRoots cardinalities
-- allRoots / negativeRoots return matrices whose rows are the roots.
TEST ///
    assert(numrows allRoots cartanA 4 === 12);        -- A_3: 2*6 = 12
    assert(numrows negativeRoots cartanA 4 === 6);
    assert(numrows allRoots cartanG2() === 12);
    assert(numrows allRoots cartanD 4 === 24);
///

-- Test 6: Coxeter and dual-Coxeter numbers (small types)
TEST ///
    assert(coxeterNumber cartanA 4 === 4);       -- A_3 = sl_4
    assert(coxeterNumber cartanB 3 === 6);       -- 2n
    assert(coxeterNumber cartanC 3 === 6);
    assert(coxeterNumber cartanD 4 === 6);       -- 2n-2
    assert(coxeterNumber cartanG2() === 6);
    assert(coxeterNumber cartanF4() === 12);
    -- Dual Coxeter (non-simply-laced differs)
    assert(dualCoxeterNumber cartanA 4 === 4);
    assert(dualCoxeterNumber cartanB 3 === 5);   -- 2n-1
    assert(dualCoxeterNumber cartanC 3 === 4);   -- n+1
    assert(dualCoxeterNumber cartanG2() === 4);
    assert(dualCoxeterNumber cartanF4() === 9);
///

-- Test 7: Weyl dimensions of classical/exceptional standard representations
-- (small/medium types; the big exceptionals E_7 and E_8 would take several
-- seconds each and are deliberately omitted from the test suite.)
TEST ///
    -- sl_n = A_{n-1} = cartanA n
    assert(weylDimension(cartanA 4, {1,0,0}) == 4);       -- std of sl_4
    assert(weylDimension(cartanA 4, {1,0,1}) == 15);      -- adjoint sl_4
    assert(weylDimension(cartanA 5, {1,0,0,0}) == 5);     -- std of sl_5
    -- so_{2n+1} vector and spin
    assert(weylDimension(cartanB 3, {1,0,0}) == 7);       -- vector so_7
    assert(weylDimension(cartanB 3, {0,0,1}) == 8);       -- spin so_7
    assert(weylDimension(cartanB 3, {0,1,0}) == 21);      -- adjoint so_7
    -- sp_{2n}
    assert(weylDimension(cartanC 3, {1,0,0}) == 6);       -- standard sp_6
    assert(weylDimension(cartanC 3, {2,0,0}) == 21);      -- adjoint sp_6
    -- so_{2n}: vector + half-spins + adjoint
    assert(weylDimension(cartanD 4, {1,0,0,0}) == 8);     -- vector
    assert(weylDimension(cartanD 4, {0,0,1,0}) == 8);     -- half-spin
    assert(weylDimension(cartanD 4, {0,1,0,0}) == 28);    -- adjoint
    -- G_2
    assert(weylDimension(cartanG2(), {1,0}) == 7);        -- short rep
    assert(weylDimension(cartanG2(), {0,1}) == 14);       -- adjoint
    -- F_4
    assert(weylDimension(cartanF4(), {0,0,0,1}) == 26);
    assert(weylDimension(cartanF4(), {1,0,0,0}) == 52);   -- adjoint
///

-- Test 8: Weyl-group orders match classical formulas (small types only)
TEST ///
    -- |W(A_{n-1})| = n!;  cartanA k = A_{k-1}
    assert(weylGroupOrder cartanA 3 === 6);      -- A_2: 3!
    assert(weylGroupOrder cartanA 4 === 24);     -- A_3: 4!
    assert(weylGroupOrder cartanB 3 === 48);     -- 2^3 * 3!
    assert(weylGroupOrder cartanC 3 === 48);
    assert(weylGroupOrder cartanD 4 === 192);    -- 2^{4-1} * 4!
    assert(weylGroupOrder cartanG2() === 12);
///

-- Test 9: Highest root in simple-root basis (the "marks")
TEST ///
    assert(highestRoot cartanA 3 === {1,1});                -- A_2
    assert(highestRoot cartanA 4 === {1,1,1});              -- A_3
    assert(highestRoot cartanB 3 === {1,2,2});
    assert(highestRoot cartanC 3 === {2,2,1});
    assert(highestRoot cartanD 4 === {1,2,1,1});
    assert(highestRoot cartanG2() === {3,2});
    assert(highestRoot cartanF4() === {2,3,4,2});
    -- Height of highest root equals Coxeter number minus 1 (small types)
    for A in {cartanA 4, cartanB 3, cartanD 4, cartanG2(), cartanF4()} do
        assert(rootHeight highestRoot A === coxeterNumber A - 1);
///

-- Test 10: Dynkin labels of the highest root describe the adjoint rep
TEST ///
    -- dim adjoint of sl_n = n^2 - 1; cartanA k = A_{k-1} = sl_k
    cases := {
        {cartanA 4, 15}, {cartanB 3, 21}, {cartanC 3, 21},
        {cartanD 4, 28}, {cartanG2(), 14}};
    for p in cases do (
        A := p#0; expected := p#1;
        theta := highestRoot A;
        dynk  := toFundamental(A, theta);
        dynkInt := apply(dynk, c -> lift(c, ZZ));
        assert(weylDimension(A, dynkInt) == expected);
    );
///

-- Test 11: Fundamental-weight / simple-root round-trip
TEST ///
    for A in {cartanA 4, cartanB 3, cartanC 3, cartanD 4, cartanG2()} do (
        r := numrows A;
        W := fundamentalWeights A;
        assert((A ** QQ) * transpose W == id_(QQ^r));
        rho := toList(r:1);
        assert(apply(toFundamental(A, toSimple(A, rho)), c -> lift(c,ZZ)) === rho);
    );
///

-- Test 12: Weyl vector has all-1 Dynkin labels
TEST ///
    for A in {cartanA 4, cartanB 3, cartanC 3, cartanD 4, cartanG2()} do (
        r := numrows A;
        assert(apply(toFundamental(A, weylVector A), c -> lift(c,ZZ))
               === toList(r:1));
    );
///

-- Test 13: Dominance test behaves as expected
TEST ///
    -- cartanA 5 has rank 4 (it's A_4); Dynkin labels must have length 4
    assert(isDominant(cartanA 5, {1,0,1,0}));
    assert(isDominant(cartanE 6, {0,0,0,0,0,0}));
    assert(not isDominant(cartanA 5, {1,-1,2,0}));
    assert(not isDominant(cartanD 4, {0,0,-1,0}));
///

-- Test 14: Long vs short root norms in non-simply-laced types
TEST ///
    -- B_3: package normalizes so short root has norm^2 = 1, long has norm^2 = 2
    assert(rootNormSquared(cartanB 3, {1,0,0}) == 2);  -- long
    assert(rootNormSquared(cartanB 3, {0,0,1}) == 1);  -- short
    -- C_3: package normalizes so short has norm^2 = 2, long has norm^2 = 4
    assert(rootNormSquared(cartanC 3, {0,0,1}) == 4);  -- long
    assert(rootNormSquared(cartanC 3, {1,0,0}) == 2);  -- short
    -- G_2: long:short ratio = 3:1 (regardless of normalization)
    assert(rootNormSquared(cartanG2(), {0,1}) == 3 * rootNormSquared(cartanG2(), {1,0}));
///

-- Test 15: Tensor-decomposition sanity - dimensions multiply
TEST ///
    -- sl_3 (= A_2 = cartanA 3) standard (tensor) dual = trivial + adjoint
    T := tensorDecomposition(cartanA 3, {1,0}, {0,1});
    assert(sum apply(keys T, k -> (
        v := (toList k)#0;           -- first component of Irrep
        T#k * weylDimension(cartanA 3, v)
    )) == 9);                      -- 3 * 3
    -- adjoint x adjoint of sl_3: 8 * 8 = 64
    T2 := tensorDecomposition(cartanA 3, {1,1}, {1,1});
    assert(sum apply(keys T2, k -> (
        v := (toList k)#0;
        T2#k * weylDimension(cartanA 3, v)
    )) == 64);
    -- G_2 adjoint x adjoint = 14 * 14 = 196
    T3 := tensorDecomposition(cartanG2(), {0,1}, {0,1});
    assert(sum apply(keys T3, k -> (
        v := (toList k)#0;
        T3#k * weylDimension(cartanG2(), v)
    )) == 196);
///

-- Test 16: leviHighestWeights depth matches highest-root marks
-- (fast, closed-form checks rather than iterating through every graded piece)
TEST ///
    -- For cominuscule parabolic P_i (coefficient of a_i in theta = 1),
    -- g_1 is nontrivial but g_2 must be empty.
    --   A_4 / P_2 = Gr(2,5): coefficient of a_2 in theta = 1
    assert(#leviHighestWeights(cartanA 5, 1, {2}) > 0);
    assert(#leviHighestWeights(cartanA 5, 2, {2}) === 0);
    -- G_2 / P_1: coefficient of a_1 = 3, so g_1, g_2, g_3 nontrivial, g_4 empty
    --   (short test: just check depth 3 and 4)
    assert(#leviHighestWeights(cartanG2(), 3, {1}) > 0);
    assert(#leviHighestWeights(cartanG2(), 4, {1}) === 0);
///

-- Test 17: the multiplicity-counted number of Kostant summands is |W/W_L|
-- (on a small test case to keep run time <1s)
TEST ///
    -- A_2 / P_1 gives P^2:  |W/W_L| = 6/2 = 3 summands for trivial rep
    H := kostantLieAlgCohomology(cartanA 3, {1}, {0,0});
    total := sum apply(keys H, k -> sum values H#k);
    assert(total === 3);
    -- A_3 / P_1 gives P^3:  |W/W_L| = 24/6 = 4 summands
    H2 := kostantLieAlgCohomology(cartanA 4, {1}, {0,0,0});
    total2 := sum apply(keys H2, k -> sum values H2#k);
    assert(total2 === 4);
///

-- Test 18: Longest element has maximal length = number of positive roots
TEST ///
    for A in {cartanA 3, cartanB 3, cartanG2()} do (
        w0 := longElement A;
        assert(weylLength(A, w0) === numrows cartanPositiveRoots A);
    );
///

-- Test 19: Reduced-word detection and braid relations
TEST ///
    -- cartanA 4 = A_3, three simple reflections s_1, s_2, s_3
    A := cartanA 4;
    -- Unreduced word collapses to shorter one
    assert(weylLength(A, {1,1,2}) < 3);     -- s_1 s_1 = e
    -- Braid: s_1 s_2 s_1 = s_2 s_1 s_2
    assert(weylElementMatrix(A, {1,2,1}) == weylElementMatrix(A, {2,1,2}));
    -- Commutation: s_1 s_3 = s_3 s_1 (non-adjacent nodes)
    assert(weylElementMatrix(A, {1,3}) == weylElementMatrix(A, {3,1}));
    -- reducedWord returns a reduced expression for the same element;
    -- an already-reduced word keeps its length, a non-reduced one shrinks.
    assert(#reducedWord(A, {1,2,3}) === 3);
    assert(#reducedWord(A, {1,1,2}) < 3);
///

-- Test 20: Weyl orbit sizes
TEST ///
    -- Orbit of the highest root under W(A_2) has 6 elements
    -- (corresponding to 6 roots of sl_3); A_2 = cartanA 3
    assert(# weightOrbit(cartanA 3, {1,1}) === 6);
    -- Orbit of fundamental weight omega_1 of A_3 has 4 elements; A_3 = cartanA 4
    assert(# weightOrbit(cartanA 4, {1,0,0}) === 4);
///

-- Test 21: Partition <-> Dynkin round-trip in type A
TEST ///
    lam := {3,2,1,0};   -- partition of length 4 for A_3
    A := cartanA 4;
    m := partitionToFundamental(A, lam);
    assert(m === {1,1,1});
    assert(fundamentalToPartition(A, m) === lam);
    m2 := {2,0,1};
    assert(partitionToFundamental(A, fundamentalToPartition(A, m2)) === m2);
///

-- Test 21b: Partition <-> Dynkin round-trip in type B (so_{2n+1}).
-- B_n convention: m_i = lam_i - lam_{i+1} for i<n and m_n = 2 lam_n, so the
-- spin weights have half-integer lambda.
TEST ///
    A := cartanB 3;
    -- Vector representation of so_7: lambda = (1,0,0), Dynkin (1,0,0)
    assert(partitionToFundamental(A, {1,0,0}) === {1,0,0});
    assert(fundamentalToPartition(A, {1,0,0}) === {1,0,0});
    -- Adjoint of so_7: lambda = (1,1,0), Dynkin (0,1,0)
    assert(partitionToFundamental(A, {1,1,0}) === {0,1,0});
    assert(fundamentalToPartition(A, {0,1,0}) === {1,1,0});
    -- Spin of so_7: Dynkin (0,0,1), lambda = (1/2,1/2,1/2)
    assert(fundamentalToPartition(A, {0,0,1}) === {1/2, 1/2, 1/2});
    assert(partitionToFundamental(A, {1/2, 1/2, 1/2}) === {0,0,1});
    -- Round trip via the dispatcher on a generic partition
    lam := {3,2,1};
    m := partitionToFundamental(A, lam);
    assert(m === {1,1,2});
    assert(fundamentalToPartition(A, m) === lam);
    -- Dimension sanity: weylDimension agrees with classical dim formulas.
    --   dim of so_7 vector = 7, adjoint = 21, spin = 8.
    assert(weylDimension(A, partitionToFundamental(A, {1,0,0})) == 7);
    assert(weylDimension(A, partitionToFundamental(A, {1,1,0})) == 21);
    assert(weylDimension(A, {0,0,1}) == 8);
///

-- Test 21c: Partition <-> Dynkin round-trip in type C (sp_{2n}).
-- C_n convention: m_i = lam_i - lam_{i+1} for i<n and m_n = lam_n; always
-- integer (no spin).
TEST ///
    A := cartanC 3;
    -- Vector of sp_6: lambda = (1,0,0), Dynkin (1,0,0)
    assert(partitionToFundamental(A, {1,0,0}) === {1,0,0});
    assert(fundamentalToPartition(A, {1,0,0}) === {1,0,0});
    -- Adjoint of sp_6: lambda = (2,0,0), Dynkin (2,0,0)
    assert(partitionToFundamental(A, {2,0,0}) === {2,0,0});
    assert(fundamentalToPartition(A, {2,0,0}) === {2,0,0});
    -- Wedge^2 sp_6 (irred piece): Dynkin (0,1,0), lambda = (1,1,0)
    assert(partitionToFundamental(A, {1,1,0}) === {0,1,0});
    assert(fundamentalToPartition(A, {0,1,0}) === {1,1,0});
    -- Round trip through dispatcher on a generic partition
    lam := {3,2,1};
    m := partitionToFundamental(A, lam);
    assert(m === {1,1,1});
    assert(fundamentalToPartition(A, m) === lam);
    -- Dimension sanity
    assert(weylDimension(A, partitionToFundamental(A, {1,0,0})) == 6);
    assert(weylDimension(A, partitionToFundamental(A, {2,0,0})) == 21);
///

-- Test 21d: Partition <-> Dynkin round-trip in type D (so_{2n}).
-- D_n convention: m_i = lam_i - lam_{i+1} (i<=n-2),
-- m_{n-1} = lam_{n-1}-lam_n, m_n = lam_{n-1}+lam_n;
-- the two half-spin weights have half-integer lambda and lam_n can be negative.
TEST ///
    A := cartanD 4;
    -- Vector of so_8: lambda = (1,0,0,0), Dynkin (1,0,0,0)
    assert(partitionToFundamental(A, {1,0,0,0}) === {1,0,0,0});
    assert(fundamentalToPartition(A, {1,0,0,0}) === {1,0,0,0});
    -- Two half-spin reps of so_8: Dynkin (0,0,0,1) and (0,0,1,0).
    -- In the lambda-convention these are (1/2,1/2,1/2,1/2) and (1/2,1/2,1/2,-1/2).
    assert(fundamentalToPartition(A, {0,0,0,1}) === {1/2, 1/2, 1/2, 1/2});
    assert(fundamentalToPartition(A, {0,0,1,0}) === {1/2, 1/2, 1/2, -1/2});
    assert(partitionToFundamental(A, {1/2, 1/2, 1/2, 1/2}) === {0,0,0,1});
    assert(partitionToFundamental(A, {1/2, 1/2, 1/2, -1/2}) === {0,0,1,0});
    -- Adjoint of so_8: lambda = (1,1,0,0), Dynkin (0,1,0,0)
    assert(partitionToFundamental(A, {1,1,0,0}) === {0,1,0,0});
    assert(fundamentalToPartition(A, {0,1,0,0}) === {1,1,0,0});
    -- Dispatcher round trip
    lam := {2,1,0,0};
    m := partitionToFundamental(A, lam);
    assert(fundamentalToPartition(A, m) === lam);
    -- Round trip preserving negative last entry
    lam2 := {1/2, 1/2, 1/2, -1/2};
    m2 := partitionToFundamental(A, lam2);
    assert(fundamentalToPartition(A, m2) === lam2);
    -- Dimension sanity: all three 8-dim reps of so_8 (triality).
    assert(weylDimension(A, {1,0,0,0}) == 8);
    assert(weylDimension(A, {0,0,0,1}) == 8);
    assert(weylDimension(A, {0,0,1,0}) == 8);
    assert(weylDimension(A, {0,1,0,0}) == 28);  -- adjoint
///

-- Test 21e: tensorDecomposition and leviHighestWeights under
-- WeightFormat => "Partition" for classical types B, C, D.
TEST ///
    -- Keys of a tensorDecomposition Tally are Irrep objects; the
    -- underlying partition is the first component of (toList k).
    partitionSummands := T -> set apply(keys T, k -> (toList k)#0);
    -- sp_6 vector x vector = Sym^2 + wedge^2 + trivial
    --   Sym^2: lam=(2,0,0); wedge^2: lam=(1,1,0); trivial: lam=(0,0,0).
    decC := tensorDecomposition(cartanC 3, {1,0,0}, {1,0,0},
        WeightFormat => "Partition");
    assert(partitionSummands decC === set{{2,0,0}, {1,1,0}, {0,0,0}});
    -- so_7 vector x vector = Sym^2 traceless + adjoint + trivial
    --   Sym^2 traceless: lam=(2,0,0); adjoint wedge^2: lam=(1,1,0); trivial.
    decB := tensorDecomposition(cartanB 3, {1,0,0}, {1,0,0},
        WeightFormat => "Partition");
    assert(partitionSummands decB === set{{2,0,0}, {1,1,0}, {0,0,0}});
    -- so_8: V(omega_3) tensor V(omega_4) = V(omega_1) + V(omega_3 + omega_4),
    -- dimensions 8 + 56 = 64; partitions (1,0,0,0) and (1,1,1,0) in D_4.
    -- In partition (lambda) coordinates, omega_4 = (1/2,1/2,1/2,1/2) and
    -- omega_3 = (1/2,1/2,1/2,-1/2).
    decD := tensorDecomposition(cartanD 4, {1/2,1/2,1/2,1/2}, {1/2,1/2,1/2,-1/2},
        WeightFormat => "Partition");
    assert(partitionSummands decD === set{{1,0,0,0}, {1,1,1,0}});
    -- Partition-form LeviHighestWeights on so_8 with parabolic {4}
    -- (isotropic Grassmannian).  Exercises the D-type dispatcher.
    T := leviHighestWeights(cartanD 4, 1, {4},
        WeightFormat => "Partition");
    assert(#T >= 1);
///

-- Test 21f: WeightFormat=>"Partition" is a symmetric input/output convention.
-- For every function accepting Dynkin weights, the Partition-input path must
-- agree (after round-tripping via fundamentalToPartition) with the Dynkin-
-- input path.  Covers weylDimension, weightsOfRep, dominantWeightsInRep,
-- weightMultiplicity, weightOrbit, tensorDecomposition and
-- kostantLieAlgCohomology on representative classical examples with both
-- integer and half-integer partition inputs.
TEST ///
    sameTallyExact := (T,U) ->
        (#keys T == #keys U and all(keys T,k -> U#?k and T#k == U#k));
    dynkinTallyToPartitions := (AL,T) -> (
        C := cartanData AL;
        comps := apply(C#"components",component ->
            apply(component#"nodes",i -> i-1));
        new Tally from for V in keys T list (
            pieces := toList V;
            converted := for c from 0 to #comps-1 list
                fundamentalToPartition(
                    submatrix(AL,comps#c,comps#c),pieces#c);
            (new Irrep from converted) => T#V
        )
    );

    -- ---- weylDimension ----
    checkDimPar := (A, p) -> (
        d := partitionToFundamental(A, p);
        assert(weylDimension(A, p, WeightFormat => "Partition")
               == weylDimension(A, d));
    );
    checkDimPar(cartanA 5, {3,1,0,0,0});   -- sl_5 mixed symmetry
    checkDimPar(cartanB 3, {1/2,1/2,1/2}); -- so_7 spin (8)
    checkDimPar(cartanB 3, {1,1,0});       -- so_7 adjoint (21)
    checkDimPar(cartanC 3, {2,1,0});       -- sp_6 mixed symmetry
    checkDimPar(cartanD 4, {1,1,0,0});     -- so_8 adjoint (28)
    checkDimPar(cartanD 4, {1/2,1/2,1/2, 1/2});  -- so_8 half-spin+ (8)
    checkDimPar(cartanD 4, {1/2,1/2,1/2,-1/2});  -- so_8 half-spin- (8)

    -- ---- weightsOfRep, dominantWeightsInRep, weightOrbit ----
    checkSetPar := (f, A, dLam) -> (
        pLam := fundamentalToPartition(A, dLam);
        wD := f(A, dLam);
        wP := f(A, pLam, WeightFormat => "Partition");
        assert(set wP === set apply(wD, w -> fundamentalToPartition(A, w)));
    );
    -- cartanA n = A_{n-1} has n-1 Dynkin labels.
    checkSetPar(weightsOfRep,         cartanA 4, {1,0,0});         -- sl_4 std
    checkSetPar(weightsOfRep,         cartanB 3, {0,0,1});         -- so_7 spin
    checkSetPar(weightsOfRep,         cartanC 3, {1,0,0});         -- sp_6 vec
    checkSetPar(dominantWeightsInRep, cartanA 5, {1,0,0,1});       -- sl_5
    checkSetPar(dominantWeightsInRep, cartanB 3, {0,0,1});
    checkSetPar(dominantWeightsInRep, cartanD 4, {0,1,0,0});
    checkSetPar(weightOrbit,          cartanA 5, {1,0,0,1});
    checkSetPar(weightOrbit,          cartanB 3, {1,0,0});
    checkSetPar(weightOrbit,          cartanD 4, {0,0,0,1});

    -- ---- weightMultiplicity ----
    checkMultPar := (A, dLam, dMu) -> (
        pLam := fundamentalToPartition(A, dLam);
        pMu  := fundamentalToPartition(A, dMu);
        assert(weightMultiplicity(A, pLam, pMu, WeightFormat => "Partition")
               == weightMultiplicity(A, dLam, dMu));
    );
    checkMultPar(cartanA 5, {1,0,0,1}, {0,0,0,0});    -- sl_5 adj at 0
    checkMultPar(cartanB 3, {0,0,1},   {0,0,-1});     -- so_7 spin at opposite
    checkMultPar(cartanD 4, {0,1,0,0}, {0,0,0,0});    -- so_8 adj at 0

    -- ---- tensorDecomposition: input + output both converted ----
    checkTensorPar := (A, pLam, pMu) -> (
        dLam := partitionToFundamental(A, pLam);
        dMu  := partitionToFundamental(A, pMu);
        dD := tensorDecomposition(A, dLam, dMu);
        dP := tensorDecomposition(A, pLam, pMu, WeightFormat => "Partition");
        assert(sameTallyExact(dP,dynkinTallyToPartitions(A,dD)));
    );
    checkTensorPar(cartanA 3, {1,0,0},       {1,1,0});
    checkTensorPar(cartanA 3, {2,1,0},       {2,1,0});
    checkTensorPar(cartanB 3, {1,0,0},       {1/2,1/2,1/2});
    checkTensorPar(cartanB 3, {1/2,1/2,1/2}, {1/2,1/2,1/2});
    checkTensorPar(cartanC 3, {1,0,0},       {1,0,0});
    checkTensorPar(cartanD 4, {1/2,1/2,1/2,1/2}, {1/2,1/2,1/2,-1/2});
    checkTensorPar(cartanD 4, {1/2,1/2,1/2,1/2}, {1/2,1/2,1/2, 1/2});

    -- ---- kostantLieAlgCohomology: lambda input converted ----
    checkKostPar := (A, parab, dLam) -> (
        pLam := fundamentalToPartition(A, dLam);
        hD := kostantLieAlgCohomology(A, parab, dLam);
        hP := kostantLieAlgCohomology(A, parab, pLam, WeightFormat => "Partition");
        assert(sort keys hD === sort keys hP);
        L := leviSubalgebra(A,parab);
        AL := L#"cartanMatrix";
        for d in keys hD do assert(sameTallyExact(
            dynkinTallyToPartitions(AL,hD#d),hP#d));
    );
    -- cartanA n = A_{n-1} has n-1 Dynkin labels.
    checkKostPar(cartanA 5, {1}, {0,0,0,0});
    checkKostPar(cartanA 5, {1}, {1,0,0,0});
    checkKostPar(cartanD 4, {4}, {0,1,0,0});

    -- ---- Multi-node parabolic: leviHighestWeights and
    -- kostantLieAlgCohomology in both Dynkin and Partition forms ----
    -- Fl(1,3,5) in SL_5 (= cartanA 5): remove nodes {1,3}.
    -- Depth = c_1(theta) + c_3(theta) = 1 + 1 = 2.  Check piece counts,
    -- and verify Partition-output is just the per-component conversion
    -- of the Dynkin-output.
    for d in {1, 2} do (
        TD := leviHighestWeights(cartanA 5, d, {1, 3});
        TP := leviHighestWeights(cartanA 5, d, {1, 3},
            WeightFormat => "Partition");
        LA := leviSubalgebra(cartanA 5,{1,3});
        assert(sameTallyExact(
            dynkinTallyToPartitions(LA#"cartanMatrix",TD),TP));
    );
    assert(#leviHighestWeights(cartanA 5, 3, {1,3}) === 0);
    -- D_6 / P_{2,6}: multi-node parabolic with one cominuscule node.
    -- Depth = c_2(theta) + c_6(theta) = 2 + 1 = 3.
    for d in {1, 2, 3} do (
        TD := leviHighestWeights(cartanD 6, d, {2, 6});
        TP := leviHighestWeights(cartanD 6, d, {2, 6},
            WeightFormat => "Partition");
        LD := leviSubalgebra(cartanD 6,{2,6});
        assert(sameTallyExact(
            dynkinTallyToPartitions(LD#"cartanMatrix",TD),TP));
    );
    assert(#leviHighestWeights(cartanD 6, 4, {2,6}) === 0);
    -- Multi-node kostantLieAlgCohomology parity.
    checkKostPar(cartanA 5, {1, 3}, {0,0,0,0});
    checkKostPar(cartanA 5, {1, 3}, {1,0,0,1});
    checkKostPar(cartanD 5, {1, 5}, {0,0,0,0,0});
///

-- Test 22: an independent hook-length dimension formula in type A
TEST ///
    -- The hook-length product supplies an independent dimension oracle.
    -- Keep it local to the tests, rather than a separate package command.
    hookLengthDimension := (n,lambda) -> product flatten apply(#lambda,i ->
        apply(lambda#i,j -> (n+j-i)/(lambda#i-j
            + #select(lambda,p -> p > j)-i-1)));
    for lam in {{2,1,0}, {3,2,1}, {4,2,0}, {1,1,1}} do (
        n := #lam;
        m := partitionToFundamental(cartanA n, lam);
        assert(hookLengthDimension(n, lam) == weylDimension(cartanA n, m));
    );
///

-- Test 23: Affine snapshot structure is populated with expected keys
-- (use small snapshot height to keep the test <1s)
TEST ///
    -- Affine sl_2: rank 2, snapshot to height 2
    A := matrix{{2,-2},{-2,2}};
    snap := affineRootSnapshot(A, 2);
    assert(snap#?"real" and snap#?"imag" and snap#?"mult" and snap#?"delta");
    -- Multiplicity of the simple roots is always 1
    assert((snap#"mult")#{1,0} === 1);
    assert((snap#"mult")#{0,1} === 1);
///

-- Test 24: corrected Peterson recurrence and BCL graded ascent agree.
-- This reaches the first singular-denominator multiple in rank 2 and checks
-- values independently known from the multigraded free-Lie/Witt formula.
TEST ///
    A := matrix{{2,-3},{-3,2}};
    bcl := indefiniteRootSnapshot(A, 20, Algorithm => "BCL");
    pet := indefiniteRootSnapshot(A, 20, Algorithm => "Peterson");
    petMults := pet#"mult";
    assert(#keys(bcl#"mult") == #keys petMults
        and all(keys(bcl#"mult"), beta -> petMults#?beta
            and petMults#beta == (bcl#"mult")#beta));
    mults := bcl#"mult";
    assert(numrows(bcl#"real") === 6);
    assert(numrows(bcl#"imag") === 94);
    assert(#keys mults === 100);
    assert(mults#{5,12} === 4);
    assert(mults#{7,13} === 162);
    assert(mults#{8,12} === 758);
    assert(mults#{10,10} === 2278);
    assert(not mults#?{12,4});
    assert(all(values mults, m -> instance(m, ZZ) and m > 0));
///

-- Test 24a: nonsymmetric symmetrizer weights and a low-height rank-3
-- regression where the former zero-denominator branch created false roots.
TEST ///
    A := cartanRank2(2,3);
    bcl := indefiniteRootSnapshot(A, 12, Algorithm => "BCL");
    pet := indefiniteRootSnapshot(A, 12, Algorithm => "Peterson");
    petMults := pet#"mult";
    assert(#keys(bcl#"mult") == #keys petMults
        and all(keys(bcl#"mult"), beta -> petMults#?beta
            and petMults#beta == (bcl#"mult")#beta));
    mults := bcl#"mult";
    assert(numrows(bcl#"real") === 6);
    assert(numrows(bcl#"imag") === 25);
    assert(mults#{3,3} === 2);
    assert(mults#{3,4} === 3);
    assert(mults#{5,7} === 15);
    assert(not mults#?{8,4});

    A3 := matrix{{2,-2,0},{-2,2,-1},{0,-1,2}};
    b3 := indefiniteRootSnapshot(A3, 6, Algorithm => "BCL");
    p3 := indefiniteRootSnapshot(A3, 6, Algorithm => "Peterson");
    p3Mults := p3#"mult";
    assert(#keys(b3#"mult") == #keys p3Mults
        and all(keys(b3#"mult"), beta -> p3Mults#?beta
            and p3Mults#beta == (b3#"mult")#beta));
    assert(numrows(b3#"real") === 10);
    assert(numrows(b3#"imag") === 9);
    assert(not (b3#"mult")#?{0,2,2});
///

-- Test 24b: divisor-only isotropic coefficients must survive sparse
-- convolution.  The affine A1 null roots n*delta all have multiplicity 1.
TEST ///
    A := matrix{{2,-2},{-2,2}};
    snap := indefiniteRootSnapshot(A, 8, Algorithm => "Peterson");
    mults := snap#"mult";
    assert(mults#{1,1} === 1);
    assert(mults#{2,2} === 1);
    assert(mults#{3,3} === 1);
    assert(mults#{4,4} === 1);
///

-- Test 25: Cominuscule parabolic depth = 1 (small types keep runtime <1s)
TEST ///
    -- A_4 / P_2 = Gr(2,5): highest root = (1,1,1,1), coefficient of a_2 = 1
    --   so depth = 1 and g_2 = 0.  A_4 = cartanA 5.
    assert(#leviHighestWeights(cartanA 5, 2, {2}) === 0);
    -- A_3 / P_1 = P^3: end node, coefficient of a_1 in theta = 1
    assert(#leviHighestWeights(cartanA 4, 2, {1}) === 0);
///

-- Test 26: Non-cominuscule parabolic has depth > 1
TEST ///
    -- G_2 / P_2: coefficient of a_2 in theta = 2, so g_1, g_2 nontrivial, g_3 = 0.
    -- Much faster than the E_7 case and exercises the same code path.
    assert(#leviHighestWeights(cartanG2(), 1, {2}) > 0);
    assert(#leviHighestWeights(cartanG2(), 2, {2}) > 0);
    assert(#leviHighestWeights(cartanG2(), 3, {2}) === 0);
///

-- Test 27: rootSnapshotByHeight produces a Net and doesn't error
TEST ///
    A := matrix{{2,-2},{-2,2}};
    snap := affineRootSnapshot(A, 2);
    N := rootSnapshotByHeight snap;
    assert(instance(N, Net));
///

-- Test 28: Bruhat order sanity
TEST ///
    A := cartanA 3;
    -- Identity is below everything
    assert(bruhatLessOrEqual(A, {}, {1,2,1}));
    -- w0 is above everything
    w0 := longElement A;
    assert(bruhatLessOrEqual(A, {1,2}, w0));
    -- Reflexive
    assert(bruhatLessOrEqual(A, {1,2,1}, {1,2,1}));
///

-- Test 29: Weyl-length generating polynomial is Poincaré polynomial
TEST ///
    -- For A_3 (= cartanA 4) this is [4]_q! = (1+q)(1+q+q^2)(1+q+q^2+q^3)
    -- Evaluation at q=1 gives 24 = |W(A_3)|.
    pol := weylLengthGenerating cartanA 4;
    q := (ring pol)_0;
    assert(instance(pol,RingElement));
    assert(instance(ring pol,PolynomialRing));
    assert(ring pol === ring kazhdanLusztigVariable());
    assert(toString q === "q");
    assert(sub(pol,{q=>1}) == 24);
    assert(degree pol === {6});
    assert(apply(toList(0..6),k -> coefficient(q^k,pol)) == {1,3,5,6,5,3,1});
    assert(pol == (1+q)*(1+q+q^2)*(1+q+q^2+q^3));
    assert(pol == (q+1)^2*(q^2+q+1)*(q^2+1));
    assert(value factor pol == pol);
    -- For A_2: |W| = 6
    assert(sub(weylLengthGenerating cartanA 3,{q=>1}) == 6);
    -- For G_2: |W| = 12
    polG2 := weylLengthGenerating cartanG2();
    assert(sub(polG2,{q=>1}) == 12);
    assert(degree polG2 === {6});
    assert(apply(toList(0..6),k -> coefficient(q^k,polG2)) == {1,2,2,2,2,2,1});
    assert(polG2 == (1+q)*sum apply(toList(0..5),k -> q^k));
    assert(value factor polG2 == polG2);

    -- Direct products multiply their ordinary Poincare polynomials.
    Aproduct := matrix{{2,-1,0},{-1,2,0},{0,0,2}};
    productPolynomial := weylLengthGenerating Aproduct;
    assert(productPolynomial ==
        weylLengthGenerating(cartanA 3)*weylLengthGenerating(cartanA 2));
    assert(sub(productPolynomial,{q=>1}) == 12);

    -- Empty root data have the constant polynomial 1 in the same ring.
    Azero := matrix mutableMatrix(ZZ,0,0);
    polZero := weylLengthGenerating Azero;
    assert(instance(polZero,RingElement));
    assert(ring polZero === ring pol);
    assert(polZero == 1 and toString polZero === "1");
    assert(degree polZero === {0});
    assert(value factor polZero == polZero);

    -- A legacy cached coefficient Tally cannot leak through the polynomial
    -- interface. Rational Cartan input also gives the same polynomial.
    legacyA := matrix{{2}};
    legacyA.cache#"weylLengthGenerating" = new Tally from {0=>1,1=>1};
    legacyPolynomial := weylLengthGenerating legacyA;
    assert(instance(legacyPolynomial,RingElement));
    assert(legacyPolynomial == 1+q);
    oldRing := ZZ[local oldq];
    foreignCacheA := matrix{{2}};
    foreignCacheA.cache#"weylLengthGenerating" = 1+oldRing_0;
    repairedPolynomial := weylLengthGenerating foreignCacheA;
    assert(ring repairedPolynomial === ring pol);
    assert(repairedPolynomial == 1+q);
    assert(weylLengthGenerating(cartanG2() ** QQ) == polG2);
///

-- Test 30: parabolicDegrees top-degree matches coefficient of removed simple
-- root in the highest root
TEST ///
    -- parabolicDegrees returns a list of {root, degree} pairs over the
    -- positive roots; the top degree = coef of alpha_i in theta.
    -- For parabolic {2} in A_3: alpha_2 coefficient in theta = 1.
    degs := parabolicDegrees(cartanA 3, {2});
    assert(max(degs / last) === 1);
    -- For parabolic {8} in E_8: the top degree matches the coefficient of
    -- the removed simple root in the package's highest-root labeling.
    degsE8 := parabolicDegrees(cartanE 8, {8});
    assert(max(degsE8 / last) === (highestRoot cartanE 8)#7);
///

-- Test 31: bridges to LieAlgebraRepresentations / WeylGroups
TEST ///
    -- rejects non-finite / non-GCM inputs (runs without companion packages)
    Aaff := matrix{{2,-2},{-2,2}};       -- affine sl_2 (corank 1)
    assert(cartanType Aaff =!= "finite");
    assert(try (toLieAlgebra Aaff; false) else true);
    assert(try (toRootSystem  Aaff; false) else true);
    NotCM := matrix{{2,1},{-1,2}};       -- not a GCM (wrong sign)
    assert(try (toLieAlgebra NotCM; false) else true);
    assert(try (toRootSystem  NotCM; false) else true);
    -- positive cases: the bridges lazy-load their companion packages
    needsPackage "LieAlgebraRepresentations";
    needsPackage "WeylGroups";
    LA := value (value "LieAlgebraRepresentations")#"private dictionary"#"LieAlgebra";
    RS := value (value "WeylGroups")#"private dictionary"#"RootSystem";
    assert(class toLieAlgebra cartanA 4 === LA);
    assert(class toLieAlgebra cartanB 3 === LA);
    assert(class toRootSystem cartanA 4 === RS);
    assert(class toRootSystem cartanG2() === RS);
///

-- Test 32: tripod (cartanT) Coxeter classification and well-known
-- identifications with E_n / affine E_n / hyperbolic E_10.
TEST ///
    -- basic shape
    assert(numrows cartanT(2,3,3) === 6);
    assert(numrows cartanT(2,3,5) === 8);
    assert(numrows cartanT(2,3,7) === 10);  -- E_10 has rank 10
    -- Coxeter classification: 1/p + 1/q + 1/r  >, =, < 1
    -- finite cases
    assert(cartanType cartanT(2,3,3) === "finite");  -- E_6
    assert(cartanType cartanT(2,3,4) === "finite");  -- E_7
    assert(cartanType cartanT(2,3,5) === "finite");  -- E_8
    assert(cartanType cartanT(2,2,5) === "finite");  -- D_6
    -- affine cases
    assert(cartanType cartanT(3,3,3) === "affine");  -- affine E_6
    assert(cartanType cartanT(2,4,4) === "affine");  -- affine E_7
    assert(cartanType cartanT(2,3,6) === "affine");  -- affine E_8
    -- hyperbolic / indefinite
    assert(cartanType cartanT(2,3,7) === "indefinite");  -- E_10
    assert(cartanType cartanT(3,3,4) === "indefinite");
    assert(cartanType cartanT(4,4,4) === "indefinite");
    -- cartanT(2,3,3) agrees with E_6 as a Kac-Moody datum: same
    -- rank, determinant, and symmetrizer (invariants independent
    -- of node permutation).  The full Weyl-group check is exercised
    -- in the main weyl-group TEST block.
    assert(numrows cartanT(2,3,3) === 6);
    assert(det (cartanT(2,3,3) ** QQ) === det (cartanE 6 ** QQ));
    -- correct error behavior
    assert(try (cartanT(0,3,3); false) else true);
    assert(try (cartanT(2,3,3.0); false) else true);
///

-- Test 33: cartanStar shape and classification.
TEST ///
    -- {2,2,2} == D_4 (trivalent center, three leaves): same Weyl group
    assert(numrows cartanStar({2,2,2}) === 4);
    assert(cartanType cartanStar({2,2,2}) === "finite");
    assert(weylGroupOrder cartanStar({2,2,2}) === weylGroupOrder cartanD 4);
    -- 4-armed star is affine D_4^(1) when all arms have length 2
    assert(cartanType cartanStar({2,2,2,2}) === "affine");
    -- 5-armed star is always indefinite
    assert(cartanType cartanStar({2,2,2,2,2}) === "indefinite");
    -- short arms (length 1) are ignored: {1,1,...} is trivial center
    assert(numrows cartanStar({1,1,1,1}) === 1);
    assert(cartanType cartanStar({1,1,1,1}) === "finite");  -- rank 1, A_1
    -- two nontrivial arms give a chain (type A); {3, 3} has rank 5
    assert(numrows cartanStar({3,3}) === 5);
    assert(cartanType cartanStar({3,3}) === "finite");  -- A_5
    -- errors
    assert(try (cartanStar({}); false) else true);
    assert(try (cartanStar({0,2}); false) else true);
///

-- Test 34: cartanRank2 generic rank-2 GCM classification by ab.
TEST ///
    -- ab = 1: A_2
    assert(cartanType cartanRank2(1,1) === "finite");
    assert(weylGroupOrder cartanRank2(1,1) === 6);
    -- ab = 2: B_2 = C_2
    assert(cartanType cartanRank2(1,2) === "finite");
    assert(weylGroupOrder cartanRank2(1,2) === 8);
    assert(cartanType cartanRank2(2,1) === "finite");
    -- ab = 3: G_2
    assert(cartanType cartanRank2(1,3) === "finite");
    assert(weylGroupOrder cartanRank2(1,3) === 12);
    assert(cartanType cartanRank2(3,1) === "finite");
    -- ab = 4: affine
    assert(cartanType cartanRank2(2,2) === "affine");  -- A_1^(1)
    assert(cartanType cartanRank2(1,4) === "affine");  -- A_2^(2) diagram
    assert(cartanType cartanRank2(4,1) === "affine");
    -- ab >= 5: indefinite
    assert(cartanType cartanRank2(2,3) === "indefinite");
    assert(cartanType cartanRank2(3,2) === "indefinite");
    assert(cartanType cartanRank2(5,1) === "indefinite");
    -- input validation
    assert(try (cartanRank2(0,1); false) else true);
    assert(try (cartanRank2(-1,2); false) else true);
///

-- Test 35: cartanExtendUntwisted matches hard-coded affine constructors.
TEST ///
    assert((cartanExtendUntwisted cartanA 2) == cartanAffineA 1);
    assert((cartanExtendUntwisted cartanA 4) == cartanAffineA 3);
    assert((cartanExtendUntwisted cartanA 5) == cartanAffineA 4);
    assert((cartanExtendUntwisted cartanB 3) == cartanAffineB 3);
    assert((cartanExtendUntwisted cartanC 3) == cartanAffineC 3);
    assert((cartanExtendUntwisted cartanD 4) == cartanAffineD 4);
    assert((cartanExtendUntwisted cartanF4()) == cartanAffineF4());
    assert((cartanExtendUntwisted cartanG2()) == cartanAffineG2());
    -- untwisted extensions are affine
    assert(cartanType cartanExtendUntwisted cartanA 4 === "affine");
    assert(cartanType cartanExtendUntwisted cartanE 6 === "affine");
    -- reject non-finite input
    assert(try (cartanExtendUntwisted cartanAffineA 3; false) else true);
///

-- Test 36: untwisted affine constructors (shape + type + radical).
TEST ///
    -- all hard-coded affines are of affine type
    assert(cartanType cartanAffineA 1 === "affine");
    assert(cartanType cartanAffineA 3 === "affine");
    assert(cartanType cartanAffineB 3 === "affine");
    assert(cartanType cartanAffineC 2 === "affine");
    assert(cartanType cartanAffineD 4 === "affine");
    assert(cartanType cartanAffineE 6 === "affine");
    assert(cartanType cartanAffineE 7 === "affine");
    assert(cartanType cartanAffineE 8 === "affine");
    assert(cartanType cartanAffineF4() === "affine");
    assert(cartanType cartanAffineG2() === "affine");
    -- shapes
    assert(numrows cartanAffineA 3 === 4);
    assert(numrows cartanAffineB 3 === 4);
    assert(numrows cartanAffineC 3 === 4);
    assert(numrows cartanAffineD 4 === 5);
    assert(numrows cartanAffineE 6 === 7);
    assert(numrows cartanAffineE 7 === 8);
    assert(numrows cartanAffineE 8 === 9);
    assert(numrows cartanAffineF4() === 5);
    assert(numrows cartanAffineG2() === 3);
    -- determinant of every affine Cartan matrix is 0
    assert(det cartanAffineA 3 === 0);
    assert(det cartanAffineB 3 === 0);
    assert(det cartanAffineC 3 === 0);
    assert(det cartanAffineD 4 === 0);
    assert(det cartanAffineE 6 === 0);
    assert(det cartanAffineF4() === 0);
    assert(det cartanAffineG2() === 0);
    -- input validation
    assert(try (cartanAffineA 0; false) else true);
    assert(try (cartanAffineB 2; false) else true);   -- B requires n >= 3
    assert(try (cartanAffineC 1; false) else true);
    assert(try (cartanAffineD 3; false) else true);   -- D requires n >= 4
    assert(try (cartanAffineE 5; false) else true);
    assert(try (cartanAffineE 9; false) else true);
///

-- Test 37: over-extended and very-extended (E_10, E_11).
TEST ///
    -- E_10 = cartanOverExtended cartanE 8 is rank 10
    E10 := cartanOverExtended cartanE 8;
    assert(numrows E10 === 10);
    assert(isCartanMatrix E10);
    assert(cartanType E10 === "indefinite");
    -- E_11 = cartanVeryExtended cartanE 8 is rank 11
    E11 := cartanVeryExtended cartanE 8;
    assert(numrows E11 === 11);
    assert(isCartanMatrix E11);
    assert(cartanType E11 === "indefinite");
    -- Consistency: the very-extension is the over-extension prepended
    -- by one more node, so the lower-right 10x10 of E11 is E10 (with
    -- the extra node at index 0).
    Esub := submatrix(E11, toList(1..10), toList(1..10));
    assert(Esub == E10);
    -- Low-rank sanity: over-extending A_2 is rank 4 and indefinite
    O4 := cartanOverExtended cartanA 3;  -- A_2 (rank 2) -> 2+2 = 4
    assert(numrows O4 === 4);
    assert(cartanType O4 === "indefinite");
///

----------------------------------------------------------------------
-- Stress tests: self-consistency checks exercising boundary cases
-- and cross-invariants across finite types.
----------------------------------------------------------------------

-- Test 38: rank-1 (A_1) boundary case
TEST ///
    A1 := cartanA 2;  -- A_1 = sl_2
    assert(numrows A1 === 1);
    assert(numrows cartanPositiveRoots A1 === 1);
    assert(weylGroupOrder A1 === 2);
    assert(weylLength(A1, longElement A1) === 1);
    assert(entries weylElementMatrix(A1, longElement A1) == entries(- id_(ZZ^1)));
    assert(weylDimension(A1, {0}) == 1);
    assert(weylDimension(A1, {1}) == 2);
    assert(weylDimension(A1, {2}) == 3);
    assert(# leftDescentSet(A1, longElement A1) === 1);
    assert(# leftDescentSet(A1, {}) === 0);
    assert(highestRoot A1 == {1});
    assert(entries simpleRoots A1 == entries cartanPositiveRoots A1);
///

-- Test 39: identity element across types
TEST ///
    for T in {cartanA 4, cartanB 3, cartanC 3, cartanD 4, cartanG2(), cartanF4()} do (
        r := numrows T;
        assert(weylLength(T, {}) === 0);
        assert(entries weylElementMatrix(T, {}) == entries id_(ZZ^r));
        assert(# leftDescentSet(T, {})  === 0);
        assert(# rightDescentSet(T, {}) === 0);
        assert(reducedWord(T, {}) == {});
    );
///

-- Test 40: longest element w_0 properties across types
TEST ///
    for T in {cartanA 3, cartanA 4, cartanB 3, cartanC 3, cartanD 4, cartanG2(), cartanF4()} do (
        r := numrows T;  w0 := longElement T;
        assert(length reducedWord(T, w0 | w0) === 0);
        assert(weylLength(T, w0) === numrows cartanPositiveRoots T);
        assert(sort leftDescentSet(T, w0)  == toList(1..r));
        assert(sort rightDescentSet(T, w0) == toList(1..r));
    );
///

-- Test 41: P_W(1)=|W|; degrees and coefficients are palindromic
TEST ///
    for T in {cartanA 2, cartanA 3, cartanA 4, cartanB 2, cartanB 3, cartanC 3, cartanD 4, cartanG2(), cartanF4()} do (
        P := weylLengthGenerating T;
        q := (ring P)_0;
        assert(instance(P,RingElement));
        assert(sub(P,{q=>1}) == weylGroupOrder T);
        assert(degree P === {numrows cartanPositiveRoots T});
        ppos := numrows cartanPositiveRoots T;
        assert(all(toList(0..ppos),k ->
            coefficient(q^k,P) == coefficient(q^(ppos-k),P)));
        assert(coefficient(1_(ring P),P) == 1);
        assert(coefficient(q^ppos,P) == 1);
    );
///

-- Test 42: the longest element is an involution, so its left and right
-- descent sets agree.  (Centrality is neither asserted nor needed.)
TEST ///
    for T in {cartanA 3, cartanB 3, cartanD 4} do (
        w0 := longElement T;
        assert(sort leftDescentSet(T, w0) == sort rightDescentSet(T, w0));
    );
///

-- Test 43: root-count identities
TEST ///
    for T in {cartanA 3, cartanA 4, cartanB 3, cartanC 3, cartanD 4, cartanG2(), cartanF4()} do (
        pr := numrows cartanPositiveRoots T;
        assert(numrows negativeRoots T === pr);
        assert(numrows allRoots T === 2*pr);
        -- height-1 positive roots are exactly the simple roots: count = rank
        h1 := select(entries cartanPositiveRoots T, v -> rootHeight v === 1);
        assert(#h1 === numrows T);
    );
///

-- Test 44: dim V(0)=1, dim V(theta) = dim g for the adjoint highest weight
TEST ///
    for T in {cartanA 2, cartanA 4, cartanB 3, cartanC 3, cartanD 4, cartanG2(), cartanF4(), cartanE 6} do (
        r := numrows T;
        assert(weylDimension(T, toList(r:0)) == 1);
        hrSimple := highestRoot T;
        hrDynkin := flatten entries (T * transpose matrix {hrSimple});
        expected := 2 * (numrows cartanPositiveRoots T) + r;
        assert(weylDimension(T, hrDynkin) == expected);
    );
///

-- Test 45: tensor-decomposition multiplicativity -- sum of component dims = product of factor dims
TEST ///
    triples := {
        (cartanA 3, {1,0}, {0,1}),
        (cartanA 3, {2,0}, {0,2}),
        (cartanA 4, {1,0,0}, {0,0,1}),
        (cartanB 3, {1,0,0}, {1,0,0}),
        (cartanD 4, {1,0,0,0}, {0,0,1,0}),
        (cartanG2(), {1,0}, {0,1}),
        (cartanC 3, {1,0,0}, {1,0,0})
    };
    for triple in triples do (
        T := triple#0;  lam := triple#1;  mu := triple#2;
        dLam := weylDimension(T, lam);  dMu := weylDimension(T, mu);
        TD := tensorDecomposition(T, lam, mu);
        sumDim := sum apply(keys TD, k -> (
            lambda := try k#0 else k;
            weylDimension(T, lambda) * TD#k
        ));
        assert(sumDim === dLam * dMu);
    );
///

-- Test 46: sum of weight multiplicities = dim V(lambda)
TEST ///
    for pair in {
        (cartanA 3, {1,1}),
        (cartanA 4, {1,0,0}),
        (cartanB 3, {0,0,1}),
        (cartanD 4, {1,1,0,0}),
        (cartanG2(), {1,0}),
        (cartanG2(), {0,1}),
        (cartanC 3, {0,1,0}),
        (cartanA 4, {2,0,0})
    } do (
        T := pair#0;  lam := pair#1;
        ws := weightsOfRep(T, lam);
        total := sum apply(ws, mu -> weightMultiplicity(T, lam, mu));
        assert(total == weylDimension(T, lam));
    );
///

-- Test 47: Weyl-invariance of weight multiplicities under simple reflections.
-- s_i acts on Dynkin labels by d_j -> d_j - d_i * A_{j,i} (column i of Cartan).
TEST ///
    testSimpleReflInv := (T, lam) -> (
        ws := weightsOfRep(T, lam);
        wsSet := set ws;
        r := numrows T;
        ent := entries T;
        ok := true;
        for mu in ws do (
            mMu := weightMultiplicity(T, lam, mu);
            for i from 1 to r do (
                muP := apply(r, j -> mu#j - mu#(i-1) * ent#j#(i-1));
                if member(muP, wsSet) then (
                    if mMu != weightMultiplicity(T, lam, muP) then (ok = false; break);
                ) else (
                    if muP =!= mu then (ok = false; break);
                );
            );
            if not ok then break;
        );
        ok
    );
    for pair in {
        (cartanA 4, {1,1,0}),
        (cartanB 3, {0,0,1}),
        (cartanG2(), {0,1}),
        (cartanA 3, {2,2})
    } do (
        T := pair#0;  lam := pair#1;
        assert(testSimpleReflInv(T, lam));
    );
///

-- Test 48: partition <-> fundamental round-trip (type A)
TEST ///
    -- The public wrappers choose the type-A representative whose final
    -- partition coordinate is zero; a global partition shift is forgotten.
    for m in {{0,0,0}, {1,0,0}, {0,1,0}, {0,0,1}, {1,1,0}, {2,1,0}, {2,0,1}, {3,2,1}, {1,1,1,1}} do (
        A := cartanA(#m + 1);
        part := fundamentalToPartition(A, m);
        back := partitionToFundamental(A, part);
        assert(back === m);
    );
    for part in {{1,0,0,0}, {2,1,0,0}, {3,2,1,0}, {1,1,1,1}, {5,0,0,0}, {2,2,2,0}, {4,4,4,4}} do (
        A := cartanA(#part);
        m := partitionToFundamental(A, part);
        back := fundamentalToPartition(A, m);
        shift := last part;
        assert(back === apply(part, x -> x - shift));
    );
///

-- Test 49: |Phi| = Coxeter number * rank for finite types
TEST ///
    for T in {cartanA 4, cartanB 3, cartanC 3, cartanD 4, cartanG2(), cartanF4(), cartanE 6} do (
        h := (rootHeight highestRoot T) + 1;
        assert(2 * numrows cartanPositiveRoots T === h * numrows T);
    );
///

-- Test 50: Langlands duality via Cartan transpose preserves |W| and |Phi|
TEST ///
    for T in {cartanA 3, cartanB 3, cartanC 3, cartanF4(), cartanG2(), cartanE 6} do (
        Ttrans := transpose T;
        assert(isCartanMatrix Ttrans);
        assert(weylGroupOrder T === weylGroupOrder Ttrans);
        assert(numrows cartanPositiveRoots T === numrows cartanPositiveRoots Ttrans);
    );
///

-- Test 51: dim V(rho) = 2^{|Phi^+|} (Weyl denominator identity at q=1)
TEST ///
    for T in {cartanA 3, cartanB 3, cartanC 3, cartanD 4, cartanG2(), cartanF4()} do (
        rho := toList(numrows T:1);
        assert(weylDimension(T, rho) == 2^(numrows cartanPositiveRoots T));
    );
///

-- Test 52: adjoint representation: mult(0) = rank,  mult(theta) = 1
TEST ///
    for T in {cartanA 4, cartanB 3, cartanG2()} do (
        r := numrows T;
        hrDynkin := flatten entries (T * transpose matrix {highestRoot T});
        zero := toList(r:0);
        assert(weightMultiplicity(T, hrDynkin, zero) == r);
        assert(weightMultiplicity(T, hrDynkin, hrDynkin) == 1);
    );
///

-- Test 53: length of reducedWord(T, w) agrees with weylLength(T, w)
TEST ///
    for T in {cartanA 3, cartanA 4, cartanB 3, cartanD 4} do (
        for w in {{1}, {1,2}, {1,2,1}, {2,1,2}, {1,2,3,2,1}} do (
            if all(w, i -> i <= numrows T) then
                assert(length reducedWord(T, w) === weylLength(T, w));
        );
    );
///

-- Test 54: s_i^2 = id for every simple reflection, including affine type
TEST ///
    for T in {cartanA 3, cartanB 3, cartanG2(), cartanF4(), cartanAffineA 3} do (
        r := numrows T;
        for i from 1 to r do
            assert(entries weylElementMatrix(T, {i,i}) == entries id_(ZZ^r));
    );
///

-- Test 55: every braid and commuting relation for A_3 and A_3^(1)
TEST ///
    A3 := cartanA 4;
    for p in {{1,2},{2,3}} do
        assert(weylElementMatrix(A3,{p#0,p#1,p#0}) ==
            weylElementMatrix(A3,{p#1,p#0,p#1}));
    assert(weylElementMatrix(A3,{1,3}) ==
        weylElementMatrix(A3,{3,1}));

    A3aff := cartanAffineA 3;
    for p in {{1,2},{2,3},{3,4},{4,1}} do
        assert(weylElementMatrix(A3aff,{p#0,p#1,p#0}) ==
            weylElementMatrix(A3aff,{p#1,p#0,p#1}));
    for p in {{1,3},{2,4}} do
        assert(weylElementMatrix(A3aff,{p#0,p#1}) ==
            weylElementMatrix(A3aff,{p#1,p#0}));
///

-- Test 56: tensorCoefficient agrees with tensorDecomposition
TEST ///
    for spec in {
        (cartanA 3, {1,0}, {0,1}),
        (cartanA 3, {1,1}, {1,1}),
        (cartanA 4, {1,0,0}, {0,0,1}),
        (cartanA 4, {1,1,0}, {0,1,1}),
        (cartanB 3, {1,0,0}, {1,0,0}),
        (cartanB 3, {0,0,1}, {0,0,1}),
        (cartanC 3, {1,0,0}, {1,0,0}),
        (cartanD 4, {1,0,0,0}, {1,0,0,0}),
        (cartanG2(), {1,0}, {1,0}),
        (cartanG2(), {0,1}, {0,1})
    } do (
        T := spec#0; lam := spec#1; mu := spec#2;
        TD := tensorDecomposition(T, lam, mu);
        r := numrows T;
        -- Every constituent must agree
        for k in keys TD do (
            nu := try k#0 else k;
            assert(tensorCoefficient(T, lam, mu, nu) == TD#k);
        );
        -- A weight outside the support returns 0
        bigNu := apply(r, i -> (lam#i + mu#i) + 1);
        assert(tensorCoefficient(T, lam, mu, bigNu) == 0);
    );
    -- Partition-format sanity: 7 (x) 7 in B_3 = 1 + 21 + 27
    assert(tensorCoefficient(cartanB 3, {1,0,0},{1,0,0},{0,0,0},WeightFormat=>"Partition") == 1);
    assert(tensorCoefficient(cartanB 3, {1,0,0},{1,0,0},{1,1,0},WeightFormat=>"Partition") == 1);
    assert(tensorCoefficient(cartanB 3, {1,0,0},{1,0,0},{2,0,0},WeightFormat=>"Partition") == 1);
///

----------------------------------------------------------------------
-- Cross-compatibility tests against sibling packages.  These are
-- guarded with `try needsPackage` and an explicit skip notice when
-- the sibling package cannot be loaded.  When it is available, every listed
-- invariant must agree on the nose.
--
-- Convention bridge (KacMoody  <->  LieAlgebraRepresentations):
--   cartanA n   <-->  simpleLieAlgebra("A", n-1)   (both sl_n)
--   cartanB n   <-->  simpleLieAlgebra("B", n)
--   cartanC n   <-->  simpleLieAlgebra("C", n)
--   cartanD n   <-->  simpleLieAlgebra("D", n)
--   cartanE n   <-->  simpleLieAlgebra("E", n)
--   cartanF4()  <-->  simpleLieAlgebra("F", 4)
--   cartanG2()  <-->  simpleLieAlgebra("G", 2)
-- LieAlgebraRepresentations uses the dual labeling for B<->C and the
-- transposed Cartan for F4/G2, so the Cartan matrices themselves do
-- not match on the nose in those types; but all *numerical*
-- invariants (Weyl dimensions, tensor coefficients, adjoint dims)
-- agree because the natural relabeling is absorbed by the
-- fundamental-weight conventions.  We only compare Cartan matrices
-- directly for simply-laced types (A, D, E).
----------------------------------------------------------------------

-- Test 57: LieAlgebraRepresentations cross-compatibility
TEST ///
    LAR := try needsPackage "LieAlgebraRepresentations" else null;
    if LAR === null then print "SKIP: LieAlgebraRepresentations comparison tests (package could not be loaded)";
    if LAR =!= null then (
        -- Map from KM name to LAR's simpleLieAlgebra constructor
        kmToLar := (tp, n) -> (
            if tp === "A" then simpleLieAlgebra(tp, n-1)
            else if tp === "F" then simpleLieAlgebra("F", 4)
            else if tp === "G" then simpleLieAlgebra("G", 2)
            else simpleLieAlgebra(tp, n)
        );
        kmCartan := (tp, n) -> (
            if tp === "A" then cartanA n
            else if tp === "B" then cartanB n
            else if tp === "C" then cartanC n
            else if tp === "D" then cartanD n
            else if tp === "E" then cartanE n
            else if tp === "F" then cartanF4()
            else if tp === "G" then cartanG2()
        );

        -- (a) Cartan matrices agree for simply-laced (A, D, E).
        for pair in {("A",3), ("A",4), ("D",4), ("D",5),
                     ("E",6), ("E",7), ("E",8)} do (
            (tp, n) := pair;
            assert(entries cartanMatrix kmToLar(tp, n) == entries kmCartan(tp, n));
        );

        -- (b) Irreducible-module dimensions agree across all six families.
        --     Same Dynkin labels; both dimensions are exact integers.
        for spec in {
            ("A",3,{1,0}), ("A",3,{1,1}),
            ("A",4,{1,0,0}), ("A",4,{1,1,1}),
            ("A",5,{2,0,0,0}),
            ("B",3,{1,0,0}), ("B",3,{0,0,1}),    -- std and spin
            ("C",3,{1,0,0}), ("C",3,{0,1,0}),
            ("D",4,{1,0,0,0}), ("D",4,{0,0,1,0}),
            ("G",2,{1,0}), ("G",2,{0,1}), ("G",2,{1,1}),
            ("F",4,{1,0,0,0}),
            ("E",8,{0,0,0,0,0,0,0,1})
        } do (
            (tp, n, lam) := spec;
            gLar := kmToLar(tp, n);
            Tkm := kmCartan(tp, n);
            assert(dim irreducibleLieAlgebraModule(lam, gLar) == weylDimension(Tkm, lam));
        );

        -- (c) Adjoint module dimension agrees (= dim g = 2|Phi^+| + rank).
        for pair in {("A",3),("A",4),("B",3),("C",3),("D",4),("G",2),("F",4),("E",6)} do (
            (tp, n) := pair;
            Tkm := kmCartan(tp, n);
            r := numrows Tkm;
            hr := KacMoody$highestRoot Tkm;
            hrDyn := flatten entries (Tkm * transpose matrix {hr});
            assert(dim adjointModule kmToLar(tp, n) == weylDimension(Tkm, hrDyn));
            assert(dim adjointModule kmToLar(tp, n) == 2 * numrows cartanPositiveRoots Tkm + r);
        );

        -- (d) Tensor-product total dimension agrees (V(lam) ** V(mu) has
        --     dim(V(lam)) * dim(V(mu))).
        for spec in {
            ("A",3,{1,0},{0,1}),
            ("A",4,{1,0,0},{0,0,1}),
            ("B",3,{1,0,0},{1,0,0}),
            ("G",2,{1,0},{1,0})
        } do (
            (tp, n, lam, mu) := spec;
            gLar := kmToLar(tp, n);
            V := irreducibleLieAlgebraModule(lam, gLar);
            W := irreducibleLieAlgebraModule(mu, gLar);
            Tkm := kmCartan(tp, n);
            TD := tensorDecomposition(Tkm, lam, mu);
            sumDimKm := sum apply(keys TD, k -> (
                nu := try k#0 else k;
                weylDimension(Tkm, nu) * TD#k
            ));
            assert(dim(V ** W) == sumDimKm);
        );

        -- (e) Individual tensor coefficients match on common inputs.
        --     Use LieAlgebraRepresentations$tensorCoefficient to
        --     disambiguate -- KacMoody exports the same name.
        for spec in {
            ("A",3,{1,0},{0,1},{0,0}),   -- std (x) dual = adj + triv
            ("A",3,{1,0},{0,1},{1,1}),
            ("A",4,{1,1,0},{0,1,1},{1,0,1}),
            ("B",3,{1,0,0},{1,0,0},{0,0,0}),
            ("B",3,{1,0,0},{1,0,0},{2,0,0}),
            ("G",2,{1,0},{1,0},{0,0})
        } do (
            (tp, n, lam, mu, nu) := spec;
            gLar := kmToLar(tp, n);
            V := irreducibleLieAlgebraModule(lam, gLar);
            W := irreducibleLieAlgebraModule(mu, gLar);
            U := irreducibleLieAlgebraModule(nu, gLar);
            Tkm := kmCartan(tp, n);
            assert(LieAlgebraRepresentations$tensorCoefficient(V, W, U)
                   == KacMoody$tensorCoefficient(Tkm, lam, mu, nu));
        );
    );
///

-- Test 58: WeylGroups cross-compatibility
TEST ///
    WG := try needsPackage "WeylGroups" else null;
    if WG === null then print "SKIP: WeylGroups comparison tests (package could not be loaded)";
    if WG =!= null then (
        -- WG exposes rootSystemA/B/C/D/E/F4/G2 and matches KM for *all*
        -- combinatorial invariants of the Weyl group (|W|, length,
        -- reduced-decomposition length, |Phi^+|).
        wgRoot := (tp, n) -> (
            if tp === "A" then rootSystemA n
            else if tp === "B" then rootSystemB n
            else if tp === "C" then rootSystemC n
            else if tp === "D" then rootSystemD n
            else if tp === "E" then rootSystemE n
            else if tp === "F" then rootSystemF4
            else if tp === "G" then rootSystemG2
        );
        kmCartan := (tp, n) -> (
            if tp === "A" then cartanA(n+1)
            else if tp === "B" then cartanB n
            else if tp === "C" then cartanC n
            else if tp === "D" then cartanD n
            else if tp === "E" then cartanE n
            else if tp === "F" then cartanF4()
            else if tp === "G" then cartanG2()
        );

        -- (a) |W| matches (kept small to bound test time).
        for pair in {("A",2),("A",3),("A",4),("B",2),("B",3),("C",3),("D",4),("G",2)} do (
            (tp, n) := pair;
            R := wgRoot(tp, n);
            Tkm := kmCartan(tp, n);
            nWG := sum apply(numberOfPositiveRoots R + 1, k -> # listWeylGroupElements(R, k));
            assert(nWG == weylGroupOrder Tkm);
        );

        -- (b) |Phi^+| matches (including exceptional).
        for pair in {("A",3),("A",4),("B",3),("C",3),("D",4),("G",2),("F",4),("E",6)} do (
            (tp, n) := pair;
            R := wgRoot(tp, n);
            Tkm := kmCartan(tp, n);
            assert(numberOfPositiveRoots R == numrows cartanPositiveRoots Tkm);
        );

        -- (c) Length of longest element agrees (= |Phi^+|).
        for pair in {("A",3),("A",4),("B",3),("C",3),("D",4),("G",2),("F",4)} do (
            (tp, n) := pair;
            R := wgRoot(tp, n);
            Tkm := kmCartan(tp, n);
            assert(coxeterLength longestWeylGroupElement R == weylLength(Tkm, longElement Tkm));
        );

        -- (d) Any reduced decomposition of w0 has length |Phi^+|.
        for pair in {("A",3),("A",4),("B",3),("D",4),("G",2)} do (
            (tp, n) := pair;
            R := wgRoot(tp, n);
            Tkm := kmCartan(tp, n);
            wWG := reducedDecomposition longestWeylGroupElement R;
            wKM := reducedWord(Tkm, longElement Tkm);
            assert(# wWG == # wKM);
            assert(# wWG == numrows cartanPositiveRoots Tkm);
        );
    );
///

-- Test 59: dualCartanMatrix is an involution on every finite type.
TEST ///
    -- Non-simply-laced: dual swaps B_n <-> C_n, fixes F_4 and G_2 up
    -- to transpose, so dual of dual must be the identity.
    for A in {cartanA 4, cartanB 3, cartanB 4, cartanC 3, cartanC 4,
              cartanD 4, cartanD 5, cartanE 6, cartanF4(), cartanG2()} do (
        assert(A == dualCartanMatrix dualCartanMatrix A);
    );
    -- B <-> C swap in particular.
    assert(dualCartanMatrix cartanB 3 == cartanC 3);
    assert(dualCartanMatrix cartanC 3 == cartanB 3);
    -- Simply-laced: dual is the identity.
    for A in {cartanA 5, cartanD 5, cartanE 6, cartanE 7, cartanE 8} do (
        assert(dualCartanMatrix A == A);
    );
///

-- Test 60: affine imaginary-root multiplicity equals the rank of the
-- underlying finite-type Lie algebra for every untwisted affine type.
TEST ///
    specs := {
        (cartanAffineA 3,  4,  3),  -- aff A_3 (sl_4): finite rank 3
        (cartanAffineA 4,  5,  4),
        (cartanAffineB 3,  6,  3),  -- delta has max coord 2, need H >= height(delta) = 5
        (cartanAffineC 3,  6,  3),
        (cartanAffineD 4,  8,  4),  -- delta has max coord 2
        (cartanAffineF4(), 12, 4),  -- delta height = 9
        (cartanAffineG2(), 6,  2)   -- delta height = 5
    };
    for spec in specs do (
        (Aa, H, expectedMult) := spec;
        snap := affineRootSnapshot(Aa, H);
        assert(numrows snap#"imag" >= 1);
        -- Every imaginary root in the snapshot has the expected mult.
        for r in entries snap#"imag" do (
            assert((snap#"mult")#r == expectedMult);
        );
        -- Default imaginary multiplicity agrees with finite rank
        -- (numrows - 1 for untwisted affine).
        assert(expectedMult == numrows Aa - 1);
    );
///

-- Test 61: parabolic decomposition at extreme Levi subalgebras.
TEST ///
    -- parabolic = {} means the Levi is the whole algebra, so the
    -- nilradical is zero: no Levi-irreducibles at any degree.
    for A in {cartanA 3, cartanB 3, cartanD 4, cartanG2()} do (
        assert(numrows nilradicalRoots(A, {}) == 0);
        for d from 1 to 3 do (
            assert(# leviHighestWeights(A, d, {}) == 0);
        );
    );

    -- parabolic = all simple nodes removed means the Levi is just
    -- the Cartan torus (Borel decomposition).  At degree 1 the
    -- nilradical is spanned by the simple roots.  After forgetting
    -- the torus characters and restricting to the zero derived Levi,
    -- we get (rank) copies of V() at degree 1 and (# Phi^+ at height d)
    -- copies at degree d.
    A := cartanA 3;
    n := numrows A;
    removeAll := toList(1..n);
    T := leviHighestWeights(A, 1, removeAll);
    -- There are no derived simple factors, so each restricted module
    -- is an empty tensor product, stored as (new Irrep from {}).
    emptyIrrep := new Irrep from {};
    assert(T#emptyIrrep == n);
    -- Total number of nilradical roots = |Phi^+|.
    total := sum for d from 1 to 10 list sum values leviHighestWeights(A, d, removeAll);
    assert(total == numrows cartanPositiveRoots A);
///

-- Test 62: error paths on malformed input are caught cleanly.
TEST ///
    -- Degenerate rank arguments to type constructors.
    assert(try (cartanA 1; false) else true);   -- sl_1 doesn't exist
    assert(try (cartanA 0; false) else true);
    assert(try (cartanB 1; false) else true);
    assert(try (cartanC 1; false) else true);
    assert(try (cartanD 3; false) else true);   -- D_n needs n >= 4
    assert(try (cartanE 5; false) else true);   -- E_n needs 6 <= n <= 8
    assert(try (cartanE 9; false) else true);
    -- bruhatLessOrEqual rejects out-of-range simple-reflection indices.
    A := cartanA 3;  -- rank 2, valid indices 1..2
    assert(try (bruhatLessOrEqual(A, {5}, {1}); false) else true);
    assert(try (bruhatLessOrEqual(A, {1}, {0}); false) else true);
    assert(try (bruhatLessOrEqual(A, {-1}, {1}); false) else true);
    -- isCartanMatrix returns false (does not error) on invalid input.
    assert(not isCartanMatrix matrix {{2, -1}, {-1, 3}});   -- bad diag
    assert(not isCartanMatrix matrix {{2, 1}, {1, 2}});     -- bad off-diag sign
    assert(not isCartanMatrix matrix {{2, -1, 0}, {-1, 2, -1}});  -- not square
///

-- Test 63: cartanPositiveRoots returns exactly the positive rows of
-- allRoots (as sets), for every finite type the package supports.
TEST ///
    for A in {cartanA 3, cartanA 4, cartanB 3, cartanC 3, cartanD 4,
              cartanG2(), cartanF4(), cartanE 6} do (
        pos := entries cartanPositiveRoots A;
        allR := entries allRoots A;
        posFromAll := select(allR, r -> (all(r, x -> x >= 0) and any(r, x -> x > 0)));
        assert(set pos === set posFromAll);
        -- Sanity: |Phi| = 2 |Phi^+|, and negative roots are just the
        -- negatives of positive roots.
        negFromAll := select(allR, r -> (all(r, x -> x <= 0) and any(r, x -> x < 0)));
        assert(#allR == #pos + #negFromAll);
        assert(set pos === set apply(negFromAll, r -> apply(r, x -> -x)));
    );
///

-- Test 64: corrected Cartan classification and exceptional root data.
TEST ///
    rowsZZSet := M -> set apply(entries M,
        v -> apply(v, x -> lift(x, ZZ)));

    -- A generalized Cartan matrix is integral, and affine type is
    -- independent of component order.
    assert(not isCartanMatrix matrix(QQ, {{2,-1/2},{-2,2}}));
    X := matrix{{2,-2,0},{-2,2,0},{0,0,2}};
    Y := matrix{{2,0,0},{0,2,-2},{0,-2,2}};
    assert(cartanType X === "affine");
    assert(cartanType Y === "affine");
    twoAffine := matrix{{2,-2,0,0},{-2,2,0,0},
                        {0,0,2,-2},{0,0,-2,2}};
    -- Aggregate classification is componentwise: a direct sum of affine
    -- components remains affine even though its total corank is two.
    assert(cartanType twoAffine === "affine");
    assert(rowsZZSet cartanPositiveRoots(X,3)
        === set {{1,0,0},{0,1,0},{0,0,1},{1,1,0},{1,2,0},{2,1,0}});

    exceptional := {
        {simpleRootsE6(), cartanE 6, positiveRootsE6(), 36,
            {1,2,2,3,2,1}, {0,1,0,0,0,0}},
        {simpleRootsE7(), cartanE 7, positiveRootsE7(), 63,
            {2,2,3,4,3,2,1}, {1,0,0,0,0,0,0}},
        {simpleRootsE8(), cartanE 8, positiveRootsE8(), 120,
            {2,3,4,6,5,4,3,2}, {0,0,0,0,0,0,0,1}}
    };
    for spec in exceptional do (
        S := spec#0;
        A := spec#1;
        rootsEps := spec#2;
        assert(entries(S * transpose S) == entries A);
        assert(numrows rootsEps === spec#3);
        gram := rootsEps * transpose rootsEps;
        for i from 0 to numrows gram - 1 do assert(gram_(i,i) == 2);
        theta := highestRoot A;
        assert(theta === spec#4);
        assert(apply(toFundamental(A, theta), x -> lift(x, ZZ)) === spec#5);
    );
    assert(rowsZZSet cartanPositiveRoots(cartanG2())
        === set {{1,0},{0,1},{1,1},{2,1},{3,1},{3,2}});
///

-- Test 65: strong Bruhat order and total-height bounded roots.
TEST ///
    -- Independent reduced-subword oracle for every pair in W(A_3).
    subwordOracle := (A,u,v) -> (
        U := weylElementMatrix(A,u);
        vRed := reducedWord(A,v);
        positions := if #vRed == 0 then {} else toList(0..#vRed-1);
        any(subsets positions,
            J -> U == weylElementMatrix(A, apply(J,j -> vRed#j)))
    );
    A3 := cartanA 4;
    eltsA3 := weylGroupElements A3;
    for eu in eltsA3 do for ev in eltsA3 do (
        u := eu#"word";
        v := ev#"word";
        assert(bruhatLessOrEqual(A3,u,v) === subwordOracle(A3,u,v));
    );
    -- These comparabilities distinguish strong Bruhat from weak order.
    assert(bruhatLessOrEqual(cartanA 3,{1},{2,1}));
    assert(bruhatLessOrEqual(cartanA 3,{2},{1,2}));

    rowsZZSet := M -> set apply(entries M,
        v -> apply(v, x -> lift(x, ZZ)));
    mustError := f -> try (f(); false) else true;
    Ahyp := matrix{{2,-3},{-3,2}};
    expectedH3 := set {{1,0},{0,1},{1,1},{1,2},{2,1}};
    assert(rowsZZSet cartanPositiveRoots(Ahyp,3) === expectedH3);
    assert(all(entries cartanPositiveRoots(Ahyp,3), v -> sum v <= 3));
    assert(rowsZZSet negativeRoots(Ahyp,3)
        === set apply(toList expectedH3, v -> apply(v,x -> -x)));
    assert(numrows allRoots(Ahyp,3) === 2 * #expectedH3);
    assert(mustError(() -> cartanPositiveRoots Ahyp));
///

-- Test 66: affine null roots and exact imaginary multiplicities.
TEST ///
    rowsZZSet := M -> set apply(entries M,
        v -> apply(v, x -> lift(x, ZZ)));
    mustError := f -> try (f(); false) else true;
    affineSpecs := {
        {cartanAffineB 3, {1,1,2,2}},
        {cartanAffineC 3, {1,2,2,1}},
        {cartanAffineF4(), {1,2,3,4,2}},
        {cartanAffineG2(), {1,3,2}}
    };
    for spec in affineSpecs do (
        A := spec#0;
        delta := spec#1;
        snap := affineRootSnapshot(A, sum delta);
        assert(flatten entries snap#"delta" === delta);
        assert(A * transpose matrix {delta}
            == matrix mutableMatrix(ZZ, numrows A, 1));
        assert(rootNormSquared(A,delta) == 0);
        assert(gcd delta === 1 and all(delta,x -> x > 0));
        realSet := rowsZZSet snap#"real";
        imagSet := rowsZZSet snap#"imag";
        assert(#(realSet * imagSet) === 0);
        assert(member(delta,imagSet) and not member(delta,realSet));
        assert((snap#"mult")#delta === numrows A - 1);
    );

    -- Twisted affine multiplicities are periodic, not a constant r-1.
    Atw := transpose cartanAffineB 3;
    tw := affineRootSnapshot(Atw,20);
    dtw := flatten entries tw#"delta";
    assert(dtw === {1,1,2,1});
    assert(apply(toList(1..4), k ->
        (tw#"mult")#(apply(dtw,x -> k*x))) === {2,3,2,3});
    assert(mustError(() ->
        affineRootSnapshot(Atw,10,ImaginaryMultiplicity=>2)));
    assert(mustError(() ->
        affineRootSnapshot(cartanAffineG2(),5,ImaginaryMultiplicity=>99)));
///

-- Test 67: complete derived-Levi decompositions and representation domains.
TEST ///
    mustError := f -> try (f(); false) else true;
    tallySignature := T -> set apply(keys T, k -> {toList k, T#k});
    PD := (value "KacMoody")#"private dictionary";
    affineLeviBackend := value PD#"affineLeviHighestWeights";
    indefiniteLeviBackend := value PD#"indefiniteLeviHighestWeights";

    T0 := leviHighestWeights(cartanA 3,0,{1});
    assert(T0#(new Irrep from {{2}}) === 1);
    assert(T0#(new Irrep from {{0}}) === 1);
    T0Borel := leviHighestWeights(cartanA 3,0,{1,2});
    assert(T0Borel#(new Irrep from {}) === 2);
    T0Aff := leviHighestWeights(matrix{{2,-2},{-2,2}},0,{1});
    assert(T0Aff#(new Irrep from {{2}}) === 1);
    assert(T0Aff#(new Irrep from {{0}}) === 2);

    TD4Part := leviHighestWeights(cartanD 4,1,{1},
        WeightFormat=>"Partition");
    assert(tallySignature TD4Part === set {{{{1,1,0,0}},1}});

    Aaff1 := matrix{{2,-2},{-2,2}};
    assert(mustError(() -> affineLeviBackend(Aaff1,1,{1},2)));
    assert(tallySignature affineLeviBackend(Aaff1,1,{1},3)
        === tallySignature affineLeviBackend(Aaff1,1,{1},7));
    Ahyp := matrix{{2,-3},{-3,2}};
    assert(mustError(() -> indefiniteLeviBackend(Ahyp,1,{1},3)));
    assert(tallySignature indefiniteLeviBackend(Ahyp,1,{1},4)
        === tallySignature indefiniteLeviBackend(Ahyp,1,{1},8));

    assert(mustError(() -> weylDimension(cartanA 3,{-1,0})));
    assert(mustError(() -> weylDimension(cartanA 3,{1/2,0})));
    assert(mustError(() -> weightsOfRep(cartanA 3,{-1,0})));
    assert(mustError(() -> weightMultiplicity(cartanA 3,{1,0},{1/2,0})));
    assert(mustError(() -> fromEpsilon(simpleRootsA 3,{1,0,0})));
    assert(mustError(() -> specialCartanFromPartition(3,1,{0,1,0,0})));
    assert(instance(weylDimension(cartanA 3,{1,0}),ZZ));
    assert(instance(weightMultiplicity(cartanA 3,{1,1},{0,0}),ZZ));
    assert(instance(tensorCoefficient(cartanA 3,{1,0},{0,1},{0,0}),ZZ));

    A1xA1 := diagonalMatrix {2,2};
    Tred := tensorDecomposition(A1xA1,{1,0},{0,1});
    assert(tallySignature Tred === set {{{{1},{1}},1}});
///

-- Test 69: self-describing irreducibles and reductive-Levi central
-- characters in structured Kostant output.
TEST ///
    forgetCentralCharacters := T -> (
        accumulated := new MutableHashTable;
        for V in keys T do (
            assert(instance(V,LeviIrrep));
            U := V#"semisimple";
            if accumulated#?U then
                accumulated#U = accumulated#U + T#V
            else accumulated#U = T#V;
        );
        new Tally from for U in keys accumulated list U => accumulated#U
    );
    sameTally := (T,U) -> (
        #keys T == #keys U and
        all(keys T, k -> U#?k and T#k === U#k)
    );

    -- cartanA 3 is type A2.  The structured wrapper records its parent,
    -- component convention, and the fact that an arbitrary legacy Irrep
    -- carries no known central character.
    A2 := cartanA 3;
    wrapped := irrepData(A2,new Irrep from {{1,0}});
    assert(instance(wrapped,LeviIrrep));
    assert(wrapped#"components" === {{1,2}});
    assert(wrapped#"centralCharacter" === null);
    assert(wrapped#"centralCharacterStatus" === "unknown");
    assert(irrepDimension wrapped === 3);
    wrappedB2 := irrepData(cartanB 2,new Irrep from {{1,0}});
    assert(wrapped != wrappedB2);
    assert(#keys(new Tally from {wrapped=>1,wrappedB2=>1}) === 2);
    reducibleParent := diagonalMatrix {2,2};
    wrappedReducible := irrepData(reducibleParent,
        new Irrep from {{1},{2}});
    assert(wrappedReducible#"components" === {{1},{2}});
    assert(irrepDimension wrappedReducible === 6);

    ordinaryA2 := kostantLieAlgCohomology(A2,{1},{0,0});
    structuredA2 := kostantLieAlgCohomology(A2,{1},{0,0},
        IncludeCentralCharacter => true);
    assert(sort keys ordinaryA2 === sort keys structuredA2);
    for k in keys ordinaryA2 do
        assert(sameTally(ordinaryA2#k,
            forgetCentralCharacters(structuredA2#k)));
    keyA2Degree1 := first keys structuredA2#1;
    keyA2Degree2 := first keys structuredA2#2;
    assert(keyA2Degree1#"centralCharacter" === {-1_QQ});
    assert(keyA2Degree2#"centralCharacter" === {-2_QQ});
    assert(keyA2Degree1#"ambientWeight" === {-2,1});
    assert(decompositionDimension(structuredA2#1) === 2);
    assert(decompositionDimension(
        submatrix(A2,{1},{1}),structuredA2#1) === 2);

    -- cartanA 4 is type A3.  Removing the two end nodes leaves an A1
    -- derived Levi.  In degree one the two summands are both V(1) after
    -- forgetting the center, but their two-dimensional central characters
    -- are distinct, so the structured Tally must retain two separate keys.
    A3 := cartanA 4;
    removed := {1,3};
    ordinaryA3 := kostantLieAlgCohomology(A3,removed,{0,0,0});
    structuredA3 := kostantLieAlgCohomology(A3,removed,{0,0,0},
        IncludeCentralCharacter => true);
    for k in keys ordinaryA3 do
        assert(sameTally(ordinaryA3#k,
            forgetCentralCharacters(structuredA3#k)));
    degreeOneKeys := keys structuredA3#1;
    assert(#degreeOneKeys === 2);
    assert(all(degreeOneKeys,
        V -> V#"semisimple" === new Irrep from {{1}}));
    assert(set apply(degreeOneKeys,V -> V#"centralCharacter") ===
        set {{-1_QQ,0_QQ},{0_QQ,-1_QQ}});
    assert((ordinaryA3#1)#(new Irrep from {{1}}) === 2);
    assert(decompositionDimension(structuredA3#1) === 4);
    dimsA3 := cohomologyDimensions(A3,removed,structuredA3);
    assert(dimsA3#1 === 4);
    assert(dimsA3#2 === 5);
///

-- Test 70: public metadata records, cache controls, and structured
-- cohomology remain self-describing under coefficient-ring normalization.
TEST ///
    A := cartanA 4;
    C := cartanData(A ** QQ);
    assert(C#"rank" === 3 and C#"matrixRank" === 3);
    assert(C#"type" === "finite" and ring(C#"cartanMatrix") === ZZ);
    assert((first C#"components")#"nodes" === {1,2,3});
    assert((first C#"components")#"canonicalNodeOrder" === {1,2,3});

    P := parabolicData(A,{2,2});
    assert(P#"removedNodes" === {2} and P#"leviNodes" === {1,3});
    assert(instance(P#"levi",LeviData));
    assert((P#"levi")#"components" === {{1},{3}});
    assert(P#"gradingVector" === {0,1,0});
    assert(P#"nilradicalDimension" === 4);
    assert(parabolicDiagram(A,{2,2}) === parabolicDiagram(A,{2}));

    Aa := matrix{{2,-2},{-2,2}};
    clearRootSnapshotCache Aa;
    S4 := affineRootSnapshot(Aa,4);
    S2 := affineRootSnapshot(Aa,2);
    assert(instance(S4,RootSnapshot) and instance(S2,RootSnapshot));
    assert(S4#"completeThroughHeight" === 4);
    assert(S2#"completeThroughHeight" === 2);
    assert(all(entries rootSnapshotRoots S2,beta -> sum beta <= 2));
    assert((rootSnapshotCacheInfo Aa)#"affineHeight" === 4);
    clearRootSnapshotCache Aa;
    assert((rootSnapshotCacheInfo Aa)#"affineHeight" === null);

    clearRepresentationCache A;
    assert((representationCacheInfo A)#"weightSets" === 0);
    weightsOfRep(A,{1,0,0});
    assert((representationCacheInfo A)#"weightSets" === 1);
    clearRepresentationCache A;
    assert((representationCacheInfo A)#"weightSets" === 0);

    A2 := cartanA 3;
    H := kostantLieAlgCohomology(A2,{1},{0,0},
        IncludeCentralCharacter=>true);
    assert(instance(H,GradedDecomposition));
    assert(cohomologyDimensions H === cohomologyDimensions(
        A2,{1},H));
///

-- Test 71: Weyl and Kostant enumeration guards enforce exact output bounds.
TEST ///
    mustError := f -> try (f(); false) else true;
    A2 := cartanA 3;
    assert(mustError(() -> weylGroupElements(A2,MaxElements=>5)));
    assert(#weylGroupElements(A2,MaxElements=>6) === 6);
    assert(mustError(() -> kostantLieAlgCohomology(
        A2,{1},{0,0},MaxElements=>2)));
    assert(sort keys kostantLieAlgCohomology(
        A2,{1},{0,0},MaxElements=>3) === {0,1,2});


///

-- Test 72: companion-package bridges accept integral QQ matrices,
-- arbitrary node permutations, and reducible finite root systems.
TEST ///
    WG := try needsPackage "WeylGroups" else null;
    if WG === null then print "SKIP: WeylGroups typed-bridge tests (package could not be loaded)";
    if WG =!= null then (
        WGdict := (value "WeylGroups")#"private dictionary";
        RootSystemType := value WGdict#"RootSystem";
        numberPositive := value WGdict#"numberOfPositiveRoots";
        Bwg := cartanB 3;
        Pwg := submatrix(Bwg,{2,0,1},{2,0,1});
        Rwg := matrix(QQ,{{2,0,0},{0,2,-1},{0,-2,2}});
        for spec in {{Bwg ** QQ,9},{Pwg,9},{Rwg,5}} do (
            X := toRootSystem(spec#0);
            assert(instance(X,RootSystemType));
            assert(numberPositive X === spec#1);
        );
    );

    LAR := try needsPackage "LieAlgebraRepresentations" else null;
    if LAR === null then print "SKIP: LieAlgebraRepresentations typed-bridge tests (package could not be loaded)";
    if LAR =!= null then (
        LAdict := (value "LieAlgebraRepresentations")#"private dictionary";
        LieAlgebraType := value LAdict#"LieAlgebra";
        Blar := cartanB 3;
        Plar := submatrix(Blar,{2,0,1},{2,0,1});
        Rlar := matrix(QQ,{{2,0,0},{0,2,-1},{0,-2,2}});
        for C in {Blar ** QQ,Plar,Rlar} do
            assert(instance(toLieAlgebra C,LieAlgebraType));
    );
///

-- Test 73: unified root and parabolic entry points infer Cartan type,
-- choose the corresponding backend, and retain certified snapshot semantics.
TEST ///
    mustError := f -> try (f(); false) else true;
    sameTallyLocal := (T,U) ->
        (#keys T == #keys U and all(keys T,k -> U#?k and T#k == U#k));
    PD := (value "KacMoody")#"private dictionary";
    finiteLeviBackend := value PD#"cartanLeviHighestWeights";
    affineLeviBackend := value PD#"affineLeviHighestWeights";
    indefiniteLeviBackend := value PD#"indefiniteLeviHighestWeights";
    exportedNames := apply((value "KacMoody")#"exported symbols",toString);
    for publicName in {
        "leviHighestWeights", "fundamentalWeights", "cartanPositiveRoots",
        "partitionToFundamental", "fundamentalToPartition"
    } do assert(member(publicName,exportedNames));
    privateOnlyNames := {
        "cartanLeviHighestWeights", "affineLeviHighestWeights",
        "indefiniteLeviHighestWeights", "partitionToFundamentalA",
        "fundamentalToPartitionA", "fundamentalToPartitionB",
        "fundamentalToPartitionC", "fundamentalToPartitionD",
        "vermaCharacter", "integrableCharacter"
    };
    assert(all(privateOnlyNames,n -> not member(n,exportedNames)));
    assert(all(privateOnlyNames,n -> PD#?n));
    retiredPublicNames := {
        "toLegacyList",
        "toLegacyCartanMatrix",
        "datumFromLegacyCartan",
        "rootFromLegacy",
        "corootFromLegacy",
        "weightFromLegacy",
        "coweightFromLegacy",
        "affineCentralCoefficients",
        "affineFullWeight",
        "weylWord",
        "vermaCharacter",
        "integrableCharacter",
        "affineStructuralCapabilities"
    };
    assert(all(retiredPublicNames,n -> not member(n,exportedNames)));
    assert(fundamentalToPartition(cartanB 3,{-1,0,0}) === {-1,0,0});
    assert(mustError(() ->
        fundamentalToPartition(cartanC 3,{1/2,0,0})));
    assert(mustError(() ->
        partitionToFundamental(cartanD 4,{0,1,0,0})));

    Af := cartanA 4;
    Sf := rootSnapshot Af;
    assert(instance(Sf,RootSnapshot));
    assert(Sf#"type" === "finite");
    assert(Sf#"algorithm" === "FiniteEnumeration");
    assert(Sf#"globallyComplete" and Sf#"heightBound" === 3);
    assert(numrows(Sf#"real") === 6 and numrows(Sf#"imag") === 0);
    assert(rootMultiplicity(Sf,{1,1,1}) === 1);
    -- A globally complete finite snapshot certifies absence at every height.
    assert(rootMultiplicity(Sf,{2,2,2}) === 0);

    Sf2 := rootSnapshot(Af ** QQ,2);
    assert(ring(Sf2#"cartanMatrix") === ZZ);
    assert(not Sf2#"globallyComplete");
    assert(Sf2#"completeThroughHeight" === 2);
    assert(all(entries(Sf2#"real"),beta -> sum beta <= 2));
    assert(mustError(() -> rootMultiplicity(Sf2,{1,1,1})));
    Sf3 := rootSnapshot(Af,3);
    assert(Sf3#"globallyComplete" and Sf3#"completeThroughHeight" === 3);

    Aa := matrix{{2,-2},{-2,2}};
    Sa := rootSnapshot(Aa,5);
    Ta := affineRootSnapshot(Aa,5);
    assert(Sa#"type" === "affine" and Sa#"algorithm" === "AffineExact");
    assert(Sa#"mult" === Ta#"mult");
    assert(mustError(() -> rootSnapshot Aa));
    assert(mustError(() -> rootSnapshot(Aa,0)));

    Ai := matrix{{2,-3},{-3,2}};
    Si := rootSnapshot(Ai,5);
    Ti := indefiniteRootSnapshot(Ai,5);
    assert(Si#"type" === "indefinite" and Si#"mult" === Ti#"mult");

    -- A reducible affine GCM is handled componentwise even though the
    -- specialized affine API intentionally requires indecomposability.
    Ad := matrix{{2,-2,0},{-2,2,0},{0,0,2}};
    Sd := rootSnapshot(Ad,4);
    assert(Sd#"type" === "affine" and Sd#"rank" === 3);
    assert(instance(leviHighestWeights(Ad,1,{1,3}),Tally));

    -- Aggregate type detection is componentwise: several affine blocks
    -- remain positive semidefinite and must not be labeled indefinite.
    Aaa := matrix{{2,-2,0,0},{-2,2,0,0},
                  {0,0,2,-2},{0,0,-2,2}};
    assert(cartanType Aaa === "affine");
    Saa := rootSnapshot(Aaa,3);
    assert(Saa#"type" === "affine" and Saa#"rank" === 4);
    assert(instance(leviHighestWeights(Aaa,1,{1,3}),Tally));

    assert(sameTallyLocal(
        leviHighestWeights(Af,1,{1}),
        finiteLeviBackend(Af,1,{1})));
    assert(sameTallyLocal(
        leviHighestWeights(Af,1,set{1}),
        leviHighestWeights(Af,1,{1})));
    assert(sameTallyLocal(
        leviHighestWeights(Af,0,{1}),
        finiteLeviBackend(Af,0,{1})));
    assert(sameTallyLocal(
        leviHighestWeights(Af,1,{1},WeightFormat=>"Partition"),
        finiteLeviBackend(Af,1,{1},WeightFormat=>"Partition")));
    assert(sameTallyLocal(
        leviHighestWeights(Aa,1,{1}),
        affineLeviBackend(Aa,1,{1},6)));
    assert(sameTallyLocal(
        leviHighestWeights(Ai,1,{1}),
        indefiniteLeviBackend(Ai,1,{1},6)));
    assert(sameTallyLocal(
        leviHighestWeights(Ai,2,{1}),
        indefiniteLeviBackend(Ai,2,{1},12)));
    assert(instance(
        leviHighestWeights(Aa,1,{1},WeightFormat=>"Partition"),Tally));
    Ans := matrix{{2,-1,-1},{-2,2,-1},{-1,-2,2}};
    assert(cartanType Ans === "nonsymmetrizable");
    assert(mustError(() -> rootSnapshot(Ans,3)));
    assert(mustError(() -> leviHighestWeights(Ans,1,{1})));
    assert(mustError(() -> leviHighestWeights(Af,-1,{1})));
    assert(mustError(() -> leviHighestWeights(Af,1,{0})));
    assert(mustError(() ->
        leviHighestWeights(Af,1,{1},WeightFormat=>"Bad")));
///

-- Test 74: Kostant cohomology for symmetrizable Kac--Moody algebras.
-- Infinite parabolic quotients require an explicit degree cap; finite
-- quotients are detected componentwise and may still be returned in full.
TEST ///
    mustError := f -> try (f(); false) else true;
    sameTallyLocal := (T,U) ->
        (#keys T == #keys U and all(keys T,k -> U#?k and T#k == U#k));
    sameGradedLocal := (H,K) ->
        (#keys H == #keys K and all(keys H,d ->
            K#?d and sameTallyLocal(H#d,K#d)));
    isOnlySummand := (H,k,V,m) ->
        (H#?k and #keys(H#k) == 1 and H#k#?V and H#k#V == m);

    -- Existing finite-type calls retain their complete output.  MaxDegree is
    -- a genuine cap when explicitly supplied, even for a finite quotient.
    Hfin := kostantLieAlgCohomology(cartanA 4,{1},{0,0,0});
    HfinAgain := kostantLieAlgCohomology(cartanA 4,{1},{0,0,0},
        MaxDegree=>10);
    assert(sameGradedLocal(Hfin,HfinAgain));
    Hfin0 := kostantLieAlgCohomology(cartanA 4,{1},{0,0,0},
        MaxDegree=>0);
    assert(sort keys Hfin0 === {0});

    -- A_1^(1) / W_{A_1}: one summand in every degree, with finite-Levi
    -- Dynkin label 2k for trivial ambient coefficients.
    Aaff := matrix{{2,-2},{-2,2}};
    assert(mustError(() ->
        kostantLieAlgCohomology(Aaff,{1},{0,0})));
    Haff := kostantLieAlgCohomology(Aaff,{1},{0,0},MaxDegree=>4);
    for k from 0 to 4 do assert(isOnlySummand(
        Haff,k,new Irrep from {{2*k}},1));

    -- The affine Borel has two Weyl elements in every positive length.
    HaffBorel := kostantLieAlgCohomology(
        Aaff,{1,2},{0,0},MaxDegree=>3,MaxElements=>7);
    assert(isOnlySummand(HaffBorel,0,new Irrep from {},1));
    for k from 1 to 3 do assert(isOnlySummand(
        HaffBorel,k,new Irrep from {},2));
    assert(mustError(() -> kostantLieAlgCohomology(
        Aaff,{1,2},{0,0},MaxDegree=>3,MaxElements=>6)));

    -- Hyperbolic rank two gives the expected rapidly growing A_1 labels.
    Ahyp := matrix{{2,-3},{-3,2}};
    Hhyp := kostantLieAlgCohomology(Ahyp,{1},{0,0},MaxDegree=>3);
    expectedHyp := {0,3,10,28};
    for k from 0 to 3 do assert(isOnlySummand(
        Hhyp,k,new Irrep from {{expectedHyp#k}},1));

    -- The theorem also permits an infinite-type Levi.  Dynkin output records
    -- its integrable highest weights without asking for finite dimensions.
    AinfLevi := matrix{{2,-2,0},{-2,2,-1},{0,-1,2}};
    HinfLevi := kostantLieAlgCohomology(
        AinfLevi,{3},{0,0,0},MaxDegree=>2);
    assert(isOnlySummand(HinfLevi,0,new Irrep from {{0,0}},1));
    assert(isOnlySummand(HinfLevi,1,new Irrep from {{0,1}},1));
    assert(isOnlySummand(HinfLevi,2,new Irrep from {{2,0}},1));

    -- An affine component wholly contained in the Levi contributes quotient
    -- order one.  Only the finite A_1 component is quotiented, so this mixed
    -- infinite ambient matrix has a complete two-element quotient.
    Amixed := matrix{{2,-2,0},{-2,2,0},{0,0,2}};
    Hmixed := kostantLieAlgCohomology(Amixed,{3},{0,0,0});
    assert(sort keys Hmixed === {0,1});
    assert(isOnlySummand(Hmixed,0,new Irrep from {{0,0}},1));
    assert(isOnlySummand(Hmixed,1,new Irrep from {{0,0}},1));
    assert(mustError(() -> kostantLieAlgCohomology(
        Amixed,{3},{0,0,0},MaxElements=>1)));

    -- Full central characters are available for nonsingular indefinite A.
    Hcentral := kostantLieAlgCohomology(Ahyp,{1},{0,0},
        MaxDegree=>1,IncludeCentralCharacter=>true);
    centralKey := first keys Hcentral#1;
    assert(instance(centralKey,LeviIrrep));
    assert(centralKey#"centralCharacterStatus" === "known");
    -- Singular A cannot recover removed-node central data from Dynkin labels.
    assert(mustError(() -> kostantLieAlgCohomology(Aaff,{1},{0,0},
        MaxDegree=>1,IncludeCentralCharacter=>true)));
    -- Even with no removed nodes, Dynkin labels for a singular realization
    -- do not specify every full-Cartan direction, so structured full central
    -- characters remain unavailable without additional realization data.
    assert(mustError(() -> kostantLieAlgCohomology(Aaff,{},{0,0},
        IncludeCentralCharacter=>true)));

    Ans := matrix{{2,-1,-1},{-2,2,-1},{-1,-2,2}};
    assert(mustError(() -> kostantLieAlgCohomology(
        Ans,{1},{0,0,0},MaxDegree=>1)));
    assert(mustError(() -> kostantLieAlgCohomology(
        Aaff,{1},{0,0},MaxDegree=>-1)));
    assert(mustError(() -> kostantLieAlgCohomology(
        Aaff,{1},{0,0},MaxDegree=>"bad")));
    assert(mustError(() -> kostantLieAlgCohomology(
        Aaff,{1},{0,0},MaxDegree=>1,WeightFormat=>"Partition")));
///

-- Test 75: the sparse-engine simple-reflection updates in the parabolic BFS
-- agree exactly with independently filtering the full Weyl
-- group, for every standard parabolic of A_3 and nonsymmetric B_2.
TEST ///
    matrixKey := M -> flatten entries M;
    positiveColumn := (M,j) -> (
        v := for i from 0 to numrows M-1 list M_(i,j);
        all(v,x -> x >= 0) and any(v,x -> x > 0)
    );

    for A in {cartanA 4,cartanB 2} do (
        W := weylGroupElements(A,MaxElements=>24);
        maxLength := numrows cartanPositiveRoots A;
        for I in subsets toList(1..numrows A) do (
            I0 := apply(I,i -> i-1);
            expected := select(W,e ->
                all(I0,j -> positiveColumn(e#"matrix",j)));
            for k from 0 to maxLength do (
                expectedAtK := set apply(
                    select(expected,e -> e#"length" == k),
                    e -> matrixKey(e#"matrix"));
                actualAtK := set apply(
                    parabolicCosetReps(A,k,I,MaxElements=>24),
                    w -> matrixKey weylElementMatrix(A,w));
                assert(actualAtK === expectedAtK);
            );
        );
    );
///

-- Test 76: every node-set API accepts List and Set syntax consistently,
-- and validation still runs on empty/out-of-range output strata.
TEST ///
    mustError := f -> try (f(); false) else true;
    sameTallyLocal := (T,U) ->
        (#keys T == #keys U and all(keys T,k -> U#?k and T#k == U#k));
    sameGradedLocal := (H,K) ->
        (#keys H == #keys K and all(keys H,d ->
            K#?d and sameTallyLocal(H#d,K#d)));

    A := cartanA 4;
    removedList := {1,3};
    removedSet := set removedList;
    leviList := {2,3};
    leviSet := set leviList;
    assert(sameTallyLocal(
        leviHighestWeights(A,1,removedList),
        leviHighestWeights(A,1,removedSet)));
    assert(parabolicCosetReps(A,2,leviList) ===
        parabolicCosetReps(A,2,leviSet));
    assert(parabolicDotImages(A,2,leviList,{1,0,0}) ===
        parabolicDotImages(A,2,leviSet,{1,0,0}));
    assert(parabolicDiagram(A,removedList) ===
        parabolicDiagram(A,removedSet));
    PL := parabolicData(A,removedList);
    PS := parabolicData(A,removedSet);
    assert(PL#"removedNodes" === PS#"removedNodes");
    assert(PL#"leviNodes" === PS#"leviNodes");
    assert(PL#"gradingVector" === PS#"gradingVector");
    assert(PL#"nilradicalRoots" == PS#"nilradicalRoots");

    HL := kostantLieAlgCohomology(A,removedList,{1,0,1});
    HS := kostantLieAlgCohomology(A,removedSet,{1,0,1});
    assert(sameGradedLocal(HL,HS));
    assert(cohomologyDimensions(A,removedList,HL) ===
        cohomologyDimensions(A,removedSet,HS));

    -- These calls have empty output strata.  Invalid options and weights
    -- must nevertheless be rejected rather than bypassed by an early return.
    assert(mustError(() ->
        weylGroupElements(A,100,MaxElements=>0)));
    assert(mustError(() ->
        parabolicDotImages(A,100,{},{})));
    assert(mustError(() ->
        parabolicDotImages(A,100,{}, {0,0,0},MaxElements=>false)));
///

-- Test 77: exact nonsimply-laced braid and tensor-product fixtures,
-- including a multiplicity-two summand and the exceptional F_4 example.
TEST ///
    sameTallyLocal := (T,U) ->
        (#keys T == #keys U and all(keys T,k -> U#?k and T#k == U#k));
    labelTally := T -> new Tally from for V in keys T list
        ((toList V)#0) => T#V;

    B2 := cartanB 2;
    G2 := cartanG2();
    assert(weylElementMatrix(B2,{1,2,1,2}) ==
        weylElementMatrix(B2,{2,1,2,1}));
    assert(weylElementMatrix(B2,{1,2,1}) !=
        weylElementMatrix(B2,{2,1,2}));
    assert(weylElementMatrix(G2,{1,2,1,2,1,2}) ==
        weylElementMatrix(G2,{2,1,2,1,2,1}));
    assert(weylElementMatrix(G2,{1,2,1,2,1}) !=
        weylElementMatrix(G2,{2,1,2,1,2}));
    assert(weylElementMatrix(diagonalMatrix{2,2},{1,2}) ==
        weylElementMatrix(diagonalMatrix{2,2},{2,1}));

    -- 8 tensor 8 for A_2 contains two copies of the adjoint.
    assert(sameTallyLocal(
        labelTally tensorDecomposition(cartanA 3,{1,1},{1,1}),
        tally{{0,0},{1,1},{1,1},{3,0},{0,3},{2,2}}));
    -- 7 tensor 7 = 1 + 7 + 14 + 27 for G_2.
    assert(sameTallyLocal(
        labelTally tensorDecomposition(G2,{1,0},{1,0}),
        tally{{0,0},{1,0},{0,1},{2,0}}));
    -- 26 tensor 26 = 1 + 26 + 52 + 273 + 324 for F_4.
    F4 := cartanF4();
    assert(sameTallyLocal(
        labelTally tensorDecomposition(F4,{0,0,0,1},{0,0,0,1}),
        tally{{0,0,0,0},{0,0,0,1},{1,0,0,0},
              {0,0,1,0},{0,0,0,2}}));
    char26 := weightMultiplicityTable(F4,{0,0,0,1});
    assert(#keys char26 === 25);
    assert(char26#{0,0,0,0} === 2);
    assert(sum values char26 === 26);

    -- Native representation code is equivariant under simultaneous node
    -- relabeling, not only under canonical constructor order.
    A := cartanB 3;
    p := {2,0,1};
    AP := submatrix(A,p,p);
    lambda := {1,1,0};
    mu := {1,0,0};
    lambdaP := apply(p,i -> lambda#i);
    muP := apply(p,i -> mu#i);
    assert(weylDimension(AP,lambdaP) === weylDimension(A,lambda));
    assert(set weightsOfRep(AP,lambdaP) ===
        set apply(weightsOfRep(A,lambda),w -> apply(p,i -> w#i)));
    T := tensorDecomposition(A,lambda,mu);
    mapped := new Tally from for V in keys T list (
        labels := (toList V)#0;
        (new Irrep from {apply(p,i -> labels#i)}) => T#V
    );
    assert(sameTallyLocal(
        tensorDecomposition(AP,lambdaP,muP),mapped));
///

-- Test 78: root-degree slices and derived-Levi decompositions are checked
-- for every standard parabolic of B_3 and G_2, including the Borel and
-- empty-parabolic boundaries.
TEST ///
    for A in {cartanB 3,cartanG2()} do (
        roots := entries cartanPositiveRoots A;
        r := numrows A;
        for removed in subsets toList(1..r) do (
            removed0 := apply(removed,i -> i-1);
            maxDegree := max apply(roots,b ->
                sum apply(removed0,j -> b#j));
            for d from 0 to maxDegree+1 do (
                expected := set select(roots,b ->
                    sum apply(removed0,j -> b#j) == d);
                actual := set entries cartanPositiveRootsDegree(A,d,removed);
                assert(actual === expected);
            );
            if #removed > 0 then (
                L := leviSubalgebra(A,removed);
                AL := L#"cartanMatrix";
                for d from 1 to maxDegree do (
                    expectedDimension := #(select(roots,b ->
                        sum apply(removed0,j -> b#j) == d));
                    T := leviHighestWeights(A,d,removed);
                    actualDimension := if numrows AL == 0 then sum values T
                        else decompositionDimension(AL,T);
                    assert(actualDimension === expectedDimension);
                );
            );
        );
    );
///

-- Test 79: every affine constructor and its transpose survives exact
-- dispatch.  The transposed C_3^(1) case is a regression for a finite but
-- disconnected coefficient-one deletion in affine-type recognition.
TEST ///
    sameTableLocal := (T,U) ->
        (#keys T == #keys U and all(keys T,k -> U#?k and T#k == U#k));
    affineFamilies := {
        cartanAffineA 1,cartanAffineA 3,cartanAffineB 3,
        cartanAffineC 3,cartanAffineD 4,cartanAffineE 6,
        cartanAffineF4(),cartanAffineG2()
    };
    for A in affineFamilies do for At in {A,transpose A} do (
        assert(cartanType At === "affine");
        S := affineRootSnapshot(At,8);
        U := rootSnapshot(At,8);
        assert(S#"type" === "affine");
        assert(S#"completeThroughHeight" === 8);
        assert(sameTableLocal(S#"mult",U#"mult"));
    );

    At := transpose cartanAffineC 3;
    exact := affineRootSnapshot(At,12);
    peterson := indefiniteRootSnapshot(At,12,Algorithm=>"Peterson");
    assert(sameTableLocal(exact#"mult",peterson#"mult"));
    delta := flatten entries exact#"delta";
    hdelta := sum delta;
    for n in {1,2} do (
        before := affineRootSnapshot(At,n*hdelta-1);
        atBoundary := affineRootSnapshot(At,n*hdelta);
        ndelta := apply(delta,x -> n*x);
        assert(not (before#"mult")#?ndelta);
        assert((atBoundary#"mult")#?ndelta);
    );
///

-- Test 80: independent Peterson-identity, schema, norm, Weyl-invariance,
-- cache-alias, and cold height-stability checks for nonfinite snapshots.
TEST ///
    gcdVectorLocal := b -> (
        g := 0;
        for x in b do g = gcd(g,x);
        abs g
    );
    cFromMultiplicitiesLocal := (mult,b) -> (
        g := gcdVectorLocal b;
        c := 0_QQ;
        for n from 1 to g do if g % n == 0 then (
            q := apply(b,x -> x // n);
            if mult#?q then c = c + mult#q/n;
        );
        c
    );
    boxVectorsLocal := bounds -> (
        result := {{}};
        for bound in bounds do (
            next := {};
            for prefix in result do for x from 0 to bound do
                next = append(next,prefix|{x});
            result = next;
        );
        result
    );
    compositionsLocal := (h,r) -> (
        if r == 1 then return {{h}};
        answer := {};
        for x from 0 to h do
            for tail in compositionsLocal(h-x,r-1) do
                answer = append(answer,{x}|tail);
        answer
    );
    checkPetersonIdentityLocal := (A,S) -> (
        mult := S#"mult";
        H := S#"completeThroughHeight";
        r := numrows A;
        D := symmetrizer A;
        d := for i from 0 to r-1 list D_(i,i);
        pair := (b,c) -> sum for i from 0 to r-1 list
            sum for j from 0 to r-1 list d#i*A_(i,j)*b#i*c#j;
        rhoPair := b -> sum for i from 0 to r-1 list d#i*b#i;
        for h from 2 to H do for beta in compositionsLocal(h,r) do (
            cbeta := cFromMultiplicitiesLocal(mult,beta);
            lhs := (pair(beta,beta)-2*rhoPair beta)*cbeta;
            rhs := 0_QQ;
            for gamma in boxVectorsLocal beta do
                if any(gamma,x -> x > 0) and gamma =!= beta then (
                    delta := for i from 0 to r-1 list beta#i-gamma#i;
                    cgamma := cFromMultiplicitiesLocal(mult,gamma);
                    cdelta := cFromMultiplicitiesLocal(mult,delta);
                    if cgamma != 0 and cdelta != 0 then
                        rhs = rhs + pair(gamma,delta)*cgamma*cdelta;
                );
            assert(lhs == rhs);
        );
    );
    checkSnapshotLocal := (A,S) -> (
        H := S#"completeThroughHeight";
        realRows := entries S#"real";
        imagRows := entries S#"imag";
        rows := realRows|imagRows;
        mult := S#"mult";
        assert(#rows === #set rows);
        assert(set rows === set keys mult);
        assert(all(rows,b ->
            #b == numrows A and all(b,x -> instance(x,ZZ) and x >= 0)
            and sum b >= 1 and sum b <= H));
        assert(all(realRows,b ->
            rootNormSquared(A,b) > 0 and mult#b === 1));
        assert(all(imagRows,b -> rootNormSquared(A,b) <= 0));
        for b in rows do for i from 1 to numrows A do (
            column := reflectionMatrix(A,i)*transpose matrix{b};
            reflected := for j from 0 to numrows A-1 list
                lift(column_(j,0),ZZ);
            if all(reflected,x -> x >= 0) and
               any(reflected,x -> x > 0) and sum reflected <= H then (
                assert(mult#?reflected);
                assert(mult#reflected === mult#b);
            );
        );
    );
    sameTableLocal := (T,U) ->
        (#keys T == #keys U and all(keys T,k -> U#?k and T#k == U#k));
    restrictTableLocal := (S,H) -> new HashTable from
        for b in keys(S#"mult") list if sum b <= H then
            b => (S#"mult")#b;

    Ah := cartanRank2(2,3);
    Sh := indefiniteRootSnapshot(Ah,9,Algorithm=>"BCL");
    checkPetersonIdentityLocal(Ah,Sh);
    checkSnapshotLocal(Ah,Sh);
    Aw := matrix{{2,-2,-1},{-1,2,-1},{-1,-2,2}};
    Sw := indefiniteRootSnapshot(Aw,7,Algorithm=>"BCL");
    checkPetersonIdentityLocal(Aw,Sw);
    checkSnapshotLocal(Aw,Sw);

    -- A connected singular indefinite GCM (determinant zero) exercises a
    -- boundary not represented by the named affine families.
    Asing := matrix{
        {2,0,0,0,0,-1,-1},{0,2,0,0,-1,0,-1},
        {0,0,2,-1,0,-1,0},{0,0,-1,2,0,-1,0},
        {0,-1,0,0,2,0,-1},{-1,0,-1,-1,0,2,0},
        {-1,-1,0,0,-1,0,2}};
    assert(cartanType Asing === "indefinite");
    assert(det Asing == 0 and rank(Asing ** QQ) === 6);
    Bs := indefiniteRootSnapshot(Asing,5,Algorithm=>"BCL");
    Ps := indefiniteRootSnapshot(Asing,5,Algorithm=>"Peterson");
    assert(#keys(Bs#"mult") === 52);
    assert(sameTableLocal(Bs#"mult",Ps#"mult"));
    checkSnapshotLocal(Asing,Bs);

    -- Use distinct equal matrices so cache truncation cannot hide a
    -- height-dependent result.
    A5 := matrix entries Ah;
    A9 := matrix entries Ah;
    for alg in {"BCL","Peterson"} do (
        small := indefiniteRootSnapshot(A5,5,Algorithm=>alg);
        large := indefiniteRootSnapshot(A9,9,Algorithm=>alg);
        assert(sameTableLocal(small#"mult",restrictTableLocal(large,5)));
    );

    clearRootSnapshotCache Ah;
    B8 := indefiniteRootSnapshot(Ah,8,Algorithm=>"BCL");
    B3 := indefiniteRootSnapshot(Ah,3,Algorithm=>"GradedAscent");
    assert(B3#"algorithm" === "BackusConnickLin");
    assert(sameTableLocal(B3#"mult",restrictTableLocal(B8,3)));
    P7 := indefiniteRootSnapshot(Ah,7,Algorithm=>"Sparse");
    P2 := indefiniteRootSnapshot(Ah,2,Algorithm=>"Peterson");
    assert(P2#"algorithm" === "SparsePeterson");
    assert(sameTableLocal(P2#"mult",restrictTableLocal(P7,2)));
///

-- Test 81: generalized Kostant output is compared with a small exhaustive
-- reference model that enumerates all words, including nonreduced ones,
-- and implements the dot action independently.
TEST ///
    sameTallyLocal := (T,U) ->
        (#keys T == #keys U and all(keys T,k -> U#?k and T#k == U#k));
    sameGradedLocal := (H,K) ->
        (#keys H == #keys K and all(keys H,d ->
            K#?d and sameTallyLocal(H#d,K#d)));
    matrixKeyLocal := M -> flatten entries M;
    positiveColumnLocal := (M,j) -> (
        v := for i from 0 to numrows M-1 list M_(i,j);
        all(v,x -> x >= 0) and any(v,x -> x > 0)
    );
    wordsOfLengthLocal := (n,k) -> (
        if k == 0 then return {{}};
        previous := wordsOfLengthLocal(n,k-1);
        answer := {};
        for w in previous do for i from 1 to n do
            answer = append(answer,append(w,i));
        answer
    );
    bruteElementsThroughLocal := (A,d) -> (
        firstAt := new MutableHashTable;
        answer := new MutableList;
        for k from 0 to d do for w in wordsOfLengthLocal(numrows A,k) do (
            M := id_((ring A)^(numrows A));
            for i in w do M = M*reflectionMatrix(A,i);
            key := matrixKeyLocal M;
            if not firstAt#?key then (
                firstAt#key = k;
                answer#(#answer) = {k,w,M};
            );
        );
        toList answer
    );
    bruteParabolicAtLocal := (A,k,I) -> (
        I0 := apply(sort toList set I,i -> i-1);
        select(bruteElementsThroughLocal(A,k),e ->
            e#0 == k and all(I0,j -> positiveColumnLocal(e#2,j)))
    );
    referenceDotLocal := (A,w,lambda) -> (
        current := lambda;
        for t from 0 to #w-1 do (
            j := w#(#w-1-t)-1;
            old := current;
            current = for i from 0 to numrows A-1 list
                old#i-(old#j+1)*A_(i,j);
        );
        current
    );
    bruteKostantThroughLocal := (A,removed,lambda,d) -> (
        L := leviSubalgebra(A,removed);
        leviNodes := L#"nodes";
        levi0 := L#"nodes0";
        comps := L#"components0";
        pieces := new MutableHashTable;
        for k from 0 to d do (
            T := new MutableHashTable;
            for e in bruteParabolicAtLocal(A,k,leviNodes) do (
                mu := referenceDotLocal(A,reverse e#1,lambda);
                leviMu := for i in levi0 list mu#i;
                V := new Irrep from for comp in comps list
                    for i in comp list leviMu#i;
                if T#?V then T#V = T#V+1 else T#V = 1;
            );
            pieces#k = new Tally from for V in keys T list V => T#V;
        );
        new GradedDecomposition from for k in sort keys pieces list
            k => pieces#k
    );

    for spec in {
        {cartanB 3,{2},{1,2,0},4},
        {matrix{{2,-2},{-2,2}},{1},{2,1},6},
        {matrix{{2,-1,0},{-4,2,-1},{0,-1,2}},
            {1,3},{1,2,0},4},
        {matrix{{2,-2,0},{-2,2,-1},{0,-1,2}},
            {3},{0,1,2},4}
    } do (
        A := spec#0;
        removed := spec#1;
        lambda := spec#2;
        d := spec#3;
        assert(sameGradedLocal(
            kostantLieAlgCohomology(A,removed,lambda,
                MaxDegree=>d,MaxElements=>null),
            bruteKostantThroughLocal(A,removed,lambda,d)));
    );

    -- Two independent affine components give k+1 quotient elements in
    -- degree k; the cumulative safety threshold is triangular.
    A2aff := matrix{{2,-2,0,0},{-2,2,0,0},
                    {0,0,2,-2},{0,0,-2,2}};
    H2aff := kostantLieAlgCohomology(A2aff,{1,3},{0,0,0,0},
        MaxDegree=>5,MaxElements=>21);
    assert(apply(toList(0..5),k -> sum values H2aff#k) ===
        {1,2,3,4,5,6});
    assert(try (kostantLieAlgCohomology(A2aff,{1,3},{0,0,0,0},
        MaxDegree=>5,MaxElements=>20);false) else true);

    -- For nonsingular indefinite A, verify the stored full-Levi central
    -- character directly from the stated (A^T)^(-1) basis.
    Ac := matrix{{2,-1,0},{-4,2,-1},{0,-1,2}};
    Hc := kostantLieAlgCohomology(Ac,{3,1,3},{2,0,1},
        MaxDegree=>3,IncludeCentralCharacter=>true);
    invAT := inverse transpose(Ac ** QQ);
    for d in keys Hc do for V in keys Hc#d do (
        mu := V#"ambientWeight";
        expectedCentral := for j in {1,3} list sum(
            for i from 0 to 2 list mu#i*invAT_(i,j-1));
        assert(V#"removedNodes" === {1,3});
        assert(V#"centralCharacter" === expectedCentral);
    );
///

-- Test 82: Coxeter exponents cover every Cartan-product case, agree for
-- dual Cartan matrices, and enforce the public validation conventions.
TEST ///
    mustError := f -> try (f(); false) else true;

    assert(coxeterMatrix cartanA 2 === {{1}});
    assert(coxeterMatrix cartanA 4 ===
        {{1,3,2},{3,1,3},{2,3,1}});
    assert(coxeterMatrix cartanAffineA 3 ===
        {{1,3,2,3},{3,1,3,2},{2,3,1,3},{3,2,3,1}});
    assert(coxeterMatrix matrix{{2,0},{0,2}} === {{1,2},{2,1}});
    assert(coxeterMatrix cartanRank2(1,1) === {{1,3},{3,1}});
    assert(coxeterMatrix cartanRank2(1,2) === {{1,4},{4,1}});
    assert(coxeterMatrix cartanRank2(1,3) === {{1,6},{6,1}});
    assert(coxeterMatrix cartanRank2(2,2) ===
        {{1,infinity},{infinity,1}});
    assert(coxeterMatrix cartanRank2(2,3) ===
        {{1,infinity},{infinity,1}});

    A := cartanB 3;
    assert(coxeterMatrix A === coxeterMatrix transpose A);
    assert(coxeterMatrix(matrix(QQ, entries A)) === coxeterMatrix A);
    for i from 1 to numrows A do for j from 1 to numrows A do
        assert(coxeterMatrix(A,i,j) === (coxeterMatrix A)#(i-1)#(j-1));
    assert(coxeterMatrix(cartanRank2(2,2),1,2) === infinity);

    assert(mustError(() -> coxeterMatrix {{2,-1},{-1,2}}));
    assert(mustError(() -> coxeterMatrix matrix{{2,-1},{0,2}}));
    assert(mustError(() -> coxeterMatrix(A,0,1)));
    assert(mustError(() -> coxeterMatrix(A,1,4)));
    assert(mustError(() -> coxeterMatrix(A,1,1_QQ)));
///

-- Test 83: snapshot displays show actual numerical roots and multiplicities;
-- a display preview is distinct from the computed height range.
TEST ///
    Ah := matrix{{2,-3},{-3,2}};
    S4 := rootSnapshot(Ah,4);
    expectedReal := {{0,1},{1,0},{1,3},{3,1}};
    expectedImag := {{1,1},{1,2},{2,1},{2,2}};
    assert(sort entries rootSnapshotRoots(S4,Kind=>"Real") === expectedReal);
    assert(sort entries rootSnapshotRoots(S4,Kind=>"Imaginary") === expectedImag);
    assert(all(expectedReal|expectedImag,beta -> rootMultiplicity(S4,beta) === 1));
    before := new HashTable from pairs S4;
    text4 := toString S4;
    assert(text4 === toString net S4);
    assert(match("Positive roots through height 4",text4));
    assert(match("simple-root basis",text4));
    assert(match("multiplicity",text4) and match("imaginary",text4));
    assert(not match("algorithm=|SparsePeterson|additional computed",text4));
    assert(all(expectedReal|expectedImag,beta -> match(
        "\\("|toString beta#0|", "|toString beta#1|"\\)",text4)));
    assert((new HashTable from pairs S4) === before);

    -- The two height-five imaginary roots have two-dimensional root spaces.
    S5 := rootSnapshot(Ah,5);
    assert(rootMultiplicity(S5,{2,3}) === 2);
    assert(rootMultiplicity(S5,{3,2}) === 2);
    assert(match("\\(2, 3\\).*2.*imaginary",toString S5));
    assert(match("\\(3, 2\\).*2.*imaginary",toString S5));

    -- Algorithm metadata does not replace or alter the mathematical table.
    B5 := indefiniteRootSnapshot(Ah,5,Algorithm=>"BCL");
    P5 := indefiniteRootSnapshot(Ah,5,Algorithm=>"Peterson");
    assert(toString B5 === toString P5);

    F := rootSnapshot cartanA 4;
    assert(match("All positive roots",toString F));
    F1 := rootSnapshot(cartanA 4,1);
    assert(match("Positive roots through height 1",toString F1));
    assert(not match("All positive roots",toString F1));
    empty := rootSnapshot matrix mutableMatrix(ZZ,0,0);
    assert(match("No positive roots",toString empty));
    assert(not match("additional computed|\\.\\.\\.",toString empty));

    -- Affine imaginary roots retain the actual multiplicity, including
    -- the periodic change in twisted affine A5^(2).
    A2aff := rootSnapshot(cartanAffineA 2,6);
    assert(rootMultiplicity(A2aff,{1,1,1}) === 2);
    assert(match("\\(1, 1, 1\\).*2.*imaginary",toString A2aff));
    twisted := rootSnapshot(transpose cartanAffineB 3,10);
    assert(rootMultiplicity(twisted,{1,1,2,1}) === 2);
    assert(rootMultiplicity(twisted,{2,2,4,2}) === 3);
    twistedFull := toString rootSnapshotByHeight twisted;
    assert(match("\\(1, 1, 2, 1\\).*2.*imaginary",twistedFull));
    assert(match("\\(2, 2, 4, 2\\).*3.*imaginary",twistedFull));

    -- More than twenty roots: the omitted count is a display count only.
    large := rootSnapshot cartanA 8;
    assert(numrows rootSnapshotRoots large === 28);
    largeBefore := new HashTable from pairs large;
    preview := toString large;
    assert(match("8 additional computed roots not displayed",preview));
    assert(match("rootSnapshotByHeight to display all 28 roots",preview));
    full := toString rootSnapshotByHeight large;
    assert(match("\\(1, 1, 1, 1, 1, 1, 1\\)",full));
    assert(not match("\\(1, 1, 1, 1, 1, 1, 1\\)",preview));
    assert(rootMultiplicity(large,{1,1,1,1,1,1,1}) === 1);
    assert((new HashTable from pairs large) === largeBefore);

    -- Reducible input keeps full ambient coordinates; no mixed roots.
    product := rootSnapshot diagonalMatrix {2,2};
    assert(sort entries rootSnapshotRoots product === {{0,1},{1,0}});
    assert(match("\\(0, 1\\)",toString product));
    assert(match("\\(1, 0\\)",toString product));
    assert(not match("\\(1, 1\\)",toString product));
    assert(instance(rootSnapshotByHeight(new HashTable from pairs S4),Net));
    assert(toString(new RootSnapshot from {}) ===
        "RootSnapshot (root data unavailable)");
///


-------------------------------------------------------------------------
-- Documentation Tests
-------------------------------------------------------------------------

-- Keep user-facing method help connected to the mathematical reference.
-- Only presentation hooks and inherited ZZ[q] machinery may be excluded.
TEST ///
    P := package "KacMoody";
    corePrivate := Core#"private dictionary";
    makeTag := value corePrivate#"makeDocumentTag";
    publicMethods := value corePrivate#"documentableMethods";
    primaryTag := value corePrivate#"getPrimaryTag";
    fetchDocumentation := value corePrivate#"fetchRawDocumentation";

    -- These inherited methods are created when polynomial arithmetic
    -- is first used, so check coverage after they have been installed too.
    q := KazhdanLusztigPolynomialRing_0;
    dummySum := q+q;
    dummyProduct := q*q;
    dummyDifference := q-q;
    assert(dummySum == 2*q and dummyProduct == q^2 and dummyDifference == 0);

    presentationKey := key -> (
        isMember(key#0,{net,toString}) or (
            isMember(key#0,{lift,promote,expression,symbol ?,symbol +,symbol -,symbol *,symbol ==})
            and isMember(KazhdanLusztigPolynomialRing,toList drop(key,1))));
    scan(publicMethods P,key -> (
        tag := makeTag(key,Package=>"KacMoody");
        name := format tag;
        record := fetchDocumentation tag;
        if record === null then
            error("missing public method documentation: "|name);
        if record#?"undocumented" and record#"undocumented" === true then
            assert(presentationKey key)
        else (
            primary := primaryTag tag;
            page := fetchDocumentation primary;
            assert(page =!= null);
            assert(page#?Description and #page.Description > 0);
        );
    ));
    scan(P#"exported symbols",name -> (
        tag := makeTag(name,Package=>"KacMoody");
        record := fetchDocumentation tag;
        assert(record =!= null);
        assert(not record#?"undocumented");
        page := fetchDocumentation primaryTag tag;
        assert(page =!= null);
        assert(page#?Description and #page.Description > 0);
        -- A chapter alias is not a command reference.  Every exported
        -- symbol has its own primary page, including types and options.
        assert(format primaryTag tag === toString name);
        object := value name;
        if instance(object,Function) and not instance(object,Type) then assert(page#?Usage);
        -- Examples are rendered as tables by SimpleDoc.  Require prose
        -- after an example so that its mathematical meaning is explained.
        sections := toList page.Description;
        examples := select(toList(0..#sections-1),i -> toString class sections#i === "TABLE");
        assert(#examples > 0);
        assert(any(examples,i -> any(drop(sections,i+1),s -> toString class s === "DIV")));
    ));

    -- Representative mathematical methods must link to the expected
    -- command page, while arithmetic remains in its existing topic guide.
    expectedPages := {
        ((kmWeight,RootDatum,List),"kmWeight"),
        ((symbol +,KMRoot,KMRoot),"Roots, weights, and pairings"),
        ((standardParabolic,RootDatum,Set),"standardParabolic"),
        ((dualWeight,RootDatum,KMWeight),"dualWeight"),
        ((affineDegree,KMWeight,KMCoweight),"affineDegree"),
        ((finiteCharacter,RootDatum,KMWeight),"finiteCharacter"),
        ((characterCoefficient,LeviFormalCharacter,LeviWeight),"characterCoefficient"),
        ((adamsOperation,ZZ,FormalCharacter),"adamsOperation"),
        ((kazhdanLusztigPolynomial,WeylElement,WeylElement),"kazhdanLusztigPolynomial"),
        ((minusculeCrystal,RootDatum,ZZ),"minusculeCrystal")
    };
    expectedPages = expectedPages | {
        ((symbol +,LeviFormalCharacter,LeviFormalCharacter),"Reductive Levi restriction"),
        ((symbol *,LeviFormalCharacter,LeviFormalCharacter),"Reductive Levi restriction"),
        ((symbol ^,LeviFormalCharacter,ZZ),"Reductive Levi restriction")
    };
    scan(expectedPages,item -> assert(
        format primaryTag makeTag(item#0,Package=>"KacMoody") === item#1));

    characterGuide := fetchDocumentation makeTag(
        "Working with formal characters",Package=>"KacMoody");
    assert(characterGuide =!= null);
    assert(characterGuide#?Description and #characterGuide.Description > 0);

    -- Before-and-after examples must show their mathematical inputs,
    -- either at assignment or by an explicit display.
    -- Installed help can come from the documentation database without
    -- populating the transient example-input table.
    visibleExampleValue := (node,variableName) -> (
        inputLines := lines toString examples makeTag(node,Package=>"KacMoody");
        any(inputLines,line -> (
            match("^[[:space:]]*"|variableName|"[[:space:]]*=",line)
            and not match(";[[:space:]]*$",line)))
        or any(inputLines,line -> match(
            "^[[:space:]]*"|variableName|"[[:space:]]*$",line))
    );
    visibilityFixtures := {
        {"demazureOperator","C"},
        {"dualCharacter","C"},
        {"dualCharacter","Cd"},
        {"characterTerms","C"},
        {"characterCoefficient","C"},
        {"restrictCharacter","C"},
        {"decomposeCharacter","P"}
    };
    assert(all(visibilityFixtures,fixture ->
        visibleExampleValue(fixture#0,fixture#1)));

    -- The same checks must work with no transient example inputs.
    savedExampleInputs := P#"example inputs";
    P#"example inputs" = new MutableHashTable;
    visibleWithoutCache := try all(visibilityFixtures,fixture ->
        visibleExampleValue(fixture#0,fixture#1)) else false;
    P#"example inputs" = savedExampleInputs;
    assert(visibleWithoutCache);
///


-------------------------------------------------------------------------
-- Levi Restriction Regression Tests
-------------------------------------------------------------------------

-- The full Levi coordinates must distinguish complete ambient weights.
-- Singular retained Cartan matrices do not admit the current separation
-- into independent Dynkin coordinates and center coordinates.
TEST ///
    mustErrorLeviRegression = f -> (try (f(); false) else true);
    liftLeviRegression = value (package "KacMoody")#"private dictionary"#"draftLiftLeviWeight";

    DaffLeviRegression = rootDatum cartanAffineA 1;
    PaffLeviRegression = standardParabolic(DaffLeviRegression,LeviNodes=>{1,2});
    lambdaLeviRegression = weightFromRealizationCoordinates(
        DaffLeviRegression,{1,0,0});
    deltaLeviRegression = toWeight affineNullRoot DaffLeviRegression;
    lowerLeviRegression = lambdaLeviRegression-deltaLeviRegression;
    assert(lambdaLeviRegression =!= lowerLeviRegression);
    assert(datumCoordinates lambdaLeviRegression ===
        datumCoordinates lowerLeviRegression);
    exactAffineLeviRegression = formalCharacter(DaffLeviRegression,
        new Tally from {lambdaLeviRegression=>1,lowerLeviRegression=>2});
    assert(mustErrorLeviRegression(() ->
        reductiveLeviDatum(PaffLeviRegression,DaffLeviRegression)));
    assert(mustErrorLeviRegression(() ->
        restrictWeight(PaffLeviRegression,lambdaLeviRegression)));
    assert(mustErrorLeviRegression(() ->
        restrictCharacter(PaffLeviRegression,exactAffineLeviRegression)));

    -- Retaining just one affine node is nonsingular: its central character
    -- still distinguishes the null-root translation.  The same holds for
    -- the Borel, whose entire Cartan is the center.
    for retainedLeviRegression in {{1},{2},{}} do (
        P := standardParabolic(DaffLeviRegression,
            LeviNodes=>retainedLeviRegression);
        L := reductiveLeviDatum(P,DaffLeviRegression);
        top := restrictWeight(L,lambdaLeviRegression);
        lower := restrictWeight(L,lowerLeviRegression);
        assert(top =!= lower);
        assert(liftLeviRegression top === lambdaLeviRegression);
        assert(liftLeviRegression lower === lowerLeviRegression);
        RC := restrictCharacter(L,exactAffineLeviRegression);
        assert(#characterSupport RC == 2);
        assert(characterCoefficient(RC,top) == 1);
        assert(characterCoefficient(RC,lower) == 2);
    );

    -- Nonsingular indefinite type is not rejected merely for being
    -- nonfinite.  Retaining all nodes gives faithful full coordinates.
    DhypLeviRegression = rootDatum matrix{{2,-3},{-3,2}};
    PhypLeviRegression = standardParabolic(DhypLeviRegression,LeviNodes=>{1,2});
    LhypLeviRegression = reductiveLeviDatum(PhypLeviRegression,DhypLeviRegression);
    assert(LhypLeviRegression#"centerDimension" == 0);
    uhypLeviRegression = kmWeight(DhypLeviRegression,{1,0});
    vhypLeviRegression = kmWeight(DhypLeviRegression,{0,1});
    assert(restrictWeight(LhypLeviRegression,uhypLeviRegression) =!=
        restrictWeight(LhypLeviRegression,vhypLeviRegression));
    assert(liftLeviRegression restrictWeight(LhypLeviRegression,uhypLeviRegression) ===
        uhypLeviRegression);
    assert(#characterSupport restrictCharacter(LhypLeviRegression,
        formalCharacter(DhypLeviRegression,new Tally from {
            uhypLeviRegression=>1,vhypLeviRegression=>3})) == 2);

    -- An affine principal submatrix can occur inside a nonsingular
    -- indefinite ambient matrix.  The retained matrix, not ambient type,
    -- determines whether the coordinate map is faithful.
    DmixedLeviRegression = rootDatum matrix{{2,-2,-1},{-2,2,-1},{-1,-1,2}};
    PmixedLeviRegression = standardParabolic(DmixedLeviRegression,LeviNodes=>{1,2});
    assert(mustErrorLeviRegression(() ->
        reductiveLeviDatum(PmixedLeviRegression,DmixedLeviRegression)));
    assert(instance(reductiveLeviDatum(standardParabolic(
        DmixedLeviRegression,LeviNodes=>{1,3}),DmixedLeviRegression),
        ReductiveLeviDatum));

    -- A nonminimal finite realization retains all additional central
    -- directions, including when the simple-coroot basis is not standard.
    RextraLeviRegression = realization(cartanDatum(cartanA 2),
        matrix(QQ,{{1,0,0}}),matrix(QQ,{{2,0,0}}));
    DextraLeviRegression = rootDatum RextraLeviRegression;
    PextraLeviRegression = standardParabolic(DextraLeviRegression,LeviNodes=>{1});
    LextraLeviRegression = reductiveLeviDatum(PextraLeviRegression,DextraLeviRegression);
    assert(LextraLeviRegression#"centerDimension" == 2);
    extraTopLeviRegression = weightFromRealizationCoordinates(DextraLeviRegression,{1,7,-4});
    extraLowerLeviRegression = weightFromRealizationCoordinates(DextraLeviRegression,{1,7,-3});
    assert(datumCoordinates extraTopLeviRegression === {2});
    assert(restrictWeight(LextraLeviRegression,extraTopLeviRegression) =!=
        restrictWeight(LextraLeviRegression,extraLowerLeviRegression));
    assert(liftLeviRegression restrictWeight(LextraLeviRegression,extraTopLeviRegression) ===
        extraTopLeviRegression);

    -- Apply a skew change of basis, contragrediently on coroots.  The
    -- inverse coordinate map must work in that realization too.
    DskewBaseLeviRegression = rootDatum "A2";
    skewLeviRegression = matrix(QQ,{{1,2},{3,7}});
    RskewLeviRegression = realization(cartanDatum DskewBaseLeviRegression,
        matrix(QQ,(realization DskewBaseLeviRegression)#"simpleRootCoordinates") * skewLeviRegression,
        matrix(QQ,(realization DskewBaseLeviRegression)#"simpleCorootCoordinates") *
            transpose inverse skewLeviRegression);
    DskewLeviRegression = rootDatum RskewLeviRegression;
    xskewLeviRegression = weightFromRealizationCoordinates(DskewLeviRegression,{5,-2});
    for retainedSkewLeviRegression in {{},{1},{2},{1,2}} do (
        L := reductiveLeviDatum(standardParabolic(DskewLeviRegression,
            LeviNodes=>retainedSkewLeviRegression),DskewLeviRegression);
        assert(liftLeviRegression restrictWeight(L,xskewLeviRegression) ===
            xskewLeviRegression);
    );

    -- Rank-zero tori also have faithful center coordinates, including the
    -- genuine zero-dimensional boundary.
    for torusDimensionLeviRegression in {0,2} do (
        A0 := matrix mutableMatrix(ZZ,0,0);
        E0 := matrix mutableMatrix(QQ,0,torusDimensionLeviRegression);
        D0 := rootDatum realization(cartanDatum A0,E0,E0);
        P0 := standardParabolic(D0,LeviNodes=>{});
        L0 := reductiveLeviDatum(P0,D0);
        assert(L0#"centerDimension" == torusDimensionLeviRegression);
        x0 := weightFromRealizationCoordinates(D0,
            apply(torusDimensionLeviRegression,i -> i+2));
        assert(centralCharacter restrictWeight(L0,x0) ===
            apply(torusDimensionLeviRegression,i -> i+2));
        assert(liftLeviRegression restrictWeight(L0,x0) === x0);
    );
///


-------------------------------------------------------------------------
-- Character Certification Regression Tests
-------------------------------------------------------------------------

-- Mathematical regression tests for coefficients known from finite prefixes.
-- Complete finite characters are independent oracles for the operations below.

TEST ///

mustErrorCert = f -> (try (f(); false) else true);
offsetsCert = H -> flatten apply(toList(0..H),h ->
    apply(toList(0..h),i -> {i,h-i}));
prefixOracleCert = (F,lambda,H) -> new Tally from
    for p in pairs characterTerms F list (
        beta := weightToRootCoordinates(lambda-p#0);
        if all(beta,b -> instance(b,ZZ) and b >= 0) and sum beta <= H
            then p#0 => p#1 else continue
    );

Dcert = rootDatum "A2";
lcert = kmWeight(Dcert,{2,1});
Mcert = integrableHighestWeightModule(Dcert,lcert);
Fcert = finiteCharacter(Dcert,lcert);
C1cert = moduleCharacter(Mcert,Depth=>1);
C3cert = moduleCharacter(Mcert,Depth=>3);
S13cert = C1cert+C3cert;
P13cert = C1cert*C3cert;
FPcert = Fcert*C1cert;
PFcert = C1cert*Fcert;
F2cert = Fcert*Fcert;
assert(characterTerms S13cert == prefixOracleCert(2*Fcert,lcert,1));
assert(characterTerms P13cert == prefixOracleCert(F2cert,2*lcert,1));
assert(characterTerms FPcert == characterTerms PFcert);
for betaCert in offsetsCert 1 do (
    muCert := lcert-toWeight kmRoot(Dcert,betaCert);
    assert(characterCoefficient(S13cert,muCert) ==
        characterCoefficient(2*Fcert,muCert));
    productWeightCert := 2*lcert-toWeight kmRoot(Dcert,betaCert);
    assert(characterCoefficient(P13cert,productWeightCert) ==
        characterCoefficient(F2cert,productWeightCert));
    assert(characterCoefficient(FPcert,productWeightCert) ==
        characterCoefficient(F2cert,productWeightCert));
);
for pCert in pairs characterTerms FPcert do
    assert(pCert#1 == characterCoefficient(F2cert,pCert#0));
assert(not isExactCharacter S13cert);
assert(not isExactCharacter P13cert);
assert(S13cert#"truncation"#"completeThroughDepth" == 1);
assert(P13cert#"truncation"#"highestWeight" === 2*lcert);
deepCert = lcert-toWeight kmRoot(Dcert,{1,1});
assert(characterCoefficient(Fcert,deepCert) == 2);
assert(mustErrorCert(() -> characterCoefficient(C1cert,deepCert)));
assert(mustErrorCert(() -> characterCoefficient(S13cert,deepCert)));
assert(mustErrorCert(() -> characterCoefficient(P13cert,
    2*lcert-toWeight kmRoot(Dcert,{1,1}))));
assert(characterCoefficient(C1cert,lcert+toWeight kmRoot(Dcert,{1,0})) == 0);
assert(characterCoefficient(C1cert,kmWeight(Dcert,{1,3/2})) == 0);
fundCert = kmWeight(Dcert,{1,0});
fundPrefixCert = moduleCharacter(integrableHighestWeightModule(Dcert,fundCert),Depth=>1);
assert(characterCoefficient(fundPrefixCert,
    fundCert-toWeight kmRoot(Dcert,{0,1})) == 0);
assert(characterCoefficient((-3)*C1cert,lcert) == -3);
zeroCert = 0*C1cert;
assert(isExactCharacter zeroCert);
assert(#keys characterTerms zeroCert == 0);
assert(characterDimension zeroCert == 0);
assert(characterCoefficient(zeroCert,deepCert) == 0);
assert(isExactCharacter(zeroCert*C1cert));

-- Several finite factors can be composed without assuming that the other
-- factor has an ordinary highest-weight cone after every transformation.
nestedProductCert = Fcert*(Fcert*C1cert);
F3cert = Fcert*F2cert;
for pCert in pairs characterTerms nestedProductCert do
    assert(pCert#1 == characterCoefficient(F3cert,pCert#0));
for betaCert in offsetsCert 1 do (
    muCert := 3*lcert-toWeight kmRoot(Dcert,betaCert);
    assert(characterCoefficient(nestedProductCert,muCert) ==
        characterCoefficient(F3cert,muCert));
);

///

TEST ///

mustErrorTransportCert = f -> (try (f(); false) else true);
DtransportCert = rootDatum "A2";
ltransportCert = kmWeight(DtransportCert,{2,1});
CtransportCert = moduleCharacter(
    integrableHighestWeightModule(DtransportCert,ltransportCert),Depth=>1);
FtransportCert = finiteCharacter(DtransportCert,ltransportCert);
wtransportCert = weylElement(weylGroup DtransportCert,{1,2});
WtransportCert = weylAction(wtransportCert,CtransportCert);
FWtransportCert = weylAction(wtransportCert,FtransportCert);
DualtransportCert = dualCharacter CtransportCert;
FDualtransportCert = dualCharacter FtransportCert;
for pTransportCert in pairs characterTerms CtransportCert do (
    assert(characterCoefficient(WtransportCert,
        weylAction(wtransportCert,pTransportCert#0)) == pTransportCert#1);
    assert(characterCoefficient(DualtransportCert,-pTransportCert#0) == pTransportCert#1);
);
for pTransportCert in pairs characterTerms WtransportCert do
    assert(pTransportCert#1 == characterCoefficient(FWtransportCert,pTransportCert#0));
for pTransportCert in pairs characterTerms DualtransportCert do
    assert(pTransportCert#1 == characterCoefficient(FDualtransportCert,pTransportCert#0));
outsideTransportCert = ltransportCert+toWeight kmRoot(DtransportCert,{1,0});
deepTransportCert = ltransportCert-toWeight kmRoot(DtransportCert,{1,1});
assert(characterCoefficient(WtransportCert,
    weylAction(wtransportCert,outsideTransportCert)) == 0);
assert(characterCoefficient(DualtransportCert,-outsideTransportCert) == 0);
assert(mustErrorTransportCert(() -> characterCoefficient(WtransportCert,
    weylAction(wtransportCert,deepTransportCert))));
assert(mustErrorTransportCert(() -> characterCoefficient(DualtransportCert,-deepTransportCert)));
assert(not match("through root depth",toString WtransportCert));
assert(not match("through root depth",toString DualtransportCert));
assert(characterTerms dualCharacter DualtransportCert == characterTerms CtransportCert);
assert(mustErrorTransportCert(() -> CtransportCert*DualtransportCert));
assert(mustErrorTransportCert(() -> WtransportCert*CtransportCert));
finiteTransportProductCert = FtransportCert*WtransportCert;
finiteTransportOracleCert = FtransportCert*FWtransportCert;
for pTransportCert in pairs characterTerms finiteTransportProductCert do
    assert(pTransportCert#1 == characterCoefficient(
        finiteTransportOracleCert,pTransportCert#0));

-- The displayed coefficient 2 at weight 2 would be wrong if one applied
-- D_1 to the depth-one prefix of the five-dimensional sl_2 character.
DA1DemazureCert = rootDatum "A1";
lDemazureCert = kmWeight(DA1DemazureCert,{4});
CDemazureCert = moduleCharacter(
    integrableHighestWeightModule(DA1DemazureCert,lDemazureCert),Depth=>1);
FDemazureCert = finiteCharacter(DA1DemazureCert,lDemazureCert);
assert(mustErrorTransportCert(() -> demazureOperator(1,CDemazureCert)));
assert(mustErrorTransportCert(() -> demazureOperator(CDemazureCert,1)));
assert(characterTerms demazureOperator(1,FDemazureCert) == characterTerms FDemazureCert);
assert(characterCoefficient(demazureCharacter(DA1DemazureCert,{4},{1}),
    kmWeight(DA1DemazureCert,{2})) == 1);
assert(mustErrorTransportCert(() -> demazureOperator(1,
    moduleCharacter(vermaModule(DA1DemazureCert,lDemazureCert),Depth=>1))));

///

TEST ///

mustErrorMetadataCert = f -> (try (f(); false) else true);
RmodeCert = realization(cartanDatum cartanA 2,
    matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
DmodeCert = rootDatum RmodeCert;
lmodeCert = weightFromRealizationCoordinates(DmodeCert,{1,7});
classModeCert = kmWeight(DmodeCert,{1});
VMmodeCert = moduleCharacter(vermaModule(DmodeCert,lmodeCert),Depth=>0);
IMmodeCert = moduleCharacter(integrableHighestWeightModule(DmodeCert,lmodeCert),Depth=>0);
emptyModeCert = VMmodeCert-IMmodeCert;
assert(#keys characterTerms emptyModeCert == 0);
assert(not isExactCharacter emptyModeCert);
assert(characterCoefficient(emptyModeCert,lmodeCert) == 0);
assert(mustErrorMetadataCert(() -> characterCoefficient(emptyModeCert,classModeCert)));
assert(mustErrorMetadataCert(() -> emptyModeCert+finiteCharacter(DmodeCert,classModeCert)));
assert(mustErrorMetadataCert(() -> emptyModeCert*finiteCharacter(DmodeCert,classModeCert)));
assert(mustErrorMetadataCert(() -> characterCoefficient(
    dualCharacter emptyModeCert,-classModeCert)));
assert(mustErrorMetadataCert(() -> characterCoefficient(
    weylAction(weylElement(weylGroup DmodeCert,{1}),emptyModeCert),classModeCert)));
assert(mustErrorMetadataCert(() -> characterCoefficient(
    finiteCharacter(DmodeCert,lmodeCert),classModeCert)));
assert(characterCoefficient(emptyModeCert,
    weightFromRealizationCoordinates(DmodeCert,{1,8})) == 0);

nativeMetadataCert = (lambda,H) -> new HashTable from {
    "coefficientCertificate"=>true,"kind"=>"root-depth",
    "support"=>"highest-weight cone","highestWeight"=>lambda,
    "completeThroughDepth"=>H,"depth"=>H};
makeMetadataCert = (terms,metadata) -> formalCharacter(DmodeCert,terms,
    CharacterStatus=>"Truncated",Truncation=>metadata);
assert(mustErrorMetadataCert(() -> makeMetadataCert(new Tally from {lmodeCert=>1},
    nativeMetadataCert(lmodeCert,-1))));
assert(mustErrorMetadataCert(() -> makeMetadataCert(new Tally from {lmodeCert=>1},
    nativeMetadataCert(lmodeCert,1/2))));
foreignMetadataCert = kmWeight(rootDatum "A2",{1,0});
assert(mustErrorMetadataCert(() -> makeMetadataCert(new Tally from {lmodeCert=>1},
    nativeMetadataCert(foreignMetadataCert,0))));
assert(mustErrorMetadataCert(() -> makeMetadataCert(new Tally from {classModeCert=>1},
    nativeMetadataCert(lmodeCert,0))));
assert(mustErrorMetadataCert(() -> makeMetadataCert(new Tally from {
    weightFromRealizationCoordinates(DmodeCert,{1,8})=>1},
    nativeMetadataCert(lmodeCert,0))));
assert(mustErrorMetadataCert(() -> makeMetadataCert(new Tally from {
    lmodeCert+toWeight kmRoot(DmodeCert,{1})=>1},nativeMetadataCert(lmodeCert,1))));
assert(mustErrorMetadataCert(() -> makeMetadataCert(new Tally from {
    lmodeCert-toWeight kmRoot(DmodeCert,{1})=>1},nativeMetadataCert(lmodeCert,0))));
assert(mustErrorMetadataCert(() -> makeMetadataCert(new Tally from {
    weightFromRealizationCoordinates(DmodeCert,{0,7})=>1},
    nativeMetadataCert(lmodeCert,1))));
assert(mustErrorMetadataCert(() -> makeMetadataCert(new Tally from {lmodeCert=>1},
    new HashTable from {"coefficientCertificate"=>true,"kind"=>"root-depth",
        "support"=>"highest-weight cone","completeThroughDepth"=>0})));

-- Public descriptive metadata must not accidentally become an internal
-- operation or a proof that an omitted coefficient vanishes.
for operationMetadataCert in {"sum","scale","dual","weyl","restriction","finite product"} do (
    descriptiveMetadataCert := makeMetadataCert(new Tally from {lmodeCert=>1},
        new HashTable from {"operation"=>operationMetadataCert,
            "completeThroughDepth"=>9,"highestWeight"=>foreignMetadataCert});
    assert(characterCoefficient(descriptiveMetadataCert,lmodeCert) == 1);
    assert(mustErrorMetadataCert(() -> characterCoefficient(descriptiveMetadataCert,
        lmodeCert-toWeight kmRoot(DmodeCert,{1}))));
    assert(not match("through root depth",toString descriptiveMetadataCert));
);
descriptiveRootCert = makeMetadataCert(new Tally from {lmodeCert=>1},
    new HashTable from {"kind"=>"root-depth","support"=>"highest-weight cone",
        "highestWeight"=>foreignMetadataCert,"completeThroughDepth"=>9});
scaledDescriptiveRootCert = 2*descriptiveRootCert;
assert(characterCoefficient(scaledDescriptiveRootCert,lmodeCert) == 2);
assert(mustErrorMetadataCert(() -> characterCoefficient(scaledDescriptiveRootCert,
    lmodeCert-toWeight kmRoot(DmodeCert,{1}))));
assert(not match("through root depth",toString scaledDescriptiveRootCert));
assert(mustErrorMetadataCert(() -> descriptiveRootCert*descriptiveRootCert));

///

TEST ///

mustErrorAffineRangeCert = f -> (try (f(); false) else true);
DaffRangeCert = rootDatum cartanAffineA 1;
laffRangeCert = weightFromRealizationCoordinates(DaffRangeCert,{1,0,0});
deltaRangeCert = toWeight affineNullRoot DaffRangeCert;
basicRangeCert = integrableHighestWeightModule(DaffRangeCert,laffRangeCert);
CaffRangeCert = moduleCharacter(basicRangeCert,Depth=>2);
assert(CaffRangeCert#"truncation"#"highestWeight" === laffRangeCert);
assert(characterCoefficient(CaffRangeCert,laffRangeCert-deltaRangeCert) == 1);
assert(characterCoefficient(CaffRangeCert,
    laffRangeCert-toWeight kmRoot(DaffRangeCert,{0,1})) == 0);
assert(characterCoefficient(CaffRangeCert,laffRangeCert+deltaRangeCert) == 0);
assert(characterCoefficient(CaffRangeCert,
    weightFromRealizationCoordinates(DaffRangeCert,{2,0,0})) == 0);
assert(mustErrorAffineRangeCert(() -> characterCoefficient(CaffRangeCert,
    laffRangeCert-2*deltaRangeCert)));
shiftedRangeCert = moduleCharacter(integrableHighestWeightModule(
    DaffRangeCert,laffRangeCert+2*deltaRangeCert),Depth=>2);
assert(shiftedRangeCert#"truncation"#"highestWeight" === laffRangeCert+2*deltaRangeCert);
assert(characterCoefficient(shiftedRangeCert,laffRangeCert+deltaRangeCert) == 1);
assert(mustErrorAffineRangeCert(() -> characterCoefficient(shiftedRangeCert,laffRangeCert)));

DA2EulerCert = rootDatum "A2";
lA2EulerCert = kmWeight(DA2EulerCert,{2,1});
FA2EulerCert = finiteCharacter(DA2EulerCert,lA2EulerCert);
EA2EulerCert = bggEulerCharacter(bggResolution(DA2EulerCert,lA2EulerCert),Depth=>2);
assert(EA2EulerCert#"truncation"#"highestWeight" === lA2EulerCert);
for hEulerCert from 0 to 2 do for iEulerCert from 0 to hEulerCert do (
    muEulerCert := lA2EulerCert-toWeight kmRoot(
        DA2EulerCert,{iEulerCert,hEulerCert-iEulerCert});
    assert(characterCoefficient(EA2EulerCert,muEulerCert) ==
        characterCoefficient(FA2EulerCert,muEulerCert));
);
assert(mustErrorAffineRangeCert(() -> characterCoefficient(EA2EulerCert,
    lA2EulerCert-toWeight kmRoot(DA2EulerCert,{3,0}))));
BaffRangeCert = bggResolution(DaffRangeCert,laffRangeCert,MaxDegree=>2);
EaffRangeCert = bggEulerCharacter(BaffRangeCert,Depth=>2);
assert(characterTerms EaffRangeCert == characterTerms CaffRangeCert);
assert(EaffRangeCert#"truncation"#"highestWeight" === laffRangeCert);
assert(mustErrorAffineRangeCert(() -> characterCoefficient(EaffRangeCert,
    laffRangeCert-2*deltaRangeCert)));
assert(mustErrorAffineRangeCert(() -> bggEulerCharacter(BaffRangeCert,Depth=>3)));

DindefRangeCert = rootDatum matrix{{2,-3},{-3,2}};
lindefRangeCert = kmWeight(DindefRangeCert,{1,1});
VindefRangeCert = moduleCharacter(vermaModule(DindefRangeCert,lindefRangeCert),Depth=>2);
assert(VindefRangeCert#"truncation"#"highestWeight" === lindefRangeCert);
assert(characterCoefficient(VindefRangeCert,
    lindefRangeCert-toWeight kmRoot(DindefRangeCert,{1,1})) == 2);
assert(characterCoefficient(VindefRangeCert,
    lindefRangeCert+toWeight kmRoot(DindefRangeCert,{1,0})) == 0);
assert(mustErrorAffineRangeCert(() -> characterCoefficient(VindefRangeCert,
    lindefRangeCert-toWeight kmRoot(DindefRangeCert,{2,1}))));

///

TEST ///

mustErrorLeviRangeCert = f -> (try (f(); false) else true);
DLeviRangeCert = rootDatum "A2";
lLeviRangeCert = kmWeight(DLeviRangeCert,{2,1});
PLeviRangeCert = standardParabolic(DLeviRangeCert,RemovedNodes=>{1});
LLeviRangeCert = reductiveLeviDatum(PLeviRangeCert,DLeviRangeCert);
MLeviRangeCert = integrableHighestWeightModule(DLeviRangeCert,lLeviRangeCert);
C0LeviRangeCert = moduleCharacter(MLeviRangeCert,Depth=>0);
C2LeviRangeCert = moduleCharacter(MLeviRangeCert,Depth=>2);
R0LeviRangeCert = restrictCharacter(LLeviRangeCert,C0LeviRangeCert);
R2LeviRangeCert = restrictCharacter(LLeviRangeCert,C2LeviRangeCert);
assert(characterCoefficient(R0LeviRangeCert,
    restrictWeight(LLeviRangeCert,lLeviRangeCert)) == 1);
assert(mustErrorLeviRangeCert(() -> characterCoefficient(R0LeviRangeCert,
    restrictWeight(LLeviRangeCert,lLeviRangeCert-toWeight kmRoot(DLeviRangeCert,{1,0})))));
assert(characterCoefficient(R0LeviRangeCert,
    restrictWeight(LLeviRangeCert,lLeviRangeCert+toWeight kmRoot(DLeviRangeCert,{1,0}))) == 0);
assert(characterCoefficient(R2LeviRangeCert,
    restrictWeight(LLeviRangeCert,lLeviRangeCert-toWeight kmRoot(DLeviRangeCert,{1,1}))) == 2);
assert(mustErrorLeviRangeCert(() -> characterCoefficient(R2LeviRangeCert,
    restrictWeight(LLeviRangeCert,lLeviRangeCert-toWeight kmRoot(DLeviRangeCert,{3,0})))));
for pLeviRangeCert in pairs characterTerms C2LeviRangeCert do
    assert(characterCoefficient(R2LeviRangeCert,
        restrictWeight(LLeviRangeCert,pLeviRangeCert#0)) == pLeviRangeCert#1);

-- The Borel retains no derived nodes, but its full Cartan coordinates still
-- distinguish every ambient weight and carry the same coefficient range.
LBorelRangeCert = reductiveLeviDatum(
    standardParabolic(DLeviRangeCert,LeviNodes=>{}),DLeviRangeCert);
RBorelRangeCert = restrictCharacter(LBorelRangeCert,C0LeviRangeCert);
assert(characterCoefficient(RBorelRangeCert,
    restrictWeight(LBorelRangeCert,lLeviRangeCert)) == 1);
assert(mustErrorLeviRangeCert(() -> characterCoefficient(RBorelRangeCert,
    restrictWeight(LBorelRangeCert,lLeviRangeCert-toWeight kmRoot(DLeviRangeCert,{1,0})))));

DAffLeviRangeCert = rootDatum cartanAffineA 1;
lAffLeviRangeCert = weightFromRealizationCoordinates(DAffLeviRangeCert,{1,0,0});
deltaLeviRangeCert = toWeight affineNullRoot DAffLeviRangeCert;
LAffLeviRangeCert = reductiveLeviDatum(
    standardParabolic(DAffLeviRangeCert,RemovedNodes=>{1}),DAffLeviRangeCert);
CAffLeviRangeCert = moduleCharacter(integrableHighestWeightModule(
    DAffLeviRangeCert,lAffLeviRangeCert),Depth=>2);
RAffLeviRangeCert = restrictCharacter(LAffLeviRangeCert,CAffLeviRangeCert);
assert(restrictWeight(LAffLeviRangeCert,lAffLeviRangeCert) =!=
    restrictWeight(LAffLeviRangeCert,lAffLeviRangeCert-deltaLeviRangeCert));
assert(characterCoefficient(RAffLeviRangeCert,
    restrictWeight(LAffLeviRangeCert,lAffLeviRangeCert-deltaLeviRangeCert)) == 1);
assert(mustErrorLeviRangeCert(() -> characterCoefficient(RAffLeviRangeCert,
    restrictWeight(LAffLeviRangeCert,lAffLeviRangeCert-2*deltaLeviRangeCert))));

///


-------------------------------------------------------------------------
-- Character Decomposition Regression Tests
-------------------------------------------------------------------------

-- Finite decomposition must compare complete weights within their actual
-- root-lattice translates, not conflate distinct Weyl-fixed characters.
TEST ///

mustErrorDecompositionRegression = f -> (try (f(); false) else true);

fromIrrepsDecompositionRegression = (D,T) -> (
    result := formalCharacter(D,new Tally);
    for p in pairs T do result = result+p#1*finiteCharacter(D,p#0);
    result
);

checkDecompositionRegression = (C,expected) -> (
    D := characterDatum C;
    decomposition := decomposeCharacter C;
    assert(decomposition == expected);
    assert(all(keys decomposition,lambda -> parentDatum lambda === D));
    reconstructed := fromIrrepsDecompositionRegression(D,decomposition);
    assert(characterTerms reconstructed == characterTerms C);
    assert(characterDimension reconstructed == characterDimension C);

    -- Neither the character's insertion order nor the summand insertion
    -- order may determine which incomparable highest weight is removed.
    reordered := formalCharacter(D,new HashTable from
        reverse pairs characterTerms C);
    assert(decomposeCharacter reordered == expected);
    reversedSummands := new Tally from reverse pairs expected;
    assert(characterTerms fromIrrepsDecompositionRegression(D,reversedSummands)
        == characterTerms C);
    assert(decomposeCharacter fromIrrepsDecompositionRegression(D,reversedSummands)
        == expected);
    true
);

-- The original crash: two identical A1 highest Dynkin labels, with
-- different full Weyl-fixed coordinates.  Both summands are genuine.
RcentralDecompositionRegression = realization(cartanDatum(cartanA 2),
    matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
DcentralDecompositionRegression = rootDatum RcentralDecompositionRegression;
fullA1DecompositionRegression = coordinates ->
    weightFromRealizationCoordinates(DcentralDecompositionRegression,coordinates);
uDecompositionRegression = fullA1DecompositionRegression {1,0};
vDecompositionRegression = fullA1DecompositionRegression {1,3};
assert(datumCoordinates uDecompositionRegression === {1});
assert(datumCoordinates vDecompositionRegression === {1});
assert(uDecompositionRegression =!= vDecompositionRegression);
originalExpectedDecompositionRegression = new Tally from {
    uDecompositionRegression=>1,vDecompositionRegression=>1};
originalCharacterDecompositionRegression = fromIrrepsDecompositionRegression(
    DcentralDecompositionRegression,originalExpectedDecompositionRegression);
assert(checkDecompositionRegression(originalCharacterDecompositionRegression,
    originalExpectedDecompositionRegression));

-- Equal labels in different sectors, label-dominance across sectors, and
-- nested irreducibles in a single sector must all remain distinguishable.
mixedExpectedDecompositionRegression = new Tally from {
    (fullA1DecompositionRegression {1,0})=>2,
    (fullA1DecompositionRegression {1,3})=>3,
    (fullA1DecompositionRegression {4,-2})=>1,
    (fullA1DecompositionRegression {2,-2})=>2,
    (fullA1DecompositionRegression {2,5})=>1,
    (fullA1DecompositionRegression {0,0})=>4,
    (fullA1DecompositionRegression {0,7})=>1};
assert(checkDecompositionRegression(fromIrrepsDecompositionRegression(
    DcentralDecompositionRegression,mixedExpectedDecompositionRegression),
    mixedExpectedDecompositionRegression));

-- Tensor products and lambda powers of a reducible module have several
-- central sectors.  These expected decompositions are the A1 product rule
-- and the direct-sum formulas for Sym^2 and exterior square.
tensorExpectedDecompositionRegression = new Tally from {
    (fullA1DecompositionRegression {2,0})=>1,
    (fullA1DecompositionRegression {0,0})=>1,
    (fullA1DecompositionRegression {2,3})=>2,
    (fullA1DecompositionRegression {0,3})=>2,
    (fullA1DecompositionRegression {2,6})=>1,
    (fullA1DecompositionRegression {0,6})=>1};
assert(checkDecompositionRegression(originalCharacterDecompositionRegression*
    originalCharacterDecompositionRegression,tensorExpectedDecompositionRegression));
symExpectedDecompositionRegression = new Tally from {
    (fullA1DecompositionRegression {2,0})=>1,
    (fullA1DecompositionRegression {2,3})=>1,
    (fullA1DecompositionRegression {0,3})=>1,
    (fullA1DecompositionRegression {2,6})=>1};
exteriorExpectedDecompositionRegression = new Tally from {
    (fullA1DecompositionRegression {0,0})=>1,
    (fullA1DecompositionRegression {2,3})=>1,
    (fullA1DecompositionRegression {0,3})=>1,
    (fullA1DecompositionRegression {0,6})=>1};
assert(symmetricPowerDecomposition(originalCharacterDecompositionRegression,2)
    == symExpectedDecompositionRegression);
assert(exteriorPowerDecomposition(originalCharacterDecompositionRegression,2)
    == exteriorExpectedDecompositionRegression);
assert(checkDecompositionRegression(symmetricPowerCharacter(
    originalCharacterDecompositionRegression,2),symExpectedDecompositionRegression));
assert(checkDecompositionRegression(exteriorPowerCharacter(
    originalCharacterDecompositionRegression,2),exteriorExpectedDecompositionRegression));
assert(checkDecompositionRegression(exteriorPowerCharacter(
    originalCharacterDecompositionRegression,4),new Tally from {
        (fullA1DecompositionRegression {0,6})=>1}));
assert(checkDecompositionRegression(exteriorPowerCharacter(
    originalCharacterDecompositionRegression,5),new Tally));

-- Two invisible directions and a rational skew basis: no particular
-- coordinate column can be assumed to be the semisimple or central part.
skewDecompositionRegression = matrix(QQ,{
    {1/2,1,2},{0,2/3,1},{1,0,3}});
RskewDecompositionRegression = realization(cartanDatum(cartanA 2),
    matrix(QQ,{{2,0,0}})*skewDecompositionRegression,
    matrix(QQ,{{1,0,0}})*transpose inverse skewDecompositionRegression);
DskewDecompositionRegression = rootDatum RskewDecompositionRegression;
skewWeightDecompositionRegression = coordinates ->
    weightFromRealizationCoordinates(DskewDecompositionRegression,
        flatten entries(matrix(QQ,{coordinates})*skewDecompositionRegression));
skewExpectedDecompositionRegression = new Tally from {
    (skewWeightDecompositionRegression {4,0,0})=>1,
    (skewWeightDecompositionRegression {2,0,0})=>2,
    (skewWeightDecompositionRegression {2,1/3,-4/5})=>3,
    (skewWeightDecompositionRegression {2,1/3,7/5})=>1,
    (skewWeightDecompositionRegression {0,-3/2,2/7})=>2,
    (skewWeightDecompositionRegression {1,-1,2})=>1};
assert(checkDecompositionRegression(fromIrrepsDecompositionRegression(
    DskewDecompositionRegression,skewExpectedDecompositionRegression),
    skewExpectedDecompositionRegression));

-- A product root system needs the full multi-node dominance cone, while
-- preserving two extra directions after a nonstandard basis change.
AproductDecompositionRegression = diagonalMatrix {2,2};
skewProductDecompositionRegression = matrix(QQ,{
    {1,1/2,1,0},{0,1,2,1},{0,0,2/3,1},{0,0,0,3/5}});
EproductDecompositionRegression = matrix mutableMatrix(QQ,2,2);
RproductDecompositionRegression = realization(cartanDatum AproductDecompositionRegression,
    ((AproductDecompositionRegression ** QQ) | EproductDecompositionRegression)*
        skewProductDecompositionRegression,
    ((id_(QQ^2)) | EproductDecompositionRegression)*
        transpose inverse skewProductDecompositionRegression);
DproductDecompositionRegression = rootDatum RproductDecompositionRegression;
productWeightDecompositionRegression = coordinates ->
    weightFromRealizationCoordinates(DproductDecompositionRegression,
        flatten entries(matrix(QQ,{coordinates})*skewProductDecompositionRegression));
productExpectedDecompositionRegression = new Tally from {
    (productWeightDecompositionRegression {2,1,0,0})=>1,
    (productWeightDecompositionRegression {0,1,0,0})=>2,
    (productWeightDecompositionRegression {2,1,1,-2})=>3,
    (productWeightDecompositionRegression {0,3,1,-2})=>1,
    (productWeightDecompositionRegression {1,0,-1/3,5/7})=>2};
assert(checkDecompositionRegression(fromIrrepsDecompositionRegression(
    DproductDecompositionRegression,productExpectedDecompositionRegression),
    productExpectedDecompositionRegression));
productPairDecompositionRegression = fromIrrepsDecompositionRegression(
    DproductDecompositionRegression,new Tally from {
        (productWeightDecompositionRegression {1,1,0,0})=>1,
        (productWeightDecompositionRegression {1,1,1,0})=>1});
productTensorExpectedDecompositionRegression = new Tally from flatten(flatten(
    for z from 0 to 2 list for a in {0,2} list for b in {0,2} list
        (productWeightDecompositionRegression {a,b,z,0})=>
            (if z == 1 then 2 else 1)));
assert(checkDecompositionRegression(productPairDecompositionRegression*
    productPairDecompositionRegression,productTensorExpectedDecompositionRegression));

-- A nonminimal A2 realization gives an adversarial display-order test for
-- label classes.  Projecting a label-class weight into an ambient section
-- must not rank the lower dominant tuple (0,1) above its highest tuple
-- (2,0).  Both label-class and complete-weight decomposition stay exact.
RclassOrderDecompositionRegression = realization(cartanDatum "A2",
    matrix(QQ,{{2,-1,2},{-1,2,0}}),
    matrix(QQ,{{1,0,0},{0,1,0}}));
DclassOrderDecompositionRegression = rootDatum RclassOrderDecompositionRegression;
classOrderTopDecompositionRegression = kmWeight(DclassOrderDecompositionRegression,{2,0});
classOrderCharacterDecompositionRegression = finiteCharacter(
    DclassOrderDecompositionRegression,classOrderTopDecompositionRegression);
assert(checkDecompositionRegression(classOrderCharacterDecompositionRegression,
    new Tally from {classOrderTopDecompositionRegression=>1}));
assert(match("^e\\^[(]2, 0[)]",toString classOrderCharacterDecompositionRegression));
fullOrderTopDecompositionRegression = weightFromRealizationCoordinates(
    DclassOrderDecompositionRegression,{2,0,0});
fullOrderCharacterDecompositionRegression = finiteCharacter(
    DclassOrderDecompositionRegression,fullOrderTopDecompositionRegression);
assert(checkDecompositionRegression(fullOrderCharacterDecompositionRegression,
    new Tally from {fullOrderTopDecompositionRegression=>1}));
assert(match("^e\\^[(]2, 0, 0[)]",toString fullOrderCharacterDecompositionRegression));

-- Rank zero is a legitimate torus, including dimension zero.  With no
-- roots, distinct torus weights cannot dominate one another.
for torusDimensionDecompositionRegression in {0,2} do (
    A0 := matrix mutableMatrix(ZZ,0,0);
    E0 := matrix mutableMatrix(QQ,0,torusDimensionDecompositionRegression);
    D0 := rootDatum realization(cartanDatum A0,E0,E0);
    zero0 := weightFromRealizationCoordinates(D0,
        toList(torusDimensionDecompositionRegression:0));
    assert(checkDecompositionRegression(5*finiteCharacter(D0,zero0),
        new Tally from {zero0=>5}));
    assert(checkDecompositionRegression(formalCharacter(D0,new Tally),new Tally));
    if torusDimensionDecompositionRegression == 2 then (
        u0 := weightFromRealizationCoordinates(D0,{2/3,-3/5});
        v0 := weightFromRealizationCoordinates(D0,{-1/2,4/7});
        mixed0 := new Tally from {u0=>2,v0=>3,zero0=>1};
        assert(checkDecompositionRegression(fromIrrepsDecompositionRegression(D0,mixed0),mixed0));
        C0 := finiteCharacter(D0,u0)+finiteCharacter(D0,v0);
        assert(checkDecompositionRegression(C0*C0,
            new Tally from {2*u0=>1,(u0+v0)=>2,2*v0=>1}));
        assert(checkDecompositionRegression(symmetricPowerCharacter(C0,2),
            new Tally from {2*u0=>1,(u0+v0)=>1,2*v0=>1}));
        assert(checkDecompositionRegression(exteriorPowerCharacter(C0,2),
            new Tally from {(u0+v0)=>1}));
    );
    assert(mustErrorDecompositionRegression(() ->
        decomposeCharacter(-finiteCharacter(D0,zero0))));
);

-- Dynkin-label-class arithmetic must retain its existing semantics even
-- when complete weights are available in the same nonminimal datum.
classExpectedDecompositionRegression = new Tally from {
    (kmWeight(DcentralDecompositionRegression,{4}))=>1,
    (kmWeight(DcentralDecompositionRegression,{2}))=>3,
    (kmWeight(DcentralDecompositionRegression,{1}))=>2,
    (kmWeight(DcentralDecompositionRegression,{0}))=>5};
classCharacterDecompositionRegression = fromIrrepsDecompositionRegression(
    DcentralDecompositionRegression,classExpectedDecompositionRegression);
assert(checkDecompositionRegression(classCharacterDecompositionRegression,
    classExpectedDecompositionRegression));
assert(all(keys decomposeCharacter classCharacterDecompositionRegression,
    lambda -> mustErrorDecompositionRegression(() -> realizationCoordinates lambda)));
assert(mustErrorDecompositionRegression(() ->
    classCharacterDecompositionRegression+originalCharacterDecompositionRegression));

-- A change in highest-weight selection must not make invalid, negative,
-- or non-Weyl-invariant finite polynomials pass as genuine characters.
assert(mustErrorDecompositionRegression(() -> decomposeCharacter(
    -originalCharacterDecompositionRegression)));
assert(mustErrorDecompositionRegression(() -> decomposeCharacter(
    formalCharacter(DcentralDecompositionRegression,new Tally from {
        uDecompositionRegression=>1}))));
assert(mustErrorDecompositionRegression(() -> decomposeCharacter(
    formalCharacter(DcentralDecompositionRegression,new Tally from {
        (fullA1DecompositionRegression {-1,3})=>1}))));
assert(mustErrorDecompositionRegression(() -> decomposeCharacter(
    formalCharacter(DcentralDecompositionRegression,new Tally from {
        (fullA1DecompositionRegression {1/2,3})=>1}))));
assert(checkDecompositionRegression(originalCharacterDecompositionRegression-
    originalCharacterDecompositionRegression,new Tally));

///


-------------------------------------------------------------------------
-- Character Efficiency Regression Tests
-------------------------------------------------------------------------

-- Efficiency shortcuts must preserve the same coefficients, coordinate
-- modes, and validation guarantees as the general character algorithms.

TEST ///

mustErrorEfficiencyProduct = f -> (try (f(); false) else true);
offsetsEfficiencyProduct = H -> flatten apply(toList(0..H),h ->
    apply(toList(0..h),i -> {i,h-i}));

-- Unequal-depth Verma products have the same depth-two coefficients in
-- finite A2, affine A1^(1), and rank-two indefinite type.  At alpha1+alpha2
-- the two one-root factors and the positive mixed root give coefficient 6.
DEfficiencyAffine = rootDatum cartanAffineA 1;
lEfficiencyAffine = weightFromRealizationCoordinates(DEfficiencyAffine,{1,0,0});
DEfficiencyIndefinite = rootDatum matrix{{2,-3},{-3,2}};
for fixtureEfficiencyProduct in {
    {rootDatum "A2",{2,1}},
    {DEfficiencyAffine,lEfficiencyAffine},
    {DEfficiencyIndefinite,{1,1}}
} do (
    D := fixtureEfficiencyProduct#0;
    lambda := if instance(fixtureEfficiencyProduct#1,KMWeight)
        then fixtureEfficiencyProduct#1 else kmWeight(D,fixtureEfficiencyProduct#1);
    M := vermaModule(D,lambda);
    shallow := moduleCharacter(M,Depth=>2);
    deep := moduleCharacter(M,Depth=>9);
    product := shallow*deep;
    reversed := deep*shallow;
    signed := ((-2)*shallow)*(3*deep);
    expected := new HashTable from {
        {0,0}=>1,{1,0}=>2,{0,1}=>2,
        {2,0}=>3,{0,2}=>3,{1,1}=>6};
    assert(characterTerms product == characterTerms reversed);
    assert(not isExactCharacter product);
    assert(product#"truncation"#"completeThroughDepth" == 2);
    assert(product#"truncation"#"highestWeight" === 2*lambda);
    assert(#characterSupport product == 6);
    for beta in offsetsEfficiencyProduct 2 do (
        mu := 2*lambda-toWeight kmRoot(D,beta);
        assert(characterCoefficient(product,mu) == expected#beta);
        assert(characterCoefficient(signed,mu) == -6*expected#beta);
    );
    assert(characterCoefficient(product,
        2*lambda+toWeight kmRoot(D,{1,0})) == 0);
    assert(mustErrorEfficiencyProduct(() -> characterCoefficient(product,
        2*lambda-toWeight kmRoot(D,{3,0}))));

    -- Cancellation is still a partial zero, not proof of a globally zero
    -- completed character; multiplication must retain its certified range.
    cancelled := shallow-shallow;
    emptyProduct := cancelled*deep;
    assert(#characterSupport emptyProduct == 0);
    assert(not isExactCharacter emptyProduct);
    assert(emptyProduct#"truncation"#"completeThroughDepth" == 2);
    assert(characterCoefficient(emptyProduct,
        2*lambda-toWeight kmRoot(D,{1,1})) == 0);
    assert(mustErrorEfficiencyProduct(() -> characterCoefficient(emptyProduct,
        2*lambda-toWeight kmRoot(D,{3,0}))));
    exactZeroProduct := (0*shallow)*deep;
    assert(isExactCharacter exactZeroProduct);
    assert(#characterSupport exactZeroProduct == 0);
    assert(characterCoefficient(exactZeroProduct,
        2*lambda-toWeight kmRoot(D,{3,0})) == 0);

    depthZeroProduct := moduleCharacter(M,Depth=>0)*deep;
    assert(characterTerms depthZeroProduct == new Tally from {(2*lambda)=>1});
    assert(depthZeroProduct#"truncation"#"completeThroughDepth" == 0);
);

-- A complete finite irreducible supplies every coefficient of an independent
-- convolution oracle for unequal integrable prefixes, including signed ones.
DEfficiencyFinite = rootDatum "A2";
lEfficiencyFinite = kmWeight(DEfficiencyFinite,{2,1});
MEfficiencyFinite = integrableHighestWeightModule(DEfficiencyFinite,lEfficiencyFinite);
FEfficiencyFinite = finiteCharacter(DEfficiencyFinite,lEfficiencyFinite);
C2EfficiencyFinite = moduleCharacter(MEfficiencyFinite,Depth=>2);
C12EfficiencyFinite = moduleCharacter(MEfficiencyFinite,Depth=>12);
PEfficiencyFinite = C2EfficiencyFinite*C12EfficiencyFinite;
oracleEfficiencyFinite = FEfficiencyFinite*FEfficiencyFinite;
for betaEfficiencyFinite in offsetsEfficiencyProduct 2 do (
    muEfficiencyFinite := 2*lEfficiencyFinite-toWeight kmRoot(
        DEfficiencyFinite,betaEfficiencyFinite);
    assert(characterCoefficient(PEfficiencyFinite,muEfficiencyFinite) ==
        characterCoefficient(oracleEfficiencyFinite,muEfficiencyFinite));
    assert(characterCoefficient(((-3)*C2EfficiencyFinite)*C12EfficiencyFinite,
        muEfficiencyFinite) == -3*characterCoefficient(
            oracleEfficiencyFinite,muEfficiencyFinite));
);
for pEfficiencyFinite in pairs characterTerms PEfficiencyFinite do
    assert(pEfficiencyFinite#1 == characterCoefficient(
        oracleEfficiencyFinite,pEfficiencyFinite#0));

-- Exact times partial is not the bounded-prefix branch.  A large monomial
-- shift and transformed source must retain every transported known term.
wEfficiencyFinite = weylElement(weylGroup DEfficiencyFinite,{1,2});
WEfficiencyFinite = weylAction(wEfficiencyFinite,C2EfficiencyFinite);
WOracleEfficiencyFinite = weylAction(wEfficiencyFinite,FEfficiencyFinite);
shiftEfficiencyFinite = -toWeight kmRoot(DEfficiencyFinite,{17,0});
shiftFactorEfficiencyFinite = formalCharacter(DEfficiencyFinite,
    new Tally from {shiftEfficiencyFinite=>2});
shiftProductEfficiencyFinite = shiftFactorEfficiencyFinite*WEfficiencyFinite;
shiftOracleEfficiencyFinite = shiftFactorEfficiencyFinite*WOracleEfficiencyFinite;
assert(#characterSupport shiftProductEfficiencyFinite == #characterSupport WEfficiencyFinite);
assert(characterTerms shiftProductEfficiencyFinite ==
    characterTerms(WEfficiencyFinite*shiftFactorEfficiencyFinite));
for pEfficiencyFinite in pairs characterTerms shiftProductEfficiencyFinite do
    assert(pEfficiencyFinite#1 == characterCoefficient(
        shiftOracleEfficiencyFinite,pEfficiencyFinite#0));
assert(characterCoefficient(shiftProductEfficiencyFinite,
    shiftEfficiencyFinite+weylAction(wEfficiencyFinite,lEfficiencyFinite)) == 2);
assert(mustErrorEfficiencyProduct(() -> characterCoefficient(
    shiftProductEfficiencyFinite,shiftEfficiencyFinite+weylAction(
        wEfficiencyFinite,lEfficiencyFinite-toWeight kmRoot(DEfficiencyFinite,{3,0})))));
dualProductEfficiencyFinite = shiftFactorEfficiencyFinite*dualCharacter C2EfficiencyFinite;
dualOracleEfficiencyFinite = shiftFactorEfficiencyFinite*dualCharacter FEfficiencyFinite;
for pEfficiencyFinite in pairs characterTerms dualProductEfficiencyFinite do
    assert(pEfficiencyFinite#1 == characterCoefficient(
        dualOracleEfficiencyFinite,pEfficiencyFinite#0));

///

TEST ///

mustErrorEfficiencyPower = f -> (try (f(); false) else true);
RPowerEfficiency = realization(cartanDatum "A1",
    matrix(QQ,{{2,1}}),matrix(QQ,{{1,0}}));
DPowerEfficiency = rootDatum RPowerEfficiency;
fullPowerEfficiency = weightFromRealizationCoordinates(DPowerEfficiency,{1/2,7/3});
classPowerEfficiency = kmWeight(DPowerEfficiency,{1/2});

-- Million-degree single-weight powers reduce to a single binomial number,
-- without constructing intermediate tables.  Full/class modes both survive.
for muPowerEfficiency in {fullPowerEfficiency,classPowerEfficiency} do (
    C := formalCharacter(DPowerEfficiency,new Tally from {muPowerEfficiency=>101});
    k := 1000000;
    Sym := symmetricPowerCharacter(C,k);
    assert(characterTerms Sym == new Tally from {
        (k*muPowerEfficiency)=>binomial(100+k,k)});
    assert(isExactCharacter Sym);
    assert(characterTerms symmetricPowerCharacter(C,0) ==
        new Tally from {(0*muPowerEfficiency)=>1});
    assert(characterTerms exteriorPowerCharacter(C,0) ==
        new Tally from {(0*muPowerEfficiency)=>1});

    many := formalCharacter(DPowerEfficiency,
        new Tally from {muPowerEfficiency=>1000005});
    Ext := exteriorPowerCharacter(many,k);
    assert(characterTerms Ext == new Tally from {
        (k*muPowerEfficiency)=>binomial(1000005,k)});
    assert(isExactCharacter Ext);
    assert(characterTerms exteriorPowerCharacter(C,101) ==
        new Tally from {(101*muPowerEfficiency)=>1});
    assert(#characterSupport exteriorPowerCharacter(C,k) == 0);
);
fullSymEfficiency = symmetricPowerCharacter(formalCharacter(DPowerEfficiency,
    new Tally from {fullPowerEfficiency=>2}),3);
assert(realizationCoordinates(first characterSupport fullSymEfficiency) ===
    {3/2,7});
classSymEfficiency = symmetricPowerCharacter(formalCharacter(DPowerEfficiency,
    new Tally from {classPowerEfficiency=>2}),3);
assert(mustErrorEfficiencyPower(() ->
    realizationCoordinates(first characterSupport classSymEfficiency)));

-- The top exterior power is the determinant monomial even when there are
-- many repeated copies of multiple weights.  No intermediate degrees are
-- needed, and rational central coordinates are part of the determinant.
otherFullPowerEfficiency = weightFromRealizationCoordinates(DPowerEfficiency,
    {-3/2,11/5});
determinantInputEfficiency = formalCharacter(DPowerEfficiency,new Tally from {
    fullPowerEfficiency=>10000,otherFullPowerEfficiency=>20000});
determinantWeightEfficiency = 10000*fullPowerEfficiency+20000*otherFullPowerEfficiency;
assert(characterTerms exteriorPowerCharacter(determinantInputEfficiency,30000) ==
    new Tally from {determinantWeightEfficiency=>1});
assert(realizationCoordinates determinantWeightEfficiency === {-25000,202000/3});
assert(#characterSupport exteriorPowerCharacter(determinantInputEfficiency,30001) == 0);
standardDeterminantEfficiency = exteriorPowerCharacter(
    finiteCharacter(rootDatum "A2",{1,0}),3);
assert(characterDimension standardDeterminantEfficiency == 1);
assert(characterCoefficient(standardDeterminantEfficiency,
    kmWeight(characterDatum standardDeterminantEfficiency,{0,0})) == 1);

-- The degree-above-dimension guard also handles multiweight characters and
-- exact zero, and preserves the usual degree-zero unit.
VPowerEfficiency = finiteCharacter(rootDatum "A2",{1,0});
assert(isExactCharacter exteriorPowerCharacter(VPowerEfficiency,1000000));
assert(characterDimension exteriorPowerCharacter(VPowerEfficiency,1000000) == 0);
zeroPowerEfficiency = 0*VPowerEfficiency;
assert(#characterSupport symmetricPowerCharacter(zeroPowerEfficiency,1000000) == 0);
assert(#characterSupport exteriorPowerCharacter(zeroPowerEfficiency,1000000) == 0);
assert(characterDimension symmetricPowerCharacter(zeroPowerEfficiency,0) == 1);
assert(characterDimension exteriorPowerCharacter(zeroPowerEfficiency,0) == 1);

-- Early-return cases must not bypass input validation.
virtualPowerEfficiency = formalCharacter(DPowerEfficiency,
    new Tally from {fullPowerEfficiency=>-1});
partialPowerEfficiency = moduleCharacter(vermaModule(DPowerEfficiency,
    weightFromRealizationCoordinates(DPowerEfficiency,{1,7})),Depth=>0);
for CPowerInvalidEfficiency in {virtualPowerEfficiency,partialPowerEfficiency} do (
    assert(mustErrorEfficiencyPower(() ->
        symmetricPowerCharacter(CPowerInvalidEfficiency,0)));
    assert(mustErrorEfficiencyPower(() ->
        exteriorPowerCharacter(CPowerInvalidEfficiency,0)));
    assert(mustErrorEfficiencyPower(() ->
        exteriorPowerCharacter(CPowerInvalidEfficiency,1000000)));
);
assert(mustErrorEfficiencyPower(() -> symmetricPowerCharacter(zeroPowerEfficiency,-1)));
assert(mustErrorEfficiencyPower(() -> exteriorPowerCharacter(zeroPowerEfficiency,-1)));
assert(mustErrorEfficiencyPower(() -> symmetricPowerCharacter(zeroPowerEfficiency,1/2)));
assert(mustErrorEfficiencyPower(() -> exteriorPowerCharacter(zeroPowerEfficiency,1/2)));

///

TEST ///

mustErrorEfficiencyLevi = f -> (try (f(); false) else true);
DLeviEfficiency = rootDatum "A2";
CLeviEfficiency = finiteCharacter(DLeviEfficiency,{2,1});
partialLeviEfficiency = moduleCharacter(integrableHighestWeightModule(
    DLeviEfficiency,{2,1}),Depth=>0);
for nodesLeviEfficiency in {{},{1},{2},{1,2}} do (
    L := reductiveLeviDatum(standardParabolic(DLeviEfficiency,
        LeviNodes=>nodesLeviEfficiency),DLeviEfficiency);
    assert(characterDimension restrictCharacter(L,CLeviEfficiency) ==
        characterDimension CLeviEfficiency);
    assert(characterDimension restrictCharacter(L,(-2)*CLeviEfficiency) ==
        -2*characterDimension CLeviEfficiency);
    assert(characterDimension restrictCharacter(L,0*CLeviEfficiency) == 0);
    assert(mustErrorEfficiencyLevi(() ->
        characterDimension restrictCharacter(L,partialLeviEfficiency)));
);

-- Both pure tori and the zero-dimensional torus have rank-zero derived
-- Levi factors; ambient full coordinates still give the correct dimension.
for dimensionLeviEfficiency in {0,2} do (
    A := matrix mutableMatrix(ZZ,0,0);
    E := matrix mutableMatrix(QQ,0,dimensionLeviEfficiency);
    D := rootDatum realization(cartanDatum A,E,E);
    lambda := weightFromRealizationCoordinates(D,
        take({2/3,-7/5},dimensionLeviEfficiency));
    C := finiteCharacter(D,lambda);
    L := reductiveLeviDatum(standardParabolic(D,LeviNodes=>{}),D);
    RC := restrictCharacter(L,C);
    assert(characterDimension RC == characterDimension C);
    assert(characterDimension RC == 1);
    assert(mustErrorEfficiencyLevi(() -> characterDimension restrictCharacter(L,
        moduleCharacter(vermaModule(D,lambda),Depth=>0))));
);

///


-------------------------------------------------------------------------
-- Affine Character Oracle Tests
-------------------------------------------------------------------------

-- An independent lattice/Fock-space oracle for the level-one A1^(1)
-- characters, not another application of Freudenthal or the BGG formula.
-- For Lambda0 + m*alpha1 - n*delta the multiplicity is p(n-m^2),
-- where p is the ordinary partition function and p(k)=0 for k<0.
-- See J.-H. Yang, arXiv:math/0612474, Example 2.3, for the equivalent
-- basic-representation multiplicity p(1-(alpha,alpha)/2).
-- The other fundamental module follows by interchanging the two nodes.
TEST ///

mustErrorAffineOracle = f -> (try (f(); false) else true);
DAffineOracle = rootDatum cartanAffineA 1;
deltaAffineOracle = toWeight affineNullRoot DAffineOracle;
partitionAffineOracle = {1,1,2,3,5,7,11};
HAffineOracle = 12;
for highestNodeAffineOracle in {1,2} do (
    lambda := weightFromRealizationCoordinates(DAffineOracle,
        if highestNodeAffineOracle == 1 then {1,0,0} else {0,1,0});
    C := moduleCharacter(integrableHighestWeightModule(DAffineOracle,lambda),
        Depth=>HAffineOracle);
    expected := new MutableHashTable;
    for h from 0 to HAffineOracle do for a from 0 to h do (
        b := h-a;
        beta := if highestNodeAffineOracle == 1 then {a,b} else {b,a};
        -- beta = n*delta - m*the other simple root.
        n := a;
        m := a-b;
        oscillatorDegree := n-m^2;
        multiplicity := if oscillatorDegree < 0 then 0
            else partitionAffineOracle#oscillatorDegree;
        mu := lambda-toWeight kmRoot(DAffineOracle,beta);
        assert(characterCoefficient(C,mu) == multiplicity);
        if multiplicity > 0 then expected#mu = multiplicity;
    );
    assert(characterTerms C == new Tally from pairs expected);
    assert(#characterSupport C == 24);
    for n from 0 to 6 do
        assert(characterCoefficient(C,lambda-n*deltaAffineOracle) ==
            partitionAffineOracle#n);
    assert(mustErrorAffineOracle(() ->
        characterCoefficient(C,lambda-7*deltaAffineOracle)));

    -- A null-root translation changes the complete exponents, not their
    -- multiplicities or the relative depth bound.
    shifted := moduleCharacter(integrableHighestWeightModule(
        DAffineOracle,lambda+2*deltaAffineOracle),Depth=>HAffineOracle);
    translatedTerms := new Tally from apply(pairs expected,p ->
        (p#0+2*deltaAffineOracle)=>p#1);
    assert(characterTerms shifted == translatedTerms);
);

///


-------------------------------------------------------------------------
-- Character Arithmetic Regression Tests
-------------------------------------------------------------------------

-- Ordinary powers are powers in the weight group algebra, not symmetric
-- powers.  Parent checks must preserve the chosen realization and lattice.

TEST ///

mustErrorArithmeticPower = f -> (try (f(); false) else true);
DArithmeticPower = rootDatum "A1";
CArithmeticPower = finiteCharacter(DArithmeticPower,{1});

-- The k-th tensor power of the two-dimensional A1 module has binomial
-- coefficients at weights k,k-2,...,-k.  This is an independent formula
-- for every coefficient, including signed and virtual characters.
for kArithmeticPower in {0,1,2,3,10} do (
    P := CArithmeticPower^kArithmeticPower;
    oracle := new Tally from apply(toList(0..kArithmeticPower),j ->
        kmWeight(DArithmeticPower,{kArithmeticPower-2*j}) =>
            binomial(kArithmeticPower,j));
    assert(characterTerms P == oracle);
    assert(isExactCharacter P);
    assert(characterDimension P == 2^kArithmeticPower);
    assert(characterTerms((-CArithmeticPower)^kArithmeticPower) ==
        characterTerms(((-1)^kArithmeticPower)*P));
);
assert(characterTerms(CArithmeticPower^1) == characterTerms CArithmeticPower);
assert(characterTerms(CArithmeticPower^3) ==
    characterTerms(CArithmeticPower*CArithmeticPower*CArithmeticPower));
assert(characterTerms(CArithmeticPower^2) =!=
    characterTerms symmetricPowerCharacter(CArithmeticPower,2));

-- An ordinary character power is meaningful for virtual characters too.
vArithmeticPower = formalCharacter(DArithmeticPower,new Tally from {
    kmWeight(DArithmeticPower,{1})=>1,kmWeight(DArithmeticPower,{-1})=>-1});
assert(characterTerms(vArithmeticPower^3) == new Tally from {
    kmWeight(DArithmeticPower,{3})=>1,kmWeight(DArithmeticPower,{1})=>-3,
    kmWeight(DArithmeticPower,{-1})=>3,kmWeight(DArithmeticPower,{-3})=>-1});

-- A large exponent must not require a linear number of multiplications.
muArithmeticPower = kmWeight(DArithmeticPower,{2/3});
monoArithmeticPower = formalCharacter(DArithmeticPower,
    new Tally from {muArithmeticPower=>-1});
assert(characterTerms(monoArithmeticPower^1000000001) ==
    new Tally from {(1000000001*muArithmeticPower)=>-1});
threeArithmeticPower = formalCharacter(DArithmeticPower,
    new Tally from {muArithmeticPower=>3});
assert(characterTerms(threeArithmeticPower^64) ==
    new Tally from {(64*muArithmeticPower)=>3^64});

zeroArithmeticPower = 0*CArithmeticPower;
assert(characterTerms(zeroArithmeticPower^0) ==
    new Tally from {kmWeight(DArithmeticPower,{0})=>1});
assert(isExactCharacter(zeroArithmeticPower^0));
assert(#characterSupport(zeroArithmeticPower^1) == 0);
assert(#characterSupport(zeroArithmeticPower^1000000000) == 0);
assert(mustErrorArithmeticPower(() -> CArithmeticPower^(-1)));
assert(mustErrorArithmeticPower(() -> CArithmeticPower^(1/2)));

-- Rational full coordinates and central directions remain in the exponent.
RArithmeticPower = realization(cartanDatum "A1",
    matrix(QQ,{{2,1,0}}),matrix(QQ,{{1,0,0}}));
DExtArithmeticPower = rootDatum RArithmeticPower;
fullArithmeticPower = weightFromRealizationCoordinates(DExtArithmeticPower,
    {1/2,7/3,-4/5});
classArithmeticPower = kmWeight(DExtArithmeticPower,{1/2});
for wtArithmeticPower in {fullArithmeticPower,classArithmeticPower} do (
    C := formalCharacter(DExtArithmeticPower,new Tally from {wtArithmeticPower=>2});
    for k in {0,1,3} do (
        P := C^k;
        assert(characterTerms P == new Tally from {(k*wtArithmeticPower)=>2^k});
        assert(isExactCharacter P);
    );
);
assert(realizationCoordinates(first characterSupport(
    (formalCharacter(DExtArithmeticPower,new Tally from {fullArithmeticPower=>2}))^3))
    === {3/2,7,-12/5});
assert(mustErrorArithmeticPower(() -> realizationCoordinates(
    first characterSupport((formalCharacter(DExtArithmeticPower,
        new Tally from {classArithmeticPower=>2}))^0))));

-- A pure torus, including the zero-dimensional one, has an honest unit.
for torusDimensionArithmeticPower in {0,2} do (
    A := matrix mutableMatrix(ZZ,0,0);
    E := matrix mutableMatrix(QQ,0,torusDimensionArithmeticPower);
    D := rootDatum realization(cartanDatum A,E,E);
    mu := weightFromRealizationCoordinates(D,
        take({2/3,-7/5},torusDimensionArithmeticPower));
    C := formalCharacter(D,new Tally from {mu=>2});
    for k in {0,1,3} do
        assert(characterTerms(C^k) == new Tally from {(k*mu)=>2^k});
);

///

TEST ///

mustErrorArithmeticParents = f -> (try (f(); false) else true);

-- Equal immutable root data can be constructed independently.  Character
-- arithmetic should not mistake object identity for mathematical equality.
DLeftArithmeticParents = rootDatum matrix{{2,-1},{-1,2}};
DRightArithmeticParents = rootDatum matrix{{2,-1},{-1,2}};
assert(DLeftArithmeticParents === DRightArithmeticParents);
CLeftArithmeticParents = finiteCharacter(DLeftArithmeticParents,{1,0});
CRightArithmeticParents = finiteCharacter(DRightArithmeticParents,{0,1});
CExpectedArithmeticParents = finiteCharacter(DLeftArithmeticParents,{0,1});
assert(characterTerms(CLeftArithmeticParents+CRightArithmeticParents) ==
    characterTerms(CLeftArithmeticParents+CExpectedArithmeticParents));
assert(characterTerms(CLeftArithmeticParents-CRightArithmeticParents) ==
    characterTerms(CLeftArithmeticParents-CExpectedArithmeticParents));
assert(characterTerms(CLeftArithmeticParents*CRightArithmeticParents) ==
    characterTerms(CLeftArithmeticParents*CExpectedArithmeticParents));

-- A same-rank but different Cartan matrix is not the same datum.  Even the
-- zero character retains its parent, so zero must not bypass this check.
CDifferentArithmeticParents = finiteCharacter(rootDatum "B2",{1,0});
for otherArithmeticParents in {
    CDifferentArithmeticParents,0*CDifferentArithmeticParents} do (
    assert(mustErrorArithmeticParents(() ->
        CLeftArithmeticParents+otherArithmeticParents));
    assert(mustErrorArithmeticParents(() ->
        CLeftArithmeticParents-otherArithmeticParents));
    assert(mustErrorArithmeticParents(() ->
        CLeftArithmeticParents*otherArithmeticParents));
);

-- Root data also remember node labels, the full realization, and the
-- selected global form.  No implicit relabeling or change of lattice occurs.
DNamedArithmeticParents = rootDatum cartanDatum(matrix{{2}},NodeLabels=>{"x"});
DRelabeledArithmeticParents = rootDatum cartanDatum(matrix{{2}},NodeLabels=>{"y"});
RFirstArithmeticParents = realization(cartanDatum "A1",
    matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
RSecondArithmeticParents = realization(cartanDatum "A1",
    matrix(QQ,{{2,1}}),matrix(QQ,{{1,0}}));
DFirstArithmeticParents = rootDatum RFirstArithmeticParents;
DSecondArithmeticParents = rootDatum RSecondArithmeticParents;
for parentPairArithmeticParents in {
    {DNamedArithmeticParents,DRelabeledArithmeticParents},
    {DFirstArithmeticParents,DSecondArithmeticParents},
    {rootDatum("A1",GlobalForm=>"SimplyConnected"),
        rootDatum("A1",GlobalForm=>"Adjoint")}
} do (
    D := parentPairArithmeticParents#0;
    E := parentPairArithmeticParents#1;
    C := finiteCharacter(D,{2});
    F := finiteCharacter(E,{2});
    assert(mustErrorArithmeticParents(() -> C+F));
    assert(mustErrorArithmeticParents(() -> C-F));
    assert(mustErrorArithmeticParents(() -> C*F));
);

-- Identical Dynkin labels do not allow mixing full weights with their
-- quotient classes when there are invisible realization coordinates.
fullArithmeticParents = weightFromRealizationCoordinates(
    DFirstArithmeticParents,{1,3});
classArithmeticParents = kmWeight(DFirstArithmeticParents,{1});
CFullArithmeticParents = formalCharacter(DFirstArithmeticParents,
    new Tally from {fullArithmeticParents=>1});
CClassArithmeticParents = formalCharacter(DFirstArithmeticParents,
    new Tally from {classArithmeticParents=>1});
for fullPowerArithmeticParents in {CFullArithmeticParents,CFullArithmeticParents^0} do (
    assert(mustErrorArithmeticParents(() -> fullPowerArithmeticParents+CClassArithmeticParents));
    assert(mustErrorArithmeticParents(() -> fullPowerArithmeticParents-CClassArithmeticParents));
    assert(mustErrorArithmeticParents(() -> fullPowerArithmeticParents*CClassArithmeticParents));
);

-- A neutral empty table acquires no artificial coordinate-mode restriction.
zeroArithmeticParents = 0*CFullArithmeticParents;
assert(characterTerms(zeroArithmeticParents+CClassArithmeticParents) ==
    characterTerms CClassArithmeticParents);
assert(characterTerms(zeroArithmeticParents+CFullArithmeticParents) ==
    characterTerms CFullArithmeticParents);

-- Compatibility decisions, including refusals, are retained separately
-- from character coefficients.  Reuse must not suppress fresh mode checks.
clearInfrastructureCaches();
assert((infrastructureCacheInfo())#"characterParents" == 0);
cacheSumArithmeticParents = CLeftArithmeticParents+CRightArithmeticParents;
assert((infrastructureCacheInfo())#"characterParents" == 1);
cacheProductArithmeticParents = CLeftArithmeticParents*CRightArithmeticParents;
cacheDifferenceArithmeticParents = CLeftArithmeticParents-CRightArithmeticParents;
assert((infrastructureCacheInfo())#"characterParents" == 1);
assert(mustErrorArithmeticParents(() ->
    CLeftArithmeticParents+CDifferentArithmeticParents));
assert((infrastructureCacheInfo())#"characterParents" == 2);
assert(mustErrorArithmeticParents(() ->
    CLeftArithmeticParents*CDifferentArithmeticParents));
assert((infrastructureCacheInfo())#"characterParents" == 2);
assert(mustErrorArithmeticParents(() ->
    CDifferentArithmeticParents+CLeftArithmeticParents));
assert((infrastructureCacheInfo())#"characterParents" == 2);
cacheFullArithmeticParents = CFullArithmeticParents+CFullArithmeticParents;
cacheCountArithmeticParents = (infrastructureCacheInfo())#"characterParents";
assert(mustErrorArithmeticParents(() -> CFullArithmeticParents+CClassArithmeticParents));
assert(mustErrorArithmeticParents(() -> CFullArithmeticParents*CClassArithmeticParents));
assert((infrastructureCacheInfo())#"characterParents" == cacheCountArithmeticParents);
clearInfrastructureCaches();
assert((infrastructureCacheInfo())#"characterParents" == 0);
assert(characterTerms(CLeftArithmeticParents+CRightArithmeticParents) ==
    characterTerms cacheSumArithmeticParents);
assert((infrastructureCacheInfo())#"characterParents" == 1);

///

TEST ///

mustErrorArithmeticPartial = f -> (try (f(); false) else true);

-- A1 Verma coefficients are all one along the negative simple-root ray.
-- Its k-th power has coefficient binomial(k+d-1,d) at root depth d.
DArithmeticPartial = rootDatum "A1";
lambdaArithmeticPartial = kmWeight(DArithmeticPartial,{1});
CArithmeticPartial = moduleCharacter(vermaModule(DArithmeticPartial,
    lambdaArithmeticPartial),Depth=>4);
for kArithmeticPartial in {1,2,3,8} do (
    P := CArithmeticPartial^kArithmeticPartial;
    assert(not isExactCharacter P);
    assert(P#"truncation"#"completeThroughDepth" == 4);
    assert(P#"truncation"#"highestWeight" === kArithmeticPartial*lambdaArithmeticPartial);
    for depth in toList(0..4) do (
        mu := kArithmeticPartial*lambdaArithmeticPartial-
            toWeight kmRoot(DArithmeticPartial,{depth});
        assert(characterCoefficient(P,mu) == binomial(kArithmeticPartial+depth-1,depth));
    );
    assert(mustErrorArithmeticPartial(() -> characterCoefficient(P,
        kArithmeticPartial*lambdaArithmeticPartial-
            toWeight kmRoot(DArithmeticPartial,{5}))));
);
assert(isExactCharacter(CArithmeticPartial^0));
assert(characterTerms(CArithmeticPartial^0) ==
    new Tally from {(0*lambdaArithmeticPartial)=>1});
signedArithmeticPartial = (-2)*CArithmeticPartial;
assert(characterTerms(signedArithmeticPartial^3) ==
    characterTerms((-8)*(CArithmeticPartial^3)));
cancelledArithmeticPartial = CArithmeticPartial-CArithmeticPartial;
assert(not isExactCharacter(cancelledArithmeticPartial^2));
assert(#characterSupport(cancelledArithmeticPartial^2) == 0);
assert((cancelledArithmeticPartial^2)#"truncation"#"completeThroughDepth" == 4);
assert(isExactCharacter(cancelledArithmeticPartial^0));

-- At alpha0+alpha1 the affine and indefinite rank-two Verma coefficients
-- are two.  The k-th power therefore has coefficient k*(k+1), while each
-- simple-root coefficient is k and each doubled-simple coefficient k(k+1)/2.
DAffArithmeticPartial = rootDatum cartanAffineA 1;
for fixtureArithmeticPartial in {
    {DAffArithmeticPartial,
        weightFromRealizationCoordinates(DAffArithmeticPartial,{1,0,0})},
    {rootDatum matrix{{2,-3},{-3,2}},{1,1}}
} do (
    D := fixtureArithmeticPartial#0;
    lambda := if instance(fixtureArithmeticPartial#1,KMWeight)
        then fixtureArithmeticPartial#1 else kmWeight(D,fixtureArithmeticPartial#1);
    C := moduleCharacter(vermaModule(D,lambda),Depth=>2);
    for k in {0,1,2,3} do (
        P := C^k;
        if k == 0 then (
            assert(isExactCharacter P);
            assert(characterTerms P == new Tally from {(0*lambda)=>1});
        ) else (
            assert(not isExactCharacter P);
            assert(P#"truncation"#"completeThroughDepth" == 2);
            for beta in {{0,0},{1,0},{0,1},{2,0},{0,2},{1,1}} do (
                expected := if sum beta == 0 then 1 else
                    if sum beta == 1 then k else
                    if beta === {1,1} then k*(k+1) else binomial(k+1,2);
                assert(characterCoefficient(P,k*lambda-toWeight kmRoot(D,beta)) == expected);
            );
        );
    );
);

-- Weyl and dual transformations transport known coefficients, but need
-- not be expansions in the same positive root cone.  The safe behavior is
-- identity/unit powers, with higher powers refused rather than invented.
wArithmeticPartial = weylElement(weylGroup DArithmeticPartial,{1});
for transformedArithmeticPartial in {
    weylAction(wArithmeticPartial,CArithmeticPartial),
    dualCharacter CArithmeticPartial
} do (
    assert(isExactCharacter(transformedArithmeticPartial^0));
    assert(characterTerms(transformedArithmeticPartial^1) ==
        characterTerms transformedArithmeticPartial);
    assert(not isExactCharacter(transformedArithmeticPartial^1));
    assert(mustErrorArithmeticPartial(() -> transformedArithmeticPartial^2));
    assert(mustErrorArithmeticPartial(() -> transformedArithmeticPartial^3));
);

///


-------------------------------------------------------------------------
-- Character Conversion Regression Tests
-------------------------------------------------------------------------

-- The irreducible-character basis and the weight-monomial basis must give
-- inverse descriptions of a genuine finite-dimensional character.
TEST ///

checkCharacterConversion = C -> (
    D := characterDatum C;
    T := decomposeCharacter C;
    restored := characterFromDecomposition(D,T);
    assert(isExactCharacter restored);
    assert(characterTerms restored == characterTerms C);
    assert(characterDimension restored == characterDimension C);
    assert(decomposeCharacter restored == T);
    if #keys T > 0 then (
        inferred := characterFromDecomposition T;
        assert(characterTerms inferred == characterTerms C);
        assert(decomposeCharacter inferred == T);
    );
    true
);

checkDecompositionConversion = (D,T) -> (
    canonical := new Tally from select(pairs T,p -> p#1 != 0);
    C := characterFromDecomposition(D,T);
    assert(isExactCharacter C);
    assert(decomposeCharacter C == canonical);
    assert(checkCharacterConversion C);
    assert(characterTerms characterFromDecomposition(D,
        new HashTable from reverse pairs T) == characterTerms C);
    true
);

-- Independent A1 tensor-power multiplicities.  The formula is the
-- difference of consecutive binomial coefficients, rather than a result
-- obtained by running the decomposition algorithm a second time.
DA1Conversion = rootDatum "A1";
CA1Conversion = finiteCharacter(DA1Conversion,{1});
for n from 0 to 8 do (
    expected := new Tally from for j from 0 to n//2 list
        (kmWeight(DA1Conversion,{n-2*j})) =>
            (binomial(n,j)-(if j == 0 then 0 else binomial(n,j-1)));
    C := CA1Conversion^n;
    assert(characterDimension C == 2^n);
    assert(decomposeCharacter C == expected);
    assert(checkCharacterConversion C);
    assert(checkDecompositionConversion(DA1Conversion,expected));
);

DA2Conversion = rootDatum "A2";
CA2Conversion = finiteCharacter(DA2Conversion,{2,1});
assert(characterDimension CA2Conversion == 15);
expectedA2SquareConversion = new Tally from apply({
    {{0,1},1},{{0,4},1},{{1,2},2},{{2,0},1},
    {{2,3},1},{{3,1},2},{{4,2},1},{{5,0},1}},
    p -> (kmWeight(DA2Conversion,p#0))=>p#1);
assert(decomposeCharacter(CA2Conversion^2) == expectedA2SquareConversion);
assert(checkDecompositionConversion(DA2Conversion,expectedA2SquareConversion));
SA2Conversion = symmetricPowerCharacter(CA2Conversion,3);
EA2Conversion = exteriorPowerCharacter(CA2Conversion,3);
assert(characterDimension SA2Conversion == binomial(17,3));
assert(characterDimension EA2Conversion == binomial(15,3));
assert(#keys decomposeCharacter SA2Conversion == 12);
assert((decomposeCharacter SA2Conversion)#(kmWeight(DA2Conversion,{0,0})) == 1);
for C in {CA2Conversion^2,SA2Conversion,EA2Conversion,
    CA2Conversion*dualCharacter CA2Conversion} do
    assert(checkCharacterConversion C);

-- Reducible root systems and the two root lengths in B2 and G2 must not
-- introduce coordinate-orientation assumptions into either conversion.
DProductConversion = rootDatum diagonalMatrix{2,2};
expectedProductConversion = new Tally from flatten(
    for a in {0,2} list for b in {0,2} list
        (kmWeight(DProductConversion,{a,b}))=>1);
assert(decomposeCharacter((finiteCharacter(DProductConversion,{1,1}))^2)
    == expectedProductConversion);
assert(checkDecompositionConversion(DProductConversion,expectedProductConversion));
for fixture in {{cartanB 2,{{0,0},{2,0},{0,2}}},
    {cartanG2(),{{0,0},{1,0},{0,1},{2,0}}}} do (
    D := rootDatum fixture#0;
    expected := new Tally from apply(fixture#1,lambda -> (kmWeight(D,lambda))=>1);
    C := (finiteCharacter(D,{1,0}))^2;
    assert(decomposeCharacter C == expected);
    assert(checkCharacterConversion C);
    assert(checkDecompositionConversion(D,expected));
);

///

TEST ///

checkFullConversion = (D,T) -> (
    C := characterFromDecomposition(D,T);
    assert(isExactCharacter C);
    assert(decomposeCharacter C == new Tally from select(pairs T,p -> p#1 != 0));
    assert(characterTerms characterFromDecomposition decomposeCharacter C
        == characterTerms C);
    assert(characterTerms characterFromDecomposition T == characterTerms C);
    assert(all(keys characterTerms C,mu -> parentDatum mu === D));
    true
);

-- The two highest weights have equal Dynkin labels but genuinely
-- different central characters; neither conversion may identify them.
RCentralConversion = realization(cartanDatum "A1",
    matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
DCentralConversion = rootDatum RCentralConversion;
centralWeightConversion = v -> weightFromRealizationCoordinates(DCentralConversion,v);
TCentralConversion = new Tally from {
    (centralWeightConversion{1,0})=>2,
    (centralWeightConversion{1,3})=>3,
    (centralWeightConversion{4,-2})=>1,
    (centralWeightConversion{2,-2})=>2,
    (centralWeightConversion{0,7/5})=>4};
assert(checkFullConversion(DCentralConversion,TCentralConversion));
CCentralConversion = characterFromDecomposition TCentralConversion;
for C in {CCentralConversion*finiteCharacter(DCentralConversion,
    centralWeightConversion{1,1}),dualCharacter CCentralConversion,
    exteriorPowerCharacter(characterFromDecomposition(DCentralConversion,
        new Tally from {(centralWeightConversion{1,0})=>1,
            (centralWeightConversion{1,3})=>1}),4)} do (
    T := decomposeCharacter C;
    assert(checkFullConversion(DCentralConversion,T));
    assert(characterTerms characterFromDecomposition T == characterTerms C);
);

-- Rational central coordinates are valid even though every highest
-- Dynkin label must be integral.  A skew basis makes the distinction
-- invisible to a test that merely inspects individual coordinate columns.
skewConversion = matrix(QQ,{{1/2,1,2},{0,2/3,1},{1,0,3}});
RSkewConversion = realization(cartanDatum "A1",
    matrix(QQ,{{2,0,0}})*skewConversion,
    matrix(QQ,{{1,0,0}})*transpose inverse skewConversion);
DSkewConversion = rootDatum RSkewConversion;
skewWeightConversion = v -> weightFromRealizationCoordinates(DSkewConversion,
    flatten entries(matrix(QQ,{v})*skewConversion));
TSkewConversion = new HashTable from {
    (skewWeightConversion{4,0,0})=>1,
    (skewWeightConversion{2,1/3,-4/5})=>3,
    (skewWeightConversion{2,1/3,7/5})=>2,
    (skewWeightConversion{0,-3/2,2/7})=>4};
assert(checkFullConversion(DSkewConversion,TSkewConversion));
classSkewConversion = new Tally from {
    (kmWeight(DSkewConversion,{4}))=>1,
    (kmWeight(DSkewConversion,{2}))=>2};
assert(checkFullConversion(DSkewConversion,classSkewConversion));

-- In rank zero every irreducible is a one-dimensional torus character.
-- Include the zero-dimensional datum and a torus with rational weights.
for d in {0,2} do (
    A := matrix mutableMatrix(ZZ,0,0);
    E := matrix mutableMatrix(QQ,0,d);
    D := rootDatum realization(cartanDatum A,E,E);
    zero := weightFromRealizationCoordinates(D,toList(d:0));
    T := new Tally from {zero=>5};
    if d == 2 then T = new Tally from {zero=>1,
        (weightFromRealizationCoordinates(D,{2/3,-3/5}))=>2,
        (weightFromRealizationCoordinates(D,{-1/2,4/7}))=>3};
    assert(checkFullConversion(D,T));
    C := characterFromDecomposition(D,T);
    assert(characterDimension C == sum values T);
    square := C^2;
    assert(characterTerms characterFromDecomposition(D,decomposeCharacter square)
        == characterTerms square);
    empty := characterFromDecomposition(D,new Tally);
    assert(isExactCharacter empty);
    assert(#keys characterTerms empty == 0);
    assert(decomposeCharacter empty == new Tally);
);

///

TEST ///

mustErrorConversion = f -> (try (f();false) else true);
DValidationConversion = rootDatum "A1";
uValidationConversion = kmWeight(DValidationConversion,{2});
emptyValidationConversion = characterFromDecomposition(DValidationConversion,new Tally);
assert(isExactCharacter emptyValidationConversion);
assert(characterDimension emptyValidationConversion == 0);
assert(#keys characterTerms emptyValidationConversion == 0);
assert(decomposeCharacter emptyValidationConversion == new Tally);
assert(mustErrorConversion(() -> characterFromDecomposition(new Tally)));
assert(mustErrorConversion(() -> characterFromDecomposition(new HashTable)));

-- Zero multiplicities disappear canonically, and either supported table
-- type gives the same result.  Integral rational input is normalized.
withZeroValidationConversion = new HashTable from {
    uValidationConversion=>0,(kmWeight(DValidationConversion,{0}))=>3_QQ};
assert(decomposeCharacter characterFromDecomposition(DValidationConversion,
    withZeroValidationConversion) == new Tally from {
        (kmWeight(DValidationConversion,{0}))=>3});
assert(characterDimension characterFromDecomposition(
    DValidationConversion,new Tally from {uValidationConversion=>0}) == 0);
assert(characterDimension characterFromDecomposition(
    new Tally from {uValidationConversion=>0}) == 0);

-- Independently constructed identical data are compatible.  A different
-- Cartan matrix, realization, or global form is not silently substituted.
identicalValidationConversion = rootDatum "A1";
assert(characterTerms characterFromDecomposition(DValidationConversion,
    new Tally from {(kmWeight(identicalValidationConversion,{2}))=>1})
    == characterTerms finiteCharacter(DValidationConversion,{2}));
wrongDValidationConversion = rootDatum "A2";
assert(mustErrorConversion(() -> characterFromDecomposition(DValidationConversion,
    new Tally from {(kmWeight(wrongDValidationConversion,{1,0}))=>1})));
adjointValidationConversion = rootDatum("A1",GlobalForm=>"Adjoint");
assert(mustErrorConversion(() -> characterFromDecomposition(DValidationConversion,
    new Tally from {(kmWeight(adjointValidationConversion,{2}))=>1})));
mixedParentsValidationConversion = new Tally from {
    uValidationConversion=>1,(kmWeight(adjointValidationConversion,{2}))=>1};
assert(mustErrorConversion(() -> characterFromDecomposition(DValidationConversion,
    mixedParentsValidationConversion)));
assert(mustErrorConversion(() -> characterFromDecomposition mixedParentsValidationConversion));
assert(mustErrorConversion(() -> characterFromDecomposition(DValidationConversion,
    new Tally from {(kmWeight(adjointValidationConversion,{2}))=>0})));
for c in {-1,1/2,"one"} do (
    T := new HashTable from {uValidationConversion=>c};
    assert(mustErrorConversion(() -> characterFromDecomposition(DValidationConversion,T)));
    assert(mustErrorConversion(() -> characterFromDecomposition T));
);
for labels in {{-1},{1/2}} do (
    T := new Tally from {(kmWeight(DValidationConversion,labels))=>1};
    assert(mustErrorConversion(() -> characterFromDecomposition(DValidationConversion,T)));
    assert(mustErrorConversion(() -> characterFromDecomposition T));
);
assert(mustErrorConversion(() -> characterFromDecomposition(
    DValidationConversion,new Tally from {{2}=>1})));
assert(mustErrorConversion(() -> characterFromDecomposition(new Tally from {{2}=>1})));
assert(mustErrorConversion(() -> characterFromDecomposition(DValidationConversion,{})));

-- A nonminimal realization distinguishes full weights from Dynkin-label
-- classes, and two incompatible descriptions cannot be merged in a sum.
RModesValidationConversion = realization(cartanDatum "A1",
    matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
DModesValidationConversion = rootDatum RModesValidationConversion;
fullValidationConversion = weightFromRealizationCoordinates(DModesValidationConversion,{1,0});
classValidationConversion = kmWeight(DModesValidationConversion,{1});
mixedValidationConversion = new Tally from {
    fullValidationConversion=>1,classValidationConversion=>1};
assert(mustErrorConversion(() -> characterFromDecomposition(DModesValidationConversion,
    mixedValidationConversion)));
assert(mustErrorConversion(() -> characterFromDecomposition mixedValidationConversion));
assert(mustErrorConversion(() -> characterFromDecomposition(DValidationConversion,
    new Tally from {fullValidationConversion=>1})));

for nonfinite in {rootDatum cartanAffineA 1,
    rootDatum matrix{{2,-3},{-3,2}}} do (
    T := new Tally from {(kmWeight(nonfinite,{1,0}))=>1};
    assert(mustErrorConversion(() -> characterFromDecomposition(nonfinite,T)));
    assert(mustErrorConversion(() -> characterFromDecomposition T));
    assert(mustErrorConversion(() -> characterFromDecomposition(nonfinite,new Tally)));
);

-- Levi highest weights belong to a different representation ring; their
-- semisimple tuples alone do not specify ambient highest weights.
DLeviValidationConversion = rootDatum "A2";
PLeviValidationConversion = standardParabolic(DLeviValidationConversion,RemovedNodes=>{1});
LLeviValidationConversion = reductiveLeviDatum(PLeviValidationConversion,DLeviValidationConversion);
leviValidationConversion = restrictWeight(LLeviValidationConversion,
    kmWeight(DLeviValidationConversion,{1,0}));
assert(mustErrorConversion(() -> characterFromDecomposition(DLeviValidationConversion,
    new Tally from {leviValidationConversion=>1})));
assert(mustErrorConversion(() -> characterFromDecomposition(
    new Tally from {leviValidationConversion=>1})));
TLeviValidationConversion = decomposeLeviCharacter restrictCharacter(
    LLeviValidationConversion,finiteCharacter(DLeviValidationConversion,{1,0}));
assert(mustErrorConversion(() -> characterFromDecomposition(TLeviValidationConversion)));

///


-------------------------------------------------------------------------
-- KLEfficiency Regression Tests
-------------------------------------------------------------------------

TEST ///

-- In a direct product, both the principal Bruhat interval cardinality and
-- the KL polynomial factor.  Each singular A3 factor has fourteen elements
-- below 3412 and principal polynomial 1+q.  The full interval has 196
-- elements: factoring the answer must not circumvent MaxElements.
mustErrorKL = f -> (try (f();false) else true);
q = kazhdanLusztigVariable();
A = (cartanA 4)++(cartanA 4);
W = weylGroup A;
e = identityWeylElement W;
v = weylElement(W,{2,1,3,2,5,4,6,5});
clearInfrastructureCaches();
assert(mustErrorKL(() -> kazhdanLusztigPolynomial(e,v,MaxElements=>195)));
assert(kazhdanLusztigPolynomial(e,v,MaxElements=>196) == (1+q)^2);
assert(kazhdanLusztigCoefficients(e,v,MaxElements=>196) === {1,2,1});
assert(mustErrorKL(() -> kazhdanLusztigPolynomial(e,v,MaxElements=>195)));
assert(kazhdanLusztigPolynomial(e,v,MaxElements=>null) == (1+q)^2);
assert(kazhdanLusztigPolynomial(weylElement(W,{2,5}),v) == (1+q)^2);
assert(kazhdanLusztigPolynomial(weylElement(W,{1,2,1,3,2,5}),v) == 0);
for badLimit in {0,-1,1/2,"196",false} do
    assert(mustErrorKL(() -> kazhdanLusztigPolynomial(e,v,MaxElements=>badLimit)));

-- The support can split even when the ambient diagram is connected.  Node
-- 4 is absent, so these are two commuting A3 factors inside A7.
A7 = cartanA 8;
W7 = weylGroup A7;
v7 = weylElement(W7,{2,1,3,2,6,5,7,6});
assert(kazhdanLusztigPolynomial(identityWeylElement W7,v7,MaxElements=>196) == (1+q)^2);
assert(mustErrorKL(() -> kazhdanLusztigPolynomial(identityWeylElement W7,v7,MaxElements=>195)));
assert(kazhdanLusztigPolynomial(identityWeylElement W7,
    weylElement(W7,{1,3,5,7}),MaxElements=>16) == 1);
assert(mustErrorKL(() -> kazhdanLusztigPolynomial(identityWeylElement W7,
    weylElement(W7,{1,3,5,7}),MaxElements=>15)));

-- Unused infinite components change neither the answer nor its interval
-- size.  Public node labels and simultaneous permutations remain valid.
Aunused = (cartanAffineA 1)++A;
assert(kazhdanLusztigPolynomial(Aunused,{}, {4,3,5,4,7,6,8,7},MaxElements=>196) == (1+q)^2);
Dlabeled = cartanDatum(A,NodeLabels=>{"a","b","c","d","e","f"});
Wlabeled = weylGroup Dlabeled;
assert(kazhdanLusztigPolynomial(identityWeylElement Wlabeled,
    weylElement(Wlabeled,{"b","a","c","b","e","d","f","e"})) == (1+q)^2);
permutation = {1,4,2,5,3,6};
Apermuted = matrix table(6,6,(i,j) -> A_(permutation#i-1,permutation#j-1));
remapWord = word -> apply(word,i -> first positions(permutation,j -> i==j)+1);
assert(kazhdanLusztigPolynomial(Apermuted,{},remapWord {2,1,3,2,5,4,6,5}) == (1+q)^2);

-- Eight commuting reflections have 256 distinct subwords.  They need not
-- be individually materialized to compute the constant polynomial.
Acommuting = matrix table(8,8,(i,j) -> if i==j then 2 else 0);
assert(kazhdanLusztigPolynomial(Acommuting,{},toList(1..8),MaxElements=>256) == 1);
assert(mustErrorKL(() -> kazhdanLusztigPolynomial(Acommuting,{},toList(1..8),MaxElements=>255)));

///

TEST ///

-- Independent published CHEVIE table of all 48 principal B3 polynomials:
-- https://www.math.rwth-aachen.de/~GAP/WWW2/Gap3/Manual3/C083S001.htm
-- Its B3 numbering has the double edge at 1--2; reversing the three nodes
-- converts it to KacMoody's convention.  In particular the degree-two
-- entries 1+q^2 and 1+q+q^2 exercise cancellations invisible to tests that
-- only check positivity, degrees, or disconnected product formulas.
publishedWords = {
    {},{3},{2},{1},{3,2},{2,1},{2,3},{1,3},{1,2},
    {2,1,2},{3,2,1},{2,3,2},{2,1,3},{1,2,1},{1,3,2},{1,2,3},
    {3,2,1,2},{2,1,2,3},{2,3,2,1},{2,1,3,2},{1,2,1,2},
    {1,3,2,1},{1,2,1,3},{1,2,3,2},{3,2,1,2,3},{2,1,2,3,2},
    {2,3,2,1,2},{2,1,3,2,1},{1,3,2,1,2},{1,2,1,2,3},
    {1,2,1,3,2},{1,2,3,2,1},{2,3,2,1,2,3},{2,1,2,3,2,1},
    {2,1,3,2,1,2},{1,3,2,1,2,3},{1,2,1,2,3,2},{1,2,1,3,2,1},
    {1,2,3,2,1,2},{2,1,2,3,2,1,2},{2,1,3,2,1,2,3},
    {1,2,3,2,1,2,3},{1,2,1,2,3,2,1},{1,2,1,3,2,1,2},
    {2,1,2,3,2,1,2,3},{1,2,1,2,3,2,1,2},
    {1,2,1,3,2,1,2,3},{1,2,1,2,3,2,1,2,3}
};
publishedCodes = {
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,
    1,0,0,1,0,0,1,1,0,1,0,1,0,2,1,3,1,1,0,0,2,0,1,0
};
coefficientLists = {{1},{1,1},{1,0,1},{1,1,1}};
A = cartanB 3;
W = weylGroup A;
e = identityWeylElement W;
clearInfrastructureCaches();
for k from 0 to #publishedWords-1 do (
    word := apply(publishedWords#k,i -> 4-i);
    v := weylElement(W,word);
    expected := coefficientLists#(publishedCodes#k);
    assert(weylLength v == #word);
    assert(kazhdanLusztigCoefficients(e,v) === expected);
    assert(kazhdanLusztigCoefficients(inverseWeylElement e,inverseWeylElement v) === expected);
    assert(kazhdanLusztigCoefficients(transpose A,{},word) === expected);
);
assert(#unique apply(publishedWords,word -> reducedWord weylElement(W,apply(word,i -> 4-i))) == 48);
assert(kazhdanLusztigCoefficients(cartanB 3,{}, {3,2,3,1,2,3}) === {1,0,1});
assert(kazhdanLusztigCoefficients(cartanB 3,{}, {2,3,2,1,2,3,2}) === {1,1,1});

-- The guard is unchanged for connected support too, including a failed
-- cold enumeration followed by a successful query and then a smaller warm
-- limit.  Count the interval independently using all published B3 elements.
upper = weylElement(W,{3,2,3,1,2,3});
intervalSize = #select(publishedWords,word -> bruhatLessOrEqual(
    weylElement(W,apply(word,i -> 4-i)),upper));
clearInfrastructureCaches();
assert(try (kazhdanLusztigCoefficients(e,upper,MaxElements=>intervalSize-1);false) else true);
assert(kazhdanLusztigCoefficients(e,upper,MaxElements=>intervalSize) === {1,0,1});
assert(try (kazhdanLusztigCoefficients(e,upper,MaxElements=>intervalSize-1);false) else true);

///

TEST ///

-- Independent permutation rank matrices give every Bruhat comparison in
-- S4.  Test every KL pair, rather than just principal intervals and covers,
-- so cached products, descents, and comparisons see every endpoint choice.
A = cartanA 4;
W = weylGroup A;
e = identityWeylElement W;
weylEls = apply(weylGroupElements A,h -> weylElement(W,h#"word"));
wordPermutation = word -> (
    p := toList(1..4);
    for i in word do p = apply(4,j ->
        if j==i-1 then p#i else if j==i then p#(i-1) else p#j);
    p
);
rankBruhat = (p,q) -> all(toList(1..4),a -> all(toList(1..4),b ->
    #(select(take(p,a),i -> i<=b)) >= #(select(take(q,a),i -> i<=b))));
clearInfrastructureCaches();
for u in weylEls do for v in weylEls do (
    comparable := rankBruhat(wordPermutation reducedWord u,wordPermutation reducedWord v);
    assert(bruhatLessOrEqual(u,v) === comparable);
    coeffs := kazhdanLusztigCoefficients(u,v);
    assert(coeffs === kazhdanLusztigCoefficients(inverseWeylElement u,inverseWeylElement v));
    assert(all(coeffs,c -> instance(c,ZZ) and c>=0));
    if comparable then (
        assert(first coeffs == 1);
        if u =!= v then assert(2*(#coeffs-1) < weylLength v-weylLength u);
    ) else assert(coeffs === {0});
);

-- Finite and infinite dihedral KL polynomials are 1 on comparable pairs.
for fixture in {{cartanB 2,4},{cartanG2(),6},
    {cartanAffineA 1,7},{matrix{{2,-3},{-3,2}},7}} do (
    Wd := weylGroup(fixture#0);
    for ell from 1 to fixture#1 do for start in {1,2} do (
        word := apply(ell,j -> if j%2==0 then start else 3-start);
        v := weylElement(Wd,word);
        assert(kazhdanLusztigPolynomial(identityWeylElement Wd,v) == 1);
        for i in {1,2} do assert(kazhdanLusztigPolynomial(
            simpleWeylReflection(Wd,i),v) ==
            if ell==1 and i!=start then 0 else 1);
    );
);

-- Empty semisimple rank is an honest one-element Coxeter group.
D = rootDatum parabolicLevi standardParabolic(rootDatum "A3",LeviNodes=>{});
W0 = weylGroup D;
e0 = identityWeylElement W0;
assert(kazhdanLusztigPolynomial(e0,e0) == 1);
assert(kazhdanLusztigCoefficients(e0,e0) === {1});
B = bggResolution(D,{});
assert(B#"exact" and B#"length"==0);
assert(#keys(B#"terms"#0)==1);
assert(characterDimension formalCharacter(D,characterTerms bggEulerCharacter(B,Depth=>0)) == 1);

///


-------------------------------------------------------------------------
-- Indicator Regression Tests
-------------------------------------------------------------------------

-- Tits' criterion is checked against its invariant-bilinear-form meaning,
-- explicit coroot enumeration, and complete realization weights.

TEST ///

mustErrorIndicator = f -> (try (f(); false) else true);

-- This oracle deliberately uses characters and the two second powers,
-- independently of the highest-weight parity calculation.
squareIndicatorOracle = (D,lambda) -> (
    C := finiteCharacter(D,lambda);
    zero := 0*lambda;
    S := symmetricPowerDecomposition(C,2);
    E := exteriorPowerDecomposition(C,2);
    s := if S#?zero then S#zero else 0;
    a := if E#?zero then E#zero else 0;
    assert(member({s,a},{{0,0},{1,0},{0,1}}));
    s-a
);

for n from 0 to 5 do (
    D := rootDatum "A1";
    lambda := kmWeight(D,{n});
    assert(frobeniusSchurIndicator(D,lambda) == squareIndicatorOracle(D,lambda));
    assert(frobeniusSchurIndicator(D,{n}) == (-1)^n);
    assert(frobeniusSchurIndicator(cartanA 2,{n}) == (-1)^n);
);
for fixture in {
    {"A2",{1,0},0},{"A2",{1,1},1},{"A3",{0,1,0},1},
    {"B2",{0,1},-1},{"B3",{0,0,1},1},
    {"C2",{1,0},-1},{"C2",{0,1},1},
    {"G2",{1,0},1},{"D4",{0,0,0,1},1},
    {"D5",{0,0,0,0,1},0}
} do (
    D := rootDatum fixture#0;
    lambda := kmWeight(D,fixture#1);
    assert(squareIndicatorOracle(D,lambda) == fixture#2);
    assert(frobeniusSchurIndicator(D,lambda) == fixture#2);
);

-- The tensor product of alternating forms is symmetric.  This also checks
-- reducible finite type without conflating separate simple components.
DproductIndicator := rootDatum diagonalMatrix{2,2};
for a from 0 to 3 do for b from 0 to 3 do (
    lambda := kmWeight(DproductIndicator,{a,b});
    assert(frobeniusSchurIndicator(DproductIndicator,lambda) == (-1)^(a+b));
    if a <= 2 and b <= 2 then
        assert(squareIndicatorOracle(DproductIndicator,lambda) == (-1)^(a+b));
);

///

TEST ///

-- Positive roots of A^T are positive coroots of A.  Compare their explicit
-- sum with the linear-equation calculation in every finite family,
-- including exceptional types, both root-length orientations, reducible
-- products, and reordered nodes.
sumCorootsIndicator := value getGlobalSymbol(
    (package "KacMoody")#"private dictionary","draftPositiveCorootSum");
fixturesCorootIndicator := join(
    apply(toList(1..10),r -> cartanA(r+1)),
    apply(toList(2..8),r -> cartanB r),
    apply(toList(2..8),r -> cartanC r),
    apply(toList(4..9),r -> cartanD r),
    {cartanE 6,cartanE 7,cartanE 8,cartanF4(),cartanG2(),
     transpose cartanF4(),transpose cartanG2(),
     (cartanA 3) ++ (cartanC 2)});
for A in fixturesCorootIndicator do (
    explicit := sum entries cartanPositiveRoots transpose A;
    assert(sumCorootsIndicator A === explicit);
    assert(matrix{explicit} * A == matrix{toList(numrows A:2)});
    permutation := reverse toList(0..numrows A-1);
    Ap := submatrix(A,permutation,permutation);
    assert(sumCorootsIndicator Ap === apply(permutation,i -> explicit#i));
    -- Realizability and labels are unaffected by integral Cartan entries
    -- being supplied over QQ instead of ZZ.
    assert(frobeniusSchurIndicator(A ** QQ,toList(numrows A:0)) == 1);
);

for r from 2 to 8 do (
    spin := join(toList(r-1:0),{1});
    expected := if member(r % 4,{1,2}) then -1 else 1;
    assert(frobeniusSchurIndicator(cartanB r,spin) == expected);
    for j from 0 to r-1 do (
        fundamental := for i from 0 to r-1 list if i == j then 1 else 0;
        assert(frobeniusSchurIndicator(cartanC r,fundamental) == (-1)^(j+1));
    );
);
for r from 4 to 9 do (
    spin := join(toList(r-1:0),{1});
    expected := if r % 2 == 1 then 0 else if r % 4 == 0 then 1 else -1;
    assert(frobeniusSchurIndicator(cartanD r,spin) == expected);
);
-- Exceptional fundamental representations: the E6 pair is not self-dual,
-- the 56-dimensional E7 representation is alternating, and every E8,
-- F4, and G2 fundamental representation is orthogonal.
assert(frobeniusSchurIndicator(cartanE 6,{1,0,0,0,0,0}) == 0);
assert(frobeniusSchurIndicator(cartanE 6,{0,0,0,0,0,1}) == 0);
assert(frobeniusSchurIndicator(cartanE 7,{0,0,0,0,0,0,1}) == -1);
for A in {cartanE 8,cartanF4(),cartanG2()} do
    for j from 0 to numrows A-1 do (
        lambda := for i from 0 to numrows A-1 list if i == j then 1 else 0;
        assert(frobeniusSchurIndicator(A,lambda) == 1);
    );

///

TEST ///

mustErrorIndicator = f -> (try (f(); false) else true);

-- The center must be checked in the actual realization, not by looking
-- at one ambient coordinate or only at the Dynkin labels.
RextraIndicator := realization(cartanDatum "A1",
    matrix(QQ,{{2,0}}),matrix(QQ,{{1,0}}));
DextraIndicator := rootDatum RextraIndicator;
lambdaNeutralIndicator := weightFromRealizationCoordinates(DextraIndicator,{1,0});
lambdaCentralIndicator := weightFromRealizationCoordinates(DextraIndicator,{1,3});
assert(frobeniusSchurIndicator(DextraIndicator,lambdaNeutralIndicator) == -1);
assert(frobeniusSchurIndicator(DextraIndicator,lambdaCentralIndicator) == 0);
assert(frobeniusSchurIndicator(DextraIndicator,kmWeight(DextraIndicator,{1})) == -1);
assert(frobeniusSchurIndicator(DextraIndicator,{1}) == -1);
assert(frobeniusSchurIndicator(DextraIndicator,
    weightFromRealizationCoordinates(DextraIndicator,{0,1/3})) == 0);
XskewIndicator := matrix(QQ,{{1/2,1},{2,3}});
RskewIndicator := realization(cartanDatum "A1",
    matrix(QQ,{{2,0}})*XskewIndicator,
    matrix(QQ,{{1,0}})*transpose inverse XskewIndicator);
DskewIndicator := rootDatum RskewIndicator;
for coordinates in {{1,0},{1,3},{2,0},{0,1/3}} do (
    mu := weightFromRealizationCoordinates(DextraIndicator,coordinates);
    transported := weightFromRealizationCoordinates(DskewIndicator,
        flatten entries(matrix(QQ,{coordinates})*XskewIndicator));
    assert(datumCoordinates mu === datumCoordinates transported);
    assert(frobeniusSchurIndicator(DextraIndicator,mu)
        == frobeniusSchurIndicator(DskewIndicator,transported));
);

A0Indicator := matrix(ZZ,{});
D0Indicator := rootDatum A0Indicator;
assert(frobeniusSchurIndicator(A0Indicator,{}) == 1);
assert(frobeniusSchurIndicator(D0Indicator,kmWeight(D0Indicator,{})) == 1);
emptyTwoColumnsIndicator := matrix mutableMatrix(QQ,0,2);
DtorusIndicator := rootDatum realization(cartanDatum A0Indicator,
    emptyTwoColumnsIndicator,emptyTwoColumnsIndicator);
assert(frobeniusSchurIndicator(DtorusIndicator,
    weightFromRealizationCoordinates(DtorusIndicator,{0,0})) == 1);
assert(frobeniusSchurIndicator(DtorusIndicator,
    weightFromRealizationCoordinates(DtorusIndicator,{2,-3})) == 0);
assert(frobeniusSchurIndicator(DtorusIndicator,{}) == 1);

-- The indicator belongs to the Lie-algebra representation: choosing the
-- adjoint group does not make the odd A1 highest weight disappear.
DadjointIndicator := rootDatum("A1",GlobalForm=>"Adjoint");
assert(not integratesToGlobalForm kmWeight(DadjointIndicator,{1}));
assert(frobeniusSchurIndicator(DadjointIndicator,{1}) == -1);

assert(mustErrorIndicator(() -> frobeniusSchurIndicator(
    rootDatum "A2",kmWeight(rootDatum "B2",{1,0}))));
assert(mustErrorIndicator(() -> frobeniusSchurIndicator(cartanA 2,{-1})));
assert(mustErrorIndicator(() -> frobeniusSchurIndicator(cartanA 2,{1/2})));
assert(mustErrorIndicator(() -> frobeniusSchurIndicator(cartanA 2,{1,0})));
assert(mustErrorIndicator(() -> frobeniusSchurIndicator(cartanAffineA 1,{1,0})));
assert(mustErrorIndicator(() -> frobeniusSchurIndicator(matrix{{2,-3},{-3,2}},{1,1})));
assert(mustErrorIndicator(() -> frobeniusSchurIndicator(matrix{{2,-1},{0,2}},{1,0})));

///

TEST ///

-- Huge labels must not construct characters, tensor products, weight
-- multiplicity tables, positive-root tables, or the longest Weyl word.
clearInfrastructureCaches();
AcoldIndicator := cartanC 12;
DcoldIndicator := rootDatum AcoldIndicator;
AcoldIndicator = datumCartanMatrix DcoldIndicator;
clearRepresentationCache AcoldIndicator;
assert(not AcoldIndicator.cache#?"cartanPositiveRoots");
assert(not AcoldIndicator.cache#?"longElement");
beforeInfrastructureIndicator := infrastructureCacheInfo();
hugeIndicator := 10^100;
labelsHugeIndicator := join({hugeIndicator+1},toList(11:hugeIndicator));
assert(frobeniusSchurIndicator(DcoldIndicator,labelsHugeIndicator) == -1);
assert(frobeniusSchurIndicator(DcoldIndicator,toList(12:hugeIndicator)) == 1);
assert(all(values representationCacheInfo AcoldIndicator,m -> m == 0));
assert(not AcoldIndicator.cache#?"cartanPositiveRoots");
assert(not AcoldIndicator.cache#?"longElement");
afterInfrastructureIndicator := infrastructureCacheInfo();
assert(afterInfrastructureIndicator#"vermaPartitions"
    == beforeInfrastructureIndicator#"vermaPartitions");
assert(afterInfrastructureIndicator#"characterParents"
    == beforeInfrastructureIndicator#"characterParents");
assert(afterInfrastructureIndicator#"kazhdanLusztigData"
    == beforeInfrastructureIndicator#"kazhdanLusztigData");
assert(afterInfrastructureIndicator#"crystalData"
    == beforeInfrastructureIndicator#"crystalData");

-- Boundary powers include the determinant and orders above the dimension.
-- Complementary exterior powers are dual for this semisimple module.
DlambdaBoundaryIndicator := rootDatum "A2";
CboundaryIndicator := finiteCharacter(DlambdaBoundaryIndicator,{2,1});
dimensionBoundaryIndicator := characterDimension CboundaryIndicator;
assert(dimensionBoundaryIndicator == 15);
for k in {0,1,2,13,14,15,16} do (
    E := exteriorPowerCharacter(CboundaryIndicator,k);
    assert(characterDimension E == binomial(dimensionBoundaryIndicator,k));
    if k <= dimensionBoundaryIndicator then
        assert(characterTerms E == characterTerms dualCharacter(
            exteriorPowerCharacter(CboundaryIndicator,dimensionBoundaryIndicator-k)));
);

///


-------------------------------------------------------------------------
-- Audit Root Data Regression Tests
-------------------------------------------------------------------------

-- Independent Euclidean root systems, lattice equations, and syntax bounds.
TEST ///
    dotAuditRoot = (u,v) -> sum apply(#u,i -> u#i*v#i);
    integralAuditRoot = x -> instance(x,ZZ) or denominator x == 1;
    canonicalAuditRoot = L -> apply(L,x ->
        if integralAuditRoot x then lift(x,ZZ) else x);
    mustErrorAuditRoot = f -> try (f();false) else true;
    fixturesAuditRoot = {
        {cartanA 6,simpleRootsA 6,positiveRootsA 6,15,{2=>15}},
        {cartanB 5,simpleRootsB 5,positiveRootsB 5,25,{1=>5,2=>20}},
        {cartanC 5,simpleRootsC 5,positiveRootsC 5,25,{2=>20,4=>5}},
        {cartanD 5,simpleRootsD 5,positiveRootsD 5,20,{2=>20}},
        {cartanF4(),simpleRootsF4(),positiveRootsF4(),24,{1=>12,2=>12}},
        {cartanG2(),simpleRootsG2(),positiveRootsG2(),6,{2=>3,6=>3}}
    };
    for fixture in fixturesAuditRoot do (
        A := fixture#0; S := fixture#1; R := fixture#2;
        roots := apply(entries R,canonicalAuditRoot);
        sr := apply(entries S,canonicalAuditRoot);
        assert(cartanRank A == #sr);
        assert(#roots == fixture#3 and #set roots == #roots);
        assert(tally apply(roots,u -> lift(dotAuditRoot(u,u),ZZ))
            == new Tally from fixture#4);
        assert(matrix(QQ,table(#sr,#sr,(i,j) ->
            2*dotAuditRoot(sr#i,sr#j)/dotAuditRoot(sr#i,sr#i))) == A**QQ);
        full := set(roots|apply(roots,u -> -u));
        for u in toList full do for a in sr do
            assert(member(canonicalAuditRoot(u-
                (2*dotAuditRoot(u,a)/dotAuditRoot(a,a))*a),full));
        for u in roots do (
            b := fromEpsilon(S,u);
            assert(all(b,x -> integralAuditRoot x and x >= 0));
            assert(canonicalAuditRoot toEpsilon(S,b) == u);
            assert(canonicalAuditRoot toEpsilon(S,matrix{b}) == u);
        );
        assert(mustErrorAuditRoot(() -> toEpsilon(S,toList((#sr+1):0))));
        assert(mustErrorAuditRoot(() ->
            toEpsilon(S,transpose matrix{toList(#sr:1)})));
    );
///

TEST ///
    integralAuditLattice = x -> instance(x,ZZ) or denominator x == 1;
    canonicalAuditLattice = L -> apply(L,x ->
        if integralAuditLattice x then lift(x,ZZ) else x);
    -- The inverse Cartan matrix gives an independent membership criterion
    -- for both the character lattice and the cocharacter lattice.
    for A in {cartanA 3,cartanB 2,cartanC 2,cartanG2()} do (
        Dsc := rootDatum(A,GlobalForm=>"SimplyConnected");
        Dad := rootDatum(A,GlobalForm=>"Adjoint");
        Ai := inverse(A**QQ);
        for a from -3 to 3 do for b from -3 to 3 do (
            labels := {a,b};
            rc := canonicalAuditLattice flatten entries(
                matrix(QQ,{labels})*transpose Ai);
            cc := canonicalAuditLattice flatten entries(matrix(QQ,{labels})*Ai);
            w := kmWeight(Dad,labels);
            c := kmCoweight(Dsc,labels);
            assert(weightToRootCoordinates w == rc);
            assert(coweightToCorootCoordinates c == cc);
            assert(isRootLatticeWeight w == all(rc,integralAuditLattice));
            assert(isCorootLatticeCoweight c == all(cc,integralAuditLattice));
            assert(integratesToGlobalForm w == all(rc,integralAuditLattice));
            assert(integratesCoweightToGlobalForm c == all(cc,integralAuditLattice));
            assert(integratesToGlobalForm kmWeight(Dsc,labels));
            assert(integratesCoweightToGlobalForm kmCoweight(Dad,labels));
        );
        assert(coordinateBasis kmRoot(Dsc,{1,0}) === "SimpleRoots");
        assert(coordinateBasis kmCoroot(Dsc,{1,0}) === "SimpleCoroots");
        assert(coordinateBasis kmWeight(Dsc,{1,0}) === "DynkinLabels");
        assert(coordinateBasis kmCoweight(Dsc,{1,0}) === "SimpleRootEvaluations");
    );
    for fixture in {
        {(cartanD 4)++(cartanA 4),{2,2,4},16},
        {(cartanE 6)++(cartanA 4),{12},12},
        {(cartanA 6)++(cartanA 4),{2,12},24},
        {(cartanB 2)++(cartanC 3),{2,2},4}
    } do (
        Q := pModQInvariants fixture#0;
        assert(Q#"torsionInvariants" == fixture#1);
        assert(Q#"order" == fixture#2);
        assert(Q#"order" == abs det fixture#0);
    );
///



TEST ///
    -- On P^1 every integral weight gives O(k), even when its action
    -- does not descend from SL2 to the selected adjoint group PGL2.
    DscAuditBWB = rootDatum(cartanA 2,GlobalForm=>"SimplyConnected");
    DadAuditBWB = rootDatum(cartanA 2,GlobalForm=>"Adjoint");
    mustErrorAuditBWB = f -> try (f();false) else true;
    for k from -7 to 7 do (
        lambda := kmWeight(DadAuditBWB,{k});
        assert(integratesToGlobalForm lambda == (k%2 == 0));
        R := borelWeilBott(DadAuditBWB,lambda);
        S := borelWeilBott(DscAuditBWB,{k});
        C := borelWeilBottCharacter(DadAuditBWB,lambda);
        assert(borelWeilBott(DadAuditBWB,lambda,
            LineBundleConvention=>"FiberMinusWeight") == R);
        assert(characterTerms borelWeilBottCharacter(DadAuditBWB,lambda,
            LineBundleConvention=>"FiberMinusWeight") == characterTerms C);
        assert(R#"vanishes" == (k == -1));
        assert(R#"vanishes" == S#"vanishes");
        assert(R#"degree" == S#"degree");
        if k == -1 then (
            assert(characterDimension C == 0);
            assert(R#"cohomologyHighestWeight" === null);
        ) else (
            assert(R#"degree" == (if k >= 0 then 0 else 1));
            expected := if k >= 0 then k else -k-2;
            assert(datumCoordinates R#"cohomologyHighestWeight" == {expected});
            assert(characterDimension C == abs(k+1));
        );
    );
    assert(mustErrorAuditBWB(() -> borelWeilBott(DadAuditBWB,{1},
        LineBundleConvention=>"FiberPlusWeight")));
    assert(mustErrorAuditBWB(() -> borelWeilBottCharacter(DadAuditBWB,{1},
        LineBundleConvention=>"FiberPlusWeight")));
///


-------------------------------------------------------------------------
-- Audit Nonfinite Regression Tests
-------------------------------------------------------------------------

-- Independent free-Lie and affine lattice/Fock-space formulas.
TEST ///
    offsetsAuditFree = H -> flatten apply(toList(0..H),h ->
        apply(toList(0..h),a -> {a,h-a}));
    mobiusAuditFree = {0,1,-1,-1,0,-1,1,-1,0,0,1};
    wittAuditFree = (a,b) -> (
        h := a+b;
        if h == 0 then return 0;
        g := gcd(a,b);
        (sum for d from 1 to g list if g%d == 0 then
            mobiusAuditFree#d*binomial(h//d,a//d) else 0)//h
    );
    AAuditFree = matrix{{2,-10},{-10,2}};
    SAuditFree = indefiniteRootSnapshot(AAuditFree,10,Algorithm=>"SparsePeterson");
    BAuditFree = indefiniteRootSnapshot(AAuditFree,10,Algorithm=>"BackusConnickLin");
    -- The Serre relations first occur in height 12, so through height 10
    -- the positive Lie algebra is the free Lie algebra on two generators.
    for beta in select(offsetsAuditFree 10,b -> sum b > 0) do (
        expected := wittAuditFree(beta#0,beta#1);
        assert(rootMultiplicity(SAuditFree,beta) == expected);
        assert(rootMultiplicity(BAuditFree,beta) == expected);
    );
    DAuditFree = rootDatum AAuditFree;
    lambdaAuditFree = kmWeight(DAuditFree,{2,3});
    VAuditFree = moduleCharacter(vermaModule(DAuditFree,lambdaAuditFree),Depth=>8);
    -- Its enveloping algebra has the free associative word counts.
    for beta in offsetsAuditFree 8 do assert(characterCoefficient(VAuditFree,
        lambdaAuditFree-toWeight kmRoot(DAuditFree,beta)) == binomial(sum beta,beta#0));
    trivialAuditFree = moduleCharacter(
        integrableHighestWeightModule(DAuditFree,{0,0}),Depth=>8);
    assert(#keys characterTerms trivialAuditFree == 1);
    mustErrorAuditFree = f -> try (f();false) else true;
    assert(mustErrorAuditFree(() -> characterCoefficient(VAuditFree,
        lambdaAuditFree-toWeight kmRoot(DAuditFree,{5,4}))));
///

TEST ///
    equalTablesAuditIndefinite = (T,U) -> (#keys T == #keys U and
        all(keys T,k -> U#?k and U#k == T#k));
    AAuditIndefinite = matrix{{2,-5},{-3,2}};
    SAuditIndefinite = indefiniteRootSnapshot(AAuditIndefinite,12,Algorithm=>"SparsePeterson");
    BAuditIndefinite = indefiniteRootSnapshot(AAuditIndefinite,12,Algorithm=>"BackusConnickLin");
    assert(equalTablesAuditIndefinite(SAuditIndefinite#"mult",BAuditIndefinite#"mult"));
    APermutedAuditIndefinite = submatrix(AAuditIndefinite,{1,0},{1,0});
    SPermutedAuditIndefinite = indefiniteRootSnapshot(
        APermutedAuditIndefinite,12,Algorithm=>"BackusConnickLin");
    assert(equalTablesAuditIndefinite(new HashTable from
        apply(pairs SAuditIndefinite#"mult",p -> reverse(p#0)=>p#1),
        SPermutedAuditIndefinite#"mult"));
///

TEST ///
    offsetsAuditBasic = H -> flatten apply(toList(0..H),h ->
        apply(toList(0..h),a -> {a,h-a}));
    -- The basic A1^(1) coefficient at Lambda0-a*alpha0-b*alpha1 is
    -- p(a-(a-b)^2), from its rank-one lattice/Fock-space realization.
    partitionsAuditBasic = {1,1,2,3,5,7,11,15,22};
    basic0Audit = b -> (n := b#0-(b#0-b#1)^2;
        if n < 0 then 0 else partitionsAuditBasic#n);
    basic1Audit = b -> basic0Audit reverse b;
    DAuditBasic = rootDatum cartanAffineA 1;
    l0AuditBasic = weightFromRealizationCoordinates(DAuditBasic,{1,0,0});
    l1AuditBasic = weightFromRealizationCoordinates(DAuditBasic,{0,1,0});
    C0AuditBasic = moduleCharacter(integrableHighestWeightModule(
        DAuditBasic,l0AuditBasic),Depth=>16);
    C1AuditBasic = moduleCharacter(integrableHighestWeightModule(
        DAuditBasic,l1AuditBasic),Depth=>16);
    for beta in offsetsAuditBasic 16 do (
        assert(characterCoefficient(C0AuditBasic,
            l0AuditBasic-toWeight kmRoot(DAuditBasic,beta)) == basic0Audit beta);
        assert(characterCoefficient(C1AuditBasic,
            l1AuditBasic-toWeight kmRoot(DAuditBasic,beta)) == basic1Audit beta);
    );
    mustErrorAuditBasic = f -> try (f();false) else true;
    deltaAuditBasic = toWeight affineNullRoot DAuditBasic;
    assert(characterCoefficient(C0AuditBasic,l0AuditBasic-8*deltaAuditBasic) == 22);
    assert(mustErrorAuditBasic(() ->
        characterCoefficient(C0AuditBasic,l0AuditBasic-9*deltaAuditBasic)));
    C0smallAuditBasic = moduleCharacter(integrableHighestWeightModule(
        DAuditBasic,l0AuditBasic),Depth=>8);
    C1smallAuditBasic = moduleCharacter(integrableHighestWeightModule(
        DAuditBasic,l1AuditBasic),Depth=>12);
    PAuditBasic = C0smallAuditBasic*C1smallAuditBasic;
    for beta in offsetsAuditBasic 8 do (
        expected := sum for a from 0 to beta#0 list sum for b from 0 to beta#1 list
            basic0Audit({a,b})*basic1Audit({beta#0-a,beta#1-b});
        assert(characterCoefficient(PAuditBasic,
            l0AuditBasic+l1AuditBasic-toWeight kmRoot(DAuditBasic,beta)) == expected);
    );
    assert(mustErrorAuditBasic(() -> characterCoefficient(PAuditBasic,
        l0AuditBasic+l1AuditBasic-toWeight kmRoot(DAuditBasic,{5,4}))));
    sumAuditBasic = C0smallAuditBasic+C1smallAuditBasic;
    for beta in offsetsAuditBasic 8 do (
        assert(characterCoefficient(sumAuditBasic,
            l0AuditBasic-toWeight kmRoot(DAuditBasic,beta)) == basic0Audit beta);
        assert(characterCoefficient(sumAuditBasic,
            l1AuditBasic-toWeight kmRoot(DAuditBasic,beta)) == basic1Audit beta);
    );
///

TEST ///
    -- An independent PBW product uses the explicitly classified positive
    -- roots (n+1,n), (n,n+1), and (n+1,n+1), each of multiplicity one.
    HAuditPBW = 10;
    QAuditPBW = new MutableHashTable from {{0,0}=>1};
    rootsAuditPBW = flatten apply(toList(0..HAuditPBW),n -> select(
        {{n+1,n},{n,n+1},{n+1,n+1}},b -> sum b <= HAuditPBW));
    for alpha in rootsAuditPBW do (
        old := new HashTable from pairs QAuditPBW;
        next := new MutableHashTable;
        for p in pairs old do for k from 0 to (HAuditPBW-sum(p#0))//sum(alpha) do (
            beta := {p#0#0+k*alpha#0,p#0#1+k*alpha#1};
            next#beta = (if next#?beta then next#beta else 0)+p#1;
        );
        QAuditPBW = next;
    );
    DAuditPBW = rootDatum cartanAffineA 1;
    l0AuditPBW = weightFromRealizationCoordinates(DAuditPBW,{1,0,0});
    VAuditPBW = moduleCharacter(vermaModule(DAuditPBW,l0AuditPBW),Depth=>HAuditPBW);
    offsetsAuditPBW = H -> flatten apply(toList(0..H),h ->
        apply(toList(0..h),a -> {a,h-a}));
    for beta in offsetsAuditPBW HAuditPBW do assert(characterCoefficient(VAuditPBW,
        l0AuditPBW-toWeight kmRoot(DAuditPBW,beta)) ==
        (if QAuditPBW#?beta then QAuditPBW#beta else 0));
///

TEST ///
    equalTablesAuditTwisted = (T,U) -> (#keys T == #keys U and
        all(keys T,k -> U#?k and U#k == T#k));
    -- In this twisted affine algebra the imaginary-root multiplicities
    -- alternate between 2 and 3; replacing them by a constant is wrong.
    AAuditTwisted = transpose cartanAffineB 3;
    SAuditTwisted = affineRootSnapshot(AAuditTwisted,20);
    BAuditTwisted = indefiniteRootSnapshot(AAuditTwisted,20,Algorithm=>"BackusConnickLin");
    assert(equalTablesAuditTwisted(SAuditTwisted#"mult",BAuditTwisted#"mult"));
    deltaAuditTwisted = flatten entries SAuditTwisted#"delta";
    for n from 1 to 4 do assert(rootMultiplicity(SAuditTwisted,
        apply(deltaAuditTwisted,x -> n*x)) == (if n%2 == 1 then 2 else 3));
    permutationAuditTwisted = {2,0,3,1};
    SPAuditTwisted = rootSnapshot(submatrix(AAuditTwisted,
        permutationAuditTwisted,permutationAuditTwisted),20);
    assert(equalTablesAuditTwisted(new HashTable from apply(pairs SAuditTwisted#"mult",
        p -> apply(permutationAuditTwisted,i -> p#0#i)=>p#1),SPAuditTwisted#"mult"));
///

TEST ///
    SAuditMembership = indefiniteRootSnapshot(matrix{{2,-10},{-10,2}},10);
    assert(isRootInSnapshot(SAuditMembership,{4,6}));
    assert(not isRootInSnapshot(SAuditMembership,{10,0}));
    assert(not isRootInSnapshot(SAuditMembership,{0,0}));
    assert(not isRootInSnapshot(SAuditMembership,{-1,0}));
    assert(not isRootInSnapshot(SAuditMembership,{10,1}));
    mustErrorAuditMembership = f -> try (f();false) else true;
    assert(mustErrorAuditMembership(() -> isRootInSnapshot(SAuditMembership,{1})));
    assert(mustErrorAuditMembership(() -> isRootInSnapshot(SAuditMembership,{1,0_QQ})));
    assert(mustErrorAuditMembership(() -> isRootInSnapshot(new HashTable,{1,0})));
///

TEST ///
    offsetsAuditRealization = H -> flatten apply(toList(0..H),h ->
        apply(toList(0..h),a -> {a,h-a}));
    partitionsAuditRealization = {1,1,2,3,5};
    basicAuditRealization = b -> (n := b#0-(b#0-b#1)^2;
        if n < 0 then 0 else partitionsAuditRealization#n);
    DAuditRealization = rootDatum cartanAffineA 1;
    l0AuditRealization = weightFromRealizationCoordinates(DAuditRealization,{1,0,0});
    C0AuditRealization = moduleCharacter(integrableHighestWeightModule(
        DAuditRealization,l0AuditRealization),Depth=>8);
    basisAuditRealization = matrix(QQ,{{1,1/2,0},{0,2,1},{1,0,1}});
    SRAuditRealization = matrix(QQ,(realization DAuditRealization)#"simpleRootCoordinates");
    SCAuditRealization = matrix(QQ,(realization DAuditRealization)#"simpleCorootCoordinates");
    DChangedAuditRealization = rootDatum realization(cartanDatum DAuditRealization,
        SRAuditRealization*basisAuditRealization,
        SCAuditRealization*transpose inverse basisAuditRealization);
    liftAuditRealization = w -> weightFromRealizationCoordinates(DChangedAuditRealization,
        flatten entries(matrix {realizationCoordinates w}*basisAuditRealization));
    CChangedAuditRealization = moduleCharacter(integrableHighestWeightModule(
        DChangedAuditRealization,liftAuditRealization l0AuditRealization),Depth=>8);
    assert(characterTerms CChangedAuditRealization == new Tally from
        apply(pairs characterTerms C0AuditRealization,p -> liftAuditRealization(p#0)=>p#1));
    RExtendedAuditRealization = realization(cartanDatum DAuditRealization,
        SRAuditRealization|matrix mutableMatrix(QQ,2,1),
        SCAuditRealization|matrix mutableMatrix(QQ,2,1));
    DExtendedAuditRealization = rootDatum RExtendedAuditRealization;
    le0AuditRealization = weightFromRealizationCoordinates(DExtendedAuditRealization,{1,0,0,7});
    le1AuditRealization = weightFromRealizationCoordinates(DExtendedAuditRealization,{1,0,0,10});
    CE0AuditRealization = moduleCharacter(integrableHighestWeightModule(
        DExtendedAuditRealization,le0AuditRealization),Depth=>8);
    CE1AuditRealization = moduleCharacter(integrableHighestWeightModule(
        DExtendedAuditRealization,le1AuditRealization),Depth=>8);
    CESAuditRealization = CE0AuditRealization+CE1AuditRealization;
    for beta in offsetsAuditRealization 8 do (
        shift := toWeight kmRoot(DExtendedAuditRealization,beta);
        assert(characterCoefficient(CESAuditRealization,le0AuditRealization-shift)
            == basicAuditRealization beta);
        assert(characterCoefficient(CESAuditRealization,le1AuditRealization-shift)
            == basicAuditRealization beta);
    );
    assert(#keys characterTerms CESAuditRealization == 2*#keys characterTerms CE0AuditRealization);
    mustErrorAuditRealization = f -> try (f();false) else true;
    assert(mustErrorAuditRealization(() -> characterCoefficient(CESAuditRealization,
        kmWeight(DExtendedAuditRealization,{1,0}))));
    -- The direct product with A1 must reproduce the tensor product of
    -- the basic affine module and the three-dimensional A1 module.
    AMixedAuditRealization = (cartanAffineA 1)++(cartanA 2);
    DMixedAuditRealization = rootDatum AMixedAuditRealization;
    lMixedAuditRealization = weightFromRealizationCoordinates(DMixedAuditRealization,{1,0,2,0});
    CMixedAuditRealization = moduleCharacter(integrableHighestWeightModule(
        DMixedAuditRealization,lMixedAuditRealization),Depth=>8);
    for h from 0 to 8 do for a from 0 to h do for b from 0 to h-a do (
        c := h-a-b;
        expected := if c <= 2 then basicAuditRealization({a,b}) else 0;
        assert(characterCoefficient(CMixedAuditRealization,
            lMixedAuditRealization-toWeight kmRoot(DMixedAuditRealization,{a,b,c})) == expected);
    );
///


-------------------------------------------------------------------------
-- Audit Finite Regression Tests
-------------------------------------------------------------------------

-- Exact finite characters are checked against independent closed formulas.
TEST ///
    DAuditA1 = rootDatum "A1";
    for a from 0 to 7 do (
        literal := formalCharacter(DAuditA1,
            tally for j from 0 to a list kmWeight(DAuditA1,{a-2*j}));
        assert(characterTerms finiteCharacter(DAuditA1,{a}) == characterTerms literal);
        for b from 0 to 7 do (
            expected := new Tally from for j from 0 to min(a,b) list
                (kmWeight(DAuditA1,{a+b-2*j}))=>1;
            assert(decomposeCharacter(finiteCharacter(DAuditA1,{a})*
                finiteCharacter(DAuditA1,{b})) == expected);
        );
    );
    -- The 100th tensor power tests large integers, not just small modules.
    tensorAuditA1 = (finiteCharacter(DAuditA1,{1}))^100;
    assert(characterDimension tensorAuditA1 == 2^100);
    expectedTermsAuditA1 = new Tally from for j from 0 to 100 list
        (kmWeight(DAuditA1,{100-2*j}))=>binomial(100,j);
    assert(characterTerms tensorAuditA1 == expectedTermsAuditA1);
    expectedDecompositionAuditA1 = new Tally from for j from 0 to 50 list
        (kmWeight(DAuditA1,{100-2*j}))=>
            (binomial(100,j)-(if j == 0 then 0 else binomial(100,j-1)));
    assert(decomposeCharacter tensorAuditA1 == expectedDecompositionAuditA1);
    assert(characterTerms characterFromDecomposition(DAuditA1,
        expectedDecompositionAuditA1) == expectedTermsAuditA1);
///

TEST ///
    -- Check the entire Weyl-character identity, rather than only weights
    -- whose support has already been found by the multiplicity algorithm.
    for fixture in {
        {"A2",{4,3}}, {"B2",{3,2}}, {"C2",{2,3}},
        {"G2",{2,1}}, {"G2",{1,2}}, {"A3",{2,1,1}}
    } do (
        D := rootDatum fixture#0;
        A := datumCartanMatrix D;
        r := numrows A;
        zero := kmWeight(D,toList(r:0));
        rho := kmWeight(D,toList(r:1));
        lambda := kmWeight(D,fixture#1);
        denominatorCharacter := formalCharacter(D,new Tally from {zero=>1});
        for alphaCoordinates in entries cartanPositiveRoots A do (
            alpha := toWeight kmRoot(D,alphaCoordinates);
            denominatorCharacter = denominatorCharacter*
                formalCharacter(D,new Tally from {zero=>1,(-alpha)=>-1});
        );
        alternant := formalCharacter(D,new Tally);
        for legacyElement in weylGroupElements A do (
            w := weylElement(D,legacyElement#"word");
            exponent := weylAction(w,lambda+rho)-rho;
            alternant = alternant+(-1)^(weylLength w)*
                formalCharacter(D,new Tally from {exponent=>1});
        );
        C := finiteCharacter(D,lambda);
        assert(characterTerms(C*denominatorCharacter) == characterTerms alternant);
    );
///

TEST ///
    -- Expand weight repetitions into distinct labeled basis vectors and
    -- enumerate symmetric monomials or exterior subsets directly.
    bruteBasisPowerAuditFinite = (C,k,exterior) -> (
        D := characterDatum C;
        basisWeights := flatten apply(pairs characterTerms C,p -> toList(p#1:p#0));
        n := #basisWeights;
        zero := 0*first basisWeights;
        if k == 0 then return formalCharacter(D,new Tally from {zero=>1});
        indexSets := if exterior then subsets(toList(0..n-1),k)
            else apply(subsets(toList(0..n+k-2),k),S -> apply(k,j -> S#j-j));
        formalCharacter(D,tally apply(indexSets,S -> sum(S,j -> basisWeights#j)))
    );
    DAuditLambda = rootDatum "A2";
    for labels in {{1,0},{1,1},{2,0}} do (
        C := finiteCharacter(DAuditLambda,labels);
        for k from 0 to 3 do (
            assert(characterTerms symmetricPowerCharacter(C,k) ==
                characterTerms bruteBasisPowerAuditFinite(C,k,false));
            assert(characterTerms exteriorPowerCharacter(C,k) ==
                characterTerms bruteBasisPowerAuditFinite(C,k,true));
        );
        for a from 1 to 3 do for b from 1 to 3 do assert(
            characterTerms adamsOperation(a,adamsOperation(b,C)) ==
                characterTerms adamsOperation(a*b,C));
        CdualStandard := finiteCharacter(DAuditLambda,{0,1});
        Csum := C+CdualStandard;
        for k from 0 to 4 do (
            E := formalCharacter(DAuditLambda,new Tally);
            S := formalCharacter(DAuditLambda,new Tally);
            for j from 0 to k do (
                E = E+exteriorPowerCharacter(C,j)*exteriorPowerCharacter(CdualStandard,k-j);
                S = S+symmetricPowerCharacter(C,j)*symmetricPowerCharacter(CdualStandard,k-j);
            );
            assert(characterTerms exteriorPowerCharacter(Csum,k) == characterTerms E);
            assert(characterTerms symmetricPowerCharacter(Csum,k) == characterTerms S);
        );
    );
///

TEST ///
    -- Exterior duality retains central coordinates even in a skew basis.
    GAuditDeterminant = matrix(QQ,{{1,2},{1/3,1}});
    RAuditDeterminant = realization(cartanDatum "A1",
        matrix(QQ,{{2,0}})*GAuditDeterminant,
        matrix(QQ,{{1,0}})*transpose inverse GAuditDeterminant);
    DAuditDeterminant = rootDatum RAuditDeterminant;
    fullWeightAuditDeterminant = v -> weightFromRealizationCoordinates(
        DAuditDeterminant,flatten entries(matrix(QQ,{v})*GAuditDeterminant));
    for n from 1 to 6 do (
        top := fullWeightAuditDeterminant{n,2/7};
        C := finiteCharacter(DAuditDeterminant,top);
        determinant := formalCharacter(DAuditDeterminant,new Tally from {
            (fullWeightAuditDeterminant{0,(n+1)*2/7})=>1});
        assert(characterTerms exteriorPowerCharacter(C,n+1) == characterTerms determinant);
        for k from 0 to n+1 do (
            expected := determinant*dualCharacter exteriorPowerCharacter(C,n+1-k);
            assert(characterTerms exteriorPowerCharacter(C,k) == characterTerms expected);
        );
    );
///

TEST ///
    -- The minuscule A4 tableau crystals are precisely the subset model
    -- of wedge^k of the five-dimensional standard representation.
    DAuditWedge = rootDatum "A4";
    for k from 1 to 4 do (
        labels := for j from 1 to 4 list if j == k then 1 else 0;
        B := highestWeightCrystal(DAuditWedge,labels,MaxElements=>100);
        expectedData := set apply(subsets(toList(1..5),k),S -> apply(S,x -> {x}));
        assert(set apply(crystalVertices B,crystalTableau) == expectedData);
        for b in crystalVertices B do for i from 1 to 4 do (
            column := flatten crystalTableau b;
            f := crystalF(b,i);
            expectedColumn := if member(i,column) and not member(i+1,column)
                then apply(column,x -> if x == i then i+1 else x) else null;
            assert(if expectedColumn === null then f === null
                else crystalTableau f == apply(expectedColumn,x -> {x}));
        );
    );
///

TEST ///
    -- Hook-content dimensions and lowering-operator connectivity for
    -- larger multirow semistandard tableaux are independent checks.
    hookContentAuditFinite = (shape,N) -> (
        answer := 1_QQ;
        for i from 0 to #shape-1 do for j from 0 to shape#i-1 do (
            below := #select(toList(i+1..#shape-1),a -> shape#a > j);
            hook := shape#i-j+below;
            answer = answer*(N+j-i)/hook;
        );
        answer
    );
    for fixture in {{"A3",{3,1,0}},{"A3",{1,2,1}},
        {"A4",{2,0,1,0}},{"A4",{0,1,0,1}}} do (
        D := rootDatum fixture#0;
        labels := fixture#1;
        r := #labels;
        shape := select(for i from 0 to r-1 list
            sum(for j from i to r-1 list labels#j),x -> x > 0);
        B := highestWeightCrystal(D,labels,MaxElements=>1000);
        vertices := crystalVertices B;
        assert(#vertices == hookContentAuditFinite(shape,r+1));
        highest := select(vertices,b -> all(toList(1..r),i -> crystalE(b,i) === null));
        assert(#highest == 1);
        visited := new MutableHashTable from {crystalVertexData first highest=>true};
        queue := {first highest};
        while #queue > 0 do (
            b := first queue;
            queue = drop(queue,1);
            for i from 1 to r do (
                next := crystalF(b,i);
                if next =!= null and not visited#?(crystalVertexData next) then (
                    visited#(crystalVertexData next) = true;
                    queue = append(queue,next);
                );
            );
        );
        assert(#keys visited == #vertices);
    );
///

TEST ///
    -- Symmetric and exterior powers reject virtual characters, while
    -- Adams operations remain linear on their integer coefficients.
    mustErrorAuditVirtual = f -> try (f();false) else true;
    negativeAuditVirtual = -finiteCharacter(rootDatum "A1",{1});
    assert(mustErrorAuditVirtual(() -> symmetricPowerCharacter(negativeAuditVirtual,2)));
    assert(mustErrorAuditVirtual(() -> exteriorPowerCharacter(negativeAuditVirtual,2)));
    assert(characterTerms adamsOperation(2,negativeAuditVirtual) ==
        characterTerms(-adamsOperation(2,-negativeAuditVirtual)));
    for fixture in {{cartanA 4,{1,1,1}},{cartanB 4,{1,1,1,1}},
        {cartanC 4,{1,2,2,1}},{cartanF4(),{1,2,3,2}},{cartanG2(),{2,1}}} do
        assert(highestShortRoot fixture#0 == fixture#1);
///

TEST ///
    -- The canonical first-node normalization agrees with these
    -- independently written epsilon-coordinate Euclidean Gram matrices.
    for fixture in {{cartanA 5,"A",4},{cartanB 4,"B",4},
        {cartanC 4,"C",4},{cartanD 4,"D",4}} do (
        A := fixture#0;
        kind := fixture#1;
        r := fixture#2;
        N := if kind == "A" then r+1 else r;
        S := for i from 0 to r-1 list (
            if i < r-1 or kind == "A" then
                for j from 0 to N-1 list
                    (if j == i then 1 else if j == i+1 then -1 else 0)
            else if kind == "B" then
                for j from 0 to N-1 list if j == r-1 then 1 else 0
            else if kind == "C" then
                for j from 0 to N-1 list if j == r-1 then 2 else 0
            else for j from 0 to N-1 list if j == r-2 or j == r-1 then 1 else 0
        );
        for i from 0 to r-1 do for j from 0 to r-1 do (
            ei := for k from 0 to r-1 list if k == i then 1 else 0;
            ej := for k from 0 to r-1 list if k == j then 1 else 0;
            expected := sum(N,k -> S#i#k*S#j#k);
            assert(rootInnerProduct(A,ei,ej) == expected);
        );
        u := toList(1..r);
        v := reverse u;
        w := toList(r:2);
        assert(rootInnerProduct(A,apply(r,i -> u#i+v#i),w) ==
            rootInnerProduct(A,u,w)+rootInnerProduct(A,v,w));
    );
///


-------------------------------------------------------------------------
-- General Crystals Tests
-------------------------------------------------------------------------

-- Exact path crystals and tensor products: independent characters, every
-- string, connected components, and associativity of the tensor rule.

draftAuditGeneralCrystalAxioms = B -> (
    D := crystalDatum B;
    A := datumCartanMatrix D;
    r := numrows A;
    vertices := crystalVertices B;
    assert(instance(B,Crystal));
    assert(#vertices == #(set apply(vertices,crystalVertexData)));
    for b in vertices do (
        assert(instance(b,CrystalVertex));
        assert(crystalVertex(B,crystalVertexData b) === b);
        wt := crystalWeight b;
        assert(parentDatum wt === D);
        for i from 1 to r do (
            epsilon := crystalEpsilon(b,i);
            phi := crystalPhi(b,i);
            assert(instance(epsilon,ZZ) and epsilon >= 0);
            assert(instance(phi,ZZ) and phi >= 0);
            assert(phi-epsilon == (datumCoordinates wt)#(i-1));
            lower := crystalF(b,i);
            upper := crystalE(b,i);
            if lower =!= null then (
                assert(crystalE(lower,i) === b);
                assert(crystalWeight lower === draftWeightMinusRoot(wt,kmRoot(D,
                    for j from 1 to r list if j == i then 1 else 0)));
                assert(crystalPhi(lower,i) == phi-1);
                assert(crystalEpsilon(lower,i) == epsilon+1);
            );
            if upper =!= null then (
                assert(crystalF(upper,i) === b);
                assert(crystalWeight upper === draftWeightMinusRoot(wt,kmRoot(D,
                    for j from 1 to r list if j == i then -1 else 0)));
            );
            cursor := b;
            for stringStep from 1 to phi do cursor = crystalF(cursor,i);
            assert(cursor =!= null and crystalF(cursor,i) === null);
            cursor = b;
            for stringStep from 1 to epsilon do cursor = crystalE(cursor,i);
            assert(cursor =!= null and crystalE(cursor,i) === null);
        );
    );
    true
);

TEST ///

checkGeneralCrystal = value((package "KacMoody")#"private dictionary")#"draftAuditGeneralCrystalAxioms";
for fixture in {
    {cartanB 2,{1,1}}, {cartanB 2,{2,0}}, {cartanB 2,{0,2}},
    {cartanC 2,{1,2}}, {cartanB 3,{0,1,0}}, {cartanC 3,{0,0,1}},
    {cartanD 4,{0,1,0,0}}, {cartanF4(),{0,0,0,1}},
    {cartanG2(),{1,1}}, {cartanG2(),{2,1}},
    {cartanE 6,{1,0,0,0,0,0}}, {cartanE 7,{0,0,0,0,0,0,1}},
    {(cartanA 3)++(cartanB 2),{1,1,0,1}}
} do (
    A := fixture#0;
    D := rootDatum A;
    lambda := kmWeight(D,fixture#1);
    B := highestWeightCrystal(D,lambda);
    assert(B#"model" === "LittelmannPaths");
    assert(#crystalVertices B == weylDimension(A,fixture#1));
    assert(checkGeneralCrystal B);
    assert(characterTerms crystalCharacter B == characterTerms finiteCharacter(D,lambda));
    highest := select(crystalVertices B,b ->
        all(toList(1..numrows A),i -> crystalE(b,i) === null));
    assert(#highest == 1 and crystalWeight(first highest) === lambda);

    -- Each segment has a Weyl translate of lambda as its velocity.  This
    -- verifies parametrization as well as the endpoint character.
    if numrows A <= 4 then (
        orbit := set weightOrbit(A,fixture#1);
        for b in crystalVertices B do (
            path := crystalVertexData b;
            assert((first path)#0 == 0 and (last path)#0 == 1);
            assert((first path)#1 == toList(numrows A:0));
            for k from 0 to #path-2 do (
                a := path#k;
                c := path#(k+1);
                assert(a#0 < c#0);
                velocity := for j from 0 to numrows A-1 list (
                    coordinate := (c#1#j-a#1#j)/(c#0-a#0)+0_QQ;
                    assert(denominator coordinate == 1);
                    numerator coordinate
                );
                assert(member(velocity,orbit));
            );
        );
    );
);

-- The adjoint has two distinct zero-weight vertices, not one vertex of
-- multiplicity two.  Their path data and displayed paths remain distinct.
D := rootDatum "B2";
B := highestWeightCrystal(D,{0,2});
zeroVertices := select(crystalVertices B,b -> datumCoordinates crystalWeight b == {0,0});
assert(#zeroVertices == 2);
assert(crystalVertexData(first zeroVertices) != crystalVertexData(last zeroVertices));
assert(toString(first zeroVertices) != toString(last zeroVertices));

///

TEST ///

pathData = value((package "KacMoody")#"private dictionary")#"draftCrystalPathData";
-- Compare the graph, not merely its character, with the independent type-A
-- tableau model and the classification-free minuscule Weyl-orbit model.
checkCrystalIsomorphism = (B,C) -> (
    r := numrows datumCartanMatrix crystalDatum B;
    hB := first select(crystalVertices B,b -> all(toList(1..r),i -> crystalE(b,i) === null));
    hC := first select(crystalVertices C,b -> all(toList(1..r),i -> crystalE(b,i) === null));
    mapping := new MutableHashTable;
    mapping#hB = hC;
    queue := new MutableList;
    queue#0 = hB;
    head := 0;
    while head < #queue do (
        b := queue#head;
        c := mapping#b;
        head = head+1;
        assert(crystalWeight b === crystalWeight c);
        for i from 1 to r do (
            fB := crystalF(b,i);
            fC := crystalF(c,i);
            assert((fB === null) == (fC === null));
            assert(crystalEpsilon(b,i) == crystalEpsilon(c,i));
            assert(crystalPhi(b,i) == crystalPhi(c,i));
            if fB =!= null then (
                if mapping#?fB then assert(mapping#fB === fC)
                else (mapping#fB=fC; queue#(#queue)=fB);
            );
        );
    );
    assert(#queue == #crystalVertices B and #queue == #crystalVertices C);
    assert(#(set values mapping) == #queue);
    true
);
for labels in {{0,0},{1,0},{0,1},{1,1},{2,1},{1,2},{2,2}} do (
    D := rootDatum "A2";
    paths := pathData(datumCartanMatrix D,labels,100000);
    B := new Crystal from {
        "parent"=>D,"model"=>"LittelmannPaths","highestWeight"=>kmWeight(D,labels),
        "rank"=>2,"canonicalNodeOrder"=>{1,2},"vertexData"=>paths};
    assert(checkCrystalIsomorphism(B,highestWeightCrystal(D,labels)));
);
for fixture in {{"B3",3},{"C3",1},{"D4",4},{"E6",1},{"E7",7}} do (
    D := rootDatum fixture#0;
    labels := for i from 1 to numrows datumCartanMatrix D list if i == fixture#1 then 1 else 0;
    assert(checkCrystalIsomorphism(highestWeightCrystal(D,labels),minusculeCrystal(D,labels)));
);

///

TEST ///

checkGeneralCrystal = value((package "KacMoody")#"private dictionary")#"draftAuditGeneralCrystalAxioms";
for fixture in {
    {"A1",{3},{2}}, {"A2",{1,0},{0,1}}, {"A2",{1,1},{1,0}},
    {"B2",{0,1},{0,1}}, {"B2",{1,0},{0,1}}, {"G2",{1,0},{1,0}}
} do (
    D := rootDatum fixture#0;
    r := numrows datumCartanMatrix D;
    B := highestWeightCrystal(D,fixture#1);
    C := highestWeightCrystal(D,fixture#2);
    T := tensorProductCrystal(B,C);
    assert(checkGeneralCrystal T);
    assert(#crystalVertices T == #crystalVertices B * #crystalVertices C);
    product := crystalCharacter B*crystalCharacter C;
    assert(characterTerms crystalCharacter T == characterTerms product);
    highest := select(crystalVertices T,b -> all(toList(1..r),i -> crystalE(b,i) === null));
    decomposition := tally apply(highest,crystalWeight);
    assert(decomposition == decomposeCharacter product);

    -- Every component individually has its highest-weight character.  A
    -- highest-weight tally alone could miss defective or intersecting graphs.
    covered := new MutableHashTable;
    for h in highest do (
        component := new MutableHashTable;
        queue := new MutableList;
        queue#0=h;
        component#h=true;
        head := 0;
        while head < #queue do (
            b := queue#head;
            head=head+1;
            for i from 1 to r do (
                lower := crystalF(b,i);
                if lower =!= null and not component#?lower then (
                    component#lower=true;
                    queue#(#queue)=lower;
                );
            );
        );
        for b in toList queue do (
            assert(not covered#?b);
            covered#b=true;
        );
        componentCharacter := formalCharacter(D,tally apply(toList queue,crystalWeight));
        assert(characterTerms componentCharacter ==
            characterTerms finiteCharacter(D,crystalWeight h));
    );
    assert(#keys covered == #crystalVertices T);
);

-- Associativity is a vertex-level isomorphism, including all e_i and f_i,
-- under ((b,c),d) <-> (b,(c,d)).  This detects tie-condition errors in the
-- tensor rule which character multiplication cannot see.
for fixture in {{"A1",{2}},{"A2",{1,0}},{"G2",{1,0}}} do (
    D := rootDatum fixture#0;
    r := numrows datumCartanMatrix D;
    B := highestWeightCrystal(D,fixture#1);
    BC := tensorProductCrystal(B,B);
    left := tensorProductCrystal(BC,B);
    right := tensorProductCrystal(B,BC);
    rebracket := b -> (
        data := crystalVertexData b;
        crystalVertex(right,{data#0#0,{data#0#1,data#1}})
    );
    for b in crystalVertices left do (
        c := rebracket b;
        assert(crystalWeight b === crystalWeight c);
        for i from 1 to r do (
            for operation in {crystalE,crystalF} do (
                operatorResult := operation(b,i);
                assert((operatorResult === null and operation(c,i) === null) or
                    (operatorResult =!= null and rebracket(operatorResult) === operation(c,i)));
            );
            assert(crystalEpsilon(b,i) == crystalEpsilon(c,i));
            assert(crystalPhi(b,i) == crystalPhi(c,i));
        );
    );
);

///

TEST ///

checkGeneralCrystal = value((package "KacMoody")#"private dictionary")#"draftAuditGeneralCrystalAxioms";
mustErrorGeneralCrystal = f -> try (f(); false) else true;
A := cartanB 2;
permutation := {1,0};
Ap := submatrix(A,permutation,permutation);
D := rootDatum cartanDatum(Ap,NodeLabels=>{"spin","vector"});
B := highestWeightCrystal(D,{1,1},MaxElements=>16);
assert(#crystalVertices B == 16);
assert(checkGeneralCrystal B);
for b in crystalVertices B do (
    assert(crystalF(b,"spin") === crystalF(b,1));
    assert(crystalE(b,"vector") === crystalE(b,2));
);
assert(mustErrorGeneralCrystal(() -> highestWeightCrystal(D,{1,1},MaxElements=>15)));
assert(mustErrorGeneralCrystal(() -> crystalF(first crystalVertices B,"missing")));
assert(mustErrorGeneralCrystal(() -> crystalE(first crystalVertices B,0)));

-- Full central coordinates survive root operators and tensor products.
R := realization(cartanDatum A,matrix(QQ,{{2,-2,0},{-1,2,0}}),
    matrix(QQ,{{1,0,0},{0,1,0}}));
Dcentral := rootDatum R;
lambda := weightFromRealizationCoordinates(Dcentral,{1,1,5/2});
mu := weightFromRealizationCoordinates(Dcentral,{0,1,-5/2});
Bcentral := highestWeightCrystal(Dcentral,lambda);
Ccentral := highestWeightCrystal(Dcentral,mu);
assert(checkGeneralCrystal Bcentral);
assert(all(crystalVertices Bcentral,b -> (realizationCoordinates crystalWeight b)#2 == 5/2));
assert(characterTerms crystalCharacter Bcentral == characterTerms finiteCharacter(Dcentral,lambda));
Tcentral := tensorProductCrystal(Bcentral,Ccentral);
assert(checkGeneralCrystal Tcentral);
assert(all(crystalVertices Tcentral,b -> (realizationCoordinates crystalWeight b)#2 == 0));
assert(characterTerms crystalCharacter Tcentral ==
    characterTerms(crystalCharacter Bcentral*crystalCharacter Ccentral));
assert(mustErrorGeneralCrystal(() -> tensorProductCrystal(Bcentral,
    highestWeightCrystal(Dcentral,{0,1}))));
assert(mustErrorGeneralCrystal(() -> tensorProductCrystal(Bcentral,
    highestWeightCrystal(rootDatum "B2",{0,1}))));
assert(mustErrorGeneralCrystal(() -> tensorProductCrystal(Bcentral,Ccentral,MaxElements=>63)));
assert(#crystalVertices tensorProductCrystal(Bcentral,Ccentral,MaxElements=>64) == 64);
assert(mustErrorGeneralCrystal(() -> tensorProductCrystal(Bcentral,Ccentral,MaxElements=>0)));
assert(mustErrorGeneralCrystal(() -> tensorProductCrystal(Bcentral,Ccentral,MaxElements=>false)));
assert(#crystalVertices tensorProductCrystal(Bcentral,Ccentral,MaxElements=>null) == 64);

-- Change both realization bases by a rational matrix, transporting the
-- character lattice as well.  The center need not be a coordinate axis.
G := matrix(QQ,{{1/2,1,2},{0,2/3,1},{1,0,3}});
Dskew := rootDatum(realization(cartanDatum A,
    matrix(QQ,R#"simpleRootCoordinates")*G,
    matrix(QQ,R#"simpleCorootCoordinates")*transpose inverse G),CharacterLattice=>G);
transport := w -> weightFromRealizationCoordinates(Dskew,
    flatten entries(matrix(QQ,{realizationCoordinates w})*G));
Bskew := highestWeightCrystal(Dskew,transport lambda);
Cskew := highestWeightCrystal(Dskew,transport mu);
Tskew := tensorProductCrystal(Bskew,Cskew);
for modelPair in {{Bcentral,Bskew},{Ccentral,Cskew},{Tcentral,Tskew}} do (
    source := modelPair#0;
    target := modelPair#1;
    assert(set apply(crystalVertices source,crystalVertexData) ==
        set apply(crystalVertices target,crystalVertexData));
    for b in crystalVertices source do (
        image := crystalVertex(target,crystalVertexData b);
        assert(crystalWeight image === transport crystalWeight b);
        for i from 1 to 2 do for operation in {crystalE,crystalF} do (
            sourceResult := operation(b,i);
            targetResult := operation(image,i);
            assert((sourceResult === null and targetResult === null) or
                (sourceResult =!= null and targetResult =!= null and
                    crystalVertexData sourceResult == crystalVertexData targetResult));
        );
    );
);
assert(characterTerms crystalCharacter Bskew ==
    characterTerms finiteCharacter(Dskew,transport lambda));

-- Rank-zero tori and disconnected semisimple algebras are finite too.
A0 := map(ZZ^0,ZZ^0,0);
R0 := realization(cartanDatum A0,map(QQ^0,QQ^2,0),map(QQ^0,QQ^2,0));
D0 := rootDatum R0;
lambda0 := weightFromRealizationCoordinates(D0,{2,3/2});
B0 := highestWeightCrystal(D0,lambda0);
assert(#crystalVertices B0 == 1);
assert(crystalWeight(first crystalVertices B0) === lambda0);
assert(checkGeneralCrystal B0);
T0 := tensorProductCrystal(B0,B0);
assert(crystalWeight(first crystalVertices T0) === 2*lambda0);
assert(characterDimension crystalCharacter T0 == 1);
Dproduct := rootDatum((cartanA 3)++(cartanG2()));
Bproduct := highestWeightCrystal(Dproduct,{1,0,1,0});
assert(#crystalVertices Bproduct == 21);
assert(checkGeneralCrystal Bproduct);
assert(characterTerms crystalCharacter Bproduct ==
    characterTerms finiteCharacter(Dproduct,{1,0,1,0}));

///


-------------------------------------------------------------------------
-- Audit Geometry Regression Tests
-------------------------------------------------------------------------

TEST ///

-- Every word of length at most five is evaluated in the independent S4
-- permutation model.  Roots e_a-e_b are written as consecutive ones in
-- simple-root coordinates; the inversion roots below are left inversions,
-- so the relevant ordered values, not just their positions, determine them.
A = cartanA 4;
wordPermutationGeometry = word -> (
    p := toList(1..4);
    for i in word do p = apply(4,j ->
        if j == i-1 then p#i else if j == i then p#(i-1) else p#j);
    p
);
permutationLengthGeometry = p -> sum flatten table(4,4,(i,j) ->
    if i < j and p#i > p#j then 1 else 0);
permutationInversionRootsGeometry = p -> flatten flatten table(4,4,(i,j) ->
    if i < j and p#i > p#j then {
        apply(3,k -> if p#j <= k+1 and k+1 < p#i then 1 else 0)
    } else {});
stratumGeometry = {{}};
wordsCheckedGeometry = 0;
for k from 0 to 5 do (
    for word in stratumGeometry do (
        p := wordPermutationGeometry word;
        H := weylInversionData(A,word);
        assert(H#"word" === word);
        assert(H#"length" == permutationLengthGeometry p);
        assert(set(H#"inversions") === set permutationInversionRootsGeometry p);
        assert(#H#"inversions" == H#"length");
        wordsCheckedGeometry = wordsCheckedGeometry+1;
    );
    stratumGeometry = flatten apply(stratumGeometry,word ->
        apply({1,2,3},i -> word|{i}));
);
assert(wordsCheckedGeometry == 364);

-- Cancellation, nonreduced words, empty rank, and invalid public node
-- positions must all retain their actual meanings.
assert((weylInversionData(A,{1,1,2,2,3,3}))#"inversions" === {});
assert((weylInversionData(matrix(ZZ,{}),{}))#"length" == 0);
mustErrorGeometry = f -> (try (f();false) else true);
for badWord in {{0},{4},{1/2},{"1"},{-1}} do
    assert(mustErrorGeometry(() -> weylInversionData(A,badWord)));
assert(mustErrorGeometry(() -> weylInversionData(matrix(ZZ,{}),{1})));
assert(mustErrorGeometry(() -> weylInversionData(matrix{{2,-1},{0,2}},{1})));

///

TEST ///

-- The GL4 description gives Borel--Weil--Bott without using Weyl-group
-- routines: shift by (3,2,1,0), detect repetitions, sort, and count reversed
-- pairs.  Adjacent differences recover Dynkin labels; duality reverses
-- those three labels in type A3.
D = rootDatum "A3";
weightsCheckedGeometry = 0;
for a from -3 to 3 do for b from -3 to 3 do for c from -3 to 3 do (
    shifted := {a+b+c+3,b+c+2,c+1,0};
    R := borelWeilBott(D,{a,b,c});
    singular := #unique shifted < 4;
    assert(R#"vanishes" === singular);
    if singular then (
        assert(R#"degree" === null);
        assert(R#"dominantWeight" === null);
        assert(R#"cohomologyHighestWeight" === null);
    ) else (
        ordered := reverse sort shifted;
        highest := apply(3,i -> ordered#i-ordered#(i+1)-1);
        degree := sum flatten table(4,4,(i,j) ->
            if i < j and shifted#i < shifted#j then 1 else 0);
        assert(R#"degree" == degree);
        assert(datumCoordinates(R#"dominantWeight") === highest);
        assert(datumCoordinates(R#"cohomologyHighestWeight") === reverse highest);
    );
    weightsCheckedGeometry = weightsCheckedGeometry+1;
);
assert(weightsCheckedGeometry == 343);

///

TEST ///

-- An A3 maximal-parabolic nilradical is abelian: all its positive roots
-- have coefficient one at the removed node, so the sum of any two cannot
-- be a root.  With trivial coefficients its cohomology is therefore
-- Lambda^k(n^*) with zero differential.  Compare the complete derived-Levi
-- characters, not merely their dimensions, against Kostant's summands.
D = rootDatum "A3";
A = datumCartanMatrix D;
for j in {1,2,3} do (
    P := standardParabolic(D,RemovedNodes=>{j});
    L := leviNodes P;
    DL := rootDatum parabolicLevi P;
    roots := entries nilradicalRoots P;
    assert(all(roots,beta -> beta#(j-1) == 1));
    n := j*(4-j);
    assert(#roots == n);
    cotangent := formalCharacter(DL,tally apply(roots,beta -> (
        ambientLabels := flatten entries(A * transpose matrix{beta});
        kmWeight(DL,apply(L,i -> -ambientLabels#(i-1)))
    )));
    assert(characterDimension cotangent == n);
    H := kostantLieAlgCohomology(P,{0,0,0});
    dimensions := cohomologyDimensions(P,H);
    assert(sort keys H === toList(0..n));
    for k from 0 to n do (
        exterior := exteriorPowerCharacter(cotangent,k);
        summands := new Tally from apply(pairs H#k,p ->
            (kmWeight(DL,flatten toList p#0)) => p#1);
        cohomologyCharacter := characterFromDecomposition(DL,summands);
        assert(dimensions#k == binomial(n,k));
        assert(characterDimension exterior == binomial(n,k));
        assert(characterTerms exterior === characterTerms cohomologyCharacter);
    );
    assert(characterDimension exteriorPowerCharacter(cotangent,n+1) == 0);
);

-- Whole-algebra and Borel parabolics provide the opposite boundaries.
Pwhole := standardParabolic(D,RemovedNodes=>{});
Hwhole := kostantLieAlgCohomology(Pwhole,{2,1,0});
assert(sort keys Hwhole === {0});
assert((cohomologyDimensions(Pwhole,Hwhole))#0 == 45);
Pbor := standardParabolic(D,LeviNodes=>{});
Hbor := kostantLieAlgCohomology(Pbor,{0,0,0});
dimensionsBor := cohomologyDimensions(Pbor,Hbor);
assert(apply(toList(0..6),k -> dimensionsBor#k) === {1,3,5,6,5,3,1});

-- Changing node order must preserve these graded dimensions, including a
-- nontrivial coefficient representation and a disconnected derived Levi.
permutation := {2,3,1};
Ap := matrix table(3,3,(i,j) -> A_(permutation#i-1,permutation#j-1));
Dp := rootDatum Ap;
for j in {1,2,3} do (
    oldP := standardParabolic(D,RemovedNodes=>{j});
    newP := standardParabolic(Dp,
        RemovedNodes=>{first positions(permutation,i -> i == j)+1});
    oldH := kostantLieAlgCohomology(oldP,{2,1,0});
    newH := kostantLieAlgCohomology(newP,apply(permutation,i -> ({2,1,0})#(i-1)));
    assert(cohomologyDimensions(oldP,oldH) === cohomologyDimensions(newP,newH));
);

///

TEST ///

-- Demazure operators obey idempotence and the 3-, 4-, and 6-term braid
-- relations on the entire integral group ring, not just dominant weights.
-- Virtual sums also exercise cancellation through the negative branches.
applyOperatorsGeometry = (C,word) -> (
    answer := C;
    for i in reverse word do answer = demazureOperator(i,answer);
    answer
);
for A in {cartanA 3,cartanB 2,cartanG2()} do (
    D := rootDatum A;
    m := coxeterMatrix(A,1,2);
    for labels in {{-2,1},{0,-1},{2,-3},{3,2}} do (
        lambda := kmWeight(D,labels);
        monomial := formalCharacter(D,new Tally from {lambda=>1});
        virtualSum := formalCharacter(D,new Tally from {
            lambda=>2,(-lambda)=>-1,(0*lambda)=>3});
        for C in {monomial,virtualSum} do (
            for i in {1,2} do (
                single := demazureOperator(i,C);
                assert(characterTerms demazureOperator(i,single) === characterTerms single);
                assert(characterTerms demazureOperator(C,i) === characterTerms single);
            );
            lhs := applyOperatorsGeometry(C,apply(m,k -> if k%2 == 0 then 1 else 2));
            rhs := applyOperatorsGeometry(C,apply(m,k -> if k%2 == 0 then 2 else 1));
            assert(characterTerms lhs === characterTerms rhs);
        );
    );
);
mustErrorGeometry = f -> (try (f();false) else true);
D = rootDatum "A3";
assert(mustErrorGeometry(() -> demazureOperator(1,formalCharacter(D,
    new Tally from {(kmWeight(D,{1/2,0,0}))=>1}))));
assert(mustErrorGeometry(() -> demazureOperator(0,formalCharacter(D,new Tally))));
assert(mustErrorGeometry(() -> demazureOperator(4,formalCharacter(D,new Tally))));

///

TEST ///

-- Rank-zero Borel--Weil--Bott is the cohomology of a point.  For A1+A1,
-- independently known P1 cohomology combines by the Kunneth formula.
Dzero := rootDatum parabolicLevi standardParabolic(rootDatum "A3",LeviNodes=>{});
Rzero := borelWeilBott(Dzero,{});
assert(not Rzero#"vanishes" and Rzero#"degree" == 0);
assert(datumCoordinates(Rzero#"cohomologyHighestWeight") === {});
assert(characterDimension borelWeilBottCharacter(Dzero,{}) == 1);
assert(sort keys kostantLieAlgCohomology(standardParabolic(Dzero,{}),{}) === {0});
D11 := rootDatum((cartanA 2)++(cartanA 2));
for a from -4 to 4 do for b from -4 to 4 do (
    R := borelWeilBott(D11,{a,b});
    singular := a == -1 or b == -1;
    assert(R#"vanishes" === singular);
    C := borelWeilBottCharacter(D11,{a,b});
    if singular then assert(characterDimension C == 0) else (
        highest := {if a >= 0 then a else -a-2,if b >= 0 then b else -b-2};
        assert(R#"degree" == (if a < -1 then 1 else 0)+(if b < -1 then 1 else 0));
        assert(datumCoordinates(R#"cohomologyHighestWeight") === highest);
        assert(characterDimension C == (highest#0+1)*(highest#1+1));
    );
);

-- Typed parabolics must detect a finite quotient even when the untouched
-- Levi component is infinite.  An infinite Borel quotient still requires
-- a degree bound, and asking for finite dimensions of an infinite-type
-- Levi is rejected rather than treated as a finite root system.
mustErrorGeometry = f -> (try (f();false) else true);
Dinf := rootDatum((cartanAffineA 1)++(cartanA 2));
Pfinite := standardParabolic(Dinf,RemovedNodes=>{3});
Hfinite := kostantLieAlgCohomology(Pfinite,{0,0,0});
assert(sort keys Hfinite === {0,1});
assert(apply({0,1},k -> sum values(Hfinite#k)) === {1,1});
assert(all(flatten apply(values Hfinite,T -> keys T),V ->
    all(flatten toList V,x -> x == 0)));
assert(mustErrorGeometry(() -> kostantLieAlgCohomology(Pfinite,{0,0,0},MaxElements=>1)));
Pwhole := standardParabolic(Dinf,RemovedNodes=>{});
Hwhole := kostantLieAlgCohomology(Pwhole,{1,0,2});
assert(sort keys Hwhole === {0});
assert(sum values(Hwhole#0) == 1);
assert(mustErrorGeometry(() -> cohomologyDimensions(Pwhole,Hwhole)));
assert(mustErrorGeometry(() -> kostantLieAlgCohomology(
    standardParabolic(Dinf,LeviNodes=>{}),{0,0,0})));
assert(mustErrorGeometry(() -> borelWeilBott(Dinf,{0,0,0})));
assert(mustErrorGeometry(() -> borelWeilBott(rootDatum "A3",{1/2,0,0})));
assert(mustErrorGeometry(() -> standardParabolic(rootDatum "A3",{1},LeviNodes=>{2,3})));

///


-------------------------------------------------------------------------
-- Audit Result Types Tests
-------------------------------------------------------------------------

-- The mathematical result types are checked through their own operations,
-- not solely through their printed representations.
TEST ///
    D := rootDatum "A2";
    lambda := kmWeight(D,{1,1});
    W := weylGroup D;
    assert(instance(W,KMWeylGroup));
    assert(weylLength weylElement(W,{1,2,1}) == 3);

    V := simpleHighestWeightModule(D,lambda);
    assert(instance(V,KMRepresentation));
    assert(V#"parent" === D and V#"highestWeight" === lambda);

    B := bggResolution(D,lambda);
    assert(instance(B,BGGResolution));
    assert(apply(toList(0..3),d -> #keys(B#"terms"#d)) == {1,2,2,1});
    assert((first keys(B#"terms"#0))#"highestWeight" === lambda);

    R := borelWeilBott(D,{-3,2});
    assert(instance(R,BorelWeilBottResult));
    assert(R#"degree" == 1);
    assert(datumCoordinates(R#"cohomologyHighestWeight") == {0,1});
    assert(characterDimension borelWeilBottCharacter(D,{-3,2}) == 3);

    P := standardParabolic(D,RemovedNodes=>{2});
    decomposition := decomposeLeviCharacter restrictCharacter(P,finiteCharacter(D,lambda));
    assert(#keys decomposition == 4);
    assert(all(keys decomposition,h -> instance(h,LeviHighestWeight)));
    assert(sum apply(pairs decomposition,p -> p#1*weylDimension(
        datumCartanMatrix parabolicLevi P,datumCoordinates semisimplePart p#0)) == 8);
///


------------------------------------------------------------------------
-- Levi formal-character arithmetic
------------------------------------------------------------------------

TEST ///
-- Independent fixtures for LeviFormalCharacter arithmetic.
-- Intended as the body of one permanent TEST block.

mustErrorLeviArithmetic = f -> (try (f(); false) else true);
sameLeviTermsArithmetic = (C,E) -> (
    assert(instance(C,LeviFormalCharacter));
    assert(characterDatum C === characterDatum E);
    assert(isExactCharacter C == isExactCharacter E);
    assert(characterTerms C === characterTerms E);
);

-- The standard SL_3 module restricts as
-- V_A1(1) with central weight -1, plus V_A1(0) with central weight 2.
-- Its dual reverses the two central weights.  This gives a decomposition
-- oracle that is independent of character multiplication itself.
DaLArith = rootDatum "A2";
PaLArith = standardParabolic(DaLArith,RemovedNodes=>{1});
LaLArith = reductiveLeviDatum(PaLArith,DaLArith);
CaLArith = finiteCharacter(DaLArith,{1,0});
EaLArith = finiteCharacter(DaLArith,{0,1});
RaLArith = restrictCharacter(LaLArith,CaLArith);
SaLArith = restrictCharacter(LaLArith,EaLArith);
sameLeviTermsArithmetic(RaLArith+SaLArith,
    restrictCharacter(LaLArith,CaLArith+EaLArith));
sameLeviTermsArithmetic(RaLArith-SaLArith,
    restrictCharacter(LaLArith,CaLArith-EaLArith));
sameLeviTermsArithmetic(-RaLArith,restrictCharacter(LaLArith,-CaLArith));
sameLeviTermsArithmetic(3*RaLArith,restrictCharacter(LaLArith,3*CaLArith));
sameLeviTermsArithmetic(RaLArith*(-3),restrictCharacter(LaLArith,(-3)*CaLArith));
sameLeviTermsArithmetic(RaLArith*SaLArith,
    restrictCharacter(LaLArith,CaLArith*EaLArith));
sameLeviTermsArithmetic(SaLArith*RaLArith,RaLArith*SaLArith);
sameLeviTermsArithmetic(RaLArith^5,restrictCharacter(LaLArith,CaLArith^5));
sameLeviTermsArithmetic(RaLArith*(SaLArith+RaLArith),
    RaLArith*SaLArith+RaLArith^2);
sameLeviTermsArithmetic((RaLArith*SaLArith)*RaLArith,
    RaLArith*(SaLArith*RaLArith));
unitLArith = RaLArith^0;
assert(isExactCharacter unitLArith);
assert(characterDimension unitLArith == 1);
assert(characterCoefficient(unitLArith,
    restrictWeight(LaLArith,kmWeight(DaLArith,{0,0}))) == 1);
sameLeviTermsArithmetic(unitLArith*RaLArith,RaLArith);
sameLeviTermsArithmetic(RaLArith^1,RaLArith);
zeroLArith = 0*RaLArith;
assert(isExactCharacter zeroLArith);
assert(characterDimension zeroLArith == 0);
sameLeviTermsArithmetic(RaLArith-RaLArith,zeroLArith);
sameLeviTermsArithmetic(zeroLArith*RaLArith,zeroLArith);
assert(mustErrorLeviArithmetic(() -> RaLArith^(-1)));

decompositionLArith = decomposeLeviCharacter(RaLArith*SaLArith);
labelsCentersLArith = new Tally from apply(pairs decompositionLArith,p ->
    {datumCoordinates semisimplePart p#0,centralCharacter p#0} => p#1);
assert(labelsCentersLArith === new Tally from {
    {{2},{0}}=>1, {{0},{0}}=>2, {{1},{-3}}=>1, {{1},{3}}=>1});
assert(characterDimension(RaLArith*SaLArith) == 9);
assert(characterDimension(RaLArith+SaLArith) == 6);
assert(characterDimension(RaLArith-SaLArith) == 0);
assert(mustErrorLeviArithmetic(() -> decomposeLeviCharacter(-RaLArith)));

-- Direct records must not silently reinterpret foreign semisimple data,
-- fractional multiplicities, or malformed central coordinates.
foreignSemiLArith = rootDatum(cartanA 2,GlobalForm=>"Adjoint");
for malformedWeightLArith in {
    new LeviWeight from {"parent"=>LaLArith,
        "semisimplePart"=>kmWeight(foreignSemiLArith,{0}),"centralCharacter"=>{0}},
    new LeviWeight from {"parent"=>LaLArith,
        "semisimplePart"=>kmWeight(LaLArith#"semisimpleRootDatum",{0}),"centralCharacter"=>{}},
    new LeviWeight from {"parent"=>LaLArith,
        "semisimplePart"=>kmWeight(LaLArith#"semisimpleRootDatum",{0}),"centralCharacter"=>{"zero"}}
} do (
    malformedCharacterLArith := new LeviFormalCharacter from {
        "parent"=>LaLArith,"terms"=>new Tally from {malformedWeightLArith=>1},
        "status"=>"Exact","truncation"=>null};
    assert(mustErrorLeviArithmetic(() -> malformedCharacterLArith+RaLArith));
);
fractionalMultiplicityLArith = new LeviFormalCharacter from {
    "parent"=>LaLArith,"terms"=>new HashTable from {
        restrictWeight(LaLArith,kmWeight(DaLArith,{0,0}))=>1/2},
    "status"=>"Exact","truncation"=>null};
assert(mustErrorLeviArithmetic(() -> 2*fractionalMultiplicityLArith));

-- Equal derived Cartan matrices do not identify different parabolics,
-- global forms, node labels, or full Cartan coordinate systems.
otherLeviLArith = reductiveLeviDatum(
    standardParabolic(DaLArith,RemovedNodes=>{2}),DaLArith);
otherCharacterLArith = restrictCharacter(otherLeviLArith,CaLArith);
assert(mustErrorLeviArithmetic(() -> RaLArith+otherCharacterLArith));
assert(mustErrorLeviArithmetic(() -> RaLArith*otherCharacterLArith));
assert(mustErrorLeviArithmetic(() -> RaLArith+CaLArith));
assert(mustErrorLeviArithmetic(() -> CaLArith+RaLArith));
assert(mustErrorLeviArithmetic(() -> RaLArith*CaLArith));
assert(mustErrorLeviArithmetic(() -> CaLArith*RaLArith));
assert(mustErrorLeviArithmetic(() -> (1/2)*RaLArith));
assert(mustErrorLeviArithmetic(() -> RaLArith^(1/2)));
adjointDLArith = rootDatum(cartanA 3,GlobalForm=>"Adjoint");
adjointLLArith = reductiveLeviDatum(
    standardParabolic(adjointDLArith,RemovedNodes=>{1}),adjointDLArith);
adjointCLArith = restrictCharacter(adjointLLArith,
    finiteCharacter(adjointDLArith,{1,0}));
assert(mustErrorLeviArithmetic(() -> RaLArith+adjointCLArith));
assert(mustErrorLeviArithmetic(() -> RaLArith*adjointCLArith));
copyLLArith = reductiveLeviDatum(
    standardParabolic(rootDatum "A2",RemovedNodes=>{1}),rootDatum "A2");
sameLeviTermsArithmetic(RaLArith+restrictCharacter(copyLLArith,CaLArith),
    2*RaLArith);

-- A nonminimal realization carries a central character independent of the
-- Dynkin labels.  A rational, skew change of basis prevents zero-padding
-- tricks from accidentally passing this test.
rootRowsLArith = matrix(QQ,{{2,0}});
corootRowsLArith = matrix(QQ,{{1,0}});
changeLArith = matrix(QQ,{{1,1/3},{2,1}});
DsLArith = rootDatum realization(cartanDatum "A1",
    rootRowsLArith*changeLArith,
    corootRowsLArith*transpose inverse changeLArith);
LsLArith = reductiveLeviDatum(
    standardParabolic(DsLArith,LeviNodes=>{1}),DsLArith);
liftLArith = v -> weightFromRealizationCoordinates(DsLArith,
    flatten entries(matrix(QQ,{v})*changeLArith));
weightALArith = liftLArith {2,7/3};
weightBLArith = liftLArith {1,-5/2};
weightSumLArith = weightALArith+weightBLArith;
centralShiftLArith = liftLArith {0,1};
CsLArith = restrictCharacter(LsLArith,finiteCharacter(DsLArith,weightALArith));
EsLArith = restrictCharacter(LsLArith,finiteCharacter(DsLArith,weightBLArith));
PsLArith = CsLArith*EsLArith;
for k from 0 to 3 do (
    mu := weightSumLArith-toWeight kmRoot(DsLArith,{k});
    -- Multiplying (x^2+1+x^-2) by (x+x^-1) gives 1,2,2,1.
    assert(characterCoefficient(PsLArith,restrictWeight(LsLArith,mu)) ==
        ({1,2,2,1})#k);
    assert(characterCoefficient(PsLArith,
        restrictWeight(LsLArith,mu+centralShiftLArith)) == 0);
);
assert(all(characterSupport PsLArith,mu ->
    centralCharacter mu === centralCharacter restrictWeight(LsLArith,weightSumLArith)));
sameLeviTermsArithmetic(CsLArith^2,
    restrictCharacter(LsLArith,(finiteCharacter(DsLArith,weightALArith))^2));
assert(characterCoefficient(CsLArith^0,
    restrictWeight(LsLArith,liftLArith {0,0})) == 1);
skewVermaALArith = restrictCharacter(LsLArith,
    moduleCharacter(vermaModule(DsLArith,weightALArith),Depth=>3));
skewVermaBLArith = restrictCharacter(LsLArith,
    moduleCharacter(vermaModule(DsLArith,weightBLArith),Depth=>2));
for k from 0 to 2 do assert(characterCoefficient(
    skewVermaALArith*skewVermaBLArith,
    restrictWeight(LsLArith,weightSumLArith-toWeight kmRoot(DsLArith,{k}))) == k+1);
assert(mustErrorLeviArithmetic(() -> characterCoefficient(
    skewVermaALArith*skewVermaBLArith,
    restrictWeight(LsLArith,weightSumLArith-toWeight kmRoot(DsLArith,{3})))));
incompleteNativeLArith = formalCharacter(DsLArith,new Tally,
    CharacterStatus=>"Truncated",Truncation=>new HashTable from {
        "coefficientCertificate"=>true,"kind"=>"root-depth",
        "support"=>"highest-weight cone","highestWeight"=>kmWeight(DsLArith,{2}),
        "depth"=>0,"completeThroughDepth"=>0});
assert(mustErrorLeviArithmetic(() -> restrictCharacter(LsLArith,incompleteNativeLArith)));

-- The rank-zero boundary is an actual torus group algebra, not just the
-- scalar ring: addition and multiplication retain all central coordinates.
A0LArith = matrix mutableMatrix(ZZ,0,0);
E0LArith = matrix mutableMatrix(QQ,0,2);
D0LArith = rootDatum realization(cartanDatum A0LArith,E0LArith,E0LArith);
L0LArith = reductiveLeviDatum(
    standardParabolic(D0LArith,LeviNodes=>{}),D0LArith);
x0LArith = weightFromRealizationCoordinates(D0LArith,{2,-3});
y0LArith = weightFromRealizationCoordinates(D0LArith,{-1,1/2});
C0LArith = restrictCharacter(L0LArith,formalCharacter(D0LArith,
    new Tally from {x0LArith=>2,y0LArith=>-1}));
assert(characterCoefficient(C0LArith^2,
    restrictWeight(L0LArith,2*x0LArith)) == 4);
assert(characterCoefficient(C0LArith^2,
    restrictWeight(L0LArith,x0LArith+y0LArith)) == -4);
assert(characterCoefficient(C0LArith^2,
    restrictWeight(L0LArith,2*y0LArith)) == 1);
assert(characterDimension C0LArith == 1);
assert(characterDimension(C0LArith^2) == 1);
assert(characterCoefficient(C0LArith^0,restrictWeight(L0LArith,
    weightFromRealizationCoordinates(D0LArith,{0,0}))) == 1);
assert(characterCoefficient((0*C0LArith)^0,restrictWeight(L0LArith,
    weightFromRealizationCoordinates(D0LArith,{0,0}))) == 1);
sameLeviTermsArithmetic((0*CsLArith)^0,CsLArith^0);

-- A disconnected semisimple Levi contributes two independent A1 factors.
DdLArith = rootDatum((cartanA 2)++(cartanA 2));
LdLArith = reductiveLeviDatum(
    standardParabolic(DdLArith,LeviNodes=>{1,2}),DdLArith);
CdLArith = restrictCharacter(LdLArith,finiteCharacter(DdLArith,{1,1}));
DdSquareLArith = decomposeLeviCharacter(CdLArith^2);
assert((new Tally from apply(pairs DdSquareLArith,p ->
    datumCoordinates semisimplePart p#0 => p#1)) ===
    (new Tally from {{2,2}=>1,{2,0}=>1,{0,2}=>1,{0,0}=>1}));
assert(characterDimension(CdLArith^2) == 16);

-- User-specified partial characters know only their listed coefficients.
-- An absent coefficient must not become zero under restriction or addition.
uLArith = kmWeight(DaLArith,{2,0});
sLArith = kmWeight(DaLArith,{1,0});
vLArith = uLArith-sLArith;
partialAmbientLArith = formalCharacter(DaLArith,
    new Tally from {uLArith=>4,vLArith=>2},
    CharacterStatus=>"Truncated",Truncation=>new HashTable from {"note"=>"two known coefficients"});
partialLArith = restrictCharacter(LaLArith,partialAmbientLArith);
shiftAmbientLArith = formalCharacter(DaLArith,
    new Tally from {kmWeight(DaLArith,{0,0})=>1,sLArith=>1});
shiftLArith = restrictCharacter(LaLArith,shiftAmbientLArith);
productPartialLArith = partialLArith*shiftLArith;
assert(not isExactCharacter productPartialLArith);
assert(characterCoefficient(productPartialLArith,
    restrictWeight(LaLArith,uLArith)) == 6);
assert(mustErrorLeviArithmetic(() -> characterCoefficient(productPartialLArith,
    restrictWeight(LaLArith,vLArith))));
assert(mustErrorLeviArithmetic(() -> characterCoefficient(productPartialLArith,
    restrictWeight(LaLArith,uLArith+sLArith))));
assert(characterCoefficient(partialLArith+partialLArith,
    restrictWeight(LaLArith,uLArith)) == 8);
assert(characterCoefficient(-partialLArith,
    restrictWeight(LaLArith,vLArith)) == -2);
assert(mustErrorLeviArithmetic(() -> characterCoefficient(partialLArith-partialLArith,
    restrictWeight(LaLArith,uLArith+sLArith))));
assert(mustErrorLeviArithmetic(() -> partialLArith*partialLArith));
assert(mustErrorLeviArithmetic(() -> partialLArith^2));
assert(isExactCharacter(0*partialLArith));
assert(isExactCharacter(partialLArith^0));
assert(mustErrorLeviArithmetic(() -> characterDimension productPartialLArith));
assert(mustErrorLeviArithmetic(() -> decomposeLeviCharacter productPartialLArith));

-- A directly written partial Levi character need not carry a source
-- character.  Its provided coefficients remain usable, but no unprovided
-- coefficient can be inferred.  The inverse full-Cartan restriction must
-- transport these queries without inventing a highest-weight bound.
rawPartialLArith = new LeviFormalCharacter from {
    "parent"=>LaLArith,
    "terms"=>new Tally from {
        restrictWeight(LaLArith,uLArith)=>4,
        restrictWeight(LaLArith,vLArith)=>2},
    "status"=>"Truncated",
    "truncation"=>new HashTable from {"note"=>"two known Levi coefficients"}};
assert(characterCoefficient(rawPartialLArith*shiftLArith,
    restrictWeight(LaLArith,uLArith)) == 6);
assert(characterCoefficient(3*rawPartialLArith,
    restrictWeight(LaLArith,vLArith)) == 6);
assert(characterCoefficient(rawPartialLArith+partialLArith,
    restrictWeight(LaLArith,uLArith)) == 8);
assert(mustErrorLeviArithmetic(() -> characterCoefficient(rawPartialLArith*shiftLArith,
    restrictWeight(LaLArith,vLArith))));
assert(mustErrorLeviArithmetic(() -> rawPartialLArith*rawPartialLArith));
assert(not match("through root depth",toString(rawPartialLArith+partialLArith)));

-- A purported restriction certificate is not trustworthy if its source
-- belongs to a different ambient datum or contradicts the listed terms.
falseParentSourceLArith = new LeviFormalCharacter from {
    "parent"=>LaLArith,"terms"=>characterTerms partialLArith,
    "status"=>"Truncated","truncation"=>new HashTable from {
        "coefficientCertificate"=>true,"operation"=>"restriction",
        "source"=>formalCharacter(adjointDLArith,
            new Tally from {kmWeight(adjointDLArith,{2,0})=>4},
            CharacterStatus=>"Truncated",Truncation=>new HashTable from {"note"=>"foreign"})}};
assert(mustErrorLeviArithmetic(() -> falseParentSourceLArith+partialLArith));
falseCoefficientSourceLArith = new LeviFormalCharacter from {
    "parent"=>LaLArith,"terms"=>new Tally from {
        restrictWeight(LaLArith,uLArith)=>7},
    "status"=>"Truncated","truncation"=>new HashTable from {
        "coefficientCertificate"=>true,"operation"=>"restriction",
        "source"=>partialAmbientLArith}};
assert(mustErrorLeviArithmetic(() -> falseCoefficientSourceLArith+partialLArith));

-- The one-root Verma denominator is 1/(1-e^-alpha).  Its square and cube
-- have coefficients k+1 and binomial(k+2,2), respectively, within depth 3.
DvLArith = rootDatum "A1";
LvLArith = reductiveLeviDatum(
    standardParabolic(DvLArith,LeviNodes=>{}),DvLArith);
highestLArith = kmWeight(DvLArith,{2});
verma3AmbientLArith = moduleCharacter(vermaModule(DvLArith,highestLArith),Depth=>3);
verma3LArith = restrictCharacter(LvLArith,verma3AmbientLArith);
verma5LArith = restrictCharacter(LvLArith,
    moduleCharacter(vermaModule(DvLArith,highestLArith),Depth=>5));
for k from 0 to 3 do (
    root := toWeight kmRoot(DvLArith,{k});
    assert(characterCoefficient(verma3LArith^2,
        restrictWeight(LvLArith,2*highestLArith-root)) == k+1);
    assert(characterCoefficient(verma3LArith^3,
        restrictWeight(LvLArith,3*highestLArith-root)) == binomial(k+2,2));
    assert(characterCoefficient(verma3LArith+verma5LArith,
        restrictWeight(LvLArith,highestLArith-root)) == 2);
);
assert(mustErrorLeviArithmetic(() -> characterCoefficient(verma3LArith^2,
    restrictWeight(LvLArith,2*highestLArith-toWeight kmRoot(DvLArith,{4})))));
assert(characterCoefficient(verma3LArith^2,
    restrictWeight(LvLArith,2*highestLArith+toWeight kmRoot(DvLArith,{1}))) == 0);
assert(mustErrorLeviArithmetic(() -> characterCoefficient(verma3LArith+verma5LArith,
    restrictWeight(LvLArith,highestLArith-toWeight kmRoot(DvLArith,{4})))));
assert(match("through root depth 3",toString(verma3LArith^3)));

-- Two transported arbitrary partial expansions cannot simply be convolved:
-- reflecting the completed support may make a coefficient an infinite sum.
dualPartialLArith = restrictCharacter(LvLArith,dualCharacter verma3AmbientLArith);
assert(mustErrorLeviArithmetic(() -> dualPartialLArith*dualPartialLArith));
assert(mustErrorLeviArithmetic(() -> dualPartialLArith*verma3LArith));

-- Affine restriction retains both the null-root and central directions.
-- In A1^(1), the Verma denominator through height 2 is
-- 1+x+y+x^2+2xy+y^2.  Its square has xy coefficient 6, its cube 12.
DaffLArith = rootDatum cartanAffineA 1;
LaffLArith = reductiveLeviDatum(
    standardParabolic(DaffLArith,RemovedNodes=>{1}),DaffLArith);
lambdaAffLArith = weightFromRealizationCoordinates(DaffLArith,{1,0,0});
VaffLArith = restrictCharacter(LaffLArith,
    moduleCharacter(vermaModule(DaffLArith,lambdaAffLArith),Depth=>2));
deltaAffLArith = toWeight kmRoot(DaffLArith,{1,1});
assert(characterCoefficient(VaffLArith^2,
    restrictWeight(LaffLArith,2*lambdaAffLArith-deltaAffLArith)) == 6);
assert(characterCoefficient(VaffLArith^3,
    restrictWeight(LaffLArith,3*lambdaAffLArith-deltaAffLArith)) == 12);
assert(characterCoefficient(VaffLArith^2,restrictWeight(LaffLArith,
    2*lambdaAffLArith-toWeight kmRoot(DaffLArith,{2,0}))) == 3);
assert(mustErrorLeviArithmetic(() -> characterCoefficient(VaffLArith^2,
    restrictWeight(LaffLArith,2*lambdaAffLArith-toWeight kmRoot(DaffLArith,{2,1})))));
assert(restrictWeight(LaffLArith,lambdaAffLArith) =!=
    restrictWeight(LaffLArith,lambdaAffLArith-deltaAffLArith));
assert(mustErrorLeviArithmetic(() -> VaffLArith^(-1)));

-- Arithmetic is not a finite-type decomposition algorithm.  It also works
-- for a nonsingular indefinite full Levi, through a certified finite depth.
-- No Serre relation appears in rank two at height 2 for this matrix, so the
-- same two-letter PBW coefficients give 2 before squaring and 6 afterward.
DhypLArith = rootDatum matrix{{2,-3},{-3,2}};
LhypLArith = reductiveLeviDatum(
    standardParabolic(DhypLArith,LeviNodes=>{1,2}),DhypLArith);
lambdaHypLArith = kmWeight(DhypLArith,{1,0});
VhypLArith = restrictCharacter(LhypLArith,
    moduleCharacter(vermaModule(DhypLArith,lambdaHypLArith),Depth=>2));
mixedRootLArith = toWeight kmRoot(DhypLArith,{1,1});
assert(characterCoefficient(VhypLArith,
    restrictWeight(LhypLArith,lambdaHypLArith-mixedRootLArith)) == 2);
assert(characterCoefficient(VhypLArith^2,
    restrictWeight(LhypLArith,2*lambdaHypLArith-mixedRootLArith)) == 6);
assert(mustErrorLeviArithmetic(() -> characterDimension(VhypLArith^2)));
///


------------------------------------------------------------------------
-- Kazhdan-Lusztig polynomials in a supplied variable
------------------------------------------------------------------------

TEST ///
-- A supplied variable changes only the ordinary polynomial ring of the
-- answer.  The coefficient recurrence and its Bruhat-interval bound remain
-- independent of that choice.
mustErrorKLVariable := f -> (try (f();false) else true);
Avariable := cartanA 4;
Wvariable := weylGroup Avariable;
evariable := identityWeylElement Wvariable;
vvariable := weylElement(Wvariable,{2,1,3,2});
Rvariable := ZZ[local t];
tvariable := Rvariable_0;
Svariable := QQ[local x,local y,local z,
    Degrees=>{2,3,7},MonomialOrder=>Lex];
xvariable := Svariable_0;
yvariable := Svariable_1;
zvariable := Svariable_2;
qSentinelVariable := 971234;
q := qSentinelVariable;
defaultVariable := kazhdanLusztigVariable();
assert(ring defaultVariable === KazhdanLusztigPolynomialRing);
assert(kazhdanLusztigPolynomial(evariable,vvariable) == 1+defaultVariable);
assert(kazhdanLusztigPolynomial(evariable,vvariable,Variable=>null) == 1+defaultVariable);
assert(q === qSentinelVariable);

-- The classical singular Schubert variety indexed by 3412 gives 1+t.
-- Test both public signatures and every generator of a multigraded ring,
-- not only the first variable or a variable with the printed name q.
Pvariable := kazhdanLusztigPolynomial(evariable,vvariable,Variable=>tvariable);
assert(instance(Pvariable,RingElement) and ring Pvariable === Rvariable);
assert(Pvariable == 1+tvariable);
assert(kazhdanLusztigPolynomial(Avariable,{}, {2,1,3,2},
    Variable=>tvariable) == Pvariable);
for generatorVariable in gens Svariable do (
    P := kazhdanLusztigPolynomial(evariable,vvariable,Variable=>generatorVariable);
    assert(ring P === Svariable and P == 1+generatorVariable);
    assert(kazhdanLusztigPolynomial(Avariable,{2},{2,1,3,2},
        Variable=>generatorVariable) == 1+generatorVariable);
);

-- Zero and one are polynomial elements in the requested ring, even when
-- the endpoints are incomparable or equal.  This matters for native
-- addition, ring maps, matrix entries, and ideal construction.
for generatorVariable in {tvariable,yvariable} do (
    R := ring generatorVariable;
    P0 := kazhdanLusztigPolynomial(vvariable,evariable,Variable=>generatorVariable);
    P1 := kazhdanLusztigPolynomial(evariable,evariable,Variable=>generatorVariable);
    assert(instance(P0,RingElement) and ring P0 === R and P0 == 0_R);
    assert(instance(P1,RingElement) and ring P1 === R and P1 == 1_R);
    assert(ring matrix{{P0,P1}} === R);
    assert(ring ideal(P1) === R);
);

-- A product of two singular A3 intervals gives an independent degree-two
-- polynomial (1+t)^2.  Reusing its coefficients in several coefficient
-- rings must never return a polynomial in the previously requested ring.
AproductVariable := (cartanA 4)++(cartanA 4);
WproductVariable := weylGroup AproductVariable;
eproductVariable := identityWeylElement WproductVariable;
vproductVariable := weylElement(WproductVariable,{2,1,3,2,5,4,6,5});
clearInfrastructureCaches();
assert(mustErrorKLVariable(() -> kazhdanLusztigPolynomial(
    eproductVariable,vproductVariable,Variable=>tvariable,MaxElements=>195)));
for generatorVariable in {tvariable,xvariable,zvariable,defaultVariable,tvariable} do (
    P := kazhdanLusztigPolynomial(eproductVariable,vproductVariable,
        Variable=>generatorVariable,MaxElements=>196);
    assert(ring P === ring generatorVariable and P == (1+generatorVariable)^2);
    assert(kazhdanLusztigCoefficients(eproductVariable,vproductVariable,
        MaxElements=>196) === {1,2,1});
    assert(mustErrorKLVariable(() -> kazhdanLusztigPolynomial(
        eproductVariable,vproductVariable,Variable=>generatorVariable,MaxElements=>195)));
);
PproductVariable := kazhdanLusztigPolynomial(eproductVariable,vproductVariable,
    Variable=>yvariable,MaxElements=>null);
assert(degree PproductVariable === {6});
assert(coefficient(yvariable,PproductVariable) == 2);
assert(coefficient(yvariable^2,PproductVariable) == 1);
assert(sub(PproductVariable,{yvariable=>3}) == 16);
assert(diff(yvariable,PproductVariable) == 2+2*yvariable);
assert(value factor PproductVariable == PproductVariable);
assert(ring ideal(PproductVariable) === Svariable);
mapVariable := map(Svariable,Rvariable,{yvariable});
assert(mapVariable(Pvariable) == 1+yvariable);

-- Every pair in A3 checks ring placement and coefficient conversion,
-- including the zero pairs.  The polynomial coefficients themselves are
-- independently tested by the existing KL recurrence fixtures.
elementsVariable := apply(weylGroupElements Avariable,
    data -> weylElement(Wvariable,data#"word"));
for lowerVariable in elementsVariable do for upperVariable in elementsVariable do (
    coefficientsVariable := kazhdanLusztigCoefficients(lowerVariable,upperVariable);
    P := kazhdanLusztigPolynomial(lowerVariable,upperVariable,Variable=>zvariable);
    assert(ring P === Svariable);
    expected := sum apply(toList(0..#coefficientsVariable-1),
        i -> coefficientsVariable#i*zvariable^i);
    assert(P == expected);
);

-- The option is equally valid in an infinite ambient Weyl group and in
-- rank zero; only the finite interval used by the calculation matters.
for A in {cartanAffineA 1,cartanRank2(3,3)} do (
    P := kazhdanLusztigPolynomial(A,{}, {1,2,1,2,1},Variable=>tvariable);
    assert(ring P === Rvariable and P == 1_Rvariable);
    P0 := kazhdanLusztigPolynomial(A,{2},{1},Variable=>yvariable);
    assert(ring P0 === Svariable and P0 == 0_Svariable);
);
AemptyVariable := map(ZZ^0,ZZ^0,0);
PemptyVariable := kazhdanLusztigPolynomial(AemptyVariable,{}, {},Variable=>tvariable);
assert(ring PemptyVariable === Rvariable and PemptyVariable == 1_Rvariable);

-- The mathematical polynomial is integral.  Finite characteristic and
-- approximate coefficients would change or approximate it; quotient and
-- noncommutative variables are not ordinary polynomial generators.
Qvariable := Rvariable/ideal(tvariable^2);
Fvariable := (ZZ/5)[local a];
Nvariable := RR[local b];
Kvariable := QQ[local c,local d,SkewCommutative=>{0,1}];
Tvariable := Rvariable[local w];
for badVariable in {0,1,-1,1/2,true,false,"t",symbol t,
    0_Rvariable,1_Rvariable,tvariable^2,tvariable+1,2*tvariable,
    xvariable+yvariable,Qvariable_0,Fvariable_0,Nvariable_0,
    Kvariable_0,Tvariable_0} do (
    assert(mustErrorKLVariable(() -> kazhdanLusztigPolynomial(
        evariable,vvariable,Variable=>badVariable)));
    assert(mustErrorKLVariable(() -> kazhdanLusztigPolynomial(
        Avariable,{}, {},Variable=>badVariable)));
);
for badLimitVariable in {0,-1,1/2,"196",false} do
    assert(mustErrorKLVariable(() -> kazhdanLusztigPolynomial(
        eproductVariable,vproductVariable,Variable=>tvariable,
        MaxElements=>badLimitVariable)));
WotherVariable := weylGroup(cartanB 3);
assert(mustErrorKLVariable(() -> kazhdanLusztigPolynomial(
    evariable,identityWeylElement WotherVariable,Variable=>tvariable)));
assert(q === qSentinelVariable);
///
