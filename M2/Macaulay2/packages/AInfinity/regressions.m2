
-- Typed structures, signs, bounds and sparse components
TEST ///
-- A binary homotopy is required: d m2 + m2 d must be multiplication by x^2.
Q = QQ[x]
R = Q/x^2
B = sAInfAlgModel R
G = sAInfModModel(B,coker vars R)
H = sAInfMod(B,G#"natural",hashTable select(pairs G#"mm",(k,v) -> #k == 1))
assert(checkStasheff G and checkStasheff aInfMod G)
assert(checkStasheff(H,1) and not checkStasheff(H,2))
assert(not checkStasheff(aInfMod H,2))
assert(not checkStasheff(H[3]) and not checkStasheff((aInfMod H)[-2]))
assert(sAInfMod(aInfMod G) == G and G[3][-3] == G)
assert(try (checkStasheff(G,0); false) else true)

-- The map to the ground ring is part of the unary identity.
V = complex({Q^1,Q^1,Q^1},Base=>1)
badD = aInfAlg(Q,V,hashTable {{2} => id_(Q^1)},id_(Q^1))
assert(not checkStasheff(badD,1) and not checkStasheff(sAInfAlg badD,1))
assert((complex badD).dd^2 != 0)

-- This binary product is nonassociative in arity 3. The map to degree 3
-- is given only on (degree 2, degree 1), making one bracketing zero.
m = hashTable {{1,1} => id_(Q^1), {2,1} => id_(Q^1)}
A = aInfAlg(Q,V,m,map(Q^1,Q^1,0))
assert(checkStasheff(A,2) and checkStasheff(A,3) === false)
assert(not checkStasheff(sAInfAlg A,3))
assert(A == aInfAlg sAInfAlg A)
assert((complex A)_2 == Q^1 and (complex A).dd_2 == 0)
assert(try (aInfAlg(Q,V,hashTable {{1,1} => id_(Q^2)},map(Q^1,Q^1,0)); false) else true)

-- A sparse complete structure declares omitted operations zero. An explicit
-- arity cutoff leaves higher operations unknown and survives conversions.
zeroA = aInfAlg(Q,V,hashTable {},map(Q^1,Q^1,0))
assert(zeroA == aInfAlg(Q,V, hashTable {{1,1}=>map(Q^1,Q^1,0)},map(Q^1,Q^1,0)))
partialA = aInfAlg(A,TensorOrder=>1)
assert(checkStasheff partialA and checkStasheff(partialA,1))
assert(try (checkStasheff(partialA,2); false) else true)
assert(partialA != zeroA and partialA == aInfAlg sAInfAlg partialA)
assert(try (checkStasheff(sAInfAlg partialA,2); false) else true)
shortA = aInfAlg(A,LengthLimit=>1)
assert(max shortA#"natural" == 1 and all(keys shortA#"mm",k-> #k == 1))
assert(checkStasheff shortA and shortA == aInfAlg sAInfAlg shortA)
assert(try (aInfAlg(A,TensorOrder=>0); false) else true)

-- A top degree of 3 admits a shifted binary operation in degree (2,2).
W = complex({Q^1,Q^1},Base=>2)
B = sAInfAlg(Q,W,hashTable {{2,2}=>id_(Q^1)},map(Q^1,Q^1,0),TensorOrder=>2)
assert(B#"mm"#?{2,2} and B#"mm"#{2,2} != 0)
assert(checkStasheff B and B == sAInfAlg aInfAlg B)

-- Sparse module differentials preserve the endpoints, including for shifts.
T = sAInfAlg Q
W = complex({Q^2,Q^1,Q^3},Base=> -2)
G = sAInfMod(T,W,hashTable {})
assert(min complex G == -2 and max complex G == 0)
assert((complex G)_(-2) == Q^2 and (complex G)_0 == Q^3)
assert(complex(G[4]) == (complex G)[4] and G[-1][1] == G)
assert(sAInfMod(T,W,hashTable {{0}=>map(W_(-1),W_0,0)}) == G)

-- Basis indices determine coordinates independently of hash table order.
Q = QQ[z_0..z_8]
generatorData = hashTable apply(reverse toList(0..8),i -> {{1,i}} => {Q_i})
A = aInfAlg(Q,generatorData)
assert(A#"d" == vars Q and ring complex A === Q)
assert((aInfAlg(Q,hashTable {{{1,0}}=>{1}}))#"d" == id_(Q^1))
assert(try (aInfAlg(Q,hashTable {{{1,2}}=>{z_0}}); false) else true)
assert(try (aInfAlg(Q,hashTable {{{1,0}}=>{z_0,z_1}}); false) else true)
assert(try (aInfAlg(Q,hashTable {{{1,0}}=>{z_0},{{2,0}}=>{z_1},{{1,1},{1,0}}=>{1}}); false) else true)
gapData = hashTable {{{3,0}}=>{}}
gapAlg = aInfAlg(Q,gapData)
assert((complex gapAlg)_3 == Q^1 and (complex gapAlg)_1 == 0 and checkStasheff gapAlg)

-- A change of convention preserves the quotient, local point, computed
-- operation bounds, and last known term of an incomplete resolution.
P = QQ[u,v,w]
RP = P/(ideal vars P)^2
modelB = sAInfAlgModel RP
-- Install representative provenance on this copy to also exercise paths for
-- local and truncated resolutions in this small homogeneous example.
modelB.cache#"AInfinityPresentedRing" = RP
modelB.cache#"AInfinityModelPoint" = ideal vars P
modelB.cache#"AInfinityResolutionThrough" = max modelB#"natural"
modelB.cache#"AInfinityCheckLimit" = max modelB#"natural" - 1
C = aInfAlg modelB
for key in {"AInfinityPresentedRing","AInfinityModelPoint"} do (
    assert(C.cache#key === modelB.cache#key);
    assert((sAInfAlg(modelB)).cache#key === modelB.cache#key);
    assert((sAInfAlg C).cache#key === modelB.cache#key))
for key in {"AInfinityResolutionThrough","AInfinityLimit","AInfinityCheckLimit"} do (
    assert(C.cache#key == modelB.cache#key-1);
    assert((sAInfAlg C).cache#key == modelB.cache#key))
regular = sAInfMod modelB
suspended = regular[3]
assert(suspended.cache#"AInfinityResolutionThrough" == regular.cache#"AInfinityResolutionThrough"-3)
assert((aInfMod suspended).cache#"AInfinityCheckLimit" == suspended.cache#"AInfinityCheckLimit")
assert((sAInfMod aInfMod suspended).cache#"AInfinityModelPoint" === modelB.cache#"AInfinityModelPoint")

-- Products on the two Koszul generators must land in their named columns.
Q = QQ[u,v]
koszulData = hashTable {{{1,0}}=>{u},{{1,1}}=>{v},{{2,0}}=>{-v,u},
                      {{1,0},{1,1}}=>{1},{{1,1},{1,0}}=>{-1}}
koszulAlg = aInfAlg(Q,koszulData)
assert(entries(koszulAlg#"mm"#{1,1}) == {{0_Q,1_Q,-1_Q,0_Q}})
assert(isHomogeneous(koszulAlg#"mm"#{1,1}) and checkStasheff koszulAlg)
assert(checkStasheff sAInfAlg koszulAlg)

for V in {modelB,aInfAlg modelB,regular,aInfMod regular} do assert checkStasheff(V,1000)
///

-- Labeled sums, tensor differentials and basis order
TEST ///
debug AInfinity
S = QQ[x,y,z];
K = complex koszul vars S;
U = labeledTensor K;
assert(U.dd_1 == K.dd_1 and U.dd_3 == K.dd_3 and U.dd^2 == 0)
V = labeledTensor{K,K};
assert(min V == 0 and max V == 6 and V.dd^2 == 0)
assert(V.dd_2^[{1,0}]_[{1,1}] == -(id_(K_1)**K.dd_1))
assert(V.dd_2^[{0,1}]_[{1,1}] == K.dd_1**id_(K_1))
T = labeledTensor{K,K,K};
assert(T.dd^2 == 0)
assert(T.dd_4^[{0,0,3}]_[{1,1,2}] == 0)
W = labeledTensor{K[2],K[-1]};
assert(min W == -1 and max W == 5 and W.dd^2 == 0)
assert(W.dd_1^[{-1,1}]_[{-1,2}] == -(id_((K[2])_(-1))**(K[-1]).dd_2))
R = S/(x^2,y^2);
WR = labeledTensor(R,{K[2],K[-1]},-1,5);
assert(WR.dd^2 == 0 and ring WR === R and WR.dd_1 == tensor(R,W.dd_1))
assert((min labeledTensor({K},-3,-2),max labeledTensor({K},-3,-2)) == (-3,-2))
assert((labeledTensor({K},-3,-2))_(-3) == S^0)
assert(try (labeledTensor {}; false) else true)
assert(try (labeledTensor({K},4,3); false) else true)
E = complex(label("term",S^{2:-1}),Base => -2);
ER = labeledTensor(R,E);
assert(min ER == -2 and max ER == -2 and labels ER_(-2) == {"term"} and ER_(-2) == R^{2:-1})
A = label("left",S^1); B = label("right",S^{-2});
C = labeledDirectSum(A,B);
assert(labels C == {"left","right"})
assert(labels relabel(C,k -> k|"!") == {"left!","right!"})
assert(labels unlabel C == {0,1} and labels C == {"left","right"})
assert(try (summands directSum("x"=>S^1,"x"=>S^1);false) else true)
assert(try (labeledDirectSum{A,A};false) else true)
H = labeledDirectSum{complex(A,Base=>-2), complex(B,Base=>1)};
assert((min H,max H)==(-2,1) and H_(-2)==A and H_1 == B and H.dd^2==0 and labels H_1 == {"right"})
///

-- Burke terms and differentials, block extraction and local exactness
TEST ///
debug AInfinity
S=QQ[x,y]; R=S/(x^2,y^2);
M=coker vars R;
B=sAInfAlgModel R;
G=sAInfModModel(B,M);
assert(try(burkeResolution(M,-1);false) else true)
F0=burkeResolution(M,0,Check=>true);
assert(min F0 == 0 and max F0 == 0 and ring F0 === R and F0_0 == R^1 and indices F0_0 == {{0}})
assert(burkeModule(S,B,G,-1) == S^0)
assert(burkeModule(R,B,G,-1) == R^0)
assert(source burkeDifferential(R,B,G,0) == R^1 and target burkeDifferential(R,B,G,0) == R^0)
-- Use the same chosen typed lifts as the four-argument differential below.
F=burkeResolutionAtPoint(M,5,Check=>true);
assert(ann(coker F.dd_1) == ideal vars R)
for t from 1 to 5 do (
    dS:=burkeDifferential(S,B,G,t); dR:=burkeDifferential(R,B,G,t);
    assert(ring dR === R and ring dS === S and dR == tensor(R,dS) and dR == F.dd_t);
    assert(rankBurkeModule(B#"natural",G#"natural",t) == rank F_t));
C=complex(S^1,Base=>-1); Z=complex(S^0,Base=>0);
assert(burkeModule(S,Z,C,-1)==S^1 and burkeModule(S,Z,C,300) == S^0)
assert(burkeModule(S,complex(S^1,Base=>2),C,1) == S^1)
assert(try(burkeModule(S,complex S^1,C,3);false) else true)
A=sAInfAlg S;
H=sAInfMod(A,C,hashTable {});
d0=burkeDifferential(R,A,H,-1);
assert(source d0 == R^1 and target d0 == R^0)
T=directSum({1}=>S^1,{9}=>S^2);
U=directSum({3}=>S^2,{5}=>S^1);
f=map(T,U,matrix{{S_0,2,0},{0,0,1},{0,3,0}});
assert(extractBlocks(f,{9},{3})==f^[{9}]_[{3}])
assert(extractBlocks(f,{{9},{1}},{{5},{3}}) == f^[{9},{1}]_[{5},{3}])
assert(extractBlocks(f,{}) == map(T,S^0,0))
assert(extractBlocks(f,{},{3}) == map(S^0,S^2,0))
assert(try(extractBlocks(f,{100});false) else true)
assert(try(extractBlocks(f,{100},{});false) else true)
-- Two blocks of local rank 1 in an ambient matrix of rank 3.
p = toString picture f;
assert(member("|{9}| 1 | 1 |",lines p) and member("|{1}| 1 | . |",lines p))
assert(class displayBlocks f === Net)
assert(class picture matrix{{S_0}} === Net and class displayBlocks matrix{{1_QQ}} === Net)
assert(class picture F0 === Net)
assert(class picture map(S^0,S^0,0) === Net)
B.cache#"AInfinityResolutionThrough" = 1;
assert(try(burkeModule(S,B,G,3); false) else true)
remove(B.cache,"AInfinityResolutionThrough");
G.cache#"AInfinityResolutionThrough" = 1;
assert(try(burkeDifferential(S,B,G,3);false) else true)
remove(G.cache,"AInfinityResolutionThrough");
-- H_1 = S/(x-1), supported away from the origin.
E = complex {map(S^1,S^1,0),map(S^1,S^1,S_0-1)};
assert(E.dd^2 == 0 and HH_1 E != 0)
assert(checkBurkeExactness(E,ideal vars S))
assert(try (checkBurkeExactness(E,null);false) else true)
assert(try (checkBurkeExactness(E,ideal(S_0-1,S_1));false) else true)
///

-- Sparse actions remain unknown outside a recorded computation
TEST ///
-- Copies, shifts and changes of convention preserve unknown operations.
Q = QQ[s,t,u]
R = Q/(ideal vars Q)^2
B = sAInfAlgModel R
G = sAInfMod(sAInfModModel(B,coker vars R),TensorOrder=>1)
for H in {sAInfMod G, G[3], aInfMod G, sAInfMod aInfMod G} do (
    assert(all(keys H#"mm",k-> #k == 1));
    assert(checkStasheff H);
    assert(try (checkStasheff(H,2); false) else true))
///

-- The available Burke differential follows module suspension
TEST ///
debug AInfinity
S=QQ[x]; R=S/(x^2); M=coker vars R;
B=sAInfAlgModel R;
G=sAInfModModel(B,M,TensorOrder=>1);
assert(G.cache#"AInfinityCheckOrder" == 1)
assert(ring burkeDifferential(R,B,G,1) === R)
assert(try(burkeDifferential(R,B,G,2);false) else true)
Gt=G[3];
assert(Gt.cache#"AInfinityCheckOrder" == 1)
assert(try(burkeDifferential(R,B,Gt,-1);false) else true)
assert(ring burkeDifferential(R,B,Gt,-2) === R)
Z=sAInfMod(aInfMod Gt);
assert(Z.cache#"AInfinityCheckOrder" == 1)
assert(try(burkeDifferential(R,B,Z,-1);false) else true)
///

-- Display at the selected point and at the origin
TEST ///
debug AInfinity
S=QQ[x,y];
C=complex {matrix{{x}}};
origin=ideal vars S;
other=ideal(x-1,y);
C.cache#"AInfinityModelPoint" = origin;
atOrigin=toString picture C;
C.cache#"AInfinityModelPoint" = other;
atOther=toString picture C;

assert(match("[*]",atOrigin) and not match("[*]",atOther) and match("1",atOther))
assert(match("[*]",toString picture(C.dd_1)))
R=S/ideal(x-1);
m=matrix{{2_R}};
assert(match("[*]",toString picture m))
assert(match("[*]",toString picture matrix{{2_ZZ}}))
///

-- The local Burke model of a module supported away from the selected point
TEST ///
S=QQ[x,y,z]; setMaxIdeal ideal vars S;
R=S/ideal(0_S);
M=R^1/ideal(R_0-1,R_1,R_2);
F=burkeResolution(M,4,ideal vars S,Check=>true);
assert(F.dd^2 == 0);
assert(all(5,i->F_i==0));
///
-- The operation degree is the shifted target degree. The latest request
-- cannot reuse a short or externally seeded ambient resolution by mistake.
TEST ///
S = QQ[x,y,z]
R = S/ideal(x^2,y^2,z^2)
M = coker vars R
R.cache#"resolution" = complex {S^1}
M.cache#"resolution" = complex {S^1}
B0 = sAInfAlgModel(R,LengthLimit=>0)
G0 = sAInfModModel(B0,M,LengthLimit=>0)
assert(B0.cache#?"AInfinityResolutionThrough" and G0.cache#?"AInfinityResolutionThrough")
assert not(B0#"mm"#?{2,2})
B = sAInfAlgModel R
G = sAInfModModel(B,M)
assert(max B#"natural" == 4 and max G#"natural" == 3)
assert(B#"mm"#?{2,2} and G#"mm"#?{2,0})
assert((sAInfAlgModel(R,LengthLimit=>0))#"mm" === B0#"mm")
G1 = sAInfModModel(M,TensorOrder=>1)
assert all(keys G1#"mm", w->#w==1)
assert(G1.cache#"AInfinityCheckOrder" == 1)
assert(try (sAInfModModel(B0,M); false) else true)
assert(R.cache#"resolution" == complex {S^1} and M.cache#"resolution" == complex {S^1})
///

TEST ///
S = QQ[x,y]
R = S/ideal(0_S)
B = sAInfAlgModel R
assert(B#"natural"_2 == 0 and B#"d" == 0)
B = sAInfAlgModel S
G = sAInfModModel(B,coker vars S)
assert all(keys G#"mm",w->#w==1)
assert(G#"mm"#{1} * G#"mm"#{2} == 0)
assert(max (sAInfModModel S^1)#"natural" == 0)
assert((sAInfModModel S^0)#"natural"_0 == 0)
for opts in {LengthLimit=>-1,TensorOrder=>0,Check=>4} do (
    assert(try(sAInfAlgModel(R,opts);false) else true);
    assert(try(sAInfModModel(coker vars S,opts);false) else true))
assert(try (sAInfAlgModel(complex {S^2}); false) else true)
bad = complex {map(S^1,S^{-1},{{S_0}}), map(S^{-1},S^{-2},{{S_1}})}
assert(try (sAInfAlgModel bad; false) else true)
assert(try (sAInfModModel(B,coker vars S,bad); false) else true)
///

TEST ///
-- Relative finite and infinite projective dimensions over a singular ring.
S=QQ[u,v]
Q=S/ideal(u^2)
T=Q/ideal(v)
B=sAInfAlgModel(Q,T)
assert(ideal B#"d" == ideal Q_1 and max B#"natural" == 2)
G=sAInfModModel(B,T^1)
assert(G#"mm"#{1} * G#"mm"#{2,0} == B#"d" ** id_((G#"natural")_0))
assert((sAInfAlgModel(Q,Q))#"d" == 0)
assert((sAInfModModel(sAInfAlgModel(Q,Q),Q^1))#"natural"_0 == Q^1)
R=Q/ideal Q_0
assert(try (sAInfAlgModel(Q,R); false) else true)
B=sAInfAlgModel(Q,R,LengthLimit=>3)
G=sAInfModModel(B,coker vars R,LengthLimit=>3)
assert(B.cache#?"AInfinityResolutionThrough" and G.cache#?"AInfinityResolutionThrough")
assert(ring B#"d" === Q and max G#"natural" >= 3)
assert(try (sAInfModModel(B,T^1,LengthLimit=>3); false) else true)
///

TEST ///
-- The local point controls the inhomogeneous resolution, including cached calls.
S=QQ[x,y]
R=S/ideal(x^2-x)
M=coker vars R
assert(try (sAInfAlgModel R; false) else true)
setMaxIdeal ideal vars S
B=sAInfAlgModel R
G=sAInfModModel(B,M)
assert(checkStasheff(aInfMod G))
assert((sAInfModModel(B,M,LengthLimit=>1))#"mm"#{2,0} == G#"mm"#{2,0})
setMaxIdeal ideal(S_0-1,S_1)
assert(try(sAInfModModel(B,M);false) else true)
C=sAInfAlgModel R
assert(C.cache#"AInfinityModelPoint" == ideal(S_0-1,S_1))
setMaxIdeal ideal vars S
assert((sAInfAlgModel R)#"mm" === B#"mm")
///

TEST ///
-- A nonzero ternary operation certifies the arity-three path really ran.
S = ZZ/101[x,y,z,u]
I = ideal(u^3,x*y^2,(x+y)*z^2,x^2*u+z*u^2,y^2*u+x*z*u,y^2*z+y*z^2)
R = S/I
B2 = sAInfAlgModel(R,TensorOrder=>2)
B = sAInfAlgModel R
assert(not B2#"mm"#?{2,2,2} and B#"mm"#{2,2,2} != 0)
assert checkStasheff B
G2 = sAInfModModel(coker vars R,TensorOrder=>2)
assert all(keys G2#"mm",w->#w<=2)
assert(checkStasheff(aInfMod G2,2))
///

TEST ///
-- Checked explicit complexes can be shifted, with noncanonical bases.
S=QQ[x,y]
R=S/ideal(x^2)
B=sAInfAlgModel R
M=coker vars R
C=complex koszul vars S
G=sAInfModModel(B,M,C[-3])
assert(min G#"natural" == 3 and max G#"natural" == 5)
assert(try (sAInfModModel(B,M,complex {S^1});false) else true)
-- Same annihilator but different number of generators.
assert(try (sAInfModModel(B,M++M,C);false) else true)
-- An actual cycle in an interior degree; all consecutive composites vanish.
d1=matrix{{S_0^2,0_S}}
D=complex {d1,map(source d1,S^1,{{0_S},{S_1}})}
assert(D.dd^2==0 and HH_1 D != 0)
assert(try (sAInfAlgModel D;false) else true)
assert(try (sAInfModModel(B,R^1,D);false) else true)
-- A finite prefix with an unresolved top syzygy is explicitly marked.
E=naiveTruncation(C,,1)
assert(try(sAInfModModel(B,M,E);false) else true)
G=sAInfModModel(B,M,E,LengthLimit=>1)
assert(G.cache#"AInfinityResolutionThrough"==1 and G.cache#"AInfinityCheckLimit"==0)
-- An ambient module on which the quotient acts is legitimate.
G=sAInfModModel(B,coker vars S,C)
assert(G#"mm"#{2,0} != 0)
///

TEST ///
S=QQ[x,y]
R=S/ideal(S_0*(S_0-1))
setMaxIdeal ideal vars S
M=R^1/(R_0-1)
assert((sAInfModModel M)#"natural"_0 == 0)
T=S/ideal(S_0*S_1)
N=T^1/(T_1^2+T_1)
G=sAInfModModel N
assert(checkStasheff(aInfMod G))
U=S/ideal(S_0*S_1-1)
assert(try(sAInfAlgModel U;false) else true)
///


-- Both Burke representations expose their word labels in the block tools.
TEST ///
S=QQ[x,y]; R=S/(x^2,y^2); M=coker vars R
F=burkeResolution(M,3)
B=sAInfAlgModel R; G=sAInfModModel(B,M)
f=burkeDifferential(R,B,G,3)
-- The individual lifts can differ; the word summands have the same dimensions.
for w in indices source f do (
    globalBlock:=extractBlocks(F.dd_3,w);
    typedBlock:=extractBlocks(f,w);
    assert(numrows globalBlock == numrows typedBlock and numcols globalBlock == numcols typedBlock))
assert(match("[{]2, 1[}]",toString picture F.dd_3))
-- An explicit outer label takes precedence over a nested label.
T=directSum("outer"=>directSum("inner"=>S^1), "second"=>S^1)
assert(extractBlocks(id_T,{"outer"}) == T_["outer"])
///
