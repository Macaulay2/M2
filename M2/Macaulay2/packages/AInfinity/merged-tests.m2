TEST ///
S=ZZ/101[x,y,z,u]
I=ideal(u^3,x*y^2,(x+y)*z^2,x^2*u+z*u^2,y^2*u+x*z*u,y^2*z+y*z^2)
R=S/I
M=coker vars R
p=aInfinity R
h=aInfinity(p,M)
dualTable=dualAInfinityStructure(p,h)
for pair in {(p,h),(dualTable#"algebra",dualTable)} do (
    (a,m):=pair;
    B:=sAInfAlg a;
    G:=sAInfMod(a,m);
    assert(checkStasheff B and checkStasheff(aInfAlg B));
    assert(checkStasheff G and checkStasheff(aInfMod G));
    for n from 1 to 6 do assert(burkeDifferential(a,m,n)==burkeDifferential(S,B,G,n)))
assert(try(sAInfAlg(aInfinity(R,Order=>2));false) else true)
assert(try(sAInfMod(p,aInfinity(p,M,Order=>2));false) else true)
assert(try(sAInfMod(p,dualTable);false) else true)
///

TEST ///
debug needsPackage "AInfinity"
S=QQ[x,y]; R=S/ideal(S_0*(S_0-1),S_1); M=R^1/ideal(R_0-1);
P0=ideal(S_0,S_1); P1=ideal(S_0-1,S_1); S0=S_0; R0=R_0; oldX=x;
for N in {M,R^1,R^0} do (
 C:=burkeResolution(N,0,Check=>true);
 D:=burkeResolution(N,3,Check=>true);
 assert(min C==0 and max C==0 and C_0==D_0 and componentsAndIndices C_0==componentsAndIndices D_0));
setMaxIdeal P0;
F0=burkeResolution(M,0,P0,Check=>true);
F1=burkeResolution(M,3,P1,Check=>true);
L0=burkeResolution(M,0,P1,Check=>true);
F=burkeResolution(M,3,Check=>true);
assert(F0_0==0 and L0_0==F1_0 and indices L0_0 == indices F1_0)
assert(F1.dd^2 == 0 and ring F1 === R and F1.cache#"AInfinityModelPoint"==ideal(R_0-1,R_1))
assert(isSubset(ann coker F1.dd_1,ideal(R_0-1,R_1)))
assert(S.maxIdeal==P0 and oldX===x)
assert(not F.cache#?"AInfinityModelPoint" and isSubset(ann coker F.dd_1,ideal(R_0-1)))
assert(try(burkeResolution(M,-1,P1); false) else true)
T=QQ[h]; N=T^1/ideal h; hp=ideal(T_0-1);
H=burkeResolution(N,4,hp,Check=>true);
assert(all(5,i->H_i == T^0) and H.cache#"AInfinityModelPoint"==hp)
assert(not T.?maxIdeal)
///

TEST ///
debug needsPackage "AInfinity"
prefixOK=(M,G,L)->(
    assert(min G===0 and max G===0 and length G===0 and ring G===ring M);
    assert(G_0 == L_0 and degrees G_0 == degrees L_0);
    assert(componentsAndIndices G_0 == componentsAndIndices L_0);
    if G_0 != 0 then assert(indices G_0 == {{0}} and G_0_[{0}] == L_0_[{0}]));
S=QQ[a,b,Degrees=>{{1,0},{0,1}}];
R=S/ideal(S_0^2,S_1^3);
M=R^{{-1,0},{0,-2}};
p=ideal vars R;
for N in {M,R^0,coker vars R} do (
    prefixOK(N,burkeResolution(N,0,Check=>true),burkeResolution(N,2,Check=>true));
    prefixOK(N,burkeResolution(N,0,p,Check=>true),burkeResolution(N,2,p,Check=>true)));
assert(try(burkeResolution(M,-1);false) else true)
assert(try(burkeResolution(M,-1,p);false) else true)
assert(not S.?maxIdeal)
T=QQ[t]; N=T^1/ideal T_0;
w=T_0; at1=ideal(w-1); at0=ideal w;
assert(all(4,i->(burkeResolution(N,3,at1,Check=>true))_i==0));
assert(coker (burkeResolution(N,3,at0,Check=>true)).dd_1 == N)
assert(w===t and not T.?maxIdeal)
assert(try(burkeResolution(N,1,ideal(w^2+1));false) else true)
assert(try(burkeResolution(N,1,ideal(1_T));false) else true)
assert(try(burkeResolution(N,1,ideal(0_T));false) else true)
assert(try(burkeResolution(N,1,p);false) else true)
assert(w===t and not T.?maxIdeal)
setMaxIdeal at1;
prior=T.residueMap;
G=burkeResolution(N,2,at0,Check=>true);
assert(w===t and T.maxIdeal==at1 and T.residueMap===prior)
assert(G.cache#"AInfinityModelPoint"==at0 and ann(coker G.dd_1)==at0)
assert(try(burkeResolution(N,1,ideal(w^2+1));false) else true)
assert(w===t and T.maxIdeal==at1 and T.residueMap===prior)
-- A matrix presentation containing a unit split summand.
U=QQ[u,v]; V=U/ideal(U_0^2); F=coker map(V^{-1,-2},V^{-1,-3},matrix{{1_V,0},{0,V_1}});
prefixOK(F,burkeResolution(F,0),burkeResolution(F,2))
-- Mixed, nonpositive grading handled by the PR's regrading path.
W=QQ[e,f,Degrees=>{0,1}]; Z=W/ideal(W_0^2); K=coker vars Z;
prefixOK(K,burkeResolution(K,0),burkeResolution(K,2))
-- Inhomogeneous ZZ global construction.
U=ZZ[z]; V=U/ideal(U_0^2-2); N=coker matrix{{V_0}};
prefixOK(N,burkeResolution(N,0),burkeResolution(N,2))
///

TEST ///
debug needsPackage "AInfinity"
S=QQ[x,y,z]; R=S/ideal(0_S);
N=R^1/ideal(R_0*(R_0-1),R_0*R_1,R_0*R_2);
P=ideal vars S;
F=burkeResolution(N,4,P,Check=>true);
assert(HH_1 F != 0 and not isSubset(ann HH_1 F,ideal vars R))
assert(not S.?maxIdeal)
///

TEST ///
debug needsPackage "AInfinity"
S=QQ[x]; M=S^1 ++ coker vars S; p=ideal(S_0-1); o=ideal vars S;
for pt in {o,p} do (
    C:=burkeResolution(M,3,pt,Check=>true);
    F0:=burkeResolution(M,0,pt,Check=>true);
    assert(F0_0 == C_0 and indices F0_0 == indices C_0);
    assert(numgens C_0 == (if pt === p then 1 else 2)));
R=S/ideal(S_0*(S_0-1)); N=R^1 ++ (R^1/ideal R_0);
F=burkeResolution(N,3,p,Check=>true);
assert(numgens F_0 == 1)
L=S^{-3} ++ (S^{-5}/ideal S_0);
LF=burkeResolution(L,3,p,Check=>true); L0=burkeResolution(L,0,p,Check=>true);
assert(degrees LF_0=={{3}} and degrees L0_0=={{3}})
-- At p, inclusion of the free summand has zero kernel and a cokernel killed by x.
tf=map(M,S^1,{{1_S},{0_S}});
assert(ker tf == 0 and not isSubset(ann coker tf,p))
///

TEST ///
-- Resolution of one of the two components of Spec(QQ[x]/(x*(x-1))).
S=QQ[x]
R=S/(S_0*(S_0-1))
M=R^1/ideal(R_0-1)
for p in {ideal S_0,ideal(S_0-1)} do (
    setMaxIdeal p;
    C:=burkeResolution(M,3,Check=>true);
    assert(apply(4,i -> rank C_i) == {1,1,1,1});
    assert(apply(3,i -> ideal C.dd_(i+1)) == {ideal(R_0-1),ideal R_0,ideal(R_0-1)});
    assert(coker C.dd_1 == M);
    assert((burkeResolution(M,0))_0 == C_0);
    D:=burkeResolution(M,3,ideal R_0,Check=>true);
    assert(apply(4,i -> rank D_i) == {0,0,0,0});
    assert(S.maxIdeal == p))
assert(try(burkeResolution(M,0,Check=>1);false) else true)
///

TEST ///
-- Tensor summands retain compositions order, including for shifted factors.
debug needsPackage "AInfinity"
S=QQ[x]
C=complex({map(S^1,S^1,0),map(S^1,S^1,0)},Base=>-2)
D=complex({map(S^1,S^1,0),map(S^1,S^1,0)},Base=>1)
F=labeledTensorComplex{C,D,D}
for n from 0 to 6 do (
    expected:=apply(select(compositions(3,n),v -> all(v,i -> i<=2)),v -> v+{-2,1,1});
    assert(indices F_n == expected))
///
TEST ///
S=QQ[x,y,z]
-- A redundant zero outside the algebra support changes table identity only.
for R in {S,S/ideal 0_S,S/(ideal vars S)^2} do (
    p:=aInfinity R;
    for M in {R^0,R^1,coker vars R} do (
        h:=aInfinity(p,M);
        G:=sAInfMod(p,h);
        d:=dualAInfinityStructure(p,h);
        op:=d#"algebra";
        C:=op#"resolution";
        extra:=max C+1;
        copy:=merge(op,hashTable {{extra}=>C.dd_extra},(u,v)->v);
        assert(copy =!= op and sAInfAlg copy == sAInfAlg op);
        H:=sAInfMod(copy,d);
        assert(checkStasheff G and checkStasheff H);
        assert(checkStasheff aInfMod G and checkStasheff aInfMod H);
        for j from 1 to 3 do (
            assert(burkeDifferential(p,h,j) == burkeDifferential(S,G#"algebra",G,j));
            assert(burkeDifferential(copy,d,j) == burkeDifferential(S,H#"algebra",H,j)));))
R=S/(ideal vars S)^2; p=aInfinity R; h=aInfinity(p,coker vars R)
wrong=applyPairs(h,(k,v)->(k,if k === {2,0} then 0*v else v))
assert(try (sAInfMod(p,wrong,TensorOrder=>1);false) else true)
low=dualAInfinityModule(p,aInfinity(p,R^1))
assert(try (sAInfMod(p,low,TensorOrder=>1);false) else true)
unary=sAInfMod(p,h,TensorOrder=>1)
assert(checkStasheff unary and all(keys unary#"mm",k->#k==1))
///

TEST ///
-- The same quotient has a non-Golod complete intersection at one point
-- and a field at the other. A preselected point must not change the answer.
S=QQ[x,y]
P=ideal(S_0-1,S_1)
I=intersect(ideal(S_0^2,S_1^2),P)
R=S/I
setMaxIdeal P
rho=S.residueMap
assert not isGolodAInf R
assert isGolodAInf(R,P)
assert isGolodAInf(I,substitute(P,R))
assert(S.maxIdeal === P and S.residueMap === rho)
///
