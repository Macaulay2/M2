-- Regression tests for the typed models.

TEST ///
-- Degree zero of the predicted Burke complex retains its generators.
S = QQ[x]
R = S/ideal(x^2)
assert(golodBetti(R^{2:-3},0) == betti complex {S^{2:-3}})
///

TEST ///
--ring and module have pdim 1
S = QQ[t]
R = S/t^3
M = R^1/t^2
F = burkeResolution(M, 5)
assert (0 == F.dd^2 and all(length F -1 , i-> 0 == HH_(i+1) F));

--resolution of the ring as module
M = R^1
F = burkeResolution(M,5) -- a nonminimal resolution!
F.dd
assert (0 == F.dd^2 and all(length F -1 , i-> 0 == HH_(i+1) F));

--ring is Golod, module is pdim 1
S = ZZ/101[s,t,u,v]
R = S/(s*ideal(s,t,u,v))
M = R^1/s
F = burkeResolution(M,5, Check=>false)
assert (0 == F.dd^2 and all(length F -1 , i-> 0 == HH_(i+1) F));
///

TEST ///
debug needsPackage "AInfinity"
u = {2,2,3}
assert(sort sModIndicesStasheff u == sort {{2, 2, {2, 2, 2}}, {0, 1, {3, 3}}, {0, 2, {6}}, {1, 2, {2, 4}}})
///

TEST ///
   S = ZZ/101[a,b,c]
   R = S/(ideal(a)*ideal(a,b,c))
   Y = R^1/a
   --  mR = sAInfAlgModel R
   --  sAInfModModel(mR,Y)
   F = burkeResolution(Y,8)
   assert(F.dd^2 == 0)
   assert(all(7, i-> prune HH_(i+1)F == 0))
///

TEST ///
kk = ZZ/101
S = kk[a,b,c]
R = S/((ideal a^2)*ideal(a,b,c)) -- a simple 3 variable Golod ring
K = koszul vars R
M = coker K.dd_2
F = res(M, LengthLimit => 5)
N = coker F.dd_5 ;
mA = sAInfAlgModel R;
mG = sAInfModModel(mA,N);
F = burkeResolution(N,5)
assert(betti F == betti res (N, LengthLimit => 5))
assert(F.dd^2 == 0)
assert all(length F -1, i-> prune HH_(i+1)F == 0)
///

TEST ///
kk = ZZ/101
S = kk[x,y]
R = S/ideal"xy"
mR = sAInfAlgModel R
mG = sAInfModModel(mR,coker vars R,Check=>true)
N = coker vars R
F = burkeResolution(N, 5)
assert(F.dd^2 == 0)
assert all(length F -1, i-> prune HH_(i+1)F == 0)
assert(betti burkeResolution(N,5) == betti res (N, LengthLimit => 5))
///

TEST ///
-- multigraded example
kk = ZZ/101
S = kk[x,y,u,v,Degrees => {{1,0},{1,0},{0,1},{0,1}}]
R = S/ideal"xu-yv"
mR = sAInfAlgModel R
mG = sAInfModModel(mR,coker vars R,Check=>true)
N = coker vars R
F = burkeResolution(N, 6)
assert(isHomogeneous F)
assert(F.dd^2 == 0)
assert all(length F -1, i-> prune HH_(i+1)F == 0)
assert(betti burkeResolution(N,6) == betti res (N, LengthLimit => 6))
///

TEST ///
kk = ZZ/101
S = kk[x,y,z,u]
I = ideal(u^3, x*y^2, (x+y)*z^2, x^2*u+z*u^2, y^2*u+x*z*u, y^2*z+y*z^2) -- has the betti nums as in Roos
R = S/I
mR = sAInfAlgModel(R,TensorOrder => 2);
-- checking that limiting order worked
assert(any(keys mR#"mm", k -> #k == 2))
assert all(keys mR#"mm", k -> #k <= 2)
mR = sAInfAlgModel(R,TensorOrder => 3);
assert(any(keys mR#"mm", k -> #k == 3))
assert all(keys mR#"mm", k -> #k <= 3)
///

TEST ///
S = ZZ/101[a..d]
R = S/ideal"a3,b3,c3,d3"
mA = sAInfAlgModel R;
--tensorCommutativity(M,N): M**N --> N**M.
B = mA#"natural"
--m{2,2} is skew-symmetric, mimicking A_1**A_1 -> A_2
assert (0==mA#"mm"#{2,2}+mA#"mm"#{2,2}*tensorCommutativity (B_2,B_2)) -- anti-commutative
assert (0==mA#"mm"#{3,3}-mA#"mm"#{3,3}*tensorCommutativity (B_3,B_3)) -- commutative
-- this seems wrong; I would have thought that 2,3 should commute.
-- FM + JL: due to Burke's sign convention, this is ok.
assert (0==mA#"mm"#{3,2}+mA#"mm"#{2,3}*tensorCommutativity (B_3,B_2))
///

TEST ///
S = ZZ/101[a,b,c,d,e,f]
R = S/(ideal(a,b,c)*ideal(d,e,f))
mR = sAInfAlgModel R;
assert checkStasheff mR
///

TEST ///
debug needsPackage"AInfinity"
S = ZZ/101[a,b,c]
K' = complex koszul vars S

K = labeledTensor K' --this should work!
assert(labels K_2 == {{2}})
assert(K_0_[{0}] == map(S^1,S^1,id_(S^1)))

K2 = labeledTensor({K',K'},0,3)
picture K2
labelsAndComponents(K2_1)
assert(K2_1_[{1,0}] == map(S^{6:-1},S^{3:-1},id_(S^{3:-1})||0*id_(S^{3:-1})))
assert(max K2 == 3)

R = S/ideal a
RK = labeledTensor(R,K)
picture RK
assert(labels RK_2 == {{2}})
assert(RK_0_[{0}] == map(R^1,R^1,1))
///

TEST ///

kk = ZZ/101
S = kk[x,y,z]
I = ideal"x2,y2,z2"*(ideal vars S)
R =S/I
mA = sAInfAlgModel R
mG = sAInfModModel(mA,coker vars R)
F = burkeResolution(coker vars R,6)
assert (F.dd^2==0)
assert all(5, i-> prune HH_(i+1)F == 0)
///

TEST ///
kk = ZZ/101
S = kk[a,b,c,d]
R = S/(ideal(a,b,c,d))^2
--R = S/ideal"a3,b3,c3"
mA = sAInfAlgModel R
K = koszul vars R
--  apply(3,i-> sAInfModModel (mA,coker K.dd_(i+1)));
-- takes too long
///

TEST ///
--a long resolution with more vars, pd M small
S = ZZ/101[x_1..x_4]
I = x_1*ideal(vars S)
R = S/I
M = R^1/ideal(x_1..x_2)
F = burkeResolution(M, 8, Check =>false)
assert(F.dd^2 == 0)
assert all(length F - 1, i-> prune HH_(i+1)F == 0)
///

TEST ///
kk = ZZ/101
S = kk[a,b,c]
R = S/((ideal a^2)*ideal(a,b,c)) -- a simple 3 variable Golod ring
sAInfR = sAInfAlgModel R
aInfR = aInfAlg sAInfR
assert (checkStasheff sAInfR)
assert (checkStasheff aInfR)
///

TEST ///
S = ZZ/101[x_0..x_2]
gor = n -> (
    m = 2*n+1;
    pfaffians(m-1, map(S^m, S^{m:-2}, (i,j) -> if i>=j then 0 else
	                        if i == j-1 then (if even i then S_0 else S_1) else
				if i == m-j-1 then S_2 else
				0_S)))
R = S/gor 3
burkeRes = burkeResolution(coker vars R, 5)
-- make better checks?
assert(rank burkeRes_5 == 276)
///

TEST ///
kk = ZZ/101
n = 4 -- make ring big enough
Q = kk[x_0..x_n]
I = ideal(flatten apply(n,i -> apply(i, j-> x_j*x_i)) | apply(n,i -> x_i^2-x_(i+1)^2))
R = Q/I
B = sAInfAlgModel R;
A = aInfAlg B;
G = sAInfMod B;
F = aInfMod A;
assert(B == sAInfAlg A)
assert(checkStasheff B)
assert(checkStasheff A)
assert(checkStasheff F)
assert(checkStasheff G)
assert(sAInfMod F == G)
assert(checkStasheff(F[3]))
assert(checkStasheff(G[3]))
assert(checkStasheff(F[-2]))
assert(checkStasheff(G[-2]))
///
