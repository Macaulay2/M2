G = flagBundle({2,2}, VariableNames => {b,c})
G.StructureMap
g = G/point
target g
intersectionRing oo
intersectionRing G
G.Bundles
(S,Q) = G.Bundles
G.BundleRanks
Q
chern Q
segre Q
chern S
chern (S + Q)
I = ideal intersectionRing G
groebnerBasis I
compactMatrixForm = false
transpose groebnerBasis I
c_1^2
c_1^3
c_1^4
integral oo
sectionClass (G/point)
-- Lines on hypersurfaces:
B = symmetricPower_3 Q
integral chern B
chern B
ctop B
code(integral, class chern B)
g_*
g_* chern B
code(g_*, class chern B)
f = n -> (
     -- number of lines in P^n lying on a hypersurface
     -- of degree 2n-3
     G := flagBundle {n-1,2};
     integral chern symmetricPower_(2*n-3) last G.Bundles)
f 3
f 10
-- Conics on a quintic threefold:
G = flagBundle {2,3} -- planes in P^4
(S,Q) = G.Bundles
B = symmetricPower_2 Q -- quadratic forms on them
-- space of conics in planes in P^4:
X = projectiveBundle(dual B, VariableNames => {,{z}})
-- forms of degree 5 on planes in P^4 modulo those vanishing
-- on the conic
A = symmetricPower_5 Q - symmetricPower_3 Q ** OO(-z)
-- the number of conics lying in a quintic hypersurface
integral chern A
-- intersection rings with parameters:
pt = base symbol n
X = abstractProjectiveSpace_1 pt
chi OO_X(1)
chi OO_X(2)
chi OO_X(3)
chi OO_X(n)
X = abstractProjectiveSpace_2 pt
chi OO_X(n)
tangentBundle X
todd X
F = symmetricPower_n tangentBundle X
chern F
integral oo
ch F
chi F
F^**4
-- The Horrocks-Mumford bundle:
X = abstractProjectiveSpace_4 pt
A = intersectionRing X
cotangentBundle X
exteriorPower_2 oo
F = 2 * (exteriorPower_2 cotangentBundle X)(2) - 5 * OO_X(-1) - 5 * OO_X
chern F
chi F(n*h)
-- base varieties:
S = base(2,p,q,Bundle =>(A,1,a), Bundle => (B,2,{b1,b2}))
B
S = base(2,p,q,Bundle =>(symbol A,1,a), Bundle => (symbol B,2,{b1,b2}))
p
A
chern A
B
chern B
chern(A*B)
X = abstractProjectiveSpace(3,S,VariableName => H)
intersectionRing X
f = X/S
-- check the projection formula:
x = chern f_* (f^* OO_S(p*a_1) * OO_X(q*H))
y = chern f_* OO_X((f^*(p*a_1))+q*H)
x == y
-- schubertCycle()
base(2, Bundle=>(A, n=8, a))
F = flagBundle ({5,q=3},A)
CH = intersectionRing F;
describe CH
F_(1,3,5)
{n-q+0-1,n-q+1-3,n-q+2-5}
-- abstractVariety
A = QQ[c,d,Degrees=>{1,2}]
X = abstractVariety(3,A)
peek A
intersectionRing X
F = abstractSheaf(X, ChernCharacter => 3+c+d)
ch F
rank F
chern F
adams_101 F
ch oo
chern ooo
clearAll
-- Chern class variables:
F = flagBundle({2,2,2},VariableNames => {{chern_1 S .. chern_2 S},{chern_1 M,chern_2 M},{chern_1 Q,chern_2 Q}})
A = intersectionRing F
(S,M,Q) = F.Bundles
chern Q
chern M
chern S
-- Schur functors
base(3, Bundle => (E,4,e))
chern E
chern schur_{1,1,1,1} E
chern exteriorPower_4 E
oo === ooo
chern schur_{3,1} E
chern schur_{2,2} E
chern schur_{2,1,1} E
chern schur_{4} E
chern symmetricPower_4 E
oo === ooo
-- do they make sense for virtual bundles?
chern schur_{2,3} E
clearAll
-- Riemann-Roch on a curve:
-- We follow Example 15.2.1 of Fulton's book, Intersection Theory.
X = abstractVariety(1,QQ[r,s,e_1,f_1,D,K,Degrees=>{2:0,4:1}])
X.TangentBundle = abstractSheaf(X,Rank=>1,ChernClass=>1-K)
chi OO_X
chi OO(D)
E = abstractSheaf(X,Rank => r, ChernClass => 1+e_1)
F = abstractSheaf(X,Rank => s, ChernClass => 1+f_1)
chi Hom(E,F)
-- Riemann-Roch on a surface
-- We follow Example 15.2.2 of Fulton's book, Intersection Theory.
X = abstractVariety(2,QQ[r,D,d_1,K,c_2,d_2,Degrees=>{0,3:1,2:2}])
X.TangentBundle = abstractSheaf(X,Rank=>2,ChernClass=>1-K+c_2)
todd X
chi OO_X
E = abstractSheaf(X,Rank => r, ChernClass => 1+d_1+d_2)
chi E - rank E * chi OO_X
chi OO(D) - chi OO_X
chi OO_D
p_a = D -> 1 - chi OO_D;
p_a D
Y = abstractProjectiveSpace_2 base n
factor p_a (n*h)
Z = abstractProjectiveSpace_1( use abstractProjectiveSpace_1 base(m,n), VariableName => k)
factor p_a (m*h + n*k)
-- Riemann-Roch without denominators
-- We display some of the universal polynomials described in Lemma 15.3 of Fulton's book, Intersection Theory.
f = (n,d,e) -> (
    (D,E) := (symbol D,symbol E);
    X = base(n,
	 Bundle => (D,d,chern_1 D .. chern_d D),
	 Bundle => (E,e,chern_1 E .. chern_e E));
    p := chern(exteriorPower dual D * E) - 1;
    assert( p % ctop D == 0 );
    p // ctop D );
n = 4;
for d from 1 to 3 do for e from 1 to 4 do << endl << "d=" << d << " e=" << e << " P(D,E) = " << f(n,d,e) << endl
-- examples from Schubert:
 -- Hilbert polynomial and Todd class of projective 3-space
   -- > proj(3,h,all): factor(chi(o(n*h)));
   -- 			     1/6 (n + 3) (n + 2) (n + 1)
   Ph = abstractProjectiveSpace_3 base symbol n; factor chi OO(n*h)
   -- > Ph[toddclass_];
   -- 					      2  2    3  3
   -- 			    1 + 2 h t + 11/6 h  t  + h  t
   todd Ph
 -- Generation of formulas
   -- > DIM:=4:
   -- > A:=bundle(2,c):        # a bundle with Chern classes c1,c2 and rank 2
   -- > B:=bundle(3,d):        # a bundle with Chern classes d1,d2,d3 and rank 3
   base(4, Bundle => (A,2,c), Bundle => (B,3,d))
   -- > chern(A);
   --                                                2
   --                                 1 + c1 t + c2 t
   chern A
   -- > segre(B);
   --                              2        2      3                  3
   --                1 + d1 t + (d1  - d2) t  + (d1  - 2 d1 d2 + d3) t
   --
   --                          4          2               2   4
   --                     + (d1  - 3 d2 d1  + 2 d1 d3 + d2 ) t
   segre B
   -- > chern(A&*B);           # The Chern class of the tensor product
   --                          2                 2                 2
   -- 1 + (2 d1 + 3 c1) t + (d1  + 5 c1 d1 + 3 c1  + 2 d2 + 3 c2) t  +
   --   
   --                          3                           2                    2   3
   --   (6 c1 c2 + 2 d1 d2 + c1  + 2 d3 + 4 c1 d2 + 2 c1 d1  + 4 d1 c2 + 4 d1 c1 ) t
   --   
   --                                     2          2               2
   --    + (3 c1 d1 d2 + 6 d1 c1 c2 + 3 c2  + 3 c2 c1  + 2 d1 d3 + d2  + 3 c1 d3
   --   
   --             2       2        2   2        3   4
   --    + 2 c2 d1  + 3 c1  d2 + c1  d1  + d1 c1 ) t
   chern(A*B)
   -- > chern(3,symm(3,dual(A)));
   --                                      3
   --                                - 6 c1  - 30 c1 c2
   chern_3 symmetricPower_3 dual A
   -- > segre(2,Hom(wedge(2,A),wedge(2,B)));
   --                               2                 2
   --                           3 d1  - 8 c1 d1 + 6 c1  - d2
   segre_2 Hom(exteriorPower_2 A,exteriorPower_2 B)
 -- Grassmannian of lines in P3
   -- > grass(2,4,b,all): 
   Gb = flagBundle({2,2}, base n, VariableNames => {,b})
   Qb = last Gb.Bundles
   -- > chi(Gb,Symm(n,Qb));
   --                              2            3
   --                             n  + 1 + 1/6 n  + 11/6 n
   chi symmetricPower_n Qb
   -- > chi(Gb,o(n*b1));
   --                            4        3    23   2
   --                      1/12 n  + 2/3 n  + ---- n  + 7/3 n + 1
   --                                          12
   chi OO_Gb(n*b_1)
   -- > 
   -- ## This should be a quadric in P5:
   -- > 
   -- > proj(5,H,all): chi(o(n*H)-o((n-2)*H));
   --                            4        3    23   2
   --                      1/12 n  + 2/3 n  + ---- n  + 7/3 n + 1
   --                                          12
   P5 = abstractProjectiveSpace(5,base n,VariableName=>H); chi(OO(n*H)-OO((n-2)*H))
 -- Lines on a quintic threefold
   -- # Lines on a quintic threefold.  This is the top Chern class of the 
   -- # 5th symmetric power of the universal quotient bundle on the Grassmannian
   -- # of lines.
   -- > 
   -- > grass(2,5,c):        # Lines in P^4. 
   Gc = flagBundle({3,2}, VariableNames => {,c})
   (Sc,Qc) = Gc.Bundles
   -- > B:=symm(5,Qc):       # Qc is the rank 2 quotient bundle, B its 5th 
   -- >                      # symmetric power.
   B = symmetricPower(5,Qc)
   -- > c6:=chern(rank(B),B):# the 6th Chern class of this rank 6 bundle.
   c6 = chern(rank B,B)
   -- > integral(c6);
   --                                       2875
   integral c6
 -- Conics on a quintic threefold
   -- # Conics on a quintic threefold. This is the top Chern class of the 
   -- # quotient of the 5th symmetric power of the universal quotient on the
   -- # Grassmannian of 2 planes in P^5 by the subbundle of quintic containing the 
   -- # tautological conic over the moduli space of conics.
   -- > 
   -- > grass(3,5,c):         # 2-planes in P^4.  
   Gc = flagBundle({2,3}, VariableNames => {,c})
   (Sc,Qc) = Gc.Bundles
   -- > B:=Symm(2,Qc):        # The bundle of conics in the 2-plane. 
   B = symmetricPower(2,Qc)
   -- > Proj(X,dual(B),z):    # X is the projective bundle of all conics. 
   X = projectiveBundle(dual B, VariableNames => {,{z}})
   -- > A:=Symm(5,Qc)-Symm(3,Qc)&*o(-z):  # The rank 11 bundle of quintics 
   -- >                                   # restricted to the universal conic. 
   A = symmetricPower_5 Qc - symmetricPower_3 Qc ** OO(-z)
   -- > c11:=chern(rank(A),A):# its top Chern class.
   c11 = chern(rank A, A)
   -- > lowerstar(X,c11):     # push down to G(3,5).
   (X/Gc)_* c11
   -- > integral(Gc,");       # and integrate there.
   --                                      609250
   integral oo
 -- Count the number of space conics intersecting 8 given lines
   -- > grass(3,4,d,all):
   Gd = flagBundle({1,3}, VariableNames => {,d})
   (Sd,Qd) = Gd.Bundles
   -- > Proj(f,dual(symm(2,Qd)),e):
   f = projectiveBundle(dual symmetricPower_2 Qd, VariableNames => {,{e}})
   -- > integral(Gd,lowerstar(f,(2*d1+e)^8));
   --                                        92
   integral (2*d_1 + e)^8
 -- Euler characteristic of Horrocks-Mumford bundle
   -- > proj(4,h,tang):            # need tangentbundle for chi
   X = abstractProjectiveSpace(4,base n,VariableName => h)
   -- > F:=sheaf(2,[5*h,10*h^2]):  # defines the Horrocks-Mumford bundle
   F = abstractSheaf(X, Rank => 2, ChernClass => 1 + 5*h + 10*h^2)
   -- > chi(F&*o(n*h));            # computes chi of its twists
   --                           4        3   125  2
   --                     1/12 n  + 5/3 n  + --- n  + 125/6 n + 2
   --                                         12
   chi (F ** OO(n*h))
 -- Riemann-Roch formulas
   -- # Line bundle O(D) on a threefold.
   -- > 
   -- > variety(X,dim=3,tan=sheaf(3,[-K,c2,c3])): # traditionally, -K is 
   -- >                                           # used instead of c1
   X = abstractVariety(3,QQ[K,c_2,c_3, Degrees => {1..3}][D,Join=>false])
   X.TangentBundle = abstractSheaf(X,Rank=>3,ChernClass=>1-K+c_2+c_3)
   -- > chi(o(D));
   --                         3          2          2
   --        integral(X, 1/6 D  - 1/4 K D  + (1/12 K  + 1/12 c2) D - 1/24 K c2)
   chi OO(D)
 -- The number of elliptic cubics on a sextic 4-fold
   -- > grass(3,6,c):
   Gc = flagBundle({3,3}, VariableNames => {,c})
   (Sc,Qc) = Gc.Bundles
   -- > B:=Symm(3,Qc):
   B = symmetricPower_3 Qc
   -- > Proj(X,dual(B),z):
   X = projectiveBundle(dual B, VariableNames => {,{z}})
   -- > A:=Symm(6,Qc)-Symm(3,Qc)&@o(-z):
   A = symmetricPower_6 Qc - symmetricPower_3 Qc ** OO(-z)
   -- > c18:=chern(rank(A),A):
   -- > lowerstar(X,c18):
   -- > integral(Gc,%);
   -- Ans =  2734099200
   integral chern A
-- Local Variables:
-- coding: utf-8
-- compile-command: "$M2BUILDDIR/M2 -q Schubert2.m2 <demo4.m2"
-- End:
