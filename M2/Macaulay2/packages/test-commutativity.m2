needsPackage "CompleteIntersectionResolutions"
needsPackage "HigherCIOperators"
load "compose.m2"
{*
--This is the code Dan wrote May 20 2015 to replace the incorrect Hom (temporarily called Hom2)
Hom(Module,Module) := (M,N) -> kernel (transpose presentation M ** N)
Hom(Matrix,Module) := (f,N) -> inducedMap(Hom(source f,N),Hom(target f,N),transpose cover f ** N)
Hom(Module,Matrix) := (M,f) -> inducedMap(Hom(M,target f),Hom(M,source f),dual cover M ** f)
*}

{*
triv(Module, Module) := (M,N)->(
    --image triv is the submodule of stably trivial homomorphisms
    --this version has source a free module; might make for faster computation.
    s := dual (syz (dual presentation M));
    inc := map (target s, M, s);
    proj := map(N, cover N, 1);
    map(Hom(M,N), Hom(target s, source proj), Hom(inc,proj))
    )
*}
{*
triv(Module, Module) := (M,N) -> (
    --image triv is the submodule of stably trivial homomorphisms
    p :=map(N, cover N,1);
    Hom(M, p))
*}

triv = method()
triv(Module, Module) := (M,N) ->(
    --image triv is the submodule of stably trivial homomorphisms
    gN := map(N,cover N, 1);
    MCN := Hom(M,cover N);
    freegens := map(MCN, cover MCN, 1);
    Hom(M, gN)*freegens)

end -- 

--the following implements "the" obstruction to commutativity at the
--syzygy level: whether the composition q*p \in Hom(M_(k+4), M_k)
--of the CI operators starting from
--stage k+4 is in the submodule q*triv+triv*p+triv*triv.

restart
load "test-commutativity.m2"
S = ZZ/101[a,b,c]
ff = matrix"a2,b2,c2"
R = S/ideal ff
red = map(R,S)
M0 = R^1/ideal(a,b*c)

FR = complete res(M0, LengthLimit=>10)
--M=apply(9,i->coker FR.dd_(i+1));
M={M0}|apply(9,i->image FR.dd_(i+1));
A = chainComplex(apply(length FR-1, i->lift (FR.dd_(i+1),S)))
L = trueKoszul ff
u = higherCIOperators(A,L);

k = 0
B = M_(k+4);
C = M_(k+2)**red L_1;
D = M_k**red L_2;

p = map(C,B, red u#{2,k+4,0}); --M_(k+4) -> M_(k+2)**red L_1
q = map(D,C, red u#{2,k+2,1}); --M_(k+2)**red L_1 -> M_(k)**red L_2

--The CI operators are not stably trivial, but their composition is:
assert(isStablyTrivial (q*p) and not isStablyTrivial p and not isStablyTrivial p)

--form the map Hom(B,D)<--Hom(B,C)**Hom(C,D)
elapsedTime c = compose(B,C,D);
elapsedTime c1 = compose1(B,C,D);
c == c1
betti c
betti c1
source c == source c1
target c == target c1
(gens target c * matrix c)%relations target c == (gens target c1 * matrix c1) % relations target c1

betti gens Hom(B,C)
betti gens Hom(C,D)
-- test whether q*p
-- is in the submodule q*str + str'*p + str'*str, where 
--str = stably trivial maps from B to C
--str' = stably trivial maps C to D

P = homomorphism' p;
Q = homomorphism' q;
QP = homomorphism' (q*p);

str = triv(B,C);
str' = triv(C,D);

str'p =  map(Hom(B,D),,matrix compose(B,C,D)*(matrix P ** matrix str'));
qstr = map(Hom(B,D),,matrix compose(B,C,D)*(matrix str ** matrix Q));
time str'str = map(Hom(B,D),,matrix c*(matrix str ** matrix str'));

--confirm that q*p does not map into mm^2*D:
--assert(inmm2(q*p) == false)

time test = map(coker(str'p|qstr|str'str), source QP, QP);
assert(test !=0)

--================
--the following code shows that q*p has image in the max ideal times D, even though
--it's not 0. This follows anyway from stable triviality.
mm = ideal vars R
(q*p) 
inD = map(D, mm*D, gens(mm*D)//gens D); -- the inclusion of mm*D into D
in2D = map(D, mm^2*D, gens(mm^2*D)//gens D); -- the inclusion of mm^2*D into D
modmm = map(coker inD, D, 1)
modmm2 = map(coker in2D, D, 1)
modmm*q*p
modmm2*q*p




---In the Tate resolution, it seems that commutativity holds even for perturbed differentials
restart
load "test-commutativity.m2"

S = ZZ/101[a,b,c]
ff = matrix"a2,b2,c2"
R = S/ideal ff
red = map(R,S)
M0 = coker vars R
FR = complete res(M0, LengthLimit=>8)
M=apply(7,i->coker FR.dd_(i+1));
A0 = chainComplex apply(length FR-1, i->lift (FR.dd_(i+1),S))
--make a general perturbation:
L = trueKoszul ff
A = chainComplex apply(length FR-1, i-> A0.dd_(i+1) + (A0_i**L.dd_1)*random(A0_i**L_1,A0_(i+1)))
k = 2
B = M_(k+4);
C = M_(k+2)**red L_1;
D = M_k**red L_2;

--now make the CI operators
--now make the CI operators
u = higherCIOperators(A,L);
p = map(C,B, red u#{2,k+4,0}); --M_(k+4) -> M_(k+2)**red L_1
q = map(D,C, red u#{2,k+2,1}); --M_(k+2)**red L_1 -> M_(k)**red L_2

--curious: commutativity holds here even for the perturbed maps
assert(q*p == 0)



load "test-commutativity.m2"
--viewHelp HigherCIOperators
S = ZZ/101[a,b,c]
ff = matrix"a2,b2,c2"
R = S/ideal ff
red = map(R,S)
M0 = R^1/ideal(a,b*c)
isPossiblyCommutative(ff, M0, 2)

use R
map (R^1/((ideal a)*R^1), R^1/ideal a, a) 

--make the inclusion Hom(B,mmR*D) --> Hom(B,D):
inD = map(cover D, cover (mmR*D), gens (mmR*D));
hBD = prune (Hom(B,D)/image Hom(B,inD));
m = map( hBD, Hom(B,D)/image Hom(B,inD), hBD.cache.pruningMap);
n = map(Hom(B,D)/image Hom(B, inD), Hom(B,D), 1);
m*n*triv(B,D)==0 -- it seems that triv(B,D) is not contained
--in Hom(B, mmR*D), something I find hard to believe...
unique flatten entries (m*n*triv(B,D))


tr = triv(B,D);
rank source tr
p =map(D, cover D,1);
rank source p
rank gens Hom(B,cover D)
rank source Hom(B, p)

hhh = Hom(cover B, id_(cover D));
rank source hhh

rank source tr
betti tr
apply(rank source tr, i->print (coker vars R**homomorphism tr_{i}==0))
homomorphism (tr_{0}) 
{rank source tr -2})
u = matrix{{1,2},{3,4}}
apply(rank source u, i->u_{i})
rank source u


--The following doesn't quite work, and would not be useful if it did!!
--The function call 
--isPossiblyCommutative(ff,MR,k) 
--computes whether the class of the map 
--M_(k+4) --> M_k**(L_2), modulo the maximal ideal times the space of stably trivial
--maps, where L_2 is the 2nd exterior power of R^c 
--and M_k is the k-th R-syzygy of MR = M_0.
--This is nonzero, for example, nonzero in the case M = R/(a,bc) 
--where R = k[a,b,c]/(a2, b2, c2) for small values of k.

isPossiblyCommutative = (ff,MR,k) ->(
    --MR a module over R = S/ff.
    --the script returns false if the test shows that
    --the CI operators for ff on the R-free res of MR
    --starting at the (k+4)-th syzygy
    --can NOT be made commutative 
    S := ring ff;
    R := ring MR;
    mmR := ideal vars R;
    red := map(R,S);
    FR := complete res(M0, LengthLimit=>k+6);
    M := apply(k+5,i->coker FR.dd_(i+1));
    A := chainComplex(apply(length FR-1, i->lift (FR.dd_(i+1),S)));
    L := trueKoszul ff;
    u := higherCIOperators(A,L);
    B := M_(k+4);
    C := M_(k+2)**red L_1;
    D := M_k**red L_2;
--now make the CI operators
    p := map(C,B, red u#{2,k+4,0}); --M_(k+4) -> M_(k+2)**red L_1
    q := map(D,C, red u#{2,k+2,1}); --M_(k+2)**red L_1 -> M_(k)**red L_2
--is q*p in mm*triv?
   QP := homomorphism' (q*p);
   TrivBD := mmR*image triv(B,D);
--   HBD := Hom(B,D)/TrivBD;
   HBD := Hom(B,D)/Hom(B,mmR*D);
   test1 := map(HBD, Hom(B,D), 1);
   test := (gens HBD)*matrix (test1*QP);
   0==test % gb relations HBD
   )

