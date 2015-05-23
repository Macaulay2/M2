--The function call 
--isPossiblyCommutative(ff,MR,k) 
--computes an obstruction to the commutativity of the CI operators
--at the k-th stage (ie F_(k+4) --> F_k) 
--of the resolution of a module MR over a complete intersection S/(ideal ff)
--of codimension c. It returns false if the obstruction is nonzero.

--This obstruction is the class of the map 
--M_(k+4) --> M_k**(L_2), modulo the maximal ideal times the space of stably trivial
--maps, where L_2 is the 2nd exterior power of R^c 
--and M_k is the k-th R-syzygy of MR = M_0.
--This obstruction is, for example, nonzero in the case M = R/(a,bc) 
--where R = k[a,b,c]/(a2, b2, c2) for small values of k.

needsPackage "CompleteIntersectionResolutions"
needsPackage "HigherCIOperators"

--This is the code Dan wrote May 20 2015 to replace the incorrect Hom (temporarily called Hom2)
Hom(Module,Module) := (M,N) -> kernel (transpose presentation M ** N)
Hom(Matrix,Module) := (f,N) -> inducedMap(Hom(source f,N),Hom(target f,N),transpose cover f ** N)
Hom(Module,Matrix) := (M,f) -> inducedMap(Hom(M,target f),Hom(M,source f),dual cover M ** f)

{*
ev = N -> (
    --if N is a free R-module returns the contraction map N**N^* -> R.
    reshape((ring N)^1, N**(dual N), id_N)
    )
*}
compose = method()
compose(Module, Module, Module) := (M,N,P) ->(
        --defines the map Hom(M,N)**Hom(N,P) -> Hom(M,P)
    MN := Hom(M,N);
    NP := Hom(N,P);
    MP := Hom(M,P);
    CN := cover N;
    ev := reshape((ring CN)^1, CN**(dual CN), id_CN);
    ambMap := map(ambient MP, (ambient MN) ** ambient(NP), 
--	    (cover M)**ev(cover N)**(cover P));
	    (cover M)**ev**(cover P));
    gensMap := (ambMap*((gens MN)**(gens NP)))//gens(MP);
    map(MP, MN**NP,gensMap)
    )

triv = method()
triv(Module, Module) := (M,N)->(
    --image triv is the submodule of stably trivial homomorphisms
    --this version has source a free module; might make for faster computation.
    s := dual (syz (dual presentation M));
    inc := map (target s, M, s);
    proj := map(N, cover N, 1);
    map(Hom(M,N), Hom(target s, source proj), Hom(inc,proj))
    )
{*
triv(Module, Module) := (M,N) -> (
    --image triv is the submodule of stably trivial homomorphisms
    p :=map(N, cover N,1);
    Hom(M, p))
*}

TEST///
S = ZZ/101[a,b,c]
M = S^1/ideal a^3
N= S^1/ideal a^2
P = S^1/a^3
assert(homomorphism((compose(M,N,P))_{0}) == map(M,P,matrix{{a}}))
triv(M,N)
///

inmm2 = u ->(
    mm = ideal vars ring u;
    D := target u;
    in2 := map(D, mm^2*D, gens(mm^2*D)//gens D); -- the inclusion of mm^2*D into D
    modmm2 := map(coker in2D, D, 1);
    0 == modmm2*u)



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
M=apply(9,i->coker FR.dd_(i+1));
A = chainComplex(apply(length FR-1, i->lift (FR.dd_(i+1),S)))
L = trueKoszul ff
u = higherCIOperators(A,L);

k = 0
B = M_(k+4);
C = M_(k+2)**red L_1;
D = M_k**red L_2;

--form the map Hom(B,D)<--Hom(B,C)**Hom(C,D)
time c = compose(B,C,D);

--now make the CI operators
p = map(C,B, red u#{2,k+4,0}); --M_(k+4) -> M_(k+2)**red L_1
q = map(D,C, red u#{2,k+2,1}); --M_(k+2)**red L_1 -> M_(k)**red L_2


--The CI operators are not stably trivial, but their composition is:
assert(isStablyTrivial (q*p) and not isStablyTrivial p and not isStablyTrivial p)
--But we have a subtler test:
--the question is whether q*p
-- is in the submodule q*str + str'*p + str'*str, where 
--str = stably trivial maps from B to C
--str' = stably trivial maps C to D

P = mapToHomomorphism p;
Q = mapToHomomorphism q;
QP = mapToHomomorphism (q*p);

str = triv(B,C);
str' = triv(C,D);

--time Pstr' = P**str';--SLOW!: when k=1 it takes 100 sec on my fastest machine.
--time pstr = c*(Pstr'); 

--the following is a much faster way of composing:
str'p = map(Hom(B,D), R^0,0)
time scan(rank source str', i-> str'p = str'p|mapToHomomorphism (homomorphism (str'_{i})*p))

--time strQ = str**Q;
--time strq = c*(strQ);
--time str'str = c*(str**str');

qstr = map(Hom(B,D), R^0,0)
target (str_{0}) == source q
time scan(rank source str, i-> qstr = qstr|mapToHomomorphism (q*homomorphism (str_{i})))

str'str = map(Hom(B,D), R^0, 0)
time scan(rank source str', i-> (
     scan(rank source str, j->
	    str'str = str'str|mapToHomomorphism(homomorphism(str'_{i})*homomorphism(str_{j})))))

--confirm that q*p does not map into mm^2*D:
--assert(inmm2(q*p) == false)

time test = map(coker(str'p|qstr|str'str), source QP, QP);
assert(test !=0)

--================
the following code shows that q*p has image in the max ideal times D, even though
it's not 0. This follows anyway from stable triviality.
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
   QP := mapToHomomorphism (q*p);
   TrivBD := mmR*image triv(B,D);
--   HBD := Hom(B,D)/TrivBD;
   HBD := Hom(B,D)/Hom(B,mmR*D);
   test1 := map(HBD, Hom(B,D), 1);
   test := (gens HBD)*matrix (test1*QP);
   0==test % gb relations HBD
   )
