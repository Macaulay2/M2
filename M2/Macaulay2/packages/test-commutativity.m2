--This file computes an (the?) obstruction to the commutativity of the CI operators
--at a given stage (ie F_k --> F_(k-4)).
--it is nonzero in the case M = R/(a,bc) where R = k[a,b,c]/(a2, b2, c2) and k = 4,5.

needsPackage "CompleteIntersectionResolutions"
needsPackage "HigherCIOperators"

ev = N -> (
    --if N is a free R-module returns the contraction map N**N^* -> R.
    reshape((ring N)^1, N**(dual N), id_N)
    )
compose = method()
compose(Module, Module, Module) := (M,N,P) ->(
        --defines the map Hom(M,N)**Hom(N,P) -> Hom(M,P)
    MN := Hom(M,N);
    NP := Hom(N,P);
    MP := Hom(M,P);
    ambMap := map(ambient MP, (ambient MN) ** ambient(NP), 
	    (cover M)**ev(cover N)**(cover P));
    gensMap := (ambMap*((gens MN)**(gens NP)))//gens(MP);
    map(MP, MN**NP,gensMap)
    )
triv = method()
triv(Module, Module) := (M,N) -> (
    --image triv is the submodule of stably trivial homomorphisms
    p :=map(N, cover N,1);
    Hom(M, p))

TEST///
S = ZZ/101[a,b,c]
M = S^1/ideal a^3
N= S^1/ideal a^2
P = S^1/a^3
assert(homomorphism((compose(M,N,P))_{0}) == map(M,P,matrix{{a}}))
triv(M,N)
///

end -- 

restart
load "test-commutativity.m2"
--viewHelp HigherCIOperators
S = ZZ/101[a,b,c]
ff = matrix"a2,b2,c2"
R = S/ideal ff
red = map(R,S)
M0 = R^1/ideal(a,b*c)
FR = complete res(M0, LengthLimit=>8)
M=apply(7,i->coker FR.dd_(i+1));
A = chainComplex(apply(length FR-1, i->lift (FR.dd_(i+1),S)))
L = trueKoszul ff
u = higherCIOperators(A,L);

k = 1
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
betti P
betti str'

time (source P)**source str'; -- fast!
time (target P)**target str'; -- fast!
time Pstr' = P**str';--SLOW!: when k=1 it takes 100 sec on my fastest machine.
--MIKE

time pstr = c*(Pstr'); 
time strQ = str**Q;
time strq = c*(strQ);
time str'str = c*(str**str');

assert(str'str ==0)
assert(pstr!=0)
assert(strq !=0)


time test = map(coker(pstr|strq|str'str), source QP, QP);
assert(test !=0)

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

