--This file computes an (the?) obstruction to the commutativity of the CI operators
--at a given stage (ie F_k --> F_(k-4)).
--it is nonzero in the case M = R/(a,bc) where R = k[a,b,c]/(a2, b2, c2) and k = 4,5.

needsPackage "CompleteIntersectionResolutions"
needsPackage "HigherCIOperators"

triv = method()
triv(Module, Module) := (M,N) -> (
    --image triv is the submodule of stably trivial homomorphisms
    p :=map(N, cover N,1);
    Hom(M, p))

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
ev = N -> (
    --if N is a free R-module returns the contraction map N**N^* -> R.
    reshape((ring N)^1, N**(dual N), id_N)
    )

{*
compose1 = (M,N,P)-> (
    --defines the map Hom(M,N)**Hom(N,P) -> Hom(M,P)
    S := ring M;
    MN := Hom(M,N);
    NP := Hom(N,P);
    gMN := numgens MN;
    gNP := numgens NP;
    phi := map(Hom(M,P), S^0,0);
    mn:= null;
    np:= null;
    scan(gMN, i->(
	    mn = homomorphism MN_{i}; 
	    scan(gNP, j->(
	    np = homomorphism NP_{j};
	    phi = phi|mapToHomomorphism(np*mn)))));
    map(Hom(M,P), MN**NP, phi)
    )
--time c1 = compose1(B,C,D); -- 2 min for k=4
time c  = compose(B,C,D); -- .06 sec for k=4
--assert(c == c1) -- amazing!
*}


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

k = 5
B = M_k;
C = M_(k-2)**red L_1;
D = M_(k-4)**red L_2;

--form the map Hom(B,D)<--Hom(B,C)**Hom(C,D)
time c = compose(B,C,D);

--now make the CI operators
p = map(C,B, red u#{2,k,0}); --M_k -> M_(k-2)**red L_1
q = map(D,C, red u#{2,k-2,1}); --M_(k-2)**red L_1 -> M_(k-4)**red L_2

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
time strp = c*(P**str');
time qstr = c*(str**Q);
time str'str = c*(str**str');

assert(str'str ==0)
assert(strp!=0)
assert(qstr !=0)

time test = map(coker(strp|qstr|str'str), source QP, QP);
assert(test !=0)
unique flatten entries (q*p)
betti(q*p)
