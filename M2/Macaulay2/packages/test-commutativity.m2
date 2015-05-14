
needsPackage "CompleteIntersectionResolutions"
needsPackage "HigherCIOperators"

triv = method()
triv(Module, Module) := (M,N) -> (
    --image triv is the submodule of stably trivial homomorphisms
    p :=map(N, cover N,1);
    Hom(M, p))

compose = method()
compose(Module, Module, Module) := (M,N,P)-> (
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
    phi)

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
viewHelp HigherCIOperators
S = ZZ/101[a,b,c]
ff = matrix"a2,b2,c2"
R = S/ideal ff
red = map(R,S)
M0 = R^1/ideal(a,b*c)
FR = complete res(MR, LengthLimit=>8)
M=apply(7,i->coker FR.dd_(i+1));
betti FR
A = chainComplex(apply(length FR-1, i->lift (FR.dd_(i+1),S)))
betti A
L = trueKoszul ff
u = higherCIOperators(A,L);
isHomogeneous red (u#{2,2,1}*u#{2,4,0})

--The square of the ordinary CI operators:
t2 = map(M_0**red L_2, M_4, red(u#{2,2,1}*u#{2,4,0})) 
isStablyTrivial t2
--But we have a subtler test

isStablyTrivial (p4 = map(M_2**red L_1, M_4, red u#{2,4,0}))
isStablyTrivial (p2=map(M_0**red L_2, M_2**red L_1, red u#{2,2,1}))
--the question is whether
-- p2*p4
-- is in the submodule p2*triv + triv*p4 + triv*triv
--compute these one at a time!


