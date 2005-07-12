-- Code for Simplicial Complexes
newPackage(
	"SimplicialComplexes",
    	Version => "1.0", 
    	Date => "May 4, 2005",
    	Author => "Sorin Popescu",
	Email => "sorin@math.sunysb.edu",
    	HomePage => "http://www.math.sunysb.edu/~sorin/",
    	Headline => "simplicial complexes",
    	DebuggingMode => true
    	)

export(SimplicialComplex,
     simplicialComplex,
     bd,fVector,
     faces,facets,support)

complement := local complement
complement = (m) -> (
     A := ring m;
     F := matrix{{product gens A}};
     contract(m,F))

debug Macaulay2Core; -- for rawKoszulMonomials

SimplicialComplex = new Type of HashTable

SimplicialComplex.synonym = "simplicial complex"

SimplicialComplex.AfterPrint = Delta -> (
     << endl;
     << concatenate(interpreterDepth:"o") << lineNumber << " : Facets of the simplicial complex "
     << endl;)

net SimplicialComplex := Delta -> Delta.facets

simplicialComplex = method(TypicalValue => SimplicialComplex)

newSimplicialComplex := (I,F) ->
     new SimplicialComplex from {
	  symbol ring => ring I,
	  symbol faceIdeal => I,
	  symbol facets => F,
	  symbol cache => new CacheTable
	  }
     
simplicialComplex MonomialIdeal := (I) -> (
     R := ring I;
     if not isPolynomialRing R or isQuotientRing R
     then error "expected a polynomial ring";
     if not isSquareFree I then
         error "expected squarefree monomial ideal";
     newSimplicialComplex(I, complement generators dual I)
     )

simplicialComplex List := SimplicialComplex => (faces) -> (
     if #faces === 0 then error "expected at least one facet";
     R := ring faces#0;
     if not isPolynomialRing R or isQuotientRing R
     then error "expected a polynomial ring";
     I := matrix {faces};
     L := monomialIdeal complement I;
     J := dual L;
     newSimplicialComplex(J, complement generators L)
     )

dual SimplicialComplex := (D) -> (
     newSimplicialComplex(monomialIdeal complement D.facets,
	  complement generators D.faceIdeal)
     )

facets = method()
facets SimplicialComplex := (D) -> D.facets

ideal SimplicialComplex := (D) -> ideal D.faceIdeal
monomialIdeal SimplicialComplex := (D) -> D.faceIdeal

SimplicialComplex == SimplicialComplex := (D,E) -> D.faceIdeal === E.faceIdeal

--simplicialComplex Ring := SimplicialComplex => (R) -> (
--     S := new SimplicialComplex;
--     S.ring = ring presentation R;
--     S.facering = R;
--     n := numgens R;
--     S.qring = R/ideal map(R^1, n, (i,j) -> R_j^2);
--     S.faces = new MutableHashTable;
--     S)
--simplicialComplex List := SimplicialComplex => (faces) -> (
--     n := max apply(faces, m -> max m);
--     A := ZZ/101[Variables=>n+1];
--     I := matrix{apply(faces, m -> product(m, i -> A_i))};
--     J := complement dual2 complement I;  -- minimal non-faces
--     simplicialComplex ((ring J)/(ideal J)))

support = (m) -> (
     x := rawIndices raw m;
     apply(x, i -> (ring m)_i))

--support = (m) -> (
--     n := numgens ring m;
--     m = leadMonomial m;
--     m = new List from m;
--     m1 = select(m, k -> k#0 < n);
--     apply(m1, k -> k#0))

--dim SimplicialComplex := (D) -> dim D.faceideal - 1

dim SimplicialComplex := (D) -> max apply(first entries D.facets, s -> # support(s)) - 1

faces = method()
faces (ZZ, SimplicialComplex) := (r,D) -> (
     R := ring D;
     if not D.cache.?faces then (
         D.cache.faces = new MutableHashTable;
	 D.cache.faces.qring = R/(D.faceIdeal + ideal apply(gens R, x -> x^2));
	 );
     if r < -1 or r > dim D then matrix(R, {{}})
     else (
	  if not D.cache.faces#?r then (
               A := D.cache.faces.qring;
               D.cache.faces#r = lift(matrix basis(r+1,A), R));
     	  D.cache.faces#r
     ))

bd = method()
bd (ZZ,SimplicialComplex) := (r,D) -> (
     R := ring D;
     b1 := faces(r,D);
     b2 := faces(r-1,D);
     ones := map(coefficientRing R,R, toList(numgens R:1));
     ones map(R, rawKoszulMonomials(raw b2,raw b1))
     )

chainComplex SimplicialComplex := (D) -> (
     d := dim D;
     C := if d < -1 then (ring D)^0[-1]
          else if d === -1 then (ring D)^1
          else chainComplex apply(0..d, r -> bd(r,D));
     C[1]
     )

homology(ZZ,SimplicialComplex,Ring) := Module => opts -> (i,Delta,R) -> (
     homology(i, chainComplex Delta ** R))
homology(ZZ,SimplicialComplex) := Module => opts -> (i,Delta) -> (
     homology(i, chainComplex Delta))
homology(Nothing,SimplicialComplex,Ring) :=
homology(SimplicialComplex,Ring) := Chaincomplex => opts -> (Delta,R) -> (
     homology(chainComplex Delta ** R))
homology(Nothing,SimplicialComplex) :=
homology(SimplicialComplex) := Chaincomplex => opts -> Delta -> (
     homology(chainComplex Delta))

fVector = method(TypicalValue => List)
fVector SimplicialComplex := List => D -> (
     N := poincare cokernel generators ideal D;
     d := dim D + 1;
     t := first gens ring N;
     while 0 == substitute(N, t => 1) do N = N // (1-t);
     h := apply(reverse toList(0..d), i -> coefficient(t^i,N));
     f := j -> sum(0..j+1, i -> binomial(d-i, j+1-i)*h#(d-i));
     apply(toList(0..d-1), j -> f(j)));

--bd (Ring,ZZ,SimplicialComplex) := (R,r,D) -> (
--     b1 := faces(r,D);
--     b2 := faces(r-1,D);
--     ones := map(R,ring b1, toList(numgens ring b1:1_R));
--     m := map(ring b1, rawKoszulMonomials(raw b2,raw b1));
--     --sendgg(ggPush b2, ggPush b1, ggkoszul);
--     --m := getMatrix ring b1;
--     ones m)

bd SimplicialComplex := (D) -> (
     b := bd(dim D,D);
     ones := map(source b, ZZ^1, (i,j) -> 1);
     c := b * ones;
     x := positions(0..numgens target c-1, i -> c_(i,0) % 2 =!= 0);
     d := complement dual2 complement (faces(dim D-1,D))_x;
     simplicialComplex ((ring d)/(ideal d)))


--chainComplex (Ring,SimplicialComplex) := (R,D) -> (
--     d := dim D;
--     chainComplex apply(0..d, r -> bd(R,r,D)))

-- NOT NEEDED:    
--nonfaces = method()
--nonfaces SimplicialComplex := (D) ->
--     presentation D.facering


--dual2 = (m) -> (
--     A := ring m;
--     I := ideal(m) + ideal map(A^1, numgens A, (i,j)->A_j^2);
--     B := A/I;
--     lift(syz transpose vars B, A))

     
--net SimplicialComplex := (D) -> 
--     net presentation D.facering


beginDocumentation()
document {  Key => SimplicialComplex,
     TT "SimplicialComplex", " -- the class of simplical complexes",
     BR,NOINDENT,
     "A simplicial complex is represented as a hash table, with 
     stashed values.  Typically a simplicial complex is manipulated
     via its face ring: the ideal of non-faces.",
     PARA,
     "Create a new simplicial complex using ", TT "simplicialComplex",
     ".",
     "  Operations which produce information from a simplicial complex D:",
     MENU {
	  (TO "chainComplex", "(D) -- the chain complex of D"),
	  (TO "bd", "(r,D) -- the boundary map from r faces to r-1 faces"),
	  (TO "dim", "(D) -- the dimension of D"),
	  (TO "bd", "(D) -- the boundary simplicial complex of D"),
	  (TO "dual", "(D) -- the dual simplicial complex"),
	  (TO "faces", "(r,D) -- a matrix of squarefree monomials corresponding to 
	       the faces of dimension r of D"),
	  (TO "facets", "(D) -- return the matrix of maximal faces"),
	  (TO "nonfaces", "(D) -- return the matrix of minimal non-faces")
	  },
     PARA,
     "Some support routines",
     MENU {
	  (TO "support", "(m) -- the support of the monomial m, as a list of integers")
	  }
     }
document {
     Key => {(dim, SimplicialComplex)},
     Headline => "dimension of a simplicial complex",
     Usage => "dim X",
     Inputs => {"X" => ""
	  },
     Outputs => {ZZ => ""
	  },
     "Computes the dimension of the given simplicial complex.",
     EXAMPLE {
	  "D = simplicialComplex {{0,1,2},{0,2,3},{0,3,4},{0,1,4}}",
          "dim D"
	  },
     SeeAlso => {(dim,MonomialIdeal)}
     }
     
TEST ///
restart
loadPackage "SimplicialComplexes"

R = ZZ[x]
D3 = simplicialComplex monomialIdeal(1_R)
dim D3
faces(0,D3)
faces(-1,D3)
dual D3
C = chainComplex D3
assert(HH_0(D3) == 0)
assert(HH_-1(D3) == 0)
fVector D3

R = ZZ[x]
D4 = simplicialComplex monomialIdeal gens R
dim D4
faces(0,D4)
faces(-1,D4)
dual D4
C = chainComplex D4
assert(HH_0(D4) == 0)
assert(HH_-1(D4) == R^1)

D5 = simplicialComplex {1_R}
D5 == D4

x = symbol x
kk = ZZ
R = kk[x_1..x_4]
D6 = simplicialComplex monomialIdeal gens R
time A6 = dual D6
time C = chainComplex A6;
C
time prune HH(C)

D7 = simplicialComplex monomialIdeal 1_R
dual D7

-- examples
-----------------------------------------
-- Miller and Sturmfels, example 1.8 ----
-----------------------------------------
R = kk[a..e]
D = simplicialComplex monomialIdeal(a*d, a*e, b*c*d, d*e, c*e, b*e)
fVector D
ideal dual D = monomialIdeal (a*b*c*d, a*b*e, a*c*e, d*e)
-----------------------------------------
-- torus  : Munkres page 15 example 3 ---
-----------------------------------------
kk = QQ
R = kk[a..j]
D = simplicialComplex{a*b*i, a*e*i, i*b*j, j*c*b, j*c*a, j*a*e,
     e*i*f, i*h*f, i*h*j, j*e*d, j*g*d, j*h*g, g*h*f, f*e*d,
     d*f*a, f*b*a, f*g*c, f*b*c, g*c*a, g*d*a}
C = chainComplex D
prune HH(C)
D' = dual D
C' = chainComplex D'
prune HH(C')
fVector D
----------------------------------------------
-- Klein bottle : Munkres page 18 example 5 --
----------------------------------------------
kk = ZZ/2
R = kk[a..j]
D = simplicialComplex {a*b*i, a*e*i, b*i*j, b*c*j, a*c*j, 
     a*d*j, e*f*i, f*h*i, h*i*j, d*e*j, e*g*j, g*h*j, 
     f*g*h, d*e*f, a*d*f, a*b*f, c*f*g, b*c*f, a*c*g, a*e*g}
C = chainComplex D
prune HH(C)
fVector D
---------------------------
-- Real Projective Plane --
---------------------------
kk = ZZ
R = kk[a..f]
D = simplicialComplex monomialIdeal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f,b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
C = chainComplex D
prune HH(C)
fVector D
----------------------------------------
-- Degenerations of Abelian surfaces ---
-- Gross and Popescu, math.AG/9609001 --
----------------------------------------
-- n >= 13
kk = QQ
abelian = (n) -> (
     R := kk[x_0..x_(n-1)];
     L1 = toList apply(0..n-1, i -> x_i * x_((i+3)%n) * x_((i+4)%n));
     L2 = toList apply(0..n-1, i -> x_i * x_((i+1)%n) * x_((i+4)%n));
    join(L1,L2))

D = simplicialComplex abelian 8
numgens source faces(0,D)
numgens source faces(1,D)
numgens source faces(2,D)
numgens source faces(3,D)
C = chainComplex D
prune HH(C)
transpose gens ideal D     
fVector D

-- testing the chain complexes
R = ZZ/101[a..e]
D = simplicialComplex monomialIdeal product gens R
bd(0,D)
bd(1,D)
bd(2,D)
bd(3,D)
bd(4,D)
C = chainComplex D
HH_3(C)
HH_2(C)
prune oo

R = ZZ/101[a..h]
I = monomialIdeal(a*b*c*d,e*f*g*h)
D = simplicialComplex I
E = simplicialComplex{a*b*c*d, e*f*g*h}
dual D
dual E
faces(2,D)
faces(3,D)
faces(4,D)
faces(5,D)
faces(6,D)
faces(7,D)
faces(-1,D)
faces(-2,D)
faces(0,D)

simplicialComplex {}

R = ZZ/101[x_0 .. x_3]
A = R/ideal(x_0 * x_1 * x_2, x_1 * x_2 * x_3)
D = simplicialComplex A
assert(A === ring D)
maxfaces D
dual D
faces(0,D)
chainComplex D
dual D

-- Example 1: boundary of a tetrahedron
D = simplicialComplex {{0,1,2},{0,1,3},{0,2,3},{1,2,3}}
maxfaces D
dim D
nonfaces D
chainComplex D
bd(2,D)

-- Example2: trivverts
D = simplicialComplex {{0,1,2},{0,2,3},{0,3,4},{0,1,4}}
dim D
maxfaces D
nonfaces D
chainComplex D
bd(2,D)

///
