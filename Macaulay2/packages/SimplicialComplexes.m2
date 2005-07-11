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
     bd,dual2,
     faces,maxfaces,facets,nonfaces,support)

complement := local complement
complement = (m) -> (
     A := ring m;
     F := matrix {{product(numgens A, i -> A_i)}};
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
     if not isSquareFree I then
         error "expected squarefree monomial ideal";
     newSimplicialComplex(I, complement generators dual I)
     )

simplicialComplex List := SimplicialComplex => (faces) -> (
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

ideal SimplicialComplex := (D) -> D.faceIdeal

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
     if not D.cache.faces#?r then (
         A := D.cache.faces.qring;
         D.cache.faces#r = lift(matrix basis(r+1,A), R));
     D.cache.faces#r)

bd = method()
bd (ZZ,SimplicialComplex) := (r,D) -> (
     b1 := faces(r,D);
     b2 := faces(r-1,D);
     ones := map(ZZ,ring b1, toList(numgens ring b1:1));
     m := map(ring b1, rawKoszulMonomials(raw b2,raw b1));
     --sendgg(ggPush b2, ggPush b1, ggkoszul);
     --m := getMatrix ring b1;
     ones m)

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

chainComplex SimplicialComplex := (D) -> (
     d := dim D;
     chainComplex apply(0..d, r -> bd(r,D)))

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
	  (TO "maxfaces", "(D) -- return the matrix of maximal faces"),
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
