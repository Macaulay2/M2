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
     bd,
     faces,maxfaces,nonfaces,support)

SimplicialComplex = new Type of MutableHashTable

simplicialComplex = method()
simplicialComplex Ring := (R) -> (
     S := new SimplicialComplex;
     S.ring = ring presentation R;
     S.facering = R;
     n := numgens R;
     S.qring = R/ideal map(R^1, n, (i,j) -> R_j^2);
     S.faces = new MutableHashTable;
     S)

simplicialComplex List := (faces) -> (
     n := max apply(faces, m -> max m);
     A := ZZ/101[Variables=>n+1];
     I := matrix{apply(faces, m -> product(m, i -> A_i))};
     J := dual1 dual2 dual1 I;  -- minimal non-faces
     simplicialComplex ((ring J)/(ideal J)))

dim SimplicialComplex := (D) -> dim D.facering - 1

support = (m) -> (
     n := numgens ring m;
     m = leadMonomial m;
     m = new List from m;
     m1 = select(m, k -> k#0 < n);
     apply(m1, k -> k#0))

faces = method()
faces (ZZ, SimplicialComplex) := (r,D) -> (
     if not D.faces#?r then (
         A := D.qring;
	 B := ring presentation A;
         D.faces#r = lift(matrix basis(r+1,A), B));
     D.faces#r)

bd = method()
bd (ZZ,SimplicialComplex) := (r,D) -> (
     b1 := faces(r,D);
     b2 := faces(r-1,D);
     ones := map(ZZ,ring b1, toList(numgens ring b1:1));
     sendgg(ggPush b2, ggPush b1, ggkoszul);
     m := getMatrix ring b1;
     ones m)

bd (Ring,ZZ,SimplicialComplex) := (R,r,D) -> (
     b1 := faces(r,D);
     b2 := faces(r-1,D);
     ones := map(R,ring b1, toList(numgens ring b1:1_R));
     sendgg(ggPush b2, ggPush b1, ggkoszul);
     m := getMatrix ring b1;
     ones m)

bd SimplicialComplex := (D) -> (
     b := bd(dim D,D);
     ones := map(source b, ZZ^1, (i,j) -> 1);
     c := b * ones;
     x := positions(0..numgens target c-1, i -> c_(i,0) % 2 =!= 0);
     d := dual1 dual2 dual1 (faces(dim D-1,D))_x;
     simplicialComplex ((ring d)/(ideal d)))

chainComplex SimplicialComplex := (D) -> (
     d := dim D;
     chainComplex apply(0..d, r -> bd(r,D)))

chainComplex (Ring,SimplicialComplex) := (R,D) -> (
     d := dim D;
     chainComplex apply(0..d, r -> bd(R,r,D)))
    
maxfaces = method()
maxfaces SimplicialComplex := (D) -> dual2 nonfaces D
     
nonfaces = method()
nonfaces SimplicialComplex := (D) ->
     presentation D.facering

dual1 = (m) -> (
     A := ring m;
     F := matrix {{product(numgens A, i -> A_i)}};
     contract(m,F))

dual2 = (m) -> (
     A := ring m;
     I := ideal(m) + ideal map(A^1, numgens A, (i,j)->A_j^2);
     B := A/I;
     lift(syz transpose vars B, A))

dual SimplicialComplex := (D) -> (
     -- MES: this needs to be redone, using dual1, dual2
     -- MES: this is probably not right: what is the right formula?
     simplicialComplex dual2 dual1 nonfaces D)
     
net SimplicialComplex := (D) -> 
     net presentation D.facering


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
     
TEST ///
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
