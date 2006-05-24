newPackage(
	"ConvexHulls",
    	Version => "1.0", 
    	Date => "January 12, 2005",
    	Authors => {{Name => "Greg Smith", Email => "ggsmith@mast.queensu.ca"}},
    	HomePage => "http://www.math.uiuc.edu/Macaulay2/",
    	Headline => "convex hulls and polar cones",
    	DebuggingMode => true
    	)

export(polarCone, convexHull)

------------------------------------------------------------
-- PURPOSE:    compute the polar dual of a rational convex 
--             polyhedral cone using Fourier-Motzkin 
--             elimination,
--
-- PROGRAMMER     : Greg Smith 
-- UPDATE HISTORY : 2 July 2000
-- GLOBAL METHODS : polarCone
-- REFERENCE : G\"unter M. Zielger, Lectures on Polytopes,
--             Graduate Texts in Mathematics 152, 
--             Springer-Verlag, New York, 1995. 
------------------------------------------------------------



--  rotateMatrix  ------------------------------------------
-- PURPOSE  : transposition along the antidiagonal?
-- RECEIVES : 'M' : a matrix. 
-- RETURNS  : a matrix.
-- COMMENT  : used to compute Gaussian elimination in the 
--            in the form in which I think.  
------------------------------------------------------------
rotateMatrix := (M) -> (
     numRow := rank source M;
     numCol := rank target M;
     
     matrix table(numRow, numCol, (i,j) -> 
	  M_(numCol - j - 1, numRow - i - 1))
     );



--  isRedundant  -------------------------------------------
-- PURPOSE  : determine if an row vector/inequality is 
--            redundant.
-- RECEIVES : 'V' : a list of sets of integers.  Each entry 
--                  contains indices of the original rays  
--                  which do NOT vanish at the corresponding  
--                  row vector.
--     'vertices' : a set of integers; the original rays for
--                  the row vector in question.
-- RETURNS  : a boolean.
-- COMMENT  : see Exercise 2.15 (i) in Ziegler.
------------------------------------------------------------
isRedundant := (V, vertices) -> (
     -- the row vector is redundant iff 'vertices' contains
     -- an entry in 'V'.
     x := 0;
     k := 0;
     numRow := #V;  -- equals the number of inequalities
     while ((x < 1) and (k < numRow)) do (
	  if isSubset(V#k, vertices) then x = x + 1;
	  k = k + 1;
	  );
     
     x === 1
     );

     

--  fourierMotzkin  ----------------------------------------
-- PURPOSE  : Eliminates the first variable in the 
--            inequalities 'A' using the double description 
--            version of Fourier-Motzkin elimination.
-- RECEIVES : 'A' : a list of lists of integers.  Each entry 
--                  is a corresponds to a row vector in the  
--                  system of inequalities.
--            'V' : a list of sets of integers.  Each entry 
--                  contains indices of the original rays  
--                  which do NOT vanish at the corresponding 
--                  row vector;  the complement of the 'V_i" 
--                  appearing in Exercise 2.15 in Ziegler.
--         'spot' : an integer.  The index of the variable 
--                  being eliminated.
-- RETURNS  :  a list {projA, projV} where
--        'projA' : a list of lists of integers.  Each entry 
--                  is a corresponds to a row vector in the 
--                  projected system of inequalities.
--        'projV' : a list of sets of integers.  Each entry 
--                  contains indices of the original rays  
--                  which do NOT vanish at the corresponding
--                  row vector in 'projA'
------------------------------------------------------------
fourierMotzkin := (A, V, spot) -> (
     -- initializing local variables
     numCol := #(A#0);
     numRow := #A;  -- equal to the length of V
     pos := {};
     neg := {};
     projA := {};
     projV := {};

     -- divide the inequalities into three groups.
     k := 0;
     while (k < numRow) do (
	  if (A#k#0 < 0) then neg = append(neg, k)
	  else if (A#k#0 > 0) then pos = append(pos, k)
	  else (
	       projA = append(projA, A#k);
	       projV = append(projV, V#k);
	       );
	  k = k+1;
	  );	  
       
     -- generate new irredundant inequalities.
     scan(pos, i -> 
	  scan(neg, j -> (
		    vertices := V#i + V#j;
		    if not isRedundant(projV, vertices) 
		    then (
			 iRow := A#i;
			 jRow := A#j;
			 iCoeff := - jRow#0;
			 jCoeff := iRow#0;
			 a := iCoeff*iRow + jCoeff*jRow;
			 projA = append(projA, a);
			 projV = append(projV, vertices);
			 );
		    )));
     -- don't forget the implicit inequalities '-t <= 0'.
     scan(pos, i -> (
	  vertices := V#i + set{spot};
	  if not isRedundant(projV, vertices) 
	  then (
	       projA = append(projA, A#i);
	       projV = append(projV, vertices);
	       );
	  ));

     -- remove the first column 
     projA = apply(projA, e -> e_{1..(numCol-1)});
          
     {projA, projV}
     );   



--  primitive  ---------------------------------------------
-- PURPOSE  : divides a list of integers by their gcd.
-- RECEIVES : 'L' : a non-empty list of integers. 
-- RETURNS  : a list of integers.
------------------------------------------------------------
primitive := (L) -> (
     -- checking of input errors
     if (class L#0 =!= ZZ) then 
     error "primitive: expected non-empty list of integers";
     
     -- finding greatest common divisor
     n := #L-1;
     g := abs(L#n);
     while (n > 0) do (
	  n = n-1;
	  g = gcd(g, L#n);
	  if g === 1 then n = 0
	  );
     
     if g === 1 then L 
     else apply(L, i -> i // g)
     );



--  lcm  ---------------------------------------------------
-- PURPOSE  : determines the least common multiple of a list
--            of integers.
-- RECEIVES : 'L' : a list of integers. 
-- RETURNS  : an integers.
------------------------------------------------------------
lcm := (L) -> (
     R := ring L#0;
     l := 1_R;
     scan(L, i -> (l = ((l*i) // (gcd(l,i))) ));
     
     l
     );



--  toZZ  --------------------------------------------------
-- PURPOSE  : converts a list of 'QQ' to 'ZZ' by multiplying
--            by a common denominator.
-- RECEIVES : 'L' : a list of 'QQ'. 
-- RETURNS  : a list of integers.
------------------------------------------------------------
-- converts a list of QQ to ZZ by multiplying by a common 
-- denominator.
toZZ := (L) -> (
     -- checking for input errors
     if (class L#0 =!= QQ) then
     error "toZZ: expected non-empty list of 'QQ'";
     
     -- finding common denominator
     d := apply(L, e -> denominator e);
     l := lcm d;
     
     apply(L, e -> (numerator(l*e)))
     );


--  polarCone  ---------------------------------------------
-- PURPOSE : computes the polar cone
------------------------------------------------------------
polarCone = method();



--  polarCone(Matrix, Matrix)  -----------------------------
-- RECEIVES : 'Z' : a matrix.  The columns are the rays 
--                  generating the cone.
--	      'H' : a matrix.  The columns are the rays 
--                  generaing the linear space in the cone.  
-- RETURNS  : a sequence (A,E)
--            'A' : a matrix.  The columns are the rays
--                  generating the polar cone.
--            'E' : a matrix.  The columns are the rays
--                  generating the linear space in the polar 
--                  cone.
-- COMMENT  :  
--   'cone(Z) + affine(H) = {x : A^t * x <= 0, E^t * x = 0}'
------------------------------------------------------------
polarCone(Matrix, Matrix) := (Z, H) -> (
     -- checking for input errors
     R := ring source Z;
     if (R =!= ring source H) then
     error ("polarCone: " | 
	  "expected matrices over the same ring");
     if (rank target Z =!= rank target H) then
     error ("polarCone: expected matrices to have the " |
	  "same number of rows");
     
     -- making 'QQ' versions of the input
     local Y;
     local B;
     outputZZ := false; 
     if (R === ZZ) then (
	  outputZZ = true;
	  Y = substitute(Z, QQ);
	  B = substitute(H, QQ);
	  )
     else if (R === QQ) then (
	  Y = Z;
	  B = H;
	  )
     else error ("polarCone: " | 
	  "expected a matrix over 'ZZ' or 'QQ'");
     
     -- expressing 'cone(Y)+affine(B)' in the form 
     -- {x : Ax <= 0}
     d := rank target Y;
     if (rank source B > 0) then Y = Y | B | -B;
     n := rank source Y;
     A := Y | -id_(QQ^d);
     
     -- computing the row echelon form of 'A'
     A = gens gb rotateMatrix A;
     L := rotateMatrix leadTerm A;
     A = rotateMatrix A;
     
     -- find pivots
     numRow = rank target A;     -- numRow <= d
     i := 0;
     pivotCol := {};
     while (i < numRow) do (
	  j := 0;
	  while ((j < n+d) and (L_(i,j) =!= 1_QQ)) do (
	       j = j+1;
	       );
	  pivotCol = append(pivotCol, j);
	  i = i+1;
	  );
     
     -- computing the row-reduced echelon form of 'A'
     A = ((submatrix(A, pivotCol))^(-1)) * A;
     
     -- converting 'A' into a list of integer row vectors 
     A = entries A;
     A = apply(A, e -> primitive toZZ e);

     -- creating the vertex list 'V' for double description
     -- and listing the variables 'T' which remain to be
     --  eliminated
     V := {};
     T := toList(0..(n-1));
     scan(pivotCol, e -> (
	       if (e < n) then (
	       	    T = delete(e, T);
	       	    V = append(V, set{e});
		    )));

     -- separating inequalities 'A' and equalities 'E'
     eqnRow := {};
     ineqnRow := {};
     scan(numRow, i -> (
	       if (pivotCol#i >= n) then 
	       eqnRow = append(eqnRow, i)
	       else
	       ineqnRow = append(ineqnRow, i);	    
	       ));	  
     E := apply(eqnRow, i -> A#i);
     E = apply(E, e -> e_{n..(n+d-1)});
     A = apply(ineqnRow, i -> A#i);
     A = apply(A, e -> e_(T | toList(n..(n+d-1)))); 
    
     -- successive projections eliminate the remaining 
     -- variables 'T'.
     if (A =!= {}) then
     scan(T, t -> (
	       D := fourierMotzkin(A, V, t);
	       A = D#0;
	       V = D#1;
	       ));
     
     -- output formating
     A = apply(A, e -> primitive e);
     if (A === {}) then A = map(ZZ^d, ZZ^0, 0)
     else A = transpose matrix A;
     if (E === {}) then E = map(ZZ^d, ZZ^0, 0)
     else E = transpose matrix E;
     if (outputZZ === false) then (
	  A = substitute(A, QQ); 
	  E = substitute(E, QQ);
	  );
     
     (A, E)
     ); 



--  polarCone(Matrix)  -------------------------------------
-- RECEIVES : 'Z' : a matrix.  The columns are the rays 
--                  generating the cone.
-- COMMENT  : calls polarCone(Matrix, Matrix)
------------------------------------------------------------
polarCone(Matrix) := (Z) -> (
     -- creating zero equalities
     R := ring target Z;
     d := rank target Z;
     H := map(R^d, R^0, 0);
     
     polarCone(Z,H)
     );

convexHull = method()

convexHull(Matrix) := (Z) -> (
     R := ring target Z;
     Z = addRowOfOnes Z;
     polarCone polarCone(Z)
     )

--------------
-- Examples --
--------------

addRowOfOnes = (m) -> (
    n := numgens source m;
	m2 := map(ZZ^1, ZZ^n, {toList(n:1)});
	m2 || m)

addColumns = (m) -> (
    n := numgens source m;
	R := ring m;
	ones := map(R^n, R^1, (i,j) -> 1);
	m * ones)
	
--permutations = (n) -> (
--    if #n === 1 then {n}
--	else (
--	    flatten apply(#n, i -> (
--	        m := drop(n,{i,i});
--			apply(permutations m, p -> prepend(n#i,p))))))
		
cyclicPolytope = (d,n) -> map(ZZ^(d+1), ZZ^n, (i,j) -> j^i)

permutahedron = (n) -> (
    addRowOfOnes transpose matrix permutations toList(1..n))

hypersimplex = (d,k) -> (
    x := subsets(d+1,k);
	addRowOfOnes transpose matrix apply(x, s -> (
	    x = set s;
	    apply(d+1, i -> if member(i,x) then 1 else 0))))

beginDocumentation()

document { 
	Key => ConvexHulls,
	Headline => "A Macaulay2 package for convex hulls and polar cones",
	EM "ConvexHulls", " is a package which computes convex hulls of rational polytopes
	and rational polar cones (supporting hyperplanes) of polyhedral cones."
	}

document {
     Key => polarCone,
     Headline => "find the polar dual of a rational convex polyhedral cone",
     Usage => {
	  TT "polarCone(A, B)", " -- find the polar cone", BR{},
	  "(A', B') = polarCone(A, B)"
	  },
     Inputs => {
	  "A" => {"a ", TT "d", " by ", TT "n", " matrix over ", TO "ZZ", " or ", TO "QQ", "."},
	  "B" => {"a ", TT "d", " by ", TT "r", " matrix over ", TO "ZZ", " or ", TO "QQ", "."}
	  },
     Outputs => {
	  "A'" => { "a matrix having ", TT "d", " rows" },
	  "B'" => { "a matrix having ", TT "d", " rows" }
	  },
     PARA {
	  "The input pair ", TT "(A,B)", "gives a cone", TT "C", 
	  "generated by the column vectors of ", TT "A", " and
	  containing the lineality space generated by the column 
	  vectors of ", TT "B", ".  Dually, ", TT "C", " the set 
	  of vectors ", TT "x", "satisfying ", TT "(transpose A) * x <= 0", " and ", TT "(transpose B) * x == 0", "."},
     PARA {
     	  "The polar cone ", TT "(A',B')", " is, by definition, the 
     	  set of vectors ", TT "z", " satisfying ", TT "(transpose z) * x <= 0", " for all ", TT "x", " in ", TT "C", "."},
     PARA {
     	  "The output", TT "(A', B')", " is valid input and hence 
     	  describes a cone in same manner that ", TT "(A, B)", " does."}, 
     PARA { "Three sample calculations using", TT "polarCone", " are given below."},
     UL {
	  {
	       "The cone over the cube is dual to the cone over the octahedron.",
	       EXAMPLE {
		    "octahedron = transpose matrix{
	       { 1,  1,  1, -1},
	       {-1,  1,  1, -1},
	       { 1, -1,  1, -1},
	       { 1,  1, -1, -1},
	       {-1, -1,  1, -1},
	       {-1,  1, -1, -1},
	       { 1, -1, -1, -1},
	       {-1, -1, -1, -1}}",
		    "cube = polarCone octahedron",
		    "polarCone cube"
		    }
	       },
	  {
	       "Finding minimal generators for a rational convex polyhedral cone.  Consider the cone generated by ", 
	       TT "rays", ".",  
	       EXAMPLE {
		    "rays = transpose matrix(QQ, {
		    {-1,  2, 0},
		    { 0,  1, 3},
		    { 0, -1, 3},
		    { 1, -2, 3},
		    { 2, -4, 0}})",
		    "polarCone polarCone rays"
		    },
	       "Two applications of ", TT "polarCone", " indicate that 
	       this cone is the sum of a pointed polyhedral cone 
	       generated by two rays and a 1-dimensional lineality 
	       space."
	       },
	  {
	       "Finding the vertices of a cone presented as an 
	       intersection of halfspaces.  The 4-dimenional 
	       associahedron is the set of all vectors ", TT "x", 
	       " satisfying ", TT "(transpose A) * x <= b", " and ", 
	       TT "(transpose B) * x == c", " where",
	       EXAMPLE {
		    "A = matrix{
	       {-1,  0,  0, 0},
	       { 0, -1,  0, 0},
	       { 0,  0, -1, 0},
	       {-2, -2,  0, 0},
	       { 0, -2, -2, 0}}",
		    "b = transpose matrix{{1, 1, 1, 10, 10}}",
		    "B = matrix{
	       {1, 2, 3,  4},
	       {1, 4, 9, 16}}",
		    "c = transpose matrix{{-60, -194}}",
		    "P = polarCone(transpose(A | -b), transpose(B | -c))",
		    "vertices = submatrix(P#0, {0..3},)"
		    },
	       NOINDENT{},
	       "By homogenizing, computing the polar cone and 
	       dehomogenizing, we find the five vertices of this 
	       cone."
	       }
	  },
     PARA {
	  "The polar cone is computed using the Fourier-Motzkin 
	  elimination algorithm described in G.M. Ziegler, 
	  Lectures on Polytopes, Graduate Texts in Mathematics 
	  152, Springer-Verlag, 1995."},
     PARA "Implemented by Greg Smith"
     }


TEST ///
C = transpose matrix{{1,1,1,1}};
assert(C == (polarCone polarCone C)#0)

C = transpose matrix(QQ, {{0,0,1}, {1,0,1}, {0,1,1}});
assert( (entries transpose C) == 
     (entries transpose ((polarCone polarCone C)#0)) )

C = map(ZZ^3,ZZ^0,0);
H = transpose matrix{{1,0,-1},{0,1,-1}};
P = polarCone (C,H);
assert(P#0 == C)
assert(P#1 == transpose matrix{{1,1,1}})

C = transpose matrix{{1,1,0}, {0,1,1}};
H = transpose matrix{{1,0,-1}};
P = polarCone polarCone (C,H);
assert(P#0 == transpose matrix{{0,1,1}})
assert(P#1 == H)

///

end
installPackage "ConvexHulls"
uninstallPackage "ConvexHulls"

octahedron = transpose matrix{
	       { 1,  1,  1, -1},
	       {-1,  1,  1, -1},
	       { 1, -1,  1, -1},
	       { 1,  1, -1, -1},
	       {-1, -1,  1, -1},
	       {-1,  1, -1, -1},
	       { 1, -1, -1, -1},
	       {-1, -1, -1, -1}}
cube = polarCone octahedron
polarCone cube
rays = transpose matrix(QQ, {
		    {-1,  2, 0},
		    { 0,  1, 3},
		    { 0, -1, 3},
		    { 1, -2, 3},
		    { 2, -4, 0}})
polarCone polarCone rays
A = matrix{
	       {-1,  0,  0, 0},
	       { 0, -1,  0, 0},
	       { 0,  0, -1, 0},
	       {-2, -2,  0, 0},
	       { 0, -2, -2, 0}}
b = transpose matrix{{1, 1, 1, 10, 10}}
B = matrix{
	       {1, 2, 3,  4},
	       {1, 4, 9, 16}}
c = transpose matrix{{-60, -194}}
P = polarCone(transpose(A | -b), transpose(B | -c))
vertices = submatrix(P#0, {0..3},)

------------------------------------------------------------
-- BENCHMARK EXAMPLES
------------------------------------------------------------

-- symmetric traveling salesman polytope with n=5
stsp5 = matrix{
     {0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1}, 
     {1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0}, 
     {0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0}, 
     {1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1}, 
     {1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1}, 
     {1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0}, 
     {0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0}, 
     {0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1}, 
     {0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0}, 
     {1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1}, 
     {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}}
P = time polarCone stsp5
P = time polarCone P
C = entries transpose stsp5;
scan(entries transpose P#0, e -> (C = delete(e, C)))



-- symmetric traveling salesman polytope with n=6
stsp6 = matrix{
     {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
	  1,1,1,0,1,1,1,1,0,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,1,
	  1,1,1,0,1,1,1,1},
     {1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,1,1,
	  1,1,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	  0,0,0,0,0,0,0,0},
     {0,0,0,0,0,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,0,0,0,0,0,0,
	  0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,1,1,1,0,1,1,
	  1,1,0,0,0,0,0,0},
     {0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0,
	  0,0,0,0,0,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0,0,0,0,0,0,
	  0,0,0,1,1,1,1,0},
     {1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,
	  0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,
	  0,0,1,1,0,0,0,1},
     {1,1,1,0,1,1,1,1,0,1,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,0,0,
	  0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,1,0,
	  1,1,1,1,0,1,1,1},
     {1,1,0,1,1,0,0,0,0,0,1,0,1,1,1,1,0,1,1,1,1,1,0,1,1,0,0,
	  0,0,0,1,0,1,1,1,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
	  0,0,0,0,0,0,0,0},
     {0,0,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,0,0,0,1,0,1,1,1,1,0,
	  1,1,1,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,1,0,1,1,1,0,0,
	  0,0,0,0,0,0,0,0},
     {0,0,1,1,0,0,0,1,1,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,1,1,
	  0,0,0,1,1,0,0,0,1,1,0,0,0,0,1,1,0,0,1,1,0,0,0,1,1,
	  0,0,0,1,1,0,0,0},
     {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,1,1,
	  1,0,1,0,0,0,0,0,1,1,0,1,1,1,1,1,0,1,1,1,1,0,1,0,0,
	  0,0,0,1,1,0,1,1},
     {0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,1,1,1,0,1,0,0,0,0,0,0,0,
	  0,0,0,1,1,1,0,1,1,1,1,0,1,0,0,0,0,0,1,1,0,1,1,1,1,
	  0,1,1,0,0,0,0,0},
     {0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,1,1,0,0,0,0,1,1,0,0,
	  0,1,1,0,0,0,1,1,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,1,
	  1,0,0,0,1,1,0,0},
     {1,0,1,1,1,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
	  0,1,1,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
	  1,0,1,1,1,1,0,1},
     {0,1,1,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,
	  1,1,0,0,1,1,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,1,1,0,0,
	  0,1,1,0,0,1,1,0},
     {1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,
	  1,0,0,0,0,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,0,0,0,0,
	  1,1,0,0,0,0,1,1},
     {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	  1,1,1,1,1,1,1,1}}
P = time polarCone stsp6
P = time polarCone P
C = entries transpose stsp6;
scan(entries transpose P#0, e -> (C = delete(e, C)));
assert(C == {})

------------------------------------------------------------
needsPackage "ConvexHulls"
m = matrix{{1,2,0},{2,1,-3}}
convexHull m
