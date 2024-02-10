------------------------------------------------------------
-- PURPOSE:    compute the polar dual of a rational convex 
--             polyhedral cone using Fourier-Motzkin elimination,
-- PROGRAMMER     : Greg Smith 
-- UPDATE HISTORY : 7 August 2000
-- GLOBAL METHODS : polarCone
-- REFERENCE : G\"unter M. Ziegler, Lectures on Polytopes,
--             Graduate Texts in Mathematics 152, 
--             Springer-Verlag, New York, 1995. 
------------------------------------------------------------
--  primitive  ---------------------------------------------
-- PURPOSE  : divides a list of integers by their gcd.
-- RECEIVES : 'L' a non-empty list of integers. 
-- RETURNS  : a list of integers.
------------------------------------------------------------
primitive = (L) -> (
     n := #L-1;                    g := L#n;
     while n > 0 do (n = n-1;      g = gcd(g, L#n);
	  if g === 1 then n = 0);
     if g === 1 then L else apply(L, i -> i // g));

--  toZZ  --------------------------------------------------
-- PURPOSE  : converts a list of 'QQ' to 'ZZ' by multiplying
--   by a common denominator.
-- RECEIVES : 'L' : a list of 'QQ'. 
-- RETURNS  : a list of integers.
------------------------------------------------------------
toZZ = (L) -> (
     d := apply(L, e -> denominator e);
     R := ring d#0;             l := 1_R;
     scan(d, i -> (l = (l*i // gcd(l,i))));    
     apply(L, e -> (numerator(l*e))));

--  rotateMatrix  ------------------------------------------
-- PURPOSE  : transposition along the antidiagonal?
-- RECEIVES : 'M' a matrix. 
-- RETURNS  : a matrix.
-- COMMENT  : used to compute Gaussian elimination in the 
--   in the form in which I think.  
------------------------------------------------------------
rotateMatrix = (M) -> (
     r := rank source M;        c := rank target M;
     matrix table(r, c, (i,j) -> M_(c-j-1, r-i-1)));

--  isRedundant  -------------------------------------------
-- PURPOSE  : determine if an row vector/inequality is 
--   redundant.
-- RECEIVES : 
--   'V'  a list of sets of integers.  Each entry contains 
--        indices of the original rays which do NOT vanish 
--        at the corresponding row vector.
-- 'vert' a set of integers; the original rays for the row 
--        vector in question.
-- RETURNS  : a boolean.
-- COMMENT  : see Exercise 2.15 (i) in Ziegler.
------------------------------------------------------------
isRedundant = (V, vert) -> (
     -- the row vector is redundant iff 'vert' contains an
     -- entry in 'V'.
     x := 0;            k := 0;
     numRow := #V;      -- equals the number of inequalities
     while x < 1 and k < numRow do (
	  if isSubset(V#k, vert) then x = x+1;
	  k = k+1;);     
     x === 1);

--  fourierMotzkin  ----------------------------------------
-- PURPOSE  : Eliminates the first variable in the 
--   inequalities 'A' using the double description version 
--   of Fourier-Motzkin elimination.
-- RECEIVES : 
--    'A' a list of lists of integers.  Each entry is a 
--        corresponds to a row vector in the system of 
--        inequalities.
--    'V' a list of sets of integers.  Each entry contains 
--        indices of the original rays which do NOT vanish at 
--        the corresponding row vector;  the complement of 
--        the 'V_i" appearing in Exercise 2.15 in Ziegler.
-- 'spot' an integer.  The index of the variable being 
--        eliminated.
-- RETURNS  :  a list {projA, projV} where
-- 'projA' a list of lists of integers.  Each entry is a 
--         corresponds to a row vector in the projected 
--         system of inequalities.
-- 'projV' a list of sets of integers.  Each entry contains 
--         indices of the original rays which do NOT vanish 
--         at the corresponding row vector in 'projA'
------------------------------------------------------------
fourierMotzkin' = (A, V, spot) -> (
     -- initializing local variables
     numRow := #A;               -- equal to the length of V
     numCol := #(A#0);           pos := {};       
     neg := {};                  projA := {};     
     projV := {};                k := 0;
     -- divide the inequalities into three groups.
     while k < numRow do (
	  if A#k#0 < 0 then neg = append(neg, k)
	  else if A#k#0 > 0 then pos = append(pos, k)
	  else (projA = append(projA, A#k);
	       projV = append(projV, V#k););
	  k = k+1;);	  
     -- generate new irredundant inequalities.
     scan(pos, i -> scan(neg, j -> (vert := V#i + V#j;
		    if not isRedundant(projV, vert)  
		    then (iRow := A#i;     jRow := A#j;
			 iCoeff := - jRow#0;
			 jCoeff := iRow#0;
			 a := iCoeff*iRow + jCoeff*jRow;
			 projA = append(projA, a);
			 projV = append(projV, vert););)));
     -- don't forget the implicit inequalities '-t <= 0'.
     scan(pos, i -> (vert := V#i + set{spot};
	  if not isRedundant(projV, vert) then (
	       projA = append(projA, A#i);
	       projV = append(projV, vert););));
     -- remove the first column 
     projA = apply(projA, e -> e_{1..(numCol-1)});
     {projA, projV});   

--  polarCone  ---------------------------------------------
-- PURPOSE : computes the polar cone
------------------------------------------------------------
polarCone = method();

--  polarCone(Matrix, Matrix)  -----------------------------
-- RECEIVES : 
--   'Z' a matrix; the columns are the rays generating the cone.
--   'H' a matrix; the columns are the rays generating the 
--       linear space in the cone.  
-- RETURNS : a sequence (A,E)
--   'A' a matrix; the columns are the rays generating the polar cone.
--   'E' a matrix; the columns are the rays generating the 
--       linear space in the polar cone.
-- COMMENT  :  
--   'cone(Z) + affine(H) = {x : A^t * x <= 0, E^t * x = 0}'
------------------------------------------------------------
polarCone(Matrix, Matrix) := (Z, H) -> (
     R := ring source Z;
     if R =!= ring source H then error ("polarCone: " | 
	  "expected matrices over the same ring");
     if rank target Z =!= rank target H then error (
     	  "polarCone: expected matrices to have the " |
	  "same number of rows");     
     if (R =!= ZZ) then error ("polarCone: expected " | 
	  "matrices over 'ZZ'");
     -- expressing 'cone(Y)+affine(B)' as '{x : Ax <= 0}'
     Y := substitute(Z, QQ);     B := substitute(H, QQ);   
     if rank source B > 0 then Y = Y | B | -B;
     n := rank source Y;         d := rank target Y;     
     A := Y | -id_(QQ^d);
     -- computing the row echelon form of 'A'
     A = gens gb rotateMatrix A;
     L := rotateMatrix leadTerm A;
     A = rotateMatrix A;
     -- find pivots
     numRow = rank target A;                  -- numRow <= d
     i := 0;                     pivotCol := {};
     while i < numRow do (j := 0;
	  while j < n+d and L_(i,j) =!= 1_QQ do j = j+1;
	  pivotCol = append(pivotCol, j);
	  i = i+1;);
     -- computing the row-reduced echelon form of 'A'
     A = ((submatrix(A, pivotCol))^(-1)) * A;
     -- converting 'A' into a list of integer row vectors 
     A = entries A;
     A = apply(A, e -> primitive toZZ e);
     -- creating the vertex list 'V' for double description
     -- and listing the variables 'T' which remain to be
     -- eliminated
     V := {};                    T := toList(0..(n-1));
     scan(pivotCol, e -> (if e < n then (T = delete(e, T);
	       	    V = append(V, set{e});)));
     -- separating inequalities 'A' and equalities 'E'
     eqnRow := {};               ineqnRow := {};
     scan(numRow, i -> (if pivotCol#i >= n then 
	       eqnRow = append(eqnRow, i)
	       else ineqnRow = append(ineqnRow, i);));	  
     E := apply(eqnRow, i -> A#i);
     E = apply(E, e -> e_{n..(n+d-1)});
     A = apply(ineqnRow, i -> A#i);
     A = apply(A, e -> e_(T | toList(n..(n+d-1)))); 
     -- successive projections eliminate the variables 'T'.
     if A =!= {} then scan(T, t -> (
	       D := fourierMotzkin'(A, V, t);
	       A = D#0;          V = D#1;));
     -- output formatting
     A = apply(A, e -> primitive e);
     if A === {} then A = map(ZZ^d, ZZ^0, 0)
     else A = transpose matrix A;
     if E === {} then E = map(ZZ^d, ZZ^0, 0)
     else E = transpose matrix E;
     (A, E)); 

--  polarCone(Matrix)  -------------------------------------
-- RECEIVES : 'Z' a matrix.  The columns are the rays 
--   generating the cone.
-- COMMENT  : calls polarCone(Matrix, Matrix)
------------------------------------------------------------
polarCone(Matrix) := (Z) -> (
     polarCone(Z, map(ZZ^(rank target Z), ZZ^0, 0)));
