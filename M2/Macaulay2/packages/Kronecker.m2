newPackage(
     "Kronecker",
     Version => "0.3.3",
     Date => "July 16, 2010",
     Headline => "Kronecker and rational normal forms",
     Authors => {{Name => "Edward Carter",
               Email => "edward.carter@gmail.com"}},
     Keywords => {"Commutative Algebra"},
     DebuggingMode => false
     )
    
export {"kroneckerNormalForm", "kroneckerIndices", "rationalNormalForm", 
     "decomposeModule", "decomposeGradedModule", "doubleDualMap"}

-- columns(A)
--
-- Returns a list consisting of the columns of the matrix A.  The zero matrix
-- gives an empty list.
columns = method()
columns(Nothing) := N -> {}
columns(Matrix) := A -> (
     if A == 0 then {}
     else apply(numgens source A, i -> submatrix(A, {i}))
     )

-- joinColumns(M,L)
--
-- The opposite of the method columns(A).  The elements of the list L are
-- assumed to be columns which are joined to form a matrix.
--
-- The parameter M is only used in the case of L being an empty list.  In this
-- case, we return the zero map from M to itself.
joinColumns = method()
joinColumns(Module, List) := (M,L) -> (
     if #L == 0 then map(M, M, 0)
     else fold((v,w) -> v | w, L)
     )
joinColumns(GradedModule, List) := (M,L) -> (
     if #L == 0 then map(M, M, j -> map(M_j, M_j, 0))
     else fold((v,w) -> v | w, L)
     )

-- gen(I)
--
-- Assert that the ideal I is principal and return its generator.
gen = method()
gen(Ideal) := I -> (
     if I == 0 then 0 else (
     	  assert(numgens I == 1);
     	  (gens I)_0_0
	  )
     )

-- monomialMatrix(M, n, x, y)
--
-- Returns the homogeneous, degree 0 map to the module M determined by the
-- matrix:
--
--     | y^n -xy^(n-1) . . . (-x)^n |
--
monomialMatrix = method()
monomialMatrix(Module, ZZ, RingElement, RingElement) := (M, deg, x, y) -> (
     map(M, deg+1, (i,j) -> y^(deg-j)*(-x)^j)
     )

-- getDegree(v)
--
-- Return the degree of the entries of a homogeneous matrix v.
getDegree = method()
getDegree(Matrix) := v -> (
     seenNonzero := v_0_0 != 0;
     deg := first degree v_0_0;
     scan(numgens target v, i -> (
	       newdeg := first degree v_0_i;
	       if newdeg != deg then (
		    if (newdeg < deg) then (
			 assert(v_0_i == 0);
			 )
		    else (
		    	 assert(not seenNonzero);
		    	 deg = newdeg;
			 seenNonzero = true;
			 );
		    );
	       ));
     deg
     )

-- expandPolynomialRelation(v, x, y)
--
-- Assuming v is a homogeneous column vector of degree n, returns a matrix
-- with columns w0...wn where
-- 
--     v = y^n*w0 - xy^(n-1)*w1 + . . . + (-x)^n*wn
--
expandPolynomialRelation = method()
expandPolynomialRelation(Matrix, RingElement, RingElement) := (v, x, y) -> (
     deg := getDegree v;
     dual(dual v // monomialMatrix(target dual v, deg, x, y))
     )
	
-- complementaryForm(f,x,y)
--
-- Here, f is some linear form, and we return a complementary linear form.  The parameters x
-- and y should be set to the two variables of the ring.
complementaryForm = method()
complementaryForm(RingElement, RingElement, RingElement) := (f,x,y) -> (
     if f % ideal(y) == 0 then x
     else y
     )

-- findGen(M,N,P)
--
-- Given modules N and P which are both submodules of M, return
-- some element of N which is not in P, in the form of a column
-- vector.  If no such element exists, we return 0.
findGen = method()
findGen(Module,Module,Module) := (M,N,P) -> (
     f := inducedMap(M,P);
     G := select(columns mingens N, w -> (
	       w2 := map(M, , w);
	       v := w2 // f;
	       w2-f*v != 0
	       )
	  );
     if #G > 0 then G#0 else map(M, 1, (i,j) -> 0)
     )

leftInverse = method()
leftInverse(Matrix) := A -> (
     ((dual A)*A)^-1 * (dual A)
     )

-- kroneckerBases(P,x,y,g)
--
-- Returns (Q,A1,A2), where A1 and A2 are automorphisms on the target and source
-- of P, respectively, such that P relative to the ordered bases which are
-- the columns of A1 and A2, P is equal to Q, which is in Kronecker normal form.
kroneckerBases = method()
kroneckerBases(Matrix,RingElement,RingElement,RingMap) := (P,x,y,g) -> (
     -- The following variables will be referred to frequently in the comments.
     -- Our pencil is equal to Ax+By.
     R := ring P;
     M := source P;
     N := target P;
     A := map(N, , P ** R^1/(x-1,y));
     B := map(N, , P ** R^1/(x,y-1));
     
     kerList := {};
     basisList := {};
     
     -- Each homogeneous generator of the kernel corresponds to a block of the form
     --
     --     | x y 0 0 . . . 0 0 |
     --     | 0 x y 0 . . . 0 0 |
     --     | 0 0 x y . . . 0 0 |
     --     | . . . . . . . . . |
     --     | 0 0 0 0 . . . x y |
     --
     -- in the final Kronecker normal form.  The degree, d, of the element of the 
     -- kernel is equal to the number of rows.  We use expandPolynomialRelation 
     -- to produce d suitable basis elements in M.  Multiplying all of these by A 
     -- gives d basis elements in N, and multiplying B times the last one gives
     -- a (d+1)-st basis element in N.  Then P relative to these vectors is the
     -- above block.  Linear independence follows from the fact that "ker P"
     -- gives us *minimal* homogeneous generators of the kernel.
     --
     -- If d=0, using that kernel vector as a basis element in M produces a
     -- zero column, as desired.
     --
     -- The variables *SourceList are lists of basis elements in M, and the
     -- variables *TargetList are lists of basis elements in N.
     index := 0;
     scan(sort apply(apply(columns gens ker P, v -> map(M, , expandPolynomialRelation(v, x, y))),
	       v -> (index = index + 1;  (numgens source v, index, v))),
	  (n, i, v) -> (
	       -- n := numgens source v;
	       if n == 1 then (
	       	    kerList = kerList | {v};
	       	    )
	       else (
	       	    basisList = basisList | {(map(N, , A*submatrix(v, {0..(n-2)})), v)};
		    );
	       ));
		    
     -- The second main type of block in the Kronecker normal form corresponds to 
     -- elements in the kernel of dual P.  (Or, after sheafification, to the locally
     -- free part of the cokernel of P)
     L := columns gens ker dual P;
     L2 := select(L, v -> getDegree(v) == 0);
     cokerList := if #L2 == 0 then {} else (
	  X := dual joinColumns(N, L2);
	  {map(N, , id_(target X) // X)}
	  );
     
     -- Each homogeneous generator of the kernel of dual P corresponds to a
     -- block of the form
     --
     --     | x 0 0 . . . 0 |
     --     | y x 0 . . . 0 |
     --     | 0 y x . . . 0 |
     --     | 0 0 y . . . 0 |
     --     | . . . . . . . |
     --     | 0 0 0 . . . x |
     --     | 0 0 0 . . . y |
     --
     -- where the degree, d, is equal to the number of columns.  An element of
     -- this kernel is a row matrix w' such that w'P=0.  By using 
     -- expandPolynomialRelation on dual w', we get a matrix w with d+1 rows
     -- such that 
     --
     --     | (-y)^d  (-y)^(d-1)*x  (-y)^(d-2)*x^2  . . .  x^d | * w * P = 0
     --
     -- Therefore the columns of w*P are all linear combinations of columns
     -- of the above block.  Since "ker dual P" gives minimal generators of
     -- the kernel, we also know that all these columns are in the span of
     -- of the columns of w*P.  Therefore we can pull the above block back
     -- through w*P to get suitable basis vectors in M, then multiply by A
     -- and B to get suitable basis vectors in N.
     L2 = select(L, v -> getDegree(v) > 0);
     index = 0;
     if #L2 > 0 then (
	  wList := apply(sort apply(apply(L2, v -> expandPolynomialRelation(v,x,y)),
		    v -> (index = index + 1; (numgens source v, index, v))),
	       (n,i,v) -> map(R^(numgens source v), N, dual v));
	  tvList := apply(#wList, i -> (
		    curr := if i == 0 then first columns gens target wList#0 
		    else map(target wList#0, 1, (i,j) -> 0);
		    for j from 1 to #wList-1 do (
			 curr = curr || if i == j then first columns gens target wList#i 
			 else map(target wList#j, 1, (i,j) -> 0);
			 );
		    (-1 + numgens target wList#i, curr)
		    )
	       );
	  w := fold(wList, (v1,v2) -> v1 || v2);
	  scan(tvList, (deg, tmpv) -> (
		    tv := A*(map(target w, , tmpv) // (w*A));
		    tmpTargetList := {tv};
		    tmpSourceList := {};
	       	    for i from 1 to deg do (
		    	 sv := tv // A;
		    	 tv = B*sv;
			 tmpTargetList = tmpTargetList | {tv};
			 tmpSourceList = tmpSourceList | {sv};
		    	 );
		    basisList = basisList | {(joinColumns(N, tmpTargetList), 
			      joinColumns(M, tmpSourceList))};
		    )
	       );
	  );
     
     -- What's left at this point is the regular part of the pencil (square blocks
     -- in the normal form with nonzero determinant, analogous to Jordan or rational
     -- normal form).  For this, we will have to generate basis elements in N
     -- which will span Q.
     targetList := cokerList | apply(basisList, a -> a#0);
     Q := coker joinColumns(N, {P} | targetList);
     if #targetList > 0 then (
	  -- We're supposed to be building bases for M and N, so the list
	  -- of vectors in N at each point should be linearly independent.
	  assert(ker joinColumns(N, targetList) == 0);
	  );
     if Q != 0 then (
	  -- The cokernel of the regular part of the pencil is annihilated
	  -- by the determinant.  Each irreducible factor of the determinant
	  -- will give rise to blocks analogous to the blocks of the rational
	  -- normal form.
	  Q2 := coker P;
	  scan(select(factor gen ann Q, t -> t#0 % ideal(x,y) == 0), t -> (
		    -- f is an irreducible polynomial, n = deg f, and e is
		    -- the degree to which f occurs in the minimal polynomial
		    -- of the matrix (or the size of the biggest Jordan block
		    -- with that eigenvalue)
		    f := t#0;
		    e := t#1;
		    n := (degree f)#0;
		    
		    regularList := {};

     	       	    -- We will produce basis vectors corresponding to
		    -- a block of the form diagForm*I+compForm*J, where
		    -- J is a matrix in rational normal form.
		    diagForm := if n == 1 then f else x;
		    compForm := complementaryForm(diagForm, x, y);
		    A = map(N, , P ** R^1/(diagForm-1, compForm));
		    B = map(N, , P ** R^1/(diagForm, compForm-1));
		    
		    rels := joinColumns(Q2, cokerList | apply(basisList, a -> a#0));
		    for i from 0 to e-1 do (
			 -- Find a block in the rational normal form of
			 -- size n*(e-i).
			 N2 := Q2/ker(f^(e-i-1)*id_Q2);
			 u := map(N2, , findGen(N2, ker(f*id_N2), image map(N2, , rels)));
			 while u ** R^1/(x,y) != 0 do (
			      -- First we need a pair (v,w) of vectors in
			      -- M and N, respectively, such that:
			      --
			      --   1. w = A*v
			      --   2. w in Q is killed by f^(e-i) but
			      --      not by f^(e-i-1)
			      --
			      -- If both n and e-i are 1, we want to
			      -- produce a 1x1 block.  That means v should
			      -- be in the kernel of B, since compForm should
			      -- not appear in that column in the normal form.
			      v := if n*(e-i) == 1 then (
				   tmpv := u // (inducedMap(N2,N)*A*inducedMap(source A, ker B));
				   inducedMap(source A, ker B)*tmpv
				   )
			      else (
				   u // (inducedMap(N2, N)*A)
				   );
			      w := A*v;
			      
			      if n == 1 then (
				   -- If e-i>1, this block has the form
				   -- 
				   --     | x y . . 0 |
				   --     | 0 x . . 0 |
				   --     | . . . . . |
				   --     | 0 0 . . y |
				   --     | 0 0 . . x |
				   --
				   -- where x=diagForm, y=compForm for brevity.
				   -- Our original (v,w) pair gives us the x in the
				   -- lower right corner.  Then each new basis
				   -- vector w in N can be obtained by B*v from the
				   -- previous basis vector v in M.  Then the new
				   -- v can be obtained by pulling back w through A.
				   -- We just need to make sure that the last vector
				   -- v in M is in the kernel of B.
				   v2 := v;
				   for j from 2 to e-i do (
					w2 := map(N, , B*v2);
					v2 = if j == e-i then (
					     map(M, , (w2 || map(N, 1, (i,j) -> 0)) // (A || B))
					     )
					else (
					     map(M, , w2 // A)
					     );
					w = w2 | w;
					v = v2 | v;
					);
				   )
			      else (
				   -- This block has a form like
 				   --
				   --     | x-a1*y y 0 . . 0 |
				   --     |  -a2*y x y . . 0 |
				   --     |  -a3*y 0 x . . 0 |
				   --     |    .   . . . . . |
				   --     |  -an*y 0 0 . . y |
				   --     | -an'*y 0 0 . . x |
				   --
				   -- where n'=n+1.  We proceed as above, except for
				   -- the leftmost column, where we don't want the
				   -- last v to be in the kernel of B.
				   atmp := f // map(R^1, n+1, (i,j) -> (-compForm)^j*diagForm^(n-j));
				   a := map(R^n, 1, (i,j) -> atmp_j_(i+1) // atmp_j_0);
				   
				   w2 := {w};
				   v3 := {v};
				   for k from 1 to e-i do (
					for j from 1 to n do (
					     if (1 < j and j < n) or (j == n and k < e-i) then (
						  w2 = {map(N, , B*first v3)} | w2;
						  v3 = {map(M, , (first w2) // A)} | v3;
						  )	
					     else (
						  if 1 == j and k > 1 then (
						       a2 := joinColumns(N, select(n, w2, b -> true))*a;
						       w2 = {map(N, , a2+B*first v3)} | w2;
						       v3 = {map(M, , (first w2) // A)} | v3;
						       )
						  else if j == n and k == e-i then (
						       w2 = {map(N, , B*first v3)} | w2;
						       a3 := joinColumns(N, select(n, w2, b -> true))*a;
						       v3 = {map(M, , ((first w2) || (-a3)) // (A || B))} | v3;
						       );
						  );
					     );
					);
				   w = joinColumns(N, w2);
				   v = joinColumns(M, v3);
				   );
			      regularList = {(w,v)} | regularList;
			      assert(numgens N   
			      	   >= #columns joinColumns(N, cokerList | apply(basisList | regularList, a -> a#0)));
			      assert(w != 0);
			      rels = rels | w;
			      u = map(N2, , findGen(N2, ker(f*id_N2), image map(N2, , rels)));
     			      );
			 );
		    basisList = basisList | regularList;
		    ));
	  );
     
     -- Return bases as automorphisms on N and M.
     R2 := target g;
     (directSum(apply(kerList, v -> (
			 map(R2^0, numgens source v, (i,j) -> 0_R2)
			 ))
	       | apply(cokerList, v -> (
			 map(R2^(numgens source v), R2^0, (i,j) -> 0_R2)
			 ))
	       | apply(basisList, a -> (
			 block := (P*a#1) // a#0;
			 map(R2^(numgens target block), numgens source block,
			      (i,j) -> g(block_j_i))
			 ))),
	  joinColumns(N, cokerList | apply(basisList, a -> a#0)),
	  joinColumns(M, kerList | apply(basisList, a -> a#1)))
     )

returnNormalForm = method()
returnNormalForm(Sequence, List) := (m,cm) -> (
     (A,P,Q) := (m#0, m#1, m#2);
     (tchg,schg) := (cm#0, cm#1);
     if tchg then (
	  if schg then (A, P, Q) else (A, P)
	  )
     else (
	  if schg then (A, Q) else A
	  )
     )

dumbLift = method()
dumbLift(Ring, Ring) := (Q, R) -> (
     g := r -> (
	  g2 := map(Q, R, apply(numgens source vars R, i -> 0_Q));
	  coeffs := r // vars R;
	  ((vars Q)*map(source vars Q, 1, (i,j) -> g2(coeffs_j_i)))_0_0
	  );
     g
     )
dumbLift(Matrix, Ring, Ring) := (M, Q, R) -> (
     g := dumbLift(Q, R);
     map(Q^(numgens target M), numgens source M, (i,j) -> g(M_j_i))
     )

checkPencil = method()
checkPencil(Module) := M -> (
     if isFreeModule M and isHomogeneous M then true else (
	  error "expected a map between homogeneous free modules";
	  )
     )
checkPencil(Matrix,RingElement,RingElement) := (P,x,y) -> (
     if P != 0 then (
     	  R := ring P;
     	  M := source P;
     	  N := target P;
     	  
	  scan(numgens N, i -> scan(numgens M, 
	       j -> (
		    r := P_j_i;
		    if r != 0_R then (
			 if first degree r != 1 then error "expected a matrix of linear forms";
			 );
		    if r % ideal(x,y) != 0_R then error "expected a matrix in two variables";
		    )));
     	  checkPencil(M);
     	  checkPencil(N);
	  --if not isHomogeneous P then (
	  --     error "expected a homogeneous map";
	  --     );
	  
	  Q2 := coefficientRing R;
	  while isPolynomialRing Q2 do (
	       Q2 = coefficientRing Q2;
	       );
	  if not isField Q2 then error "coefficient ring must be a field";
	  v := vars R;
	  Q := Q2[apply(numgens source v, i -> v_i_0)];
	  f := map(R,Q);
	  g := dumbLift(Q,R);
	  (map(Q^(numgens N), Q^(numgens M), (i,j) -> g(P_j_i)), g(x), g(y))
	  )
     else (
	  (P,x,y)
	  )     
     )

shortBlock = method()
shortBlock(RingMap,ZZ,RingElement,RingElement) := (g,n,x,y) -> (
     R := ring g(x);
     if n == 0 then map(R^0, R^1, 0)
     else map(R^n, n+1, (i,j) -> if i == j then g(x) else if i+1 == j then g(y) else 0_R)
     )

tallBlock = method()
tallBlock(RingMap,ZZ,RingElement,RingElement) := (g,n,x,y) -> (
     R := ring g(x);
     if n == 0 then map(R^1, R^0, 0)
     else map(R^(n+1), n, (i,j) -> if i == j then g(x) else if i == j+1 then g(y) else 0_R)
     )

regularBlock = method()
regularBlock(RingMap,Power,RingElement,RingElement) := (g,t,x,y) -> (
     f := t#0;
     e := t#1;
     n := (degree f)#0;
     
     diagForm := if n == 1 then f else x;
     compForm := complementaryForm(f,x,y);
     
     R := ring g(x);
     if n == 1 then map(R^e, e, (i,j) -> (
	       if i == j then g(diagForm)
	       else if i+1 == j then g(compForm)
	       else 0_R))
     else (
	  m := e*n;
	  Q := ring f;
	  atmp := f // map(Q^1, n+1, (i,j) -> (-compForm)^j*diagForm^(n-j));
	  a := map(Q^n, 1, (i,j) -> atmp_j_(i+1) // atmp_j_0);
	  map(R^m, m, (i,j) -> (
		    if i+1 == j then g(compForm)
		    else if j % n == 0 then (
			 if j <= i and i < j+n then (
			      if j == i then g(diagForm - compForm*a_0_0)
			      else g(compForm*(-a_0_(i-j)))
			      )
			 else 0_R
			 )
		    else if i == j then g(diagForm) else 0_R
		    ))
	  )
     )

normalFormFromIndices = method()
normalFormFromIndices(RingMap,RingElement,RingElement,List) := (g,x,y,L) -> (
     if ring x =!= ring y then (
	  error "expected elements of the same ring";
	  )
     else (
	  ind := L#0;
     	  (kerList, dualKerList, divisors) := ind;
	  R := ring g(x);
	  
	  blockList := apply(kerList, n -> shortBlock(g,n,x,y))
	       | apply(dualKerList, n -> tallBlock(g,n,x,y));
	  if #divisors > 0 then (
	       blockList = blockList | select(apply(divisors, t -> regularBlock(g,t,x,y)),
		    B -> B != 0);
	       );
	  if #blockList > 0 then directSum(blockList) else 0_R
     	  )
     )

pencilFromMatrixPair = method()
pencilFromMatrixPair(Matrix,Matrix) := (f,g) -> (
     R2 := ring f;
     R := if isPolynomialRing R2 then coefficientRing R2 else R2;
     if target f == target g and source f == source g then (
	  n := numgens target f;
	  m := numgens source f;
	  NN := target f;
	  MM := source f;
	  (f2,g2) := apply((f,g), ff -> map(R^n, m, (i,j) -> (
			 gg := map(R, R2, apply(numgens source vars R2, i -> 0_R));
			 r := ff_j_i;
			 if #(degree r) > 0 then (
			      if first degree r > 0 then (
			      	   error "expected a matrix of scalars";
			      	   );
			      );
			 gg(r)
			 )));
	  X0 := local X0;
	  X1 := local X1;
     	  S := R[X0,X1];
     	  use S;
     	  N := S^n;
     	  M := S^m ** S^{-1};
     	  (map(N, M, X0*(f2**S) + X1*(g2**S)), X0, X1)
	  )
     else error "expected maps with the same source and target"
     )
	
kroneckerNormalForm = method(
     Options => {
	  ChangeMatrix => {true, true}		    -- target, source
	  }
     )
kroneckerNormalForm(Matrix) := o -> f -> (
     v := vars ring f;
     kroneckerNormalForm(f, v_0_0, v_1_0, ChangeMatrix => o.ChangeMatrix)
     )
kroneckerNormalForm(Matrix,RingElement,RingElement) := o -> (f,x,y) -> (
     (f2,x2,y2) := checkPencil(f,x,y);
     
     R := ring f;
     cm := o.ChangeMatrix;
     g := map(R, ring f2);
     if cm#0 or cm#1 then (
	  (A,P,Q) := kroneckerBases(f2,x2,y2,g);
	  P = map(target f, target f, (i,j) -> g(P_j_i));
	  Q = map(source f, source f, (i,j) -> g(Q_j_i));
	  tmat := map(target f, target f, P^-1);
	  use R;
	  returnNormalForm((A, tmat, Q), o.ChangeMatrix)
	  )
     else (
	  B := normalFormFromIndices(g, x2, y2, {kroneckerIndices(f2,x2,y2)});
	  use R;
	  if B == 0 then map(target f, source f, (i,j) -> 0_R) else B
	  )
     )
kroneckerNormalForm(Matrix,Matrix) := o -> (f,g) -> (
     R2 := ring f;
     R := if isPolynomialRing R2 then coefficientRing R2 else R2;
     (P, rv0, rv1) := pencilFromMatrixPair(f,g);
     S := ring P;
     fproj := map(R, S, matrix{{1_R,0_R}});
     gproj := map(R, S, matrix{{0_R,1_R}});
     inc := map(R2, R);
     use R2;
     (B,P1,P2) := kroneckerNormalForm(P, rv0, rv1);
     f3 := directSum(apply(components B, b -> map(R2^(numgens target b), 
		    numgens source b, (i,j) -> inc fproj(b_j_i))));
     g3 := directSum(apply(components B, b -> map(R2^(numgens target b), 
		    numgens source b, (i,j) -> inc gproj(b_j_i))));
     NN := target f;
     MM := source f;
     (f3, g3, map(NN, NN, (i,j) -> inc gproj(P1_j_i)),
	  map(MM, MM, (i,j) -> inc gproj(P2_j_i)))
     )

factorList = method()
factorList(RingElement) := r -> (
     p := factor r;
     apply(#p, i -> p#i)
     )

elementaryDivisors = method()
elementaryDivisors(Matrix, Ideal, ZZ, RingElement) := (P, J, n, prevGen) -> (
     r := gen saturate(minors(n, P), J);
     if r == 0 then {} else (
     	  if r == 1 then elementaryDivisors(P, J, n+1, prevGen)
     	  else factorList(r // prevGen) | elementaryDivisors(P, J, n+1, r)
	  )
     )

kroneckerIndices = method()
kroneckerIndices(Matrix, RingElement, RingElement) := (P,x,y) -> (
     checkPencil(P,x,y);
     (apply(columns sort(gens ker P, DegreeOrder => Ascending), getDegree),
     	  apply(columns sort(gens ker dual P, DegreeOrder => Ascending), getDegree),
	  elementaryDivisors(P, ideal(x,y), 1, promote(1, ring P)))
     )
kroneckerIndices(Matrix, Matrix) := (f,g) -> (
     (P, rv0, rv1) := pencilFromMatrixPair(f,g);
     kroneckerIndices(P, rv0, rv1)
     )

rationalNormalForm = method(
     Options => {
	  ChangeMatrix => {true, true}
	  }
     )
rationalNormalForm(Matrix) := o -> f -> (
     R2 := ring f;
     R := coefficientRing R2;
     if target f == source f then (
	  a := kroneckerNormalForm(id_(source f), f);
	  returnNormalForm((a#1, a#2, a#3), o.ChangeMatrix)
	  )
     else error "expected a square matrix"
     )

-- We compare the Kronecker normal form results for the same matrices,
-- asking for change of basis matrices on the one hand, and asking
-- for them to be omitted on the other.  This results in entirely
-- separate code paths being taken.  The code path where the change
-- of basis matrices are omitted can be easily verified to produce
-- a valid Kronecker normal form, whereas the code path where we ask
-- for change of basis matrices can be easily verified to give
-- a sequence whose product is equal to the input.  Then we compare
-- the two results to test both paths.
--
-- For test matrices, we have chosen examples with regular blocks
-- corresponding to only one eigenvalue or conjugate collection of
-- eigenvalues, or no regular blocks at all, for easier comparison
-- of results.  Otherwise we'd have to worry about rearranging the 
-- blocks.
TEST ///
     obfuscate = method()
     obfuscate(Matrix) := A -> (
	  P := random(target A, target A);
	  while det P == 0 do (
	       P = random(target A, target A);
	       );
	  Q := random(source A, source A);
	  while det Q == 0 do (
	       Q = random(source A, source A);
	       );
	  P*A*Q
	  )
     Q = ZZ/101[x,y]
     use Q
     maxDiff = 8
     maxN = 4
     LL = apply(maxDiff*(maxN+1), i -> (i // maxDiff, (i%maxDiff) + 1))
     L = apply(LL, (n,diff) -> random(Q^(n+diff), Q^n ** Q^{-1})) |
	       apply(LL, (n,diff) -> random(Q^n, Q^(n+diff) ** Q^{-1})) |
	       apply({matrix{{x,0,0,0},{y,x,0,0},{0,y,0,0},{0,0,x-y,0},{0,0,0,x-y}},
			 matrix{{x,y,0,0,0,0},{0,0,x,2*y,0,0},
			      {0,0,-y,x,y,0},{0,0,0,0,x,-2*y},{0,0,0,0,y,x}},
			 matrix{{y,x,0},{0,y,0},{0,0,y}},
			 matrix{{x,y,0,0,0,0},{-2*y,x,y,0,0,0},{0,0,x,y,0,0},
			      {0,0,-2*y,x,0,0},{0,0,0,0,x,y},{0,0,0,0,-2*y,x}}},
		    P -> obfuscate(map(Q^(numgens target P), , P))) |
	       {matrix{{0,x},{0,x-y}}} |
	       {map(Q^0, Q^1, {})}
     scan(L, P -> (
	       err := kroneckerNormalForm(P, ChangeMatrix => {false, false}) - (kroneckerNormalForm P)#0;
	       --if err != 0 then print P;
	       assert(err == 0);
	       ))
///

-- decomposeModule
--
-- Given a module over the ring k[x,y]/(x^2,y^2), where k is some field,
-- returns a direct sum decomposition of the module which is obtained
-- via Kronecker normal form.  That is, if (N,f) = decomposeModule M,
-- then f:N -- > M is an isomorphism, and N is the direct sum of simple
-- modules.
decomposeModule = method()
decomposeModule(Module) := M -> (
     M = trim M;
     R := ring M;
     a := gens R;
     K := coefficientRing R;
     assert(#a == 2);
     assert(isField K);
     use R;
     x := a#0;
     y := a#1;
     I := ann M;
     assert(x^2 % I == 0_R);
     assert(y^2 % I == 0_R);
     
     p := x*y*id_M;
     freeGens := apply(columns mingens image p, v -> (
	       map(M, , map(M, , v) // p)
	       ));
     
     MM := image map(M, , joinColumns(M, 
	       select(columns mingens ker p, v -> getDegree(v) == 0)));
     R2 := R/(x^2, x*y, y^2);
     f := presentation(MM**R2);
     (g,P,Q) := kroneckerNormalForm f;
     f2 := dumbLift(f, R, R2);
     f3 := map(R, R2, apply(numgens source vars R2, i -> 0_R));
     use R;
     N := directSum(apply(components g, b -> (
		    b2 := dumbLift(b, R, R2);
		    trim((coker b2) ** R^1/(x^2, x*y, y^2))
		    )));
     (directSum(apply(freeGens, source) | components N), 
	  joinColumns(M, freeGens | {inducedMap(M,MM)
	       	    *map(MM, coker f2, 1)
	       	    *map(coker f2, N, (i,j) -> f3((P^(-1))_j_i))}))
     )

if version#"VERSION" <= "1.1" then (

GradedModuleMap + GradedModuleMap := (f,g) -> (
     if target f != target g or source f != source g or degree f != degree g then (
	  error "maps cannot be added";
	  );
     
     map(target f, source f, i -> f_i + g_i, Degree => degree f)
     );

GradedModuleMap * GradedModuleMap := (f,g) -> (
     if target g != source f then (
	  error "maps not composable";
	  );
     
     dg := degree g;
     d := dg + degree f;
     map(target f, source g, i -> (
	       map((target f)_(i+d), (source g)_i, f_(i+dg)*g_i)
	       ), Degree => d)
     );

GradedModuleMap // GradedModuleMap := (f,g) -> (
     if target f != target g then (
	  error "expected maps with the same target";
	  );
     
     d := degree f - degree g;
     -- The target of f_i has degree i+deg f, which is
     -- equal to i+d+deg g.  This is the same as the target
     -- of g_(i+d).
     map(source g, source f, i -> f_i // g_(i+d), Degree => d)
     );

GradedModuleMap | GradedModuleMap := (f,g) -> (
     if target f != target g then (
	  error "expected maps with the same target";
	  );
     if degree f != degree g then (
	  error "expected maps with the same degree";
	  );
     
     d := degree f;
     map(target f, source f ++ source g, j -> f_j | g_j, Degree => d)
     );

min(GradedModule) := M -> min chainComplex M;
max(GradedModule) := M -> max chainComplex M;

ker(GradedModuleMap) := o -> f -> (
     M := source f;
     K := new GradedModule;
     K.ring = ring M;
     scan(min M .. max M, i -> (
	       K#i = trim ker f_i
	       )
	  );
     K
     );

image(GradedModuleMap) := f -> (
     M := target f;
     I := new GradedModule;
     I.ring = ring M;
     d := degree f;
     scan(select(min M .. max M, i -> f_(i-d) != 0), i -> (
	       I#i = image f_(i-d)
	       )
	  );
     I
     );

GradedModule.directSum = args -> (
     R := ring args#0;
     if not all(args, f -> ring f === R)
     then error "expected graded modules all over the same ring";
     N := new GradedModule;
     N.ring = R;
     scan(min apply(args, min) .. max apply(args, max), i -> (
	       N#i = directSum apply(args, M -> M_i)
	       )
	  );
     N.cache = new CacheTable;
     N.cache.components = toList args;
     N.cache.formation = FunctionApplication { directSum, args };
     N
     );

GradedModule ++ GradedModule := (M,N) -> directSum(M,N);

GradedModule ^ Array := (M,w) -> (
     w = toList w;
     if not M.?cache or not M.cache.?components
     then error "expected a direct sum graded module";
     map(directSum M.cache.components_w, M, j -> (
	       k := 0;
	       v := apply(M.cache.components, N -> k .. (k = k + numgens N_j) - 1);
	       map((directSum M.cache.components_w)_j, M_j, (cover M_j)^(splice apply(w, i -> v#i)))
	       )
	  )
     );

GradedModule _ Array := (M,w) -> (
     w = toList w;
     if not M.?cache or not M.cache.?components
     then error "expected a direct sum graded module";
     map(M, directSum M.cache.components_w, j -> (
	       k := 0;
	       v := apply(M.cache.components, N -> k .. (k = k + numgens N_j) - 1);
	       map(M_j, (directSum M.cache.components_w)_j, (cover M_j)_(splice apply(w, i -> v#i)))
	       )
	  )
     );

GradedModuleMap.directSum = args -> (
     R := ring args#0;
     if not all(args, f -> ring f === R)
     then error "expected graded module maps all over the same ring";
     d := degree args#0;
     if not all(args, f -> degree f == d)
     then error "expected graded module maps all of the same degree";
     M := directSum apply(args, source);
     N := directSum apply(args, target);
     g := map(directSum apply(args, target), directSum apply(args, source), 
	  j -> directSum apply(args, f -> f_j), Degree => d);
     g.cache = new CacheTable;
     g.cache.components = toList args;
     g.cache.formation = FunctionApplication { directSum, args };
     g
     );

trim(GradedModule) := o -> M -> (
     rval := new GradedModule;
     rval.ring = ring M;
     scan(min M .. max M, i -> (
	       rval#i = trim M_i;
	       )
	  );
     rval
     );

annihilator(GradedModule) := o -> M -> intersect apply(min M .. max M, j -> ann M_j);

GradedModule ** Ring := (M,R) -> (
     N := new GradedModule;
     N.ring = R;
     scan(min M .. max M, j -> (
	       N#j = M_j ** R;
	       )
	  );
     N
     );

GradedModuleMap ** Ring := (f,R) -> (
     map((target f) ** R, (source f) ** R, j -> (
	       f_j ** R
	       ),
	  Degree => degree f)
     );

GradedModule / GradedModule := (M,N) -> (
     if ring M != ring N then (
	  error "expected graded modules over the same ring";
	  );
     
     Q := new GradedModule;
     Q.ring = M.ring;
     scan(min{min M, min N} .. max{max M,max N}, j -> (
	       Q#j = M_j / N_j;
	       )
	  );
     Q
     );

 coimage(GradedModuleMap) := f -> (
      source f / ker f
      )
 
 )

injection = method()
injection(GradedModuleMap) := f -> map(target f, coimage f, j -> f_j, Degree => degree f)

doubleDualMap = method()
doubleDualMap(Module,Module) := (M,N) -> (
     F := Hom(M,N);
     maps := apply(columns gens F, v -> homomorphism map(F, , v));
     apply(columns gens M, v -> map(N, F, joinColumns(N, apply(maps, phi -> phi*map(M, , v)))))
     )

decomposeGradedModule = method()
decomposeGradedModule(GradedModuleMap, GradedModuleMap) := (x,y) -> (
     scan({x,y}, z -> (
	       if source z != target z then (
	       	    error "expected a map from a graded module to itself";
	       	    );
	       if degree z != 1 then (
		    error "expected a graded module map of degree 1";
		    );
	       if z*z != 0 then (
		    error "expected a graded module map which squares to zero";
		    );
	       ));
     if source x != source y then (
	  error "expected two maps on the same graded module";
	  );
     if x*y+y*x != 0 then (
	  error "expected graded module maps which anticommute";
	  );
     
     M := source x;
     Q := ring M;
     degList := toList(min M .. max M);
     
     p := x*y;
     mygens := flatten apply(degList, i -> apply(columns mingens image p_i, v -> (
		    src := new GradedModule;
		    src.ring = M.ring;
		    v1 := map(target p_i, , v) // p_i;
		    v2 := (x_i*v1) | (y_i*v1);
		    v3 := p_i*v1;
		    src#i = source v1;
		    src#(i+1) = source v2;
		    src#(i+2) = source v3;
		    injection map(M, src, j -> (
			      if j == i then (
				   map(M_j, , v1)
				   )
			      else if j == i+1 then (
				   map(M_j, , v2)
				   )
			      else if j == i+2 then (
				   map(M_j, , v3)
				   )
			      else 0
			      )
			 )
		    )));
     
     MM := ker p;
     mygens = mygens | flatten apply(degList, i -> (
	       tmpSrc := MM_i / image(x_(i-1) | y_(i-1));
	       if tmpSrc == 0 then {} else (
		    src := trim image map(MM_i, , id_tmpSrc // inducedMap(tmpSrc, MM_i));
	       	    inc := inducedMap(M_i, MM_i)*inducedMap(MM_i, src);
		    im := inducedMap(M_(i+1), MM_(i+1))*map(MM_(i+1), , 
			 mingens(image(x_i*inc) + image(y_i*inc)) // generators MM_(i+1));
	       	    (A,B,P1,P2) := kroneckerNormalForm((x_i*inc) // im, (y_i*inc) // im);
	       	    apply(#(components A), j -> (
			      v := map(M_i, , inc*P2*map(source P2, , (source A)_[j]));
			      w := map(M_(i+1), , im*P1^(-1)*map(target P1, , (target A)_[j]));
			      tmpGM := (gradedModule source v)[-i];
			      tmpGM#(i+1) = source w;
			      injection map(M, tmpGM, k -> (
				   	if k == i then map(M_k, tmpGM_k, v)
				   	else if k == i+1 then map(M_k, tmpGM_k, w)
				   	else map(M_k, tmpGM_k, 0)
				   	)
			      	   )
			      )
		    	 )
	       	    )
	       )
	  );
     
     (newx, newy) := apply((x,y), z -> directSum(apply(mygens, g -> (
			 map(source g, source g, j -> (
				   (z_j*g_j) // g_(j+1)
				   ), Degree => 1)
			 )
		    )
	       )
	  );
     (newx, newy, joinColumns(M, mygens))
     )

beginDocumentation()

document {
     Key => { Kronecker },
     Headline => "Kronecker and rational normal forms",
     }

document {
     Key => { kroneckerNormalForm,
	  (kroneckerNormalForm,Matrix,RingElement,RingElement),
	  (kroneckerNormalForm,Matrix),
	  [kroneckerNormalForm,ChangeMatrix]
	  },
     Headline => "Kronecker normal form of a matrix of linear forms",
     Usage => concatenate("(B,P,Q) = kroneckerNormalForm A\n",
	  "(B,P) = kroneckerNormalForm(A,ChangeMatrix=>{true,false})\n",
	  "(B,Q) = kroneckerNormalForm(A,ChangeMatrix=>{false,true})\n",
	  "B = kroneckerNormalForm(A,ChangeMatrix=>{false,false})\n",
	  "(B,P,Q) = kroneckerNormalForm(A,x,y)\n",
	  "(B,P) = kroneckerNormalForm(A,x,y,ChangeMatrix=>{true,false})\n",
	  "(B,Q) = kroneckerNormalForm(A,x,y,ChangeMatrix=>{false,true})\n",
	  "B = kroneckerNormalForm(A,x,y,ChangeMatrix=>{false,false})"),
     Inputs => {
	  "A" => Matrix => "a matrix of linear forms in two variables",
	  "x" => RingElement,
	  "y" => RingElement
	  },
     Outputs => {
	  "B" => Matrix => {"the Kronecker normal form of ", TT "A"},
	  "P" => Matrix => "invertible (left) change of basis matrix",
	  "Q" => Matrix => "invertible (right) change of basis matrix"
	  },
     "This function produces a matrix of linear forms ", TT "B", ", and invertible matrices ",
     TT "P", " and ", TT "Q", " such that ", TT "B = P*A*Q", " and ", TT "B", 
     " is in Kronecker normal form.  The optional parameters ", TT "x", " and ",
     TT "y", ", if given, should be complementary linear forms. ",
     "They are used to fill in nonzero entries of all blocks in the ",
     "Kronecker normal form for which there is a choice of which linear forms to use. ",
     "If they are not given, they are assumed to be the first two variables of the ring.",
     EXAMPLE lines ///
         R = ZZ/101[x,y]
         A = matrix{{x,y,x,y},{y,x,y,x},{x,y,x,y},{y,y,y,y},{x,x,y,y}}
	 (B,P,Q) = kroneckerNormalForm A
	 B - P*A*Q == 0
	 kroneckerNormalForm(A, ChangeMatrix => {true,false})
	 kroneckerNormalForm(A, ChangeMatrix => {false,true})
	 kroneckerNormalForm(A, ChangeMatrix => {false,false})
	 (B,P,Q) = kroneckerNormalForm(A,x+y,x-y)
	 B - P*A*Q == 0
     ///
     }

document {
     Key => { rationalNormalForm,
	  (rationalNormalForm,Matrix),
	  [rationalNormalForm,ChangeMatrix]
	  },
     Headline => "rational normal form of a matrix",
     Usage => concatenate("(B,P,Q) = rationalNormalForm A\n",
	  "(B,P) = rationalNormalForm(A,ChangeMatrix=>{true,false})\n",
	  "(B,Q) = rationalNormalForm(A,ChangeMatrix=>{false,true})\n",
	  "B = rationalNormalForm(A,ChangeMatrix=>{false,false})"),
     Inputs => {
	  "A" => Matrix => "a square matrix with constant entries"
	  },
     Outputs => {
	  "B" => Matrix => {"the rational normal form of ", TT "A"},
	  "P" => Matrix => "invertible (left) change of basis matrix",
	  "Q" => Matrix => "invertible (right) change of basis matrix"
	  },
     "This function produces a matrix ", TT "B", " in rational normal form, and ",
     "invertible matrices ", TT "P", " and ", TT "Q", " such that ",
     TT "P*Q = I", " and ", TT "B = P*A*Q", ".",
     EXAMPLE lines ///
         R = ZZ/101[x]
	 M = R^4
	 A = random(M,M)
	 factor det(x*id_M - A)
	 (B,P,Q) = rationalNormalForm A
	 B - P*A*Q == 0
	 P*Q - id_M == 0
     ///
     }

document {
     Key => { kroneckerIndices,
	  (kroneckerIndices,Matrix,RingElement,RingElement)
	  },
     Headline => "data which classify a matrix pencil up to strict equivalence",
     Usage => "(L, dualL, d) = kroneckerIndices(P,x,y)",
     Inputs => {
	  "P" => Matrix => "a matrix of linear forms",
	  "x" => RingElement,
	  "y" => RingElement
	  },
     Outputs => {
	  "L" => List => "degrees of minimal generators of the kernel",
	  "dualL" => List => "degrees of minimal generators of the kernel of the dual",
	  "d" => List => "elementary divisors"
	  },
     "This function gives data which classify a matrix pencil up to strict ",
     "equivalence.  That is, these data determine the Kronecker normal form ",
     "of the pencil, up to rearranging the blocks.",
     EXAMPLE lines ///
         R = ZZ/101[x,y]
	 A = matrix{{x,y,x,y},{y,x,y,x},{x,y,x,y},{y,y,y,y},{x,x,y,y}}
	 kroneckerNormalForm(A, ChangeMatrix => {false,false})
	 kroneckerIndices(A,x,y)
     ///
     }

document {
     Key => {
	  decomposeModule,
	  (decomposeModule,Module)
	  },
     Headline => "decompose a module into a direct sum of simple modules",
     Usage => "(N,f) = decomposeModule M",
     Inputs => {
	  "M" => Module => "a module over a ring k[x,y]/(x^2,y^2)"
	  },
     Outputs => {
	  "N" => Module => {"a module, isomorphic to ", TT "M",
	       " which is a direct sum of simple modules"},
	  "f" => Matrix => {"an isomorphism from ", TT "N", " to ", TT "M"}
	  },
     "This function decomposes a module into a direct sum of simple modules, ",
     "given some fairly strong assumptions on the ring which acts on the ",
     "ring which acts on the module.  This ring must only have two variables, ",
     "and the square of each of those variables must kill the module.",
     EXAMPLE lines ///
     	  Q = ZZ/101[x,y]
	  R = Q/(x^2,y^2)
	  M = coker random(R^5, R^8 ** R^{-1})
	  (N,f) = decomposeModule M
	  components N
	  ker f == 0
	  coker f == 0
     ///
     }

document {
     Key => {
	  decomposeGradedModule,
	  (decomposeGradedModule,GradedModuleMap,GradedModuleMap)
	  },
     Headline => "decompose a graded module over a ring of graded module maps",
     Usage => "(x',y',f) = decomposeGradedModule(x,y)",
     Inputs => {
	  "x" => GradedModuleMap => "a graded module map of degree 1",
	  "y" => GradedModuleMap => "a graded module map of degree 1"
	  },
     Outputs => {
	  "x'" => GradedModuleMap,
	  "y'" => GradedModuleMap,
	  "f" => GradedModuleMap => "an isomorphism"
	  },
     "Given maps ", TT "x", " and ", TT "y", " of degree 1 from a graded ",
     "module ", TT "M", " over a field k to ", TT "M", ", we can think of ",
     TT "M", " as a module over ", TT "k[x,y]", ".  If ", TT "x^2=0", ", ",
     TT "y^2=0", ", and ", TT "x*y+y*x=0", ", we can decompose ", TT "M",
     " into a direct sum of simple modules."
     }

document {
     Key => {
	  (kroneckerNormalForm,Matrix,Matrix)
	  },
     Headline => "normal form of a pair of matrices of scalars",
     Usage => "(A',B',P,Q) = kroneckerNormalForm(A,B)",
     Inputs => {
	  "A" => Matrix,
	  "B" => Matrix
	  },
     Outputs => {
	  "A'" => Matrix,
	  "B'" => Matrix,
	  "P" => Matrix,
	  "Q" => Matrix
	  },
     "This function gives the normal form of a pair of matrices ", TT "(A,B)", " over a field of the same ",
     "dimensions up to multiplication on either side by an invertible matrix.  The return values are such ",
     "that ", TT "P*A*Q=A'", " and ", TT "P*B*Q=B'", ".",
     EXAMPLE lines ///
     	  R = QQ
	  A = random(R^2, R^5)
	  B = random(R^2, R^5)
	  (A',B',P,Q) = kroneckerNormalForm(A,B)
	  P*A*Q - A' == 0
	  P*B*Q - B' == 0
     ///
     }

document {
     Key => {
	  (kroneckerIndices,Matrix,Matrix)
	  },
     Headline => "data classifying the normal form of a pair of matrices",
     Usage => "kroneckerIndices(A,B)",
     Inputs => {
	  "A" => Matrix,
	  "B" => Matrix
	  },
     Outputs => {
	  List
	  }
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Kronecker RemakeAllDocumentation=true "
-- End:
