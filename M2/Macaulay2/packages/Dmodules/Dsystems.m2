-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai
needsPackage "FourTiTwo"; -- Needed only for gkz


-----------------------------------------
-- GKZ related things
-----------------------------------------

gkzInputValidation = method();
gkzInputValidation (Matrix, List, PolynomialRing) := (A, b, W) -> (
    if (numRows A != #b) then
        error "expected number of rows of A to equal length of b in gkz(A,b,W)";
    if W.monoid.Options.WeylAlgebra === {} then 
        error "expected a Weyl algebra";
    createDpairs W;
    if #W.dpairVars#0 != numColumns A then
        error "expected number of columns of A to equal number of \"x\"s of Weyl algebra in gkz(A,b,W)";
    )
gkzInputValidation (Matrix, PolynomialRing) := (A, W) -> gkzInputValidation(A, toList((numRows A): 0), W)


eulerOperators = method();
eulerOperators (Matrix, List, PolynomialRing) := (A, b, W) -> (
    gkzInputValidation(A, b, W);
    apply(numRows A, i -> sum(numColumns A, j -> A_(i,j) * W.dpairVars#0#j * W.dpairVars#1#j) - b#i)
    )
eulerOperators (Matrix, PolynomialRing) := (A, W) -> eulerOperators(A, toList((numRows A): 0), W)


toricIdealPartials = method();
toricIdealPartials (Matrix, PolynomialRing) := (A, W) -> (
    gkzInputValidation(A, W);
    -- Extract the polynomial ring of the partials
    partialsRing := extractDiffsAlgebra W;
    -- Make the toric ideal
    toricMarkov(A, partialsRing)
    )


-- This routine returns the GKZ hypergeometric system of PDE's associated
-- to the matrix A and the parameter vector b.
gkz = method();
gkz(Matrix, List, PolynomialRing) := (A, b, W) -> (
    gkzInputValidation(A, b, W);
    
    d := numRows A;
    n := numColumns A;
        
    -- Make the toric ideal
    toricIdeal := toricIdealPartials(A, W);
    
    -- Make the Euler operators
    eulerOpsWithBeta := eulerOperators(A, b, W);
    
    -- Make the gkz ideal
    ideal eulerOpsWithBeta + (map(W, ring toricIdeal)) toricIdeal
    )

gkz(Matrix, List) := (A, b) -> (
    d := numRows A;
    n := numColumns A;
    D := symbol D;
    x := symbol x;
    W := QQ(monoid [x_1 .. x_n, D_1 .. D_n,
	WeylAlgebra => apply(toList(1..n), i -> x_i=>D_i)]);
        
    gkz(A, b, W)
    )
    


-- gkz(Matrix, List) := options -> (A, b) -> (
--      d := numgens target A;     
--      n := numgens source A;
--      if (d != #b) then
--        error "expected number of rows of A to equal length of b in gkz(A,b)";
--      y := symbol y;
--      S := QQ[y_1 .. y_n];
--      tempList := {};
--      i := 0;
--      Ker := kernel A;
--      B := generators Ker;
--      while i < numgens Ker do (
-- 	  posComp := {};
-- 	  negComp := {};
-- 	  j := 0;
-- 	  while j < n do (
-- 	       if B_i_j >= 0 then (
-- 		    posComp = append(posComp, B_i_j);
-- 		    negComp = append(negComp, 0);)
-- 	       else (
-- 		    posComp = append(posComp, 0);
-- 		    negComp = append(negComp, -B_i_j);
-- 		    );
-- 	       j = j+1;
-- 	       );
-- 	  tempList = append(tempList,
-- 	       product(posComp, toList(y_1..y_n), (k,l)->l^k) -
-- 	       product(negComp, toList(y_1..y_n), (k,l)->l^k));
-- 	  i = i+1;
-- 	  );
--      -- should do smarter saturation in future
--      J := saturate ( ideal(tempList), product toList(y_1..y_n) );
--      -- local version: better way to do this?
--      if options.Vars == Local then (
-- 	  u := symbol u;
-- 	  Du := symbol Du;
-- 	  R := QQ[u_1 .. u_n, Du_1 .. Du_n,
-- 	       WeylAlgebra => (toList(1..n)) / (i->(u_i=>Du_i))];
--      	  StoR := map(R, S, (vars R)_{n..numgens R - 1});
--      	  i = 0;
--      	  tempList = {};
--      	  while i < d do (
-- 	       tempList = append(tempList,
-- 	       	    sum(1..n, j -> (A^{i}_(0,j-1))*u_j*Du_j) - b#i);
-- 	       i = i+1;
-- 	       );     
--      	  out := (StoR J) + ideal(tempList);
-- 	  )
--      else (
-- 	  D := symbol D;
-- 	  x := symbol x;
--      	  R = QQ[x_1 .. x_n, D_1 .. D_n,
-- 	       WeylAlgebra => (toList(1..n)) / (i->(x_i=>D_i))];  
--      	  StoR = map(R, S, (vars R)_{n..numgens R - 1});
--      	  i = 0;
--      	  tempList = {};
--      	  while (i < d) do (
-- 	       tempList = append(tempList,
-- 	       	    sum(1..n, j -> (A^{i}_(0,j-1))*x_j*D_j) - b#i);
-- 	       i = i+1;
-- 	       );     
--      	  out =(StoR J) + ideal(tempList)
-- 	  );
--      out)

-- ///
-- gkz Matrix := options -> A -> (
--      d := numgens target A;     
--      n := numgens source A;
--      y := symbol y;
--      S := QQ[y_1 .. y_n];
--      tempList := {};
--      i := 0;
--      Ker := kernel A;
--      B := generators Ker;
--      while i < numgens Ker do (
-- 	  posComp := {};
-- 	  negComp := {};
-- 	  j := 0;
-- 	  while j < n do (
-- 	       if B_i_j >= 0 then (
-- 		    posComp = append(posComp, B_i_j);
-- 		    negComp = append(negComp, 0);)
-- 	       else (
-- 		    posComp = append(posComp, 0);
-- 		    negComp = append(negComp, -B_i_j);
-- 		    );
-- 	       j = j+1;
-- 	       );
-- 	  tempList = append(tempList,
-- 	       product(posComp, toList(y_1..y_n), (k,l)->l^k) -
-- 	       product(negComp, toList(y_1..y_n), (k,l)->l^k));
-- 	  i = i+1;
-- 	  );
--      -- should do smarter saturation in future
--      J := saturate ( ideal(tempList), product toList(y_1..y_n) );
--      D := symbol D; s := symbol s; x := symbol x;
--      R := QQ[s_1 .. s_d, x_1 .. x_n, 
-- 	  D_1 .. D_n,
-- 	  WeylAlgebra => (toList(1..n)) / (i->(x_i=>D_i)),
-- 	  MonomialOrder => Eliminate d];
--      StoR := map(R, S, (vars R)_{d+n..numgens R - 1});
--      i = 0;
--      tempList = {};
--      while i < d do (
-- 	  tempList = append(tempList,
-- 	       sum(1..n, j -> (A^{i}_(0,j-1))*x_j*D_j) - s_(i+1));
-- 	  i = i+1;
-- 	  );     
--      (StoR J) + ideal(tempList))
-- ///

-- Appell F1 system --
AppellF1 = method(Options => {Vars => Global})
AppellF1 List := options -> w -> (
     if #w != 4 then error "expected list of 4 parameters";
     if options.Vars == Local then (
     	  u := symbol u;
     	  v := symbol v;
     	  Du := symbol Du;
     	  Dv := symbol Dv;
     	  W := QQ[u, v, Du, Dv, 
	       WeylAlgebra => {u=>Du, v=>Dv}];
     	  I := ideal(u*Du*(u*Du+v*Dv+w#3-1) - u*(u*Du+v*Dv+w#0)*(u*Du+w#1),
	       v*Dv*(u*Du+v*Dv+w#3-1) - v*(u*Du+v*Dv+w#0)*(v*Dv+w#2),
	       (u-v)*Du*Dv - w#2*Du + w#1*Dv);
	  )
     else (
	  x := symbol x; y := symbol y; Dx := symbol Dx; Dy := symbol Dy; 
     	  W = QQ[x, y, Dx, Dy, 
	       WeylAlgebra => {x=>Dx, y=>Dy}];
     	  I = ideal(x*Dx*(x*Dx+y*Dy+w#3-1) - x*(x*Dx+y*Dy+w#0)*(x*Dx+w#1),
	       y*Dy*(x*Dx+y*Dy+w#3-1) - y*(x*Dx+y*Dy+w#0)*(y*Dy+w#2),
	       (x-y)*Dx*Dy - w#2*Dx + w#1*Dy);
	  );
     --J = ideal(I_2, I_1+y^2*I_2); 
     I)


--------------------------------------
-- Other things
--------------------------------------

-- This routine takes a polynomial element f of the Weyl algebra 
-- and returns its annihilator ideal.
PolyAnn = method()
PolyAnn RingElement := f -> (
     W := ring f;
     createDpairs W;
     dpV := W.dpairVars;
     -- error checking
     if W.monoid.Options.WeylAlgebra === {} then
     error "Expected element of a Weyl algebra";
     if substitute(f, (dpV#1 | dpV#2) / (i->(i=>0))) != f then
     error "Expected polynomial element of Weyl algebra";
     if W.monoid.Options.Degrees =!= toList(numgens W:{1}) then
     error "Expect all degrees in a Weyl algebra to be 1";
     tempL := (dpV#1 / (i -> 2*f*i - i*f));  -- (fD_i - df/dx_i)
     suffHigh := (degree f)#0 + 1;
     tempL = join(tempL, (dpV#1 / (i -> i^suffHigh)));  -- D_i^(large m)
     ideal tempL
     )

-- This routine takes a polynomial element f of the Weyl algebra 
-- and returns the annihilator ideal of 1/f, or takes two polynomials
-- g and f and returns the annihilator ideal of g/f
RatAnn = method()
RatAnn RingElement := f -> (
     W := ring f;
     RatAnn(1_W, f)
     )

RatAnn(RingElement, RingElement) := (g,f) -> (
     W := ring f;
     createDpairs W;
     dpV := W.dpairVars;
     -- error checking
     if W =!= ring g then
     error "Expected elements of the same ring";
     if W.monoid.Options.WeylAlgebra === {} then
     error "Expected element of a Weyl algebra";
     if substitute(f, (dpV#1 | dpV#2) / (i->(i=>0))) != f then
     error "Expected polynomial element of Weyl algebra";
     if substitute(g, (dpV#1 | dpV#2) / (i->(i=>0))) != g then
     error "Expected polynomial element of Weyl algebra";
     if W.monoid.Options.Degrees =!= toList(numgens W:{1}) then
     error "Expect all degrees in a Weyl algebra to be 1";

     -- get min root of b-function
     a := min getIntRoots globalBFunction f;

     IFs := AnnFs f;
     WFs := ring IFs;
     nFs := numgens WFs;
     Ia := substitute ( substitute(IFs, {WFs_(nFs-1) => a}), W);
     
     if a == -1 and g == 1_W then Ia
     else (
	  compensate := -1 - a;
	  F := map(W^1/Ia, W^1, matrix{{g*f^compensate}});
	  ideal mingens kernel F)
     )

-- reiffen curves
reiffen = method()
reiffen(ZZ,ZZ) := (p,q) -> (
     assert(p>=4 and q>=p+1);
     n := 2; 
     x := symbol x;
     R := QQ[x_1..x_n]; 
     x_1^p+x_2^q+x_1*x_2^(q-1) 
     )
