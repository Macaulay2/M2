-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

-----------------------------------------
-- GKZ related things
-----------------------------------------

-- internal
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

--Takes a Weyl algebra in n vars and dxn matrix and gives a list of d Euler Operators
eulerOperators = method();
eulerOperators (Matrix, List, PolynomialRing) := (A, b, W) -> (
    gkzInputValidation(A, b, W);
    apply(numRows A, i -> sum(numColumns A, j -> A_(i,j) * W.dpairVars#0#j * W.dpairVars#1#j) - b#i)
    )
eulerOperators (Matrix, PolynomialRing) := (A, W) -> eulerOperators(A, toList((numRows A): 0), W)


-- Constructs a toric ideal in the partials as an ideal in the Weyl algebra
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

TEST///
------------------------------------
-- A = {{1,1,1},{0,1,2}}; custom W
------------------------------------
W = makeWeylAlgebra(QQ[x_0..x_2]);
A = matrix{{1,1,1},{0,1,2}};

for b in {{1,2}, {-1,3}, {1/2, 1/3}} do (
    gkzIdl = 
    gkz(A,b,W);
    correctGkzIdl = ideal( dx_1^2 - dx_0*dx_2,
			   x_0*dx_0 + x_1*dx_1 + x_2*dx_2 - b_0,
			   x_1*dx_1 + 2*x_2*dx_2 - b_1 );
    assert(gkzIdl == correctGkzIdl);
    );
///

TEST///
---------------------------------------
-- A = {{1,1,1},{0,1,2}}; no W
---------------------------------------
A = matrix{{1,1,1},{0,1,2}};
x_1 = "test1";
x_2 = "test2";
x_3 = "test3";
D_1 = "test4";
D_2 = "test5";
D_3 = "test6";
for b in {{1,2}, {-1,3}, {1/2, 1/3}} do (
    gkzIdl = gkz(A, b);
    
    -- Confirm that gkz didn't change x_i's or D_i's
    assert(x_1 == "test1");
    assert(x_2 == "test2");
    assert(x_3 == "test3");
    assert(D_1 == "test4");
    assert(D_2 == "test5");
    assert(D_3 == "test6");
    
    W' = ring gkzIdl;
    correctGkzIdl = ideal( W'_4^2 - W'_3*W'_5,
			   W'_0*W'_3 + W'_1*W'_4 + W'_2*W'_5 - b_0,
			   W'_1*W'_4 + 2*W'_2*W'_5 - b_1);
    assert(gkzIdl == correctGkzIdl);
    );
///


TEST///
---------------------------------------
-- A = {{1,1},{0,1}}; W;
---------------------------------------
W = makeWeylAlgebra(QQ[x,y]);
A = matrix{{1,1},{0,1}};
b = {0,0};
gkzIdl = gkz(A, b, W);
correctGkzIdl = ideal(x*dx + y*dy, y*dy);
assert(gkzIdl == correctGkzIdl);
///



TEST///
----------------------------
-- A = {{1,1},{0,1}}; no b
----------------------------
W = makeWeylAlgebra(QQ[x,y]);
A = matrix{{1,1},{0,1}};
correctEuls = {x*dx + y*dy, y*dy};
assert(eulerOperators(A, W) == correctEuls);
///

TEST///
----------------------------
-- A = {{1,1},{0,1}}; b
----------------------------
W = makeWeylAlgebra(QQ[x,y]);
A = matrix{{1,1},{0,1}};
b = {1,2}
correctEuls = {x*dx + y*dy - 1, y*dy - 2};
assert(eulerOperators(A, b, W) == correctEuls);
///

TEST///
----------------------------
-- A = {{1,1,1},{0,1,2}};
----------------------------
W = makeWeylAlgebra(QQ[x,y,z]);
A = matrix{{1,1,1},{0,1,2}};
I = toricIdealPartials(A, W);
R = ring I;
correctI = ideal(R_1^2 - R_0*R_2);
assert(I == correctI);
///
