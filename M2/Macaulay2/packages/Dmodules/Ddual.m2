-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- These routines compute the dual of a holonomic D-module M, which is
-- the transposition of the right module Ext^n_D(M,D)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
Ddual = method()
Ddual Ideal := I -> (
     Ddual ((ring I)^1/I)
     )

Ddual Module := M -> (
     pInfo(1, "ENTERING Ddual ... ");
     W := ring M;
     if W.monoid.Options.WeylAlgebra === {} then
     error "expected an element of a Weyl algebra";
     pInfo (1, "Ddual: holonomicity check ...");
     if not isHolonomic M then
     error "expected a holonomic module";
     createDpairs W;
     n := #W.dpairVars#0; 
     outputList := {};
     
     C := Dres(M, LengthLimit => n+1);
     m0 := transpose Dtransposition C.dd#n;
     m1 := transpose Dtransposition C.dd#(n+1);  
     
     F0 := map(C#n, C#(n-1), m0);
     F1 := map(C#(n+1), C#n, m1);
     
     pres := presentation homology(F1,F0);
     dualM := cokernel zeroize Dprune pres;

     dualM
     )

TEST ///
-- dual of an Appell F1
I = AppellF1({2,-3,-2,5});
assert(ideal relations Ddual I == 
     substitute(AppellF1({-1,4,2,-3}), vars ring I));

-- dual of gkz associated to quadratic equation
A = matrix{{1,1,1},{0,1,2}};
b = {8/3,9/17};
I = gkz(A,b);
assert(ideal relations Ddual I == substitute(gkz(A,-b-{1,1}), vars ring I));
///