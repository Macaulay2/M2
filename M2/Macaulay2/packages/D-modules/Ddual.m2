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
