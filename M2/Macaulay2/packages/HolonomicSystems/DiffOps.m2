-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

------------------------------------------------------------------------
-- This function computes the differential operators of order k or less
-- on an affine variety defined by an ideal I in a polynomial ring
------------------------------------------------------------------------
diffOps = method()
diffOps (RingElement, ZZ) := (f, k) -> (diffOps(ideal f, k))

diffOps (Ideal, ZZ) := (I, k) -> (
     R := ring I;
     W := makeWeylAlgebra( R, SetVariables=>false);
     createDpairs W;
     
     -- make coeffs of derivations
     F := gens I;

     -- make Dk
     Dbasis := matrix {flatten apply( toList(0..k-1), i -> (
	       (entries symmetricPower(k-i, matrix{W.dpairVars#1}))#0 ) )};
     xbasis := matrix {flatten apply( toList(0..k-1), i -> (
	       (entries symmetricPower(k-i, vars R))#0 ) )};
     smallxbasis := matrix {flatten apply( toList(0..k-2), i -> (
	       (entries symmetricPower(k-1-i, vars R))#0 ) )};
     
     pInfo(1, "Making the matrix of partial derivatives of elements in I...");
     temp := transpose diff(transpose xbasis, F | 
	       flatten ((transpose smallxbasis)*F) );

     addon := directSum toList(rank target temp:matrix{{F}});
          
     full := temp | addon;
     pInfo(1, "Computing syzygies on an " | rank source full | " by " |
	  rank target full | " matrix...");
     syzy := (syz full)^{0..rank source temp - 1}; 
     pInfo(1, "Reducing matrix of " | rank source syzy | " by " |
	  rank target syzy | " syzygies with respect to I...");
     syzy = gens gb (syzy % I);
     
     hashTable {PolyGens => syzy, BasisElts => Dbasis}
     )

----------------------------------------------------------------
-- Puts Rbasis of diffOps into the Weyl algebra representation
----------------------------------------------------------------
putWeylAlgebra = method()
putWeylAlgebra (HashTable) := (h) -> (
     (h.BasisElts) * substitute(h.PolyGens, ring h.BasisElts)
     )
