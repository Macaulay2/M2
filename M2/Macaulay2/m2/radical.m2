-- Based on the Macaulay (classic) scripts written by 
-- D. Eisenbud.  Translated to Macaulay2 by M. Stillman

-- Copyright 1996 by Michael E. Stillman

-- translated: remove_low_dim --> topComponents
--             remove_low_dim_id --> topComponents
--             remove_lowest_dim --> removeLowestDimension
--             radical --> radical
--             unmixed_radical --> radical(I,Unmixed=>true)

---------------------------
-- removeLowestDimension --
---------------------------

removeLowestDimension = method()
removeLowestDimension Module := Module => (M) -> (
     -- only works for polynomial rings...
    local E;
    R := ring M;
    c := codim M;
    p := pdim M;
    -- now loop (starting at p) trying to find the largest
    -- d such that codim Ext^d(M,R) == d
    while p > c and codim (E = Ext^p(M,R)) > p do p = p-1;
    if p == c then (
        -- M is C.M. and unmixed, so return (1):
        ambient M
        )
    else (
        -- use the annihilator of Ext to improve M
        I := annihilator E;
        cokernel generators saturate(image presentation M,I))
    )
removeLowestDimension Ideal := Ideal => (I) -> (
     -- only works for polynomial rings...
    local E;
    M := cokernel generators I;
    R := ring M;
    c := codim M;
    p := pdim M;
    -- now loop (starting at p) trying to find the largest
    -- d such that codim Ext^d(M,R) == d
    while p > c and codim (E = Ext^p(M,R)) > p do p = p-1;
    if p == c then (
        -- M is C.M. and unmixed, so return (1):
        ideal(1_R)
        )
    else (
        -- use the annihilator of Ext to improve M
        J := annihilator E;
        saturate(I,J))
    )

---------------------------
-- top dimensional part ---
---------------------------

-- TODO: move this to PrimaryDecomposition?
-- it is documented in packages/Macaulay2Doc/functions/top-doc.m2
topComponents = method()
topComponents Ideal := Ideal => (I) -> (
     R := ring I;
     c := codim I;
     annihilator Ext^c(cokernel generators I, R))

topComponents Module := Module => (M) -> (
    R := ring M;
    if not isPolynomialRing R or not isAffineRing R
    then error "expected a polynomial ring";
    c := codim M;
    p := pdim M;  -- will compute a resolution if needed...
    while p > c do (
	E := minimalPresentation Ext^p(M,R);
	if E != 0 and codim E === p then (
	    -- improve M
	    J := annihilator E;
	    I := saturate(M, J);
	    -- alternate strategy: modify M as well:
	    -- this next line could be commented out
	    M = (ambient I)/I;
	);
	if pdim M < p 
	  then p = pdim M
	  else p = p-1;
	);
    M
    )

-- This used to be commented out in modules2.m2
-- if it isn't useful anymore, delete it
--topComponents Module := M -> (
--     R := ring M;
--     c := codim M;
--     annihilator minimalPresentation Ext^c(M, R))
--document { topComponents,
--     TT "topComponents M", "produce the annihilator of Ext^c(M, R), where c
--     is the codimension of the support of the module M."
--     }

-------------
-- radical --
-------------

unmixedradical := (I) -> (
     -- First lift I to a polynomial ring...
     A := ring I;
     f := presentation A;
     B := ring f;
     I = lift(I,B);
     if I != ideal(1_B) and 
        I.generators =!= 0 
     then (
    	  c := codim I;
    	  size := 1;
	  R := A;
    	  while size <= c do (
	       R = B/I;
	       dR := jacobian R;
      	       J := minors(size,dR);

	       g1 := leadTerm generators gb presentation R;
	       g1 = g1 | lift(leadTerm J, B);

      	       if codim ideal g1 > c
	       then size = size+1
      	       else (
		    -- we would like the next line to read:
		    -- I = annihilator J;
		    I = ideal syz(transpose mingens J, 
		                  SyzygyRows=>1);
		    I = lift(I,B); 
		    );
      	       );
	  );
     trim (I*A)
     )
-- unmixed radical, another Eisenbud-Huneke-Vasconcelos method
-- to compute radical(m), given a max regular sequence n contained in m.

unmixedradical2 := (J, CI) -> (
  if ring J =!= ring CI then 
      error "unmixedradical: expected ideals to be in the same ring";
  D := jacobian CI;
  c := numgens CI;  -- we assume that this is a complete intersection...
  K := CI : minors(c, D);  -- maybe work mod CI?
  K : (K : J)) -- do these mod K?

radical1 := (I) -> (
    -- possibly massage input, by removing obvious extraneous powers?
    -- at least of the monomials in the ideal?
    R := ring I;
    I1 := removeLowestDimension I;
    J := saturate(I, I1);
    J = unmixedradical J;
    if I1 == ideal(1_R)
        then J 
        else intersect(J, radical1 I1))

protect Decompose
radical Ideal := Ideal => options -> (I) -> (
     if isMonomialIdeal I then
          radical monomialIdeal I
     else if class options.CompleteIntersection === Ideal then
          unmixedradical2(I,options.CompleteIntersection)
     else if options.Unmixed then 
          unmixedradical I
     else if options.Strategy === Decompose then (
     	  C := minimalPrimes I;
	  if #C === 0 
	    then ideal(1_(ring I))
            else intersect C)
     else if options.Strategy === Unmixed then
          radical1 I
     else error "radical Ideal: unrecognized strategy"
     )


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
