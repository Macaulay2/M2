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

topComponents (Module, ZZ) := Module => (M, e) -> (
     S := ring M;
     if not isPolynomialRing S or not isAffineRing S then error "expected a polynomial ring";
     N := 0*M;
     f := pdim M;  -- will compute a resolution if needed...
     while f > e do (
          E := Ext^f(M,S);
          if codim E == f then (
               if debugLevel > 0 then print("Removing components of codim " | toString(f));
               N = N : annihilator E;
          );
          f = f-1;
     );
     N
)

topComponents (Module, Module, ZZ) := (M, N, e) -> (
     Q := M/N;
     if debugLevel > 0 then print("Determining strategy for top components...");
     strat := "ExtAsHom"; 
     try ( alarm 30; if sum values betti resolution Q < 1000 then strat = "ExtViaRes" );
     if debugLevel > 0 then print("Using strategy " | strat);
     C := if strat == "ExtViaRes" then topComponents(Q, e) else equidimHull Q;
     trim subquotient(generators C | generators N, relations M)
)

equidimHull = method()
equidimHull Module := Module => M -> ( -- equidimensional hull of 0 in a module M
     R := ring M;
     if debugLevel > 0 then print("Finding maximal regular sequence...");
     S := comodule maxRegSeq annihilator M;
     -- G := mingens ann M;
     -- c := codim M;
     -- S := comodule ideal(G*random(R^(numcols G), R^c)); -- should be (the quotient by) a maximal R-regular sequence contained in ann(M)
     if debugLevel > 0 then print("Computing Ext ...");
     if numColumns mingens M == 1 then (
          if debugLevel > 0 then print("Using case for cyclic module...");
          E := Hom(M, S); -- = Ext^c(M, R) by choice of S
          if debugLevel > 0 then print("Getting annihilator ...");
          return subquotient(generators annihilator E, relations M);
     );
     -- the following uses code from doubleDualMap in AnalyzeSheafOnP1
     h := coverMap M;
     ddh := Hom(Hom(h, S), S); -- = Ext^c(Ext^c(h, R), R) by choice of S
     if debugLevel > 0 then print("Getting hull as kernel of " | toString(numRows matrix ddh) | " by " | toString(numColumns matrix ddh) | " matrix...");
     kernel map(target ddh, M, matrix ddh)
)

maxRegSeq = method(Options => {Strategy => "Quick"})
maxRegSeq Ideal := Ideal => opts -> I -> (
     -- attempts to find sparse maximal regular sequence contained in an ideal
     -- ideal should be in a CM ring (with sufficiently large coefficientRing)
     G := sort flatten entries mingens I;
     t := timing codim I;
     c := last t;
     t0 := 1 + ceiling first t;
     if c == #G then return ideal G;
     k := coefficientRing ring I;
     J := ideal(G#0);
     for i from 1 to c-1 do (
          (j, foundNextNZD) := (#G-1, false); -- starts searching from end
          while not foundNextNZD and j >= c do (
               coeffList := {0_k, 1_k, random k};
               if debugLevel > 0 then print("Trying generators " | toString(i, j));
               for a in coeffList do (
                    cand := G#i + a*G#j;
                    K := J + ideal cand;
                    if debugLevel > 0 then print("Testing regular sequence...");
                    n := if opts.Strategy == "Quick" then ( 
                         try ( alarm t0; codim K ) else infinity
                    ) else codim K;
                    if n == 1 + #J_* then (
                         J = K;
                         foundNextNZD = true;
                         break;
                    );
               );
               j = j-1;
          );
     );
     J
)

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
