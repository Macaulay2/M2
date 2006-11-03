-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- this routine computes the de Rham cohomolgy of K^n - V(f) using the
-- algorithm of Oaku-Takayama.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
deRham = method( Options => {Strategy => Schreyer})
deRham RingElement := options -> f -> (
     pInfo(1, "ENTERING deRham ...");     
     R := ring f;
     if class R =!= PolynomialRing
     then error "expected element of a polynomial ring";
     if R.monoid.Options.WeylAlgebra =!= {}
     then error "expected element of a commutative polynomial ring";
     
     outputList := {};
     n := numgens R;
     x := symbol x;
     D := symbol D;
     WA := (coefficientRing R)[x_1 .. x_n, D_1 .. D_n,
	  WeylAlgebra => apply(toList(1..n), i -> x_i => D_i)];
     RtoWA := map(WA, R, (vars WA)_(toList(0..n-1)));
     M := cokernel matrix{{D_1 .. D_n}};
     locMod := Dlocalize(M, RtoWA f, Strategy => Oaku);
     w := toList(n:1);
     integrateTable := Dintegration(locMod, w, options);
     homologyTable := hashTable apply(toList(0..n),
	  i -> (-i+n) => integrateTable#i);
     homologyTable
     )

deRham (ZZ, RingElement) := options -> (k, f) -> (
     pInfo(1, "ENTERING deRham ...");     
     R := ring f;
     if class R =!= PolynomialRing
     then error "expected element of a polynomial ring";
     if R.monoid.Options.WeylAlgebra =!= {}
     then error "expected element of a commutative polynomial ring";
     
     outputList := {};
     n := numgens R;
     x := symbol x;
     D := symbol D;
     WA := (coefficientRing R)[x_1 .. x_n, D_1 .. D_n,
	  WeylAlgebra => apply(toList(1..n), i -> x_i => D_i)];
     RtoWA := map(WA, R, (vars WA)_(toList(0..n-1)));
     M := cokernel matrix{{D_1 .. D_n}};
     locMod := Dlocalize(M, RtoWA f, Strategy => Oaku);
     w := toList(n:1);
     homologyModule := Dintegration(n-k, locMod, w, options);
     homologyModule
     )

deRhamAll = method( Options => {Strategy => Schreyer})
deRhamAll RingElement := options -> f -> (
     pInfo(1, "ENTERING deRhamAll ...");     
     R := ring f;
    if class R =!= PolynomialRing
     then error "expected element of a polynomial ring";
     if R.monoid.Options.WeylAlgebra =!= {}
     then error "expected element of a commutative polynomial ring";
     
     n := numgens R;
     x := symbol x;
     D := symbol D;
     WA := (coefficientRing R)[x_1 .. x_n, D_1 .. D_n,
	  WeylAlgebra => apply(toList(1..n), i -> x_i => D_i)];
     RtoWA := map(WA, R, (vars WA)_(toList(0..n-1)));
     M := cokernel matrix{{D_1 .. D_n}};
     WAf := RtoWA f;
     LocMap := DlocalizeMap(M, WAf, Strategy => Oaku);
     Mf := target LocMap;
     w := toList(n:1);
     
     outputRequest := {GenCycles, HomologyModules,
	  VResolution, BFunction, Explicit};
     MF := cokernel Fourier relations Mf; 
     outputTable := computeRestriction (MF, w, -1, n+1,
	  outputRequest, options);
     outputList := {BFunction => outputTable#BFunction,
	  LocalizeMap => LocMap};
     if outputTable#VResolution =!= null then 
     outputList = append(outputList, 
	  VResolution => FourierInverse outputTable#VResolution)
     else outputList = append(outputList, VResolution => null);
     
     outputList = outputList | {
	  PreCycles => hashTable apply(toList(0..n), 
	       i -> (n-i => FourierInverse outputTable#GenCycles#i)),
	  CohomologyGroups => hashTable apply(toList(0..n), 
	       i -> (n-i => outputTable#HomologyModules#i)) };
     
     intTable := hashTable outputList;
     
     --P := DintegrationDeRham(Mf, w, Strategy => options.Strategy,
     --     Info => options.Info, ExplicitHomology => options.ExplicitHomology);

     if intTable#VResolution =!= null then (     
     	  pInfo(1, "Making the Koszul complex... ");
     	  createDpairs WA;
     	  Omega := Dres ideal WA.dpairVars#1;
     	  pInfo(1, "Transferring cohomology classes to deRham complex... ");
     	  dbl := Omega**(intTable#VResolution);
     	  transfers := {};
     	  i := 0;
     	  while i <= n do (
	       pull := intTable#PreCycles#i;
	       j := 0;
	       pInfo(2, "\t Degree " | i | "...");
	       tInfo := toString first timing (
	       	    while j < n-i do (
	            	 vertMap := zeroize dbl#(n-i-1)^[(j,-j+n-i-1)]*
	            	 (dbl.dd#(n-i))*(dbl#(n-i)_[(j,-j+n-i)]);
	            	 push := vertMap*pull;
	            	 horMap := zeroize dbl#(n-i-1)^[(j,-j+n-i-1)]*
	            	 (dbl.dd#(n-i))*(dbl#(n-i)_[(j+1,-j+n-i-1)]);
	            	 if ((Dtransposition push) % (Dtransposition horMap) != 0)
	            	 then error "syzygy should be produced but wasn't!";
	            	 pull = (Dtransposition push) // (Dtransposition horMap);
	            	 pull = Dtransposition pull;
	            	 j = j+1;
	            	 );
	       	    );
	       pInfo(2, "\t\t\t time = " | tInfo | " seconds" );
	       transfers = append(transfers, i => pull);
	       i = i+1;
	       );
     	  outputList = outputList | {TransferCycles => 
	       hashTable transfers, OmegaRes => Omega};
     	  );
     hashTable outputList
     )
