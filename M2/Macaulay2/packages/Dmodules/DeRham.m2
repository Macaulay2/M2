-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Computes deRham cohomology of a smooth affine variety. For more options on localCohom strategy use ICcohom instead.
-- Special algorithm for the de Rham cohomolgy of K^n - V(f) using the algorithm of Oaku-Takayama.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------



deRham = method( Options => {Strategy => Schreyer})
deRham Ideal := options -> I -> (
    R:= ring I;
    if class R =!= PolynomialRing
    then error "expected an ideal in a polynomial ring";
    primes := minimalPrimes I;
    if #primes > 1 then error "The variety defined by the ideal must be irreducible";
    P := primes#0;
    S := ideal (singularLocus P);
    if R != S then error "expected an ideal defining a smooth affine variety";
    n:= numgens R;
    d:= dim I;
    c:= n-d;
    H:=localCohom(c,I);
    M:=minimalPresentation H;
    integrateTable := Dintegration(M, apply(n,i->1), options);
    new HashTable from (for i from 0 to d list i=>integrateTable#(d-i))
)

deRham (ZZ, Ideal) := options -> (k, I) -> (    
    R:= ring I;
    if class R =!= PolynomialRing
    then error "expected an ideal in a polynomial ring";
    primes := minimalPrimes I;
    if #primes > 1 then error "The variety defined by the ideal must be irreducible";
    P := primes#0;
    S := ideal (singularLocus P);
    if R != S then error "expected an ideal defining a smooth affine variety";
    n:= numgens R;
    d:= dim I;
    if k>d then 0
    else (
    	c:= n-d;
    	H:=localCohom(c,I);
    	M:=minimalPresentation H;
    	Dintegration(d-k, M, apply(n,i->1),options)
	)
)



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

-- iAllt is by Nobuki Takayama, 2007.
-- iAllt(I) computes integration cohomology groups of D/I with transfer's
-- logCohomology(I) computes DR(tilde M).
-- 
-- Example:
-- M2   
-- load "D-modules.m2"
-- load "trans.m2"
-- use S; f=x*y*(x-y);  logCohomology(f);

protect Input						    -- not exported?

iAllt = method( Options => {Strategy => Schreyer})
iAllt Module:= options -> I -> (
     pInfo(2, "iAllt (integrationAllWithTransfer) in trans.m2");
     WA := ring gens I;
     Mf := I;
     pInfo(2,Mf);
     n := numgens(WA) // 2;  -- do not use numgens(WA)/2 
     w := toList(n:1);

     outputRequest := {GenCycles, HomologyModules,
      VResolution, BFunction, Explicit};
     MF := cokernel Fourier relations Mf; 
     outputTable := computeRestriction (MF, w, -1, n+1,
      outputRequest, options);
     outputList := {BFunction => outputTable#BFunction,
      LocalizeMap => LocMap};
     outputList = outputList | { Input=> I };
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
          pInfo(2,dbl); -- print double complex
          transfers := {};
          i := 0;
          while i <= n do (
           pull := intTable#PreCycles#i;
           j := 0;
           pInfo(2,{i,j,pull});
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
                     pInfo(2,{i,j,push,pull}); 
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

-- derLogF returns generators for widetilde{Der}(-log f)
-- Example:  QQ[x,y]
--           f = x*y*(x-y)
--           iAllt cokernel matrix derLogF(f)
derLogF = method();
derLogF RingElement := f -> (
  R := ring f;
  if class R =!= PolynomialRing
  then error "expected element of a polynomial ring";
  if R.monoid.Options.WeylAlgebra =!= {}
  then error "expected element of a commutative polynomial ring";
  v := generators(R);
  n := #v;
  d := apply(v, i->diff(i,f));
  d = join({f},d);
  syzf := kernel matrix{d};
  m := numgens syzf;
  -- msyz = gens syzf;
  msyz := gens syzf;
                                 --  note: msyz^{1}  get 1-th row
                                 --  note: msyz_{i} get i-th column
                                 --  note: msyz^{i}_{j} get (i,j)-element
                                 --  note: entries 
                                 --  note: #List,  length
  pInfo(2,msyz);
  -- see annFs.m2, oxclient.m2
  W := makeWeylAlgebra( R, SetVariables=>false);   --note: g=map(R,W,matrix{{x,y,0,0}}); g(x) does not work  
  phi := map(W,R);
  vw := generators(W);
  i:=0; j:=0; ell:=0; t:=0;
  op:={};
  while i<m do (  
    j = 1;
    ell=-phi msyz_(0,i);
    while j<=n do (
      ell = ell + phi msyz_(j,i) * vw_(n+j-1);
      j = j+1;
    );
    op = join(op,{ell});
    i = i+1;
  );
  {op}
)

logCohomology=method();
logCohomology RingElement := f -> (
     ii := derLogF f;
     iAllt(cokernel(matrix(ii)))
)

-- use S; logCohomology(x^3-y)   
-- Warning: do not input like logCohomology x^3-y

-- Example:
-- use S; f=(x^3+y^4+x*y^3)*(x^2+y^2); logCohomology f;
--  the transfer cycles contains dx and dy.
-- use S; f=(x^3+y^4+x*y^3)*(x^2-y^2); logCohomology f;
--  the transfer cycles contains dx and dy.

getTransfer=method();
getTransfer (HashTable,ZZ) := (cohom,k) -> (
  t:=cohom#TransferCycles;
  tk:=transpose(t)#k;
  entries(tk)
)

-- Example:
-- use S; f=x*y*(x-y); cc=logCohomology f; getReducedTransfer(cc,1);
-- use S; f=(x^3+y^4+x*y^3)*(x^2+y^2); cc=logCohomology f; getReducedTransfer(cc,1);
getReducedTransfer=method();
getReducedTransfer (HashTable,ZZ) := (cohom,k) -> (
  tmp:=cohom;
  tk:=getTransfer(cohom,k);
  -- cf. Dbasic.m2, holonomicRank
  W := ring(cohom#VResolution);
  createDpairs W;
  n := #(W.dpairInds#0);
  m := numgens W;
  -- get weight vectors for the order filtration refined 
  -- by lex on the derivatives
  weightList := { apply ( toList(0..m-1), i -> if member(i, W.dpairInds#1) 
     then 1 else 0 ) };
  -- ring equipped with the new order
  tempW := (coefficientRing W)(monoid [(entries vars W)#0,
            WeylAlgebra => W.monoid.Options.WeylAlgebra,
            Weights => weightList]);
  WtotempW := map (tempW, W, vars tempW);
  -- map to the lex ring.
  tktemp := apply(tk,i->apply(i,j->WtotempW(j)));
  iii := WtotempW( ((cohom#VResolution).dd)#1 );
  ggg := gb iii;
  tkreduced := apply(tktemp,i->apply(i,j-> (j % ggg)))
)

TEST ///
--Boundary cases
x = symbol x; y = symbol y;
R = QQ[x,y]
default := hashTable {0=>QQ^1, 1=>QQ^0, 2=>QQ^0};
F2 = deRham(1_R); -- affine space
assert all (keys default, i -> (F2#i == default#i));

--Small change doesn't affect deRham groups
F1 = deRham(x^2-30*y^3)
F2 = deRham(x^2-31*y^3)
assert all (keys F1, i -> (F1#i == F2#i));

--These lead to problems in factor at the moment
F1 = deRham(x^2-y^10)
F2 = deRham(2*x^2 - 3*y^10)
assert all (keys F1, i -> (F1#i == F2#i));    
///
