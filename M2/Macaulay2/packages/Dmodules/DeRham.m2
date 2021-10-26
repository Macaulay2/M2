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

end -- the stuff after this point sets global variables and appears to be unused

makeTauInputTest=method();
makeTauInputTest ZZ:= dummy ->(
  S=QQ[x,y];
  -- ff=x*y*(x-y);
  -- mysyzz={{-3,x,y},{2*x-y,-x^2+x*y,0}};
  -- logco = logCohomology(ff);
  ff := (x^3+y^4+x*y^3)*(x^2+y^2);
  mysyzz:={{9*y-5/2*x-43/6*y^2+115/6*y*x-6*y^3, -2*y*x+1/2*x^2+x*y^2-23/6*y*x^2-5/6*y^3+x*y^3, 
           -3/2*y^2+1/2*y*x+4/3*y^3+1/2*x^2-3*x*y^2+y^4+1/3*y*x^2}, 
          {12/5*y^2+22/75*y*x+46/15*x^2-24/25*x*y^2, -8/15*x*y^2-2/25*y*x^2-46/75*x^3+4/25*y^2*x^2, 
           -2/5*y^3-2/75*x*y^2-12/25*y*x^2+4/25*x*y^3+4/75*x^3}};
  logco := logCohomology(ff);
  ans := makeTauInput({ff,mysyzz},logco,1);
  "t.txt" << toString ans << endl << close ;
  ans  
)

makeTauInput=method();
makeTauInput (List,HashTable,ZZ) := (fmysyz,cohom,k) -> (
  f:=fmysyz#0;
  mysyz:=fmysyz#1;
  tkreduced := getReducedTransfer(cohom,k);

  -- check if the syzygy is OK.
  iii := (((cohom#VResolution).dd)#1 );
  ggg := gb iii;
  ggg2 := entries gens ggg; ggg2 = ggg2#0;
  W := ring(ggg2#0);
  vw := generators(W);
  i:=0; j:=0; ell:=0; t:=0;
  op:={};
  m:=#mysyz;
  n := numgens(W) // 2;
  while i<m do (  
    t = mysyz#i#0;
    j = 1; ell=-value(toString(t));
    while j<=n do (
      t = mysyz#i#j;
      ell = ell + value(toString(t))*vw_(n+j-1);
      j = j+1;
    );
    op = join(op,{ell});
    i = i+1;
  );
  gggell := gb ideal op;
  -- check if op % ggg == 0, ggg2 % op == 0
  apply(op,ell-> (if ((ell % ggg) != 0) then (print(ell,ggg); ell2=ell; error "syzygy error by ggg")));
  apply(ggg2,ell-> (if ((ell % gggell) != 0) then (print(ell,gggell); ell2=ell; error "syzygy error by gggell")));

  pInfo(2, (mysyz#0#1)*(mysyz#1#2)-(mysyz#0#2)*(mysyz#1#1));
  pInfo(2, f);

  abcd:={{mysyz#1#2,-mysyz#1#1},{-mysyz#0#2,mysyz#0#1}};  -- {{a,b},{c,d}}  in logc2.tex
  {abcd,op,tkreduced}
)
