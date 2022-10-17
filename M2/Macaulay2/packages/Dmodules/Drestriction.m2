-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- These local routines are needed for Drestriction
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This routine computes a monomial from a list of variables 
-- and an exponent vector
List ^ List := (Vars, Exps) -> (
     product (Vars, Exps, (i,j) -> (i^j)) )

-- This routine takes a weight vector w of strictly positive integers
-- and returns the exponents beta in N^n such that 
-- ( k_0 < w_1 beta_1 + ... + w_n bet_n <= k_1) 
findExps := (w, k0, k1) -> (
     local minimum, local alpha, local tempExps;
     -- base case: weight vector w has dim 1
     if #w == 1 and k0 >= 0 then (
     	  tempExps = (toList((k0//w#0+1)..k1//w#0) / (i -> {i}) ) )
     else if #w == 1 and k0 < 0 then (
	  tempExps = ( toList(0..k1//w#0) / (i -> {i}) ) )
     else ( -- general case
	  tempExps = {};
	  alpha = 0;
	  while alpha <= k1//w#0 do (
	       tempExps = join( tempExps,
		    apply ( findExps( drop(w,1), k0-alpha*(w#0), 
			      k1-alpha*(w#0) ), j -> prepend(alpha,j) ) );
	       alpha = alpha+1;
	       );
	  );
     tempExps)


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--
-- Computes the derived restriction complex of D_n/I or (D_n)^r/N
-- with respect to the weight vector w
--     
-- Computes the p-th derived restriction module of D_n/I or (D_n)^r/N 
-- with respect to the weight vector w
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Drestrict = method( Options => {Strategy => Schreyer} )
Drestrict(Ideal,List)  := options -> (I,w) -> (     print("WARNING! Drestrict is an obsolete name for Drestriction");
Drestriction(I,w,options))
Drestrict(Module,List) := options -> (M,w) -> (     print("WARNING! Drestrict is an obsolete name for Drestriction");
Drestriction(M,w,options))
Drestrict(ZZ,Ideal,List) := options->(k,I,w)->(     print("WARNING! Drestrict is an obsolete name for Drestriction");
Drestriction(k,I,w,options))
Drestrict(ZZ,Module,List) := options->(k,M,w)->(     print("WARNING! Drestrict is an obsolete name for Drestriction");
Drestriction(k,M,w,options))

Drestriction = method( Options => {Strategy => Schreyer} )
Drestriction(Ideal,List) := options -> (I,w) -> (
     Drestriction ((ring I)^1/I, w, options )     )

Drestriction(Module, List) := options -> (M,w) -> (
     ensureQuotientModule(M, 
	  "Drestriction currently only handles quotient modules");
     d := #positions(w, i -> (i>0));
     outputRequest := {HomologyModules};
     outputTable := computeRestriction (M, w, -1, d+1, 
	  outputRequest, options);
     outputTable#HomologyModules
     )

Drestriction(ZZ,Ideal,List) := options -> (k,I,w) -> (
     Drestriction (k, (ring I)^1/I, w, options)     )

Drestriction(ZZ,Module,List) := options -> (k,M,w) -> (
     ensureQuotientModule(M, 
	  "Drestriction currently only handles quotient modules");
     d := #positions(w, i -> (i>0));
     if k > d then output := 0
     else (
     	  outputRequest := {HomologyModules};
     	  outputTable := computeRestriction (M, w, k-1, k+1, 
	       outputRequest, options);
     	  output = outputTable#HomologyModules#k;
	  );
     output
     )

DrestrictClasses = method( Options => {Strategy => Schreyer} )
DrestrictClasses(Ideal,List)  := options -> (I,w) -> (
     print("WARNING! Drestrict is an obsolete name for Drestriction");
     DrestrictionClasses(I,w,options))

DrestrictClasses(Module,List) := options -> (M,w) -> (
     print("WARNING! Drestrict is an obsolete name for Drestriction");
     DrestrictionClasses(M,w,options))

DrestrictClasses(Ideal,List,ZZ) := options->(I,w,k)->(
     print("WARNING! Drestrict is an obsolete name for Drestriction");
     DrestrictionClasses(I,w,k,options))

DrestrictClasses(Module,List,ZZ) := options->(M,w,k)->(
     print("WARNING! Drestrict is an obsolete name for Drestriction");
     DrestrictionClasses(M,w,k,options))


DrestrictionClasses = method( Options => {Strategy => Schreyer} )
DrestrictionClasses(Ideal,List) := options -> (I,w) -> (
     DrestrictionClasses ((ring I)^1/I, w, options)     )

DrestrictionClasses(Module, List) := options -> (M,w) -> (
     ensureQuotientModule(M, 
	  "Drestriction currently only handles quotient modules");
     d := #positions(w, i -> (i>0));
     n := numgens ring M;
     if n == 2*d then outputRequest := {GenCycles,
	  VResolution, Explicit}
     else outputRequest = {Cycles, Boundaries, 
	  VResolution, Explicit};
     outputTable := computeRestriction (M, w, -1, d+1, 
	  outputRequest, options);
     outputTable
     )

DrestrictionClasses(ZZ,Ideal,List) := options -> (k,I,w) -> (
     DrestrictionClasses (k, (ring I)^1/I, w, options)     )

DrestrictionClasses(ZZ,Module,List) := options -> (k,M,w) -> (
     ensureQuotientModule(M, 
	  "Drestriction currently only handles quotient modules");
     d := #positions(w, i -> (i>0));
     W := ring M;
     n := numgens W;
     if k > d then (
	  if n == 2*d then output := hashTable {
	       GenCycles => hashTable {k => gens W^0},
	       VResolution => null}
	  else output = hashTable {
	       Cycles => hashTable {k => gens W^0},
	       Boundaries => hashTable {k => gens W^0},
	       VResolution => null};
	  )
     else (
     	  if n == 2*d then outputRequest := {GenCycles,
	       VResolution,Explicit}
     	  else outputRequest = {Cycles, Boundaries, 
	       VResolution, Explicit};
     	  outputTable := computeRestriction (M, w, k-1, k+1, 
	       outputRequest, options);
     	  output = outputTable;
	  );
     output
     )


DrestrictComplex = method( Options => {Strategy => Schreyer} )
DrestrictComplex(Ideal,List)  := options -> (I,w) -> (
     print("WARNING! Drestrict is an obsolete name for Drestriction");
     DrestrictionComplex(I,w,options))

DrestrictComplex(Module,List) := options -> (M,w) -> (
     print("WARNING! Drestrict is an obsolete name for Drestriction");
     DrestrictionComplex(M,w,options))

DrestrictComplex(Ideal,List,ZZ) := options->(I,w,k)->(
     print("WARNING! Drestrict is an obsolete name for Drestriction");
     DrestrictionComplex(I,w,k,options))

DrestrictComplex(Module,List,ZZ) := options->(M,w,k)->(
     print("WARNING! Drestrict is an obsolete name for Drestriction");
     DrestrictionComplex(M,w,k,options))


DrestrictionComplex = method( Options => {Strategy => Schreyer} )
DrestrictionComplex(Ideal,List) := options -> (I,w) -> (
     DrestrictionComplex ((ring I)^1/I, w, options)     )

protect RestrictComplex

DrestrictionComplex(Module, List) := options -> (M,w) -> (
     ensureQuotientModule(M, 
	  "Drestriction currently only handles quotient modules");
     d := #positions(w, i -> (i>0));
     outputRequest := {RestrictComplex};
     outputTable := computeRestriction (M, w, -1, d+1, 
	  outputRequest, options);
     outputTable#RestrictComplex
     )

DrestrictIdeal = method( Options => {Strategy => Schreyer} )
DrestrictIdeal(Ideal,List)  := options -> (I,w) -> (
     print("WARNING! Drestrict is an obsolete name for Drestriction");
     DrestrictionIdeal(I,w,options))

DrestrictionIdeal = method( Options => {Strategy => Schreyer} )
DrestrictionIdeal(Ideal,List) := options -> (I,w) -> (
     d := #positions(w, i -> (i>0));
     outputRequest := {RestrictComplex, Explicit};
     outputTable := computeRestriction ((ring I)^1/I, w, -1, 1, 
	  outputRequest, options);
     M := cokernel (outputTable#RestrictComplex).dd#1;
     R := ring M;
     if M == 0 then outputIdeal := ideal 1_R
     else (
	  F := map(M, R^1, matrix ({{1_R}} | 
		    toList(rank ambient M - 1: {0_R})) );
	  outputIdeal = ideal kernel F;
	  );
     outputIdeal
     )

DrestrictAll = method( Options => {Strategy => Schreyer} )
DrestrictAll(Ideal,List)  := options -> (I,w) -> (
     print("WARNING! Drestrict is an obsolete name for Drestriction");
     DrestrictionAll(I,w,options))

DrestrictAll(Module,List) := options -> (M,w) -> (
     print("WARNING! Drestrict is an obsolete name for Drestriction");
     DrestrictionAll(M,w,options))

DrestrictionAll = method( Options => {Strategy => Schreyer} )
DrestrictionAll(Ideal,List) := options -> (I,w) -> (
     DrestrictionAll ((ring I)^1/I, w, options)     )

DrestrictionAll(Module, List) := options -> (M,w) -> (
     ensureQuotientModule(M, 
	  "Drestriction currently only handles quotient modules");
     d := #positions(w, i -> (i>0));
     n := numgens ring M;
     if n == 2*d then outputRequest := {GenCycles, HomologyModules,
	  VResolution, BFunction, RestrictComplex, Exponents, 
	  Explicit}
     else outputRequest = {Cycles, Boundaries, HomologyModules,
	  VResolution, BFunction, RestrictComplex, Exponents,
	  Explicit};
     outputTable := computeRestriction (M, w, -1, d+1, 
	  outputRequest, options);
     outputTable
     )


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- These routines compute the derived integration by using the Fourier transform
-- and Drestriction routines
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Dintegrate = method( Options => {Strategy => Schreyer} )
Dintegrate(Ideal,List)  := options -> (I,w) -> (     print("WARNING! Dintegrate is an obsolete name for Dintegration");
Dintegration(I,w,options))
Dintegrate(Module,List) := options -> (M,w) -> (     print("WARNING! Dintegrate is an obsolete name for Dintegration");
     Dintegration(M,w,options))
Dintegrate(ZZ,Ideal,List)  := options -> (k,I,w) -> (     print("WARNING! Dintegrate is an obsolete name for Dintegration");
     Dintegration(k,I,w,options))
Dintegrate(ZZ,Module,List) := options -> (k,M,w) -> (     print("WARNING! Dintegrate is an obsolete name for Dintegration");
     Dintegration(k,M,w,options))

Dintegration = method( Options => {Strategy => Schreyer} )
Dintegration(Ideal,List) := options -> (I,w) -> (
     Dintegration ((ring I)^1/I, w, options)     )

Dintegration(Module, List) := options -> (M,w) -> (
     W := ring M;
     F := W^(numgens source gens M);
     ensureQuotientModule(M, 
	  "Dintegration currently only handles quotient modules");

     d := #positions(w, i -> (i>0));
     MF := cokernel Fourier relations M; 
     outputRequest := {HomologyModules};
     restrictOut := (computeRestriction(MF, w, -1, d+1, 
	       outputRequest, options))#HomologyModules;
     
     resW := ring restrictOut#0;
     nrW := numgens resW;
     if nrW == 0 then Mout := restrictOut
     else Mout = hashTable apply(toList(0..d), i -> 
	  (i => FourierInverse restrictOut#i) );     
     Mout
     )     

Dintegration(ZZ, Ideal, List) := options -> (k,I,w)  -> (
     Dintegration (k, (ring I)^1/I, w, options)	 )

Dintegration(ZZ, Module, List) := options -> (k,M,w)  -> (
     W := ring M;
     ensureQuotientModule(M, 
	  "Dintegration currently only handles quotient modules");

     d := #positions(w, i -> (i>0));
     MF := cokernel Fourier relations M; 
     outputRequest := {HomologyModules, BFunction};
     restriction := computeRestriction (MF, w, k-1, k+1,
	 outputRequest, options);
     restrictOut := restriction#HomologyModules#k;
    
     resW := ring restrictOut;
     nrW := numgens resW;
     if nrW == 0 then Mout := restrictOut
     else Mout = FourierInverse restrictOut; 
     Mout.cache.BFunction = restriction#BFunction;
     Mout
     )     

DintegrateClasses = method( Options => {Strategy => Schreyer} )
DintegrateClasses(Ideal,List)  := options -> (I,w) -> (
     print("WARNING! Dintegrate is an obsolete name for Dintegration");
     DintegrationClasses(I,w,options))
DintegrateClasses(Module,List) := options -> (M,w) -> (
     print("WARNING! Dintegrate is an obsolete name for Dintegration");
     DintegrationClasses(M,w,options))
DintegrateClasses(ZZ,Ideal,List) := options->(k,I,w)->(
     print("WARNING! Dintegrate is an obsolete name for Dintegration");
     DintegrationClasses(I,w,k,options))
DintegrateClasses(ZZ,Module,List) := options->(k,M,w)->(
     print("WARNING! Dintegrate is an obsolete name for Dintegration");
     DintegrationClasses(M,w,k,options))

DintegrationClasses = method( Options => {Strategy => Schreyer} )
DintegrationClasses(Ideal,List) := options -> (I,w) -> (
     DintegrationClasses ((ring I)^1/I, w, options)     )

DintegrationClasses(Module, List) := options -> (M,w) -> (
     W := ring M;
     ensureQuotientModule(M, 
	  "Dintegration currently only handles quotient modules");
     d := #positions(w, i -> (i>0));
     n := numgens W;
     if n == 2*d then outputRequest := {GenCycles,
	  VResolution,Explicit}
     else outputRequest = {Cycles, Boundaries, 
	  VResolution,Explicit};
     MF := cokernel Fourier relations M; 
     outputTable := computeRestriction (MF, w, -1, d+1, 
	  outputRequest, options);
     if outputTable#VResolution =!= null then
     outputList := {VResolution => FourierInverse outputTable#VResolution}
     else outputList = {VResolution => null};

     if n == 2*d then outputList = outputList | {
	  GenCycles => hashTable apply(toList(0..d), i -> 
	       (i => FourierInverse outputTable#GenCycles#i))}
     else outputList = outputList | {
	  Cycles => hashTable apply(toList(0..d), 
	       i -> (i => FourierInverse outputTable#Cycles#i)),
	  Boundaries => hashTable apply(toList(0..d), 
	       i -> (i => FourierInverse outputTable#Boundaries#i))
	  };

     hashTable outputList
     )

DintegrationClasses(ZZ,Ideal,List) := options -> (k,I,w) -> (
     DintegrationClasses (k, (ring I)^1/I, w, options)     )

DintegrationClasses(ZZ,Module,List) := options -> (k,M,w) -> (
     W := ring M;
     ensureQuotientModule(M, 
	  "Dintegration currently only handles quotient modules");
     d := #positions(w, i -> (i>0));
     n := numgens ring M;

     if k > d then (
	  if n == 2*d then output := hashTable {
	       GenCycles => hashTable {k => gens W^0},
	       VResolution => null}
	  else output = hashTable {
	       Cycles => hashTable {k => gens W^0},
	       Boundaries => hashTable {k => gens W^0},
	       VResolution => null};
	  )
     else (
          if n == 2*d then outputRequest := {GenCycles,
	       VResolution,Explicit}
     	  else outputRequest = {Cycles, Boundaries, 
	       VResolution, Explicit};
     	  MF := cokernel Fourier relations M; 
     	  outputTable := computeRestriction (MF, w, k-1, k+1, 
	       outputRequest, options);
     	  if outputTable#VResolution =!= null then
     	  outputList := {VResolution => FourierInverse outputTable#VResolution}
     	  else outputList = {VResolution => null};
	  
	  if n == 2*d then outputList = outputList | {
	       GenCycles => hashTable { k => 
	       	    FourierInverse outputTable#GenCycles#k } }
     	  else outputList = outputList | {
	       Cycles => hashTable { k =>
	       	    FourierInverse outputTable#Cycles#k },
	       Boundaries => hashTable { k => 
	       	    FourierInverse outputTable#Boundaries#k }
	       };
	  output = hashTable outputList;
	  );
     
     output
     )


DintegrateComplex = method( Options => {Strategy => Schreyer} )
DintegrateComplex(Ideal,List)  := options -> (I,w) -> (
     print("WARNING! Dintegrate is an obsolete name for Dintegration");
     DintegrationComplex(I,w,options))
DintegrateComplex(Module,List) := options -> (M,w) -> (
     print("WARNING! Dintegrate is an obsolete name for Dintegration");
     DintegrationComplex(M,w,options))


DintegrationComplex = method( Options => {Strategy => Schreyer} )
DintegrationComplex(Ideal, List) := options -> (I,w) -> (
     DintegrationComplex ((ring I)^1/I, w, options)     )

DintegrationComplex(Module, List) := options -> (M,w) -> (
     W := ring M;
     ensureQuotientModule(M, 
	  "Dintegration currently only handles quotient modules");
     d := #positions(w, i -> (i>0));
     n := numgens W;
     MF := cokernel Fourier relations M; 
     outputRequest := {RestrictComplex,Explicit};
     outputTable := computeRestriction (MF, w, -1, d+1, 
	  outputRequest, options);
     F := outputTable#RestrictComplex;

     if n != 2*d then F = FourierInverse F;
     F
     )

DintegrateIdeal = method( Options => {Strategy => Schreyer} )
DintegrateIdeal(Ideal,List)  := options -> (I,w) -> (
     print("WARNING! Dintegrate is an obsolete name for Dintegration");
     DintegrationIdeal(I,w,options))

DintegrationIdeal = method( Options => {Strategy => Schreyer} )
DintegrationIdeal(Ideal,List) := options -> (I,w) -> (
     W := ring I;
     d := #positions(w, i -> (i>0));
     n := numgens W;
     IF := Fourier I; 
     outputRequest := {RestrictComplex, Explicit};
     outputTable := computeRestriction (W^1/IF, w, -1, 1, 
	  outputRequest, options);
     M := cokernel (outputTable#RestrictComplex).dd#1;
     resW := ring M;
     if M == 0 then outputIdeal := ideal 1_resW
     else (
	  if n != 2*d then (
	       M = FourierInverse M;
	       );
	  F := map(M, (resW)^1, matrix ({{1_resW}} | 
		    toList(rank ambient M - 1: {0_resW})) );
	  outputIdeal = ideal kernel F;
	  );
     outputIdeal
     )

DintegrateAll = method( Options => {Strategy => Schreyer} )
DintegrateAll(Ideal,List)  := options -> (I,w) -> (
     print("WARNING! Dintegrate is an obsolete name for Dintegration");
     DintegrationAll(I,w,options))

DintegrateAll(Module,List) := options -> (M,w) -> (
     print("WARNING! Dintegrate is an obsolete name for Dintegration");
     DintegrationAll(M,w,options))

DintegrationAll = method( Options => {Strategy => Schreyer} )
DintegrationAll(Ideal,List) := options -> (I,w) -> (
     DintegrationAll ((ring I)^1/I, w, options)     )

DintegrationAll(Module, List) := options -> (M,w) -> (
     W := ring M;
     ensureQuotientModule(M, 
	  "Dintegration currently only handles quotient modules");
     d := #positions(w, i -> (i>0));
     n := numgens ring M;
     if n == 2*d then outputRequest := {GenCycles, HomologyModules,
	  VResolution, BFunction, RestrictComplex, Exponents, 
	  Explicit}
     else outputRequest = {Cycles, Boundaries, HomologyModules,
	  VResolution, BFunction, RestrictComplex, Exponents,
	  Explicit};
     
     MF := cokernel Fourier relations M; 
     outputTable := computeRestriction (MF, w, -1, d+1, 
	  outputRequest, options);
     outputList := {BFunction => outputTable#BFunction};
     if outputTable#VResolution =!= null then 
     outputList = append(outputList, 
	  VResolution => FourierInverse outputTable#VResolution)
     else outputList = append(outputList, VResolution => null);

     if n == 2*d then outputList = outputList | {
	  GenCycles => hashTable apply(toList(0..d), 
		    i -> (i => FourierInverse outputTable#GenCycles#i)),
	       HomologyModules => hashTable apply(toList(0..d), 
		    i -> (i => outputTable#HomologyModules#i)),
	       IntegrateComplex => outputTable#RestrictComplex}
     else (
     	  outputList = outputList | {
	       HomologyModules => hashTable apply(toList(0..d), 
		    i -> (i => FourierInverse outputTable#HomologyModules#i)),
	       Cycles => hashTable apply(toList(0..d), 
		    i -> (i => FourierInverse outputTable#Cycles#i)),
	       Boundaries => hashTable apply(toList(0..d), 
		    i -> (i => FourierInverse outputTable#Boundaries#i)),
	       IntegrateComplex => FourierInverse outputTable#RestrictComplex};
	  );

     hashTable outputList
     )


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--
-- computeRestriction (Module N, List w, ZZ n0, ZZ n1) 
--
-- This routine computes the derived restriction complex of (D_n^r)/N
-- in the degrees n0 to n1 with respect to the weight vector w.
--
--  Let t_1...t_d denote coordinates corresponding to nonzero entries of w.
--  Let x_1...x_m denote coordinates corresponding to zero entries of w.
--  Then it means restriction to the subspace {t_1 = ... = t_d = 0}
--
-- The routine performs the following algorithm, due to Oaku-Takayama:
-- (See OT, "Algorithms for D-modules")
--
-- 1. {k0+1, k1}, the min and max roots of b-function of N wrt w.
--
-- 2. Free resolution of N adapted to w of length n1, where [u_i]
--    denotes w-degree shifts:
--
--     0 <-- D_n^r <-- D_n^(r_1)[u_1] <-- ... <-- D_n^(r_n1)[u_n1]  (**)
--
-- 3. Restricted complex of the free resolution in degrees n0 to n1 as
--    a complex of left D_m modules.
--
--    F_k1[u_n0](D_n/tD_n)^(r_n0)              F_k1[u_n1](D_n/tD_n)^(r_n1)
--   ------------------------------- <- .. <- -----------------------------
--    F_k0[u_n0](D_n/tD_n)^(r_n0)              F_k0[u_n0](D_n/tD_n)^(r_n1)
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

protect ResToOrigRing

computeRestriction = (M,wt,n0,n1,output,options) -> (

-- ERROR CHECKING
     W := ring M;
     createDpairs W;
     -- check weight vector; we need it to be non-negative and to have length 
     -- equal to the number of x_i's
     if #wt != #W.dpairInds#0
     then error ("expected weight vector of length " | #W.dpairInds#0);
     if any(wt, i -> (i<0))
     then error "expected non-negative weight vector";

-- PREPROCESSING
     local tInfo;
     local tempvar;
     tempvar = symbol tempvar;
     tInfo = "";
     nW := numgens W;
     -- make the (-w,w) weight vector
     w := new MutableList from join(W.dpairInds#0,W.dpairInds#1);
     i := 0;
     while i < #W.dpairInds#0 do (
	  w#(W.dpairInds#0#i) = -wt#i;
	  w#(W.dpairInds#1#i) = wt#i; 
	  i = i+1;
	  );
     w = toList w;
     d := #positions(w, i->(i>0)); -- the number of positive entries of w
     -- the variables t_1, ..., t_d
     negVars := (entries vars W)#0_(positions(w, i->(i<0)));
     -- the substitution which sets t_1 = ... = t_d = 0
     resSub := apply( negVars, i -> (i => 0) );
     -- the variables Dt_1, ..., Dt_d, their indices, and their weights
     posVars := (entries vars W)#0_(positions(w, i->(i>0)));
     posInds := positions( w, i->(i>0) );
     posWeights := select( w, i->(i>0) );
     diffSub := apply( posVars, i -> (i => 0) );
     -- the rest of the variables x_1, ..., x_n, D_1, ..., D_n 
     otherVars := (entries vars W)#0_(positions(w, i->(i==0)));


-- MAKE THE WEYL ALGEBRA "resW" OF THE RESTRICTED SUBSPACE
     -- Case 1: if restriction to pt, then "resW" a field
     if #otherVars == 0 then (
	  resW := coefficientRing W;
	  WtoresW := map(resW, W, matrix{toList(numgens W: 0_resW)});
	  resWtoW := map(W, resW)
	  )
     -- Case 2: if restriction to coordinate subspace of dim d, then
     --         resW a Weyl algebra D_d.
     else (i = 0;
     	  resPairsList := {};
     	  while i < #otherVars do (
	       deriv := select(otherVars, j -> 
		    (j*otherVars#i - otherVars#i*j == 1));
	       if (#deriv == 1) then 
	       resPairsList = append(resPairsList, otherVars#i=>deriv#0);
	       i = i+1;
	       );
     	  resW = (coefficientRing W)(monoid [otherVars, WeylAlgebra=>resPairsList]);
     	  -- make the inclusion ring map "WtoresW" mapping W --> resW
     	  counter := 0;
     	  tempList := {};
	  WList := {};
	  i = 0;
     	  while i < numgens W do (
	       if w#i == 0 then (
	       	    tempList = append(tempList, resW_counter);
		    WList = append(WList, W_i);
	       	    counter = counter+1;)
	       else (tempList = append(tempList, 0_resW));
	       i = i+1;
	       );
     	  WtoresW = map(resW, W, matrix{tempList});
	  resWtoW = map(W, resW, matrix{WList});
	  );

-- INITIALIZE THE OUTPUT FORMS

     if member(HomologyModules, output) then homologyList := {};
     if member(Cycles, output) then kerList := {};
     if member(Boundaries, output) then imList := {};
     if member(GenCycles, output) then explicitList := {};
     if member(RestrictComplex,output) then (
	  restrictComplex := new ChainComplex;
	  restrictComplex.ring = resW;
	  );
     outputList := {};
     if member(Cycles, output) or member(Boundaries, output) or
     member(GenCycles, output) then explicitFlag := true 
     else explicitFlag = false;

-- GET MIN AND MAX ROOTS OF THE B-FUNCTION
     --<< "Computing b-function ";
     if n0 >= d then b := 1_(QQ[tempvar])
     else if rank ambient M == 1 then (
	  -- used to use "ideal presentation M" here
	  --<< "using ideal bFunction ";
	  pInfo(1, "Computing b-function using ideal bFunction... ");
     	  tInfo = toString first timing(
	       b = bFunction(ideal relations M, wt);
	       );
	  )
     else (
	  pInfo(1, "Computing b-function using module bFunction... ");
	  tInfo = toString first timing(
	       b = bFunction(M, wt, toList(rank ambient M: 0));
	       );
	  );
     
     if b == 0 then (
     	  error "Module not specializable. Restriction cannot be computed.";
	  );

     intRoots := getIntRoots b;
	  
-- NO INTEGER ROOTS
     if #intRoots == 0 then (
     	  k0 := 0;
     	  k1 := 0;
     	  pInfo(4, "\t bFunction = " | toString b);
     	  pInfo(2, "\t No integer roots");
     	  pInfo(3, "\t time = " | tInfo); 
	  if member(RestrictComplex, output) then (
	       restrictComplex#n0 = resW^0;
	       restrictComplex#n1 = resW^0;
	       restrictComplex.dd#n0 = map(resW^0,resW^0,0);
	       restrictComplex.dd#n1 = map(resW^0,resW^0,0);
	       i = n0+1;
	       while i < n1 do (
	       	    restrictComplex#i = resW^0;
	       	    restrictComplex.dd#i = map(resW^0,resW^0,0);
	       	    i = i+1;
	       	    );
	       );
	  if member(HomologyModules, output) then
     	  homologyList = apply (toList(n0+1..n1-1), i ->  (i => resW^0));
	  if member(Cycles, output) then
     	  kerList = apply (toList(n0+1..n1-1), i ->  (i => gens W^0));
	  if member(Boundaries, output) then
     	  imList = apply (toList(n0+1..n1-1), i ->  (i => gens W^0));
	  if member(GenCycles, output) then
     	  explicitList = apply (toList(n0+1..n1-1), i ->  (i => gens W^0));
	  )
     
-- INTEGER ROOTS EXIST
     else (
     k0 = min intRoots - 1;
     k1 = max intRoots;
     pInfo(4, "\t bFunction = " | toString b);
     pInfo(2, "\t min root =  " | k0+1 | " , max root =  " | k1);
     pInfo(3, "\t time = " | tInfo | " seconds");
     pInfo(2, " ");

----- SET k0 TO -infinity FOR EXPLICIT COHOMOLOGY CLASSES -----
     if member(Explicit, output) then k0 = -infinity;

-- COMPUTE FREE RESOLUTION ADAPTED TO THE WEIGHT VECTOR "w"
     tInfo = toString first timing (
     	  C := Dresolution (M, w, LengthLimit => n1, Strategy => options.Strategy);
	  );
     pInfo(2, "\t Finished...");
     pInfo(2, "\t\t\t Total time = " | tInfo | " seconds");
     pInfo(2, " ");


-- COMPUTE THE RESTRICTED COMPLEX IN DEGREES "n0" TO "n1" 
     tInfo = toString first timing (
     pInfo(1, "Computing induced restriction complex in degrees " |
	  n0 | " to " | n1 | "...");

     -- INITIALIZE THE ITERATION : making the first differential
     pInfo(2, "\t Degree " | n0+1 | "...");

     -- MAKE THE TARGET MODULE AS DIRECT SUM OF D_m MODULES
     -- "targetGens" is a list of s lists, where the i-th list contains
     -- the monomial generators {dx^a} (as left D_m-module) of
     --      F_k1[u_i]((D_n/tD_n) e_i) / F_k0[u_i]((D_n/tD_n) e_i)
     tInfo = toString first timing (
     	  s := numgens target C.dd#(n0+1);
     	  targetDeg := degrees target C.dd#(n0+1);
     	  targetGens := {};
     	  if explicitFlag then targetMat := map(W^0,W^0,0);
     	  i = 0;
     	  while i < s do (
	       tempExps := findExps(posWeights, k0-targetDeg#i#0, k1-targetDeg#i#0);
	       tempGens := apply(tempExps, j -> posVars^j);
	       targetGens = append(targetGens, tempGens);
	       if explicitFlag then (
	       	    if tempGens == {} then (
		    	 targetMat = directSum(targetMat, compress matrix{{0_W}}); )
	       	    else (
		    	 targetMat = directSum(targetMat, matrix{tempGens}); );
	       	    );
	       i = i+1;	  
	       );
     	  targetSize := sum(targetGens, i->#i);  
          
	  -- MAKE THE SOURCE MODULE AS DIRECT SUM OF D_m MODULES
     	  -- "sourceGens" is a list of r lists, where the i-th list contains
     	  -- the monomial generators {dx^a} (as left D_m-module) of
     	  --    F_k1[u_i]((D_n/tD_n) e_i) / F_k0[u_i]((D_n/tD_n) e_i)
     	  m := C.dd#(n0+1);
     	  r := numgens C#(n0+1);
     	  sourceDeg := degrees C#(n0+1);
     	  sourceGens := {};
     	  if explicitFlag then sourceMat := map(W^0,W^0,0);
     	  i = 0;
     	  while i < r do (
	       -- Find generators of the current source
	       --    "F_k1(D_n/tD_n)^r/F_k0(D_n/tD_n)^r"
	       -- as a left D_m module.
	       -- They have the form { \prod_{i=1}^n D_i^{a_i} }.
	       tempExps = findExps(posWeights, k0-sourceDeg#i#0, 
	       	    k1-sourceDeg#i#0);
	       tempGens = apply(tempExps, j -> posVars^j);
	       sourceGens = append(sourceGens, tempGens );
	       if explicitFlag then (
	       	    if tempGens == {} then (
		    	 sourceMat = directSum(sourceMat, compress matrix{{0_W}}); )
	       	    else (
		    	 sourceMat = directSum(sourceMat, matrix{tempGens}); );
	       	    );
	       i = i+1;
	       );
     	  sourceSize := sum(sourceGens, i -> #i);

	  
	  -- MAKE THE DIFFERENTIAL AS MATRIX OF D_m MODULES
     	  if sourceSize == 0 and targetSize == 0 then (
	       oldDiff := map(resW^0,resW^0,0) )
     	  else if sourceSize == 0 then ( oldDiff =
	       compress matrix toList(targetSize:{0_resW}) )
     	  else if targetSize == 0 then ( oldDiff =
	       transpose compress matrix toList(sourceSize:{0_resW}) )
     	  else (
	       -- For each generator j = \prod_i D_i^{a_i}, compute its image
	       -- j*m_i as an element of the RHS. Get a matrix of image vectors.
	       imageMat := matrix join toSequence apply( r, a -> 
	       	    apply(sourceGens#a, b -> substitute(b*m_a, resSub) ) );
	       -- differentiate with respect to targetGens
	       oldDiff = transpose compress matrix toList(sourceSize:{0_W});
	       i = 0;
	       -- compute the induced image
	       while i < s do (
	       	    if targetGens#i =!= {} 
	       	    then oldDiff = oldDiff || substitute( 
		    	 contract(transpose matrix{targetGens#i}, imageMat^{i}), 
		    	 diffSub);
	       	    i = i+1;
	       	    );
	       oldDiff = WtoresW oldDiff;
	       );
     	  if member(RestrictComplex, output) then (
	       restrictComplex#n0 = resW^(rank target oldDiff);
	       restrictComplex#(n0+1) = resW^(rank source oldDiff);
	       restrictComplex.dd#(n0+1) = map(restrictComplex#n0,
	       	    restrictComplex#(n0+1), oldDiff);
	       );
     	  if member(Cycles, output) or member(Boundaries, output)
	  or member(GenCycles, output) then (
	       newKernel := zeroize mingens kernel oldDiff;
	       -- newKernel := zeroize gens kernel oldDiff;
	       explicitKernel := compress (sourceMat * resWtoW(newKernel));
	       );
	  );
     pInfo(2, "\t\t\t Rank = " | sourceSize | "\t time = " | tInfo | " seconds" );
     
     -- DO THE COMPUTATION IN HIGHER COHOMOLOGICAL DEGREES     
     s = r;
     targetGens = sourceGens;
     targetSize = sourceSize;
     if explicitFlag then targetMat = sourceMat;
     targetMat = sourceMat;
     currDeg := n0 + 2;
     --newKernel = 0;
     --explicitKernel = 0;
     while currDeg <= n1 and C#?(currDeg) do (
	  -- MAKE THE NEXT SOURCE MODULE
	  -- "sourceGens" is a list of r lists, where the i-th list contains
	  -- the monomial generators {dx^a} (as left D_m-module) of
	  --    F_k1[u_i]((D_n/tD_n) e_i) / F_k0[u_i]((D_n/tD_n) e_i)
	  pInfo(2, "\t Degree " | currDeg | "...");
	  tInfo = toString first timing (
	  r = numgens C#currDeg;
	  m = C.dd#currDeg;
	  sourceDeg = degrees C#(currDeg);
	  sourceGens = {};
	  if explicitFlag then sourceMat = map(W^0,W^0,0);
	  i = 0;
	  while i < r do (
	       -- Find generators of the current source
	       --    "F_k1(D_n/tD_n)^r/F_k0(D_n/tD_n)^r"
	       -- as a left D_m module.
	       -- They have the form { \prod_{i=1}^n D_i^{a_i} }.
	       tempExps = findExps(posWeights, k0-sourceDeg#i#0, 
		    k1-sourceDeg#i#0);
	       tempGens = apply(tempExps, j -> posVars^j);
	       sourceGens = append(sourceGens, tempGens );
	       if explicitFlag then (
		    if tempGens == {} then (
 	       		 sourceMat = directSum(sourceMat, compress matrix{{0_W}}); )
		    else (
	       		 sourceMat = directSum(sourceMat, matrix{tempGens}); );
		    );
	       i = i+1;
	       );
	  sourceSize = sum(sourceGens, i -> #i);
	  
	  -- MAKE THE NEXT DIFFERENTIAL OF D_m MODULES	       
	  if sourceSize == 0 and targetSize == 0 then (
	       newDiff := map(resW^0,resW^0,0) )
	  else if sourceSize == 0 then ( newDiff =
	       compress matrix toList(targetSize:{0_resW}) )
	  else if targetSize == 0 then ( newDiff =
	       transpose compress matrix toList(sourceSize:{0_resW}) )
	  else (
	       -- For each generator j = \prod_i D_i^{a_i}, compute its image
	       -- j*m_i as an element of the RHS. Get a matrix of image vectors.
	       imageMat = matrix join toSequence apply( r, a -> 
		    apply(sourceGens#a, b -> substitute(b*m_a, resSub) ) );
	       -- differentiate with respect to targetGens
	       newDiff = transpose compress matrix toList(sourceSize:{0_W});
	       i = 0;
	       while i < s do (
		    if targetGens#i =!= {}
		    then newDiff = newDiff || substitute( 
			 contract(transpose matrix{targetGens#i}, imageMat^{i}), 
			 diffSub);
		    i = i+1;
		    );
	       newDiff = WtoresW newDiff;
	       );
	  
	  -- UPDATE THE OUTPUT LIST
	  if member(RestrictComplex, output) then (
	       restrictComplex#currDeg = resW^(rank source newDiff);
	       restrictComplex.dd#currDeg = map(restrictComplex#(currDeg-1),
		    restrictComplex#currDeg, newDiff);
	       );
	  if member(HomologyModules, output) then (
	       tempHomology := homology(zeroize oldDiff, zeroize newDiff);
	       if tempHomology =!= null then
	       tempHomology = cokernel Dprune presentation tempHomology;
	       if tempHomology === null then tempHomology = resW^0;
	       homologyList = append(homologyList,
		    (currDeg-1) => tempHomology);
	       );
	  
	  -- MAKE EXPLICIT COHOMOLOGY CLASSES	       
     	  if member(Cycles, output) or member(Boundaries, output) or
	  member(GenCycles, output) then (
	       oldImage := zeroize mingens image newDiff;
	       -- oldImage := zeroize gens image newDiff;
	       if member(GenCycles, output) then (
	       	    if #otherVars == 0 then (
		    	 explicitList = append(explicitList,
			      (currDeg-1) => targetMat *
			      resWtoW(mingens subquotient(newKernel, oldImage))) )
	       	    else (
		    	 explicitList = append(explicitList,
			      (currDeg-1) => explicitKernel);
		    	 );
		    );
	       if member(Cycles, output) then 
	       kerList = append(kerList, (currDeg-1) => explicitKernel);
	       if member(Boundaries, output) then (
		    explicitImage := compress (targetMat * resWtoW(oldImage));
		    imList = append(imList, (currDeg-1) => explicitImage);
		    );
	       newKernel = zeroize mingens kernel newDiff;
	       -- newKernel = zeroize gens kernel newDiff;
	       explicitKernel = compress (sourceMat * resWtoW(newKernel));
	       );
	  
	  -- PREPARE FOR NEXT ITERATION
	  s = r;
	  targetGens = sourceGens;
	  targetSize = sourceSize;
	  if explicitFlag then targetMat = sourceMat;
	  oldDiff = newDiff;
	  currDeg = currDeg+1;
	  );
     	  pInfo(2, "\t\t\t Rank = " | targetSize | "\t time = " | tInfo | " seconds");
     	  );
     );
     pInfo(2, "\t Finished...");
     pInfo(2, "\t\t\t Total time = " | tInfo | " seconds");
     pInfo(2, " ");
  );

-- OUTPUT FORMAT
if member(HomologyModules, output) then outputList = append(
     outputList, HomologyModules => hashTable homologyList);
if member(GenCycles, output) then outputList = append(
     outputList, GenCycles => hashTable explicitList);
if member(Cycles, output) then outputList = append(
     outputList, Cycles => hashTable kerList);
if member(Boundaries, output) then outputList = append(
     outputList, Boundaries => hashTable imList);
if member(BFunction, output) then outputList = append(
     outputList, BFunction => factorBFunction b);
if member(VResolution, output) then outputList = append(
     outputList, VResolution => C);
if member(RestrictComplex, output) then outputList = append(
     outputList, RestrictComplex => restrictComplex);
if member(ResToOrigRing, output) then outputList = append(
     outputList, ResToOrigRing => resWtoW);

hashTable outputList 
)

TEST ///
R1 = QQ[a,x,y,Da,Dx,Dy,WeylAlgebra=>{x=>Dx,y=>Dy,a=>Da}] -- order a,x,y
I = ideal(x*Dx+y*Dy+a*Da+1, Dx^2 - Da*Dy, -2*a*Da -x*Dx -3)
resIdeal1 = DrestrictionIdeal(I, {1,0,0}) -- correct restriction ideal
R2 = QQ[x,y,a,Dx,Dy,Da,WeylAlgebra=>{x=>Dx,y=>Dy,a=>Da}] -- order x,y,a
I = ideal(x*Dx+y*Dy+a*Da+1, Dx^2 - Da*Dy, -2*a*Da -x*Dx -3)
resIdeal2 = DrestrictionIdeal(I, {0,0,1}) -- wrong restriction ideal
assert((map(ring resIdeal2, ring resIdeal1)) resIdeal1 == resIdeal2)
///