-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- These local routines are also used in Drestriction
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
     if #w == 1 and k0 >= 0 
     then tempExps = ( toList((k0//w#0+1)..k1//w#0) / (i -> {i}) )
     else if (#w == 1 and k0 < 0) 
     then tempExps = ( toList(0..k1//w#0) / (i -> {i}) )
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

divideOutGCD = method()
divideOutGCD RingElement := L -> (
     W := ring L;
     createDpairs W;
     LCMexps := apply(toList(0..numgens W-1), j -> (
	       if member(j, W.dpairInds#1) then 0
	       else min apply(exponents L, i -> i#j)) );
     newlistForm := apply(listForm L, i -> (i#0 - LCMexps, i#1));
     sum(newlistForm, i -> (i#1)*( ((entries vars W)#0)^(i#0) ) )
     )

divideOutGCD Matrix := m -> (
     if rank target m > 1 then error "only handles 1 by n matrices";
     matrix{apply(flatten entries m, i -> divideOutGCD i)}
     )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This routine computes a basis of Hom_D(M, k[x]), polynomial solutions
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
PolySols = method(Options => {Alg => GD} )

PolySols Ideal := options -> I -> ( 
     PolySols((ring I)^1/I, options) )

PolySols(Ideal,List) := options -> (I,w) -> ( 
     PolySols((ring I)^1/I, w, options) )

PolySols Module := options -> M -> (
     W := ring M;
     if W.monoid.Options.WeylAlgebra === {} then
     error "expected an element of a Weyl algebra";     
     createDpairs W;
     n := #W.dpairVars#0;     
     w := toList(n:1);
     PolySols(M, w, options)
     )

PolySols(Module, List) := options -> (M, w) -> (

     pInfo (1, "ENTERING PolySols ...");
     W := ring M;
     K := coefficientRing W;
     outputList := {};     
     if W.monoid.Options.WeylAlgebra === {} then
     error "expected a Weyl algebra";
     createDpairs W;
     n := #W.dpairVars#0;

     if options#Alg == GD then (
	  -- error checking
	  if not isQuotientModule M then error "expected
	  input to be a quotient module";
     	  if numgens target gens M > 1 then error "non-cyclic
	  modules not yet supported for Grobner deformation method";
     	  if any(w, i -> i <= 0) then error "expected strictly
	  positive weight vector";
	  
     	  I := ideal relations M;
	  inI := inw(I, join(w,-w));
	  if not W.?ThetaRing then createThetaRing W;
	  if all(flatten entries gens inI, W.isGeneric) then (
	       genI := gens inI;
	       inI = ideal divideOutGCD gens inI;
	       )
	  else pInfo(0, "Warning: not a generic weight vector.  Could be difficult...");
	       
     	  b := bFunction(inI,-w,Strategy => TryGeneric);
          if b == 0 
	  then error "Module not specializable. Poly Sols cannot be computed by
	  Grobner deformations.  Try taking Weyl closure first.";
     	  intRoots := getIntRoots b;

     	  if #intRoots == 0 then answer := {0_W}
     	  else (
	       degBound := -(min intRoots);
	       if degBound < 0 then answer = {0_W}
	       else (
		    possExps := findExps(w, -1, degBound);
		    nUnknowns := #possExps;
		    a := symbol a;
		    S := (coefficientRing W)(monoid [a_1..a_nUnknowns]);
	       	    SW := S(monoid [(entries vars W)#0, WeylAlgebra =>
	    	    	 W.monoid.Options.WeylAlgebra]);
	       	    WtoSW := map(SW, W, vars SW);
	       	    SWtoS := map(S, SW, toList(numgens SW: 1_S));
	       	    matI := WtoSW gens I;
	       	    createDpairs SW;
		    monBasis := matrix{ apply(possExps, i -> 
			      (W.dpairVars#0)^i) };
	       	    testPoly := (vars S)*(transpose WtoSW monBasis);
	       	    testIdeal := ideal SW.dpairVars#1;
	       	    resultPolys := substitute(testPoly*matI,
		    	 apply(SW.dpairVars#1, i -> i => 0));
	       	    linEqns := SWtoS transpose (coefficients resultPolys_(0,0))#1;
	       	    i := 1;
	       	    while i < numgens source resultPolys do (
		    	 linEqns = linEqns |
		    	 SWtoS transpose (coefficients resultPolys_(0,i))#1;
     	       	    	 i = i + 1;
		    	 );
		    pInfo(1, "Need to solve " | rank source linEqns |
			 " equations");
		    dummyEqn := sum((entries vars S)#0);
	       	    linEqns = (gens gb (linEqns, DegreeLimit 
			      => 1)) | matrix{{dummyEqn}};
--MES		    coeffs := substitute(
--MES			 transpose (coefficients transpose linEqns)#1, QQ);
		    coeffs := substitute(
			 transpose (coefficients linEqns)#1, QQ);
		    kerCoeffs := kernel (coeffs^{0..(rank target coeffs - 2)});
		    answer = (entries (monBasis*(gens kerCoeffs)))#0;
		    );
	       );
     	  )

     else if options.Alg == Duality then (
     	  diffSub := apply(W.dpairVars#1, i -> i => 0);
     	  if #w != n then error "expected weight vector of length n";
     	  if any(w, i->i<=0) then error "expected strictly positive weight vector";
	  B := Dres(M, LengthLimit => n+1);
     	  pInfo (1, "Dualizing (slow method for now) ...");
	  tInfo := toString first timing (
	       C := Dtransposition dual B);
     	  pInfo (2, "\t\t\t time = " | tInfo | " seconds");
	  
     	  Ker := zeroize gens kernel C.dd#-n;
     	  Rels := presentation image Ker;
     	  Im := zeroize C.dd#-(n-1);
     	  if (Im%Ker) != 0 then error "expected reduction to 0";
     	  Syz := Im//Ker;
     	  NewDual := Rels | Syz;
     	  P := cokernel NewDual;
	  
	  outTable := DintegrationClasses(P, w);
     	  F := outTable#VResolution;
	  integrateTable := outTable#GenCycles;

     	  if integrateTable#n == 0 then answer = matrix{{0_K}}
     	  else (
     	       chainMap := new MutableHashTable;
     	       chainMap.source = F;
	       chainMap.target = C;
	       chainMap.degree = -n;
	       chainMap#0 = map(C#-n, F#0, Ker);
	       pInfo(1, "computing chain map 1 ...");
	       tInfo = toString first timing (
	       bottomCompose := (Ker)*F.dd#1;
	       if (zeroize bottomCompose)%(zeroize C.dd#-(n-1)) != 0 then 
	       error "expected reduction to 0 -- possible lack of gb problem?";
	       nextLiftMap := (zeroize bottomCompose)//(zeroize C.dd#-(n-1));
	       chainMap#1 = map(C#-(n-1), F#1, nextLiftMap); );
	       pInfo (2, "\t\t\t time = " | tInfo | " seconds");
	       i = 2;
	       while i <= n do (
	       	    pInfo(1, "computing chain map " | i | " ...");
		    tInfo = toString first timing (
	       	    	 bottomCompose = nextLiftMap*F.dd#i;
	       	    	 if ((zeroize bottomCompose)%(zeroize C.dd#-(n-i)) != 0) then 
	       	    	 error "expected reduction to 0 -- 
			 possible lack of gb problem?";
	       	    	 nextLiftMap = (zeroize bottomCompose)//(zeroize C.dd#-(n-i));
	       	    	 chainMap#i = map(C#-(n-i), F#i, nextLiftMap);
     	       	    	 i = i+1; 
		    	 );
	       	    pInfo (2, "\t\t\t time = " | tInfo | " seconds");
	       	    );
	       answer = nextLiftMap*(integrateTable#n);
	       answer = substitute(Dtransposition(answer), diffSub);
     	       chainMap = new ChainComplexMap from chainMap;
	       );
     	  answer = (entries answer)#0;
     	  )

     else error "expected Alg 'GD' or 'Duality'";
     
     R := (coefficientRing W)(monoid [W.dpairVars#0]);
     apply(answer, i -> substitute(i,R))
     )

------------------------ BETA ROUTINES -------------------------------



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This routine computes the dimensions of Ext^i(M,k[x]) for holonomic M
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
PolyExt = method(Options => {Strategy => Schreyer})
PolyExt Ideal := options -> I -> (
     PolyExt ((ring I)^1/I, options)
     )

PolyExt Module := options -> (M) -> (
     pInfo (1, "ENTERING PolyExt ...");
     W := ring M;
     createDpairs W;
     n := #W.dpairVars#0;
     w := toList(n:1);
     outputList := {};
     N := Ddual M;
     integrateTable := Dintegration(zeroize N, w, options);
     homologyTable := hashTable apply(toList(0..n), 
	       i -> (n-i) => integrateTable#i);
     homologyTable
     )

PolyExt(ZZ, Ideal) := options -> (k, I) -> (
     if not I.?quotient then I.quotient = (ring I)^1/I;
     PolyExt (k, I.quotient, options)
     )

PolyExt(ZZ, Module) := options -> (k, M) -> (
     pInfo (1, "ENTERING PolyExt ...");
     W := ring M;
     createDpairs W;
     n := #W.dpairVars#0;
     w := toList(n:1);
     outputList := {};
      N := Ddual M;
     integrateModule := Dintegration(n-k, zeroize N, w, options);
     integrateModule
     )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- These routines compute the dimensions of rational solutions of a
-- holonomic ideal I of linear PDE's
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
RatSols = method()
RatSols(Ideal) := (I) -> (
     W := ring I;
     createDpairs W;
     w := toList(#W.dpairVars#0: 1);
     f := (singLocus I)_0;
     RatSols(I, f, w)
     )

RatSols(Ideal, List) := (I, w) -> (
     f := (singLocus I)_0;
     RatSols(I, f, w)
     )

RatSols(Ideal, RingElement) := (I, f) -> (
     W := ring I;
     createDpairs W;
     w := toList(#W.dpairVars#0: 1);
     RatSols(I, f, w)
     )

RatSols(Ideal, RingElement, List) := (I, f, w) -> (
     W := ring I;
     bfunc := (globalB(I, f))#Bpolynomial;
     k := (max getIntRoots bfunc) + 1;
     if k > 0 then (
	  twistI := TwistOperator(I, f, k);
     	  numerators := PolySols (twistI, w);
     	  R := (coefficientRing W)(monoid [W.dpairVars#0]);
     	  F := substitute(f^k, R);
     	  solsList := apply (numerators, i -> ( substitute(i,R)/F ) );
	  )
     else solsList = PolySols(I, w);
     solsList
     )

RatSols(Ideal, List, List) := (I, f, w) -> (
     W := ring I;
     createDpairs W;
     bfuncs := apply(f, i -> (globalB(I, i))#Bpolynomial);
     k := apply(bfuncs, i -> (max getIntRoots i) + 1);
     nonzero := positions(k, i -> (i > 0));
     if nonzero != {} then (
	  newk := k_nonzero;
	  newf := f_nonzero;
     	  twistI := TwistOperator(I, newf, newk);
     	  numerators := PolySols (twistI, w);
     	  R := (coefficientRing W)(monoid [W.dpairVars#0]);
     	  F := substitute(newf^newk, R);
     	  solsList := apply (numerators, i -> (substitute(i,R) / F) );
	  )
     else solsList = PolySols(I, w);
     solsList
     )

TwistOperator = method()
TwistOperator(Ideal, RingElement, ZZ) := (I, f, k) -> (
     ideal apply((entries gens I)#0, L -> TwistOperator(L, f, k))
     )
TwistOperator(Ideal, List, List) := (I, f, k) -> (
     ideal apply((entries gens I)#0, L -> TwistOperator(L, f, k))
     )

TwistOperator(RingElement, RingElement, ZZ) := (L, f, k) -> (
     W := ring L;
     createDpairs W;
     partialf := hashTable apply(W.dpairInds#1, i -> i => (W_i*f-f*W_i));
     twistList := hashTable apply(W.dpairInds#1, i -> i => (f*W_i - k*(W_i*f-f*W_i)));
     setOneSub := apply (W.dpairVars#0, i -> (i => 1));
     diffSub := apply (W.dpairVars#1, i -> (i => 0));
     w := apply( toList(0..numgens W - 1), 
	  i -> if member(i, W.dpairInds#1) then 1 else 0 );
     inL := substitute (leadTerm inw(L, w), W);
     ordL := (degree substitute(inL, setOneSub))#0;
     newL := 0_W;
     currL := L;
     
     while currL != 0 do (
	  currDiff := substitute((1/leadCoefficient currL)*leadTerm currL, setOneSub);
	  currPoly := (substitute(contract(matrix{{currDiff}}, matrix{{currL}}), 
		    diffSub))_(0,0);
	  multFactor := (degree currDiff)#0;
	  currExp := (exponents currDiff)#0;
	  currk := multFactor-1;
	  currNew := f^(ordL - multFactor);
	  i := 0;
	  while i < #currExp do (
     	       j := 0;
     	       while (j < currExp#i) do (
	  	    currNew  = currNew * (twistList#i-currk*partialf#i);
	  	    currk = currk - 1;
	  	    j = j+1;
	  	    );
     	       i = i+1;
     	       );
	  currL = currL - currPoly * currDiff;
	  newL = newL + currPoly*currNew;
	  );
     newL
     )

TwistOperator(RingElement, List, List) := (L, f, k) -> (
     W := ring L;
     createDpairs W;
     fprod := product f;
     partialfprod := hashTable apply(W.dpairInds#1, i -> i => (W_i*fprod-fprod*W_i));
     fhat := apply(f, i -> product(apply(f, j -> if (j == i) then 1_W else j)) );
     partialf := apply(f, i-> (
	       hashTable apply(W.dpairInds#1, j -> j => (W_j*i-i*W_j))) );
     twistList := hashTable apply(W.dpairInds#1, i -> 
	  i => (fprod*W_i - sum(toList(0..#f-1), j -> k_j*fhat_j*(partialf_j#i))) );
     setOneSub := apply (W.dpairVars#0, i -> (i => 1));
     diffSub := apply (W.dpairVars#1, i -> (i => 0));
     w := apply( toList(0..numgens W - 1), 
	  i -> if member(i, W.dpairInds#1) then 1 else 0 );
     inL := substitute (leadTerm inw(L, w), W);
     ordL := (degree substitute(inL, setOneSub))#0;
     newL := 0_W;
     currL := L;
     
     while currL != 0 do (
	  currDiff := substitute((1/leadCoefficient currL)*leadTerm currL, setOneSub);
	  currPoly := (substitute(contract(matrix{{currDiff}}, matrix{{currL}}), 
		    diffSub))_(0,0);
	  multFactor := (degree currDiff)#0;
	  currExp := (exponents currDiff)#0;
	  currk := multFactor-1;
	  currNew := fprod^(ordL - multFactor);
	  i := 0;
	  while i < #currExp do (
     	       j := 0;
     	       while j < currExp#i do (
	  	    currNew  = currNew * (twistList#i-currk*partialfprod#i);
	  	    currk = currk - 1;
	  	    j = j+1;
	  	    );
     	       i = i+1;
     	       );
	  currL = currL - currPoly * currDiff;
	  newL = newL + currPoly*currNew;
	  );
     newL
     )


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- These routines compute the dimensions of Ext^i(M, k[x][1/f]) where f is
-- defaulted to the singular locus if no input is given
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
RatExt = method(Options => {Strategy => Schreyer} )

RatExt Ideal := options -> I -> (
     f := (singLocus(I))_0;
     RatExt (I, f)
     )

RatExt Module := options -> M -> (
     r := numgens target gens M; 
     -- case 1: M is a proper submodule of (D_n)^r/N
     if gens M != map (ring M)^r
     then error "expected input to be a cokernel";
     -- case 2: M is a cokernel
     if r > 1 then error "non-cyclic modules not yet supported";
     
     f := (mingens singLocus(ideal relations M))_(0,0);
     RatExt (M, f)
     )


RatExt(Ideal, RingElement) := options -> (I, f) -> (
     RatExt ((ring I)^1/I, f, options)
     )

RatExt(Module, RingElement) := options -> (M, f) -> (
     pInfo (1, "ENTERING RatlExt ...");
     W := ring M;
     createDpairs W;
     n := #W.dpairVars#0;
     w := toList(n:1);
     outputList := {};
     N := zeroize Ddual M;
     N2 := Dlocalize(N, f);
     integrateTable := Dintegration(zeroize N2, w, options);
     homologyTable := hashTable apply(toList(0..n), 
	  i -> (n-i) => integrateTable#i);
     homologyTable
     )

RatExt(ZZ, Module) := options -> (k, M) -> (
     r := numgens target gens M; 
     -- case 1: M is a proper submodule of (D_n)^r/N
     if gens M != map (ring M)^r 
     then error "expected input to be a cokernel";
     -- case 2: M is a cokernel
     if r > 1 then error "non-cyclic modules not yet supported";
     
     f := (mingens singLocus(ideal relations M))_(0,0);
     RatExt (k, M, f)
     )

RatExt(ZZ, Ideal) := options -> (k, I) -> (
     f := (singLocus(I))_0;
     RatExt (k, I, f)
     )

RatExt(ZZ, Ideal, RingElement) := options -> (k, I, f) -> (
     RatExt (k, (ring I)^1/I, f, options)
     )

RatExt(ZZ, Module, RingElement) := options -> (k, M, f) -> (
     pInfo (1, "ENTERING RatlExt ...");
     W := ring M;
     createDpairs W;
     n := #W.dpairVars#0;
     w := toList(n:1);
     outputList := {};
      N := zeroize Ddual M;
      N2 := Dlocalize(N, f);
     integrateModule := Dintegration(n-k, zeroize N2, w, options);
     integrateModule
     )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This routine computes explicitly Hom_D(M,N) for holonomic M and N 
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

---------------------  VERSION -----------------------------

DHom = method(Options => {Strategy => Schreyer})

DHom(Ideal, Ideal) := options -> (I, J) -> (
     W := ring I;
     createDpairs W;
     n := #W.dpairVars#0;
     w := toList(2*n:1);
     DHom(W^1/I, W^1/J, w, options)
     )
     
DHom(Module, Module) := options -> (M, N) -> (
     W := ring M;
     createDpairs W;
     n := #W.dpairVars#0;
     w := toList(2*n:1);
     DHom(M, N, w, options)
     )
     
protect diagonal

DHom(Module, Module, List) := options -> (M, N, w) -> (
     pInfo (1, "ENTERING DHom ...");
     W := ring M;
     nW := numgens W;
     K := coefficientRing W;

     pInfo (1, "DHom: holonomicity check ...");
     if not (isHolonomic M and isHolonomic N) then
     error "expected holonomic ideals/modules";     
     
     outputList := {};
     if (ring N =!= W) then error "expected modules over the same Weyl algebra";
     if (W.monoid.Options.WeylAlgebra === {}) then
     error "expected an element of a Weyl algebra";
     createDpairs W;
     n := #W.dpairVars#0;
     naux := #W.dpairVars#2;
     if naux > 0 then error "expected Weyl algebra without parameters";
     if #w != 2*n then error "expected weight vector of length 2n";
     if any(w, i->i<=0) then error "expected strictly positive weight vector";
     
     pInfo(1, "Beginning computation of Hom_D(M,N) ...");
     pInfo(1, "Computing resolution of M");
     FM := Dres(M, LengthLimit => n+1);
     pInfo(1, "Dualizing (slow for now) ... ");
     tInfo := toString first timing (FM = Dtransposition dual FM);
     pInfo(2, "\t\t\t time = " | tInfo | " seconds");
     
     pInfo(1, "Computing resolution of N");
     FN := Dres(N, LengthLimit => n+1);
     
     FMN := ExternalProduct(FM, FN, TwistMap => true);
     WW := ring FMN;
     FMN = FMN[-n]; 
     C := WW.twistMap FMN;
     
     Ker := zeroize gens kernel C.dd#0;
     Rels := presentation image Ker;
     Im := zeroize C.dd#1;

     if Im%Ker != 0 then error "expected reduction to 0";
     Syz := Im//Ker;
     NewRels := Rels | Syz;
     twistMN := cokernel NewRels;

     outTable := computeRestriction(twistMN, w, -1, n+1, 
	  {GenCycles, VResolution}, hashTable {Strategy => Schreyer});
     F := outTable#VResolution;
     restrictTable := outTable#GenCycles;

     local answer;

     if restrictTable#n == 0 then answer = 0_K
     else (
     	  chainMap := new MutableHashTable;
     	  chainMap.source = F;
	  chainMap.target = C;
	  chainMap.degree = 0;
	  chainMap#0 = map(C#0, F#0, Ker);
     	  pInfo(1, "computing chain map 1 ... ");
	  tInfo = toString first timing (
	  bottomCompose := (Ker)*F.dd#1;
	  if (zeroize bottomCompose)%(zeroize C.dd#1) != 0 then 
	  error "expected reduction to 0 -- possible lack of gb problem?";
	  nextLiftMap := (zeroize bottomCompose)//(zeroize C.dd#1);
	  chainMap#1 = map(C#1, F#1, nextLiftMap);
	  );
          pInfo (2, "\t\t\t time = " | tInfo | " seconds");
	  i := 2;
	  while i <= n do (
	       pInfo(1, "computing chain map " | i | " ..." );
	       tInfo = toString first timing (
	       bottomCompose = nextLiftMap*F.dd#i;
	       if (zeroize bottomCompose)%(zeroize C.dd#i) != 0 then 
	       error "expected reduction to 0 -- possible lack of gb problem?";
	       nextLiftMap = (zeroize bottomCompose)//(zeroize C.dd#i);
	       chainMap#i = map(C#i, F#i, nextLiftMap);
     	       i = i+1;
	       );
               pInfo (2, "\t\t\t time = " | tInfo | " seconds");
	       );
	  answer = WW.twistInvMap(nextLiftMap*restrictTable#n);
	  temp := answer;
     	  chainMap = new ChainComplexMap from chainMap;
	  );
     
     if answer == 0 then basisList := matrix{{0_K}}
     else (
	  answer = WW.projMap2 Dtransposition ( (Dtransposition answer)%
	       (directSum (rank target answer:gens Dtransposition WW.diagonal)));
     	  homSize := (rank ambient M)*(rank ambient N);
     	  answerInds := {rank target answer - homSize..rank target answer-1};
     	  answer = answer^answerInds;
     	  basisList = apply(toList(0..rank source answer - 1), i ->
	       map(N, M, transpose matrix pack( 
			 (entries transpose answer_{i})#0, rank ambient N) ) );
     	  );
     
     basisList
     )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--- This routine computes the dimensions of Ext^i(M,N) for holonomic M and N
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------- BETA VERSIONS --------------------------

DExt = method(Options => {Strategy => Schreyer, Info => 1,
	  Output => HomologyModules, Special => None})

DExt(Module, Module) := options -> (M, N) -> (
     W := ring M;
     createDpairs W;
     n := #W.dpairVars#0;
     w := toList(2*n:1);
     DExt(M, N, w, options)
     )

DExt(Module, Module, List) := options -> (M, N, w) -> (
     pInfo (1, "ENTERING DExt ...");
     W := ring M;
     nW := numgens W;
     K := coefficientRing W;
     outputList := {};

     -- ERROR CHECKING
     pInfo (1, "DExt: holonomicity check ...");
     if not (isHolonomic M and isHolonomic N) then
     error "expected holonomic ideals/modules";     

     if ring M =!= ring N then
     error "Expected modules over the same rings";
     if W.monoid.Options.WeylAlgebra === {} then
     error "expected an element of a Weyl algebra";
     createDpairs W;
     n := #W.dpairVars#0;
     naux := #W.dpairVars#2;
     if naux > 0 then error "expected Weyl algebra without parameters";
     if #w != 2*n then error "expected weight vector of length 2n";
     if any(w, i->i<=0) then 
     error "expected strictly positive weight vector";
     
     --<< "Computing holonomic dual of M";
      Mdual := Ddual M;

     MdualN := ExternalProduct(Mdual,N, TwistMap => true);

     restrictTable := computeRestriction((ring MdualN).twistMap ** MdualN, 
	  w, -1, n+1, {HomologyModules}, options);
     ExtTable := hashTable apply(toList(0..n), 
	  i -> i => restrictTable#HomologyModules#(n-i));
     ExtTable
     )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- These routines compute the external product of modules or complexes
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
ExternalProduct = method(Options => {TwistMap => false})
ExternalProduct(Module, Module) := options -> (M, N) -> (

     pInfo(1, "ENTERING ExternalProduct ...");

-- CURRENT LIMITATIONS
     if not isQuotientModule M or not isQuotientModule N
     then error "ExternalProduct currently only handles quotient modules";     

-- PRE-PROCESSING     
     W1 := ring M; 
     W2 := ring N;
     createDpairs W1;
     createDpairs W2;
     n1 := #W1.dpairVars#0;
     m1 := #W1.dpairVars#2;
     nW1 := numgens W1;
     n2 := #W2.dpairVars#0;
     m2 := #W2.dpairVars#2;
     nW2 := numgens W2;
     
-- MAKE THE RING D_n \os D_n
     a := symbol a;
     b := symbol b;
     Da := symbol Da;
     Db := symbol Db;
     Ca := symbol Ca;
     Cb := symbol Cb;
     if #W1.dpairVars#2 == 0 then
     WW := (coefficientRing W1)(monoid [a_1..a_n1, Da_1..Da_n1, b_1..b_n2, Db_1..Db_n2,
          WeylAlgebra => join( apply(toList(1..n1), i -> a_i => Da_i),
	       apply(toList(1..n2), i -> b_i => Db_i))])
     else
     WW = (coefficientRing W1)(monoid [a_1..a_n1, Da_1..Da_n1, Ca_1..Ca_m1, 
	  b_1..b_n2, Db_1..Db_n2, Cb_1..Cb_m2,
          WeylAlgebra => join( apply(toList(1..n1), i -> a_i => Da_i),
	       apply(toList(1..n2), i -> b_i => Db_i))]);
     ordList1 := join( W1.dpairInds#0, W1.dpairInds#1, W1.dpairInds#2 );
     ordList2 := join( W2.dpairInds#0, W2.dpairInds#1, W2.dpairInds#2 );
     invList1 := inversePermutation ordList1;
     invList2 := inversePermutation ordList2;
     incList1 := apply(invList1, i -> WW_i);
     incList2 := apply(invList2, i -> WW_(i+nW1));
     incMap1 := map(WW, W1, incList1);
     incMap2 := map(WW, W2, incList2);
     projList1 := join(W1.dpairVars#0, W1.dpairVars#1, W1.dpairVars#2);
     projList2 := join(W2.dpairVars#0, W2.dpairVars#1, W2.dpairVars#2);
     WW.projMap1 = map(W1, WW, join(projList1, toList(nW2:0)));
     WW.projMap2 = map(W2, WW, join(toList(nW1:0), projList2));

-- MAKE TWISTMAP TO DIAGONAL
     if W1 === W2 and options.TwistMap then (
	  n := n1;
	  nW := nW1;
	  naux := m1;
     	  twistList := join (
	       apply (toList(0..n-1), i -> (1/2)*WW_i - WW_(nW+n+i)),
	       apply (toList(0..n-1), i -> (1/2)*WW_(nW+i) + WW_(n+i)),
	       apply (toList(0..naux-1), i -> WW_(2*n+i)),
	       apply (toList(0..n-1), i -> (-1/2)*WW_i - WW_(nW+n+i)),
	       apply (toList(0..n-1), i -> (1/2)*WW_(nW+i) - WW_(n+i)),
	       apply (toList(0..naux-1), i -> WW_(nW+2*n+i)));	   	  
	  twistInvList := join (
	       apply (toList(0..n-1), i -> WW_i - WW_(nW+i)),
	       apply (toList(0..n-1), i -> (1/2)*WW_(n+i) - (1/2)*WW_(nW+n+i)),
	       apply (toList(0..naux-1), i -> WW_(2*n+i)),
	       apply (toList(0..n-1), i -> WW_(n+i) + WW_(nW+n+i)),
	       apply (toList(0..n-1), i -> (-1/2)*WW_i + (-1/2)*WW_(nW+i)),
	       apply (toList(0..naux-1), i -> WW_(nW+2*n+i)));
	  WW.twistMap = map(WW, WW, twistList);
     	  WW.twistInvMap = map(WW, WW, twistInvList);

	  WW.diagonal = ideal join(
	       apply (toList(0..n-1), i -> WW_i - WW_(nW+i)),
	       apply (toList(0..n-1), i -> WW_(n+i) + WW_(nW+n+i)));
	  );

-- MAKE M (external tensor) N
     incM := incMap1 ** M;
     incN := incMap2 ** N;
     incM**incN    
     )

ExternalProduct(ChainComplex, ChainComplex) := options -> (F, G) -> (

     pInfo(1, "ENTERING ExternalProduct ...");

-- PRE-PROCESSING     
     W1 := ring F; 
     W2 := ring G;
     createDpairs W1;
     createDpairs W2;
     n1 := #W1.dpairVars#0;
     m1 := #W1.dpairVars#2;
     nW1 := numgens W1;
     n2 := #W2.dpairVars#0;
     m2 := #W2.dpairVars#2;
     nW2 := numgens W2;
     
-- MAKE THE RING D_n \os D_n
     a := symbol a;
     b := symbol b;
     Da := symbol Da;
     Db := symbol Db;
     Ca := symbol Ca;
     Cb := symbol Cb;
     if #W1.dpairVars#2 == 0 then
     WW := (coefficientRing W1)(monoid [a_1..a_n1, Da_1..Da_n1, b_1..b_n2, Db_1..Db_n2,
          WeylAlgebra => join( apply(toList(1..n1), i -> a_i => Da_i),
	       apply(toList(1..n2), i -> b_i => Db_i))])
     else
     WW = (coefficientRing W1)(monoid [a_1..a_n1, Da_1..Da_n1, Ca_1..Ca_m1, 
	  b_1..b_n2, Db_1..Db_n2, Cb_1..Cb_m2,
          WeylAlgebra => join( apply(toList(1..n1), i -> a_i => Da_i),
	       apply(toList(1..n2), i -> b_i => Db_i))]);
     ordList1 := join( W1.dpairInds#0, W1.dpairInds#1, W1.dpairInds#2 );
     ordList2 := join( W2.dpairInds#0, W2.dpairInds#1, W2.dpairInds#2 );
     invList1 := inversePermutation ordList1;
     invList2 := inversePermutation ordList2;
     incList1 := apply(invList1, i -> WW_i);
     incList2 := apply(invList2, i -> WW_(i+nW1));
     incMap1 := map(WW, W1, incList1);
     incMap2 := map(WW, W2, incList2);
     projList1 := join(W1.dpairVars#0, W1.dpairVars#1, W1.dpairVars#2);
     projList2 := join(W2.dpairVars#0, W2.dpairVars#1, W2.dpairVars#2);
     WW.projMap1 = map(W1, WW, join(projList1, toList(nW2:0)));
     WW.projMap2 = map(W2, WW, join(toList(nW1:0), projList2));


-- MAKE TWISTMAP TO DIAGONAL
     if W1 === W2 and options.TwistMap then (
	  n := n1;
	  nW := nW1;
	  naux := m1;
     	  twistList := join (
	       apply (toList(0..n-1), i -> (1/2)*WW_i - WW_(nW+n+i)),
	       apply (toList(0..n-1), i -> (1/2)*WW_(nW+i) + WW_(n+i)),
	       apply (toList(0..naux-1), i -> WW_(2*n+i)),
	       apply (toList(0..n-1), i -> (-1/2)*WW_i - WW_(nW+n+i)),
	       apply (toList(0..n-1), i -> (1/2)*WW_(nW+i) - WW_(n+i)),
	       apply (toList(0..naux-1), i -> WW_(nW+2*n+i)));	   	  
	  twistInvList := join (
	       apply (toList(0..n-1), i -> WW_i - WW_(nW+i)),
	       apply (toList(0..n-1), i -> (1/2)*WW_(n+i) - (1/2)*WW_(nW+n+i)),
	       apply (toList(0..naux-1), i -> WW_(2*n+i)),
	       apply (toList(0..n-1), i -> WW_(n+i) + WW_(nW+n+i)),
	       apply (toList(0..n-1), i -> (-1/2)*WW_i + (-1/2)*WW_(nW+i)),
	       apply (toList(0..naux-1), i -> WW_(nW+2*n+i)));
	  WW.twistMap = map(WW, WW, twistList);
     	  WW.twistInvMap = map(WW, WW, twistInvList);
	  
	  WW.diagonal = ideal join(
	       apply (toList(0..n-1), i -> WW_i - WW_(nW+i)),
	       apply (toList(0..n-1), i -> WW_(n+i) + WW_(nW+n+i)));
	  );

-- MAKE F (external tensor) G
     FW := incMap1 F;
     GW := incMap2 G;
     FW**GW
     )
