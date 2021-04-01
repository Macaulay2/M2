-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

local GBprune
-- Basic D-routines

----------------------------------------------------------------
-- this function makes a Weyl algebra associated to a polynomial
-- ring.
----------------------------------------------------------------
makeWeylAlgebra = method(Options => {SetVariables => true})
makeWeylAlgebra PolynomialRing := opts -> R -> (
     coordVars := gens R;
     diffVars := apply(coordVars, i -> value("symbol d" | toString(i)) );
     allVars := join(coordVars, diffVars);
     W := (coefficientRing R)(monoid [allVars, WeylAlgebra =>
	  apply(coordVars, diffVars, (x,dx) -> x => dx)]);
     if opts.SetVariables then use W;
     W)

-- this function associates to a Weyl algebra W
-- the key "dpairVars", which consists of the following 3 lists
-- 1. coordinate variables 
-- 2. corresponding derivative variables
-- 3. central variables
-- and the key "dpairInds", which consists of lists holding the
-- index in W of each variable in the lists of "dpairVars".
createDpairs = method()
createDpairs PolynomialRing := W -> (
     if W.monoid.Options.WeylAlgebra === {} then
     error "Expected a Weyl algebra" ;

     if not W.?dpairVars then (
     	  coordListV := {};
     	  diffListV := {};
     	  centralListV := {};
     	  coordListI := {};
     	  diffListI := {};
     	  centralListI := {};
     	  nW := numgens W;
     	  i := 0;
     	  while i < numgens W do (
	       tempFlag := false;
     	       j := 0;
	       while j < numgens W do (
	            if W_i * W_j - W_j * W_i == -1 then (
		    	 coordListV = append(coordListV, W_i);
		    	 diffListV = append(diffListV, W_j);
		    	 coordListI = append(coordListI, i);
		    	 diffListI = append(diffListI, j);		    
		    	 tempFlag = true;
		    	 );
	            if W_i * W_j - W_j * W_i == 1 then
	       	    tempFlag = true;
	       	    j = j + 1;
	       	    );
	       if not tempFlag then (
	       	    centralListV = append(centralListV, W_i);
	       	    centralListI = append(centralListI, i);
	       	    );
	       i = i + 1;
	       );
     	  W.dpairVars = {coordListV, diffListV, centralListV}; 
     	  W.dpairInds = {coordListI, diffListI, centralListI};
     	  );
     );

-- This routine extracts from a Weyl algebra D the polynomial ring in its ordinary variables (not its differentials).
extractVarsAlgebra = method()
extractVarsAlgebra PolynomialRing := D -> (
     if D.monoid.Options.WeylAlgebra === {} then
     error "Expected a Weyl algebra" ;
     createDpairs D;
     L := (D.dpairVars)#0;
     (coefficientRing D)(monoid [L, Degrees => L/degree])
     );

-- This routine extracts from a Weyl algebra D the polynomial ring in its differentials only.
extractDiffsAlgebra = method()
extractDiffsAlgebra PolynomialRing := D -> (
     if D.monoid.Options.WeylAlgebra === {} then
     error "Expected a Weyl algebra" ;
     createDpairs D;
     L := (D.dpairVars)#1;
     (coefficientRing D)(monoid [L, Degrees => L/degree])
     );

-- this new version of Dan's breaks something else:
--- createDpairs PolynomialRing := W -> (
---      if not W.?dpairVars then (
--- 	  xv := (options W).WeylAlgebra /first/baseName/(x -> W_x);
--- 	  dv := (options W).WeylAlgebra / last/baseName/(x -> W_x);
--- 	  cv := gens W - (set xv + set dv);
--- --	  xv = gens W - (set gens W - set xv);
--- --	  dv = gens W - (set gens W - set dv);
--- 	  W.dpairVars = {xv,dv,cv};
--- 	  W.dpairInds = applyTable(W.dpairVars, x -> position(gens W, y -> x == y));
--- 	  if debugLevel > 0 then (
--- 	       stderr << "-- gens W = " << gens W << endl;
--- 	       stderr << "-- W.dpairVars = " << W.dpairVars << endl;
--- 	       );
--- 	  );
---      )
--- 

-- This routine attaches to a Weyl algebra W the key "CommAlgebra"
-- which holds a commutative ring with the same variables
protect CommAlgebra
protect CAtoWA
protect WAtoCA
createCommAlgebra = method()
createCommAlgebra PolynomialRing := W -> (
     if W.monoid.Options.WeylAlgebra === {} then
     error "Expected a Weyl algebra" ;     
     W.CommAlgebra = (coefficientRing W)(monoid [W_*]);
     W.WAtoCA = map(W.CommAlgebra, W, vars W.CommAlgebra);
     W.CAtoWA = map(W, W.CommAlgebra, vars W);
     );




-- These routines compute the Fourier transform which is the automorhpism
-- of the Weyl algebra sending x -> -dx, dx -> x.
-- Input: RingElement f, Matrix m, Ideal I, ChainComplex C, or Module M
-- Output: Fourier transform of f, m, I, C, or M
Fourier = method()
FourierLocal := M -> (
     W := ring M;
     if W.monoid.Options.WeylAlgebra === {}
     then error "expected a Weyl algebra";
     createDpairs W;
     L := new MutableList from join(W.dpairInds#0, W.dpairInds#1, W.dpairInds#2);
     i := 0;
     while i < #W.dpairVars#0 do (
	  L#(W.dpairInds#0#i) = -W.dpairVars#1#i;
	  L#(W.dpairInds#1#i) = W.dpairVars#0#i;
	  i = i+1;
	  );
     i = 0;
     while i < #W.dpairVars#2 do (
	  L#(W.dpairInds#2#i) = W.dpairVars#2#i;
	  i = i+1;
	  );	  
     FMap := map(W, W, matrix {toList L});
     FMap M
     )
Fourier RingElement := M -> (FourierLocal M)
Fourier Ideal := M -> (FourierLocal M)
Fourier Matrix := M -> (FourierLocal M)
Fourier ChainComplex := M -> (FourierLocal M)
Fourier Module := M -> (cokernel FourierLocal relations prune M)

FourierInverse = method()
FourierInverseLocal := M -> (
     W := ring M;
     if W.monoid.Options.WeylAlgebra === {}
     then error "expected a Weyl algebra";
     createDpairs W;
     L := new MutableList from join(W.dpairInds#0, W.dpairInds#1, W.dpairInds#2);
     i := 0;
     while i < #W.dpairVars#0 do (
	  L#(W.dpairInds#0#i) = W.dpairVars#1#i;
	  L#(W.dpairInds#1#i) = -W.dpairVars#0#i;
	  i = i+1;
	  );
     i = 0;
     while i < #W.dpairVars#2 do (
	  L#(W.dpairInds#2#i) = W.dpairVars#2#i;
	  i = i+1;
	  );	  
     FInvMap := map(W, W, matrix {toList L});
     FInvMap M
     )
FourierInverse RingElement := M -> (FourierInverseLocal M)
FourierInverse Ideal := M -> (FourierInverseLocal M)
FourierInverse Matrix := M -> (FourierInverseLocal M)
FourierInverse ChainComplex := M -> (FourierInverseLocal M)
FourierInverse Module := M -> (cokernel FourierInverseLocal relations prune M)

-- These routines compute the transposition automorphism, which is used
-- to turn right modules to left modules and vice versa.  Currently slow.
Dtransposition = method()
Dtransposition RingElement := L -> (
     W := ring L;
     if W.monoid.Options.WeylAlgebra === {} then
     error "expected an element of a Weyl algebra";
     createDpairs W;

     if L != 0 then (     
     	  coordPart := substitute( (coefficients L)#0, 
	       join ( apply(W.dpairVars#1, j -> j=>1), 
		    apply(W.dpairVars#2, j -> j=>1) ) );
     	  otherPart := substitute( matrix{terms L}, 
	       join ( apply(W.dpairVars#0, j -> j=>1),
	       	    apply(W.dpairVars#1, j -> j=>-j) ) );
     	  transL := (coordPart * (transpose otherPart))_(0,0);
	  )
     else transL = 0_W;
     transL
     )

Dtransposition Matrix := m -> (
     W := ring m;
     if W.monoid.Options.WeylAlgebra === {} then
     error "expected an element of a Weyl algebra";
     createDpairs W;
     
     if numgens source m == 0 or numgens target m == 0 then mtrans := m
     else mtrans = matrix apply( entries m, i -> 
	  (apply (i, j -> Dtransposition j)) );
     mtrans
     )

Dtransposition Ideal := I -> (
     ideal Dtransposition gens I
     )

Dtransposition ChainComplex := C -> (
     W := ring C;
     if W.monoid.Options.WeylAlgebra === {} then
     error "expected an element of a Weyl algebra";
     createDpairs W;

     apply( keys C.dd, i -> if (class C.dd#i === Matrix) then
	       	    C.dd#i = Dtransposition C.dd#i);
     C
     )

-- puts a module or matrix purely in shift degree 0.
zeroize = method()
zeroize Module := M -> (
     W := ring M;
     P := presentation M;
     coker map(W^(numgens target P), W^(numgens source P), P)
     )

zeroize Matrix := m -> (
     W := ring m;
     map(W^(numgens target m), W^(numgens source m), m)
     )

-- MES added 1/30/05 temorpary fix
-- check whether a module is a quotient of a free module.
--   In the Dmodule code, it appears that this is checked in
--   3 ways: using isQuotientModule, doing what is done here,
--   and doing what is done here, without the zeroize.
ensureQuotientModule = method()
ensureQuotientModule(Module, String) := (M,errorString) -> (
   F := (ring M)^(numgens source gens M);
   if zeroize gens M != map(F,F,1) 
   then error errorString;
   )

-- This routine computes the dimension of a D-module
Ddim = method()
Ddim Ideal := I -> (
     -- preprocessing
     W := ring I;
     -- error checking
     if  W.monoid.Options.WeylAlgebra === {} 
     then error "expected a Weyl algebra";
     -- do the computation
     gbI := gb I;
     if not W.?CommAlgebra then createCommAlgebra W;
     ltI := W.WAtoCA leadTerm gens gbI;
     dim ideal ltI
     )

Ddim Module := M -> (
     -- preprocessing
     W := ring M;
     m := presentation M;
     -- error checking
     if  W.monoid.Options.WeylAlgebra === {} 
     then error "expected a Weyl algebra";
     -- do the computation
     gbm := gb m;
     if not W.?CommAlgebra then createCommAlgebra(W);
     ltm := W.WAtoCA leadTerm gens gbm;
     dim cokernel ltm
     )

-- This routine determines whether a D-module is holonomic
isHolonomic = method()
isHolonomic Ideal := I -> (
     -- preprocessing
     W := ring I;
     -- error checking
     if W.monoid.Options.WeylAlgebra === {}
     then error "expected a Weyl algebra";
     createDpairs W;
     if W.dpairVars#2 =!= {}
     then error "expected a Weyl algebra without central parameters";
     Ddim I == #(W.dpairVars#0)
     )

isHolonomic Module := M -> (
     -- preprocessing
     W := ring M;
     -- error checking
     if W.monoid.Options.WeylAlgebra === {}
     then error "expected a Weyl algebra";
     createDpairs W;
     if W.dpairVars#2 =!= {}
     then error "expected a Weyl algebra without central parameters";
     Ddim M == #(W.dpairVars#0)
     )

-- This routine computes the rank of a D-module
-- QUESTION: this changes the current ring?
holonomicRank = method()
holonomicRank Ideal := I -> (
     holonomicRank ((ring I)^1/I)
     )

holonomicRank Module := M -> (
     W := ring M;
     createDpairs W;
     n := #(W.dpairInds#0);
     m := numgens W;
     presM := presentation M;
     -- get weight vectors for the order filtration refined 
     -- by lex on the derivatives
     weightList := { apply ( toList(0..m-1), i -> if member(i, W.dpairInds#1) 
	  then 1 else 0 ) };
     -- ring equipped with the new order
     tempW := (coefficientRing W)(monoid [W_*,
	  WeylAlgebra => W.monoid.Options.WeylAlgebra,
	  Weights => weightList]);
     WtotempW := map (tempW, W, vars tempW);
     -- commutative ring of derivative variables
     Rvars := symbol Rvars;
     R := (coefficientRing W)(monoid [apply(toList(0..n-1), i -> Rvars_i)]);
     newInds := inversePermutation join(W.dpairInds#1, W.dpairInds#0);
     matList := apply( toList(0..m-1), i -> if newInds#i < n
	  then R_(newInds#i) else 1_R );
     tempWtoR := map (R, tempW, matrix{ matList });
     -- computing GB with respect to new order
     ltM := leadTerm gens gb WtotempW presM;
     -- compute the rank
     redI := cokernel tempWtoR ltM;
     if dim redI > 0 then holRank := infinity
     else if redI == 0 then holRank = 0
     else holRank = numgens source basis redI;
     holRank
     )

-- This routine computes the characteristic ideal of a D-module
charIdeal = method()
charIdeal Ideal := I -> (
     W := ring I;
     if W.monoid.Options.WeylAlgebra == {}
     then error "expected a Weyl algebra";
     createDpairs W;
     w := apply( toList(0..numgens W - 1), 
	  i -> if member(i, W.dpairInds#1) then 1 else 0 );
     ideal mingens inw (I, w)
     )

charIdeal Module := M -> (
     W := ring M;
     m := presentation M;
     if W.monoid.Options.WeylAlgebra == {}
     then error "expected a Weyl algebra";
     createDpairs W;
     w := apply( toList(0..numgens W - 1), 
	  i -> if member(i, W.dpairInds#1) then 1 else 0 );
     ideal mingens ann cokernel inw (m, w)
     )

-- This routine computes the singular locus of a D-ideal
-- SHOULD IT BE CHANGED SO THAT OUTPUT IS IN POLY SUBRING?
singLocus = method()
singLocus Ideal := I -> (
     singLocus ((ring I)^1/I)
     )

singLocus Module := M -> (
     W := ring M;
     createDpairs W;
     if not W.?CommAlgebra then createCommAlgebra W;
     I1 := charIdeal M;
     I2 := W.WAtoCA ideal W.dpairVars#1;
     -- do the saturation
     SatI := saturate(I1, I2);
     -- set up an auxiliary ring to perform intersection
     tempCA := (coefficientRing W)(monoid [W.dpairVars#1, W.dpairVars#0, 
          MonomialOrder => Eliminate (#W.dpairInds#1)]);
     newInds := inversePermutation join(W.dpairInds#1, W.dpairInds#0);
     CAtotempCA := map(tempCA, W.CommAlgebra, 
	  matrix {apply(newInds, i -> tempCA_i)});
     tempCAtoCA := map(W.CommAlgebra, tempCA, matrix{ join (
		    apply(W.dpairVars#1, i -> W.WAtoCA i),
	            apply(W.dpairVars#0, i -> W.WAtoCA i) ) } );
     -- do the intersection

     gbSatI := gb CAtotempCA SatI;
     I3 := ideal compress tempCAtoCA selectInSubring(1, gens gbSatI);
     if I3 == ideal 1_(W.CommAlgebra) then W.CAtoWA I3
     else W.CAtoWA radical I3
     )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This routine prunes a matrix (whose cokernel represents a module) by
-- computing a GB and removing any column whose leadterm is a constant
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- GBprune = method()
-- GBprune Matrix := M -> (
--      Mgb := gens gb M;
--      n := numgens source Mgb;
--      columnList := {};
--      i := 0;
--      while i < n do (
--      	  lc := leadComponent Mgb_i;
-- 	  if Mgb_(lc,i) != 1 then
-- 	       columnList = append(columnList, i);
-- 	  i = i+1;
-- 	  );
--      if columnList == {} then Mnew := 0
--      else Mnew = transpose compress matrix apply(columnList, 
--      	  i -> ( (entries transpose Mgb_{i})#0 ) );
--      Mnew
--      )
-- 
-- GBprune Module := M -> (
--      if not isQuotientModule M then error "GBprune expected a quotient module";
--      cokernel GBprune relations M
--      )


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- The routine Dprune prunes a matrix by using engine's reduceCompress()
--
-- Options:
--   2) GB:     By default, a Grobner basis is computed and returned.
--     	    	The algorithm doesn't stop until the GB stabilizes.
--     	    	To only return the matrix after reducing pivots and compressing,
--     	    	set "GB => false".  The difference is that computing a gb may
--     	    	lead to the appearance of more 1's
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
Dprune = method(Options => {optGB => true})
Dprune Matrix := options -> M -> (
     temp := reduceCompress M;
     if options.optGB then (
	  proceedflag := true;
	  while proceedflag do (
	       temp2 := reduceCompress gens gb temp;
	       if temp2 === null or temp2 == temp then proceedflag = false;
	       temp = temp2;
	       );
	  );
     temp
     )

Dprune Module := options -> M -> (
     if not isQuotientModule M then error "Dprune expected a quotient module";
     cokernel Dprune relations M
     )

--- OLD VERSION OF Dprune.  Will be phased out ---
Dprune2 = method(Options => {optGB => true})
Dprune2 Matrix := options -> M -> (
     Dprune cokernel M
     )

debug Core

reduceCompress = method()
reduceCompress Matrix := (m) -> (
     R := ring m;
     msparse := rawMutableMatrix(raw m, true); -- true: make this a dense matrix...
     rawReduceByPivots msparse;
     mout := compress map(R,rawMatrix msparse);
     colCounter := numgens source mout - 1;
     rowCounter := numgens target mout - 1;
     --if (mout == id_(target mout)) then (mout = null)
     --else (
     while (rowCounter >= 0 and colCounter >= 0 and
	  mout_colCounter == (id_(target mout))_rowCounter) do (
	  colCounter = colCounter - 1;
	  rowCounter = rowCounter - 1;
	  );
     if (rank source mout == 0) then mout = mout
     else if (colCounter == -1 and rowCounter == -1) then mout = gens R^0
     else (
	  if (colCounter == -1) then colCounter = 0;
	  if (rowCounter == -1) then rowCounter = 0;
	  mout = compress mout_{0..colCounter}^{0..rowCounter};
	  );
     mout
     )
