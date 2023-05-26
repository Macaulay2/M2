-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- The routine shifts returns the vector of degree shifts of matrix
-- m with respect to weight w, where the target is shifted by oldshifts
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
shifts := (m, w, oldshifts) -> (
     tempmat := compress leadTerm m;
     if numgens source tempmat == 0 then newshifts := {}
     else (
     	  expmat := matrix(apply(toList(0..numgens source tempmat - 1), 
		    i -> (k := leadComponent tempmat_i;
	       	    	 append((exponents tempmat_(k,i))#0, oldshifts#k))));
     	  newshifts = (entries transpose (
	     	    expmat*(transpose matrix{ append(w, 1) })) )#0;
     	  );
     newshifts)
     

     
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- The following routines are needed to use Mike's new engine schreyer code.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

importFrom_Core {"rawKernelOfGB"}
kerGB := m -> (
     -- m should be a matrix which is a GB, and
     -- whose source has the Schreyer order.
     -- The resulting map will have the same form.
     map(ring m, rawKernelOfGB raw m)
     )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- 
-- Routine: "Dresolution"   /   Abbreviations: "Dres"
--
-- This routine computes a free resolution adapted to the weight vector w.
--
-- Input: I, an ideal, or M, a module
--        w, weight vector of the form (-u,u)
--        k, length of resolution
--
-- Output: Free resolution of (D/I) or (M) of length k adapted to
-- 	   the filtration determined by  w
--
-- Method: At each step of the resolution, computes a GB of the syzygy module
--         adapted to the w-filtration.  Uses V-homogenization to do this.
--
-- Reference: Oaku-Takayama 1999, "Algorithms for D-modules"
--
-- Caveats: 
--    1. Resolutions not unique.  In particular, stopping computation and 
--       restarting it can give a different result than doing it all at once.
--
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Dresolution = method( Options => {Strategy => Schreyer, LengthLimit => infinity} )
Dresolution Ideal  := options -> I -> Dresolution(comodule I, options)
Dresolution Module := options -> M -> (

     pInfo (1, "ENTERING Dresolution ... ");
     
     W := ring M;
     N := presentation M;

     pInfo (1, "Computing usual resolution using Schreyer order ..."); 
     pInfo (2, "\t Degree " | 0 | "...");
     pInfo (2, "\t\t\t Rank = " | rank target N | "\t time = 0. seconds");
     pInfo (2, "\t Degree " | 1 | "...");
     tInfo := toString first timing (m := schreyerOrder gens gb N);
     pInfo (2, "\t\t\t Rank = " | rank source N | "\t time = " |
	  tInfo | " seconds");

     M.cache.resolution = new ChainComplex;
     M.cache.resolution.ring = W;
     M.cache.resolution#0 = target m;
     M.cache.resolution#1 = source m;
     M.cache.resolution.dd#0 = map(W^0, target m, 0);
     M.cache.resolution.dd#1 = m;

     i := 2;
     while source m != 0 and i <= options.LengthLimit do (
	  pInfo (2, "\t Degree " | i | "...");
	  tInfo = toString first timing (m = kerGB m);
     	  M.cache.resolution#i = source m;
     	  M.cache.resolution.dd#i = m;
	  pInfo(2, "\t\t\t Rank = " | rank source m | "\t time = " |
	       tInfo | " seconds");
	  i = i+1;
	  );
     M.cache.resolution.length = i-1;
     M.cache.resolution
     )

Dresolution (Ideal, List) := options -> (I, w) -> (
     Dresolution ((ring I)^1/I, w, options)
     )

Dresolution (Module, List) := options -> (M, w) -> (

     pInfo (1, "ENTERING Dresolution ... ");
     
     -- ERROR CHECKING:
     W := ring M;
     k := options.LengthLimit;

     -- check that W is a Weyl algebra
     if W.monoid.Options.WeylAlgebra == {}
     then error "expected a Weyl algebra";
     if any(W.monoid.Options.WeylAlgebra, v -> class v =!= List)
     then error "expected non-homogenized Weyl algebra";
     -- check that w is of the form (-u,u)
     createDpairs W;
     if #w =!= numgens W
     then error ("expected weight vector of length " | numgens W);
     if any( toList(0..#W.dpairInds#0 - 1),
	  i -> ( w#(W.dpairInds#0#i) + w#(W.dpairInds#1#i) != 0 ) )
     then error "expected weight vector of the form (-u,u)";

     -- PREPROCESSING
     if k == infinity then (
	  pInfo (1, "Computing adapted free resolution of length infinity using " 
	       | toString options.Strategy | " method...");
	  if (options.Strategy == Vhomogenize) then
     	  pInfo(2, "Warning: resolution via Vhomogenize might not terminate");
	  )
     else pInfo (1, "Computing adapted free resolution of length " | k | 
	  " using " | toString options.Strategy | " method...");

     homVar := symbol homVar;
     hvw := symbol hvw;
     if options.Strategy == Schreyer then (
    	  -- Make the homogenizing weight vector in HW
	  Hwt := toList(numgens W + 1:1);
     	  -- Make the V-filtration weight vector in HW
     	  Vwt := append(w,0);
     	  -- Make the homogeneous Weyl algebra
     	  HW := (coefficientRing W)(monoid [(entries vars W)#0, homVar,
	       WeylAlgebra => append(W.monoid.Options.WeylAlgebra, homVar),
	       MonomialOrder => {Weights=>Hwt, Weights=>Vwt, GRevLex}]);
     	  homVar = HW_homVar;
     	  WtoHW := map(HW, W, (vars HW)_{0..numgens W - 1});
     	  HWtoW := map(W, HW, (vars W)_{0..numgens W - 1} | matrix{{1_W}});
	  -- Also make the homogenizing Weyl algebra for shifts
     	  VW := (coefficientRing W)(monoid [hvw, (entries vars W)#0,
	       WeylAlgebra => W.monoid.Options.WeylAlgebra,
	       MonomialOrder => Eliminate 1]);
     	  HWtoVW := map(VW, HW, (vars VW)_{1..numgens W} | matrix{{VW_0}});
     	  VWtoHW := map(HW, VW, matrix{{homVar}} | (vars HW)_{0..numgens HW - 2});
	  hvwVar := VW_0;
	  HVWwt := prepend(-1,w);
	  VWwt := prepend(0,w);
	  )
     else if options.Strategy == Vhomogenize then (
     	  Hwt = prepend(-1,w);
     	  Vwt = prepend(0,w);
	  -- make the homogenizing Weyl algebra
     	  HW = (coefficientRing W)(monoid [homVar, (entries vars W)#0,
	       WeylAlgebra => W.monoid.Options.WeylAlgebra,
	       MonomialOrder => Eliminate 1]);
     	  homVar = HW_homVar;
     	  WtoHW = map(HW, W, (vars HW)_{1..numgens W});
     	  HWtoW = map(W, HW, matrix{{1_W}} | (vars W));
	  );

     -- CREATE AND INITIALIZE THE CHAIN COMPLEX
     --else 
     N := presentation M;
     --if (isSubmodule M) then N := presentation ((ambient M)/M);
     -- get the degree shifts right (need to check this against OT paper)
     if not M.cache.?resolution 
     then M.cache.resolution = new MutableHashTable;
     M.cache.resolution#w = new ChainComplex;
     M.cache.resolution#w.ring = W;
     s := rank source N;
     t := rank target N;
     M.cache.resolution#w#0 = target N;
     M.cache.resolution#w.dd#0 = map(W^0, M.cache.resolution#w#0, 0);

     -- MAKE THE FIRST STEP OF THE RESOLUTION
     shiftvec := apply(degrees target N, i -> i#0); 
     tempMap := map(HW^(-shiftvec), HW^(rank source N), WtoHW N);
     pInfo (2, "\t Degree 0...");
     pInfo (2, "\t\t\t Rank = " | t | "\t time = 0 seconds");
     pInfo (3, "\t Degree 1...");
     tInfo := toString first timing (
	  Jgb := gens gb homogenize(tempMap, homVar, Hwt);
	  if options.Strategy == Schreyer then Jgb = schreyerOrder Jgb;
	  if options.Strategy == Schreyer then (
	       tempMat := map(VW^(-shiftvec), VW^(numgens source Jgb), HWtoVW(Jgb));
	       shiftvec = shifts(homogenize(HWtoVW Jgb, hvwVar, HVWwt),
		    VWwt, shiftvec);
	       )
	  else shiftvec = shifts(Jgb, Vwt, shiftvec);
	  M.cache.resolution#w#1 = W^(-shiftvec);
	  M.cache.resolution#w.dd#1 = map(M.cache.resolution#w#0, 
	       M.cache.resolution#w#1, HWtoW Jgb); 
	  );	
     pInfo(2, "\t\t\t Rank = " | #shiftvec | "\t time = " |
	  tInfo | " seconds");
     startDeg := 2;
	  
     -- COMPUTE REST OF THE RESOLUTION
     i := startDeg;
     while i < k+1 and numgens source Jgb != 0 do (
	  pInfo (2, "\t Degree " | i | "...");
	  tInfo = toString first timing (
	       if options.Strategy == Schreyer then Jgb = kerGB Jgb
     	       else if options.Strategy == Vhomogenize then (
	     	    -- compute the kernel / syzygies
     	     	    Jsyz := syz Jgb;
	     	    -- put syzygies in the free module with the correct degree shifts
	     	    Jsyzmap := map(HW^(-shiftvec), HW^(numgens source Jsyz), Jsyz);
	     	    -- compute an adapted (-w,w)-GB of the syzygies module
     	     	    Jgb = gens gb homogenize(Jsyzmap, homVar, Hwt);
	     	    );
	       if options.Strategy == Schreyer then (
	       	    tempMat = map(VW^(-shiftvec), VW^(numgens source Jgb), HWtoVW(Jgb));
	       	    shiftvec = shifts(homogenize(tempMat, hvwVar, HVWwt), 
		       	 VWwt, shiftvec);
	       	    )
	       else shiftvec = shifts(Jgb, Vwt, shiftvec);
	       M.cache.resolution#w#i = W^(-shiftvec);
	       M.cache.resolution#w.dd#i = map(M.cache.resolution#w#(i-1),
	       	    M.cache.resolution#w#i, HWtoW Jgb);
	       );
	  pInfo(2, "\t\t\t Rank = " | #shiftvec | "\t time = " |
	       tInfo | " seconds");
	  i = i+1;
     	  );
     M.cache.resolution#w
     )

-- install a new hook
addHook((resolution, Module), Strategy => WeylAlgebra,
    (o, M) -> if isWeylAlgebra(R := ring M) then Dresolution(M,
	applyPairs(options Dresolution, (key, val) -> (key, o#key))))

TEST ///
-- Boundary cases
x = symbol x; Dx = symbol Dx; 
W = QQ[x, Dx, WeylAlgebra => {x=>Dx}];
I = ideal 0_W;
J = ideal 1_W;
w = {-1,1}
assert( Dres(I) == Dres(I, w) );
assert( Dres(W^1/I) == Dres(W^1/I, w) );
assert( Dres(module I) == Dres(module I, w) );
assert( Dres(J) == Dres(J, w) );
assert( Dres(W^1/J) == Dres(W^1/J, w) );
assert( Dres (module J) == Dres(module J, w) );

-- Resolutions in the same res Grobner cone
A = matrix{{1,1,1},{1,3,6}};
b = {3,2};
I = gkz(A,b);
F1 = Dres(I, {-1,-2,-21,1,2,21});
F2 = Dres(I, {-1,-2,-20,1,2,20});
assert all(toList(0..length F1), i -> F1.dd#i - F2.dd#i == 0);

F3 = Dres(I, {-1,-2,-21,1,2,21}, Strategy => Vhomogenize);
F4a = Dres(I, {-1,-2,-20,1,2,20}, Strategy => Vhomogenize);
assert all(toList(0..length F3), i -> F3.dd#i - F4a.dd#i == 0);

F5 = Dres(I, {-3,-1,-3,3,1,3});
assert(F5.dd#1 - gbw(gens I, {-3,-1,-3,3,1,3}) == 0);
///
