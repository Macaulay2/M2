-----------------------------------
-- Primary decomposition code -----
-- ShimoyamaYokoyama algorithm
-- 
-- authors: W. Decker, G. Smith, M. Stillman, C. Yackel
--

quotMin = (I, facs, F) -> (
     J := quotient(I,F);
     if #facs > 1 then (
	  i := 0;
     	  while i < #facs and #facs > 1 do (
	       fac1 := drop(facs,{i,i});
	       G := product fac1;
	       J1 := quotient(I,G);
	       if J == J1
	       then (
	      	    facs = fac1;
	      	    F = G;
	      	    )
	       else i = i+1;
     	       );
	  );
     {J,facs,F}
     )
-- Sort the list polynomials in increasing degree order
sortByDegree = (facs) -> (
     x := apply(facs, f -> (degree f, f));
     x = sort x;
     apply(x, i -> i#1))

minSatPPD = (I, facs) -> (
     R := ring I;
     facs = sortByDegree facs;
     F0 := product facs;
     ret := quotMin(I,facs,F0);
     facs0 := ret#1;
     F := 1_R;   -- will be s.t. sat(I,F0) = I:F
     Iprev := I;
     while not ret#0 == Iprev do (
	  F = F * ret#2;
	  Iprev = ret#0;
	  ret = quotMin toSequence ret;
	  );
     {ret#0, F0, F, facs0}
     )

minSatPPD2 = (I, facs) -> (
     (satI, G) := greedySat(I, product facs);
     {satI, product facs, G, factors G})

TEST ///
debug PrimaryDecomposition
R = ZZ/32003[b,s,t,u,v,w,x,y,z]
I = ideal(
    b*v+s*u,
    b*w+t*u,
    s*w+t*v,
    b*y+s*x,
    b*z+t*x,
    s*z+t*y,
    u*y+v*x,
    u*z+w*x,
    v*z+w*y)
F = x*y*z
time gb generators I
time gb I
time (I:F)
time quotMin(I,{x,y,z},F)
time minSatPPD(I,{x,y,z})

I1 = I + ideal(v*w*y, u*v*x, s*v*y, s*t*y, b*s*x, x*y*z, s*t*v, u*v*w, 
     b*s*t, b*s*u, u*w*x, b*u*x, b*t*x, b*t*u, t*w*z)
time minSatPPD(I1,{b,t})

///

removeScalarMultipleColumns = m -> map(target m,,rawRemoveScalarMultiples raw m)

-- This version of newdecompose can be truly awful: the ideal I2 which is
--  passed to 'minimalPrimes' can have a huge number of generators.  Even cleaning the
--  gb by taking squarefree parts doesn't help
newdecompose = method()
newdecompose(Ideal) := List => (I) -> (
     squarefree := (f) -> (
     	  g := factor f;
     	  value apply(g, i -> i#0));
     if I.cache#?"MinimalPrimes" then I.cache#"MinimalPrimes"
     else I.cache#"MinimalPrimes" = (
       I1 := ideal apply(numgens I, i -> squarefree(I_i));
       I2 := trim ideal generators gb I1;
       if numgens I2 > 0 then minimalPrimes I2 else {}))

newdecompose(Ideal) := List => (I) -> minimalPrimes I

-- Make a list of the variables which occur in the lead 
-- monomial of f.
variables = (f) -> apply(positions(first exponents leadMonomial f, i -> i>0),j -> (ring f)_j)
 
-- Compute the flattener with respect to the 
-- variables in a monomial
flattener = (I, m) -> (
     -- First create a new ring with correct order
     local ones;
     local mm;
     local F;
     R := ring I;
     n := numgens R;
     vars1 := variables m;
     d := #vars1;
     vars2 := variables ((product generators R)//m);
     RU := (coefficientRing R) monoid([vars2,vars1,
	  MonomialOrder=>ProductOrder{n-d,d},
	  MonomialSize=>16]);
     J := substitute(I,RU);
     -- Collect lead coefficients of GB
     leads := leadTerm(1,generators gb J);
     ret := coefficients(leads, Variables => toList(0..n-d-1));
     -- coefficients returns two things.  The first is the
     -- list of coefficients.  The second is the 
     -- corresponding list of monomials.
     -- Note that leads looked like it switched the variable
     -- set under consideration, but by taking ret#1 instead
     -- of ret#0 next, the sets are again switched.
     coeffmat := ret#1;
     -- Intersect these ideals, taking first element found
     coeffmat = map(RU^(numgens target coeffmat),, coeffmat);
     ones = map(target coeffmat, 1, (i,j)->1);
     mm = ones | coeffmat;
     F = generators gb syz(mm, SyzygyLimit=>1, SyzygyRows=>1);
     result := substitute(F_(0,0),R);
     --print factors result;
     result
     )

flattener2 = (I,m) -> (
     C := flatt(I,m);
     F := (intersect values C)_0;
     --print factors F;
     F)
     
TEST ///
debug PrimaryDecomposition
R = ZZ/32003[a..d]
I = monomialCurveIdeal(R,{1,3,4})
IS = independentSets I
F1 = IS#0
variables F1
flattener(I,F1)
///

-- Return a list of the prime factors of F
-- TODO: replace with factors from MinimalPrimes?
factors = (F) -> (
     facs := factor F;
     facs = apply(#facs, i -> facs#i#0);
     select(facs, f -> # support f =!= 0))

-- find, if any, an element of the GB of I which is NOT in the ideal J.
findNonMember = (I,J) -> (
     m := generators I;
     n := gb J;
     x := rawGBContains(raw n, raw m);
     if x === -1 then false else I_x)

TEST ///
R = ZZ/32003[a..d]
I = monomialCurveIdeal(R,{1,3,4})
J = I + ideal(a^5-b^5)
debug PrimaryDecomposition
findNonMember(I,J)
findNonMember(J,I)

///

---------------------------------------------
-- (Hopefully) Fast quotient routines -------
---------------------------------------------

-- Return a list of three things:
-- 1. The ideal quotient I:F
-- 2. The list of factors in 'facs' that contribute to this quotient
--    i.e. (I : product facs) = I:F
-- 3. The product of the elements in 'facs'.

-- quotMinold := (I, facs, F) -> (
-- --     J := time(I:F);
--      J := (I:F);
--      if #facs > 1 then (
-- 	  i := 0;
--      	  while i < #facs and #facs > 1 do (
-- 	       fac1 := drop(facs,{i,i});
-- 	       G := product fac1;
-- 	       --J1 := time(I:G);
-- 	       J1 := (I:G);
-- 	       if J == J1 
-- 	       then (
-- 	      	    facs = fac1;
-- 	      	    F = G;
-- 	      	    )
-- 	       else i = i+1;
--      	       );
-- 	  );
--      {J,facs,F}
--      )

minSat = (I, F) -> minSatPPD(I, factors F)

-- minSatPPDold := (I, facs) -> (
--      R := ring I;
--      facs = sortByDegree facs;
--      F0 := product facs;
--      ret := quotMinold(I,facs,F0);
--      facs0 := ret#1;
--      F := 1_R;   -- will be s.t. sat(I,F0) = I:F
--      Iprev := I;
--      while ret#0 != Iprev do (
-- 	  F = F * ret#2;
-- 	  Iprev = ret#0;
-- 	  ret = quotMinold toSequence ret;
-- 	  );
--      {ret#0, F0, F, facs0}
--      )

TEST ///
R = ZZ/32003[a..d]
I = monomialCurveIdeal(R,{1,3,4})
J = monomialCurveIdeal(R,{1,2,3})
L = I^2 * J^3;
fac1 = I_1
fac2 = J_1
F = fac1 * fac2
debug PrimaryDecomposition
minSat(L,F)
quotMin(L,{fac1,fac2},F)
-- quotMinold(L,{fac1,fac2},F)
///


-- extract: Given an ideal I, whose radical is the prime ideal P,
-- compute the P-primary part Q of I, and another ideal I' such that
-- I = intersect(Q,I').
-- Returns the list {Q, I', list of irred factors of the sep poly}.

extract = (I,P) -> (
     -- return P-primary component of I, the remaining component
     -- and the irred factors of the min divisor of the extractor
     -- (a la minSat)
     --
     R := ring I;
     UU := independentSets(P, Limit=>1);  -- list of sets of variables
     local f;
     if #UU == 0 then
	  f = 1_R
     else (
	  exes := apply(UU, U -> flattener(I,U));
	  -- select the element of least degree
	  f = exes#0;
	  degf := degree f;
	  scan(exes, g -> if degree g < degf then 
	       (f = g; degf = degree g));
	  );
     if f != 1_R then (
	  ret := minSat(I,f);
	  {trim ret#0, I + ideal(ret#2), ret#3})
     else {I, ideal(1_R), ideal(1_R)}
     )

TEST ///
debug PrimaryDecomposition
R = ZZ/32003[a..d]
I = monomialCurveIdeal(R,{1,3,4})
L = I^2
extract(L,I)
PPD(L,{I})
///

-- PPD: produces a pseudo-primary decomposition of I,
-- where PP is a list of the isolated prime ideals of I
-- The return value is a a list of length 2: PPP, I'.
-- PPP is a list of quadruples, each having the items:
-- #0: pseudo-primary ideal
-- #1: prime ideal
-- #2: separator (polynomial)
-- #3: minimal divisor of the sep, as computed by minSat
--     (list of polynomials, which are the factors).

PPD = (I,PP) -> (
     R := ring I;
     if #PP === 1 then 
         {{{I, PP#0, 1_R, {1_R}}}, ideal(1_R)}
     else (
	  -- compute the separators
	  I' := I;
	  PPP := apply(toList(0..#PP-1), i -> (
	       ss := apply(toList(0..#PP-1), j -> if i == j then 0_R
		    else findNonMember(PP#j, PP#i));
	       -- the following throws out zeros, and elements which are 
	       -- not constant multiples of others.
	       seps := flatten entries removeScalarMultipleColumns matrix {ss};
	       ret := minSatPPD(I,seps);
	       I' = I' + ideal(ret#2);
	       {ret#0, PP#i, ret#1, ret#3}));
          {PPP, I'}))

TEST ///
debug PrimaryDecomposition
R = ZZ/32003[a..d]
I = ideal(a,b,c-1)
J = ideal(c,d,a-1)
L = intersect(I^2, J)
PPD(L,{I,J})
///

PPDcharSets = (I) -> PPD(I, newdecompose I)

PPDSpecialCharSets = (I, PP) -> (
     -- PP is a list of pairs (prime ideal P_i, a list of factors of a polynomial f_i).
     -- assumption: rad I = intersection(rad(P_i,f_i), all i)
     -- returns a PPD of I.
     -- make a list of all (prime,f), of some (P_i, factor of f_i), which have 
     -- the correct dimension.
     -- Then call PPD with this list.
     R := ring I;
     PPP := apply(PP, P1 -> (
	       P := P1#0;
	       II := apply(P1#1, f -> (
			 I := P + ideal(f);
			 c := codim I;
			 if 1_R % I == 0 then null
			 else select(newdecompose(I), i -> codim i == c)));
	       II = select(II, i -> i =!= null)));
     PPP = flatten flatten PPP;
     -- The following removes redundant prime ideals from this list
     PPP = new MutableList from PPP;
     scan(0..#PPP-1, i -> (
        if PPP#i =!= null then 
	scan(i+1..#PPP-1, j -> (
	    if PPP#i =!= null and PPP#j =!= null 
            then (
		if isSubset(PPP#i, PPP#j)
                then PPP#j = null
	        else if isSubset(PPP#j, PPP#i)
                then PPP#i = null)))));
     PPP = select(PPP, i -> i =!= null);
     PPD(I, PPP)
     )

TEST ///
debug PrimaryDecomposition
R = ZZ/32003[a..d]
I = ideal(a,b,c-1)
J = ideal(c,d,a-1)
L = intersect(I^2, J)
PPDSpecialCharSets(L, {{I,{d}},{J,{a}}})

///

ROOT := symbol ROOT
PSEUDO := symbol PSEUDO
REMAIN := symbol REMAIN

-- Nodes have the following structure
-- #0  ideal at this node
-- #1  1:root, 2:pseudo-primary, 3:remaining
-- #2  tree depth
-- #3  tester polynomial
-- #4  (if type 2) associated prime  (null otherwise)
-- #5  (if type 3) list of {associated prime ideal, {irred factors of extractor}}

if primdecComputation === symbol primdecComputation then
primdecComputation = new Type of MutableHashTable

-- private symbols used as keys:
protect thisNode, protect H, protect U, protect W

PDinitialize = (I) -> (
     if I.cache#?"PDC" then I.cache#"PDC"
     else (
     	  PDC := new primdecComputation;
     	  R := ring I;
     	  PDC.ideal = I;
     	  PDC.depth = 0;
     	  PDC.H = ideal(1_R);
     	  PDC.U = {};
     	  PDC.W = {{I, ROOT, 0, 1_R, null, {ideal(0_R), 1_R}}};
	  PDC.thisNode = null;
     	  I.cache#"PDC" = PDC;
     	  PDC
     ))

PDnext = (PDC) -> (
     if PDC.thisNode =!= null then PDC.thisNode else (
     -- returns the next node, and removes it from the list
     if #PDC.W === 0 then 
         error "no more nodes to try!";
     i := 0;
     done := false;
     while i < #PDC.W and not done do (
       	  if PDC.W#i#2 === PDC.depth then (
	       done = true;
	       PDC.thisNode = PDC.W#i;
	       PDC.W = drop(PDC.W, {i,i}));
	  i = i+1;
	  );
     if done then 
          PDC.thisNode 
     else (
	  PDC.depth = PDC.depth+1;
	  PDnext PDC)
     ))

PDdonode = (PDC) -> (
     local ret;
     node := PDC.thisNode;
     done := false;
     if node#1 === PSEUDO then (
	  ret = extract(node#0, node#4);
	  if (generators PDC.H) % (ret#0) != 0 then (
	       -- add this component to answer
	       PDC.U = append(PDC.U, {ret#0,node#4});
	       PDC.H = intersect(PDC.H,ret#0);
	       -- MES: special equality here of course:
	       if generators PDC.H % PDC.ideal == 0 then (
		    -- We have the final answer:
		    done = true;
		    PDC.W = {}));
	  if not done then (
		    -- check whether ret#1 is redundant
		    if not radicalContainment(node#3, ret#1)
		    then (
			 -- build a new vertex
			 vertex := {ret#1, REMAIN, node#2 + 1, 
			      node#3, null, {{node#4, ret#2}}};
			 PDC.W = append(PDC.W, vertex)
			 )
		    )
          )	  
     else (
     	  if node#1 === REMAIN then 
     	  ret = PPDSpecialCharSets(node#0, node#5)
     	  else
     	  ret = PPDcharSets(node#0);
     	  -- for each quadruple:
     	  scan(ret#0, PPPP -> (
	       	    -- change the tester
	       	    tester := node#3 * PPPP#2;
	       	    P := PPPP#1;
	       	    if tester % P != 0 then (
		    	 -- build a new vertex
		    	 newvertex := {
			      PPPP#0,      -- a Qibar
			      PSEUDO,           -- type is 2
			      node#2 + 1, -- new tree depth
			      tester,
			      P,
			      null
			      };
		    	 PDC.W = append(PDC.W,newvertex);
		    	 )
	       	    ));
     	  -- The remaining component
     	  I' := ret#1;
     	  if I' != 1 then (
	       -- check whether is root of redundant tree
	       if not radicalContainment(node#3, I') then (
	       	    -- build a new vertex
	       	    newvertex := {
		    	 I',
		    	 REMAIN,    -- new type
		    	 node#2 + 2,
		    	 node#3,  -- tester
		    	 null,
		    	 apply(ret#0, PPPP -> {PPPP#1, PPPP#3})
		    	 };
	       	    PDC.W = append(PDC.W, newvertex);
	       	    );
	       )
     	  );
     PDC.thisNode = null;
     )

SYprimaryDecomposition = (I) -> (		    -- called by a later file
     C := PDinitialize I;
     while #C.W > 0 do (
	  PDnext C;
	  PDdonode C
	  );
     if C.H != I then error "algorithm missed components!";
     storeAssociatedPrimesComputation(comodule I, apply(C.U, i -> trim(i#1)), infinity);
     apply(C.U, i -> trim(i#0)))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PrimaryDecomposition.installed "
-- End:
