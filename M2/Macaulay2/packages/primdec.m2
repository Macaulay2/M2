needs "fastgb.m2"

-- debugging/timing test routine
timefun = (f,str) -> (args -> (
      t := timing f(args);
      << str << " time " << t#0 << " args: " << args << endl;
      t#1))

sortColumns := (m,degorder,monorder) -> (
     sendgg(ggPush m, ggPush degorder, ggPush monorder, ggsortcolumns);
     eePopIntarray())

///
R = ZZ/101[a..d]
m = matrix{{0,a,1,0,a^3,a^7,a^2,b}}
aa = sortColumns(m,0,1)
m_aa
aa = sortColumns(m,1,-1)
m_aa
aa = sortColumns(m,-1,-1)
m_aa
m = vars R
aa = sortColumns(m,0,1)
m_aa
m = symmetricPower(5,matrix{{1,a}})
///

-- Auto-reduction
autoReduce = method()
autoReduce Matrix := (m) -> (
     sendgg(ggPush m, ggautoreduce);
     getMatrix ring m)

///
R = ZZ/101[a..f]
m = matrix{{a^2-b-c,b^2-c-d,c^4-b^3-2*d}}
autoReduce m

///     
-- New simplify routine
simplify = method()
simplify (Matrix, ZZ) := (m,n) -> (
     sendgg(ggPush m, ggPush n, ggsimplify);
     getMatrix ring m)

-- Workaround for codim bug
--codim Ideal := (I) -> (
--     mi := monomialIdeal leadTerm I;
--     sendgg(ggPush mi, ggcodim);
--     eePopInt())

-- New decompose routine that stashes the answer
newdecompose = method()
///
newdecompose Ideal := (I) -> (
     if I.?components then I.components
     else I.components = decompose ideal mingens I)
///
newdecompose Ideal := (I) -> (
     print "decomposing I";
     c := time decompose I;
     if c === {} then error "bad decompose";
     c)
--newdecompose = timefun(newdecompose,"decompose")

-- Find the independent sets of the ideal I
-- This doesn't necessarily find them all
independentSets = (I) -> (
     inI := monomialIdeal leadTerm I;
     result := flatten entries generators minprimes inI;
     if #result === 1 and result#0 == 1_(ring I) then 
         {}
     else
         result
     )

-- Make a list of the variables which occur in the lead monomial of f.
variables = (f) -> (
     m := leadMonomial f;
     m1 := select(m, i -> i#1 > 0);
     m1 = new List from m1;
     apply(m1, i -> (ring f)_(i#0)))


-- Compute the flattener with respect to the variables in a monomial
flattener = (I, m) -> (
     -- First create a new ring with correct order
     --local RU, local J, local vars1, local vars2, local leads, local ret, local coeffmat, local ones, local mm, localF;
     R := ring I;
     n := numgens R;
     vars1 = variables m;
     d := #vars1;
     vars2 = variables ((product gens R)//m);
     RU := (coefficientRing R) monoid([vars2,vars1,MonomialOrder=>ProductOrder{n-d,d},MonomialSize=>16]);
     J = substitute(I,RU);
     -- Collect lead coefficients of GB
     time leads = leadTerm(n-d,gens gb J);
     ret = coefficients(toList(0..n-d-1),transpose leads);
     coeffmat = ret#1;
     -- Intersect these ideals, taking first element found
     coeffmat = map(RU^(numgens source coeffmat),, transpose coeffmat);
     ones = map(RU^(numgens target coeffmat), 1, (i,j)->1);
     time mm = ones | coeffmat;
     time F = gens gb syz(mm, SyzygyLimit=>1, SyzygyRows=>1);
     time substitute(F_(0,0),R)
     )
--flattener = timefun(flattener,"flattener")
     
TEST ///
R = ZZ/32003[a..d]
I = monomialCurveIdeal(R,{1,3,4})
IS = independentSets I
F1 = IS#0
variables F1
flattener(I,F1)
///

-- Sort the list polynomials in increasing degree order
sortByDegree = (facs) -> (
     x := apply(facs, f -> {degree f, f});
     x = sort x;
     apply(x, i -> i#1))

-- Return a list of the prime factors of F
factors = (F) -> (
     facs := factor F;
     facs = apply(#facs, i -> facs#i#0);
     select(facs, f -> degree f =!= {0}))

///
isSubset = (m,n) -> (
    sendgg(ggPush gens m, ggPush gb n, ggissubset);
    eePopInt() === -1)
///

-- find, if any, an element of the GB of J which is
-- NOT in the ideal I.  This can be made MUCH faster.
findNonMember = (I,J) -> (
     m := gens I;
     n := gb J;
     sendgg(ggPush m, ggPush n, ggissubset);
     x := eePopInt();
     if x === -1 then false else I_x)

///
     f := 0_(ring I);
     i := 0;
     found := false;
     g := gens gb J;
     while not found do (
	  f1 := g_(0,i);
	  if f1 % I != 0
	  then (f = f1; found=true)
	  else i = i+1;
	  );
     f
     )
///

TEST ///
R = ZZ/32003[a..d]
I = monomialCurveIdeal(R,{1,3,4})
J = I + ideal(a^5-b^5)
findNonMember(I,J)
findNonMember(J,I)

///

-- Determine whether g is in rad(I)
radicalContainment = (g,I) -> (
     R := ring I;
     n := numgens R;
     S := (coefficientRing R)[Variables=>n+1];
     mapto := map(S,R,submatrix(vars S,{0..n-1}));
     I = mapto I;
     g = mapto g;
     J := I + ideal(g*S_n-1);
     1_S % J == 0_S
     )

TEST ///
R = ZZ/32003[a..f]
F = map(R,R,symmetricPower(2,matrix{{a,b,c}}))
I = ker F
J = I^2
G = I_0
radicalContainment(G,J)
radicalContainment(G-a^2,J)
///

---------------------------------------------
-- (Hopefully) Fast quotient routines -------
---------------------------------------------

-- Return a list of three things:
-- 1. The ideal quotient I:F
-- 2. The list of factors in 'facs' that contribute to this quotient
--    i.e. (I : product facs) = I:F
-- 3. The product of the elements in 'facs'.

quotMinold = (I, facs, F) -> (
--     J := time(I:F);
     J := (I:F);
     if #facs > 1 then (
	  i := 0;
     	  while i < #facs and #facs > 1 do (
	       fac1 := drop(facs,{i,i});
	       G := product fac1;
	       --J1 := time(I:G);
	       J1 := (I:G);
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
--quotMinold = timefun(quotMinold,"quotMinold")
TEST ///
R = ZZ/32003[a..d]
I = monomialCurveIdeal(R,{1,3,4})
J = monomialCurveIdeal(R,{1,2,3})
L = I^2 * J^3
fac1 = I_1
fac2 = J_1
F = fac1 * fac2
quotMinold(L,{fac1,fac2},F)
///

minSat = (I, F) -> minSatPPD(I, factors F)

minSatPPDold = (I, facs) -> (
     local R, F0, ret, facs0, F, Iprev;
     R = ring I;
     facs = sortByDegree facs;
     F0 = product facs;
     ret = quotMinold(I,facs,F0);
     facs0 = ret#1;
     F = 1_R;   -- will be s.t. sat(I,F0) = I:F
     Iprev = I;
     while ret#0 != Iprev do (
	  F = F * ret#2;
	  Iprev = ret#0;
	  ret = quotMinold toSequence ret;
	  );
     {ret#0, F0, F, facs0}
     )
--minSatPPDold = timefun(minSatPPDold,"minSatPPDold")
TEST ///
R = ZZ/32003[a..d]
I = monomialCurveIdeal(R,{1,3,4})
J = monomialCurveIdeal(R,{1,2,3})
L = I^2 * J^3
fac1 = I_1
fac2 = J_1
F = fac1 * fac2
minSatold(L,F)
quotMinold(L,{fac1,fac2},F)
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
     UU := independentSets(P);  -- list of sets of variables
     local f;
     if #UU == 0 then
	  f = 1_R
     else (
	  exes = apply(UU, U -> flattener(I,U));
	  -- select the element of least degree
	  f = exes#0;
	  degf := degree f;
	  scan(exes, g -> if degree g < degf then (f = g; degf = degree g));
	  );
     if f != 1_R then (
	  ret := minSat(I,f);
	  {ret#0, I + ideal(ret#2), ret#3})
     else {I, ideal(1_R), ideal(1_R)}
     )

--extract = timefun(extract, "extract")

TEST ///
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
	  I' = I;
	  PPP := apply(toList(0..#PP-1), i -> (
	       ss = apply(toList(0..#PP-1), j -> if i == j then 0_R
		    else findNonMember(PP#j, PP#i));
	       -- the following throws out zeros, and elements which are 
	       -- not constant multiples of others.
	       seps = flatten entries simplify(matrix {ss},2);
	       ret = minSatPPD(I,seps);
	       I' = I' + ideal(ret#2);
	       {ret#0, PP#i, ret#1, ret#3}));
          {PPP, I'}))

TEST ///
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
     time PPP = apply(PP, P1 -> (
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
     time scan(0..#PPP-1, i -> (
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
R = ZZ/32003[a..d]
I = ideal(a,b,c-1)
J = ideal(c,d,a-1)
L = intersect(I^2, J)
PPDSpecialCharSets(L, {{I,{d}},{J,{a}}})
///

--ROOT := 1
--PSEUDO := 2
--REMAIN := 3

-- Nodes have the following structure
-- #0  ideal at this node
-- #1  1:root, 2:pseudo-primary, 3:remaining
-- #2  tree depth
-- #3  tester polynomial
-- #4  (if type 2) associated prime  (null otherwise)
-- #5  (if type 3) list of {associated prime ideal, {irred factors of extractor}}

if primdecComputation === symbol primdecComputation
then
primdecComputation = new Type of MutableHashTable

PD = method()
PD Ideal := (I) -> (
     if I.cache.?PDC then 
          I.cache.PDC 
     else (
     	  PDC := new primdecComputation;
     	  R := ring I;
     	  PDC.ideal = I;
     	  PDC.depth = 0;
     	  PDC.H = ideal(1_R);
     	  PDC.U = {};
     	  PDC.W = {{I, ROOT, 0, 1_R, null, {ideal(0_R), 1_R}}};
	  PDC.thisNode = null;
     	  I.cache.PDC = PDC;
     	  PDC
     ))

next = method()
next primdecComputation := (PDC) -> (
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
	  next PDC)
     ))

donode = (PDC) -> (
     node := PDC.thisNode;
     done := false;
     if node#1 === PSEUDO then (
	  ret = extract(node#0, node#4);
	  if (gens PDC.H) % (ret#0) != 0 then (
	       -- add this component to answer
	       PDC.U = append(PDC.U, {ret#0,node#4});
	       PDC.H = intersect(PDC.H,ret#0);
	       -- MES: special equality here of course:
	       if gens PDC.H % PDC.ideal == 0 then (
		    -- We have the final answer:
		    done = true;
		    PDC.W = {}));
	  if not done then (
		    -- check whether ret#1 is redundant
		    if not radicalContainment(node#3, ret#1)
		    then (
			 -- build a new vertex
			 vertex = {ret#1, REMAIN, node#2 + 1, node#3, null, {{node#4, ret#2}}};
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
     	  if I' != ideal(1_R) then (
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


primaryDecomposition = (I) -> (
     C := PD I;
     while #C.W > 0 do (
	  next C;
	  donode C
	  );
     if C.H != I then error "algorithm missed components!";
     C.U)
     
TEST ///
-- Simple examples
-- Example 1.
R = ZZ/32003[a..d]
I = ideal(a*b, c*d, a*c+b*d)
time primaryDecomposition I
  -- 3 components
  -- (a,d), (b,c), ((c,d)^2,(a,b)^2,ac+bd)
  
-- Example 2.
R = ZZ/32003[a,b,c]
I = ideal(a^2, a*b)
time primaryDecomposition I
  -- two components: (a), (a2, b)

-- Example 3.  
R = ZZ/32003[a..d]
I = ideal(a,b,c-1)
J = ideal(c,d,a-1)
L = intersect(I^2, J)
time primaryDecomposition L

-- By hand: 
C1 = PD L
next C1
donode C1
peek C1

///
end
flattener = profile("flattener",flattener)
minSatPPD = profile("minSatPPD", minSatPPD)
extract = profile("extract", extract)
PPD = profile("PPD",PPD)
PPDSpecialCharSets = profile("PPDSpecialCharSets", PPDSpecialCharSets)
donode = profile("donode", donode)
newdecompose = profile("newdecompose", newdecompose)
